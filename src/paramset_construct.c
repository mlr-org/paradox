#include <limits.h>
#include <stddef.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Utils.h>

#include "r_api_compat.h"
#include "r_utils.h"

/* data.table's secondary-index representation is intentionally internal.
 * Enable native construction only after .onLoad has compared this exact
 * process' data.table output with the schema understood below.  The first
 * attempt seals the DSO state even when the probe is unsupported: registered
 * native symbols are externally reachable, so a later call must not be able
 * to change the process-wide decision made during package loading. */
enum data_table_index_layout_state {
  DATA_TABLE_INDEX_LAYOUT_UNCONFIGURED = 0,
  DATA_TABLE_INDEX_LAYOUT_DISABLED,
  DATA_TABLE_INDEX_LAYOUT_ENABLED
};

static enum data_table_index_layout_state data_table_index_layout_state =
  DATA_TABLE_INDEX_LAYOUT_UNCONFIGURED;

enum domain_column {
  DOMAIN_ID = 0,
  DOMAIN_CLS,
  DOMAIN_GROUPING,
  DOMAIN_CARGO,
  DOMAIN_LOWER,
  DOMAIN_UPPER,
  DOMAIN_TOLERANCE,
  DOMAIN_LEVELS,
  DOMAIN_SPECIAL_VALS,
  DOMAIN_DEFAULT,
  DOMAIN_STORAGE_TYPE,
  DOMAIN_TAGS,
  DOMAIN_TRAFO,
  DOMAIN_REQUIREMENTS,
  DOMAIN_INIT_GIVEN,
  DOMAIN_INIT,
  DOMAIN_COLUMN_COUNT
};

enum construction_result {
  RESULT_PARAMS = 0,
  RESULT_TAGS,
  RESULT_TRAFOS,
  RESULT_REQUIREMENTS,
  RESULT_INIT_VALUES,
  RESULT_COUNT
};

static const char *const domain_column_names[DOMAIN_COLUMN_COUNT] = {
  "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
  "levels", "special_vals", "default", "storage_type", ".tags",
  ".trafo", ".requirements", ".init_given", ".init"
};

static const SEXPTYPE domain_column_types[DOMAIN_COLUMN_COUNT] = {
  STRSXP, STRSXP, STRSXP, VECSXP, REALSXP, REALSXP, REALSXP, VECSXP,
  VECSXP, VECSXP, STRSXP, VECSXP, VECSXP, VECSXP, LGLSXP, VECSXP
};

static const char *const permanent_column_names[DOMAIN_TAGS] = {
  "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
  "levels", "special_vals", "default", "storage_type"
};

static const char *const result_names[RESULT_COUNT] = {
  "params", "tags", "trafos", "requirements", "init_values"
};

static int string_is(SEXP value, const char *expected) {
  return value != NA_STRING && strcmp(CHAR(value), expected) == 0;
}

static int scalar_string_is_valid(SEXP value) {
  return TYPEOF(value) == STRSXP && !ALTREP(value) &&
    XLENGTH(value) == 1 &&
    STRING_ELT(value, 0) != NA_STRING;
}

/* This is the documented checkmate `type = "strict"` grammar used by the R
 * constructor: ^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$. Enforcing it here makes the
 * common native path independent of a preceding checkmate call and also makes
 * bytewise key ordering unambiguous. */
static int string_is_strict_id(SEXP value) {
  if (value == NA_STRING || LENGTH(value) == 0) {
    return FALSE;
  }

  const unsigned char *bytes = (const unsigned char *) CHAR(value);
  const int size = LENGTH(value);
  int index = 0;
  while (index < size && bytes[index] == '.') {
    ++index;
  }
  if (index == size ||
      !((bytes[index] >= 'a' && bytes[index] <= 'z') ||
        (bytes[index] >= 'A' && bytes[index] <= 'Z'))) {
    return FALSE;
  }

  for (++index; index < size; ++index) {
    const unsigned char byte = bytes[index];
    if (!((byte >= 'a' && byte <= 'z') ||
          (byte >= 'A' && byte <= 'Z') ||
          (byte >= '0' && byte <= '9') || byte == '.' || byte == '_')) {
      return FALSE;
    }
  }
  return TRUE;
}

static int class_is_builtin_domain(SEXP domain, SEXP cls) {
  if (TYPEOF(cls) != STRSXP || ALTREP(cls) || XLENGTH(cls) != 1) {
    return FALSE;
  }

  SEXP classes = PROTECT(Rf_getAttrib(domain, R_ClassSymbol));
  if (TYPEOF(classes) != STRSXP || ALTREP(classes) ||
      XLENGTH(classes) != 4) {
    UNPROTECT(1);
    return FALSE;
  }

  SEXP class_name = STRING_ELT(cls, 0);
  if (!string_is(class_name, "ParamDbl") &&
      !string_is(class_name, "ParamInt") &&
      !string_is(class_name, "ParamFct") &&
      !string_is(class_name, "ParamLgl") &&
      !string_is(class_name, "ParamUty")) {
    UNPROTECT(1);
    return FALSE;
  }

  SEXP first = STRING_ELT(classes, 0);
  SEXP second = STRING_ELT(classes, 1);
  SEXP third = STRING_ELT(classes, 2);
  SEXP fourth = STRING_ELT(classes, 3);
  const int supported = first != NA_STRING &&
    strcmp(CHAR(first), CHAR(class_name)) == 0 &&
    string_is(second, "Domain") && string_is(third, "data.table") &&
    string_is(fourth, "data.frame");
  UNPROTECT(1);
  return supported;
}

static SEXP snapshot_scalar_vector(SEXP source) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
  SEXP result = PROTECT(Rf_allocVector(type, 1));
  switch (type) {
  case STRSXP:
    SET_STRING_ELT(result, 0, STRING_ELT(source, 0));
    break;
  case VECSXP:
    SET_VECTOR_ELT(result, 0, VECTOR_ELT(source, 0));
    break;
  case REALSXP:
    SET_REAL_ELT(result, 0, REAL_ELT(source, 0));
    break;
  case INTSXP:
    SET_INTEGER_ELT(result, 0, INTEGER_ELT(source, 0));
    break;
  case LGLSXP:
    SET_LOGICAL_ELT(result, 0, LOGICAL_ELT(source, 0));
    break;
  default:
    UNPROTECT(1);
    Rf_error("Internal error: unsupported Domain snapshot type");
  }
  UNPROTECT(1);
  return result;
}

/* Capture each column exactly once into ordinary length-one vectors. This is
 * both an ALTREP materialization boundary and an independent GC root for every
 * child used after the constructor starts allocating its result tables. */
static SEXP snapshot_domain(SEXP domain) {
  if (TYPEOF(domain) != VECSXP || ALTREP(domain) ||
      XLENGTH(domain) != DOMAIN_COLUMN_COUNT) {
    return R_NilValue;
  }

  SEXP names = PROTECT(Rf_getAttrib(domain, R_NamesSymbol));
  if (TYPEOF(names) != STRSXP || ALTREP(names) ||
      XLENGTH(names) != DOMAIN_COLUMN_COUNT) {
    UNPROTECT(1);
    return R_NilValue;
  }

  SEXP snapshot = PROTECT(Rf_allocVector(VECSXP, DOMAIN_COLUMN_COUNT));
  for (R_xlen_t column = 0; column < DOMAIN_COLUMN_COUNT; ++column) {
    if (!string_is(STRING_ELT(names, column), domain_column_names[column])) {
      UNPROTECT(2);
      return R_NilValue;
    }
    SEXP value = PROTECT(VECTOR_ELT(domain, column));
    const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
    const int numeric_column = column == DOMAIN_LOWER ||
      column == DOMAIN_UPPER || column == DOMAIN_TOLERANCE;
    if (ALTREP(value) || (numeric_column
        ? type != INTSXP && type != REALSXP
          : type != domain_column_types[column]) ||
        XLENGTH(value) != 1) {
      UNPROTECT(3);
      return R_NilValue;
    }
    SEXP copy = PROTECT(snapshot_scalar_vector(value));
    SET_VECTOR_ELT(snapshot, column, copy);
    UNPROTECT(2);
  }

  SEXP cls = VECTOR_ELT(snapshot, DOMAIN_CLS);
  if (!class_is_builtin_domain(domain, cls) ||
      !scalar_string_is_valid(VECTOR_ELT(snapshot, DOMAIN_ID)) ||
      !scalar_string_is_valid(VECTOR_ELT(snapshot, DOMAIN_GROUPING)) ||
      !scalar_string_is_valid(VECTOR_ELT(snapshot, DOMAIN_STORAGE_TYPE))) {
    UNPROTECT(2);
    return R_NilValue;
  }

  SEXP tags = VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_TAGS), 0);
  if (TYPEOF(tags) != STRSXP || ALTREP(tags)) {
    UNPROTECT(2);
    return R_NilValue;
  }
  const R_xlen_t tag_count = XLENGTH(tags);
  SEXP stable_tags = PROTECT(Rf_allocVector(STRSXP, tag_count));
  for (R_xlen_t tag = 0; tag < tag_count; ++tag) {
    SEXP value = STRING_ELT(tags, tag);
    if (value == NA_STRING) {
      UNPROTECT(3);
      return R_NilValue;
    }
    SET_STRING_ELT(stable_tags, tag, value);
  }
  SET_VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_TAGS), 0, stable_tags);
  UNPROTECT(1);

  SEXP trafo = VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_TRAFO), 0);
  if (trafo != R_NilValue && !Rf_isFunction(trafo)) {
    UNPROTECT(2);
    return R_NilValue;
  }

  SEXP requirements = VECTOR_ELT(
    VECTOR_ELT(snapshot, DOMAIN_REQUIREMENTS),
    0
  );
  if (requirements != R_NilValue &&
      (TYPEOF(requirements) != VECSXP || ALTREP(requirements))) {
    UNPROTECT(2);
    return R_NilValue;
  }

  const int init_given = LOGICAL_ELT(
    VECTOR_ELT(snapshot, DOMAIN_INIT_GIVEN),
    0
  );
  if (init_given == NA_LOGICAL) {
    UNPROTECT(2);
    return R_NilValue;
  }
  UNPROTECT(2);
  return snapshot;
}

static int checked_add(R_xlen_t *total, R_xlen_t increment) {
  if (increment < 0 || *total > R_XLEN_T_MAX - increment) {
    return FALSE;
  }
  *total += increment;
  return TRUE;
}

static inline void account_work(R_xlen_t *work_since_interrupt) {
  ++*work_since_interrupt;
  if (*work_since_interrupt >= PARADOX_INTERRUPT_CHECK_INTERVAL) {
    R_CheckUserInterrupt();
    *work_since_interrupt = 0;
  }
}

static int character_precedes(SEXP values, R_xlen_t left, R_xlen_t right) {
  return strcmp(
    CHAR(STRING_ELT(values, left)),
    CHAR(STRING_ELT(values, right))
  ) <= 0;
}

static void stable_character_order(SEXP values, R_xlen_t *order,
    R_xlen_t *workspace,
    R_xlen_t size, R_xlen_t *work_since_interrupt) {
  for (R_xlen_t index = 0; index < size; ++index) {
    account_work(work_since_interrupt);
    order[index] = index;
  }

  for (R_xlen_t width = 1; width < size;) {
    for (R_xlen_t begin = 0; begin < size;) {
      const R_xlen_t middle = begin > size - width ? size : begin + width;
      const R_xlen_t remaining = size - middle;
      const R_xlen_t end = remaining < width ? size : middle + width;
      R_xlen_t left = begin;
      R_xlen_t right = middle;
      R_xlen_t output = begin;

      while (left < middle && right < end) {
        account_work(work_since_interrupt);
        workspace[output++] = character_precedes(
          values,
          order[left],
          order[right]
        )
          ? order[left++]
          : order[right++];
      }
      while (left < middle) {
        account_work(work_since_interrupt);
        workspace[output++] = order[left++];
      }
      while (right < end) {
        account_work(work_since_interrupt);
        workspace[output++] = order[right++];
      }

      if (end == size) {
        break;
      }
      begin = end;
    }

    for (R_xlen_t index = 0; index < size; ++index) {
      account_work(work_since_interrupt);
      order[index] = workspace[index];
    }

    if (width > size / 2) {
      break;
    }
    width *= 2;
  }
}

static int exact_plain_integer_vector(SEXP value,
    const int *expected, R_xlen_t size) {
  if (TYPEOF(value) != INTSXP || ALTREP(value) || XLENGTH(value) != size ||
      !paradox_api_has_no_attributes(value)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    if (INTEGER_ELT(value, index) != expected[index]) {
      return FALSE;
    }
  }
  return TRUE;
}

static int exact_integer_attribute(SEXP value, const char *name,
    int expected) {
  SEXP attribute = Rf_getAttrib(value, Rf_install(name));
  return TYPEOF(attribute) == INTSXP && !ALTREP(attribute) &&
    XLENGTH(attribute) == 1 &&
    paradox_api_has_no_attributes(attribute) &&
    INTEGER_ELT(attribute, 0) == expected;
}

static int exact_index_probe(SEXP index, const char *marker,
    const int *expected_order, R_xlen_t order_size,
    const int *expected_starts, R_xlen_t start_count,
    int expected_max_group) {
  if (TYPEOF(index) != INTSXP || ALTREP(index) || XLENGTH(index) != 0 ||
      !paradox_api_has_single_attribute(index, marker)) {
    return FALSE;
  }
  SEXP cache = Rf_getAttrib(index, Rf_install(marker));
  static const char *const cache_attributes[] = {
    "starts", "maxgrpn", "anyna", "anyinfnan", "anynotascii",
    "anynotutf8"
  };
  if (TYPEOF(cache) != INTSXP || ALTREP(cache) ||
      XLENGTH(cache) != order_size || !paradox_api_has_only_attributes(
        cache,
        cache_attributes,
        sizeof(cache_attributes) / sizeof(cache_attributes[0])
      )) {
    return FALSE;
  }
  for (R_xlen_t index_position = 0;
      index_position < order_size;
      ++index_position) {
    if (INTEGER_ELT(cache, index_position) !=
        expected_order[index_position]) {
      return FALSE;
    }
  }
  SEXP starts = Rf_getAttrib(cache, Rf_install("starts"));
  return exact_plain_integer_vector(
      starts,
      expected_starts,
      start_count
    ) && exact_integer_attribute(cache, "maxgrpn", expected_max_group) &&
    exact_integer_attribute(cache, "anyna", 0) &&
    exact_integer_attribute(cache, "anyinfnan", 0) &&
    exact_integer_attribute(cache, "anynotascii", 0) &&
    exact_integer_attribute(cache, "anynotutf8", 0);
}

SEXP paradox_param_set_index_layout(SEXP version, SEXP params_index,
    SEXP tags_index, SEXP identity_index, SEXP empty_index) {
  if (data_table_index_layout_state !=
      DATA_TABLE_INDEX_LAYOUT_UNCONFIGURED) {
    return Rf_ScalarLogical(
      data_table_index_layout_state == DATA_TABLE_INDEX_LAYOUT_ENABLED
    );
  }

  static const int params_order[] = {2, 3, 1};
  static const int params_starts[] = {1, 2, 3};
  static const int tags_order[] = {3, 6, 2, 4, 1, 7, 5};
  static const int tags_starts[] = {1, 2, 3, 4, 5, 7};
  static const int identity_starts[] = {1, 2, 3};
  const int reviewed_version = TYPEOF(version) == STRSXP &&
    !ALTREP(version) && XLENGTH(version) == 1 &&
    paradox_api_has_no_attributes(version) &&
    (string_is(STRING_ELT(version, 0), "1.17.8") ||
      string_is(STRING_ELT(version, 0), "1.18.4"));
  const int enabled = reviewed_version && exact_index_probe(
      params_index,
      "__id__cls__grouping",
      params_order,
      3,
      params_starts,
      3,
      1
    ) && exact_index_probe(
      tags_index,
      "__tag",
      tags_order,
      7,
      tags_starts,
      6,
      2
    ) && exact_index_probe(
      identity_index,
      "__id__cls__grouping",
      NULL,
      0,
      identity_starts,
      3,
      1
    ) && exact_index_probe(
      empty_index,
      "__tag",
      NULL,
      0,
      NULL,
      0,
      0
    );
  data_table_index_layout_state = enabled
    ? DATA_TABLE_INDEX_LAYOUT_ENABLED
    : DATA_TABLE_INDEX_LAYOUT_DISABLED;
  return Rf_ScalarLogical(enabled);
}

static int ascii_string(SEXP value) {
  if (value == NA_STRING || Rf_getCharCE(value) == CE_BYTES) {
    return FALSE;
  }
  const int size = LENGTH(value);
  const unsigned char *bytes = (const unsigned char *) CHAR(value);
  for (int index = 0; index < size; ++index) {
    if (bytes[index] == '\0' || bytes[index] >= 0x80) {
      return FALSE;
    }
  }
  return TRUE;
}

static int character_vector_is_ascii(SEXP values) {
  if (TYPEOF(values) != STRSXP || ALTREP(values)) {
    return FALSE;
  }
  const R_xlen_t size = XLENGTH(values);
  for (R_xlen_t index = 0; index < size; ++index) {
    if (!ascii_string(STRING_ELT(values, index))) {
      return FALSE;
    }
  }
  return TRUE;
}

static void attach_secondary_index(SEXP table, const char *marker,
    const R_xlen_t *order, R_xlen_t size,
    const R_xlen_t *group_starts, R_xlen_t group_count,
    R_xlen_t max_group) {
  if (size > INT_MAX || group_count > INT_MAX || max_group > INT_MAX) {
    return;
  }
  int identity = TRUE;
  for (R_xlen_t position = 0; position < size; ++position) {
    if (order[position] != position) {
      identity = FALSE;
      break;
    }
  }

  SEXP index = PROTECT(Rf_allocVector(INTSXP, 0));
  SEXP cache = PROTECT(Rf_allocVector(INTSXP, identity ? 0 : size));
  if (!identity) {
    for (R_xlen_t position = 0; position < size; ++position) {
      INTEGER(cache)[position] = (int) order[position] + 1;
    }
  }
  SEXP starts = PROTECT(Rf_allocVector(INTSXP, group_count));
  for (R_xlen_t group = 0; group < group_count; ++group) {
    INTEGER(starts)[group] = (int) group_starts[group] + 1;
  }
  SEXP maxgrpn = PROTECT(Rf_ScalarInteger((int) max_group));
  SEXP zero = PROTECT(Rf_ScalarInteger(0));
  Rf_setAttrib(cache, Rf_install("starts"), starts);
  Rf_setAttrib(cache, Rf_install("maxgrpn"), maxgrpn);
  Rf_setAttrib(cache, Rf_install("anyna"), zero);
  Rf_setAttrib(cache, Rf_install("anyinfnan"), zero);
  Rf_setAttrib(cache, Rf_install("anynotascii"), zero);
  Rf_setAttrib(cache, Rf_install("anynotutf8"), zero);
  Rf_setAttrib(index, Rf_install(marker), cache);
  Rf_setAttrib(table, Rf_install("index"), index);
  UNPROTECT(5);
}

static int attach_params_index(SEXP params, const R_xlen_t *order,
    R_xlen_t size) {
  if (data_table_index_layout_state != DATA_TABLE_INDEX_LAYOUT_ENABLED ||
      size > INT_MAX ||
      !character_vector_is_ascii(VECTOR_ELT(params, DOMAIN_ID)) ||
      !character_vector_is_ascii(VECTOR_ELT(params, DOMAIN_CLS)) ||
      !character_vector_is_ascii(VECTOR_ELT(params, DOMAIN_GROUPING))) {
    return FALSE;
  }
  R_xlen_t *starts = size == 0
    ? NULL
    : paradox_temporary_alloc(size, sizeof(*starts));
  for (R_xlen_t group = 0; group < size; ++group) {
    starts[group] = group;
  }
  attach_secondary_index(
    params,
    "__id__cls__grouping",
    order,
    size,
    starts,
    size,
    size == 0 ? 0 : 1
  );
  return TRUE;
}

int paradox_param_set_attach_singleton_index(SEXP params) {
  if (TYPEOF(params) != VECSXP || ALTREP(params) ||
      XLENGTH(params) != DOMAIN_TAGS) {
    return FALSE;
  }
  for (enum domain_column column = DOMAIN_ID;
      column <= DOMAIN_GROUPING;
      column = (enum domain_column) (column + 1)) {
    SEXP values = VECTOR_ELT(params, column);
    if (TYPEOF(values) != STRSXP || ALTREP(values) ||
        XLENGTH(values) != 1) {
      return FALSE;
    }
  }

  static const R_xlen_t singleton_order[] = {0};
  return attach_params_index(params, singleton_order, 1);
}

static void attach_tags_index(SEXP tags,
    R_xlen_t *work_since_interrupt) {
  SEXP values = VECTOR_ELT(tags, 1);
  const R_xlen_t size = XLENGTH(values);
  if (data_table_index_layout_state != DATA_TABLE_INDEX_LAYOUT_ENABLED ||
      size > INT_MAX ||
      !character_vector_is_ascii(values)) {
    return;
  }
  R_xlen_t *order = size == 0
    ? NULL
    : paradox_temporary_alloc(size, sizeof(*order));
  R_xlen_t *workspace = size == 0
    ? NULL
    : paradox_temporary_alloc(size, sizeof(*workspace));
  if (size != 0) {
    stable_character_order(
      values,
      order,
      workspace,
      size,
      work_since_interrupt
    );
  }

  R_xlen_t *starts = size == 0
    ? NULL
    : paradox_temporary_alloc(size, sizeof(*starts));
  R_xlen_t group_count = 0;
  R_xlen_t max_group = 0;
  for (R_xlen_t position = 0; position < size; ++position) {
    account_work(work_since_interrupt);
    if (position == 0 || strcmp(
        CHAR(STRING_ELT(values, order[position - 1])),
        CHAR(STRING_ELT(values, order[position]))
      ) != 0) {
      starts[group_count++] = position;
    }
  }
  for (R_xlen_t group = 0; group < group_count; ++group) {
    const R_xlen_t end = group + 1 == group_count
      ? size
      : starts[group + 1];
    const R_xlen_t group_size = end - starts[group];
    if (group_size > max_group) {
      max_group = group_size;
    }
  }
  attach_secondary_index(
    tags,
    "__tag",
    order,
    size,
    starts,
    group_count,
    max_group
  );
}

static SEXP character_vector(const char *const *values, R_xlen_t size) {
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    SET_STRING_ELT(result, index, Rf_mkChar(values[index]));
  }
  UNPROTECT(1);
  return result;
}

static SEXP set_data_table_attributes(SEXP table,
    const char *const *column_names, R_xlen_t column_count,
    R_xlen_t row_count, int include_row_names, int sorted_by_id) {
  SEXP names = PROTECT(character_vector(column_names, column_count));
  const char *const class_names[] = {"data.table", "data.frame"};
  SEXP classes = PROTECT(character_vector(class_names, 2));
  Rf_setAttrib(table, R_NamesSymbol, names);
  Rf_setAttrib(table, R_ClassSymbol, classes);

  int n_protected = 2;
  if (include_row_names) {
    SEXP row_names;
    if (row_count == 0) {
      row_names = PROTECT(Rf_allocVector(INTSXP, 0));
    } else {
      row_names = PROTECT(Rf_allocVector(INTSXP, 2));
      INTEGER(row_names)[0] = NA_INTEGER;
      INTEGER(row_names)[1] = -(int) row_count;
    }
    ++n_protected;
    Rf_setAttrib(table, R_RowNamesSymbol, row_names);
  }

  if (sorted_by_id) {
    SEXP sorted = PROTECT(Rf_mkString("id"));
    ++n_protected;
    Rf_setAttrib(table, Rf_install("sorted"), sorted);
  }
  SEXP result = PROTECT(paradox_prepare_data_table(table, FALSE));
  UNPROTECT(n_protected + 1);
  return result;
}

static SEXP new_table(const char *const *column_names,
    const SEXPTYPE *column_types, R_xlen_t column_count,
    R_xlen_t row_count, int include_row_names, int sorted_by_id) {
  SEXP table = PROTECT(Rf_allocVector(VECSXP, column_count));
  for (R_xlen_t column = 0; column < column_count; ++column) {
    SEXP value = PROTECT(Rf_allocVector(column_types[column], row_count));
    SET_VECTOR_ELT(table, column, value);
    UNPROTECT(1);
  }
  SEXP result = PROTECT(set_data_table_attributes(
    table,
    column_names,
    column_count,
    row_count,
    include_row_names,
    sorted_by_id
  ));
  UNPROTECT(2);
  return result;
}

static void copy_permanent_value(SEXP destination, R_xlen_t row,
    SEXP source) {
  switch (TYPEOF(destination)) {
  case STRSXP:
    SET_STRING_ELT(destination, row, STRING_ELT(source, 0));
    return;
  case INTSXP:
    INTEGER(destination)[row] = INTEGER_ELT(source, 0);
    return;
  case REALSXP:
    if (TYPEOF(source) == REALSXP) {
      REAL(destination)[row] = REAL_ELT(source, 0);
    } else {
      const int value = INTEGER_ELT(source, 0);
      REAL(destination)[row] = value == NA_INTEGER
        ? NA_REAL
        : (double) value;
    }
    return;
  case VECSXP:
    SET_VECTOR_ELT(destination, row, VECTOR_ELT(source, 0));
    return;
  default:
    Rf_error("Internal error: unsupported permanent Domain column type");
  }
}

SEXP paradox_param_set_construct(SEXP domains) {
  if (TYPEOF(domains) != VECSXP) {
    return R_NilValue;
  }

  const R_xlen_t size = XLENGTH(domains);
  if (size > INT_MAX) {
    return R_NilValue;
  }

  SEXP ids = PROTECT(paradox_stored_attribute(domains, R_NamesSymbol));
  if (size == 0) {
    if (ids != R_NilValue &&
        (TYPEOF(ids) != STRSXP || ALTREP(ids) || XLENGTH(ids) != 0)) {
      UNPROTECT(1);
      return R_NilValue;
    }
  } else if (TYPEOF(ids) != STRSXP || ALTREP(ids) ||
      XLENGTH(ids) != size) {
    UNPROTECT(1);
    return R_NilValue;
  }

  SEXP stable_ids = PROTECT(Rf_allocVector(STRSXP, size));
  SEXP domain_snapshots = PROTECT(Rf_allocVector(VECSXP, size));

  R_xlen_t tag_count = 0;
  R_xlen_t trafo_count = 0;
  R_xlen_t init_count = 0;
  SEXPTYPE permanent_column_types[DOMAIN_TAGS];
  for (R_xlen_t column = 0; column < DOMAIN_TAGS; ++column) {
    permanent_column_types[column] = domain_column_types[column];
  }
  if (size > 0) {
    permanent_column_types[DOMAIN_LOWER] = INTSXP;
    permanent_column_types[DOMAIN_UPPER] = INTSXP;
    permanent_column_types[DOMAIN_TOLERANCE] = INTSXP;
  }

  for (R_xlen_t row = 0; row < size; ++row) {
    if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP id = STRING_ELT(ids, row);
    if (!string_is_strict_id(id)) {
      UNPROTECT(3);
      return R_NilValue;
    }
    SET_STRING_ELT(stable_ids, row, id);

    SEXP domain = PROTECT(VECTOR_ELT(domains, row));
    SEXP snapshot = PROTECT(snapshot_domain(domain));
    if (snapshot == R_NilValue) {
      UNPROTECT(5);
      return R_NilValue;
    }
    SET_VECTOR_ELT(domain_snapshots, row, snapshot);

    for (enum domain_column column = DOMAIN_LOWER;
        column <= DOMAIN_TOLERANCE;
        column = (enum domain_column) (column + 1)) {
      if (TYPEOF(VECTOR_ELT(snapshot, column)) == REALSXP) {
        permanent_column_types[column] = REALSXP;
      }
    }

    SEXP tags = VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_TAGS), 0);
    if (!checked_add(&tag_count, XLENGTH(tags))) {
      UNPROTECT(5);
      return R_NilValue;
    }
    if (VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_TRAFO), 0) != R_NilValue) {
      ++trafo_count;
    }
    if (LOGICAL_ELT(VECTOR_ELT(snapshot, DOMAIN_INIT_GIVEN), 0)) {
      ++init_count;
    }
    UNPROTECT(2);
  }
  ids = stable_ids;

  R_xlen_t *order = NULL;
  R_xlen_t *workspace = NULL;
  R_xlen_t work_since_interrupt = 0;
  if (size > 0) {
    order = paradox_temporary_alloc(size, sizeof(*order));
    workspace = paradox_temporary_alloc(size, sizeof(*workspace));
    stable_character_order(
      ids,
      order,
      workspace,
      size,
      &work_since_interrupt
    );
    for (R_xlen_t index = 1; index < size; ++index) {
      account_work(&work_since_interrupt);
      if (strcmp(
          CHAR(STRING_ELT(ids, order[index - 1])),
          CHAR(STRING_ELT(ids, order[index]))
        ) == 0) {
        UNPROTECT(3);
        return R_NilValue;
      }
    }
  }

  SEXP params = PROTECT(new_table(
    permanent_column_names,
    permanent_column_types,
    DOMAIN_TAGS,
    size,
    TRUE,
    FALSE
  ));
  for (R_xlen_t row = 0; row < size; ++row) {
    if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP domain = VECTOR_ELT(domain_snapshots, row);
    SET_STRING_ELT(VECTOR_ELT(params, DOMAIN_ID), row, STRING_ELT(ids, row));
    for (enum domain_column column = DOMAIN_CLS;
        column < DOMAIN_TAGS;
        column = (enum domain_column) (column + 1)) {
      copy_permanent_value(
        VECTOR_ELT(params, column),
        row,
        VECTOR_ELT(domain, column)
      );
    }
  }
  attach_params_index(params, order, size);

  const char *const tag_column_names[] = {"id", "tag"};
  const SEXPTYPE tag_column_types[] = {STRSXP, STRSXP};
  SEXP tags = PROTECT(new_table(
    tag_column_names,
    tag_column_types,
    2,
    tag_count,
    FALSE,
    TRUE
  ));
  R_xlen_t tag_row = 0;
  for (R_xlen_t position = 0; position < size; ++position) {
    if (position != 0 &&
        position % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    R_xlen_t row = order[position];
    SEXP row_tags = VECTOR_ELT(
      VECTOR_ELT(VECTOR_ELT(domain_snapshots, row), DOMAIN_TAGS),
      0
    );
    for (R_xlen_t tag = 0; tag < XLENGTH(row_tags); ++tag) {
      if (tag_row != 0 &&
          tag_row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
        R_CheckUserInterrupt();
      }
      if (tag_row >= tag_count) {
        Rf_error("Internal error: ParamSet tag output exceeded its capacity");
      }
      SET_STRING_ELT(VECTOR_ELT(tags, 0), tag_row, STRING_ELT(ids, row));
      SET_STRING_ELT(VECTOR_ELT(tags, 1), tag_row, STRING_ELT(row_tags, tag));
      ++tag_row;
    }
  }
  if (tag_row != tag_count) {
    Rf_error("Internal error: incomplete ParamSet tag output");
  }
  attach_tags_index(tags, &work_since_interrupt);

  const char *const trafo_column_names[] = {"id", "trafo"};
  const SEXPTYPE trafo_column_types[] = {STRSXP, VECSXP};
  SEXP trafos = PROTECT(new_table(
    trafo_column_names,
    trafo_column_types,
    2,
    trafo_count,
    FALSE,
    TRUE
  ));
  R_xlen_t trafo_row = 0;
  for (R_xlen_t position = 0; position < size; ++position) {
    if (position != 0 &&
        position % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    const R_xlen_t row = order[position];
    SEXP trafo = VECTOR_ELT(
      VECTOR_ELT(VECTOR_ELT(domain_snapshots, row), DOMAIN_TRAFO),
      0
    );
    if (trafo != R_NilValue) {
      if (trafo_row >= trafo_count) {
        Rf_error("Internal error: ParamSet trafo output exceeded its capacity");
      }
      SET_STRING_ELT(
        VECTOR_ELT(trafos, 0),
        trafo_row,
        STRING_ELT(ids, row)
      );
      SET_VECTOR_ELT(VECTOR_ELT(trafos, 1), trafo_row, trafo);
      ++trafo_row;
    }
  }
  if (trafo_row != trafo_count) {
    Rf_error("Internal error: incomplete ParamSet trafo output");
  }

  SEXP requirements = PROTECT(Rf_allocVector(VECSXP, size));
  for (R_xlen_t row = 0; row < size; ++row) {
    if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SET_VECTOR_ELT(
      requirements,
      row,
      VECTOR_ELT(
        VECTOR_ELT(VECTOR_ELT(domain_snapshots, row), DOMAIN_REQUIREMENTS),
        0
      )
    );
  }

  SEXP init_values = PROTECT(Rf_allocVector(VECSXP, init_count));
  SEXP init_names = PROTECT(Rf_allocVector(STRSXP, init_count));
  R_xlen_t init_row = 0;
  for (R_xlen_t row = 0; row < size; ++row) {
    if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP domain = VECTOR_ELT(domain_snapshots, row);
    if (LOGICAL_ELT(VECTOR_ELT(domain, DOMAIN_INIT_GIVEN), 0)) {
      if (init_row >= init_count) {
        Rf_error("Internal error: ParamSet init output exceeded its capacity");
      }
      SET_VECTOR_ELT(
        init_values,
        init_row,
        VECTOR_ELT(VECTOR_ELT(domain, DOMAIN_INIT), 0)
      );
      SET_STRING_ELT(init_names, init_row, STRING_ELT(ids, row));
      ++init_row;
    }
  }
  if (init_row != init_count) {
    Rf_error("Internal error: incomplete ParamSet init output");
  }
  Rf_setAttrib(init_values, R_NamesSymbol, init_names);

  SEXP result = PROTECT(Rf_allocVector(VECSXP, RESULT_COUNT));
  SET_VECTOR_ELT(result, RESULT_PARAMS, params);
  SET_VECTOR_ELT(result, RESULT_TAGS, tags);
  SET_VECTOR_ELT(result, RESULT_TRAFOS, trafos);
  SET_VECTOR_ELT(result, RESULT_REQUIREMENTS, requirements);
  SET_VECTOR_ELT(result, RESULT_INIT_VALUES, init_values);
  SEXP names = PROTECT(character_vector(result_names, RESULT_COUNT));
  Rf_setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(11);
  return result;
}
