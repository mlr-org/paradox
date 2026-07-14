#include <limits.h>
#include <stddef.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Utils.h>

#include "r_utils.h"

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

static int id_precedes(SEXP ids, R_xlen_t left, R_xlen_t right) {
  return strcmp(
    CHAR(STRING_ELT(ids, left)),
    CHAR(STRING_ELT(ids, right))
  ) <= 0;
}

static void stable_id_order(SEXP ids, R_xlen_t *order, R_xlen_t *workspace,
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
        workspace[output++] = id_precedes(ids, order[left], order[right])
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

static SEXP character_vector(const char *const *values, R_xlen_t size) {
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    SET_STRING_ELT(result, index, Rf_mkChar(values[index]));
  }
  UNPROTECT(1);
  return result;
}

static void set_data_table_attributes(SEXP table,
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
  paradox_set_data_table_selfref(table);
  UNPROTECT(n_protected);
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
  set_data_table_attributes(
    table,
    column_names,
    column_count,
    row_count,
    include_row_names,
    sorted_by_id
  );
  UNPROTECT(1);
  return table;
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
  if (size > 0) {
    R_xlen_t work_since_interrupt = 0;
    order = paradox_temporary_alloc(size, sizeof(*order));
    workspace = paradox_temporary_alloc(size, sizeof(*workspace));
    stable_id_order(ids, order, workspace, size, &work_since_interrupt);
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
