#include <limits.h>
#include <string.h>

#include "paramset_domain_common.h"
#include <R_ext/Utils.h>

#include "builtin_condition.h"
#include "paramset_params_internal.h"
#include "r_api_compat.h"
#include "r_utils.h"
#include "core_state.h"

typedef enum {
  DOMAIN_KIND_UNKNOWN = 0,
  DOMAIN_KIND_DBL,
  DOMAIN_KIND_INT,
  DOMAIN_KIND_FCT,
  DOMAIN_KIND_LGL,
  DOMAIN_KIND_UTY
} domain_kind_t;

const char *const paradox_domain_column_names[PARADOX_DOMAIN_COLUMN_COUNT] = {
  "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
  "levels", "special_vals", "default", "storage_type", ".tags",
  ".trafo", ".requirements", ".init_given", ".init"
};

static const SEXPTYPE permanent_column_types[PARADOX_DOMAIN_TAGS] = {
  STRSXP, STRSXP, STRSXP, VECSXP, REALSXP, REALSXP, REALSXP, VECSXP,
  VECSXP, VECSXP, STRSXP
};

int paradox_domain_string_is(SEXP string, const char *expected) {
  return string != NA_STRING && strcmp(CHAR(string), expected) == 0;
}

/* Interned CHARSXPs for the sixteen canonical column names. R's global
 * CHARSXP cache stores one object per exact string, so a canonically built
 * table name is pointer-identical to its entry here; the byte comparison in
 * the selector below only runs for names this table does not know. */
static SEXP interned_domain_column_names[PARADOX_DOMAIN_COLUMN_COUNT];

void paradox_domain_intern_column_names(void) {
  for (int column = 0; column < PARADOX_DOMAIN_COLUMN_COUNT; ++column) {
    SEXP name = Rf_mkChar(paradox_domain_column_names[column]);
    R_PreserveObject(name);
    interned_domain_column_names[column] = name;
  }
}

void paradox_domain_select_columns(SEXP table, const char *corrupt_context,
    const char *storage_name, unsigned int required_mask, SEXP *columns) {
  if (TYPEOF(table) != VECSXP || Rf_isS4(table)) {
    Rf_error("Corrupt %s: `%s` must be a list", corrupt_context, storage_name);
  }
  if (ALTREP(table)) {
    Rf_error(
      "Corrupt %s: `%s` must use an ordinary list representation",
      corrupt_context,
      storage_name
    );
  }

  SEXP names = PROTECT(Rf_getAttrib(table, R_NamesSymbol));
  const R_xlen_t n_columns = XLENGTH(table);
  if (TYPEOF(names) != STRSXP || Rf_isS4(names) ||
      Rf_isObject(names) || !paradox_api_has_no_attributes(names)) {
    Rf_error(
      "Corrupt %s: `%s` must be a named list",
      corrupt_context,
      storage_name
    );
  }
  if (ALTREP(names)) {
    Rf_error(
      "Corrupt %s: `%s` names must use an ordinary character representation",
      corrupt_context,
      storage_name
    );
  }
  if (XLENGTH(names) != n_columns) {
    Rf_error(
      "Corrupt %s: `%s` must be a named list",
      corrupt_context,
      storage_name
    );
  }

  int counts[PARADOX_DOMAIN_COLUMN_COUNT] = {0};
  for (int target = 0; target < PARADOX_DOMAIN_COLUMN_COUNT; ++target) {
    columns[target] = R_NilValue;
  }
  for (R_xlen_t index = 0; index < n_columns; ++index) {
    SEXP name = STRING_ELT(names, index);
    if (name == NA_STRING) {
      continue;
    }
    int matched = -1;
    for (int target = 0; target < PARADOX_DOMAIN_COLUMN_COUNT; ++target) {
      if (name == interned_domain_column_names[target]) {
        matched = target;
        break;
      }
    }
    if (matched < 0) {
      /* A name that is not the interned object may still spell a canonical
       * name in a foreign representation; bytes decide, exactly as the
       * single-column selector always has. */
      for (int target = 0; target < PARADOX_DOMAIN_COLUMN_COUNT; ++target) {
        if (strcmp(CHAR(name), paradox_domain_column_names[target]) == 0) {
          matched = target;
          break;
        }
      }
    }
    if (matched < 0 || !((required_mask >> matched) & 1U)) {
      continue;
    }
    columns[matched] = VECTOR_ELT(table, index);
    ++counts[matched];
  }
  for (int target = 0; target < PARADOX_DOMAIN_COLUMN_COUNT; ++target) {
    if (!((required_mask >> target) & 1U) || counts[target] == 1) {
      continue;
    }
    if (counts[target] > 1) {
      Rf_error(
        "Corrupt %s: `%s` has more than one `%s` column",
        corrupt_context,
        storage_name,
        paradox_domain_column_names[target]
      );
    }
    Rf_error(
      "Corrupt %s: `%s` has no `%s` column",
      corrupt_context,
      storage_name,
      paradox_domain_column_names[target]
    );
  }
  UNPROTECT(1);
}

SEXP paradox_domain_character_vector(const char *const *values,
    R_xlen_t size) {
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    SET_STRING_ELT(result, index, Rf_mkChar(values[index]));
  }
  UNPROTECT(1);
  return result;
}

SEXP paradox_domain_finish_plain_table(SEXP table,
    const char *const *column_names, R_xlen_t column_count,
    R_xlen_t row_count) {
  if (row_count > INT_MAX) {
    Rf_error("ParamSet state table exceeds the supported row count");
  }
  PROTECT(table);
  SEXP names = PROTECT(paradox_domain_character_vector(
    column_names,
    column_count
  ));
  Rf_setAttrib(table, R_NamesSymbol, names);
  SEXP classes = PROTECT(Rf_mkString("data.frame"));
  Rf_setAttrib(table, R_ClassSymbol, classes);
  SEXP row_names = PROTECT(Rf_allocVector(
    INTSXP,
    row_count == 0 ? 0 : 2
  ));
  if (row_count != 0) {
    SET_INTEGER_ELT(row_names, 0, NA_INTEGER);
    /* Negate in R_xlen_t after the INT_MAX bound above, then narrow the
     * representable result.  This also makes the overflow proof visible to
     * conservative integer-flow analyzers. */
    SET_INTEGER_ELT(row_names, 1, (int) (-row_count));
  }
  Rf_setAttrib(table, R_RowNamesSymbol, row_names);
  UNPROTECT(4);
  return table;
}

SEXP paradox_domain_new_plain_table(const char *const *column_names,
    const SEXPTYPE *column_types, R_xlen_t column_count,
    R_xlen_t row_count) {
  SEXP table = PROTECT(Rf_allocVector(VECSXP, column_count));
  for (R_xlen_t column = 0; column < column_count; ++column) {
    SEXP values = PROTECT(Rf_allocVector(
      column_types[column],
      row_count
    ));
    SET_VECTOR_ELT(table, column, values);
    UNPROTECT(1);
  }
  SEXP result = PROTECT(paradox_domain_finish_plain_table(
    table,
    column_names,
    column_count,
    row_count
  ));
  UNPROTECT(2);
  return result;
}

static int has_no_attributes(SEXP value) {
  return paradox_api_has_no_attributes(value);
}

static char *copy_utf8_string(SEXP string) {
  return paradox_temporary_utf8_copy(string, NULL);
}

static int native_ascii_strings_equal(SEXP left, SEXP right, int *known) {
  const unsigned char *left_text = (const unsigned char *) CHAR(left);
  const unsigned char *right_text = (const unsigned char *) CHAR(right);
  for (;;) {
    const unsigned char left_byte = *left_text;
    const unsigned char right_byte = *right_text;
    if ((left_byte | right_byte) >= 0x80U) {
      *known = FALSE;
      return FALSE;
    }
    if (left_byte != right_byte) {
      *known = TRUE;
      return FALSE;
    }
    if (left_byte == '\0') {
      *known = TRUE;
      return TRUE;
    }
    ++left_text;
    ++right_text;
  }
}

int paradox_domain_strings_equal(SEXP left, SEXP right) {
  if (left == right) {
    return TRUE;
  }
  if (left == NA_STRING || right == NA_STRING) {
    return FALSE;
  }

  const cetype_t left_encoding = Rf_getCharCE(left);
  const cetype_t right_encoding = Rf_getCharCE(right);
  if (left_encoding == right_encoding) {
    if (left_encoding == CE_UTF8 || left_encoding == CE_LATIN1 ||
        left_encoding == CE_BYTES) {
      return strcmp(CHAR(left), CHAR(right)) == 0;
    }
    if (left_encoding == CE_NATIVE) {
      int known = FALSE;
      const int equal = native_ascii_strings_equal(left, right, &known);
      if (known) {
        return equal;
      }
    }
  }
  if (left_encoding == CE_BYTES || right_encoding == CE_BYTES) {
    return FALSE;
  }

  PROTECT(left);
  PROTECT(right);
  const void *vmax = vmaxget();
  const char *left_text = copy_utf8_string(left);
  const char *right_text = Rf_translateCharUTF8(right);
  const int equal = strcmp(left_text, right_text) == 0;
  vmaxset(vmax);
  UNPROTECT(2);
  return equal;
}

R_xlen_t paradox_domain_find_string(SEXP strings, SEXP sought,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t count = XLENGTH(strings);
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP candidate = STRING_ELT(strings, index);
    if (candidate == sought ||
        paradox_domain_strings_equal(candidate, sought)) {
      return index;
    }
  }
  return R_XLEN_T_MAX;
}

SEXP paradox_domain_apply_tag_override(SEXP derived, SEXP override,
    R_xlen_t *work_since_interrupt) {
  if (override == R_NilValue) {
    return derived;
  }
  PROTECT(derived);
  PROTECT(override);
  SEXP governed = VECTOR_ELT(override, PARADOX_TAG_OVERRIDE_IDS);
  SEXP asserted = VECTOR_ELT(override, PARADOX_TAG_OVERRIDE_TAGS);
  SEXP derived_ids = VECTOR_ELT(derived, 0);
  SEXP derived_tags = VECTOR_ELT(derived, 1);
  SEXP asserted_ids = VECTOR_ELT(asserted, 0);
  SEXP asserted_tags = VECTOR_ELT(asserted, 1);
  const R_xlen_t derived_rows = XLENGTH(derived_ids);
  const R_xlen_t asserted_rows = XLENGTH(asserted_ids);

  R_xlen_t kept = 0;
  for (R_xlen_t row = 0; row < derived_rows; ++row) {
    kept += !paradox_domain_string_in(
      governed,
      STRING_ELT(derived_ids, row),
      work_since_interrupt
    );
  }
  if (kept == derived_rows && asserted_rows == 0) {
    UNPROTECT(2);
    return derived;
  }
  if (kept > R_XLEN_T_MAX - asserted_rows) {
    UNPROTECT(2);
    Rf_error("ParamSet tag table exceeds the supported row count");
  }

  static const char *const columns[] = {"id", "tag"};
  static const SEXPTYPE types[] = {STRSXP, STRSXP};
  SEXP result = PROTECT(paradox_domain_new_plain_table(
    columns,
    types,
    2,
    kept + asserted_rows
  ));
  SEXP result_ids = VECTOR_ELT(result, 0);
  SEXP result_tags = VECTOR_ELT(result, 1);
  R_xlen_t output = 0;
  for (R_xlen_t row = 0; row < derived_rows; ++row) {
    paradox_account_work(work_since_interrupt);
    if (paradox_domain_string_in(
        governed,
        STRING_ELT(derived_ids, row),
        work_since_interrupt
      )) {
      continue;
    }
    SET_STRING_ELT(result_ids, output, STRING_ELT(derived_ids, row));
    SET_STRING_ELT(result_tags, output, STRING_ELT(derived_tags, row));
    ++output;
  }
  for (R_xlen_t row = 0; row < asserted_rows; ++row) {
    paradox_account_work(work_since_interrupt);
    SET_STRING_ELT(result_ids, output, STRING_ELT(asserted_ids, row));
    SET_STRING_ELT(result_tags, output, STRING_ELT(asserted_tags, row));
    ++output;
  }
  UNPROTECT(3);
  return result;
}

int paradox_domain_string_in(SEXP strings, SEXP sought,
    R_xlen_t *work_since_interrupt) {
  return paradox_domain_find_string(
    strings,
    sought,
    work_since_interrupt
  ) != R_XLEN_T_MAX;
}

int paradox_domain_exact_string_vector(SEXP value,
    const char *const *expected, R_xlen_t size,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(value) != STRSXP || ALTREP(value) || Rf_isS4(value) ||
      Rf_isObject(value) || !has_no_attributes(value)) {
    return FALSE;
  }
  const R_xlen_t observed_size = XLENGTH(value);
  if (observed_size != size) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_account_work(work_since_interrupt);
    if (!paradox_domain_string_is(STRING_ELT(value, index), expected[index])) {
      return FALSE;
    }
  }
  return TRUE;
}

int paradox_domain_exact_plain_table(SEXP table,
    const char *const *column_names, R_xlen_t column_count,
    R_xlen_t *row_count, R_xlen_t *work_since_interrupt) {
  static const char *const table_classes[] = {"data.frame"};
  static const char *const allowed_attributes[] = {
    "names", "class", "row.names"
  };
  if (TYPEOF(table) != VECSXP || ALTREP(table) || Rf_isS4(table)) {
    return FALSE;
  }
  PROTECT(table);
  const R_xlen_t observed_column_count = XLENGTH(table);
  SEXP names = PROTECT(Rf_getAttrib(table, R_NamesSymbol));
  SEXP classes = PROTECT(Rf_getAttrib(table, R_ClassSymbol));
  SEXP row_names = PROTECT(paradox_api_raw_attribute(
    table,
    R_RowNamesSymbol
  ));
  R_xlen_t observed_row_count = 0;
  int valid = observed_column_count == column_count &&
    !Rf_isS4(names) && !Rf_isObject(names) &&
    !Rf_isS4(classes) && !Rf_isObject(classes) &&
    !Rf_isS4(row_names) && !Rf_isObject(row_names);
  if (valid && column_count != 0) {
    /*
     * Column zero defines the row count, but it is subject to the same strict
     * representation contract as every later column.  Reject it before
     * observing XLENGTH: an ALTREP Length method may execute arbitrary R.
     */
    SEXP first = VECTOR_ELT(table, 0);
    valid = !ALTREP(first) && !Rf_isS4(first) && !Rf_isObject(first);
    if (valid) observed_row_count = XLENGTH(first);
  }
  valid = valid &&
    paradox_api_has_only_attributes(table, allowed_attributes, 3) &&
    has_no_attributes(names) &&
    has_no_attributes(classes) &&
    paradox_domain_exact_string_vector(
      names,
      column_names,
      column_count,
      work_since_interrupt
    ) &&
    paradox_domain_exact_string_vector(
      classes,
      table_classes,
      1,
      work_since_interrupt
    ) && TYPEOF(row_names) == INTSXP && !ALTREP(row_names) &&
    paradox_api_has_no_attributes(row_names);
  if (valid) {
    /*
     * Read the stored attribute above, not Rf_getAttrib()'s expanded `1:n`
     * view.  Existing package producers use both exact ordinary `1:n` row
     * names and R's compact c(NA, +/-n) representation.  Reject a
     * callback-capable row-name ALTREP before observing either Length or Elt;
     * compact forms then remain constant-time while the explicit form pays
     * the same exact validation it did before this raw-attribute reader.
     */
    const R_xlen_t row_name_count = XLENGTH(row_names);
    if (observed_row_count == 0) {
      valid = row_name_count == 0;
    } else if (observed_row_count > INT_MAX) {
      valid = FALSE;
    } else if (row_name_count == 2 &&
        INTEGER_ELT(row_names, 0) == NA_INTEGER) {
      const int encoded = INTEGER_ELT(row_names, 1);
      const int expected = (int) observed_row_count;
      valid = encoded == expected || encoded == -expected;
    } else if (row_name_count == observed_row_count) {
      for (R_xlen_t row = 0; valid && row < observed_row_count; ++row) {
        paradox_account_work(work_since_interrupt);
        valid = INTEGER_ELT(row_names, row) == (int) row + 1;
      }
    } else {
      valid = FALSE;
    }
  }
  /* An exact plain capsule table is rectangular and ordinary. Column zero
   * defined the row count above; admitting a table whose remaining columns
   * are shorter would let every consumer index them out of bounds. */
  for (R_xlen_t column = 1; valid && column < column_count; ++column) {
    SEXP child = VECTOR_ELT(table, column);
    if (ALTREP(child) || Rf_isS4(child) || Rf_isObject(child) ||
        XLENGTH(child) != observed_row_count) {
      valid = FALSE;
    }
  }
  if (valid && row_count != NULL) {
    *row_count = observed_row_count;
  }
  UNPROTECT(4);
  return valid;
}

SEXP paradox_domain_plain_table_snapshot(SEXP source,
    const char *const *column_names, R_xlen_t column_count) {
  if (column_count < 0 || TYPEOF(source) != VECSXP || ALTREP(source) ||
      Rf_isS4(source) || Rf_isObject(source) ||
      XLENGTH(source) != column_count) {
    return R_NilValue;
  }
  PROTECT(source);
  R_xlen_t row_count = 0;
  if (column_count != 0) {
    if (ALTREP(VECTOR_ELT(source, 0)) ||
        Rf_isS4(VECTOR_ELT(source, 0)) ||
        Rf_isObject(VECTOR_ELT(source, 0))) {
      UNPROTECT(1);
      return R_NilValue;
    }
    row_count = XLENGTH(VECTOR_ELT(source, 0));
  }
  if (row_count > INT_MAX) {
    UNPROTECT(1);
    return R_NilValue;
  }
  for (R_xlen_t column = 1; column < column_count; ++column) {
    SEXP child = VECTOR_ELT(source, column);
    if (ALTREP(child) || Rf_isS4(child) || Rf_isObject(child) ||
        XLENGTH(child) != row_count) {
      UNPROTECT(1);
      return R_NilValue;
    }
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, column_count));
  for (R_xlen_t column = 0; column < column_count; ++column) {
    SET_VECTOR_ELT(result, column, VECTOR_ELT(source, column));
  }
  SEXP names = PROTECT(Rf_allocVector(STRSXP, column_count));
  for (R_xlen_t column = 0; column < column_count; ++column) {
    SET_STRING_ELT(names, column, Rf_mkChar(column_names[column]));
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(classes, 0, Rf_mkChar("data.frame"));
  Rf_setAttrib(result, R_ClassSymbol, classes);
  SEXP row_names = PROTECT(Rf_allocVector(
    INTSXP,
    row_count == 0 ? 0 : 2
  ));
  if (row_count != 0) {
    SET_INTEGER_ELT(row_names, 0, NA_INTEGER);
    SET_INTEGER_ELT(row_names, 1, -(int) row_count);
  }
  Rf_setAttrib(result, R_RowNamesSymbol, row_names);
  UNPROTECT(5);
  return result;
}

SEXP paradox_domain_local_value(SEXP environment, const char *name) {
  if (TYPEOF(environment) != ENVSXP || Rf_isS4(environment)) {
    return R_UnboundValue;
  }
  /* Current state has exactly one authority. Retired private-table bindings
   * are neither inspected nor used as a compatibility path. */
  return paradox_core_local_value(environment, name);
}

int paradox_domain_owns_private_environment(SEXP self,
    SEXP private_environment) {
  if (TYPEOF(self) != ENVSXP || Rf_isS4(self) ||
      TYPEOF(private_environment) != ENVSXP ||
      Rf_isS4(private_environment)) {
    return FALSE;
  }
#if R_VERSION < R_Version(4, 2, 0)
  /*
   * Old R's required ordinary-binding boundary has no allocation-free
   * existence query and deliberately assumes an admitted package shell.
   * Reject a plain malformed environment by its constant-time object bit
   * before that reader; current runtimes keep their existing hot path.
   */
  if (!Rf_isObject(self)) {
    return FALSE;
  }
#endif
  /* Current package-generated shells are authorized by their sealed capsule,
   * not by replaying R6's generated closure/private-environment topology.
   * Registered entry points receive `self` and `private` from thin package
   * wrappers; direct calls with a different environment remain unsupported,
   * while every payload field is still structurally validated before use. */
  SEXP owned_private = PROTECT(
    paradox_domain_required_private_environment(self)
  );
  const int owns = owned_private != R_UnboundValue &&
    owned_private == private_environment;
  UNPROTECT(1);
  return owns;
}

/*
 * Generate the required operation reader and the absence-tolerant candidate
 * reader from one source template.  This is deliberately compile-time rather
 * than a function-pointer or run-time-flag abstraction: on R < 4.2 the
 * optional reader must call base::exists(), while a known package shell must
 * stay on the allocation-free required-binding API.  The two readers otherwise
 * retain exactly the same structural admission.
 */
#define PARADOX_DEFINE_PRIVATE_ENVIRONMENT_READER( \
    name, binding_snapshot, core_snapshot) \
  static SEXP name(SEXP self) { \
    if (TYPEOF(self) != ENVSXP || Rf_isS4(self)) { \
      return R_UnboundValue; \
    } \
    SEXP enclosure_symbol = Rf_install(".__enclos_env__"); \
    SEXP enclosure = PROTECT(binding_snapshot(self, enclosure_symbol)); \
    if (TYPEOF(enclosure) != ENVSXP || Rf_isS4(enclosure)) { \
      UNPROTECT(1); \
      return R_UnboundValue; \
    } \
    SEXP private_symbol = Rf_install("private"); \
    SEXP private_environment = PROTECT( \
      binding_snapshot(enclosure, private_symbol) \
    ); \
    SEXP result = TYPEOF(private_environment) == ENVSXP && \
        !Rf_isS4(private_environment) && \
        core_snapshot(private_environment) != R_UnboundValue \
      ? private_environment \
      : R_UnboundValue; \
    UNPROTECT(2); \
    return result; \
  }

PARADOX_DEFINE_PRIVATE_ENVIRONMENT_READER(
  required_private_environment,
  paradox_api_plain_binding_snapshot,
  paradox_core_from_private
)

PARADOX_DEFINE_PRIVATE_ENVIRONMENT_READER(
  optional_private_environment,
  paradox_api_optional_plain_binding_snapshot,
  paradox_core_from_private_optional
)

#undef PARADOX_DEFINE_PRIVATE_ENVIRONMENT_READER

SEXP paradox_domain_required_private_environment(SEXP self) {
  return required_private_environment(self);
}

SEXP paradox_domain_private_environment(SEXP self) {
  return optional_private_environment(self);
}

static domain_kind_t domain_kind(SEXP cls, SEXP storage_type) {
  if (paradox_domain_string_is(cls, "ParamDbl") &&
      paradox_domain_string_is(storage_type, "numeric")) {
    return DOMAIN_KIND_DBL;
  }
  if (paradox_domain_string_is(cls, "ParamInt") &&
      paradox_domain_string_is(storage_type, "integer")) {
    return DOMAIN_KIND_INT;
  }
  if (paradox_domain_string_is(cls, "ParamFct") &&
      paradox_domain_string_is(storage_type, "character")) {
    return DOMAIN_KIND_FCT;
  }
  if (paradox_domain_string_is(cls, "ParamLgl") &&
      paradox_domain_string_is(storage_type, "logical")) {
    return DOMAIN_KIND_LGL;
  }
  if (paradox_domain_string_is(cls, "ParamUty") &&
      paradox_domain_string_is(storage_type, "list")) {
    return DOMAIN_KIND_UTY;
  }
  return DOMAIN_KIND_UNKNOWN;
}

static int canonical_levels(domain_kind_t kind, SEXP levels,
    R_xlen_t *work_since_interrupt) {
  switch (kind) {
  case DOMAIN_KIND_DBL:
  case DOMAIN_KIND_INT:
  case DOMAIN_KIND_UTY:
    return levels == R_NilValue;
  case DOMAIN_KIND_LGL:
    if (TYPEOF(levels) != LGLSXP || ALTREP(levels) ||
        Rf_isS4(levels) || Rf_isObject(levels) ||
        !has_no_attributes(levels)) {
      return FALSE;
    }
    return XLENGTH(levels) == 2 && LOGICAL_ELT(levels, 0) == TRUE &&
      LOGICAL_ELT(levels, 1) == FALSE;
  case DOMAIN_KIND_FCT:
    if (TYPEOF(levels) != STRSXP || ALTREP(levels) ||
        Rf_isS4(levels) || Rf_isObject(levels) ||
        !has_no_attributes(levels)) {
      return FALSE;
    }
    const R_xlen_t level_count = XLENGTH(levels);
    for (R_xlen_t level = 0; level < level_count; ++level) {
      paradox_account_work(work_since_interrupt);
      if (STRING_ELT(levels, level) == NA_STRING) {
        return FALSE;
      }
    }
    return Rf_any_duplicated(levels, FALSE) == 0;
  case DOMAIN_KIND_UNKNOWN:
    return FALSE;
  }
  return FALSE;
}

/* Levels/special-values validation for one parameter row; shared by the
 * all-rows and selected-row admission branches. */
static int validate_param_row(domain_kind_t kind, SEXP levels,
    SEXP special_values, R_xlen_t row, R_xlen_t *work_since_interrupt) {
  SEXP row_levels = PROTECT(VECTOR_ELT(levels, row));
  SEXP row_special_values = PROTECT(VECTOR_ELT(special_values, row));
  SEXP special_classes = PROTECT(Rf_getAttrib(
    row_special_values,
    R_ClassSymbol
  ));
  const int valid = canonical_levels(
      kind,
      row_levels,
      work_since_interrupt
    ) && TYPEOF(row_special_values) == VECSXP &&
      !ALTREP(row_special_values) && !Rf_isS4(row_special_values) &&
      special_classes == R_NilValue;
  UNPROTECT(3);
  return valid;
}

static int validate_params_rooted(SEXP params, SEXP selected_id,
    int validate_all_rows, paradox_domain_params_t *result,
    R_xlen_t *selected_row, R_xlen_t *work_since_interrupt, SEXP roots) {
  SEXP ids = VECTOR_ELT(roots, PARADOX_DOMAIN_ID);
  if (TYPEOF(ids) != STRSXP || ALTREP(ids) || Rf_isS4(ids) ||
      Rf_isObject(ids) || !has_no_attributes(ids)) {
    return FALSE;
  }
  const R_xlen_t row_count = XLENGTH(ids);
  for (enum paradox_domain_column column = PARADOX_DOMAIN_CLS;
      column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    paradox_account_work(work_since_interrupt);
    SEXP value = VECTOR_ELT(roots, column);
    const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
    const int numeric = column == PARADOX_DOMAIN_LOWER ||
      column == PARADOX_DOMAIN_UPPER ||
      column == PARADOX_DOMAIN_TOLERANCE;
    if (ALTREP(value) || Rf_isS4(value) || Rf_isObject(value) || (numeric
          ? type != REALSXP && type != INTSXP
          : type != permanent_column_types[column]) ||
        !has_no_attributes(value) || XLENGTH(value) != row_count) {
      return FALSE;
    }
  }

  SEXP classes = VECTOR_ELT(roots, PARADOX_DOMAIN_CLS);
  SEXP grouping = VECTOR_ELT(roots, PARADOX_DOMAIN_GROUPING);
  SEXP storage_types = VECTOR_ELT(roots, PARADOX_DOMAIN_STORAGE_TYPE);
  R_xlen_t match_count = 0;
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    if (STRING_ELT(ids, row) == NA_STRING ||
        STRING_ELT(grouping, row) == NA_STRING ||
        domain_kind(
          STRING_ELT(classes, row),
          STRING_ELT(storage_types, row)
        ) == DOMAIN_KIND_UNKNOWN) {
      return FALSE;
    }
    if (selected_id != R_NilValue && paradox_domain_strings_equal(
        STRING_ELT(ids, row),
        STRING_ELT(selected_id, 0)
      )) {
      *selected_row = row;
      ++match_count;
    }
  }
  if (Rf_any_duplicated(ids, FALSE) != 0 ||
      (selected_id != R_NilValue && match_count != 1)) {
    return FALSE;
  }

  SEXP levels = VECTOR_ELT(roots, PARADOX_DOMAIN_LEVELS);
  SEXP special_values = VECTOR_ELT(roots, PARADOX_DOMAIN_SPECIAL_VALS);
  if (validate_all_rows) {
    for (R_xlen_t row = 0; row < row_count; ++row) {
      paradox_account_work(work_since_interrupt);
      const domain_kind_t kind = domain_kind(
        STRING_ELT(classes, row),
        STRING_ELT(storage_types, row)
      );
      if (!validate_param_row(
          kind,
          levels,
          special_values,
          row,
          work_since_interrupt
        )) {
        return FALSE;
      }
    }
  } else {
    if (selected_id == R_NilValue) {
      return FALSE;
    }
    const domain_kind_t kind = domain_kind(
      STRING_ELT(classes, *selected_row),
      STRING_ELT(storage_types, *selected_row)
    );
    if (!validate_param_row(
        kind,
        levels,
        special_values,
        *selected_row,
        work_since_interrupt
      )) {
      return FALSE;
    }
  }

  result->table = params;
  result->ids = ids;
  result->classes = classes;
  result->row_count = row_count;
  return TRUE;
}

int paradox_domain_validate_params(SEXP params, SEXP selected_id,
    int validate_all_rows, paradox_domain_params_t *result,
    R_xlen_t *selected_row, R_xlen_t *work_since_interrupt) {
  /* Allocate the root plan before inspecting any table attribute. A GC
   * finalizer may replace a user-visible data.table column during allocation;
   * validation must consistently observe and retain the post-allocation
   * children rather than leave earlier borrowed children bare. */
  PROTECT(params);
  PROTECT(selected_id);
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, PARADOX_DOMAIN_TAGS));
  if (!paradox_domain_exact_plain_table(
      params,
      paradox_domain_column_names,
      PARADOX_DOMAIN_TAGS,
      NULL,
      work_since_interrupt
    ) || (selected_id != R_NilValue &&
      (TYPEOF(selected_id) != STRSXP || ALTREP(selected_id) ||
       Rf_isS4(selected_id) || Rf_isObject(selected_id) ||
       !has_no_attributes(selected_id) || XLENGTH(selected_id) != 1 ||
       STRING_ELT(selected_id, 0) == NA_STRING))) {
    UNPROTECT(3);
    return FALSE;
  }

  for (enum paradox_domain_column column = PARADOX_DOMAIN_ID;
      column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    SEXP child = PROTECT(VECTOR_ELT(params, column));
    SET_VECTOR_ELT(roots, column, child);
    UNPROTECT(1);
  }
  const int valid = validate_params_rooted(
    params,
    selected_id,
    validate_all_rows,
    result,
    selected_row,
    work_since_interrupt,
    roots
  );
  UNPROTECT(3);
  return valid;
}

int paradox_domain_validate_tags(SEXP tags, paradox_domain_tags_t *result,
    R_xlen_t *work_since_interrupt) {
  static const char *const column_names[] = {"id", "tag"};
  PROTECT(tags);
  if (!paradox_domain_exact_plain_table(
      tags, column_names, 2, NULL, work_since_interrupt
    )) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP ids = PROTECT(VECTOR_ELT(tags, 0));
  SEXP values = PROTECT(VECTOR_ELT(tags, 1));
  if (TYPEOF(ids) != STRSXP || TYPEOF(values) != STRSXP || ALTREP(ids) ||
      ALTREP(values) || Rf_isS4(ids) || Rf_isObject(ids) ||
      Rf_isS4(values) || Rf_isObject(values) || !has_no_attributes(ids) ||
      !has_no_attributes(values)) {
    UNPROTECT(3);
    return FALSE;
  }
  const R_xlen_t row_count = XLENGTH(ids);
  const R_xlen_t value_count = XLENGTH(values);
  if (value_count != row_count) {
    UNPROTECT(3);
    return FALSE;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    if (STRING_ELT(ids, row) == NA_STRING ||
        STRING_ELT(values, row) == NA_STRING) {
      UNPROTECT(3);
      return FALSE;
    }
  }
  result->ids = ids;
  result->values = values;
  result->row_count = row_count;
  UNPROTECT(3);
  return TRUE;
}

int paradox_domain_validate_trafos(SEXP trafos,
    paradox_domain_trafos_t *result,
    R_xlen_t *work_since_interrupt) {
  static const char *const column_names[] = {"id", "trafo"};
  PROTECT(trafos);
  if (!paradox_domain_exact_plain_table(
      trafos, column_names, 2, NULL, work_since_interrupt
    )) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP ids = PROTECT(VECTOR_ELT(trafos, 0));
  SEXP values = PROTECT(VECTOR_ELT(trafos, 1));
  if (TYPEOF(ids) != STRSXP || TYPEOF(values) != VECSXP || ALTREP(ids) ||
      ALTREP(values) || Rf_isS4(ids) || Rf_isObject(ids) ||
      Rf_isS4(values) || Rf_isObject(values) || !has_no_attributes(ids) ||
      !has_no_attributes(values)) {
    UNPROTECT(3);
    return FALSE;
  }
  const R_xlen_t row_count = XLENGTH(ids);
  const R_xlen_t value_count = XLENGTH(values);
  if (value_count != row_count) {
    UNPROTECT(3);
    return FALSE;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    if (STRING_ELT(ids, row) == NA_STRING ||
        !Rf_isFunction(VECTOR_ELT(values, row))) {
      UNPROTECT(3);
      return FALSE;
    }
  }
  result->ids = ids;
  result->values = values;
  result->row_count = row_count;
  UNPROTECT(3);
  return TRUE;
}

static int validate_dependencies(SEXP dependencies,
    paradox_domain_dependencies_t *result,
    SEXP **condition_rhs,
    R_xlen_t *work_since_interrupt) {
  static const char *const column_names[] = {"id", "on", "cond"};
  PROTECT(dependencies);
  if (!paradox_domain_exact_plain_table(
      dependencies,
      column_names,
      3,
      NULL,
      work_since_interrupt
    )) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP ids = PROTECT(VECTOR_ELT(dependencies, 0));
  SEXP on = PROTECT(VECTOR_ELT(dependencies, 1));
  SEXP conditions = PROTECT(VECTOR_ELT(dependencies, 2));
  if (TYPEOF(ids) != STRSXP || TYPEOF(on) != STRSXP ||
      TYPEOF(conditions) != VECSXP || ALTREP(ids) || ALTREP(on) ||
      ALTREP(conditions) || Rf_isS4(ids) || Rf_isObject(ids) ||
      Rf_isS4(on) || Rf_isObject(on) ||
      Rf_isS4(conditions) || Rf_isObject(conditions)) {
    UNPROTECT(4);
    return FALSE;
  }
  const R_xlen_t row_count = XLENGTH(ids);
  const R_xlen_t on_count = XLENGTH(on);
  const R_xlen_t condition_count = XLENGTH(conditions);
  if (!has_no_attributes(ids) ||
      !has_no_attributes(on) || !has_no_attributes(conditions) ||
      on_count != row_count || condition_count != row_count) {
    UNPROTECT(4);
    return FALSE;
  }
  SEXP *rhs_by_row = condition_rhs == NULL
    ? NULL
    : paradox_temporary_alloc(row_count, sizeof(*rhs_by_row));
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    SEXP condition = PROTECT(VECTOR_ELT(conditions, row));
    paradox_builtin_condition_kind_t kind;
    SEXP rhs = R_NilValue;
    if (STRING_ELT(ids, row) == NA_STRING ||
        Rf_getCharCE(STRING_ELT(ids, row)) == CE_BYTES ||
        STRING_ELT(on, row) == NA_STRING ||
        Rf_getCharCE(STRING_ELT(on, row)) == CE_BYTES ||
        !paradox_builtin_condition_exact(
          condition,
          &kind,
          &rhs,
          work_since_interrupt
        )) {
      UNPROTECT(5);
      return FALSE;
    }
    if (rhs_by_row != NULL) {
      rhs_by_row[row] = rhs;
    }
    (void) kind;
    UNPROTECT(1);
  }
  result->ids = ids;
  result->on = on;
  result->conditions = conditions;
  result->row_count = row_count;
  if (condition_rhs != NULL) {
    *condition_rhs = rhs_by_row;
  }
  UNPROTECT(4);
  return TRUE;
}

int paradox_domain_validate_dependencies(SEXP dependencies,
    paradox_domain_dependencies_t *result,
    R_xlen_t *work_since_interrupt) {
  return validate_dependencies(
    dependencies,
    result,
    NULL,
    work_since_interrupt
  );
}

int paradox_domain_validate_dependencies_with_rhs(SEXP dependencies,
    paradox_domain_dependencies_t *result,
    SEXP **condition_rhs,
    R_xlen_t *work_since_interrupt) {
  if (condition_rhs == NULL) {
    Rf_error("Internal error: missing dependency RHS output");
  }
  return validate_dependencies(
    dependencies,
    result,
    condition_rhs,
    work_since_interrupt
  );
}

int paradox_domain_validate_values(SEXP values,
    paradox_domain_values_t *result,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(values) != VECSXP || ALTREP(values) || Rf_isS4(values) ||
      Rf_isObject(values) ||
      !paradox_api_has_single_attribute(values, "names")) {
    return FALSE;
  }
  PROTECT(values);
  const R_xlen_t value_count = XLENGTH(values);
  SEXP names = PROTECT(Rf_getAttrib(values, R_NamesSymbol));
  if (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
      Rf_isObject(names) ||
      !has_no_attributes(names)) {
    UNPROTECT(2);
    return FALSE;
  }
  const R_xlen_t name_count = XLENGTH(names);
  if (name_count != value_count) {
    UNPROTECT(2);
    return FALSE;
  }
  for (R_xlen_t index = 0; index < name_count; ++index) {
    paradox_account_work(work_since_interrupt);
    if (STRING_ELT(names, index) == NA_STRING) {
      UNPROTECT(2);
      return FALSE;
    }
  }
  if (Rf_any_duplicated(names, FALSE) != 0) {
    UNPROTECT(2);
    return FALSE;
  }
  result->values = values;
  result->names = names;
  result->size = value_count;
  UNPROTECT(2);
  return TRUE;
}

static void set_scalar_column(SEXP result,
    enum paradox_domain_column output_column, SEXP source,
    R_xlen_t source_row) {
  PROTECT(source);
  SEXP column = PROTECT(Rf_allocVector((SEXPTYPE) TYPEOF(source), 1));
  switch (TYPEOF(source)) {
  case STRSXP:
    SET_STRING_ELT(column, 0, STRING_ELT(source, source_row));
    break;
  case VECSXP:
    SET_VECTOR_ELT(column, 0, VECTOR_ELT(source, source_row));
    break;
  case REALSXP:
    SET_REAL_ELT(column, 0, REAL_ELT(source, source_row));
    break;
  case INTSXP:
    SET_INTEGER_ELT(column, 0, INTEGER_ELT(source, source_row));
    break;
  default:
    Rf_error("Internal error: unsupported ParamSet Domain column type");
  }
  SET_VECTOR_ELT(result, output_column, column);
  UNPROTECT(2);
}

SEXP paradox_domain_prepare_facade(SEXP result, SEXP cls,
    R_xlen_t *work_since_interrupt) {
  PROTECT(cls);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, PARADOX_DOMAIN_COLUMN_COUNT));
  for (R_xlen_t column = 0;
      column < PARADOX_DOMAIN_COLUMN_COUNT;
      ++column) {
    paradox_account_work(work_since_interrupt);
    SET_STRING_ELT(names, column, Rf_mkChar(paradox_domain_column_names[column]));
  }
  Rf_setAttrib(result, R_NamesSymbol, names);

  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 4));
  SET_STRING_ELT(classes, 0, cls);
  SET_STRING_ELT(classes, 1, Rf_mkChar("Domain"));
  SET_STRING_ELT(classes, 2, Rf_mkChar("data.table"));
  SET_STRING_ELT(classes, 3, Rf_mkChar("data.frame"));
  Rf_setAttrib(result, R_ClassSymbol, classes);

  SEXP row_names = PROTECT(Rf_allocVector(INTSXP, 2));
  SET_INTEGER_ELT(row_names, 0, NA_INTEGER);
  SET_INTEGER_ELT(row_names, 1, -1);
  Rf_setAttrib(result, R_RowNamesSymbol, row_names);

  SEXP prepared = PROTECT(paradox_prepare_fresh_data_table(result));
  UNPROTECT(5);
  return prepared;
}

SEXP paradox_domain_fill(SEXP domain, const paradox_domain_row_t *row,
    R_xlen_t *work_since_interrupt) {
  paradox_account_work(work_since_interrupt);
  for (enum paradox_domain_column column = PARADOX_DOMAIN_ID;
      column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    paradox_account_work(work_since_interrupt);
    set_scalar_column(
      domain,
      column,
      VECTOR_ELT(row->params->table, column),
      row->parameter_row
    );
  }
  const int typed = !paradox_domain_string_is(
    STRING_ELT(row->params->classes, row->parameter_row),
    "ParamUty"
  );
  static const enum paradox_domain_column detached_static[] = {
    PARADOX_DOMAIN_CARGO,
    PARADOX_DOMAIN_LEVELS,
    PARADOX_DOMAIN_SPECIAL_VALS,
    PARADOX_DOMAIN_DEFAULT
  };
  for (size_t index = 0;
      index < sizeof(detached_static) / sizeof(detached_static[0]);
      ++index) {
    const enum paradox_domain_column column = detached_static[index];
    SEXP output = VECTOR_ELT(domain, column);
    SEXP detached = PROTECT(paradox_detach_domain_row_field(
      VECTOR_ELT(output, 0),
      column,
      typed,
      work_since_interrupt
    ));
    if (detached == R_UnboundValue) {
      UNPROTECT(1);
      Rf_error(
        "Corrupt ParamSet capsule: cannot detach Domain `%s` field",
        paradox_domain_column_names[column]
      );
    }
    SET_VECTOR_ELT(output, 0, detached);
    UNPROTECT(1);
  }

  SEXP selected_tags = PROTECT(Rf_allocVector(STRSXP, row->tag_count));
  for (R_xlen_t index = 0; index < row->tag_count; ++index) {
    paradox_account_work(work_since_interrupt);
    SET_STRING_ELT(
      selected_tags,
      index,
      STRING_ELT(row->tags->values, row->tag_rows[index])
    );
  }
  SEXP tag_column = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(tag_column, 0, selected_tags);
  SET_VECTOR_ELT(domain, PARADOX_DOMAIN_TAGS, tag_column);
  UNPROTECT(2);

  SEXP trafo_column = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(trafo_column, 0, row->trafo);
  SET_VECTOR_ELT(domain, PARADOX_DOMAIN_TRAFO, trafo_column);
  UNPROTECT(1);

  SEXP requirements_column = PROTECT(Rf_allocVector(VECSXP, 1));
  if (row->dependency_count == 0) {
    SET_VECTOR_ELT(requirements_column, 0, R_NilValue);
  } else {
    SEXP requirements = PROTECT(Rf_allocVector(
      VECSXP,
      row->dependency_count
    ));
    for (R_xlen_t index = 0; index < row->dependency_count; ++index) {
      paradox_account_work(work_since_interrupt);
      const R_xlen_t dependency_row = row->dependency_rows[index];
      SEXP requirement = PROTECT(Rf_allocVector(VECSXP, 2));
      SEXP on = PROTECT(Rf_allocVector(STRSXP, 1));
      SET_STRING_ELT(
        on,
        0,
        STRING_ELT(row->dependencies->on, dependency_row)
      );
      SET_VECTOR_ELT(requirement, 0, on);
      SEXP condition = PROTECT(paradox_builtin_condition_snapshot(
        VECTOR_ELT(row->dependencies->conditions, dependency_row),
        work_since_interrupt
      ));
      if (condition == R_UnboundValue) {
        UNPROTECT(5);
        Rf_error("Corrupt ParamSet capsule: malformed dependency Condition");
      }
      SET_VECTOR_ELT(requirement, 1, condition);
      SEXP requirement_names = PROTECT(Rf_allocVector(STRSXP, 2));
      SET_STRING_ELT(requirement_names, 0, Rf_mkChar("on"));
      SET_STRING_ELT(requirement_names, 1, Rf_mkChar("cond"));
      Rf_setAttrib(requirement, R_NamesSymbol, requirement_names);
      SET_VECTOR_ELT(requirements, index, requirement);
      UNPROTECT(4);
    }
    SET_VECTOR_ELT(requirements_column, 0, requirements);
    UNPROTECT(1);
  }
  SET_VECTOR_ELT(domain, PARADOX_DOMAIN_REQUIREMENTS, requirements_column);
  UNPROTECT(1);

  SEXP init_given_column = PROTECT(Rf_allocVector(LGLSXP, 1));
  SET_LOGICAL_ELT(init_given_column, 0, row->init_given);
  SET_VECTOR_ELT(domain, PARADOX_DOMAIN_INIT_GIVEN, init_given_column);
  SEXP init_column = PROTECT(Rf_allocVector(VECSXP, 1));
  SEXP detached_init = PROTECT(paradox_detach_domain_row_field(
    row->init_value,
    PARADOX_DOMAIN_INIT,
    typed,
    work_since_interrupt
  ));
  SET_VECTOR_ELT(init_column, 0, detached_init);
  SET_VECTOR_ELT(domain, PARADOX_DOMAIN_INIT, init_column);
  UNPROTECT(3);

  return paradox_domain_prepare_facade(
    domain,
    STRING_ELT(row->params->classes, row->parameter_row),
    work_since_interrupt
  );
}
