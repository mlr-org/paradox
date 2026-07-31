#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Arith.h>
#include <R_ext/Utils.h>

#include "domain_admission.h"
#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

typedef enum {
  DOMAIN_KIND_UNKNOWN = 0,
  DOMAIN_KIND_EMPTY,
  DOMAIN_KIND_DBL,
  DOMAIN_KIND_INT,
  DOMAIN_KIND_FCT,
  DOMAIN_KIND_LGL,
  DOMAIN_KIND_UTY
} domain_kind_t;

typedef struct {
  domain_kind_t kind;
  R_xlen_t size;
  int grouped;
} domain_info_t;

static int zero_length_vector(SEXP value) {
  switch ((SEXPTYPE) TYPEOF(value)) {
  case NILSXP:
    return TRUE;
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case VECSXP:
  case EXPRSXP:
  case RAWSXP:
    return XLENGTH(value) == 0;
  default:
    return FALSE;
  }
}

static inline void periodic_interrupt(R_xlen_t iteration) {
  if (iteration != 0 &&
      iteration % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
    R_CheckUserInterrupt();
  }
}

static domain_kind_t class_kind(SEXP param) {
  if (TYPEOF(param) != VECSXP || ALTREP(param) || Rf_isS4(param)) {
    return DOMAIN_KIND_UNKNOWN;
  }
  SEXP classes = PROTECT(Rf_getAttrib(param, R_ClassSymbol));
  if (TYPEOF(classes) != STRSXP || ALTREP(classes) ||
      Rf_isS4(classes) || Rf_isObject(classes) ||
      !paradox_api_has_no_attributes(classes)) {
    UNPROTECT(1);
    return DOMAIN_KIND_UNKNOWN;
  }

  if (XLENGTH(classes) == 2 &&
      paradox_domain_string_is(STRING_ELT(classes, 0), "data.table") &&
      paradox_domain_string_is(STRING_ELT(classes, 1), "data.frame")) {
    UNPROTECT(1);
    return DOMAIN_KIND_EMPTY;
  }
  if (XLENGTH(classes) != 4) {
    UNPROTECT(1);
    return DOMAIN_KIND_UNKNOWN;
  }

  static const char *const tail[] = {
    "Domain", "data.table", "data.frame"
  };
  for (R_xlen_t index = 0; index < 3; ++index) {
    SEXP value = STRING_ELT(classes, index + 1);
    if (value == NA_STRING || strcmp(CHAR(value), tail[index]) != 0) {
      UNPROTECT(1);
      return DOMAIN_KIND_UNKNOWN;
    }
  }

  SEXP first = STRING_ELT(classes, 0);
  if (first == NA_STRING) {
    UNPROTECT(1);
    return DOMAIN_KIND_UNKNOWN;
  }
  const char *name = CHAR(first);
  if (strcmp(name, "ParamDbl") == 0) {
    UNPROTECT(1);
    return DOMAIN_KIND_DBL;
  }
  if (strcmp(name, "ParamInt") == 0) {
    UNPROTECT(1);
    return DOMAIN_KIND_INT;
  }
  if (strcmp(name, "ParamFct") == 0) {
    UNPROTECT(1);
    return DOMAIN_KIND_FCT;
  }
  if (strcmp(name, "ParamLgl") == 0) {
    UNPROTECT(1);
    return DOMAIN_KIND_LGL;
  }
  if (strcmp(name, "ParamUty") == 0) {
    UNPROTECT(1);
    return DOMAIN_KIND_UTY;
  }
  UNPROTECT(1);
  return DOMAIN_KIND_UNKNOWN;
}

static const char *kind_name(domain_kind_t kind) {
  switch (kind) {
  case DOMAIN_KIND_EMPTY:
    return "";
  case DOMAIN_KIND_DBL:
    return "ParamDbl";
  case DOMAIN_KIND_INT:
    return "ParamInt";
  case DOMAIN_KIND_FCT:
    return "ParamFct";
  case DOMAIN_KIND_LGL:
    return "ParamLgl";
  case DOMAIN_KIND_UTY:
    return "ParamUty";
  case DOMAIN_KIND_UNKNOWN:
    return "";
  }
  return "";
}

static paradox_builtin_domain_kind_t builtin_domain_kind(domain_kind_t kind) {
  switch (kind) {
  case DOMAIN_KIND_DBL: return PARADOX_BUILTIN_DOMAIN_DBL;
  case DOMAIN_KIND_INT: return PARADOX_BUILTIN_DOMAIN_INT;
  case DOMAIN_KIND_FCT: return PARADOX_BUILTIN_DOMAIN_FCT;
  case DOMAIN_KIND_LGL: return PARADOX_BUILTIN_DOMAIN_LGL;
  case DOMAIN_KIND_UTY: return PARADOX_BUILTIN_DOMAIN_UTY;
  case DOMAIN_KIND_EMPTY:
  case DOMAIN_KIND_UNKNOWN:
    Rf_error("Internal error: unknown built-in Domain kind");
  }
  Rf_error("Internal error: unknown built-in Domain kind");
  return PARADOX_BUILTIN_DOMAIN_UNKNOWN;
}

static void validate_empty_domain(SEXP param) {
  static const int column_types[] = {
    STRSXP, STRSXP, STRSXP, VECSXP, REALSXP, REALSXP, REALSXP, VECSXP,
    VECSXP, VECSXP, STRSXP, STRSXP, VECSXP, VECSXP, LGLSXP, VECSXP
  };
  static const char *const allowed_attributes[] = {
    "names", "class", "row.names", ".internal.selfref"
  };
  const R_xlen_t column_count = (R_xlen_t) (
    PARADOX_DOMAIN_COLUMN_COUNT
  );
  if (TYPEOF(param) != VECSXP || ALTREP(param) || Rf_isS4(param) ||
      XLENGTH(param) != column_count ||
      !paradox_api_has_only_attributes(param, allowed_attributes, 4)) {
    Rf_error("Corrupt empty Domain storage");
  }
  SEXP names = PROTECT(Rf_getAttrib(param, R_NamesSymbol));
  SEXP row_names = PROTECT(Rf_getAttrib(param, R_RowNamesSymbol));
  SEXP selfref = PROTECT(Rf_getAttrib(
    param,
    Rf_install(".internal.selfref")
  ));
  if (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
      Rf_isObject(names) ||
      !paradox_api_has_no_attributes(names) ||
      XLENGTH(names) != column_count || TYPEOF(row_names) != INTSXP ||
      ALTREP(row_names) || Rf_isS4(row_names) ||
      Rf_isObject(row_names) ||
      !paradox_api_has_no_attributes(row_names) ||
      XLENGTH(row_names) != 0 || TYPEOF(selfref) != EXTPTRSXP ||
      Rf_isS4(selfref)) {
    UNPROTECT(3);
    Rf_error("Corrupt empty Domain storage");
  }
  for (R_xlen_t column = 0; column < column_count; ++column) {
    SEXP value = VECTOR_ELT(param, column);
    if (!paradox_domain_string_is(
        STRING_ELT(names, column),
        paradox_domain_column_names[column]
      ) || TYPEOF(value) != column_types[column] || ALTREP(value) ||
        Rf_isS4(value) || Rf_isObject(value) ||
        !paradox_api_has_no_attributes(value) ||
        XLENGTH(value) != 0) {
      UNPROTECT(3);
      Rf_error("Corrupt empty Domain storage");
    }
  }
  UNPROTECT(3);
}

static domain_info_t domain_info(SEXP param) {
  const domain_kind_t kind = class_kind(param);
  if (kind == DOMAIN_KIND_UNKNOWN) {
    Rf_error(
      "Unsupported Domain class; expected one of ParamDbl, ParamInt, "
      "ParamFct, ParamLgl, or ParamUty"
    );
  }
  if (kind == DOMAIN_KIND_EMPTY) {
    validate_empty_domain(param);
    const domain_info_t empty = {DOMAIN_KIND_EMPTY, 0, TRUE};
    return empty;
  }

  SEXP schema_columns[PARADOX_DOMAIN_COLUMN_COUNT];
  paradox_domain_select_columns(
    param,
    "Domain storage",
    "Domain",
    (1U << PARADOX_DOMAIN_ID) | (1U << PARADOX_DOMAIN_CLS) |
      (1U << PARADOX_DOMAIN_GROUPING) | (1U << PARADOX_DOMAIN_STORAGE_TYPE),
    schema_columns
  );
  SEXP ids = PROTECT(schema_columns[PARADOX_DOMAIN_ID]);
  if (TYPEOF(ids) != STRSXP) {
    Rf_error("Corrupt Domain storage: `id` must have type `character`");
  }
  if (ALTREP(ids)) {
    Rf_error(
      "Corrupt Domain storage: `id` must use an ordinary character representation"
    );
  }
  const R_xlen_t size = XLENGTH(ids);
  SEXP classes = PROTECT(schema_columns[PARADOX_DOMAIN_CLS]);
  SEXP grouping = PROTECT(schema_columns[PARADOX_DOMAIN_GROUPING]);
  SEXP storage = PROTECT(schema_columns[PARADOX_DOMAIN_STORAGE_TYPE]);
  paradox_require_column_checked(
    ids,
    STRSXP,
    size,
    "Domain storage",
    "id"
  );
  paradox_require_column_checked(
    classes,
    STRSXP,
    size,
    "Domain storage",
    "cls"
  );
  paradox_require_column_checked(
    grouping,
    STRSXP,
    size,
    "Domain storage",
    "grouping"
  );
  paradox_require_column_checked(
    storage,
    STRSXP,
    size,
    "Domain storage",
    "storage_type"
  );
  /* Package-owned Domain facades carry ordinary, unclassed schema columns.
   * Dispatch-sensitive metadata is corrupt state, not a request to restart in
   * a second semantic engine. */
  if (!paradox_api_has_no_attributes(ids) ||
      !paradox_api_has_no_attributes(classes) ||
      !paradox_api_has_no_attributes(grouping) ||
      !paradox_api_has_no_attributes(storage)) {
    Rf_error("Corrupt Domain storage: schema columns must be ordinary vectors");
  }

  int grouped = TRUE;
  const char *expected_class = kind_name(kind);
  const char *expected_storage = kind == DOMAIN_KIND_DBL ? "numeric" :
    (kind == DOMAIN_KIND_INT ? "integer" :
    (kind == DOMAIN_KIND_FCT ? "character" :
    (kind == DOMAIN_KIND_LGL ? "logical" : "list")));
  SEXP first_group = size == 0 ? R_NilValue : STRING_ELT(grouping, 0);
  /* Every row of a canonical table stores the same interned class and storage
   * strings, so after one byte comparison the accepted CHARSXP answers all
   * later rows by identity. */
  SEXP accepted_cls = NA_STRING;
  SEXP accepted_storage = NA_STRING;
  for (R_xlen_t row = 0; row < size; ++row) {
    periodic_interrupt(row);
    SEXP id = STRING_ELT(ids, row);
    SEXP cls = STRING_ELT(classes, row);
    SEXP group = STRING_ELT(grouping, row);
    SEXP storage_value = STRING_ELT(storage, row);
    if (id == NA_STRING) {
      Rf_error("Corrupt Domain storage: `id` contains a missing value");
    }
    if (cls != accepted_cls) {
      if (cls == NA_STRING || strcmp(CHAR(cls), expected_class) != 0) {
        Rf_error(
          "Corrupt Domain storage: `cls` is inconsistent with its class"
        );
      }
      accepted_cls = cls;
    }
    if (storage_value != accepted_storage) {
      if (storage_value == NA_STRING ||
          strcmp(CHAR(storage_value), expected_storage) != 0) {
        Rf_error(
          "Corrupt Domain storage: `storage_type` is inconsistent with its "
          "class"
        );
      }
      accepted_storage = storage_value;
    }
    if (group == NA_STRING || !paradox_domain_strings_equal(group, first_group)) {
      grouped = FALSE;
    }
  }

  const domain_info_t result = {kind, size, grouped};
  UNPROTECT(4);
  return result;
}

static int numeric_scalar(SEXP value, double *result, int allow_logical) {
  /* XLENGTH() is not defined for every SEXP type (notably NILSXP).  Reject
   * unsupported values by type before asking for their vector length so that
   * list(NULL), which is a legitimate special-value shape in paradox, falls
   * back to the established R special-value handling instead of raising from
   * the C API. */
  if (Rf_isS4(value) || Rf_isObject(value)) {
    return FALSE;
  }

  switch ((SEXPTYPE) TYPEOF(value)) {
  case REALSXP:
    if (XLENGTH(value) != 1) {
      return FALSE;
    }
    *result = REAL_ELT(value, 0);
    return TRUE;
  case INTSXP: {
    if (XLENGTH(value) != 1) {
      return FALSE;
    }
    const int element = INTEGER_ELT(value, 0);
    *result = element == NA_INTEGER ? NA_REAL : (double) element;
    return TRUE;
  }
  case LGLSXP: {
    if (!allow_logical || XLENGTH(value) != 1) {
      return FALSE;
    }
    const int element = LOGICAL_ELT(value, 0);
    *result = element == NA_LOGICAL ? NA_REAL : (double) element;
    return TRUE;
  }
  default:
    return FALSE;
  }
}

static int vector_numeric_at(SEXP values, R_xlen_t index, double *result) {
  if (TYPEOF(values) == VECSXP) {
    return numeric_scalar(VECTOR_ELT(values, index), result, TRUE);
  }
  if (Rf_isObject(values)) {
    return FALSE;
  }

  switch ((SEXPTYPE) TYPEOF(values)) {
  case REALSXP:
    *result = REAL_ELT(values, index);
    return TRUE;
  case INTSXP: {
    const int element = INTEGER_ELT(values, index);
    *result = element == NA_INTEGER ? NA_REAL : (double) element;
    return TRUE;
  }
  case LGLSXP: {
    const int element = LOGICAL_ELT(values, index);
    *result = element == NA_LOGICAL ? NA_REAL : (double) element;
    return TRUE;
  }
  default:
    return FALSE;
  }
}

static SEXP materialize_domain_value(SEXP value) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  if (type != LGLSXP && type != INTSXP && type != REALSXP &&
      type != CPLXSXP && type != STRSXP && type != RAWSXP &&
      type != VECSXP) {
    Rf_error("Unsupported ALTREP Domain value type");
  }
  const R_xlen_t size = XLENGTH(value);
  SEXP result = PROTECT(Rf_allocVector(type, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    periodic_interrupt(index);
    switch (type) {
    case LGLSXP:
      SET_LOGICAL_ELT(result, index, LOGICAL_ELT(value, index));
      break;
    case INTSXP:
      SET_INTEGER_ELT(result, index, INTEGER_ELT(value, index));
      break;
    case REALSXP:
      SET_REAL_ELT(result, index, REAL_ELT(value, index));
      break;
    case CPLXSXP:
      paradox_api_set_complex_elt(
        result,
        index,
        COMPLEX_ELT(value, index)
      );
      break;
    case STRSXP:
      SET_STRING_ELT(result, index, STRING_ELT(value, index));
      break;
    case RAWSXP:
      paradox_api_set_raw_elt(result, index, RAW_ELT(value, index));
      break;
    case VECSXP:
      SET_VECTOR_ELT(result, index, VECTOR_ELT(value, index));
      break;
    default:
      UNPROTECT(1);
      Rf_error("Unsupported ALTREP Domain value type");
    }
  }
  SHALLOW_DUPLICATE_ATTRIB(result, value);
  UNPROTECT(1);
  return result;
}

static SEXP stable_list_argument(SEXP values, R_xlen_t expected_size,
    domain_kind_t kind, int *callback_capable_admission) {
  *callback_capable_admission = FALSE;
  if (TYPEOF(values) != VECSXP) {
    Rf_error("`values` must be an ordinary list with one element per Domain row");
  }
  *callback_capable_admission = ALTREP(values);
  if (Rf_isObject(values) || XLENGTH(values) != expected_size) {
    Rf_error("`values` must be an ordinary list with one element per Domain row");
  }

  SEXP stable = PROTECT(Rf_allocVector(VECSXP, expected_size));
  for (R_xlen_t row = 0; row < expected_size; ++row) {
    periodic_interrupt(row);
    SEXP value = PROTECT(VECTOR_ELT(values, row));
    /* ParamUty values are deliberately opaque and identity-bearing. Built-in
     * scalar kinds instead own one ordinary snapshot before special-value and
     * type/bounds passes can observe the value independently. */
    const int materialize = kind != DOMAIN_KIND_UTY && ALTREP(value);
    if (materialize) *callback_capable_admission = TRUE;
    SEXP snapshot = PROTECT(
      materialize ? materialize_domain_value(value) : value
    );
    SET_VECTOR_ELT(stable, row, snapshot);
    UNPROTECT(2);
  }
  UNPROTECT(1);
  return stable;
}

static SEXP check_failure(SEXP id, SEXP reason) {
  PROTECT(id);
  PROTECT(reason);
  /* `reason` is whatever a user `custom_check` returned. Both fragments must
   * be diagnostic-safe before the result below is minted as UTF-8, or it
   * would declare an encoding its bytes do not satisfy. */
  SEXP safe_id = PROTECT(paradox_diagnostic_charsxp(id));
  SEXP safe_reason = PROTECT(paradox_diagnostic_charsxp(reason));
  size_t id_size;
  char *id_text = paradox_temporary_utf8_copy(safe_id, &id_size);
  size_t reason_size;
  char *reason_text = paradox_temporary_utf8_copy(
    safe_reason,
    &reason_size
  );
  if (id_size > (size_t) R_XLEN_T_MAX - 3U ||
      reason_size > (size_t) R_XLEN_T_MAX - id_size - 3U) {
    UNPROTECT(4);
    Rf_error("Domain diagnostic is too large");
  }
  char *message = paradox_temporary_alloc(
    (R_xlen_t) (id_size + reason_size + 3U),
    sizeof(*message)
  );
  memcpy(message, id_text, id_size);
  message[id_size] = ':';
  message[id_size + 1U] = ' ';
  memcpy(message + id_size + 2U, reason_text, reason_size + 1U);
  SEXP text = PROTECT(Rf_mkCharCE(message, CE_UTF8));
  SEXP result = PROTECT(Rf_ScalarString(text));
  UNPROTECT(6);
  return result;
}

static SEXP check_failure_literal(SEXP id, const char *reason) {
  SEXP reason_string = PROTECT(Rf_mkCharCE(reason, CE_UTF8));
  SEXP result = PROTECT(check_failure(id, reason_string));
  UNPROTECT(2);
  return result;
}

static int *snapshot_special_hits(
    const paradox_admitted_domain_table_t *table, SEXP values,
    const domain_info_t *info, int internal) {
  int *hits = paradox_temporary_alloc(
    info->size == 0 ? 1 : info->size,
    sizeof(*hits)
  );
  memset(hits, 0, (size_t) info->size * sizeof(*hits));
  if (internal || info->size == 0) {
    return hits;
  }

  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t row = 0; row < info->size; ++row) {
    paradox_account_work(&work_since_interrupt);
    SEXP choices = paradox_admitted_domain_field(
      table,
      row,
      PARADOX_ADMITTED_SPECIAL_VALS
    );
    SEXP value = PROTECT(VECTOR_ELT(values, row));
    hits[row] = paradox_builtin_special_values_contain(
      builtin_domain_kind(info->kind),
      choices,
      value,
      &work_since_interrupt
    );
    UNPROTECT(1);
  }
  return hits;
}

static SEXP check_numeric_domain(
    const paradox_admitted_domain_table_t *table, SEXP values,
    const domain_info_t *info, const int *skip) {
  SEXP ids = VECTOR_ELT(table->columns, PARADOX_DOMAIN_ID);
  for (R_xlen_t row = 0; row < info->size; ++row) {
    periodic_interrupt(row);
    if (skip[row]) {
      continue;
    }
    SEXP element = PROTECT(VECTOR_ELT(values, row));
    const paradox_builtin_value_spec_t spec = {
      builtin_domain_kind(info->kind),
      table->lower[row],
      table->upper[row],
      table->tolerance[row],
      R_NilValue,
      R_NilValue
    };
    R_xlen_t work_since_interrupt = 0;
    const paradox_builtin_value_result_t checked =
      paradox_builtin_value_check(
        &spec,
        element,
        FALSE,
        &work_since_interrupt
      );
    if (checked.failure != PARADOX_BUILTIN_VALUE_OK) {
      SEXP diagnostic = PROTECT(paradox_builtin_value_diagnostic(
        STRING_ELT(ids, row),
        &spec,
        element,
        &checked
      ));
      UNPROTECT(2);
      return diagnostic;
    }
    UNPROTECT(1);
  }
  return R_NilValue;
}

static SEXP check_factor_domain(
    const paradox_admitted_domain_table_t *table, SEXP values,
    const domain_info_t *info, const int *skip) {
  SEXP ids = VECTOR_ELT(table->columns, PARADOX_DOMAIN_ID);
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t row = 0; row < info->size; ++row) {
    paradox_account_work(&work_since_interrupt);
    if (skip[row]) {
      continue;
    }
    SEXP value = PROTECT(VECTOR_ELT(values, row));
    SEXP choices = paradox_admitted_domain_field(
      table,
      row,
      PARADOX_ADMITTED_LEVELS
    );
    const paradox_builtin_value_spec_t spec = {
      PARADOX_BUILTIN_DOMAIN_FCT,
      NA_REAL,
      NA_REAL,
      NA_REAL,
      choices,
      R_NilValue
    };
    const paradox_builtin_value_result_t checked =
      paradox_builtin_value_check(
        &spec,
        value,
        FALSE,
        &work_since_interrupt
      );
    if (checked.failure != PARADOX_BUILTIN_VALUE_OK) {
      SEXP diagnostic = PROTECT(paradox_builtin_value_diagnostic(
        STRING_ELT(ids, row),
        &spec,
        value,
        &checked
      ));
      UNPROTECT(2);
      return diagnostic;
    }
    UNPROTECT(1);
  }
  return R_NilValue;
}

static SEXP check_logical_domain(
    const paradox_admitted_domain_table_t *table, SEXP values,
    const domain_info_t *info, const int *skip) {
  SEXP ids = VECTOR_ELT(table->columns, PARADOX_DOMAIN_ID);
  for (R_xlen_t row = 0; row < info->size; ++row) {
    periodic_interrupt(row);
    if (skip[row]) {
      continue;
    }
    SEXP value = PROTECT(VECTOR_ELT(values, row));
    const paradox_builtin_value_spec_t spec = {
      PARADOX_BUILTIN_DOMAIN_LGL,
      NA_REAL,
      NA_REAL,
      NA_REAL,
      R_NilValue,
      R_NilValue
    };
    R_xlen_t work_since_interrupt = 0;
    const paradox_builtin_value_result_t checked =
      paradox_builtin_value_check(
        &spec,
        value,
        FALSE,
        &work_since_interrupt
      );
    if (checked.failure != PARADOX_BUILTIN_VALUE_OK) {
      SEXP diagnostic = PROTECT(paradox_builtin_value_diagnostic(
        STRING_ELT(ids, row),
        &spec,
        value,
        &checked
      ));
      UNPROTECT(2);
      return diagnostic;
    }
    UNPROTECT(1);
  }
  return R_NilValue;
}

static SEXP named_list_element(SEXP values, const char *target) {
  /* Reject ALTREP and measure once, exactly like the ParamSet twin: the
   * `names` length is checked against this container's length, so a second
   * Length observation must not be able to answer differently. */
  if (TYPEOF(values) != VECSXP || ALTREP(values)) {
    return R_UnboundValue;
  }
  const R_xlen_t size = XLENGTH(values);
  SEXP names = PROTECT(Rf_getAttrib(values, R_NamesSymbol));
  if (TYPEOF(names) != STRSXP || ALTREP(names) || XLENGTH(names) != size) {
    UNPROTECT(1);
    return R_UnboundValue;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP name = STRING_ELT(names, index);
    if (name != NA_STRING && strcmp(CHAR(name), target) == 0) {
      SEXP result = VECTOR_ELT(values, index);
      UNPROTECT(1);
      return result;
    }
  }
  UNPROTECT(1);
  return R_UnboundValue;
}

/* Select each admitted row's `custom_check` leaf. Cargo canonicality is the
 * row owner's rule and has already been decided; this only extracts. */
static SEXP snapshot_utility_callbacks(
    const paradox_admitted_domain_table_t *table,
    const domain_info_t *info) {
  SEXP callbacks = PROTECT(Rf_allocVector(VECSXP, info->size));
  for (R_xlen_t row = 0; row < info->size; ++row) {
    periodic_interrupt(row);
    SEXP row_cargo = paradox_admitted_domain_field(
      table,
      row,
      PARADOX_ADMITTED_CARGO
    );
    SEXP callback = row_cargo == R_NilValue
      ? R_NilValue
      : named_list_element(row_cargo, "custom_check");
    /* Canonical cargo may legitimately omit the optional callback. */
    SET_VECTOR_ELT(
      callbacks,
      row,
      callback == R_UnboundValue ? R_NilValue : callback
    );
  }
  UNPROTECT(1);
  return callbacks;
}

static SEXP check_utility_domain(
    const paradox_admitted_domain_table_t *table, SEXP values,
    const domain_info_t *info, const int *skip) {
  SEXP callbacks = PROTECT(snapshot_utility_callbacks(table, info));
  SEXP ids = VECTOR_ELT(table->columns, PARADOX_DOMAIN_ID);
  for (R_xlen_t row = 0; row < info->size; ++row) {
    periodic_interrupt(row);
    if (skip[row]) {
      continue;
    }
    SEXP callback = VECTOR_ELT(callbacks, row);
    if (callback == R_NilValue) {
      continue;
    }
    SEXP call = PROTECT(paradox_unary_callback_call(
      callback,
      VECTOR_ELT(values, row)
    ));
    SEXP answer = PROTECT(Rf_eval(call, R_BaseEnv));
    if (TYPEOF(answer) == LGLSXP && XLENGTH(answer) == 1 &&
        LOGICAL_ELT(answer, 0) == TRUE) {
      UNPROTECT(2);
      continue;
    }
    SEXP id = STRING_ELT(ids, row);
    SEXP result;
    SEXP reason = TYPEOF(answer) == STRSXP && XLENGTH(answer) == 1
      ? STRING_ELT(answer, 0)
      : NA_STRING;
    if (reason != NA_STRING) {
      result = PROTECT(check_failure(id, reason));
    } else {
      result = PROTECT(check_failure_literal(
        id, "`custom_check` must return TRUE or one non-missing string"
      ));
    }
    UNPROTECT(4);
    return result;
  }
  UNPROTECT(1);
  return Rf_ScalarLogical(TRUE);
}

SEXP paradox_domain_check_builtin(SEXP param, SEXP values, SEXP internal) {
  if (TYPEOF(internal) != LGLSXP || XLENGTH(internal) != 1) {
    Rf_error("`internal` must be TRUE or FALSE");
  }
  /* Observe the flag exactly once. Value admission below may materialize a
   * semantic ALTREP and reenter R, and a second observation of an ALTREP
   * scalar can answer differently -- including with NA, which the
   * special-value snapshot would read as "internal" and silently stop
   * honoring `special_vals`. */
  const int internal_flag = LOGICAL_ELT(internal, 0);
  if (internal_flag == NA_LOGICAL) {
    Rf_error("`internal` must be TRUE or FALSE");
  }

  const domain_info_t admission_info = domain_info(param);
  if (!admission_info.grouped) {
    Rf_error("Corrupt Domain storage: rows must share one grouping");
  }
  if (zero_length_vector(values)) {
    return Rf_ScalarLogical(TRUE);
  }
  if (admission_info.size == 0) {
    Rf_error("Cannot check nonempty values against an empty Domain");
  }
  int callback_capable_admission = FALSE;
  SEXP stable_values = PROTECT(stable_list_argument(
    values,
    admission_info.size,
    admission_info.kind,
    &callback_capable_admission
  ));
  /* Materializing a semantic ALTREP value may reenter R and mutate this
   * outward Domain table.  The contract deliberately makes that mutation part
   * of the operation snapshot, so re-admit the complete current shape before
   * reading special values, callbacks, levels, or bounds.  A changed kind or
   * row count cannot reinterpret the already materialized value list safely;
   * reject it instead of combining fields from two Domain generations. */
  domain_info_t info = admission_info;
  if (callback_capable_admission) {
    info = domain_info(param);
    if (info.kind != admission_info.kind || info.size != admission_info.size) {
      UNPROTECT(1);
      Rf_error("Domain shape changed during value admission");
    }
    if (!info.grouped) {
      UNPROTECT(1);
      Rf_error("Corrupt Domain storage: rows must share one grouping");
    }
  }
  /*
   * Route the complete outward Domain through the canonical row owner before
   * any operation-specific work. The special-value fast path below may skip a
   * row's value check, but it may not skip that row's schema admission.
   */
  R_xlen_t work_since_interrupt = 0;
  paradox_admitted_domain_table_t table;
  PROTECT(paradox_admit_public_domain_table(
    param,
    builtin_domain_kind(info.kind),
    info.size,
    &table,
    &work_since_interrupt
  ));
  const int *skip = snapshot_special_hits(
    &table,
    stable_values,
    &info,
    internal_flag
  );

  if (info.kind == DOMAIN_KIND_UTY) {
    SEXP result = PROTECT(check_utility_domain(
      &table, stable_values, &info, skip
    ));
    UNPROTECT(3);
    return result;
  }

  SEXP diagnostic = R_NilValue;
  int diagnostic_protected = FALSE;
  switch (info.kind) {
  case DOMAIN_KIND_EMPTY:
    break;
  case DOMAIN_KIND_DBL:
  case DOMAIN_KIND_INT:
    diagnostic = PROTECT(check_numeric_domain(
      &table, stable_values, &info, skip
    ));
    diagnostic_protected = TRUE;
    break;
  case DOMAIN_KIND_FCT:
    diagnostic = PROTECT(check_factor_domain(
      &table, stable_values, &info, skip
    ));
    diagnostic_protected = TRUE;
    break;
  case DOMAIN_KIND_LGL:
    diagnostic = PROTECT(check_logical_domain(
      &table, stable_values, &info, skip
    ));
    diagnostic_protected = TRUE;
    break;
  case DOMAIN_KIND_UTY:
    break;
  case DOMAIN_KIND_UNKNOWN:
    break;
  }
  if (diagnostic == R_NilValue) {
    if (diagnostic_protected) {
      UNPROTECT(1);
    }
    UNPROTECT(2);
    return Rf_ScalarLogical(TRUE);
  }
  if (!diagnostic_protected) {
    UNPROTECT(2);
    Rf_error("Internal error: unrooted Domain diagnostic");
  }
  UNPROTECT(3);
  return diagnostic;
}

static SEXP numeric_vector_as_list(SEXP values) {
  return Rf_coerceVector(values, VECSXP);
}

static SEXP sanitize_double(const paradox_admitted_domain_table_t *table,
    SEXP values, const domain_info_t *info) {
  const R_xlen_t value_size = XLENGTH(values);
  if (value_size == 0) {
    return values;
  }
  if (info->size == 0) {
    Rf_error("Cannot sanitize values against an empty Domain");
  }

  /* Establish support before emitting recycling warnings. Materialize each
   * observation at the same time: callback-capable ALTREP vectors and list
   * elements must not be reread after output allocation. */
  SEXP stable_values = PROTECT(Rf_allocVector(REALSXP, value_size));
  for (R_xlen_t index = 0; index < value_size; ++index) {
    periodic_interrupt(index);
    double value;
    if (!vector_numeric_at(values, index, &value)) {
      Rf_error("`values` must contain only numeric scalar values");
    }
    SET_REAL_ELT(stable_values, index, value);
  }

  const R_xlen_t result_size = value_size > info->size
    ? value_size
    : info->size;
  const R_xlen_t shorter = value_size < info->size ? value_size : info->size;
  if (result_size % shorter != 0) {
    Rf_warning("longer object length is not a multiple of shorter object length");
  }

  SEXP numeric = PROTECT(Rf_allocVector(REALSXP, result_size));
  for (R_xlen_t index = 0; index < result_size; ++index) {
    periodic_interrupt(index);
    double value = REAL_ELT(stable_values, index % value_size);
    const double row_lower = table->lower[index % info->size];
    const double row_upper = table->upper[index % info->size];
    if (!ISNAN(value) && value < row_lower) {
      value = row_lower;
    }
    if (!ISNAN(value) && value > row_upper) {
      value = row_upper;
    }
    SET_REAL_ELT(numeric, index, value);
  }

  SEXP result = PROTECT(numeric_vector_as_list(numeric));
  UNPROTECT(3);
  return result;
}

static int integer_from_double(double value, int *out) {
  if (ISNAN(value)) {
    *out = NA_INTEGER;
    return FALSE;
  }
  if (!R_FINITE(value) || value > (double) INT_MAX ||
      value <= (double) INT_MIN) {
    *out = NA_INTEGER;
    return TRUE;
  }
  *out = (int) value;
  return FALSE;
}

static SEXP sanitize_integer(SEXP values) {
  const R_xlen_t size = XLENGTH(values);
  SEXP stable_values = PROTECT(Rf_allocVector(REALSXP, size));
  int warn_range = FALSE;
  for (R_xlen_t index = 0; index < size; ++index) {
    periodic_interrupt(index);
    double value;
    int ignored;
    if (!vector_numeric_at(values, index, &value)) {
      Rf_error("`values` must contain only numeric scalar values");
    }
    if (integer_from_double(nearbyint(value), &ignored)) {
      warn_range = TRUE;
    }
    SET_REAL_ELT(stable_values, index, value);
  }

  SEXP integer = PROTECT(Rf_allocVector(INTSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    periodic_interrupt(index);
    const double value = REAL_ELT(stable_values, index);
    int mapped;
    (void) integer_from_double(nearbyint(value), &mapped);
    SET_INTEGER_ELT(integer, index, mapped);
  }

  SEXP result = PROTECT(numeric_vector_as_list(integer));
  if (warn_range) {
    Rf_warning("NAs introduced by coercion to integer range");
  }
  UNPROTECT(3);
  return result;
}

SEXP paradox_domain_sanitize_builtin(SEXP param, SEXP values) {
  const domain_info_t info = domain_info(param);
  if (!info.grouped) {
    Rf_error("Corrupt Domain storage: rows must share one grouping");
  }
  if (info.size == 0) {
    if (!zero_length_vector(values)) {
      Rf_error("Cannot sanitize nonempty values against an empty Domain");
    }
    return values;
  }
  R_xlen_t work_since_interrupt = 0;
  paradox_admitted_domain_table_t table;
  PROTECT(paradox_admit_public_domain_table(
    param,
    builtin_domain_kind(info.kind),
    info.size,
    &table,
    &work_since_interrupt
  ));
  if (info.kind == DOMAIN_KIND_FCT || info.kind == DOMAIN_KIND_LGL ||
      info.kind == DOMAIN_KIND_UTY) {
    UNPROTECT(1);
    return values;
  }
  if (zero_length_vector(values)) {
    UNPROTECT(1);
    return values;
  }
  if (Rf_isObject(values) ||
      (TYPEOF(values) != VECSXP && TYPEOF(values) != REALSXP &&
       TYPEOF(values) != INTSXP && TYPEOF(values) != LGLSXP)) {
    Rf_error("`values` must be an unclassed numeric vector or list");
  }
  if (info.kind == DOMAIN_KIND_DBL) {
    SEXP result = PROTECT(sanitize_double(&table, values, &info));
    UNPROTECT(2);
    return result;
  }
  SEXP result = PROTECT(sanitize_integer(values));
  UNPROTECT(2);
  return result;
}

SEXP paradox_domain_property_builtin(SEXP param, SEXP property) {
  if (TYPEOF(property) != INTSXP || XLENGTH(property) != 1) {
    Rf_error("Internal error: unknown Domain property");
  }
  /* One observation, like the ParamSet-table property reader: the selector is
   * validated and used from the same read. */
  const int selector = INTEGER_ELT(property, 0);
  if (selector < 0 || selector >= PARADOX_PROPERTY_COUNT) {
    Rf_error("Internal error: unknown Domain property");
  }
  const paradox_property_t requested = (paradox_property_t) selector;
  const domain_info_t info = domain_info(param);
  /* Keep dispatch independent of the helper calls that receive `&info`. */
  const domain_kind_t kind = info.kind;
  if (!info.grouped) {
    Rf_error("Corrupt Domain storage: rows must share one grouping");
  }
  if (info.size == 0) {
    return Rf_allocVector(
      requested == PARADOX_PROPERTY_NLEVELS ? INTSXP : LGLSXP,
      0
    );
  }

  R_xlen_t work_since_interrupt = 0;
  paradox_admitted_domain_table_t table;
  PROTECT(paradox_admit_public_domain_table(
    param,
    builtin_domain_kind(kind),
    info.size,
    &table,
    &work_since_interrupt
  ));

  if (requested == PARADOX_PROPERTY_IS_NUMBER) {
    SEXP result = Rf_ScalarLogical(
      kind == DOMAIN_KIND_DBL || kind == DOMAIN_KIND_INT
    );
    UNPROTECT(1);
    return result;
  }
  if (requested == PARADOX_PROPERTY_IS_CATEG) {
    SEXP result = Rf_ScalarLogical(
      kind == DOMAIN_KIND_FCT || kind == DOMAIN_KIND_LGL
    );
    UNPROTECT(1);
    return result;
  }

  if (requested == PARADOX_PROPERTY_IS_BOUNDED) {
    SEXP result = PROTECT(Rf_allocVector(LGLSXP, info.size));
    for (R_xlen_t row = 0; row < info.size; ++row) {
      int value;
      if (kind == DOMAIN_KIND_DBL || kind == DOMAIN_KIND_INT) {
        value = R_FINITE(table.lower[row]) && R_FINITE(table.upper[row]);
      } else if (kind == DOMAIN_KIND_FCT || kind == DOMAIN_KIND_LGL) {
        value = TRUE;
      } else {
        value = FALSE;
      }
      SET_LOGICAL_ELT(result, row, value);
    }
    UNPROTECT(2);
    return result;
  }

  SEXP result = PROTECT(Rf_allocVector(REALSXP, info.size));
  for (R_xlen_t row = 0; row < info.size; ++row) {
    double value = R_PosInf;
    switch (kind) {
    case DOMAIN_KIND_EMPTY:
      break;
    case DOMAIN_KIND_DBL:
      value = table.lower[row] == table.upper[row] ? 1.0 : R_PosInf;
      break;
    case DOMAIN_KIND_INT:
      value = paradox_integer_domain_nlevels(
        table.lower[row],
        table.upper[row]
      );
      break;
    case DOMAIN_KIND_FCT: {
      /* The admitted level vector, not a fresh read of the live column: the
       * result carrier above allocated, and only the admitted element is
       * proven canonical. */
      const R_xlen_t level_count = XLENGTH(paradox_admitted_domain_field(
        &table,
        row,
        PARADOX_ADMITTED_LEVELS
      ));
      value = (double) level_count;
      break;
    }
    case DOMAIN_KIND_LGL:
      value = 2.0;
      break;
    case DOMAIN_KIND_UTY:
    case DOMAIN_KIND_UNKNOWN:
      break;
    }
    SET_REAL_ELT(result, row, value);
  }
  UNPROTECT(2);
  return result;
}

static int valid_unit_value(double value) {
  return R_FINITE(value) && value >= 0.0 && value <= 1.0;
}

static double clamp(double value, double lower, double upper) {
  if (!ISNAN(value) && value > upper) {
    value = upper;
  }
  if (!ISNAN(value) && value < lower) {
    value = lower;
  }
  return value;
}

static double r_compatible_affine_map(double unit, double lower, double upper) {
  /* The historical R methods evaluate these operators as separate vector
   * primitives.  On targets with fused multiply-add, contracting the single C
   * expression changes observable doubles and can cross an integer floor
   * boundary.  Volatile automatic intermediates supply portable rounding
   * barriers even when an embedding build enables aggressive contraction;
   * they do not protect shared state. */
  volatile double shifted_unit = unit - 1.0;
  volatile double upper_product = unit * upper;
  volatile double lower_product = shifted_unit * lower;
  return upper_product - lower_product;
}

double paradox_qunif_double_value(double unit, double lower, double upper) {
  /* Preserve every value of a fixed Domain, including +/-Inf.  Handle the
   * closed interval endpoints before the affine expression so a defined
   * endpoint never becomes NaN through 0 * Inf.  The interior of
   * [-Inf, Inf] remains deliberately undefined. */
  if (lower == upper) {
    return lower;
  }
  if (unit == 0.0) {
    return lower;
  }
  if (unit == 1.0) {
    return upper;
  }
  return clamp(
    r_compatible_affine_map(unit, lower, upper),
    lower,
    upper
  );
}

int paradox_qunif_integer_value(double unit, double lower, double upper,
    int *result) {
  double mapped;
  if (lower == upper && R_FINITE(lower)) {
    mapped = lower;
  } else if (unit == 0.0 && R_FINITE(lower)) {
    mapped = lower;
  } else if (unit == 1.0 && R_FINITE(upper)) {
    mapped = upper;
  } else {
    mapped = floor(clamp(
      r_compatible_affine_map(unit, lower, upper + 1.0),
      lower,
      upper
    ));
  }
  return !integer_from_double(mapped, result);
}

R_xlen_t paradox_qunif_level_index(double unit, R_xlen_t level_count) {
  if (!R_FINITE(unit) || unit < 0.0 || unit > 1.0 || level_count <= 0) {
    return R_XLEN_T_MAX;
  }
  double selected = floor(unit * (double) level_count) + 1.0;
  if (selected > (double) level_count) {
    selected = (double) level_count;
  }
  if (selected < 1.0 || selected > (double) R_XLEN_T_MAX) {
    return R_XLEN_T_MAX;
  }
  const R_xlen_t index = (R_xlen_t) selected - 1;
  return index < level_count ? index : R_XLEN_T_MAX;
}

static void copy_logical_structure(SEXP result, SEXP attribute_carrier) {
  SEXP names = PROTECT(Rf_getAttrib(attribute_carrier, R_NamesSymbol));
  SEXP dimensions = PROTECT(Rf_getAttrib(attribute_carrier, R_DimSymbol));
  SEXP dimension_names = PROTECT(Rf_getAttrib(
    attribute_carrier,
    R_DimNamesSymbol
  ));
  if (names != R_NilValue && dimensions == R_NilValue) {
    Rf_setAttrib(result, R_NamesSymbol, names);
  }
  if (dimensions != R_NilValue) {
    Rf_setAttrib(result, R_DimSymbol, dimensions);
  }
  if (dimension_names != R_NilValue) {
    Rf_setAttrib(result, R_DimNamesSymbol, dimension_names);
  }
  UNPROTECT(3);
}

static SEXP qunif_numeric(const paradox_admitted_domain_table_t *table,
    SEXP x, const domain_info_t *info) {
  const R_xlen_t size = XLENGTH(x);
  if (info->kind == DOMAIN_KIND_DBL) {
    const int x_is_altrep = ALTREP(x);
    SEXP attribute_carrier = PROTECT(R_MakeExternalPtr(
      NULL,
      R_NilValue,
      R_NilValue
    ));
    SHALLOW_DUPLICATE_ATTRIB(attribute_carrier, x);
    SEXP dimensions = PROTECT(Rf_getAttrib(attribute_carrier, R_DimSymbol));
    SEXP names = PROTECT(Rf_getAttrib(attribute_carrier, R_NamesSymbol));
    SEXP result = PROTECT(Rf_allocVector(REALSXP, size));
    for (R_xlen_t index = 0; index < size; ++index) {
      periodic_interrupt(index);
      const R_xlen_t row = index % info->size;
      const double value = paradox_numeric_elt(x, index);
      if (!valid_unit_value(value)) {
        Rf_error("`x` must contain only finite values between zero and one");
      }
      const double mapped = paradox_qunif_double_value(
        value,
        table->lower[row],
        table->upper[row]
      );
      SET_REAL_ELT(result, index, mapped);
    }
    if (x_is_altrep) {
      SHALLOW_DUPLICATE_ATTRIB(result, attribute_carrier);
    } else {
      DUPLICATE_ATTRIB(result, attribute_carrier);
    }
    if (dimensions != R_NilValue && names != R_NilValue) {
      Rf_setAttrib(result, R_NamesSymbol, R_NilValue);
    }
    UNPROTECT(4);
    return result;
  }

  SEXP result = PROTECT(Rf_allocVector(INTSXP, size));
  int warn_range = FALSE;
  for (R_xlen_t index = 0; index < size; ++index) {
    periodic_interrupt(index);
    const R_xlen_t row = index % info->size;
    const double value = paradox_numeric_elt(x, index);
    if (!valid_unit_value(value)) {
      Rf_error("`x` must contain only finite values between zero and one");
    }
    int mapped;
    if (!paradox_qunif_integer_value(
          value,
          table->lower[row],
          table->upper[row],
          &mapped
        )) {
      mapped = NA_INTEGER;
      warn_range = TRUE;
    }
    SET_INTEGER_ELT(result, index, mapped);
  }
  if (warn_range) {
    Rf_warning("NAs introduced by coercion to integer range");
  }
  UNPROTECT(1);
  return result;
}

static SEXP qunif_factor(const paradox_admitted_domain_table_t *table,
    SEXP x, const domain_info_t *info) {
  R_xlen_t work_since_interrupt = 0;
  const R_xlen_t size = XLENGTH(x);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_account_work(&work_since_interrupt);
    const R_xlen_t row = index % info->size;
    /* The admitted level vector: canonical levels are ordinary, unique, and
     * non-missing, so the mapping observes exactly what the owner accepted. */
    SEXP choices = paradox_admitted_domain_field(
      table,
      row,
      PARADOX_ADMITTED_LEVELS
    );
    const R_xlen_t n_choices = XLENGTH(choices);
    if (n_choices == 0) {
      Rf_error("Cannot map quantiles for a factor Domain with no levels");
    }
    const double value = paradox_numeric_elt(x, index);
    if (!valid_unit_value(value)) {
      Rf_error("`x` must contain only finite values between zero and one");
    }
    const R_xlen_t selected = paradox_qunif_level_index(value, n_choices);
    if (selected == R_XLEN_T_MAX) {
      Rf_error("Internal error while mapping factor quantiles");
    }
    SET_STRING_ELT(
      result,
      index,
      STRING_ELT(choices, selected)
    );
  }
  UNPROTECT(1);
  return result;
}

static SEXP qunif_logical(SEXP x) {
  const R_xlen_t size = XLENGTH(x);
  SEXP attribute_carrier = PROTECT(R_MakeExternalPtr(
    NULL,
    R_NilValue,
    R_NilValue
  ));
  SHALLOW_DUPLICATE_ATTRIB(attribute_carrier, x);
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    periodic_interrupt(index);
    const double value = paradox_numeric_elt(x, index);
    if (!valid_unit_value(value)) {
      Rf_error("`x` must contain only finite values between zero and one");
    }
    SET_LOGICAL_ELT(result, index, value < 0.5);
  }
  copy_logical_structure(result, attribute_carrier);
  UNPROTECT(2);
  return result;
}

SEXP paradox_domain_qunif_builtin(SEXP param, SEXP x) {
  const domain_info_t info = domain_info(param);
  if (!info.grouped) {
    Rf_error("Corrupt Domain storage: rows must share one grouping");
  }
  if (info.size == 0) {
    return Rf_allocVector(LGLSXP, 0);
  }
  if (Rf_isObject(x) ||
      (TYPEOF(x) != REALSXP && TYPEOF(x) != INTSXP)) {
    Rf_error("`x` must be an unclassed numeric vector");
  }
  if (XLENGTH(x) % info.size != 0) {
    Rf_error("Length of `x` must be a multiple of the number of Domain rows");
  }

  R_xlen_t work_since_interrupt = 0;
  paradox_admitted_domain_table_t table;
  PROTECT(paradox_admit_public_domain_table(
    param,
    builtin_domain_kind(info.kind),
    info.size,
    &table,
    &work_since_interrupt
  ));
  SEXP result;
  switch (info.kind) {
  case DOMAIN_KIND_EMPTY:
    break;
  case DOMAIN_KIND_DBL:
  case DOMAIN_KIND_INT:
    result = PROTECT(qunif_numeric(&table, x, &info));
    UNPROTECT(2);
    return result;
  case DOMAIN_KIND_FCT:
    result = PROTECT(qunif_factor(&table, x, &info));
    UNPROTECT(2);
    return result;
  case DOMAIN_KIND_LGL:
    result = PROTECT(qunif_logical(x));
    UNPROTECT(2);
    return result;
  case DOMAIN_KIND_UTY:
    UNPROTECT(1);
    Rf_error("Quantile mapping is undefined for ParamUty Domains");
  case DOMAIN_KIND_UNKNOWN:
    break;
  }
  UNPROTECT(1);
  return R_NilValue;
}
