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
} domain_shape_t;

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
  SEXP classes = R_NilValue;
  if (!paradox_api_ordinary_class_snapshot(param, &classes) ||
      TYPEOF(classes) != STRSXP || ALTREP(classes) ||
      Rf_isS4(classes) || Rf_isObject(classes) ||
      !paradox_api_has_no_attributes(classes)) {
    return DOMAIN_KIND_UNKNOWN;
  }

  if (XLENGTH(classes) == 2 &&
      paradox_domain_string_is(STRING_ELT(classes, 0), "data.table") &&
      paradox_domain_string_is(STRING_ELT(classes, 1), "data.frame")) {
    return DOMAIN_KIND_EMPTY;
  }
  if (XLENGTH(classes) != 4) {
    return DOMAIN_KIND_UNKNOWN;
  }

  static const char *const tail[] = {
    "Domain", "data.table", "data.frame"
  };
  for (R_xlen_t index = 0; index < 3; ++index) {
    SEXP value = STRING_ELT(classes, index + 1);
    if (value == NA_STRING || strcmp(CHAR(value), tail[index]) != 0) {
      return DOMAIN_KIND_UNKNOWN;
    }
  }

  SEXP first = STRING_ELT(classes, 0);
  if (first == NA_STRING) {
    return DOMAIN_KIND_UNKNOWN;
  }
  const char *name = CHAR(first);
  if (strcmp(name, "ParamDbl") == 0) {
    return DOMAIN_KIND_DBL;
  }
  if (strcmp(name, "ParamInt") == 0) {
    return DOMAIN_KIND_INT;
  }
  if (strcmp(name, "ParamFct") == 0) {
    return DOMAIN_KIND_FCT;
  }
  if (strcmp(name, "ParamLgl") == 0) {
    return DOMAIN_KIND_LGL;
  }
  if (strcmp(name, "ParamUty") == 0) {
    return DOMAIN_KIND_UTY;
  }
  return DOMAIN_KIND_UNKNOWN;
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

typedef struct {
  SEXP names;
  SEXP classes;
  SEXP row_names;
  SEXP selfref;
  SEXP selfref_symbol;
  int valid;
} empty_domain_metadata_t;

static void capture_empty_domain_attribute(
    SEXP tag, SEXP value, void *data) {
  empty_domain_metadata_t *metadata = data;
  if (!metadata->valid || TYPEOF(tag) != SYMSXP ||
      value == R_NilValue) {
    metadata->valid = FALSE;
    return;
  }
  SEXP *destination = NULL;
  if (tag == R_NamesSymbol) {
    destination = &metadata->names;
  } else if (tag == R_ClassSymbol) {
    destination = &metadata->classes;
  } else if (tag == R_RowNamesSymbol) {
    destination = &metadata->row_names;
  } else if (tag == metadata->selfref_symbol) {
    destination = &metadata->selfref;
  } else {
    metadata->valid = FALSE;
    return;
  }
  if (*destination != R_NilValue) {
    metadata->valid = FALSE;
    return;
  }
  *destination = value;
}

static void validate_empty_domain(SEXP param) {
  static const int column_types[] = {
    STRSXP, STRSXP, STRSXP, VECSXP, REALSXP, REALSXP, REALSXP, VECSXP,
    VECSXP, VECSXP, STRSXP, STRSXP, VECSXP, VECSXP, LGLSXP, VECSXP
  };
  const R_xlen_t column_count = (R_xlen_t) (
    PARADOX_DOMAIN_COLUMN_COUNT
  );
  /*
   * Intern the sole non-global tag before selecting caller-owned state.  One
   * bounded mapper then proves and captures the complete four-cell metadata
   * generation; in particular, neither special row-name lookup nor a later
   * raw selector can allocate and let an old-R finalizer splice generations.
   */
  SEXP selfref_symbol = Rf_install(".internal.selfref");
  if (TYPEOF(param) != VECSXP || ALTREP(param) || Rf_isS4(param) ||
      XLENGTH(param) != column_count) {
    Rf_error("Corrupt empty Domain storage");
  }
  empty_domain_metadata_t metadata = {
    R_NilValue,
    R_NilValue,
    R_NilValue,
    R_NilValue,
    selfref_symbol,
    TRUE
  };
  R_xlen_t attribute_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
    param,
    4,
    capture_empty_domain_attribute,
    &metadata,
    &attribute_count
  ) || !metadata.valid || attribute_count != 4 ||
      metadata.names == R_NilValue ||
      metadata.classes == R_NilValue ||
      metadata.row_names == R_NilValue ||
      metadata.selfref == R_NilValue) {
    Rf_error("Corrupt empty Domain storage");
  }
  SEXP names = PROTECT(metadata.names);
  SEXP classes = PROTECT(metadata.classes);
  SEXP row_names = PROTECT(metadata.row_names);
  SEXP selfref = PROTECT(metadata.selfref);
  if (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
      Rf_isObject(names) ||
      !paradox_api_has_no_attributes(names) ||
      XLENGTH(names) != column_count ||
      TYPEOF(classes) != STRSXP || ALTREP(classes) ||
      Rf_isS4(classes) || Rf_isObject(classes) ||
      !paradox_api_has_no_attributes(classes) ||
      XLENGTH(classes) != 2 ||
      !paradox_domain_string_is(STRING_ELT(classes, 0), "data.table") ||
      !paradox_domain_string_is(STRING_ELT(classes, 1), "data.frame") ||
      TYPEOF(row_names) != INTSXP ||
      ALTREP(row_names) || Rf_isS4(row_names) ||
      Rf_isObject(row_names) ||
      !paradox_api_has_no_attributes(row_names) ||
      XLENGTH(row_names) != 0 || TYPEOF(selfref) != EXTPTRSXP ||
      Rf_isS4(selfref)) {
    UNPROTECT(4);
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
      UNPROTECT(4);
      Rf_error("Corrupt empty Domain storage");
    }
  }
  UNPROTECT(4);
}

/*
 * Pre-callback shape probe.  The canonical row adapter below is the sole full
 * Domain admission owner; kernels need only the closed outer kind and the
 * ordinary ID carrier's length before they can size value-side work.  Passing
 * both values into that adapter makes a callback-induced shape replacement
 * fail there without repeating class/storage/grouping and per-row scans here.
 */
static domain_shape_t domain_shape(SEXP param) {
  const domain_kind_t kind = class_kind(param);
  if (kind == DOMAIN_KIND_UNKNOWN) {
    Rf_error(
      "Unsupported Domain class; expected one of ParamDbl, ParamInt, "
      "ParamFct, ParamLgl, or ParamUty"
    );
  }
  if (kind == DOMAIN_KIND_EMPTY) {
    validate_empty_domain(param);
    const domain_shape_t empty = {DOMAIN_KIND_EMPTY, 0};
    return empty;
  }

  SEXP columns[PARADOX_DOMAIN_COLUMN_COUNT];
  paradox_domain_select_columns(
    param,
    "Domain storage",
    "Domain",
    1U << PARADOX_DOMAIN_ID,
    columns
  );
  SEXP ids = PROTECT(columns[PARADOX_DOMAIN_ID]);
  if (TYPEOF(ids) != STRSXP) {
    Rf_error("Corrupt Domain storage: `id` must have type `character`");
  }
  if (ALTREP(ids)) {
    Rf_error(
      "Corrupt Domain storage: `id` must use an ordinary character representation"
    );
  }
  const R_xlen_t size = XLENGTH(ids);
  paradox_require_column_checked(
    ids,
    STRSXP,
    size,
    "Domain storage",
    "id"
  );
  const domain_shape_t result = {kind, size};
  UNPROTECT(1);
  return result;
}

/*
 * Typed zero-row Domains and operations with an empty value input have no row
 * loop in which to perform canonical admission. They must still cross the
 * public-table boundary: the complete sixteen-column outward schema is
 * structural even when there is no semantic row to interpret. This helper is
 * confined to those cold exits; ordinary nonempty kernels retain their
 * existing single admission and hot path.
 */
static void admit_domain_before_empty_exit(SEXP param,
    const domain_shape_t *info, unsigned int interpreted) {
  if (info->kind == DOMAIN_KIND_EMPTY) {
    /* `domain_shape()` already ran the dedicated exact empty-Domain validator. */
    return;
  }
  R_xlen_t work_since_interrupt = 0;
  paradox_admitted_domain_table_t table;
  PROTECT(paradox_admit_public_domain_table(
    param,
    builtin_domain_kind(info->kind),
    info->size,
    interpreted,
    &table,
    &work_since_interrupt
  ));
  UNPROTECT(1);
}

static int bounded_metadata_is_unclassed(SEXP value) {
  int has_class = FALSE;
  return paradox_bounded_metadata_has_tag(
      value,
      R_ClassSymbol,
      &has_class
    ) && !has_class;
}

static int numeric_scalar_altrep_current(SEXP value, SEXPTYPE type) {
  return (SEXPTYPE) TYPEOF(value) == type &&
    !Rf_isS4(value) && !Rf_isObject(value) &&
    bounded_metadata_is_unclassed(value);
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

  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  const int altrep = ALTREP(value);
  switch (type) {
  case REALSXP: {
    if (XLENGTH(value) != 1 ||
        (altrep && !numeric_scalar_altrep_current(value, type))) {
      return FALSE;
    }
    const double element = REAL_ELT(value, 0);
    if (altrep && !numeric_scalar_altrep_current(value, type)) {
      return FALSE;
    }
    *result = element;
    return TRUE;
  }
  case INTSXP: {
    if (XLENGTH(value) != 1 ||
        (altrep && !numeric_scalar_altrep_current(value, type))) {
      return FALSE;
    }
    const int element = INTEGER_ELT(value, 0);
    if (altrep && !numeric_scalar_altrep_current(value, type)) {
      return FALSE;
    }
    *result = element == NA_INTEGER ? NA_REAL : (double) element;
    return TRUE;
  }
  case LGLSXP: {
    if (!allow_logical || XLENGTH(value) != 1 ||
        (altrep && !numeric_scalar_altrep_current(value, type))) {
      return FALSE;
    }
    const int element = LOGICAL_ELT(value, 0);
    if (altrep && !numeric_scalar_altrep_current(value, type)) {
      return FALSE;
    }
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

static int ordinary_list_argument_shell(SEXP values) {
  return TYPEOF(values) == VECSXP && !ALTREP(values) &&
    !Rf_isS4(values) &&
    !Rf_isObject(values) &&
    bounded_metadata_is_unclassed(values);
}

static void require_ordinary_list_argument_shell(SEXP values,
    R_xlen_t expected_size) {
  if (!ordinary_list_argument_shell(values) ||
      XLENGTH(values) != expected_size) {
    Rf_error("`values` must be an ordinary list with one element per Domain row");
  }
}

static SEXP stable_list_source_receipt(SEXP stable,
    R_xlen_t expected_size) {
  SEXP receipt = VECTOR_ELT(stable, expected_size);
  return receipt == R_NilValue ? stable : receipt;
}

static void require_stable_list_source(SEXP values, SEXP stable,
    R_xlen_t expected_size) {
  require_ordinary_list_argument_shell(values, expected_size);
  SEXP receipt = stable_list_source_receipt(stable, expected_size);
  for (R_xlen_t row = 0; row < expected_size; ++row) {
    if (VECTOR_ELT(values, row) != VECTOR_ELT(receipt, row)) {
      Rf_error("`values` changed during Domain value admission");
    }
  }
}

static SEXP stable_list_argument(SEXP values, R_xlen_t expected_size,
    domain_kind_t kind) {
  require_ordinary_list_argument_shell(values, expected_size);

  if (expected_size == R_XLEN_T_MAX) {
    Rf_error("`values` has an unsupported length");
  }
  /*
   * The final slot owns a rare-path source receipt when a typed ALTREP leaf
   * must be materialized. Ordinary and ParamUty paths retain their captured
   * source pointers directly in the first `expected_size` slots, so they keep
   * one allocation and pay only one additional pointer of storage.
   */
  SEXP stable = PROTECT(Rf_allocVector(VECSXP, expected_size + 1));
  /* Allocation may run a pending finalizer. Select the complete valid source
   * generation only after the destination that will own it exists. */
  require_ordinary_list_argument_shell(values, expected_size);
  int materialize_any = FALSE;
  for (R_xlen_t row = 0; row < expected_size; ++row) {
    periodic_interrupt(row);
    SEXP value = VECTOR_ELT(values, row);
    SET_VECTOR_ELT(stable, row, value);
    if (kind != DOMAIN_KIND_UTY && ALTREP(value)) {
      if (TYPEOF(value) == VECSXP) {
        UNPROTECT(1);
        Rf_error(
          "Typed Domain values may use ALTREP only for atomic vectors"
        );
      }
      materialize_any = TRUE;
    }
  }

  if (materialize_any) {
    SEXP receipt = PROTECT(Rf_allocVector(VECSXP, expected_size));
    SET_VECTOR_ELT(stable, expected_size, receipt);
    for (R_xlen_t row = 0; row < expected_size; ++row) {
      SET_VECTOR_ELT(receipt, row, VECTOR_ELT(stable, row));
    }
    /*
     * The receipt allocation itself may have run a finalizer. Refuse that
     * splice before invoking any ALTREP Elt method from the older captured
     * generation.
     */
    require_stable_list_source(values, stable, expected_size);
    for (R_xlen_t row = 0; row < expected_size; ++row) {
      periodic_interrupt(row);
      SEXP source = VECTOR_ELT(receipt, row);
      if (ALTREP(source)) {
        SEXP snapshot = PROTECT(
          paradox_snapshot_builtin_value_leaf(source)
        );
        SET_VECTOR_ELT(stable, row, snapshot);
        UNPROTECT(1);
      }
    }
    /*
     * An ALTREP value may reenter R and mutate the caller-owned ordinary
     * outer shell. Reject that splice immediately. Ordinary/ParamUty paths
     * have no callback window here and defer their one necessary receipt to
     * the post-Domain-admission check in the caller.
     */
    require_stable_list_source(values, stable, expected_size);
    UNPROTECT(1);
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
    const domain_shape_t *info, int internal) {
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
    const domain_shape_t *info, const int *skip) {
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
        paradox_admitted_domain_field(table, row, PARADOX_ADMITTED_ID),
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
    const domain_shape_t *info, const int *skip) {
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
        paradox_admitted_domain_field(table, row, PARADOX_ADMITTED_ID),
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
    const domain_shape_t *info, const int *skip) {
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
        paradox_admitted_domain_field(table, row, PARADOX_ADMITTED_ID),
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
    const domain_shape_t *info) {
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
    const domain_shape_t *info, const int *skip) {
  SEXP callbacks = PROTECT(snapshot_utility_callbacks(table, info));
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
    SEXP id = paradox_admitted_domain_field(
      table,
      row,
      PARADOX_ADMITTED_ID
    );
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

  /*
   * A list is a structural value carrier at this boundary. Reject dispatch-
   * bearing shells before the empty-value probe can invoke an ALTREP Length
   * method; non-list zero-length vectors retain their established empty-input
   * compatibility below.
   */
  if (TYPEOF(values) == VECSXP &&
      !ordinary_list_argument_shell(values)) {
    Rf_error("`values` must be an ordinary list with one element per Domain row");
  }

  const domain_shape_t info = domain_shape(param);
  /*
   * Length is an observable ALTREP method. The adapter below receives the
   * already observed kind and size: a callback may replace same-shaped
   * semantics, which become the admitted generation, but cannot splice in a
   * different shape.
   */
  const int empty_values = zero_length_vector(values);
  if (info.size == 0 || empty_values) {
    admit_domain_before_empty_exit(
      param,
      &info,
      PARADOX_DOMAIN_INTERPRET_ALL
    );
  }
  if (empty_values) {
    return Rf_ScalarLogical(TRUE);
  }
  if (info.size == 0) {
    Rf_error("Cannot check nonempty values against an empty Domain");
  }
  SEXP stable_values = PROTECT(stable_list_argument(
    values,
    info.size,
    info.kind
  ));
  /*
   * Route the complete outward Domain through the canonical row owner before
   * any operation-specific work. `check` is the one operation that certifies
   * the entire row -- the special-value fast path below may skip a row's
   * value check, but it may not skip any part of that row's admission.
   */
  R_xlen_t work_since_interrupt = 0;
  paradox_admitted_domain_table_t table;
  PROTECT(paradox_admit_public_domain_table(
    param,
    builtin_domain_kind(info.kind),
    info.size,
    PARADOX_DOMAIN_INTERPRET_ALL,
    &table,
    &work_since_interrupt
  ));
  /*
   * Domain ownership can allocate after the value snapshot. A pending
   * finalizer may therefore mutate the caller-owned outer shell even though
   * no value callback ran. Retained per-row selections remain authoritative,
   * but their structural source must still be the admitted ordinary list
   * shell immediately before they are consumed.
   */
  require_stable_list_source(values, stable_values, info.size);
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

static int sanitize_noop_list_shell(SEXP values) {
  return TYPEOF(values) == VECSXP && !ALTREP(values) &&
    !Rf_isS4(values);
}

static int sanitize_numeric_values_shell(SEXP values) {
  if (Rf_isS4(values) || Rf_isObject(values) ||
      !bounded_metadata_is_unclassed(values)) {
    return FALSE;
  }
  switch ((SEXPTYPE) TYPEOF(values)) {
  case VECSXP:
    return !ALTREP(values);
  case REALSXP:
  case INTSXP:
  case LGLSXP:
    return TRUE;
  default:
    return FALSE;
  }
}

static void require_sanitize_numeric_values_shell(SEXP values) {
  if (!sanitize_numeric_values_shell(values)) {
    Rf_error("`values` must be an unclassed numeric vector or list");
  }
}

static void require_sanitize_numeric_values_current(SEXP values,
    R_xlen_t size) {
  require_sanitize_numeric_values_shell(values);
  /* Stable semantic atomic ALTREP gets exactly the already selected Length.
   * Every other accepted carrier is ordinary, so this repeat is
   * allocation-free and cannot dispatch. */
  if (!ALTREP(values) && XLENGTH(values) != size) {
    Rf_error("`values` changed during Domain sanitization");
  }
}

typedef struct {
  SEXPTYPE type;
  double real_value;
  int integer_value;
} sanitize_scalar_receipt_t;

static int capture_ordinary_sanitize_scalar(
    SEXP value, sanitize_scalar_receipt_t *receipt, double *converted) {
  if (ALTREP(value) || Rf_isS4(value) || Rf_isObject(value) ||
      !bounded_metadata_is_unclassed(value) ||
      XLENGTH(value) != 1) {
    return FALSE;
  }
  receipt->type = (SEXPTYPE) TYPEOF(value);
  switch (receipt->type) {
  case REALSXP:
    receipt->real_value = REAL_ELT(value, 0);
    *converted = receipt->real_value;
    return TRUE;
  case INTSXP:
    receipt->integer_value = INTEGER_ELT(value, 0);
    *converted = receipt->integer_value == NA_INTEGER
      ? NA_REAL
      : (double) receipt->integer_value;
    return TRUE;
  case LGLSXP:
    receipt->integer_value = LOGICAL_ELT(value, 0);
    *converted = receipt->integer_value == NA_LOGICAL
      ? NA_REAL
      : (double) receipt->integer_value;
    return TRUE;
  default:
    return FALSE;
  }
}

static int ordinary_sanitize_scalar_receipt_current(
    SEXP value, const sanitize_scalar_receipt_t *receipt) {
  if (ALTREP(value) || (SEXPTYPE) TYPEOF(value) != receipt->type ||
      Rf_isS4(value) || Rf_isObject(value) ||
      !bounded_metadata_is_unclassed(value) ||
      XLENGTH(value) != 1) {
    return FALSE;
  }
  switch (receipt->type) {
  case REALSXP: {
    const double current = REAL_ELT(value, 0);
    return memcmp(
      &current,
      &receipt->real_value,
      sizeof(current)
    ) == 0;
  }
  case INTSXP:
    return INTEGER_ELT(value, 0) == receipt->integer_value;
  case LGLSXP:
    return LOGICAL_ELT(value, 0) == receipt->integer_value;
  default:
    return FALSE;
  }
}

static void require_sanitize_list_source(SEXP values, SEXP receipt,
    const sanitize_scalar_receipt_t *scalar_receipts, R_xlen_t size) {
  require_sanitize_numeric_values_current(values, size);
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP source = VECTOR_ELT(receipt, index);
    if (VECTOR_ELT(values, index) != source) {
      Rf_error("`values` changed during Domain sanitization");
    }
    const SEXPTYPE type = scalar_receipts[index].type;
    const int current = ALTREP(source)
      ? (SEXPTYPE) TYPEOF(source) == type &&
        (type == REALSXP || type == INTSXP || type == LGLSXP) &&
        numeric_scalar_altrep_current(source, type)
      : ordinary_sanitize_scalar_receipt_current(
          source,
          &scalar_receipts[index]
        );
    if (!current) {
      Rf_error("`values` changed during Domain sanitization");
    }
  }
}

static SEXP snapshot_sanitize_numeric_values(SEXP values,
    R_xlen_t value_size) {
  /*
   * Allocate the semantic destination before selecting any caller-owned
   * element. A pending finalizer therefore either contributes to the
   * generation selected below or is rejected by its terminal receipt.
   */
  SEXP stable_values = PROTECT(Rf_allocVector(REALSXP, value_size));
  require_sanitize_numeric_values_current(values, value_size);

  if (TYPEOF(values) != VECSXP) {
    for (R_xlen_t index = 0; index < value_size; ++index) {
      periodic_interrupt(index);
      double value;
      if (!vector_numeric_at(values, index, &value)) {
        UNPROTECT(1);
        Rf_error("`values` must contain only numeric scalar values");
      }
      SET_REAL_ELT(stable_values, index, value);
    }
    UNPROTECT(1);
    return stable_values;
  }

  int has_altrep = FALSE;
  for (R_xlen_t index = 0; index < value_size; ++index) {
    periodic_interrupt(index);
    SEXP source = VECTOR_ELT(values, index);
    if (ALTREP(source)) {
      if (TYPEOF(source) == VECSXP) {
        UNPROTECT(1);
        Rf_error(
          "Numeric Domain values may use ALTREP only for atomic vectors"
        );
      }
      has_altrep = TRUE;
    }
  }

  if (!has_altrep) {
    /* No observation in this loop can call back or allocate, so the ordinary
     * path needs no source carrier beyond the caller-owned list itself. */
    for (R_xlen_t index = 0; index < value_size; ++index) {
      periodic_interrupt(index);
      double value;
      if (!numeric_scalar(VECTOR_ELT(values, index), &value, TRUE)) {
        UNPROTECT(1);
        Rf_error("`values` must contain only numeric scalar values");
      }
      SET_REAL_ELT(stable_values, index, value);
    }
    UNPROTECT(1);
    return stable_values;
  }

  /*
   * Only the callback-capable path allocates a source receipt. Select its
   * complete generation after that allocation, root every row before
   * observing one ALTREP leaf, and then require all outer pointers to remain
   * that generation.
   */
  SEXP receipt = PROTECT(Rf_allocVector(VECSXP, value_size));
  sanitize_scalar_receipt_t *scalar_receipts = paradox_temporary_alloc(
    value_size == 0 ? 1 : value_size,
    sizeof(*scalar_receipts)
  );
  require_sanitize_numeric_values_current(values, value_size);
  for (R_xlen_t index = 0; index < value_size; ++index) {
    SET_VECTOR_ELT(receipt, index, VECTOR_ELT(values, index));
  }
  /*
   * Own the exact type/length/value bits of every ordinary scalar before the
   * first ALTREP callback. Pointer identity alone cannot detect an in-place
   * data.table-style write to an earlier scalar leaf.
   */
  for (R_xlen_t index = 0; index < value_size; ++index) {
    SEXP source = VECTOR_ELT(receipt, index);
    scalar_receipts[index].type = (SEXPTYPE) TYPEOF(source);
    if (!ALTREP(source)) {
      double value;
      if (!capture_ordinary_sanitize_scalar(
          source,
          &scalar_receipts[index],
          &value
        )) {
        UNPROTECT(2);
        Rf_error("`values` must contain only numeric scalar values");
      }
      SET_REAL_ELT(stable_values, index, value);
    } else if ((scalar_receipts[index].type != REALSXP &&
        scalar_receipts[index].type != INTSXP &&
        scalar_receipts[index].type != LGLSXP) ||
        !numeric_scalar_altrep_current(
          source,
          scalar_receipts[index].type
        )) {
      UNPROTECT(2);
      Rf_error("`values` must contain only numeric scalar values");
    }
  }
  for (R_xlen_t index = 0; index < value_size; ++index) {
    periodic_interrupt(index);
    SEXP source = VECTOR_ELT(receipt, index);
    if (!ALTREP(source)) continue;
    double value;
    if (!numeric_scalar(source, &value, TRUE)) {
      UNPROTECT(2);
      Rf_error("`values` must contain only numeric scalar values");
    }
    SET_REAL_ELT(stable_values, index, value);
  }
  require_sanitize_list_source(
    values,
    receipt,
    scalar_receipts,
    value_size
  );
  UNPROTECT(2);
  return stable_values;
}

static SEXP sanitize_double(const paradox_admitted_domain_table_t *table,
    SEXP stable_values, R_xlen_t value_size, const domain_shape_t *info) {
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
  UNPROTECT(2);
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

static SEXP sanitize_integer(SEXP stable_values, R_xlen_t size) {
  SEXP integer = PROTECT(Rf_allocVector(INTSXP, size));
  int warn_range = FALSE;
  for (R_xlen_t index = 0; index < size; ++index) {
    periodic_interrupt(index);
    const double value = REAL_ELT(stable_values, index);
    int mapped;
    if (integer_from_double(nearbyint(value), &mapped)) {
      warn_range = TRUE;
    }
    SET_INTEGER_ELT(integer, index, mapped);
  }

  SEXP result = PROTECT(numeric_vector_as_list(integer));
  if (warn_range) {
    Rf_warning("NAs introduced by coercion to integer range");
  }
  UNPROTECT(2);
  return result;
}

SEXP paradox_domain_sanitize_builtin(SEXP param, SEXP values) {
  /* A list is structural even for empty and categorical no-op paths. Reject
   * ALTREP/S4 shells before any Length/Elt observation. Classed ordinary
   * lists remain opaque identity-bearing values on ParamUty and the other
   * categorical no-op paths. */
  if (TYPEOF(values) == VECSXP &&
      !sanitize_noop_list_shell(values)) {
    Rf_error("`values` must be an unclassed numeric vector or list");
  }

  const domain_shape_t info = domain_shape(param);
  if (info.size == 0) {
    /*
     * Select the complete Domain generation before observing a value-side
     * ALTREP Length. A typed zero-row Domain has an ordinary admitted bundle;
     * keep it rooted through the return. The canonical empty Domain has no
     * rows to detach, so its dedicated exact validator is the admission
     * boundary.
     */
    int protect_count = 0;
    if (info.kind == DOMAIN_KIND_EMPTY) {
      validate_empty_domain(param);
    } else {
      R_xlen_t work_since_interrupt = 0;
      paradox_admitted_domain_table_t table;
      PROTECT(paradox_admit_public_domain_table(
        param,
        builtin_domain_kind(info.kind),
        info.size,
        PARADOX_DOMAIN_INTERPRET_BOUNDS,
        &table,
        &work_since_interrupt
      ));
      ++protect_count;
    }
    const int empty_values = zero_length_vector(values);
    if (TYPEOF(values) == VECSXP &&
        !sanitize_noop_list_shell(values)) {
      UNPROTECT(protect_count);
      Rf_error("`values` must be an unclassed numeric vector or list");
    }
    if (!empty_values) {
      UNPROTECT(protect_count);
      Rf_error("Cannot sanitize nonempty values against an empty Domain");
    }
    UNPROTECT(protect_count);
    return values;
  }

  if (info.kind == DOMAIN_KIND_FCT || info.kind == DOMAIN_KIND_LGL ||
      info.kind == DOMAIN_KIND_UTY) {
    R_xlen_t work_since_interrupt = 0;
    paradox_admitted_domain_table_t table;
    PROTECT(paradox_admit_public_domain_table(
      param,
      builtin_domain_kind(info.kind),
      info.size,
      PARADOX_DOMAIN_INTERPRET_BOUNDS,
      &table,
      &work_since_interrupt
    ));
    if (TYPEOF(values) == VECSXP &&
        !sanitize_noop_list_shell(values)) {
      Rf_error("`values` must be an unclassed numeric vector or list");
    }
    UNPROTECT(1);
    return values;
  }

  /*
   * Unlike check/qunif, sanitization can select its complete Domain before
   * any value-side Length or Elt method. Keep that exact admitted bundle
   * rooted throughout value materialization and output construction: a
   * callback may mutate the live table, but cannot replace the enclosing
   * operation's already selected bounds.
   */
  R_xlen_t work_since_interrupt = 0;
  paradox_admitted_domain_table_t table;
  PROTECT(paradox_admit_public_domain_table(
    param,
    builtin_domain_kind(info.kind),
    info.size,
    PARADOX_DOMAIN_INTERPRET_BOUNDS,
    &table,
    &work_since_interrupt
  ));

  require_sanitize_numeric_values_shell(values);
  const R_xlen_t value_size = XLENGTH(values);
  require_sanitize_numeric_values_current(values, value_size);
  SEXP stable_values = PROTECT(snapshot_sanitize_numeric_values(
    values,
    value_size
  ));

  require_sanitize_numeric_values_current(values, value_size);
  if (value_size == 0) {
    UNPROTECT(2);
    return values;
  }
  if (info.kind == DOMAIN_KIND_DBL) {
    SEXP result = PROTECT(sanitize_double(
      &table,
      stable_values,
      value_size,
      &info
    ));
    UNPROTECT(3);
    return result;
  }
  SEXP result = PROTECT(sanitize_integer(stable_values, value_size));
  UNPROTECT(3);
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
  const domain_shape_t info = domain_shape(param);
  /* Keep dispatch independent of the helper calls that receive `&info`. */
  const domain_kind_t kind = info.kind;

  /* Each property declares what it reads: the kind alone for the class
   * predicates, the numeric schema for boundedness, and for level counts the
   * numeric schema (ParamInt) together with the levels (ParamFct). */
  unsigned int interpreted = PARADOX_DOMAIN_INTERPRET_NONE;
  if (requested == PARADOX_PROPERTY_IS_BOUNDED) {
    interpreted = PARADOX_DOMAIN_INTERPRET_BOUNDS;
  } else if (requested == PARADOX_PROPERTY_NLEVELS) {
    interpreted =
      PARADOX_DOMAIN_INTERPRET_BOUNDS | PARADOX_DOMAIN_INTERPRET_LEVELS;
  }
  if (info.size == 0) {
    admit_domain_before_empty_exit(param, &info, interpreted);
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
    interpreted,
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

static void require_unclassed_qunif_source_shell(SEXP x) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(x);
  if ((type != REALSXP && type != INTSXP) ||
      Rf_isS4(x) || Rf_isObject(x)) {
    Rf_error("`x` must be an unclassed numeric vector");
  }
  int has_class = FALSE;
  if (!paradox_bounded_metadata_has_tag(
      x,
      R_ClassSymbol,
      &has_class
    )) {
    Rf_error(
      "`x` must have ordinary, acyclic, bounded metadata"
    );
  }
  if (has_class) {
    Rf_error("`x` must be an unclassed numeric vector");
  }
}

static void require_unclassed_qunif_selected_shell(SEXP selected) {
  /*
   * The fresh result owns the bounded, shallowly selected attribute
   * generation. Checking its flags and raw class attribute is allocation-free
   * and cannot dispatch. A finalizer may have changed `x` during result or
   * metadata-carrier allocation; this selected result generation, rather than
   * an earlier live-source observation, is the generation the caller sees.
   */
  if (Rf_isS4(selected) || Rf_isObject(selected) ||
      paradox_stored_attribute(selected, R_ClassSymbol) != R_NilValue) {
    Rf_error("`x` must be an unclassed numeric vector");
  }
}

static SEXP qunif_numeric(const paradox_admitted_domain_table_t *table,
    SEXP x, R_xlen_t size, const domain_shape_t *info) {
  if (info->kind == DOMAIN_KIND_DBL) {
    SEXP result = PROTECT(Rf_allocVector(REALSXP, size));
    /*
     * Result allocation precedes attribute selection. If it runs a finalizer
     * which classes `x`, the post-allocation shell check rejects it. The
     * bounded copier then freezes one exact shallow metadata generation
     * directly on the fresh result; its canonical attr-free path allocates
     * nothing. After this point the direct Elt loop allocates nothing.
     */
    require_unclassed_qunif_source_shell(x);
    paradox_copy_bounded_shallow_attributes(
      result,
      x,
      PARADOX_SHALLOW_ATTRIBUTES_ALL,
      "`x` must be an unclassed numeric vector with ordinary, acyclic, "
        "bounded metadata"
    );
    require_unclassed_qunif_selected_shell(result);
    require_unclassed_qunif_source_shell(x);
    const int drops_names =
      paradox_stored_attribute(result, R_DimSymbol) != R_NilValue &&
      paradox_stored_attribute(result, R_NamesSymbol) != R_NilValue;
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
    /* Presentation metadata is shallowly selected, not recursively owned.
     * Exact nested values retain R's ordinary copy-on-write aliasing without
     * exposing an adversarial top-level pairlist to R's duplicator. */
    if (drops_names) {
      Rf_setAttrib(result, R_NamesSymbol, R_NilValue);
    }
    UNPROTECT(1);
    return result;
  }

  SEXP result = PROTECT(Rf_allocVector(INTSXP, size));
  require_unclassed_qunif_source_shell(x);
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
    SEXP x, R_xlen_t size, const domain_shape_t *info) {
  R_xlen_t work_since_interrupt = 0;
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  require_unclassed_qunif_source_shell(x);
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

static SEXP qunif_logical(SEXP x, R_xlen_t size) {
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, size));
  require_unclassed_qunif_source_shell(x);
  paradox_copy_bounded_shallow_attributes(
    result,
    x,
    PARADOX_SHALLOW_ATTRIBUTES_LOGICAL_STRUCTURE,
    "`x` must be an unclassed numeric vector with ordinary, acyclic, "
      "bounded metadata"
  );
  require_unclassed_qunif_selected_shell(result);
  require_unclassed_qunif_source_shell(x);
  for (R_xlen_t index = 0; index < size; ++index) {
    periodic_interrupt(index);
    const double value = paradox_numeric_elt(x, index);
    if (!valid_unit_value(value)) {
      Rf_error("`x` must contain only finite values between zero and one");
    }
    SET_LOGICAL_ELT(result, index, value < 0.5);
  }
  UNPROTECT(1);
  return result;
}

SEXP paradox_domain_qunif_builtin(SEXP param, SEXP x) {
  const domain_shape_t info = domain_shape(param);
  if (info.size == 0) {
    admit_domain_before_empty_exit(
      param,
      &info,
      PARADOX_DOMAIN_INTERPRET_BOUNDS | PARADOX_DOMAIN_INTERPRET_LEVELS
    );
    return Rf_allocVector(LGLSXP, 0);
  }
  require_unclassed_qunif_source_shell(x);
  /*
   * Length is the only semantic observation before complete Domain
   * admission. The adapter receives the already observed kind and size, so a
   * stateful ALTREP may replace same-shaped semantics but cannot splice in a
   * different shape.
   */
  const R_xlen_t size = XLENGTH(x);
  require_unclassed_qunif_source_shell(x);
  if (size % info.size != 0) {
    Rf_error("Length of `x` must be a multiple of the number of Domain rows");
  }

  R_xlen_t work_since_interrupt = 0;
  paradox_admitted_domain_table_t table;
  /* Quantile mapping reads the numeric schema (dbl/int) or the levels
   * (fct); the mask is kind-independent so every branch is covered. */
  PROTECT(paradox_admit_public_domain_table(
    param,
    builtin_domain_kind(info.kind),
    info.size,
    PARADOX_DOMAIN_INTERPRET_BOUNDS | PARADOX_DOMAIN_INTERPRET_LEVELS,
    &table,
    &work_since_interrupt
  ));
  require_unclassed_qunif_source_shell(x);
  SEXP result;
  switch (info.kind) {
  case DOMAIN_KIND_EMPTY:
    break;
  case DOMAIN_KIND_DBL:
  case DOMAIN_KIND_INT:
    result = PROTECT(qunif_numeric(&table, x, size, &info));
    UNPROTECT(2);
    return result;
  case DOMAIN_KIND_FCT:
    result = PROTECT(qunif_factor(&table, x, size, &info));
    UNPROTECT(2);
    return result;
  case DOMAIN_KIND_LGL:
    result = PROTECT(qunif_logical(x, size));
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
