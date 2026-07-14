#include <limits.h>
#include <math.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Arith.h>
#include <R_ext/Utils.h>

#include "r_utils.h"

typedef enum {
  DOMAIN_KIND_UNKNOWN = 0,
  DOMAIN_KIND_DBL,
  DOMAIN_KIND_INT,
  DOMAIN_KIND_FCT,
  DOMAIN_KIND_LGL
} domain_kind_t;

typedef struct {
  domain_kind_t kind;
  R_xlen_t size;
  int grouped;
} domain_info_t;

static inline void periodic_interrupt(R_xlen_t iteration) {
  if (iteration != 0 &&
      iteration % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
    R_CheckUserInterrupt();
  }
}

static inline void account_work(R_xlen_t *work_since_interrupt) {
  ++*work_since_interrupt;
  if (*work_since_interrupt >= PARADOX_INTERRUPT_CHECK_INTERVAL) {
    R_CheckUserInterrupt();
    *work_since_interrupt = 0;
  }
}

static int strings_equal(SEXP left, SEXP right) {
  if (left == right) {
    return TRUE;
  }
  if (left == NA_STRING || right == NA_STRING) {
    return FALSE;
  }

  PROTECT(left);
  PROTECT(right);
  const cetype_t left_encoding = Rf_getCharCE(left);
  const cetype_t right_encoding = Rf_getCharCE(right);
  if (left_encoding == CE_BYTES || right_encoding == CE_BYTES) {
    const int equal = left_encoding == CE_BYTES && right_encoding == CE_BYTES &&
      strcmp(CHAR(left), CHAR(right)) == 0;
    UNPROTECT(2);
    return equal;
  }

  const void *vmax = vmaxget();
  const char *left_text = Rf_translateCharUTF8(left);
  const char *right_text = Rf_translateCharUTF8(right);
  const int equal = strcmp(left_text, right_text) == 0;
  vmaxset(vmax);
  UNPROTECT(2);
  return equal;
}

static domain_kind_t class_kind(SEXP param) {
  SEXP classes = PROTECT(Rf_getAttrib(param, R_ClassSymbol));
  if (TYPEOF(classes) != STRSXP || ALTREP(classes) ||
      XLENGTH(classes) == 0) {
    UNPROTECT(1);
    return DOMAIN_KIND_UNKNOWN;
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
  UNPROTECT(1);
  return DOMAIN_KIND_UNKNOWN;
}

static const char *kind_name(domain_kind_t kind) {
  switch (kind) {
  case DOMAIN_KIND_DBL:
    return "ParamDbl";
  case DOMAIN_KIND_INT:
    return "ParamInt";
  case DOMAIN_KIND_FCT:
    return "ParamFct";
  case DOMAIN_KIND_LGL:
    return "ParamLgl";
  case DOMAIN_KIND_UNKNOWN:
    return "";
  }
  return "";
}

static domain_info_t domain_info(SEXP param) {
  const domain_kind_t kind = class_kind(param);
  if (kind == DOMAIN_KIND_UNKNOWN) {
    const domain_info_t unknown = {DOMAIN_KIND_UNKNOWN, 0, FALSE};
    return unknown;
  }

  SEXP ids = PROTECT(paradox_get_named_column_checked(
    param,
    "Domain storage",
    "Domain",
    "id"
  ));
  if (TYPEOF(ids) != STRSXP) {
    Rf_error("Corrupt Domain storage: `id` must have type `character`");
  }
  if (ALTREP(ids)) {
    Rf_error(
      "Corrupt Domain storage: `id` must use an ordinary character representation"
    );
  }
  const R_xlen_t size = XLENGTH(ids);
  SEXP classes = PROTECT(paradox_get_named_column_checked(
    param,
    "Domain storage",
    "Domain",
    "cls"
  ));
  SEXP grouping = PROTECT(paradox_get_named_column_checked(
    param,
    "Domain storage",
    "Domain",
    "grouping"
  ));
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

  int grouped = TRUE;
  const char *expected_class = kind_name(kind);
  SEXP first_group = size == 0 ? R_NilValue : STRING_ELT(grouping, 0);
  for (R_xlen_t row = 0; row < size; ++row) {
    periodic_interrupt(row);
    SEXP id = STRING_ELT(ids, row);
    SEXP cls = STRING_ELT(classes, row);
    SEXP group = STRING_ELT(grouping, row);
    if (id == NA_STRING) {
      Rf_error("Corrupt Domain storage: `id` contains a missing value");
    }
    if (cls == NA_STRING || strcmp(CHAR(cls), expected_class) != 0) {
      Rf_error("Corrupt Domain storage: `cls` is inconsistent with its class");
    }
    if (group == NA_STRING || !strings_equal(group, first_group)) {
      grouped = FALSE;
    }
  }

  const domain_info_t result = {kind, size, grouped};
  UNPROTECT(3);
  return result;
}

static int numeric_scalar(SEXP value, double *result, int allow_logical) {
  /* XLENGTH() is not defined for every SEXP type (notably NILSXP).  Reject
   * unsupported values by type before asking for their vector length so that
   * list(NULL), which is a legitimate special-value shape in paradox, falls
   * back to the established R special-value handling instead of raising from
   * the C API. */
  if (Rf_isObject(value)) {
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

typedef struct {
  const double *lower;
  const double *upper;
  const double *tolerance;
} numeric_domain_snapshot_t;

static void snapshot_numeric_column(SEXP column, R_xlen_t size,
    const char *column_name, double *snapshot) {
  /* Validate the canonical column without retaining its raw view.  Element
   * access through the public API then copies it into callback-independent
   * native storage before any public value is observed. */
  (void) paradox_get_numeric_column(
    column,
    size,
    "Domain storage",
    column_name
  );

  const SEXPTYPE type = (SEXPTYPE) TYPEOF(column);
  for (R_xlen_t row = 0; row < size; ++row) {
    periodic_interrupt(row);
    if (type == REALSXP) {
      snapshot[row] = REAL_ELT(column, row);
    } else {
      const int value = INTEGER_ELT(column, row);
      snapshot[row] = value == NA_INTEGER ? NA_REAL : (double) value;
    }
  }
}

static numeric_domain_snapshot_t snapshot_numeric_domain(
    SEXP lower, SEXP upper, SEXP tolerance, R_xlen_t size) {
  const R_xlen_t allocation_size = size == 0 ? 1 : size;
  const size_t column_count = tolerance == R_NilValue ? 2U : 3U;
  double *storage = paradox_temporary_alloc(
    allocation_size,
    column_count * sizeof(*storage)
  );
  double *lower_snapshot = storage;
  double *upper_snapshot = storage + allocation_size;
  double *tolerance_snapshot = tolerance == R_NilValue
    ? NULL
    : storage + 2 * allocation_size;

  snapshot_numeric_column(lower, size, "lower", lower_snapshot);
  snapshot_numeric_column(upper, size, "upper", upper_snapshot);
  if (tolerance != R_NilValue) {
    snapshot_numeric_column(
      tolerance,
      size,
      "tolerance",
      tolerance_snapshot
    );
  }

  const numeric_domain_snapshot_t result = {
    lower_snapshot,
    upper_snapshot,
    tolerance_snapshot
  };
  return result;
}

static void require_valid_numeric_domain(
    const numeric_domain_snapshot_t *bounds,
    R_xlen_t size) {
  for (R_xlen_t row = 0; row < size; ++row) {
    periodic_interrupt(row);
    const double row_lower = bounds->lower[row];
    const double row_upper = bounds->upper[row];
    const double row_tolerance = bounds->tolerance[row];
    if (ISNAN(row_lower) || ISNAN(row_upper) || ISNAN(row_tolerance) ||
        row_tolerance < 0.0 || row_lower > row_upper) {
      Rf_error("Corrupt Domain storage: invalid numeric bounds or tolerance");
    }
  }
}

static SEXP check_numeric_domain(SEXP param, SEXP values,
    const domain_info_t *info) {
  SEXP lower_sexp = PROTECT(paradox_get_named_column_checked(
    param, "Domain storage", "Domain", "lower"
  ));
  SEXP upper_sexp = PROTECT(paradox_get_named_column_checked(
    param, "Domain storage", "Domain", "upper"
  ));
  SEXP tolerance_sexp = PROTECT(paradox_get_named_column_checked(
    param, "Domain storage", "Domain", "tolerance"
  ));
  const numeric_domain_snapshot_t bounds = snapshot_numeric_domain(
    lower_sexp,
    upper_sexp,
    tolerance_sexp,
    info->size
  );
  require_valid_numeric_domain(&bounds, info->size);

  for (R_xlen_t row = 0; row < info->size; ++row) {
    periodic_interrupt(row);
    double value;
    SEXP element = PROTECT(VECTOR_ELT(values, row));
    const int supported = numeric_scalar(element, &value, FALSE);
    UNPROTECT(1);
    if (!supported ||
        ISNAN(value)) {
      SEXP result = Rf_ScalarLogical(FALSE);
      UNPROTECT(3);
      return result;
    }

    const double row_lower = bounds.lower[row];
    const double row_upper = bounds.upper[row];
    const double row_tolerance = bounds.tolerance[row];
    if (info->kind == DOMAIN_KIND_DBL) {
      const double accepted_lower = paradox_accepted_lower(
        row_lower,
        row_tolerance
      );
      const double accepted_upper = paradox_accepted_upper(
        row_upper,
        row_tolerance
      );
      if (ISNAN(accepted_lower) || ISNAN(accepted_upper) ||
          value < accepted_lower || value > accepted_upper) {
        SEXP result = Rf_ScalarLogical(FALSE);
        UNPROTECT(3);
        return result;
      }
    } else {
      const double rounded = nearbyint(value);
      if (!R_FINITE(value) || fabs(value - rounded) > row_tolerance ||
          rounded < row_lower || rounded > row_upper) {
        SEXP result = Rf_ScalarLogical(FALSE);
        UNPROTECT(3);
        return result;
      }
    }
  }
  SEXP result = Rf_ScalarLogical(TRUE);
  UNPROTECT(3);
  return result;
}

static SEXP check_factor_domain(SEXP param, SEXP values,
    const domain_info_t *info) {
  SEXP levels = PROTECT(paradox_get_named_column_checked(
    param, "Domain storage", "Domain", "levels"
  ));
  paradox_require_column_checked(
    levels, VECSXP, info->size, "Domain storage", "levels"
  );

  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t row = 0; row < info->size; ++row) {
    account_work(&work_since_interrupt);
    SEXP value = PROTECT(VECTOR_ELT(values, row));
    SEXP choices = PROTECT(VECTOR_ELT(levels, row));
    if (Rf_isObject(value) || TYPEOF(value) != STRSXP ||
        XLENGTH(value) != 1) {
      SEXP result = Rf_ScalarLogical(FALSE);
      UNPROTECT(3);
      return result;
    }
    SEXP selected = STRING_ELT(value, 0);
    if (selected == NA_STRING) {
      SEXP result = Rf_ScalarLogical(FALSE);
      UNPROTECT(3);
      return result;
    }
    if (TYPEOF(choices) != STRSXP) {
      Rf_error("Corrupt Domain storage: each `levels` element must be character");
    }

    int found = FALSE;
    const R_xlen_t n_choices = XLENGTH(choices);
    for (R_xlen_t choice = 0; choice < n_choices; ++choice) {
      account_work(&work_since_interrupt);
      SEXP candidate = STRING_ELT(choices, choice);
      if (candidate == NA_STRING) {
        Rf_error("Corrupt Domain storage: `levels` may not contain missing values");
      }
      if (strings_equal(selected, candidate)) {
        found = TRUE;
      }
    }
    if (!found) {
      SEXP result = Rf_ScalarLogical(FALSE);
      UNPROTECT(3);
      return result;
    }
    UNPROTECT(2);
  }
  SEXP result = Rf_ScalarLogical(TRUE);
  UNPROTECT(1);
  return result;
}

static SEXP check_logical_domain(SEXP values, const domain_info_t *info) {
  for (R_xlen_t row = 0; row < info->size; ++row) {
    periodic_interrupt(row);
    SEXP value = PROTECT(VECTOR_ELT(values, row));
    if (Rf_isObject(value) || TYPEOF(value) != LGLSXP ||
        XLENGTH(value) != 1 || LOGICAL_ELT(value, 0) == NA_LOGICAL) {
      UNPROTECT(1);
      return Rf_ScalarLogical(FALSE);
    }
    UNPROTECT(1);
  }
  return Rf_ScalarLogical(TRUE);
}

SEXP paradox_domain_check_builtin(SEXP param, SEXP values) {
  if (class_kind(param) == DOMAIN_KIND_UNKNOWN || Rf_isObject(values) ||
      TYPEOF(values) != VECSXP) {
    return Rf_ScalarLogical(FALSE);
  }

  const domain_info_t info = domain_info(param);
  if (!info.grouped || XLENGTH(values) != info.size) {
    return Rf_ScalarLogical(FALSE);
  }

  switch (info.kind) {
  case DOMAIN_KIND_DBL:
  case DOMAIN_KIND_INT:
    return check_numeric_domain(param, values, &info);
  case DOMAIN_KIND_FCT:
    return check_factor_domain(param, values, &info);
  case DOMAIN_KIND_LGL:
    return check_logical_domain(values, &info);
  case DOMAIN_KIND_UNKNOWN:
    break;
  }
  return Rf_ScalarLogical(FALSE);
}

static SEXP numeric_vector_as_list(SEXP values) {
  return Rf_coerceVector(values, VECSXP);
}

static SEXP sanitize_double(SEXP param, SEXP values,
    const domain_info_t *info) {
  const R_xlen_t value_size = XLENGTH(values);
  if (info->size == 0 || value_size == 0) {
    return R_NilValue;
  }

  SEXP lower_sexp = PROTECT(paradox_get_named_column_checked(
    param, "Domain storage", "Domain", "lower"
  ));
  SEXP upper_sexp = PROTECT(paradox_get_named_column_checked(
    param, "Domain storage", "Domain", "upper"
  ));
  const numeric_domain_snapshot_t bounds = snapshot_numeric_domain(
    lower_sexp,
    upper_sexp,
    R_NilValue,
    info->size
  );
  for (R_xlen_t row = 0; row < info->size; ++row) {
    periodic_interrupt(row);
    const double row_lower = bounds.lower[row];
    const double row_upper = bounds.upper[row];
    if (ISNAN(row_lower) || ISNAN(row_upper) || row_lower > row_upper) {
      Rf_error("Corrupt Domain storage: invalid numeric bounds");
    }
  }

  /* Establish support before emitting recycling warnings. Materialize each
   * observation at the same time: callback-capable ALTREP vectors and list
   * elements must not be reread after output allocation. */
  SEXP stable_values = PROTECT(Rf_allocVector(REALSXP, value_size));
  for (R_xlen_t index = 0; index < value_size; ++index) {
    periodic_interrupt(index);
    double value;
    if (!vector_numeric_at(values, index, &value)) {
      UNPROTECT(3);
      return R_NilValue;
    }
    SET_REAL_ELT(stable_values, index, value);
  }

  const R_xlen_t result_size = value_size > info->size
    ? value_size
    : info->size;
  const R_xlen_t shorter = value_size < info->size ? value_size : info->size;
  if (result_size % shorter != 0) {
    /* Let pmax()/pmin() retain their exact warning count and localization. */
    UNPROTECT(3);
    return R_NilValue;
  }

  SEXP numeric = PROTECT(Rf_allocVector(REALSXP, result_size));
  for (R_xlen_t index = 0; index < result_size; ++index) {
    periodic_interrupt(index);
    double value = REAL_ELT(stable_values, index % value_size);
    const double row_lower = bounds.lower[index % info->size];
    const double row_upper = bounds.upper[index % info->size];
    if (!ISNAN(value) && value < row_lower) {
      value = row_lower;
    }
    if (!ISNAN(value) && value > row_upper) {
      value = row_upper;
    }
    SET_REAL_ELT(numeric, index, value);
  }

  SEXP result = PROTECT(numeric_vector_as_list(numeric));
  UNPROTECT(5);
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
  for (R_xlen_t index = 0; index < size; ++index) {
    periodic_interrupt(index);
    double value;
    int ignored;
    if (!vector_numeric_at(values, index, &value) ||
        integer_from_double(nearbyint(value), &ignored)) {
      /* Preserve base coercion's warning and its active-locale translation. */
      UNPROTECT(1);
      return R_NilValue;
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
  UNPROTECT(3);
  return result;
}

SEXP paradox_domain_sanitize_builtin(SEXP param, SEXP values) {
  const domain_kind_t kind = class_kind(param);
  if ((kind != DOMAIN_KIND_DBL && kind != DOMAIN_KIND_INT) ||
      Rf_isObject(values) ||
      (TYPEOF(values) != VECSXP && TYPEOF(values) != REALSXP &&
       TYPEOF(values) != INTSXP && TYPEOF(values) != LGLSXP)) {
    return R_NilValue;
  }

  const domain_info_t info = domain_info(param);
  if (info.kind == DOMAIN_KIND_DBL) {
    return sanitize_double(param, values, &info);
  }
  return sanitize_integer(values);
}

static double numeric_vector_at(SEXP x, R_xlen_t index) {
  if (TYPEOF(x) == REALSXP) {
    return REAL_ELT(x, index);
  }
  const int value = INTEGER_ELT(x, index);
  return value == NA_INTEGER ? NA_REAL : (double) value;
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
    unit * upper - (unit - 1.0) * lower,
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
      unit * (upper + 1.0) - (unit - 1.0) * lower,
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

static SEXP qunif_numeric(SEXP param, SEXP x, const domain_info_t *info) {
  SEXP lower_sexp = PROTECT(paradox_get_named_column_checked(
    param, "Domain storage", "Domain", "lower"
  ));
  SEXP upper_sexp = PROTECT(paradox_get_named_column_checked(
    param, "Domain storage", "Domain", "upper"
  ));
  const numeric_domain_snapshot_t bounds = snapshot_numeric_domain(
    lower_sexp,
    upper_sexp,
    R_NilValue,
    info->size
  );
  for (R_xlen_t row = 0; row < info->size; ++row) {
    periodic_interrupt(row);
    const double row_lower = bounds.lower[row];
    const double row_upper = bounds.upper[row];
    if (ISNAN(row_lower) || ISNAN(row_upper) || row_lower > row_upper) {
      Rf_error("Corrupt Domain storage: invalid numeric bounds");
    }
  }

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
      const double value = numeric_vector_at(x, index);
      if (!valid_unit_value(value)) {
        UNPROTECT(6);
        return R_NilValue;
      }
      const double mapped = paradox_qunif_double_value(
        value,
        bounds.lower[row],
        bounds.upper[row]
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
    UNPROTECT(6);
    return result;
  }

  SEXP result = PROTECT(Rf_allocVector(INTSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    periodic_interrupt(index);
    const R_xlen_t row = index % info->size;
    const double value = numeric_vector_at(x, index);
    if (!valid_unit_value(value)) {
      UNPROTECT(3);
      return R_NilValue;
    }
    int mapped;
    if (!paradox_qunif_integer_value(
          value,
          bounds.lower[row],
          bounds.upper[row],
          &mapped
        )) {
      /* Preserve base coercion's warning and active-locale translation. */
      UNPROTECT(3);
      return R_NilValue;
    }
    SET_INTEGER_ELT(result, index, mapped);
  }
  UNPROTECT(3);
  return result;
}

static SEXP qunif_factor(SEXP param, SEXP x, const domain_info_t *info) {
  SEXP levels = PROTECT(paradox_get_named_column_checked(
    param, "Domain storage", "Domain", "levels"
  ));
  paradox_require_column_checked(
    levels, VECSXP, info->size, "Domain storage", "levels"
  );
  R_xlen_t work_since_interrupt = 0;
  SEXP stable_levels = PROTECT(Rf_allocVector(VECSXP, info->size));
  for (R_xlen_t row = 0; row < info->size; ++row) {
    account_work(&work_since_interrupt);
    SEXP choices = PROTECT(VECTOR_ELT(levels, row));
    if (TYPEOF(choices) != STRSXP) {
      Rf_error("Corrupt Domain storage: each `levels` element must be character");
    }
    const R_xlen_t n_choices = XLENGTH(choices);
    if (n_choices == 0) {
      UNPROTECT(3);
      return R_NilValue;
    }
    SEXP stable_choices = PROTECT(Rf_allocVector(STRSXP, n_choices));
    for (R_xlen_t choice = 0; choice < n_choices; ++choice) {
      account_work(&work_since_interrupt);
      SEXP value = STRING_ELT(choices, choice);
      if (value == NA_STRING) {
        Rf_error("Corrupt Domain storage: `levels` may not contain missing values");
      }
      SET_STRING_ELT(stable_choices, choice, value);
    }
    SET_VECTOR_ELT(stable_levels, row, stable_choices);
    UNPROTECT(2);
  }

  const R_xlen_t size = XLENGTH(x);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    account_work(&work_since_interrupt);
    const R_xlen_t row = index % info->size;
    SEXP choices = VECTOR_ELT(stable_levels, row);
    const R_xlen_t n_choices = XLENGTH(choices);
    const double value = numeric_vector_at(x, index);
    if (!valid_unit_value(value)) {
      UNPROTECT(3);
      return R_NilValue;
    }
    const R_xlen_t selected = paradox_qunif_level_index(value, n_choices);
    if (selected == R_XLEN_T_MAX) {
      UNPROTECT(3);
      return R_NilValue;
    }
    SET_STRING_ELT(
      result,
      index,
      STRING_ELT(choices, selected)
    );
  }
  UNPROTECT(3);
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
    const double value = numeric_vector_at(x, index);
    if (!valid_unit_value(value)) {
      UNPROTECT(2);
      return R_NilValue;
    }
    SET_LOGICAL_ELT(result, index, value < 0.5);
  }
  copy_logical_structure(result, attribute_carrier);
  UNPROTECT(2);
  return result;
}

SEXP paradox_domain_qunif_builtin(SEXP param, SEXP x) {
  if (class_kind(param) == DOMAIN_KIND_UNKNOWN || Rf_isObject(x) ||
      (TYPEOF(x) != REALSXP && TYPEOF(x) != INTSXP)) {
    return R_NilValue;
  }

  const domain_info_t info = domain_info(param);
  if (!info.grouped || info.size == 0) {
    return R_NilValue;
  }

  switch (info.kind) {
  case DOMAIN_KIND_DBL:
  case DOMAIN_KIND_INT:
    return qunif_numeric(param, x, &info);
  case DOMAIN_KIND_FCT:
    return qunif_factor(param, x, &info);
  case DOMAIN_KIND_LGL:
    return qunif_logical(x);
  case DOMAIN_KIND_UNKNOWN:
    break;
  }
  return R_NilValue;
}
