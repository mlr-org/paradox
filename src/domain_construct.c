#include <float.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Utils.h>

#include "r_api_compat.h"

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

typedef enum {
  DOMAIN_KIND_UNKNOWN = 0,
  DOMAIN_KIND_DBL,
  DOMAIN_KIND_INT,
  DOMAIN_KIND_FCT,
  DOMAIN_KIND_LGL,
  DOMAIN_KIND_UTY
} domain_kind_t;

enum domain_frame_value {
  DOMAIN_FRAME_TAGS = 0,
  DOMAIN_FRAME_CARGO,
  DOMAIN_FRAME_CLS,
  DOMAIN_FRAME_GROUPING,
  DOMAIN_FRAME_LOWER,
  DOMAIN_FRAME_UPPER,
  DOMAIN_FRAME_TOLERANCE,
  DOMAIN_FRAME_LEVELS,
  DOMAIN_FRAME_SPECIAL_VALS,
  DOMAIN_FRAME_TRAFO,
  DOMAIN_FRAME_VALUE_COUNT
};

static const char *const domain_column_names[DOMAIN_COLUMN_COUNT] = {
  "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
  "levels", "special_vals", "default", "storage_type", ".tags",
  ".trafo", ".requirements", ".init_given", ".init"
};

static int string_is(SEXP value, const char *expected) {
  return value != NA_STRING && strcmp(CHAR(value), expected) == 0;
}

static int scalar_string(SEXP value) {
  return TYPEOF(value) == STRSXP && !ALTREP(value) &&
    XLENGTH(value) == 1 &&
    STRING_ELT(value, 0) != NA_STRING;
}

static int scalar_numeric(SEXP value) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  if ((type != INTSXP && type != REALSXP) || ALTREP(value) ||
      XLENGTH(value) != 1) {
    return FALSE;
  }
  SEXP classes = PROTECT(Rf_getAttrib(value, R_ClassSymbol));
  const int valid = !ALTREP(classes) && !Rf_inherits(value, "factor");
  UNPROTECT(1);
  return valid;
}

/* Admission must not inspect objects whose metadata access can dispatch into
 * user code. Ordinary closures are plain objects and remain eligible: the
 * constructor stores callback values but never invokes them. ALTREP values are
 * also declined because materialization can execute provider callbacks. */
static int plain_metadata(SEXP value) {
  return !ALTREP(value) && !Rf_isObject(value) &&
    Rf_getAttrib(value, R_ClassSymbol) == R_NilValue;
}

static int function_or_null(SEXP value) {
  return value == R_NilValue || Rf_isFunction(value);
}

/* Match checkmate's list predicate: data frames have VECSXP storage but are
 * deliberately not lists for assert_list(). Other classed lists are valid. */
static int checkmate_list(SEXP value) {
  if (TYPEOF(value) != VECSXP || ALTREP(value)) {
    return FALSE;
  }
  SEXP classes = PROTECT(Rf_getAttrib(value, R_ClassSymbol));
  const int valid = !ALTREP(classes) &&
    !Rf_inherits(value, "data.frame");
  UNPROTECT(1);
  return valid;
}

static int unique_nonmissing_strings(SEXP values, int require_names) {
  if (TYPEOF(values) != STRSXP || ALTREP(values)) {
    return FALSE;
  }

  PROTECT(values);
  int valid = TRUE;
  const R_xlen_t size = XLENGTH(values);
  for (R_xlen_t index = 0; index < size; ++index) {
    if (index != 0 &&
        index % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP value = STRING_ELT(values, index);
    if (value == NA_STRING || (require_names && LENGTH(value) == 0)) {
      valid = FALSE;
      break;
    }
  }
  if (valid) {
    valid = Rf_any_duplicated(values, FALSE) == 0;
  }
  UNPROTECT(1);
  return valid;
}

static int named_unique_list_or_null(SEXP value) {
  if (value == R_NilValue) {
    return TRUE;
  }
  PROTECT(value);
  if (!checkmate_list(value)) {
    UNPROTECT(1);
    return FALSE;
  }

  const R_xlen_t size = XLENGTH(value);
  if (size == 0) {
    UNPROTECT(1);
    return TRUE;
  }
  SEXP names = PROTECT(Rf_getAttrib(value, R_NamesSymbol));
  const int valid = TYPEOF(names) == STRSXP && !ALTREP(names) &&
    XLENGTH(names) == size &&
    unique_nonmissing_strings(names, TRUE);
  UNPROTECT(2);
  return valid;
}

static SEXP named_element(SEXP values, const char *name) {
  if (values == R_NilValue || TYPEOF(values) != VECSXP || ALTREP(values)) {
    return R_NilValue;
  }
  const R_xlen_t size = XLENGTH(values);
  SEXP names = Rf_getAttrib(values, R_NamesSymbol);
  if (TYPEOF(names) != STRSXP || ALTREP(names) || XLENGTH(names) != size) {
    return R_NilValue;
  }

  for (R_xlen_t index = 0; index < size; ++index) {
    if (string_is(STRING_ELT(names, index), name)) {
      return VECTOR_ELT(values, index);
    }
  }
  return R_NilValue;
}

static int cargo_names_are_canonical(SEXP cargo) {
  if (cargo == R_NilValue) {
    return TRUE;
  }
  PROTECT(cargo);
  if (TYPEOF(cargo) != VECSXP || ALTREP(cargo)) {
    UNPROTECT(1);
    return FALSE;
  }
  const R_xlen_t size = XLENGTH(cargo);
  if (size == 0) {
    UNPROTECT(1);
    return TRUE;
  }

  SEXP names = PROTECT(Rf_getAttrib(cargo, R_NamesSymbol));
  if (TYPEOF(names) != STRSXP || ALTREP(names) ||
      XLENGTH(names) != size ||
      !unique_nonmissing_strings(names, TRUE)) {
    UNPROTECT(2);
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP name = STRING_ELT(names, index);
    if (!string_is(name, "logscale") && !string_is(name, "aggr") &&
        !string_is(name, "in_tune_fn") &&
        !string_is(name, "disable_in_tune") &&
        !string_is(name, "custom_check") && !string_is(name, "repr")) {
      UNPROTECT(2);
      return FALSE;
    }
  }
  UNPROTECT(2);
  return TRUE;
}

static domain_kind_t domain_kind_from_class(SEXP cls) {
  if (!scalar_string(cls)) {
    return DOMAIN_KIND_UNKNOWN;
  }

  SEXP class_name = STRING_ELT(cls, 0);
  if (string_is(class_name, "ParamDbl")) {
    return DOMAIN_KIND_DBL;
  }
  if (string_is(class_name, "ParamInt")) {
    return DOMAIN_KIND_INT;
  }
  if (string_is(class_name, "ParamFct")) {
    return DOMAIN_KIND_FCT;
  }
  if (string_is(class_name, "ParamLgl")) {
    return DOMAIN_KIND_LGL;
  }
  if (string_is(class_name, "ParamUty")) {
    return DOMAIN_KIND_UTY;
  }
  return DOMAIN_KIND_UNKNOWN;
}

static const char *domain_storage_name(domain_kind_t kind) {
  switch (kind) {
  case DOMAIN_KIND_DBL:
    return "numeric";
  case DOMAIN_KIND_INT:
    return "integer";
  case DOMAIN_KIND_FCT:
    return "character";
  case DOMAIN_KIND_LGL:
    return "logical";
  case DOMAIN_KIND_UTY:
    return "list";
  case DOMAIN_KIND_UNKNOWN:
    return NULL;
  }
  return NULL;
}

static domain_kind_t domain_kind(SEXP cls, SEXP storage_type) {
  const domain_kind_t kind = domain_kind_from_class(cls);
  const char *expected_storage = domain_storage_name(kind);
  if (expected_storage == NULL || !scalar_string(storage_type) ||
      !string_is(STRING_ELT(storage_type, 0), expected_storage)) {
    return DOMAIN_KIND_UNKNOWN;
  }
  return kind;
}

static int levels_are_canonical(domain_kind_t kind, SEXP levels) {
  switch (kind) {
  case DOMAIN_KIND_DBL:
  case DOMAIN_KIND_INT:
  case DOMAIN_KIND_UTY:
    return levels == R_NilValue;
  case DOMAIN_KIND_FCT:
    return unique_nonmissing_strings(levels, FALSE);
  case DOMAIN_KIND_LGL:
    return TYPEOF(levels) == LGLSXP && !ALTREP(levels) &&
      XLENGTH(levels) == 2 &&
      LOGICAL_ELT(levels, 0) == TRUE && LOGICAL_ELT(levels, 1) == FALSE;
  case DOMAIN_KIND_UNKNOWN:
    return FALSE;
  }
  return FALSE;
}

static int has_tag(SEXP tags, const char *target) {
  if (ALTREP(tags)) {
    return FALSE;
  }
  const R_xlen_t size = XLENGTH(tags);
  for (R_xlen_t index = 0; index < size; ++index) {
    if (index != 0 &&
        index % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    if (string_is(STRING_ELT(tags, index), target)) {
      return TRUE;
    }
  }
  return FALSE;
}

static int cargo_is_canonical_common(SEXP cargo, SEXP tags) {
  PROTECT(cargo);
  PROTECT(tags);
  if (!cargo_names_are_canonical(cargo)) {
    UNPROTECT(2);
    return FALSE;
  }

  SEXP aggr = PROTECT(named_element(cargo, "aggr"));
  SEXP in_tune_fn = PROTECT(named_element(cargo, "in_tune_fn"));
  SEXP disable_in_tune = PROTECT(named_element(cargo, "disable_in_tune"));
  if (!function_or_null(aggr) || !function_or_null(in_tune_fn) ||
      !named_unique_list_or_null(disable_in_tune)) {
    UNPROTECT(5);
    return FALSE;
  }

  const int internal_tuning = has_tag(tags, "internal_tuning");
  if (internal_tuning && aggr == R_NilValue) {
    UNPROTECT(5);
    return FALSE;
  }
  if ((in_tune_fn != R_NilValue || disable_in_tune != R_NilValue) &&
      !internal_tuning) {
    UNPROTECT(5);
    return FALSE;
  }
  if ((in_tune_fn == R_NilValue) != (disable_in_tune == R_NilValue)) {
    UNPROTECT(5);
    return FALSE;
  }

  UNPROTECT(5);
  return TRUE;
}

static int cargo_matches_kind(SEXP cargo, domain_kind_t kind) {
  PROTECT(cargo);
  SEXP custom_check = PROTECT(named_element(cargo, "custom_check"));
  SEXP repr = PROTECT(named_element(cargo, "repr"));
  int valid;
  if (kind == DOMAIN_KIND_UTY) {
    valid = function_or_null(custom_check) && scalar_string(repr);
  } else {
    valid = custom_check == R_NilValue && repr == R_NilValue;
  }

  UNPROTECT(3);
  return valid;
}

static int cargo_is_canonical(SEXP cargo, SEXP tags, domain_kind_t kind) {
  return cargo_is_canonical_common(cargo, tags) &&
    cargo_matches_kind(cargo, kind);
}

static int cargo_metadata_is_plain(SEXP cargo) {
  if (cargo == R_NilValue) {
    return TRUE;
  }
  PROTECT(cargo);
  if (!plain_metadata(cargo)) {
    UNPROTECT(1);
    return FALSE;
  }

  SEXP names = PROTECT(Rf_getAttrib(cargo, R_NamesSymbol));
  if (names != R_NilValue && !plain_metadata(names)) {
    UNPROTECT(2);
    return FALSE;
  }

  static const char *const callback_names[] = {
    "aggr", "in_tune_fn", "custom_check"
  };
  for (size_t index = 0;
       index < sizeof(callback_names) / sizeof(callback_names[0]);
       ++index) {
    SEXP callback = PROTECT(named_element(cargo, callback_names[index]));
    if (callback != R_NilValue && !plain_metadata(callback)) {
      UNPROTECT(3);
      return FALSE;
    }
    UNPROTECT(1);
  }

  SEXP disable_in_tune = PROTECT(named_element(cargo, "disable_in_tune"));
  if (disable_in_tune != R_NilValue && !plain_metadata(disable_in_tune)) {
    UNPROTECT(3);
    return FALSE;
  }
  if (disable_in_tune != R_NilValue) {
    SEXP disable_names = PROTECT(Rf_getAttrib(
      disable_in_tune,
      R_NamesSymbol
    ));
    if (disable_names != R_NilValue && !plain_metadata(disable_names)) {
      UNPROTECT(4);
      return FALSE;
    }
    UNPROTECT(1);
  }
  UNPROTECT(1);

  SEXP repr = PROTECT(named_element(cargo, "repr"));
  if (repr != R_NilValue && !plain_metadata(repr)) {
    UNPROTECT(3);
    return FALSE;
  }
  UNPROTECT(1);

  SEXP logscale = PROTECT(named_element(cargo, "logscale"));
  if (logscale != R_NilValue && !plain_metadata(logscale)) {
    UNPROTECT(3);
    return FALSE;
  }
  UNPROTECT(3);
  return TRUE;
}

static SEXP one_element_list(SEXP value) {
  PROTECT(value);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(result, 0, value);
  UNPROTECT(2);
  return result;
}

static SEXP domain_names(void) {
  SEXP result = PROTECT(Rf_allocVector(STRSXP, DOMAIN_COLUMN_COUNT));
  for (R_xlen_t column = 0; column < DOMAIN_COLUMN_COUNT; ++column) {
    SET_STRING_ELT(result, column, Rf_mkChar(domain_column_names[column]));
  }
  UNPROTECT(1);
  return result;
}

static SEXP build_domain_shell(
    SEXP cls,
    SEXP grouping,
    SEXP cargo,
    SEXP lower,
    SEXP upper,
    SEXP tolerance,
    SEXP levels,
    SEXP special_vals,
    SEXP default_value,
    SEXP tags,
    SEXP trafo,
    SEXP storage_type,
    SEXP init_given,
    SEXP init_value) {
  PROTECT(cls);
  PROTECT(grouping);
  PROTECT(cargo);
  PROTECT(lower);
  PROTECT(upper);
  PROTECT(tolerance);
  PROTECT(levels);
  PROTECT(special_vals);
  PROTECT(default_value);
  PROTECT(tags);
  PROTECT(trafo);
  PROTECT(storage_type);
  PROTECT(init_given);
  PROTECT(init_value);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, DOMAIN_COLUMN_COUNT));
  SEXP id = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(id, 0, NA_STRING);
  SET_VECTOR_ELT(result, DOMAIN_ID, id);
  SET_VECTOR_ELT(result, DOMAIN_CLS, cls);
  SET_VECTOR_ELT(result, DOMAIN_GROUPING, grouping);
  SEXP cargo_column = PROTECT(one_element_list(cargo));
  SET_VECTOR_ELT(result, DOMAIN_CARGO, cargo_column);
  SET_VECTOR_ELT(result, DOMAIN_LOWER, lower);
  SET_VECTOR_ELT(result, DOMAIN_UPPER, upper);
  SET_VECTOR_ELT(result, DOMAIN_TOLERANCE, tolerance);
  SEXP levels_column = PROTECT(one_element_list(levels));
  SET_VECTOR_ELT(result, DOMAIN_LEVELS, levels_column);
  SEXP special_column = PROTECT(one_element_list(special_vals));
  SET_VECTOR_ELT(result, DOMAIN_SPECIAL_VALS, special_column);
  SEXP default_column = PROTECT(one_element_list(default_value));
  SET_VECTOR_ELT(result, DOMAIN_DEFAULT, default_column);
  SET_VECTOR_ELT(result, DOMAIN_STORAGE_TYPE, storage_type);
  SEXP tags_column = PROTECT(one_element_list(tags));
  SET_VECTOR_ELT(result, DOMAIN_TAGS, tags_column);
  SEXP trafo_column = PROTECT(one_element_list(trafo));
  SET_VECTOR_ELT(result, DOMAIN_TRAFO, trafo_column);
  SEXP requirements_column = PROTECT(one_element_list(R_NilValue));
  SET_VECTOR_ELT(result, DOMAIN_REQUIREMENTS, requirements_column);
  SET_VECTOR_ELT(result, DOMAIN_INIT_GIVEN, init_given);
  SEXP init_column = PROTECT(one_element_list(init_value));
  SET_VECTOR_ELT(result, DOMAIN_INIT, init_column);
  SEXP names = PROTECT(domain_names());
  Rf_setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(25);
  return result;
}

SEXP paradox_domain_construct(
    SEXP cls,
    SEXP grouping,
    SEXP cargo,
    SEXP lower,
    SEXP upper,
    SEXP tolerance,
    SEXP levels,
    SEXP special_vals,
    SEXP default_value,
    SEXP tags,
    SEXP trafo,
    SEXP storage_type,
    SEXP init_given,
    SEXP init_value) {
  const domain_kind_t kind = domain_kind(cls, storage_type);
  if (kind == DOMAIN_KIND_UNKNOWN || !scalar_string(grouping) ||
      !scalar_numeric(lower) || !scalar_numeric(upper) ||
      !scalar_numeric(tolerance) || !levels_are_canonical(kind, levels) ||
      !checkmate_list(special_vals) ||
      !unique_nonmissing_strings(tags, FALSE) || !function_or_null(trafo) ||
      TYPEOF(init_given) != LGLSXP || ALTREP(init_given) ||
      XLENGTH(init_given) != 1 ||
      LOGICAL_ELT(init_given, 0) == NA_LOGICAL ||
      !cargo_is_canonical(cargo, tags, kind)) {
    return R_NilValue;
  }
  if (XLENGTH(special_vals) != 0 && trafo != R_NilValue) {
    return R_NilValue;
  }

  return build_domain_shell(
    cls,
    grouping,
    cargo,
    lower,
    upper,
    tolerance,
    levels,
    special_vals,
    default_value,
    tags,
    trafo,
    storage_type,
    init_given,
    init_value
  );
}

static int force_frame_value(
    SEXP frame,
    SEXP anchor,
    R_xlen_t index,
    const char *name,
    SEXP *result) {
  SEXP symbol = Rf_install(name);
  if (R_existsVarInFrame(frame, symbol) == FALSE ||
      R_BindingIsActive(symbol, frame) != FALSE) {
    return FALSE;
  }
  *result = Rf_eval(symbol, frame);
  SET_VECTOR_ELT(anchor, index, *result);
  return TRUE;
}

static int plain_scalar_number_value(SEXP value, double *result) {
  if (ALTREP(value) || Rf_isObject(value) ||
      !paradox_api_has_no_attributes(value) || XLENGTH(value) != 1) {
    return FALSE;
  }
  if (TYPEOF(value) == INTSXP) {
    const int input = INTEGER_ELT(value, 0);
    if (input == NA_INTEGER) {
      return FALSE;
    }
    *result = (double) input;
    return TRUE;
  }
  if (TYPEOF(value) == REALSXP) {
    const double input = REAL_ELT(value, 0);
    if (ISNAN(input)) {
      return FALSE;
    }
    *result = input;
    return TRUE;
  }
  return FALSE;
}

static int plain_integer_bound(double value) {
  if (!R_FINITE(value)) {
    return TRUE;
  }
  if (value < -(double) INT_MAX || value > (double) INT_MAX) {
    return FALSE;
  }
  return value == (double) ((int) value);
}

static int fct_grouping_output_size(SEXP levels, int check_interrupts,
    size_t *result) {
  const R_xlen_t size = XLENGTH(levels);
  if (size != 0 && (size_t) size >
      ((size_t) INT_MAX + 1) / 3) {
    return FALSE;
  }
  size_t output_size = size == 0 ? 2 : (size_t) size * 3 - 1;
  for (R_xlen_t index = 0; index < size; ++index) {
    if (check_interrupts && index != 0 &&
        index % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP value = STRING_ELT(levels, index);
    if (value == NA_STRING || Rf_getCharCE(value) != CE_NATIVE) {
      return FALSE;
    }
    const int length = LENGTH(value);
    const unsigned char *bytes = (const unsigned char *) CHAR(value);
    size_t added = (size_t) length;
    for (int byte_index = 0; byte_index < length; ++byte_index) {
      if (check_interrupts && byte_index != 0 &&
          byte_index % (int) PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
        R_CheckUserInterrupt();
      }
      const unsigned char byte = bytes[byte_index];
      if (byte == '\0' || byte >= 0x80) {
        return FALSE;
      }
      if (byte == '\\' || byte == '"') {
        ++added;
      }
    }
    if (added > (size_t) INT_MAX - output_size) {
      return FALSE;
    }
    output_size += added;
  }
  *result = output_size;
  return TRUE;
}

SEXP paradox_domain_fct_grouping(SEXP levels) {
  if (TYPEOF(levels) != STRSXP || ALTREP(levels) || Rf_isObject(levels) ||
      !paradox_api_has_no_attributes(levels) ||
      XLENGTH(levels) > (R_xlen_t) INT_MAX) {
    return R_NilValue;
  }

  const R_xlen_t size = XLENGTH(levels);
  size_t output_size;
  if (!fct_grouping_output_size(levels, TRUE, &output_size)) {
    return R_NilValue;
  }

  char *buffer = R_alloc(output_size + 1, sizeof(*buffer));
  /* R_alloc() may run a finalizer that mutates a caller-owned vector by
   * reference. Revalidate the complete size and byte grammar after that last
   * allocation; the subsequent fill is allocation- and callback-free. */
  size_t verified_output_size;
  if (!fct_grouping_output_size(levels, FALSE, &verified_output_size) ||
      verified_output_size != output_size) {
    return R_NilValue;
  }
  size_t offset = 0;
  if (size == 0) {
    buffer[offset++] = '"';
    buffer[offset++] = '"';
  } else {
    for (R_xlen_t index = 0; index < size; ++index) {
      if (index != 0) {
        buffer[offset++] = ',';
      }
      buffer[offset++] = '"';
      SEXP value = STRING_ELT(levels, index);
      const int length = LENGTH(value);
      const char *bytes = CHAR(value);
      for (int byte_index = 0; byte_index < length; ++byte_index) {
        if (bytes[byte_index] == '\\' || bytes[byte_index] == '"') {
          buffer[offset++] = '\\';
        }
        buffer[offset++] = bytes[byte_index];
      }
      buffer[offset++] = '"';
    }
  }
  if (offset != output_size) {
    Rf_error("Internal error while constructing factor grouping");
  }
  buffer[offset] = '\0';
  SEXP grouping = PROTECT(Rf_mkCharLen(buffer, (int) output_size));
  SEXP result = PROTECT(Rf_ScalarString(grouping));
  UNPROTECT(2);
  return result;
}

SEXP paradox_domain_numeric_bounds_admit(SEXP frame, SEXP integer_kind) {
  if (TYPEOF(frame) != ENVSXP || Rf_isObject(frame) ||
      TYPEOF(integer_kind) != LGLSXP || ALTREP(integer_kind) ||
      !paradox_api_has_no_attributes(integer_kind) ||
      XLENGTH(integer_kind) != 1 ||
      LOGICAL_ELT(integer_kind, 0) == NA_LOGICAL) {
    return Rf_ScalarLogical(FALSE);
  }
  const int integer = LOGICAL_ELT(integer_kind, 0);
  SEXP values = PROTECT(Rf_allocVector(VECSXP, 4));

  SEXP tolerance;
  double tolerance_value;
  if (!force_frame_value(frame, values, 0, "tolerance", &tolerance) ||
      !plain_scalar_number_value(tolerance, &tolerance_value) ||
      tolerance_value < 0.0 || (integer && tolerance_value > 0.5)) {
    goto decline;
  }

  SEXP lower;
  double lower_value;
  if (!force_frame_value(frame, values, 1, "lower", &lower) ||
      !plain_scalar_number_value(lower, &lower_value) ||
      (integer && !plain_integer_bound(lower_value))) {
    goto decline;
  }

  SEXP upper;
  double upper_value;
  if (!force_frame_value(frame, values, 2, "upper", &upper) ||
      !plain_scalar_number_value(upper, &upper_value) ||
      (integer && !plain_integer_bound(upper_value)) ||
      lower_value > upper_value) {
    goto decline;
  }

  SEXP logscale;
  if (!force_frame_value(frame, values, 3, "logscale", &logscale) ||
      TYPEOF(logscale) != LGLSXP || ALTREP(logscale) ||
      !paradox_api_has_no_attributes(logscale) || XLENGTH(logscale) != 1 ||
      LOGICAL_ELT(logscale, 0) != FALSE) {
    goto decline;
  }

  UNPROTECT(1);
  return Rf_ScalarLogical(TRUE);

decline:
  UNPROTECT(1);
  return Rf_ScalarLogical(FALSE);
}

SEXP paradox_domain_uty_check_result(SEXP result) {
  int valid = FALSE;
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(result);
  if ((type == LGLSXP || type == STRSXP) && !ALTREP(result) &&
      XLENGTH(result) == 1) {
    if (type == LGLSXP) {
      valid = LOGICAL_ELT(result, 0) == TRUE;
    } else {
      valid = STRING_ELT(result, 0) != NA_STRING;
    }
  }
  return Rf_ScalarLogical(valid);
}

#if R_VERSION >= R_Version(4, 5, 0)

#define DOMAIN_REPR_MAX_OUTPUT ((size_t) 80)
#define DOMAIN_REPR_MAX_ARGUMENTS ((size_t) 13)
#define DOMAIN_REPR_MAX_CHARACTER_VALUES ((R_xlen_t) 16)

#define DOMAIN_REPR_DBL (1U << 0)
#define DOMAIN_REPR_INT (1U << 1)
#define DOMAIN_REPR_FCT (1U << 2)
#define DOMAIN_REPR_LGL (1U << 3)
#define DOMAIN_REPR_UTY (1U << 4)

#define DOMAIN_REPR_LEVELS (1U << 0)
#define DOMAIN_REPR_LOWER (1U << 1)
#define DOMAIN_REPR_UPPER (1U << 2)
#define DOMAIN_REPR_SPECIAL_VALS (1U << 3)
#define DOMAIN_REPR_DEFAULT (1U << 4)
#define DOMAIN_REPR_TAGS (1U << 5)
#define DOMAIN_REPR_TOLERANCE (1U << 6)
#define DOMAIN_REPR_DEPENDS (1U << 7)
#define DOMAIN_REPR_TRAFO (1U << 8)
#define DOMAIN_REPR_LOGSCALE (1U << 9)
#define DOMAIN_REPR_INIT (1U << 10)
#define DOMAIN_REPR_AGGR (1U << 11)
#define DOMAIN_REPR_IN_TUNE_FN (1U << 12)
#define DOMAIN_REPR_DISABLE_IN_TUNE (1U << 13)
#define DOMAIN_REPR_CUSTOM_CHECK (1U << 14)
#define DOMAIN_REPR_REPR (1U << 15)

struct domain_repr_buffer {
  char bytes[DOMAIN_REPR_MAX_OUTPUT + 1];
  size_t length;
};

struct domain_repr_constructor {
  const char *name;
  unsigned int mask;
  unsigned int required;
};

struct domain_repr_formal {
  const char *name;
  unsigned int bit;
  unsigned int constructors;
};

static const struct domain_repr_constructor domain_repr_constructors[] = {
  {"p_dbl", DOMAIN_REPR_DBL, 0U},
  {"p_int", DOMAIN_REPR_INT, 0U},
  {"p_fct", DOMAIN_REPR_FCT, DOMAIN_REPR_LEVELS},
  {"p_lgl", DOMAIN_REPR_LGL, 0U},
  {"p_uty", DOMAIN_REPR_UTY, 0U}
};

static const struct domain_repr_formal domain_repr_formals[] = {
  {"levels", DOMAIN_REPR_LEVELS, DOMAIN_REPR_FCT},
  {"lower", DOMAIN_REPR_LOWER, DOMAIN_REPR_DBL | DOMAIN_REPR_INT},
  {"upper", DOMAIN_REPR_UPPER, DOMAIN_REPR_DBL | DOMAIN_REPR_INT},
  {"special_vals", DOMAIN_REPR_SPECIAL_VALS,
    DOMAIN_REPR_DBL | DOMAIN_REPR_INT | DOMAIN_REPR_FCT |
      DOMAIN_REPR_LGL | DOMAIN_REPR_UTY},
  {"default", DOMAIN_REPR_DEFAULT,
    DOMAIN_REPR_DBL | DOMAIN_REPR_INT | DOMAIN_REPR_FCT |
      DOMAIN_REPR_LGL | DOMAIN_REPR_UTY},
  {"tags", DOMAIN_REPR_TAGS,
    DOMAIN_REPR_DBL | DOMAIN_REPR_INT | DOMAIN_REPR_FCT |
      DOMAIN_REPR_LGL | DOMAIN_REPR_UTY},
  {"tolerance", DOMAIN_REPR_TOLERANCE, DOMAIN_REPR_DBL | DOMAIN_REPR_INT},
  {"depends", DOMAIN_REPR_DEPENDS,
    DOMAIN_REPR_DBL | DOMAIN_REPR_INT | DOMAIN_REPR_FCT |
      DOMAIN_REPR_LGL | DOMAIN_REPR_UTY},
  {"trafo", DOMAIN_REPR_TRAFO,
    DOMAIN_REPR_DBL | DOMAIN_REPR_INT | DOMAIN_REPR_FCT |
      DOMAIN_REPR_LGL | DOMAIN_REPR_UTY},
  {"logscale", DOMAIN_REPR_LOGSCALE, DOMAIN_REPR_DBL | DOMAIN_REPR_INT},
  {"init", DOMAIN_REPR_INIT,
    DOMAIN_REPR_DBL | DOMAIN_REPR_INT | DOMAIN_REPR_FCT |
      DOMAIN_REPR_LGL | DOMAIN_REPR_UTY},
  {"aggr", DOMAIN_REPR_AGGR,
    DOMAIN_REPR_DBL | DOMAIN_REPR_INT | DOMAIN_REPR_FCT |
      DOMAIN_REPR_LGL | DOMAIN_REPR_UTY},
  {"in_tune_fn", DOMAIN_REPR_IN_TUNE_FN,
    DOMAIN_REPR_DBL | DOMAIN_REPR_INT | DOMAIN_REPR_FCT |
      DOMAIN_REPR_LGL | DOMAIN_REPR_UTY},
  {"disable_in_tune", DOMAIN_REPR_DISABLE_IN_TUNE,
    DOMAIN_REPR_DBL | DOMAIN_REPR_INT | DOMAIN_REPR_FCT |
      DOMAIN_REPR_LGL | DOMAIN_REPR_UTY},
  {"custom_check", DOMAIN_REPR_CUSTOM_CHECK, DOMAIN_REPR_UTY},
  {"repr", DOMAIN_REPR_REPR, DOMAIN_REPR_UTY}
};

static int domain_repr_append_bytes(
    struct domain_repr_buffer *buffer,
    const char *bytes,
    size_t length) {
  if (length > DOMAIN_REPR_MAX_OUTPUT - buffer->length) {
    return FALSE;
  }
  memcpy(buffer->bytes + buffer->length, bytes, length);
  buffer->length += length;
  buffer->bytes[buffer->length] = '\0';
  return TRUE;
}

static int domain_repr_append(
    struct domain_repr_buffer *buffer,
    const char *text) {
  return domain_repr_append_bytes(buffer, text, strlen(text));
}

static const struct domain_repr_constructor *domain_repr_constructor(
    SEXP constructor) {
  if (TYPEOF(constructor) != SYMSXP || ALTREP(constructor) ||
      Rf_isS4(constructor) ||
      Rf_isObject(constructor) ||
      !paradox_api_has_no_attributes(constructor)) {
    return NULL;
  }
  SEXP name = PRINTNAME(constructor);
  if (Rf_getCharCE(name) != CE_NATIVE) {
    return NULL;
  }
  const char *bytes = CHAR(name);
  const size_t count = sizeof(domain_repr_constructors) /
    sizeof(domain_repr_constructors[0]);
  for (size_t index = 0; index < count; ++index) {
    if (strcmp(bytes, domain_repr_constructors[index].name) == 0) {
      return &domain_repr_constructors[index];
    }
  }
  return NULL;
}

static const struct domain_repr_formal *domain_repr_formal(
    SEXP tag,
    unsigned int constructor) {
  if (TYPEOF(tag) != SYMSXP || ALTREP(tag) || Rf_isS4(tag) ||
      Rf_isObject(tag) ||
      !paradox_api_has_no_attributes(tag)) {
    return NULL;
  }
  SEXP name = PRINTNAME(tag);
  if (Rf_getCharCE(name) != CE_NATIVE) {
    return NULL;
  }
  const char *bytes = CHAR(name);
  const size_t count = sizeof(domain_repr_formals) /
    sizeof(domain_repr_formals[0]);
  for (size_t index = 0; index < count; ++index) {
    if ((domain_repr_formals[index].constructors & constructor) != 0U &&
        strcmp(bytes, domain_repr_formals[index].name) == 0) {
      return &domain_repr_formals[index];
    }
  }
  return NULL;
}

static int domain_repr_scipen_value(SEXP symbol, int *value) {
  SEXP option = PROTECT(Rf_GetOption1(symbol));
  const int result = TYPEOF(option) == INTSXP && !ALTREP(option) &&
    !Rf_isS4(option) && !Rf_isObject(option) &&
    paradox_api_has_no_attributes(option) &&
    XLENGTH(option) == 1 && INTEGER_ELT(option, 0) != NA_INTEGER;
  if (result) {
    *value = INTEGER_ELT(option, 0);
  }
  UNPROTECT(1);
  return result;
}

static int domain_repr_scipen_is_zero(SEXP symbol) {
  int value;
  return domain_repr_scipen_value(symbol, &value) && value == 0;
}

static int domain_repr_outdec_is_dot(SEXP symbol) {
  SEXP option = PROTECT(Rf_GetOption1(symbol));
  const int result = TYPEOF(option) == STRSXP && !ALTREP(option) &&
    !Rf_isS4(option) && !Rf_isObject(option) &&
    paradox_api_has_no_attributes(option) && XLENGTH(option) == 1 &&
    STRING_ELT(option, 0) != NA_STRING &&
    Rf_getCharCE(STRING_ELT(option, 0)) == CE_NATIVE &&
    strcmp(CHAR(STRING_ELT(option, 0)), ".") == 0;
  UNPROTECT(1);
  return result;
}

static int domain_repr_append_character(
    struct domain_repr_buffer *buffer,
    SEXP value) {
  const R_xlen_t length = XLENGTH(value);
  if (length > DOMAIN_REPR_MAX_CHARACTER_VALUES) {
    return FALSE;
  }
  if (length == 0) {
    return domain_repr_append(buffer, "character(0)");
  }
  if (length > 1 && !domain_repr_append(buffer, "c(")) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < length; ++index) {
    SEXP string = STRING_ELT(value, index);
    if (string == NA_STRING || Rf_getCharCE(string) != CE_NATIVE) {
      return FALSE;
    }
    const int byte_count = LENGTH(string);
    if (byte_count < 0 || (size_t) byte_count > DOMAIN_REPR_MAX_OUTPUT) {
      return FALSE;
    }
    const unsigned char *bytes = (const unsigned char *) CHAR(string);
    for (int byte_index = 0; byte_index < byte_count; ++byte_index) {
      const unsigned char byte = bytes[byte_index];
      if (byte < 0x20 || byte > 0x7e || byte == '"' || byte == '\\') {
        return FALSE;
      }
    }
    if ((index != 0 && !domain_repr_append(buffer, ", ")) ||
        !domain_repr_append(buffer, "\"") ||
        !domain_repr_append_bytes(buffer, (const char *) bytes,
          (size_t) byte_count) ||
        !domain_repr_append(buffer, "\"")) {
      return FALSE;
    }
  }
  return length == 1 || domain_repr_append(buffer, ")");
}

static int domain_repr_append_integer(
    struct domain_repr_buffer *buffer,
    SEXP value) {
  if (XLENGTH(value) != 1) {
    return FALSE;
  }
  const int integer = INTEGER_ELT(value, 0);
  if (integer == NA_INTEGER) {
    return FALSE;
  }
  char encoded[32];
  const int written = snprintf(encoded, sizeof(encoded), "%dL", integer);
  return written > 0 && (size_t) written < sizeof(encoded) &&
    domain_repr_append_bytes(buffer, encoded, (size_t) written);
}

static int domain_repr_append_real(
    struct domain_repr_buffer *buffer,
    SEXP value,
    int scipen_zero,
    int allow_general) {
  if (XLENGTH(value) != 1) {
    return FALSE;
  }
  const double real = REAL_ELT(value, 0);
  if (ISNAN(real)) {
    return FALSE;
  }
  if (!R_FINITE(real)) {
    return domain_repr_append(buffer, real < 0.0 ? "-Inf" : "Inf");
  }
  if (scipen_zero && real >= -9999.0 && real <= 9999.0 &&
      real == (double) ((int) real)) {
    char encoded[32];
    const int written = snprintf(encoded, sizeof(encoded), "%d", (int) real);
    return written > 0 && (size_t) written < sizeof(encoded) &&
      domain_repr_append_bytes(buffer, encoded, (size_t) written);
  }
  if (!allow_general) {
    return FALSE;
  }

  /* Numeric-to-character coercion and deparse() share R's DBL_DIG scalar
   * formatter.  Coercion deliberately drops trailing zeroes, however.  Keep
   * the coercion only when it already contains all DBL_DIG digits or parsing
   * its complete ASCII result reproduces the input double bit for bit.  This
   * rejects shortened output that deparse() would pad to preserve the value.
   * The relaxed entry point renders only a private deep duplicate, so its
   * allocations cannot tear caller-owned scalar values. */
  SEXP encoded = PROTECT(Rf_coerceVector(value, STRSXP));
  SEXP string = STRING_ELT(encoded, 0);
  if (string == NA_STRING || Rf_getCharCE(string) != CE_NATIVE) {
    UNPROTECT(1);
    return FALSE;
  }
  const int length = LENGTH(string);
  if (length <= 0 || (size_t) length > DOMAIN_REPR_MAX_OUTPUT) {
    UNPROTECT(1);
    return FALSE;
  }
  const char *bytes = CHAR(string);
  const char *exponent = NULL;
  int significant_digits = 0;
  int significant_started = FALSE;
  for (int index = 0; index < length; ++index) {
    const unsigned char byte = (unsigned char) bytes[index];
    if (byte >= (unsigned char) '0' && byte <= (unsigned char) '9') {
      if (exponent == NULL &&
          (significant_started || byte != (unsigned char) '0')) {
        significant_started = TRUE;
        ++significant_digits;
      }
    } else if (byte == (unsigned char) 'e' && exponent == NULL) {
      exponent = bytes + index;
    } else if (byte != (unsigned char) '.' &&
        byte != (unsigned char) '+' && byte != (unsigned char) '-') {
      UNPROTECT(1);
      return FALSE;
    }
  }
  if (significant_digits != DBL_DIG) {
    char *end = NULL;
    const double parsed = R_strtod(bytes, &end);
    if (end != bytes + length ||
        memcmp(&parsed, &real, sizeof(parsed)) != 0) {
      UNPROTECT(1);
      return FALSE;
    }
  }
  const int appended = domain_repr_append_bytes(
    buffer,
    bytes,
    (size_t) length
  );
  UNPROTECT(1);
  return appended;
}

static int domain_repr_append_value(
    struct domain_repr_buffer *buffer,
    SEXP value,
    int scipen_zero,
    int allow_general_reals) {
  if (value == R_NilValue) {
    return domain_repr_append(buffer, "NULL");
  }
  if (ALTREP(value) || Rf_isS4(value) || Rf_isObject(value) ||
      !paradox_api_has_no_attributes(value)) {
    return FALSE;
  }
  switch (TYPEOF(value)) {
  case LGLSXP: {
    if (XLENGTH(value) != 1) {
      return FALSE;
    }
    const int logical = LOGICAL_ELT(value, 0);
    if (logical == NA_LOGICAL) {
      return FALSE;
    }
    return domain_repr_append(buffer, logical ? "TRUE" : "FALSE");
  }
  case INTSXP:
    return domain_repr_append_integer(buffer, value);
  case REALSXP:
    return domain_repr_append_real(
      buffer,
      value,
      scipen_zero,
      allow_general_reals
    );
  case STRSXP:
    return domain_repr_append_character(buffer, value);
  default:
    return FALSE;
  }
}

static int domain_repr_render(
    SEXP representation,
    int scipen_zero,
    int allow_general_reals,
    struct domain_repr_buffer *buffer) {
  if (TYPEOF(representation) != LANGSXP || ALTREP(representation) ||
      Rf_isS4(representation) || Rf_isObject(representation) ||
      !paradox_api_has_no_attributes(representation)) {
    return FALSE;
  }

  const struct domain_repr_constructor *constructor =
    domain_repr_constructor(CAR(representation));
  if (constructor == NULL) {
    return FALSE;
  }

  buffer->length = 0;
  buffer->bytes[0] = '\0';
  if (!domain_repr_append(buffer, constructor->name) ||
      !domain_repr_append(buffer, "(")) {
    return FALSE;
  }

  unsigned int seen = 0U;
  size_t argument_count = 0;
  SEXP arguments = CDR(representation);
  while (arguments != R_NilValue) {
    if (TYPEOF(arguments) != LISTSXP ||
        argument_count >= DOMAIN_REPR_MAX_ARGUMENTS ||
        ALTREP(arguments) || Rf_isS4(arguments) || Rf_isObject(arguments) ||
        !paradox_api_has_no_attributes(arguments)) {
      return FALSE;
    }
    const struct domain_repr_formal *formal =
      domain_repr_formal(TAG(arguments), constructor->mask);
    if (formal == NULL || (seen & formal->bit) != 0U) {
      return FALSE;
    }
    seen |= formal->bit;
    if ((argument_count != 0 && !domain_repr_append(buffer, ", ")) ||
        !domain_repr_append(buffer, formal->name) ||
        !domain_repr_append(buffer, " = ") ||
        !domain_repr_append_value(
          buffer,
          CAR(arguments),
          scipen_zero,
          allow_general_reals
        )) {
      return FALSE;
    }
    ++argument_count;
    arguments = CDR(arguments);
  }
  if ((seen & constructor->required) != constructor->required ||
      !domain_repr_append(buffer, ")")) {
    return FALSE;
  }
  return TRUE;
}

#endif

SEXP paradox_domain_simple_repr_id(SEXP representation) {
#if R_VERSION < R_Version(4, 5, 0)
  (void) representation;
  return R_NilValue;
#else
  SEXP scipen_symbol = PROTECT(Rf_install("scipen"));
  const int first_scipen_zero =
    domain_repr_scipen_is_zero(scipen_symbol);
  struct domain_repr_buffer first;
  if (!domain_repr_render(
      representation, first_scipen_zero, FALSE, &first
    )) {
    UNPROTECT(1);
    return R_NilValue;
  }

  SEXP result = PROTECT(Rf_mkString(first.bytes));

  const int second_scipen_zero =
    domain_repr_scipen_is_zero(scipen_symbol);
  struct domain_repr_buffer second;
  if (!domain_repr_render(
      representation, second_scipen_zero, FALSE, &second
    ) || first.length != second.length ||
      memcmp(first.bytes, second.bytes, first.length) != 0) {
    UNPROTECT(2);
    return R_NilValue;
  }

  UNPROTECT(2);
  return result;
#endif
}

SEXP paradox_domain_relaxed_repr_id(SEXP representation) {
#if R_VERSION < R_Version(4, 5, 0)
  (void) representation;
  return R_NilValue;
#else
  SEXP scipen_symbol = PROTECT(Rf_install("scipen"));
  SEXP outdec_symbol = PROTECT(Rf_install("OutDec"));
  int scipen;
  if (!domain_repr_scipen_value(scipen_symbol, &scipen) ||
      !domain_repr_outdec_is_dot(outdec_symbol)) {
    UNPROTECT(2);
    return R_NilValue;
  }

  /* Internal contract: representation_id() passes a rooted deep duplicate
   * owned by the direct constructor.  Numeric coercion can allocate, but no
   * callback can reach and mutate this private formatting source. */
  int observed_scipen;
  if (!domain_repr_scipen_value(scipen_symbol, &observed_scipen) ||
      observed_scipen != scipen ||
      !domain_repr_outdec_is_dot(outdec_symbol)) {
    UNPROTECT(2);
    return R_NilValue;
  }

  struct domain_repr_buffer first;
  if (!domain_repr_render(
      representation,
      scipen == 0,
      TRUE,
      &first
    ) || !domain_repr_scipen_value(scipen_symbol, &observed_scipen) ||
      observed_scipen != scipen ||
      !domain_repr_outdec_is_dot(outdec_symbol)) {
    UNPROTECT(2);
    return R_NilValue;
  }

  struct domain_repr_buffer second;
  if (!domain_repr_render(
      representation,
      scipen == 0,
      TRUE,
      &second
    ) || first.length != second.length ||
      memcmp(first.bytes, second.bytes, first.length) != 0) {
    UNPROTECT(2);
    return R_NilValue;
  }

  SEXP result = PROTECT(Rf_mkString(first.bytes));

  /* The source is private; only mutable formatting options need a final
   * commit audit after allocating the returned string. */
  if (!domain_repr_scipen_value(scipen_symbol, &observed_scipen) ||
      observed_scipen != scipen ||
      !domain_repr_outdec_is_dot(outdec_symbol)) {
    UNPROTECT(3);
    return R_NilValue;
  }

  UNPROTECT(3);
  return result;
#endif
}

SEXP paradox_domain_construct_frame(SEXP frame) {
  if (TYPEOF(frame) != ENVSXP || Rf_isObject(frame)) {
    return R_NilValue;
  }

  SEXP values = PROTECT(Rf_allocVector(VECSXP, DOMAIN_FRAME_VALUE_COUNT));

  /* This is deliberately validation order, not formal order. Returning to R
   * at the first unsupported value lets the historical validators produce
   * their diagnostics without forcing any later promise. */
  SEXP tags;
  if (!force_frame_value(
      frame, values, DOMAIN_FRAME_TAGS, "tags", &tags
    ) || !plain_metadata(tags) ||
      !unique_nonmissing_strings(tags, FALSE)) {
    goto decline;
  }

  SEXP cargo;
  if (!force_frame_value(
      frame, values, DOMAIN_FRAME_CARGO, "cargo", &cargo
    ) || !cargo_metadata_is_plain(cargo) ||
      !cargo_is_canonical_common(cargo, tags)) {
    goto decline;
  }

  SEXP cls;
  if (!force_frame_value(
      frame, values, DOMAIN_FRAME_CLS, "cls", &cls
    ) || !plain_metadata(cls) || !scalar_string(cls)) {
    goto decline;
  }
  const domain_kind_t kind = domain_kind_from_class(cls);
  if (kind == DOMAIN_KIND_UNKNOWN || !cargo_matches_kind(cargo, kind)) {
    goto decline;
  }

  SEXP grouping;
  if (!force_frame_value(
      frame, values, DOMAIN_FRAME_GROUPING, "grouping", &grouping
    ) || !plain_metadata(grouping) || !scalar_string(grouping)) {
    goto decline;
  }

  SEXP lower;
  if (!force_frame_value(
      frame, values, DOMAIN_FRAME_LOWER, "lower", &lower
    ) || !plain_metadata(lower) || !scalar_numeric(lower)) {
    goto decline;
  }

  SEXP upper;
  if (!force_frame_value(
      frame, values, DOMAIN_FRAME_UPPER, "upper", &upper
    ) || !plain_metadata(upper) || !scalar_numeric(upper)) {
    goto decline;
  }

  SEXP tolerance;
  if (!force_frame_value(
      frame, values, DOMAIN_FRAME_TOLERANCE, "tolerance", &tolerance
    ) || !plain_metadata(tolerance) || !scalar_numeric(tolerance)) {
    goto decline;
  }

  SEXP levels;
  if (!force_frame_value(
      frame, values, DOMAIN_FRAME_LEVELS, "levels", &levels
    ) || !plain_metadata(levels) || !levels_are_canonical(kind, levels)) {
    goto decline;
  }

  SEXP special_vals;
  if (!force_frame_value(
      frame,
      values,
      DOMAIN_FRAME_SPECIAL_VALS,
      "special_vals",
      &special_vals
    ) || !plain_metadata(special_vals) || !checkmate_list(special_vals)) {
    goto decline;
  }

  SEXP trafo;
  if (!force_frame_value(
      frame, values, DOMAIN_FRAME_TRAFO, "trafo", &trafo
    ) || !plain_metadata(trafo) || !function_or_null(trafo) ||
      (XLENGTH(special_vals) != 0 && trafo != R_NilValue)) {
    goto decline;
  }

  const char *expected_name = domain_storage_name(kind);
  SEXP expected_storage = PROTECT(Rf_mkString(expected_name));
  SEXP init_given = PROTECT(Rf_ScalarLogical(FALSE));
  SEXP shell = PROTECT(build_domain_shell(
    cls,
    grouping,
    cargo,
    lower,
    upper,
    tolerance,
    levels,
    special_vals,
    R_NilValue,
    tags,
    trafo,
    expected_storage,
    init_given,
    R_NilValue
  ));

  SEXP plan = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(plan, 0, shell);
  SET_VECTOR_ELT(plan, 1, expected_storage);
  UNPROTECT(5);
  return plan;

decline:
  UNPROTECT(1);
  return R_NilValue;
}
