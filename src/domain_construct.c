#include <string.h>

#include "paradox.h"
#include <R_ext/Utils.h>

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
