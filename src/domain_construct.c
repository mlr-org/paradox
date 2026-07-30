#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Utils.h>

#include "builtin_condition.h"
#include "domain_admission.h"
#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

typedef enum {
  DOMAIN_KIND_UNKNOWN = 0,
  DOMAIN_KIND_DBL,
  DOMAIN_KIND_INT,
  DOMAIN_KIND_FCT,
  DOMAIN_KIND_LGL,
  DOMAIN_KIND_UTY
} domain_kind_t;

typedef enum {
  NUMERIC_SOURCE_NONE = 0,
  NUMERIC_SOURCE_DBL = 1,
  NUMERIC_SOURCE_INT = 2
} numeric_source_kind_t;

static int plain_scalar_number_value(SEXP value, double *result);
static int plain_integer_bound(double value);

static int scalar_string(SEXP value) {
  return TYPEOF(value) == STRSXP && !ALTREP(value) &&
    !Rf_isS4(value) && !Rf_isObject(value) &&
    paradox_api_has_no_attributes(value) &&
    XLENGTH(value) == 1 &&
    STRING_ELT(value, 0) != NA_STRING;
}

static int scalar_numeric(SEXP value) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  if ((type != INTSXP && type != REALSXP) || ALTREP(value) ||
      Rf_isS4(value) || Rf_isObject(value) ||
      !paradox_api_has_no_attributes(value) ||
      XLENGTH(value) != 1) {
    return FALSE;
  }
  return TRUE;
}

static int representation_shell_attributes(SEXP value) {
  static const char *const allowed[] = {"names"};
  return !Rf_isS4(value) &&
    paradox_api_has_only_attributes(value, allowed, 1);
}

static int scalar_logical(SEXP value) {
  return TYPEOF(value) == LGLSXP && !ALTREP(value) &&
    !Rf_isS4(value) && !Rf_isObject(value) &&
    paradox_api_has_no_attributes(value) &&
    XLENGTH(value) == 1 && LOGICAL_ELT(value, 0) != NA_LOGICAL;
}

static int scalar_integer_code(SEXP value, int lower, int upper,
    int *result) {
  if (TYPEOF(value) != INTSXP || ALTREP(value) || Rf_isS4(value) ||
      Rf_isObject(value) ||
      !paradox_api_has_no_attributes(value) || XLENGTH(value) != 1) {
    return FALSE;
  }
  const int code = INTEGER_ELT(value, 0);
  if (code < lower || code > upper) {
    return FALSE;
  }
  *result = code;
  return TRUE;
}

static int function_or_null(SEXP value) {
  return value == R_NilValue || (!Rf_isS4(value) && Rf_isFunction(value));
}

/* Names on scalar constructor arguments are a representation artifact of
 * ordinary R subsetting and arithmetic (for example `param$lower`). They do
 * not identify a second Domain dimension. All inputs below are owned native
 * snapshots, so removing only this non-semantic attribute cannot mutate a
 * caller object. Classes and every other attribute remain fail-closed. */
static void discard_representation_names(SEXP value) {
  if (value != R_NilValue) {
    Rf_setAttrib(value, R_NamesSymbol, R_NilValue);
  }
}

/* Semantic list shells are canonical base lists. Opaque elements may carry
 * arbitrary classes, but classed outer containers are not a second Domain
 * dispatch mechanism. */
static int checkmate_list(SEXP value) {
  return TYPEOF(value) == VECSXP && !ALTREP(value) &&
    !Rf_isS4(value) && !Rf_isObject(value);
}

/*
 * Duplicate detection over a canonical string vector. `Rf_any_duplicated()`
 * builds a hash table, which is real per-row cost for the vectors admitted
 * here -- tags and levels are usually empty or a single element. Answering
 * those two lengths directly is exactly the same predicate with no allocation.
 */
static int strings_have_duplicates(SEXP values) {
  if (XLENGTH(values) < 2) return FALSE;
  return Rf_any_duplicated(values, FALSE) != 0;
}

static int unique_nonmissing_strings(SEXP values, int require_names) {
  if (TYPEOF(values) != STRSXP || ALTREP(values) || Rf_isS4(values)) {
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
    valid = !strings_have_duplicates(values);
  }
  UNPROTECT(1);
  return valid;
}

static int named_unique_list_or_null(SEXP value) {
  static const char *const allowed[] = {"names"};
  if (value == R_NilValue) {
    return TRUE;
  }
  PROTECT(value);
  if (!checkmate_list(value) || !paradox_api_has_only_attributes(
      value,
      allowed,
      1
    )) {
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
    !Rf_isS4(names) &&
    XLENGTH(names) == size &&
    unique_nonmissing_strings(names, TRUE);
  UNPROTECT(2);
  return valid;
}

static SEXP named_element(SEXP values, const char *name) {
  if (values == R_NilValue || TYPEOF(values) != VECSXP || ALTREP(values) ||
      Rf_isS4(values)) {
    return R_NilValue;
  }
  const R_xlen_t size = XLENGTH(values);
  SEXP names = Rf_getAttrib(values, R_NamesSymbol);
  if (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
      XLENGTH(names) != size) {
    return R_NilValue;
  }

  for (R_xlen_t index = 0; index < size; ++index) {
    if (paradox_domain_string_is(STRING_ELT(names, index), name)) {
      return VECTOR_ELT(values, index);
    }
  }
  return R_NilValue;
}

/* `cargo` is an owned outer list at this point. Own the nested containers
 * whose shells/names have Paradox semantics; callback and opaque leaf objects
 * deliberately retain identity. */
static void snapshot_cargo_containers(SEXP cargo) {
  if (cargo == R_NilValue) {
    return;
  }
  PROTECT(cargo);
  SEXP names = PROTECT(Rf_getAttrib(cargo, R_NamesSymbol));
  if (TYPEOF(names) != STRSXP || Rf_isS4(names) ||
      XLENGTH(names) != XLENGTH(cargo)) {
    UNPROTECT(2);
    return;
  }
  for (R_xlen_t index = 0; index < XLENGTH(cargo); ++index) {
    SEXP name = STRING_ELT(names, index);
    if (name == NA_STRING) {
      continue;
    }
    const char *bytes = CHAR(name);
    if (strcmp(bytes, "disable_in_tune") != 0 &&
        strcmp(bytes, "logscale") != 0 && strcmp(bytes, "repr") != 0) {
      continue;
    }
    SEXP value = VECTOR_ELT(cargo, index);
    if (value != R_NilValue &&
        (ALTREP(value) || Rf_isS4(value) || Rf_isObject(value))) {
      UNPROTECT(2);
      Rf_error(
        "Invalid built-in Domain state: interpreted cargo entries must "
        "use ordinary unclassed storage"
      );
    }
    if (value != R_NilValue) {
      static const char *const names_only[] = {"names"};
      const int attributes_ok = strcmp(bytes, "disable_in_tune") == 0
        ? paradox_api_has_only_attributes(value, names_only, 1)
        : paradox_api_has_no_attributes(value);
      if (!attributes_ok) {
        UNPROTECT(2);
        Rf_error(
          "Invalid built-in Domain state: interpreted cargo entries must "
          "use canonical attributes"
        );
      }
      /* Only an ordinary vector can be snapshotted. Anything else is left for
       * the canonical cargo validator below, which names the offending
       * constructor argument instead of reporting an internal snapshot type. */
      const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
      if (type == LGLSXP || type == INTSXP || type == REALSXP ||
          type == CPLXSXP || type == STRSXP || type == RAWSXP ||
          type == VECSXP) {
        SEXP snapshot = PROTECT(paradox_snapshot_semantic_vector(value));
        SET_VECTOR_ELT(cargo, index, snapshot);
        UNPROTECT(1);
      }
    }
  }
  UNPROTECT(2);
}

static int cargo_shell_is_ordinary(SEXP cargo) {
  static const char *const allowed[] = {"names"};
  if (cargo == R_NilValue) {
    return TRUE;
  }
  if (TYPEOF(cargo) != VECSXP || ALTREP(cargo) || Rf_isS4(cargo) ||
      Rf_isObject(cargo) ||
      !paradox_api_has_only_attributes(cargo, allowed, 1)) {
    return FALSE;
  }
  SEXP names = PROTECT(Rf_getAttrib(cargo, R_NamesSymbol));
  const int valid = names == R_NilValue ||
    (TYPEOF(names) == STRSXP && !ALTREP(names) && !Rf_isS4(names) &&
      !Rf_isObject(names) && paradox_api_has_no_attributes(names) &&
      XLENGTH(names) == XLENGTH(cargo));
  UNPROTECT(1);
  return valid;
}

static int cargo_names_are_canonical(SEXP cargo) {
  static const char *const allowed[] = {"names"};
  if (cargo == R_NilValue) {
    return TRUE;
  }
  PROTECT(cargo);
  if (TYPEOF(cargo) != VECSXP || ALTREP(cargo) || Rf_isS4(cargo) ||
      Rf_isObject(cargo) ||
      !paradox_api_has_only_attributes(cargo, allowed, 1)) {
    UNPROTECT(1);
    return FALSE;
  }
  const R_xlen_t size = XLENGTH(cargo);
  if (size == 0) {
    UNPROTECT(1);
    return FALSE;
  }

  SEXP names = PROTECT(Rf_getAttrib(cargo, R_NamesSymbol));
  if (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
      XLENGTH(names) != size ||
      !unique_nonmissing_strings(names, TRUE)) {
    UNPROTECT(2);
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP name = STRING_ELT(names, index);
    if (!paradox_domain_string_is(name, "logscale") && !paradox_domain_string_is(name, "aggr") &&
        !paradox_domain_string_is(name, "in_tune_fn") &&
        !paradox_domain_string_is(name, "disable_in_tune") &&
        !paradox_domain_string_is(name, "custom_check") && !paradox_domain_string_is(name, "repr")) {
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
  if (paradox_domain_string_is(class_name, "ParamDbl")) {
    return DOMAIN_KIND_DBL;
  }
  if (paradox_domain_string_is(class_name, "ParamInt")) {
    return DOMAIN_KIND_INT;
  }
  if (paradox_domain_string_is(class_name, "ParamFct")) {
    return DOMAIN_KIND_FCT;
  }
  if (paradox_domain_string_is(class_name, "ParamLgl")) {
    return DOMAIN_KIND_LGL;
  }
  if (paradox_domain_string_is(class_name, "ParamUty")) {
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
      !paradox_domain_string_is(STRING_ELT(storage_type, 0), expected_storage)) {
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
    return TYPEOF(levels) == LGLSXP && !ALTREP(levels) && !Rf_isS4(levels) &&
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
    if (paradox_domain_string_is(STRING_ELT(tags, index), target)) {
      return TRUE;
    }
  }
  return FALSE;
}

static int cargo_is_canonical_common(SEXP cargo, SEXP tags,
    paradox_domain_field_t *failure) {
  PROTECT(cargo);
  PROTECT(tags);
  if (!cargo_names_are_canonical(cargo)) {
    *failure = PARADOX_DOMAIN_FIELD_CARGO;
    UNPROTECT(2);
    return FALSE;
  }

  SEXP aggr = PROTECT(named_element(cargo, "aggr"));
  SEXP in_tune_fn = PROTECT(named_element(cargo, "in_tune_fn"));
  SEXP disable_in_tune = PROTECT(named_element(cargo, "disable_in_tune"));
  if (!function_or_null(aggr)) {
    *failure = PARADOX_DOMAIN_FIELD_CARGO_AGGR;
    UNPROTECT(5);
    return FALSE;
  }
  if (!function_or_null(in_tune_fn)) {
    *failure = PARADOX_DOMAIN_FIELD_CARGO_IN_TUNE_FN;
    UNPROTECT(5);
    return FALSE;
  }
  if (!named_unique_list_or_null(disable_in_tune)) {
    *failure = PARADOX_DOMAIN_FIELD_CARGO_DISABLE_IN_TUNE;
    UNPROTECT(5);
    return FALSE;
  }

  const int internal_tuning = has_tag(tags, "internal_tuning");
  if (internal_tuning && aggr == R_NilValue) {
    *failure = PARADOX_DOMAIN_FIELD_CARGO_TUNING_AGGR;
    UNPROTECT(5);
    return FALSE;
  }
  if ((in_tune_fn != R_NilValue || disable_in_tune != R_NilValue) &&
      !internal_tuning) {
    *failure = PARADOX_DOMAIN_FIELD_CARGO_TUNING_TAG;
    UNPROTECT(5);
    return FALSE;
  }
  if ((in_tune_fn == R_NilValue) != (disable_in_tune == R_NilValue)) {
    *failure = PARADOX_DOMAIN_FIELD_CARGO_TUNING_PAIR;
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
  SEXP logscale = PROTECT(named_element(cargo, "logscale"));
  int valid;
  if (kind == DOMAIN_KIND_UTY) {
    valid = function_or_null(custom_check) && scalar_string(repr) &&
      logscale == R_NilValue;
  } else if (kind == DOMAIN_KIND_DBL || kind == DOMAIN_KIND_INT) {
    valid = custom_check == R_NilValue && repr == R_NilValue &&
      (logscale == R_NilValue ||
        (scalar_logical(logscale) &&
          paradox_api_has_no_attributes(logscale) &&
          LOGICAL_ELT(logscale, 0) == TRUE));
  } else {
    valid = custom_check == R_NilValue && repr == R_NilValue &&
      logscale == R_NilValue;
  }

  UNPROTECT(4);
  return valid;
}

static int cargo_is_canonical(SEXP cargo, SEXP tags, domain_kind_t kind,
    paradox_domain_field_t *failure) {
  paradox_domain_field_t observed = PARADOX_DOMAIN_FIELD_CARGO;
  int valid = cargo_is_canonical_common(cargo, tags, &observed);
  if (valid && !cargo_matches_kind(cargo, kind)) {
    observed = PARADOX_DOMAIN_FIELD_CARGO;
    valid = FALSE;
  }
  if (!valid && failure != NULL) {
    *failure = observed;
  }
  return valid;
}

/* Constructor-boundary diagnostics for the documented cargo arguments. The
 * admission owner reports the same codes through the shared field-name
 * template; this direct entry names the argument the caller actually typed. */
static void cargo_argument_error(paradox_domain_field_t failure) {
  switch (failure) {
  case PARADOX_DOMAIN_FIELD_CARGO_AGGR:
    Rf_error("`aggr` must be a function");
  case PARADOX_DOMAIN_FIELD_CARGO_IN_TUNE_FN:
    Rf_error("`in_tune_fn` must be a function");
  case PARADOX_DOMAIN_FIELD_CARGO_DISABLE_IN_TUNE:
    Rf_error("`disable_in_tune` must be a uniquely named list");
  case PARADOX_DOMAIN_FIELD_CARGO_TUNING_AGGR:
    Rf_error(
      "Parameters tagged 'internal_tuning' require an `aggr` function"
    );
  case PARADOX_DOMAIN_FIELD_CARGO_TUNING_TAG:
    Rf_error(
      "Arguments in_tune_fn and disable_in_tune require the tag "
      "'internal_tuning' to be present."
    );
  case PARADOX_DOMAIN_FIELD_CARGO_TUNING_PAIR:
    Rf_error(
      "Arguments in_tune_fn and disable_in_tune must both be present"
    );
  default:
    Rf_error(
      "Invalid built-in Domain state; Paradox 2 supports only canonical "
      "p_dbl, p_int, p_fct, p_lgl, and p_uty Domains."
    );
  }
}

static paradox_builtin_domain_kind_t public_domain_kind(domain_kind_t kind) {
  switch (kind) {
  case DOMAIN_KIND_DBL: return PARADOX_BUILTIN_DOMAIN_DBL;
  case DOMAIN_KIND_INT: return PARADOX_BUILTIN_DOMAIN_INT;
  case DOMAIN_KIND_FCT: return PARADOX_BUILTIN_DOMAIN_FCT;
  case DOMAIN_KIND_LGL: return PARADOX_BUILTIN_DOMAIN_LGL;
  case DOMAIN_KIND_UTY: return PARADOX_BUILTIN_DOMAIN_UTY;
  case DOMAIN_KIND_UNKNOWN: return PARADOX_BUILTIN_DOMAIN_UNKNOWN;
  }
  return PARADOX_BUILTIN_DOMAIN_UNKNOWN;
}

static int canonical_scalar_missing_number(SEXP value) {
  return TYPEOF(value) == REALSXP && !ALTREP(value) &&
    !Rf_isS4(value) && !Rf_isObject(value) &&
    paradox_api_has_no_attributes(value) &&
    XLENGTH(value) == 1 && ISNA(REAL_ELT(value, 0));
}

static int canonical_opaque_list(SEXP value) {
  static const char *const allowed[] = {"names"};
  if (TYPEOF(value) != VECSXP || ALTREP(value) || Rf_isS4(value) ||
      Rf_isObject(value) ||
      !paradox_api_has_only_attributes(value, allowed, 1)) {
    return FALSE;
  }
  SEXP names = PROTECT(Rf_getAttrib(value, R_NamesSymbol));
  const int valid = names == R_NilValue ||
    (TYPEOF(names) == STRSXP && !ALTREP(names) && !Rf_isS4(names) &&
      !Rf_isObject(names) && paradox_api_has_no_attributes(names) &&
      XLENGTH(names) == XLENGTH(value));
  UNPROTECT(1);
  return valid;
}

static int exact_no_default(SEXP value) {
  static const char *const allowed[] = {"class"};
  static const char *const expected[] = {"NoDefault"};
  if (!Rf_inherits(value, "NoDefault")) return 0;
  if (TYPEOF(value) != VECSXP || ALTREP(value) || Rf_isS4(value) ||
      XLENGTH(value) != 0 ||
      !paradox_api_has_only_attributes(value, allowed, 1)) {
    return -1;
  }
  R_xlen_t work_since_interrupt = 0;
  SEXP classes = PROTECT(Rf_getAttrib(value, R_ClassSymbol));
  const int exact = !Rf_isS4(classes) &&
    paradox_api_has_no_attributes(classes) &&
    paradox_domain_exact_string_vector(
      classes,
      expected,
      1,
      &work_since_interrupt
    );
  UNPROTECT(1);
  return exact ? 1 : -1;
}

static int exact_requirement_condition(SEXP condition,
    R_xlen_t *work_since_interrupt) {
  static const char *const equal[] = {"CondEqual", "Condition"};
  static const char *const any_of[] = {"CondAnyOf", "Condition"};
  if (TYPEOF(condition) != VECSXP || ALTREP(condition) ||
      Rf_isS4(condition)) return FALSE;
  SEXP classes = PROTECT(Rf_getAttrib(condition, R_ClassSymbol));
  const int plain = !Rf_isS4(classes) &&
    paradox_api_has_no_attributes(classes);
  const int known = plain && (paradox_domain_exact_string_vector(
      classes,
      equal,
      2,
      work_since_interrupt
    ) || paradox_domain_exact_string_vector(
      classes,
      any_of,
      2,
      work_since_interrupt
    ));
  UNPROTECT(1);
  if (!known) return FALSE;
  paradox_builtin_condition_kind_t condition_kind;
  SEXP rhs = R_NilValue;
  return paradox_builtin_condition_exact(
    condition,
    &condition_kind,
    &rhs,
    work_since_interrupt
  );
}

SEXP paradox_snapshot_builtin_requirements(SEXP requirements,
    R_xlen_t *work_since_interrupt) {
  static const char *const allowed[] = {"names"};
  static const char *const expected_names[] = {"on", "cond"};
  if (requirements == R_NilValue) return R_NilValue;
  if (TYPEOF(requirements) != VECSXP || ALTREP(requirements) ||
      Rf_isS4(requirements) ||
      Rf_isObject(requirements) ||
      !paradox_api_has_no_attributes(requirements)) {
    return R_UnboundValue;
  }

  const R_xlen_t size = XLENGTH(requirements);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  /*
   * Allocation may run a pending finalizer.  Revalidate the outer carrier
   * afterwards, then retain every exact row identity before admitting any
   * semantic field: materializing an earlier Condition RHS may re-enter R and
   * rewrite a later caller-owned row.
   */
  if (TYPEOF(requirements) != VECSXP || ALTREP(requirements) ||
      Rf_isS4(requirements) || Rf_isObject(requirements) ||
      !paradox_api_has_no_attributes(requirements) ||
      !paradox_capture_list_identities(
        requirements,
        R_NilValue,
        result
      )) {
    UNPROTECT(1);
    return R_UnboundValue;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP requirement = PROTECT(VECTOR_ELT(result, index));
    SEXP owned_requirement = PROTECT(Rf_allocVector(VECSXP, 2));
    SEXP owned_names = PROTECT(Rf_allocVector(STRSXP, 2));
    /*
     * The row carriers are now allocated.  Capture each field beside its
     * matching name in one allocation-free pass before observing `on` or an
     * RHS.  This closes the same reentry boundary within a single row.
     */
    if (TYPEOF(requirement) != VECSXP || ALTREP(requirement) ||
        Rf_isS4(requirement) || Rf_isObject(requirement) ||
        XLENGTH(requirement) != 2 ||
        !paradox_api_has_only_attributes(requirement, allowed, 1) ||
        !paradox_capture_list_identities(
          requirement,
          owned_names,
          owned_requirement
        ) ||
        !paradox_domain_exact_string_vector(
          owned_names,
          expected_names,
          2,
          work_since_interrupt
        )) {
      UNPROTECT(4);
      return R_UnboundValue;
    }

    SEXP raw_on = VECTOR_ELT(owned_requirement, 0);
    if (TYPEOF(raw_on) != STRSXP || Rf_isS4(raw_on) ||
        Rf_isObject(raw_on) ||
        !paradox_api_has_no_attributes(raw_on)) {
      UNPROTECT(4);
      return R_UnboundValue;
    }
    SEXP on = PROTECT(paradox_snapshot_semantic_vector(
      raw_on
    ));
    if (!scalar_string(on) || CHAR(STRING_ELT(on, 0))[0] == '\0') {
      UNPROTECT(5);
      return R_UnboundValue;
    }

    SEXP condition = PROTECT(paradox_builtin_condition_snapshot(
      VECTOR_ELT(owned_requirement, 1),
      work_since_interrupt
    ));
    if (condition == R_UnboundValue) {
      UNPROTECT(6);
      return R_UnboundValue;
    }

    SET_VECTOR_ELT(owned_requirement, 0, on);
    SET_VECTOR_ELT(owned_requirement, 1, condition);
    Rf_setAttrib(owned_requirement, R_NamesSymbol, owned_names);
    SET_VECTOR_ELT(result, index, owned_requirement);
    UNPROTECT(5);
  }
  UNPROTECT(1);
  return result;
}

static int canonical_requirements(SEXP requirements,
    R_xlen_t *work_since_interrupt) {
  static const char *const allowed[] = {"names"};
  static const char *const expected_names[] = {"on", "cond"};
  if (requirements == R_NilValue) return TRUE;
  if (TYPEOF(requirements) != VECSXP || ALTREP(requirements) ||
      Rf_isS4(requirements) ||
      Rf_isObject(requirements) ||
      !paradox_api_has_no_attributes(requirements)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(requirements); ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP requirement = VECTOR_ELT(requirements, index);
    if (TYPEOF(requirement) != VECSXP || ALTREP(requirement) ||
        Rf_isS4(requirement) ||
        Rf_isObject(requirement) || XLENGTH(requirement) != 2 ||
        !paradox_api_has_only_attributes(requirement, allowed, 1)) {
      return FALSE;
    }
    SEXP names = PROTECT(Rf_getAttrib(requirement, R_NamesSymbol));
    const int exact_names = !Rf_isS4(names) &&
      paradox_api_has_no_attributes(names) &&
      paradox_domain_exact_string_vector(
        names,
        expected_names,
        2,
        work_since_interrupt
      );
    UNPROTECT(1);
    SEXP on = VECTOR_ELT(requirement, 0);
    if (!exact_names || !scalar_string(on) ||
        CHAR(STRING_ELT(on, 0))[0] == '\0' ||
        !exact_requirement_condition(
          VECTOR_ELT(requirement, 1),
          work_since_interrupt
        )) {
      return FALSE;
    }
  }
  return TRUE;
}

const char *paradox_domain_field_name(paradox_domain_field_t field) {
  switch (field) {
  case PARADOX_DOMAIN_FIELD_NONE: return "unknown";
  case PARADOX_DOMAIN_FIELD_ID: return "id";
  case PARADOX_DOMAIN_FIELD_CLASS_STORAGE: return "class/storage_type";
  case PARADOX_DOMAIN_FIELD_GROUPING: return "grouping";
  case PARADOX_DOMAIN_FIELD_CARGO: return "cargo";
  case PARADOX_DOMAIN_FIELD_BOUNDS: return "lower/upper/tolerance";
  case PARADOX_DOMAIN_FIELD_LEVELS: return "levels";
  case PARADOX_DOMAIN_FIELD_SPECIAL_VALUES: return "special_vals";
  case PARADOX_DOMAIN_FIELD_DEFAULT: return "default";
  case PARADOX_DOMAIN_FIELD_REQUIRED_DEFAULT: return "required/default";
  case PARADOX_DOMAIN_FIELD_DEFAULT_VALUE: return "default value";
  case PARADOX_DOMAIN_FIELD_TAGS: return ".tags";
  case PARADOX_DOMAIN_FIELD_TAGS_DUPLICATE: return ".tags (duplicates)";
  case PARADOX_DOMAIN_FIELD_TRAFO: return ".trafo";
  case PARADOX_DOMAIN_FIELD_REQUIREMENTS: return ".requirements";
  case PARADOX_DOMAIN_FIELD_INIT: return ".init_given/.init";
  case PARADOX_DOMAIN_FIELD_INIT_TRAFO: return "Initial value and trafo";
  case PARADOX_DOMAIN_FIELD_INIT_VALUE: return "initial value";
  case PARADOX_DOMAIN_FIELD_LEVELS_DUPLICATE: return "levels (duplicates)";
  case PARADOX_DOMAIN_FIELD_CARGO_AGGR: return "cargo (aggr)";
  case PARADOX_DOMAIN_FIELD_CARGO_IN_TUNE_FN: return "cargo (in_tune_fn)";
  case PARADOX_DOMAIN_FIELD_CARGO_DISABLE_IN_TUNE:
    return "cargo (disable_in_tune)";
  case PARADOX_DOMAIN_FIELD_CARGO_TUNING_TAG:
  case PARADOX_DOMAIN_FIELD_CARGO_TUNING_PAIR:
  case PARADOX_DOMAIN_FIELD_CARGO_TUNING_AGGR:
    return "cargo (internal-tuning pairing)";
  }
  return "unknown";
}

static paradox_builtin_value_spec_t admitted_value_spec(domain_kind_t kind,
    SEXP lower, SEXP upper, SEXP tolerance, SEXP levels,
    SEXP special_values) {
  double lower_value = NA_REAL;
  double upper_value = NA_REAL;
  double tolerance_value = NA_REAL;
  if (kind == DOMAIN_KIND_DBL || kind == DOMAIN_KIND_INT) {
    if (!plain_scalar_number_value(lower, &lower_value) ||
        !plain_scalar_number_value(upper, &upper_value) ||
        !plain_scalar_number_value(tolerance, &tolerance_value)) {
      Rf_error("Internal error: invalid admitted numeric Domain");
    }
  }
  const paradox_builtin_value_spec_t result = {
    public_domain_kind(kind),
    lower_value,
    upper_value,
    tolerance_value,
    levels,
    special_values
  };
  return result;
}

int paradox_prepare_builtin_special_values(SEXP cls, SEXP storage,
    SEXP special_values, paradox_special_values_receipt_t *receipt,
    R_xlen_t *work_since_interrupt) {
  receipt->special_values = R_NilValue;
  receipt->typed = FALSE;

  const domain_kind_t private_kind = domain_kind(cls, storage);
  if (private_kind == DOMAIN_KIND_UNKNOWN) {
    /*
     * Preserve the row owner's class/storage diagnostic precedence.  There is
     * no built-in kind whose leaf policy can be selected at this point.
     */
    return TRUE;
  }
  if (!canonical_opaque_list(special_values)) {
    return FALSE;
  }

  const int typed = private_kind != DOMAIN_KIND_UTY;
  if (typed) {
    for (R_xlen_t index = 0; index < XLENGTH(special_values); ++index) {
      paradox_account_work(work_since_interrupt);
      /*
       * ALTREP() is a structural predicate.  In particular, do not call
       * Length/Elt/DATAPTR or duplicate the leaf before rejecting it.
       */
      if (ALTREP(VECTOR_ELT(special_values, index))) {
        return FALSE;
      }
    }
  }

  receipt->special_values = special_values;
  receipt->typed = typed;
  return TRUE;
}

/*
 * The schema half of canonical row admission: identity, closed kind, grouping,
 * tags, cargo, transformation, special values, bounds, and levels. It is the
 * complete rule set for the fields an operation on an existing Domain
 * interprets, and `paradox_admit_builtin_domain_row()` below is its only
 * extension -- the default/requirement/initialization rules a constructor
 * additionally owns. Splitting the owner here keeps one implementation of
 * every rule; it does not create a second admission mode.
 */
int paradox_admit_builtin_domain_schema_row(SEXP id, SEXP cls, SEXP grouping,
    SEXP cargo, SEXP lower, SEXP upper, SEXP tolerance, SEXP levels,
    SEXP special_values, SEXP storage, SEXP tags, SEXP trafo,
    const paradox_special_values_receipt_t *special_receipt,
    paradox_builtin_domain_kind_t *kind, double *admitted_bounds,
    paradox_domain_field_t *failure, R_xlen_t *work_since_interrupt) {
#define REJECT_DOMAIN_FIELD(field_) do { \
  *failure = (field_); \
  return FALSE; \
} while (0)
  *kind = PARADOX_BUILTIN_DOMAIN_UNKNOWN;
  *failure = PARADOX_DOMAIN_FIELD_NONE;
  if (id != R_NilValue &&
      (!scalar_string(id) || CHAR(STRING_ELT(id, 0))[0] == '\0')) {
    REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_ID);
  }
  const domain_kind_t private_kind = domain_kind(cls, storage);
  if (private_kind == DOMAIN_KIND_UNKNOWN) {
    REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_CLASS_STORAGE);
  }
  *kind = public_domain_kind(private_kind);
  if (!scalar_string(grouping) ||
      (private_kind != DOMAIN_KIND_FCT && !paradox_domain_string_is(
        STRING_ELT(grouping, 0),
        CHAR(STRING_ELT(cls, 0))
      ))) {
    REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_GROUPING);
  }
  if (TYPEOF(tags) != STRSXP || ALTREP(tags) || Rf_isS4(tags) ||
      Rf_isObject(tags) ||
      !paradox_api_has_no_attributes(tags)) {
    REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_TAGS);
  }
  for (R_xlen_t index = 0; index < XLENGTH(tags); ++index) {
    paradox_account_work(work_since_interrupt);
    if (STRING_ELT(tags, index) == NA_STRING) {
      REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_TAGS);
    }
  }
  if (strings_have_duplicates(tags)) {
    REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_TAGS_DUPLICATE);
  }
  {
    paradox_domain_field_t cargo_failure = PARADOX_DOMAIN_FIELD_CARGO;
    if (!cargo_is_canonical(cargo, tags, private_kind, &cargo_failure)) {
      REJECT_DOMAIN_FIELD(cargo_failure);
    }
  }
  if (!function_or_null(trafo)) {
    REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_TRAFO);
  }
  SEXP logscale = named_element(cargo, "logscale");
  if (logscale != R_NilValue &&
      (private_kind != DOMAIN_KIND_DBL || trafo == R_NilValue)) {
    REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_CARGO);
  }
  const int typed = private_kind != DOMAIN_KIND_UTY;
  if (special_receipt == NULL ||
      special_receipt->special_values != special_values ||
      special_receipt->typed != typed) {
    REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_SPECIAL_VALUES);
  }
  if (XLENGTH(special_values) != 0 && trafo != R_NilValue) {
    REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_SPECIAL_VALUES);
  }

  double admitted_lower = NA_REAL;
  double admitted_upper = NA_REAL;
  double admitted_tolerance = NA_REAL;
  if (private_kind == DOMAIN_KIND_DBL || private_kind == DOMAIN_KIND_INT) {
    if (!plain_scalar_number_value(lower, &admitted_lower) ||
        !plain_scalar_number_value(upper, &admitted_upper) ||
        !plain_scalar_number_value(tolerance, &admitted_tolerance) ||
        admitted_lower > admitted_upper || !R_FINITE(admitted_tolerance) ||
        admitted_tolerance < 0.0 ||
        (private_kind == DOMAIN_KIND_INT &&
          (!plain_integer_bound(admitted_lower) ||
            !plain_integer_bound(admitted_upper) ||
            admitted_tolerance > 0.5))) {
      REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_BOUNDS);
    }
    if (levels != R_NilValue) {
      REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_LEVELS);
    }
  } else {
    if (!canonical_scalar_missing_number(lower) ||
        !canonical_scalar_missing_number(upper) ||
        !canonical_scalar_missing_number(tolerance)) {
      REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_BOUNDS);
    }
    if (private_kind == DOMAIN_KIND_FCT && TYPEOF(levels) == STRSXP &&
        !ALTREP(levels) && !Rf_isS4(levels) && !Rf_isObject(levels) &&
        paradox_api_has_no_attributes(levels) && XLENGTH(levels) != 0 &&
        strings_have_duplicates(levels)) {
      REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_LEVELS_DUPLICATE);
    }
    if (!levels_are_canonical(private_kind, levels) || (levels != R_NilValue &&
        (Rf_isS4(levels) || Rf_isObject(levels) ||
          !paradox_api_has_no_attributes(levels)))) {
      REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_LEVELS);
    }
  }
  /* Publish the values this admission actually accepted. Re-reading the
   * caller's scalars afterwards would reopen the window the tags and levels
   * hash tables above leave for a pending finalizer. */
  if (admitted_bounds != NULL) {
    admitted_bounds[0] = admitted_lower;
    admitted_bounds[1] = admitted_upper;
    admitted_bounds[2] = admitted_tolerance;
  }
  return TRUE;
#undef REJECT_DOMAIN_FIELD
}

int paradox_admit_builtin_domain_row(SEXP id, SEXP cls, SEXP grouping,
    SEXP cargo, SEXP lower, SEXP upper, SEXP tolerance, SEXP levels,
    SEXP special_values, SEXP default_value, SEXP storage, SEXP tags,
    SEXP trafo, SEXP requirements, SEXP init_given, SEXP init_value,
    const paradox_special_values_receipt_t *special_receipt,
    paradox_builtin_domain_kind_t *kind, paradox_domain_field_t *failure,
    paradox_builtin_value_result_t *value_failure,
    R_xlen_t *work_since_interrupt) {
#define REJECT_DOMAIN_FIELD(field_) do { \
  *failure = (field_); \
  return FALSE; \
} while (0)
  if (value_failure != NULL) {
    const paradox_builtin_value_result_t valid = {
      PARADOX_BUILTIN_VALUE_OK,
      NA_REAL,
      NA_REAL,
      NA_REAL,
      FALSE
    };
    *value_failure = valid;
  }
  double admitted_bounds[3] = {NA_REAL, NA_REAL, NA_REAL};
  if (!paradox_admit_builtin_domain_schema_row(
      id,
      cls,
      grouping,
      cargo,
      lower,
      upper,
      tolerance,
      levels,
      special_values,
      storage,
      tags,
      trafo,
      special_receipt,
      kind,
      admitted_bounds,
      failure,
      work_since_interrupt
    )) {
    return FALSE;
  }
  const domain_kind_t private_kind = domain_kind(cls, storage);
  const double admitted_lower = admitted_bounds[0];
  const double admitted_upper = admitted_bounds[1];
  const double admitted_tolerance = admitted_bounds[2];

  if (default_value == R_MissingArg || default_value == R_UnboundValue ||
      Rf_inherits(default_value, "TuneToken")) {
    REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_DEFAULT);
  }
  const int marker = exact_no_default(default_value);
  if (marker < 0) {
    REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_DEFAULT);
  }
  if (marker == 0 && has_tag(tags, "required")) {
    REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_REQUIRED_DEFAULT);
  }
  const paradox_builtin_value_spec_t value_spec = {
    public_domain_kind(private_kind),
    admitted_lower,
    admitted_upper,
    admitted_tolerance,
    levels,
    special_values
  };
  if (marker == 0) {
    const paradox_builtin_value_result_t checked =
      paradox_builtin_value_check(
        &value_spec,
        default_value,
        private_kind != DOMAIN_KIND_UTY,
        work_since_interrupt
      );
    if (checked.failure != PARADOX_BUILTIN_VALUE_OK) {
      if (value_failure != NULL) *value_failure = checked;
      REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_DEFAULT_VALUE);
    }
  }
  if (!canonical_requirements(requirements, work_since_interrupt)) {
    REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_REQUIREMENTS);
  }
  if (TYPEOF(init_given) != LGLSXP || ALTREP(init_given) ||
      Rf_isS4(init_given) || Rf_isObject(init_given) ||
      !paradox_api_has_no_attributes(init_given) || XLENGTH(init_given) != 1 ||
      LOGICAL_ELT(init_given, 0) == NA_LOGICAL ||
      init_value == R_MissingArg || init_value == R_UnboundValue ||
      Rf_inherits(init_value, "TuneToken") ||
      (LOGICAL_ELT(init_given, 0) == FALSE && init_value != R_NilValue)) {
    REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_INIT);
  }
  if (LOGICAL_ELT(init_given, 0) == TRUE && trafo != R_NilValue) {
    REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_INIT_TRAFO);
  }
  if (LOGICAL_ELT(init_given, 0) == TRUE) {
    const paradox_builtin_value_result_t checked =
      paradox_builtin_value_check(
        &value_spec,
        init_value,
        private_kind != DOMAIN_KIND_UTY,
        work_since_interrupt
      );
    if (checked.failure != PARADOX_BUILTIN_VALUE_OK) {
      if (value_failure != NULL) *value_failure = checked;
      REJECT_DOMAIN_FIELD(PARADOX_DOMAIN_FIELD_INIT_VALUE);
    }
  }
  return TRUE;
#undef REJECT_DOMAIN_FIELD
}

static SEXP cargo_with_logscale(SEXP cargo) {
  const R_xlen_t size = cargo == R_NilValue ? 0 : XLENGTH(cargo);
  if (size == R_XLEN_T_MAX) {
    Rf_error("Numeric Domain cargo is too large");
  }
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size + 1));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, size + 1));
  SET_VECTOR_ELT(result, 0, Rf_ScalarLogical(TRUE));
  SET_STRING_ELT(names, 0, Rf_mkChar("logscale"));
  if (size != 0) {
    SEXP cargo_names = PROTECT(Rf_getAttrib(cargo, R_NamesSymbol));
    for (R_xlen_t index = 0; index < size; ++index) {
      SET_VECTOR_ELT(result, index + 1, VECTOR_ELT(cargo, index));
      SET_STRING_ELT(names, index + 1, STRING_ELT(cargo_names, index));
    }
    UNPROTECT(1);
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
}

static SEXP integer_logscale_trafo(SEXP lower, SEXP upper) {
  SEXP namespace_name = PROTECT(Rf_mkString("paradox"));
  SEXP namespace = PROTECT(R_FindNamespace(namespace_name));
  if (TYPEOF(namespace) != ENVSXP) {
    UNPROTECT(2);
    Rf_error("Internal error: Paradox namespace is unavailable");
  }
  /* The locked namespace owns this package-private factory.  Rf_findFun() is
   * public API; the frame-only lookup helper is not available under
   * R_NO_REMAP on all supported R releases. */
  SEXP factory = PROTECT(Rf_findFun(
    Rf_install(".make_p_int_logscale_trafo"),
    namespace
  ));
  if (!Rf_isFunction(factory)) {
    UNPROTECT(3);
    Rf_error("Internal error: integer logscale factory is unavailable");
  }
  SEXP call = PROTECT(Rf_lang3(factory, lower, upper));
  SEXP result = PROTECT(Rf_eval(call, namespace));
  if (!Rf_isFunction(result)) {
    UNPROTECT(5);
    Rf_error("Internal error: integer logscale factory returned no function");
  }
  UNPROTECT(5);
  return result;
}

static SEXP double_logscale_trafo(void) {
  SEXP result = PROTECT(Rf_findFun(Rf_install("exp"), R_BaseEnv));
  if (!Rf_isFunction(result)) {
    UNPROTECT(1);
    Rf_error("Internal error: base exp function is unavailable");
  }
  UNPROTECT(1);
  return result;
}

static SEXP one_element_list(SEXP value) {
  PROTECT(value);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(result, 0, value);
  UNPROTECT(2);
  return result;
}

static SEXP build_domain_shell(
    SEXP id,
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
    SEXP init_value,
    SEXP requirements,
    R_xlen_t *work_since_interrupt) {
  PROTECT(id);
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
  PROTECT(requirements);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, PARADOX_DOMAIN_COLUMN_COUNT));
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_ID, id);
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_CLS, cls);
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_GROUPING, grouping);
  SEXP cargo_column = PROTECT(one_element_list(cargo));
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_CARGO, cargo_column);
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_LOWER, lower);
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_UPPER, upper);
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_TOLERANCE, tolerance);
  SEXP levels_column = PROTECT(one_element_list(levels));
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_LEVELS, levels_column);
  SEXP special_column = PROTECT(one_element_list(special_vals));
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_SPECIAL_VALS, special_column);
  SEXP default_column = PROTECT(one_element_list(default_value));
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_DEFAULT, default_column);
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_STORAGE_TYPE, storage_type);
  SEXP tags_column = PROTECT(one_element_list(tags));
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_TAGS, tags_column);
  SEXP trafo_column = PROTECT(one_element_list(trafo));
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_TRAFO, trafo_column);
  SEXP requirements_column = PROTECT(one_element_list(requirements));
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_REQUIREMENTS, requirements_column);
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_INIT_GIVEN, init_given);
  SEXP init_column = PROTECT(one_element_list(init_value));
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_INIT, init_column);
  SEXP prepared = PROTECT(paradox_domain_prepare_facade(
    result,
    STRING_ELT(cls, 0),
    work_since_interrupt
  ));

  UNPROTECT(26);
  return prepared;
}

static SEXP snapshot_constructor_vector(SEXP value) {
  if (Rf_isObject(value)) {
    Rf_error(
      "Invalid built-in Domain state: constructor fields must use unclassed "
      "canonical vectors"
    );
  }
  if (!representation_shell_attributes(value)) {
    Rf_error(
      "Invalid built-in Domain state: structural constructor fields may "
      "carry at most scalar representation names"
    );
  }
  return paradox_snapshot_semantic_vector(value);
}

static SEXP snapshot_constructor_cargo(SEXP cargo) {
  if (!cargo_shell_is_ordinary(cargo)) {
    Rf_error(
      "Invalid built-in Domain state: constructor fields must use unclassed "
      "canonical vectors"
    );
  }
  return paradox_snapshot_semantic_vector(cargo);
}

static SEXP snapshot_constructor_special_values(SEXP special_vals) {
  if (!canonical_opaque_list(special_vals)) {
    Rf_error(
      "Invalid built-in Domain state: constructor fields must use unclassed "
      "canonical vectors"
    );
  }
  return paradox_snapshot_semantic_vector(special_vals);
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
    SEXP init_value,
    SEXP numeric_source_kind,
    SEXP numeric_logscale,
    SEXP id,
    SEXP requirements) {
  /*
   * Every interpreted field is admitted immediately before its own semantic
   * snapshot below. Earlier field materialization may allocate or invoke a
   * stable ALTREP method, so one global preflight here would leave later
   * fields vulnerable to structural mutation before they were copied.
   */
  enum {
    ROOT_CLS = 0,
    ROOT_GROUPING,
    ROOT_CARGO,
    ROOT_LOWER,
    ROOT_UPPER,
    ROOT_TOLERANCE,
    ROOT_LEVELS,
    ROOT_SPECIAL_VALS,
    ROOT_TAGS,
    ROOT_STORAGE,
    ROOT_INIT_GIVEN,
    ROOT_NUMERIC_SOURCE_KIND,
    ROOT_NUMERIC_LOGSCALE,
    ROOT_ID,
    ROOT_REQUIREMENTS,
    ROOT_DEFAULT,
    ROOT_INIT_VALUE,
    ROOT_TRAFO,
    ROOT_COUNT
  };
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, ROOT_COUNT));

  /*
   * Select the kind and own its special-values shell before observing any
   * other semantic vector.  A hostile ALTREP leaf may also have been supplied
   * as a default, init, requirement RHS, or another constructor argument; for
   * a typed kind its special role must reject it before one of those aliases
   * can trigger an Elt method.
   */
  SET_VECTOR_ELT(roots, ROOT_CLS, snapshot_constructor_vector(cls));
  SET_VECTOR_ELT(
    roots, ROOT_STORAGE, snapshot_constructor_vector(storage_type)
  );
  SET_VECTOR_ELT(
    roots, ROOT_SPECIAL_VALS,
    snapshot_constructor_special_values(special_vals)
  );
  /*
   * Scalar representation names are not Domain semantics.  Remove them from
   * these privately owned snapshots before selecting the kind.  The remaining
   * scalar fields are normalized after their snapshots have been installed.
   */
  discard_representation_names(VECTOR_ELT(roots, ROOT_CLS));
  discard_representation_names(VECTOR_ELT(roots, ROOT_STORAGE));
  R_xlen_t admission_work = 0;
  paradox_special_values_receipt_t special_receipt;
  if (!paradox_prepare_builtin_special_values(
      VECTOR_ELT(roots, ROOT_CLS),
      VECTOR_ELT(roots, ROOT_STORAGE),
      VECTOR_ELT(roots, ROOT_SPECIAL_VALS),
      &special_receipt,
      &admission_work
    )) {
    UNPROTECT(1);
    Rf_error("Invalid built-in Domain final state in field `special_vals`");
  }
  /* The outer shell was just snapshotted above.  Typed atomic specials are
   * semantic values rather than identity tokens, so detach them now; S4 and
   * non-atomic typed specials, and all ParamUty specials, keep identity. */
  paradox_own_builtin_special_value_leaves(
    VECTOR_ELT(roots, ROOT_SPECIAL_VALS),
    special_receipt.typed
  );

  SET_VECTOR_ELT(
    roots, ROOT_GROUPING, snapshot_constructor_vector(grouping)
  );
  SET_VECTOR_ELT(
    roots, ROOT_CARGO, snapshot_constructor_cargo(cargo)
  );
  SET_VECTOR_ELT(
    roots, ROOT_LOWER, snapshot_constructor_vector(lower)
  );
  SET_VECTOR_ELT(
    roots, ROOT_UPPER, snapshot_constructor_vector(upper)
  );
  SET_VECTOR_ELT(
    roots, ROOT_TOLERANCE, snapshot_constructor_vector(tolerance)
  );
  SET_VECTOR_ELT(
    roots, ROOT_LEVELS, snapshot_constructor_vector(levels)
  );
  SET_VECTOR_ELT(
    roots, ROOT_TAGS, snapshot_constructor_vector(tags)
  );
  SET_VECTOR_ELT(
    roots, ROOT_INIT_GIVEN, snapshot_constructor_vector(init_given)
  );
  SET_VECTOR_ELT(
    roots, ROOT_NUMERIC_SOURCE_KIND,
    snapshot_constructor_vector(numeric_source_kind)
  );
  SET_VECTOR_ELT(
    roots, ROOT_NUMERIC_LOGSCALE,
    snapshot_constructor_vector(numeric_logscale)
  );
  SET_VECTOR_ELT(roots, ROOT_ID, snapshot_constructor_vector(id));
  R_xlen_t requirement_work = 0;
  SEXP stable_requirements = paradox_snapshot_builtin_requirements(
    requirements,
    &requirement_work
  );
  if (stable_requirements == R_UnboundValue) {
    UNPROTECT(1);
    Rf_error("Invalid built-in Domain requirements");
  }
  SET_VECTOR_ELT(roots, ROOT_REQUIREMENTS, stable_requirements);
  SET_VECTOR_ELT(roots, ROOT_DEFAULT, default_value);
  SET_VECTOR_ELT(roots, ROOT_INIT_VALUE, init_value);
  SET_VECTOR_ELT(roots, ROOT_TRAFO, trafo);

  cls = VECTOR_ELT(roots, ROOT_CLS);
  grouping = VECTOR_ELT(roots, ROOT_GROUPING);
  cargo = VECTOR_ELT(roots, ROOT_CARGO);
  lower = VECTOR_ELT(roots, ROOT_LOWER);
  upper = VECTOR_ELT(roots, ROOT_UPPER);
  tolerance = VECTOR_ELT(roots, ROOT_TOLERANCE);
  levels = VECTOR_ELT(roots, ROOT_LEVELS);
  special_vals = VECTOR_ELT(roots, ROOT_SPECIAL_VALS);
  tags = VECTOR_ELT(roots, ROOT_TAGS);
  storage_type = VECTOR_ELT(roots, ROOT_STORAGE);
  init_given = VECTOR_ELT(roots, ROOT_INIT_GIVEN);
  numeric_source_kind = VECTOR_ELT(roots, ROOT_NUMERIC_SOURCE_KIND);
  numeric_logscale = VECTOR_ELT(roots, ROOT_NUMERIC_LOGSCALE);
  id = VECTOR_ELT(roots, ROOT_ID);
  requirements = VECTOR_ELT(roots, ROOT_REQUIREMENTS);
  default_value = VECTOR_ELT(roots, ROOT_DEFAULT);
  init_value = VECTOR_ELT(roots, ROOT_INIT_VALUE);
  trafo = VECTOR_ELT(roots, ROOT_TRAFO);
  discard_representation_names(grouping);
  discard_representation_names(lower);
  discard_representation_names(upper);
  discard_representation_names(tolerance);
  discard_representation_names(tags);
  discard_representation_names(init_given);
  discard_representation_names(numeric_source_kind);
  discard_representation_names(numeric_logscale);
  discard_representation_names(id);
  snapshot_cargo_containers(cargo);
  if (levels != R_NilValue) {
    Rf_setAttrib(levels, R_NamesSymbol, R_NilValue);
  }

  int source_code = NUMERIC_SOURCE_NONE;
  if (!scalar_integer_code(
      numeric_source_kind,
      NUMERIC_SOURCE_NONE,
      NUMERIC_SOURCE_INT,
      &source_code
    ) || !scalar_logical(numeric_logscale)) {
    UNPROTECT(1);
    Rf_error(
      "Invalid built-in Domain numeric admission flags; expected one "
      "integer source kind and one non-missing logical logscale flag"
    );
  }
  const numeric_source_kind_t source_kind =
    (numeric_source_kind_t) source_code;
  const int logscale = LOGICAL_ELT(numeric_logscale, 0);
  domain_kind_t kind = domain_kind(cls, storage_type);
  const int source_matches_kind =
    (source_kind == NUMERIC_SOURCE_NONE &&
      kind != DOMAIN_KIND_DBL && kind != DOMAIN_KIND_INT) ||
    (source_kind == NUMERIC_SOURCE_DBL && kind == DOMAIN_KIND_DBL) ||
    (source_kind == NUMERIC_SOURCE_INT && kind == DOMAIN_KIND_INT);
  const int numeric_source = source_kind != NUMERIC_SOURCE_NONE;
  /* One argument-named diagnostic per canonical-state clause.  The clauses are
   * tested in exactly the order of the single combined condition they replace,
   * so an input that violates several of them still reports the one it always
   * reported -- now naming the argument that is actually wrong instead of one
   * sentence that covered every field at once. */
  if (kind == DOMAIN_KIND_UNKNOWN) {
    UNPROTECT(1);
    Rf_error(
      "`cls` and `storage_type` must describe one canonical Domain; "
      "Paradox 2 supports only p_dbl, p_int, p_fct, p_lgl, and p_uty"
    );
  }
  if (!source_matches_kind ||
      (source_kind == NUMERIC_SOURCE_NONE && logscale)) {
    UNPROTECT(1);
    Rf_error(
      "Internal error: `.numeric_source_kind` and `.numeric_logscale` do not "
      "match `cls`/`storage_type`"
    );
  }
  if (!scalar_string(grouping)) {
    UNPROTECT(1);
    Rf_error("`grouping` must be one non-missing string");
  }
  if (!numeric_source && (!scalar_numeric(lower) ||
      !scalar_numeric(upper) || !scalar_numeric(tolerance))) {
    UNPROTECT(1);
    Rf_error("`lower`, `upper`, and `tolerance` must each be one number");
  }
  if (!levels_are_canonical(kind, levels)) {
    UNPROTECT(1);
    if (kind == DOMAIN_KIND_FCT) {
      Rf_error(
        "`levels` must be a character vector of unique, non-missing values"
      );
    }
    if (kind == DOMAIN_KIND_LGL) {
      Rf_error("`levels` must be `c(TRUE, FALSE)`");
    }
    Rf_error("`levels` must be NULL for a p_dbl, p_int, or p_uty Domain");
  }
  if (!checkmate_list(special_vals)) {
    UNPROTECT(1);
    Rf_error("`special_vals` must be an ordinary list");
  }
  if (!unique_nonmissing_strings(tags, FALSE) ||
      !paradox_api_has_no_attributes(tags)) {
    UNPROTECT(1);
    Rf_error(
      "`tags` must be an attribute-free character vector of unique, "
      "non-missing values"
    );
  }
  if (!function_or_null(trafo)) {
    UNPROTECT(1);
    Rf_error("`trafo` must be a function or NULL");
  }
  if (TYPEOF(init_given) != LGLSXP || ALTREP(init_given) ||
      Rf_isObject(init_given) ||
      !paradox_api_has_no_attributes(init_given) ||
      XLENGTH(init_given) != 1 ||
      LOGICAL_ELT(init_given, 0) == NA_LOGICAL) {
    UNPROTECT(1);
    Rf_error("Internal error: invalid `init` admission flag");
  }
  if (named_element(cargo, "logscale") != R_NilValue) {
    UNPROTECT(1);
    Rf_error("`logscale` must be given as an argument, not through `cargo`");
  }
  {
    paradox_domain_field_t cargo_failure = PARADOX_DOMAIN_FIELD_CARGO;
    if (!cargo_is_canonical(cargo, tags, kind, &cargo_failure)) {
      UNPROTECT(1);
      cargo_argument_error(cargo_failure);
    }
  }

  if (source_kind != NUMERIC_SOURCE_NONE) {
    const int integer = source_kind == NUMERIC_SOURCE_INT;
    const char *expected_grouping = integer ? "ParamInt" : "ParamDbl";
    if (!paradox_domain_string_is(STRING_ELT(grouping, 0), expected_grouping)) {
      UNPROTECT(1);
      Rf_error("Invalid numeric Domain grouping");
    }

    double tolerance_value;
    /* A canonical numeric Domain stores a finite tolerance, so admit only
     * that here: an infinite `tolerance` would otherwise pass this named
     * argument gate and be rejected by the final canonical-state check, which
     * can only name the whole `lower/upper/tolerance` field group. */
    if (!plain_scalar_number_value(tolerance, &tolerance_value) ||
        tolerance_value < 0.0 || !R_FINITE(tolerance_value) ||
        (integer && tolerance_value > 0.5)) {
      UNPROTECT(1);
      Rf_error(
        integer
          ? "`tolerance` must be one number between 0 and 0.5"
          : "`tolerance` must be one finite non-negative number"
      );
    }

    double lower_value;
    if (!plain_scalar_number_value(lower, &lower_value) ||
        (integer && !plain_integer_bound(lower_value))) {
      UNPROTECT(1);
      Rf_error(
        integer
          ? "`lower` must be one integer-valued number or infinity"
          : "`lower` must be one number"
      );
    }

    double upper_value;
    if (!plain_scalar_number_value(upper, &upper_value) ||
        (integer && !plain_integer_bound(upper_value))) {
      UNPROTECT(1);
      Rf_error(
        integer
          ? "`upper` must be one integer-valued number or infinity"
          : "`upper` must be one number"
      );
    }
    if (lower_value > upper_value) {
      UNPROTECT(1);
      Rf_error("`lower` must not be greater than `upper`");
    }
    if (logscale && trafo != R_NilValue) {
      UNPROTECT(1);
      Rf_error("When a trafo is given then logscale must be FALSE");
    }
    if (logscale && ((integer && lower_value < 0.0) ||
        (!integer && lower_value <= 0.0))) {
      UNPROTECT(1);
      Rf_error(
        integer
          ? "When logscale is TRUE then lower bound must be greater or equal 0"
          : "When logscale is TRUE then lower bound must be strictly greater than 0"
      );
    }

    if (logscale) {
      SEXP generated_cargo = PROTECT(cargo_with_logscale(cargo));
      SET_VECTOR_ELT(roots, ROOT_CARGO, generated_cargo);
      UNPROTECT(1);
      cargo = VECTOR_ELT(roots, ROOT_CARGO);

      SEXP generated_trafo = PROTECT(integer
        ? integer_logscale_trafo(lower, upper)
        : double_logscale_trafo());
      SET_VECTOR_ELT(roots, ROOT_TRAFO, generated_trafo);
      UNPROTECT(1);
      trafo = VECTOR_ELT(roots, ROOT_TRAFO);

      const double stored_lower = integer
        ? log(fmax(lower_value, 0.5))
        : log(lower_value);
      const double stored_upper = integer
        ? log(upper_value + 1.0)
        : log(upper_value);
      SEXP transformed_lower = PROTECT(Rf_ScalarReal(stored_lower));
      SET_VECTOR_ELT(roots, ROOT_LOWER, transformed_lower);
      UNPROTECT(1);
      SEXP transformed_upper = PROTECT(Rf_ScalarReal(stored_upper));
      SET_VECTOR_ELT(roots, ROOT_UPPER, transformed_upper);
      UNPROTECT(1);
      lower = VECTOR_ELT(roots, ROOT_LOWER);
      upper = VECTOR_ELT(roots, ROOT_UPPER);

      if (integer) {
        SEXP transformed_class = PROTECT(Rf_mkString("ParamDbl"));
        SET_VECTOR_ELT(roots, ROOT_CLS, transformed_class);
        UNPROTECT(1);
        SEXP transformed_grouping = PROTECT(Rf_mkString("ParamDbl"));
        SET_VECTOR_ELT(roots, ROOT_GROUPING, transformed_grouping);
        UNPROTECT(1);
        SEXP transformed_storage = PROTECT(Rf_mkString("numeric"));
        SET_VECTOR_ELT(roots, ROOT_STORAGE, transformed_storage);
        UNPROTECT(1);
        cls = VECTOR_ELT(roots, ROOT_CLS);
        grouping = VECTOR_ELT(roots, ROOT_GROUPING);
        storage_type = VECTOR_ELT(roots, ROOT_STORAGE);
      }
      kind = domain_kind(cls, storage_type);
      if (kind != DOMAIN_KIND_DBL ||
          !cargo_is_canonical(cargo, tags, kind, NULL)) {
        UNPROTECT(1);
        Rf_error("Internal error while constructing logscale Domain state");
      }
    }
  }
  if (XLENGTH(special_vals) != 0 && trafo != R_NilValue) {
    UNPROTECT(1);
    Rf_error("`trafo` and `special_vals` cannot both be supplied");
  }

  if (kind != DOMAIN_KIND_UTY) {
    SEXP stable_default = PROTECT(
      paradox_snapshot_builtin_value_leaf(default_value)
    );
    SET_VECTOR_ELT(roots, ROOT_DEFAULT, stable_default);
    UNPROTECT(1);
    SEXP stable_init = PROTECT(
      paradox_snapshot_builtin_value_leaf(init_value)
    );
    SET_VECTOR_ELT(roots, ROOT_INIT_VALUE, stable_init);
    UNPROTECT(1);
    default_value = VECTOR_ELT(roots, ROOT_DEFAULT);
    init_value = VECTOR_ELT(roots, ROOT_INIT_VALUE);
  }

  paradox_builtin_domain_kind_t admitted_kind;
  paradox_domain_field_t failed_field;
  paradox_builtin_value_result_t value_failure;
  if (!paradox_admit_builtin_domain_row(
      id,
      cls,
      grouping,
      cargo,
      lower,
      upper,
      tolerance,
      levels,
      special_vals,
      default_value,
      storage_type,
      tags,
      trafo,
      requirements,
      init_given,
      init_value,
      &special_receipt,
      &admitted_kind,
      &failed_field,
      &value_failure,
      &admission_work
    )) {
    if (failed_field == PARADOX_DOMAIN_FIELD_REQUIRED_DEFAULT) {
      UNPROTECT(1);
      Rf_error("A 'required' parameter can not have a 'default'.");
    }
    if (failed_field == PARADOX_DOMAIN_FIELD_INIT_TRAFO) {
      UNPROTECT(1);
      Rf_error("Initial value and trafo can not both be given at the same time.");
    }
    if ((failed_field == PARADOX_DOMAIN_FIELD_DEFAULT_VALUE ||
        failed_field == PARADOX_DOMAIN_FIELD_INIT_VALUE) &&
        scalar_string(id) &&
        value_failure.failure != PARADOX_BUILTIN_VALUE_OK) {
      const paradox_builtin_value_spec_t value_spec = admitted_value_spec(
        kind,
        lower,
        upper,
        tolerance,
        levels,
        special_vals
      );
      SEXP failed_value = failed_field == PARADOX_DOMAIN_FIELD_DEFAULT_VALUE
        ? default_value
        : init_value;
      SEXP diagnostic = PROTECT(paradox_builtin_value_diagnostic(
        STRING_ELT(id, 0),
        &value_spec,
        failed_value,
        &value_failure
      ));
      paradox_assertion_error("param", diagnostic);
    }
    UNPROTECT(1);
    Rf_error(
      "Invalid built-in Domain final state in field `%s`",
      paradox_domain_field_name(failed_field)
    );
  }

  SEXP result = PROTECT(build_domain_shell(
    id,
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
    init_value,
    requirements,
    &admission_work
  ));
  UNPROTECT(2);
  return result;
}

static int plain_scalar_number_value(SEXP value, double *result) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  if ((type != INTSXP && type != REALSXP) || ALTREP(value) ||
      Rf_isS4(value) || Rf_isObject(value) ||
      !paradox_api_has_no_attributes(value) || XLENGTH(value) != 1) {
    return FALSE;
  }
  if (type == INTSXP) {
    const int input = INTEGER_ELT(value, 0);
    if (input == NA_INTEGER) {
      return FALSE;
    }
    *result = (double) input;
    return TRUE;
  }
  if (type == REALSXP) {
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

SEXP paradox_domain_uty_validate_custom_check(SEXP callback) {
  if (!Rf_isFunction(callback)) {
    Rf_error("`custom_check` must be a function");
  }
  /*
   * Keep the exact callback as the registered `.Call` argument while it is
   * probed. In particular, callback code assigning `custom_check` in
   * parent.frame() must not replace the function that was just validated
   * before p_uty() stores it. The fresh evaluation frame preserves ordinary
   * callback code which writes harmless temporary bindings in parent.frame(),
   * while avoiding exposure of the p_uty() construction frame. Constructing
   * it through base `new.env()` is portable to R 3.6 without private API.
   */
  SEXP frame_call = PROTECT(Rf_lang1(Rf_install("new.env")));
  SEXP frame = PROTECT(Rf_eval(frame_call, R_BaseEnv));
  if (TYPEOF(frame) != ENVSXP) {
    UNPROTECT(2);
    Rf_error("Internal error: could not create custom-check probe frame");
  }
  SEXP probe = PROTECT(Rf_ScalarReal(1.0));
  SEXP call = PROTECT(paradox_unary_callback_call(callback, probe));
  SEXP result = PROTECT(Rf_eval(call, frame));
  SEXP valid = PROTECT(paradox_domain_uty_check_result(result));
  if (LOGICAL_ELT(valid, 0) != TRUE) {
    UNPROTECT(6);
    Rf_error(
      "The result of `custom_check(1)` must be TRUE or one non-missing string"
    );
  }
  UNPROTECT(6);
  return callback;
}

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
  SEXP option = PROTECT(paradox_api_option_snapshot(symbol));
  int result = FALSE;
  if (!ALTREP(option) && !Rf_isS4(option) && !Rf_isObject(option) &&
      paradox_api_has_no_attributes(option) && XLENGTH(option) == 1) {
    if (TYPEOF(option) == INTSXP &&
        INTEGER_ELT(option, 0) != NA_INTEGER) {
      *value = INTEGER_ELT(option, 0);
      result = TRUE;
    } else if (TYPEOF(option) == REALSXP) {
      /*
       * R 3.6's public getOption() preserves a numeric scipen assignment as a
       * REALSXP, whereas newer R normalizes the same option to integer. Both
       * represent the documented integer-valued option. Keep malformed,
       * non-integral, and out-of-range option states on the deparse fallback.
       */
      const double real = REAL_ELT(option, 0);
      if (R_FINITE(real) && real >= (double) INT_MIN &&
          real <= (double) INT_MAX && real == (double) ((int) real)) {
        *value = (int) real;
        result = TRUE;
      }
    }
  }
  UNPROTECT(1);
  return result;
}

static int domain_repr_scipen_is_zero(SEXP symbol) {
  int value;
  return domain_repr_scipen_value(symbol, &value) && value == 0;
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
        parsed != real ||
        (parsed == 0.0 && signbit(parsed) != signbit(real))) {
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

SEXP paradox_domain_simple_repr_id(SEXP representation) {
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
}
