#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Random.h>

#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

/* This is deliberately a narrow whole-operation kernel.  It is entered only
 * by the exact SamplerUnif private method and returns R_NilValue before
 * touching the RNG whenever an object can still have observable R behavior. */

#if R_VERSION >= R_Version(4, 6, 0)

enum sampler_kind {
  SAMPLER_KIND_UNKNOWN = 0,
  SAMPLER_KIND_DBL,
  SAMPLER_KIND_INT,
  SAMPLER_KIND_FCT,
  SAMPLER_KIND_LGL
};

enum {
  SAMPLER_PARAM_COLUMN_COUNT = 11,
  SAMPLER_PS_METHOD_COUNT = 9,
  SAMPLER_IMPORT_COUNT = 4
};

typedef struct {
  SEXP values;
  R_xlen_t used;
} sampler_roots_t;

typedef struct {
  const char *name;
  SEXP owner;
  R_BindingType_t type;
  SEXP expression;
  SEXP evaluation_environment;
  SEXP value;
} binding_snapshot_t;

typedef struct {
  const char *binding_name;
  const char *target_name;
  int active;
  int has_n;
  SEXP self;
  SEXP private_environment;
  SEXP owner;
  SEXP closure;
  SEXP environment;
  binding_snapshot_t target;
} wrapper_snapshot_t;

typedef struct {
  binding_snapshot_t imported;
  binding_snapshot_t provider;
} import_snapshot_t;

typedef struct {
  SEXP object;
  SEXP classes;
  SEXP enclosure;
  SEXP private_environment;
  SEXP methods[SAMPLER_PS_METHOD_COUNT];
} param_set_snapshot_t;

typedef struct {
  SEXP table;
  SEXP names;
  SEXP classes;
  SEXP columns[SAMPLER_PARAM_COLUMN_COUNT];
} params_snapshot_t;

typedef struct {
  enum sampler_kind kind;
  SEXP id;
  double lower;
  double upper;
  double tolerance;
  SEXP source_levels;
  SEXP stable_levels;
} sampler_spec_t;

typedef struct {
  SEXP child;
  SEXP classes;
  SEXP enclosure;
  SEXP private_environment;
  SEXP param_set;
  SEXP values;
  SEXP dependencies;
  wrapper_snapshot_t public_sample;
  wrapper_snapshot_t private_sample;
  wrapper_snapshot_t active_param;
  param_set_snapshot_t param_set_surface;
  params_snapshot_t params;
  sampler_spec_t spec;
} child_snapshot_t;

typedef struct {
  SEXP self;
  SEXP classes;
  SEXP enclosure;
  SEXP private_environment;
  SEXP param_set;
  SEXP samplers;
  SEXP sampler_names;
  SEXP root_values;
  SEXP namespace_environment;
  SEXP imports_environment;
  import_snapshot_t imports[SAMPLER_IMPORT_COUNT];
  wrapper_snapshot_t public_sample;
  wrapper_snapshot_t private_sample;
  param_set_snapshot_t param_set_surface;
  params_snapshot_t params;
  child_snapshot_t *children;
  R_xlen_t size;
} sampler_graph_t;

static const char *const params_column_names[SAMPLER_PARAM_COLUMN_COUNT] = {
  "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
  "levels", "special_vals", "default", "storage_type"
};

static const SEXPTYPE params_column_types[SAMPLER_PARAM_COLUMN_COUNT] = {
  STRSXP, STRSXP, STRSXP, VECSXP, REALSXP, REALSXP, REALSXP, VECSXP,
  VECSXP, VECSXP, STRSXP
};

static void retain(sampler_roots_t *roots, SEXP value) {
  if (roots == NULL) {
    return;
  }
  if (roots->used >= XLENGTH(roots->values)) {
    Rf_error("Internal error: SamplerUnif root plan is too small");
  }
  SET_VECTOR_ELT(roots->values, roots->used, value);
  ++roots->used;
}

static void retain_if_present(sampler_roots_t *roots, SEXP value) {
  if (value != R_NilValue && value != R_UnboundValue) {
    retain(roots, value);
  }
}

static int capture_locked_binding(SEXP owner, const char *name,
    sampler_roots_t *roots, binding_snapshot_t *snapshot) {
  SEXP symbol = Rf_install(name);
  if (TYPEOF(owner) != ENVSXP ||
      !R_existsVarInFrame(owner, symbol) ||
      R_BindingIsActive(symbol, owner) ||
      !R_BindingIsLocked(symbol, owner)) {
    return FALSE;
  }
  const R_BindingType_t type = R_GetBindingType(symbol, owner);
  SEXP expression = R_NilValue;
  SEXP evaluation_environment = R_NilValue;
  SEXP value = R_UnboundValue;
  switch (type) {
  case R_BindingTypeValue:
  case R_BindingTypeForced:
    value = paradox_api_stable_local_value(owner, symbol);
    if (value == R_UnboundValue) {
      return FALSE;
    }
    if (type == R_BindingTypeForced) {
      expression = R_ForcedBindingExpression(symbol, owner);
    }
    break;
  case R_BindingTypeDelayed:
    expression = R_DelayedBindingExpression(symbol, owner);
    evaluation_environment = R_DelayedBindingEnvironment(symbol, owner);
    if (expression == R_UnboundValue ||
        TYPEOF(evaluation_environment) != ENVSXP) {
      return FALSE;
    }
    break;
  case R_BindingTypeUnbound:
  case R_BindingTypeMissing:
  case R_BindingTypeActive:
    return FALSE;
  }
  snapshot->name = name;
  snapshot->owner = owner;
  snapshot->type = type;
  snapshot->expression = expression;
  snapshot->evaluation_environment = evaluation_environment;
  snapshot->value = value;
  retain(roots, owner);
  retain_if_present(roots, expression);
  retain_if_present(roots, evaluation_environment);
  retain_if_present(roots, value);
  return TRUE;
}

static int audit_locked_binding(const binding_snapshot_t *snapshot) {
  SEXP symbol = Rf_install(snapshot->name);
  if (!R_existsVarInFrame(snapshot->owner, symbol) ||
      R_BindingIsActive(symbol, snapshot->owner) ||
      !R_BindingIsLocked(symbol, snapshot->owner) ||
      R_GetBindingType(symbol, snapshot->owner) != snapshot->type) {
    return FALSE;
  }
  switch (snapshot->type) {
  case R_BindingTypeValue:
    return paradox_api_stable_local_value(snapshot->owner, symbol) ==
      snapshot->value;
  case R_BindingTypeForced:
    return paradox_api_stable_local_value(snapshot->owner, symbol) ==
        snapshot->value &&
      R_ForcedBindingExpression(symbol, snapshot->owner) ==
        snapshot->expression;
  case R_BindingTypeDelayed:
    return R_DelayedBindingExpression(symbol, snapshot->owner) ==
        snapshot->expression &&
      R_DelayedBindingEnvironment(symbol, snapshot->owner) ==
        snapshot->evaluation_environment;
  case R_BindingTypeUnbound:
  case R_BindingTypeMissing:
  case R_BindingTypeActive:
    return FALSE;
  }
  return FALSE;
}

static int bindings_have_same_value(
    const binding_snapshot_t *left, const binding_snapshot_t *right) {
  if (left->value != R_UnboundValue || right->value != R_UnboundValue) {
    return left->value != R_UnboundValue &&
      right->value != R_UnboundValue && left->value == right->value;
  }
  return FALSE;
}

static int exact_classes(SEXP object, const char *const *expected,
    R_xlen_t count, SEXP *classes) {
  if (TYPEOF(object) != ENVSXP) {
    return FALSE;
  }
  SEXP value = Rf_getAttrib(object, R_ClassSymbol);
  R_xlen_t work = 0;
  if (!paradox_api_has_no_attributes(value) ||
      !paradox_domain_exact_string_vector(value, expected, count, &work)) {
    return FALSE;
  }
  *classes = value;
  return TRUE;
}

static int exact_formals(SEXP formals, int has_n) {
  if (!has_n) {
    return formals == R_NilValue;
  }
  return TYPEOF(formals) == LISTSXP &&
    TAG(formals) == Rf_install("n") && CAR(formals) == R_MissingArg &&
    CDR(formals) == R_NilValue;
}

static int exact_forwarding_body(SEXP body, const char *target_name,
    int has_n) {
  if (TYPEOF(body) != LANGSXP || CAR(body) != Rf_install(target_name)) {
    return FALSE;
  }
  static const char *const fixed[] = {"self", "private", "super"};
  SEXP argument = CDR(body);
  for (size_t index = 0; index < 3; ++index) {
    SEXP symbol = Rf_install(fixed[index]);
    if (TYPEOF(argument) != LISTSXP || TAG(argument) != symbol ||
        CAR(argument) != symbol) {
      return FALSE;
    }
    argument = CDR(argument);
  }
  if (has_n) {
    SEXP symbol = Rf_install("n");
    if (TYPEOF(argument) != LISTSXP || TAG(argument) != symbol ||
        CAR(argument) != symbol) {
      return FALSE;
    }
    argument = CDR(argument);
  }
  return argument == R_NilValue;
}

static int capture_wrapper(SEXP owner, SEXP self, SEXP private_environment,
    SEXP expected_environment, const char *binding_name,
    const char *target_name, int active, int has_n,
    SEXP namespace_environment, sampler_roots_t *roots,
    wrapper_snapshot_t *snapshot) {
  SEXP binding = Rf_install(binding_name);
  if (TYPEOF(owner) != ENVSXP ||
      !R_existsVarInFrame(owner, binding) ||
      R_BindingIsActive(binding, owner) != active ||
      (!active && !R_BindingIsLocked(binding, owner))) {
    return FALSE;
  }

  SEXP closure = PROTECT(active
    ? R_ActiveBindingFunction(binding, owner)
    : paradox_api_local_value(owner, binding));
  if (TYPEOF(closure) != CLOSXP) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP formals = PROTECT(paradox_api_closure_formals(closure));
  SEXP body = PROTECT(paradox_api_closure_expression(closure));
  SEXP environment = PROTECT(paradox_api_closure_environment(closure));
  const int formals_ok = exact_formals(formals, has_n);
  const int body_ok = exact_forwarding_body(body, target_name, has_n);
  const int environment_ok = TYPEOF(environment) == ENVSXP;
  const int expected_environment_ok = expected_environment == R_NilValue ||
    environment == expected_environment;
  const int parent_ok = paradox_api_parent_environment(environment) ==
    namespace_environment;
  const int self_ok = paradox_api_local_value(
    environment,
    Rf_install("self")
  ) == self;
  const int private_ok = paradox_api_local_value(
    environment,
    Rf_install("private")
  ) == private_environment;
  const int target_ok = capture_locked_binding(
    namespace_environment,
    target_name,
    roots,
    &snapshot->target
  ) && snapshot->target.value != R_UnboundValue &&
    TYPEOF(snapshot->target.value) == CLOSXP;
  const int valid = formals_ok && body_ok && environment_ok &&
    expected_environment_ok && parent_ok && self_ok && private_ok &&
    target_ok;
  if (valid) {
    snapshot->binding_name = binding_name;
    snapshot->target_name = target_name;
    snapshot->active = active;
    snapshot->has_n = has_n;
    snapshot->self = self;
    snapshot->private_environment = private_environment;
    snapshot->owner = owner;
    snapshot->closure = closure;
    snapshot->environment = environment;
    retain(roots, owner);
    retain(roots, closure);
    retain(roots, environment);
  }
  UNPROTECT(4);
  return valid;
}

static int audit_wrapper(const wrapper_snapshot_t *snapshot,
    SEXP namespace_environment) {
  SEXP binding = Rf_install(snapshot->binding_name);
  if (!R_existsVarInFrame(snapshot->owner, binding) ||
      R_BindingIsActive(binding, snapshot->owner) != snapshot->active ||
      (!snapshot->active &&
        !R_BindingIsLocked(binding, snapshot->owner))) {
    return FALSE;
  }
  SEXP closure = snapshot->active
    ? R_ActiveBindingFunction(binding, snapshot->owner)
    : paradox_api_local_value(snapshot->owner, binding);
  if (closure != snapshot->closure || TYPEOF(closure) != CLOSXP ||
      paradox_api_closure_environment(closure) != snapshot->environment ||
      paradox_api_parent_environment(snapshot->environment) !=
        namespace_environment ||
      paradox_api_local_value(snapshot->environment, Rf_install("self")) !=
        snapshot->self ||
      paradox_api_local_value(
        snapshot->environment,
        Rf_install("private")
      ) != snapshot->private_environment ||
      !exact_formals(
        paradox_api_closure_formals(closure),
        snapshot->has_n
      ) || !exact_forwarding_body(
        paradox_api_closure_expression(closure),
        snapshot->target_name,
        snapshot->has_n
      )) {
    return FALSE;
  }
  return audit_locked_binding(&snapshot->target);
}

static int capture_param_set_surface(SEXP param_set,
    sampler_roots_t *roots, param_set_snapshot_t *snapshot) {
  static const char *const classes[] = {"ParamSet", "R6"};
  if (!paradox_param_set_random_design_auth(param_set) ||
      !exact_classes(param_set, classes, 2, &snapshot->classes)) {
    return FALSE;
  }
  SEXP enclosure = PROTECT(paradox_domain_local_value(
    param_set,
    ".__enclos_env__"
  ));
  SEXP private_environment = PROTECT(paradox_domain_local_value(
    enclosure,
    "private"
  ));
  if (TYPEOF(enclosure) != ENVSXP || TYPEOF(private_environment) != ENVSXP ||
      !R_EnvironmentIsLocked(param_set) ||
      !R_EnvironmentIsLocked(private_environment)) {
    UNPROTECT(2);
    return FALSE;
  }

  static const char *const methods[] = {
    "clone", "ids", "qunif", "subspaces"
  };
  static const char *const active[] = {
    "length", "class", "is_bounded", "has_deps", "values"
  };
  snapshot->object = param_set;
  snapshot->enclosure = enclosure;
  snapshot->private_environment = private_environment;
  retain(roots, param_set);
  retain(roots, snapshot->classes);
  retain(roots, enclosure);
  retain(roots, private_environment);
  for (size_t index = 0; index < 4; ++index) {
    SEXP symbol = Rf_install(methods[index]);
    SEXP method = paradox_api_local_value(param_set, symbol);
    if (TYPEOF(method) != CLOSXP || R_BindingIsActive(symbol, param_set) ||
        !R_BindingIsLocked(symbol, param_set)) {
      UNPROTECT(2);
      return FALSE;
    }
    snapshot->methods[index] = method;
    retain(roots, method);
  }
  for (size_t index = 0; index < 5; ++index) {
    SEXP symbol = Rf_install(active[index]);
    if (!R_BindingIsActive(symbol, param_set)) {
      UNPROTECT(2);
      return FALSE;
    }
    SEXP method = R_ActiveBindingFunction(symbol, param_set);
    if (TYPEOF(method) != CLOSXP) {
      UNPROTECT(2);
      return FALSE;
    }
    snapshot->methods[index + 4] = method;
    retain(roots, method);
  }
  UNPROTECT(2);
  return TRUE;
}

static int audit_param_set_surface(const param_set_snapshot_t *snapshot) {
  static const char *const classes[] = {"ParamSet", "R6"};
  SEXP observed_classes;
  if (!exact_classes(snapshot->object, classes, 2, &observed_classes) ||
      observed_classes != snapshot->classes ||
      paradox_domain_local_value(snapshot->object, ".__enclos_env__") !=
        snapshot->enclosure ||
      paradox_domain_local_value(snapshot->enclosure, "private") !=
        snapshot->private_environment) {
    return FALSE;
  }
  static const char *const methods[] = {
    "clone", "ids", "qunif", "subspaces"
  };
  static const char *const active[] = {
    "length", "class", "is_bounded", "has_deps", "values"
  };
  for (size_t index = 0; index < 4; ++index) {
    SEXP symbol = Rf_install(methods[index]);
    if (R_BindingIsActive(symbol, snapshot->object) ||
        !R_BindingIsLocked(symbol, snapshot->object) ||
        paradox_api_local_value(snapshot->object, symbol) !=
          snapshot->methods[index]) {
      return FALSE;
    }
  }
  for (size_t index = 0; index < 5; ++index) {
    SEXP symbol = Rf_install(active[index]);
    if (!R_BindingIsActive(symbol, snapshot->object) ||
        R_ActiveBindingFunction(symbol, snapshot->object) !=
          snapshot->methods[index + 4]) {
      return FALSE;
    }
  }
  return TRUE;
}

static int capture_params(SEXP params, sampler_roots_t *roots,
    params_snapshot_t *snapshot) {
  paradox_domain_params_t validated;
  R_xlen_t selected = 0;
  R_xlen_t work = 0;
  if (!paradox_domain_validate_params(
      params,
      R_NilValue,
      TRUE,
      &validated,
      &selected,
      &work
    )) {
    return FALSE;
  }
  snapshot->table = params;
  snapshot->names = Rf_getAttrib(params, R_NamesSymbol);
  snapshot->classes = Rf_getAttrib(params, R_ClassSymbol);
  retain(roots, params);
  retain(roots, snapshot->names);
  retain(roots, snapshot->classes);
  for (R_xlen_t column = 0; column < SAMPLER_PARAM_COLUMN_COUNT; ++column) {
    snapshot->columns[column] = VECTOR_ELT(params, column);
    retain(roots, snapshot->columns[column]);
  }
  return TRUE;
}

static int audit_params(const params_snapshot_t *snapshot,
    R_xlen_t expected_rows) {
  static const char *const table_classes[] = {"data.table", "data.frame"};
  R_xlen_t work = 0;
  if (TYPEOF(snapshot->table) != VECSXP || ALTREP(snapshot->table) ||
      XLENGTH(snapshot->table) != SAMPLER_PARAM_COLUMN_COUNT ||
      Rf_getAttrib(snapshot->table, R_NamesSymbol) != snapshot->names ||
      Rf_getAttrib(snapshot->table, R_ClassSymbol) != snapshot->classes ||
      !paradox_domain_exact_string_vector(
        snapshot->names,
        params_column_names,
        SAMPLER_PARAM_COLUMN_COUNT,
        &work
      ) || !paradox_domain_exact_string_vector(
        snapshot->classes,
        table_classes,
        2,
        &work
      )) {
    return FALSE;
  }
  for (R_xlen_t column = 0; column < SAMPLER_PARAM_COLUMN_COUNT; ++column) {
    SEXP value = VECTOR_ELT(snapshot->table, column);
    const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
    const int numeric = column == PARADOX_DOMAIN_LOWER ||
      column == PARADOX_DOMAIN_UPPER ||
      column == PARADOX_DOMAIN_TOLERANCE;
    if (value != snapshot->columns[column] || ALTREP(value) ||
        XLENGTH(value) != expected_rows ||
        (numeric
          ? type != REALSXP && type != INTSXP
          : type != params_column_types[column])) {
      return FALSE;
    }
  }
  SEXP ids = snapshot->columns[PARADOX_DOMAIN_ID];
  SEXP classes = snapshot->columns[PARADOX_DOMAIN_CLS];
  SEXP grouping = snapshot->columns[PARADOX_DOMAIN_GROUPING];
  SEXP storage_types = snapshot->columns[PARADOX_DOMAIN_STORAGE_TYPE];
  for (R_xlen_t row = 0; row < expected_rows; ++row) {
    if (STRING_ELT(ids, row) == NA_STRING ||
        STRING_ELT(classes, row) == NA_STRING ||
        STRING_ELT(grouping, row) == NA_STRING ||
        STRING_ELT(storage_types, row) == NA_STRING) {
      return FALSE;
    }
  }
  return TRUE;
}

static double numeric_at(SEXP value, R_xlen_t index) {
  if (TYPEOF(value) == REALSXP) {
    return REAL_ELT(value, index);
  }
  const int observed = INTEGER_ELT(value, index);
  return observed == NA_INTEGER ? NA_REAL : (double) observed;
}

static enum sampler_kind parameter_kind(SEXP class_name,
    SEXP storage_type) {
  if (paradox_domain_string_is(class_name, "ParamDbl") &&
      paradox_domain_string_is(storage_type, "numeric")) {
    return SAMPLER_KIND_DBL;
  }
  if (paradox_domain_string_is(class_name, "ParamInt") &&
      paradox_domain_string_is(storage_type, "integer")) {
    return SAMPLER_KIND_INT;
  }
  if (paradox_domain_string_is(class_name, "ParamFct") &&
      paradox_domain_string_is(storage_type, "character")) {
    return SAMPLER_KIND_FCT;
  }
  if (paradox_domain_string_is(class_name, "ParamLgl") &&
      paradox_domain_string_is(storage_type, "logical")) {
    return SAMPLER_KIND_LGL;
  }
  return SAMPLER_KIND_UNKNOWN;
}

static int capture_spec(const params_snapshot_t *params, R_xlen_t row,
    int copy_levels, sampler_roots_t *roots, sampler_spec_t *spec) {
  SEXP ids = params->columns[PARADOX_DOMAIN_ID];
  SEXP classes = params->columns[PARADOX_DOMAIN_CLS];
  SEXP storage_types = params->columns[PARADOX_DOMAIN_STORAGE_TYPE];
  SEXP levels = params->columns[PARADOX_DOMAIN_LEVELS];
  spec->id = STRING_ELT(ids, row);
  spec->kind = parameter_kind(
    STRING_ELT(classes, row),
    STRING_ELT(storage_types, row)
  );
  spec->lower = NA_REAL;
  spec->upper = NA_REAL;
  spec->tolerance = NA_REAL;
  spec->source_levels = VECTOR_ELT(levels, row);
  spec->stable_levels = spec->source_levels;
  retain(roots, spec->source_levels);
  if (spec->id == NA_STRING || spec->kind == SAMPLER_KIND_UNKNOWN) {
    return FALSE;
  }

  if (spec->kind == SAMPLER_KIND_DBL ||
      spec->kind == SAMPLER_KIND_INT) {
    spec->lower = numeric_at(params->columns[PARADOX_DOMAIN_LOWER], row);
    spec->upper = numeric_at(params->columns[PARADOX_DOMAIN_UPPER], row);
    spec->tolerance = numeric_at(
      params->columns[PARADOX_DOMAIN_TOLERANCE],
      row
    );
    if (!R_FINITE(spec->lower) || !R_FINITE(spec->upper) ||
        !R_FINITE(spec->tolerance) || spec->tolerance < 0.0 ||
        spec->lower > spec->upper) {
      return FALSE;
    }
    if (spec->kind == SAMPLER_KIND_INT &&
        (spec->lower <= (double) INT_MIN ||
         spec->upper > (double) INT_MAX ||
         floor(spec->lower) != spec->lower ||
         floor(spec->upper) != spec->upper)) {
      return FALSE;
    }
    return TRUE;
  }

  if (spec->kind == SAMPLER_KIND_FCT) {
    if (TYPEOF(spec->source_levels) != STRSXP ||
        ALTREP(spec->source_levels) ||
        !paradox_api_has_no_attributes(spec->source_levels) ||
        XLENGTH(spec->source_levels) == 0) {
      return FALSE;
    }
    if (copy_levels) {
      const R_xlen_t count = XLENGTH(spec->source_levels);
      PROTECT(spec->source_levels);
      SEXP stable = PROTECT(Rf_allocVector(STRSXP, count));
      for (R_xlen_t index = 0; index < count; ++index) {
        SEXP value = STRING_ELT(spec->source_levels, index);
        if (value == NA_STRING) {
          UNPROTECT(2);
          return FALSE;
        }
        SET_STRING_ELT(stable, index, value);
      }
      spec->stable_levels = stable;
      retain(roots, stable);
      UNPROTECT(2);
    }
    return TRUE;
  }

  return TYPEOF(spec->source_levels) == LGLSXP &&
    !ALTREP(spec->source_levels) &&
    paradox_api_has_no_attributes(spec->source_levels) &&
    XLENGTH(spec->source_levels) == 2 &&
    LOGICAL_ELT(spec->source_levels, 0) == TRUE &&
    LOGICAL_ELT(spec->source_levels, 1) == FALSE;
}

static int audit_spec(const params_snapshot_t *params, R_xlen_t row,
    const sampler_spec_t *spec) {
  SEXP id = STRING_ELT(params->columns[PARADOX_DOMAIN_ID], row);
  enum sampler_kind kind = parameter_kind(
    STRING_ELT(params->columns[PARADOX_DOMAIN_CLS], row),
    STRING_ELT(params->columns[PARADOX_DOMAIN_STORAGE_TYPE], row)
  );
  SEXP levels = VECTOR_ELT(params->columns[PARADOX_DOMAIN_LEVELS], row);
  if (id != spec->id || kind != spec->kind || levels != spec->source_levels) {
    return FALSE;
  }
  if (kind == SAMPLER_KIND_DBL || kind == SAMPLER_KIND_INT) {
    return numeric_at(params->columns[PARADOX_DOMAIN_LOWER], row) ==
        spec->lower &&
      numeric_at(params->columns[PARADOX_DOMAIN_UPPER], row) == spec->upper &&
      numeric_at(params->columns[PARADOX_DOMAIN_TOLERANCE], row) ==
        spec->tolerance;
  }
  if (kind == SAMPLER_KIND_FCT) {
    const R_xlen_t count = XLENGTH(spec->stable_levels);
    if (TYPEOF(levels) != STRSXP || ALTREP(levels) ||
        XLENGTH(levels) != count) {
      return FALSE;
    }
    for (R_xlen_t index = 0; index < count; ++index) {
      if (STRING_ELT(levels, index) !=
          STRING_ELT(spec->stable_levels, index)) {
        return FALSE;
      }
    }
    return TRUE;
  }
  return TYPEOF(levels) == LGLSXP && !ALTREP(levels) &&
    XLENGTH(levels) == 2 && LOGICAL_ELT(levels, 0) == TRUE &&
    LOGICAL_ELT(levels, 1) == FALSE;
}

static int matching_specs(const sampler_spec_t *left,
    const sampler_spec_t *right) {
  if (left->id != right->id || left->kind != right->kind) {
    return FALSE;
  }
  if (left->kind == SAMPLER_KIND_DBL ||
      left->kind == SAMPLER_KIND_INT) {
    return left->lower == right->lower && left->upper == right->upper &&
      left->tolerance == right->tolerance;
  }
  if (TYPEOF(left->source_levels) != TYPEOF(right->source_levels) ||
      XLENGTH(left->source_levels) != XLENGTH(right->source_levels)) {
    return FALSE;
  }
  const R_xlen_t count = XLENGTH(left->source_levels);
  if (left->kind == SAMPLER_KIND_FCT) {
    for (R_xlen_t index = 0; index < count; ++index) {
      if (STRING_ELT(left->source_levels, index) !=
          STRING_ELT(right->source_levels, index)) {
        return FALSE;
      }
    }
    return TRUE;
  }
  for (R_xlen_t index = 0; index < count; ++index) {
    if (LOGICAL_ELT(left->source_levels, index) !=
        LOGICAL_ELT(right->source_levels, index)) {
      return FALSE;
    }
  }
  return TRUE;
}

static int empty_values(SEXP value) {
  if (TYPEOF(value) != VECSXP || ALTREP(value) || Rf_isObject(value) ||
      XLENGTH(value) != 0 ||
      !paradox_api_has_single_attribute(value, "names")) {
    return FALSE;
  }
  SEXP names = Rf_getAttrib(value, R_NamesSymbol);
  return TYPEOF(names) == STRSXP && !ALTREP(names) &&
    XLENGTH(names) == 0 && paradox_api_has_no_attributes(names);
}

static int empty_dependencies(SEXP value) {
  paradox_domain_dependencies_t dependencies;
  R_xlen_t work = 0;
  return paradox_domain_validate_dependencies(
    value,
    &dependencies,
    &work
  ) && dependencies.row_count == 0;
}

static int capture_import(SEXP namespace_environment,
    const char *binding_name, const char *provider_name,
    sampler_roots_t *roots, import_snapshot_t *snapshot) {
  SEXP provider = PROTECT(paradox_api_registered_namespace(provider_name));
  const int valid = TYPEOF(provider) == ENVSXP &&
    capture_locked_binding(
      namespace_environment,
      binding_name,
      roots,
      &snapshot->imported
    ) && capture_locked_binding(
      provider,
      binding_name,
      roots,
      &snapshot->provider
    ) && bindings_have_same_value(
      &snapshot->imported,
      &snapshot->provider
    ) && TYPEOF(snapshot->imported.value) == CLOSXP;
  if (!valid) {
    UNPROTECT(1);
    return FALSE;
  }
  UNPROTECT(1);
  return TRUE;
}

static int audit_import(const import_snapshot_t *snapshot) {
  return audit_locked_binding(&snapshot->imported) &&
    audit_locked_binding(&snapshot->provider) &&
    bindings_have_same_value(
      &snapshot->imported,
      &snapshot->provider
    );
}

static int load_param_set_state(SEXP param_set, sampler_roots_t *roots,
    param_set_snapshot_t *surface, params_snapshot_t *params,
    SEXP *values, SEXP *dependencies, int require_empty_dependencies) {
  if (!capture_param_set_surface(param_set, roots, surface)) {
    return FALSE;
  }
  SEXP private_environment = surface->private_environment;
  SEXP params_value = PROTECT(paradox_domain_local_value(
    private_environment,
    ".params"
  ));
  SEXP values_value = PROTECT(paradox_domain_local_value(
    private_environment,
    ".values"
  ));
  SEXP dependencies_value = PROTECT(paradox_domain_local_value(
    private_environment,
    ".deps"
  ));
  const int valid = capture_params(params_value, roots, params) &&
    empty_values(values_value) &&
    (!require_empty_dependencies || empty_dependencies(dependencies_value));
  if (valid) {
    *values = values_value;
    *dependencies = dependencies_value;
    retain(roots, values_value);
    retain(roots, dependencies_value);
  }
  UNPROTECT(3);
  return valid;
}

static int capture_graph(SEXP self, SEXP param_set, SEXP samplers,
    R_xlen_t size, int copy_levels, sampler_roots_t *roots,
    sampler_graph_t *graph) {
  static const char *const root_classes[] = {
    "SamplerUnif", "SamplerHierarchical", "Sampler", "R6"
  };
  static const char *const child_classes[] = {
    "Sampler1DUnif", "Sampler1D", "Sampler", "R6"
  };
  graph->self = self;
  graph->param_set = param_set;
  graph->samplers = samplers;
  graph->size = size;
  if (!exact_classes(self, root_classes, 4, &graph->classes) ||
      !R_EnvironmentIsLocked(self) || TYPEOF(samplers) != VECSXP ||
      ALTREP(samplers) || XLENGTH(samplers) != size ||
      !paradox_api_has_single_attribute(samplers, "names")) {
    return FALSE;
  }
  graph->sampler_names = Rf_getAttrib(samplers, R_NamesSymbol);
  if (TYPEOF(graph->sampler_names) != STRSXP ||
      ALTREP(graph->sampler_names) || XLENGTH(graph->sampler_names) != size ||
      !paradox_api_has_no_attributes(graph->sampler_names)) {
    return FALSE;
  }

  SEXP enclosure = PROTECT(paradox_domain_local_value(
    self,
    ".__enclos_env__"
  ));
  SEXP private_environment = PROTECT(paradox_domain_local_value(
    enclosure,
    "private"
  ));
  SEXP namespace_environment = PROTECT(
    paradox_api_registered_namespace("paradox")
  );
  if (TYPEOF(enclosure) != ENVSXP || TYPEOF(private_environment) != ENVSXP ||
      TYPEOF(namespace_environment) != ENVSXP ||
      paradox_api_parent_environment(enclosure) != namespace_environment ||
      paradox_domain_local_value(enclosure, "self") != self ||
      paradox_domain_local_value(enclosure, "private") !=
        private_environment ||
      !R_EnvironmentIsLocked(private_environment) ||
      paradox_domain_local_value(self, "param_set") != param_set ||
      paradox_domain_local_value(self, "samplers") != samplers) {
    UNPROTECT(3);
    return FALSE;
  }
  graph->enclosure = enclosure;
  graph->private_environment = private_environment;
  graph->namespace_environment = namespace_environment;
  graph->imports_environment = paradox_api_parent_environment(
    namespace_environment
  );
  if (TYPEOF(graph->imports_environment) != ENVSXP ||
      !R_EnvironmentIsLocked(graph->imports_environment)) {
    UNPROTECT(3);
    return FALSE;
  }
  retain(roots, self);
  retain(roots, graph->classes);
  retain(roots, enclosure);
  retain(roots, private_environment);
  retain(roots, namespace_environment);
  retain(roots, graph->imports_environment);
  retain(roots, samplers);
  retain(roots, graph->sampler_names);

  if (!capture_wrapper(
      self, self, private_environment, R_NilValue,
      "sample", ".__Sampler__sample", FALSE, TRUE,
      namespace_environment, roots, &graph->public_sample
    )) {
    UNPROTECT(3);
    return FALSE;
  }
  if (!capture_wrapper(
      private_environment, self, private_environment, enclosure,
      ".sample", ".__SamplerUnif__.sample", FALSE, TRUE,
      namespace_environment, roots, &graph->private_sample
    )) {
    UNPROTECT(3);
    return FALSE;
  }

  static const char *const import_names[] = {
    "runif", "data.table", "setnames", "map_dtc"
  };
  static const char *const provider_names[] = {
    "stats", "data.table", "data.table", "mlr3misc"
  };
  for (size_t index = 0; index < SAMPLER_IMPORT_COUNT; ++index) {
    if (!capture_import(
        graph->imports_environment,
        import_names[index],
        provider_names[index],
        roots,
        &graph->imports[index]
      )) {
      UNPROTECT(3);
      return FALSE;
    }
  }

  SEXP ignored_dependencies = R_NilValue;
  if (!load_param_set_state(
      param_set,
      roots,
      &graph->param_set_surface,
      &graph->params,
      &graph->root_values,
      &ignored_dependencies,
      FALSE
    ) || XLENGTH(graph->params.columns[PARADOX_DOMAIN_ID]) != size) {
    UNPROTECT(3);
    return FALSE;
  }

  for (R_xlen_t index = 0; index < size; ++index) {
    child_snapshot_t *child = &graph->children[index];
    SEXP child_object = PROTECT(VECTOR_ELT(samplers, index));
    if (!exact_classes(
        child_object,
        child_classes,
        4,
        &child->classes
      ) || !R_EnvironmentIsLocked(child_object)) {
      UNPROTECT(4);
      return FALSE;
    }
    SEXP child_enclosure = PROTECT(paradox_domain_local_value(
      child_object,
      ".__enclos_env__"
    ));
    SEXP child_private = PROTECT(paradox_domain_local_value(
      child_enclosure,
      "private"
    ));
    SEXP child_param_set = PROTECT(paradox_domain_local_value(
      child_object,
      "param_set"
    ));
    if (TYPEOF(child_enclosure) != ENVSXP ||
        TYPEOF(child_private) != ENVSXP ||
        paradox_api_parent_environment(child_enclosure) !=
          namespace_environment ||
        paradox_domain_local_value(child_enclosure, "self") != child_object ||
        paradox_domain_local_value(child_enclosure, "private") !=
        child_private || !R_EnvironmentIsLocked(child_private)) {
      UNPROTECT(7);
      return FALSE;
    }
    child->child = child_object;
    child->enclosure = child_enclosure;
    child->private_environment = child_private;
    child->param_set = child_param_set;
    retain(roots, child_object);
    retain(roots, child->classes);
    retain(roots, child_enclosure);
    retain(roots, child_private);
    retain(roots, child_param_set);

    if (!capture_wrapper(
        child_object, child_object, child_private, R_NilValue,
        "sample", ".__Sampler__sample", FALSE, TRUE,
        namespace_environment, roots, &child->public_sample
      ) || !capture_wrapper(
        child_private, child_object, child_private, child_enclosure,
        ".sample", ".__Sampler1DUnif__.sample", FALSE, TRUE,
        namespace_environment, roots, &child->private_sample
      ) || !capture_wrapper(
        child_object, child_object, child_private, R_NilValue,
        "param", ".__Sampler1D__param", TRUE, FALSE,
        namespace_environment, roots, &child->active_param
      ) || !load_param_set_state(
        child_param_set,
        roots,
        &child->param_set_surface,
        &child->params,
        &child->values,
        &child->dependencies,
        TRUE
      ) || XLENGTH(child->params.columns[PARADOX_DOMAIN_ID]) != 1 ||
      !capture_spec(
        &child->params,
        0,
        copy_levels,
        roots,
        &child->spec
      )) {
      UNPROTECT(7);
      return FALSE;
    }

    sampler_spec_t root_spec;
    if (!capture_spec(
        &graph->params,
        index,
        FALSE,
        NULL,
        &root_spec
      ) || !matching_specs(&root_spec, &child->spec) ||
        STRING_ELT(graph->sampler_names, index) != child->spec.id) {
      UNPROTECT(7);
      return FALSE;
    }
    UNPROTECT(4);
  }
  UNPROTECT(3);
  return TRUE;
}

static int audit_graph(const sampler_graph_t *graph) {
  static const char *const root_classes[] = {
    "SamplerUnif", "SamplerHierarchical", "Sampler", "R6"
  };
  static const char *const child_classes[] = {
    "Sampler1DUnif", "Sampler1D", "Sampler", "R6"
  };
  SEXP classes;
  if (!exact_classes(graph->self, root_classes, 4, &classes) ||
      classes != graph->classes ||
      paradox_domain_local_value(graph->self, ".__enclos_env__") !=
        graph->enclosure ||
      paradox_domain_local_value(graph->enclosure, "private") !=
        graph->private_environment ||
      paradox_domain_local_value(graph->self, "param_set") !=
        graph->param_set ||
      paradox_domain_local_value(graph->self, "samplers") !=
        graph->samplers ||
      !paradox_api_has_single_attribute(graph->samplers, "names") ||
      Rf_getAttrib(graph->samplers, R_NamesSymbol) != graph->sampler_names ||
      !audit_wrapper(&graph->public_sample, graph->namespace_environment) ||
      !audit_wrapper(&graph->private_sample, graph->namespace_environment) ||
      !audit_param_set_surface(&graph->param_set_surface) ||
      paradox_domain_local_value(
        graph->param_set_surface.private_environment,
        ".params"
      ) != graph->params.table ||
      paradox_domain_local_value(
        graph->param_set_surface.private_environment,
        ".values"
      ) != graph->root_values ||
      !audit_params(&graph->params, graph->size) ||
      !empty_values(graph->root_values)) {
    return FALSE;
  }
  for (size_t index = 0; index < SAMPLER_IMPORT_COUNT; ++index) {
    if (!audit_import(&graph->imports[index])) {
      return FALSE;
    }
  }
  for (R_xlen_t index = 0; index < graph->size; ++index) {
    const child_snapshot_t *child = &graph->children[index];
    SEXP child_classes_value;
    sampler_spec_t root_spec;
    const int root_spec_matches = capture_spec(
      &graph->params,
      index,
      FALSE,
      NULL,
      &root_spec
    ) && matching_specs(&root_spec, &child->spec);
    if (VECTOR_ELT(graph->samplers, index) != child->child ||
        !exact_classes(
          child->child,
          child_classes,
          4,
          &child_classes_value
        ) || child_classes_value != child->classes ||
        paradox_domain_local_value(child->child, ".__enclos_env__") !=
          child->enclosure ||
        paradox_domain_local_value(child->enclosure, "private") !=
          child->private_environment ||
        paradox_domain_local_value(child->child, "param_set") !=
          child->param_set ||
        !audit_wrapper(&child->public_sample, graph->namespace_environment) ||
        !audit_wrapper(&child->private_sample, graph->namespace_environment) ||
        !audit_wrapper(&child->active_param, graph->namespace_environment) ||
        !audit_param_set_surface(&child->param_set_surface) ||
        paradox_domain_local_value(
          child->param_set_surface.private_environment,
          ".params"
        ) != child->params.table ||
        paradox_domain_local_value(
          child->param_set_surface.private_environment,
          ".values"
        ) != child->values ||
        paradox_domain_local_value(
          child->param_set_surface.private_environment,
          ".deps"
        ) != child->dependencies ||
        !audit_params(&child->params, 1) ||
        !empty_values(child->values) ||
        !empty_dependencies(child->dependencies) ||
        !audit_spec(&child->params, 0, &child->spec) ||
        !root_spec_matches ||
        STRING_ELT(graph->sampler_names, index) != child->spec.id) {
      return FALSE;
    }
  }
  return TRUE;
}

static int parse_row_count(SEXP n, R_xlen_t *rows) {
  if (ALTREP(n) || XLENGTH(n) != 1 ||
      !paradox_api_has_no_attributes(n)) {
    return FALSE;
  }
  double value;
  if (TYPEOF(n) == INTSXP) {
    const int observed = INTEGER_ELT(n, 0);
    if (observed == NA_INTEGER) {
      return FALSE;
    }
    value = (double) observed;
  } else if (TYPEOF(n) == REALSXP) {
    value = REAL_ELT(n, 0);
  } else {
    return FALSE;
  }
  if (!R_FINITE(value) || value < 0.0 || value > (double) INT_MAX ||
      floor(value) != value) {
    return FALSE;
  }
  *rows = (R_xlen_t) value;
  return TRUE;
}

static int ordinary_builtin_seed(void) {
  SEXP symbol = Rf_install(".Random.seed");
  SEXP seed = paradox_api_local_value(R_GlobalEnv, symbol);
  if (seed == R_UnboundValue || R_BindingIsLocked(symbol, R_GlobalEnv) ||
      TYPEOF(seed) != INTSXP || ALTREP(seed) ||
      MAYBE_SHARED(seed) || !paradox_api_has_no_attributes(seed) ||
      XLENGTH(seed) < 1) {
    return FALSE;
  }
  const int code = INTEGER_ELT(seed, 0);
  if (code < 0 || code > 11000 || (code % 10000) / 100 > 5 ||
      code / 10000 > 1) {
    return FALSE;
  }
  const int kind = code % 100;
  static const R_xlen_t seed_lengths[] = {4, 3, 3, 626, 102, 1, 102, 7};
  return kind >= 0 && kind < 8 && kind != 5 &&
    XLENGTH(seed) == seed_lengths[kind];
}

static SEXPTYPE output_type(enum sampler_kind kind) {
  switch (kind) {
  case SAMPLER_KIND_DBL:
    return REALSXP;
  case SAMPLER_KIND_INT:
    return INTSXP;
  case SAMPLER_KIND_FCT:
    return STRSXP;
  case SAMPLER_KIND_LGL:
    return LGLSXP;
  case SAMPLER_KIND_UNKNOWN:
    break;
  }
  return NILSXP;
}

static SEXP allocate_result(const sampler_spec_t *specs, R_xlen_t size,
    R_xlen_t rows) {
  SEXP table = PROTECT(Rf_allocVector(VECSXP, size));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t column = 0; column < size; ++column) {
    SEXP output = PROTECT(Rf_allocVector(
      output_type(specs[column].kind),
      rows
    ));
    SET_VECTOR_ELT(table, column, output);
    SET_STRING_ELT(names, column, specs[column].id);
    UNPROTECT(1);
  }
  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(classes, 0, Rf_mkChar("data.table"));
  SET_STRING_ELT(classes, 1, Rf_mkChar("data.frame"));
  SEXP row_names;
  if (rows == 0) {
    row_names = PROTECT(Rf_allocVector(INTSXP, 0));
  } else {
    row_names = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(row_names)[0] = NA_INTEGER;
    INTEGER(row_names)[1] = -(int) rows;
  }
  Rf_setAttrib(table, R_RowNamesSymbol, row_names);
  Rf_setAttrib(table, R_ClassSymbol, classes);
  Rf_setAttrib(table, R_NamesSymbol, names);
  SEXP result = PROTECT(paradox_prepare_data_table(table, TRUE));
  UNPROTECT(5);
  return result;
}

static void fill_result(SEXP result, const sampler_graph_t *graph,
    R_xlen_t rows) {
  GetRNGstate();
  for (R_xlen_t column = 0; column < graph->size; ++column) {
    SEXP output = VECTOR_ELT(result, column);
    const sampler_spec_t *spec = &graph->children[column].spec;
    for (R_xlen_t row = 0; row < rows; ++row) {
      const double unit = unif_rand();
      switch (spec->kind) {
      case SAMPLER_KIND_DBL:
        REAL(output)[row] = paradox_qunif_double_value(
          unit,
          spec->lower,
          spec->upper
        );
        break;
      case SAMPLER_KIND_INT: {
        int mapped = NA_INTEGER;
        /* Admission proves finite integer bounds and R's built-in generators
         * return a strict unit variate, so failure is unreachable. Retaining
         * the checked helper still makes an impossible platform violation a
         * missing value rather than undefined conversion behavior. */
        (void) paradox_qunif_integer_value(
          unit,
          spec->lower,
          spec->upper,
          &mapped
        );
        INTEGER(output)[row] = mapped;
        break;
      }
      case SAMPLER_KIND_FCT: {
        const R_xlen_t count = XLENGTH(spec->stable_levels);
        R_xlen_t selected = paradox_qunif_level_index(
          unit,
          count
        );
        if (selected == R_XLEN_T_MAX || selected >= count) {
          selected = count - 1;
        }
        SET_STRING_ELT(
          output,
          row,
          STRING_ELT(spec->stable_levels, selected)
        );
        break;
      }
      case SAMPLER_KIND_LGL:
        LOGICAL(output)[row] = unit < 0.5;
        break;
      case SAMPLER_KIND_UNKNOWN:
        break;
      }
    }
  }
  PutRNGstate();
}

#endif

SEXP paradox_sampler_unif_sample_builtin(SEXP self, SEXP param_set,
    SEXP samplers, SEXP n) {
#if R_VERSION < R_Version(4, 6, 0)
  (void) self;
  (void) param_set;
  (void) samplers;
  (void) n;
  return R_NilValue;
#else
  R_xlen_t rows;
  if (!parse_row_count(n, &rows) || TYPEOF(samplers) != VECSXP ||
      ALTREP(samplers)) {
    return R_NilValue;
  }
  const R_xlen_t size = XLENGTH(samplers);
  if (size == 0 || size > (R_xlen_t) INT_MAX ||
      (rows != 0 && !ordinary_builtin_seed())) {
    return R_NilValue;
  }
  if (size > (R_XLEN_T_MAX - 128) / 80) {
    return R_NilValue;
  }

  sampler_spec_t *preliminary_specs = paradox_temporary_alloc(
    size,
    sizeof(*preliminary_specs)
  );
  child_snapshot_t *preliminary_children = paradox_temporary_alloc(
    size,
    sizeof(*preliminary_children)
  );
  sampler_graph_t preliminary = {0};
  preliminary.children = preliminary_children;
  SEXP preliminary_root_values = PROTECT(Rf_allocVector(
    VECSXP,
    128 + size * 80
  ));
  sampler_roots_t preliminary_roots = {preliminary_root_values, 0};
  if (!capture_graph(
      self,
      param_set,
      samplers,
      size,
      FALSE,
      &preliminary_roots,
      &preliminary
    )) {
    UNPROTECT(1);
    return R_NilValue;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    preliminary_specs[index] = preliminary.children[index].spec;
  }

  SEXP result = PROTECT(allocate_result(preliminary_specs, size, rows));
  SEXP root_values = PROTECT(Rf_allocVector(VECSXP, 128 + size * 80));
  sampler_roots_t roots = {root_values, 0};
  child_snapshot_t *children = paradox_temporary_alloc(
    size,
    sizeof(*children)
  );
  sampler_graph_t graph = {0};
  graph.children = children;
  if (!capture_graph(
      self,
      param_set,
      samplers,
      size,
      TRUE,
      &roots,
      &graph
    )) {
    UNPROTECT(3);
    return R_NilValue;
  }
  if (!audit_graph(&graph)) {
    UNPROTECT(3);
    return R_NilValue;
  }
  R_xlen_t audited_rows;
  if (!parse_row_count(n, &audited_rows) || audited_rows != rows) {
    UNPROTECT(3);
    return R_NilValue;
  }
  if (rows != 0 && !ordinary_builtin_seed()) {
    UNPROTECT(3);
    return R_NilValue;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    if (graph.children[index].spec.kind != preliminary_specs[index].kind ||
        graph.children[index].spec.id != preliminary_specs[index].id ||
        (SEXPTYPE) TYPEOF(VECTOR_ELT(result, index)) !=
          output_type(graph.children[index].spec.kind) ||
        XLENGTH(VECTOR_ELT(result, index)) != rows) {
      UNPROTECT(3);
      return R_NilValue;
    }
  }

  if (rows != 0) {
    fill_result(result, &graph, rows);
  }
  UNPROTECT(3);
  return result;
#endif
}
