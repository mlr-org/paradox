#include <stddef.h>

#include "paradox.h"

#include "paramset_domain_common.h"
#include "r_api_compat.h"

typedef enum {
  SURFACE_DESIGN_TRAFO = 1,
  SURFACE_CHECK = 2,
  SURFACE_CHECK_DT = 3,
  SURFACE_RANDOM_DESIGN = 4
} surface_mode_t;

typedef enum {
  FORMAL_MISSING = 0,
  FORMAL_NULL,
  FORMAL_FALSE,
  FORMAL_TRUE,
  FORMAL_STRING_NONE,
  FORMAL_SELF,
  FORMAL_PRIVATE_PARAMS_ID
} formal_default_t;

typedef struct {
  SEXP self;
  SEXP enclosure;
  SEXP private_environment;
  SEXP namespace_environment;
  SEXP super_object;
  SEXP super_enclosure;
  int is_collection;
  R_xlen_t work_since_interrupt;
} surface_state_t;

enum surface_root_slot {
  SURFACE_ROOT_SELF = 0,
  SURFACE_ROOT_CLASSES,
  SURFACE_ROOT_NAMESPACE,
  SURFACE_ROOT_ENCLOSURE,
  SURFACE_ROOT_PRIVATE,
  SURFACE_ROOT_SUPER_OBJECT,
  SURFACE_ROOT_SUPER_ENCLOSURE,
  SURFACE_ROOT_COUNT
};

static int scalar_flag_is(SEXP value, int expected) {
  return TYPEOF(value) == LGLSXP && !ALTREP(value) &&
    XLENGTH(value) == 1 && paradox_api_has_no_attributes(value) &&
    LOGICAL_ELT(value, 0) == expected;
}

static int scalar_string_is(SEXP value, const char *expected) {
  return TYPEOF(value) == STRSXP && !ALTREP(value) &&
    XLENGTH(value) == 1 && paradox_api_has_no_attributes(value) &&
    paradox_domain_string_is(STRING_ELT(value, 0), expected);
}

static int private_params_id_expression(SEXP value) {
  if (TYPEOF(value) != LANGSXP || CAR(value) != Rf_install("$") ||
      CDR(value) == R_NilValue ||
      TYPEOF(CDR(value)) != LISTSXP ||
      CDR(CDR(value)) == R_NilValue ||
      TYPEOF(CDR(CDR(value))) != LISTSXP ||
      CDR(CDR(CDR(value))) != R_NilValue ||
      CAR(CDR(CDR(value))) != Rf_install("id")) {
    return FALSE;
  }
  SEXP inner = CAR(CDR(value));
  return TYPEOF(inner) == LANGSXP && CAR(inner) == Rf_install("$") &&
    CDR(inner) != R_NilValue && TYPEOF(CDR(inner)) == LISTSXP &&
    CDR(CDR(inner)) != R_NilValue &&
    TYPEOF(CDR(CDR(inner))) == LISTSXP &&
    CDR(CDR(CDR(inner))) == R_NilValue &&
    CAR(CDR(inner)) == Rf_install("private") &&
    CAR(CDR(CDR(inner))) == Rf_install(".params");
}

static int formal_value_is(SEXP value, formal_default_t expected) {
  switch (expected) {
    case FORMAL_MISSING:
      return value == R_MissingArg;
    case FORMAL_NULL:
      return value == R_NilValue;
    case FORMAL_FALSE:
      return scalar_flag_is(value, FALSE);
    case FORMAL_TRUE:
      return scalar_flag_is(value, TRUE);
    case FORMAL_STRING_NONE:
      return scalar_string_is(value, "none");
    case FORMAL_SELF:
      return value == Rf_install("self");
    case FORMAL_PRIVATE_PARAMS_ID:
      return private_params_id_expression(value);
    default:
      return FALSE;
  }
}

static int exact_formals(SEXP formals, const char *const *names,
    const formal_default_t *defaults, R_xlen_t count) {
  for (R_xlen_t index = 0; index < count; ++index) {
    if (TYPEOF(formals) != LISTSXP ||
        TAG(formals) != Rf_install(names[index]) ||
        !formal_value_is(CAR(formals), defaults[index])) {
      return FALSE;
    }
    formals = CDR(formals);
  }
  return formals == R_NilValue;
}

static int exact_forwarding_body(SEXP body, const char *target,
    const char *const *formal_names, R_xlen_t formal_count) {
  if (TYPEOF(body) != LANGSXP || CAR(body) != Rf_install(target)) {
    return FALSE;
  }

  static const char *const fixed_names[] = {"self", "private", "super"};
  SEXP argument = CDR(body);
  for (R_xlen_t index = 0; index < 3; ++index) {
    SEXP symbol = Rf_install(fixed_names[index]);
    if (TYPEOF(argument) != LISTSXP || TAG(argument) != symbol ||
        CAR(argument) != symbol) {
      return FALSE;
    }
    argument = CDR(argument);
  }
  for (R_xlen_t index = 0; index < formal_count; ++index) {
    SEXP symbol = Rf_install(formal_names[index]);
    if (TYPEOF(argument) != LISTSXP || TAG(argument) != symbol ||
        CAR(argument) != symbol) {
      return FALSE;
    }
    argument = CDR(argument);
  }
  return argument == R_NilValue;
}

static int active_list_contains(surface_state_t *state,
    const char *binding_name, SEXP closure, SEXP closure_environment,
    const char *target, const char *const *formal_names,
    const formal_default_t *formal_defaults, R_xlen_t formal_count) {
  SEXP active = PROTECT(paradox_domain_local_value(
    state->enclosure,
    ".__active__"
  ));
  SEXP names = PROTECT(Rf_getAttrib(active, R_NamesSymbol));
  if (TYPEOF(active) != VECSXP || ALTREP(active) ||
      TYPEOF(names) != STRSXP || ALTREP(names) ||
      XLENGTH(active) != XLENGTH(names)) {
    UNPROTECT(2);
    return FALSE;
  }

  R_xlen_t found = R_XLEN_T_MAX;
  for (R_xlen_t index = 0; index < XLENGTH(names); ++index) {
    paradox_domain_account_work(&state->work_since_interrupt);
    if (paradox_domain_string_is(STRING_ELT(names, index), binding_name)) {
      if (found != R_XLEN_T_MAX) {
        UNPROTECT(2);
        return FALSE;
      }
      found = index;
    }
  }
  if (found == R_XLEN_T_MAX) {
    UNPROTECT(2);
    return FALSE;
  }
  SEXP registered_closure = PROTECT(VECTOR_ELT(active, found));
  if (registered_closure == closure) {
    UNPROTECT(3);
    return TRUE;
  }

  /* Serialization duplicates the active binding closure and the R6 registry
   * closure separately.  Match that canonical representation without
   * R_compute_identical(): its default closure comparison strips source
   * references by allocating duplicate closures, so a finalizer could change
   * state authenticated earlier in this call.  The native lane is unavailable
   * before R 4.6; on admitted runtimes these public closure accessors are
   * direct, and the exact generated formals/body checks below do not allocate.
   * A merely similar closure with different executable structure still fails
   * closed. */
  if (TYPEOF(registered_closure) != CLOSXP) {
    UNPROTECT(3);
    return FALSE;
  }
  SEXP registered_formals = PROTECT(paradox_api_closure_formals(
    registered_closure
  ));
  SEXP registered_body = PROTECT(paradox_api_closure_expression(
    registered_closure
  ));
  SEXP registered_environment = PROTECT(paradox_api_closure_environment(
    registered_closure
  ));
  const int canonical = registered_environment == closure_environment &&
    exact_formals(
      registered_formals,
      formal_names,
      formal_defaults,
      formal_count
    ) && exact_forwarding_body(
      registered_body,
      target,
      formal_names,
      formal_count
    );
  UNPROTECT(6);
  return canonical;
}

static int canonical_wrapper(surface_state_t *state, SEXP container,
    SEXP expected_environment, SEXP expected_super,
    const char *binding_name, const char *target,
    const char *const *formal_names,
    const formal_default_t *formal_defaults, R_xlen_t formal_count,
    Rboolean active) {
  SEXP binding_symbol = Rf_install(binding_name);
  SEXP target_symbol = Rf_install(target);
  if (TYPEOF(container) != ENVSXP ||
      !R_existsVarInFrame(container, binding_symbol) ||
      R_BindingIsActive(binding_symbol, container) != active ||
      (!active && !R_BindingIsLocked(binding_symbol, container)) ||
      !R_existsVarInFrame(state->namespace_environment, target_symbol) ||
      R_BindingIsActive(target_symbol, state->namespace_environment) ||
      !R_BindingIsLocked(target_symbol, state->namespace_environment)) {
    return FALSE;
  }

  SEXP closure = PROTECT(active
    ? R_ActiveBindingFunction(binding_symbol, container)
    : paradox_domain_local_value(container, binding_name));
  if (TYPEOF(closure) != CLOSXP) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP formals = PROTECT(paradox_api_closure_formals(closure));
  SEXP body = PROTECT(paradox_api_closure_expression(closure));
  if (!exact_formals(
        formals,
        formal_names,
        formal_defaults,
        formal_count
      ) ||
      !exact_forwarding_body(
        body,
        target,
        formal_names,
        formal_count
      )) {
    UNPROTECT(3);
    return FALSE;
  }

  SEXP environment = PROTECT(paradox_api_closure_environment(closure));
  SEXP super_symbol = Rf_install("super");
  const int expects_super = expected_super != R_UnboundValue;
  const int exact = TYPEOF(environment) == ENVSXP &&
    environment == expected_environment &&
    paradox_api_parent_environment(environment) ==
      state->namespace_environment &&
    paradox_domain_local_value(environment, "self") == state->self &&
    paradox_domain_local_value(environment, "private") ==
      state->private_environment &&
    !R_existsVarInFrame(environment, target_symbol) &&
    ((!expects_super && !R_existsVarInFrame(environment, super_symbol)) ||
      (expects_super && R_existsVarInFrame(environment, super_symbol) &&
        paradox_domain_local_value(environment, "super") ==
          expected_super)) &&
    (!active || (container == state->self && active_list_contains(
      state,
      binding_name,
      closure,
      environment,
      target,
      formal_names,
      formal_defaults,
      formal_count
    )));
  UNPROTECT(4);
  return exact;
}

static int canonical_method(surface_state_t *state, SEXP container,
    SEXP expected_environment, SEXP expected_super,
    const char *binding_name, const char *target,
    const char *const *formal_names,
    const formal_default_t *formal_defaults, R_xlen_t formal_count) {
  return canonical_wrapper(
    state,
    container,
    expected_environment,
    expected_super,
    binding_name,
    target,
    formal_names,
    formal_defaults,
    formal_count,
    FALSE
  );
}

static int canonical_active(surface_state_t *state,
    SEXP expected_environment, SEXP expected_super,
    const char *binding_name, const char *target,
    const char *const *formal_names,
    const formal_default_t *formal_defaults, R_xlen_t formal_count) {
  return canonical_wrapper(
    state,
    state->self,
    expected_environment,
    expected_super,
    binding_name,
    target,
    formal_names,
    formal_defaults,
    formal_count,
    TRUE
  );
}

static int load_state(SEXP self, surface_state_t *state, SEXP roots) {
  static const char *const set_classes[] = {"ParamSet", "R6"};
  static const char *const collection_classes[] = {
    "ParamSetCollection", "ParamSet", "R6"
  };
  if (TYPEOF(self) != ENVSXP || TYPEOF(roots) != VECSXP ||
      XLENGTH(roots) < SURFACE_ROOT_COUNT) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, SURFACE_ROOT_SELF, self);

  SEXP classes = Rf_getAttrib(self, R_ClassSymbol);
  SET_VECTOR_ELT(roots, SURFACE_ROOT_CLASSES, classes);
  if (TYPEOF(classes) != STRSXP || ALTREP(classes)) {
    return FALSE;
  }
  state->work_since_interrupt = 0;
  const int is_set = paradox_domain_exact_string_vector(
    classes,
    set_classes,
    2,
    &state->work_since_interrupt
  );
  const int is_collection = paradox_domain_exact_string_vector(
    classes,
    collection_classes,
    3,
    &state->work_since_interrupt
  );
  if (!is_set && !is_collection) {
    return FALSE;
  }

  state->self = self;
  state->is_collection = is_collection;
  state->namespace_environment = paradox_api_registered_namespace("paradox");
  if (state->namespace_environment != R_UnboundValue) {
    SET_VECTOR_ELT(
      roots,
      SURFACE_ROOT_NAMESPACE,
      state->namespace_environment
    );
  }
  state->enclosure = paradox_domain_local_value(self, ".__enclos_env__");
  if (state->enclosure != R_UnboundValue) {
    SET_VECTOR_ELT(roots, SURFACE_ROOT_ENCLOSURE, state->enclosure);
  }
  state->private_environment = paradox_domain_local_value(
    state->enclosure,
    "private"
  );
  if (state->private_environment != R_UnboundValue) {
    SET_VECTOR_ELT(
      roots,
      SURFACE_ROOT_PRIVATE,
      state->private_environment
    );
  }
  if (TYPEOF(state->namespace_environment) != ENVSXP ||
      TYPEOF(state->enclosure) != ENVSXP ||
      TYPEOF(state->private_environment) != ENVSXP ||
      paradox_api_parent_environment(state->enclosure) !=
        state->namespace_environment ||
      paradox_domain_local_value(state->enclosure, "self") != self) {
    return FALSE;
  }

  SEXP super_symbol = Rf_install("super");
  if (is_set) {
    if (R_existsVarInFrame(state->enclosure, super_symbol)) {
      return FALSE;
    }
    state->super_object = R_UnboundValue;
    state->super_enclosure = R_UnboundValue;
    return TRUE;
  }

  state->super_object = paradox_domain_local_value(
    state->enclosure,
    "super"
  );
  if (state->super_object != R_UnboundValue) {
    SET_VECTOR_ELT(roots, SURFACE_ROOT_SUPER_OBJECT, state->super_object);
  }
  state->super_enclosure = paradox_domain_local_value(
    state->super_object,
    ".__enclos_env__"
  );
  if (state->super_enclosure != R_UnboundValue) {
    SET_VECTOR_ELT(
      roots,
      SURFACE_ROOT_SUPER_ENCLOSURE,
      state->super_enclosure
    );
  }
  return TYPEOF(state->super_object) == ENVSXP &&
    TYPEOF(state->super_enclosure) == ENVSXP &&
    paradox_api_parent_environment(state->super_enclosure) ==
      state->namespace_environment &&
    paradox_domain_local_value(state->super_enclosure, "self") == self &&
    paradox_domain_local_value(state->super_enclosure, "private") ==
      state->private_environment &&
    !R_existsVarInFrame(state->super_enclosure, super_symbol);
}

static SEXP inherited_environment(const surface_state_t *state) {
  return state->is_collection ? state->super_enclosure : state->enclosure;
}

static int direct_private_binding(const surface_state_t *state,
    const char *name) {
  /* `paradox_domain_local_value()` rejects active and promise bindings without
   * evaluating them.  Public wrapper authentication alone is insufficient:
   * the generated method may be canonical while its private backing store was
   * replaced with a delayed or active binding that the R fast-path gate would
   * otherwise force immediately after this call. */
  return paradox_domain_local_value(state->private_environment, name) !=
    R_UnboundValue;
}

static int authenticate_design_trafo(surface_state_t *state) {
  static const char *const trafo_names[] = {"x", "param_set"};
  static const formal_default_t trafo_defaults[] = {
    FORMAL_MISSING, FORMAL_SELF
  };
  static const char *const setter_name[] = {"f"};
  static const formal_default_t setter_default[] = {FORMAL_MISSING};
  return !state->is_collection && direct_private_binding(state, ".trafos") &&
    canonical_method(
      state,
      state->self,
      state->enclosure,
      R_UnboundValue,
      "trafo",
      ".__ParamSet__trafo",
      trafo_names,
      trafo_defaults,
      2
    ) && canonical_active(
    state,
    state->enclosure,
    R_UnboundValue,
    "extra_trafo",
    ".__ParamSet__extra_trafo",
    setter_name,
    setter_default,
    1
  ) && canonical_active(
    state,
    state->enclosure,
    R_UnboundValue,
    "has_trafo",
    ".__ParamSet__has_trafo",
    NULL,
    NULL,
    0
  );
}

static int authenticate_check(surface_state_t *state, int table,
    int allow_collection) {
  static const char *const check_names[] = {
    "xs", "check_strict", "sanitize", "presence", "allow_token"
  };
  static const formal_default_t check_defaults[] = {
    FORMAL_MISSING, FORMAL_TRUE, FORMAL_FALSE, FORMAL_STRING_NONE,
    FORMAL_TRUE
  };
  static const char *const check_dt_names[] = {
    "xdt", "check_strict", "presence", "allow_token"
  };
  static const formal_default_t check_dt_defaults[] = {
    FORMAL_MISSING, FORMAL_TRUE, FORMAL_STRING_NONE, FORMAL_TRUE
  };
  static const char *const constraint_names[] = {"x", "assert_value"};
  static const formal_default_t constraint_defaults[] = {
    FORMAL_MISSING, FORMAL_TRUE
  };
  static const char *const xs_name[] = {"xs"};
  static const formal_default_t xs_default[] = {FORMAL_MISSING};
  static const char *const active_name[] = {"f"};
  static const formal_default_t active_default[] = {FORMAL_MISSING};
  static const char *const deps_name[] = {"v"};
  static const formal_default_t deps_default[] = {FORMAL_MISSING};
  SEXP inherited = inherited_environment(state);
  if ((state->is_collection && !allow_collection) ||
      !direct_private_binding(state, ".params") ||
      !direct_private_binding(state, ".deps") ||
      (!state->is_collection &&
        !direct_private_binding(state, ".constraint")) ||
      (state->is_collection && !direct_private_binding(state, ".sets")) ||
      !canonical_method(
      state,
      state->self,
      inherited,
      R_UnboundValue,
      "check",
      ".__ParamSet__check",
      check_names,
      check_defaults,
      5
    ) || !canonical_method(
      state,
      state->self,
      inherited,
      R_UnboundValue,
      "test_constraint",
      ".__ParamSet__test_constraint",
      constraint_names,
      constraint_defaults,
      2
    ) || !canonical_method(
      state,
      state->self,
      inherited,
      R_UnboundValue,
      "check_dependencies",
      ".__ParamSet__check_dependencies",
      xs_name,
      xs_default,
      1
    ) || !canonical_active(
      state,
      state->enclosure,
      state->is_collection ? state->super_object : R_UnboundValue,
      "constraint",
      state->is_collection
        ? ".__ParamSetCollection__constraint"
        : ".__ParamSet__constraint",
      active_name,
      active_default,
      1
    ) || !canonical_active(
      state,
      state->enclosure,
      state->is_collection ? state->super_object : R_UnboundValue,
      "deps",
      state->is_collection
        ? ".__ParamSetCollection__deps"
        : ".__ParamSet__deps",
      deps_name,
      deps_default,
      1
    )) {
    return FALSE;
  }
  return !table || canonical_method(
    state,
    state->self,
    inherited,
    R_UnboundValue,
    "check_dt",
    ".__ParamSet__check_dt",
    check_dt_names,
    check_dt_defaults,
    4
  );
}

static int authenticate_random_design(surface_state_t *state) {
  static const char *const clone_names[] = {"deep"};
  static const formal_default_t clone_defaults[] = {FORMAL_FALSE};
  static const char *const ids_names[] = {"class", "tags", "any_tags"};
  static const formal_default_t ids_defaults[] = {
    FORMAL_NULL, FORMAL_NULL, FORMAL_NULL
  };
  static const char *const x_name[] = {"x"};
  static const formal_default_t x_default[] = {FORMAL_MISSING};
  static const char *const subspaces_names[] = {"ids"};
  static const formal_default_t subspaces_defaults[] = {
    FORMAL_PRIVATE_PARAMS_ID
  };
  static const char *const values_names[] = {"xs"};
  static const formal_default_t values_defaults[] = {FORMAL_MISSING};
  if (!direct_private_binding(state, ".params") ||
      (state->is_collection && !direct_private_binding(state, ".sets"))) {
    return FALSE;
  }
  SEXP inherited = inherited_environment(state);
  if (!canonical_method(
      state,
      state->self,
      state->is_collection ? state->enclosure : inherited,
      state->is_collection ? state->super_object : R_UnboundValue,
      "clone",
      state->is_collection
        ? ".__ParamSetCollection__clone"
        : ".__ParamSet__clone",
      clone_names,
      clone_defaults,
      1
    ) || !canonical_method(
      state,
      state->self,
      inherited,
      R_UnboundValue,
      "ids",
      ".__ParamSet__ids",
      ids_names,
      ids_defaults,
      3
    ) || !canonical_method(
      state,
      state->self,
      inherited,
      R_UnboundValue,
      "qunif",
      ".__ParamSet__qunif",
      x_name,
      x_default,
      1
    ) || !canonical_method(
      state,
      state->self,
      inherited,
      R_UnboundValue,
      "subspaces",
      ".__ParamSet__subspaces",
      subspaces_names,
      subspaces_defaults,
      1
    )) {
    return FALSE;
  }

  static const char *const active_names[] = {
    "length", "class", "is_bounded", "has_deps"
  };
  static const char *const active_targets[] = {
    ".__ParamSet__length", ".__ParamSet__class",
    ".__ParamSet__is_bounded", ".__ParamSet__has_deps"
  };
  for (R_xlen_t index = 0; index < 4; ++index) {
    if (!canonical_active(
        state,
        inherited,
        R_UnboundValue,
        active_names[index],
        active_targets[index],
        NULL,
        NULL,
        0
      )) {
      return FALSE;
    }
  }
  if (!canonical_active(
      state,
      inherited,
      R_UnboundValue,
      "values",
      ".__ParamSet__values",
      values_names,
      values_defaults,
      1
    )) {
    return FALSE;
  }

  static const char *const sets_names[] = {"v"};
  static const formal_default_t sets_defaults[] = {FORMAL_MISSING};
  return !state->is_collection || canonical_active(
    state,
    state->enclosure,
    state->super_object,
    "sets",
    ".__ParamSetCollection__sets",
    sets_names,
    sets_defaults,
    1
  );
}

static int authenticate_design_dependencies(surface_state_t *state) {
  static const char *const ids_names[] = {"class", "tags", "any_tags"};
  static const formal_default_t ids_defaults[] = {
    FORMAL_NULL, FORMAL_NULL, FORMAL_NULL
  };
  static const char *const value_name[] = {"v"};
  static const formal_default_t value_default[] = {FORMAL_MISSING};
  if (state->is_collection ||
      !direct_private_binding(state, ".params") ||
      !direct_private_binding(state, ".deps") || !canonical_method(
        state,
        state->self,
        state->enclosure,
        R_UnboundValue,
        "ids",
        ".__ParamSet__ids",
        ids_names,
        ids_defaults,
        3
      ) || !canonical_active(
        state,
        state->enclosure,
        R_UnboundValue,
        "deps",
        ".__ParamSet__deps",
        value_name,
        value_default,
        1
      )) {
    return FALSE;
  }

  static const char *const active_names[] = {
    "storage_type", "has_deps"
  };
  static const char *const active_targets[] = {
    ".__ParamSet__storage_type", ".__ParamSet__has_deps"
  };
  for (R_xlen_t index = 0; index < 2; ++index) {
    if (!canonical_active(
        state,
        state->enclosure,
        R_UnboundValue,
        active_names[index],
        active_targets[index],
        NULL,
        NULL,
        0
      )) {
      return FALSE;
    }
  }
  return TRUE;
}

int paradox_param_set_design_dependencies_auth(SEXP self) {
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, SURFACE_ROOT_COUNT));
  surface_state_t state;
  const int authenticated = load_state(self, &state, roots) &&
    authenticate_design_dependencies(&state);
  UNPROTECT(1);
  return authenticated;
}

int paradox_param_set_design_trafo_auth(SEXP self) {
  /* Allocate the root plan before inspecting the R6 object.  Callers use this
   * internal integer result at their final admission boundary, avoiding the
   * ScalarLogical allocation required by the registered diagnostic wrapper. */
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, SURFACE_ROOT_COUNT));
  surface_state_t state;
  const int authenticated = load_state(self, &state, roots) &&
    authenticate_design_trafo(&state);
  UNPROTECT(1);
  return authenticated;
}

int paradox_param_set_collection_check_auth(SEXP self) {
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, SURFACE_ROOT_COUNT));
  surface_state_t state;
  const int authenticated = load_state(self, &state, roots) &&
    state.is_collection && authenticate_check(&state, FALSE, TRUE);
  UNPROTECT(1);
  return authenticated;
}

int paradox_param_set_random_design_auth(SEXP self) {
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, SURFACE_ROOT_COUNT));
  surface_state_t state;
  const int authenticated = load_state(self, &state, roots) &&
    authenticate_random_design(&state);
  UNPROTECT(1);
  return authenticated;
}

SEXP paradox_param_set_surface_auth(SEXP self, SEXP mode) {
  if (TYPEOF(mode) != INTSXP || ALTREP(mode) || XLENGTH(mode) != 1 ||
      !paradox_api_has_no_attributes(mode)) {
    return Rf_ScalarLogical(FALSE);
  }
  const int requested = INTEGER_ELT(mode, 0);
  if (requested < SURFACE_DESIGN_TRAFO ||
      requested > SURFACE_RANDOM_DESIGN) {
    return Rf_ScalarLogical(FALSE);
  }

  SEXP roots = PROTECT(Rf_allocVector(VECSXP, SURFACE_ROOT_COUNT));
  surface_state_t state;
  if (!load_state(self, &state, roots)) {
    UNPROTECT(1);
    return Rf_ScalarLogical(FALSE);
  }

  int authenticated = FALSE;
  switch ((surface_mode_t) requested) {
    case SURFACE_DESIGN_TRAFO:
      authenticated = authenticate_design_trafo(&state);
      break;
    case SURFACE_CHECK:
      authenticated = authenticate_check(&state, FALSE, FALSE);
      break;
    case SURFACE_CHECK_DT:
      authenticated = authenticate_check(&state, TRUE, FALSE);
      break;
    case SURFACE_RANDOM_DESIGN:
      authenticated = authenticate_random_design(&state);
      break;
    default:
      authenticated = FALSE;
      break;
  }
  SEXP result = PROTECT(Rf_ScalarLogical(authenticated));
  UNPROTECT(2);
  return result;
}
