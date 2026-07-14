#include <math.h>
#include <stddef.h>
#include <string.h>

#include "paradox.h"

#include "paramset_domain_common.h"
#include "paramset_params_internal.h"
#include "r_api_compat.h"
#include "r_utils.h"

typedef struct {
  SEXPTYPE type;
  const int *integer_values;
  const double *real_values;
} match_vector_t;

typedef enum {
  DOMAINS_NODE_UNSUPPORTED = 0,
  DOMAINS_NODE_PARAM_SET,
  DOMAINS_NODE_COLLECTION
} domains_node_kind_t;

enum domains_state_slot {
  DOMAINS_STATE_GRAPH = 0,
  DOMAINS_STATE_VALUES_BINDING,
  DOMAINS_STATE_DEPS_BINDING,
  DOMAINS_STATE_PARAMS,
  DOMAINS_STATE_RESULT_NAMES,
  DOMAINS_STATE_VALUES,
  DOMAINS_STATE_DEPS,
  DOMAINS_STATE_TAGS,
  DOMAINS_STATE_TRAFOS,
  DOMAINS_STATE_SIZE
};

typedef struct {
  SEXP state;
  SEXP graph;
  R_xlen_t size;
  R_xlen_t capacity;
} domains_plan_t;

typedef struct {
  domains_node_kind_t kind;
  SEXP private_environment;
  SEXP params;
  SEXP sets;
  R_xlen_t set_count;
  SEXP values_binding;
  SEXP deps_binding;
} domains_node_t;

typedef struct {
  SEXP self;
  SEXP sets;
  R_xlen_t child_count;
  R_xlen_t next_child;
  domains_node_kind_t kind;
  int entered;
} domains_frame_t;

typedef struct {
  SEXP private_environment;
  SEXP params;
  SEXP values_binding;
  SEXP deps_binding;
} domains_preflight_t;

typedef enum {
  DOMAINS_FORMAL_MISSING = 0,
  DOMAINS_FORMAL_NULL
} domains_formal_default_t;

static domains_node_kind_t node_kind(SEXP self,
    R_xlen_t *work_since_interrupt) {
  static const char *const param_set_classes[] = {"ParamSet", "R6"};
  static const char *const collection_classes[] = {
    "ParamSetCollection", "ParamSet", "R6"
  };
  if (TYPEOF(self) != ENVSXP) {
    return DOMAINS_NODE_UNSUPPORTED;
  }
  SEXP classes = Rf_getAttrib(self, R_ClassSymbol);
  if (paradox_domain_exact_string_vector(
      classes,
      param_set_classes,
      2,
      work_since_interrupt
    )) {
    return DOMAINS_NODE_PARAM_SET;
  }
  if (paradox_domain_exact_string_vector(
      classes,
      collection_classes,
      3,
      work_since_interrupt
    )) {
    return DOMAINS_NODE_COLLECTION;
  }
  return DOMAINS_NODE_UNSUPPORTED;
}

static int names_only_attribute(SEXP value) {
  return paradox_api_has_single_attribute(value, "names");
}

static void plan_initialize(domains_plan_t *plan, SEXP state) {
  const R_xlen_t initial_capacity = 32;
  SEXP graph = PROTECT(Rf_allocVector(VECSXP, initial_capacity));
  SET_VECTOR_ELT(state, DOMAINS_STATE_GRAPH, graph);
  plan->state = state;
  plan->graph = graph;
  plan->size = 0;
  plan->capacity = initial_capacity;
  UNPROTECT(1);
}

static void plan_add(domains_plan_t *plan, SEXP value) {
  PROTECT(value);
  if (plan->size == plan->capacity) {
    if (plan->capacity > R_XLEN_T_MAX / 2) {
      UNPROTECT(1);
      Rf_error("Unable to allocate exact ParamSetCollection domains plan");
    }
    const R_xlen_t new_capacity = plan->capacity * 2;
    SEXP grown = PROTECT(Rf_allocVector(VECSXP, new_capacity));
    for (R_xlen_t index = 0; index < plan->size; ++index) {
      SET_VECTOR_ELT(grown, index, VECTOR_ELT(plan->graph, index));
    }
    SET_VECTOR_ELT(plan->state, DOMAINS_STATE_GRAPH, grown);
    plan->graph = grown;
    plan->capacity = new_capacity;
    UNPROTECT(1);
  }
  SET_VECTOR_ELT(plan->graph, plan->size, value);
  ++plan->size;
  UNPROTECT(1);
}

static int exact_formals(SEXP formals, const char *const *names,
    const domains_formal_default_t *defaults, R_xlen_t count) {
  SEXP current = formals;
  for (R_xlen_t index = 0; index < count; ++index) {
    if (TYPEOF(current) != LISTSXP ||
        TAG(current) != Rf_install(names[index])) {
      return FALSE;
    }
    SEXP value = CAR(current);
    if ((defaults[index] == DOMAINS_FORMAL_MISSING
          ? value != R_MissingArg
          : value != R_NilValue)) {
      return FALSE;
    }
    current = CDR(current);
  }
  return current == R_NilValue;
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

static int exact_r6_wrapper(SEXP function, SEXP self,
    SEXP private_environment, SEXP namespace_environment,
    SEXP expected_wrapper_environment, SEXP expected_super,
    const char *target,
    const char *const *formal_names,
    const domains_formal_default_t *formal_defaults,
    R_xlen_t formal_count) {
  if (TYPEOF(function) != CLOSXP) {
    return FALSE;
  }
  PROTECT(function);
  SEXP formals = PROTECT(paradox_api_closure_formals(function));
  SEXP body = PROTECT(paradox_api_closure_expression(function));
  SEXP environment = PROTECT(paradox_api_closure_environment(function));
  if (!exact_formals(
      formals,
      formal_names,
      formal_defaults,
      formal_count
    ) || !exact_forwarding_body(
      body,
      target,
      formal_names,
      formal_count
    )) {
    UNPROTECT(4);
    return FALSE;
  }
  SEXP target_symbol = Rf_install(target);
  SEXP super_symbol = Rf_install("super");
  const int expects_super = expected_super != R_UnboundValue;
  if (TYPEOF(environment) != ENVSXP ||
      environment != expected_wrapper_environment ||
      paradox_api_parent_environment(environment) != namespace_environment ||
      paradox_domain_local_value(environment, "self") != self ||
      paradox_domain_local_value(environment, "private") !=
        private_environment ||
      R_existsVarInFrame(environment, target_symbol) ||
      (!expects_super && R_existsVarInFrame(environment, super_symbol)) ||
      (expects_super &&
        (!R_existsVarInFrame(environment, super_symbol) ||
          paradox_domain_local_value(environment, "super") !=
            expected_super))) {
    UNPROTECT(4);
    return FALSE;
  }
  UNPROTECT(4);
  return TRUE;
}

static SEXP canonical_active_wrapper(SEXP self, SEXP private_environment,
    SEXP expected_wrapper_environment, const char *binding_name,
    const char *target, const char *formal_name,
    const char *super_target, R_xlen_t *work_since_interrupt) {
  /* Intern before authentication so no symbol-table allocation can separate
   * the successful check from the active-function snapshot. */
  SEXP symbol = Rf_install(binding_name);
  if (!paradox_params_canonical_active_member(
      self,
      private_environment,
      binding_name,
      target,
      formal_name,
      super_target,
      work_since_interrupt
  )) {
    return R_UnboundValue;
  }
  SEXP function = PROTECT(R_ActiveBindingFunction(symbol, self));
  const int exact = TYPEOF(function) == CLOSXP &&
    paradox_api_closure_environment(function) == expected_wrapper_environment;
  UNPROTECT(1);
  return exact ? function : R_UnboundValue;
}

static SEXP exact_locked_wrapper(SEXP environment, SEXP self,
    SEXP private_environment, SEXP namespace_environment,
    int require_locked, SEXP expected_wrapper_environment,
    SEXP expected_super, const char *binding_name,
    const char *target, const char *const *formal_names,
    const domains_formal_default_t *formal_defaults,
    R_xlen_t formal_count) {
  SEXP symbol = Rf_install(binding_name);
  if (!R_existsVarInFrame(environment, symbol) ||
      R_BindingIsActive(symbol, environment) ||
      (require_locked && !R_BindingIsLocked(symbol, environment))) {
    return R_UnboundValue;
  }
  SEXP function = PROTECT(paradox_domain_local_value(
    environment,
    binding_name
  ));
  const int exact = exact_r6_wrapper(
    function,
    self,
    private_environment,
    namespace_environment,
    expected_wrapper_environment,
    expected_super,
    target,
    formal_names,
    formal_defaults,
    formal_count
  );
  UNPROTECT(1);
  return exact ? function : R_UnboundValue;
}

static int valid_set_names(SEXP sets, R_xlen_t *set_count,
    R_xlen_t *work_since_interrupt) {
  /* `.sets` and its names are exact private storage.  Authenticate both
   * representations before an ALTREP Length/Elt method can run: declining the
   * fast path must be observationally inert. */
  if (TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isObject(sets) ||
      !names_only_attribute(sets)) {
    return FALSE;
  }
  const R_xlen_t observed_set_count = XLENGTH(sets);
  SEXP names = PROTECT(Rf_getAttrib(sets, R_NamesSymbol));
  if (TYPEOF(names) != STRSXP || ALTREP(names) ||
      !paradox_api_has_no_attributes(names)) {
    UNPROTECT(1);
    return FALSE;
  }
  const R_xlen_t name_count = XLENGTH(names);
  if (name_count != observed_set_count) {
    UNPROTECT(1);
    return FALSE;
  }
  for (R_xlen_t left = 0; left < name_count; ++left) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP name = STRING_ELT(names, left);
    if (name == NA_STRING) {
      UNPROTECT(1);
      return FALSE;
    }
    if (CHAR(name)[0] == '\0') {
      continue;
    }
    for (R_xlen_t right = 0; right < left; ++right) {
      paradox_domain_account_work(work_since_interrupt);
      if (paradox_domain_strings_equal(name, STRING_ELT(names, right))) {
        UNPROTECT(1);
        return FALSE;
      }
    }
  }
  *set_count = observed_set_count;
  UNPROTECT(1);
  return TRUE;
}

static int inspect_node(SEXP self, domains_plan_t *plan,
    SEXP namespace_environment, int is_root, domains_node_t *result,
    R_xlen_t *work_since_interrupt) {
  paradox_domain_account_work(work_since_interrupt);
  const domains_node_kind_t kind = node_kind(
    self,
    work_since_interrupt
  );
  if (kind == DOMAINS_NODE_UNSUPPORTED) {
    return FALSE;
  }

  plan_add(plan, self);
  SEXP enclosure = paradox_domain_local_value(self, ".__enclos_env__");
  if (TYPEOF(enclosure) != ENVSXP) {
    return FALSE;
  }
  plan_add(plan, enclosure);
  SEXP private_environment = paradox_domain_local_value(
    enclosure,
    "private"
  );
  if (TYPEOF(private_environment) != ENVSXP) {
    return FALSE;
  }
  plan_add(plan, private_environment);
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    return FALSE;
  }

  SEXP super_proxy = R_UnboundValue;
  SEXP super_enclosure = R_UnboundValue;
  if (kind == DOMAINS_NODE_COLLECTION) {
    SEXP super_symbol = Rf_install("super");
    if (!R_existsVarInFrame(enclosure, super_symbol)) {
      return FALSE;
    }
    super_proxy = paradox_domain_local_value(enclosure, "super");
    if (TYPEOF(super_proxy) != ENVSXP) {
      return FALSE;
    }
    plan_add(plan, super_proxy);
    super_enclosure = paradox_domain_local_value(
      super_proxy,
      ".__enclos_env__"
    );
    if (TYPEOF(super_enclosure) != ENVSXP) {
      return FALSE;
    }
    plan_add(plan, super_enclosure);
  }

  SEXP values_binding = canonical_active_wrapper(
    self,
    private_environment,
    kind == DOMAINS_NODE_COLLECTION ? super_enclosure : enclosure,
    "values",
    ".__ParamSet__values",
    "xs",
    NULL,
    work_since_interrupt
  );
  if (values_binding == R_UnboundValue) {
    return FALSE;
  }
  plan_add(plan, values_binding);
  SEXP deps_binding = canonical_active_wrapper(
    self,
    private_environment,
    enclosure,
    "deps",
    kind == DOMAINS_NODE_COLLECTION
      ? ".__ParamSetCollection__deps"
      : ".__ParamSet__deps",
    "v",
    kind == DOMAINS_NODE_COLLECTION ? ".__ParamSet__deps" : NULL,
    work_since_interrupt
  );
  if (deps_binding == R_UnboundValue) {
    return FALSE;
  }
  plan_add(plan, deps_binding);
  static const char *const ids_formals[] = {
    "class", "tags", "any_tags"
  };
  static const domains_formal_default_t ids_defaults[] = {
    DOMAINS_FORMAL_NULL, DOMAINS_FORMAL_NULL, DOMAINS_FORMAL_NULL
  };
  SEXP ids_method = exact_locked_wrapper(
    self,
    self,
    private_environment,
    namespace_environment,
    TRUE,
    kind == DOMAINS_NODE_COLLECTION ? super_enclosure : enclosure,
    R_UnboundValue,
    "ids",
    ".__ParamSet__ids",
    ids_formals,
    ids_defaults,
    3
  );
  if (ids_method == R_UnboundValue) {
    return FALSE;
  }
  plan_add(plan, ids_method);
  if (kind == DOMAINS_NODE_COLLECTION) {
    SEXP super_ids_method = PROTECT(exact_locked_wrapper(
      super_proxy,
      self,
      private_environment,
      namespace_environment,
      FALSE,
      super_enclosure,
      R_UnboundValue,
      "ids",
      ".__ParamSet__ids",
      ids_formals,
      ids_defaults,
      3
    ));
    if (super_ids_method == R_UnboundValue || !R_compute_identical(
        ids_method,
        super_ids_method,
        IDENT_USE_CLOENV
      )) {
      UNPROTECT(1);
      return FALSE;
    }
    plan_add(plan, super_ids_method);
    UNPROTECT(1);
  }

  if (is_root) {
    static const char *const get_domain_formals[] = {"id"};
    static const domains_formal_default_t get_domain_defaults[] = {
      DOMAINS_FORMAL_MISSING
    };
    SEXP get_domain_method = exact_locked_wrapper(
      self,
      self,
      private_environment,
      namespace_environment,
      TRUE,
      kind == DOMAINS_NODE_COLLECTION ? super_enclosure : enclosure,
      R_UnboundValue,
      "get_domain",
      ".__ParamSet__get_domain",
      get_domain_formals,
      get_domain_defaults,
      1
    );
    if (get_domain_method == R_UnboundValue) {
      return FALSE;
    }
    plan_add(plan, get_domain_method);
    if (kind == DOMAINS_NODE_COLLECTION) {
      SEXP super_get_domain_method = PROTECT(exact_locked_wrapper(
        super_proxy,
        self,
        private_environment,
        namespace_environment,
        FALSE,
        super_enclosure,
        R_UnboundValue,
        "get_domain",
        ".__ParamSet__get_domain",
        get_domain_formals,
        get_domain_defaults,
        1
      ));
      if (super_get_domain_method == R_UnboundValue || !R_compute_identical(
          get_domain_method,
          super_get_domain_method,
          IDENT_USE_CLOENV
        )) {
        UNPROTECT(1);
        return FALSE;
      }
      plan_add(plan, super_get_domain_method);
      UNPROTECT(1);
    }
  }
  const char *const no_formals[] = {NULL};
  const domains_formal_default_t no_defaults[] = {
    DOMAINS_FORMAL_MISSING
  };
  SEXP get_values_method = exact_locked_wrapper(
    private_environment,
    self,
    private_environment,
    namespace_environment,
    TRUE,
    enclosure,
    kind == DOMAINS_NODE_COLLECTION ? super_proxy : R_UnboundValue,
    ".get_values",
    kind == DOMAINS_NODE_COLLECTION
      ? ".__ParamSetCollection__.get_values"
      : ".__ParamSet__.get_values",
    no_formals,
    no_defaults,
    0
  );
  if (get_values_method == R_UnboundValue) {
    return FALSE;
  }
  plan_add(plan, get_values_method);

  if (kind == DOMAINS_NODE_COLLECTION) {
    SEXP super_get_values_method = exact_locked_wrapper(
      super_proxy,
      self,
      private_environment,
      namespace_environment,
      FALSE,
      super_enclosure,
      R_UnboundValue,
      ".get_values",
      ".__ParamSet__.get_values",
      no_formals,
      no_defaults,
      0
    );
    if (super_get_values_method == R_UnboundValue) {
      return FALSE;
    }
    plan_add(plan, super_get_values_method);
  }

  if (kind == DOMAINS_NODE_COLLECTION) {
    static const char *const prefix_formals[] = {"owner", "id"};
    static const domains_formal_default_t prefix_defaults[] = {
      DOMAINS_FORMAL_MISSING, DOMAINS_FORMAL_MISSING
    };
    SEXP prefix_method = exact_locked_wrapper(
      private_environment,
      self,
      private_environment,
      namespace_environment,
      TRUE,
      enclosure,
      super_proxy,
      ".add_name_prefix",
      ".__ParamSetCollection__.add_name_prefix",
      prefix_formals,
      prefix_defaults,
      2
    );
    if (prefix_method == R_UnboundValue) {
      return FALSE;
    }
    plan_add(plan, prefix_method);
  }

  SEXP params_sexp = paradox_domain_local_value(
    private_environment,
    ".params"
  );
  if (params_sexp == R_UnboundValue) {
    return FALSE;
  }
  plan_add(plan, params_sexp);
  SEXP tags_sexp = paradox_domain_local_value(
    private_environment,
    ".tags"
  );
  if (tags_sexp == R_UnboundValue) {
    return FALSE;
  }
  plan_add(plan, tags_sexp);
  SEXP trafos_sexp = paradox_domain_local_value(
    private_environment,
    ".trafos"
  );
  if (trafos_sexp == R_UnboundValue) {
    return FALSE;
  }
  plan_add(plan, trafos_sexp);
  SEXP dependencies_sexp = paradox_domain_local_value(
    private_environment,
    ".deps"
  );
  if (dependencies_sexp == R_UnboundValue) {
    return FALSE;
  }
  plan_add(plan, dependencies_sexp);
  SEXP values_sexp = paradox_domain_local_value(
    private_environment,
    ".values"
  );
  if (values_sexp == R_UnboundValue) {
    return FALSE;
  }
  plan_add(plan, values_sexp);

  paradox_domain_params_t params;
  paradox_domain_tags_t tags;
  paradox_domain_trafos_t trafos;
  paradox_domain_dependencies_t dependencies;
  paradox_domain_values_t values;
  R_xlen_t unused_row = 0;
  if (!paradox_domain_validate_params(
        params_sexp,
        R_NilValue,
        TRUE,
        &params,
        &unused_row,
        work_since_interrupt
      ) || !paradox_domain_validate_tags(
        tags_sexp,
        &tags,
        work_since_interrupt
      ) || !paradox_domain_validate_trafos(
        trafos_sexp,
        &trafos,
        work_since_interrupt
      ) || !paradox_domain_validate_dependencies(
        dependencies_sexp,
        &dependencies,
        work_since_interrupt
      ) || !paradox_domain_validate_values(
        values_sexp,
        &values,
        work_since_interrupt
      )) {
    return FALSE;
  }

  SEXP sets = R_NilValue;
  R_xlen_t set_count = 0;
  if (kind == DOMAINS_NODE_COLLECTION) {
    sets = paradox_domain_local_value(private_environment, ".sets");
    if (sets == R_UnboundValue) {
      return FALSE;
    }
    plan_add(plan, sets);
    SEXP postfix = paradox_domain_local_value(
      private_environment,
      ".postfix"
    );
    if (postfix == R_UnboundValue) {
      return FALSE;
    }
    plan_add(plan, postfix);
    if (!valid_set_names(sets, &set_count, work_since_interrupt) ||
        TYPEOF(postfix) != LGLSXP || ALTREP(postfix) ||
        !paradox_api_has_no_attributes(postfix)) {
      return FALSE;
    }
    const R_xlen_t postfix_size = XLENGTH(postfix);
    if (postfix_size != 1 || LOGICAL_ELT(postfix, 0) == NA_LOGICAL) {
      return FALSE;
    }
  }

  result->kind = kind;
  result->private_environment = private_environment;
  result->params = params_sexp;
  result->sets = sets;
  result->set_count = set_count;
  result->values_binding = values_binding;
  result->deps_binding = deps_binding;
  return TRUE;
}

static int on_path(SEXP self, const domains_frame_t *frames,
    R_xlen_t depth) {
  for (R_xlen_t index = 0; index < depth; ++index) {
    if (frames[index].self == self) {
      return TRUE;
    }
  }
  return FALSE;
}

static int preflight_graph(SEXP self, domains_plan_t *plan,
    domains_preflight_t *result, R_xlen_t *work_since_interrupt) {
  SEXP namespace_environment = paradox_api_registered_namespace("paradox");
  if (TYPEOF(namespace_environment) != ENVSXP) {
    return FALSE;
  }
  plan_add(plan, namespace_environment);

  R_xlen_t capacity = 16;
  domains_frame_t *frames = paradox_temporary_alloc(
    capacity,
    sizeof(*frames)
  );
  R_xlen_t depth = 1;
  frames[0] = (domains_frame_t) {
    self,
    R_NilValue,
    0,
    0,
    DOMAINS_NODE_UNSUPPORTED,
    FALSE
  };

  while (depth != 0) {
    domains_frame_t *frame = &frames[depth - 1];
    if (!frame->entered) {
      domains_node_t node;
      if (!inspect_node(
          frame->self,
          plan,
          namespace_environment,
          depth == 1,
          &node,
          work_since_interrupt
        )) {
        return FALSE;
      }
      frame->kind = node.kind;
      frame->sets = node.sets;
      frame->child_count = node.set_count;
      frame->next_child = 0;
      frame->entered = TRUE;
      if (depth == 1) {
        result->private_environment = node.private_environment;
        result->params = node.params;
        result->values_binding = node.values_binding;
        result->deps_binding = node.deps_binding;
      }
    }

    if (frame->kind != DOMAINS_NODE_COLLECTION ||
        frame->next_child == frame->child_count) {
      --depth;
      continue;
    }

    paradox_domain_account_work(work_since_interrupt);
    SEXP child = VECTOR_ELT(frame->sets, frame->next_child);
    ++frame->next_child;
    /* A subsequent frame-stack growth uses R_alloc() and can collect.  Root
     * the selected child independently before that allocation, even if a GC
     * finalizer removes it from the parent set in the meantime. */
    plan_add(plan, child);
    if (on_path(child, frames, depth)) {
      Rf_error("ParamSetCollection nesting contains a cycle");
    }
    if (depth == capacity) {
      if (capacity > R_XLEN_T_MAX / 2) {
        Rf_error("Unable to allocate exact ParamSetCollection domains plan");
      }
      const R_xlen_t new_capacity = capacity * 2;
      domains_frame_t *grown = paradox_temporary_alloc(
        new_capacity,
        sizeof(*grown)
      );
      memcpy(grown, frames, (size_t) depth * sizeof(*grown));
      frames = grown;
      capacity = new_capacity;
    }
    frames[depth] = (domains_frame_t) {
      child,
      R_NilValue,
      0,
      0,
      DOMAINS_NODE_UNSUPPORTED,
      FALSE
    };
    ++depth;
  }
  return TRUE;
}

static int preflight_base(SEXP self, SEXP private_environment,
    R_xlen_t *work_since_interrupt) {
  SEXP state = PROTECT(Rf_allocVector(VECSXP, DOMAINS_STATE_SIZE));
  domains_plan_t plan;
  plan_initialize(&plan, state);
  SEXP namespace_environment = paradox_api_registered_namespace("paradox");
  if (TYPEOF(namespace_environment) != ENVSXP) {
    UNPROTECT(1);
    return FALSE;
  }
  plan_add(&plan, namespace_environment);

  domains_node_t node;
  const int exact = inspect_node(
    self,
    &plan,
    namespace_environment,
    TRUE,
    &node,
    work_since_interrupt
  ) && node.kind == DOMAINS_NODE_PARAM_SET &&
    node.private_environment == private_environment;
  UNPROTECT(1);
  return exact;
}

static int snapshot_vector_supported(SEXP source, R_xlen_t *size) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
  if (ALTREP(source) || (type != STRSXP && type != VECSXP &&
      type != REALSXP && type != INTSXP && type != LGLSXP)) {
    return FALSE;
  }
  *size = XLENGTH(source);
  return TRUE;
}

static void snapshot_vector_contents(SEXP result, SEXP source,
    R_xlen_t size, R_xlen_t *work_since_interrupt) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    switch (type) {
    case STRSXP:
      SET_STRING_ELT(result, index, STRING_ELT(source, index));
      break;
    case VECSXP:
      SET_VECTOR_ELT(result, index, VECTOR_ELT(source, index));
      break;
    case REALSXP:
      SET_REAL_ELT(result, index, REAL_ELT(source, index));
      break;
    case INTSXP:
      SET_INTEGER_ELT(result, index, INTEGER_ELT(source, index));
      break;
    case LGLSXP:
      SET_LOGICAL_ELT(result, index, LOGICAL_ELT(source, index));
      break;
    default:
      Rf_error("Unsupported exact ParamSetCollection snapshot type");
    }
  }
}

static SEXP snapshot_values(SEXP source,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(source) != VECSXP || ALTREP(source) || Rf_isObject(source)) {
    return R_NilValue;
  }
  PROTECT(source);

  /* Allocate the root carrier before selecting the names attribute.  A GC
   * finalizer may mutate the public callback result during that allocation;
   * the snapshot starts only after the carrier exists. */
  SEXP inputs = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(inputs, 0, source);
  SEXP names_source = Rf_getAttrib(source, R_NamesSymbol);
  SET_VECTOR_ELT(inputs, 1, names_source);

  R_xlen_t value_count = 0;
  R_xlen_t name_count = 0;
  if (!snapshot_vector_supported(source, &value_count) ||
      TYPEOF(names_source) != STRSXP ||
      !snapshot_vector_supported(names_source, &name_count) ||
      !paradox_api_has_no_attributes(names_source) ||
      name_count != value_count) {
    UNPROTECT(2);
    return R_NilValue;
  }

  /* Allocate and root every destination before reading any input element.
   * Consequently a finalizer run by allocation can change which state is
   * copied, but can neither mix shell children nor invalidate a bare SEXP. */
  SEXP outputs = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP result = PROTECT(Rf_allocVector(VECSXP, value_count));
  SET_VECTOR_ELT(outputs, 0, result);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, name_count));
  SET_VECTOR_ELT(outputs, 1, names);
  UNPROTECT(1);

  snapshot_vector_contents(
    VECTOR_ELT(outputs, 0),
    VECTOR_ELT(inputs, 0),
    value_count,
    work_since_interrupt
  );
  snapshot_vector_contents(
    VECTOR_ELT(outputs, 1),
    VECTOR_ELT(inputs, 1),
    name_count,
    work_since_interrupt
  );
  Rf_setAttrib(result, R_NamesSymbol, VECTOR_ELT(outputs, 1));
  UNPROTECT(4);
  return result;
}

static SEXP snapshot_table(SEXP source, R_xlen_t column_count,
    int include_sorted, R_xlen_t *work_since_interrupt) {
  if (column_count < 0 || column_count > PARADOX_DOMAIN_TAGS ||
      TYPEOF(source) != VECSXP || ALTREP(source)) {
    return R_NilValue;
  }
  PROTECT(source);
  const R_xlen_t observed_column_count = XLENGTH(source);
  if (observed_column_count != column_count) {
    UNPROTECT(1);
    return R_NilValue;
  }

  SEXP sorted_symbol = Rf_install("sorted");
  const R_xlen_t attribute_count = include_sorted ? 3 : 2;
  const R_xlen_t root_count = column_count + attribute_count;
  SEXP inputs = PROTECT(Rf_allocVector(VECSXP, root_count));

  /* Selection is deliberately allocation-free.  The rooted carrier freezes
   * the exact columns and structural attributes even if later allocations run
   * a finalizer which replaces them in the public source shell. */
  for (R_xlen_t column = 0; column < column_count; ++column) {
    SET_VECTOR_ELT(inputs, column, VECTOR_ELT(source, column));
  }
  SET_VECTOR_ELT(
    inputs,
    column_count,
    Rf_getAttrib(source, R_NamesSymbol)
  );
  SET_VECTOR_ELT(
    inputs,
    column_count + 1,
    Rf_getAttrib(source, R_ClassSymbol)
  );
  if (include_sorted) {
    SET_VECTOR_ELT(
      inputs,
      column_count + 2,
      Rf_getAttrib(source, sorted_symbol)
    );
  }

  R_xlen_t sizes[PARADOX_DOMAIN_TAGS + 3];
  for (R_xlen_t index = 0; index < root_count; ++index) {
    SEXP input = VECTOR_ELT(inputs, index);
    if (!snapshot_vector_supported(input, &sizes[index]) ||
        !paradox_api_has_no_attributes(input)) {
      UNPROTECT(2);
      return R_NilValue;
    }
  }
  if (TYPEOF(VECTOR_ELT(inputs, column_count)) != STRSXP ||
      sizes[column_count] != column_count ||
      TYPEOF(VECTOR_ELT(inputs, column_count + 1)) != STRSXP ||
      (include_sorted &&
        TYPEOF(VECTOR_ELT(inputs, column_count + 2)) != STRSXP)) {
    UNPROTECT(2);
    return R_NilValue;
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, column_count));
  SEXP outputs = PROTECT(Rf_allocVector(VECSXP, root_count));
  for (R_xlen_t index = 0; index < root_count; ++index) {
    SEXP input = VECTOR_ELT(inputs, index);
    SEXP output = PROTECT(Rf_allocVector(
      (SEXPTYPE) TYPEOF(input),
      sizes[index]
    ));
    SET_VECTOR_ELT(outputs, index, output);
    if (index < column_count) {
      SET_VECTOR_ELT(result, index, output);
    }
    UNPROTECT(1);
  }
  /* No allocation occurs while any selected input element is still unread. */
  for (R_xlen_t index = 0; index < root_count; ++index) {
    snapshot_vector_contents(
      VECTOR_ELT(outputs, index),
      VECTOR_ELT(inputs, index),
      sizes[index],
      work_since_interrupt
    );
  }
  Rf_setAttrib(
    result,
    R_NamesSymbol,
    VECTOR_ELT(outputs, column_count)
  );
  Rf_setAttrib(
    result,
    R_ClassSymbol,
    VECTOR_ELT(outputs, column_count + 1)
  );
  if (include_sorted) {
    Rf_setAttrib(
      result,
      sorted_symbol,
      VECTOR_ELT(outputs, column_count + 2)
    );
  }
  UNPROTECT(4);
  return result;
}

static void evaluate_binding_into(SEXP state, R_xlen_t slot,
    SEXP function) {
  SEXP call = PROTECT(Rf_lang1(function));
  SEXP value = PROTECT(Rf_eval(call, R_BaseEnv));
  SET_VECTOR_ELT(state, slot, value);
  UNPROTECT(2);
}

static void evaluate_current_active_binding_into(SEXP state,
    R_xlen_t binding_slot, R_xlen_t value_slot, SEXP self,
    const char *binding_name) {
  SEXP symbol = Rf_install(binding_name);
  if (!R_existsVarInFrame(self, symbol) ||
      !R_BindingIsActive(symbol, self)) {
    Rf_error(
      "Corrupt exact ParamSetCollection state after values callback: "
      "missing active %s binding",
      binding_name
    );
  }
  SEXP function = PROTECT(R_ActiveBindingFunction(symbol, self));
  if (!Rf_isFunction(function)) {
    UNPROTECT(1);
    Rf_error(
      "Corrupt exact ParamSetCollection state after values callback: "
      "invalid active %s binding",
      binding_name
    );
  }
  SET_VECTOR_ELT(state, binding_slot, function);
  evaluate_binding_into(state, value_slot, function);
  UNPROTECT(1);
}

static void require_current_private_owner(SEXP self,
    SEXP private_environment) {
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error(
      "Corrupt exact ParamSetCollection state after callbacks: "
      "private environment changed"
    );
  }
}

static match_vector_t match_vector(SEXP value, R_xlen_t expected_size) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  if ((type != INTSXP && type != REALSXP) || ALTREP(value) ||
      XLENGTH(value) != expected_size) {
    Rf_error("Internal error: unexpected result from R's matching primitive");
  }
  const match_vector_t result = {
    type,
    type == INTSXP ? INTEGER_RO(value) : NULL,
    type == REALSXP ? REAL_RO(value) : NULL
  };
  return result;
}

static R_xlen_t match_at(const match_vector_t *matches, R_xlen_t index) {
  if (matches->type == INTSXP) {
    const int value = matches->integer_values[index];
    return value == NA_INTEGER || value <= 0 ? 0 : (R_xlen_t) value;
  }
  const double value = matches->real_values[index];
  return ISNAN(value) || value <= 0.0 ? 0 : (R_xlen_t) value;
}

static void group_rows(const match_vector_t *owners, R_xlen_t input_size,
    R_xlen_t output_size, R_xlen_t *offsets, R_xlen_t *order,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t row = 0; row <= output_size; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    offsets[row] = 0;
  }
  for (R_xlen_t input = 0; input < input_size; ++input) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t owner = match_at(owners, input);
    if (owner > output_size) {
      Rf_error("Internal error: invalid result from R's matching primitive");
    }
    if (owner != 0) {
      ++offsets[owner];
    }
  }
  for (R_xlen_t row = 1; row <= output_size; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    offsets[row] += offsets[row - 1];
  }

  R_xlen_t *cursor = paradox_temporary_alloc(
    output_size,
    sizeof(*cursor)
  );
  for (R_xlen_t row = 0; row < output_size; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    cursor[row] = offsets[row];
  }
  for (R_xlen_t input = 0; input < input_size; ++input) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t owner = match_at(owners, input);
    if (owner > output_size) {
      Rf_error("Internal error: invalid result from R's matching primitive");
    }
    if (owner != 0) {
      order[cursor[owner - 1]] = input;
      ++cursor[owner - 1];
    }
  }
}

static int index_unique_rows(const match_vector_t *owners,
    R_xlen_t input_size, R_xlen_t output_size, R_xlen_t *index,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t row = 0; row < output_size; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    index[row] = 0;
  }
  for (R_xlen_t input = 0; input < input_size; ++input) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t owner = match_at(owners, input);
    if (owner > output_size) {
      Rf_error("Internal error: invalid result from R's matching primitive");
    }
    if (owner == 0) {
      continue;
    }
    if (index[owner - 1] != 0) {
      return FALSE;
    }
    index[owner - 1] = input + 1;
  }
  return TRUE;
}

static SEXP assemble_domains(const paradox_domain_params_t *params,
    const paradox_domain_tags_t *tags,
    const paradox_domain_trafos_t *trafos,
    const paradox_domain_dependencies_t *dependencies,
    const paradox_domain_values_t *values, SEXP result_names,
    int can_decline, R_xlen_t *work_since_interrupt) {
  /* Rf_match(table, x, nomatch) maps every auxiliary owner to its one-based
   * parameter position with R's long-vector and encoding semantics. */
  SEXP tag_match_sexp = PROTECT(Rf_match(params->ids, tags->ids, 0));
  SEXP trafo_match_sexp = PROTECT(Rf_match(params->ids, trafos->ids, 0));
  SEXP dependency_match_sexp = PROTECT(Rf_match(
    params->ids,
    dependencies->ids,
    0
  ));
  SEXP value_match_sexp = PROTECT(Rf_match(
    params->ids,
    values->names,
    0
  ));
  const match_vector_t tag_matches = match_vector(
    tag_match_sexp,
    tags->row_count
  );
  const match_vector_t trafo_matches = match_vector(
    trafo_match_sexp,
    trafos->row_count
  );
  const match_vector_t dependency_matches = match_vector(
    dependency_match_sexp,
    dependencies->row_count
  );
  const match_vector_t value_matches = match_vector(
    value_match_sexp,
    values->size
  );

  if (params->row_count == R_XLEN_T_MAX) {
    UNPROTECT(4);
    Rf_error("Unable to allocate temporary native workspace");
  }
  R_xlen_t *tag_offsets = paradox_temporary_alloc(
    params->row_count + 1,
    sizeof(*tag_offsets)
  );
  R_xlen_t *tag_order = paradox_temporary_alloc(
    tags->row_count,
    sizeof(*tag_order)
  );
  R_xlen_t *dependency_offsets = paradox_temporary_alloc(
    params->row_count + 1,
    sizeof(*dependency_offsets)
  );
  R_xlen_t *dependency_order = paradox_temporary_alloc(
    dependencies->row_count,
    sizeof(*dependency_order)
  );
  R_xlen_t *trafo_index = paradox_temporary_alloc(
    params->row_count,
    sizeof(*trafo_index)
  );
  R_xlen_t *value_index = paradox_temporary_alloc(
    params->row_count,
    sizeof(*value_index)
  );

  group_rows(
    &tag_matches,
    tags->row_count,
    params->row_count,
    tag_offsets,
    tag_order,
    work_since_interrupt
  );
  group_rows(
    &dependency_matches,
    dependencies->row_count,
    params->row_count,
    dependency_offsets,
    dependency_order,
    work_since_interrupt
  );
  if (!index_unique_rows(
      &trafo_matches,
      trafos->row_count,
      params->row_count,
      trafo_index,
      work_since_interrupt
    )) {
    UNPROTECT(4);
    if (can_decline) {
      return R_NilValue;
    }
    Rf_error(
      "Corrupt exact ParamSetCollection state after callbacks: duplicate trafos"
    );
  }
  if (!index_unique_rows(
      &value_matches,
      values->size,
      params->row_count,
      value_index,
      work_since_interrupt
    )) {
    UNPROTECT(4);
    if (can_decline) {
      return R_NilValue;
    }
    Rf_error(
      "Corrupt exact ParamSetCollection state after callbacks: duplicate values"
    );
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, params->row_count));
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  for (R_xlen_t row = 0; row < params->row_count; ++row) {
    const R_xlen_t first_tag = tag_offsets[row];
    const R_xlen_t tag_count = tag_offsets[row + 1] - first_tag;
    const R_xlen_t first_dependency = dependency_offsets[row];
    const R_xlen_t dependency_count =
      dependency_offsets[row + 1] - first_dependency;
    const R_xlen_t selected_trafo = trafo_index[row];
    const R_xlen_t selected_value = value_index[row];
    const paradox_domain_row_t domain_row = {
      params,
      row,
      tags,
      tag_count == 0 ? NULL : tag_order + first_tag,
      tag_count,
      selected_trafo == 0
        ? R_NilValue
        : VECTOR_ELT(trafos->values, selected_trafo - 1),
      dependencies,
      dependency_count == 0
        ? NULL
        : dependency_order + first_dependency,
      dependency_count,
      selected_value != 0,
      selected_value == 0
        ? R_NilValue
        : VECTOR_ELT(values->values, selected_value - 1)
    };
    SEXP domain = PROTECT(Rf_allocVector(
      VECSXP,
      PARADOX_DOMAIN_COLUMN_COUNT
    ));
    paradox_domain_fill(domain, &domain_row, work_since_interrupt);
    SET_VECTOR_ELT(result, row, domain);
    UNPROTECT(1);
  }

  UNPROTECT(5);
  return result;
}

SEXP paradox_param_set_domains(
    SEXP private_environment,
    SEXP self) {
  R_xlen_t work_since_interrupt = 0;
  const domains_node_kind_t kind = node_kind(
    self,
    &work_since_interrupt
  );
  if (kind == DOMAINS_NODE_UNSUPPORTED ||
      !paradox_domain_owns_private_environment(self, private_environment)) {
    return R_NilValue;
  }

  SEXP params_sexp = paradox_domain_local_value(
    private_environment,
    ".params"
  );
  paradox_domain_params_t params;
  R_xlen_t unused_row = 0;
  if (params_sexp == R_UnboundValue) {
    return R_NilValue;
  }
  PROTECT(params_sexp);
  if (!paradox_domain_validate_params(
      params_sexp,
      R_NilValue,
      TRUE,
      &params,
      &unused_row,
      &work_since_interrupt
    )) {
    UNPROTECT(1);
    return R_NilValue;
  }

  /* NULL is the native fallback sentinel. The historical empty path is cheap
   * and preserves its explicitly named empty list without a second sentinel. */
  if (params.row_count == 0) {
    UNPROTECT(1);
    return R_NilValue;
  }

  if (kind == DOMAINS_NODE_PARAM_SET && !preflight_base(
      self,
      private_environment,
      &work_since_interrupt
    )) {
    UNPROTECT(1);
    return R_NilValue;
  }

  if (kind == DOMAINS_NODE_COLLECTION) {
    UNPROTECT(1);
    SEXP state = PROTECT(Rf_allocVector(VECSXP, DOMAINS_STATE_SIZE));
    domains_plan_t plan;
    plan_initialize(&plan, state);
    domains_preflight_t preflight = {
      R_NilValue,
      R_NilValue,
      R_NilValue,
      R_NilValue
    };
    if (!preflight_graph(
        self,
        &plan,
        &preflight,
        &work_since_interrupt
      ) || preflight.private_environment != private_environment) {
      UNPROTECT(1);
      return R_NilValue;
    }

    paradox_domain_params_t checked_params;
    if (!paradox_domain_validate_params(
        preflight.params,
        R_NilValue,
        TRUE,
        &checked_params,
        &unused_row,
        &work_since_interrupt
      )) {
      UNPROTECT(1);
      return R_NilValue;
    }
    if (checked_params.row_count == 0) {
      UNPROTECT(1);
      return R_NilValue;
    }

    SET_VECTOR_ELT(
      state,
      DOMAINS_STATE_VALUES_BINDING,
      preflight.values_binding
    );
    SET_VECTOR_ELT(
      state,
      DOMAINS_STATE_DEPS_BINDING,
      preflight.deps_binding
    );
    SEXP params_snapshot = PROTECT(snapshot_table(
      preflight.params,
      PARADOX_DOMAIN_TAGS,
      FALSE,
      &work_since_interrupt
    ));
    if (params_snapshot == R_NilValue) {
      UNPROTECT(2);
      return R_NilValue;
    }
    SET_VECTOR_ELT(state, DOMAINS_STATE_PARAMS, params_snapshot);
    UNPROTECT(1);
    if (!paradox_domain_validate_params(
        params_snapshot,
        R_NilValue,
        TRUE,
        &params,
        &unused_row,
        &work_since_interrupt
      )) {
      UNPROTECT(1);
      return R_NilValue;
    }
    SET_VECTOR_ELT(state, DOMAINS_STATE_RESULT_NAMES, params.ids);

    evaluate_binding_into(
      state,
      DOMAINS_STATE_VALUES,
      VECTOR_ELT(state, DOMAINS_STATE_VALUES_BINDING)
    );
    require_current_private_owner(self, private_environment);
    evaluate_current_active_binding_into(
      state,
      DOMAINS_STATE_DEPS_BINDING,
      DOMAINS_STATE_DEPS,
      self,
      "deps"
    );

    require_current_private_owner(self, private_environment);

    SEXP tags_source = paradox_domain_local_value(
      private_environment,
      ".tags"
    );
    if (tags_source == R_UnboundValue) {
      Rf_error(
        "Corrupt exact ParamSetCollection state after callbacks: missing tags"
      );
    }
    SET_VECTOR_ELT(state, DOMAINS_STATE_TAGS, tags_source);
    SEXP trafos_source = paradox_domain_local_value(
      private_environment,
      ".trafos"
    );
    if (trafos_source == R_UnboundValue) {
      Rf_error(
        "Corrupt exact ParamSetCollection state after callbacks: missing trafos"
      );
    }
    SET_VECTOR_ELT(state, DOMAINS_STATE_TRAFOS, trafos_source);

    SEXP values_source = VECTOR_ELT(state, DOMAINS_STATE_VALUES);
    SEXP dependencies_source = VECTOR_ELT(state, DOMAINS_STATE_DEPS);
    SEXP values_snapshot = PROTECT(snapshot_values(
      values_source,
      &work_since_interrupt
    ));
    if (values_snapshot == R_NilValue) {
      UNPROTECT(1);
      Rf_error(
        "Corrupt exact ParamSetCollection state after callbacks: values"
      );
    }
    SET_VECTOR_ELT(state, DOMAINS_STATE_VALUES, values_snapshot);
    UNPROTECT(1);
    SEXP dependencies_snapshot = PROTECT(snapshot_table(
      dependencies_source,
      3,
      FALSE,
      &work_since_interrupt
    ));
    if (dependencies_snapshot == R_NilValue) {
      UNPROTECT(1);
      Rf_error(
        "Corrupt exact ParamSetCollection state after callbacks: deps"
      );
    }
    SET_VECTOR_ELT(
      state,
      DOMAINS_STATE_DEPS,
      dependencies_snapshot
    );
    UNPROTECT(1);
    SEXP tags_snapshot = PROTECT(snapshot_table(
      tags_source,
      2,
      TRUE,
      &work_since_interrupt
    ));
    if (tags_snapshot == R_NilValue) {
      UNPROTECT(1);
      Rf_error(
        "Corrupt exact ParamSetCollection state after callbacks: tags"
      );
    }
    SET_VECTOR_ELT(state, DOMAINS_STATE_TAGS, tags_snapshot);
    UNPROTECT(1);
    SEXP trafos_snapshot = PROTECT(snapshot_table(
      trafos_source,
      2,
      TRUE,
      &work_since_interrupt
    ));
    if (trafos_snapshot == R_NilValue) {
      UNPROTECT(1);
      Rf_error(
        "Corrupt exact ParamSetCollection state after callbacks: trafos"
      );
    }
    SET_VECTOR_ELT(state, DOMAINS_STATE_TRAFOS, trafos_snapshot);
    UNPROTECT(1);

    paradox_domain_values_t values;
    paradox_domain_dependencies_t dependencies;
    paradox_domain_tags_t tags;
    paradox_domain_trafos_t trafos;
    if (!paradox_domain_validate_values(
          VECTOR_ELT(state, DOMAINS_STATE_VALUES),
          &values,
          &work_since_interrupt
        ) || !paradox_domain_validate_dependencies(
          VECTOR_ELT(state, DOMAINS_STATE_DEPS),
          &dependencies,
          &work_since_interrupt
        ) || !paradox_domain_validate_tags(
          VECTOR_ELT(state, DOMAINS_STATE_TAGS),
          &tags,
          &work_since_interrupt
        ) || !paradox_domain_validate_trafos(
          VECTOR_ELT(state, DOMAINS_STATE_TRAFOS),
          &trafos,
          &work_since_interrupt
        )) {
      Rf_error(
        "Corrupt exact ParamSetCollection state while snapshotting callbacks"
      );
    }

    SEXP result = assemble_domains(
      &params,
      &tags,
      &trafos,
      &dependencies,
      &values,
      VECTOR_ELT(state, DOMAINS_STATE_RESULT_NAMES),
      FALSE,
      &work_since_interrupt
    );
    UNPROTECT(1);
    return result;
  }

  SEXP state = PROTECT(Rf_allocVector(VECSXP, DOMAINS_STATE_SIZE));
  SET_VECTOR_ELT(state, DOMAINS_STATE_PARAMS, params_sexp);

  SEXP tags_sexp = paradox_domain_local_value(private_environment, ".tags");
  if (tags_sexp != R_UnboundValue) {
    SET_VECTOR_ELT(state, DOMAINS_STATE_TAGS, tags_sexp);
  }
  SEXP trafos_sexp = paradox_domain_local_value(
    private_environment,
    ".trafos"
  );
  if (trafos_sexp != R_UnboundValue) {
    SET_VECTOR_ELT(state, DOMAINS_STATE_TRAFOS, trafos_sexp);
  }
  paradox_domain_tags_t checked_tags;
  paradox_domain_trafos_t checked_trafos;
  if (tags_sexp == R_UnboundValue || trafos_sexp == R_UnboundValue ||
      !paradox_domain_validate_tags(
        tags_sexp,
        &checked_tags,
        &work_since_interrupt
      ) || !paradox_domain_validate_trafos(
        trafos_sexp,
        &checked_trafos,
        &work_since_interrupt
      )) {
    UNPROTECT(2);
    return R_NilValue;
  }

  SEXP values_sexp = paradox_domain_local_value(
    private_environment,
    ".values"
  );
  if (values_sexp != R_UnboundValue) {
    SET_VECTOR_ELT(state, DOMAINS_STATE_VALUES, values_sexp);
  }
  SEXP dependencies_sexp = paradox_domain_local_value(
    private_environment,
    ".deps"
  );
  if (dependencies_sexp != R_UnboundValue) {
    SET_VECTOR_ELT(state, DOMAINS_STATE_DEPS, dependencies_sexp);
  }
  paradox_domain_values_t checked_values;
  paradox_domain_dependencies_t checked_dependencies;
  if (values_sexp == R_UnboundValue ||
      dependencies_sexp == R_UnboundValue ||
      !paradox_domain_validate_values(
        values_sexp,
        &checked_values,
        &work_since_interrupt
      ) || !paradox_domain_validate_dependencies(
        dependencies_sexp,
        &checked_dependencies,
        &work_since_interrupt
      )) {
    UNPROTECT(2);
    return R_NilValue;
  }

  SEXP params_snapshot = PROTECT(snapshot_table(
    params_sexp,
    PARADOX_DOMAIN_TAGS,
    FALSE,
    &work_since_interrupt
  ));
  if (params_snapshot == R_NilValue) {
    UNPROTECT(3);
    return R_NilValue;
  }
  SET_VECTOR_ELT(state, DOMAINS_STATE_PARAMS, params_snapshot);
  UNPROTECT(1);
  SEXP tags_snapshot = PROTECT(snapshot_table(
    tags_sexp,
    2,
    TRUE,
    &work_since_interrupt
  ));
  if (tags_snapshot == R_NilValue) {
    UNPROTECT(3);
    return R_NilValue;
  }
  SET_VECTOR_ELT(state, DOMAINS_STATE_TAGS, tags_snapshot);
  UNPROTECT(1);
  SEXP trafos_snapshot = PROTECT(snapshot_table(
    trafos_sexp,
    2,
    TRUE,
    &work_since_interrupt
  ));
  if (trafos_snapshot == R_NilValue) {
    UNPROTECT(3);
    return R_NilValue;
  }
  SET_VECTOR_ELT(state, DOMAINS_STATE_TRAFOS, trafos_snapshot);
  UNPROTECT(1);
  SEXP dependencies_snapshot = PROTECT(snapshot_table(
    dependencies_sexp,
    3,
    FALSE,
    &work_since_interrupt
  ));
  if (dependencies_snapshot == R_NilValue) {
    UNPROTECT(3);
    return R_NilValue;
  }
  SET_VECTOR_ELT(state, DOMAINS_STATE_DEPS, dependencies_snapshot);
  UNPROTECT(1);
  SEXP values_snapshot = PROTECT(snapshot_values(
    values_sexp,
    &work_since_interrupt
  ));
  if (values_snapshot == R_NilValue) {
    UNPROTECT(3);
    return R_NilValue;
  }
  SET_VECTOR_ELT(state, DOMAINS_STATE_VALUES, values_snapshot);
  UNPROTECT(1);

  paradox_domain_params_t snapshot_params;
  paradox_domain_tags_t tags;
  paradox_domain_trafos_t trafos;
  paradox_domain_values_t values;
  paradox_domain_dependencies_t dependencies;
  if (!paradox_domain_validate_params(
        VECTOR_ELT(state, DOMAINS_STATE_PARAMS),
        R_NilValue,
        TRUE,
        &snapshot_params,
        &unused_row,
        &work_since_interrupt
      ) || !paradox_domain_validate_tags(
        VECTOR_ELT(state, DOMAINS_STATE_TAGS),
        &tags,
        &work_since_interrupt
      ) || !paradox_domain_validate_trafos(
        VECTOR_ELT(state, DOMAINS_STATE_TRAFOS),
        &trafos,
        &work_since_interrupt
      ) || !paradox_domain_validate_dependencies(
        VECTOR_ELT(state, DOMAINS_STATE_DEPS),
        &dependencies,
        &work_since_interrupt
      ) || !paradox_domain_validate_values(
        VECTOR_ELT(state, DOMAINS_STATE_VALUES),
        &values,
        &work_since_interrupt
      )) {
    UNPROTECT(2);
    return R_NilValue;
  }
  SET_VECTOR_ELT(
    state,
    DOMAINS_STATE_RESULT_NAMES,
    snapshot_params.ids
  );
  SEXP result = assemble_domains(
    &snapshot_params,
    &tags,
    &trafos,
    &dependencies,
    &values,
    VECTOR_ELT(state, DOMAINS_STATE_RESULT_NAMES),
    TRUE,
    &work_since_interrupt
  );
  UNPROTECT(2);
  return result;
}
