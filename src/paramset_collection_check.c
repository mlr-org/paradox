#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "paramset_domain_common.h"
#include "paramset_params_internal.h"
#include "r_api_compat.h"
#include "r_utils.h"

typedef enum {
  CHECK_GRAPH_UNKNOWN = 0,
  CHECK_GRAPH_SET,
  CHECK_GRAPH_COLLECTION
} check_graph_kind_t;

typedef struct {
  SEXP enclosure;
  SEXP private_environment;
  SEXP constraint;
  SEXP dependencies;
  SEXP private_constraint;
  SEXP private_dependencies;
  SEXP params;
  SEXP sets;
  SEXP children_with_constraints;
  SEXP self;
  SEXP check;
  SEXP test_constraint;
  SEXP check_dependencies;
} check_symbols_t;

enum check_graph_root_slot {
  CHECK_GRAPH_ROOT_SELF = 0,
  CHECK_GRAPH_ROOT_CLASSES,
  CHECK_GRAPH_ROOT_ENCLOSURE,
  CHECK_GRAPH_ROOT_PRIVATE,
  CHECK_GRAPH_ROOT_CONSTRAINT_BINDING,
  CHECK_GRAPH_ROOT_DEPS_BINDING,
  CHECK_GRAPH_ROOT_CONSTRAINT_VALUE,
  CHECK_GRAPH_ROOT_DEPS_TABLE,
  CHECK_GRAPH_ROOT_DEPS_IDS,
  CHECK_GRAPH_ROOT_DEPS_ON,
  CHECK_GRAPH_ROOT_DEPS_CONDITIONS,
  CHECK_GRAPH_ROOT_SETS,
  CHECK_GRAPH_ROOT_SET_NAMES,
  CHECK_GRAPH_ROOT_CHILDREN_METHOD,
  CHECK_GRAPH_ROOT_COUNT
};

enum check_method_slot {
  CHECK_METHOD_CHECK = 0,
  CHECK_METHOD_TEST_CONSTRAINT,
  CHECK_METHOD_CHECK_DEPENDENCIES,
  CHECK_METHOD_COUNT
};

enum check_param_snapshot_slot {
  CHECK_PARAM_IDS = 0,
  CHECK_PARAM_CLASSES,
  CHECK_PARAM_LOWER,
  CHECK_PARAM_UPPER,
  CHECK_PARAM_TOLERANCE,
  CHECK_PARAM_LEVELS,
  CHECK_PARAM_SPECIAL_VALUES,
  CHECK_PARAM_STORAGE_TYPES,
  CHECK_PARAM_COUNT
};

typedef struct {
  SEXP roots;
  check_graph_kind_t kind;
  R_xlen_t parent;
  R_xlen_t child_position;
  R_xlen_t next_child;
} check_graph_node_t;

typedef struct {
  check_graph_node_t *nodes;
  R_xlen_t *path;
  R_xlen_t count;
  R_xlen_t capacity;
} check_graph_t;

static check_symbols_t install_check_symbols(void) {
  const check_symbols_t symbols = {
    Rf_install(".__enclos_env__"),
    Rf_install("private"),
    Rf_install("constraint"),
    Rf_install("deps"),
    Rf_install(".constraint"),
    Rf_install(".deps"),
    Rf_install(".params"),
    Rf_install(".sets"),
    Rf_install(".children_with_constraints"),
    Rf_install("self"),
    Rf_install("check"),
    Rf_install("test_constraint"),
    Rf_install("check_dependencies")
  };
  return symbols;
}

static int exact_class(SEXP self, check_graph_kind_t *kind,
    R_xlen_t *work_since_interrupt) {
  static const char *const set_classes[] = {"ParamSet", "R6"};
  static const char *const collection_classes[] = {
    "ParamSetCollection", "ParamSet", "R6"
  };
  if (TYPEOF(self) != ENVSXP) {
    return FALSE;
  }
  SEXP classes = Rf_getAttrib(self, R_ClassSymbol);
  if (paradox_domain_exact_string_vector(
      classes,
      set_classes,
      2,
      work_since_interrupt
    )) {
    *kind = CHECK_GRAPH_SET;
    return TRUE;
  }
  if (paradox_domain_exact_string_vector(
      classes,
      collection_classes,
      3,
      work_since_interrupt
    )) {
    *kind = CHECK_GRAPH_COLLECTION;
    return TRUE;
  }
  return FALSE;
}

static int exact_forwarder(SEXP function, const char *target) {
  if (TYPEOF(function) != CLOSXP) {
    return FALSE;
  }
  SEXP body = paradox_api_closure_expression(function);
  SEXP formals = paradox_api_closure_formals(function);
  if (TYPEOF(body) != LANGSXP || CAR(body) != Rf_install(target) ||
      formals != R_NilValue) {
    return FALSE;
  }
  static const char *const arguments[] = {"self", "private", "super"};
  SEXP argument = CDR(body);
  for (R_xlen_t index = 0; index < 3; ++index) {
    SEXP symbol = Rf_install(arguments[index]);
    if (TYPEOF(argument) != LISTSXP || TAG(argument) != symbol ||
        CAR(argument) != symbol) {
      return FALSE;
    }
    argument = CDR(argument);
  }
  return argument == R_NilValue;
}

static int canonical_private_method(SEXP private_environment,
    SEXP enclosure, SEXP namespace_environment, SEXP symbol,
    const char *target, SEXP *result) {
  SEXP target_symbol = Rf_install(target);
  if (!R_existsVarInFrame(private_environment, symbol) ||
      R_BindingIsActive(symbol, private_environment) ||
      !R_BindingIsLocked(symbol, private_environment) ||
      !R_existsVarInFrame(namespace_environment, target_symbol) ||
      R_BindingIsActive(target_symbol, namespace_environment) ||
      !R_BindingIsLocked(target_symbol, namespace_environment)) {
    return FALSE;
  }
  SEXP function = PROTECT(paradox_api_local_value(
    private_environment,
    symbol
  ));
  if (!exact_forwarder(
        function,
        target
      ) || paradox_api_closure_environment(function) != enclosure ||
      R_existsVarInFrame(enclosure, target_symbol)) {
    UNPROTECT(1);
    return FALSE;
  }
  *result = function;
  UNPROTECT(1);
  return TRUE;
}

static SEXP active_function(SEXP self, SEXP symbol) {
  return TYPEOF(self) == ENVSXP && R_existsVarInFrame(self, symbol) &&
      R_BindingIsActive(symbol, self)
    ? R_ActiveBindingFunction(symbol, self)
    : R_UnboundValue;
}

static SEXP append_node_roots(SEXP *root_plan,
    PROTECT_INDEX root_plan_index, SEXP self) {
  PROTECT(self);
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, CHECK_GRAPH_ROOT_COUNT));
  SET_VECTOR_ELT(roots, CHECK_GRAPH_ROOT_SELF, self);
  SEXP expanded = PROTECT(Rf_cons(roots, *root_plan));
  REPROTECT(expanded, root_plan_index);
  *root_plan = expanded;
  UNPROTECT(3);
  return roots;
}

static void initialize_graph(check_graph_t *graph) {
  graph->capacity = 8;
  graph->nodes = paradox_temporary_alloc(
    graph->capacity,
    sizeof(*graph->nodes)
  );
  graph->path = paradox_temporary_alloc(
    graph->capacity,
    sizeof(*graph->path)
  );
  graph->count = 0;
}

static int reserve_graph_node(check_graph_t *graph) {
  if (graph->count < graph->capacity) {
    return TRUE;
  }
  if (graph->capacity > R_XLEN_T_MAX / 2) {
    return FALSE;
  }
  const R_xlen_t capacity = graph->capacity * 2;
  check_graph_node_t *nodes = paradox_temporary_alloc(
    capacity,
    sizeof(*nodes)
  );
  R_xlen_t *path = paradox_temporary_alloc(capacity, sizeof(*path));
  memcpy(
    nodes,
    graph->nodes,
    (size_t) graph->count * sizeof(*nodes)
  );
  memcpy(
    path,
    graph->path,
    (size_t) graph->count * sizeof(*path)
  );
  graph->nodes = nodes;
  graph->path = path;
  graph->capacity = capacity;
  return TRUE;
}

static int initialize_graph_node(SEXP self, R_xlen_t parent,
    R_xlen_t child_position, SEXP namespace_environment,
    const check_symbols_t *symbols, check_graph_node_t *node,
    SEXP *root_plan, PROTECT_INDEX root_plan_index,
    R_xlen_t *work_since_interrupt) {
  node->roots = append_node_roots(root_plan, root_plan_index, self);
  node->parent = parent;
  node->child_position = child_position;
  node->next_child = 0;

  SEXP classes = Rf_getAttrib(self, R_ClassSymbol);
  SET_VECTOR_ELT(node->roots, CHECK_GRAPH_ROOT_CLASSES, classes);
  if (!exact_class(self, &node->kind, work_since_interrupt)) {
    return FALSE;
  }

  SEXP enclosure = paradox_api_local_value(self, symbols->enclosure);
  SET_VECTOR_ELT(node->roots, CHECK_GRAPH_ROOT_ENCLOSURE, enclosure);
  SEXP private_environment = paradox_api_local_value(
    enclosure,
    symbols->private_environment
  );
  SET_VECTOR_ELT(node->roots, CHECK_GRAPH_ROOT_PRIVATE, private_environment);
  if (TYPEOF(enclosure) != ENVSXP || TYPEOF(private_environment) != ENVSXP ||
      paradox_api_parent_environment(enclosure) != namespace_environment ||
      paradox_api_local_value(enclosure, symbols->self) != self ||
      paradox_api_local_value(
        enclosure,
        symbols->private_environment
      ) != private_environment ||
      !paradox_params_canonical_active_member(
        self,
        private_environment,
        "constraint",
        node->kind == CHECK_GRAPH_COLLECTION
          ? ".__ParamSetCollection__constraint"
          : ".__ParamSet__constraint",
        "f",
        node->kind == CHECK_GRAPH_COLLECTION
          ? ".__ParamSet__constraint"
          : NULL,
        work_since_interrupt
      ) || !paradox_params_canonical_active_member(
        self,
        private_environment,
        "deps",
        node->kind == CHECK_GRAPH_COLLECTION
          ? ".__ParamSetCollection__deps"
          : ".__ParamSet__deps",
        "v",
        node->kind == CHECK_GRAPH_COLLECTION ? ".__ParamSet__deps" : NULL,
        work_since_interrupt
      )) {
    return FALSE;
  }

  SEXP constraint_binding = active_function(self, symbols->constraint);
  SEXP deps_binding = active_function(self, symbols->dependencies);
  SET_VECTOR_ELT(
    node->roots,
    CHECK_GRAPH_ROOT_CONSTRAINT_BINDING,
    constraint_binding
  );
  SET_VECTOR_ELT(node->roots, CHECK_GRAPH_ROOT_DEPS_BINDING, deps_binding);

  SEXP dependencies = paradox_api_local_value(
    private_environment,
    symbols->private_dependencies
  );
  SET_VECTOR_ELT(node->roots, CHECK_GRAPH_ROOT_DEPS_TABLE, dependencies);
  paradox_domain_dependencies_t checked_dependencies;
  if (dependencies == R_UnboundValue ||
      !paradox_params_supported_table_attributes(dependencies, TRUE) ||
      !paradox_domain_validate_dependencies(
        dependencies,
        &checked_dependencies,
        work_since_interrupt
      ) || checked_dependencies.row_count != 0) {
    return FALSE;
  }
  SET_VECTOR_ELT(
    node->roots,
    CHECK_GRAPH_ROOT_DEPS_IDS,
    checked_dependencies.ids
  );
  SET_VECTOR_ELT(
    node->roots,
    CHECK_GRAPH_ROOT_DEPS_ON,
    checked_dependencies.on
  );
  SET_VECTOR_ELT(
    node->roots,
    CHECK_GRAPH_ROOT_DEPS_CONDITIONS,
    checked_dependencies.conditions
  );

  if (node->kind == CHECK_GRAPH_SET) {
    SEXP constraint = paradox_api_local_value(
      private_environment,
      symbols->private_constraint
    );
    SET_VECTOR_ELT(
      node->roots,
      CHECK_GRAPH_ROOT_CONSTRAINT_VALUE,
      constraint
    );
    return constraint == R_NilValue;
  }

  SEXP children_method = R_UnboundValue;
  if (!canonical_private_method(
      private_environment,
      enclosure,
      namespace_environment,
      symbols->children_with_constraints,
      ".__ParamSetCollection__.children_with_constraints",
      &children_method
    )) {
    return FALSE;
  }
  SET_VECTOR_ELT(
    node->roots,
    CHECK_GRAPH_ROOT_CHILDREN_METHOD,
    children_method
  );

  SEXP sets = paradox_api_local_value(private_environment, symbols->sets);
  SET_VECTOR_ELT(node->roots, CHECK_GRAPH_ROOT_SETS, sets);
  if (TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isObject(sets) ||
      !paradox_params_names_are_only_attribute(sets)) {
    return FALSE;
  }
  SEXP set_names = Rf_getAttrib(sets, R_NamesSymbol);
  SET_VECTOR_ELT(node->roots, CHECK_GRAPH_ROOT_SET_NAMES, set_names);
  return TYPEOF(set_names) == STRSXP && !ALTREP(set_names) &&
    paradox_api_has_no_attributes(set_names) &&
    XLENGTH(set_names) == XLENGTH(sets);
}

static int build_graph(SEXP self, SEXP private_environment,
    SEXP namespace_environment, const check_symbols_t *symbols,
    check_graph_t *graph, SEXP *root_plan,
    PROTECT_INDEX root_plan_index) {
  initialize_graph(graph);
  R_xlen_t work_since_interrupt = 0;
  const int root_initialized = initialize_graph_node(
      self,
      R_XLEN_T_MAX,
      R_XLEN_T_MAX,
      namespace_environment,
      symbols,
      &graph->nodes[0],
      root_plan,
      root_plan_index,
      &work_since_interrupt
    );
  if (!root_initialized || graph->nodes[0].kind != CHECK_GRAPH_COLLECTION ||
      VECTOR_ELT(graph->nodes[0].roots, CHECK_GRAPH_ROOT_PRIVATE) !=
        private_environment) {
    return FALSE;
  }
  graph->count = 1;
  graph->path[0] = 0;
  R_xlen_t depth = 1;

  while (depth != 0) {
    const R_xlen_t node_index = graph->path[depth - 1];
    check_graph_node_t *node = &graph->nodes[node_index];
    SEXP sets = VECTOR_ELT(node->roots, CHECK_GRAPH_ROOT_SETS);
    if (node->kind == CHECK_GRAPH_COLLECTION &&
        node->next_child < XLENGTH(sets)) {
      const R_xlen_t child_position = node->next_child;
      SEXP child = PROTECT(VECTOR_ELT(sets, child_position));
      for (R_xlen_t ancestor = 0; ancestor < depth; ++ancestor) {
        paradox_domain_account_work(&work_since_interrupt);
        if (VECTOR_ELT(
            graph->nodes[graph->path[ancestor]].roots,
            CHECK_GRAPH_ROOT_SELF
          ) == child) {
          UNPROTECT(1);
          return FALSE;
        }
      }
      if (!reserve_graph_node(graph)) {
        UNPROTECT(1);
        return FALSE;
      }
      const R_xlen_t child_index = graph->count;
      const int child_initialized = initialize_graph_node(
          child,
          node_index,
          child_position,
          namespace_environment,
          symbols,
          &graph->nodes[child_index],
          root_plan,
          root_plan_index,
          &work_since_interrupt
        );
      if (!child_initialized) {
        UNPROTECT(1);
        return FALSE;
      }
      UNPROTECT(1);
      ++graph->nodes[node_index].next_child;
      ++graph->count;
      graph->path[depth] = child_index;
      ++depth;
      continue;
    }
    --depth;
  }
  return TRUE;
}

static int exact_live_class(SEXP classes, check_graph_kind_t kind) {
  if (TYPEOF(classes) != STRSXP || ALTREP(classes)) {
    return FALSE;
  }
  if (kind == CHECK_GRAPH_SET) {
    return XLENGTH(classes) == 2 &&
      paradox_domain_string_is(STRING_ELT(classes, 0), "ParamSet") &&
      paradox_domain_string_is(STRING_ELT(classes, 1), "R6");
  }
  return XLENGTH(classes) == 3 &&
    paradox_domain_string_is(
      STRING_ELT(classes, 0),
      "ParamSetCollection"
    ) && paradox_domain_string_is(STRING_ELT(classes, 1), "ParamSet") &&
    paradox_domain_string_is(STRING_ELT(classes, 2), "R6");
}

/* No operation in this commit audit allocates or dispatches R code. Symbols
 * are interned before the graph is first observed, and R < 4.6 cannot enter
 * this lane because its public API cannot classify inert value bindings. */
static int graph_stable(const check_graph_t *graph,
    SEXP namespace_environment, const check_symbols_t *symbols) {
  for (R_xlen_t index = 0; index < graph->count; ++index) {
    const check_graph_node_t *node = &graph->nodes[index];
    SEXP self = VECTOR_ELT(node->roots, CHECK_GRAPH_ROOT_SELF);
    SEXP classes = VECTOR_ELT(node->roots, CHECK_GRAPH_ROOT_CLASSES);
    SEXP enclosure = VECTOR_ELT(node->roots, CHECK_GRAPH_ROOT_ENCLOSURE);
    SEXP private_environment = VECTOR_ELT(
      node->roots,
      CHECK_GRAPH_ROOT_PRIVATE
    );
    if (Rf_getAttrib(self, R_ClassSymbol) != classes ||
        !exact_live_class(classes, node->kind) ||
        paradox_api_local_value(self, symbols->enclosure) != enclosure ||
        paradox_api_local_value(
          enclosure,
          symbols->private_environment
        ) != private_environment ||
        paradox_api_parent_environment(enclosure) != namespace_environment ||
        active_function(self, symbols->constraint) != VECTOR_ELT(
          node->roots,
          CHECK_GRAPH_ROOT_CONSTRAINT_BINDING
        ) || active_function(self, symbols->dependencies) != VECTOR_ELT(
          node->roots,
          CHECK_GRAPH_ROOT_DEPS_BINDING
        )) {
      return FALSE;
    }

    SEXP dependencies = VECTOR_ELT(
      node->roots,
      CHECK_GRAPH_ROOT_DEPS_TABLE
    );
    SEXP ids = VECTOR_ELT(node->roots, CHECK_GRAPH_ROOT_DEPS_IDS);
    SEXP on = VECTOR_ELT(node->roots, CHECK_GRAPH_ROOT_DEPS_ON);
    SEXP conditions = VECTOR_ELT(
      node->roots,
      CHECK_GRAPH_ROOT_DEPS_CONDITIONS
    );
    paradox_domain_dependencies_t checked_dependencies;
    R_xlen_t work_since_interrupt = 0;
    if (paradox_api_local_value(
          private_environment,
          symbols->private_dependencies
        ) != dependencies || TYPEOF(dependencies) != VECSXP ||
        !paradox_params_supported_table_attributes(dependencies, TRUE) ||
        !paradox_domain_validate_dependencies(
          dependencies,
          &checked_dependencies,
          &work_since_interrupt
        ) || checked_dependencies.row_count != 0 ||
        checked_dependencies.ids != ids || checked_dependencies.on != on ||
        checked_dependencies.conditions != conditions ||
        XLENGTH(dependencies) != 3 || VECTOR_ELT(dependencies, 0) != ids ||
        VECTOR_ELT(dependencies, 1) != on ||
        VECTOR_ELT(dependencies, 2) != conditions ||
        TYPEOF(ids) != STRSXP || TYPEOF(on) != STRSXP ||
        TYPEOF(conditions) != VECSXP || ALTREP(ids) || ALTREP(on) ||
        ALTREP(conditions) || XLENGTH(ids) != 0 || XLENGTH(on) != 0 ||
        XLENGTH(conditions) != 0) {
      return FALSE;
    }

    if (node->kind == CHECK_GRAPH_SET) {
      if (paradox_api_local_value(
          private_environment,
          symbols->private_constraint
        ) != R_NilValue) {
        return FALSE;
      }
    } else {
      SEXP children_method = R_UnboundValue;
      if (!canonical_private_method(
          private_environment,
          enclosure,
          namespace_environment,
          symbols->children_with_constraints,
          ".__ParamSetCollection__.children_with_constraints",
          &children_method
        ) || children_method != VECTOR_ELT(
          node->roots,
          CHECK_GRAPH_ROOT_CHILDREN_METHOD
        )) {
        return FALSE;
      }
      SEXP sets = VECTOR_ELT(node->roots, CHECK_GRAPH_ROOT_SETS);
      SEXP set_names = VECTOR_ELT(
        node->roots,
        CHECK_GRAPH_ROOT_SET_NAMES
      );
      if (paradox_api_local_value(
            private_environment,
            symbols->sets
          ) != sets || Rf_getAttrib(sets, R_NamesSymbol) != set_names ||
          TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isObject(sets) ||
          !paradox_params_names_are_only_attribute(sets) ||
          TYPEOF(set_names) != STRSXP || ALTREP(set_names) ||
          !paradox_api_has_no_attributes(set_names) ||
          XLENGTH(set_names) != XLENGTH(sets)) {
        return FALSE;
      }
    }

    if (node->parent != R_XLEN_T_MAX) {
      const check_graph_node_t *parent = &graph->nodes[node->parent];
      SEXP parent_sets = VECTOR_ELT(
        parent->roots,
        CHECK_GRAPH_ROOT_SETS
      );
      if (node->child_position >= XLENGTH(parent_sets) ||
          VECTOR_ELT(parent_sets, node->child_position) != self) {
        return FALSE;
      }
    }
  }
  return TRUE;
}

static int ascii_string(SEXP string) {
  if (string == NA_STRING || Rf_getCharCE(string) == CE_BYTES) {
    return FALSE;
  }
  const unsigned char *text = (const unsigned char *) CHAR(string);
  for (; *text != '\0'; ++text) {
    if (*text >= 0x80U) {
      return FALSE;
    }
  }
  return TRUE;
}

static int same_ascii_strings(SEXP left, SEXP right) {
  return ascii_string(left) && ascii_string(right) &&
    strcmp(CHAR(left), CHAR(right)) == 0;
}

static SEXP named_column(SEXP table, SEXP names, const char *name) {
  SEXP result = R_NilValue;
  int found = 0;
  for (R_xlen_t index = 0; index < XLENGTH(table); ++index) {
    SEXP candidate = STRING_ELT(names, index);
    if (ascii_string(candidate) && strcmp(CHAR(candidate), name) == 0) {
      result = VECTOR_ELT(table, index);
      ++found;
    }
  }
  return found == 1 ? result : R_NilValue;
}

static int exact_table_class(SEXP classes) {
  return TYPEOF(classes) == STRSXP && !ALTREP(classes) &&
    paradox_api_has_no_attributes(classes) &&
    XLENGTH(classes) == 2 &&
    paradox_domain_string_is(STRING_ELT(classes, 0), "data.table") &&
    paradox_domain_string_is(STRING_ELT(classes, 1), "data.frame");
}

static int exact_string_vector(SEXP left, SEXP right) {
  if (TYPEOF(left) != STRSXP || TYPEOF(right) != STRSXP || ALTREP(left) ||
      ALTREP(right) || XLENGTH(left) != XLENGTH(right)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(left); ++index) {
    if (!same_ascii_strings(
        STRING_ELT(left, index),
        STRING_ELT(right, index)
      )) {
      return FALSE;
    }
  }
  return TRUE;
}

static int exact_numeric_vector(SEXP left, SEXP right) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(left);
  if (type != (SEXPTYPE) TYPEOF(right) ||
      (type != INTSXP && type != REALSXP) || ALTREP(left) ||
      ALTREP(right) || XLENGTH(left) != XLENGTH(right)) {
    return FALSE;
  }
  if (type == INTSXP) {
    for (R_xlen_t index = 0; index < XLENGTH(left); ++index) {
      if (INTEGER_ELT(left, index) != INTEGER_ELT(right, index)) {
        return FALSE;
      }
    }
    return TRUE;
  }
  if (TYPEOF(left) != REALSXP) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(left); ++index) {
    const double observed = REAL_ELT(left, index);
    const double expected = REAL_ELT(right, index);
    if (memcmp(&observed, &expected, sizeof(observed)) != 0) {
      return FALSE;
    }
  }
  return TRUE;
}

static int exact_levels(SEXP left, SEXP right) {
  if (TYPEOF(left) != VECSXP || TYPEOF(right) != VECSXP || ALTREP(left) ||
      ALTREP(right) || XLENGTH(left) != XLENGTH(right)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(left); ++index) {
    SEXP observed = VECTOR_ELT(left, index);
    SEXP expected = VECTOR_ELT(right, index);
    if (observed == R_NilValue || expected == R_NilValue) {
      if (observed != expected) {
        return FALSE;
      }
      continue;
    }
    const SEXPTYPE type = (SEXPTYPE) TYPEOF(observed);
    if (type != (SEXPTYPE) TYPEOF(expected) ||
        (type != LGLSXP && type != STRSXP) || ALTREP(observed) ||
        ALTREP(expected) || !paradox_api_has_no_attributes(observed) ||
        !paradox_api_has_no_attributes(expected) ||
        XLENGTH(observed) != XLENGTH(expected)) {
      return FALSE;
    }
    if (type == STRSXP) {
      if (!exact_string_vector(observed, expected)) {
        return FALSE;
      }
      continue;
    }
    for (R_xlen_t level = 0; level < XLENGTH(observed); ++level) {
      if (LOGICAL_ELT(observed, level) != LOGICAL_ELT(expected, level)) {
        return FALSE;
      }
    }
  }
  return TRUE;
}

static int exact_empty_special_values(SEXP left, SEXP right) {
  if (TYPEOF(left) != VECSXP || TYPEOF(right) != VECSXP || ALTREP(left) ||
      ALTREP(right) || XLENGTH(left) != XLENGTH(right)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(left); ++index) {
    SEXP observed = VECTOR_ELT(left, index);
    SEXP expected = VECTOR_ELT(right, index);
    if (TYPEOF(observed) != VECSXP || TYPEOF(expected) != VECSXP ||
        ALTREP(observed) || ALTREP(expected) ||
        !paradox_api_has_no_attributes(observed) ||
        !paradox_api_has_no_attributes(expected) ||
        XLENGTH(observed) != 0 || XLENGTH(expected) != 0) {
      return FALSE;
    }
  }
  return TRUE;
}

static SEXP snapshot_params(SEXP params) {
  static const char *const names[] = {
    "id", "cls", "lower", "upper", "tolerance", "levels",
    "special_vals", "storage_type"
  };
  if (TYPEOF(params) != VECSXP || ALTREP(params)) {
    return R_NilValue;
  }
  SEXP source_names = PROTECT(Rf_getAttrib(params, R_NamesSymbol));
  SEXP source_classes = PROTECT(Rf_getAttrib(params, R_ClassSymbol));
  if (TYPEOF(source_names) != STRSXP || ALTREP(source_names) ||
      !paradox_api_has_no_attributes(source_names) ||
      XLENGTH(source_names) != XLENGTH(params) ||
      !exact_table_class(source_classes)) {
    UNPROTECT(2);
    return R_NilValue;
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, CHECK_PARAM_COUNT));
  SEXP result_names = PROTECT(Rf_allocVector(STRSXP, CHECK_PARAM_COUNT));
  for (R_xlen_t index = 0; index < CHECK_PARAM_COUNT; ++index) {
    SEXP source = PROTECT(named_column(
      params,
      source_names,
      names[index]
    ));
    /* Duplicating an ALTREP vector may dispatch its Duplicate method.  The
     * snapshot is an admission barrier, so reject before any such callback.
     * Root the borrowed column independently: a finalizer during an ordinary
     * duplicate can remove it from the still-rooted table before a later
     * allocation in that duplicate completes. */
    if (source == R_NilValue || ALTREP(source) ||
        !paradox_api_has_no_attributes(source)) {
      UNPROTECT(5);
      return R_NilValue;
    }
    SEXP copy = PROTECT(Rf_duplicate(source));
    SET_VECTOR_ELT(result, index, copy);
    UNPROTECT(2);
    SET_STRING_ELT(result_names, index, Rf_mkChar(names[index]));
  }
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  SEXP result_classes = PROTECT(Rf_duplicate(source_classes));
  Rf_setAttrib(result, R_ClassSymbol, result_classes);
  UNPROTECT(5);
  return result;
}

static int params_match_snapshot(SEXP params, SEXP snapshot) {
  static const char *const names[] = {
    "id", "cls", "lower", "upper", "tolerance", "levels",
    "special_vals", "storage_type"
  };
  if (TYPEOF(params) != VECSXP || ALTREP(params) ||
      TYPEOF(snapshot) != VECSXP || XLENGTH(snapshot) != CHECK_PARAM_COUNT) {
    return FALSE;
  }
  SEXP source_names = Rf_getAttrib(params, R_NamesSymbol);
  SEXP source_classes = Rf_getAttrib(params, R_ClassSymbol);
  if (TYPEOF(source_names) != STRSXP || ALTREP(source_names) ||
      !paradox_api_has_no_attributes(source_names) ||
      XLENGTH(source_names) != XLENGTH(params) ||
      !exact_table_class(source_classes)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < CHECK_PARAM_COUNT; ++index) {
    SEXP observed = named_column(params, source_names, names[index]);
    SEXP expected = VECTOR_ELT(snapshot, index);
    if (observed == R_NilValue ||
        !paradox_api_has_no_attributes(observed) ||
        !paradox_api_has_no_attributes(expected)) {
      return FALSE;
    }
    int exact = FALSE;
    if (index == CHECK_PARAM_IDS || index == CHECK_PARAM_CLASSES ||
        index == CHECK_PARAM_STORAGE_TYPES) {
      exact = exact_string_vector(observed, expected);
    } else if (index == CHECK_PARAM_LOWER || index == CHECK_PARAM_UPPER ||
        index == CHECK_PARAM_TOLERANCE) {
      exact = exact_numeric_vector(observed, expected);
    } else if (index == CHECK_PARAM_LEVELS) {
      exact = exact_levels(observed, expected);
    } else {
      exact = exact_empty_special_values(observed, expected);
    }
    if (!exact) {
      return FALSE;
    }
  }
  return TRUE;
}

static int supported_input(SEXP values) {
  if (TYPEOF(values) != VECSXP || ALTREP(values) || Rf_isObject(values)) {
    return FALSE;
  }
  SEXP names = Rf_getAttrib(values, R_NamesSymbol);
  if (TYPEOF(names) != STRSXP || ALTREP(names) ||
      XLENGTH(names) != XLENGTH(values)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(values); ++index) {
    if (!ascii_string(STRING_ELT(names, index))) {
      return FALSE;
    }
    SEXP value = VECTOR_ELT(values, index);
    const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
    /* XLENGTH() is not defined for arbitrary utility payloads such as
     * closures, environments, or calls.  Reject their type first so they
     * retain ParamUty's ordinary R path instead of raising a native error. */
    if ((type != REALSXP && type != INTSXP && type != LGLSXP &&
          type != STRSXP) || ALTREP(value) || Rf_isObject(value) ||
        XLENGTH(value) != 1 ||
        (type == STRSXP && !ascii_string(STRING_ELT(value, 0)))) {
      return FALSE;
    }
  }
  return TRUE;
}

static int input_matches_snapshot(SEXP input, SEXP snapshot) {
  if (!supported_input(input) || !supported_input(snapshot) ||
      XLENGTH(input) != XLENGTH(snapshot)) {
    return FALSE;
  }
  SEXP input_names = Rf_getAttrib(input, R_NamesSymbol);
  SEXP snapshot_names = Rf_getAttrib(snapshot, R_NamesSymbol);
  if (!exact_string_vector(input_names, snapshot_names)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(input); ++index) {
    SEXP observed = VECTOR_ELT(input, index);
    SEXP expected = VECTOR_ELT(snapshot, index);
    if (TYPEOF(observed) != TYPEOF(expected)) {
      return FALSE;
    }
    switch ((SEXPTYPE) TYPEOF(observed)) {
    case REALSXP: {
      const double left = REAL_ELT(observed, 0);
      const double right = REAL_ELT(expected, 0);
      if (memcmp(&left, &right, sizeof(left)) != 0) return FALSE;
      break;
    }
    case INTSXP:
      if (INTEGER_ELT(observed, 0) != INTEGER_ELT(expected, 0)) return FALSE;
      break;
    case LGLSXP:
      if (LOGICAL_ELT(observed, 0) != LOGICAL_ELT(expected, 0)) return FALSE;
      break;
    case STRSXP:
      if (!same_ascii_strings(
          STRING_ELT(observed, 0),
          STRING_ELT(expected, 0)
        )) return FALSE;
      break;
    default:
      return FALSE;
    }
  }
  return TRUE;
}

static int capture_methods(SEXP self, const check_symbols_t *symbols,
    SEXP methods) {
  const SEXP names[CHECK_METHOD_COUNT] = {
    symbols->check,
    symbols->test_constraint,
    symbols->check_dependencies
  };
  for (R_xlen_t index = 0; index < CHECK_METHOD_COUNT; ++index) {
    if (!R_existsVarInFrame(self, names[index]) ||
        R_BindingIsActive(names[index], self) ||
        !R_BindingIsLocked(names[index], self)) {
      return FALSE;
    }
    SEXP method = paradox_api_local_value(self, names[index]);
    if (TYPEOF(method) != CLOSXP) {
      return FALSE;
    }
    SET_VECTOR_ELT(methods, index, method);
  }
  return TRUE;
}

static int methods_stable(SEXP self, const check_symbols_t *symbols,
    SEXP methods) {
  const SEXP names[CHECK_METHOD_COUNT] = {
    symbols->check,
    symbols->test_constraint,
    symbols->check_dependencies
  };
  for (R_xlen_t index = 0; index < CHECK_METHOD_COUNT; ++index) {
    if (!R_existsVarInFrame(self, names[index]) ||
        R_BindingIsActive(names[index], self) ||
        !R_BindingIsLocked(names[index], self) ||
        paradox_api_local_value(self, names[index]) !=
          VECTOR_ELT(methods, index)) {
      return FALSE;
    }
  }
  return TRUE;
}

SEXP paradox_param_set_collection_check_builtin(SEXP private_environment,
    SEXP self, SEXP values, SEXP sanitize, SEXP check_strict) {
  if (R_VERSION < R_Version(4, 6, 0) ||
      TYPEOF(private_environment) != ENVSXP ||
      TYPEOF(sanitize) != LGLSXP || ALTREP(sanitize) ||
      XLENGTH(sanitize) != 1 || LOGICAL_ELT(sanitize, 0) == NA_LOGICAL ||
      TYPEOF(check_strict) != LGLSXP || ALTREP(check_strict) ||
      XLENGTH(check_strict) != 1 ||
      LOGICAL_ELT(check_strict, 0) == NA_LOGICAL ||
      !supported_input(values)) {
    return R_NilValue;
  }

  const check_symbols_t symbols = install_check_symbols();
  SEXP namespace_environment = PROTECT(
    paradox_api_registered_namespace("paradox")
  );
  SEXP methods = PROTECT(Rf_allocVector(VECSXP, CHECK_METHOD_COUNT));
  if (TYPEOF(namespace_environment) != ENVSXP ||
      !paradox_param_set_collection_check_auth(self) ||
      !capture_methods(self, &symbols, methods)) {
    UNPROTECT(2);
    return R_NilValue;
  }

  SEXP params = PROTECT(paradox_api_local_value(
    private_environment,
    symbols.params
  ));
  if (params == R_UnboundValue ||
      !paradox_domain_owns_private_environment(self, private_environment)) {
    UNPROTECT(3);
    return R_NilValue;
  }
  SEXP params_snapshot = PROTECT(snapshot_params(params));
  SEXP values_snapshot = PROTECT(Rf_duplicate(values));
  SEXP sanitize_snapshot = PROTECT(Rf_ScalarLogical(
    LOGICAL_ELT(sanitize, 0)
  ));
  const int params_same = params_snapshot != R_NilValue &&
    params_match_snapshot(params, params_snapshot);
  const int input_same = input_matches_snapshot(values, values_snapshot);
  const int method_same = methods_stable(self, &symbols, methods);
  if (params_snapshot == R_NilValue ||
      !params_same || !input_same || !method_same) {
    UNPROTECT(6);
    return R_NilValue;
  }

  PROTECT_INDEX root_plan_index;
  SEXP root_plan;
  PROTECT_WITH_INDEX(root_plan = R_NilValue, &root_plan_index);
  check_graph_t graph;
  const int strict = LOGICAL_ELT(check_strict, 0);
  const int graph_built = !strict || build_graph(
        self,
        private_environment,
        namespace_environment,
        &symbols,
        &graph,
        &root_plan,
        root_plan_index
      );
  const int graph_is_stable = !strict || (graph_built &&
    graph_stable(&graph, namespace_environment, &symbols));
  if (strict && (!graph_built || !graph_is_stable ||
      !params_match_snapshot(params, params_snapshot) ||
      !input_matches_snapshot(values, values_snapshot) ||
      !methods_stable(self, &symbols, methods))) {
    UNPROTECT(7);
    return R_NilValue;
  }

  SEXP result = PROTECT(paradox_param_set_check_builtin(
    params_snapshot,
    values_snapshot,
    sanitize_snapshot
  ));
  if (result == R_NilValue) {
    UNPROTECT(8);
    return R_NilValue;
  }

  if (!paradox_param_set_collection_check_auth(self) ||
      !methods_stable(self, &symbols, methods) ||
      paradox_api_local_value(private_environment, symbols.params) != params ||
      !params_match_snapshot(params, params_snapshot) ||
      !input_matches_snapshot(values, values_snapshot) ||
      (strict && !graph_stable(
        &graph,
        namespace_environment,
        &symbols
      ))) {
    UNPROTECT(8);
    return R_NilValue;
  }

  UNPROTECT(8);
  return result;
}
