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
  DEPS_NODE_UNKNOWN = 0,
  DEPS_NODE_SET,
  DEPS_NODE_COLLECTION
} deps_node_kind_t;

enum deps_root_slot {
  DEPS_ROOT_SELF = 0,
  DEPS_ROOT_PRIVATE,
  DEPS_ROOT_PARAMS,
  DEPS_ROOT_PARAM_IDS,
  DEPS_ROOT_TABLE,
  DEPS_ROOT_IDS,
  DEPS_ROOT_ON,
  DEPS_ROOT_CONDITIONS,
  DEPS_ROOT_SETS,
  DEPS_ROOT_SET_NAMES,
  DEPS_ROOT_POSTFIX,
  DEPS_ROOT_OWNER,
  DEPS_ROOT_AFFIXED_IDS,
  DEPS_ROOT_COUNT
};

typedef struct {
  SEXP roots;
  SEXP self;
  SEXP private_environment;
  deps_node_kind_t kind;
  paradox_domain_params_t params;
  paradox_domain_dependencies_t dependencies;
  SEXP sets;
  SEXP set_names;
  SEXP owner;
  SEXP affixed_ids;
  R_xlen_t parent;
  R_xlen_t next_child;
  R_xlen_t subtree_rows;
  int postfix;
} deps_plan_node_t;

typedef struct {
  deps_plan_node_t *nodes;
  R_xlen_t *path;
  R_xlen_t *postorder;
  R_xlen_t count;
  R_xlen_t postorder_count;
  R_xlen_t capacity;
} deps_plan_t;

static deps_node_kind_t exact_node_kind(SEXP self,
    R_xlen_t *work_since_interrupt) {
  static const char *const set_classes[] = {"ParamSet", "R6"};
  static const char *const collection_classes[] = {
    "ParamSetCollection", "ParamSet", "R6"
  };
  if (TYPEOF(self) != ENVSXP) {
    return DEPS_NODE_UNKNOWN;
  }
  SEXP classes = PROTECT(Rf_getAttrib(self, R_ClassSymbol));
  if (paradox_domain_exact_string_vector(
      classes,
      set_classes,
      2,
      work_since_interrupt
    )) {
    UNPROTECT(1);
    return DEPS_NODE_SET;
  }
  if (paradox_domain_exact_string_vector(
      classes,
      collection_classes,
      3,
      work_since_interrupt
    )) {
    UNPROTECT(1);
    return DEPS_NODE_COLLECTION;
  }
  UNPROTECT(1);
  return DEPS_NODE_UNKNOWN;
}

static SEXP owned_private_environment(SEXP self) {
  SEXP enclosure = PROTECT(paradox_domain_local_value(
    self,
    ".__enclos_env__"
  ));
  if (TYPEOF(enclosure) != ENVSXP) {
    UNPROTECT(1);
    return R_UnboundValue;
  }
  SEXP private_environment = paradox_domain_local_value(enclosure, "private");
  SEXP result = TYPEOF(private_environment) == ENVSXP
    ? private_environment
    : R_UnboundValue;
  UNPROTECT(1);
  return result;
}

static int exact_data_frame_row_names(SEXP table, R_xlen_t row_count,
    R_xlen_t *work_since_interrupt) {
  SEXP row_names = PROTECT(Rf_getAttrib(table, R_RowNamesSymbol));
  if (row_count > INT_MAX || TYPEOF(row_names) != INTSXP ||
      !paradox_api_has_no_attributes(row_names)) {
    UNPROTECT(1);
    return FALSE;
  }
  /* R's public attribute accessor expands compact data-frame row names to a
   * base integer ALTREP.  The exact ordinary columns already authenticate the
   * row count, and this path never consumes row names, so do not dispatch an
   * indistinguishable third-party ALTREP merely to repeat that bound. */
  if (ALTREP(row_names)) {
    UNPROTECT(1);
    return TRUE;
  }
  if (XLENGTH(row_names) != row_count) {
    UNPROTECT(1);
    return FALSE;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    if (INTEGER_ELT(row_names, row) != (int) row + 1) {
      UNPROTECT(1);
      return FALSE;
    }
  }
  UNPROTECT(1);
  return TRUE;
}

static int has_exact_index_marker(SEXP table, const char *marker_name,
    int require_empty_cache) {
  const SEXP index_symbol = Rf_install("index");
  SEXP index = PROTECT(Rf_getAttrib(table, index_symbol));
  if (TYPEOF(index) != INTSXP || ALTREP(index) || XLENGTH(index) != 0) {
    UNPROTECT(1);
    return FALSE;
  }
  const SEXP marker = Rf_install(marker_name);
  SEXP cache = PROTECT(Rf_getAttrib(index, marker));
  if (TYPEOF(cache) != INTSXP || ALTREP(cache)) {
    UNPROTECT(2);
    return FALSE;
  }
  int exact = paradox_api_has_single_attribute(index, marker_name);
  exact = exact && (!require_empty_cache ||
    (XLENGTH(cache) == 0 && paradox_api_has_no_attributes(cache)));
  UNPROTECT(2);
  return exact;
}

static int has_exact_empty_dependencies_index(SEXP table) {
  return has_exact_index_marker(table, "__on__id", TRUE);
}

static int ascii_string(SEXP string) {
  if (string == NA_STRING || Rf_getCharCE(string) == CE_BYTES) {
    return FALSE;
  }
  const unsigned char *text = (const unsigned char *) CHAR(string);
  for (; *text != '\0'; ++text) {
    if (*text > 0x7fU) {
      return FALSE;
    }
  }
  return TRUE;
}

static int ascii_vector(SEXP strings,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(strings) != STRSXP || ALTREP(strings)) {
    return FALSE;
  }
  const R_xlen_t size = XLENGTH(strings);
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if (!ascii_string(STRING_ELT(strings, index))) {
      return FALSE;
    }
  }
  return TRUE;
}

static int validate_set_names(SEXP sets, SEXP *names,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isObject(sets) ||
      !paradox_params_names_are_only_attribute(sets)) {
    return FALSE;
  }
  const R_xlen_t set_count = XLENGTH(sets);
  SEXP observed_names = PROTECT(Rf_getAttrib(sets, R_NamesSymbol));
  if (TYPEOF(observed_names) != STRSXP || ALTREP(observed_names) ||
      !paradox_api_has_no_attributes(observed_names)) {
    UNPROTECT(1);
    return FALSE;
  }
  const R_xlen_t name_count = XLENGTH(observed_names);
  if (name_count != set_count ||
      !ascii_vector(observed_names, work_since_interrupt)) {
    UNPROTECT(1);
    return FALSE;
  }
  for (R_xlen_t right = 0; right < name_count; ++right) {
    SEXP right_name = STRING_ELT(observed_names, right);
    if (paradox_domain_string_is(right_name, "")) {
      continue;
    }
    for (R_xlen_t left = 0; left < right; ++left) {
      paradox_domain_account_work(work_since_interrupt);
      SEXP left_name = STRING_ELT(observed_names, left);
      if (!paradox_domain_string_is(left_name, "") &&
          paradox_domain_strings_equal(left_name, right_name)) {
        UNPROTECT(1);
        return FALSE;
      }
    }
  }
  *names = observed_names;
  UNPROTECT(1);
  return TRUE;
}

static int exact_params_table(SEXP table, paradox_domain_params_t *params,
    SEXP roots,
    R_xlen_t *work_since_interrupt) {
  R_xlen_t unused_row = 0;
  SEXP selfref = PROTECT(Rf_getAttrib(
    table,
    Rf_install(".internal.selfref")
  ));
  const int exact_header =
    paradox_params_supported_table_attributes(table, FALSE) &&
    TYPEOF(selfref) == EXTPTRSXP;
  UNPROTECT(1);
  if (!exact_header) {
    return FALSE;
  }
  if (!has_exact_index_marker(
      table,
      "__id__cls__grouping",
      FALSE
    ) || !paradox_domain_validate_params(
      table,
      R_NilValue,
      TRUE,
      params,
      &unused_row,
      work_since_interrupt
    )) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, DEPS_ROOT_PARAM_IDS, params->ids);
  return exact_data_frame_row_names(
      table,
      params->row_count,
      work_since_interrupt
    ) && ascii_vector(params->ids, work_since_interrupt);
}

static int exact_dependencies_table(SEXP table,
    paradox_domain_dependencies_t *dependencies,
    SEXP roots,
    R_xlen_t *work_since_interrupt) {
  SEXP selfref = PROTECT(Rf_getAttrib(
    table,
    Rf_install(".internal.selfref")
  ));
  const int exact_header =
    paradox_params_supported_table_attributes(table, FALSE) &&
    (selfref == R_NilValue || TYPEOF(selfref) == EXTPTRSXP);
  UNPROTECT(1);
  if (!exact_header || !paradox_domain_validate_dependencies(
        table,
        dependencies,
        work_since_interrupt
      )) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, DEPS_ROOT_IDS, dependencies->ids);
  SET_VECTOR_ELT(roots, DEPS_ROOT_ON, dependencies->on);
  SET_VECTOR_ELT(
    roots,
    DEPS_ROOT_CONDITIONS,
    dependencies->conditions
  );
  if (!exact_data_frame_row_names(
        table,
        dependencies->row_count,
        work_since_interrupt
      ) || !ascii_vector(dependencies->ids, work_since_interrupt) ||
      !ascii_vector(dependencies->on, work_since_interrupt)) {
    return FALSE;
  }
  SEXP index = Rf_getAttrib(table, Rf_install("index"));
  return index == R_NilValue ||
    (dependencies->row_count == 0 &&
      has_exact_empty_dependencies_index(table));
}

static int exact_ids_wrapper_call(SEXP function) {
  SEXP body = paradox_api_closure_expression(function);
  if (TYPEOF(body) != LANGSXP ||
      CAR(body) != Rf_install(".__ParamSet__ids")) {
    return FALSE;
  }
  static const char *const fixed_arguments[] = {"self", "private", "super"};
  SEXP argument = CDR(body);
  for (R_xlen_t index = 0; index < 3; ++index) {
    SEXP symbol = Rf_install(fixed_arguments[index]);
    if (argument == R_NilValue || TAG(argument) != symbol ||
        CAR(argument) != symbol) {
      return FALSE;
    }
    argument = CDR(argument);
  }
  static const char *const public_arguments[] = {"class", "tags", "any_tags"};
  for (R_xlen_t index = 0; index < 3; ++index) {
    SEXP symbol = Rf_install(public_arguments[index]);
    if (argument == R_NilValue || TAG(argument) != symbol ||
        CAR(argument) != symbol) {
      return FALSE;
    }
    argument = CDR(argument);
  }
  if (argument != R_NilValue) {
    return FALSE;
  }

  SEXP formal = paradox_api_closure_formals(function);
  for (R_xlen_t index = 0; index < 3; ++index) {
    SEXP symbol = Rf_install(public_arguments[index]);
    if (formal == R_NilValue || TAG(formal) != symbol ||
        CAR(formal) != R_NilValue) {
      return FALSE;
    }
    formal = CDR(formal);
  }
  return formal == R_NilValue;
}

static int canonical_ids_method(SEXP self, SEXP private_environment,
    SEXP namespace_environment) {
  SEXP ids_symbol = Rf_install("ids");
  if (!R_existsVarInFrame(self, ids_symbol) ||
      R_BindingIsActive(ids_symbol, self) ||
      !R_BindingIsLocked(ids_symbol, self)) {
    return FALSE;
  }
  SEXP function = PROTECT(paradox_domain_local_value(self, "ids"));
  if (TYPEOF(function) != CLOSXP || !exact_ids_wrapper_call(function)) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP environment = paradox_api_closure_environment(function);
  SEXP super_symbol = Rf_install("super");
  SEXP target_symbol = Rf_install(".__ParamSet__ids");
  const int canonical = TYPEOF(environment) == ENVSXP &&
    paradox_api_parent_environment(environment) == namespace_environment &&
    paradox_domain_local_value(environment, "self") == self &&
    paradox_domain_local_value(environment, "private") ==
      private_environment &&
    !R_existsVarInFrame(environment, super_symbol) &&
    !R_existsVarInFrame(environment, target_symbol);
  UNPROTECT(1);
  return canonical;
}

static SEXP append_node_roots(SEXP *root_plan,
    PROTECT_INDEX root_plan_index, SEXP self, SEXP private_environment,
    SEXP owner) {
  PROTECT(self);
  PROTECT(private_environment);
  PROTECT(owner);
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, DEPS_ROOT_COUNT));
  SET_VECTOR_ELT(roots, DEPS_ROOT_SELF, self);
  SET_VECTOR_ELT(roots, DEPS_ROOT_PRIVATE, private_environment);
  SET_VECTOR_ELT(roots, DEPS_ROOT_OWNER, owner);
  SEXP expanded = PROTECT(Rf_cons(roots, *root_plan));
  REPROTECT(expanded, root_plan_index);
  *root_plan = expanded;
  UNPROTECT(5);
  return roots;
}

static int initialize_node(SEXP self, SEXP private_environment,
    SEXP owner, int postfix, R_xlen_t parent, deps_plan_node_t *node,
    SEXP *root_plan, PROTECT_INDEX root_plan_index,
    R_xlen_t *work_since_interrupt) {
  node->roots = append_node_roots(
    root_plan,
    root_plan_index,
    self,
    private_environment,
    owner
  );
  node->self = self;
  node->private_environment = private_environment;
  node->kind = exact_node_kind(self, work_since_interrupt);
  node->sets = R_NilValue;
  node->set_names = R_NilValue;
  node->owner = owner;
  node->affixed_ids = R_NilValue;
  node->parent = parent;
  node->next_child = 0;
  node->subtree_rows = 0;
  node->postfix = postfix;

  if (node->kind == DEPS_NODE_UNKNOWN ||
      !paradox_domain_owns_private_environment(self, private_environment) ||
      !paradox_params_canonical_active_member(
        self,
        private_environment,
        "deps",
        node->kind == DEPS_NODE_COLLECTION
          ? ".__ParamSetCollection__deps"
          : ".__ParamSet__deps",
        "v",
        node->kind == DEPS_NODE_COLLECTION
          ? ".__ParamSet__deps"
          : NULL,
        work_since_interrupt
      )) {
    return FALSE;
  }

  SEXP params_sexp = paradox_domain_local_value(
    private_environment,
    ".params"
  );
  if (params_sexp == R_UnboundValue) {
    return FALSE;
  }
  SET_VECTOR_ELT(node->roots, DEPS_ROOT_PARAMS, params_sexp);
  SEXP dependencies_sexp = paradox_domain_local_value(
    private_environment,
    ".deps"
  );
  if (dependencies_sexp == R_UnboundValue) {
    return FALSE;
  }
  SET_VECTOR_ELT(node->roots, DEPS_ROOT_TABLE, dependencies_sexp);
  if (!exact_params_table(
      params_sexp,
      &node->params,
      node->roots,
      work_since_interrupt
    ) || !exact_dependencies_table(
      dependencies_sexp,
      &node->dependencies,
      node->roots,
      work_since_interrupt
    )) {
    return FALSE;
  }
  node->subtree_rows = node->dependencies.row_count;

  if (node->kind == DEPS_NODE_SET) {
    return TRUE;
  }

  node->sets = paradox_domain_local_value(private_environment, ".sets");
  if (node->sets == R_UnboundValue) {
    return FALSE;
  }
  SET_VECTOR_ELT(node->roots, DEPS_ROOT_SETS, node->sets);
  SEXP postfix_sexp = paradox_domain_local_value(
    private_environment,
    ".postfix"
  );
  if (postfix_sexp == R_UnboundValue) {
    return FALSE;
  }
  SET_VECTOR_ELT(node->roots, DEPS_ROOT_POSTFIX, postfix_sexp);
  if (!validate_set_names(
      node->sets,
      &node->set_names,
      work_since_interrupt
    )) {
    return FALSE;
  }
  SET_VECTOR_ELT(node->roots, DEPS_ROOT_SET_NAMES, node->set_names);
  if (TYPEOF(postfix_sexp) != LGLSXP || ALTREP(postfix_sexp) ||
      !paradox_api_has_no_attributes(postfix_sexp) ||
      XLENGTH(postfix_sexp) != 1 ||
      LOGICAL_ELT(postfix_sexp, 0) == NA_LOGICAL) {
    return FALSE;
  }
  return TRUE;
}

static void initialize_plan(deps_plan_t *plan) {
  plan->capacity = 8;
  plan->nodes = paradox_temporary_alloc(
    plan->capacity,
    sizeof(*plan->nodes)
  );
  plan->path = paradox_temporary_alloc(
    plan->capacity,
    sizeof(*plan->path)
  );
  plan->postorder = paradox_temporary_alloc(
    plan->capacity,
    sizeof(*plan->postorder)
  );
  plan->count = 0;
  plan->postorder_count = 0;
}

static int reserve_plan_node(deps_plan_t *plan) {
  if (plan->count < plan->capacity) {
    return TRUE;
  }
  if (plan->capacity > R_XLEN_T_MAX / 2) {
    return FALSE;
  }
  const R_xlen_t expanded_capacity = plan->capacity * 2;
  deps_plan_node_t *expanded_nodes = paradox_temporary_alloc(
    expanded_capacity,
    sizeof(*expanded_nodes)
  );
  R_xlen_t *expanded_path = paradox_temporary_alloc(
    expanded_capacity,
    sizeof(*expanded_path)
  );
  R_xlen_t *expanded_postorder = paradox_temporary_alloc(
    expanded_capacity,
    sizeof(*expanded_postorder)
  );
  memcpy(
    expanded_nodes,
    plan->nodes,
    (size_t) plan->count * sizeof(*expanded_nodes)
  );
  memcpy(
    expanded_path,
    plan->path,
    (size_t) plan->count * sizeof(*expanded_path)
  );
  memcpy(
    expanded_postorder,
    plan->postorder,
    (size_t) plan->postorder_count * sizeof(*expanded_postorder)
  );
  plan->nodes = expanded_nodes;
  plan->path = expanded_path;
  plan->postorder = expanded_postorder;
  plan->capacity = expanded_capacity;
  return TRUE;
}

static int affix_lengths_supported(const deps_plan_node_t *node,
    R_xlen_t *work_since_interrupt) {
  const size_t owner_size = strlen(CHAR(node->owner));
  for (R_xlen_t index = 0; index < node->params.row_count; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    const size_t id_size = strlen(CHAR(STRING_ELT(node->params.ids, index)));
    if (owner_size > SIZE_MAX - id_size - 1 ||
        owner_size + id_size + 1 > INT_MAX) {
      return FALSE;
    }
  }
  return TRUE;
}

static int validate_graph(SEXP self, SEXP private_environment,
    SEXP namespace_environment, deps_plan_t *plan, SEXP *root_plan,
    PROTECT_INDEX root_plan_index, R_xlen_t *work_since_interrupt) {
  initialize_plan(plan);
  if (!initialize_node(
      self,
      private_environment,
      R_NilValue,
      FALSE,
      R_XLEN_T_MAX,
      &plan->nodes[0],
      root_plan,
      root_plan_index,
      work_since_interrupt
    ) || plan->nodes[0].kind != DEPS_NODE_COLLECTION) {
    return FALSE;
  }
  plan->count = 1;
  plan->path[0] = 0;
  R_xlen_t depth = 1;

  while (depth != 0) {
    const R_xlen_t node_index = plan->path[depth - 1];
    deps_plan_node_t *node = &plan->nodes[node_index];
    if (node->kind == DEPS_NODE_COLLECTION &&
        node->next_child < XLENGTH(node->sets)) {
      const R_xlen_t child_position = node->next_child;
      SEXP child_self = PROTECT(VECTOR_ELT(node->sets, child_position));
      for (R_xlen_t ancestor = 0; ancestor < depth; ++ancestor) {
        paradox_domain_account_work(work_since_interrupt);
        if (plan->nodes[plan->path[ancestor]].self == child_self) {
          Rf_error("Cyclic ParamSetCollection graph is unsupported");
        }
      }
      SEXP child_private = PROTECT(owned_private_environment(child_self));
      if (child_private == R_UnboundValue || !reserve_plan_node(plan)) {
        UNPROTECT(2);
        return FALSE;
      }
      node = &plan->nodes[node_index];
      SEXP owner = PROTECT(STRING_ELT(node->set_names, child_position));
      const int postfix = LOGICAL_ELT(
        VECTOR_ELT(node->roots, DEPS_ROOT_POSTFIX),
        0
      );
      const R_xlen_t child_index = plan->count;
      if (!initialize_node(
          child_self,
          child_private,
          owner,
          postfix,
          node_index,
          &plan->nodes[child_index],
          root_plan,
          root_plan_index,
          work_since_interrupt
        )) {
        UNPROTECT(3);
        return FALSE;
      }
      UNPROTECT(3);
      ++plan->nodes[node_index].next_child;
      ++plan->count;
      plan->path[depth] = child_index;
      ++depth;
      continue;
    }

    if (node->parent != R_XLEN_T_MAX) {
      if (!paradox_domain_string_is(node->owner, "") &&
          node->subtree_rows != 0 &&
          (!canonical_ids_method(
            node->self,
            node->private_environment,
            namespace_environment
          ) || !affix_lengths_supported(
            node,
            work_since_interrupt
          ))) {
        return FALSE;
      }
      deps_plan_node_t *parent = &plan->nodes[node->parent];
      if (node->subtree_rows > R_XLEN_T_MAX - parent->subtree_rows ||
          node->subtree_rows + parent->subtree_rows > INT_MAX) {
        return FALSE;
      }
      parent->subtree_rows += node->subtree_rows;
    } else if (node->subtree_rows > INT_MAX) {
      return FALSE;
    }
    plan->postorder[plan->postorder_count] = node_index;
    ++plan->postorder_count;
    --depth;
  }
  return plan->postorder_count == plan->count;
}

static SEXP make_affixed_string(SEXP owner, SEXP id, int postfix) {
  PROTECT(owner);
  PROTECT(id);
  const char *owner_text = CHAR(owner);
  const char *id_text = CHAR(id);
  const size_t owner_size = strlen(owner_text);
  const size_t id_size = strlen(id_text);
  const size_t output_size = owner_size + id_size + 1;
  char *buffer = R_alloc(output_size + 1, sizeof(*buffer));
  if (postfix) {
    memcpy(buffer, id_text, id_size);
    buffer[id_size] = '.';
    memcpy(buffer + id_size + 1, owner_text, owner_size);
  } else {
    memcpy(buffer, owner_text, owner_size);
    buffer[owner_size] = '.';
    memcpy(buffer + owner_size + 1, id_text, id_size);
  }
  buffer[output_size] = '\0';
  SEXP result = Rf_mkCharLenCE(buffer, (int) output_size, CE_NATIVE);
  UNPROTECT(2);
  return result;
}

static void build_affix_caches(deps_plan_t *plan,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t node_index = 1; node_index < plan->count; ++node_index) {
    deps_plan_node_t *node = &plan->nodes[node_index];
    if (node->subtree_rows == 0 ||
        paradox_domain_string_is(node->owner, "")) {
      continue;
    }
    SEXP affixed = PROTECT(Rf_allocVector(STRSXP, node->params.row_count));
    for (R_xlen_t index = 0; index < node->params.row_count; ++index) {
      paradox_domain_account_work(work_since_interrupt);
      SET_STRING_ELT(
        affixed,
        index,
        make_affixed_string(
          node->owner,
          STRING_ELT(node->params.ids, index),
          node->postfix
        )
      );
    }
    SET_VECTOR_ELT(node->roots, DEPS_ROOT_AFFIXED_IDS, affixed);
    node->affixed_ids = affixed;
    UNPROTECT(1);
  }
}

static SEXP translate_dependency(const deps_plan_t *plan,
    R_xlen_t node_index, SEXP input,
    R_xlen_t *work_since_interrupt) {
  PROTECT_INDEX current_index;
  SEXP current;
  PROTECT_WITH_INDEX(current = input, &current_index);
  R_xlen_t edge_index = node_index;
  while (plan->nodes[edge_index].parent != R_XLEN_T_MAX) {
    const deps_plan_node_t *edge = &plan->nodes[edge_index];
    if (!paradox_domain_string_is(edge->owner, "")) {
      for (R_xlen_t id_index = 0;
          id_index < edge->params.row_count;
          ++id_index) {
        paradox_domain_account_work(work_since_interrupt);
        if (paradox_domain_strings_equal(
            current,
            STRING_ELT(edge->params.ids, id_index)
          )) {
          REPROTECT(
            current = STRING_ELT(edge->affixed_ids, id_index),
            current_index
          );
          break;
        }
      }
    }
    edge_index = edge->parent;
  }
  UNPROTECT(1);
  return current;
}

static void set_output_attributes(SEXP result, R_xlen_t row_count) {
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, Rf_mkChar("id"));
  SET_STRING_ELT(names, 1, Rf_mkChar("on"));
  SET_STRING_ELT(names, 2, Rf_mkChar("cond"));
  Rf_setAttrib(result, R_NamesSymbol, names);

  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(classes, 0, Rf_mkChar("data.table"));
  SET_STRING_ELT(classes, 1, Rf_mkChar("data.frame"));
  Rf_setAttrib(result, R_ClassSymbol, classes);

  SEXP row_names = PROTECT(Rf_allocVector(INTSXP, row_count == 0 ? 0 : 2));
  if (row_count != 0) {
    SET_INTEGER_ELT(row_names, 0, NA_INTEGER);
    SET_INTEGER_ELT(row_names, 1, -(int) row_count);
  }
  Rf_setAttrib(result, R_RowNamesSymbol, row_names);
  paradox_set_data_table_selfref(result);
  Rf_setAttrib(result, R_NamesSymbol, R_NilValue);
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(3);
}

static SEXP emit_plan(const deps_plan_t *plan, R_xlen_t row_count,
    R_xlen_t *work_since_interrupt) {
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP ids = PROTECT(Rf_allocVector(STRSXP, row_count));
  SEXP on = PROTECT(Rf_allocVector(STRSXP, row_count));
  SEXP conditions = PROTECT(Rf_allocVector(VECSXP, row_count));
  SET_VECTOR_ELT(result, 0, ids);
  SET_VECTOR_ELT(result, 1, on);
  SET_VECTOR_ELT(result, 2, conditions);

  R_xlen_t output_row = 0;
  for (R_xlen_t order = 0; order < plan->postorder_count; ++order) {
    const R_xlen_t node_index = plan->postorder[order];
    const deps_plan_node_t *node = &plan->nodes[node_index];
    for (R_xlen_t source_row = 0;
        source_row < node->dependencies.row_count;
        ++source_row) {
      paradox_domain_account_work(work_since_interrupt);
      if (output_row >= row_count ||
          source_row >= XLENGTH(node->dependencies.ids) ||
          source_row >= XLENGTH(node->dependencies.on) ||
          source_row >= XLENGTH(node->dependencies.conditions)) {
        UNPROTECT(4);
        Rf_error("ParamSetCollection dependency state changed during emission");
      }
      SET_STRING_ELT(
        ids,
        output_row,
        translate_dependency(
          plan,
          node_index,
          STRING_ELT(node->dependencies.ids, source_row),
          work_since_interrupt
        )
      );
      SET_STRING_ELT(
        on,
        output_row,
        translate_dependency(
          plan,
          node_index,
          STRING_ELT(node->dependencies.on, source_row),
          work_since_interrupt
        )
      );
      SEXP source_condition = PROTECT(VECTOR_ELT(
        node->dependencies.conditions,
        source_row
      ));
      SEXP condition = PROTECT(Rf_duplicate(source_condition));
      SET_VECTOR_ELT(conditions, output_row, condition);
      UNPROTECT(2);
      ++output_row;
    }
  }
  if (output_row != row_count) {
    UNPROTECT(4);
    Rf_error("ParamSetCollection dependency state changed during emission");
  }
  set_output_attributes(result, row_count);
  UNPROTECT(4);
  return result;
}

SEXP paradox_param_set_collection_deps(
    SEXP private_environment, SEXP self) {
  R_xlen_t work_since_interrupt = 0;
  SEXP namespace_environment = PROTECT(
    paradox_api_registered_namespace("paradox")
  );
  PROTECT_INDEX root_plan_index;
  SEXP root_plan;
  PROTECT_WITH_INDEX(root_plan = R_NilValue, &root_plan_index);

  deps_plan_t plan;
  if (!validate_graph(
      self,
      private_environment,
      namespace_environment,
      &plan,
      &root_plan,
      root_plan_index,
      &work_since_interrupt
    )) {
    UNPROTECT(2);
    return R_NilValue;
  }

  build_affix_caches(&plan, &work_since_interrupt);
  SEXP result = emit_plan(
    &plan,
    plan.nodes[0].subtree_rows,
    &work_since_interrupt
  );
  UNPROTECT(2);
  return result;
}
