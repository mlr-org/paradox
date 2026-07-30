#include <limits.h>

#include "paradox.h"

#include "paramset_collection_readers.h"
#include "paramset_domain_common.h"

static const char *const dependency_column_names[] = {"id", "on", "cond"};
static const char *const assertion_state_names[] = {"params", "has_deps"};

SEXP paradox_collection_translate_dependency_id(
    const paradox_collection_graph_t *graph,
    R_xlen_t node_index,
    SEXP input,
    R_xlen_t *work_since_interrupt) {
  SEXP current = input;
  while (graph->nodes[node_index].parent != R_XLEN_T_MAX) {
    const paradox_collection_graph_node_t *node = &graph->nodes[node_index];
    const paradox_collection_graph_node_t *parent =
      &graph->nodes[node->parent];
    const R_xlen_t row = paradox_domain_find_string(
      node->params.ids,
      current,
      work_since_interrupt
    );
    if (row != R_XLEN_T_MAX) {
      if (node->parent_param_start > parent->params.row_count ||
          row >= parent->params.row_count - node->parent_param_start) {
        Rf_error("Internal error: invalid dependency translation path");
      }
      current = STRING_ELT(
        parent->params.ids,
        node->parent_param_start + row
      );
    }
    node_index = node->parent;
  }
  return current;
}

SEXP paradox_collection_dependencies_from_graph(
    const paradox_collection_graph_t *graph,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t row_count = graph->nodes[0].subtree_dependencies;
  if (row_count > INT_MAX) {
    Rf_error("ParamSetCollection dependency result exceeds data.frame limits");
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP ids = PROTECT(Rf_allocVector(STRSXP, row_count));
  SEXP on = PROTECT(Rf_allocVector(STRSXP, row_count));
  SEXP conditions = PROTECT(Rf_allocVector(VECSXP, row_count));
  SET_VECTOR_ELT(result, 0, ids);
  SET_VECTOR_ELT(result, 1, on);
  SET_VECTOR_ELT(result, 2, conditions);

  R_xlen_t output = 0;
  for (R_xlen_t order = 0; order < graph->postorder_count; ++order) {
    const R_xlen_t node_index = graph->postorder[order];
    const paradox_collection_graph_node_t *node = &graph->nodes[node_index];
    for (R_xlen_t row = 0; row < node->dependencies.row_count; ++row) {
      paradox_account_work(work_since_interrupt);
      if (output >= row_count) {
        UNPROTECT(4);
        Rf_error("Internal error: dependency output exceeded its snapshot");
      }
      SET_STRING_ELT(
        ids,
        output,
        paradox_collection_translate_dependency_id(
          graph,
          node_index,
          STRING_ELT(node->dependencies.ids, row),
          work_since_interrupt
        )
      );
      SET_STRING_ELT(
        on,
        output,
        paradox_collection_translate_dependency_id(
          graph,
          node_index,
          STRING_ELT(node->dependencies.on, row),
          work_since_interrupt
        )
      );
      SEXP condition = PROTECT(Rf_duplicate(
        VECTOR_ELT(node->dependencies.conditions, row)
      ));
      SET_VECTOR_ELT(conditions, output, condition);
      UNPROTECT(1);
      ++output;
    }
  }
  if (output != row_count) {
    UNPROTECT(4);
    Rf_error("Internal error: incomplete dependency output snapshot");
  }
  paradox_domain_finish_plain_table(result, dependency_column_names, 3, row_count);
  UNPROTECT(4);
  return result;
}

SEXP paradox_param_set_has_dependencies(SEXP private_environment, SEXP self) {
  R_xlen_t work_since_interrupt = 0;
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("ParamSet method called with a foreign private environment");
  }

  PROTECT_INDEX core_index;
  SEXP core;
  PROTECT_WITH_INDEX(
    core = paradox_core_from_private(private_environment),
    &core_index
  );
  if (core == R_UnboundValue) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }

  paradox_core_kind_t kind = paradox_core_kind(core);
  if (kind == PARADOX_CORE_COLLECTION) {
    PROTECT_INDEX roots_index;
    SEXP roots;
    PROTECT_WITH_INDEX(roots = R_NilValue, &roots_index);
    paradox_collection_graph_t graph;
    paradox_collection_graph_build(
      private_environment,
      self,
      &graph,
      &roots,
      roots_index,
      &work_since_interrupt
    );
    const int result = graph.nodes[0].subtree_dependencies != 0;
    UNPROTECT(2);
    return Rf_ScalarLogical(result);
  }

  if (!paradox_core_is_verified(core)) {
    REPROTECT(
      core = paradox_core_refresh(self, private_environment),
      core_index
    );
    kind = paradox_core_kind(core);
  }
  if (kind != PARADOX_CORE_BASE && kind != PARADOX_CORE_SHADOW) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet dependency capsule kind");
  }

  SEXP state = R_ExternalPtrProtected(core);
  paradox_domain_dependencies_t dependencies;
  if (!paradox_domain_validate_dependencies(
      VECTOR_ELT(state, PARADOX_CORE_DEPS),
      &dependencies,
      &work_since_interrupt
    )) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet dependency capsule");
  }
  const int result = dependencies.row_count != 0;
  UNPROTECT(1);
  return Rf_ScalarLogical(result);
}

SEXP paradox_param_set_assertion_state(SEXP private_environment, SEXP self) {
  R_xlen_t work_since_interrupt = 0;
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("ParamSet method called with a foreign private environment");
  }

  /*
   * Select the node kind without allocating. A collection needs its complete
   * graph snapshot because dependencies below children contribute to the
   * answer. BASE and SHADOW roots instead retain and validate one exact
   * capsule generation; sending either through the collection-only graph
   * builder would reject a perfectly valid root.
   */
  PROTECT_INDEX core_index;
  SEXP core;
  PROTECT_WITH_INDEX(
    core = paradox_core_from_private(private_environment),
    &core_index
  );
  if (core == R_UnboundValue) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet assertion capsule");
  }

  SEXP params_table;
  int has_dependencies;
  paradox_core_kind_t kind = paradox_core_kind(core);
  if (kind == PARADOX_CORE_COLLECTION) {
    UNPROTECT(1);
    PROTECT_INDEX roots_index;
    SEXP roots;
    PROTECT_WITH_INDEX(roots = R_NilValue, &roots_index);
    paradox_collection_graph_t graph;
    paradox_collection_graph_build(
      private_environment,
      self,
      &graph,
      &roots,
      roots_index,
      &work_since_interrupt
    );
    params_table = graph.nodes[0].params.table;
    has_dependencies = graph.nodes[0].subtree_dependencies != 0;

    SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
    SEXP names = PROTECT(paradox_domain_character_vector(
      assertion_state_names,
      2
    ));
    SET_VECTOR_ELT(result, 0, params_table);
    SET_VECTOR_ELT(result, 1, Rf_ScalarLogical(has_dependencies));
    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(3);
    return result;
  }

  /*
   * The canonical root table is immutable capsule state and remains rooted by
   * the returned carrier. R consumes it only as this assertion's decision
   * source; no second live graph observation can produce a chimera.
   */
  if (!paradox_core_is_verified(core)) {
    REPROTECT(
      core = paradox_core_refresh(self, private_environment),
      core_index
    );
    kind = paradox_core_kind(core);
  }
  if (kind != PARADOX_CORE_BASE && kind != PARADOX_CORE_SHADOW) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet assertion capsule kind");
  }
  SEXP state = paradox_core_payload(core);
  paradox_domain_params_t params;
  paradox_domain_dependencies_t dependencies;
  R_xlen_t unused_row = 0;
  if (!paradox_domain_validate_params(
      VECTOR_ELT(state, PARADOX_CORE_PARAMS),
      R_NilValue,
      TRUE,
      &params,
      &unused_row,
      &work_since_interrupt
    ) || !paradox_domain_validate_dependencies(
      VECTOR_ELT(state, PARADOX_CORE_DEPS),
      &dependencies,
      &work_since_interrupt
    )) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet assertion capsule state");
  }
  params_table = params.table;
  has_dependencies = dependencies.row_count != 0;

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP names = PROTECT(paradox_domain_character_vector(
    assertion_state_names,
    2
  ));
  SET_VECTOR_ELT(result, 0, params_table);
  SET_VECTOR_ELT(result, 1, Rf_ScalarLogical(has_dependencies));
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(3);
  return result;
}

SEXP paradox_param_set_collection_deps(SEXP private_environment, SEXP self) {
  R_xlen_t work_since_interrupt = 0;
  PROTECT_INDEX roots_index;
  SEXP roots;
  PROTECT_WITH_INDEX(roots = R_NilValue, &roots_index);
  paradox_collection_graph_t graph;
  paradox_collection_graph_build(
    private_environment,
    self,
    &graph,
    &roots,
    roots_index,
    &work_since_interrupt
  );
  SEXP plain = PROTECT(paradox_collection_dependencies_from_graph(
    &graph,
    &work_since_interrupt
  ));
  SEXP result = PROTECT(paradox_dependency_public_facade(plain));
  UNPROTECT(3);
  return result;
}
