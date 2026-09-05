#include "paradox.h"

#include "core_state.h"
#include "paramset_collection_readers.h"
#include "paramset_params_internal.h"

SEXP paradox_param_set_collection_params(SEXP private_environment, SEXP self) {
  R_xlen_t work_since_interrupt = 0;
  PROTECT_INDEX graph_roots_index;
  SEXP graph_roots;
  PROTECT_WITH_INDEX(graph_roots = R_NilValue, &graph_roots_index);

  /* One graph admission supplies every dynamic column. In particular this
   * never invokes `$deps` or `$values`, and a shared SHADOW is synchronized
   * only once for the complete `$params` operation. */
  paradox_collection_graph_t graph;
  paradox_collection_graph_build(
    private_environment,
    self,
    PARADOX_GRAPH_VALUES | PARADOX_GRAPH_DEPENDENCIES,
    &graph,
    &graph_roots,
    graph_roots_index,
    &work_since_interrupt
  );

  /* Reuse the compact static-table builder against the exact root capsule
   * selected and rooted by graph admission. Loading the core directly avoids
   * both a temporary environment and a redundant capsule lookup. */
  SEXP static_roots = PROTECT(Rf_allocVector(VECSXP, 1));
  paradox_params_state_t state;
  if (!paradox_params_load_core_state_rooted(
      graph.nodes[0].core,
      &state,
      static_roots,
      0,
      &work_since_interrupt
    )) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSetCollection static parameter capsule");
  }

  SEXP result = PROTECT(paradox_params_build_static(
    &state,
    &work_since_interrupt
  ));
  SEXP dependencies = PROTECT(paradox_collection_dependencies_from_graph(
    &graph,
    &work_since_interrupt
  ));
  SEXP values = PROTECT(paradox_collection_values_from_graph(
    &graph,
    FALSE,
    &work_since_interrupt
  ));

  paradox_domain_params_t output_params = state.params;
  output_params.table = result;
  output_params.ids = VECTOR_ELT(result, PARADOX_DOMAIN_ID);
  if (!paradox_params_finish_dynamic(
      result,
      &output_params,
      dependencies,
      values,
      &work_since_interrupt
    )) {
    UNPROTECT(5);
    Rf_error("Corrupt ParamSetCollection dynamic parameter snapshot");
  }
  UNPROTECT(5);
  return result;
}
