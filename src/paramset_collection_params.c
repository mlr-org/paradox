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
    &graph,
    &graph_roots,
    graph_roots_index,
    &work_since_interrupt
  );

  /* Reuse the existing compact static-table builder against the exact root
   * capsule selected by the graph admission. A one-binding package-owned
   * environment prevents a reentrant capsule replacement from making this
   * helper observe a newer generation. */
  SEXP snapshot_private = PROTECT(R_NewEnv(R_EmptyEnv, TRUE, 1));
  Rf_defineVar(Rf_install(".core"), graph.nodes[0].core, snapshot_private);
  SEXP static_roots = PROTECT(Rf_allocVector(VECSXP, 1));
  paradox_params_state_t state;
  if (!paradox_params_load_private_state_rooted(
      snapshot_private,
      &state,
      static_roots,
      0,
      &work_since_interrupt
    )) {
    UNPROTECT(3);
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
    UNPROTECT(6);
    Rf_error("Corrupt ParamSetCollection dynamic parameter snapshot");
  }
  UNPROTECT(6);
  return result;
}
