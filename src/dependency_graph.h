#ifndef PARADOX_DEPENDENCY_GRAPH_H
#define PARADOX_DEPENDENCY_GRAPH_H

#include "builtin_condition.h"
#include "paramset_domain_common.h"

/*
 * Operation-local plan for one already admitted canonical dependency table.
 * All SEXP fields are borrowed from the caller-rooted table. The remaining
 * arrays use R's temporary allocation arena and are valid until the enclosing
 * native call returns.
 */
typedef struct {
  R_xlen_t child;
  /* R_XLEN_T_MAX denotes an admitted dangling parent. Such an edge remains in
   * the child's incoming range but contributes no topological indegree and can
   * never satisfy the child. */
  R_xlen_t parent;
  paradox_builtin_condition_kind_t kind;
  SEXP rhs;
} paradox_dependency_graph_edge_t;

typedef struct {
  R_xlen_t parameter_count;
  R_xlen_t dependency_count;
  paradox_dependency_graph_edge_t *edges;
  R_xlen_t *incoming_count;
  R_xlen_t *incoming_start;
  R_xlen_t *incoming_edges;
  R_xlen_t *topological_order;
} paradox_dependency_graph_plan_t;

/*
 * Map dependency IDs to parameter rows, admit every exact built-in Condition,
 * build stable incoming-edge ranges, reject unknown children and cycles, and
 * retain the parameter-order-stable topological order. An unknown parent is an
 * admitted dangling edge and is represented by R_XLEN_T_MAX.
 */
attribute_hidden void paradox_dependency_graph_plan_build(
  SEXP parameter_ids,
  const paradox_domain_dependencies_t *dependencies,
  paradox_dependency_graph_plan_t *plan,
  R_xlen_t *work_since_interrupt
);

/*
 * Recompute a topological order over an existing plan. A NULL branch-factor
 * vector preserves parameter order among ready nodes. Otherwise the ready node
 * with the smallest branch factor wins, with parameter order as the stable
 * tie-break. The caller supplies `plan->parameter_count` result slots.
 */
attribute_hidden void paradox_dependency_graph_topological_order(
  const paradox_dependency_graph_plan_t *plan,
  const R_xlen_t *branch_factors,
  R_xlen_t *result,
  R_xlen_t *work_since_interrupt
);

#endif
