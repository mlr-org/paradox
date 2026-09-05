#include <string.h>

#include "dependency_graph.h"

#include "r_utils.h"

/* Kahn's algorithm needs a priority queue, rather than a FIFO: a newly ready
 * parameter may precede one that was already ready. Grid planning additionally
 * prioritizes smaller realized axes, retaining parameter order for ties. */
static int ready_precedes(R_xlen_t left, R_xlen_t right,
    const R_xlen_t *branch_factors) {
  if (branch_factors != NULL &&
      branch_factors[left] != branch_factors[right]) {
    return branch_factors[left] < branch_factors[right];
  }
  return left < right;
}

static void ready_push(R_xlen_t *heap, R_xlen_t *size,
    R_xlen_t parameter, const R_xlen_t *branch_factors) {
  R_xlen_t position = (*size)++;
  while (position != 0) {
    const R_xlen_t parent = (position - 1) / 2;
    if (!ready_precedes(parameter, heap[parent], branch_factors)) break;
    heap[position] = heap[parent];
    position = parent;
  }
  heap[position] = parameter;
}

static R_xlen_t ready_pop(R_xlen_t *heap, R_xlen_t *size,
    const R_xlen_t *branch_factors) {
  const R_xlen_t result = heap[0];
  const R_xlen_t last = heap[--(*size)];
  R_xlen_t position = 0;
  /* This bound both identifies internal heap nodes and keeps 2 * position + 1
   * representable without assuming a narrower parameter-count limit. */
  while (position < *size / 2) {
    R_xlen_t child = 2 * position + 1;
    if (child + 1 < *size &&
        ready_precedes(heap[child + 1], heap[child], branch_factors)) {
      ++child;
    }
    if (!ready_precedes(heap[child], last, branch_factors)) break;
    heap[position] = heap[child];
    position = child;
  }
  heap[position] = last;
  return result;
}

void paradox_dependency_graph_topological_order(
    const paradox_dependency_graph_plan_t *plan,
    const R_xlen_t *branch_factors, R_xlen_t *result,
    R_xlen_t *work_since_interrupt) {
  if (plan == NULL || result == NULL || work_since_interrupt == NULL) {
    Rf_error("Internal error: missing dependency graph plan");
    /* `Rf_error()` does not return; keep static analyzers on that path. */
    return;
  }

  const R_xlen_t parameter_count = plan->parameter_count;
  const R_xlen_t dependency_count = plan->dependency_count;
  if (dependency_count == 0 && branch_factors == NULL) {
    for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
      paradox_account_work(work_since_interrupt);
      result[parameter] = parameter;
    }
    return;
  }

  /* Each edge is linked into its parent's outgoing list once. The incoming
   * ranges retained by the plan serve condition evaluation; these temporary
   * outgoing links serve ordering and count parallel predicates separately. */
  R_xlen_t *indegree = paradox_temporary_alloc(
    parameter_count == 0 ? 1 : parameter_count,
    sizeof(*indegree)
  );
  R_xlen_t *first_edge = paradox_temporary_alloc(
    parameter_count == 0 ? 1 : parameter_count,
    sizeof(*first_edge)
  );
  R_xlen_t *next_edge = paradox_temporary_alloc(
    dependency_count == 0 ? 1 : dependency_count,
    sizeof(*next_edge)
  );
  R_xlen_t *ready = paradox_temporary_alloc(
    parameter_count == 0 ? 1 : parameter_count,
    sizeof(*ready)
  );
  for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
    paradox_account_work(work_since_interrupt);
    indegree[parameter] = 0;
    first_edge[parameter] = R_XLEN_T_MAX;
  }
  for (R_xlen_t edge = 0; edge < dependency_count; ++edge) {
    paradox_account_work(work_since_interrupt);
    const R_xlen_t child = plan->edges[edge].child;
    const R_xlen_t parent = plan->edges[edge].parent;
    if (child >= parameter_count ||
        (parent != R_XLEN_T_MAX && parent >= parameter_count)) {
      Rf_error("Corrupt ParamSet dependency topology");
    }
    if (parent != R_XLEN_T_MAX) {
      if (indegree[child] == R_XLEN_T_MAX) {
        Rf_error("ParamSet dependency graph is too large");
      }
      ++indegree[child];
      next_edge[edge] = first_edge[parent];
      first_edge[parent] = edge;
    }
  }

  R_xlen_t ready_count = 0;
  for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
    paradox_account_work(work_since_interrupt);
    if (indegree[parameter] == 0) {
      ready_push(ready, &ready_count, parameter, branch_factors);
    }
  }
  for (R_xlen_t output = 0; output < parameter_count; ++output) {
    paradox_account_work(work_since_interrupt);
    if (ready_count == 0) {
      Rf_error("ParamSet dependency graph contains a cycle");
    }

    const R_xlen_t selected = ready_pop(ready, &ready_count, branch_factors);
    result[output] = selected;
    for (R_xlen_t edge = first_edge[selected]; edge != R_XLEN_T_MAX;
        edge = next_edge[edge]) {
      paradox_account_work(work_since_interrupt);
      const R_xlen_t child = plan->edges[edge].child;
      if (child >= parameter_count || indegree[child] == 0) {
        Rf_error("Corrupt ParamSet dependency topology");
      }
      --indegree[child];
      if (indegree[child] == 0) {
        ready_push(ready, &ready_count, child, branch_factors);
      }
    }
  }
}

void paradox_dependency_graph_plan_build(SEXP parameter_ids,
    const paradox_domain_dependencies_t *dependencies,
    paradox_dependency_graph_plan_t *plan,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(parameter_ids) != STRSXP || dependencies == NULL ||
      plan == NULL || work_since_interrupt == NULL) {
    Rf_error("Internal error: malformed dependency graph input");
    /* `Rf_error()` does not return; keep static analyzers on that path. */
    return;
  }

  const R_xlen_t parameter_count = XLENGTH(parameter_ids);
  const R_xlen_t dependency_count = dependencies->row_count;
  plan->parameter_count = parameter_count;
  plan->dependency_count = dependency_count;
  plan->edges = paradox_temporary_alloc(
    dependency_count == 0 ? 1 : dependency_count,
    sizeof(*plan->edges)
  );
  plan->incoming_count = paradox_temporary_alloc(
    parameter_count == 0 ? 1 : parameter_count,
    sizeof(*plan->incoming_count)
  );
  for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
    plan->incoming_count[parameter] = 0;
  }

  /* Tiny graphs need no index allocation. For wider graphs, resolve both
   * endpoints through the common encoding-aware ID map rather than scanning
   * the schema twice per edge. Its keys belong to the caller's rooted snapshot. */
  const int indexed = parameter_count >= 32 && dependency_count >= 8;
  paradox_domain_id_map_t id_map;
  if (indexed && paradox_domain_id_map_init(parameter_ids, &id_map) !=
      PARADOX_DOMAIN_ID_MAP_OK) {
    Rf_error("Corrupt or oversized ParamSet dependency identifiers");
  }
  for (R_xlen_t edge = 0; edge < dependency_count; ++edge) {
    paradox_account_work(work_since_interrupt);
    R_xlen_t child = R_XLEN_T_MAX;
    R_xlen_t parent = R_XLEN_T_MAX;
    if (indexed) {
      (void) paradox_domain_id_map_find(
        &id_map, STRING_ELT(dependencies->ids, edge),
        &child, work_since_interrupt
      );
      (void) paradox_domain_id_map_find(
        &id_map, STRING_ELT(dependencies->on, edge),
        &parent, work_since_interrupt
      );
    } else {
      child = paradox_domain_find_string(
        parameter_ids, STRING_ELT(dependencies->ids, edge), work_since_interrupt
      );
      parent = paradox_domain_find_string(
        parameter_ids, STRING_ELT(dependencies->on, edge), work_since_interrupt
      );
    }
    if (child == R_XLEN_T_MAX) {
      Rf_error("Design dependencies refer to an unknown child parameter");
    }

    paradox_builtin_condition_kind_t kind;
    SEXP rhs = R_NilValue;
    if (!paradox_builtin_condition_exact(
        VECTOR_ELT(dependencies->conditions, edge),
        &kind,
        &rhs,
        work_since_interrupt
      )) {
      Rf_error("Corrupt dependency Condition in ParamSet capsule");
    }
    plan->edges[edge] = (paradox_dependency_graph_edge_t) {
      .child = child,
      .parent = parent,
      .kind = kind,
      .rhs = rhs
    };
    if (plan->incoming_count[child] == R_XLEN_T_MAX) {
      Rf_error("ParamSet dependency graph is too large");
    }
    ++plan->incoming_count[child];
  }

  if (parameter_count == R_XLEN_T_MAX) {
    Rf_error("ParamSet dependency graph is too large");
  }
  plan->incoming_start = paradox_temporary_alloc(
    parameter_count + 1,
    sizeof(*plan->incoming_start)
  );
  plan->incoming_edges = paradox_temporary_alloc(
    dependency_count == 0 ? 1 : dependency_count,
    sizeof(*plan->incoming_edges)
  );
  plan->incoming_start[0] = 0;
  for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
    if (plan->incoming_count[parameter] >
        R_XLEN_T_MAX - plan->incoming_start[parameter]) {
      Rf_error("ParamSet dependency graph is too large");
    }
    plan->incoming_start[parameter + 1] =
      plan->incoming_start[parameter] + plan->incoming_count[parameter];
  }
  if (plan->incoming_start[parameter_count] != dependency_count) {
    Rf_error("Corrupt ParamSet dependency graph counts");
  }

  R_xlen_t *incoming_cursor = paradox_temporary_alloc(
    parameter_count == 0 ? 1 : parameter_count,
    sizeof(*incoming_cursor)
  );
  for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
    incoming_cursor[parameter] = plan->incoming_start[parameter];
  }
  for (R_xlen_t edge = 0; edge < dependency_count; ++edge) {
    const R_xlen_t child = plan->edges[edge].child;
    plan->incoming_edges[incoming_cursor[child]] = edge;
    ++incoming_cursor[child];
  }

  plan->topological_order = paradox_temporary_alloc(
    parameter_count == 0 ? 1 : parameter_count,
    sizeof(*plan->topological_order)
  );
  paradox_dependency_graph_topological_order(
    plan,
    NULL,
    plan->topological_order,
    work_since_interrupt
  );
}
