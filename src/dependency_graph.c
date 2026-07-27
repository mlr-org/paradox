#include <string.h>

#include "dependency_graph.h"

#include "r_utils.h"

static int dependency_strings_equal(SEXP left, SEXP right) {
  return left == right || (left != NA_STRING && right != NA_STRING &&
    paradox_domain_strings_equal(left, right));
}

static R_xlen_t dependency_find_string(SEXP values, SEXP sought,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t count = XLENGTH(values);
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if (dependency_strings_equal(STRING_ELT(values, index), sought)) {
      return index;
    }
  }
  return R_XLEN_T_MAX;
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
  R_xlen_t *indegree = paradox_temporary_alloc(
    parameter_count == 0 ? 1 : parameter_count,
    sizeof(*indegree)
  );
  unsigned char *emitted = paradox_temporary_alloc(
    parameter_count == 0 ? 1 : parameter_count,
    sizeof(*emitted)
  );
  for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
    indegree[parameter] = 0;
    emitted[parameter] = FALSE;
  }
  for (R_xlen_t edge = 0; edge < dependency_count; ++edge) {
    paradox_domain_account_work(work_since_interrupt);
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
    }
  }

  for (R_xlen_t output = 0; output < parameter_count; ++output) {
    R_xlen_t selected = R_XLEN_T_MAX;
    for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
      paradox_domain_account_work(work_since_interrupt);
      if (emitted[parameter] || indegree[parameter] != 0) {
        continue;
      }
      if (selected == R_XLEN_T_MAX || (branch_factors != NULL &&
          branch_factors[parameter] < branch_factors[selected])) {
        selected = parameter;
      }
    }
    if (selected == R_XLEN_T_MAX) {
      Rf_error("ParamSet dependency graph contains a cycle");
    }

    emitted[selected] = TRUE;
    result[output] = selected;
    for (R_xlen_t edge = 0; edge < dependency_count; ++edge) {
      paradox_domain_account_work(work_since_interrupt);
      if (plan->edges[edge].parent != selected) {
        continue;
      }
      const R_xlen_t child = plan->edges[edge].child;
      if (child >= parameter_count || indegree[child] == 0) {
        Rf_error("Corrupt ParamSet dependency topology");
      }
      --indegree[child];
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

  for (R_xlen_t edge = 0; edge < dependency_count; ++edge) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t child = dependency_find_string(
      parameter_ids,
      STRING_ELT(dependencies->ids, edge),
      work_since_interrupt
    );
    const R_xlen_t parent = dependency_find_string(
      parameter_ids,
      STRING_ELT(dependencies->on, edge),
      work_since_interrupt
    );
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
