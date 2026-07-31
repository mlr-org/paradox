#include <string.h>

#include "paramset_activity.h"

#include "builtin_condition.h"
#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

int paradox_activity_reason_is_satisfied(
    paradox_activity_reason_t reason) {
  return reason == PARADOX_ACTIVITY_SATISFIED_VALUE ||
    reason == PARADOX_ACTIVITY_SATISFIED_DEFAULT ||
    reason == PARADOX_ACTIVITY_SATISFIED_CHILD_TOKEN ||
    reason == PARADOX_ACTIVITY_SATISFIED_PARENT_TOKEN;
}

static int has_recorded_default(SEXP value) {
  /*
   * The enclosing canonical parameter-table admission has already proved the
   * exact package-owned NoDefault shape.  Activity only distinguishes that
   * marker from an opaque semantic default; it must not inspect the latter
   * unless the built-in Condition comparator admits it.
   */
  int marker = FALSE;
  return !paradox_api_ordinary_class_matches(
      value,
      "NoDefault",
      &marker
    ) || !marker;
}

static int activity_value_is_tune_token(SEXP value) {
  int token = FALSE;
  if (!paradox_api_ordinary_class_matches(
      value,
      "TuneToken",
      &token
    )) {
    Rf_error(
      "Dependency value class metadata must be ordinary and bounded"
    );
  }
  return token;
}

static paradox_activity_reason_t compare_operand(
    SEXP value, SEXP rhs, int from_default,
    R_xlen_t *work_since_interrupt) {
  if (!paradox_builtin_condition_scalar_supported(value, rhs)) {
    /* An ordinary scalar of a type the right-hand side can never equal does
     * not satisfy the Condition; reporting it as an unsupported operand shape
     * would contradict `condition_test()`, which is the same comparator. */
    if (paradox_builtin_condition_scalar_type_mismatch(value, rhs)) {
      return from_default
        ? PARADOX_ACTIVITY_DEFAULT_MISMATCH
        : PARADOX_ACTIVITY_VALUE_MISMATCH;
    }
    return from_default
      ? PARADOX_ACTIVITY_DEFAULT_UNSUPPORTED
      : PARADOX_ACTIVITY_VALUE_UNSUPPORTED;
  }
  if (value == R_NilValue || !paradox_builtin_condition_element_matches(
      value,
      0,
      rhs,
      work_since_interrupt
    )) {
    return from_default
      ? PARADOX_ACTIVITY_DEFAULT_MISMATCH
      : PARADOX_ACTIVITY_VALUE_MISMATCH;
  }
  return from_default
    ? PARADOX_ACTIVITY_SATISFIED_DEFAULT
    : PARADOX_ACTIVITY_SATISFIED_VALUE;
}

void paradox_activity_evaluate(const paradox_activity_plan_t *plan,
    paradox_activity_result_t *result,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t parameter_count = plan->parameter_count;
  const R_xlen_t dependency_count = plan->dependency_count;
  if (TYPEOF(plan->defaults) != VECSXP ||
      XLENGTH(plan->defaults) != parameter_count ||
      TYPEOF(plan->values) != VECSXP ||
      result->active == NULL) {
    Rf_error("Internal error: malformed ParamSet activity plan");
  }

  /*
   * Dependency-free reads are overwhelmingly common.  Keep their historical
   * O(p) mask fill and avoid all topology/DFS scratch allocation on the
   * default get_values(remove_dependencies = TRUE) path.
   */
  if (dependency_count == 0) {
    for (R_xlen_t parameter = 0;
        parameter < parameter_count;
        ++parameter) {
      result->active[parameter] = TRUE;
    }
    return;
  }

  R_xlen_t *first_dependency = paradox_temporary_alloc(
    parameter_count == 0 ? 1 : parameter_count,
    sizeof(*first_dependency)
  );
  R_xlen_t *next_dependency = paradox_temporary_alloc(
    dependency_count == 0 ? 1 : dependency_count,
    sizeof(*next_dependency)
  );
  R_xlen_t *cursor = paradox_temporary_alloc(
    parameter_count == 0 ? 1 : parameter_count,
    sizeof(*cursor)
  );
  R_xlen_t *stack = paradox_temporary_alloc(
    parameter_count == 0 ? 1 : parameter_count,
    sizeof(*stack)
  );
  unsigned char *state = paradox_temporary_alloc(
    parameter_count == 0 ? 1 : parameter_count,
    sizeof(*state)
  );

  memset(
    state,
    0,
    (size_t) (parameter_count == 0 ? 1 : parameter_count)
  );
  for (R_xlen_t parameter = 0;
      parameter < parameter_count;
      ++parameter) {
    result->active[parameter] = TRUE;
    first_dependency[parameter] = R_XLEN_T_MAX;
  }
  /*
   * Prepending in reverse preserves the capsule's deterministic dependency
   * row order within each child.  Diagnostics can consequently select the
   * first failed row without reevaluating its Condition.
   */
  for (R_xlen_t offset = dependency_count; offset > 0; --offset) {
    const R_xlen_t dependency = offset - 1;
    const R_xlen_t child = plan->dependency_child[dependency];
    if (child >= parameter_count) {
      Rf_error("Internal error: invalid ParamSet dependency target");
    }
    next_dependency[dependency] = first_dependency[child];
    first_dependency[child] = dependency;
  }

  R_xlen_t depth = 0;
  for (R_xlen_t root = 0; root < parameter_count; ++root) {
    if (state[root] == 2) continue;
    stack[depth++] = root;
    while (depth != 0) {
      const R_xlen_t child = stack[depth - 1];
      if (state[child] == 0) {
        state[child] = 1;
        cursor[child] = first_dependency[child];
      }

      const R_xlen_t dependency = cursor[child];
      if (dependency == R_XLEN_T_MAX) {
        state[child] = 2;
        --depth;
        continue;
      }

      paradox_account_work(work_since_interrupt);
      const R_xlen_t parent = plan->dependency_parent[dependency];
      if (parent != R_XLEN_T_MAX && parent >= parameter_count) {
        Rf_error("Internal error: invalid ParamSet dependency parent");
      }
      if (parent != R_XLEN_T_MAX && state[parent] == 0) {
        if (depth >= parameter_count) {
          Rf_error("Corrupt ParamSet dependency graph contains a cycle");
        }
        stack[depth++] = parent;
        continue;
      }
      if (parent != R_XLEN_T_MAX && state[parent] == 1) {
        Rf_error("Corrupt ParamSet dependency graph contains a cycle");
      }

      /*
       * TuneToken children skip dependency feasibility, but only after the
       * structural walk has proved the dependency graph acyclic.  This keeps
       * cycle admission independent of a transient evaluation basis.
       */
      const R_xlen_t child_value = plan->value_by_parameter[child];
      if (child_value != R_XLEN_T_MAX &&
          activity_value_is_tune_token(
            VECTOR_ELT(plan->values, child_value)
          )) {
        if (result->reasons != NULL) {
          result->reasons[dependency] =
            PARADOX_ACTIVITY_SATISFIED_CHILD_TOKEN;
        }
        cursor[child] = next_dependency[dependency];
        continue;
      }
      if (parent == R_XLEN_T_MAX) {
        if (result->reasons != NULL) {
          result->reasons[dependency] = PARADOX_ACTIVITY_PARENT_ABSENT;
        }
        result->active[child] = FALSE;
        cursor[child] = next_dependency[dependency];
        continue;
      }
      if (!result->active[parent]) {
        if (result->reasons != NULL) {
          result->reasons[dependency] =
            PARADOX_ACTIVITY_PARENT_INACTIVE;
        }
        result->active[child] = FALSE;
        cursor[child] = next_dependency[dependency];
        continue;
      }

      const R_xlen_t parent_value = plan->value_by_parameter[parent];
      paradox_activity_reason_t reason;
      if (parent_value != R_XLEN_T_MAX) {
        SEXP value = VECTOR_ELT(plan->values, parent_value);
        reason = activity_value_is_tune_token(value)
          ? PARADOX_ACTIVITY_SATISFIED_PARENT_TOKEN
          : compare_operand(
              value,
              plan->dependency_rhs[dependency],
              FALSE,
              work_since_interrupt
            );
      } else {
        SEXP value = VECTOR_ELT(plan->defaults, parent);
        reason = has_recorded_default(value)
          ? compare_operand(
              value,
              plan->dependency_rhs[dependency],
              TRUE,
              work_since_interrupt
            )
          : PARADOX_ACTIVITY_PARENT_ABSENT;
      }
      if (result->reasons != NULL) {
        result->reasons[dependency] = reason;
      }
      if (!paradox_activity_reason_is_satisfied(reason)) {
        result->active[child] = FALSE;
      }
      cursor[child] = next_dependency[dependency];
    }
  }
}
