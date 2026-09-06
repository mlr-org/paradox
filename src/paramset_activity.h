#ifndef PARADOX_PARAMSET_ACTIVITY_H
#define PARADOX_PARAMSET_ACTIVITY_H

#include "paradox.h"

/*
 * One callback-free activity kernel serves point checks, stored-value reads,
 * presence checks, and constraint filtering.  Callers own structural
 * admission and map their graph-specific IDs to root parameter rows once.
 * The kernel then evaluates every dependency row exactly once, recursively
 * respecting parent activity and consulting a recorded default only when the
 * basis does not contain that parent.
 */

typedef enum {
  PARADOX_ACTIVITY_SATISFIED_VALUE = 0,
  PARADOX_ACTIVITY_SATISFIED_DEFAULT,
  PARADOX_ACTIVITY_SATISFIED_CHILD_TOKEN,
  PARADOX_ACTIVITY_SATISFIED_PARENT_TOKEN,
  PARADOX_ACTIVITY_PARENT_INACTIVE,
  PARADOX_ACTIVITY_PARENT_ABSENT,
  PARADOX_ACTIVITY_VALUE_MISMATCH,
  PARADOX_ACTIVITY_VALUE_UNSUPPORTED,
  PARADOX_ACTIVITY_DEFAULT_MISMATCH,
  PARADOX_ACTIVITY_DEFAULT_UNSUPPORTED
} paradox_activity_reason_t;

typedef struct {
  R_xlen_t parameter_count;
  SEXP defaults;
  SEXP values;
  const R_xlen_t *value_by_parameter;
  R_xlen_t dependency_count;
  const R_xlen_t *dependency_child;
  const R_xlen_t *dependency_parent;
  SEXP const *dependency_rhs;
} paradox_activity_plan_t;

typedef struct {
  unsigned char *active;
  /* Optional: callers that only filter values may omit per-edge diagnostics. */
  paradox_activity_reason_t *reasons;
} paradox_activity_result_t;

attribute_hidden int paradox_activity_reason_is_satisfied(
  paradox_activity_reason_t reason
);

attribute_hidden void paradox_activity_evaluate(
  const paradox_activity_plan_t *plan,
  paradox_activity_result_t *result,
  R_xlen_t *work_since_interrupt
);

#endif
