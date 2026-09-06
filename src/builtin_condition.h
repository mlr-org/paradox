#ifndef PARADOX_BUILTIN_CONDITION_H
#define PARADOX_BUILTIN_CONDITION_H

#include "paradox.h"

typedef enum {
  PARADOX_BUILTIN_CONDITION_EQUAL = 0,
  PARADOX_BUILTIN_CONDITION_ANY_OF
} paradox_builtin_condition_kind_t;

attribute_hidden int paradox_builtin_condition_exact(
  SEXP condition,
  paradox_builtin_condition_kind_t *kind,
  SEXP *rhs,
  R_xlen_t *work_since_interrupt
);

/* Admit an exact built-in Condition at a public native boundary. Its atomic
 * right-hand side is observed once into ordinary rooted storage before the
 * operation snapshot; this is also the stable/base ALTREP admission point.
 * The returned snapshot is unprotected. R_NilValue denotes malformed input
 * and may leave `*kind` unspecified. Callers must test that sentinel
 * immediately, then protect a successful result before any allocating
 * operation. Besides owning the result in time, that ordering preserves the
 * conditional output contract for static analyzers. */
attribute_hidden SEXP paradox_builtin_condition_admit(
  SEXP condition,
  paradox_builtin_condition_kind_t *kind,
  R_xlen_t *work_since_interrupt
);

/* Return a fully detached exact built-in Condition.  The result owns its
 * list shell, names/classes/format metadata, and an ordinary snapshot of the
 * RHS.  R_UnboundValue denotes malformed built-in structure. */
attribute_hidden SEXP paradox_builtin_condition_snapshot(
  SEXP condition,
  R_xlen_t *work_since_interrupt
);

/* Two-phase terminal receipt for the exact source paired with a canonical
 * result from `paradox_builtin_condition_snapshot()`.  The first entry owns
 * the sole admitted stable-ALTREP Length observation; after all such calls
 * finish, the second entry is allocation- and callback-free. */
attribute_hidden int paradox_builtin_condition_snapshot_lengths_current(
  SEXP source,
  SEXP snapshot
);
attribute_hidden int paradox_builtin_condition_snapshot_is_current(
  SEXP source,
  SEXP snapshot
);

/* Scalar admission and element comparison are shared by the ParamSet
 * activity kernel (paramset_activity.c), the quantile/grid planner, and the
 * Design dependency planner. Mixed non-byte string encodings are compared
 * through the same native translating equality. */
attribute_hidden int paradox_builtin_condition_scalar_supported(
  SEXP value,
  SEXP rhs
);
/* TRUE when the predicate above refused an ordinary scalar leaf only because
 * the right-hand side can never equal its type: an unsatisfied comparison
 * rather than an operand the comparator cannot inspect. */
attribute_hidden int paradox_builtin_condition_scalar_type_mismatch(
  SEXP value,
  SEXP rhs
);
attribute_hidden int paradox_builtin_condition_element_matches(
  SEXP values,
  R_xlen_t index,
  SEXP rhs,
  R_xlen_t *work_since_interrupt
);

attribute_hidden SEXP paradox_condition_test_builtin(
  SEXP condition,
  SEXP x
);

#endif
