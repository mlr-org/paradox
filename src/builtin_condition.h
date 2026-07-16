#ifndef PARADOX_BUILTIN_CONDITION_H
#define PARADOX_BUILTIN_CONDITION_H

#include "paradox.h"

typedef enum {
  PARADOX_BUILTIN_CONDITION_EQUAL = 0,
  PARADOX_BUILTIN_CONDITION_ANY_OF
} paradox_builtin_condition_kind_t;

/* These helpers authenticate the complete built-in S3 condition surface and
 * admit only the exact callback-free CondEqual/CondAnyOf representation. */
attribute_hidden int paradox_builtin_condition_dispatch_is_canonical(
  SEXP namespace_environment
);
attribute_hidden int paradox_builtin_condition_exact(
  SEXP condition,
  paradox_builtin_condition_kind_t *kind,
  SEXP *rhs,
  R_xlen_t *work_since_interrupt
);

/* Scalar support and comparison are shared by ParamSet$get_values() and the
 * Design dependency planner. The element comparison can translate mixed
 * string encodings; callers requiring a non-allocating execution phase must
 * first admit the complete vector with the column predicate below. */
attribute_hidden int paradox_builtin_condition_scalar_supported(
  SEXP value,
  SEXP rhs
);
attribute_hidden int paradox_builtin_condition_column_supported(
  SEXP column,
  SEXP rhs,
  R_xlen_t *work_since_interrupt
);
attribute_hidden int paradox_builtin_condition_element_matches(
  SEXP values,
  R_xlen_t index,
  SEXP rhs,
  R_xlen_t *work_since_interrupt
);

#endif
