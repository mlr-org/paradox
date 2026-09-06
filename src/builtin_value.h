#ifndef PARADOX_BUILTIN_VALUE_H
#define PARADOX_BUILTIN_VALUE_H

#include "paradox.h"

typedef enum {
  PARADOX_BUILTIN_DOMAIN_UNKNOWN = 0,
  PARADOX_BUILTIN_DOMAIN_DBL,
  PARADOX_BUILTIN_DOMAIN_INT,
  PARADOX_BUILTIN_DOMAIN_FCT,
  PARADOX_BUILTIN_DOMAIN_LGL,
  PARADOX_BUILTIN_DOMAIN_UTY
} paradox_builtin_domain_kind_t;

typedef struct {
  paradox_builtin_domain_kind_t kind;
  double lower;
  double upper;
  double tolerance;
  SEXP levels;
  SEXP special_values;
} paradox_builtin_value_spec_t;

typedef enum {
  PARADOX_BUILTIN_VALUE_OK = 0,
  PARADOX_BUILTIN_VALUE_WRONG_TYPE,
  PARADOX_BUILTIN_VALUE_WRONG_LENGTH,
  PARADOX_BUILTIN_VALUE_MISSING,
  PARADOX_BUILTIN_VALUE_NOT_INTEGERISH,
  PARADOX_BUILTIN_VALUE_BELOW_LOWER,
  PARADOX_BUILTIN_VALUE_ABOVE_UPPER,
  PARADOX_BUILTIN_VALUE_NOT_IN_LEVELS,
  PARADOX_BUILTIN_VALUE_NOT_ATOMIC_SCALAR,
  PARADOX_BUILTIN_VALUE_NULL_FACTOR
} paradox_builtin_value_failure_t;

typedef struct {
  paradox_builtin_value_failure_t failure;
  double number;
  double canonical_number;
  double diagnostic_bound;
  int special;
} paradox_builtin_value_result_t;

/* Test whether one admitted special-value list contains `value`. Built-in
 * typed Domains retain structural identity for ordinary leaves, but an S4
 * leaf is special only by exact pointer identity. ParamUty treats every leaf
 * as opaque and therefore retains structural identity for S4 values too. */
attribute_hidden int paradox_builtin_special_values_contain(
  paradox_builtin_domain_kind_t kind,
  SEXP special_values,
  SEXP value,
  R_xlen_t *work_since_interrupt
);

/* Classify one already snapshotted built-in value. The ordinary no-special
 * numeric/logical valid path allocates nothing and invokes no R callback;
 * cross-encoding factor comparison and structural special-value identity may
 * use R's temporary allocation. When `respect_special_values` is true,
 * exact/structural special-value admission happens before typed validation.
 * ParamUty ordinary values are structurally valid here; its optional callback
 * remains an operation-level concern. */
attribute_hidden paradox_builtin_value_result_t paradox_builtin_value_check(
  const paradox_builtin_value_spec_t *spec,
  SEXP value,
  int respect_special_values,
  R_xlen_t *work_since_interrupt
);

/* Format one classified failure as an informative, checkmate-style scalar
 * diagnostic prefixed by `id`. Formatting is deliberately failure-only and
 * does not call checkmate or re-run semantic validation. */
attribute_hidden SEXP paradox_builtin_value_diagnostic(
  SEXP id,
  const paradox_builtin_value_spec_t *spec,
  SEXP value,
  const paradox_builtin_value_result_t *result
);

#endif
