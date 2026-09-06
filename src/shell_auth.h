#ifndef PARADOX_SHELL_AUTH_H
#define PARADOX_SHELL_AUTH_H

#include "core_state.h"

/* Classify the complete ordinary R6 class vector without dispatch or ALTREP
 * observation. `classes` receives the borrowed raw class attribute. */
attribute_hidden paradox_core_kind_t paradox_param_set_class_kind_raw(
  SEXP self,
  SEXP *classes
);
attribute_hidden int paradox_param_set_assert_values_is_exact(SEXP value);
/* On R 3.6, recursive discovery cannot retrieve active-binding closures.
 * Authenticate an exact built-in current shell and return its rooted capsule
 * authority so the crawler can follow state rather than package-generated
 * active facades. Additive subclasses deliberately remain unclassified. */
attribute_hidden SEXP paradox_builtin_current_core_snapshot(SEXP self);

#endif
