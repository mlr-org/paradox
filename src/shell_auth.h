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

#endif
