#ifndef PARADOX_PARAMSET_BULK_SHELL_INTERNAL_H
#define PARADOX_PARAMSET_BULK_SHELL_INTERNAL_H

#include "paradox.h"

/* Package-internal transaction pieces shared only with the specialized
 * Sampler1DUnif bulk constructor.  They deliberately do not form a general
 * R6 cloning interface. */
attribute_hidden SEXP paradox_r6_generator_snapshot(SEXP generator);

attribute_hidden int paradox_r6_generator_matches(
  SEXP generator,
  SEXP snapshot,
  SEXP names
);

attribute_hidden SEXP paradox_param_set_bulk_prepare(
  SEXP generator,
  SEXP plans,
  R_xlen_t *count
);

attribute_hidden int paradox_param_set_bulk_build_shells(
  SEXP result,
  SEXP private_environments,
  R_xlen_t count
);

attribute_hidden SEXP paradox_param_set_bulk_generator_names(SEXP generator);

attribute_hidden int paradox_param_set_bulk_generator_matches(
  SEXP generator,
  SEXP names
);

attribute_hidden int paradox_param_set_bulk_tokens_match(
  SEXP tokens,
  int sampler_safe
);

attribute_hidden int paradox_param_set_bulk_destinations_ready(
  SEXP result,
  SEXP private_environments,
  R_xlen_t count
);

attribute_hidden int paradox_param_set_bulk_commit(
  SEXP result,
  SEXP private_environments,
  SEXP tokens
);

#endif
