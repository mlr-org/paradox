#ifndef PARADOX_R_API_COMPAT_H
#define PARADOX_R_API_COMPAT_H

#include <stddef.h>

#include "paradox.h"

/* Keep all version-dependent access to R objects behind a small public-API
 * facade.  R 4.5 added direct closure/environment accessors and R 4.6 added
 * binding and attribute inspection APIs.  Older supported R releases use
 * equivalent base-R calls instead of depending on R's private object layout. */

attribute_hidden SEXP paradox_api_closure_expression(SEXP closure);
attribute_hidden SEXP paradox_api_closure_formals(SEXP closure);
attribute_hidden SEXP paradox_api_closure_environment(SEXP closure);
attribute_hidden SEXP paradox_api_parent_environment(SEXP environment);
attribute_hidden SEXP paradox_api_registered_namespace(const char *name);

attribute_hidden int paradox_api_has_no_attributes(SEXP value);
attribute_hidden int paradox_api_has_single_attribute(
  SEXP value,
  const char *name
);
attribute_hidden int paradox_api_has_only_attributes(
  SEXP value,
  const char *const *allowed_names,
  size_t allowed_count
);

/* Return a direct, non-active binding without forcing delayed computation.
 * R_UnboundValue means absent, active, missing, or otherwise unsuitable. */
attribute_hidden SEXP paradox_api_local_value(
  SEXP environment,
  SEXP symbol
);

/* Retrieve a local binding with ordinary R evaluation semantics. */
attribute_hidden SEXP paradox_api_evaluated_local_value(
  SEXP environment,
  SEXP symbol
);

#endif
