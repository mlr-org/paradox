#ifndef PARADOX_PARAMETER_SUGGESTION_H
#define PARADOX_PARAMETER_SUGGESTION_H

#include "paradox.h"

/*
 * Build the public unknown-parameter diagnostic and, when a nearby candidate
 * exists, append the native "Did you mean ...?" hint. `location` is
 * package-owned ASCII inserted after "not available" (for example,
 * " in ParamSetShadow"). `base_period` preserves the historical spelling
 * when no suggestion is found; a suggestion always gets a separating period.
 */
attribute_hidden SEXP paradox_parameter_unavailable_diagnostic(
  SEXP id,
  SEXP candidate_ids,
  const char *location,
  int base_period
);

#endif
