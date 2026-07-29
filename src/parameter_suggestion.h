#ifndef PARADOX_PARAMETER_SUGGESTION_H
#define PARADOX_PARAMETER_SUGGESTION_H

#include "paradox.h"

/*
 * Build the public unknown-parameter diagnostic and, when a nearby candidate
 * exists, append the native "Did you mean ...?" hint. `location` is
 * package-owned ASCII inserted after "not available" (for example,
 * " in ParamSetShadow"). The result carries no terminating punctuation unless
 * a suggestion clause ends it: it is a `check_*` fragment, and terminating it
 * belongs to the assertion wrapper, see `paradox_charsxp_ends_sentence()`.
 */
attribute_hidden SEXP paradox_parameter_unavailable_diagnostic(
  SEXP id,
  SEXP candidate_ids,
  const char *location
);

#endif
