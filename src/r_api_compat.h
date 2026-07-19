#ifndef PARADOX_R_API_COMPAT_H
#define PARADOX_R_API_COMPAT_H

#include <stddef.h>

#include "paradox.h"

/* Keep the few version-dependent public API spellings behind one small
 * facade.  Writing R Extensions documents FORMALS as the pre-4.5 backport for
 * R_ClosureFormals and ATTRIB traversal as the pre-4.6 attribute-inspection
 * backport. */

attribute_hidden SEXP paradox_api_closure_formals(SEXP closure);

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

/* Select one ordinary frame binding without forcing a promise. R >= 4.6 has
 * the documented experimental binding-classification API. R 4.3--4.5 has no
 * public equivalent; the compatibility branch uses the declared/exported
 * Rf_findVarInFrame and
 * explicitly rejects PROMSXP. This is the sole reviewed non-public R API
 * exception in shipped source: its ledger policy is pre-4.6-only, older-runtime
 * DSOs must contain it, and current-R DSOs must prove it absent. It is not a
 * general CRAN allowlist. Both branches are tested against pinned headers,
 * real runtimes, and delayed-binding regressions. Neither branch allocates or
 * evaluates an active/delayed binding. */
attribute_hidden SEXP paradox_api_plain_binding_snapshot(
  SEXP environment,
  SEXP symbol
);

/* Allocation-free second-scan spelling. Keeping it distinct documents the
 * simultaneous generation barrier even though both versioned implementations
 * can inspect an ordinary binding without allocation or forcing. */
attribute_hidden SEXP paradox_api_plain_binding_scan(
  SEXP environment,
  SEXP symbol
);

#endif
