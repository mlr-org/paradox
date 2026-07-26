#ifndef PARADOX_R_API_COMPAT_H
#define PARADOX_R_API_COMPAT_H

#include <stddef.h>

#include "paradox.h"

/* Keep the few version-dependent public API spellings behind one small
 * facade. Writing R Extensions documents FORMALS as the pre-4.5 backport for
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

/* Return the stored attribute value without R's special row.names expansion.
 * R >= 4.6 provides a public iterator. Older supported R versions use the
 * documented ATTRIB traversal compatibility spelling already confined to this
 * facade. The returned value is unprotected and remains owned by `value`. */
attribute_hidden SEXP paradox_api_raw_attribute(SEXP value, SEXP symbol);

/* The R_NO_REMAP declarations for SET_RAW_ELT() / SET_COMPLEX_ELT(), and the
 * IDENT_USE_CLOENV name, entered the installed API after R 3.6. These
 * version-independent spellings retain the current API on R >= 4.2 and use
 * public vector access on fresh ordinary destinations on older R. They are
 * inline so current hot loops pay no compatibility call overhead. */
static inline void paradox_api_set_raw_elt(
    SEXP value, R_xlen_t index, Rbyte element) {
#if R_VERSION >= R_Version(4, 2, 0)
  SET_RAW_ELT(value, index, element);
#else
  RAW(value)[index] = element;
#endif
}

static inline void paradox_api_set_complex_elt(
    SEXP value, R_xlen_t index, Rcomplex element) {
#if R_VERSION >= R_Version(4, 2, 0)
  SET_COMPLEX_ELT(value, index, element);
#else
  COMPLEX(value)[index] = element;
#endif
}

static inline int paradox_api_identical_default_flags(void) {
#if R_VERSION >= R_Version(4, 2, 0)
  return IDENT_USE_CLOENV;
#else
  return 16;
#endif
}

/* Select one ordinary frame binding without forcing a promise. R >= 4.6 has
 * the documented experimental binding-classification API. R 3.6--4.5 has no
 * public equivalent; the compatibility branch uses the declared/exported
 * Rf_findVarInFrame and explicitly rejects PROMSXP at this plain-value
 * boundary. This is the sole reviewed non-public frame-binding lookup in
 * shipped source: its ledger policy is pre-4.6-only, older-runtime DSOs must
 * contain it, and current-R DSOs must prove it absent. Detached promise access
 * is a separate, equally narrow exception documented below. Neither exception
 * is a general CRAN allowlist. Both branches are tested against pinned
 * headers, real runtimes, and delayed-binding regressions. After an ordinary-
 * class frame is admitted, neither selection branch allocates or evaluates an
 * active/delayed binding. */
attribute_hidden SEXP paradox_api_plain_binding_snapshot(
  SEXP environment,
  SEXP symbol
);
attribute_hidden SEXP paradox_api_optional_plain_binding_snapshot(
  SEXP environment,
  SEXP symbol
);
attribute_hidden int paradox_api_frame_has_binding(
  SEXP environment,
  SEXP symbol
);
/* Allocation-free ordinary-frame receipt scan for optional shell-topology
 * bindings. Before R 4.2, a frame containing any active/locked binding is
 * rejected rather than inspected through R internals or the evaluator. */
attribute_hidden int paradox_api_frame_has_binding_scan(
  SEXP environment,
  SEXP symbol
);

/* Retrieve an active binding's closure where R exposes the operation.
 * R 3.6 has no public or header-declared accessor; callers receive
 * R_UnboundValue and must fail closed without invoking the binding. */
attribute_hidden SEXP paradox_api_active_binding_function(
  SEXP environment,
  SEXP symbol
);

/* Allocation-free ordinary-frame second-scan spelling. Keeping it distinct
 * documents the simultaneous generation barrier even though both versioned
 * implementations can inspect an ordinary binding without allocation or
 * forcing. */
attribute_hidden SEXP paradox_api_plain_binding_scan(
  SEXP environment,
  SEXP symbol
);

typedef void (*paradox_api_attribute_callback_t)(
  SEXP tag,
  SEXP value,
  void *data
);

/* Enumerate the raw stored attribute pairlist without row.names expansion.
 * R >= 4.6 uses its public mapper; ATTRIB remains confined to this facade as
 * the documented compatibility spelling on R 3.6--4.5. The count and mapping
 * deliberately exclude the virtual names reported for tagged pairlists by
 * R_getAttribCount(): pairlist tags are not stored attributes. */
attribute_hidden R_xlen_t paradox_api_stored_attribute_count(SEXP value);
attribute_hidden void paradox_api_map_stored_attributes(
  SEXP value,
  paradox_api_attribute_callback_t callback,
  void *data
);

/* Return the stored pre-4.6 frame cell without forcing a promise. R 4.6 has
 * public binding-classification and delayed/forced-binding accessors, so the
 * graph walker uses those APIs directly there.  On older supported runtimes,
 * this narrow facade is the only route to the promise object required by the
 * version-independent promise snapshot below. */
#if R_VERSION < R_Version(4, 6, 0)
attribute_hidden SEXP paradox_api_stored_binding_snapshot(
  SEXP environment,
  SEXP symbol
);
#endif

typedef struct {
  SEXP expression;
  SEXP environment;
  SEXP value;
  int forced;
} paradox_api_promise_snapshot_t;

/* Inspect a promise without evaluating it. `value` is R_UnboundValue for an
 * unforced promise; a forced value is returned exactly as stored. This narrow
 * R 3.6--4.5 facade uses the accessors declared and exported by those headers
 * and is authenticated by the reviewed exception ledger. R >= 4.6 instead
 * uses its public binding and dots APIs; a PROMSXP reached outside such a
 * binding is opaque because strict headers expose no detached-promise API. */
#if R_VERSION < R_Version(4, 6, 0)
attribute_hidden void paradox_api_promise_snapshot(
  SEXP promise,
  paradox_api_promise_snapshot_t *snapshot
);
#endif

#endif
