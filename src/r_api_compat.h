#ifndef PARADOX_R_API_COMPAT_H
#define PARADOX_R_API_COMPAT_H

#include <stddef.h>

#include "paradox.h"

/* Keep version-dependent old-runtime spellings behind one small facade. R 4.5
 * promoted the native closure accessors below to API. Earlier runtimes retain
 * exact header-declared/exported FORMALS, R_ClosureExpr, and CLOENV exceptions
 * so recursive migration can capture one closure generation without
 * evaluation or raw-layout macros. A directly reached bytecode object still
 * takes the public as.function.default()/body() bridge there. R 4.6 added
 * public raw-attribute iteration; that genuinely irreplaceable older
 * operation is also recorded in the exact exception ledger. */

attribute_hidden SEXP paradox_api_closure_formals(SEXP closure);
attribute_hidden SEXP paradox_api_closure_expression(SEXP closure);
attribute_hidden SEXP paradox_api_bytecode_expression(SEXP bytecode);
attribute_hidden SEXP paradox_api_closure_environment(SEXP closure);
/* Validate the complete closure-formals pairlist and report literal tag
 * membership without allocation. Malformed, dotted, or cyclic formals return
 * false and reset `matches`; a valid closure with no such formal returns true
 * with `matches` false. */
attribute_hidden int paradox_api_closure_formal_matches(
  SEXP closure,
  SEXP sought,
  int *matches
);
attribute_hidden SEXP paradox_api_parent_environment(SEXP environment);

/* Snapshot one base option. R >= 4.5 uses the documented allocation-free
 * Rf_GetOption1 API. Older R declared that entry point but did not yet
 * document it as API, so the compatibility branch evaluates the public
 * base::getOption() spelling instead. Callers must protect the returned value
 * immediately; repeated reads remain responsible for detecting option changes
 * across intervening allocations. */
attribute_hidden SEXP paradox_api_option_snapshot(SEXP symbol);

/* Exact raw stored-attribute predicates.  The one/allow-list forms use the
 * same hard-bounded mapper as untrusted metadata admission, so a malformed or
 * cyclic spine returns false instead of reaching an unbounded count/lookup. */
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
 * R >= 4.6 provides a public iterator. Older supported R versions use the one
 * reviewed ATTRIB exception confined to this facade. The returned value is
 * unprotected and remains owned by `value`. */
attribute_hidden SEXP paradox_api_raw_attribute(SEXP value, SEXP symbol);

/* Select class metadata through one allocation-free raw-attribute scan bounded
 * to 64 cells, without inheritance dispatch or ALTREP observation. A missing
 * class is a valid ordinary result with `R_NilValue`; malformed, duplicate,
 * overlong, attributed, S4, or structural-ALTREP class metadata returns false. */
attribute_hidden int paradox_api_ordinary_class_snapshot(
  SEXP value,
  SEXP *classes
);
attribute_hidden int paradox_api_ordinary_class_contains(
  SEXP classes,
  const char *label
);
/* Allocation-free literal inheritance query over one bounded ordinary class
 * snapshot. TRUE means the class metadata was structurally admissible and
 * `matches` reports membership; FALSE means it was malformed and `matches`
 * is reset to false. This never invokes S3 inheritance or ALTREP methods. */
attribute_hidden int paradox_api_ordinary_class_matches(
  SEXP value,
  const char *label,
  int *matches
);

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
 * active/delayed binding. Although R_getVar is public beginning with R 4.5,
 * using it before the R 4.6 binding classifier could force a delayed binding.
 * Pre-4.6 DSOs must therefore exclude it; the R >= 4.6 branch uses it only
 * after R_GetBindingType has authenticated a direct value. */
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

/* Bounded raw-attribute iteration for caller-owned or otherwise untrusted
 * presentation metadata.  A zero limit is an allocation-free no-attributes
 * predicate.  The function returns false, without walking farther, when a
 * valid-cell spine exceeds `limit`; callers validate every delivered tag and
 * value.  On R >= 4.6 a guaranteed non-NULL stop value bounds cyclic and
 * overlong pairlist spines, while the public R_mapAttrib API owns raw-cell
 * decoding. Older R uses the same hard edge count around its one reviewed
 * ATTRIB compatibility loop and can additionally reject a non-list cell. */
attribute_hidden int paradox_api_map_bounded_stored_attributes(
  SEXP value,
  R_xlen_t limit,
  paradox_api_attribute_callback_t callback,
  void *data,
  R_xlen_t *count
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

/*
 * R 4.5's compiled-code policy classifies PRENV, PRVALUE, and
 * R_PromiseExpr as non-API even though that release's headers still expose
 * them. Keep the compatibility snapshot strictly below that boundary. The
 * R 4.5 crawler fails closed when it reaches a promise; R >= 4.6 instead uses
 * the public binding/dots inspection API where a promise is still attached to
 * such a cell.
 */
#if R_VERSION < R_Version(4, 5, 0)
typedef struct {
  SEXP expression;
  SEXP environment;
  SEXP value;
  int forced;
} paradox_api_promise_snapshot_t;

/* Inspect a promise without evaluating it. `value` is R_UnboundValue for an
 * unforced promise; a forced value is returned exactly as stored. This narrow
 * R 3.6--4.4 facade uses the accessors declared and exported by those headers
 * and is authenticated by the reviewed exception ledger. R 4.5 fails
 * recursive migration closed on every reached PROMSXP. R >= 4.6 instead uses
 * its public binding and dots APIs; a PROMSXP reached outside such a binding is
 * still opaque. */
attribute_hidden void paradox_api_promise_snapshot(
  SEXP promise,
  paradox_api_promise_snapshot_t *snapshot
);
#endif

#endif
