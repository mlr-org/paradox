#ifndef PARADOX_PARAMSET_SHADOW_H
#define PARADOX_PARAMSET_SHADOW_H

#include "paradox.h"

/* One policy, one sentence: a dependency may not span a view's visible/hidden
 * boundary, in either direction, no matter whether the origin already carries
 * the row or the caller is declaring it through the view. The two `%s` pairs
 * are the role and the ID of each end, so both messages name the direction.
 * A dependency on a parameter that does not exist at all is not a crossing;
 * see `filter_dependencies()`. */
#define PARADOX_SHADOW_CROSSING_MESSAGE \
  "Dependency of %s '%s' on %s '%s' crosses the ParamSetShadow boundary"

/* Build a complete SHADOW capsule from a static SHADOW template and the exact
 * BASE or COLLECTION origin edge. Fixed callback-carrier factories are
 * resolved from the locked package namespace only when a snapshot is built. */
attribute_hidden SEXP paradox_param_set_shadow_core_new(
  SEXP template_core,
  SEXP origin
);
attribute_hidden SEXP paradox_param_set_shadow_construct(
  SEXP origin,
  SEXP shadowed
);
attribute_hidden SEXP paradox_param_set_shadow_origin(
  SEXP private_environment,
  SEXP self
);

/* Refresh one current SHADOW against its origin capsule graph.  A replacement
 * is committed only if the selected SHADOW capsule is still current. */
attribute_hidden SEXP paradox_shadow_refresh_authoritative(
  SEXP self,
  SEXP private_environment
);

/* The committing refresh for a caller that has just healed the complete
 * origin subtree (the post-order capsule-graph heal): the origin is resolved
 * and rebuilt read-only instead of being re-healed through every contained
 * shadow, which is what made shared alternating shadow/collection graphs
 * exponential to construct. */
attribute_hidden SEXP paradox_shadow_refresh_authoritative_prehealed(
  SEXP self,
  SEXP private_environment
);

/* Build and validate the generation an ordinary refresh would select, but do
 * not install it. Used by all-roots-before-commit migration preflight. */
attribute_hidden SEXP paradox_shadow_preview_authoritative(
  SEXP self,
  SEXP private_environment
);

/* Resolve the one capsule-owned edge without consulting a parallel private
 * field.  Returns R_UnboundValue for malformed or non-SHADOW cores. */
attribute_hidden SEXP paradox_shadow_origin_from_core(SEXP core);

/* Select the complete package-private refresh signature without refreshing
 * or otherwise evaluating the origin graph. Returns R_UnboundValue unless the
 * carrier has its exact ordinary alternating shell/core shape. */
attribute_hidden SEXP paradox_shadow_metadata_signature(SEXP core);
/* Own the exact alternating entries of an already selected signature after
 * the destination allocation, or return R_NilValue for malformed input. */
attribute_hidden SEXP paradox_shadow_signature_content_snapshot(
  SEXP signature
);
/* Allocation-free terminal receipt: the core still names the same exact
 * carrier and every alternating shell/generation entry is unchanged. */
attribute_hidden int paradox_shadow_signature_receipt_is_current(
  SEXP core,
  SEXP signature,
  SEXP content_snapshot
);
/* Shape-only predicate retained for cold capsule admission call sites. */
attribute_hidden int paradox_shadow_metadata_is_exact(SEXP core);
/* Carry a SHADOW's derived-cache carrier onto a replacement generation. A
 * field-level capsule replacement rebuilds the external pointer, and a SHADOW
 * without its refresh signature is not a canonical capsule at all. */
attribute_hidden void paradox_shadow_copy_metadata(SEXP source, SEXP target);

#endif
