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

/* Refresh one current SHADOW against its origin capsule graph.  A replacement
 * is committed only if the selected SHADOW capsule is still current. */
attribute_hidden SEXP paradox_shadow_refresh_authoritative(
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

/* Authenticate the complete package-private refresh signature without
 * refreshing or otherwise evaluating the origin graph. */
attribute_hidden int paradox_shadow_metadata_is_exact(SEXP core);
/* Carry a SHADOW's derived-cache carrier onto a replacement generation. A
 * field-level capsule replacement rebuilds the external pointer, and a SHADOW
 * without its refresh signature is not a canonical capsule at all. */
attribute_hidden void paradox_shadow_copy_metadata(SEXP source, SEXP target);

#endif
