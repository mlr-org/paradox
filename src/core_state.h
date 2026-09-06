#ifndef PARADOX_CORE_STATE_H
#define PARADOX_CORE_STATE_H

#include <stdint.h>

#include "paradox.h"

typedef enum {
  PARADOX_CORE_NONE = 0,
  PARADOX_CORE_BASE = 1,
  PARADOX_CORE_COLLECTION = 2,
  PARADOX_CORE_SHADOW = 3
} paradox_core_kind_t;

typedef enum {
  PARADOX_CORE_PARAMS = 0,
  PARADOX_CORE_VALUES,
  PARADOX_CORE_TAGS,
  PARADOX_CORE_DEPS,
  PARADOX_CORE_TRAFOS,
  PARADOX_CORE_EXTRA_TRAFO,
  PARADOX_CORE_CONSTRAINT,
  PARADOX_CORE_SETS,
  PARADOX_CORE_TRANSLATION,
  PARADOX_CORE_POSTFIX,
  PARADOX_CORE_EDGES,
  PARADOX_CORE_FIELD_COUNT
} paradox_core_field_t;

/* Intern the fixed payload labels at DLL initialization. */
attribute_hidden void paradox_core_intern_field_names(void);

/* `.edges` records the derivation inputs a COLLECTION or SHADOW capsule needs
 * to recompute its own derived schema from its current children. It is
 * `NULL` for BASE, whose schema is owned rather than derived.
 *
 * COLLECTION: one row per edge, in `.sets` order.
 *   `cores`      the exact child capsule generation the flatten was built
 *                from; a child whose current generation is a different object
 *                with a different `.params`/`.tags`/`.trafos` slice makes this
 *                node stale;
 *   `tag_sets`, `tag_params`   the per-edge construction flags, which cannot
 *                be recovered from the flattened tables (an empty child added
 *                with `tag_params = TRUE` contributes no row until it grows).
 *
 * SHADOW: the origin's schema slice the visible tables were derived from, and
 * the hidden ID set. Retaining `shadowed` is what makes "origin minus hidden"
 * a live view rather than a construction-time freeze.
 *
 * Both kinds additionally carry `tag_override`, the node's own answer for the
 * IDs a `$tags<-` assignment named. Tags are the one derived field a derived
 * node may own outright, so the override is what makes that assignment survive
 * re-derivation without writing through to the sets it is derived from. It is
 * `NULL` when the node has never been assigned tags. */
enum {
  PARADOX_COLLECTION_EDGE_CORES = 0,
  PARADOX_COLLECTION_EDGE_TAG_SETS,
  PARADOX_COLLECTION_EDGE_TAG_PARAMS,
  PARADOX_COLLECTION_EDGE_TAG_OVERRIDE,
  PARADOX_COLLECTION_EDGE_FIELD_COUNT
};
enum {
  PARADOX_SHADOW_EDGE_PARAMS = 0,
  PARADOX_SHADOW_EDGE_TAGS,
  PARADOX_SHADOW_EDGE_TRAFOS,
  PARADOX_SHADOW_EDGE_SHADOWED,
  PARADOX_SHADOW_EDGE_TAG_OVERRIDE,
  PARADOX_SHADOW_EDGE_FIELD_COUNT
};
/* Layout of one `tag_override`: the IDs it governs, and the (id, tag) rows it
 * asserts for them. An ID it governs with no row simply has no tags. */
enum {
  PARADOX_TAG_OVERRIDE_IDS = 0,
  PARADOX_TAG_OVERRIDE_TAGS,
  PARADOX_TAG_OVERRIDE_FIELD_COUNT
};

/* Which semantic change a capsule installation carries. Derived state is
 * invalidated by `SCHEMA`; a SHADOW additionally projects live values, so it
 * is invalidated by `STATE` as well. A cache refresh -- a Shadow rebuild or a
 * collection re-flatten -- installs the state the graph already had and is
 * therefore `NONE`. */
typedef enum {
  PARADOX_CORE_CHANGE_NONE = 0,
  PARADOX_CORE_CHANGE_STATE = 1,
  PARADOX_CORE_CHANGE_SCHEMA = 2
} paradox_core_change_t;

/* A core is an external pointer whose protected slot is the entire
 * serializable state and whose tag records both the schema version and node
 * kind. No finalizer or unmanaged allocation is involved: the address slot
 * carries only a session-local verification stamp (see `core_state.c`), never
 * a pointer, and R restores it as `NULL` on unserialize. */
attribute_hidden int paradox_core_is_valid(SEXP core);
attribute_hidden int paradox_core_has_exact_schema(SEXP core);
/* Payload-level twin of the check above: the exact ordinary eleven-field
 * `.params`..`.edges` capsule state schema. The single validator shared by
 * every unit that inspects a detached payload directly. */
attribute_hidden int paradox_core_state_exact_schema(SEXP state);
/* A canonical installed capsule additionally has the one exact carrier
 * attribute shape allowed for its node kind. SHADOW metadata contents are
 * authenticated separately because temporary clone templates intentionally
 * have no refresh signature yet. */
attribute_hidden int paradox_core_is_canonical(SEXP core);
attribute_hidden paradox_core_kind_t paradox_core_kind(SEXP core);
attribute_hidden SEXP paradox_core_payload(SEXP core);
/* Required package-owned capsule lookup used by hot semantic entry points.
 * On old R a missing binding is corrupt and may raise at the binding API. */
attribute_hidden SEXP paradox_core_from_private(SEXP private_environment);
/* Absence-tolerant classifier for arbitrary candidate shells. It preserves
 * the current-R R_UnboundValue result on R 3.6--4.1 without making every
 * authenticated hot lookup evaluate base::exists(). */
attribute_hidden SEXP paradox_core_from_private_optional(
  SEXP private_environment
);
/* Records that a capsule installation changed semantic state, so every
 * derived capsule in the session must revalidate before it is next read.
 * Called by the capsule installers, never by a cache refresh. */
attribute_hidden void paradox_core_note_change(paradox_core_change_t change);
/* The current all-changes epoch. A builder that cannot prove its inputs stayed
 * put across an intervening R evaluation compares this before and after. */
attribute_hidden uintptr_t paradox_core_state_epoch_value(void);
/* Record that this generation's derived state agreed with its children at the
 * current epoch. A capsule carries no stamp until this is called, which is
 * also the state every unserialized or externally duplicated capsule falls
 * back to. */
attribute_hidden void paradox_core_stamp_verified(SEXP core);
/* TRUE when no capsule installation since this core was verified can have
 * invalidated it. A BASE capsule owns its schema and is always verified. */
attribute_hidden int paradox_core_is_verified(SEXP core);
/* TRUE when a COLLECTION's cached flatten no longer agrees with the schema of
 * its current children. Read-only boundaries that may not install a
 * replacement use it to say so plainly instead of reporting corruption. */
attribute_hidden int paradox_collection_flatten_is_stale(SEXP core);
/* The single semantic entry gate. Brings the capsule graph below
 * `private_environment` back in agreement with its children -- re-flattening
 * stale COLLECTION nodes bottom-up and refreshing SHADOW nodes -- and returns
 * the generation now bound at `private_environment`. Cheap and allocation-free
 * when the graph is already verified. */
attribute_hidden SEXP paradox_core_refresh(
  SEXP self,
  SEXP private_environment
);
attribute_hidden void paradox_core_validate_graph_path(SEXP root);
attribute_hidden SEXP paradox_core_state_from_private(SEXP private_environment);
/* The same payload projection for a generation the caller already selected,
 * so a gated read does not authenticate the binding a second time. */
attribute_hidden SEXP paradox_core_state_from_core(SEXP core);
attribute_hidden SEXP paradox_core_local_value(
  SEXP private_environment,
  const char *name
);
attribute_hidden SEXP paradox_core_new_from_fields(
  paradox_core_kind_t kind,
  const SEXP fields[PARADOX_CORE_FIELD_COUNT]
);

/* Implemented beside the flatten builder in `paramset_collection_construct.c`
 * because a re-flatten is exactly a reconstruction: rebuild one COLLECTION's
 * flattened schema from its current children and the per-edge flags recorded
 * in `.edges`, install it, and return the replacement. `changed_edge` is the
 * zero-based edge whose child moved on, used only to give the deferred
 * duplicate-ID diagnostic its context. */
attribute_hidden SEXP paradox_collection_reflatten(
  SEXP private_environment,
  SEXP core,
  R_xlen_t changed_edge
);

attribute_hidden SEXP paradox_param_set_core_new(SEXP kind, SEXP state);
attribute_hidden SEXP paradox_param_set_core_state(SEXP owner, SEXP self);
attribute_hidden SEXP paradox_param_set_core_replace(SEXP owner, SEXP updates);
attribute_hidden SEXP paradox_param_set_core_kind(SEXP owner);
#if defined(PARADOX_TEST_CORE_GRAPH_ROOTS)
attribute_hidden SEXP paradox_test_core_graph_root_barrier_counts(SEXP reset);
#endif

#endif
