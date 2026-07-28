#ifndef PARADOX_CORE_STATE_H
#define PARADOX_CORE_STATE_H

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
  PARADOX_CORE_FIELD_COUNT
} paradox_core_field_t;

/* A core is a NULL-address external pointer. Its protected slot is the entire
 * serializable state and its tag records both the schema version and node
 * kind. No finalizer or unmanaged allocation is involved. */
attribute_hidden int paradox_core_is_valid(SEXP core);
attribute_hidden int paradox_core_has_exact_schema(SEXP core);
/* Payload-level twin of the check above: the exact ordinary ten-field
 * `.params`..`.postfix` capsule state schema. The single validator shared by
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
attribute_hidden SEXP paradox_core_refresh_shadow(
  SEXP self,
  SEXP private_environment
);
attribute_hidden void paradox_core_validate_graph_path(SEXP root);
attribute_hidden SEXP paradox_core_state_from_private(SEXP private_environment);
attribute_hidden SEXP paradox_core_local_value(
  SEXP private_environment,
  const char *name
);
attribute_hidden SEXP paradox_core_new_from_fields(
  paradox_core_kind_t kind,
  const SEXP fields[PARADOX_CORE_FIELD_COUNT]
);

attribute_hidden SEXP paradox_param_set_core_new(SEXP kind, SEXP state);
attribute_hidden SEXP paradox_param_set_core_state(SEXP owner);
attribute_hidden SEXP paradox_param_set_core_replace(SEXP owner, SEXP updates);
attribute_hidden SEXP paradox_param_set_core_kind(SEXP owner);
#if defined(PARADOX_TEST_CORE_GRAPH_ROOTS)
attribute_hidden SEXP paradox_test_core_graph_root_barrier_counts(SEXP reset);
#endif

#endif
