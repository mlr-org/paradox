#include <limits.h>
#include <stdint.h>
#include <string.h>

#include "core_state.h"
#include <R_ext/Utils.h>
#include "paramset_domain_common.h"
#include "paramset_shadow.h"
#include "r_api_compat.h"
#include "r_utils.h"
#if defined(PARADOX_TEST_CORE_GRAPH_ROOTS)
# include <R_ext/Memory.h>
#endif

static const char *const core_field_names[PARADOX_CORE_FIELD_COUNT] = {
  ".params",
  ".values",
  ".tags",
  ".deps",
  ".trafos",
  ".extra_trafo",
  ".constraint",
  ".sets",
  ".translation",
  ".postfix",
  ".edges"
};

/* The fields a parent's flattened schema is built from. A generation that
 * differs from the stamped one only outside this slice -- a value commit is
 * the common case -- leaves every enclosing flatten correct, which is what
 * keeps a tuning loop from re-flattening its collections. */
static const paradox_core_field_t core_schema_slice[] = {
  PARADOX_CORE_PARAMS,
  PARADOX_CORE_TAGS,
  PARADOX_CORE_TRAFOS
};

enum {
  CORE_SCHEMA_SLICE_COUNT =
    (int) (sizeof(core_schema_slice) / sizeof(core_schema_slice[0]))
};

/* Installing a different object in one of these fields can invalidate a
 * derived schema somewhere above; the remaining fields are read live by every
 * consumer and therefore never stale. */
static int core_field_is_schema(int field) {
  switch (field) {
  case PARADOX_CORE_PARAMS:
  case PARADOX_CORE_TAGS:
  case PARADOX_CORE_TRAFOS:
  case PARADOX_CORE_SETS:
  case PARADOX_CORE_TRANSLATION:
  case PARADOX_CORE_POSTFIX:
  case PARADOX_CORE_EDGES:
    return TRUE;
  default:
    return FALSE;
  }
}

static SEXP core_tag(paradox_core_kind_t kind) {
  /* Interned symbols are permanent, so caching them keeps the per-read tag
   * classification free of repeated symbol-table lookups. Every capsule read
   * classifies its tag at least once. */
  static SEXP tags[PARADOX_CORE_SHADOW + 1] = {NULL, NULL, NULL, NULL};
  if (tags[PARADOX_CORE_BASE] == NULL) {
    tags[PARADOX_CORE_BASE] = Rf_install("paradox.core.base.v1");
    tags[PARADOX_CORE_COLLECTION] = Rf_install("paradox.core.collection.v1");
    tags[PARADOX_CORE_SHADOW] = Rf_install("paradox.core.shadow.v1");
  }
  switch (kind) {
  case PARADOX_CORE_BASE:
  case PARADOX_CORE_COLLECTION:
  case PARADOX_CORE_SHADOW:
    return tags[kind];
  case PARADOX_CORE_NONE:
    break;
  }
  return R_NilValue;
}

static paradox_core_kind_t kind_from_tag(SEXP tag) {
  if (tag == core_tag(PARADOX_CORE_BASE)) {
    return PARADOX_CORE_BASE;
  }
  if (tag == core_tag(PARADOX_CORE_COLLECTION)) {
    return PARADOX_CORE_COLLECTION;
  }
  if (tag == core_tag(PARADOX_CORE_SHADOW)) {
    return PARADOX_CORE_SHADOW;
  }
  return PARADOX_CORE_NONE;
}

/*
 * Session-local epochs and per-capsule verification stamps.
 *
 * A COLLECTION caches a flattened schema and a SHADOW caches a projected view
 * of its origin; both are derived state that some other node's mutation can
 * invalidate. Rather than notify parents -- which would need a back-reference
 * registry maintained at every containment and clone event -- every capsule
 * installation that changes semantic state advances a global counter, and a
 * capsule records the counter value at which its own subtree was last proven
 * to agree. `stamp == epoch` therefore means "nothing anywhere has been
 * installed since this was verified", which is exactly the condition under
 * which revalidation can only reproduce its previous answer.
 *
 * Two counters, because the two kinds are invalidated by different things: a
 * flatten only depends on its children's schema slice, so a value commit must
 * not force collections to re-verify (that is the tuning hot loop), while a
 * Shadow projects live values and must.
 *
 * The stamp lives in the capsule's external-pointer address slot. That slot is
 * never dereferenced -- it holds an integer, so the capsule still owns no
 * native resource and needs no finalizer -- and it is the one place R
 * guarantees to reset on unserialize, so a loaded capsule is automatically
 * unverified without any hook. Mixing the capsule's own address into the
 * stored value additionally makes a duplicated capsule unverified: R's
 * `attr<-` on a referenced core copies the address slot verbatim, and a
 * duplicate whose attributes were rewritten must not inherit the original's
 * proof.
 *
 * Both counters start at 1 so that the all-zero slot of a fresh or restored
 * external pointer can never be mistaken for a valid stamp.
 */
static uintptr_t core_schema_epoch = 1;
static uintptr_t core_state_epoch = 1;

static uintptr_t core_stamp_for(SEXP core, uintptr_t epoch) {
  uint64_t bits = (uint64_t) (uintptr_t) core;
  bits ^= bits >> 33;
  bits *= UINT64_C(0xff51afd7ed558ccd);
  bits ^= bits >> 29;
  return (uintptr_t) (bits ^ (uint64_t) epoch);
}

static uintptr_t core_kind_epoch(paradox_core_kind_t kind) {
  return kind == PARADOX_CORE_COLLECTION
    ? core_schema_epoch
    : core_state_epoch;
}

void paradox_core_note_change(paradox_core_change_t change) {
  if (change == PARADOX_CORE_CHANGE_NONE) {
    return;
  }
  /* Wrapping is unreachable on a 64-bit host and would at worst cost one
   * redundant revalidation on a 32-bit one, so it is documented rather than
   * engineered around; the counters must only never revisit 0. */
  core_state_epoch += 2;
  if (change == PARADOX_CORE_CHANGE_SCHEMA) {
    core_schema_epoch += 2;
  }
}

uintptr_t paradox_core_state_epoch_value(void) {
  return core_state_epoch;
}

void paradox_core_stamp_verified(SEXP core) {
  const uintptr_t stamp = core_stamp_for(
    core,
    core_kind_epoch(kind_from_tag(R_ExternalPtrTag(core)))
  );
  R_SetExternalPtrAddr(core, (void *) stamp);
}

int paradox_core_is_verified(SEXP core) {
  if (TYPEOF(core) != EXTPTRSXP) {
    return FALSE;
  }
  const paradox_core_kind_t kind = kind_from_tag(R_ExternalPtrTag(core));
  if (kind == PARADOX_CORE_BASE) {
    return TRUE;
  }
  if (kind == PARADOX_CORE_NONE) {
    return FALSE;
  }
  /* An empty slot is the fresh, restored, and cleared state and is never a
   * proof; a stamp that happens to mix to it merely revalidates once. */
  void *slot = R_ExternalPtrAddr(core);
  const uintptr_t stamp = (uintptr_t) slot;
  if (stamp == 0 || stamp != core_stamp_for(core, core_kind_epoch(kind))) {
    return FALSE;
  }
  /* The stamp proves that no capsule was installed since this generation was
   * verified. It cannot prove that the derived-cache carrier itself was not
   * rewritten in place, which is the one corruption an ordinary R attribute
   * assignment can still produce, so a SHADOW reauthenticates its refresh
   * signature before its cached projection is trusted. */
  return kind != PARADOX_CORE_SHADOW || paradox_shadow_metadata_is_exact(core);
}

static int exact_payload(SEXP payload) {
  return TYPEOF(payload) == VECSXP && !ALTREP(payload) && !Rf_isS4(payload) &&
    XLENGTH(payload) == PARADOX_CORE_FIELD_COUNT;
}

static int field_index(const char *name) {
  for (int field = 0; field < PARADOX_CORE_FIELD_COUNT; ++field) {
    if (strcmp(name, core_field_names[field]) == 0) {
      return field;
    }
  }
  return -1;
}

static int exact_names(SEXP names) {
  if (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
      XLENGTH(names) != PARADOX_CORE_FIELD_COUNT) {
    return FALSE;
  }
  for (int field = 0; field < PARADOX_CORE_FIELD_COUNT; ++field) {
    SEXP name = STRING_ELT(names, field);
    if (name == NA_STRING || strcmp(CHAR(name), core_field_names[field]) != 0) {
      return FALSE;
    }
  }
  return TRUE;
}

int paradox_core_state_exact_schema(SEXP payload) {
  if (!exact_payload(payload)) {
    return FALSE;
  }
  PROTECT(payload);
  SEXP names = PROTECT(Rf_getAttrib(payload, R_NamesSymbol));
  const int valid = paradox_api_has_single_attribute(payload, "names") &&
    !Rf_isS4(names) && paradox_api_has_no_attributes(names) &&
    exact_names(names);
  UNPROTECT(2);
  return valid;
}

static int exact_carrier_attributes(SEXP core) {
  if (Rf_isS4(core)) {
    return FALSE;
  }
  const paradox_core_kind_t kind = kind_from_tag(R_ExternalPtrTag(core));
  return kind == PARADOX_CORE_SHADOW
    ? paradox_api_has_single_attribute(
        core,
        ".paradox.shadow.snapshot.v1"
      )
    : paradox_api_has_no_attributes(core);
}

static int exact_named_list(SEXP value, const char *const *labels,
    int count) {
  if (TYPEOF(value) != VECSXP || ALTREP(value) || Rf_isS4(value) ||
      XLENGTH(value) != count ||
      !paradox_api_has_single_attribute(value, "names")) {
    return FALSE;
  }
  SEXP names = PROTECT(Rf_getAttrib(value, R_NamesSymbol));
  int valid = TYPEOF(names) == STRSXP && !ALTREP(names) && !Rf_isS4(names) &&
    paradox_api_has_no_attributes(names) && XLENGTH(names) == count;
  for (int index = 0; valid && index < count; ++index) {
    SEXP name = STRING_ELT(names, index);
    valid = name != NA_STRING && strcmp(CHAR(name), labels[index]) == 0;
  }
  UNPROTECT(1);
  return valid;
}

static int exact_edge_flags(SEXP value, R_xlen_t count) {
  if (TYPEOF(value) != LGLSXP || ALTREP(value) || Rf_isS4(value) ||
      !paradox_api_has_no_attributes(value) || XLENGTH(value) != count) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < count; ++index) {
    if (LOGICAL_ELT(value, index) == NA_LOGICAL) {
      return FALSE;
    }
  }
  return TRUE;
}

/* `.edges` is derivation bookkeeping, so it is validated by node kind rather
 * than by the shared payload validator: only a derived node has one, and its
 * width is the node's own edge count. */
/* `NULL`, or the exact two-field record above: an ordinary attribute-free
 * character vector of governed IDs and a canonical (id, tag) table whose IDs
 * it governs. */
static int exact_tag_override(SEXP override) {
  if (override == R_NilValue) {
    return TRUE;
  }
  static const char *const labels[] = {"ids", "tags"};
  if (!exact_named_list(
      override, labels, PARADOX_TAG_OVERRIDE_FIELD_COUNT
    )) {
    return FALSE;
  }
  SEXP ids = VECTOR_ELT(override, PARADOX_TAG_OVERRIDE_IDS);
  if (TYPEOF(ids) != STRSXP || ALTREP(ids) || Rf_isS4(ids) ||
      !paradox_api_has_no_attributes(ids)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(ids); ++index) {
    if (STRING_ELT(ids, index) == NA_STRING) {
      return FALSE;
    }
  }
  SEXP tags = VECTOR_ELT(override, PARADOX_TAG_OVERRIDE_TAGS);
  R_xlen_t rows = 0;
  R_xlen_t work_since_interrupt = 0;
  static const char *const columns[] = {"id", "tag"};
  if (TYPEOF(tags) != VECSXP || ALTREP(tags) ||
      !paradox_domain_exact_plain_table(
        tags, columns, 2, &rows, &work_since_interrupt
      )) {
    return FALSE;
  }
  SEXP row_ids = VECTOR_ELT(tags, 0);
  SEXP row_tags = VECTOR_ELT(tags, 1);
  if (TYPEOF(row_ids) != STRSXP || TYPEOF(row_tags) != STRSXP ||
      ALTREP(row_ids) || ALTREP(row_tags) ||
      !paradox_api_has_no_attributes(row_ids) ||
      !paradox_api_has_no_attributes(row_tags)) {
    return FALSE;
  }
  for (R_xlen_t row = 0; row < rows; ++row) {
    if (!paradox_charsxp_is_ordinary(STRING_ELT(row_ids, row)) ||
        !paradox_charsxp_is_ordinary(STRING_ELT(row_tags, row)) ||
        !paradox_domain_string_in(
          ids, STRING_ELT(row_ids, row), &work_since_interrupt
        )) {
      return FALSE;
    }
  }
  return TRUE;
}

static int exact_edges(paradox_core_kind_t kind, SEXP payload) {
  SEXP edges = VECTOR_ELT(payload, PARADOX_CORE_EDGES);
  if (kind == PARADOX_CORE_BASE) {
    return edges == R_NilValue;
  }
  if (kind == PARADOX_CORE_COLLECTION) {
    static const char *const labels[] = {
      "cores", "tag_sets", "tag_params", "tag_override"
    };
    if (!exact_named_list(
        edges, labels, PARADOX_COLLECTION_EDGE_FIELD_COUNT
      )) {
      return FALSE;
    }
    /* The record is a cache, so it is validated for shape rather than against
     * the current `.sets`: an edge list installed without a matching record is
     * simply stale, and the next read re-flattens it. */
    SEXP cores = VECTOR_ELT(edges, PARADOX_COLLECTION_EDGE_CORES);
    const R_xlen_t count = TYPEOF(cores) == VECSXP ? XLENGTH(cores) : -1;
    if (TYPEOF(cores) != VECSXP || ALTREP(cores) || Rf_isS4(cores) ||
        !paradox_api_has_no_attributes(cores) || XLENGTH(cores) != count) {
      return FALSE;
    }
    for (R_xlen_t index = 0; index < count; ++index) {
      if (!paradox_core_is_canonical(VECTOR_ELT(cores, index))) {
        return FALSE;
      }
    }
    return exact_edge_flags(
        VECTOR_ELT(edges, PARADOX_COLLECTION_EDGE_TAG_SETS),
        count
      ) && exact_edge_flags(
        VECTOR_ELT(edges, PARADOX_COLLECTION_EDGE_TAG_PARAMS),
        count
      ) && exact_tag_override(
        VECTOR_ELT(edges, PARADOX_COLLECTION_EDGE_TAG_OVERRIDE)
      );
  }
  static const char *const labels[] = {
    "params", "tags", "trafos", "shadowed", "tag_override"
  };
  if (!exact_named_list(edges, labels, PARADOX_SHADOW_EDGE_FIELD_COUNT)) {
    return FALSE;
  }
  SEXP shadowed = VECTOR_ELT(edges, PARADOX_SHADOW_EDGE_SHADOWED);
  return TYPEOF(shadowed) == STRSXP && !ALTREP(shadowed) &&
    !Rf_isS4(shadowed) && paradox_api_has_no_attributes(shadowed) &&
    exact_tag_override(VECTOR_ELT(edges, PARADOX_SHADOW_EDGE_TAG_OVERRIDE));
}

static SEXP new_core(paradox_core_kind_t kind, SEXP payload) {
  if (kind < PARADOX_CORE_BASE || kind > PARADOX_CORE_SHADOW ||
      !exact_payload(payload)) {
    Rf_error("Internal error: invalid ParamSet core construction");
  }
  if (!exact_edges(kind, payload)) {
    Rf_error("Internal error: invalid ParamSet capsule edge record");
  }
  /* The address slot starts empty, which is the unverified stamp. */
  return R_MakeExternalPtr(NULL, core_tag(kind), payload);
}

SEXP paradox_core_new_from_fields(paradox_core_kind_t kind,
    const SEXP fields[PARADOX_CORE_FIELD_COUNT]) {
  SEXP payload = PROTECT(Rf_allocVector(VECSXP, PARADOX_CORE_FIELD_COUNT));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, PARADOX_CORE_FIELD_COUNT));
  for (int field = 0; field < PARADOX_CORE_FIELD_COUNT; ++field) {
    SET_VECTOR_ELT(payload, field, fields[field]);
    SET_STRING_ELT(names, field, Rf_mkCharCE(core_field_names[field], CE_UTF8));
  }
  Rf_setAttrib(payload, R_NamesSymbol, names);
  SEXP result = PROTECT(new_core(kind, payload));
  UNPROTECT(3);
  return result;
}

int paradox_core_is_valid(SEXP core) {
  /* The address slot is the session-local verification stamp, not part of the
   * capsule's identity: an unverified capsule is valid, it merely has to
   * revalidate before its derived fields are read. */
  return TYPEOF(core) == EXTPTRSXP && !Rf_isS4(core) &&
    !Rf_isS4(R_ExternalPtrTag(core)) &&
    kind_from_tag(R_ExternalPtrTag(core)) != PARADOX_CORE_NONE &&
    exact_payload(R_ExternalPtrProtected(core));
}

int paradox_core_has_exact_schema(SEXP core) {
  if (!paradox_core_is_valid(core)) {
    return FALSE;
  }
  return paradox_core_state_exact_schema(R_ExternalPtrProtected(core));
}

int paradox_core_is_canonical(SEXP core) {
  return paradox_core_has_exact_schema(core) &&
    exact_carrier_attributes(core);
}

paradox_core_kind_t paradox_core_kind(SEXP core) {
  return paradox_core_is_valid(core)
    ? kind_from_tag(R_ExternalPtrTag(core))
    : PARADOX_CORE_NONE;
}

SEXP paradox_core_payload(SEXP core) {
  return paradox_core_is_valid(core)
    ? R_ExternalPtrProtected(core)
    : R_UnboundValue;
}

typedef SEXP (*core_binding_reader_t)(SEXP, SEXP);

static SEXP core_from_private_using(SEXP private_environment,
    core_binding_reader_t read_binding) {
  if (TYPEOF(private_environment) != ENVSXP ||
      Rf_isS4(private_environment)) {
    return R_UnboundValue;
  }
  /* All capsule-topology reads cross the same non-forcing ordinary-binding
   * boundary.  This rejects active and delayed bindings before canonical core
   * validation, including on the supported pre-4.6 runtimes. */
  SEXP core_symbol = Rf_install(".core");
  SEXP core = PROTECT(read_binding(
    private_environment,
    core_symbol
  ));
  const int canonical = paradox_core_is_canonical(core);
  UNPROTECT(1);
  return canonical ? core : R_UnboundValue;
}

SEXP paradox_core_from_private(SEXP private_environment) {
  return core_from_private_using(
    private_environment,
    paradox_api_plain_binding_snapshot
  );
}

SEXP paradox_core_from_private_optional(SEXP private_environment) {
  /*
   * On R 3.6--4.1 the optional reader evaluates base::exists() before taking
   * the non-forcing binding snapshot. Keep a cold-path root here so callers
   * may safely pass an environment reached through another protected object;
   * the required hot-path reader above remains allocation-free.
   */
  PROTECT(private_environment);
  SEXP core = core_from_private_using(
    private_environment,
    paradox_api_optional_plain_binding_snapshot
  );
  UNPROTECT(1);
  return core;
}

static SEXP private_from_self(SEXP self) {
  if (TYPEOF(self) != ENVSXP || Rf_isS4(self)) {
    return R_UnboundValue;
  }
  SEXP enclosure_symbol = Rf_install(".__enclos_env__");
  SEXP enclosure = PROTECT(paradox_api_optional_plain_binding_snapshot(
    self,
    enclosure_symbol
  ));
  SEXP private_symbol = Rf_install("private");
  if (TYPEOF(enclosure) != ENVSXP || Rf_isS4(enclosure)) {
    UNPROTECT(1);
    return R_UnboundValue;
  }
  SEXP owned_private = PROTECT(paradox_api_optional_plain_binding_snapshot(
    enclosure,
    private_symbol
  ));
  SEXP result = TYPEOF(owned_private) == ENVSXP && !Rf_isS4(owned_private)
    ? owned_private
    : R_UnboundValue;
  UNPROTECT(2);
  return result;
}

typedef struct {
  SEXP self;
  SEXP sets;
  R_xlen_t child_count;
  R_xlen_t next_child;
  int entered;
} core_graph_frame_t;

enum core_graph_root_slot {
  CORE_GRAPH_ROOT_SELF = 0,
  CORE_GRAPH_ROOT_CORE,
  CORE_GRAPH_ROOT_STRIDE
};

enum {
  CORE_GRAPH_INLINE_CAPACITY = 16
};

static R_xlen_t core_graph_root_slot(
    R_xlen_t frame, enum core_graph_root_slot slot) {
  return frame * CORE_GRAPH_ROOT_STRIDE + slot;
}

static void grow_core_graph_roots(
    SEXP *roots, PROTECT_INDEX roots_index,
    R_xlen_t depth, R_xlen_t capacity) {
  SEXP replacement = PROTECT(Rf_allocVector(
    VECSXP,
    capacity * CORE_GRAPH_ROOT_STRIDE
  ));
  const R_xlen_t retained = depth * CORE_GRAPH_ROOT_STRIDE;
  for (R_xlen_t index = 0; index < retained; ++index) {
    SET_VECTOR_ELT(replacement, index, VECTOR_ELT(*roots, index));
  }
  REPROTECT(replacement, roots_index);
  *roots = replacement;
  UNPROTECT(1);
}

/*
 * Ordinary depth-first colouring of the nodes the traversal has reached: a
 * node on the active path is CORE_GRAPH_VISIT_ACTIVE, a node whose subtree is
 * completely validated is CORE_GRAPH_VISIT_DONE.  A child that is already
 * ACTIVE closes a cycle; a child that is already DONE needs no second walk.
 * Without this, a graph that shares one child between two parents is walked
 * once per distinct path, which is exponential in the sharing depth.
 *
 * Node identity is the R6 shell itself, so the table stores the shells in an
 * R vector rather than raw addresses: a DONE node stops being reachable from
 * the active path once its parent pops, and an address the collector reused
 * for a different shell would silently skip that shell's subtree.
 *
 * A DONE node is validated once rather than once per inbound edge, so a
 * callback that swaps its capsule mid-traversal is no longer observed by a
 * later edge. This traversal never promised a post-return state -- the
 * capsule it validated can be replaced the moment it returns -- and holding
 * every visited shell here shortens, rather than widens, that window.
 */
enum {
  CORE_GRAPH_VISIT_FREE = 0,
  CORE_GRAPH_VISIT_ACTIVE,
  CORE_GRAPH_VISIT_DONE
};

/* Sized so the initial table is one of R's small vector classes and still
 * holds eight distinct nodes before it has to grow, which covers the shapes
 * that reach this traversal in practice. */
enum {
  CORE_GRAPH_VISIT_INLINE_CAPACITY = 16
};

typedef struct {
  SEXP nodes;
  unsigned char *states;
  R_xlen_t capacity;
  R_xlen_t occupied;
  PROTECT_INDEX index;
} core_graph_visit_t;

static R_xlen_t core_graph_visit_home(SEXP self, R_xlen_t capacity) {
  uint64_t bits = (uint64_t) (uintptr_t) self;
  bits ^= bits >> 33;
  bits *= UINT64_C(0xff51afd7ed558ccd);
  bits ^= bits >> 29;
  return (R_xlen_t) (bits & (uint64_t) (capacity - 1));
}

/* The slot holding `self`, or the free slot it would occupy.  The table keeps
 * at least half of its slots free, so this probe always terminates. */
static R_xlen_t core_graph_visit_probe(const core_graph_visit_t *visit,
    SEXP self) {
  const R_xlen_t mask = visit->capacity - 1;
  R_xlen_t slot = core_graph_visit_home(self, visit->capacity);
  while (visit->states[slot] != CORE_GRAPH_VISIT_FREE &&
      VECTOR_ELT(visit->nodes, slot) != self) {
    slot = (slot + 1) & mask;
  }
  return slot;
}

static unsigned char core_graph_visit_state(const core_graph_visit_t *visit,
    SEXP self) {
  return visit->states[core_graph_visit_probe(visit, self)];
}

static void core_graph_visit_grow(core_graph_visit_t *visit) {
  if (visit->capacity > R_XLEN_T_MAX / 2) {
    Rf_error("ParamSet capsule graph is too large");
  }
  const R_xlen_t expanded_capacity = visit->capacity * 2;
  SEXP replacement = PROTECT(Rf_allocVector(VECSXP, expanded_capacity));
  unsigned char *expanded_states = paradox_temporary_alloc(
    expanded_capacity,
    sizeof(*expanded_states)
  );
  memset(expanded_states, 0, (size_t) expanded_capacity);
  const R_xlen_t mask = expanded_capacity - 1;
  for (R_xlen_t slot = 0; slot < visit->capacity; ++slot) {
    if (visit->states[slot] == CORE_GRAPH_VISIT_FREE) {
      continue;
    }
    SEXP node = VECTOR_ELT(visit->nodes, slot);
    R_xlen_t target = core_graph_visit_home(node, expanded_capacity);
    while (expanded_states[target] != CORE_GRAPH_VISIT_FREE) {
      target = (target + 1) & mask;
    }
    SET_VECTOR_ELT(replacement, target, node);
    expanded_states[target] = visit->states[slot];
  }
  REPROTECT(replacement, visit->index);
  UNPROTECT(1);
  visit->nodes = replacement;
  visit->states = expanded_states;
  visit->capacity = expanded_capacity;
}

/* Records a node the traversal is entering.  The caller must have rooted it
 * already: growing the table allocates. */
static void core_graph_visit_enter(core_graph_visit_t *visit, SEXP self) {
  if ((visit->occupied + 1) * 2 > visit->capacity) {
    core_graph_visit_grow(visit);
  }
  const R_xlen_t slot = core_graph_visit_probe(visit, self);
  SET_VECTOR_ELT(visit->nodes, slot, self);
  visit->states[slot] = CORE_GRAPH_VISIT_ACTIVE;
  ++visit->occupied;
}

/* Marks an entered node validated.  The slot already exists, so this never
 * allocates and the node stays rooted for the rest of the traversal. */
static void core_graph_visit_leave(core_graph_visit_t *visit, SEXP self) {
  visit->states[core_graph_visit_probe(visit, self)] = CORE_GRAPH_VISIT_DONE;
}

#if defined(PARADOX_TEST_CORE_GRAPH_ROOTS)
static int test_core_graph_root_barriers = 0;
static int test_core_graph_root_collected = FALSE;
static int test_core_graph_root_barrier_active = FALSE;

static void test_core_graph_root_finalizer(SEXP core) {
  (void) core;
  if (test_core_graph_root_barrier_active) {
    test_core_graph_root_collected = TRUE;
  }
}

static void test_core_graph_root_barrier(
    SEXP private_environment, SEXP core) {
  if (test_core_graph_root_barriers == INT_MAX) {
    Rf_error("Instrumented core-graph root barrier counter overflow");
  }
  ++test_core_graph_root_barriers;
  R_RegisterCFinalizerEx(
    core,
    test_core_graph_root_finalizer,
    FALSE
  );
  SEXP core_symbol = Rf_install(".core");
  if (paradox_core_from_private(private_environment) != core) {
    Rf_error("Instrumented core-graph binding changed before its barrier");
  }
  Rf_defineVar(core_symbol, R_NilValue, private_environment);
  test_core_graph_root_collected = FALSE;
  test_core_graph_root_barrier_active = TRUE;
  R_gc();
  R_RunPendingFinalizers();
  test_core_graph_root_barrier_active = FALSE;
  if (test_core_graph_root_collected) {
    Rf_error("Instrumented core-graph barrier lost its selected capsule");
  }
  Rf_defineVar(core_symbol, core, private_environment);
  if (paradox_core_from_private(private_environment) != core) {
    Rf_error("Instrumented core-graph binding was not restored");
  }
}

SEXP paradox_test_core_graph_root_barrier_counts(SEXP reset) {
  if (TYPEOF(reset) != LGLSXP || ALTREP(reset) ||
      XLENGTH(reset) != 1 ||
      LOGICAL_ELT(reset, 0) == NA_LOGICAL) {
    Rf_error("`reset` must be TRUE or FALSE");
  }
  SEXP result = PROTECT(Rf_ScalarInteger(
    test_core_graph_root_barriers
  ));
  if (LOGICAL_ELT(reset, 0)) {
    test_core_graph_root_barriers = 0;
  }
  UNPROTECT(1);
  return result;
}
#endif

void paradox_core_validate_graph_path(SEXP root) {
  /*
   * R_alloc() owns only the raw frame bytes; R's collector cannot discover
   * SEXP pointers stored in them. Candidate-shell inspection may allocate or
   * enter the evaluator on supported old R, and deep-stack growth allocates
   * on every runtime. A finalizer can therefore detach an already selected
   * capsule generation while the traversal still needs its edges.
   *
   * Keep the active path in one indexed R carrier: each self slot protects
   * ancestor identity comparisons even if an edge is mutated in place, and
   * each core slot owns the exact payload/sets generation being traversed.
   * Path slots are cleared on pop; the separate visit table below then owns
   * the validated nodes, so the retained set is proportional to the distinct
   * nodes of the graph rather than to every path through it. The inline
   * common stack replaces the old initial R_alloc(), leaving only one smaller
   * managed allocation on shallow paths.
   */
  PROTECT(root);
  R_xlen_t capacity = CORE_GRAPH_INLINE_CAPACITY;
  core_graph_frame_t inline_frames[CORE_GRAPH_INLINE_CAPACITY];
  core_graph_frame_t *frames = inline_frames;
  PROTECT_INDEX roots_index;
  SEXP roots;
  PROTECT_WITH_INDEX(
    roots = Rf_allocVector(
      VECSXP,
      capacity * CORE_GRAPH_ROOT_STRIDE
    ),
    &roots_index
  );
  core_graph_visit_t visit;
  visit.capacity = CORE_GRAPH_VISIT_INLINE_CAPACITY;
  visit.occupied = 0;
  PROTECT_WITH_INDEX(
    visit.nodes = Rf_allocVector(VECSXP, visit.capacity),
    &visit.index
  );
  visit.states = paradox_temporary_alloc(
    visit.capacity,
    sizeof(*visit.states)
  );
  memset(visit.states, 0, (size_t) visit.capacity);
  R_xlen_t depth = 1;
  R_xlen_t work_since_interrupt = 0;
  frames[0] = (core_graph_frame_t) {
    root, R_NilValue, 0, 0, FALSE
  };
  SET_VECTOR_ELT(
    roots,
    core_graph_root_slot(0, CORE_GRAPH_ROOT_SELF),
    root
  );
  core_graph_visit_enter(&visit, root);

  while (depth != 0) {
    paradox_account_work(&work_since_interrupt);
    core_graph_frame_t *frame = &frames[depth - 1];
    if (!frame->entered) {
      SEXP private_environment = PROTECT(private_from_self(frame->self));
      SEXP core = PROTECT(private_environment == R_UnboundValue
        ? R_UnboundValue
        : paradox_core_from_private_optional(private_environment));
      if (core == R_UnboundValue) {
        UNPROTECT(2);
        Rf_error("Corrupt ParamSet node in capsule graph");
      }
      SET_VECTOR_ELT(
        roots,
        core_graph_root_slot(depth - 1, CORE_GRAPH_ROOT_CORE),
        core
      );
      const paradox_core_kind_t kind = paradox_core_kind(core);
      SEXP state = R_ExternalPtrProtected(core);
      frame->sets = kind == PARADOX_CORE_BASE
        ? R_NilValue
        : VECTOR_ELT(state, PARADOX_CORE_SETS);
      if (kind == PARADOX_CORE_BASE) {
        frame->child_count = 0;
      } else if (TYPEOF(frame->sets) != VECSXP || ALTREP(frame->sets) ||
          (kind == PARADOX_CORE_SHADOW && XLENGTH(frame->sets) != 1)) {
        UNPROTECT(2);
        Rf_error("Corrupt ParamSet capsule graph edges");
      } else {
        frame->child_count = XLENGTH(frame->sets);
      }
      frame->next_child = 0;
      frame->entered = TRUE;
      UNPROTECT(2);
#if defined(PARADOX_TEST_CORE_GRAPH_ROOTS)
      /*
       * Both local roots are intentionally gone: the indexed carrier is now
       * the selected core's sole direct root after the binding is detached.
       */
      test_core_graph_root_barrier(private_environment, core);
#endif
    }

    if (frame->next_child == frame->child_count) {
      core_graph_visit_leave(&visit, frame->self);
      SET_VECTOR_ELT(
        roots,
        core_graph_root_slot(depth - 1, CORE_GRAPH_ROOT_SELF),
        R_NilValue
      );
      SET_VECTOR_ELT(
        roots,
        core_graph_root_slot(depth - 1, CORE_GRAPH_ROOT_CORE),
        R_NilValue
      );
      --depth;
      continue;
    }
    if (depth == capacity) {
      if (capacity > R_XLEN_T_MAX /
          (2 * CORE_GRAPH_ROOT_STRIDE)) {
        Rf_error("ParamSet capsule graph is too deep");
      }
      const R_xlen_t expanded_capacity = capacity * 2;
      grow_core_graph_roots(
        &roots,
        roots_index,
        depth,
        expanded_capacity
      );
      core_graph_frame_t *expanded = paradox_temporary_alloc(
        expanded_capacity,
        sizeof(*expanded)
      );
      memcpy(expanded, frames, (size_t) depth * sizeof(*expanded));
      frames = expanded;
      capacity = expanded_capacity;
      frame = &frames[depth - 1];
    }
    SEXP child = VECTOR_ELT(frame->sets, frame->next_child);
    ++frame->next_child;
    if (TYPEOF(child) != ENVSXP) {
      Rf_error("Corrupt ParamSet capsule graph child");
    }
    const unsigned char child_state = core_graph_visit_state(&visit, child);
    if (child_state == CORE_GRAPH_VISIT_ACTIVE) {
      Rf_error("ParamSet capsule graph contains a cycle");
    }
    if (child_state == CORE_GRAPH_VISIT_DONE) {
      /* Already validated through another parent, and every node it can reach
       * is DONE as well, so no cycle can run through it into the active
       * path. */
      continue;
    }
    /* Root the child before the visit table can grow: recording it allocates.
     */
    SET_VECTOR_ELT(
      roots,
      core_graph_root_slot(depth, CORE_GRAPH_ROOT_SELF),
      child
    );
    core_graph_visit_enter(&visit, child);
    frames[depth] = (core_graph_frame_t) {
      child, R_NilValue, 0, 0, FALSE
    };
    ++depth;
  }
  UNPROTECT(3);
}

typedef struct {
  SEXP self;
  SEXP core;
  SEXP sets;
  paradox_core_kind_t kind;
  R_xlen_t child_count;
  R_xlen_t next_child;
  int entered;
  int verified;
} core_heal_frame_t;

/* The first edge whose child has moved to a generation with a different
 * schema slice, or -1 when the cached flatten still agrees with every child.
 * A child that merely committed values keeps its `.params`/`.tags`/`.trafos`
 * objects -- `core_replace` shallow-duplicates the payload -- so the common
 * post-mutation case costs three pointer comparisons and no rebuild. */
static R_xlen_t collection_stale_edge(SEXP core) {
  SEXP payload = R_ExternalPtrProtected(core);
  SEXP sets = VECTOR_ELT(payload, PARADOX_CORE_SETS);
  SEXP stamped = VECTOR_ELT(
    VECTOR_ELT(payload, PARADOX_CORE_EDGES),
    PARADOX_COLLECTION_EDGE_CORES
  );
  if (TYPEOF(sets) != VECSXP || ALTREP(sets)) {
    Rf_error("Corrupt ParamSet capsule graph edges");
  }
  const R_xlen_t count = XLENGTH(sets);
  if (XLENGTH(stamped) != count) {
    /* The edge list itself moved; nothing in the record describes it. */
    return 0;
  }
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP child_private = private_from_self(VECTOR_ELT(sets, index));
    SEXP current = child_private == R_UnboundValue
      ? R_UnboundValue
      : paradox_core_from_private_optional(child_private);
    if (current == R_UnboundValue) {
      Rf_error("Corrupt ParamSet node in capsule graph");
    }
    SEXP previous = VECTOR_ELT(stamped, index);
    if (current == previous) {
      continue;
    }
    SEXP current_state = R_ExternalPtrProtected(current);
    SEXP previous_state = R_ExternalPtrProtected(previous);
    for (int slice = 0; slice < CORE_SCHEMA_SLICE_COUNT; ++slice) {
      if (VECTOR_ELT(current_state, core_schema_slice[slice]) !=
          VECTOR_ELT(previous_state, core_schema_slice[slice])) {
        return index;
      }
    }
  }
  return -1;
}

int paradox_collection_flatten_is_stale(SEXP core) {
  if (paradox_core_kind(core) != PARADOX_CORE_COLLECTION ||
      paradox_core_is_verified(core)) {
    return FALSE;
  }
  SEXP payload = R_ExternalPtrProtected(core);
  SEXP sets = VECTOR_ELT(payload, PARADOX_CORE_SETS);
  SEXP stamped = VECTOR_ELT(
    VECTOR_ELT(payload, PARADOX_CORE_EDGES),
    PARADOX_COLLECTION_EDGE_CORES
  );
  if (TYPEOF(sets) != VECSXP || ALTREP(sets) ||
      XLENGTH(stamped) != XLENGTH(sets)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(sets); ++index) {
    SEXP child = VECTOR_ELT(sets, index);
    if (TYPEOF(child) != ENVSXP) {
      return FALSE;
    }
    SEXP child_private = private_from_self(child);
    SEXP current = child_private == R_UnboundValue
      ? R_UnboundValue
      : paradox_core_from_private_optional(child_private);
    if (current == R_UnboundValue) {
      return FALSE;
    }
    SEXP previous = VECTOR_ELT(stamped, index);
    if (current == previous) {
      continue;
    }
    SEXP current_state = R_ExternalPtrProtected(current);
    SEXP previous_state = R_ExternalPtrProtected(previous);
    for (int slice = 0; slice < CORE_SCHEMA_SLICE_COUNT; ++slice) {
      if (VECTOR_ELT(current_state, core_schema_slice[slice]) !=
          VECTOR_ELT(previous_state, core_schema_slice[slice])) {
        return TRUE;
      }
    }
  }
  return FALSE;
}

/*
 * Bring one capsule graph back into agreement with its children.
 *
 * The walk is the post-order twin of `paradox_core_validate_graph_path`: same
 * indexed root carrier, same shell-identity visit table, so a shared subtree
 * is healed once and a repeated node on the active path is a cycle. Post-order
 * matters twice: a parent's flatten is rebuilt from children that are already
 * correct, and every `.core` this installs is individually consistent, so an
 * interrupt or a deferred name collision leaves a partially healed graph in
 * which everything healed so far is right and the rest heals at the next
 * entry. No user code runs, so there is no reentrancy window.
 */
static SEXP heal_graph(SEXP root_self, SEXP root_private) {
  PROTECT(root_self);
  PROTECT(root_private);
  if (private_from_self(root_self) != root_private) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSet shell ownership");
  }
  R_xlen_t capacity = CORE_GRAPH_INLINE_CAPACITY;
  core_heal_frame_t inline_frames[CORE_GRAPH_INLINE_CAPACITY];
  core_heal_frame_t *frames = inline_frames;
  PROTECT_INDEX roots_index;
  SEXP roots;
  PROTECT_WITH_INDEX(
    roots = Rf_allocVector(VECSXP, capacity * CORE_GRAPH_ROOT_STRIDE),
    &roots_index
  );
  core_graph_visit_t visit;
  visit.capacity = CORE_GRAPH_VISIT_INLINE_CAPACITY;
  visit.occupied = 0;
  PROTECT_WITH_INDEX(
    visit.nodes = Rf_allocVector(VECSXP, visit.capacity),
    &visit.index
  );
  visit.states = paradox_temporary_alloc(
    visit.capacity,
    sizeof(*visit.states)
  );
  memset(visit.states, 0, (size_t) visit.capacity);
  R_xlen_t depth = 1;
  R_xlen_t work_since_interrupt = 0;
  /* Every allocation in this walk can run a finalizer, and the Shadow rebuild
   * below evaluates package closures. A capsule installed by any of them
   * describes a world this walk did not observe, so nothing it already
   * validated may be stamped afterwards; the next entry simply walks again. */
  const uintptr_t entry_epoch = core_state_epoch;
  SEXP result = R_NilValue;
  frames[0] = (core_heal_frame_t) {
    root_self, R_NilValue, R_NilValue, PARADOX_CORE_NONE, 0, 0, FALSE, FALSE
  };
  SET_VECTOR_ELT(
    roots,
    core_graph_root_slot(0, CORE_GRAPH_ROOT_SELF),
    root_self
  );
  core_graph_visit_enter(&visit, root_self);

  while (depth != 0) {
    paradox_account_work(&work_since_interrupt);
    core_heal_frame_t *frame = &frames[depth - 1];
    if (!frame->entered) {
      /* The private environment is derived where it is used rather than kept
       * in the frame: frame storage can move to unscanned temporary memory,
       * and the shell that owns it is what the root carrier holds. */
      SEXP private_environment = private_from_self(frame->self);
      SEXP core = private_environment == R_UnboundValue
        ? R_UnboundValue
        : paradox_core_from_private_optional(private_environment);
      if (core == R_UnboundValue) {
        Rf_error("Corrupt ParamSet node in capsule graph");
      }
      SET_VECTOR_ELT(
        roots,
        core_graph_root_slot(depth - 1, CORE_GRAPH_ROOT_CORE),
        core
      );
      frame->core = core;
      frame->kind = paradox_core_kind(core);
      /* A verified node proves its whole subtree, so an unrelated branch of a
       * large graph is not rewalked because something changed elsewhere. */
      frame->verified = paradox_core_is_verified(core);
      SEXP state = R_ExternalPtrProtected(core);
      frame->sets = frame->kind == PARADOX_CORE_BASE || frame->verified
        ? R_NilValue
        : VECTOR_ELT(state, PARADOX_CORE_SETS);
      if (frame->kind == PARADOX_CORE_BASE || frame->verified) {
        frame->child_count = 0;
      } else if (TYPEOF(frame->sets) != VECSXP || ALTREP(frame->sets) ||
          (frame->kind == PARADOX_CORE_SHADOW &&
            XLENGTH(frame->sets) != 1)) {
        Rf_error("Corrupt ParamSet capsule graph edges");
      } else {
        frame->child_count = XLENGTH(frame->sets);
      }
      frame->next_child = 0;
      frame->entered = TRUE;
    }

    if (frame->next_child == frame->child_count) {
      SEXP core = frame->core;
      if (frame->verified) {
        /* nothing to do */
      } else if (frame->kind == PARADOX_CORE_COLLECTION) {
        const R_xlen_t stale = collection_stale_edge(core);
        if (stale >= 0) {
          SEXP private_environment = private_from_self(frame->self);
          if (private_environment == R_UnboundValue) {
            Rf_error("Corrupt ParamSet node in capsule graph");
          }
          core = paradox_collection_reflatten(
            private_environment,
            core,
            stale
          );
          SET_VECTOR_ELT(
            roots,
            core_graph_root_slot(depth - 1, CORE_GRAPH_ROOT_CORE),
            core
          );
        }
        if (core_state_epoch == entry_epoch) {
          paradox_core_stamp_verified(core);
        }
      } else if (frame->kind == PARADOX_CORE_SHADOW) {
        SEXP private_environment = private_from_self(frame->self);
        if (private_environment == R_UnboundValue) {
          Rf_error("Corrupt ParamSet node in capsule graph");
        }
        core = paradox_shadow_refresh_authoritative(
          frame->self,
          private_environment
        );
        SET_VECTOR_ELT(
          roots,
          core_graph_root_slot(depth - 1, CORE_GRAPH_ROOT_CORE),
          core
        );
        if (core_state_epoch == entry_epoch) {
          paradox_core_stamp_verified(core);
        }
      }
      if (depth == 1) {
        result = core;
      }
      core_graph_visit_leave(&visit, frame->self);
      SET_VECTOR_ELT(
        roots,
        core_graph_root_slot(depth - 1, CORE_GRAPH_ROOT_SELF),
        R_NilValue
      );
      SET_VECTOR_ELT(
        roots,
        core_graph_root_slot(depth - 1, CORE_GRAPH_ROOT_CORE),
        R_NilValue
      );
      --depth;
      continue;
    }
    if (depth == capacity) {
      if (capacity > R_XLEN_T_MAX / (2 * CORE_GRAPH_ROOT_STRIDE)) {
        Rf_error("ParamSet capsule graph is too deep");
      }
      const R_xlen_t expanded_capacity = capacity * 2;
      grow_core_graph_roots(&roots, roots_index, depth, expanded_capacity);
      core_heal_frame_t *expanded = paradox_temporary_alloc(
        expanded_capacity,
        sizeof(*expanded)
      );
      memcpy(expanded, frames, (size_t) depth * sizeof(*expanded));
      frames = expanded;
      capacity = expanded_capacity;
      frame = &frames[depth - 1];
    }
    SEXP child = VECTOR_ELT(frame->sets, frame->next_child);
    ++frame->next_child;
    if (TYPEOF(child) != ENVSXP) {
      Rf_error("Corrupt ParamSet capsule graph child");
    }
    const unsigned char child_state = core_graph_visit_state(&visit, child);
    if (child_state == CORE_GRAPH_VISIT_ACTIVE) {
      Rf_error("ParamSet capsule graph contains a cycle");
    }
    if (child_state == CORE_GRAPH_VISIT_DONE) {
      continue;
    }
    SET_VECTOR_ELT(
      roots,
      core_graph_root_slot(depth, CORE_GRAPH_ROOT_SELF),
      child
    );
    core_graph_visit_enter(&visit, child);
    frames[depth] = (core_heal_frame_t) {
      child, R_NilValue, R_NilValue, PARADOX_CORE_NONE, 0, 0, FALSE, FALSE
    };
    ++depth;
  }
  UNPROTECT(4);
  return result;
}

SEXP paradox_core_refresh(SEXP self, SEXP private_environment) {
  SEXP core = paradox_core_from_private(private_environment);
  if (core == R_UnboundValue) {
    /* Reproduce the exact corrupt-capsule diagnostics of the authoritative
     * refresh rather than inventing a second set for the same condition. */
    return paradox_shadow_refresh_authoritative(self, private_environment);
  }
  if (paradox_core_is_verified(core)) {
    return core;
  }
  return heal_graph(self, private_environment);
}

SEXP paradox_core_state_from_core(SEXP core) {
  if (core == R_UnboundValue ||
      (paradox_core_kind(core) == PARADOX_CORE_SHADOW &&
       !paradox_shadow_metadata_is_exact(core))) {
    return R_UnboundValue;
  }
  return R_ExternalPtrProtected(core);
}

SEXP paradox_core_state_from_private(SEXP private_environment) {
  return paradox_core_state_from_core(
    paradox_core_from_private(private_environment)
  );
}

SEXP paradox_core_local_value(SEXP private_environment, const char *name) {
  const int field = field_index(name);
  if (field < 0) {
    return R_UnboundValue;
  }
  SEXP state = paradox_core_state_from_private(private_environment);
  return state == R_UnboundValue
    ? R_UnboundValue
    : VECTOR_ELT(state, field);
}

static paradox_core_kind_t scalar_kind(SEXP kind) {
  if (TYPEOF(kind) != INTSXP || ALTREP(kind) || Rf_isS4(kind) ||
      XLENGTH(kind) != 1 ||
      INTEGER_ELT(kind, 0) < PARADOX_CORE_BASE ||
      INTEGER_ELT(kind, 0) > PARADOX_CORE_SHADOW) {
    Rf_error("`kind` must identify a ParamSet core node");
  }
  const int parsed = INTEGER_ELT(kind, 0);
  return (paradox_core_kind_t) parsed;
}

/* TRUE when every edge of a freshly built COLLECTION flatten still names the
 * child generation it was built from and that child is itself verified. The
 * flatten is then correct for the current epoch and needs no first-read walk,
 * which matters because `ps_union()`/`$search_space()` build collections in
 * loops. */
static int collection_edges_current(SEXP core) {
  SEXP payload = R_ExternalPtrProtected(core);
  SEXP sets = VECTOR_ELT(payload, PARADOX_CORE_SETS);
  SEXP stamped = VECTOR_ELT(
    VECTOR_ELT(payload, PARADOX_CORE_EDGES),
    PARADOX_COLLECTION_EDGE_CORES
  );
  /* The edge record is deliberately not coupled to `.sets`, so this is the
   * point where the edge list's own shape has to be established. */
  if (TYPEOF(sets) != VECSXP || ALTREP(sets)) {
    return FALSE;
  }
  const R_xlen_t count = XLENGTH(sets);
  if (XLENGTH(stamped) != count) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP child = VECTOR_ELT(sets, index);
    if (TYPEOF(child) != ENVSXP) {
      return FALSE;
    }
    SEXP child_private = private_from_self(child);
    SEXP current = child_private == R_UnboundValue
      ? R_UnboundValue
      : paradox_core_from_private_optional(child_private);
    if (current != VECTOR_ELT(stamped, index) ||
        !paradox_core_is_verified(current)) {
      return FALSE;
    }
  }
  return TRUE;
}

SEXP paradox_param_set_core_new(SEXP kind, SEXP state) {
  const paradox_core_kind_t parsed_kind = scalar_kind(kind);
  if (!paradox_core_state_exact_schema(state)) {
    Rf_error(
      "`state` must use the exact canonical eleven-field ParamSet state schema"
    );
  }
  SEXP payload = PROTECT(Rf_shallow_duplicate(state));
  SEXP result = PROTECT(new_core(parsed_kind, payload));
  const uintptr_t entry_epoch = core_state_epoch;
  if (parsed_kind == PARADOX_CORE_COLLECTION &&
      collection_edges_current(result) &&
      core_state_epoch == entry_epoch) {
    paradox_core_stamp_verified(result);
  }
  UNPROTECT(2);
  return result;
}

static SEXP state_from_selected_core(SEXP core) {
  if (core == R_UnboundValue) {
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }
  if (paradox_core_kind(core) == PARADOX_CORE_SHADOW &&
      !paradox_shadow_metadata_is_exact(core)) {
    Rf_error("Corrupt ParamSetShadow native snapshot metadata");
  }
  return R_ExternalPtrProtected(core);
}

SEXP paradox_param_set_core_state(SEXP owner, SEXP self) {
  if (paradox_core_is_canonical(owner)) {
    /* A directly supplied generation is the caller's chosen snapshot; it is
     * read as given, exactly as before. */
    return state_from_selected_core(owner);
  }
  SEXP core = paradox_core_from_private(owner);
  /* `self` is optional: a raw inspector may read the bound generation as it
   * stands, while every package read supplies the shell so a derived schema
   * can be brought current first. */
  if (core != R_UnboundValue && TYPEOF(self) == ENVSXP &&
      !paradox_core_is_verified(core)) {
    core = paradox_core_refresh(self, owner);
  }
  return state_from_selected_core(core);
}

SEXP paradox_param_set_core_kind(SEXP owner) {
  SEXP core;
  if (TYPEOF(owner) == EXTPTRSXP) {
    if (!paradox_core_is_canonical(owner)) {
      Rf_error("Corrupt ParamSet state: noncanonical versioned core capsule");
    }
    core = owner;
  } else {
    /* The optional private lookup already admits only canonical capsules. */
    core = paradox_core_from_private_optional(owner);
    if (core == R_UnboundValue) {
      Rf_error("Corrupt ParamSet state: missing versioned core capsule");
    }
  }
  const paradox_core_kind_t kind = kind_from_tag(R_ExternalPtrTag(core));
  return Rf_ScalarInteger((int) kind);
}

SEXP paradox_param_set_core_replace(SEXP owner, SEXP updates) {
  if (TYPEOF(owner) != ENVSXP) {
    Rf_error("Internal error: ParamSet state owner must be an environment");
  }
  SEXP old_core = PROTECT(paradox_core_from_private(owner));
  if (old_core == R_UnboundValue) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }
  if (TYPEOF(updates) != VECSXP || ALTREP(updates) || Rf_isS4(updates)) {
    UNPROTECT(1);
    Rf_error("Internal error: ParamSet state updates must be a named list");
  }
  SEXP names = PROTECT(Rf_getAttrib(updates, R_NamesSymbol));
  const R_xlen_t update_count = XLENGTH(updates);
  if (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
      XLENGTH(names) != update_count) {
    UNPROTECT(2);
    Rf_error("Internal error: ParamSet state updates must be named");
  }

  SEXP payload = PROTECT(Rf_shallow_duplicate(
    R_ExternalPtrProtected(old_core)
  ));
  /* Classify the installation while both generations are in hand: replacing a
   * field with the object it already held changes nothing, and only the fields
   * some other node's schema is derived from can invalidate a cached
   * flatten. */
  paradox_core_change_t change = PARADOX_CORE_CHANGE_NONE;
  unsigned char seen[PARADOX_CORE_FIELD_COUNT] = {0};
  for (R_xlen_t index = 0; index < update_count; ++index) {
    SEXP name = STRING_ELT(names, index);
    const int field = name == NA_STRING ? -1 : field_index(CHAR(name));
    const unsigned int field_offset = (unsigned int) field;
    if (field_offset < PARADOX_CORE_FIELD_COUNT &&
        seen[field_offset] == 0U) {
      seen[field_offset] = 1U;
      SEXP replacement_field = VECTOR_ELT(updates, index);
      if (replacement_field != VECTOR_ELT(payload, field_offset)) {
        if (core_field_is_schema(field)) {
          change = PARADOX_CORE_CHANGE_SCHEMA;
        } else if (change == PARADOX_CORE_CHANGE_NONE) {
          change = PARADOX_CORE_CHANGE_STATE;
        }
      }
      SET_VECTOR_ELT(payload, field_offset, replacement_field);
      continue;
    }
    UNPROTECT(3);
    Rf_error("Internal error: invalid or duplicate ParamSet state update");
  }

  const paradox_core_kind_t kind = paradox_core_kind(old_core);
  const uintptr_t entry_epoch = core_state_epoch;
  const int was_verified = paradox_core_is_verified(old_core);
  SEXP replacement = PROTECT(new_core(kind, payload));
  if (kind == PARADOX_CORE_SHADOW) {
    paradox_shadow_copy_metadata(old_core, replacement);
  }
  /* Constructing the replacement allocates, so a finalizer could have run and
   * installed a capsule of its own; carrying the old proof across that would
   * certify a world this generation never saw. */
  const int still_verified = was_verified && core_state_epoch == entry_epoch;
  paradox_core_note_change(change);
  /* A COLLECTION whose own value/dependency fields moved keeps its proof:
   * nothing its flatten is derived from changed, and the schema epoch did not
   * advance. Every other case revalidates once. */
  if (still_verified && kind == PARADOX_CORE_COLLECTION &&
      change != PARADOX_CORE_CHANGE_SCHEMA) {
    paradox_core_stamp_verified(replacement);
  }
  Rf_defineVar(Rf_install(".core"), replacement, owner);
  UNPROTECT(4);
  return replacement;
}
