#include <limits.h>
#include <string.h>

#include "core_state.h"
#include <R_ext/Utils.h>
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
  ".postfix"
};

static SEXP core_tag(paradox_core_kind_t kind) {
  switch (kind) {
  case PARADOX_CORE_BASE:
    return Rf_install("paradox.core.base.v1");
  case PARADOX_CORE_COLLECTION:
    return Rf_install("paradox.core.collection.v1");
  case PARADOX_CORE_SHADOW:
    return Rf_install("paradox.core.shadow.v1");
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
  return 0;
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

static int exact_payload_schema(SEXP payload) {
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

static SEXP new_core(paradox_core_kind_t kind, SEXP payload) {
  if (kind < PARADOX_CORE_BASE || kind > PARADOX_CORE_SHADOW ||
      !exact_payload(payload)) {
    Rf_error("Internal error: invalid ParamSet core construction");
  }
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
  return TYPEOF(core) == EXTPTRSXP && !Rf_isS4(core) &&
    R_ExternalPtrAddr(core) == NULL &&
    !Rf_isS4(R_ExternalPtrTag(core)) &&
    kind_from_tag(R_ExternalPtrTag(core)) != 0 &&
    exact_payload(R_ExternalPtrProtected(core));
}

int paradox_core_has_exact_schema(SEXP core) {
  if (!paradox_core_is_valid(core)) {
    return FALSE;
  }
  return exact_payload_schema(R_ExternalPtrProtected(core));
}

int paradox_core_is_canonical(SEXP core) {
  return paradox_core_has_exact_schema(core) &&
    exact_carrier_attributes(core);
}

paradox_core_kind_t paradox_core_kind(SEXP core) {
  return paradox_core_is_valid(core)
    ? kind_from_tag(R_ExternalPtrTag(core))
    : 0;
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

static void account_graph_work(R_xlen_t *work_since_interrupt) {
  ++*work_since_interrupt;
  if (*work_since_interrupt >= PARADOX_INTERRUPT_CHECK_INTERVAL) {
    R_CheckUserInterrupt();
    *work_since_interrupt = 0;
  }
}

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
   * Slots are cleared on pop, so memory is proportional to depth rather than
   * to every node visited. The inline common stack replaces the old initial
   * R_alloc(), leaving only one smaller managed allocation on shallow paths.
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

  while (depth != 0) {
    account_graph_work(&work_since_interrupt);
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
    for (R_xlen_t ancestor = 0; ancestor < depth; ++ancestor) {
      account_graph_work(&work_since_interrupt);
      if (frames[ancestor].self == child) {
        Rf_error("ParamSet capsule graph contains a cycle");
      }
    }
    frames[depth] = (core_graph_frame_t) {
      child, R_NilValue, 0, 0, FALSE
    };
    SET_VECTOR_ELT(
      roots,
      core_graph_root_slot(depth, CORE_GRAPH_ROOT_SELF),
      child
    );
    ++depth;
  }
  UNPROTECT(2);
}

SEXP paradox_core_refresh_shadow(SEXP self, SEXP private_environment) {
  return paradox_shadow_refresh_authoritative(self, private_environment);
}

SEXP paradox_core_state_from_private(SEXP private_environment) {
  SEXP core = paradox_core_from_private(private_environment);
  if (core == R_UnboundValue ||
      (paradox_core_kind(core) == PARADOX_CORE_SHADOW &&
       !paradox_shadow_metadata_is_exact(core))) {
    return R_UnboundValue;
  }
  return R_ExternalPtrProtected(core);
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

SEXP paradox_param_set_core_new(SEXP kind, SEXP state) {
  const paradox_core_kind_t parsed_kind = scalar_kind(kind);
  if (!exact_payload_schema(state)) {
    Rf_error(
      "`state` must use the exact canonical ten-field ParamSet state schema"
    );
  }
  SEXP payload = PROTECT(Rf_shallow_duplicate(state));
  SEXP result = PROTECT(new_core(parsed_kind, payload));
  UNPROTECT(2);
  return result;
}

static SEXP core_from_owner(SEXP owner) {
  if (paradox_core_is_canonical(owner)) {
    return owner;
  }
  return paradox_core_from_private_optional(owner);
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

SEXP paradox_param_set_core_state(SEXP owner) {
  return state_from_selected_core(
    paradox_core_is_canonical(owner)
      ? owner
      : paradox_core_from_private(owner)
  );
}

SEXP paradox_param_set_core_kind(SEXP owner) {
  if (TYPEOF(owner) == EXTPTRSXP && !paradox_core_is_canonical(owner)) {
    Rf_error("Corrupt ParamSet state: noncanonical versioned core capsule");
  }
  SEXP core = core_from_owner(owner);
  if (core == R_UnboundValue) {
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }
  const paradox_core_kind_t kind = paradox_core_kind(core);
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
  unsigned char seen[PARADOX_CORE_FIELD_COUNT] = {0};
  for (R_xlen_t index = 0; index < update_count; ++index) {
    SEXP name = STRING_ELT(names, index);
    const int field = name == NA_STRING ? -1 : field_index(CHAR(name));
    const unsigned int field_offset = (unsigned int) field;
    if (field_offset < PARADOX_CORE_FIELD_COUNT &&
        seen[field_offset] == 0U) {
      seen[field_offset] = 1U;
      SET_VECTOR_ELT(payload, field_offset, VECTOR_ELT(updates, index));
      continue;
    }
    UNPROTECT(3);
    Rf_error("Internal error: invalid or duplicate ParamSet state update");
  }

  SEXP replacement = PROTECT(new_core(paradox_core_kind(old_core), payload));
  Rf_defineVar(Rf_install(".core"), replacement, owner);
  UNPROTECT(4);
  return replacement;
}
