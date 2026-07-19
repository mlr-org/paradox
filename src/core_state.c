#include <string.h>

#include "core_state.h"
#include <R_ext/Utils.h>
#include "paramset_shadow.h"
#include "r_api_compat.h"
#include "r_utils.h"

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

SEXP paradox_core_from_private(SEXP private_environment) {
  if (TYPEOF(private_environment) != ENVSXP ||
      Rf_isS4(private_environment)) {
    return R_UnboundValue;
  }
  /* All capsule-topology reads cross the same non-forcing ordinary-binding
   * boundary.  This rejects active and delayed bindings before canonical core
   * validation, including on the supported pre-4.6 runtimes. */
  SEXP core_symbol = Rf_install(".core");
  SEXP core = PROTECT(paradox_api_plain_binding_snapshot(
    private_environment,
    core_symbol
  ));
  const int canonical = paradox_core_is_canonical(core);
  UNPROTECT(1);
  return canonical ? core : R_UnboundValue;
}

static SEXP private_from_self(SEXP self) {
  if (TYPEOF(self) != ENVSXP || Rf_isS4(self)) {
    return R_UnboundValue;
  }
  SEXP enclosure_symbol = Rf_install(".__enclos_env__");
  SEXP enclosure = PROTECT(paradox_api_plain_binding_snapshot(
    self,
    enclosure_symbol
  ));
  SEXP private_symbol = Rf_install("private");
  if (TYPEOF(enclosure) != ENVSXP || Rf_isS4(enclosure)) {
    UNPROTECT(1);
    return R_UnboundValue;
  }
  SEXP owned_private = PROTECT(paradox_api_plain_binding_snapshot(
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

static void account_graph_work(R_xlen_t *work_since_interrupt) {
  ++*work_since_interrupt;
  if (*work_since_interrupt >= PARADOX_INTERRUPT_CHECK_INTERVAL) {
    R_CheckUserInterrupt();
    *work_since_interrupt = 0;
  }
}

void paradox_core_validate_graph_path(SEXP root) {
  R_xlen_t capacity = 16;
  core_graph_frame_t *frames = paradox_temporary_alloc(
    capacity,
    sizeof(*frames)
  );
  R_xlen_t depth = 1;
  R_xlen_t work_since_interrupt = 0;
  frames[0] = (core_graph_frame_t) {
    root, R_NilValue, 0, 0, FALSE
  };

  while (depth != 0) {
    account_graph_work(&work_since_interrupt);
    core_graph_frame_t *frame = &frames[depth - 1];
    if (!frame->entered) {
      SEXP private_environment = PROTECT(private_from_self(frame->self));
      SEXP core = private_environment == R_UnboundValue
        ? R_UnboundValue
        : paradox_core_from_private(private_environment);
      if (core == R_UnboundValue) {
        UNPROTECT(1);
        Rf_error("Corrupt ParamSet node in capsule graph");
      }
      const paradox_core_kind_t kind = paradox_core_kind(core);
      SEXP state = R_ExternalPtrProtected(core);
      frame->sets = kind == PARADOX_CORE_BASE
        ? R_NilValue
        : VECTOR_ELT(state, PARADOX_CORE_SETS);
      if (kind == PARADOX_CORE_BASE) {
        frame->child_count = 0;
      } else if (TYPEOF(frame->sets) != VECSXP || ALTREP(frame->sets) ||
          (kind == PARADOX_CORE_SHADOW && XLENGTH(frame->sets) != 1)) {
        UNPROTECT(1);
        Rf_error("Corrupt ParamSet capsule graph edges");
      } else {
        frame->child_count = XLENGTH(frame->sets);
      }
      frame->next_child = 0;
      frame->entered = TRUE;
      UNPROTECT(1);
    }

    if (frame->next_child == frame->child_count) {
      --depth;
      continue;
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
    if (depth == capacity) {
      if (capacity > R_XLEN_T_MAX / 2) {
        Rf_error("ParamSet capsule graph is too deep");
      }
      const R_xlen_t expanded_capacity = capacity * 2;
      core_graph_frame_t *expanded = paradox_temporary_alloc(
        expanded_capacity,
        sizeof(*expanded)
      );
      memcpy(expanded, frames, (size_t) depth * sizeof(*expanded));
      frames = expanded;
      capacity = expanded_capacity;
    }
    frames[depth] = (core_graph_frame_t) {
      child, R_NilValue, 0, 0, FALSE
    };
    ++depth;
  }
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
  return paradox_core_from_private(owner);
}

SEXP paradox_param_set_core_state(SEXP owner) {
  SEXP core = core_from_owner(owner);
  if (core == R_UnboundValue) {
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }
  if (paradox_core_kind(core) == PARADOX_CORE_SHADOW &&
      !paradox_shadow_metadata_is_exact(core)) {
    Rf_error("Corrupt ParamSetShadow native snapshot metadata");
  }
  return R_ExternalPtrProtected(core);
}

SEXP paradox_param_set_core_kind(SEXP owner) {
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
