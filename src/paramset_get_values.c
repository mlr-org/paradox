#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "builtin_condition.h"
#include "core_state.h"
#include "paramset_activity.h"
#include "paramset_collection_readers.h"
#include "paramset_domain_common.h"
#include "r_utils.h"

typedef enum {
  GET_VALUES_WITH_TOKEN = 0,
  GET_VALUES_WITHOUT_TOKEN,
  GET_VALUES_ONLY_TOKEN,
  GET_VALUES_WITH_INTERNAL
} get_values_type_t;

typedef struct {
  SEXP ids;
  R_xlen_t *slots;
  R_xlen_t capacity;
} get_values_id_index_t;

typedef struct {
  SEXP params;
  SEXP tags;
  SEXP values;
  SEXP dependencies;
  paradox_domain_params_t params_data;
  paradox_domain_tags_t tags_data;
  paradox_domain_values_t values_data;
  paradox_domain_dependencies_t dependencies_data;
  get_values_id_index_t parameter_ids;
  R_xlen_t *value_by_parameter;
  R_xlen_t *dependency_id_parameter;
  R_xlen_t *dependency_on_parameter;
  SEXP *dependency_rhs;
  unsigned char *required_by_parameter;
  R_xlen_t required_count;
} get_values_snapshot_t;

enum get_values_root_slot {
  GET_VALUES_ROOT_SELF = 0,
  GET_VALUES_ROOT_PRIVATE,
  GET_VALUES_ROOT_CORE,
  GET_VALUES_ROOT_PARAMS,
  GET_VALUES_ROOT_TAGS,
  GET_VALUES_ROOT_VALUES,
  GET_VALUES_ROOT_DEPENDENCIES,
  GET_VALUES_ROOT_COUNT
};

static R_xlen_t id_index_slot(SEXP id, R_xlen_t capacity) {
  uintptr_t value = (uintptr_t) id;
  value ^= value >> 4;
  value ^= value >> 9;
  return (R_xlen_t) (value & (uintptr_t) (capacity - 1));
}

static get_values_id_index_t build_id_index(SEXP ids,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t count = XLENGTH(ids);
  if (count == 0) {
    return (get_values_id_index_t) {ids, NULL, 0};
  }
  if (count > R_XLEN_T_MAX / 2) {
    Rf_error("ParamSet parameter ID index is too large");
  }
  const R_xlen_t required = count * 2;
  R_xlen_t capacity = 1;
  while (capacity < required) {
    if (capacity > R_XLEN_T_MAX / 2) {
      Rf_error("ParamSet parameter ID index is too large");
    }
    capacity *= 2;
  }

  R_xlen_t *slots = paradox_temporary_alloc(capacity, sizeof(*slots));
  for (R_xlen_t slot = 0; slot < capacity; ++slot) {
    slots[slot] = 0;
  }
  for (R_xlen_t row = 0; row < count; ++row) {
    paradox_account_work(work_since_interrupt);
    SEXP id = STRING_ELT(ids, row);
    R_xlen_t slot = id_index_slot(id, capacity);
    while (slots[slot] != 0) {
      slot = (slot + 1) & (capacity - 1);
    }
    slots[slot] = row + 1;
  }
  return (get_values_id_index_t) {ids, slots, capacity};
}

static R_xlen_t id_index_find(const get_values_id_index_t *index, SEXP sought,
    R_xlen_t *work_since_interrupt) {
  if (index->capacity != 0) {
    R_xlen_t slot = id_index_slot(sought, index->capacity);
    while (index->slots[slot] != 0) {
      paradox_account_work(work_since_interrupt);
      const R_xlen_t row = index->slots[slot] - 1;
      if (STRING_ELT(index->ids, row) == sought) {
        return row;
      }
      slot = (slot + 1) & (index->capacity - 1);
    }
  }

  /* Canonical package IDs are interned CHARSXP values, so pointer identity is
   * authoritative on the maintained path. A forged but structurally admitted
   * capsule may use an equivalent string in a different encoding; retain the
   * existing encoding-aware comparison as the uncommon fallback. */
  const R_xlen_t count = XLENGTH(index->ids);
  for (R_xlen_t row = 0; row < count; ++row) {
    paradox_account_work(work_since_interrupt);
    if (paradox_domain_strings_equal(STRING_ELT(index->ids, row), sought)) {
      return row;
    }
  }
  return R_XLEN_T_MAX;
}

static get_values_type_t parse_type(SEXP frame) {
  SEXP value = PROTECT(Rf_eval(Rf_install("type"), frame));
  if (TYPEOF(value) != STRSXP) {
    UNPROTECT(1);
    Rf_error(
      "Assertion on 'type' failed: Must be a single character value."
    );
  }
  const R_xlen_t size = XLENGTH(value);
  if (size != 1) {
    UNPROTECT(1);
    Rf_error(
      "Assertion on 'type' failed: Must be a single character value."
    );
  }

  /* Copy the one public element exactly once before comparing it. This makes
   * a compact or third-party ALTSTRING an operation input, never permanent
   * state and never a provider consulted twice. */
  SEXP element = PROTECT(STRING_ELT(value, 0));
  if (element == NA_STRING || Rf_getCharCE(element) == CE_BYTES) {
    UNPROTECT(2);
    Rf_error(
      "Assertion on 'type' failed: Must be one of 'with_token', "
      "'without_token', 'only_token', or 'with_internal'."
    );
  }

  get_values_type_t result;
  if (paradox_domain_string_is(element, "with_token")) {
    result = GET_VALUES_WITH_TOKEN;
  } else if (paradox_domain_string_is(element, "without_token")) {
    result = GET_VALUES_WITHOUT_TOKEN;
  } else if (paradox_domain_string_is(element, "only_token")) {
    result = GET_VALUES_ONLY_TOKEN;
  } else if (paradox_domain_string_is(element, "with_internal")) {
    result = GET_VALUES_WITH_INTERNAL;
  } else {
    UNPROTECT(2);
    Rf_error(
      "Assertion on 'type' failed: Must be one of 'with_token', "
      "'without_token', 'only_token', or 'with_internal'."
    );
  }
  UNPROTECT(2);
  return result;
}

static int parse_flag(SEXP frame, const char *name) {
  SEXP value = PROTECT(Rf_eval(Rf_install(name), frame));
  if (TYPEOF(value) != LGLSXP) {
    UNPROTECT(1);
    Rf_error(
      "Assertion on '%s' failed: Must be of type 'logical flag'.",
      name
    );
  }
  const R_xlen_t size = XLENGTH(value);
  if (size != 1) {
    UNPROTECT(1);
    Rf_error(
      "Assertion on '%s' failed: Must be of type 'logical flag'.",
      name
    );
  }
  const int result = LOGICAL_ELT(value, 0);
  UNPROTECT(1);
  if (result == NA_LOGICAL) {
    Rf_error(
      "Assertion on '%s' failed: Must be of type 'logical flag'.",
      name
    );
  }
  return result;
}

static int map_values_to_parameters(get_values_snapshot_t *snapshot,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t parameter_count = snapshot->params_data.row_count;
  snapshot->value_by_parameter = paradox_temporary_alloc(
    parameter_count == 0 ? 1 : parameter_count,
    sizeof(*snapshot->value_by_parameter)
  );
  for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
    snapshot->value_by_parameter[parameter] = R_XLEN_T_MAX;
  }

  R_xlen_t previous_parameter = 0;
  int have_previous = FALSE;
  for (R_xlen_t value = 0; value < snapshot->values_data.size; ++value) {
    paradox_account_work(work_since_interrupt);
    SEXP name = STRING_ELT(snapshot->values_data.names, value);
    const R_xlen_t parameter = id_index_find(
      &snapshot->parameter_ids,
      name,
      work_since_interrupt
    );
    /* Capsule values are a schema-ordered subsequence. Preserve the old
     * corruption boundary instead of accepting an arbitrary reordered store
     * merely because the index can resolve every name independently. */
    if (parameter == R_XLEN_T_MAX ||
        (have_previous && parameter <= previous_parameter)) {
      return FALSE;
    }
    snapshot->value_by_parameter[parameter] = value;
    previous_parameter = parameter;
    have_previous = TRUE;
  }
  return TRUE;
}

static int map_tags_and_required(get_values_snapshot_t *snapshot,
    SEXP required_tag,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t parameter_count = snapshot->params_data.row_count;
  snapshot->required_by_parameter = paradox_temporary_alloc(
    parameter_count == 0 ? 1 : parameter_count,
    sizeof(*snapshot->required_by_parameter)
  );
  for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
    snapshot->required_by_parameter[parameter] = FALSE;
  }
  snapshot->required_count = 0;

  for (R_xlen_t row = 0; row < snapshot->tags_data.row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    const R_xlen_t owner = id_index_find(
      &snapshot->parameter_ids,
      STRING_ELT(snapshot->tags_data.ids, row),
      work_since_interrupt
    );
    if (owner == R_XLEN_T_MAX) {
      return FALSE;
    }
    if (!snapshot->required_by_parameter[owner] &&
        paradox_domain_strings_equal(
          STRING_ELT(snapshot->tags_data.values, row),
          required_tag
        )) {
      snapshot->required_by_parameter[owner] = TRUE;
      ++snapshot->required_count;
    }
  }
  return TRUE;
}

static void admit_values(SEXP values, paradox_domain_values_t *validated,
    R_xlen_t *work_since_interrupt) {
  if (!paradox_domain_validate_values(
      values,
      validated,
      work_since_interrupt
    )) {
    Rf_error("Corrupt ParamSet capsule: invalid `.values` field");
  }
  for (R_xlen_t index = 0; index < validated->size; ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP element = VECTOR_ELT(validated->values, index);
    if (element == R_UnboundValue || element == R_MissingArg ||
        TYPEOF(element) == PROMSXP) {
      Rf_error("Corrupt ParamSet capsule: invalid `.values` element");
    }
  }
}

static void load_snapshot(SEXP private_environment, SEXP self, SEXP roots,
    get_values_snapshot_t *snapshot,
    R_xlen_t *work_since_interrupt) {
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("Corrupt ParamSet shell ownership");
  }

  SEXP core = paradox_core_from_private(private_environment);
  if (core == R_UnboundValue) {
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }
  if (paradox_core_kind(core) == PARADOX_CORE_SHADOW) {
    core = paradox_core_refresh_shadow(self, private_environment);
  }
  const paradox_core_kind_t kind = paradox_core_kind(core);
  if (kind != PARADOX_CORE_BASE && kind != PARADOX_CORE_COLLECTION &&
      kind != PARADOX_CORE_SHADOW) {
    Rf_error("Corrupt ParamSet state: unknown core node kind");
  }
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_CORE, core);

  SEXP payload = paradox_core_payload(core);
  if (payload == R_UnboundValue) {
    Rf_error("Corrupt ParamSet state capsule");
  }
  snapshot->params = VECTOR_ELT(payload, PARADOX_CORE_PARAMS);
  snapshot->tags = VECTOR_ELT(payload, PARADOX_CORE_TAGS);
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_PARAMS, snapshot->params);
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_TAGS, snapshot->tags);

  if (kind == PARADOX_CORE_COLLECTION) {
    /* Values and dependencies are two projections of the same live graph.
     * Admit that graph once so shared Shadows are refreshed once and both
     * projections retain exactly the same capsule generations.  The dependency
     * emitter already returns the canonical internal plain data.frame, so an
     * outward data.table facade and immediate plain-table copy would be both
     * wasteful and a less coherent snapshot. */
    PROTECT_INDEX graph_roots_index;
    SEXP graph_roots;
    PROTECT_WITH_INDEX(graph_roots = R_NilValue, &graph_roots_index);
    paradox_collection_graph_t graph;
    paradox_collection_graph_build(
      private_environment,
      self,
      &graph,
      &graph_roots,
      graph_roots_index,
      work_since_interrupt
    );

    SEXP values = PROTECT(paradox_collection_values_from_graph(
      &graph,
      work_since_interrupt
    ));
    snapshot->values = values;
    SET_VECTOR_ELT(roots, GET_VALUES_ROOT_VALUES, snapshot->values);
    admit_values(
      snapshot->values,
      &snapshot->values_data,
      work_since_interrupt
    );
    UNPROTECT(1);

    SEXP dependencies = PROTECT(paradox_collection_dependencies_from_graph(
      &graph,
      work_since_interrupt
    ));
    snapshot->dependencies = dependencies;
    SET_VECTOR_ELT(
      roots,
      GET_VALUES_ROOT_DEPENDENCIES,
      snapshot->dependencies
    );
    /* `roots` now owns both projections; release the temporary result and the
     * graph capsule-root chain together. */
    UNPROTECT(2);
  } else {
    snapshot->values = VECTOR_ELT(payload, PARADOX_CORE_VALUES);
    SET_VECTOR_ELT(roots, GET_VALUES_ROOT_VALUES, snapshot->values);
    admit_values(
      snapshot->values,
      &snapshot->values_data,
      work_since_interrupt
    );
    snapshot->dependencies = VECTOR_ELT(payload, PARADOX_CORE_DEPS);
    SET_VECTOR_ELT(
      roots,
      GET_VALUES_ROOT_DEPENDENCIES,
      snapshot->dependencies
    );
  }

  R_xlen_t unused_row = 0;
  if (!paradox_domain_validate_params(
      snapshot->params,
      R_NilValue,
      TRUE,
      &snapshot->params_data,
      &unused_row,
      work_since_interrupt
  )) {
    Rf_error("Corrupt ParamSet capsule: invalid `.params` field");
  }
  snapshot->parameter_ids = build_id_index(
    snapshot->params_data.ids,
    work_since_interrupt
  );
  if (!paradox_domain_validate_tags(
      snapshot->tags,
      &snapshot->tags_data,
      work_since_interrupt
    )) {
    Rf_error("Corrupt ParamSet capsule: invalid `.tags` field");
  }
  SEXP required_tag = PROTECT(Rf_mkChar("required"));
  const int valid_tags = map_tags_and_required(
    snapshot,
    required_tag,
    work_since_interrupt
  );
  UNPROTECT(1);
  if (!valid_tags) {
    Rf_error("Corrupt ParamSet capsule: invalid `.tags` field");
  }
  if (!map_values_to_parameters(snapshot, work_since_interrupt)) {
    Rf_error("Corrupt ParamSet capsule: invalid `.values` field");
  }
  if (!paradox_domain_validate_dependencies_with_rhs(
      snapshot->dependencies,
      &snapshot->dependencies_data,
      &snapshot->dependency_rhs,
      work_since_interrupt
    )) {
    Rf_error("Corrupt ParamSet capsule: invalid `.deps` field");
  }
  const R_xlen_t dependency_count = snapshot->dependencies_data.row_count;
  snapshot->dependency_id_parameter = paradox_temporary_alloc(
    dependency_count,
    sizeof(*snapshot->dependency_id_parameter)
  );
  snapshot->dependency_on_parameter = paradox_temporary_alloc(
    dependency_count,
    sizeof(*snapshot->dependency_on_parameter)
  );
  for (R_xlen_t row = 0; row < dependency_count; ++row) {
    paradox_account_work(work_since_interrupt);
    const R_xlen_t dependent = id_index_find(
      &snapshot->parameter_ids,
      STRING_ELT(snapshot->dependencies_data.ids, row),
      work_since_interrupt
    );
    if (dependent == R_XLEN_T_MAX) {
      Rf_error("Corrupt ParamSet capsule: dependency owner is unknown");
    }
    snapshot->dependency_id_parameter[row] = dependent;
    snapshot->dependency_on_parameter[row] = id_index_find(
      &snapshot->parameter_ids,
      STRING_ELT(snapshot->dependencies_data.on, row),
      work_since_interrupt
    );
  }
}

static void evaluate_activity(const get_values_snapshot_t *snapshot,
    paradox_activity_result_t *result,
    R_xlen_t *work_since_interrupt) {
  result->active = paradox_temporary_alloc(
    snapshot->params_data.row_count == 0
      ? 1
      : snapshot->params_data.row_count,
    sizeof(*result->active)
  );
  result->reasons = NULL;
  const paradox_activity_plan_t plan = {
    snapshot->params_data.row_count,
    VECTOR_ELT(snapshot->params, PARADOX_DOMAIN_DEFAULT),
    snapshot->values_data.values,
    snapshot->value_by_parameter,
    snapshot->dependencies_data.row_count,
    snapshot->dependency_id_parameter,
    snapshot->dependency_on_parameter,
    (SEXP const *) snapshot->dependency_rhs
  };
  paradox_activity_evaluate(&plan, result, work_since_interrupt);
}

static void apply_dependencies(const get_values_snapshot_t *snapshot,
    const unsigned char *active, int *kept,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t parameter = 0;
      parameter < snapshot->params_data.row_count;
      ++parameter) {
    paradox_account_work(work_since_interrupt);
    const R_xlen_t value = snapshot->value_by_parameter[parameter];
    if (value != R_XLEN_T_MAX && !active[parameter]) {
      kept[value] = FALSE;
    }
  }
}

static void apply_type_filter(const get_values_snapshot_t *snapshot,
    get_values_type_t type, int *kept,
    R_xlen_t *work_since_interrupt) {
  if (type == GET_VALUES_WITH_TOKEN) {
    return;
  }
  for (R_xlen_t index = 0; index < snapshot->values_data.size; ++index) {
    paradox_account_work(work_since_interrupt);
    if (!kept[index]) {
      continue;
    }
    SEXP value = VECTOR_ELT(snapshot->values_data.values, index);
    const int token = Rf_inherits(value, "TuneToken") != FALSE;
    if (type == GET_VALUES_WITHOUT_TOKEN) {
      kept[index] = !token;
    } else if (type == GET_VALUES_ONLY_TOKEN) {
      kept[index] = token;
    } else {
      kept[index] = Rf_inherits(value, "InternalTuneToken") != FALSE;
    }
  }
}

static void check_required(const get_values_snapshot_t *snapshot,
    const unsigned char *active,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t size = snapshot->required_count;
  if (size == 0) {
    return;
  }
  R_xlen_t missing_count = 0;
  size_t text_size = 1;
  size_t *sizes = paradox_temporary_alloc(size, sizeof(*sizes));
  const void *vmax = vmaxget();
  for (R_xlen_t parameter = 0;
      parameter < snapshot->params_data.row_count;
      ++parameter) {
    paradox_account_work(work_since_interrupt);
    if (!snapshot->required_by_parameter[parameter]) {
      continue;
    }
    if ((active != NULL && !active[parameter]) ||
        snapshot->value_by_parameter[parameter] != R_XLEN_T_MAX) {
      continue;
    }
    SEXP id = STRING_ELT(snapshot->params_data.ids, parameter);
    const size_t id_size = strlen(Rf_translateCharUTF8(id));
    const size_t separator = missing_count == 0 ? 0 : 2;
    if (separator > SIZE_MAX - text_size ||
        id_size > SIZE_MAX - text_size - separator) {
      vmaxset(vmax);
      Rf_error("Unable to construct required-parameter diagnostic");
    }
    sizes[missing_count] = id_size;
    text_size += separator + id_size;
    ++missing_count;
  }
  vmaxset(vmax);
  if (missing_count == 0) {
    return;
  }
  if ((uintmax_t) text_size > (uintmax_t) R_XLEN_T_MAX) {
    Rf_error("Unable to construct required-parameter diagnostic");
  }

  char *text = paradox_temporary_alloc((R_xlen_t) text_size, sizeof(*text));
  size_t offset = 0;
  R_xlen_t emitted = 0;
  vmax = vmaxget();
  for (R_xlen_t parameter = 0;
      parameter < snapshot->params_data.row_count;
      ++parameter) {
    paradox_account_work(work_since_interrupt);
    if (!snapshot->required_by_parameter[parameter]) {
      continue;
    }
    if ((active != NULL && !active[parameter]) ||
        snapshot->value_by_parameter[parameter] != R_XLEN_T_MAX) {
      continue;
    }
    if (emitted != 0) {
      text[offset++] = ',';
      text[offset++] = ' ';
    }
    SEXP id = STRING_ELT(snapshot->params_data.ids, parameter);
    const char *id_text = Rf_translateCharUTF8(id);
    const size_t id_size = strlen(id_text);
    if (id_size != sizes[emitted] || id_size > text_size - 1 - offset) {
      vmaxset(vmax);
      Rf_error("Unable to construct required-parameter diagnostic");
    }
    memcpy(text + offset, id_text, id_size);
    offset += id_size;
    ++emitted;
  }
  if (emitted != missing_count) {
    vmaxset(vmax);
    Rf_error("Unable to construct required-parameter diagnostic");
  }
  text[offset] = '\0';
  /* `text` predates the translation watermark and remains valid here.  The
   * non-local error exit releases both it and any translation scratch. */
  Rf_error("Missing required parameters: %s", text);
}

static SEXP build_result(const get_values_snapshot_t *snapshot,
    const int *kept, SEXP selected_ids,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t selected_count = XLENGTH(selected_ids);
  R_xlen_t result_count = 0;
  R_xlen_t selected = 0;
  for (R_xlen_t parameter = 0;
      parameter < snapshot->params_data.row_count && selected < selected_count;
      ++parameter) {
    paradox_account_work(work_since_interrupt);
    if (!paradox_domain_strings_equal(
        STRING_ELT(snapshot->params_data.ids, parameter),
        STRING_ELT(selected_ids, selected)
      )) {
      continue;
    }
    const R_xlen_t value = snapshot->value_by_parameter[parameter];
    result_count += value != R_XLEN_T_MAX && kept[value];
    ++selected;
  }
  if (selected != selected_count) {
    Rf_error("Internal error: selected ID is absent from ParamSet schema");
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, result_count));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, result_count));
  R_xlen_t output = 0;
  selected = 0;
  for (R_xlen_t parameter = 0;
      parameter < snapshot->params_data.row_count && selected < selected_count;
      ++parameter) {
    paradox_account_work(work_since_interrupt);
    if (!paradox_domain_strings_equal(
        STRING_ELT(snapshot->params_data.ids, parameter),
        STRING_ELT(selected_ids, selected)
      )) {
      continue;
    }
    const R_xlen_t value = snapshot->value_by_parameter[parameter];
    if (value == R_XLEN_T_MAX || !kept[value]) {
      ++selected;
      continue;
    }
    SET_VECTOR_ELT(
      result,
      output,
      VECTOR_ELT(snapshot->values_data.values, value)
    );
    SET_STRING_ELT(
      names,
      output,
      STRING_ELT(selected_ids, selected)
    );
    ++output;
    ++selected;
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
}

SEXP paradox_param_set_get_values(SEXP private_environment, SEXP self,
    SEXP frame) {
  if (TYPEOF(frame) != ENVSXP) {
    Rf_error("Internal error: ParamSet get_values frame must be an environment");
  }

  /* Public formals are forced and materialized exactly once in their
   * documented left-to-right order. No capsule field is observed until all
   * six inputs have crossed that boundary. */
  SEXP class_filter = PROTECT(paradox_param_set_filter_argument(
    frame,
    "class"
  ));
  SEXP all_tags = PROTECT(paradox_param_set_filter_argument(frame, "tags"));
  SEXP any_tags = PROTECT(paradox_param_set_filter_argument(
    frame,
    "any_tags"
  ));
  const get_values_type_t type = parse_type(frame);
  const int should_check_required = parse_flag(frame, "check_required");
  const int should_remove_dependencies = parse_flag(
    frame,
    "remove_dependencies"
  );

  SEXP roots = PROTECT(Rf_allocVector(VECSXP, GET_VALUES_ROOT_COUNT));
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_SELF, self);
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_PRIVATE, private_environment);
  get_values_snapshot_t snapshot;
  R_xlen_t work_since_interrupt = 0;
  load_snapshot(
    private_environment,
    self,
    roots,
    &snapshot,
    &work_since_interrupt
  );

  int *kept = paradox_temporary_alloc(
    snapshot.values_data.size,
    sizeof(*kept)
  );
  for (R_xlen_t index = 0; index < snapshot.values_data.size; ++index) {
    paradox_account_work(&work_since_interrupt);
    kept[index] = TRUE;
  }
  paradox_activity_result_t activity = {NULL, NULL};
  const int has_dependencies =
    snapshot.dependencies_data.row_count != 0;
  const int required_needs_activity =
    should_check_required && snapshot.required_count != 0;
  if (has_dependencies &&
      (should_remove_dependencies || required_needs_activity)) {
    evaluate_activity(
      &snapshot,
      &activity,
      &work_since_interrupt
    );
  }
  if (should_remove_dependencies && has_dependencies) {
    apply_dependencies(
      &snapshot,
      activity.active,
      kept,
      &work_since_interrupt
    );
  }
  apply_type_filter(&snapshot, type, kept, &work_since_interrupt);
  if (should_check_required) {
    check_required(
      &snapshot,
      activity.active,
      &work_since_interrupt
    );
  }

  SEXP selected_ids = PROTECT(
    class_filter == R_NilValue && all_tags == R_NilValue &&
      any_tags == R_NilValue
      ? snapshot.params_data.ids
      : paradox_param_set_ids(
          snapshot.params,
          snapshot.tags,
          class_filter,
          all_tags,
          any_tags
        )
  );
  SEXP result = build_result(
    &snapshot,
    kept,
    selected_ids,
    &work_since_interrupt
  );
  UNPROTECT(5);
  return result;
}
