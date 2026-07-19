#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "builtin_condition.h"
#include "core_state.h"
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
  SEXP params;
  SEXP tags;
  SEXP values;
  SEXP dependencies;
  paradox_domain_params_t params_data;
  paradox_domain_tags_t tags_data;
  paradox_domain_values_t values_data;
  paradox_domain_dependencies_t dependencies_data;
  R_xlen_t *value_by_parameter;
  R_xlen_t *dependency_id_parameter;
  R_xlen_t *dependency_on_parameter;
  SEXP *dependency_rhs;
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

static int strings_equal(SEXP left, SEXP right) {
  if (left == right) {
    return TRUE;
  }
  if (left == NA_STRING || right == NA_STRING) {
    return FALSE;
  }
  if (Rf_getCharCE(left) == Rf_getCharCE(right)) {
    return strcmp(CHAR(left), CHAR(right)) == 0;
  }
  return paradox_domain_strings_equal(left, right);
}

static R_xlen_t find_string(SEXP haystack, SEXP needle,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t size = XLENGTH(haystack);
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if (strings_equal(STRING_ELT(haystack, index), needle)) {
      return index;
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
    parameter_count,
    sizeof(*snapshot->value_by_parameter)
  );
  for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
    snapshot->value_by_parameter[parameter] = R_XLEN_T_MAX;
  }

  R_xlen_t parameter = 0;
  for (R_xlen_t value = 0; value < snapshot->values_data.size; ++value) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP name = STRING_ELT(snapshot->values_data.names, value);
    while (parameter < parameter_count && !strings_equal(
        STRING_ELT(snapshot->params_data.ids, parameter),
        name
      )) {
      paradox_domain_account_work(work_since_interrupt);
      ++parameter;
    }
    if (parameter == parameter_count) {
      return FALSE;
    }
    snapshot->value_by_parameter[parameter] = value;
    ++parameter;
  }
  return TRUE;
}

static int tag_owners_exist(const get_values_snapshot_t *snapshot,
    R_xlen_t *work_since_interrupt) {
  SEXP matches = PROTECT(Rf_match(
    snapshot->params_data.ids,
    snapshot->tags_data.ids,
    0
  ));
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(matches);
  if ((type != INTSXP && type != REALSXP) ||
      XLENGTH(matches) != snapshot->tags_data.row_count) {
    UNPROTECT(1);
    return FALSE;
  }
  for (R_xlen_t row = 0; row < snapshot->tags_data.row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    const int present = type == INTSXP
      ? INTEGER_ELT(matches, row) > 0
      : REAL_ELT(matches, row) > 0.0;
    if (!present) {
      UNPROTECT(1);
      return FALSE;
    }
  }
  UNPROTECT(1);
  return TRUE;
}

static SEXP snapshot_values(SEXP values,
    R_xlen_t *work_since_interrupt) {
  paradox_domain_values_t validated;
  if (!paradox_domain_validate_values(
      values,
      &validated,
      work_since_interrupt
    )) {
    Rf_error("Corrupt ParamSet capsule: invalid `.values` field");
  }
  SEXP result = PROTECT(Rf_allocVector(VECSXP, validated.size));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, validated.size));
  for (R_xlen_t index = 0; index < validated.size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP element = VECTOR_ELT(validated.values, index);
    if (element == R_UnboundValue || element == R_MissingArg ||
        TYPEOF(element) == PROMSXP) {
      UNPROTECT(2);
      Rf_error("Corrupt ParamSet capsule: invalid `.values` element");
    }
    SET_VECTOR_ELT(result, index, element);
    SET_STRING_ELT(names, index, STRING_ELT(validated.names, index));
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
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
    snapshot->values = snapshot_values(values, work_since_interrupt);
    SET_VECTOR_ELT(roots, GET_VALUES_ROOT_VALUES, snapshot->values);
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
    SEXP values = VECTOR_ELT(payload, PARADOX_CORE_VALUES);
    snapshot->values = snapshot_values(values, work_since_interrupt);
    SET_VECTOR_ELT(roots, GET_VALUES_ROOT_VALUES, snapshot->values);
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
  if (!paradox_domain_validate_tags(
      snapshot->tags,
      &snapshot->tags_data,
      work_since_interrupt
    ) || !tag_owners_exist(snapshot, work_since_interrupt)) {
    Rf_error("Corrupt ParamSet capsule: invalid `.tags` field");
  }
  if (!paradox_domain_validate_values(
      snapshot->values,
      &snapshot->values_data,
      work_since_interrupt
    ) || !map_values_to_parameters(snapshot, work_since_interrupt)) {
    Rf_error("Corrupt ParamSet capsule: invalid `.values` field");
  }
  if (!paradox_domain_validate_dependencies(
      snapshot->dependencies,
      &snapshot->dependencies_data,
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
  snapshot->dependency_rhs = paradox_temporary_alloc(
    dependency_count,
    sizeof(*snapshot->dependency_rhs)
  );
  for (R_xlen_t row = 0; row < dependency_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t dependent = find_string(
      snapshot->params_data.ids,
      STRING_ELT(snapshot->dependencies_data.ids, row),
      work_since_interrupt
    );
    if (dependent == R_XLEN_T_MAX) {
      Rf_error("Corrupt ParamSet capsule: dependency owner is unknown");
    }
    snapshot->dependency_id_parameter[row] = dependent;
    snapshot->dependency_on_parameter[row] = find_string(
      snapshot->params_data.ids,
      STRING_ELT(snapshot->dependencies_data.on, row),
      work_since_interrupt
    );
    paradox_builtin_condition_kind_t condition_kind;
    SEXP rhs = R_NilValue;
    if (!paradox_builtin_condition_exact(
        VECTOR_ELT(snapshot->dependencies_data.conditions, row),
        &condition_kind,
        &rhs,
        work_since_interrupt
      )) {
      Rf_error("Corrupt ParamSet capsule: invalid dependency condition");
    }
    snapshot->dependency_rhs[row] = rhs;
  }
}

static int condition_matches(SEXP value, SEXP rhs,
    R_xlen_t *work_since_interrupt) {
  if (!paradox_builtin_condition_scalar_supported(value, rhs)) {
    return FALSE;
  }
  if (value == R_NilValue || Rf_inherits(value, "TuneToken")) {
    return FALSE;
  }
  return paradox_builtin_condition_element_matches(
    value,
    0,
    rhs,
    work_since_interrupt
  );
}

static void apply_dependencies(const get_values_snapshot_t *snapshot,
    int *kept, R_xlen_t *work_since_interrupt) {
  const paradox_domain_dependencies_t *dependencies =
    &snapshot->dependencies_data;
  for (R_xlen_t row = 0; row < dependencies->row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t dependent_parameter =
      snapshot->dependency_id_parameter[row];
    const R_xlen_t dependent =
      snapshot->value_by_parameter[dependent_parameter];
    if (dependent == R_XLEN_T_MAX || !kept[dependent]) {
      continue;
    }
    const R_xlen_t parent_parameter =
      snapshot->dependency_on_parameter[row];
    const R_xlen_t parent = parent_parameter == R_XLEN_T_MAX
      ? R_XLEN_T_MAX
      : snapshot->value_by_parameter[parent_parameter];
    if (parent != R_XLEN_T_MAX && kept[parent] && Rf_inherits(
        VECTOR_ELT(snapshot->values_data.values, parent),
        "TuneToken"
      )) {
      continue;
    }
    if (parent == R_XLEN_T_MAX || !kept[parent] || !condition_matches(
        VECTOR_ELT(snapshot->values_data.values, parent),
        snapshot->dependency_rhs[row],
        work_since_interrupt
      )) {
      kept[dependent] = FALSE;
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
    paradox_domain_account_work(work_since_interrupt);
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
    R_xlen_t *work_since_interrupt) {
  SEXP required_tag = PROTECT(Rf_mkString("required"));
  SEXP required = PROTECT(paradox_param_set_ids(
    snapshot->params,
    snapshot->tags,
    R_NilValue,
    required_tag,
    R_NilValue
  ));
  const R_xlen_t size = XLENGTH(required);
  R_xlen_t missing_count = 0;
  size_t text_size = 1;
  size_t *sizes = paradox_temporary_alloc(size, sizeof(*sizes));
  const void *vmax = vmaxget();
  R_xlen_t parameter = 0;
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP id = STRING_ELT(required, index);
    while (parameter < snapshot->params_data.row_count && !strings_equal(
        STRING_ELT(snapshot->params_data.ids, parameter),
        id
      )) {
      ++parameter;
    }
    if (parameter == snapshot->params_data.row_count) {
      UNPROTECT(2);
      Rf_error("Internal error: required ID is absent from ParamSet schema");
    }
    if (snapshot->value_by_parameter[parameter] != R_XLEN_T_MAX) {
      ++parameter;
      continue;
    }
    const size_t id_size = strlen(Rf_translateCharUTF8(id));
    const size_t separator = missing_count == 0 ? 0 : 2;
    if (separator > SIZE_MAX - text_size ||
        id_size > SIZE_MAX - text_size - separator) {
      vmaxset(vmax);
      UNPROTECT(2);
      Rf_error("Unable to construct required-parameter diagnostic");
    }
    sizes[missing_count] = id_size;
    text_size += separator + id_size;
    ++missing_count;
    ++parameter;
  }
  vmaxset(vmax);
  if (missing_count == 0) {
    UNPROTECT(2);
    return;
  }
  if ((uintmax_t) text_size > (uintmax_t) R_XLEN_T_MAX) {
    UNPROTECT(2);
    Rf_error("Unable to construct required-parameter diagnostic");
  }

  char *text = paradox_temporary_alloc((R_xlen_t) text_size, sizeof(*text));
  size_t offset = 0;
  R_xlen_t emitted = 0;
  vmax = vmaxget();
  parameter = 0;
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP id = STRING_ELT(required, index);
    while (parameter < snapshot->params_data.row_count && !strings_equal(
        STRING_ELT(snapshot->params_data.ids, parameter),
        id
      )) {
      ++parameter;
    }
    if (parameter == snapshot->params_data.row_count) {
      vmaxset(vmax);
      UNPROTECT(2);
      Rf_error("Internal error: required ID is absent from ParamSet schema");
    }
    if (snapshot->value_by_parameter[parameter] != R_XLEN_T_MAX) {
      ++parameter;
      continue;
    }
    if (emitted != 0) {
      text[offset++] = ',';
      text[offset++] = ' ';
    }
    const char *id_text = Rf_translateCharUTF8(id);
    const size_t id_size = strlen(id_text);
    if (id_size != sizes[emitted] || id_size > text_size - 1 - offset) {
      vmaxset(vmax);
      UNPROTECT(2);
      Rf_error("Unable to construct required-parameter diagnostic");
    }
    memcpy(text + offset, id_text, id_size);
    offset += id_size;
    ++emitted;
    ++parameter;
  }
  text[offset] = '\0';
  /* `text` was allocated after this vmax watermark.  Error construction must
   * consume it before any vmaxset(); the non-local error exit releases the
   * temporary allocation itself. */
  UNPROTECT(2);
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
    paradox_domain_account_work(work_since_interrupt);
    if (!strings_equal(
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
    paradox_domain_account_work(work_since_interrupt);
    if (!strings_equal(
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
    paradox_domain_account_work(&work_since_interrupt);
    kept[index] = TRUE;
  }
  if (should_remove_dependencies) {
    apply_dependencies(&snapshot, kept, &work_since_interrupt);
  }
  apply_type_filter(&snapshot, type, kept, &work_since_interrupt);
  if (should_check_required) {
    check_required(&snapshot, &work_since_interrupt);
  }

  SEXP selected_ids = PROTECT(paradox_param_set_ids(
    snapshot.params,
    snapshot.tags,
    class_filter,
    all_tags,
    any_tags
  ));
  SEXP result = build_result(
    &snapshot,
    kept,
    selected_ids,
    &work_since_interrupt
  );
  UNPROTECT(5);
  return result;
}
