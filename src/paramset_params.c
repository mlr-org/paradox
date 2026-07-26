#include <limits.h>
#include <math.h>
#include <stddef.h>

#include "paradox.h"

#include "paramset_domain_common.h"
#include "paramset_params_internal.h"
#include "r_api_compat.h"
#include "r_utils.h"
#include "core_state.h"
#include "paramset_shadow.h"

typedef struct {
  SEXPTYPE type;
  SEXP values;
} params_match_vector_t;

/* One nested vector occupies a single slot in each caller's existing root
 * plan.  Keeping the exact parsed children here matters even when their
 * source tables are rooted: a finalizer can replace a table column or names
 * attribute while a later allocation is in flight. */
enum params_state_root_slot {
  PARAMS_ROOT_CORE = 0,
  PARAMS_ROOT_PARAMS_SOURCE,
  PARAMS_ROOT_TAGS_SOURCE,
  PARAMS_ROOT_TRAFOS_SOURCE,
  PARAMS_ROOT_DEPENDENCIES_SOURCE,
  PARAMS_ROOT_VALUES_SOURCE,
  PARAMS_ROOT_SOURCE_INDEX,
  PARAMS_ROOT_PARAM_COLUMNS,
  PARAMS_ROOT_TAG_IDS = PARAMS_ROOT_PARAM_COLUMNS + PARADOX_DOMAIN_TAGS,
  PARAMS_ROOT_TAG_VALUES,
  PARAMS_ROOT_TRAFO_IDS,
  PARAMS_ROOT_TRAFO_VALUES,
  PARAMS_ROOT_DEPENDENCY_IDS,
  PARAMS_ROOT_DEPENDENCY_ON,
  PARAMS_ROOT_DEPENDENCY_CONDITIONS,
  PARAMS_ROOT_VALUE_LIST,
  PARAMS_ROOT_VALUE_NAMES,
  PARAMS_ROOT_COUNT
};

static const char *const params_column_names[PARADOX_DOMAIN_COLUMN_COUNT] = {
  "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
  "levels", "special_vals", "default", "storage_type", ".tags",
  ".trafo", ".requirements", ".init_given", ".init"
};

int paradox_params_supported_table_attributes(SEXP table, int allow_sorted) {
  static const char *const supported[] = {"names", "class", "row.names"};
  (void) allow_sorted;
  return paradox_api_has_only_attributes(table, supported, 3);
}

static int exact_data_frame_row_names(SEXP table, R_xlen_t row_count,
    R_xlen_t *work_since_interrupt) {
  SEXP row_names = PROTECT(Rf_getAttrib(table, R_RowNamesSymbol));
  /* The public getter intentionally expands data-frame compact row names
   * (`c(NA, -n)`) to an ALTREP `1:n` vector. Row names do not participate in
   * this kernel: the validated ordinary columns provide the row count and
   * the result receives fresh compact row names. Never ask a callback-capable
   * row-name facade for Length/Elt merely to validate unused metadata. */
  if (row_count > INT_MAX || TYPEOF(row_names) != INTSXP ||
      !paradox_api_has_no_attributes(row_names)) {
    UNPROTECT(1);
    return FALSE;
  }
  if (ALTREP(row_names)) {
    UNPROTECT(1);
    return TRUE;
  }
  if (XLENGTH(row_names) != row_count) {
    UNPROTECT(1);
    return FALSE;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    if (INTEGER_ELT(row_names, row) != (int) row + 1) {
      UNPROTECT(1);
      return FALSE;
    }
  }
  UNPROTECT(1);
  return TRUE;
}

int paradox_params_names_are_only_attribute(SEXP value) {
  return paradox_api_has_single_attribute(value, "names");
}

static params_match_vector_t match_vector(SEXP value,
    R_xlen_t expected_size) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  if ((type != INTSXP && type != REALSXP) ||
      XLENGTH(value) != expected_size) {
    Rf_error("Internal error: unexpected result from R's matching primitive");
  }
  const params_match_vector_t result = {type, value};
  return result;
}

static R_xlen_t match_at(const params_match_vector_t *matches,
    R_xlen_t index) {
  if (matches->type == INTSXP) {
    const int value = INTEGER_ELT(matches->values, index);
    return value == NA_INTEGER || value <= 0 ? 0 : (R_xlen_t) value;
  }
  const double value = REAL_ELT(matches->values, index);
  return ISNAN(value) || value <= 0.0 ? 0 : (R_xlen_t) value;
}

static int group_rows_strict(const params_match_vector_t *owners,
    R_xlen_t input_size, R_xlen_t output_size, R_xlen_t *offsets,
    R_xlen_t *order, R_xlen_t *work_since_interrupt) {
  for (R_xlen_t row = 0; row <= output_size; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    offsets[row] = 0;
  }
  for (R_xlen_t input = 0; input < input_size; ++input) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t owner = match_at(owners, input);
    if (owner == 0 || owner > output_size ||
        offsets[owner] == R_XLEN_T_MAX) {
      return FALSE;
    }
    ++offsets[owner];
  }
  for (R_xlen_t row = 1; row <= output_size; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    if (offsets[row] > R_XLEN_T_MAX - offsets[row - 1]) {
      return FALSE;
    }
    offsets[row] += offsets[row - 1];
  }

  R_xlen_t *cursor = paradox_temporary_alloc(
    output_size,
    sizeof(*cursor)
  );
  for (R_xlen_t row = 0; row < output_size; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    cursor[row] = offsets[row];
  }
  for (R_xlen_t input = 0; input < input_size; ++input) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t owner = match_at(owners, input);
    order[cursor[owner - 1]] = input;
    ++cursor[owner - 1];
  }
  return TRUE;
}

static int index_unique_rows_strict(const params_match_vector_t *owners,
    R_xlen_t input_size, R_xlen_t output_size, R_xlen_t *index,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t row = 0; row < output_size; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    index[row] = 0;
  }
  for (R_xlen_t input = 0; input < input_size; ++input) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t owner = match_at(owners, input);
    if (owner == 0 || owner > output_size || index[owner - 1] != 0) {
      return FALSE;
    }
    index[owner - 1] = input + 1;
  }
  return TRUE;
}

static int index_last_rows_permissive(const params_match_vector_t *owners,
    R_xlen_t input_size, R_xlen_t output_size, R_xlen_t *index,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t row = 0; row < output_size; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    index[row] = 0;
  }
  for (R_xlen_t input = 0; input < input_size; ++input) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t owner = match_at(owners, input);
    /* data.table's update join ignores dependency rows whose `id` is absent
     * from x.  Dangling rows are legal, and collection callbacks can expose
     * them after native admission, so skipping them is both compatible and
     * avoids a post-callback decline. */
    if (owner == 0) {
      continue;
    }
    if (owner > output_size) {
      return FALSE;
    }
    /* data.table's update join visits duplicate i rows in input order, so the
     * final dependency for an id is the visible `.requirements` value. */
    index[owner - 1] = input + 1;
  }
  return TRUE;
}

static SEXP copy_vector(SEXP source,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t size = XLENGTH(source);
  SEXP result = PROTECT(Rf_allocVector((SEXPTYPE) TYPEOF(source), size));
  switch (TYPEOF(source)) {
  case STRSXP:
    for (R_xlen_t index = 0; index < size; ++index) {
      paradox_domain_account_work(work_since_interrupt);
      SET_STRING_ELT(result, index, STRING_ELT(source, index));
    }
    break;
  case VECSXP:
    for (R_xlen_t index = 0; index < size; ++index) {
      paradox_domain_account_work(work_since_interrupt);
      SET_VECTOR_ELT(result, index, VECTOR_ELT(source, index));
    }
    break;
  case REALSXP:
    for (R_xlen_t index = 0; index < size; ++index) {
      paradox_domain_account_work(work_since_interrupt);
      SET_REAL_ELT(result, index, REAL_ELT(source, index));
    }
    break;
  case INTSXP:
    for (R_xlen_t index = 0; index < size; ++index) {
      paradox_domain_account_work(work_since_interrupt);
      SET_INTEGER_ELT(result, index, INTEGER_ELT(source, index));
    }
    break;
  default:
    UNPROTECT(1);
    Rf_error("Internal error: unsupported ParamSet params column type");
  }
  UNPROTECT(1);
  return result;
}

static SEXP set_params_attributes(SEXP result, SEXP source_index,
    R_xlen_t row_count, R_xlen_t *work_since_interrupt) {
  PROTECT(source_index);
  SEXP names = PROTECT(Rf_allocVector(
    STRSXP,
    PARADOX_DOMAIN_COLUMN_COUNT
  ));
  for (R_xlen_t column = 0;
      column < PARADOX_DOMAIN_COLUMN_COUNT;
      ++column) {
    paradox_domain_account_work(work_since_interrupt);
    SET_STRING_ELT(names, column, Rf_mkChar(params_column_names[column]));
  }
  Rf_setAttrib(result, R_NamesSymbol, names);

  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(classes, 0, Rf_mkChar("data.table"));
  SET_STRING_ELT(classes, 1, Rf_mkChar("data.frame"));
  Rf_setAttrib(result, R_ClassSymbol, classes);

  SEXP row_names = PROTECT(Rf_allocVector(INTSXP, row_count == 0 ? 0 : 2));
  if (row_count != 0) {
    SET_INTEGER_ELT(row_names, 0, NA_INTEGER);
    SET_INTEGER_ELT(row_names, 1, -(int) row_count);
  }
  Rf_setAttrib(result, R_RowNamesSymbol, row_names);

  int protected_count = 4;
  if (source_index != R_NilValue) {
    SEXP index = PROTECT(Rf_duplicate(source_index));
    ++protected_count;
    Rf_setAttrib(result, Rf_install("index"), index);
  }

  SEXP prepared = PROTECT(paradox_prepare_data_table(result, TRUE));
  ++protected_count;
  /* `data.table` also ties the self-reference tag to this exact names vector.
   * Reattaching it last matches ordinary update-join output and keeps future
   * by-reference operations detached from the private table. */
  SEXP prepared_names = PROTECT(Rf_getAttrib(prepared, R_NamesSymbol));
  ++protected_count;
  Rf_setAttrib(prepared, R_NamesSymbol, R_NilValue);
  Rf_setAttrib(prepared, R_NamesSymbol, prepared_names);
  UNPROTECT(protected_count);
  return prepared;
}

static int validate_dynamic_state(
    const paradox_domain_params_t *params, SEXP dependencies_sexp,
    SEXP values_sexp, paradox_domain_dependencies_t *dependencies,
    paradox_domain_values_t *values, R_xlen_t *dependency_index,
    R_xlen_t *value_index, SEXP roots,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(roots) != VECSXP || XLENGTH(roots) < PARAMS_ROOT_COUNT) {
    Rf_error("Internal error: invalid ParamSet dynamic root plan");
  }
  if (!paradox_params_supported_table_attributes(dependencies_sexp, TRUE) ||
      !paradox_params_names_are_only_attribute(values_sexp) ||
      !paradox_domain_validate_dependencies(
        dependencies_sexp,
        dependencies,
        work_since_interrupt
      )) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, PARAMS_ROOT_DEPENDENCY_IDS, dependencies->ids);
  SET_VECTOR_ELT(roots, PARAMS_ROOT_DEPENDENCY_ON, dependencies->on);
  SET_VECTOR_ELT(
    roots,
    PARAMS_ROOT_DEPENDENCY_CONDITIONS,
    dependencies->conditions
  );
  if (!paradox_domain_validate_values(
        values_sexp,
        values,
        work_since_interrupt
      )) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, PARAMS_ROOT_VALUE_LIST, values->values);
  SET_VECTOR_ELT(roots, PARAMS_ROOT_VALUE_NAMES, values->names);

  SEXP dependency_match_sexp = PROTECT(Rf_match(
    params->ids,
    dependencies->ids,
    0
  ));
  SEXP value_match_sexp = PROTECT(Rf_match(params->ids, values->names, 0));
  const params_match_vector_t dependency_matches = match_vector(
    dependency_match_sexp,
    dependencies->row_count
  );
  const params_match_vector_t value_matches = match_vector(
    value_match_sexp,
    values->size
  );
  const int valid = index_last_rows_permissive(
      &dependency_matches,
      dependencies->row_count,
      params->row_count,
      dependency_index,
      work_since_interrupt
    ) && index_unique_rows_strict(
      &value_matches,
      values->size,
      params->row_count,
      value_index,
      work_since_interrupt
    );
  UNPROTECT(2);
  return valid;
}

static int load_core_state(SEXP core,
    paradox_params_state_t *state, SEXP roots, R_xlen_t roots_offset,
    R_xlen_t *work_since_interrupt) {
  const int retain = roots != R_NilValue;
  if (retain && (TYPEOF(roots) != VECSXP || roots_offset > XLENGTH(roots) ||
      XLENGTH(roots) - roots_offset < 1)) {
    Rf_error("Internal error: invalid ParamSet params root plan");
  }

  PROTECT(core);
  SEXP state_roots = PROTECT(Rf_allocVector(VECSXP, PARAMS_ROOT_COUNT));
  if (retain) {
    SET_VECTOR_ELT(roots, roots_offset, state_roots);
  }
  SET_VECTOR_ELT(state_roots, PARAMS_ROOT_CORE, core);

  int valid = FALSE;
  R_xlen_t unused_row = 0;
  if (!paradox_core_is_canonical(core) ||
      (paradox_core_kind(core) == PARADOX_CORE_SHADOW &&
        !paradox_shadow_metadata_is_exact(core))) {
    goto done;
  }
  SEXP payload = paradox_core_payload(core);
  state->params_sexp = VECTOR_ELT(payload, PARADOX_CORE_PARAMS);
  state->tags_sexp = VECTOR_ELT(payload, PARADOX_CORE_TAGS);
  state->trafos_sexp = VECTOR_ELT(payload, PARADOX_CORE_TRAFOS);
  state->dependencies_sexp = VECTOR_ELT(payload, PARADOX_CORE_DEPS);
  state->values_sexp = VECTOR_ELT(payload, PARADOX_CORE_VALUES);
  SET_VECTOR_ELT(
    state_roots,
    PARAMS_ROOT_PARAMS_SOURCE,
    state->params_sexp
  );
  SET_VECTOR_ELT(state_roots, PARAMS_ROOT_TAGS_SOURCE, state->tags_sexp);
  SET_VECTOR_ELT(
    state_roots,
    PARAMS_ROOT_TRAFOS_SOURCE,
    state->trafos_sexp
  );
  SET_VECTOR_ELT(
    state_roots,
    PARAMS_ROOT_DEPENDENCIES_SOURCE,
    state->dependencies_sexp
  );
  SET_VECTOR_ELT(
    state_roots,
    PARAMS_ROOT_VALUES_SOURCE,
    state->values_sexp
  );
  if (
      !paradox_params_supported_table_attributes(state->params_sexp, FALSE) ||
      !paradox_params_supported_table_attributes(state->tags_sexp, TRUE) ||
      !paradox_params_supported_table_attributes(state->trafos_sexp, TRUE)) {
    goto done;
  }

  state->source_index = Rf_getAttrib(
    state->params_sexp,
    Rf_install("index")
  );
  if (ALTREP(state->source_index)) {
    goto done;
  }
  SET_VECTOR_ELT(
    state_roots,
    PARAMS_ROOT_SOURCE_INDEX,
    state->source_index
  );

  if (!paradox_domain_validate_params(
        state->params_sexp,
        R_NilValue,
        TRUE,
        &state->params,
        &unused_row,
        work_since_interrupt
      )) {
    goto done;
  }
  for (enum paradox_domain_column column = PARADOX_DOMAIN_ID;
      column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    SEXP source = VECTOR_ELT(state->params_sexp, column);
    state->params_columns[column] = source;
    SET_VECTOR_ELT(
      state_roots,
      (int) PARAMS_ROOT_PARAM_COLUMNS + (int) column,
      source
    );
  }
  state->params.ids = state->params_columns[PARADOX_DOMAIN_ID];
  state->params.classes = state->params_columns[PARADOX_DOMAIN_CLS];
  if (!exact_data_frame_row_names(
      state->params_sexp,
      state->params.row_count,
      work_since_interrupt
    )) {
    goto done;
  }

  if (!paradox_domain_validate_tags(
        state->tags_sexp,
        &state->tags,
        work_since_interrupt
      )) {
    goto done;
  }
  SET_VECTOR_ELT(state_roots, PARAMS_ROOT_TAG_IDS, state->tags.ids);
  SET_VECTOR_ELT(state_roots, PARAMS_ROOT_TAG_VALUES, state->tags.values);

  if (!paradox_domain_validate_trafos(
        state->trafos_sexp,
        &state->trafos,
        work_since_interrupt
      )) {
    goto done;
  }
  SET_VECTOR_ELT(state_roots, PARAMS_ROOT_TRAFO_IDS, state->trafos.ids);
  SET_VECTOR_ELT(
    state_roots,
    PARAMS_ROOT_TRAFO_VALUES,
    state->trafos.values
  );

  if (state->params.row_count == R_XLEN_T_MAX ||
      state->params.row_count > INT_MAX) {
    goto done;
  }

  SEXP tag_match_sexp = PROTECT(Rf_match(
    state->params.ids,
    state->tags.ids,
    0
  ));
  SEXP trafo_match_sexp = PROTECT(Rf_match(
    state->params.ids,
    state->trafos.ids,
    0
  ));
  const params_match_vector_t tag_matches = match_vector(
    tag_match_sexp,
    state->tags.row_count
  );
  const params_match_vector_t trafo_matches = match_vector(
    trafo_match_sexp,
    state->trafos.row_count
  );
  state->tag_offsets = paradox_temporary_alloc(
    state->params.row_count + 1,
    sizeof(*state->tag_offsets)
  );
  state->tag_order = paradox_temporary_alloc(
    state->tags.row_count,
    sizeof(*state->tag_order)
  );
  state->trafo_index = paradox_temporary_alloc(
    state->params.row_count,
    sizeof(*state->trafo_index)
  );
  R_xlen_t *dependency_index = paradox_temporary_alloc(
    state->params.row_count,
    sizeof(*dependency_index)
  );
  R_xlen_t *value_index = paradox_temporary_alloc(
    state->params.row_count,
    sizeof(*value_index)
  );
  const int valid_static = group_rows_strict(
      &tag_matches,
      state->tags.row_count,
      state->params.row_count,
      state->tag_offsets,
      state->tag_order,
      work_since_interrupt
    ) && index_unique_rows_strict(
      &trafo_matches,
      state->trafos.row_count,
      state->params.row_count,
      state->trafo_index,
      work_since_interrupt
    );
  UNPROTECT(2);
  if (valid_static) {
    valid = validate_dynamic_state(
      &state->params,
      state->dependencies_sexp,
      state->values_sexp,
      &state->dependencies,
      &state->values,
      dependency_index,
      value_index,
      state_roots,
      work_since_interrupt
    );
  }

done:
  UNPROTECT(2);
  return valid;
}

int paradox_params_load_private_state(SEXP private_environment,
    paradox_params_state_t *state, R_xlen_t *work_since_interrupt) {
  SEXP core = PROTECT(paradox_core_from_private(private_environment));
  const int valid = load_core_state(
    core,
    state,
    R_NilValue,
    0,
    work_since_interrupt
  );
  UNPROTECT(1);
  return valid;
}

int paradox_params_load_private_state_rooted(SEXP private_environment,
    paradox_params_state_t *state, SEXP roots, R_xlen_t roots_offset,
    R_xlen_t *work_since_interrupt) {
  SEXP core = PROTECT(paradox_core_from_private(private_environment));
  const int valid = load_core_state(
    core,
    state,
    roots,
    roots_offset,
    work_since_interrupt
  );
  UNPROTECT(1);
  return valid;
}

int paradox_params_load_core_state_rooted(SEXP core,
    paradox_params_state_t *state, SEXP roots, R_xlen_t roots_offset,
    R_xlen_t *work_since_interrupt) {
  return load_core_state(
    core,
    state,
    roots,
    roots_offset,
    work_since_interrupt
  );
}

SEXP paradox_params_build_static(const paradox_params_state_t *state,
    R_xlen_t *work_since_interrupt) {
  const paradox_domain_params_t *params = &state->params;
  SEXP result = PROTECT(Rf_allocVector(VECSXP, PARADOX_DOMAIN_COLUMN_COUNT));
  for (enum paradox_domain_column column = PARADOX_DOMAIN_ID;
      column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    SEXP source = PROTECT(state->params_columns[column]);
    SEXP output = PROTECT(copy_vector(
      source,
      work_since_interrupt
    ));
    SET_VECTOR_ELT(result, column, output);
    UNPROTECT(2);
  }

  SEXP tags_column = PROTECT(Rf_allocVector(VECSXP, params->row_count));
  for (R_xlen_t row = 0; row < params->row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t first = state->tag_offsets[row];
    const R_xlen_t count = state->tag_offsets[row + 1] - first;
    SEXP selected = PROTECT(Rf_allocVector(STRSXP, count));
    for (R_xlen_t index = 0; index < count; ++index) {
      paradox_domain_account_work(work_since_interrupt);
      SET_STRING_ELT(
        selected,
        index,
        STRING_ELT(state->tags.values, state->tag_order[first + index])
      );
    }
    SET_VECTOR_ELT(tags_column, row, selected);
    UNPROTECT(1);
  }
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_TAGS, tags_column);
  UNPROTECT(1);

  SEXP trafo_column = PROTECT(Rf_allocVector(VECSXP, params->row_count));
  for (R_xlen_t row = 0; row < params->row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t selected = state->trafo_index[row];
    if (selected != 0) {
      SET_VECTOR_ELT(
        trafo_column,
        row,
        VECTOR_ELT(state->trafos.values, selected - 1)
      );
    }
  }
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_TRAFO, trafo_column);
  UNPROTECT(1);

  SEXP prepared = PROTECT(set_params_attributes(
    result,
    state->source_index,
    params->row_count,
    work_since_interrupt
  ));
  UNPROTECT(2);
  return prepared;
}

int paradox_params_finish_dynamic(SEXP result,
    const paradox_domain_params_t *params, SEXP dependencies_sexp,
    SEXP values_sexp, R_xlen_t *work_since_interrupt) {
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, PARAMS_ROOT_COUNT));
  SET_VECTOR_ELT(roots, PARAMS_ROOT_PARAMS_SOURCE, params->table);
  SET_VECTOR_ELT(
    roots,
    (int) PARAMS_ROOT_PARAM_COLUMNS + (int) PARADOX_DOMAIN_ID,
    params->ids
  );
  SET_VECTOR_ELT(
    roots,
    PARAMS_ROOT_DEPENDENCIES_SOURCE,
    dependencies_sexp
  );
  SET_VECTOR_ELT(roots, PARAMS_ROOT_VALUES_SOURCE, values_sexp);

  paradox_domain_dependencies_t dependencies;
  paradox_domain_values_t values;
  R_xlen_t *dependency_index = paradox_temporary_alloc(
    params->row_count,
    sizeof(*dependency_index)
  );
  R_xlen_t *value_index = paradox_temporary_alloc(
    params->row_count,
    sizeof(*value_index)
  );
  if (!validate_dynamic_state(
      params,
      dependencies_sexp,
      values_sexp,
      &dependencies,
      &values,
      dependency_index,
      value_index,
      roots,
      work_since_interrupt
    )) {
    UNPROTECT(1);
    return FALSE;
  }

  SEXP requirements_column = PROTECT(Rf_allocVector(VECSXP, params->row_count));
  for (R_xlen_t row = 0; row < params->row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t selected = dependency_index[row];
    if (selected == 0) {
      continue;
    }
    const R_xlen_t dependency_row = selected - 1;
    SEXP requirement = PROTECT(Rf_allocVector(VECSXP, 2));
    SEXP on = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(on, 0, STRING_ELT(dependencies.on, dependency_row));
    SET_VECTOR_ELT(requirement, 0, on);
    SET_VECTOR_ELT(
      requirement,
      1,
      VECTOR_ELT(dependencies.conditions, dependency_row)
    );
    SET_VECTOR_ELT(requirements_column, row, requirement);
    UNPROTECT(2);
  }
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_REQUIREMENTS, requirements_column);
  UNPROTECT(1);

  SEXP init_given_column = PROTECT(Rf_allocVector(
    LGLSXP,
    params->row_count
  ));
  SEXP init_column = PROTECT(Rf_allocVector(VECSXP, params->row_count));
  for (R_xlen_t row = 0; row < params->row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t selected = value_index[row];
    SET_LOGICAL_ELT(init_given_column, row, selected != 0);
    if (selected != 0) {
      SET_VECTOR_ELT(
        init_column,
        row,
        VECTOR_ELT(values.values, selected - 1)
      );
    }
  }
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_INIT_GIVEN, init_given_column);
  SET_VECTOR_ELT(result, PARADOX_DOMAIN_INIT, init_column);
  UNPROTECT(3);
  return TRUE;
}

SEXP paradox_param_set_params(SEXP private_environment, SEXP self) {
  R_xlen_t work_since_interrupt = 0;
  if (TYPEOF(private_environment) != ENVSXP || TYPEOF(self) != ENVSXP) {
    Rf_error(
      "Corrupt ParamSet parameter shell: private and self must be environments"
    );
  }
  SEXP core = paradox_core_from_private(private_environment);
  if (core == R_UnboundValue) {
    Rf_error("Corrupt ParamSet parameter state: missing core capsule");
  }
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("Corrupt ParamSet parameter shell ownership");
  }
  if (paradox_core_kind(core) == PARADOX_CORE_SHADOW) {
    core = paradox_core_refresh_shadow(self, private_environment);
  }
  const paradox_core_kind_t kind = paradox_core_kind(core);
  if (kind != PARADOX_CORE_BASE && kind != PARADOX_CORE_SHADOW) {
    Rf_error("ParamSet parameter reader requires a BASE or SHADOW core");
  }

  SEXP roots = PROTECT(Rf_allocVector(VECSXP, 5));
  paradox_params_state_t state;
  if (!paradox_params_load_private_state_rooted(
      private_environment,
      &state,
      roots,
      0,
      &work_since_interrupt
    )) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet parameter state capsule");
  }
  SEXP result = PROTECT(paradox_params_build_static(
    &state,
    &work_since_interrupt
  ));
  if (!paradox_params_finish_dynamic(
      result,
      &state.params,
      state.dependencies_sexp,
      state.values_sexp,
      &work_since_interrupt
    )) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSet dynamic parameter state capsule");
  }
  UNPROTECT(2);
  return result;
}
