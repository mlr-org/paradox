#include <limits.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "builtin_condition.h"
#include "core_state.h"
#include "parameter_suggestion.h"
#include "paramset_domain_common.h"
#include "paramset_shadow.h"
#include "r_api_compat.h"
#include "r_utils.h"

static const char *const dependency_names[] = {"id", "on", "cond"};
static const char *const tag_names[] = {"id", "tag"};

static char *utf8_error_copy(SEXP string) {
  return paradox_temporary_utf8_copy(string, NULL);
}

static int exact_flag(SEXP value, const char *name) {
  if (TYPEOF(value) != LGLSXP || ALTREP(value) || Rf_isS4(value) ||
      Rf_isObject(value) || XLENGTH(value) != 1 ||
      !paradox_api_has_no_attributes(value)) {
    Rf_error("`%s` must be an unclassed logical flag", name);
  }
  const int flag = LOGICAL_ELT(value, 0);
  if (flag == NA_LOGICAL) {
    Rf_error("`%s` may not be NA", name);
  }
  return flag;
}

static SEXP scalar_string(SEXP value, const char *name) {
  if (TYPEOF(value) != STRSXP || ALTREP(value) || Rf_isS4(value) ||
      Rf_isObject(value) || !paradox_api_has_no_attributes(value) ||
      XLENGTH(value) != 1) {
    Rf_error("`%s` must be a character scalar", name);
  }
  SEXP result = STRING_ELT(value, 0);
  if (!paradox_charsxp_is_ordinary(result)) {
    Rf_error("`%s` must be a non-missing, non-bytes string", name);
  }
  return result;
}

/*
 * Returns with one protection owned by the caller.  Selection and a possible
 * refresh must be rooted before further planning can allocate and run a
 * finalizer that replaces `.core`. Parameter-column admission cannot allocate.
 */
static SEXP protected_checked_core(SEXP private_environment, SEXP self,
    paradox_core_kind_t *kind, paradox_domain_params_t *params,
    int allow_derived,
    PROTECT_INDEX *core_index) {
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("ParamSet method called with a foreign private environment");
  }
  SEXP core;
  PROTECT_WITH_INDEX(
    core = paradox_core_from_private(private_environment),
    core_index
  );
  if (core == R_UnboundValue) {
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }
  if (!paradox_core_is_verified(core)) {
    REPROTECT(
      core = paradox_core_refresh(self, private_environment),
      *core_index
    );
  }
  *kind = paradox_core_kind(core);
  if (*kind == PARADOX_CORE_SHADOW && !allow_derived) {
    Rf_error("ParamSetShadow state is read-only at this mutation boundary");
  }
  SEXP state = R_ExternalPtrProtected(core);
  if (!paradox_domain_read_params(
      VECTOR_ELT(state, PARADOX_CORE_PARAMS),
      0U, params)) {
    Rf_error("Corrupt ParamSet parameter capsule");
  }
  return core;
}

static SEXP replace_one(SEXP private_environment, SEXP expected_core,
    const char *field, SEXP value) {
  SEXP updates = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(updates, 0, value);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(names, 0, Rf_mkCharCE(field, CE_UTF8));
  Rf_setAttrib(updates, R_NamesSymbol, names);
  /*
   * Build the complete update before the generation barrier. Either
   * allocation above may run a finalizer that installs a newer capsule. The
   * core replacement selects its input generation immediately after this
   * allocation-free receipt, so it cannot validate against `expected_core`
   * and then apply the update to an unvalidated successor.
   */
  if (paradox_core_from_private(private_environment) != expected_core) {
    UNPROTECT(2);
    Rf_error("ParamSet changed while a native mutation was being validated");
  }
  SEXP result = PROTECT(paradox_param_set_core_replace(
    private_environment,
    updates
  ));
  UNPROTECT(3);
  return result;
}

static SEXP snapshot_condition(SEXP condition,
    R_xlen_t *work_since_interrupt) {
  PROTECT(condition);
  paradox_builtin_condition_kind_t kind;
  SEXP stable_rhs = paradox_builtin_condition_admit(
    condition,
    &kind,
    work_since_interrupt
  );
  if (stable_rhs == R_NilValue) {
    UNPROTECT(1);
    Rf_error("Malformed built-in dependency Condition");
  }
  PROTECT(stable_rhs);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, stable_rhs);
  SET_VECTOR_ELT(
    result,
    1,
    Rf_mkString(
      kind == PARADOX_BUILTIN_CONDITION_EQUAL
        ? "%s == %s"
        : "%s %%in%% {%s}"
    )
  );
  static const char *const names[] = {"rhs", "condition_format_string"};
  SEXP result_names = PROTECT(paradox_domain_character_vector(names, 2));
  const char *const *classes = kind == PARADOX_BUILTIN_CONDITION_EQUAL
    ? (const char *const[]) {"CondEqual", "Condition"}
    : (const char *const[]) {"CondAnyOf", "Condition"};
  SEXP result_class = PROTECT(paradox_domain_character_vector(classes, 2));
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  Rf_setAttrib(result, R_ClassSymbol, result_class);
  UNPROTECT(5);
  return result;
}

static void require_feasible_rhs(SEXP private_environment, SEXP self,
    SEXP expected_core, SEXP parent_id, SEXP condition,
    R_xlen_t *work_since_interrupt) {
  paradox_builtin_condition_kind_t unused_kind;
  SEXP rhs = R_NilValue;
  if (!paradox_builtin_condition_exact(
      condition,
      &unused_kind,
      &rhs,
      work_since_interrupt
    )) {
    Rf_error("Internal error: invalid dependency Condition snapshot");
  }
  (void) unused_kind;

  static const SEXPTYPE types[] = {VECSXP};
  SEXP table = PROTECT(paradox_domain_new_plain_table(
    (const char *const[]) {"value"},
    types,
    1,
    XLENGTH(rhs)
  ));
  SET_VECTOR_ELT(table, 0, rhs);
  SEXP table_names = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(table_names, 0, parent_id);
  Rf_setAttrib(table, R_NamesSymbol, table_names);

  SEXP strict = PROTECT(Rf_ScalarLogical(FALSE));
  SEXP presence = PROTECT(Rf_mkString("none"));
  SEXP tokens = PROTECT(Rf_ScalarLogical(FALSE));
  SEXP result = PROTECT(paradox_param_set_check_dt_builtin(
    private_environment,
    self,
    table,
    strict,
    presence,
    tokens
  ));
  if (paradox_core_from_private(private_environment) != expected_core) {
    UNPROTECT(6);
    Rf_error("ParamSet changed while dependency values were being validated");
  }
  const int valid = TYPEOF(result) == LGLSXP && XLENGTH(result) == 1 &&
    LOGICAL_ELT(result, 0) == TRUE;
  if (!valid) {
    char *parent_text = utf8_error_copy(parent_id);
    Rf_error(
      "Condition has infeasible values for %s",
      parent_text
    );
  }
  UNPROTECT(6);
}

static SEXP snapshot_dependencies(SEXP input,
    const paradox_domain_params_t *params,
    R_xlen_t *work_since_interrupt) {
  static const SEXPTYPE types[] = {STRSXP, STRSXP, VECSXP};
  if (TYPEOF(input) != VECSXP || ALTREP(input) || Rf_isS4(input)) {
    Rf_error("`deps` must be a three-column table or an empty table");
  }
  const R_xlen_t column_count = XLENGTH(input);
  if (column_count == 0) {
    return paradox_domain_new_plain_table(dependency_names, types, 3, 0);
  }

  /*
   * Own every name beside its exact matching column before observing an
   * atomic column's ALTREP Length method.  The two carriers replace the old
   * live-table re-reads below; a callback may reorder the caller's table, but
   * cannot create a hybrid dependency generation.
   */
  SEXP stable_names = PROTECT(Rf_allocVector(STRSXP, column_count));
  SEXP stable_columns = PROTECT(Rf_allocVector(VECSXP, column_count));
  if (!paradox_capture_list_identities(
      input,
      stable_names,
      stable_columns
    )) {
    UNPROTECT(2);
    Rf_error("`deps` columns must have ordinary names");
  }
  int all_empty = TRUE;
  for (R_xlen_t column = 0; column < column_count; ++column) {
    if (XLENGTH(VECTOR_ELT(stable_columns, column)) != 0) {
      all_empty = FALSE;
    }
  }
  if (all_empty) {
    SEXP result = paradox_domain_new_plain_table(
      dependency_names,
      types,
      3,
      0
    );
    UNPROTECT(2);
    return result;
  }
  if (column_count != 3) {
    UNPROTECT(2);
    Rf_error("`deps` must be a three-column table or an empty table");
  }
  for (R_xlen_t column = 0; column < 3; ++column) {
    if (!paradox_domain_strings_equal(
        STRING_ELT(stable_names, column),
        Rf_mkCharCE(dependency_names[column], CE_UTF8)
      )) {
      UNPROTECT(2);
      Rf_error("`deps` columns must be named `id`, `on`, and `cond`");
    }
  }
  SEXP ids = VECTOR_ELT(stable_columns, 0);
  SEXP on = VECTOR_ELT(stable_columns, 1);
  SEXP conditions = VECTOR_ELT(stable_columns, 2);
  if (TYPEOF(ids) != STRSXP || TYPEOF(on) != STRSXP ||
      TYPEOF(conditions) != VECSXP || ALTREP(conditions) ||
      Rf_isS4(ids) || Rf_isS4(on) || Rf_isS4(conditions)) {
    UNPROTECT(2);
    Rf_error("`deps` columns have unsupported types");
  }
  /* Measure the columns this loop will actually index, not the lengths seen
   * before the name and type checks: everything in between can allocate or
   * dispatch an ALTREP Length method, and a callback may replace a column of
   * the caller's table. Indexing a retained column with a stale bound was an
   * out-of-range read on every supported R. */
  const R_xlen_t rows = XLENGTH(ids);
  if (XLENGTH(on) != rows || XLENGTH(conditions) != rows) {
    UNPROTECT(2);
    Rf_error("`deps` columns must have equal lengths");
  }
  SEXP result = PROTECT(paradox_domain_new_plain_table(
    dependency_names,
    types,
    3,
    rows
  ));
  SEXP result_ids = VECTOR_ELT(result, 0);
  SEXP result_on = VECTOR_ELT(result, 1);
  SEXP result_conditions = VECTOR_ELT(result, 2);
  /*
   * The condition column is ordinary interpreted structure. Retain every row
   * identity before admitting the first RHS: its stable ALTREP materializer
   * may reenter R and reorder the caller-owned list column. The final output
   * columns double as the carriers, so this closes the row-generation window
   * without another allocation.
   */
  for (R_xlen_t row = 0; row < rows; ++row) {
    SET_VECTOR_ELT(
      result_conditions,
      row,
      VECTOR_ELT(conditions, row)
    );
  }
  for (R_xlen_t row = 0; row < rows; ++row) {
    paradox_account_work(work_since_interrupt);
    SET_STRING_ELT(result_ids, row, STRING_ELT(ids, row));
    SET_STRING_ELT(result_on, row, STRING_ELT(on, row));
  }
  for (R_xlen_t row = 0; row < rows; ++row) {
    paradox_account_work(work_since_interrupt);
    SEXP id = STRING_ELT(result_ids, row);
    SEXP parent = STRING_ELT(result_on, row);
    if (!paradox_charsxp_is_ordinary(id) || !paradox_charsxp_is_ordinary(parent)) {
      UNPROTECT(3);
      Rf_error("`deps$id` and `deps$on` may not contain missing/bytes strings");
    }
    if (params != NULL &&
        paradox_domain_find_string(params->ids, id, work_since_interrupt) == R_XLEN_T_MAX) {
      UNPROTECT(3);
      Rf_error("Dependency child is not a parameter in this ParamSet");
    }
    if (params != NULL && paradox_domain_strings_equal(id, parent)) {
      UNPROTECT(3);
      Rf_error("A param cannot depend on itself!");
    }
    SEXP condition = PROTECT(snapshot_condition(
      VECTOR_ELT(result_conditions, row),
      work_since_interrupt
    ));
    SET_VECTOR_ELT(result_conditions, row, condition);
    UNPROTECT(1);
  }
  UNPROTECT(3);
  return result;
}

SEXP paradox_param_set_dependency_table_snapshot(SEXP dependencies) {
  R_xlen_t work_since_interrupt = 0;
  return snapshot_dependencies(
    dependencies,
    NULL,
    &work_since_interrupt
  );
}

SEXP paradox_dependency_public_facade(SEXP table) {
  PROTECT(table);
  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(classes, 0, Rf_mkChar("data.table"));
  SET_STRING_ELT(classes, 1, Rf_mkChar("data.frame"));
  Rf_setAttrib(table, R_ClassSymbol, classes);
  SEXP result = PROTECT(paradox_prepare_fresh_data_table(table));
  UNPROTECT(3);
  return result;
}

SEXP paradox_param_set_dependencies(SEXP private_environment, SEXP self) {
  R_xlen_t work_since_interrupt = 0;
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("ParamSet method called with a foreign private environment");
  }
  PROTECT_INDEX core_index;
  SEXP core;
  PROTECT_WITH_INDEX(
    core = paradox_core_from_private(private_environment),
    &core_index
  );
  if (core == R_UnboundValue) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }
  if (!paradox_core_is_verified(core)) {
    REPROTECT(
      core = paradox_core_refresh(self, private_environment),
      core_index
    );
  }
  const paradox_core_kind_t kind = paradox_core_kind(core);
  if (kind != PARADOX_CORE_BASE && kind != PARADOX_CORE_SHADOW) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet dependency capsule kind");
  }
  SEXP state = R_ExternalPtrProtected(core);
  SEXP plain = PROTECT(snapshot_dependencies(
    VECTOR_ELT(state, PARADOX_CORE_DEPS),
    NULL,
    &work_since_interrupt
  ));
  SEXP result = PROTECT(paradox_dependency_public_facade(plain));
  UNPROTECT(3);
  return result;
}

SEXP paradox_param_set_get_tags(SEXP private_environment, SEXP self) {
  R_xlen_t work_since_interrupt = 0;
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("ParamSet method called with a foreign private environment");
  }
  PROTECT_INDEX core_index;
  SEXP core;
  PROTECT_WITH_INDEX(
    core = paradox_core_from_private(private_environment),
    &core_index
  );
  if (core == R_UnboundValue) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }
  if (!paradox_core_is_verified(core)) {
    REPROTECT(
      core = paradox_core_refresh(self, private_environment),
      core_index
    );
  }
  SEXP state = R_ExternalPtrProtected(core);
  paradox_domain_params_t params;
  paradox_domain_tags_t tags;
  if (!paradox_domain_read_params(
      VECTOR_ELT(state, PARADOX_CORE_PARAMS),
      0U, &params) || !paradox_domain_validate_tags(
      VECTOR_ELT(state, PARADOX_CORE_TAGS),
      &tags,
      &work_since_interrupt
    )) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet tag capsule");
  }
  R_xlen_t *counts = paradox_temporary_alloc(
    params.row_count,
    sizeof(*counts)
  );
  R_xlen_t *offsets = paradox_temporary_alloc(
    params.row_count,
    sizeof(*offsets)
  );
  for (R_xlen_t parameter = 0; parameter < params.row_count; ++parameter) {
    counts[parameter] = 0;
    offsets[parameter] = 0;
  }
  R_xlen_t *owners = paradox_temporary_alloc(
    tags.row_count == 0 ? 1 : tags.row_count,
    sizeof(*owners)
  );
  for (R_xlen_t row = 0; row < tags.row_count; ++row) {
    const R_xlen_t parameter = paradox_domain_find_string(
      params.ids,
      STRING_ELT(tags.ids, row),
      &work_since_interrupt
    );
    if (parameter == R_XLEN_T_MAX || counts[parameter] == R_XLEN_T_MAX) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSet tag ID");
    }
    owners[row] = parameter;
    ++counts[parameter];
  }
  SEXP result = PROTECT(Rf_allocVector(VECSXP, params.row_count));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, params.row_count));
  for (R_xlen_t parameter = 0; parameter < params.row_count; ++parameter) {
    SEXP values = PROTECT(Rf_allocVector(STRSXP, counts[parameter]));
    SET_VECTOR_ELT(result, parameter, values);
    SET_STRING_ELT(names, parameter, STRING_ELT(params.ids, parameter));
    UNPROTECT(1);
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  for (R_xlen_t row = 0; row < tags.row_count; ++row) {
    const R_xlen_t parameter = owners[row];
    SET_STRING_ELT(
      VECTOR_ELT(result, parameter),
      offsets[parameter],
      STRING_ELT(tags.values, row)
    );
    ++offsets[parameter];
  }
  UNPROTECT(3);
  return result;
}

static void tags_name_repeated(SEXP id) {
  PROTECT(id);
  /* An id whose bytes are not valid UTF-8 in this locale is escaped rather
   * than failing the message builder with an internal error. */
  SEXP safe_id = PROTECT(paradox_diagnostic_charsxp(id));
  const paradox_utf8_piece_t pieces[] = {
    paradox_utf8_ascii_piece(
      "`tags` must name every parameter exactly once, but '"
    ),
    paradox_utf8_charsxp_piece(safe_id),
    paradox_utf8_ascii_piece("' appears more than once")
  };
  SEXP message = PROTECT(paradox_utf8_message(pieces, 3));
  paradox_error_from_scalar_string(message);
}

static SEXP param_set_set_tags_impl(SEXP private_environment, SEXP self,
    SEXP tags);

SEXP paradox_param_set_set_tags(SEXP private_environment, SEXP self,
    SEXP tags) {
  /* `setNames(tags, ids)` on a referenced list of 64 or more entries is a
   * base wrapper ALTREP; own one ordinary copy before the list gate below. */
  tags = PROTECT(paradox_materialize_public_list_shell(tags));
  SEXP result = PROTECT(param_set_set_tags_impl(
    private_environment,
    self,
    tags
  ));
  UNPROTECT(2);
  return result;
}

static SEXP param_set_set_tags_impl(SEXP private_environment, SEXP self,
    SEXP tags) {
  R_xlen_t work_since_interrupt = 0;
  paradox_core_kind_t kind;
  paradox_domain_params_t params;
  PROTECT_INDEX core_index;
  SEXP core = protected_checked_core(
    private_environment,
    self,
    &kind,
    &params,
    TRUE,
    &core_index
  );
  /* The same ordinary-container admission as every sibling structural
   * boundary: an S4-classed list is not a plain list. */
  if (TYPEOF(tags) != VECSXP || ALTREP(tags) || Rf_isS4(tags)) {
    UNPROTECT(1);
    Rf_error("`tags` must be a list");
  }
  const R_xlen_t parameter_count = params.row_count;
  const R_xlen_t input_count = XLENGTH(tags);
  if (input_count != 0) {
    if (input_count != parameter_count) {
      UNPROTECT(1);
      Rf_error("Non-empty `tags` must name every parameter exactly once");
    }
  }
  SEXP stable_names = PROTECT(Rf_allocVector(STRSXP, input_count));
  SEXP stable_tags = PROTECT(Rf_allocVector(VECSXP, input_count));
  R_xlen_t row_count = 0;
  if (input_count != 0) {
    if (!paradox_capture_list_identities(
        tags,
        stable_names,
        stable_tags
      )) {
      UNPROTECT(3);
      Rf_error("Non-empty `tags` must be completely named");
    }
    unsigned char *seen = paradox_temporary_alloc(
      parameter_count,
      sizeof(*seen)
    );
    for (R_xlen_t index = 0; index < parameter_count; ++index) {
      seen[index] = 0U;
    }
    for (R_xlen_t parameter = 0; parameter < input_count; ++parameter) {
      SEXP id = STRING_ELT(stable_names, parameter);
      /* The count check above makes this a permutation requirement, so a
       * rejected name is either no parameter ID or a repeated one.  Paradox
       * 1's assertion enumerated the offenders; name them here as well
      * instead of restating the contract anonymously. */
      if (!paradox_charsxp_is_ordinary(id)) {
        UNPROTECT(3);
        Rf_error("`tags` names may not be missing or bytes-encoded");
      }
      const R_xlen_t row = paradox_domain_find_string(
        params.ids,
        id,
        &work_since_interrupt
      );
      if (row == R_XLEN_T_MAX) {
        SEXP message = PROTECT(paradox_parameter_unavailable_diagnostic(
          id,
          params.ids,
          ""
        ));
        paradox_error_from_scalar_string(message);
      }
      if (seen[row]) {
        tags_name_repeated(id);
      }
      seen[row] = 1;
      SEXP values = VECTOR_ELT(stable_tags, parameter);
      if (TYPEOF(values) != STRSXP) {
        UNPROTECT(3);
        Rf_error("Every `tags` element must be a character vector");
      }
      const R_xlen_t size = XLENGTH(values);
      if (row_count > R_XLEN_T_MAX - size) {
        UNPROTECT(3);
        Rf_error("`tags` contains too many values");
      }
      SEXP stable_values = PROTECT(Rf_allocVector(STRSXP, size));
      for (R_xlen_t index = 0; index < size; ++index) {
        paradox_account_work(&work_since_interrupt);
        SEXP value = STRING_ELT(values, index);
        if (!paradox_charsxp_is_ordinary(value)) {
          UNPROTECT(4);
          Rf_error("Tag values may not be missing or bytes-encoded");
        }
        SET_STRING_ELT(stable_values, index, value);
      }
      SET_VECTOR_ELT(stable_tags, parameter, stable_values);
      UNPROTECT(1);
      row_count += size;
    }
  }

  static const SEXPTYPE types[] = {STRSXP, STRSXP};
  SEXP table = PROTECT(paradox_domain_new_plain_table(tag_names, types, 2, row_count));
  R_xlen_t output = 0;
  for (R_xlen_t parameter = 0; parameter < input_count; ++parameter) {
    SEXP id = STRING_ELT(stable_names, parameter);
    SEXP values = VECTOR_ELT(stable_tags, parameter);
    const R_xlen_t size = XLENGTH(values);
    for (R_xlen_t index = 0; index < size; ++index) {
      paradox_account_work(&work_since_interrupt);
      SEXP value = STRING_ELT(values, index);
      SET_STRING_ELT(VECTOR_ELT(table, 0), output, id);
      SET_STRING_ELT(VECTOR_ELT(table, 1), output, value);
      ++output;
    }
  }
  if (kind == PARADOX_CORE_BASE) {
    (void) replace_one(private_environment, core, ".tags", table);
    UNPROTECT(4);
    return tags;
  }

  /* A derived node owns the answer it was given for the IDs it was given it
   * for. Recording that in the edge record is what makes the assignment
   * survive re-derivation without writing through to the sets the schema
   * comes from: an ID added later is still derived. */
  SEXP governed = PROTECT(Rf_allocVector(STRSXP, parameter_count));
  for (R_xlen_t index = 0; index < parameter_count; ++index) {
    SET_STRING_ELT(governed, index, STRING_ELT(params.ids, index));
  }
  SEXP override = PROTECT(Rf_allocVector(
    VECSXP,
    PARADOX_TAG_OVERRIDE_FIELD_COUNT
  ));
  SET_VECTOR_ELT(override, PARADOX_TAG_OVERRIDE_IDS, governed);
  SET_VECTOR_ELT(override, PARADOX_TAG_OVERRIDE_TAGS, table);
  SEXP override_names = PROTECT(paradox_domain_character_vector(
    (const char *const[]) {"ids", "tags"},
    PARADOX_TAG_OVERRIDE_FIELD_COUNT
  ));
  Rf_setAttrib(override, R_NamesSymbol, override_names);

  SEXP state = R_ExternalPtrProtected(core);
  SEXP old_edges = VECTOR_ELT(state, PARADOX_CORE_EDGES);
  const R_xlen_t edge_count = XLENGTH(old_edges);
  SEXP edges = PROTECT(Rf_allocVector(VECSXP, edge_count));
  for (R_xlen_t index = 0; index < edge_count; ++index) {
    SET_VECTOR_ELT(edges, index, VECTOR_ELT(old_edges, index));
  }
  SET_VECTOR_ELT(
    edges,
    kind == PARADOX_CORE_COLLECTION
      ? PARADOX_COLLECTION_EDGE_TAG_OVERRIDE
      : PARADOX_SHADOW_EDGE_TAG_OVERRIDE,
    override
  );
  Rf_setAttrib(
    edges,
    R_NamesSymbol,
    Rf_getAttrib(old_edges, R_NamesSymbol)
  );

  SEXP updates = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(updates, 0, table);
  SET_VECTOR_ELT(updates, 1, edges);
  SEXP update_names = PROTECT(paradox_domain_character_vector(
    (const char *const[]) {".tags", ".edges"},
    2
  ));
  Rf_setAttrib(updates, R_NamesSymbol, update_names);
  if (paradox_core_from_private(private_environment) != core) {
    UNPROTECT(10);
    Rf_error("ParamSet changed while a native mutation was being validated");
  }
  (void) paradox_param_set_core_replace(private_environment, updates);
  UNPROTECT(10);
  return tags;
}

SEXP paradox_param_set_set_dependencies(SEXP private_environment, SEXP self,
    SEXP dependencies) {
  R_xlen_t work_since_interrupt = 0;
  paradox_core_kind_t kind;
  paradox_domain_params_t params;
  PROTECT_INDEX core_index;
  SEXP core = protected_checked_core(
    private_environment,
    self,
    &kind,
    &params,
    FALSE,
    &core_index
  );
  if (kind != PARADOX_CORE_BASE) {
    UNPROTECT(1);
    Rf_error("deps is read-only outside a base ParamSet");
  }
  SEXP stable = PROTECT(snapshot_dependencies(
    dependencies,
    &params,
    &work_since_interrupt
  ));
  (void) replace_one(private_environment, core, ".deps", stable);
  UNPROTECT(2);
  return dependencies;
}

/* Select and validate the origin generation that a forwarded Shadow mutation
 * will target. The caller immediately protects the returned core and keeps it
 * through the final binding receipt, so visibility decisions and the
 * delegated write cannot straddle two origin generations. */
static SEXP shadow_origin_core(SEXP origin, SEXP origin_private,
    paradox_domain_params_t *params) {
  PROTECT_INDEX core_index;
  SEXP core;
  PROTECT_WITH_INDEX(
    core = paradox_core_from_private(origin_private),
    &core_index
  );
  if (core == R_UnboundValue) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSetShadow origin state");
  }
  if (!paradox_core_is_verified(core)) {
    REPROTECT(core = paradox_core_refresh(origin, origin_private), core_index);
  }
  if (!paradox_core_has_exact_schema(core) ||
      !paradox_domain_read_params(
        VECTOR_ELT(paradox_core_payload(core), PARADOX_CORE_PARAMS),
        0U, params)) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSetShadow origin state");
  }
  UNPROTECT(1);
  return core;
}

SEXP paradox_param_set_add_dependency(SEXP private_environment, SEXP self,
    SEXP id_sexp, SEXP on_sexp, SEXP condition, SEXP allow_dangling) {
  R_xlen_t work_since_interrupt = 0;
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("ParamSet method called with a foreign private environment");
  }
  SEXP initial_core = paradox_core_from_private(private_environment);
  if (initial_core == R_UnboundValue) {
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }
  /* The ID lookups below read this node's own schema, so a collection whose
   * children have grown must be flattened again first. */
  if (!paradox_core_is_verified(initial_core)) {
    initial_core = paradox_core_refresh(self, private_environment);
  }
  if (paradox_core_kind(initial_core) == PARADOX_CORE_SHADOW) {
    SEXP core = PROTECT(paradox_core_refresh(
      self,
      private_environment
    ));
    SEXP id = scalar_string(id_sexp, "id");
    SEXP on = scalar_string(on_sexp, "on");
    const int dangling = exact_flag(
      allow_dangling,
      "allow_dangling_dependencies"
    );
    SEXP state = R_ExternalPtrProtected(core);
    paradox_domain_params_t visible;
    if (!paradox_domain_read_params(
        VECTOR_ELT(state, PARADOX_CORE_PARAMS),
        0U, &visible)) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSetShadow visible schema");
    }
    const int id_visible = paradox_domain_find_string(
      visible.ids,
      id,
      &work_since_interrupt
    ) != R_XLEN_T_MAX;
    const int on_visible = paradox_domain_find_string(
      visible.ids,
      on,
      &work_since_interrupt
    ) != R_XLEN_T_MAX;
    if (id_visible && paradox_domain_strings_equal(id, on)) {
      UNPROTECT(1);
      Rf_error("A param cannot depend on itself!");
    }
    SEXP sets = VECTOR_ELT(state, PARADOX_CORE_SETS);
    if (TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isS4(sets) ||
        Rf_isObject(sets) || !paradox_api_has_no_attributes(sets) ||
        XLENGTH(sets) != 1 ||
        TYPEOF(VECTOR_ELT(sets, 0)) != ENVSXP ||
        Rf_isS4(VECTOR_ELT(sets, 0))) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSetShadow origin edge");
    }
    SEXP origin = PROTECT(VECTOR_ELT(sets, 0));
    SEXP origin_private = PROTECT(paradox_domain_private_environment(origin));
    if (TYPEOF(origin_private) != ENVSXP || Rf_isS4(origin_private)) {
      UNPROTECT(3);
      Rf_error("Corrupt ParamSetShadow origin shell");
    }
    paradox_domain_params_t origin_params;
    SEXP origin_core = PROTECT(shadow_origin_core(
      origin,
      origin_private,
      &origin_params
    ));
    /* An endpoint this view does not show is either one it hides or one the
     * origin does not have at all. Only the origin's own ID universe tells
     * the two apart, and only that difference decides between refusing an
     * edge across the boundary and planting an ordinary dangling dependency
     * -- the state a view now shows and enforces like any other set. */
    if (!id_visible || !on_visible) {
      SEXP origin_ids = origin_params.ids;
      const int id_known = paradox_domain_string_in(
        origin_ids,
        id,
        &work_since_interrupt
      );
      const int on_known = paradox_domain_string_in(
        origin_ids,
        on,
        &work_since_interrupt
      );
      if (!id_visible) {
        char *shown = utf8_error_copy(id);
        UNPROTECT(4);
        if (id_known) {
          Rf_error("'%s' is hidden by this ParamSetShadow", shown);
        }
        Rf_error("`id` is not a parameter in this ParamSet");
      }
      if (on_known) {
        char *shown_id = utf8_error_copy(id);
        char *shown_on = utf8_error_copy(on);
        UNPROTECT(4);
        Rf_error(
          PARADOX_SHADOW_CROSSING_MESSAGE,
          "visible",
          shown_id,
          "hidden",
          shown_on
        );
      }
      if (!dangling) {
        UNPROTECT(4);
        Rf_error("`on` is not a parameter in this ParamSet");
      }
    }
    SEXP stable_id = PROTECT(Rf_ScalarString(id));
    SEXP stable_on = PROTECT(Rf_ScalarString(on));
    SEXP stable_condition = PROTECT(snapshot_condition(
      condition,
      &work_since_interrupt
    ));
    /* The origin decides again, from its own current schema: a parent this
     * view could not see is dangling only if the origin agrees it is. */
    SEXP delegated_dangling = PROTECT(Rf_ScalarLogical(!on_visible));
    if (paradox_core_from_private(origin_private) != origin_core) {
      UNPROTECT(8);
      Rf_error(
        "ParamSetShadow origin changed while a dependency was being constructed"
      );
    }
    SEXP result = PROTECT(paradox_param_set_add_dependency(
      origin_private,
      origin,
      stable_id,
      stable_on,
      stable_condition,
      delegated_dangling
    ));
    (void) result;
    UNPROTECT(9);
    return self;
  }
  paradox_core_kind_t kind;
  paradox_domain_params_t params;
  PROTECT_INDEX core_index;
  SEXP core = protected_checked_core(
    private_environment,
    self,
    &kind,
    &params,
    FALSE,
    &core_index
  );
  (void) kind;
  SEXP id = scalar_string(id_sexp, "id");
  SEXP on = scalar_string(on_sexp, "on");
  const int dangling = exact_flag(
    allow_dangling,
    "allow_dangling_dependencies"
  );
  if (paradox_domain_find_string(params.ids, id, &work_since_interrupt) == R_XLEN_T_MAX) {
    UNPROTECT(1);
    Rf_error("`id` is not a parameter in this ParamSet");
  }
  const int parent_present =
    paradox_domain_find_string(params.ids, on, &work_since_interrupt) != R_XLEN_T_MAX;
  if (!dangling && !parent_present) {
    UNPROTECT(1);
    Rf_error("`on` is not a parameter in this ParamSet");
  }
  if (paradox_domain_strings_equal(id, on)) {
    UNPROTECT(1);
    Rf_error("A param cannot depend on itself!");
  }
  SEXP stable_condition = PROTECT(snapshot_condition(
    condition,
    &work_since_interrupt
  ));
  if (parent_present) {
    require_feasible_rhs(
      private_environment,
      self,
      core,
      on,
      stable_condition,
      &work_since_interrupt
    );
  }

  SEXP state = R_ExternalPtrProtected(core);
  paradox_domain_dependencies_t old;
  if (!paradox_domain_validate_dependencies(
      VECTOR_ELT(state, PARADOX_CORE_DEPS),
      &old,
      &work_since_interrupt
    ) || old.row_count == R_XLEN_T_MAX) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSet dependency capsule");
  }
  const R_xlen_t rows = old.row_count + 1;
  static const SEXPTYPE types[] = {STRSXP, STRSXP, VECSXP};
  SEXP result = PROTECT(paradox_domain_new_plain_table(
    dependency_names,
    types,
    3,
    rows
  ));
  for (R_xlen_t row = 0; row < old.row_count; ++row) {
    SET_STRING_ELT(VECTOR_ELT(result, 0), row, STRING_ELT(old.ids, row));
    SET_STRING_ELT(VECTOR_ELT(result, 1), row, STRING_ELT(old.on, row));
    SET_VECTOR_ELT(
      VECTOR_ELT(result, 2),
      row,
      VECTOR_ELT(old.conditions, row)
    );
  }
  SET_STRING_ELT(VECTOR_ELT(result, 0), old.row_count, id);
  SET_STRING_ELT(VECTOR_ELT(result, 1), old.row_count, on);
  SET_VECTOR_ELT(
    VECTOR_ELT(result, 2),
    old.row_count,
    stable_condition
  );
  (void) replace_one(private_environment, core, ".deps", result);
  UNPROTECT(3);
  return self;
}

static int closure_has_formal(SEXP function, SEXP sought) {
  int matches = FALSE;
  if (!paradox_api_closure_formal_matches(
      function,
      sought,
      &matches
    )) {
    Rf_error("Callback has malformed formals");
  }
  return matches;
}

SEXP paradox_param_set_set_callback(SEXP private_environment, SEXP self,
    SEXP callback, SEXP selector) {
  paradox_core_kind_t kind;
  paradox_domain_params_t params;
  PROTECT_INDEX core_index;
  SEXP core = protected_checked_core(
    private_environment,
    self,
    &kind,
    &params,
    FALSE,
    &core_index
  );
  (void) params;
  if (kind != PARADOX_CORE_BASE) {
    UNPROTECT(1);
    Rf_error("Callbacks are read-only outside a base ParamSet");
  }
  if (TYPEOF(selector) != INTSXP || ALTREP(selector) ||
      XLENGTH(selector) != 1 || !paradox_api_has_no_attributes(selector)) {
    UNPROTECT(1);
    Rf_error("Internal error: invalid callback selector");
  }
  const int selected = INTEGER_ELT(selector, 0);
  if (selected != 0 && selected != 1) {
    UNPROTECT(1);
    Rf_error("Internal error: invalid callback selector");
  }
  if (callback != R_NilValue &&
      !closure_has_formal(callback, Rf_install("x"))) {
    UNPROTECT(1);
    Rf_error("Callback must be a function with a formal argument named `x`");
  }
  (void) replace_one(
    private_environment,
    core,
    selected == 0 ? ".extra_trafo" : ".constraint",
    callback
  );
  UNPROTECT(1);
  return callback;
}
