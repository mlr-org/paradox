#include <limits.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "builtin_condition.h"
#include "core_state.h"
#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

static const char *const dependency_names[] = {"id", "on", "cond"};
static const char *const tag_names[] = {"id", "tag"};

static char *utf8_error_copy(SEXP string) {
  PROTECT(string);
  const size_t size = strlen(Rf_translateCharUTF8(string));
  if ((uintmax_t) size >= (uintmax_t) R_XLEN_T_MAX) {
    Rf_error("ParamSet diagnostic is too large");
  }
  char *copy = paradox_temporary_alloc(
    (R_xlen_t) size + 1,
    sizeof(*copy)
  );
  /* Keep no translation workspace pointer across the allocation or the
   * subsequent error construction. */
  memcpy(copy, Rf_translateCharUTF8(string), size + 1U);
  UNPROTECT(1);
  return copy;
}

static SEXP character_vector(const char *const *values, R_xlen_t size) {
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    SET_STRING_ELT(result, index, Rf_mkCharCE(values[index], CE_UTF8));
  }
  UNPROTECT(1);
  return result;
}

static SEXP new_plain_table(const char *const *names, const SEXPTYPE *types,
    R_xlen_t column_count, R_xlen_t row_count) {
  if (row_count > INT_MAX) {
    Rf_error("ParamSet state table exceeds the supported row count");
  }
  SEXP table = PROTECT(Rf_allocVector(VECSXP, column_count));
  for (R_xlen_t column = 0; column < column_count; ++column) {
    SEXP values = PROTECT(Rf_allocVector(types[column], row_count));
    SET_VECTOR_ELT(table, column, values);
    UNPROTECT(1);
  }
  SEXP table_names = PROTECT(character_vector(names, column_count));
  static const char *const classes[] = {"data.frame"};
  SEXP table_class = PROTECT(character_vector(classes, 1));
  SEXP row_names = PROTECT(Rf_allocVector(
    INTSXP,
    row_count == 0 ? 0 : 2
  ));
  if (row_count != 0) {
    SET_INTEGER_ELT(row_names, 0, NA_INTEGER);
    SET_INTEGER_ELT(row_names, 1, -(int) row_count);
  }
  Rf_setAttrib(table, R_NamesSymbol, table_names);
  Rf_setAttrib(table, R_ClassSymbol, table_class);
  Rf_setAttrib(table, R_RowNamesSymbol, row_names);
  UNPROTECT(4);
  return table;
}

static int supported_string(SEXP value) {
  return value != NA_STRING && Rf_getCharCE(value) != CE_BYTES;
}

static int strings_equal(SEXP left, SEXP right) {
  return left == right || paradox_domain_strings_equal(left, right);
}

static R_xlen_t find_id(SEXP ids, SEXP sought,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t size = XLENGTH(ids);
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if (strings_equal(STRING_ELT(ids, index), sought)) {
      return index;
    }
  }
  return R_XLEN_T_MAX;
}

static int exact_flag(SEXP value, const char *name) {
  if (TYPEOF(value) != LGLSXP || ALTREP(value) || XLENGTH(value) != 1 ||
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
  if (TYPEOF(value) != STRSXP || XLENGTH(value) != 1) {
    Rf_error("`%s` must be a character scalar", name);
  }
  SEXP result = STRING_ELT(value, 0);
  if (!supported_string(result)) {
    Rf_error("`%s` must be a non-missing, non-bytes string", name);
  }
  return result;
}

static SEXP checked_core(SEXP private_environment, SEXP self,
    paradox_core_kind_t *kind, paradox_domain_params_t *params,
    R_xlen_t *work_since_interrupt) {
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("ParamSet method called with a foreign private environment");
  }
  SEXP core = paradox_core_from_private(private_environment);
  if (core == R_UnboundValue) {
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }
  *kind = paradox_core_kind(core);
  if (*kind == PARADOX_CORE_SHADOW) {
    Rf_error("ParamSetShadow state is read-only at this mutation boundary");
  }
  SEXP state = R_ExternalPtrProtected(core);
  R_xlen_t unused_row = 0;
  if (!paradox_domain_validate_params(
      VECTOR_ELT(state, PARADOX_CORE_PARAMS),
      R_NilValue,
      TRUE,
      params,
      &unused_row,
      work_since_interrupt
    )) {
    Rf_error("Corrupt ParamSet parameter capsule");
  }
  return core;
}

static SEXP replace_one(SEXP private_environment, SEXP expected_core,
    const char *field, SEXP value) {
  if (paradox_core_from_private(private_environment) != expected_core) {
    Rf_error("ParamSet changed while a native mutation was being validated");
  }
  SEXP updates = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(updates, 0, value);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(names, 0, Rf_mkCharCE(field, CE_UTF8));
  Rf_setAttrib(updates, R_NamesSymbol, names);
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
  SEXP rhs = R_NilValue;
  SEXP stable_rhs = PROTECT(paradox_builtin_condition_admit(
      condition,
      &kind,
      &rhs,
      work_since_interrupt
  ));
  if (stable_rhs == R_NilValue) {
    UNPROTECT(2);
    Rf_error("Malformed built-in dependency Condition");
  }
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
  SEXP result_names = PROTECT(character_vector(names, 2));
  const char *const *classes = kind == PARADOX_BUILTIN_CONDITION_EQUAL
    ? (const char *const[]) {"CondEqual", "Condition"}
    : (const char *const[]) {"CondAnyOf", "Condition"};
  SEXP result_class = PROTECT(character_vector(classes, 2));
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
  SEXP table = PROTECT(new_plain_table(
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
  if (TYPEOF(input) != VECSXP || ALTREP(input)) {
    Rf_error("`deps` must be a three-column table or an empty table");
  }
  const R_xlen_t column_count = XLENGTH(input);
  if (column_count == 0) {
    return new_plain_table(dependency_names, types, 3, 0);
  }
  R_xlen_t *column_sizes = paradox_temporary_alloc(
    column_count,
    sizeof(*column_sizes)
  );
  int all_empty = TRUE;
  for (R_xlen_t column = 0; column < column_count; ++column) {
    column_sizes[column] = XLENGTH(VECTOR_ELT(input, column));
    if (column_sizes[column] != 0) {
      all_empty = FALSE;
    }
  }
  if (all_empty) {
    return new_plain_table(dependency_names, types, 3, 0);
  }
  if (column_count != 3) {
    Rf_error("`deps` must be a three-column table or an empty table");
  }
  SEXP names = PROTECT(Rf_getAttrib(input, R_NamesSymbol));
  if (TYPEOF(names) != STRSXP || XLENGTH(names) != 3) {
    UNPROTECT(1);
    Rf_error("`deps` columns must be named `id`, `on`, and `cond`");
  }
  for (R_xlen_t column = 0; column < 3; ++column) {
    if (!strings_equal(
        STRING_ELT(names, column),
        Rf_mkCharCE(dependency_names[column], CE_UTF8)
      )) {
      UNPROTECT(1);
      Rf_error("`deps` columns must be named `id`, `on`, and `cond`");
    }
  }
  SEXP ids = PROTECT(VECTOR_ELT(input, 0));
  SEXP on = PROTECT(VECTOR_ELT(input, 1));
  SEXP conditions = PROTECT(VECTOR_ELT(input, 2));
  if (TYPEOF(ids) != STRSXP || TYPEOF(on) != STRSXP ||
      TYPEOF(conditions) != VECSXP || ALTREP(conditions)) {
    UNPROTECT(4);
    Rf_error("`deps` columns have unsupported types");
  }
  const R_xlen_t rows = column_sizes[0];
  if (column_sizes[1] != rows || column_sizes[2] != rows) {
    UNPROTECT(4);
    Rf_error("`deps` columns must have equal lengths");
  }
  SEXP result = PROTECT(new_plain_table(
    dependency_names,
    types,
    3,
    rows
  ));
  SEXP result_ids = VECTOR_ELT(result, 0);
  SEXP result_on = VECTOR_ELT(result, 1);
  SEXP result_conditions = VECTOR_ELT(result, 2);
  for (R_xlen_t row = 0; row < rows; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP id = STRING_ELT(ids, row);
    SEXP parent = STRING_ELT(on, row);
    if (!supported_string(id) || !supported_string(parent)) {
      UNPROTECT(5);
      Rf_error("`deps$id` and `deps$on` may not contain missing/bytes strings");
    }
    if (params != NULL &&
        find_id(params->ids, id, work_since_interrupt) == R_XLEN_T_MAX) {
      UNPROTECT(5);
      Rf_error("Dependency child is not a parameter in this ParamSet");
    }
    if (params != NULL && strings_equal(id, parent)) {
      UNPROTECT(5);
      Rf_error("A param cannot depend on itself!");
    }
    SEXP condition = PROTECT(snapshot_condition(
      VECTOR_ELT(conditions, row),
      work_since_interrupt
    ));
    SET_STRING_ELT(result_ids, row, id);
    SET_STRING_ELT(result_on, row, parent);
    SET_VECTOR_ELT(result_conditions, row, condition);
    UNPROTECT(1);
  }
  UNPROTECT(5);
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

SEXP paradox_param_set_dependencies(SEXP private_environment, SEXP self) {
  R_xlen_t work_since_interrupt = 0;
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("ParamSet method called with a foreign private environment");
  }
  SEXP core = paradox_core_from_private(private_environment);
  if (core == R_UnboundValue) {
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }
  if (paradox_core_kind(core) == PARADOX_CORE_SHADOW) {
    core = paradox_core_refresh_shadow(self, private_environment);
  }
  SEXP state = R_ExternalPtrProtected(core);
  return snapshot_dependencies(
    VECTOR_ELT(state, PARADOX_CORE_DEPS),
    NULL,
    &work_since_interrupt
  );
}

SEXP paradox_param_set_get_tags(SEXP private_environment, SEXP self) {
  R_xlen_t work_since_interrupt = 0;
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("ParamSet method called with a foreign private environment");
  }
  SEXP core = PROTECT(paradox_core_from_private(private_environment));
  if (core == R_UnboundValue) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }
  if (paradox_core_kind(core) == PARADOX_CORE_SHADOW) {
    SEXP refreshed = PROTECT(paradox_core_refresh_shadow(
      self,
      private_environment
    ));
    core = refreshed;
    UNPROTECT(1);
  }
  SEXP state = R_ExternalPtrProtected(core);
  paradox_domain_params_t params;
  paradox_domain_tags_t tags;
  R_xlen_t unused_row = 0;
  if (!paradox_domain_validate_params(
      VECTOR_ELT(state, PARADOX_CORE_PARAMS),
      R_NilValue,
      TRUE,
      &params,
      &unused_row,
      &work_since_interrupt
    ) || !paradox_domain_validate_tags(
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
  for (R_xlen_t row = 0; row < tags.row_count; ++row) {
    const R_xlen_t parameter = find_id(
      params.ids,
      STRING_ELT(tags.ids, row),
      &work_since_interrupt
    );
    if (parameter == R_XLEN_T_MAX || counts[parameter] == R_XLEN_T_MAX) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSet tag ID");
    }
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
    const R_xlen_t parameter = find_id(
      params.ids,
      STRING_ELT(tags.ids, row),
      &work_since_interrupt
    );
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

SEXP paradox_param_set_set_tags(SEXP private_environment, SEXP self,
    SEXP tags) {
  R_xlen_t work_since_interrupt = 0;
  paradox_core_kind_t kind;
  paradox_domain_params_t params;
  SEXP core = PROTECT(checked_core(
    private_environment,
    self,
    &kind,
    &params,
    &work_since_interrupt
  ));
  (void) kind;
  if (TYPEOF(tags) != VECSXP || ALTREP(tags)) {
    UNPROTECT(1);
    Rf_error("`tags` must be a list");
  }
  const R_xlen_t parameter_count = params.row_count;
  const R_xlen_t input_count = XLENGTH(tags);
  SEXP input_names = PROTECT(Rf_getAttrib(tags, R_NamesSymbol));
  if (input_count != 0) {
    if (input_count != parameter_count) {
      UNPROTECT(2);
      Rf_error("Non-empty `tags` must name every parameter exactly once");
    }
    if (TYPEOF(input_names) != STRSXP || XLENGTH(input_names) != input_count) {
      UNPROTECT(2);
      Rf_error("Non-empty `tags` must be completely named");
    }
  }
  SEXP stable_names = PROTECT(Rf_allocVector(STRSXP, input_count));
  SEXP stable_tags = PROTECT(Rf_allocVector(VECSXP, input_count));
  R_xlen_t row_count = 0;
  if (input_count != 0) {
    unsigned char *seen = paradox_temporary_alloc(
      parameter_count,
      sizeof(*seen)
    );
    for (R_xlen_t index = 0; index < parameter_count; ++index) {
      seen[index] = 0U;
    }
    for (R_xlen_t parameter = 0; parameter < input_count; ++parameter) {
      SEXP id = STRING_ELT(input_names, parameter);
      const R_xlen_t row = supported_string(id)
        ? find_id(params.ids, id, &work_since_interrupt)
        : R_XLEN_T_MAX;
      if (row == R_XLEN_T_MAX || seen[row]) {
        UNPROTECT(4);
        Rf_error("`tags` names must be a permutation of parameter IDs");
      }
      seen[row] = 1;
      SET_STRING_ELT(stable_names, parameter, id);
      SEXP values = VECTOR_ELT(tags, parameter);
      if (TYPEOF(values) != STRSXP) {
        UNPROTECT(4);
        Rf_error("Every `tags` element must be a character vector");
      }
      const R_xlen_t size = XLENGTH(values);
      if (row_count > R_XLEN_T_MAX - size) {
        UNPROTECT(4);
        Rf_error("`tags` contains too many values");
      }
      SEXP stable_values = PROTECT(Rf_allocVector(STRSXP, size));
      for (R_xlen_t index = 0; index < size; ++index) {
        paradox_domain_account_work(&work_since_interrupt);
        SEXP value = STRING_ELT(values, index);
        if (!supported_string(value)) {
          UNPROTECT(5);
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
  SEXP table = PROTECT(new_plain_table(tag_names, types, 2, row_count));
  R_xlen_t output = 0;
  for (R_xlen_t parameter = 0; parameter < input_count; ++parameter) {
    SEXP id = STRING_ELT(stable_names, parameter);
    SEXP values = VECTOR_ELT(stable_tags, parameter);
    const R_xlen_t size = XLENGTH(values);
    for (R_xlen_t index = 0; index < size; ++index) {
      paradox_domain_account_work(&work_since_interrupt);
      SEXP value = STRING_ELT(values, index);
      SET_STRING_ELT(VECTOR_ELT(table, 0), output, id);
      SET_STRING_ELT(VECTOR_ELT(table, 1), output, value);
      ++output;
    }
  }
  (void) replace_one(private_environment, core, ".tags", table);
  UNPROTECT(5);
  return tags;
}

SEXP paradox_param_set_set_dependencies(SEXP private_environment, SEXP self,
    SEXP dependencies) {
  R_xlen_t work_since_interrupt = 0;
  paradox_core_kind_t kind;
  paradox_domain_params_t params;
  SEXP core = PROTECT(checked_core(
    private_environment,
    self,
    &kind,
    &params,
    &work_since_interrupt
  ));
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
  if (paradox_core_kind(initial_core) == PARADOX_CORE_SHADOW) {
    SEXP core = PROTECT(paradox_core_refresh_shadow(
      self,
      private_environment
    ));
    SEXP id = scalar_string(id_sexp, "id");
    SEXP on = scalar_string(on_sexp, "on");
    (void) exact_flag(
      allow_dangling,
      "allow_dangling_dependencies"
    );
    SEXP state = R_ExternalPtrProtected(core);
    paradox_domain_params_t visible;
    R_xlen_t unused_row = 0;
    if (!paradox_domain_validate_params(
        VECTOR_ELT(state, PARADOX_CORE_PARAMS),
        R_NilValue,
        TRUE,
        &visible,
        &unused_row,
        &work_since_interrupt
      ) || find_id(visible.ids, id, &work_since_interrupt) == R_XLEN_T_MAX ||
        find_id(visible.ids, on, &work_since_interrupt) == R_XLEN_T_MAX) {
      UNPROTECT(1);
      Rf_error("Shadow dependencies must stay inside the visible schema");
    }
    if (strings_equal(id, on)) {
      UNPROTECT(1);
      Rf_error("A param cannot depend on itself!");
    }
    SEXP sets = VECTOR_ELT(state, PARADOX_CORE_SETS);
    if (TYPEOF(sets) != VECSXP || XLENGTH(sets) != 1 ||
        TYPEOF(VECTOR_ELT(sets, 0)) != ENVSXP) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSetShadow origin edge");
    }
    SEXP origin = PROTECT(VECTOR_ELT(sets, 0));
    SEXP origin_private = PROTECT(paradox_domain_private_environment(origin));
    if (TYPEOF(origin_private) != ENVSXP) {
      UNPROTECT(3);
      Rf_error("Corrupt ParamSetShadow origin shell");
    }
    SEXP stable_id = PROTECT(Rf_ScalarString(id));
    SEXP stable_on = PROTECT(Rf_ScalarString(on));
    SEXP stable_condition = PROTECT(snapshot_condition(
      condition,
      &work_since_interrupt
    ));
    SEXP no_dangling = PROTECT(Rf_ScalarLogical(FALSE));
    SEXP result = PROTECT(paradox_param_set_add_dependency(
      origin_private,
      origin,
      stable_id,
      stable_on,
      stable_condition,
      no_dangling
    ));
    (void) result;
    UNPROTECT(8);
    return self;
  }
  paradox_core_kind_t kind;
  paradox_domain_params_t params;
  SEXP core = PROTECT(checked_core(
    private_environment,
    self,
    &kind,
    &params,
    &work_since_interrupt
  ));
  (void) kind;
  SEXP id = scalar_string(id_sexp, "id");
  SEXP on = scalar_string(on_sexp, "on");
  const int dangling = exact_flag(
    allow_dangling,
    "allow_dangling_dependencies"
  );
  if (find_id(params.ids, id, &work_since_interrupt) == R_XLEN_T_MAX) {
    UNPROTECT(1);
    Rf_error("`id` is not a parameter in this ParamSet");
  }
  const int parent_present =
    find_id(params.ids, on, &work_since_interrupt) != R_XLEN_T_MAX;
  if (!dangling && !parent_present) {
    UNPROTECT(1);
    Rf_error("`on` is not a parameter in this ParamSet");
  }
  if (strings_equal(id, on)) {
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
  SEXP result = PROTECT(new_plain_table(
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
  if (TYPEOF(function) != CLOSXP) {
    return FALSE;
  }
  for (SEXP formal = paradox_api_closure_formals(function);
      formal != R_NilValue;
      formal = CDR(formal)) {
    if (TAG(formal) == sought) {
      return TRUE;
    }
  }
  return FALSE;
}

SEXP paradox_param_set_set_callback(SEXP private_environment, SEXP self,
    SEXP callback, SEXP selector) {
  R_xlen_t work_since_interrupt = 0;
  paradox_core_kind_t kind;
  paradox_domain_params_t params;
  SEXP core = PROTECT(checked_core(
    private_environment,
    self,
    &kind,
    &params,
    &work_since_interrupt
  ));
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
