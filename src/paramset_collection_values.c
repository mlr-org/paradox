#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "paramset_domain_common.h"
#include "paramset_params_internal.h"
#include "r_api_compat.h"
#include "r_utils.h"

typedef enum {
  VALUES_NODE_UNKNOWN = 0,
  VALUES_NODE_SET,
  VALUES_NODE_COLLECTION
} values_node_kind_t;

typedef struct {
  SEXP table;
  SEXP names;
  SEXP classes;
  SEXP sorted;
  SEXP index;
  SEXP index_cache;
  SEXP ids;
  SEXP original_ids;
  SEXP owner_indices;
  SEXP owner_names;
  R_xlen_t row_count;
} values_translation_t;

typedef struct {
  SEXP params_sexp;
  SEXP values_sexp;
  paradox_domain_params_t params;
  SEXP params_columns[PARADOX_DOMAIN_TAGS];
  paradox_domain_values_t values;
} values_private_state_t;

typedef struct {
  SEXP self;
  SEXP private_environment;
  values_node_kind_t kind;
  values_private_state_t params_state;
  SEXP sets;
  SEXP set_names;
  R_xlen_t child_count;
  SEXP postfix;
  values_translation_t translation;
  R_xlen_t *translation_by_param;
  R_xlen_t root_start;
  R_xlen_t next_child;
  R_xlen_t consumed_rows;
} values_frame_t;

typedef struct {
  SEXP values;
  R_xlen_t root_start;
  R_xlen_t size;
  R_xlen_t *parameter_rows;
} values_leaf_t;

typedef struct {
  SEXP roots;
  R_xlen_t used;
  R_xlen_t capacity;
} values_root_plan_t;

enum values_root_slot {
  VALUES_ROOT_SELF = 0,
  VALUES_ROOT_PRIVATE,
  VALUES_ROOT_PARAMS,
  VALUES_ROOT_PARAM_NAMES,
  VALUES_ROOT_PARAM_CLASS_ATTRIBUTE,
  VALUES_ROOT_PARAM_ROW_NAMES,
  VALUES_ROOT_PARAM_INDEX,
  VALUES_ROOT_PARAM_INDEX_CACHE,
  VALUES_ROOT_PARAM_SELFREF,
  VALUES_ROOT_PARAM_COLUMNS,
  VALUES_ROOT_VALUES = VALUES_ROOT_PARAM_COLUMNS + PARADOX_DOMAIN_TAGS,
  VALUES_ROOT_VALUE_NAMES,
  VALUES_ROOT_VALUE_SNAPSHOT,
  VALUES_ROOT_SETS,
  VALUES_ROOT_SET_NAMES,
  VALUES_ROOT_TRANSLATION,
  VALUES_ROOT_TRANSLATION_NAMES,
  VALUES_ROOT_TRANSLATION_CLASSES,
  VALUES_ROOT_TRANSLATION_SORTED,
  VALUES_ROOT_TRANSLATION_INDEX,
  VALUES_ROOT_TRANSLATION_INDEX_CACHE,
  VALUES_ROOT_TRANSLATION_IDS,
  VALUES_ROOT_TRANSLATION_ORIGINAL_IDS,
  VALUES_ROOT_TRANSLATION_OWNER_INDICES,
  VALUES_ROOT_TRANSLATION_OWNER_NAMES,
  VALUES_ROOT_POSTFIX,
  VALUES_ROOT_COUNT
};

static values_node_kind_t exact_node_kind(SEXP self,
    R_xlen_t *work_since_interrupt) {
  static const char *const set_classes[] = {"ParamSet", "R6"};
  static const char *const collection_classes[] = {
    "ParamSetCollection", "ParamSet", "R6"
  };
  if (TYPEOF(self) != ENVSXP) {
    return VALUES_NODE_UNKNOWN;
  }
  SEXP classes = Rf_getAttrib(self, R_ClassSymbol);
  if (paradox_domain_exact_string_vector(
      classes,
      set_classes,
      2,
      work_since_interrupt
    )) {
    return VALUES_NODE_SET;
  }
  if (paradox_domain_exact_string_vector(
      classes,
      collection_classes,
      3,
      work_since_interrupt
    )) {
    return VALUES_NODE_COLLECTION;
  }
  return VALUES_NODE_UNKNOWN;
}

static SEXP owned_private_environment(SEXP self) {
  SEXP enclosure = PROTECT(paradox_domain_local_value(
    self,
    ".__enclos_env__"
  ));
  if (TYPEOF(enclosure) != ENVSXP) {
    UNPROTECT(1);
    return R_UnboundValue;
  }
  SEXP private_environment = paradox_domain_local_value(enclosure, "private");
  SEXP result = TYPEOF(private_environment) == ENVSXP
    ? private_environment
    : R_UnboundValue;
  UNPROTECT(1);
  return result;
}

static int exact_private_wrapper_call(SEXP function,
    const char *method_name) {
  SEXP body = paradox_api_closure_expression(function);
  if (TYPEOF(body) != LANGSXP || CAR(body) != Rf_install(method_name)) {
    return FALSE;
  }
  static const char *const arguments[] = {"self", "private", "super"};
  SEXP argument = CDR(body);
  for (R_xlen_t index = 0; index < 3; ++index) {
    SEXP symbol = Rf_install(arguments[index]);
    if (argument == R_NilValue || TAG(argument) != symbol ||
        CAR(argument) != symbol) {
      return FALSE;
    }
    argument = CDR(argument);
  }
  return argument == R_NilValue &&
    paradox_api_closure_formals(function) == R_NilValue;
}

static int exact_public_wrapper_call(SEXP function) {
  SEXP body = paradox_api_closure_expression(function);
  if (TYPEOF(body) != LANGSXP ||
      CAR(body) != Rf_install(".__ParamSet__values")) {
    return FALSE;
  }
  static const char *const arguments[] = {
    "self", "private", "super", "xs"
  };
  SEXP argument = CDR(body);
  for (R_xlen_t index = 0; index < 4; ++index) {
    SEXP symbol = Rf_install(arguments[index]);
    if (argument == R_NilValue || TAG(argument) != symbol ||
        CAR(argument) != symbol) {
      return FALSE;
    }
    argument = CDR(argument);
  }
  SEXP formal = paradox_api_closure_formals(function);
  SEXP xs_symbol = Rf_install("xs");
  return argument == R_NilValue && formal != R_NilValue &&
    TAG(formal) == xs_symbol && CAR(formal) == R_MissingArg &&
    CDR(formal) == R_NilValue;
}

static int exact_private_getter_on(SEXP binding_environment,
    SEXP expected_self, SEXP private_environment, SEXP namespace_environment,
    const char *method_name, int require_locked, int require_unbound_super,
    SEXP *captured_super_out, SEXP *wrapper_environment_out) {
  SEXP getter_symbol = Rf_install(".get_values");
  if (TYPEOF(binding_environment) != ENVSXP ||
      !R_existsVarInFrame(binding_environment, getter_symbol) ||
      R_BindingIsActive(getter_symbol, binding_environment) ||
      (require_locked &&
        !R_BindingIsLocked(getter_symbol, binding_environment))) {
    return FALSE;
  }
  SEXP function = PROTECT(paradox_domain_local_value(
    binding_environment,
    ".get_values"
  ));
  if (TYPEOF(function) != CLOSXP ||
      !exact_private_wrapper_call(function, method_name)) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP environment = paradox_api_closure_environment(function);
  SEXP method_symbol = Rf_install(method_name);
  SEXP super_symbol = Rf_install("super");
  if (TYPEOF(environment) != ENVSXP ||
      paradox_api_parent_environment(environment) != namespace_environment ||
      paradox_domain_local_value(environment, "self") != expected_self ||
      paradox_domain_local_value(environment, "private") !=
        private_environment ||
      R_existsVarInFrame(environment, method_symbol) ||
      (require_unbound_super &&
        R_existsVarInFrame(environment, super_symbol)) ||
      (!require_unbound_super &&
        !R_existsVarInFrame(environment, super_symbol))) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP captured_super = require_unbound_super
    ? R_UnboundValue
    : paradox_domain_local_value(environment, "super");
  if (!require_unbound_super && TYPEOF(captured_super) != ENVSXP) {
    UNPROTECT(1);
    return FALSE;
  }
  if (captured_super_out != NULL) {
    *captured_super_out = captured_super;
  }
  if (wrapper_environment_out != NULL) {
    *wrapper_environment_out = environment;
  }
  UNPROTECT(1);
  return TRUE;
}

static int canonical_private_getter(SEXP self, SEXP private_environment,
    SEXP namespace_environment, values_node_kind_t kind) {
  if (kind == VALUES_NODE_SET) {
    return exact_private_getter_on(
      private_environment,
      self,
      private_environment,
      namespace_environment,
      ".__ParamSet__.get_values",
      TRUE,
      TRUE,
      NULL,
      NULL
    );
  }
  if (kind != VALUES_NODE_COLLECTION) {
    return FALSE;
  }

  SEXP captured_super = R_UnboundValue;
  if (!exact_private_getter_on(
      private_environment,
      self,
      private_environment,
      namespace_environment,
      ".__ParamSetCollection__.get_values",
      TRUE,
      FALSE,
      &captured_super,
      NULL
    ) || TYPEOF(captured_super) != ENVSXP) {
    return FALSE;
  }
  SEXP super_enclosure = paradox_domain_local_value(
    captured_super,
    ".__enclos_env__"
  );
  SEXP wrapper_environment = R_UnboundValue;
  return TYPEOF(super_enclosure) == ENVSXP && exact_private_getter_on(
      captured_super,
      self,
      private_environment,
      namespace_environment,
      ".__ParamSet__.get_values",
      FALSE,
      TRUE,
      NULL,
      &wrapper_environment
    ) && wrapper_environment == super_enclosure;
}

static int canonical_public_getter(SEXP self, SEXP private_environment,
    SEXP namespace_environment) {
  SEXP symbol = Rf_install("values");
  if (!R_existsVarInFrame(self, symbol) ||
      !R_BindingIsActive(symbol, self)) {
    return FALSE;
  }
  SEXP function = R_ActiveBindingFunction(symbol, self);
  if (TYPEOF(function) != CLOSXP) {
    return FALSE;
  }
  if (!exact_public_wrapper_call(function)) {
    return FALSE;
  }
  SEXP environment = paradox_api_closure_environment(function);
  SEXP super_symbol = Rf_install("super");
  SEXP target_symbol = Rf_install(".__ParamSet__values");
  return TYPEOF(environment) == ENVSXP &&
    paradox_api_parent_environment(environment) == namespace_environment &&
    paradox_domain_local_value(environment, "self") == self &&
    paradox_domain_local_value(environment, "private") ==
      private_environment &&
    !R_existsVarInFrame(environment, super_symbol) &&
    !R_existsVarInFrame(environment, target_symbol);
}

static int exact_index_marker(SEXP index, const char *marker_name,
    SEXP roots, R_xlen_t index_slot, R_xlen_t cache_slot) {
  if (TYPEOF(index) != INTSXP || ALTREP(index)) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, index_slot, index);
  SEXP marker = Rf_install(marker_name);
  SEXP cache = Rf_getAttrib(index, marker);
  if (TYPEOF(cache) != INTSXP || ALTREP(cache)) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, cache_slot, cache);
  if (XLENGTH(index) != 0) {
    return FALSE;
  }
  return paradox_api_has_single_attribute(index, marker_name);
}

static int exact_or_invalidated_params_index(SEXP index, SEXP roots,
    R_xlen_t index_slot, R_xlen_t cache_slot) {
  if (TYPEOF(index) != INTSXP || ALTREP(index)) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, index_slot, index);
  if (paradox_api_has_no_attributes(index)) {
    /* data.table invalidates its secondary-index marker in place after an
     * indexed column is updated.  The resulting ordinary integer(0) is part
     * of the observed private-object compatibility surface, but carries no
     * state that this reader consumes.  Admit only that inert exact shape;
     * all parameter columns are authenticated independently below. */
    SET_VECTOR_ELT(roots, cache_slot, R_NilValue);
    return XLENGTH(index) == 0;
  }
  return exact_index_marker(
    index,
    "__id__cls__grouping",
    roots,
    index_slot,
    cache_slot
  );
}

static int supported_name(SEXP name) {
  return name != NA_STRING && Rf_getCharCE(name) != CE_BYTES;
}

static int validate_set_names(SEXP sets, SEXP *names,
    R_xlen_t *set_count, SEXP roots, R_xlen_t roots_offset,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isObject(sets) ||
      !paradox_params_names_are_only_attribute(sets)) {
    return FALSE;
  }
  const R_xlen_t observed_set_count = XLENGTH(sets);
  *names = Rf_getAttrib(sets, R_NamesSymbol);
  if (TYPEOF(*names) != STRSXP || ALTREP(*names) ||
      !paradox_api_has_no_attributes(*names)) {
    return FALSE;
  }
  SET_VECTOR_ELT(
    roots,
    roots_offset + VALUES_ROOT_SET_NAMES,
    *names
  );
  const R_xlen_t name_count = XLENGTH(*names);
  if (name_count != observed_set_count) {
    return FALSE;
  }
  for (R_xlen_t right = 0; right < name_count; ++right) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP right_name = STRING_ELT(*names, right);
    if (!supported_name(right_name)) {
      return FALSE;
    }
    if (paradox_domain_string_is(right_name, "")) {
      continue;
    }
    for (R_xlen_t left = 0; left < right; ++left) {
      paradox_domain_account_work(work_since_interrupt);
      SEXP left_name = STRING_ELT(*names, left);
      if (!paradox_domain_string_is(left_name, "") &&
          paradox_domain_strings_equal(left_name, right_name)) {
        return FALSE;
      }
    }
  }
  *set_count = observed_set_count;
  return TRUE;
}

static int exact_translation_table(SEXP table,
    values_translation_t *translation, SEXP roots, R_xlen_t roots_offset,
    R_xlen_t *work_since_interrupt) {
  static const char *const column_names[] = {
    "id", "original_id", "owner_ps_index", "owner_name"
  };
  static const char *const classes[] = {"data.table", "data.frame"};
  static const char *const key[] = {"id"};
  if (TYPEOF(table) != VECSXP || ALTREP(table)) {
    return FALSE;
  }
  const R_xlen_t column_count = XLENGTH(table);
  SEXP names = Rf_getAttrib(table, R_NamesSymbol);
  SEXP table_classes = Rf_getAttrib(table, R_ClassSymbol);
  SEXP sorted = Rf_getAttrib(table, Rf_install("sorted"));
  SEXP index = Rf_getAttrib(table, Rf_install("index"));
  SET_VECTOR_ELT(
    roots,
    roots_offset + VALUES_ROOT_TRANSLATION_NAMES,
    names
  );
  SET_VECTOR_ELT(
    roots,
    roots_offset + VALUES_ROOT_TRANSLATION_CLASSES,
    table_classes
  );
  SET_VECTOR_ELT(
    roots,
    roots_offset + VALUES_ROOT_TRANSLATION_SORTED,
    sorted
  );
  if (column_count != 4 || TYPEOF(names) != STRSXP || ALTREP(names) ||
      TYPEOF(table_classes) != STRSXP || ALTREP(table_classes) ||
      TYPEOF(sorted) != STRSXP || ALTREP(sorted) ||
      !paradox_api_has_no_attributes(names) ||
      !paradox_api_has_no_attributes(table_classes) ||
      !paradox_api_has_no_attributes(sorted) ||
      !paradox_domain_exact_string_vector(
        names,
        column_names,
        4,
        work_since_interrupt
      ) || !paradox_domain_exact_string_vector(
        table_classes,
        classes,
        2,
        work_since_interrupt
      ) || !paradox_domain_exact_string_vector(
        sorted,
        key,
        1,
        work_since_interrupt
      ) || !paradox_params_supported_table_attributes(table, TRUE) ||
      Rf_getAttrib(table, R_RowNamesSymbol) != R_NilValue ||
      Rf_getAttrib(table, Rf_install(".internal.selfref")) != R_NilValue ||
      !exact_index_marker(
        index,
        "__original_id",
        roots,
        roots_offset + VALUES_ROOT_TRANSLATION_INDEX,
        roots_offset + VALUES_ROOT_TRANSLATION_INDEX_CACHE
      )) {
    return FALSE;
  }

  SEXP ids = VECTOR_ELT(table, 0);
  SEXP original_ids = VECTOR_ELT(table, 1);
  SEXP owner_indices = VECTOR_ELT(table, 2);
  SEXP owner_names = VECTOR_ELT(table, 3);
  SET_VECTOR_ELT(
    roots,
    roots_offset + VALUES_ROOT_TRANSLATION_IDS,
    ids
  );
  SET_VECTOR_ELT(
    roots,
    roots_offset + VALUES_ROOT_TRANSLATION_ORIGINAL_IDS,
    original_ids
  );
  SET_VECTOR_ELT(
    roots,
    roots_offset + VALUES_ROOT_TRANSLATION_OWNER_INDICES,
    owner_indices
  );
  SET_VECTOR_ELT(
    roots,
    roots_offset + VALUES_ROOT_TRANSLATION_OWNER_NAMES,
    owner_names
  );
  if (TYPEOF(ids) != STRSXP || TYPEOF(original_ids) != STRSXP ||
      TYPEOF(owner_indices) != INTSXP || TYPEOF(owner_names) != STRSXP ||
      ALTREP(ids) || ALTREP(original_ids) || ALTREP(owner_indices) ||
      ALTREP(owner_names) ||
      !paradox_api_has_no_attributes(ids) ||
      !paradox_api_has_no_attributes(original_ids) ||
      !paradox_api_has_no_attributes(owner_indices) ||
      !paradox_api_has_no_attributes(owner_names)) {
    return FALSE;
  }
  const R_xlen_t row_count = XLENGTH(ids);
  if (XLENGTH(original_ids) != row_count ||
      XLENGTH(owner_indices) != row_count ||
      XLENGTH(owner_names) != row_count) {
    return FALSE;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    if (!supported_name(STRING_ELT(ids, row)) ||
        !supported_name(STRING_ELT(original_ids, row)) ||
        !supported_name(STRING_ELT(owner_names, row)) ||
        INTEGER_ELT(owner_indices, row) == NA_INTEGER) {
      return FALSE;
    }
  }
  if (Rf_any_duplicated(ids, FALSE) != 0) {
    return FALSE;
  }
  translation->table = table;
  translation->names = names;
  translation->classes = table_classes;
  translation->sorted = sorted;
  translation->index = index;
  translation->index_cache = VECTOR_ELT(
    roots,
    roots_offset + VALUES_ROOT_TRANSLATION_INDEX_CACHE
  );
  translation->ids = ids;
  translation->original_ids = original_ids;
  translation->owner_indices = owner_indices;
  translation->owner_names = owner_names;
  translation->row_count = row_count;
  return TRUE;
}

static R_xlen_t match_position(SEXP matches, R_xlen_t index) {
  if (TYPEOF(matches) == INTSXP) {
    const int value = INTEGER_ELT(matches, index);
    return value == NA_INTEGER || value <= 0 ? 0 : (R_xlen_t) value;
  }
  if (TYPEOF(matches) == REALSXP) {
    const double value = REAL_ELT(matches, index);
    if (!R_FINITE(value) || value <= 0.0 ||
        value > (double) R_XLEN_T_MAX) {
      return 0;
    }
    return (R_xlen_t) value;
  }
  Rf_error("Internal error: unexpected collection values translation match");
}

static int validate_supported_ids(SEXP ids, R_xlen_t size,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t row = 0; row < size; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    if (!supported_name(STRING_ELT(ids, row))) {
      return FALSE;
    }
  }
  return TRUE;
}

static int built_in_domain_row(SEXP class_name, SEXP storage_type) {
  return
    (paradox_domain_string_is(class_name, "ParamDbl") &&
      paradox_domain_string_is(storage_type, "numeric")) ||
    (paradox_domain_string_is(class_name, "ParamInt") &&
      paradox_domain_string_is(storage_type, "integer")) ||
    (paradox_domain_string_is(class_name, "ParamFct") &&
      paradox_domain_string_is(storage_type, "character")) ||
    (paradox_domain_string_is(class_name, "ParamLgl") &&
      paradox_domain_string_is(storage_type, "logical")) ||
    (paradox_domain_string_is(class_name, "ParamUty") &&
      paradox_domain_string_is(storage_type, "list"));
}

static int load_values_private_state(SEXP private_environment,
    values_private_state_t *state, SEXP roots, R_xlen_t roots_offset,
    R_xlen_t *work_since_interrupt) {
  state->params_sexp = paradox_domain_local_value(
    private_environment,
    ".params"
  );
  if (state->params_sexp != R_UnboundValue) {
    SET_VECTOR_ELT(
      roots,
      roots_offset + VALUES_ROOT_PARAMS,
      state->params_sexp
    );
  }
  state->values_sexp = paradox_domain_local_value(
    private_environment,
    ".values"
  );
  if (state->values_sexp != R_UnboundValue) {
    SET_VECTOR_ELT(
      roots,
      roots_offset + VALUES_ROOT_VALUES,
      state->values_sexp
    );
  }

  static const char *const column_names[] = {
    "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
    "levels", "special_vals", "default", "storage_type"
  };
  static const char *const table_classes[] = {"data.table", "data.frame"};
  static const SEXPTYPE column_types[PARADOX_DOMAIN_TAGS] = {
    STRSXP, STRSXP, STRSXP, VECSXP, REALSXP, REALSXP, REALSXP, VECSXP,
    VECSXP, VECSXP, STRSXP
  };
  SEXP params = state->params_sexp;
  SEXP values = state->values_sexp;
  if (params == R_UnboundValue || values == R_UnboundValue ||
      TYPEOF(params) != VECSXP || ALTREP(params)) {
    return FALSE;
  }
  const R_xlen_t column_count = XLENGTH(params);
  SEXP params_names = Rf_getAttrib(params, R_NamesSymbol);
  SEXP params_classes = Rf_getAttrib(params, R_ClassSymbol);
  /* R's public getter expands compact data-frame row names to a fresh ALTREP
   * object.  Unlike an ordinary attribute value, that expansion is not kept
   * alive through `params`, so protect it across the allocating attribute
   * lookups below until the root carrier owns it. */
  SEXP row_names = PROTECT(Rf_getAttrib(params, R_RowNamesSymbol));
  SEXP params_index = Rf_getAttrib(params, Rf_install("index"));
  SEXP params_selfref = Rf_getAttrib(
    params,
    Rf_install(".internal.selfref")
  );
  PARADOX_TEST_GC_ROW_NAMES_BARRIER(
    row_names,
    PARADOX_TEST_GC_ROW_NAMES_COLLECTION_LOCAL
  );
  SET_VECTOR_ELT(
    roots,
    roots_offset + VALUES_ROOT_PARAM_NAMES,
    params_names
  );
  SET_VECTOR_ELT(
    roots,
    roots_offset + VALUES_ROOT_PARAM_CLASS_ATTRIBUTE,
    params_classes
  );
  SET_VECTOR_ELT(
    roots,
    roots_offset + VALUES_ROOT_PARAM_ROW_NAMES,
    row_names
  );
  UNPROTECT(1);
  PARADOX_TEST_GC_ROW_NAMES_BARRIER(
    row_names,
    PARADOX_TEST_GC_ROW_NAMES_COLLECTION_CARRIER
  );
  SET_VECTOR_ELT(
    roots,
    roots_offset + VALUES_ROOT_PARAM_SELFREF,
    params_selfref
  );
  if (column_count != PARADOX_DOMAIN_TAGS ||
      TYPEOF(params_names) != STRSXP || ALTREP(params_names) ||
      TYPEOF(params_classes) != STRSXP || ALTREP(params_classes) ||
      !paradox_api_has_no_attributes(params_names) ||
      !paradox_api_has_no_attributes(params_classes) ||
      TYPEOF(row_names) != INTSXP ||
      !paradox_api_has_no_attributes(row_names) ||
      TYPEOF(params_selfref) != EXTPTRSXP ||
      !paradox_params_supported_table_attributes(params, FALSE) ||
      !paradox_domain_exact_string_vector(
        params_names,
        column_names,
        PARADOX_DOMAIN_TAGS,
        work_since_interrupt
      ) || !paradox_domain_exact_string_vector(
        params_classes,
        table_classes,
        2,
        work_since_interrupt
      ) || !exact_or_invalidated_params_index(
        params_index,
        roots,
        roots_offset + VALUES_ROOT_PARAM_INDEX,
        roots_offset + VALUES_ROOT_PARAM_INDEX_CACHE
      )) {
    return FALSE;
  }

  for (enum paradox_domain_column column = PARADOX_DOMAIN_ID;
      column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    SEXP value = VECTOR_ELT(params, column);
    state->params_columns[column] = value;
    SET_VECTOR_ELT(
      roots,
      roots_offset + (int) VALUES_ROOT_PARAM_COLUMNS + (int) column,
      value
    );
    const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
    const int numeric = column == PARADOX_DOMAIN_LOWER ||
      column == PARADOX_DOMAIN_UPPER ||
      column == PARADOX_DOMAIN_TOLERANCE;
    if (ALTREP(value) || (numeric
          ? type != REALSXP && type != INTSXP
          : type != column_types[column]) ||
        !paradox_api_has_no_attributes(value)) {
      return FALSE;
    }
  }
  SEXP ids = state->params_columns[PARADOX_DOMAIN_ID];
  SEXP classes = state->params_columns[PARADOX_DOMAIN_CLS];
  SEXP storage_types = state->params_columns[PARADOX_DOMAIN_STORAGE_TYPE];
  const R_xlen_t row_count = XLENGTH(ids);
  for (enum paradox_domain_column column = PARADOX_DOMAIN_CLS;
      column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    if (XLENGTH(state->params_columns[column]) != row_count) {
      return FALSE;
    }
  }
  /* Rf_getAttrib() expands compact data-frame row names to an ALTREP. Values
   * aggregation neither consumes nor exports row names, so the safe public-API
   * contract is to root and type-check that attribute without invoking its
   * representation-dependent Length/Elt methods. All eleven ordinary columns
   * independently establish the exact row count above. */
  if (row_count == R_XLEN_T_MAX || row_count > INT_MAX ||
      !validate_supported_ids(ids, row_count, work_since_interrupt)) {
    return FALSE;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    if (!built_in_domain_row(
        STRING_ELT(classes, row),
        STRING_ELT(storage_types, row)
      )) {
      return FALSE;
    }
  }
  if (TYPEOF(values) != VECSXP || ALTREP(values) || Rf_isObject(values) ||
      !paradox_params_names_are_only_attribute(values)) {
    return FALSE;
  }
  const R_xlen_t value_count = XLENGTH(values);
  SEXP value_names = Rf_getAttrib(values, R_NamesSymbol);
  if (TYPEOF(value_names) != STRSXP || ALTREP(value_names) ||
      !paradox_api_has_no_attributes(value_names)) {
    return FALSE;
  }
  SET_VECTOR_ELT(
    roots,
    roots_offset + VALUES_ROOT_VALUE_NAMES,
    value_names
  );
  const R_xlen_t value_name_count = XLENGTH(value_names);
  if (value_name_count != value_count ||
      !validate_supported_ids(
        value_names,
        value_name_count,
        work_since_interrupt
      )) {
    return FALSE;
  }

  /* Values may be arbitrary, otherwise-unrooted objects. Freeze the exact
   * element pointers into a private snapshot before any later graph growth or
   * matching allocation, then decline if that allocation changed a live
   * binding or one of the structural children admitted above. */
  SEXP value_snapshot = PROTECT(Rf_allocVector(VECSXP, value_count));
  int unchanged = paradox_domain_local_value(
      private_environment,
      ".params"
    ) == params && paradox_domain_local_value(
      private_environment,
      ".values"
    ) == values && Rf_getAttrib(params, R_NamesSymbol) == params_names &&
    Rf_getAttrib(params, R_ClassSymbol) == params_classes &&
    Rf_getAttrib(params, Rf_install("index")) == params_index &&
    Rf_getAttrib(params, Rf_install(".internal.selfref")) ==
      params_selfref && Rf_getAttrib(values, R_NamesSymbol) == value_names &&
    XLENGTH(values) == value_count;
  for (enum paradox_domain_column column = PARADOX_DOMAIN_ID;
      unchanged && column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    unchanged = VECTOR_ELT(params, column) == state->params_columns[column];
  }
  if (!unchanged) {
    UNPROTECT(1);
    return FALSE;
  }
  for (R_xlen_t value = 0; value < value_count; ++value) {
    SEXP element = VECTOR_ELT(values, value);
    if (element == R_UnboundValue || element == R_MissingArg ||
        TYPEOF(element) == PROMSXP) {
      UNPROTECT(1);
      return FALSE;
    }
    SET_VECTOR_ELT(value_snapshot, value, element);
  }
  SET_VECTOR_ELT(
    roots,
    roots_offset + VALUES_ROOT_VALUE_SNAPSHOT,
    value_snapshot
  );
  UNPROTECT(1);
  if (Rf_any_duplicated(ids, FALSE) != 0) {
    return FALSE;
  }
  state->params = (paradox_domain_params_t) {
    params,
    ids,
    classes,
    row_count
  };
  state->values = (paradox_domain_values_t) {
    value_snapshot,
    value_names,
    value_count
  };
  return TRUE;
}

static void reserve_frame_roots(values_root_plan_t *root_plan,
    PROTECT_INDEX root_plan_index, R_xlen_t frame_count,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t slots_per_frame = (R_xlen_t) VALUES_ROOT_COUNT;
  if (root_plan->used > root_plan->capacity || frame_count < 0 ||
      frame_count > (R_XLEN_T_MAX - root_plan->used) / slots_per_frame) {
    Rf_error("Unable to grow collection values root plan");
  }
  const R_xlen_t required = root_plan->used +
    frame_count * slots_per_frame;
  if (required > root_plan->capacity) {
    R_xlen_t expanded_capacity = root_plan->capacity;
    while (expanded_capacity < required) {
      if (expanded_capacity > R_XLEN_T_MAX / 2) {
        expanded_capacity = required;
        break;
      }
      expanded_capacity *= 2;
    }
    SEXP expanded = PROTECT(Rf_allocVector(VECSXP, expanded_capacity));
    for (R_xlen_t index = 0; index < root_plan->used; ++index) {
      paradox_domain_account_work(work_since_interrupt);
      SET_VECTOR_ELT(
        expanded,
        index,
        VECTOR_ELT(root_plan->roots, index)
      );
    }
    REPROTECT(expanded, root_plan_index);
    root_plan->roots = expanded;
    root_plan->capacity = expanded_capacity;
    UNPROTECT(1);
  }
}

static R_xlen_t append_frame_roots(values_root_plan_t *root_plan,
    PROTECT_INDEX root_plan_index, SEXP self, SEXP private_environment,
    R_xlen_t *work_since_interrupt) {
  reserve_frame_roots(
    root_plan,
    root_plan_index,
    1,
    work_since_interrupt
  );
  const R_xlen_t offset = root_plan->used;
  root_plan->used += VALUES_ROOT_COUNT;
  SET_VECTOR_ELT(root_plan->roots, offset + VALUES_ROOT_SELF, self);
  SET_VECTOR_ELT(
    root_plan->roots,
    offset + VALUES_ROOT_PRIVATE,
    private_environment
  );
  return offset;
}

static int initialize_frame(SEXP self, SEXP private_environment,
    SEXP namespace_environment, R_xlen_t root_start, values_frame_t *frame,
    values_root_plan_t *root_plan, PROTECT_INDEX root_plan_index,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t roots_offset = append_frame_roots(
    root_plan,
    root_plan_index,
    self,
    private_environment,
    work_since_interrupt
  );
  frame->self = self;
  frame->private_environment = private_environment;
  frame->kind = exact_node_kind(self, work_since_interrupt);
  frame->sets = R_NilValue;
  frame->set_names = R_NilValue;
  frame->child_count = 0;
  frame->postfix = R_NilValue;
  frame->translation_by_param = NULL;
  frame->root_start = root_start;
  frame->next_child = 0;
  frame->consumed_rows = 0;
  if (frame->kind == VALUES_NODE_UNKNOWN ||
      !canonical_public_getter(
        self,
        private_environment,
        namespace_environment
      ) || !canonical_private_getter(
        self,
        private_environment,
        namespace_environment,
        frame->kind
      ) || !load_values_private_state(
        private_environment,
        &frame->params_state,
        root_plan->roots,
        roots_offset,
        work_since_interrupt
      )) {
    return FALSE;
  }
  if (frame->kind == VALUES_NODE_SET) {
    return TRUE;
  }

  frame->sets = paradox_domain_local_value(private_environment, ".sets");
  if (frame->sets != R_UnboundValue) {
    SET_VECTOR_ELT(
      root_plan->roots,
      roots_offset + VALUES_ROOT_SETS,
      frame->sets
    );
  }
  SEXP translation = paradox_domain_local_value(
    private_environment,
    ".translation"
  );
  if (translation != R_UnboundValue) {
    SET_VECTOR_ELT(
      root_plan->roots,
      roots_offset + VALUES_ROOT_TRANSLATION,
      translation
    );
  }
  frame->postfix = paradox_domain_local_value(private_environment, ".postfix");
  if (frame->postfix != R_UnboundValue) {
    SET_VECTOR_ELT(
      root_plan->roots,
      roots_offset + VALUES_ROOT_POSTFIX,
      frame->postfix
    );
  }
  if (frame->sets == R_UnboundValue || translation == R_UnboundValue ||
      frame->postfix == R_UnboundValue || !validate_set_names(
        frame->sets,
        &frame->set_names,
        &frame->child_count,
        root_plan->roots,
        roots_offset,
        work_since_interrupt
      ) || TYPEOF(frame->postfix) != LGLSXP || ALTREP(frame->postfix) ||
      !paradox_api_has_no_attributes(frame->postfix) ||
      XLENGTH(frame->postfix) != 1 ||
      LOGICAL_ELT(frame->postfix, 0) == NA_LOGICAL ||
      frame->params_state.values.size != 0 || !exact_translation_table(
        translation,
        &frame->translation,
        root_plan->roots,
        roots_offset,
        work_since_interrupt
      ) || frame->translation.row_count !=
        frame->params_state.params.row_count) {
    return FALSE;
  }

  const R_xlen_t child_count = frame->child_count;
  if (child_count > INT_MAX) {
    return FALSE;
  }
  for (R_xlen_t row = 0; row < frame->translation.row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    const int owner = INTEGER_ELT(frame->translation.owner_indices, row);
    if (owner <= 0 || (R_xlen_t) owner > child_count ||
        !paradox_domain_strings_equal(
          STRING_ELT(frame->translation.owner_names, row),
          STRING_ELT(frame->set_names, (R_xlen_t) owner - 1)
        )) {
      return FALSE;
    }
  }

  const R_xlen_t parameter_count = frame->params_state.params.row_count;
  frame->translation_by_param = paradox_temporary_alloc(
    parameter_count,
    sizeof(*frame->translation_by_param)
  );

  /* Construction normally keeps `.params` and the keyed translation in the
   * same order and reuses their CHARSXP ids. Prove that common case in one
   * linear pass before using either the small unordered scan or R's allocating
   * general match. Cloned, re-encoded, or otherwise non-canonical strings
   * retain the exact historical matching semantics through those fallbacks. */
  int pointer_match = frame->translation.row_count == parameter_count;
  for (R_xlen_t row = 0; pointer_match && row < parameter_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP id = STRING_ELT(frame->params_state.params.ids, row);
    if (STRING_ELT(frame->translation.ids, row) != id) {
      pointer_match = FALSE;
    } else {
      frame->translation_by_param[row] = row;
    }
  }
  if (!pointer_match && parameter_count <= 16) {
    pointer_match = TRUE;
    for (R_xlen_t row = 0; pointer_match && row < parameter_count; ++row) {
      paradox_domain_account_work(work_since_interrupt);
      SEXP id = STRING_ELT(frame->params_state.params.ids, row);
      R_xlen_t translation_row = 0;
      while (translation_row < frame->translation.row_count &&
          STRING_ELT(frame->translation.ids, translation_row) != id) {
        paradox_domain_account_work(work_since_interrupt);
        ++translation_row;
      }
      if (translation_row == frame->translation.row_count) {
        pointer_match = FALSE;
      } else {
        frame->translation_by_param[row] = translation_row;
      }
    }
  }
  if (!pointer_match) {
    SEXP matches = PROTECT(Rf_match(
      frame->translation.ids,
      frame->params_state.params.ids,
      0
    ));
    if ((TYPEOF(matches) != INTSXP && TYPEOF(matches) != REALSXP) ||
        ALTREP(matches) || XLENGTH(matches) != parameter_count) {
      UNPROTECT(1);
      Rf_error(
        "Internal error: unexpected collection values translation match"
      );
    }
    for (R_xlen_t row = 0; row < parameter_count; ++row) {
      paradox_domain_account_work(work_since_interrupt);
      const R_xlen_t position = match_position(matches, row);
      if (position == 0 || position > frame->translation.row_count) {
        UNPROTECT(1);
        return FALSE;
      }
      frame->translation_by_param[row] = position - 1;
    }
    UNPROTECT(1);
  }
  /* Every immediate child will eventually receive one authentication frame.
   * Reserve those root slots together while retaining the depth-first walk:
   * child environments and their live values are still observed only when
   * traversal reaches them. */
  reserve_frame_roots(
    root_plan,
    root_plan_index,
    child_count,
    work_since_interrupt
  );
  return TRUE;
}

static int translated_component(SEXP value, char **copy, size_t *size) {
  const int input_size = Rf_length(value);
  if (input_size < 0 || (size_t) input_size > (SIZE_MAX - 1) / 4) {
    return FALSE;
  }
  const size_t capacity = (size_t) input_size * 4 + 1;
  if (capacity > (size_t) R_XLEN_T_MAX) {
    return FALSE;
  }
  *copy = paradox_temporary_alloc((R_xlen_t) capacity, sizeof(**copy));
  const char *translated = Rf_translateCharUTF8(value);
  *size = strlen(translated);
  if (*size >= capacity) {
    return FALSE;
  }
  memcpy(*copy, translated, *size + 1);
  return TRUE;
}

static int affixed_id_equal(SEXP outer, SEXP owner, SEXP inner, int postfix) {
  if (!supported_name(outer) || !supported_name(owner) ||
      !supported_name(inner)) {
    return FALSE;
  }
  if (paradox_domain_string_is(owner, "")) {
    return paradox_domain_strings_equal(outer, inner);
  }

  PROTECT(outer);
  PROTECT(owner);
  PROTECT(inner);
  const void *vmax = vmaxget();
  char *outer_text;
  char *owner_text;
  char *inner_text;
  size_t outer_size;
  size_t owner_size;
  size_t inner_size;
  int equal = FALSE;
  if (!translated_component(outer, &outer_text, &outer_size) ||
      !translated_component(owner, &owner_text, &owner_size) ||
      !translated_component(inner, &inner_text, &inner_size) ||
      inner_size == SIZE_MAX ||
      owner_size > SIZE_MAX - inner_size - 1U ||
      outer_size != owner_size + inner_size + 1) {
    goto cleanup;
  }
  {
    if (postfix) {
      equal = memcmp(outer_text, inner_text, inner_size) == 0 &&
        outer_text[inner_size] == '.' &&
        memcmp(
          outer_text + inner_size + 1,
          owner_text,
          owner_size
        ) == 0;
    } else {
      equal = memcmp(outer_text, owner_text, owner_size) == 0 &&
        outer_text[owner_size] == '.' &&
        memcmp(
          outer_text + owner_size + 1,
          inner_text,
          inner_size
        ) == 0;
    }
  }

cleanup:
  vmaxset(vmax);
  UNPROTECT(3);
  return equal;
}

static int validate_parent_child(values_frame_t *parent,
    const values_frame_t *child, R_xlen_t child_index,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t child_rows = child->params_state.params.row_count;
  if (parent->consumed_rows > parent->params_state.params.row_count ||
      child_rows > parent->params_state.params.row_count -
        parent->consumed_rows) {
    return FALSE;
  }
  SEXP owner_name = STRING_ELT(parent->set_names, child_index);
  const int postfix = LOGICAL_ELT(parent->postfix, 0);
  for (R_xlen_t row = 0; row < child_rows; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t parent_row = parent->consumed_rows + row;
    const R_xlen_t translation_row =
      parent->translation_by_param[parent_row];
    SEXP parent_id = STRING_ELT(
      parent->params_state.params.ids,
      parent_row
    );
    SEXP child_id = STRING_ELT(child->params_state.params.ids, row);
    if (!affixed_id_equal(parent_id, owner_name, child_id, postfix) ||
        INTEGER_ELT(parent->translation.owner_indices, translation_row) !=
          (int) (child_index + 1) || !paradox_domain_strings_equal(
          STRING_ELT(parent->translation.ids, translation_row),
          parent_id
        ) || !paradox_domain_strings_equal(
          STRING_ELT(parent->translation.original_ids, translation_row),
          child_id
        ) || !paradox_domain_strings_equal(
          STRING_ELT(parent->translation.owner_names, translation_row),
          owner_name
        )) {
      return FALSE;
    }
  }
  parent->consumed_rows += child_rows;
  return TRUE;
}

static int append_leaf(const values_frame_t *frame, values_leaf_t **leaves,
    R_xlen_t *leaf_count, R_xlen_t *leaf_capacity,
    R_xlen_t *output_size, R_xlen_t *work_since_interrupt) {
  const R_xlen_t value_count = frame->params_state.values.size;
  R_xlen_t *parameter_rows = paradox_temporary_alloc(
    value_count,
    sizeof(*parameter_rows)
  );
  SEXP ids = frame->params_state.params.ids;
  SEXP value_names = frame->params_state.values.names;
  R_xlen_t parameter_row = 0;
  for (R_xlen_t value = 0; value < value_count; ++value) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP element = VECTOR_ELT(frame->params_state.values.values, value);
    if (element == R_UnboundValue || element == R_MissingArg ||
        TYPEOF(element) == PROMSXP) {
      return FALSE;
    }
    while (parameter_row < frame->params_state.params.row_count &&
        !paradox_domain_strings_equal(
          STRING_ELT(ids, parameter_row),
          STRING_ELT(value_names, value)
        )) {
      paradox_domain_account_work(work_since_interrupt);
      ++parameter_row;
    }
    if (parameter_row == frame->params_state.params.row_count) {
      return FALSE;
    }
    parameter_rows[value] = parameter_row;
    ++parameter_row;
  }
  if (value_count > R_XLEN_T_MAX - *output_size) {
    return FALSE;
  }
  if (*leaf_count == *leaf_capacity) {
    if (*leaf_capacity > R_XLEN_T_MAX / 2) {
      return FALSE;
    }
    const R_xlen_t expanded_capacity = *leaf_capacity * 2;
    values_leaf_t *expanded = paradox_temporary_alloc(
      expanded_capacity,
      sizeof(*expanded)
    );
    memcpy(
      expanded,
      *leaves,
      (size_t) *leaf_count * sizeof(*expanded)
    );
    *leaves = expanded;
    *leaf_capacity = expanded_capacity;
  }
  (*leaves)[*leaf_count] = (values_leaf_t) {
    frame->params_state.values.values,
    frame->root_start,
    value_count,
    parameter_rows
  };
  ++*leaf_count;
  *output_size += value_count;
  return TRUE;
}

static int validate_graph(SEXP self, SEXP private_environment,
    SEXP namespace_environment, values_frame_t **frames_out,
    values_leaf_t **leaves_out, R_xlen_t *leaf_count_out,
    R_xlen_t *output_size_out, values_root_plan_t *root_plan,
    PROTECT_INDEX root_plan_index, R_xlen_t *work_since_interrupt) {
  R_xlen_t frame_capacity = 8;
  values_frame_t *frames = paradox_temporary_alloc(
    frame_capacity,
    sizeof(*frames)
  );
  if (owned_private_environment(self) != private_environment ||
      !initialize_frame(
        self,
        private_environment,
        namespace_environment,
        0,
        &frames[0],
        root_plan,
        root_plan_index,
        work_since_interrupt
      ) || frames[0].kind != VALUES_NODE_COLLECTION) {
    return FALSE;
  }

  R_xlen_t leaf_capacity = 8;
  R_xlen_t leaf_count = 0;
  R_xlen_t output_size = 0;
  values_leaf_t *leaves = paradox_temporary_alloc(
    leaf_capacity,
    sizeof(*leaves)
  );
  R_xlen_t depth = 1;
  while (depth != 0) {
    values_frame_t *parent = &frames[depth - 1];
    if (parent->kind == VALUES_NODE_SET ||
        parent->next_child == parent->child_count) {
      if (parent->kind == VALUES_NODE_COLLECTION &&
          parent->consumed_rows != parent->params_state.params.row_count) {
        return FALSE;
      }
      --depth;
      continue;
    }

    const R_xlen_t child_index = parent->next_child;
    SEXP child_self = PROTECT(VECTOR_ELT(parent->sets, child_index));
    for (R_xlen_t ancestor = 0; ancestor < depth; ++ancestor) {
      paradox_domain_account_work(work_since_interrupt);
      if (frames[ancestor].self == child_self) {
        Rf_error("Cyclic ParamSetCollection values graph is unsupported");
      }
    }
    SEXP child_private = PROTECT(owned_private_environment(child_self));
    if (child_private == R_UnboundValue ||
        parent->root_start > R_XLEN_T_MAX - parent->consumed_rows) {
      UNPROTECT(2);
      return FALSE;
    }
    const R_xlen_t child_root_start = parent->root_start +
      parent->consumed_rows;

    if (depth == frame_capacity) {
      if (frame_capacity > R_XLEN_T_MAX / 2) {
        UNPROTECT(2);
        return FALSE;
      }
      const R_xlen_t expanded_capacity = frame_capacity * 2;
      values_frame_t *expanded = paradox_temporary_alloc(
        expanded_capacity,
        sizeof(*expanded)
      );
      memcpy(expanded, frames, (size_t) depth * sizeof(*expanded));
      frames = expanded;
      frame_capacity = expanded_capacity;
      parent = &frames[depth - 1];
    }
    if (!initialize_frame(
        child_self,
        child_private,
        namespace_environment,
        child_root_start,
        &frames[depth],
        root_plan,
        root_plan_index,
        work_since_interrupt
      ) || !validate_parent_child(
        parent,
        &frames[depth],
        child_index,
        work_since_interrupt
      )) {
      UNPROTECT(2);
      return FALSE;
    }
    if (frames[depth].kind == VALUES_NODE_SET && !append_leaf(
        &frames[depth],
        &leaves,
        &leaf_count,
        &leaf_capacity,
        &output_size,
        work_since_interrupt
      )) {
      UNPROTECT(2);
      return FALSE;
    }
    UNPROTECT(2);
    ++parent->next_child;
    ++depth;
  }
  if (output_size > frames[0].params_state.params.row_count) {
    return FALSE;
  }
  *frames_out = frames;
  *leaves_out = leaves;
  *leaf_count_out = leaf_count;
  *output_size_out = output_size;
  return TRUE;
}

SEXP paradox_param_set_collection_values(SEXP private_environment, SEXP self) {
  R_xlen_t work_since_interrupt = 0;
  values_frame_t *frames;
  values_leaf_t *leaves;
  R_xlen_t leaf_count;
  R_xlen_t output_size;
  SEXP namespace_environment = paradox_api_registered_namespace("paradox");
  if (TYPEOF(namespace_environment) != ENVSXP) {
    return R_NilValue;
  }
  PROTECT_INDEX root_plan_index;
  values_root_plan_t root_plan = {
    R_NilValue,
    0,
    /* Root plus three leaves covers the common small collection without a
     * resize.  Larger and nested graphs already grow geometrically, so
     * reserving sixteen complete authentication frames on every read only
     * inflated the fixed cost of the hottest small-collection path. */
    4 * VALUES_ROOT_COUNT
  };
  PROTECT_WITH_INDEX(
    root_plan.roots = Rf_allocVector(VECSXP, root_plan.capacity),
    &root_plan_index
  );
  if (!validate_graph(
      self,
      private_environment,
      namespace_environment,
      &frames,
      &leaves,
      &leaf_count,
      &output_size,
      &root_plan,
      root_plan_index,
      &work_since_interrupt
    )) {
    UNPROTECT(1);
    return R_NilValue;
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, output_size));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, output_size));
  SEXP root_ids = frames[0].params_state.params.ids;
  const R_xlen_t root_rows = frames[0].params_state.params.row_count;
  R_xlen_t output = 0;
  for (R_xlen_t leaf_index = 0; leaf_index < leaf_count; ++leaf_index) {
    const values_leaf_t *leaf = &leaves[leaf_index];
    for (R_xlen_t value = 0; value < leaf->size; ++value) {
      paradox_domain_account_work(&work_since_interrupt);
      if (leaf->root_start >= root_rows ||
          leaf->parameter_rows[value] >= root_rows - leaf->root_start) {
        UNPROTECT(3);
        Rf_error("Internal error: invalid collection values output plan");
      }
      const R_xlen_t root_row = leaf->root_start +
        leaf->parameter_rows[value];
      if (output >= output_size) {
        UNPROTECT(3);
        Rf_error("Internal error: collection values output exceeded capacity");
      }
      SEXP output_id = STRING_ELT(root_ids, root_row);
      SEXP output_value = VECTOR_ELT(leaf->values, value);
      SET_VECTOR_ELT(result, output, output_value);
      SET_STRING_ELT(names, output, output_id);
      ++output;
    }
  }
  if (output != output_size) {
    UNPROTECT(3);
    Rf_error("Internal error: incomplete collection values output plan");
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(3);
  return result;
}
