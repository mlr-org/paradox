#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

typedef enum {
  GET_VALUES_NODE_UNKNOWN = 0,
  GET_VALUES_NODE_SET,
  GET_VALUES_NODE_COLLECTION
} get_values_node_kind_t;

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
  SEXP original_names;
  int values_detached;
} get_values_state_t;

enum get_values_root_slot {
  GET_VALUES_ROOT_SELF = 0,
  GET_VALUES_ROOT_PRIVATE,
  GET_VALUES_ROOT_PARAMS,
  GET_VALUES_ROOT_TAGS,
  GET_VALUES_ROOT_VALUES,
  GET_VALUES_ROOT_DEPENDENCIES,
  GET_VALUES_ROOT_ORIGINAL_NAMES,
  GET_VALUES_ROOT_PARAM_IDS,
  GET_VALUES_ROOT_PARAM_CLASSES,
  GET_VALUES_ROOT_TAG_IDS,
  GET_VALUES_ROOT_TAG_VALUES,
  GET_VALUES_ROOT_VALUE_LIST,
  GET_VALUES_ROOT_VALUE_NAMES,
  GET_VALUES_ROOT_DEPENDENCY_IDS,
  GET_VALUES_ROOT_DEPENDENCY_ON,
  GET_VALUES_ROOT_DEPENDENCY_CONDITIONS,
  GET_VALUES_ROOT_LIVE_PARAMS,
  GET_VALUES_ROOT_LIVE_TAGS,
  GET_VALUES_ROOT_COUNT
};

static int supported_string(SEXP value) {
  return value != NA_STRING && Rf_getCharCE(value) != CE_BYTES;
}

/* The general helper must translate differently encoded strings before it
 * compares them.  Dependency scans mostly compare the ordinary, same-encoded
 * parameter IDs created by one ParamSet; translating every non-match makes
 * that otherwise simple scan needlessly expensive.  Equal encodings give
 * CHAR() the same byte representation, while mixed encodings retain the full
 * semantic comparison. */
static int get_values_strings_equal(SEXP left, SEXP right) {
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

static get_values_node_kind_t exact_node_kind(SEXP self,
    R_xlen_t *work_since_interrupt) {
  static const char *const set_classes[] = {"ParamSet", "R6"};
  static const char *const collection_classes[] = {
    "ParamSetCollection", "ParamSet", "R6"
  };
  if (TYPEOF(self) != ENVSXP) {
    return GET_VALUES_NODE_UNKNOWN;
  }
  SEXP classes = PROTECT(Rf_getAttrib(self, R_ClassSymbol));
  if (paradox_domain_exact_string_vector(
      classes,
      set_classes,
      2,
      work_since_interrupt
    )) {
    UNPROTECT(1);
    return GET_VALUES_NODE_SET;
  }
  if (paradox_domain_exact_string_vector(
      classes,
      collection_classes,
      3,
      work_since_interrupt
    )) {
    UNPROTECT(1);
    return GET_VALUES_NODE_COLLECTION;
  }
  UNPROTECT(1);
  return GET_VALUES_NODE_UNKNOWN;
}

static int exact_wrapper_call(SEXP function, const char *target,
    const char *const *arguments, R_xlen_t argument_count) {
  SEXP body = paradox_api_closure_expression(function);
  if (TYPEOF(body) != LANGSXP || CAR(body) != Rf_install(target)) {
    return FALSE;
  }
  SEXP argument = CDR(body);
  for (R_xlen_t index = 0; index < argument_count; ++index) {
    SEXP symbol = Rf_install(arguments[index]);
    if (argument == R_NilValue || TAG(argument) != symbol ||
        CAR(argument) != symbol) {
      return FALSE;
    }
    argument = CDR(argument);
  }
  return argument == R_NilValue;
}

static int exact_missing_formal(SEXP formal, const char *name) {
  return formal != R_NilValue && TAG(formal) == Rf_install(name) &&
    CAR(formal) == R_MissingArg;
}

static int exact_nil_formal(SEXP formal, const char *name) {
  return formal != R_NilValue && TAG(formal) == Rf_install(name) &&
    CAR(formal) == R_NilValue;
}

static int exact_scalar_string(SEXP value, const char *expected) {
  return TYPEOF(value) == STRSXP && !ALTREP(value) && XLENGTH(value) == 1 &&
    paradox_domain_string_is(STRING_ELT(value, 0), expected);
}

static int exact_ids_formals(SEXP function) {
  static const char *const names[] = {"class", "tags", "any_tags"};
  SEXP formal = paradox_api_closure_formals(function);
  for (R_xlen_t index = 0; index < 3; ++index) {
    if (!exact_nil_formal(formal, names[index])) {
      return FALSE;
    }
    formal = CDR(formal);
  }
  return formal == R_NilValue;
}

static int exact_get_values_formals(SEXP function) {
  static const char *const filter_names[] = {"class", "tags", "any_tags"};
  SEXP formal = paradox_api_closure_formals(function);
  for (R_xlen_t index = 0; index < 3; ++index) {
    if (!exact_nil_formal(formal, filter_names[index])) {
      return FALSE;
    }
    formal = CDR(formal);
  }
  if (formal == R_NilValue || TAG(formal) != Rf_install("type") ||
      !exact_scalar_string(CAR(formal), "with_token")) {
    return FALSE;
  }
  formal = CDR(formal);
  if (formal == R_NilValue ||
      TAG(formal) != Rf_install("check_required") ||
      TYPEOF(CAR(formal)) != LGLSXP || ALTREP(CAR(formal)) ||
      XLENGTH(CAR(formal)) != 1 ||
      LOGICAL_ELT(CAR(formal), 0) != TRUE) {
    return FALSE;
  }
  formal = CDR(formal);
  return formal != R_NilValue &&
    TAG(formal) == Rf_install("remove_dependencies") &&
    TYPEOF(CAR(formal)) == LGLSXP && !ALTREP(CAR(formal)) &&
    XLENGTH(CAR(formal)) == 1 &&
    LOGICAL_ELT(CAR(formal), 0) == TRUE && CDR(formal) == R_NilValue;
}

static int exact_single_missing_formal(SEXP function, const char *name) {
  SEXP formal = paradox_api_closure_formals(function);
  return exact_missing_formal(formal, name) && CDR(formal) == R_NilValue;
}

static int canonical_wrapper_environment(SEXP function, SEXP self,
    SEXP private_environment, SEXP namespace_environment,
    const char *target) {
  SEXP environment = paradox_api_closure_environment(function);
  SEXP super_symbol = Rf_install("super");
  SEXP target_symbol = Rf_install(target);
  return TYPEOF(environment) == ENVSXP &&
    paradox_api_parent_environment(environment) == namespace_environment &&
    !R_existsVarInFrame(environment, super_symbol) &&
    !R_existsVarInFrame(environment, target_symbol) &&
    paradox_domain_local_value(environment, "self") == self &&
    paradox_domain_local_value(environment, "private") ==
      private_environment;
}

static int canonical_locked_method(SEXP self, SEXP private_environment,
    SEXP namespace_environment, const char *binding_name, const char *target,
    const char *const *arguments, R_xlen_t argument_count,
    int (*formals_are_exact)(SEXP)) {
  SEXP symbol = Rf_install(binding_name);
  if (!R_existsVarInFrame(self, symbol) || R_BindingIsActive(symbol, self) ||
      !R_BindingIsLocked(symbol, self)) {
    return FALSE;
  }
  SEXP function = PROTECT(paradox_domain_local_value(self, binding_name));
  const int exact = TYPEOF(function) == CLOSXP &&
    exact_wrapper_call(function, target, arguments, argument_count) &&
    formals_are_exact(function) && canonical_wrapper_environment(
      function,
      self,
      private_environment,
      namespace_environment,
      target
    );
  UNPROTECT(1);
  return exact;
}

static int canonical_active_binding(SEXP self, SEXP private_environment,
    SEXP namespace_environment, const char *binding_name, const char *target,
    const char *formal_name) {
  static const char *const arguments[] = {"self", "private", "super", NULL};
  SEXP symbol = Rf_install(binding_name);
  if (!R_existsVarInFrame(self, symbol) ||
      !R_BindingIsActive(symbol, self)) {
    return FALSE;
  }
  SEXP function = PROTECT(R_ActiveBindingFunction(symbol, self));
  if (TYPEOF(function) != CLOSXP) {
    UNPROTECT(1);
    return FALSE;
  }
  const char *call_arguments[4] = {
    arguments[0], arguments[1], arguments[2], formal_name
  };
  const int exact = exact_wrapper_call(
      function,
      target,
      call_arguments,
      4
    ) &&
    exact_single_missing_formal(function, formal_name) &&
    canonical_wrapper_environment(
      function,
      self,
      private_environment,
      namespace_environment,
      target
    );
  UNPROTECT(1);
  return exact;
}

static int canonical_private_getter(SEXP self, SEXP private_environment,
    SEXP namespace_environment) {
  static const char *const arguments[] = {"self", "private", "super"};
  SEXP symbol = Rf_install(".get_values");
  if (!R_existsVarInFrame(private_environment, symbol) ||
      R_BindingIsActive(symbol, private_environment) ||
      !R_BindingIsLocked(symbol, private_environment)) {
    return FALSE;
  }
  SEXP function = PROTECT(paradox_domain_local_value(
    private_environment,
    ".get_values"
  ));
  const int exact = TYPEOF(function) == CLOSXP &&
    exact_wrapper_call(
      function,
      ".__ParamSet__.get_values",
      arguments,
      3
    ) && paradox_api_closure_formals(function) == R_NilValue &&
    canonical_wrapper_environment(
      function,
      self,
      private_environment,
      namespace_environment,
      ".__ParamSet__.get_values"
    );
  UNPROTECT(1);
  return exact;
}

static int canonical_ids_method(SEXP self, SEXP private_environment,
    SEXP namespace_environment) {
  static const char *const arguments[] = {
    "self", "private", "super", "class", "tags", "any_tags"
  };
  return canonical_locked_method(
    self,
    private_environment,
    namespace_environment,
    "ids",
    ".__ParamSet__ids",
    arguments,
    6,
    exact_ids_formals
  );
}

static int canonical_get_values_method(SEXP self, SEXP private_environment,
    SEXP namespace_environment) {
  static const char *const arguments[] = {
    "self", "private", "super", "class", "tags", "any_tags", "type",
    "check_required", "remove_dependencies"
  };
  return canonical_locked_method(
    self,
    private_environment,
    namespace_environment,
    "get_values",
    ".__ParamSet__get_values",
    arguments,
    9,
    exact_get_values_formals
  );
}

static int values_follow_parameter_order(
    const paradox_domain_params_t *params,
    const paradox_domain_values_t *values,
    R_xlen_t *work_since_interrupt) {
  R_xlen_t parameter_row = 0;
  for (R_xlen_t value = 0; value < values->size; ++value) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP name = STRING_ELT(values->names, value);
    SEXP element = VECTOR_ELT(values->values, value);
    if (!supported_string(name) || element == R_UnboundValue ||
        element == R_MissingArg || TYPEOF(element) == PROMSXP) {
      return FALSE;
    }
    while (parameter_row < params->row_count &&
        !get_values_strings_equal(
          name,
          STRING_ELT(params->ids, parameter_row)
        )) {
      paradox_domain_account_work(work_since_interrupt);
      ++parameter_row;
    }
    if (parameter_row == params->row_count) {
      return FALSE;
    }
    ++parameter_row;
  }
  return TRUE;
}

static int dependencies_use_supported_names(
    const paradox_domain_dependencies_t *dependencies,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t row = 0; row < dependencies->row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    if (!supported_string(STRING_ELT(dependencies->ids, row)) ||
        !supported_string(STRING_ELT(dependencies->on, row))) {
      return FALSE;
    }
  }
  return TRUE;
}

/* get_values() only consumes the permanent parameter ID and class columns.
 * The shared validator must still authenticate the complete table shell,
 * every permanent column, unique IDs, and every built-in cls/storage pairing,
 * so custom Domain rows continue to use the R implementation. It need not
 * repeatedly validate every nested levels/special_vals payload: those are
 * neither inspected nor returned here. Selecting one existing row asks the
 * shared validator for exactly that narrower contract. */
static int validate_relevant_params(get_values_state_t *state, SEXP roots,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(state->params) != VECSXP || ALTREP(state->params) ||
      XLENGTH(state->params) == 0) {
    return FALSE;
  }
  SEXP ids = PROTECT(VECTOR_ELT(state->params, 0));
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_PARAM_IDS, ids);
  if (TYPEOF(ids) != STRSXP || ALTREP(ids)) {
    UNPROTECT(1);
    return FALSE;
  }
  const R_xlen_t row_count = XLENGTH(ids);
  if (row_count == 0) {
    R_xlen_t selected_row = 0;
    const int valid = paradox_domain_validate_params(
      state->params,
      R_NilValue,
      TRUE,
      &state->params_data,
      &selected_row,
      work_since_interrupt
    );
    UNPROTECT(1);
    return valid;
  }

  SEXP selected_id = PROTECT(Rf_ScalarString(STRING_ELT(ids, 0)));
  R_xlen_t selected_row = 0;
  const int valid = paradox_domain_validate_params(
    state->params,
    selected_id,
    FALSE,
    &state->params_data,
    &selected_row,
    work_since_interrupt
  );
  UNPROTECT(2);
  return valid;
}

static int validate_state(get_values_state_t *state, SEXP roots,
    R_xlen_t *work_since_interrupt) {
  if (!validate_relevant_params(state, roots, work_since_interrupt)) {
    return FALSE;
  }
  SET_VECTOR_ELT(
    roots,
    GET_VALUES_ROOT_PARAM_IDS,
    state->params_data.ids
  );
  SET_VECTOR_ELT(
    roots,
    GET_VALUES_ROOT_PARAM_CLASSES,
    state->params_data.classes
  );

  if (!paradox_domain_validate_tags(
      state->tags,
      &state->tags_data,
      work_since_interrupt
    )) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_TAG_IDS, state->tags_data.ids);
  SET_VECTOR_ELT(
    roots,
    GET_VALUES_ROOT_TAG_VALUES,
    state->tags_data.values
  );
  if (!paradox_domain_validate_values(
      state->values,
      &state->values_data,
      work_since_interrupt
    )) {
    return FALSE;
  }
  SET_VECTOR_ELT(
    roots,
    GET_VALUES_ROOT_VALUE_LIST,
    state->values_data.values
  );
  SET_VECTOR_ELT(
    roots,
    GET_VALUES_ROOT_VALUE_NAMES,
    state->values_data.names
  );
  if (!values_follow_parameter_order(
      &state->params_data,
      &state->values_data,
      work_since_interrupt
    )) {
    return FALSE;
  }

  if (!paradox_domain_validate_dependencies(
      state->dependencies,
      &state->dependencies_data,
      work_since_interrupt
    )) {
    return FALSE;
  }
  SET_VECTOR_ELT(
    roots,
    GET_VALUES_ROOT_DEPENDENCY_IDS,
    state->dependencies_data.ids
  );
  SET_VECTOR_ELT(
    roots,
    GET_VALUES_ROOT_DEPENDENCY_ON,
    state->dependencies_data.on
  );
  SET_VECTOR_ELT(
    roots,
    GET_VALUES_ROOT_DEPENDENCY_CONDITIONS,
    state->dependencies_data.conditions
  );
  return dependencies_use_supported_names(
      &state->dependencies_data,
      work_since_interrupt
    );
}

static int load_static_tables(SEXP private_environment,
    get_values_state_t *state, SEXP roots) {
  state->params = paradox_domain_local_value(private_environment, ".params");
  if (state->params == R_UnboundValue) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_PARAMS, state->params);
  state->tags = paradox_domain_local_value(private_environment, ".tags");
  if (state->tags == R_UnboundValue) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_TAGS, state->tags);
  return TRUE;
}

static int load_param_set_state(SEXP private_environment,
    get_values_state_t *state, SEXP roots) {
  state->values = paradox_domain_local_value(private_environment, ".values");
  if (state->values == R_UnboundValue) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_VALUES, state->values);
  state->dependencies = paradox_domain_local_value(
    private_environment,
    ".deps"
  );
  if (state->dependencies == R_UnboundValue) {
    return FALSE;
  }
  SET_VECTOR_ELT(
    roots,
    GET_VALUES_ROOT_DEPENDENCIES,
    state->dependencies
  );
  return TRUE;
}

static int load_collection_state(SEXP private_environment, SEXP self,
    get_values_state_t *state, SEXP roots) {
  SEXP values = PROTECT(paradox_param_set_collection_values(
    private_environment,
    self
  ));
  if (values == R_NilValue) {
    UNPROTECT(1);
    return FALSE;
  }
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_VALUES, values);
  state->values = values;
  UNPROTECT(1);

  SEXP dependencies = PROTECT(paradox_param_set_collection_deps(
    private_environment,
    self
  ));
  if (dependencies == R_NilValue) {
    UNPROTECT(1);
    return FALSE;
  }
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_DEPENDENCIES, dependencies);
  state->dependencies = dependencies;
  UNPROTECT(1);
  return TRUE;
}

static int preflight(SEXP private_environment, SEXP self, SEXP frame,
    SEXP namespace_environment, get_values_state_t *state, SEXP roots,
    R_xlen_t *work_since_interrupt) {
  const get_values_node_kind_t kind = exact_node_kind(
    self,
    work_since_interrupt
  );
  if (kind == GET_VALUES_NODE_UNKNOWN || TYPEOF(frame) != ENVSXP ||
      paradox_api_parent_environment(frame) != namespace_environment ||
      !paradox_domain_owns_private_environment(self, private_environment) ||
      !canonical_get_values_method(
        self,
        private_environment,
        namespace_environment
      ) || !canonical_ids_method(
        self,
        private_environment,
        namespace_environment
      ) || !load_static_tables(private_environment, state, roots)) {
    return FALSE;
  }

  if (kind == GET_VALUES_NODE_SET) {
    if (!canonical_active_binding(
        self,
        private_environment,
        namespace_environment,
        "values",
        ".__ParamSet__values",
        "xs"
      ) || !canonical_active_binding(
        self,
        private_environment,
        namespace_environment,
        "deps",
        ".__ParamSet__deps",
        "v"
      ) || !canonical_private_getter(
        self,
        private_environment,
        namespace_environment
      ) || !load_param_set_state(private_environment, state, roots)) {
      return FALSE;
    }
  } else if (!load_collection_state(
      private_environment,
      self,
      state,
      roots
    )) {
    return FALSE;
  }
  if (!validate_state(state, roots, work_since_interrupt)) {
    return FALSE;
  }
  /* `names(values)` is a distinct historical snapshot. A dependency
   * callback may replace the list's names attribute and trigger a collection,
   * so the original vector needs its own root rather than relying on the
   * attribute link to remain intact. */
  SET_VECTOR_ELT(
    roots,
    GET_VALUES_ROOT_ORIGINAL_NAMES,
    state->values_data.names
  );
  state->original_names = state->values_data.names;
  state->values_detached = FALSE;
  return TRUE;
}

static int parse_type(SEXP value, get_values_type_t *type) {
  if (TYPEOF(value) != STRSXP || XLENGTH(value) != 1 ||
      STRING_ELT(value, 0) == NA_STRING) {
    return FALSE;
  }
  SEXP string = STRING_ELT(value, 0);
  if (paradox_domain_string_is(string, "with_token")) {
    *type = GET_VALUES_WITH_TOKEN;
    return TRUE;
  }
  if (paradox_domain_string_is(string, "without_token")) {
    *type = GET_VALUES_WITHOUT_TOKEN;
    return TRUE;
  }
  if (paradox_domain_string_is(string, "only_token")) {
    *type = GET_VALUES_ONLY_TOKEN;
    return TRUE;
  }
  if (paradox_domain_string_is(string, "with_internal")) {
    *type = GET_VALUES_WITH_INTERNAL;
    return TRUE;
  }
  return FALSE;
}

static SEXP dependency_removal_rows(SEXP frame, SEXP dependencies) {
  SEXP nrow_call = PROTECT(Rf_lang2(
    Rf_install("nrow"),
    dependencies
  ));
  SEXP condition = PROTECT(Rf_lang3(
    Rf_install("&&"),
    Rf_install("remove_dependencies"),
    nrow_call
  ));
  SEXP yes = PROTECT(Rf_ScalarLogical(TRUE));
  SEXP no = PROTECT(Rf_ScalarLogical(FALSE));
  SEXP branch = PROTECT(Rf_lang4(Rf_install("if"), condition, yes, no));
  SEXP result = PROTECT(Rf_eval(branch, frame));
  const int requested = LOGICAL_ELT(result, 0) == TRUE;
  if (!requested) {
    UNPROTECT(6);
    return R_NilValue;
  }

  /* `seq_row(deps)` is a second live nrow observation in the historical
   * body. Preserve it rather than reusing the value from the `if` test. */
  SEXP sequence_call = PROTECT(Rf_lang2(
    Rf_install("seq_row"),
    dependencies
  ));
  SEXP rows = PROTECT(Rf_eval(sequence_call, frame));
  if (TYPEOF(rows) != INTSXP && TYPEOF(rows) != REALSXP) {
    UNPROTECT(8);
    Rf_error("ParamSet dependency row sequence changed during native get_values()");
  }
  UNPROTECT(8);
  return rows;
}

static R_xlen_t dependency_row_at(SEXP rows, R_xlen_t position) {
  if (TYPEOF(rows) == INTSXP) {
    const int row = INTEGER_ELT(rows, position);
    if (row == NA_INTEGER || row <= 0) {
      Rf_error("ParamSet dependency row sequence changed during native get_values()");
    }
    return (R_xlen_t) row - 1;
  }
  const double row = REAL_ELT(rows, position);
  if (!R_FINITE(row) || row < 1.0 ||
      row > (double) R_XLEN_T_MAX) {
    Rf_error("ParamSet dependency row sequence changed during native get_values()");
  }
  const R_xlen_t converted = (R_xlen_t) row;
  if ((double) converted != row) {
    Rf_error("ParamSet dependency row sequence changed during native get_values()");
  }
  return converted - 1;
}

static R_xlen_t find_name(SEXP names, const int *kept, SEXP target,
    int require_kept, R_xlen_t *work_since_interrupt) {
  const R_xlen_t size = XLENGTH(names);
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if ((!require_kept || kept[index]) && get_values_strings_equal(
        STRING_ELT(names, index),
        target
      )) {
      return index;
    }
  }
  return R_XLEN_T_MAX;
}

static R_xlen_t match_position_at(SEXP matches, SEXPTYPE match_type,
    R_xlen_t index, R_xlen_t table_size) {
  if (match_type == INTSXP) {
    const int matched = INTEGER_ELT(matches, index);
    if (matched == 0) {
      return R_XLEN_T_MAX;
    }
    if (matched == NA_INTEGER || matched < 0 ||
        (R_xlen_t) matched > table_size) {
      Rf_error("Internal error: invalid result from R's matching primitive");
    }
    return (R_xlen_t) matched - 1;
  }

  const double matched = REAL_ELT(matches, index);
  if (matched == 0.0) {
    return R_XLEN_T_MAX;
  }
  if (!R_FINITE(matched) || matched < 0.0 ||
      matched > (double) R_XLEN_T_MAX) {
    Rf_error("Internal error: invalid result from R's matching primitive");
  }
  const R_xlen_t converted = (R_xlen_t) matched;
  if ((double) converted != matched || converted > table_size) {
    Rf_error("Internal error: invalid result from R's matching primitive");
  }
  return converted - 1;
}

static int result_is_true(SEXP value) {
  return TYPEOF(value) == LGLSXP && XLENGTH(value) == 1 &&
    LOGICAL_ELT(value, 0) == TRUE;
}

static SEXP current_value_names(const get_values_state_t *state) {
  SEXP names = PROTECT(Rf_getAttrib(
    state->values_data.values,
    R_NamesSymbol
  ));
  if (TYPEOF(names) != STRSXP ||
      XLENGTH(names) != state->values_data.size) {
    UNPROTECT(1);
    Rf_error(
      "ParamSet values names changed to a malformed shape during "
      "native get_values()"
    );
  }
  UNPROTECT(1);
  return names;
}

/* A condition callback may install an ALTSTRING names attribute. Capture one
 * fixed-size observation before indexing the fixed-size `kept` workspace or
 * retaining the names on a detached list shell. */
static SEXP snapshot_current_value_names(const get_values_state_t *state,
    R_xlen_t *work_since_interrupt) {
  SEXP names = PROTECT(current_value_names(state));
  SEXP stable = PROTECT(Rf_allocVector(STRSXP, state->values_data.size));
  for (R_xlen_t index = 0; index < state->values_data.size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    SET_STRING_ELT(stable, index, STRING_ELT(names, index));
  }
  UNPROTECT(2);
  return stable;
}

static void detach_values_shell(get_values_state_t *state, SEXP roots,
    R_xlen_t *work_since_interrupt) {
  if (state->values_detached) {
    return;
  }
  SEXP values = PROTECT(Rf_shallow_duplicate(state->values_data.values));
  SEXP names = PROTECT(snapshot_current_value_names(
    state,
    work_since_interrupt
  ));
  Rf_setAttrib(values, R_NamesSymbol, names);
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_VALUES, values);
  state->values = values;
  state->values_data.values = values;
  state->values_data.names = names;
  state->values_detached = TRUE;
  UNPROTECT(2);
}

static void apply_dependencies(get_values_state_t *state, int *kept,
    SEXP rows, SEXP namespace_environment, SEXP roots,
    R_xlen_t *work_since_interrupt) {
  SEXP original_names = state->original_names;
  const R_xlen_t row_sequence_size = XLENGTH(rows);
  for (R_xlen_t position = 0; position < row_sequence_size; ++position) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t row = dependency_row_at(rows, position);
    /* The R loop snapshots the dependency table itself and its row sequence,
     * but `$` resolves each column anew on every row. Correctly shaped
     * in-place column replacement by an earlier callback is therefore live. */
    SEXP ids = PROTECT(paradox_get_named_column(
      state->dependencies,
      ".deps",
      "id"
    ));
    SEXP on_ids = PROTECT(paradox_get_named_column(
      state->dependencies,
      ".deps",
      "on"
    ));
    SEXP conditions = PROTECT(paradox_get_named_column(
      state->dependencies,
      ".deps",
      "cond"
    ));
    if (TYPEOF(ids) != STRSXP || TYPEOF(on_ids) != STRSXP ||
        TYPEOF(conditions) != VECSXP || XLENGTH(ids) <= row ||
        XLENGTH(on_ids) <= row || XLENGTH(conditions) <= row) {
      Rf_error(
        "ParamSet dependencies changed to a malformed shape during "
        "native get_values()"
      );
    }

    SEXP id = PROTECT(STRING_ELT(ids, row));
    const R_xlen_t original_position = find_name(
      original_names,
      kept,
      id,
      FALSE,
      work_since_interrupt
    );
    if (original_position == R_XLEN_T_MAX) {
      UNPROTECT(4);
      continue;
    }
    SEXP on = PROTECT(STRING_ELT(on_ids, row));
    SEXP names = PROTECT(snapshot_current_value_names(
      state,
      work_since_interrupt
    ));
    const R_xlen_t on_index = find_name(
      names,
      kept,
      on,
      TRUE,
      work_since_interrupt
    );
    UNPROTECT(1);
    SEXP on_value = PROTECT(on_index == R_XLEN_T_MAX
      ? R_NilValue
      : VECTOR_ELT(state->values_data.values, on_index));
    if (Rf_inherits(on_value, "TuneToken")) {
      UNPROTECT(6);
      continue;
    }

    SEXP condition = PROTECT(VECTOR_ELT(conditions, row));
    SEXP call = PROTECT(Rf_lang3(
      Rf_install("condition_test"),
      condition,
      on_value
    ));
    SEXP answer = PROTECT(Rf_eval(call, namespace_environment));
    if (!result_is_true(answer)) {
      names = PROTECT(snapshot_current_value_names(
        state,
        work_since_interrupt
      ));
      const R_xlen_t dependent = find_name(
        names,
        kept,
        id,
        TRUE,
        work_since_interrupt
      );
      UNPROTECT(1);
      /* R's `values[id] = NULL` replacement detaches the local list shell
       * even when a callback renamed `id` and no current element matches. */
      detach_values_shell(state, roots, work_since_interrupt);
      if (dependent != R_XLEN_T_MAX) {
        kept[dependent] = FALSE;
      }
    }
    UNPROTECT(9);
  }
}

static void apply_type_filter(const get_values_state_t *state,
    get_values_type_t type, int *kept,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t value = 0; value < state->values_data.size; ++value) {
    paradox_domain_account_work(work_since_interrupt);
    if (!kept[value] || type == GET_VALUES_WITH_TOKEN) {
      continue;
    }
    SEXP element = PROTECT(VECTOR_ELT(
      state->values_data.values,
      value
    ));
    const int tune_token = Rf_inherits(element, "TuneToken") != FALSE;
    if (type == GET_VALUES_WITHOUT_TOKEN) {
      kept[value] = !tune_token;
    } else if (type == GET_VALUES_ONLY_TOKEN) {
      kept[value] = tune_token;
    } else {
      kept[value] = Rf_inherits(element, "InternalTuneToken") != FALSE;
    }
    UNPROTECT(1);
  }
}

static void capture_live_tables(SEXP private_environment, SEXP roots,
    SEXP *params_out, SEXP *tags_out) {
  SEXP params = paradox_param_set_evaluated_local_value(
    private_environment,
    ".params"
  );
  if (params == R_UnboundValue) {
    params = R_NilValue;
  }
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_LIVE_PARAMS, params);
  SEXP tags = paradox_param_set_evaluated_local_value(
    private_environment,
    ".tags"
  );
  if (tags == R_UnboundValue) {
    tags = R_NilValue;
  }
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_LIVE_TAGS, tags);
  *params_out = params;
  *tags_out = tags;
}

static void reject_callback_capable_match_column(SEXP table,
    const char *storage_name, const char *column_name) {
  SEXP column = PROTECT(paradox_get_named_column(
    table,
    storage_name,
    column_name
  ));
  if (TYPEOF(column) == STRSXP &&
      (ALTREP(column) || Rf_isObject(column))) {
    UNPROTECT(1);
    Rf_error(
      "ParamSet ids changed to a callback-capable representation during "
      "native get_values()"
    );
  }
  UNPROTECT(1);
}

/* Rf_match() calls mtfrm() for object inputs. Inspect exactly the live
 * character columns that ids() will use as match operands after condition and
 * filter callbacks have completed. Malformed non-character columns retain the
 * established ids() diagnostic; callback-capable character columns must error
 * here instead of dispatching or returning NULL and replaying the callbacks. */
static void reject_callback_capable_ids_matches(SEXP params, SEXP tags,
    SEXP class_filter, SEXP all_tags, SEXP any_tags) {
  if (class_filter != R_NilValue) {
    reject_callback_capable_match_column(params, ".params", "cls");
  }

  const int uses_tag_matches = any_tags != R_NilValue ||
    (all_tags != R_NilValue && XLENGTH(all_tags) != 0);
  if (uses_tag_matches) {
    reject_callback_capable_match_column(params, ".params", "id");
    reject_callback_capable_match_column(tags, ".tags", "id");
    reject_callback_capable_match_column(tags, ".tags", "tag");
  }
}

static int name_is_present(SEXP names, SEXP target,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t size = XLENGTH(names);
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if (get_values_strings_equal(STRING_ELT(names, index), target)) {
      return TRUE;
    }
  }
  return FALSE;
}

static void missing_required_error(SEXP required, SEXP original_names,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t required_size = XLENGTH(required);
  SEXP missing = PROTECT(Rf_allocVector(STRSXP, required_size));
  size_t *missing_sizes = paradox_temporary_alloc(
    required_size,
    sizeof(*missing_sizes)
  );
  R_xlen_t missing_count = 0;
  size_t text_size = 1;
  const void *vmax = vmaxget();
  for (R_xlen_t index = 0; index < required_size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP id = PROTECT(STRING_ELT(required, index));
    if (!name_is_present(original_names, id, work_since_interrupt)) {
      const size_t id_size = strlen(Rf_translateCharUTF8(id));
      const size_t separator_size = missing_count == 0 ? 0 : 2;
      if (separator_size > SIZE_MAX - text_size ||
          id_size > SIZE_MAX - text_size - separator_size) {
        vmaxset(vmax);
        UNPROTECT(2);
        Rf_error("Unable to construct required-parameter diagnostic");
      }
      SET_STRING_ELT(missing, missing_count, id);
      missing_sizes[missing_count] = id_size;
      text_size += separator_size + id_size;
      ++missing_count;
    }
    UNPROTECT(1);
  }
  vmaxset(vmax);
  if (missing_count == 0) {
    UNPROTECT(1);
    return;
  }
  if ((uintmax_t) text_size > (uintmax_t) R_XLEN_T_MAX) {
    UNPROTECT(1);
    Rf_error("Unable to construct required-parameter diagnostic");
  }

  char *text = paradox_temporary_alloc((R_xlen_t) text_size, sizeof(*text));
  size_t offset = 0;
  vmax = vmaxget();
  for (R_xlen_t emitted = 0; emitted < missing_count; ++emitted) {
    paradox_domain_account_work(work_since_interrupt);
    if (emitted != 0) {
      if (offset > text_size - 1 || text_size - 1 - offset < 2) {
        vmaxset(vmax);
        UNPROTECT(1);
        Rf_error("Unable to construct required-parameter diagnostic");
      }
      text[offset++] = ',';
      text[offset++] = ' ';
    }
    SEXP id = STRING_ELT(missing, emitted);
    const char *id_text = Rf_translateCharUTF8(id);
    const size_t id_size = strlen(id_text);
    if (id_size != missing_sizes[emitted] ||
        offset > text_size - 1 || id_size > text_size - 1 - offset) {
      vmaxset(vmax);
      UNPROTECT(1);
      Rf_error("Unable to construct required-parameter diagnostic");
    }
    memcpy(text + offset, id_text, id_size);
    offset += id_size;
  }
  if (offset != text_size - 1) {
    vmaxset(vmax);
    UNPROTECT(1);
    Rf_error("Unable to construct required-parameter diagnostic");
  }
  text[offset] = '\0';
  vmaxset(vmax);
  UNPROTECT(1);
  Rf_error("Missing required parameters: %s", text);
}

static void check_required_values(SEXP self, SEXP private_environment,
    SEXP namespace_environment, SEXP roots, SEXP original_names,
    R_xlen_t *work_since_interrupt) {
  if (!canonical_ids_method(
      self,
      private_environment,
      namespace_environment
    )) {
    Rf_error("ParamSet ids method changed during native get_values()");
  }
  SEXP params;
  SEXP tags;
  SEXP required_tag = PROTECT(Rf_mkString("required"));
  capture_live_tables(private_environment, roots, &params, &tags);
  reject_callback_capable_ids_matches(
    params,
    tags,
    R_NilValue,
    required_tag,
    R_NilValue
  );
  SEXP required = PROTECT(paradox_param_set_ids(
    params,
    tags,
    R_NilValue,
    required_tag,
    R_NilValue
  ));
  missing_required_error(required, original_names, work_since_interrupt);
  UNPROTECT(2);
}

static SEXP build_result(const get_values_state_t *state, SEXP value_names,
    const int *kept, SEXP selected_ids,
    R_xlen_t *work_since_interrupt) {
  PROTECT(value_names);
  const R_xlen_t value_count = XLENGTH(value_names);
  if (value_count != state->values_data.size) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet values names during native get_values()");
  }
  const R_xlen_t capacity = XLENGTH(selected_ids);
  R_xlen_t kept_count = 0;
  for (R_xlen_t value = 0; value < value_count; ++value) {
    paradox_domain_account_work(work_since_interrupt);
    kept_count += kept[value] != 0;
  }

  int aligned = capacity == value_count && kept_count == value_count &&
    Rf_any_duplicated(value_names, FALSE) == 0;
  for (R_xlen_t value = 0; aligned && value < value_count; ++value) {
    paradox_domain_account_work(work_since_interrupt);
    aligned = get_values_strings_equal(
      STRING_ELT(value_names, value),
      STRING_ELT(selected_ids, value)
    );
  }
  if (aligned) {
    SEXP result = PROTECT(Rf_allocVector(VECSXP, value_count));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, value_count));
    for (R_xlen_t value = 0; value < value_count; ++value) {
      paradox_domain_account_work(work_since_interrupt);
      SEXP element = PROTECT(VECTOR_ELT(
        state->values_data.values,
        value
      ));
      SET_VECTOR_ELT(result, value, element);
      SET_STRING_ELT(names, value, STRING_ELT(value_names, value));
      UNPROTECT(1);
    }
    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(3);
    return result;
  }

  SEXP match_names = value_names;
  R_xlen_t *match_to_value = NULL;
  int match_names_protected = 0;
  if (kept_count != value_count) {
    match_names = PROTECT(Rf_allocVector(STRSXP, kept_count));
    match_names_protected = 1;
    match_to_value = paradox_temporary_alloc(
      kept_count,
      sizeof(*match_to_value)
    );
    R_xlen_t match_index = 0;
    for (R_xlen_t value = 0; value < value_count; ++value) {
      paradox_domain_account_work(work_since_interrupt);
      if (!kept[value]) {
        continue;
      }
      if (match_index >= kept_count) {
        UNPROTECT(1 + match_names_protected);
        Rf_error("Internal error: get_values match table exceeded its capacity");
      }
      SET_STRING_ELT(
        match_names,
        match_index,
        STRING_ELT(value_names, value)
      );
      match_to_value[match_index] = value;
      ++match_index;
    }
    if (match_index != kept_count) {
      UNPROTECT(1 + match_names_protected);
      Rf_error("Internal error: incomplete get_values match table");
    }
  }

  SEXP matches = PROTECT(Rf_match(match_names, selected_ids, 0));
  const SEXPTYPE match_type = (SEXPTYPE) TYPEOF(matches);
  if ((match_type != INTSXP && match_type != REALSXP) ||
      XLENGTH(matches) != capacity) {
    UNPROTECT(2 + match_names_protected);
    Rf_error("Internal error: unexpected result from R's matching primitive");
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, capacity));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, capacity));
  R_xlen_t output = 0;
  for (R_xlen_t id = 0; id < capacity; ++id) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t matched = match_position_at(
      matches,
      match_type,
      id,
      kept_count
    );
    if (matched == R_XLEN_T_MAX) {
      continue;
    }
    const R_xlen_t value = match_to_value == NULL
      ? matched
      : match_to_value[matched];
    if (output >= capacity || value >= state->values_data.size ||
        !kept[value]) {
      UNPROTECT(4 + match_names_protected);
      Rf_error("Internal error: get_values output exceeded its capacity");
    }
    SEXP element = PROTECT(VECTOR_ELT(state->values_data.values, value));
    SET_VECTOR_ELT(result, output, element);
    SET_STRING_ELT(
      names,
      output,
      STRING_ELT(value_names, value)
    );
    ++output;
    UNPROTECT(1);
  }

  SEXP final_result = result;
  SEXP final_names = names;
  int trimmed_protects = 0;
  if (output != capacity) {
    final_result = PROTECT(Rf_xlengthgets(result, output));
    ++trimmed_protects;
    final_names = PROTECT(Rf_xlengthgets(names, output));
    ++trimmed_protects;
  }
  if (XLENGTH(final_result) != output || XLENGTH(final_names) != output) {
    UNPROTECT(4 + match_names_protected + trimmed_protects);
    Rf_error("Internal error: incomplete get_values output");
  }
  Rf_setAttrib(final_result, R_NamesSymbol, final_names);
  UNPROTECT(4 + match_names_protected + trimmed_protects);
  return final_result;
}

SEXP paradox_param_set_get_values(SEXP private_environment, SEXP self,
    SEXP frame) {
  R_xlen_t work_since_interrupt = 0;
  SEXP namespace_environment = paradox_api_registered_namespace("paradox");
  if (TYPEOF(namespace_environment) != ENVSXP || TYPEOF(frame) != ENVSXP ||
      paradox_api_parent_environment(frame) != namespace_environment) {
    return R_NilValue;
  }

  /* These are the first two historical forcing points. Invalid values return
   * to the unchanged R path before any later promise or object callback is
   * touched, so checkmate retains the exact established diagnostic. */
  SEXP type_value = PROTECT(Rf_eval(Rf_install("type"), frame));
  get_values_type_t type;
  if (!parse_type(type_value, &type)) {
    UNPROTECT(1);
    return R_NilValue;
  }
  SEXP check_required = PROTECT(Rf_eval(
    Rf_install("check_required"),
    frame
  ));
  if (TYPEOF(check_required) != LGLSXP || XLENGTH(check_required) != 1 ||
      LOGICAL_ELT(check_required, 0) == NA_LOGICAL) {
    UNPROTECT(2);
    return R_NilValue;
  }

  SEXP roots = PROTECT(Rf_allocVector(VECSXP, GET_VALUES_ROOT_COUNT));
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_SELF, self);
  SET_VECTOR_ELT(roots, GET_VALUES_ROOT_PRIVATE, private_environment);
  get_values_state_t state;
  if (!preflight(
      private_environment,
      self,
      frame,
      namespace_environment,
      &state,
      roots,
      &work_since_interrupt
    )) {
    UNPROTECT(3);
    return R_NilValue;
  }

  int *kept = paradox_temporary_alloc(
    state.values_data.size,
    sizeof(*kept)
  );
  for (R_xlen_t value = 0; value < state.values_data.size; ++value) {
    paradox_domain_account_work(&work_since_interrupt);
    kept[value] = TRUE;
  }

  SEXP dependency_rows = PROTECT(dependency_removal_rows(
    frame,
    state.dependencies
  ));
  if (dependency_rows != R_NilValue) {
    apply_dependencies(
      &state,
      kept,
      dependency_rows,
      namespace_environment,
      roots,
      &work_since_interrupt
    );
  }
  apply_type_filter(&state, type, kept, &work_since_interrupt);
  if (type != GET_VALUES_WITH_TOKEN) {
    detach_values_shell(&state, roots, &work_since_interrupt);
  }

  if (LOGICAL_ELT(check_required, 0) == TRUE) {
    check_required_values(
      self,
      private_environment,
      namespace_environment,
      roots,
      state.original_names,
      &work_since_interrupt
    );
  }

  /* Resolving ids() precedes forcing its arguments. A filter promise may
   * replace the binding, but the current invocation still uses the method
   * that was resolved before that promise ran. */
  if (!canonical_ids_method(
      self,
      private_environment,
      namespace_environment
    )) {
    UNPROTECT(4);
    Rf_error("ParamSet ids method changed during native get_values()");
  }
  SEXP class_filter = PROTECT(paradox_param_set_filter_argument(
    frame,
    "class"
  ));
  SEXP all_tags = PROTECT(paradox_param_set_filter_argument(frame, "tags"));
  SEXP any_tags = PROTECT(paradox_param_set_filter_argument(
    frame,
    "any_tags"
  ));

  SEXP live_params;
  SEXP live_tags;
  capture_live_tables(
    private_environment,
    roots,
    &live_params,
    &live_tags
  );
  reject_callback_capable_ids_matches(
    live_params,
    live_tags,
    class_filter,
    all_tags,
    any_tags
  );
  SEXP selected_ids = PROTECT(paradox_param_set_ids(
    live_params,
    live_tags,
    class_filter,
    all_tags,
    any_tags
  ));
  /* Rf_match() dispatches mtfrm() for object inputs. A condition or filter
   * callback can add a class to the otherwise canonical ID column after
   * preflight; admitting it here would introduce another arbitrary callback
   * after the value-name snapshot and could silently pair current values with
   * stale names. The callback has already run, so decline by error rather than
   * returning NULL and replaying it in the R fallback. Ordinary attributes
   * such as names do not dispatch and remain supported. */
  if (ALTREP(selected_ids) || Rf_isObject(selected_ids)) {
    UNPROTECT(8);
    Rf_error(
      "ParamSet ids changed to a callback-capable representation during "
      "native get_values()"
    );
  }
  SEXP value_names = PROTECT(snapshot_current_value_names(
    &state,
    &work_since_interrupt
  ));
  SEXP result = build_result(
    &state,
    value_names,
    kept,
    selected_ids,
    &work_since_interrupt
  );
  UNPROTECT(9);
  return result;
}
