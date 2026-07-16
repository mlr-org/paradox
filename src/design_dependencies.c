#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "builtin_condition.h"
#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

typedef struct {
  R_xlen_t child;
  R_xlen_t parent;
  paradox_builtin_condition_kind_t kind;
  SEXP condition;
  SEXP rhs;
} design_dependency_edge_t;

typedef struct {
  SEXP data;
  SEXP param_set;
  SEXP enclosure;
  SEXP private_environment;
  SEXP params;
  SEXP dependencies;
  SEXP param_ids;
  SEXP storage_types;
  SEXP dependency_ids;
  SEXP dependency_on;
  SEXP conditions;
  SEXP data_names;
  SEXP columns;
  SEXP condition_roots;
  SEXP rhs_roots;
  SEXP stable_param_ids;
  SEXP stable_storage_types;
  SEXP stable_dependency_ids;
  SEXP stable_dependency_on;
  SEXP stable_data_names;
  R_xlen_t parameter_count;
  R_xlen_t dependency_count;
  R_xlen_t row_count;
  R_xlen_t *param_columns;
  R_xlen_t *mask_columns;
  R_xlen_t mask_column_count;
  size_t mask_byte_count;
  R_xlen_t *topological_order;
  R_xlen_t *edge_order;
  design_dependency_edge_t *edges;
} design_dependency_state_t;

#define DESIGN_DEPENDENCY_MASK_LIMIT_BYTES \
  ((size_t) 64U * (size_t) 1024U * (size_t) 1024U)
#define DESIGN_DEPENDENCY_PLAN_INDEX_LIMIT \
  ((R_xlen_t) 8 * (R_xlen_t) 1024 * (R_xlen_t) 1024)
#define DESIGN_DEPENDENCY_PLAN_EDGE_LIMIT ((R_xlen_t) 65536)

enum design_dependency_topology_state {
  DESIGN_DEPENDENCY_TOPOLOGY_UNCONFIGURED = 0,
  DESIGN_DEPENDENCY_TOPOLOGY_DISABLED,
  DESIGN_DEPENDENCY_TOPOLOGY_ENABLED
};

static enum design_dependency_topology_state topology_state =
  DESIGN_DEPENDENCY_TOPOLOGY_UNCONFIGURED;

enum design_dependency_root_slot {
  DESIGN_DEPENDENCY_ROOT_DATA = 0,
  DESIGN_DEPENDENCY_ROOT_PARAM_SET,
  DESIGN_DEPENDENCY_ROOT_ENCLOSURE,
  DESIGN_DEPENDENCY_ROOT_PRIVATE,
  DESIGN_DEPENDENCY_ROOT_PARAMS,
  DESIGN_DEPENDENCY_ROOT_DEPENDENCIES,
  DESIGN_DEPENDENCY_ROOT_PARAM_IDS,
  DESIGN_DEPENDENCY_ROOT_STORAGE_TYPES,
  DESIGN_DEPENDENCY_ROOT_DEPENDENCY_IDS,
  DESIGN_DEPENDENCY_ROOT_DEPENDENCY_ON,
  DESIGN_DEPENDENCY_ROOT_CONDITIONS,
  DESIGN_DEPENDENCY_ROOT_DATA_NAMES,
  DESIGN_DEPENDENCY_ROOT_COLUMNS,
  DESIGN_DEPENDENCY_ROOT_CONDITION_OBJECTS,
  DESIGN_DEPENDENCY_ROOT_RHS,
  DESIGN_DEPENDENCY_ROOT_STABLE_PARAM_IDS,
  DESIGN_DEPENDENCY_ROOT_STABLE_STORAGE_TYPES,
  DESIGN_DEPENDENCY_ROOT_STABLE_DEPENDENCY_IDS,
  DESIGN_DEPENDENCY_ROOT_STABLE_DEPENDENCY_ON,
  DESIGN_DEPENDENCY_ROOT_STABLE_DATA_NAMES,
  DESIGN_DEPENDENCY_ROOT_OUTPUT,
  DESIGN_DEPENDENCY_ROOT_OUTPUT_ROWS,
  DESIGN_DEPENDENCY_ROOT_OUTPUT_COLUMNS,
  DESIGN_DEPENDENCY_ROOT_OUTPUT_VALUES,
  DESIGN_DEPENDENCY_ROOT_COUNT
};

static int supported_id(SEXP value) {
  return value != NA_STRING && Rf_getCharCE(value) != CE_BYTES;
}

static int strings_equal_without_translation(SEXP left, SEXP right) {
  return left == right || (left != NA_STRING && right != NA_STRING &&
    Rf_getCharCE(left) == Rf_getCharCE(right) &&
    strcmp(CHAR(left), CHAR(right)) == 0);
}

static R_xlen_t find_id(SEXP ids, SEXP target,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t size = XLENGTH(ids);
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if (strings_equal_without_translation(STRING_ELT(ids, index), target)) {
      return index;
    }
  }
  return R_XLEN_T_MAX;
}

static int exact_storage(SEXP cls, SEXP storage, SEXPTYPE *type) {
  if (paradox_domain_string_is(cls, "ParamDbl") &&
      paradox_domain_string_is(storage, "numeric")) {
    *type = REALSXP;
    return TRUE;
  }
  if (paradox_domain_string_is(cls, "ParamInt") &&
      paradox_domain_string_is(storage, "integer")) {
    *type = INTSXP;
    return TRUE;
  }
  if (paradox_domain_string_is(cls, "ParamFct") &&
      paradox_domain_string_is(storage, "character")) {
    *type = STRSXP;
    return TRUE;
  }
  if (paradox_domain_string_is(cls, "ParamLgl") &&
      paradox_domain_string_is(storage, "logical")) {
    *type = LGLSXP;
    return TRUE;
  }
  return FALSE;
}

static int exact_data_table_shell(SEXP data, SEXP *names,
    R_xlen_t *work_since_interrupt) {
  static const char *const classes[] = {"data.table", "data.frame"};
  static const char *const attributes[] = {
    "names", "row.names", "class", ".internal.selfref", "sorted", "index"
  };
  if (TYPEOF(data) != VECSXP || ALTREP(data) ||
      !paradox_api_has_only_attributes(data, attributes, 6)) {
    return FALSE;
  }
  SEXP candidate_names = PROTECT(Rf_getAttrib(data, R_NamesSymbol));
  SEXP candidate_classes = PROTECT(Rf_getAttrib(data, R_ClassSymbol));
  const int exact = TYPEOF(candidate_names) == STRSXP &&
    !ALTREP(candidate_names) && !Rf_isObject(candidate_names) &&
    XLENGTH(candidate_names) == XLENGTH(data) &&
    paradox_api_has_no_attributes(candidate_names) &&
    paradox_domain_exact_string_vector(
      candidate_classes,
      classes,
      2,
      work_since_interrupt
    ) && paradox_api_has_no_attributes(candidate_classes) &&
    Rf_any_duplicated(candidate_names, FALSE) == 0;
  if (exact) {
    for (R_xlen_t column = 0; column < XLENGTH(data); ++column) {
      paradox_domain_account_work(work_since_interrupt);
      if (!supported_id(STRING_ELT(candidate_names, column))) {
        UNPROTECT(2);
        return FALSE;
      }
    }
    *names = candidate_names;
  }
  UNPROTECT(2);
  return exact;
}

static int aliases_table_attribute(SEXP value, SEXP table);

static int aliases_table_storage(SEXP value, SEXP table) {
  if (value == table) {
    return TRUE;
  }
  for (R_xlen_t column = 0; column < XLENGTH(table); ++column) {
    if (value == VECTOR_ELT(table, column)) {
      return TRUE;
    }
  }
  return aliases_table_attribute(value, table);
}

static int aliases_table_attribute(SEXP value, SEXP table) {
  static const char *const attributes[] = {
    "names", "class", ".internal.selfref", "sorted", "index"
  };
  for (size_t index = 0;
       index < sizeof(attributes) / sizeof(attributes[0]);
       ++index) {
    if (value == Rf_getAttrib(table, Rf_install(attributes[index]))) {
      return TRUE;
    }
  }
  return FALSE;
}

static int aliases_condition_storage(SEXP value, SEXP condition) {
  return value == condition || value == VECTOR_ELT(condition, 0) ||
    value == VECTOR_ELT(condition, 1) ||
    value == Rf_getAttrib(condition, R_NamesSymbol) ||
    value == Rf_getAttrib(condition, R_ClassSymbol);
}

static SEXP snapshot_strings(SEXP source,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t size = XLENGTH(source);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    SET_STRING_ELT(result, index, STRING_ELT(source, index));
  }
  UNPROTECT(1);
  return result;
}

static int planning_strings_match(const design_dependency_state_t *state) {
  if (TYPEOF(state->param_ids) != STRSXP ||
      XLENGTH(state->param_ids) != state->parameter_count ||
      TYPEOF(state->storage_types) != STRSXP ||
      XLENGTH(state->storage_types) != state->parameter_count ||
      TYPEOF(state->data_names) != STRSXP ||
      XLENGTH(state->data_names) != state->parameter_count ||
      TYPEOF(state->dependency_ids) != STRSXP ||
      XLENGTH(state->dependency_ids) != state->dependency_count ||
      TYPEOF(state->dependency_on) != STRSXP ||
      XLENGTH(state->dependency_on) != state->dependency_count) {
    return FALSE;
  }
  for (R_xlen_t parameter = 0;
       parameter < state->parameter_count;
       ++parameter) {
    if (STRING_ELT(state->param_ids, parameter) != STRING_ELT(
        state->stable_param_ids,
        parameter
      ) || STRING_ELT(state->storage_types, parameter) != STRING_ELT(
        state->stable_storage_types,
        parameter
      ) || STRING_ELT(state->data_names, parameter) != STRING_ELT(
        state->stable_data_names,
        parameter
      )) {
      return FALSE;
    }
  }
  for (R_xlen_t edge = 0; edge < state->dependency_count; ++edge) {
    if (STRING_ELT(state->dependency_ids, edge) != STRING_ELT(
        state->stable_dependency_ids,
        edge
      ) || STRING_ELT(state->dependency_on, edge) != STRING_ELT(
        state->stable_dependency_on,
        edge
      )) {
      return FALSE;
    }
  }
  return TRUE;
}

/* Copy every string that defines column/edge mappings before allocating their
 * workspaces. A GC finalizer may mutate user-visible character vectors during
 * any later R allocation; mappings are built only from these private snapshots
 * and the live vectors must still match them at the final fallback boundary. */
static int snapshot_planning_strings(design_dependency_state_t *state,
    SEXP roots, R_xlen_t *work_since_interrupt) {
  state->stable_param_ids = PROTECT(snapshot_strings(
    state->param_ids,
    work_since_interrupt
  ));
  state->stable_storage_types = PROTECT(snapshot_strings(
    state->storage_types,
    work_since_interrupt
  ));
  state->stable_dependency_ids = PROTECT(snapshot_strings(
    state->dependency_ids,
    work_since_interrupt
  ));
  state->stable_dependency_on = PROTECT(snapshot_strings(
    state->dependency_on,
    work_since_interrupt
  ));
  state->stable_data_names = PROTECT(snapshot_strings(
    state->data_names,
    work_since_interrupt
  ));
  SET_VECTOR_ELT(
    roots,
    DESIGN_DEPENDENCY_ROOT_STABLE_PARAM_IDS,
    state->stable_param_ids
  );
  SET_VECTOR_ELT(
    roots,
    DESIGN_DEPENDENCY_ROOT_STABLE_STORAGE_TYPES,
    state->stable_storage_types
  );
  SET_VECTOR_ELT(
    roots,
    DESIGN_DEPENDENCY_ROOT_STABLE_DEPENDENCY_IDS,
    state->stable_dependency_ids
  );
  SET_VECTOR_ELT(
    roots,
    DESIGN_DEPENDENCY_ROOT_STABLE_DEPENDENCY_ON,
    state->stable_dependency_on
  );
  SET_VECTOR_ELT(
    roots,
    DESIGN_DEPENDENCY_ROOT_STABLE_DATA_NAMES,
    state->stable_data_names
  );
  UNPROTECT(5);
  return planning_strings_match(state);
}

static int canonical_import(SEXP namespace_environment,
    const char *package, const char *name) {
  SEXP imports = PROTECT(paradox_api_parent_environment(
    namespace_environment
  ));
  SEXP package_namespace = PROTECT(
    paradox_api_registered_namespace(package)
  );
  SEXP symbol = Rf_install(name);
  if (TYPEOF(imports) != ENVSXP || TYPEOF(package_namespace) != ENVSXP ||
      !R_existsVarInFrame(imports, symbol) ||
      R_BindingIsActive(symbol, imports) ||
      !R_BindingIsLocked(symbol, imports) ||
      !R_existsVarInFrame(package_namespace, symbol) ||
      R_BindingIsActive(symbol, package_namespace) ||
      !R_BindingIsLocked(symbol, package_namespace)) {
    UNPROTECT(2);
    return FALSE;
  }
  SEXP imported = PROTECT(paradox_api_stable_local_value(imports, symbol));
  SEXP original = PROTECT(paradox_api_stable_local_value(
    package_namespace,
    symbol
  ));
  const int exact = imported == original && TYPEOF(imported) == CLOSXP &&
    paradox_api_closure_environment(imported) == package_namespace;
  UNPROTECT(4);
  return exact;
}

static int exact_missing_formal(SEXP formal, const char *name) {
  return TYPEOF(formal) == LISTSXP && TAG(formal) == Rf_install(name) &&
    CAR(formal) == R_MissingArg;
}

static int exact_unary_symbol_call(SEXP call, const char *function,
    const char *argument) {
  return TYPEOF(call) == LANGSXP && CAR(call) == Rf_install(function) &&
    TYPEOF(CDR(call)) == LISTSXP && TAG(CDR(call)) == R_NilValue &&
    CAR(CDR(call)) == Rf_install(argument) &&
    CDR(CDR(call)) == R_NilValue;
}

static int exact_as_type_body(SEXP body) {
  if (TYPEOF(body) != LANGSXP || CAR(body) != Rf_install("{") ||
      TYPEOF(CDR(body)) != LISTSXP || CDR(CDR(body)) != R_NilValue) {
    return FALSE;
  }
  SEXP expression = CAR(CDR(body));
  if (TYPEOF(expression) != LANGSXP ||
      CAR(expression) != Rf_install("switch")) {
    return FALSE;
  }
  SEXP argument = CDR(expression);
  if (TYPEOF(argument) != LISTSXP || TAG(argument) != R_NilValue ||
      CAR(argument) != Rf_install("type")) {
    return FALSE;
  }
  argument = CDR(argument);

  static const char *const storage_types[] = {
    "logical", "integer", "numeric", "character", "list"
  };
  static const char *const conversions[] = {
    "as.logical", "as.integer", "as.numeric", "as.character", "as.list"
  };
  for (size_t index = 0;
       index < sizeof(storage_types) / sizeof(storage_types[0]);
       ++index) {
    if (TYPEOF(argument) != LISTSXP ||
        TAG(argument) != Rf_install(storage_types[index]) ||
        !exact_unary_symbol_call(
          CAR(argument),
          conversions[index],
          "x"
        )) {
      return FALSE;
    }
    argument = CDR(argument);
  }

  if (TYPEOF(argument) != LISTSXP || TAG(argument) != R_NilValue ||
      CDR(argument) != R_NilValue) {
    return FALSE;
  }
  SEXP error_call = CAR(argument);
  if (TYPEOF(error_call) != LANGSXP ||
      CAR(error_call) != Rf_install("stopf")) {
    return FALSE;
  }
  SEXP error_argument = CDR(error_call);
  if (TYPEOF(error_argument) != LISTSXP ||
      TAG(error_argument) != R_NilValue) {
    return FALSE;
  }
  SEXP format = CAR(error_argument);
  if (TYPEOF(format) != STRSXP || ALTREP(format) || XLENGTH(format) != 1 ||
      !paradox_api_has_no_attributes(format) || !paradox_domain_string_is(
        STRING_ELT(format, 0),
        "Invalid storage type '%s'"
      )) {
    return FALSE;
  }
  error_argument = CDR(error_argument);
  return TYPEOF(error_argument) == LISTSXP &&
    TAG(error_argument) == R_NilValue &&
    CAR(error_argument) == Rf_install("type") &&
    CDR(error_argument) == R_NilValue;
}

static int canonical_as_type(SEXP namespace_environment) {
  SEXP symbol = Rf_install("as_type");
  if (!R_existsVarInFrame(namespace_environment, symbol) ||
      R_BindingIsActive(symbol, namespace_environment) ||
      !R_BindingIsLocked(symbol, namespace_environment)) {
    return FALSE;
  }
  SEXP function = PROTECT(paradox_api_stable_local_value(
    namespace_environment,
    symbol
  ));
  SEXP formals = function == R_UnboundValue || TYPEOF(function) != CLOSXP
    ? R_NilValue
    : paradox_api_closure_formals(function);
  SEXP body = function == R_UnboundValue || TYPEOF(function) != CLOSXP
    ? R_NilValue
    : paradox_api_closure_expression(function);
  const int exact = TYPEOF(function) == CLOSXP &&
    paradox_api_closure_environment(function) == namespace_environment &&
    exact_missing_formal(formals, "x") &&
    exact_missing_formal(CDR(formals), "type") &&
    CDR(CDR(formals)) == R_NilValue && exact_as_type_body(body);
  UNPROTECT(1);
  return exact;
}

static int canonical_execution_surface(SEXP param_set,
    SEXP namespace_environment) {
  return paradox_param_set_design_dependencies_auth(param_set) &&
    canonical_import(namespace_environment, "mlr3misc", "topo_sort") &&
    canonical_import(namespace_environment, "mlr3misc", "seq_row") &&
    canonical_as_type(namespace_environment) &&
    canonical_import(namespace_environment, "data.table", "set") &&
    paradox_builtin_condition_dispatch_is_canonical(namespace_environment);
}

static int load_private_state(design_dependency_state_t *state,
    SEXP roots) {
  state->enclosure = paradox_domain_local_value(
    state->param_set,
    ".__enclos_env__"
  );
  if (state->enclosure == R_UnboundValue) {
    return FALSE;
  }
  SET_VECTOR_ELT(
    roots,
    DESIGN_DEPENDENCY_ROOT_ENCLOSURE,
    state->enclosure
  );
  state->private_environment = paradox_domain_local_value(
    state->enclosure,
    "private"
  );
  if (state->private_environment == R_UnboundValue) {
    return FALSE;
  }
  SET_VECTOR_ELT(
    roots,
    DESIGN_DEPENDENCY_ROOT_PRIVATE,
    state->private_environment
  );
  state->params = paradox_domain_local_value(
    state->private_environment,
    ".params"
  );
  state->dependencies = paradox_domain_local_value(
    state->private_environment,
    ".deps"
  );
  if (state->params == R_UnboundValue ||
      state->dependencies == R_UnboundValue) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, DESIGN_DEPENDENCY_ROOT_PARAMS, state->params);
  SET_VECTOR_ELT(
    roots,
    DESIGN_DEPENDENCY_ROOT_DEPENDENCIES,
    state->dependencies
  );
  return TRUE;
}

static int load_param_state(design_dependency_state_t *state, SEXP roots,
    R_xlen_t *work_since_interrupt) {
  paradox_domain_params_t params;
  R_xlen_t unused_row = 0;
  if (!paradox_domain_validate_params(
      state->params,
      R_NilValue,
      TRUE,
      &params,
      &unused_row,
      work_since_interrupt
    )) {
    return FALSE;
  }
  state->parameter_count = params.row_count;
  state->param_ids = params.ids;
  SET_VECTOR_ELT(
    roots,
    DESIGN_DEPENDENCY_ROOT_PARAM_IDS,
    state->param_ids
  );
  SEXP classes = PROTECT(params.classes);
  state->storage_types = PROTECT(VECTOR_ELT(
    state->params,
    PARADOX_DOMAIN_STORAGE_TYPE
  ));
  SET_VECTOR_ELT(
    roots,
    DESIGN_DEPENDENCY_ROOT_STORAGE_TYPES,
    state->storage_types
  );
  if (TYPEOF(state->storage_types) != STRSXP ||
      ALTREP(state->storage_types) ||
      XLENGTH(state->storage_types) != state->parameter_count ||
      !paradox_api_has_no_attributes(state->storage_types)) {
    UNPROTECT(2);
    return FALSE;
  }
  for (R_xlen_t parameter = 0;
       parameter < state->parameter_count;
       ++parameter) {
    paradox_domain_account_work(work_since_interrupt);
    SEXPTYPE type;
    if (!supported_id(STRING_ELT(state->param_ids, parameter)) ||
        !exact_storage(
          STRING_ELT(classes, parameter),
          STRING_ELT(state->storage_types, parameter),
          &type
        )) {
      UNPROTECT(2);
      return FALSE;
    }
  }
  UNPROTECT(2);
  return TRUE;
}

static int load_dependency_state(design_dependency_state_t *state,
    SEXP roots, R_xlen_t *work_since_interrupt) {
  paradox_domain_dependencies_t dependencies;
  if (!paradox_domain_validate_dependencies(
      state->dependencies,
      &dependencies,
      work_since_interrupt
    ) || dependencies.row_count == 0 ||
      dependencies.row_count > DESIGN_DEPENDENCY_PLAN_EDGE_LIMIT) {
    return FALSE;
  }
  state->dependency_count = dependencies.row_count;
  state->dependency_ids = dependencies.ids;
  state->dependency_on = dependencies.on;
  state->conditions = dependencies.conditions;
  SET_VECTOR_ELT(
    roots,
    DESIGN_DEPENDENCY_ROOT_DEPENDENCY_IDS,
    state->dependency_ids
  );
  SET_VECTOR_ELT(
    roots,
    DESIGN_DEPENDENCY_ROOT_DEPENDENCY_ON,
    state->dependency_on
  );
  SET_VECTOR_ELT(
    roots,
    DESIGN_DEPENDENCY_ROOT_CONDITIONS,
    state->conditions
  );
  return TRUE;
}

static int load_data_state(design_dependency_state_t *state, SEXP roots,
    R_xlen_t *work_since_interrupt) {
  if (!exact_data_table_shell(
      state->data,
      &state->data_names,
      work_since_interrupt
    ) || XLENGTH(state->data) != state->parameter_count) {
    return FALSE;
  }
  SET_VECTOR_ELT(
    roots,
    DESIGN_DEPENDENCY_ROOT_DATA_NAMES,
    state->data_names
  );
  state->row_count = state->parameter_count == 0
    ? 0
    : XLENGTH(VECTOR_ELT(state->data, 0));
  if (state->row_count > INT_MAX || !snapshot_planning_strings(
      state,
      roots,
      work_since_interrupt
    )) {
    return FALSE;
  }
  state->columns = PROTECT(Rf_allocVector(
    VECSXP,
    state->parameter_count
  ));
  SET_VECTOR_ELT(roots, DESIGN_DEPENDENCY_ROOT_COLUMNS, state->columns);
  UNPROTECT(1);
  state->param_columns = paradox_temporary_alloc(
    state->parameter_count,
    sizeof(*state->param_columns)
  );
  for (R_xlen_t parameter = 0;
       parameter < state->parameter_count;
       ++parameter) {
    paradox_domain_account_work(work_since_interrupt);
    R_xlen_t column = find_id(
      state->stable_data_names,
      STRING_ELT(state->stable_param_ids, parameter),
      work_since_interrupt
    );
    SEXPTYPE expected_type;
    if (column == R_XLEN_T_MAX || !exact_storage(
        STRING_ELT(VECTOR_ELT(state->params, PARADOX_DOMAIN_CLS), parameter),
        STRING_ELT(state->stable_storage_types, parameter),
        &expected_type
      )) {
      return FALSE;
    }
    SEXP value = PROTECT(VECTOR_ELT(state->data, column));
    if ((SEXPTYPE) TYPEOF(value) != expected_type || ALTREP(value) ||
        Rf_isObject(value) || XLENGTH(value) != state->row_count ||
        !paradox_api_has_no_attributes(value) ||
        aliases_table_attribute(value, state->data) ||
        aliases_table_storage(value, state->params) ||
        aliases_table_storage(value, state->dependencies)) {
      UNPROTECT(1);
      return FALSE;
    }
    for (R_xlen_t prior = 0; prior < parameter; ++prior) {
      if (value == VECTOR_ELT(state->columns, prior)) {
        UNPROTECT(1);
        return FALSE;
      }
    }
    SET_VECTOR_ELT(state->columns, parameter, value);
    state->param_columns[parameter] = column;
    UNPROTECT(1);
  }
  return TRUE;
}

static int plan_edges(design_dependency_state_t *state, SEXP roots,
    R_xlen_t *work_since_interrupt) {
  state->edges = paradox_temporary_alloc(
    state->dependency_count,
    sizeof(*state->edges)
  );
  state->condition_roots = PROTECT(Rf_allocVector(
    VECSXP,
    state->dependency_count
  ));
  state->rhs_roots = PROTECT(Rf_allocVector(
    VECSXP,
    state->dependency_count
  ));
  SET_VECTOR_ELT(
    roots,
    DESIGN_DEPENDENCY_ROOT_CONDITION_OBJECTS,
    state->condition_roots
  );
  SET_VECTOR_ELT(roots, DESIGN_DEPENDENCY_ROOT_RHS, state->rhs_roots);
  UNPROTECT(2);

  for (R_xlen_t edge = 0; edge < state->dependency_count; ++edge) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP id = STRING_ELT(state->stable_dependency_ids, edge);
    SEXP on = STRING_ELT(state->stable_dependency_on, edge);
    if (!supported_id(id) || !supported_id(on)) {
      return FALSE;
    }
    state->edges[edge].child = find_id(
      state->stable_param_ids,
      id,
      work_since_interrupt
    );
    state->edges[edge].parent = find_id(
      state->stable_param_ids,
      on,
      work_since_interrupt
    );
    if (state->edges[edge].child == R_XLEN_T_MAX ||
        state->edges[edge].parent == R_XLEN_T_MAX) {
      return FALSE;
    }
    state->edges[edge].condition = PROTECT(VECTOR_ELT(
      state->conditions,
      edge
    ));
    SET_VECTOR_ELT(
      state->condition_roots,
      edge,
      state->edges[edge].condition
    );
    if (!paradox_builtin_condition_exact(
        state->edges[edge].condition,
        &state->edges[edge].kind,
        &state->edges[edge].rhs,
        work_since_interrupt
      )) {
      UNPROTECT(1);
      return FALSE;
    }
    SET_VECTOR_ELT(state->rhs_roots, edge, state->edges[edge].rhs);
    SEXP parent = VECTOR_ELT(
      state->columns,
      state->edges[edge].parent
    );
    if (!paradox_builtin_condition_column_supported(
        parent,
        state->edges[edge].rhs,
        work_since_interrupt
      )) {
      UNPROTECT(1);
      return FALSE;
    }
    for (R_xlen_t column = 0;
         column < state->parameter_count;
         ++column) {
      if (aliases_condition_storage(
          VECTOR_ELT(state->columns, column),
          state->edges[edge].condition
        )) {
        UNPROTECT(1);
        return FALSE;
      }
    }
    UNPROTECT(1);
  }
  return TRUE;
}

/* Only parameters that are parents can be observed after an earlier edge has
 * made them inactive. Indexing those columns, rather than every parameter,
 * keeps sparse dependency graphs sparse. The row states themselves are bits
 * so a large but reasonable design does not multiply its memory footprint by
 * the full parameter count. */
static void build_mask_layout(design_dependency_state_t *state) {
  state->mask_columns = paradox_temporary_alloc(
    state->parameter_count,
    sizeof(*state->mask_columns)
  );
  for (R_xlen_t parameter = 0;
       parameter < state->parameter_count;
       ++parameter) {
    state->mask_columns[parameter] = R_XLEN_T_MAX;
  }
  state->mask_column_count = 0;
  for (R_xlen_t edge = 0; edge < state->dependency_count; ++edge) {
    const R_xlen_t parent = state->edges[edge].parent;
    if (state->mask_columns[parent] == R_XLEN_T_MAX) {
      state->mask_columns[parent] = state->mask_column_count++;
    }
  }
}

static int build_topological_order(design_dependency_state_t *state,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t parameters = state->parameter_count;
  const R_xlen_t dependencies = state->dependency_count;
  state->topological_order = paradox_temporary_alloc(
    parameters,
    sizeof(*state->topological_order)
  );
  state->edge_order = paradox_temporary_alloc(
    dependencies,
    sizeof(*state->edge_order)
  );
  R_xlen_t *base_order = paradox_temporary_alloc(
    parameters,
    sizeof(*base_order)
  );
  R_xlen_t *parent_counts = paradox_temporary_alloc(
    parameters,
    sizeof(*parent_counts)
  );
  unsigned char *seen = paradox_temporary_alloc(parameters, sizeof(*seen));
  unsigned char *processed = paradox_temporary_alloc(
    parameters,
    sizeof(*processed)
  );
  unsigned char *ready = paradox_temporary_alloc(parameters, sizeof(*ready));
  memset(seen, 0, (size_t) parameters);
  memset(processed, 0, (size_t) parameters);
  memset(parent_counts, 0, (size_t) parameters * sizeof(*parent_counts));

  R_xlen_t base_size = 0;
  for (R_xlen_t edge = 0; edge < dependencies; ++edge) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t child = state->edges[edge].child;
    ++parent_counts[child];
    if (!seen[child]) {
      seen[child] = 1;
      base_order[base_size++] = child;
    }
  }
  for (R_xlen_t parameter = 0; parameter < parameters; ++parameter) {
    if (!seen[parameter]) {
      base_order[base_size++] = parameter;
    }
  }
  if (base_size != parameters) {
    return FALSE;
  }

  /* R's order() is stable here. An insertion sort preserves the graph's
   * first-occurrence order for equal original parent-list lengths. */
  for (R_xlen_t index = 1; index < parameters; ++index) {
    R_xlen_t value = base_order[index];
    R_xlen_t prior = index;
    while (prior > 0 &&
        parent_counts[base_order[prior - 1]] > parent_counts[value]) {
      base_order[prior] = base_order[prior - 1];
      --prior;
    }
    base_order[prior] = value;
  }

  R_xlen_t topological_size = 0;
  while (topological_size < parameters) {
    memset(ready, 0, (size_t) parameters);
    R_xlen_t ready_count = 0;
    for (R_xlen_t position = 0; position < parameters; ++position) {
      const R_xlen_t parameter = base_order[position];
      if (processed[parameter]) {
        continue;
      }
      int all_parents_processed = TRUE;
      for (R_xlen_t edge = 0; edge < dependencies; ++edge) {
        if (state->edges[edge].child == parameter &&
            !processed[state->edges[edge].parent]) {
          all_parents_processed = FALSE;
          break;
        }
      }
      if (all_parents_processed) {
        ready[parameter] = 1;
        ++ready_count;
      }
    }
    if (ready_count == 0) {
      return FALSE;
    }
    for (R_xlen_t position = 0; position < parameters; ++position) {
      const R_xlen_t parameter = base_order[position];
      if (ready[parameter]) {
        processed[parameter] = 1;
        state->topological_order[topological_size++] = parameter;
      }
    }
  }

  R_xlen_t edge_size = 0;
  for (R_xlen_t position = 0; position < parameters; ++position) {
    const R_xlen_t parameter = state->topological_order[position];
    for (R_xlen_t edge = 0; edge < dependencies; ++edge) {
      if (state->edges[edge].child == parameter) {
        state->edge_order[edge_size++] = edge;
      }
    }
  }
  return edge_size == dependencies;
}

static int current_state_matches(const design_dependency_state_t *state) {
  if (TYPEOF(state->data) != VECSXP ||
      XLENGTH(state->data) != state->parameter_count ||
      TYPEOF(state->params) != VECSXP ||
      XLENGTH(state->params) <= PARADOX_DOMAIN_STORAGE_TYPE ||
      TYPEOF(state->dependencies) != VECSXP ||
      XLENGTH(state->dependencies) != 3 ||
      TYPEOF(state->param_ids) != STRSXP ||
      XLENGTH(state->param_ids) != state->parameter_count ||
      TYPEOF(state->storage_types) != STRSXP ||
      XLENGTH(state->storage_types) != state->parameter_count ||
      TYPEOF(state->dependency_ids) != STRSXP ||
      XLENGTH(state->dependency_ids) != state->dependency_count ||
      TYPEOF(state->dependency_on) != STRSXP ||
      XLENGTH(state->dependency_on) != state->dependency_count ||
      TYPEOF(state->conditions) != VECSXP ||
      XLENGTH(state->conditions) != state->dependency_count ||
      TYPEOF(state->data_names) != STRSXP ||
      XLENGTH(state->data_names) != state->parameter_count ||
      paradox_domain_local_value(state->param_set, ".__enclos_env__") !=
      state->enclosure || paradox_domain_local_value(
        state->enclosure,
        "private"
      ) != state->private_environment || paradox_domain_local_value(
        state->private_environment,
        ".params"
      ) != state->params || paradox_domain_local_value(
        state->private_environment,
        ".deps"
      ) != state->dependencies || Rf_getAttrib(state->data, R_NamesSymbol) !=
      state->data_names || VECTOR_ELT(
        state->params,
        PARADOX_DOMAIN_ID
      ) != state->param_ids || VECTOR_ELT(
        state->params,
        PARADOX_DOMAIN_STORAGE_TYPE
      ) != state->storage_types || VECTOR_ELT(state->dependencies, 0) !=
      state->dependency_ids || VECTOR_ELT(state->dependencies, 1) !=
      state->dependency_on || VECTOR_ELT(state->dependencies, 2) !=
      state->conditions) {
    return FALSE;
  }
  if (!planning_strings_match(state)) {
    return FALSE;
  }
  for (R_xlen_t parameter = 0;
       parameter < state->parameter_count;
       ++parameter) {
    if (VECTOR_ELT(
        state->data,
        state->param_columns[parameter]
      ) != VECTOR_ELT(state->columns, parameter)) {
      return FALSE;
    }
  }
  for (R_xlen_t edge = 0; edge < state->dependency_count; ++edge) {
    if (VECTOR_ELT(state->conditions, edge) !=
        state->edges[edge].condition) {
      return FALSE;
    }
  }
  return TRUE;
}

static int exact_table_surface(SEXP table,
    const char *const *expected_names, R_xlen_t column_count,
    R_xlen_t *work_since_interrupt) {
  static const char *const classes[] = {"data.table", "data.frame"};
  static const char *const attributes[] = {
    "names", "row.names", "class", ".internal.selfref", "sorted", "index"
  };
  if (TYPEOF(table) != VECSXP || ALTREP(table) ||
      XLENGTH(table) != column_count || !paradox_api_has_only_attributes(
        table,
        attributes,
        6
      )) {
    return FALSE;
  }
  SEXP names = Rf_getAttrib(table, R_NamesSymbol);
  SEXP observed_classes = Rf_getAttrib(table, R_ClassSymbol);
  return paradox_api_has_no_attributes(names) &&
    paradox_api_has_no_attributes(observed_classes) &&
    paradox_domain_exact_string_vector(
      names,
      expected_names,
      column_count,
      work_since_interrupt
    ) && paradox_domain_exact_string_vector(
      observed_classes,
      classes,
      2,
      work_since_interrupt
    );
}

/* Final, allocation-free execution validation. It runs after the last
 * allocation-capable surface authentication so neither a GC finalizer that
 * changes data/conditions nor one that changes S3/R6 dispatch can open a gap
 * between admission and the second simulation. */
static int current_execution_objects_are_exact(
    const design_dependency_state_t *state,
    R_xlen_t *work_since_interrupt) {
  static const char *const parameter_names[] = {
    "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
    "levels", "special_vals", "default", "storage_type"
  };
  static const char *const dependency_names[] = {"id", "on", "cond"};
  static const char *const table_classes[] = {"data.table", "data.frame"};
  static const char *const table_attributes[] = {
    "names", "row.names", "class", ".internal.selfref", "sorted", "index"
  };
  SEXP data_classes = Rf_getAttrib(state->data, R_ClassSymbol);
  if (!paradox_api_has_only_attributes(state->data, table_attributes, 6) ||
      !paradox_api_has_no_attributes(data_classes) ||
      !paradox_domain_exact_string_vector(
        data_classes,
        table_classes,
        2,
        work_since_interrupt
      ) || !exact_table_surface(
        state->params,
        parameter_names,
        PARADOX_DOMAIN_TAGS,
        work_since_interrupt
      ) || !exact_table_surface(
        state->dependencies,
        dependency_names,
        3,
        work_since_interrupt
      )) {
    return FALSE;
  }

  SEXP classes = VECTOR_ELT(state->params, PARADOX_DOMAIN_CLS);
  if (TYPEOF(classes) != STRSXP || ALTREP(classes) ||
      XLENGTH(classes) != state->parameter_count ||
      !paradox_api_has_no_attributes(classes)) {
    return FALSE;
  }
  for (R_xlen_t parameter = 0;
       parameter < state->parameter_count;
       ++parameter) {
    SEXPTYPE expected_type;
    SEXP column = VECTOR_ELT(state->columns, parameter);
    if (!exact_storage(
        STRING_ELT(classes, parameter),
        STRING_ELT(state->stable_storage_types, parameter),
        &expected_type
      ) || (SEXPTYPE) TYPEOF(column) != expected_type || ALTREP(column) ||
        Rf_isObject(column) || XLENGTH(column) != state->row_count ||
        !paradox_api_has_no_attributes(column) ||
        aliases_table_attribute(column, state->data) ||
        aliases_table_storage(column, state->params) ||
        aliases_table_storage(column, state->dependencies) || VECTOR_ELT(
          state->data,
          state->param_columns[parameter]
        ) != column) {
      return FALSE;
    }
    for (R_xlen_t prior = 0; prior < parameter; ++prior) {
      if (column == VECTOR_ELT(state->columns, prior)) {
        return FALSE;
      }
    }
  }

  for (R_xlen_t edge = 0; edge < state->dependency_count; ++edge) {
    paradox_builtin_condition_kind_t kind;
    SEXP rhs;
    SEXP condition = VECTOR_ELT(state->conditions, edge);
    SEXP parent = VECTOR_ELT(state->columns, state->edges[edge].parent);
    if (condition != state->edges[edge].condition ||
        !paradox_builtin_condition_exact(
          condition,
          &kind,
          &rhs,
          work_since_interrupt
        ) || kind != state->edges[edge].kind ||
        rhs != state->edges[edge].rhs ||
        !paradox_builtin_condition_column_supported(
          parent,
          rhs,
          work_since_interrupt
        )) {
      return FALSE;
    }
    for (R_xlen_t column = 0;
         column < state->parameter_count;
         ++column) {
      if (aliases_condition_storage(
          VECTOR_ELT(state->columns, column),
          condition
        )) {
        return FALSE;
      }
    }
  }
  return TRUE;
}

static int current_shapes_are_exact(const design_dependency_state_t *state,
    R_xlen_t *work_since_interrupt) {
  SEXP names;
  paradox_domain_params_t params;
  paradox_domain_dependencies_t dependencies;
  R_xlen_t unused_row = 0;
  if (!exact_data_table_shell(
      state->data,
      &names,
      work_since_interrupt
    ) || names != state->data_names || !paradox_domain_validate_params(
      state->params,
      R_NilValue,
      TRUE,
      &params,
      &unused_row,
      work_since_interrupt
    ) || params.ids != state->param_ids ||
      params.row_count != state->parameter_count ||
      !paradox_domain_validate_dependencies(
        state->dependencies,
        &dependencies,
        work_since_interrupt
      ) || dependencies.ids != state->dependency_ids ||
      dependencies.on != state->dependency_on ||
      dependencies.conditions != state->conditions ||
      dependencies.row_count != state->dependency_count) {
    return FALSE;
  }

  return current_execution_objects_are_exact(
    state,
    work_since_interrupt
  );
}

static int value_is_missing(SEXP column, R_xlen_t row) {
  switch ((SEXPTYPE) TYPEOF(column)) {
  case LGLSXP:
    return LOGICAL_ELT(column, row) == NA_LOGICAL;
  case INTSXP:
    return INTEGER_ELT(column, row) == NA_INTEGER;
  case REALSXP:
    return ISNAN(REAL_ELT(column, row));
  case STRSXP:
    return STRING_ELT(column, row) == NA_STRING;
  default:
    return TRUE;
  }
}

static int inactive_bit(const design_dependency_state_t *state,
    const unsigned char *inactive, R_xlen_t parameter, R_xlen_t row) {
  const R_xlen_t slot = state->mask_columns[parameter];
  if (slot == R_XLEN_T_MAX) {
    return FALSE;
  }
  const size_t bit = (size_t) slot * (size_t) state->row_count +
    (size_t) row;
  return (inactive[bit >> 3] & (unsigned char) (1U << (bit & 7U))) != 0;
}

static void set_inactive_bit(const design_dependency_state_t *state,
    unsigned char *inactive, R_xlen_t parameter, R_xlen_t row) {
  const R_xlen_t slot = state->mask_columns[parameter];
  if (slot == R_XLEN_T_MAX) {
    return;
  }
  const size_t bit = (size_t) slot * (size_t) state->row_count +
    (size_t) row;
  inactive[bit >> 3] |= (unsigned char) (1U << (bit & 7U));
}

static int simulate_dependencies(const design_dependency_state_t *state,
    unsigned char *inactive, R_xlen_t *counts, SEXP output_rows,
    const R_xlen_t *expected_counts,
    R_xlen_t *work_since_interrupt) {
  if (state->mask_byte_count != 0) {
    memset(inactive, 0, state->mask_byte_count);
  }

  for (R_xlen_t position = 0;
       position < state->dependency_count;
       ++position) {
    const R_xlen_t edge_index = state->edge_order[position];
    const design_dependency_edge_t *edge = &state->edges[edge_index];
    SEXP parent = VECTOR_ELT(state->columns, edge->parent);
    R_xlen_t count = 0;
    SEXP rows = output_rows == R_NilValue
      ? R_NilValue
      : VECTOR_ELT(output_rows, position);
    for (R_xlen_t row = 0; row < state->row_count; ++row) {
      paradox_domain_account_work(work_since_interrupt);
      const int parent_inactive =
        inactive_bit(state, inactive, edge->parent, row) ||
        value_is_missing(parent, row);
      const int satisfied = paradox_builtin_condition_element_matches(
        parent,
        row,
        edge->rhs,
        work_since_interrupt
      );
      if (parent_inactive || !satisfied) {
        set_inactive_bit(state, inactive, edge->child, row);
        if (rows != R_NilValue) {
          if (count >= XLENGTH(rows)) {
            return FALSE;
          }
          INTEGER(rows)[count] = (int) row + 1;
        }
        ++count;
      }
    }
    if ((expected_counts != NULL && count != expected_counts[position]) ||
        (rows != R_NilValue && count != XLENGTH(rows))) {
      return FALSE;
    }
    counts[position] = count;
  }
  return TRUE;
}

static SEXP typed_missing(SEXP storage) {
  if (paradox_domain_string_is(storage, "numeric")) {
    return Rf_ScalarReal(NA_REAL);
  }
  if (paradox_domain_string_is(storage, "integer")) {
    return Rf_ScalarInteger(NA_INTEGER);
  }
  if (paradox_domain_string_is(storage, "character")) {
    return Rf_ScalarString(NA_STRING);
  }
  if (paradox_domain_string_is(storage, "logical")) {
    return Rf_ScalarLogical(NA_LOGICAL);
  }
  return R_NilValue;
}

static int allocate_output(design_dependency_state_t *state, SEXP roots,
    const R_xlen_t *counts, SEXP *output) {
  static const char *const output_names[] = {"rows", "columns", "values"};
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
  SEXP rows = PROTECT(Rf_allocVector(VECSXP, state->dependency_count));
  SEXP columns = PROTECT(Rf_allocVector(STRSXP, state->dependency_count));
  SEXP values = PROTECT(Rf_allocVector(VECSXP, state->dependency_count));
  for (R_xlen_t index = 0; index < 3; ++index) {
    SET_STRING_ELT(names, index, Rf_mkChar(output_names[index]));
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  SET_VECTOR_ELT(result, 0, rows);
  SET_VECTOR_ELT(result, 1, columns);
  SET_VECTOR_ELT(result, 2, values);
  SET_VECTOR_ELT(roots, DESIGN_DEPENDENCY_ROOT_OUTPUT, result);
  SET_VECTOR_ELT(roots, DESIGN_DEPENDENCY_ROOT_OUTPUT_ROWS, rows);
  SET_VECTOR_ELT(roots, DESIGN_DEPENDENCY_ROOT_OUTPUT_COLUMNS, columns);
  SET_VECTOR_ELT(roots, DESIGN_DEPENDENCY_ROOT_OUTPUT_VALUES, values);

  for (R_xlen_t position = 0;
       position < state->dependency_count;
       ++position) {
    const R_xlen_t edge_index = state->edge_order[position];
    const R_xlen_t child = state->edges[edge_index].child;
    SEXP edge_rows = PROTECT(Rf_allocVector(INTSXP, counts[position]));
    SEXP missing = PROTECT(typed_missing(STRING_ELT(
      state->stable_storage_types,
      child
    )));
    if (missing == R_NilValue) {
      UNPROTECT(7);
      return FALSE;
    }
    SET_VECTOR_ELT(rows, position, edge_rows);
    SET_STRING_ELT(
      columns,
      position,
      STRING_ELT(state->stable_param_ids, child)
    );
    SET_VECTOR_ELT(values, position, missing);
    UNPROTECT(2);
  }
  *output = result;
  UNPROTECT(5);
  return TRUE;
}

SEXP paradox_design_dependency_runtime(SEXP mlr3misc_version) {
  if (topology_state == DESIGN_DEPENDENCY_TOPOLOGY_UNCONFIGURED) {
    /* The native planner clones topo_sort()'s stable layer/tie semantics.
     * Seal the lane to source versions reviewed with this implementation;
     * unknown older or future implementations retain the complete R path. */
    const int reviewed = TYPEOF(mlr3misc_version) == STRSXP &&
      !ALTREP(mlr3misc_version) && XLENGTH(mlr3misc_version) == 1 &&
      paradox_api_has_no_attributes(mlr3misc_version) &&
      (paradox_domain_string_is(
          STRING_ELT(mlr3misc_version, 0),
          "0.18.0"
        ) || paradox_domain_string_is(
          STRING_ELT(mlr3misc_version, 0),
          "0.22.0"
        ));
    topology_state = reviewed
      ? DESIGN_DEPENDENCY_TOPOLOGY_ENABLED
      : DESIGN_DEPENDENCY_TOPOLOGY_DISABLED;
  }
  return Rf_ScalarLogical(
    topology_state == DESIGN_DEPENDENCY_TOPOLOGY_ENABLED
  );
}

SEXP paradox_design_dependency_plan_builtin(SEXP data, SEXP param_set) {
  if (topology_state != DESIGN_DEPENDENCY_TOPOLOGY_ENABLED) {
    return R_NilValue;
  }
  PROTECT(data);
  PROTECT(param_set);
  SEXP roots = PROTECT(Rf_allocVector(
    VECSXP,
    DESIGN_DEPENDENCY_ROOT_COUNT
  ));
  SET_VECTOR_ELT(roots, DESIGN_DEPENDENCY_ROOT_DATA, data);
  SET_VECTOR_ELT(roots, DESIGN_DEPENDENCY_ROOT_PARAM_SET, param_set);

  SEXP namespace_environment = PROTECT(
    paradox_api_registered_namespace("paradox")
  );
  if (TYPEOF(namespace_environment) != ENVSXP ||
      !canonical_execution_surface(param_set, namespace_environment)) {
    UNPROTECT(4);
    return R_NilValue;
  }

  design_dependency_state_t state = {
    .data = data,
    .param_set = param_set
  };
  R_xlen_t work_since_interrupt = 0;
  if (!load_private_state(&state, roots) ||
      !load_param_state(&state, roots, &work_since_interrupt) ||
      !load_dependency_state(&state, roots, &work_since_interrupt) ||
      !load_data_state(&state, roots, &work_since_interrupt) ||
      !plan_edges(&state, roots, &work_since_interrupt)) {
    UNPROTECT(4);
    return R_NilValue;
  }
  build_mask_layout(&state);
  if (!build_topological_order(&state, &work_since_interrupt)) {
    UNPROTECT(4);
    return R_NilValue;
  }

  if (state.mask_column_count != 0 &&
      (state.row_count > R_XLEN_T_MAX / state.mask_column_count ||
       (uint64_t) state.row_count >
         SIZE_MAX / (uint64_t) state.mask_column_count)) {
    UNPROTECT(4);
    return R_NilValue;
  }
  const size_t mask_bits = (size_t) state.mask_column_count *
    (size_t) state.row_count;
  if (mask_bits > SIZE_MAX - 7U) {
    UNPROTECT(4);
    return R_NilValue;
  }
  state.mask_byte_count = (mask_bits + 7U) >> 3;
  if (state.mask_byte_count > DESIGN_DEPENDENCY_MASK_LIMIT_BYTES ||
      state.mask_byte_count > (size_t) R_XLEN_T_MAX) {
    UNPROTECT(4);
    return R_NilValue;
  }
  unsigned char *inactive = paradox_temporary_alloc(
    (R_xlen_t) state.mask_byte_count,
    sizeof(*inactive)
  );
  R_xlen_t *counts = paradox_temporary_alloc(
    state.dependency_count,
    sizeof(*counts)
  );
  if (!simulate_dependencies(
      &state,
      inactive,
      counts,
      R_NilValue,
      NULL,
      &work_since_interrupt
    )) {
    UNPROTECT(4);
    return R_NilValue;
  }
  R_xlen_t plan_index_count = 0;
  for (R_xlen_t edge = 0; edge < state.dependency_count; ++edge) {
    if (counts[edge] >
        DESIGN_DEPENDENCY_PLAN_INDEX_LIMIT - plan_index_count) {
      UNPROTECT(4);
      return R_NilValue;
    }
    plan_index_count += counts[edge];
  }
  R_xlen_t *final_counts = paradox_temporary_alloc(
    state.dependency_count,
    sizeof(*final_counts)
  );

  SEXP output;
  if (!allocate_output(&state, roots, counts, &output) ||
      !current_shapes_are_exact(&state, &work_since_interrupt) ||
      !canonical_execution_surface(param_set, namespace_environment) ||
      !current_shapes_are_exact(&state, &work_since_interrupt) ||
      !canonical_execution_surface(param_set, namespace_environment) ||
      !current_state_matches(&state) ||
      !current_execution_objects_are_exact(
        &state,
        &work_since_interrupt
      )) {
    UNPROTECT(4);
    return R_NilValue;
  }

  SEXP output_rows = VECTOR_ELT(output, 0);
  if (!simulate_dependencies(
      &state,
      inactive,
      final_counts,
      output_rows,
      counts,
      &work_since_interrupt
    )) {
    UNPROTECT(4);
    return R_NilValue;
  }
  UNPROTECT(4);
  return output;
}
