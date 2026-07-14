#include <limits.h>
#include <math.h>
#include <stddef.h>

#include "paradox.h"

#include "paramset_domain_common.h"
#include "paramset_params_internal.h"
#include "r_api_compat.h"
#include "r_utils.h"

typedef struct {
  SEXPTYPE type;
  const int *integer_values;
  const double *real_values;
} params_match_vector_t;

/* One nested vector occupies a single slot in each caller's existing root
 * plan.  Keeping the exact parsed children here matters even when their
 * source tables are rooted: a finalizer can replace a table column or names
 * attribute while a later allocation is in flight. */
enum params_state_root_slot {
  PARAMS_ROOT_PARAMS_SOURCE = 0,
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

int paradox_params_exact_base_param_set(SEXP self,
    R_xlen_t *work_since_interrupt) {
  static const char *const classes[] = {"ParamSet", "R6"};
  return TYPEOF(self) == ENVSXP && paradox_domain_exact_string_vector(
    Rf_getAttrib(self, R_ClassSymbol),
    classes,
    2,
    work_since_interrupt
  );
}

static int exact_wrapper_call(SEXP function, const char *method_name,
    const char *argument_name) {
  SEXP body = PROTECT(paradox_api_closure_expression(function));
  if (TYPEOF(body) != LANGSXP || CAR(body) != Rf_install(method_name)) {
    UNPROTECT(1);
    return FALSE;
  }
  static const char *const fixed_arguments[] = {"self", "private", "super"};
  SEXP argument = CDR(body);
  for (R_xlen_t index = 0; index < 3; ++index) {
    SEXP symbol = Rf_install(fixed_arguments[index]);
    if (argument == R_NilValue || TAG(argument) != symbol ||
        CAR(argument) != symbol) {
      UNPROTECT(1);
      return FALSE;
    }
    argument = CDR(argument);
  }
  SEXP final_symbol = Rf_install(argument_name);
  if (argument == R_NilValue || TAG(argument) != final_symbol ||
      CAR(argument) != final_symbol || CDR(argument) != R_NilValue) {
    UNPROTECT(1);
    return FALSE;
  }

  SEXP formals = PROTECT(paradox_api_closure_formals(function));
  const int exact = formals != R_NilValue && TAG(formals) == final_symbol &&
    CAR(formals) == R_MissingArg && CDR(formals) == R_NilValue;
  UNPROTECT(2);
  return exact;
}

static int exact_private_getter_call(SEXP function) {
  SEXP body = PROTECT(paradox_api_closure_expression(function));
  SEXP target_symbol = Rf_install(".__ParamSet__.get_values");
  if (TYPEOF(body) != LANGSXP || CAR(body) != target_symbol) {
    UNPROTECT(1);
    return FALSE;
  }
  static const char *const fixed_arguments[] = {"self", "private", "super"};
  SEXP argument = CDR(body);
  for (R_xlen_t index = 0; index < 3; ++index) {
    SEXP symbol = Rf_install(fixed_arguments[index]);
    if (argument == R_NilValue || TAG(argument) != symbol ||
        CAR(argument) != symbol) {
      UNPROTECT(1);
      return FALSE;
    }
    argument = CDR(argument);
  }
  SEXP formals = PROTECT(paradox_api_closure_formals(function));
  const int exact = argument == R_NilValue && formals == R_NilValue;
  UNPROTECT(2);
  return exact;
}

static int exact_active_member_on(SEXP binding_environment,
    SEXP expected_self, SEXP private_environment,
    const char *member_name, const char *method_name,
    const char *argument_name, int require_unbound_super,
    SEXP *captured_super_out, SEXP *wrapper_environment_out,
    R_xlen_t *work_since_interrupt) {
  SEXP member_symbol = Rf_install(member_name);
  if (TYPEOF(binding_environment) != ENVSXP ||
      !R_existsVarInFrame(binding_environment, member_symbol) ||
      !R_BindingIsActive(member_symbol, binding_environment)) {
    return FALSE;
  }
  SEXP function = PROTECT(R_ActiveBindingFunction(
    member_symbol,
    binding_environment
  ));
  int protected_count = 1;
  if (TYPEOF(function) != CLOSXP || !exact_wrapper_call(
      function,
      method_name,
      argument_name
    )) {
    UNPROTECT(protected_count);
    return FALSE;
  }
  SEXP environment = PROTECT(paradox_api_closure_environment(function));
  ++protected_count;
  SEXP namespace_environment = PROTECT(
    paradox_api_registered_namespace("paradox")
  );
  ++protected_count;
  SEXP method_symbol = Rf_install(method_name);
  SEXP super_symbol = Rf_install("super");
  if (TYPEOF(environment) != ENVSXP ||
      TYPEOF(namespace_environment) != ENVSXP ||
      paradox_api_parent_environment(environment) != namespace_environment ||
      paradox_domain_local_value(environment, "self") != expected_self ||
      paradox_domain_local_value(environment, "private") !=
      private_environment ||
      R_existsVarInFrame(environment, method_symbol) ||
      (require_unbound_super &&
        R_existsVarInFrame(environment, super_symbol)) ||
      (!require_unbound_super &&
        !R_existsVarInFrame(environment, super_symbol))) {
    UNPROTECT(protected_count);
    return FALSE;
  }
  SEXP captured_super = PROTECT(require_unbound_super
    ? R_UnboundValue
    : paradox_domain_local_value(environment, "super"));
  ++protected_count;
  if (!require_unbound_super && TYPEOF(captured_super) != ENVSXP) {
    UNPROTECT(protected_count);
    return FALSE;
  }

  SEXP active = PROTECT(paradox_domain_local_value(
    environment,
    ".__active__"
  ));
  ++protected_count;
  SEXP active_names = PROTECT(Rf_getAttrib(active, R_NamesSymbol));
  ++protected_count;
  if (TYPEOF(active) != VECSXP || ALTREP(active) ||
      TYPEOF(active_names) != STRSXP || ALTREP(active_names) ||
      XLENGTH(active_names) != XLENGTH(active)) {
    UNPROTECT(protected_count);
    return FALSE;
  }
  R_xlen_t found = R_XLEN_T_MAX;
  for (R_xlen_t index = 0; index < XLENGTH(active_names); ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if (paradox_domain_string_is(STRING_ELT(active_names, index), member_name)) {
      if (found != R_XLEN_T_MAX) {
        UNPROTECT(protected_count);
        return FALSE;
      }
      found = index;
    }
  }
  /* R6 cloning can duplicate an inherited wrapper separately from the
   * `.__active__` entry.  They remain identical closures over the cloned
   * enclosure; an actual replacement fails this identity/body check. */
  if (found == R_XLEN_T_MAX) {
    UNPROTECT(protected_count);
    return FALSE;
  }
  SEXP registered_function = PROTECT(VECTOR_ELT(active, found));
  ++protected_count;
  const Rboolean identical = R_compute_identical(
    registered_function,
    function,
    IDENT_USE_CLOENV
  );
  if (!identical) {
    UNPROTECT(protected_count);
    return FALSE;
  }

  if (captured_super_out != NULL) {
    *captured_super_out = captured_super;
  }
  if (wrapper_environment_out != NULL) {
    *wrapper_environment_out = environment;
  }
  UNPROTECT(protected_count);
  return TRUE;
}

int paradox_params_canonical_active_member(SEXP self,
    SEXP private_environment, const char *member_name,
    const char *method_name, const char *argument_name,
    const char *super_method_name,
    R_xlen_t *work_since_interrupt) {
  SEXP captured_super = R_UnboundValue;
  if (!exact_active_member_on(
      self,
      self,
      private_environment,
      member_name,
      method_name,
      argument_name,
      super_method_name == NULL,
      &captured_super,
      NULL,
      work_since_interrupt
    )) {
    return FALSE;
  }
  if (super_method_name == NULL) {
    return TRUE;
  }

  /* An overridden R6 member receives a superclass proxy in its generated
   * closure.  The proxy is mutable, so checking only the captured `self` and
   * `private` values would incorrectly admit a forged `super$params`. */
  if (TYPEOF(captured_super) != ENVSXP) {
    return FALSE;
  }
  PROTECT(captured_super);
  SEXP super_enclosure = PROTECT(paradox_domain_local_value(
    captured_super,
    ".__enclos_env__"
  ));
  SEXP wrapper_environment = R_UnboundValue;
  const int exact = TYPEOF(super_enclosure) == ENVSXP &&
    exact_active_member_on(
      captured_super,
      self,
      private_environment,
      member_name,
      super_method_name,
      argument_name,
      TRUE,
      NULL,
      &wrapper_environment,
      work_since_interrupt
    ) && wrapper_environment == super_enclosure;
  UNPROTECT(2);
  return exact;
}

int paradox_params_canonical_private_getter(SEXP self,
    SEXP private_environment) {
  SEXP enclosure = PROTECT(paradox_domain_local_value(
    self,
    ".__enclos_env__"
  ));
  SEXP getter_symbol = Rf_install(".get_values");
  if (TYPEOF(enclosure) != ENVSXP ||
      !R_existsVarInFrame(private_environment, getter_symbol) ||
      R_BindingIsActive(getter_symbol, private_environment) ||
      !R_BindingIsLocked(getter_symbol, private_environment)) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP function = PROTECT(paradox_domain_local_value(
    private_environment,
    ".get_values"
  ));
  if (TYPEOF(function) != CLOSXP || !exact_private_getter_call(function)) {
    UNPROTECT(2);
    return FALSE;
  }
  SEXP environment = PROTECT(paradox_api_closure_environment(function));
  SEXP namespace_environment = PROTECT(
    paradox_api_registered_namespace("paradox")
  );
  SEXP target_symbol = Rf_install(".__ParamSet__.get_values");
  SEXP super_symbol = Rf_install("super");
  const int canonical = TYPEOF(environment) == ENVSXP &&
    environment == enclosure && TYPEOF(namespace_environment) == ENVSXP &&
    paradox_api_parent_environment(environment) == namespace_environment &&
    paradox_domain_local_value(environment, "self") == self &&
    paradox_domain_local_value(environment, "private") ==
      private_environment &&
    !R_existsVarInFrame(environment, target_symbol) &&
    !R_existsVarInFrame(environment, super_symbol);
  UNPROTECT(4);
  return canonical;
}

int paradox_params_supported_table_attributes(SEXP table, int allow_sorted) {
  static const char *const supported[] = {
    "names", "class", "row.names", "index", ".internal.selfref", "sorted"
  };
  return paradox_api_has_only_attributes(
    table,
    supported,
    allow_sorted ? 6 : 5
  );
}

static int exact_data_frame_row_names(SEXP table, R_xlen_t row_count,
    R_xlen_t *work_since_interrupt) {
  SEXP row_names = Rf_getAttrib(table, R_RowNamesSymbol);
  /* The public getter intentionally expands data-frame compact row names
   * (`c(NA, -n)`) to an ALTREP `1:n` vector. Row names do not participate in
   * this kernel: the authenticated ordinary columns provide the row count and
   * the result receives fresh compact row names. Never ask a callback-capable
   * row-name facade for Length/Elt merely to validate unused metadata. */
  if (row_count > INT_MAX || TYPEOF(row_names) != INTSXP ||
      !paradox_api_has_no_attributes(row_names)) {
    return FALSE;
  }
  if (ALTREP(row_names)) {
    return TRUE;
  }
  if (XLENGTH(row_names) != row_count) {
    return FALSE;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    if (INTEGER_ELT(row_names, row) != (int) row + 1) {
      return FALSE;
    }
  }
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
  const params_match_vector_t result = {
    type,
    type == INTSXP ? INTEGER_RO(value) : NULL,
    type == REALSXP ? REAL_RO(value) : NULL
  };
  return result;
}

static R_xlen_t match_at(const params_match_vector_t *matches,
    R_xlen_t index) {
  if (matches->type == INTSXP) {
    const int value = matches->integer_values[index];
    return value == NA_INTEGER || value <= 0 ? 0 : (R_xlen_t) value;
  }
  const double value = matches->real_values[index];
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

static int load_private_state(SEXP private_environment,
    paradox_params_state_t *state, SEXP roots, R_xlen_t roots_offset,
    R_xlen_t *work_since_interrupt) {
  const int retain = roots != R_NilValue;
  if (retain && (TYPEOF(roots) != VECSXP || roots_offset > XLENGTH(roots) ||
      XLENGTH(roots) - roots_offset < 1)) {
    Rf_error("Internal error: invalid ParamSet params root plan");
  }

  SEXP state_roots = PROTECT(Rf_allocVector(VECSXP, PARAMS_ROOT_COUNT));
  if (retain) {
    SET_VECTOR_ELT(roots, roots_offset, state_roots);
  }
  state->params_sexp = paradox_domain_local_value(
    private_environment,
    ".params"
  );
  if (state->params_sexp != R_UnboundValue) {
    SET_VECTOR_ELT(
      state_roots,
      PARAMS_ROOT_PARAMS_SOURCE,
      state->params_sexp
    );
  }
  state->tags_sexp = paradox_domain_local_value(private_environment, ".tags");
  if (state->tags_sexp != R_UnboundValue) {
    SET_VECTOR_ELT(state_roots, PARAMS_ROOT_TAGS_SOURCE, state->tags_sexp);
  }
  state->trafos_sexp = paradox_domain_local_value(
    private_environment,
    ".trafos"
  );
  if (state->trafos_sexp != R_UnboundValue) {
    SET_VECTOR_ELT(
      state_roots,
      PARAMS_ROOT_TRAFOS_SOURCE,
      state->trafos_sexp
    );
  }
  state->dependencies_sexp = paradox_domain_local_value(
    private_environment,
    ".deps"
  );
  if (state->dependencies_sexp != R_UnboundValue) {
    SET_VECTOR_ELT(
      state_roots,
      PARAMS_ROOT_DEPENDENCIES_SOURCE,
      state->dependencies_sexp
    );
  }
  state->values_sexp = paradox_domain_local_value(
    private_environment,
    ".values"
  );
  if (state->values_sexp != R_UnboundValue) {
    SET_VECTOR_ELT(
      state_roots,
      PARAMS_ROOT_VALUES_SOURCE,
      state->values_sexp
    );
  }

  int valid = FALSE;
  R_xlen_t unused_row = 0;
  if (state->params_sexp == R_UnboundValue ||
      state->tags_sexp == R_UnboundValue ||
      state->trafos_sexp == R_UnboundValue ||
      state->dependencies_sexp == R_UnboundValue ||
      state->values_sexp == R_UnboundValue ||
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
  UNPROTECT(1);
  return valid;
}

int paradox_params_load_private_state(SEXP private_environment,
    paradox_params_state_t *state, R_xlen_t *work_since_interrupt) {
  return load_private_state(
    private_environment,
    state,
    R_NilValue,
    0,
    work_since_interrupt
  );
}

int paradox_params_load_private_state_rooted(SEXP private_environment,
    paradox_params_state_t *state, SEXP roots, R_xlen_t roots_offset,
    R_xlen_t *work_since_interrupt) {
  return load_private_state(
    private_environment,
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
  if (!paradox_params_exact_base_param_set(
      self,
      &work_since_interrupt
    ) || !paradox_domain_owns_private_environment(
      self,
      private_environment
    ) || !paradox_params_canonical_active_member(
      self,
      private_environment,
      "params",
      ".__ParamSet__params",
      "rhs",
      NULL,
      &work_since_interrupt
    ) || !paradox_params_canonical_active_member(
      self,
      private_environment,
      "tags",
      ".__ParamSet__tags",
      "v",
      NULL,
      &work_since_interrupt
    ) || !paradox_params_canonical_active_member(
      self,
      private_environment,
      "deps",
      ".__ParamSet__deps",
      "v",
      NULL,
      &work_since_interrupt
    ) || !paradox_params_canonical_active_member(
      self,
      private_environment,
      "values",
      ".__ParamSet__values",
      "xs",
      NULL,
      &work_since_interrupt
    ) || !paradox_params_canonical_private_getter(
      self,
      private_environment
    )) {
    return R_NilValue;
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
    return R_NilValue;
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
    return R_NilValue;
  }
  UNPROTECT(2);
  return result;
}
