#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <string.h>

#include "paradox.h"

#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

#define DESIGN_TRAFO_MAX_PLAN_ROWS ((R_xlen_t) (2 * 1024 * 1024))
#define DESIGN_TRAFO_MAX_PLAN_ELEMENTS ((R_xlen_t) (2 * 1024 * 1024))

/*
 * Fuse the pure, package-generated log-scale transformations used by Design
 * rows without evaluating an R closure for every scalar.  Admission is
 * deliberately all-or-nothing: an extra transformation, an overridden R6
 * surface, a user callback, noncanonical storage, or a value whose integer
 * conversion would warn returns NULL before any callback has run.
 *
 * The output is completely allocated before the final object/input audit.
 * That final audit is allocation-free after its own root plans have been
 * created, so a GC finalizer cannot leave a stale mapping between validation
 * and the fill loop.
 */

typedef enum {
  DESIGN_TRAFO_NONE = 0,
  DESIGN_TRAFO_EXP,
  DESIGN_TRAFO_BOUNDED_INTEGER_EXP
} design_trafo_kind_t;

typedef struct {
  SEXP id;
  SEXP callback;
  SEXP callback_environment;
  SEXP lower_source;
  SEXP upper_source;
  double lower;
  double upper;
  design_trafo_kind_t kind;
} parameter_plan_t;

typedef struct {
  SEXP row;
  SEXP names;
  R_xlen_t size;
  R_xlen_t element_offset;
} row_plan_t;

typedef struct {
  SEXP source;
  SEXP name;
  SEXP output;
  double input;
  double real_result;
  int integer_result;
  design_trafo_kind_t kind;
} element_plan_t;

typedef struct {
  SEXP param_set;
  SEXP enclosure;
  SEXP private_environment;
  SEXP params_table;
  SEXP trafos_table;
  SEXP param_ids;
  SEXP cargo;
  SEXP trafo_ids;
  SEXP callbacks;
  R_xlen_t parameter_count;
  R_xlen_t trafo_count;
} trafo_state_t;

enum initial_root_slot {
  INITIAL_ROOT_PARAM_SET = 0,
  INITIAL_ROOT_ENCLOSURE,
  INITIAL_ROOT_PRIVATE,
  INITIAL_ROOT_PARAMS,
  INITIAL_ROOT_TRAFOS,
  INITIAL_ROOT_PARAM_IDS,
  INITIAL_ROOT_CARGO,
  INITIAL_ROOT_TRAFO_IDS,
  INITIAL_ROOT_CALLBACKS,
  INITIAL_ROOT_COUNT
};

enum final_root_slot {
  FINAL_ROOT_PARAMS = 0,
  FINAL_ROOT_TRAFOS,
  FINAL_ROOT_PARAM_IDS,
  FINAL_ROOT_CARGO,
  FINAL_ROOT_TRAFO_IDS,
  FINAL_ROOT_CALLBACKS,
  FINAL_ROOT_COUNT
};

static int ascii_nonempty(SEXP string) {
  if (string == NA_STRING || Rf_getCharCE(string) == CE_BYTES) {
    return FALSE;
  }
  const unsigned char *cursor = (const unsigned char *) CHAR(string);
  if (*cursor == (unsigned char) '\0') {
    return FALSE;
  }
  for (; *cursor != (unsigned char) '\0'; ++cursor) {
    if (*cursor > (unsigned char) 0x7f) {
      return FALSE;
    }
  }
  return TRUE;
}

static int exact_flag(SEXP value, int expected) {
  return TYPEOF(value) == LGLSXP && !ALTREP(value) &&
    XLENGTH(value) == 1 && paradox_api_has_no_attributes(value) &&
    LOGICAL_ELT(value, 0) == expected;
}

static int cargo_has_exact_logscale_marker(SEXP cargo) {
  if (TYPEOF(cargo) != VECSXP || ALTREP(cargo) || Rf_isObject(cargo) ||
      !paradox_api_has_single_attribute(cargo, "names")) {
    return FALSE;
  }
  SEXP names = Rf_getAttrib(cargo, R_NamesSymbol);
  if (TYPEOF(names) != STRSXP || ALTREP(names) ||
      XLENGTH(names) != XLENGTH(cargo) ||
      !paradox_api_has_no_attributes(names)) {
    return FALSE;
  }

  R_xlen_t marker = R_XLEN_T_MAX;
  for (R_xlen_t index = 0; index < XLENGTH(names); ++index) {
    SEXP name = STRING_ELT(names, index);
    if (!ascii_nonempty(name)) {
      return FALSE;
    }
    if (paradox_domain_string_is(name, "logscale")) {
      if (marker != R_XLEN_T_MAX) {
        return FALSE;
      }
      marker = index;
    }
  }
  return marker != R_XLEN_T_MAX &&
    exact_flag(VECTOR_ELT(cargo, marker), TRUE);
}

static int exact_unary_call(SEXP expression, const char *function,
    SEXP argument) {
  return TYPEOF(expression) == LANGSXP &&
    CAR(expression) == Rf_install(function) &&
    TYPEOF(CDR(expression)) == LISTSXP &&
    TAG(CDR(expression)) == R_NilValue &&
    CAR(CDR(expression)) == argument &&
    CDR(CDR(expression)) == R_NilValue;
}

static int exact_integer_logscale_body(SEXP body) {
  SEXP x = Rf_install("x");
  SEXP upper = Rf_install("upper");
  SEXP lower = Rf_install("lower");
  if (TYPEOF(body) != LANGSXP ||
      CAR(body) != Rf_install("as.integer") ||
      TYPEOF(CDR(body)) != LISTSXP ||
      TAG(CDR(body)) != R_NilValue ||
      CDR(CDR(body)) != R_NilValue) {
    return FALSE;
  }
  SEXP maximum = CAR(CDR(body));
  if (TYPEOF(maximum) != LANGSXP || CAR(maximum) != Rf_install("max")) {
    return FALSE;
  }
  SEXP maximum_arguments = CDR(maximum);
  if (TYPEOF(maximum_arguments) != LISTSXP ||
      TAG(maximum_arguments) != R_NilValue ||
      TYPEOF(CDR(maximum_arguments)) != LISTSXP ||
      TAG(CDR(maximum_arguments)) != R_NilValue ||
      CAR(CDR(maximum_arguments)) != lower ||
      CDR(CDR(maximum_arguments)) != R_NilValue) {
    return FALSE;
  }
  SEXP minimum = CAR(maximum_arguments);
  if (TYPEOF(minimum) != LANGSXP || CAR(minimum) != Rf_install("min")) {
    return FALSE;
  }
  SEXP minimum_arguments = CDR(minimum);
  if (TYPEOF(minimum_arguments) != LISTSXP ||
      TAG(minimum_arguments) != R_NilValue ||
      TYPEOF(CDR(minimum_arguments)) != LISTSXP ||
      TAG(CDR(minimum_arguments)) != R_NilValue ||
      CAR(CDR(minimum_arguments)) != upper ||
      CDR(CDR(minimum_arguments)) != R_NilValue) {
    return FALSE;
  }
  return exact_unary_call(CAR(minimum_arguments), "exp", x);
}

static int scalar_bound(SEXP value, double *result) {
  if (ALTREP(value) || !paradox_api_has_no_attributes(value) ||
      XLENGTH(value) != 1) {
    return FALSE;
  }
  if (TYPEOF(value) == REALSXP) {
    *result = REAL_ELT(value, 0);
  } else if (TYPEOF(value) == INTSXP) {
    const int integer = INTEGER_ELT(value, 0);
    if (integer == NA_INTEGER) {
      return FALSE;
    }
    *result = (double) integer;
  } else {
    return FALSE;
  }
  return !ISNAN(*result);
}

static SEXP stable_base_function(const char *name) {
  SEXP symbol = Rf_install(name);
  if (!R_existsVarInFrame(R_BaseEnv, symbol) ||
      R_BindingIsActive(symbol, R_BaseEnv)) {
    return R_UnboundValue;
  }
  return paradox_api_stable_local_value(R_BaseEnv, symbol);
}

static int lookup_resolves_to_base(SEXP environment, const char *name) {
  SEXP symbol = Rf_install(name);
  SEXP expected = stable_base_function(name);
  if (expected == R_UnboundValue || !Rf_isFunction(expected)) {
    return FALSE;
  }
  for (SEXP cursor = environment;
      TYPEOF(cursor) == ENVSXP && cursor != R_EmptyEnv;
      cursor = paradox_api_parent_environment(cursor)) {
    if (R_existsVarInFrame(cursor, symbol)) {
      return paradox_api_stable_local_value(cursor, symbol) == expected;
    }
  }
  return FALSE;
}

static int exact_integer_logscale_callback(SEXP callback,
    parameter_plan_t *plan) {
  if (TYPEOF(callback) != CLOSXP) {
    return FALSE;
  }
  SEXP formals = paradox_api_closure_formals(callback);
  if (TYPEOF(formals) != LISTSXP || TAG(formals) != Rf_install("x") ||
      CAR(formals) != R_MissingArg || CDR(formals) != R_NilValue ||
      !exact_integer_logscale_body(
        paradox_api_closure_expression(callback)
      )) {
    return FALSE;
  }

  SEXP environment = paradox_api_closure_environment(callback);
  SEXP namespace_environment = paradox_api_registered_namespace("paradox");
  if (TYPEOF(environment) != ENVSXP ||
      environment == R_GlobalEnv || environment == R_EmptyEnv ||
      environment == R_BaseEnv ||
      paradox_api_parent_environment(environment) != namespace_environment ||
      !lookup_resolves_to_base(environment, "as.integer") ||
      !lookup_resolves_to_base(environment, "max") ||
      !lookup_resolves_to_base(environment, "min") ||
      !lookup_resolves_to_base(environment, "exp")) {
    return FALSE;
  }

  SEXP lower_source = PROTECT(paradox_api_local_value(
    environment,
    Rf_install("lower")
  ));
  SEXP upper_source = PROTECT(paradox_api_local_value(
    environment,
    Rf_install("upper")
  ));
  double lower;
  double upper;
  if (lower_source == R_UnboundValue || upper_source == R_UnboundValue ||
      !scalar_bound(lower_source, &lower) ||
      !scalar_bound(upper_source, &upper) ||
      !R_FINITE(lower) || lower < 0.0 || upper < lower) {
    UNPROTECT(2);
    return FALSE;
  }

  plan->callback_environment = environment;
  plan->lower_source = lower_source;
  plan->upper_source = upper_source;
  plan->lower = lower;
  plan->upper = upper;
  plan->kind = DESIGN_TRAFO_BOUNDED_INTEGER_EXP;
  UNPROTECT(2);
  return TRUE;
}

static int same_double(double left, double right) {
  unsigned char left_bytes[sizeof(left)];
  unsigned char right_bytes[sizeof(right)];
  memcpy(left_bytes, &left, sizeof(left));
  memcpy(right_bytes, &right, sizeof(right));
  return memcmp(left_bytes, right_bytes, sizeof(left)) == 0;
}

static int integer_result(double input, double lower, double upper,
    int *result) {
  if (ISNAN(input)) {
    *result = NA_INTEGER;
    return TRUE;
  }
  double transformed = exp(input);
  if (transformed > upper) {
    transformed = upper;
  }
  if (transformed < lower) {
    transformed = lower;
  }
  /* as.integer() warns for finite overflow and infinities.  Decline here so
   * the historical R callback remains the sole producer of that warning. */
  if (!R_FINITE(transformed) || transformed > (double) INT_MAX ||
      transformed <= (double) INT_MIN) {
    return FALSE;
  }
  *result = (int) transformed;
  return TRUE;
}

static int load_initial_state(SEXP param_set, trafo_state_t *state,
    SEXP roots, R_xlen_t *work) {
  if (!paradox_param_set_design_trafo_auth(param_set)) {
    return FALSE;
  }
  state->param_set = param_set;
  SET_VECTOR_ELT(roots, INITIAL_ROOT_PARAM_SET, param_set);
  state->enclosure = paradox_domain_local_value(
    param_set,
    ".__enclos_env__"
  );
  if (state->enclosure == R_UnboundValue) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, INITIAL_ROOT_ENCLOSURE, state->enclosure);
  state->private_environment = paradox_domain_local_value(
    state->enclosure,
    "private"
  );
  if (state->private_environment == R_UnboundValue ||
      paradox_domain_local_value(
        state->private_environment,
        ".extra_trafo"
      ) != R_NilValue) {
    return FALSE;
  }
  SET_VECTOR_ELT(
    roots,
    INITIAL_ROOT_PRIVATE,
    state->private_environment
  );

  state->params_table = paradox_domain_local_value(
    state->private_environment,
    ".params"
  );
  state->trafos_table = paradox_domain_local_value(
    state->private_environment,
    ".trafos"
  );
  if (state->params_table == R_UnboundValue ||
      state->trafos_table == R_UnboundValue) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, INITIAL_ROOT_PARAMS, state->params_table);
  SET_VECTOR_ELT(roots, INITIAL_ROOT_TRAFOS, state->trafos_table);

  paradox_domain_params_t params;
  R_xlen_t unused = 0;
  if (!paradox_domain_validate_params(
      state->params_table,
      R_NilValue,
      TRUE,
      &params,
      &unused,
      work
    )) {
    return FALSE;
  }
  state->param_ids = params.ids;
  state->cargo = VECTOR_ELT(state->params_table, PARADOX_DOMAIN_CARGO);
  state->parameter_count = params.row_count;
  SET_VECTOR_ELT(roots, INITIAL_ROOT_PARAM_IDS, state->param_ids);
  SET_VECTOR_ELT(roots, INITIAL_ROOT_CARGO, state->cargo);

  paradox_domain_trafos_t trafos;
  if (!paradox_domain_validate_trafos(
      state->trafos_table,
      &trafos,
      work
    ) || trafos.row_count == 0) {
    return FALSE;
  }
  state->trafo_ids = trafos.ids;
  state->callbacks = trafos.values;
  state->trafo_count = trafos.row_count;
  SET_VECTOR_ELT(roots, INITIAL_ROOT_TRAFO_IDS, state->trafo_ids);
  SET_VECTOR_ELT(roots, INITIAL_ROOT_CALLBACKS, state->callbacks);
  return TRUE;
}

static int initialize_parameter_plan(const trafo_state_t *state,
    parameter_plan_t *plan, SEXP roots, R_xlen_t *work) {
  SEXP base_exp = stable_base_function("exp");
  if (base_exp == R_UnboundValue || TYPEOF(base_exp) != BUILTINSXP) {
    return FALSE;
  }
  for (R_xlen_t parameter = 0;
      parameter < state->parameter_count;
      ++parameter) {
    paradox_domain_account_work(work);
    SEXP id = STRING_ELT(state->param_ids, parameter);
    if (!ascii_nonempty(id)) {
      return FALSE;
    }
    plan[parameter] = (parameter_plan_t) {
      id,
      R_NilValue,
      R_NilValue,
      R_NilValue,
      R_NilValue,
      0.0,
      0.0,
      DESIGN_TRAFO_NONE
    };
    for (R_xlen_t earlier = 0; earlier < parameter; ++earlier) {
      if (plan[earlier].id == id ||
          strcmp(CHAR(plan[earlier].id), CHAR(id)) == 0) {
        return FALSE;
      }
    }
  }

  for (R_xlen_t trafo = 0; trafo < state->trafo_count; ++trafo) {
    paradox_domain_account_work(work);
    SEXP id = STRING_ELT(state->trafo_ids, trafo);
    if (!ascii_nonempty(id)) {
      return FALSE;
    }
    R_xlen_t parameter = R_XLEN_T_MAX;
    for (R_xlen_t candidate = 0;
        candidate < state->parameter_count;
        ++candidate) {
      if (id == plan[candidate].id || strcmp(
          CHAR(id),
          CHAR(plan[candidate].id)
        ) == 0) {
        parameter = candidate;
        break;
      }
    }
    if (parameter == R_XLEN_T_MAX ||
        plan[parameter].kind != DESIGN_TRAFO_NONE ||
        !cargo_has_exact_logscale_marker(
          VECTOR_ELT(state->cargo, parameter)
        )) {
      return FALSE;
    }
    SEXP callback = VECTOR_ELT(state->callbacks, trafo);
    plan[parameter].callback = callback;
    if (callback == base_exp) {
      plan[parameter].kind = DESIGN_TRAFO_EXP;
    } else if (!exact_integer_logscale_callback(
      callback,
      &plan[parameter]
    )) {
      return FALSE;
    }
    SET_VECTOR_ELT(roots, parameter * 4, callback);
    SET_VECTOR_ELT(
      roots,
      parameter * 4 + 1,
      plan[parameter].callback_environment
    );
    SET_VECTOR_ELT(roots, parameter * 4 + 2, plan[parameter].lower_source);
    SET_VECTOR_ELT(roots, parameter * 4 + 3, plan[parameter].upper_source);
  }
  return TRUE;
}

static R_xlen_t parameter_for_name(const parameter_plan_t *parameters,
    R_xlen_t parameter_count, SEXP name) {
  for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
    if (parameters[parameter].id == name || strcmp(
        CHAR(parameters[parameter].id),
        CHAR(name)
      ) == 0) {
      return parameter;
    }
  }
  return R_XLEN_T_MAX;
}

static int initialize_input_plan(SEXP xs,
    const parameter_plan_t *parameters, R_xlen_t parameter_count,
    row_plan_t *rows, element_plan_t *elements, R_xlen_t total_elements,
    R_xlen_t row_count, SEXP roots, R_xlen_t *work) {
  if (XLENGTH(xs) != row_count) {
    return FALSE;
  }
  R_xlen_t element = 0;
  for (R_xlen_t row_index = 0; row_index < row_count; ++row_index) {
    paradox_domain_account_work(work);
    SEXP row = VECTOR_ELT(xs, row_index);
    if (TYPEOF(row) != VECSXP || ALTREP(row) || Rf_isObject(row) ||
        !paradox_api_has_single_attribute(row, "names")) {
      return FALSE;
    }
    SEXP names = Rf_getAttrib(row, R_NamesSymbol);
    const R_xlen_t size = XLENGTH(row);
    if (TYPEOF(names) != STRSXP || ALTREP(names) ||
        XLENGTH(names) != size || !paradox_api_has_no_attributes(names)) {
      return FALSE;
    }
    rows[row_index] = (row_plan_t) {row, names, size, element};
    SET_VECTOR_ELT(roots, row_index * 2, row);
    SET_VECTOR_ELT(roots, row_index * 2 + 1, names);
    for (R_xlen_t column = 0; column < size; ++column) {
      paradox_domain_account_work(work);
      SEXP name = STRING_ELT(names, column);
      if (!ascii_nonempty(name)) {
        return FALSE;
      }
      for (R_xlen_t earlier = 0; earlier < column; ++earlier) {
        if (STRING_ELT(names, earlier) == name || strcmp(
            CHAR(STRING_ELT(names, earlier)),
            CHAR(name)
          ) == 0) {
          return FALSE;
        }
      }
      const R_xlen_t parameter = parameter_for_name(
        parameters,
        parameter_count,
        name
      );
      const design_trafo_kind_t kind = parameter == R_XLEN_T_MAX
        ? DESIGN_TRAFO_NONE
        : parameters[parameter].kind;
      SEXP source = VECTOR_ELT(row, column);
      if (element >= total_elements) {
        return FALSE;
      }
      elements[element] = (element_plan_t) {
        source,
        name,
        R_NilValue,
        0.0,
        0.0,
        0,
        kind
      };
      SET_VECTOR_ELT(roots, row_count * 2 + element, source);
      if (kind != DESIGN_TRAFO_NONE) {
        if (TYPEOF(source) != REALSXP || ALTREP(source) ||
            XLENGTH(source) != 1 || !paradox_api_has_no_attributes(source)) {
          return FALSE;
        }
        const double input = REAL_ELT(source, 0);
        elements[element].input = input;
        if (kind == DESIGN_TRAFO_EXP) {
          elements[element].real_result = ISNA(input)
            ? NA_REAL
            : (ISNAN(input) ? R_NaN : exp(input));
        } else if (!integer_result(
          input,
          parameters[parameter].lower,
          parameters[parameter].upper,
          &elements[element].integer_result
        )) {
          return FALSE;
        }
      }
      ++element;
    }
  }
  return element == total_elements;
}

static SEXP allocate_output(R_xlen_t row_count, row_plan_t *rows,
    element_plan_t *elements) {
  SEXP result = PROTECT(Rf_allocVector(VECSXP, row_count));
  for (R_xlen_t row_index = 0; row_index < row_count; ++row_index) {
    const row_plan_t *row_plan = &rows[row_index];
    SEXP row = PROTECT(Rf_allocVector(VECSXP, row_plan->size));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, row_plan->size));
    for (R_xlen_t column = 0; column < row_plan->size; ++column) {
      element_plan_t *element = &elements[
        row_plan->element_offset + column
      ];
      SEXP output;
      if (element->kind == DESIGN_TRAFO_EXP) {
        output = PROTECT(Rf_allocVector(REALSXP, 1));
      } else if (element->kind == DESIGN_TRAFO_BOUNDED_INTEGER_EXP) {
        output = PROTECT(Rf_allocVector(INTSXP, 1));
      } else {
        output = PROTECT(element->source);
      }
      element->output = output;
      SET_VECTOR_ELT(row, column, output);
      SET_STRING_ELT(names, column, element->name);
      UNPROTECT(1);
    }
    Rf_setAttrib(row, R_NamesSymbol, names);
    SET_VECTOR_ELT(result, row_index, row);
    UNPROTECT(2);
  }
  UNPROTECT(1);
  return result;
}

static int input_plan_matches(SEXP xs, const row_plan_t *rows,
    const element_plan_t *elements, R_xlen_t row_count,
    R_xlen_t total_elements) {
  if (TYPEOF(xs) != VECSXP || ALTREP(xs) || Rf_isObject(xs) ||
      !paradox_api_has_no_attributes(xs) || XLENGTH(xs) != row_count) {
    return FALSE;
  }
  R_xlen_t element = 0;
  for (R_xlen_t row_index = 0; row_index < row_count; ++row_index) {
    const row_plan_t *row_plan = &rows[row_index];
    SEXP row = VECTOR_ELT(xs, row_index);
    if (row != row_plan->row || TYPEOF(row) != VECSXP || ALTREP(row) ||
        Rf_isObject(row) || XLENGTH(row) != row_plan->size ||
        !paradox_api_has_single_attribute(row, "names") ||
        Rf_getAttrib(row, R_NamesSymbol) != row_plan->names) {
      return FALSE;
    }
    for (R_xlen_t column = 0; column < row_plan->size; ++column) {
      if (element >= total_elements) {
        return FALSE;
      }
      const element_plan_t *planned = &elements[element];
      if (STRING_ELT(row_plan->names, column) != planned->name ||
          VECTOR_ELT(row, column) != planned->source) {
        return FALSE;
      }
      if (planned->kind != DESIGN_TRAFO_NONE) {
        SEXP source = planned->source;
        if (TYPEOF(source) != REALSXP || ALTREP(source) ||
            XLENGTH(source) != 1 ||
            !paradox_api_has_no_attributes(source) ||
            !same_double(REAL_ELT(source, 0), planned->input)) {
          return FALSE;
        }
      }
      ++element;
    }
  }
  return element == total_elements;
}

static int parameter_plan_matches(const trafo_state_t *state,
    const parameter_plan_t *parameters) {
  SEXP base_exp = stable_base_function("exp");
  if (base_exp == R_UnboundValue) {
    return FALSE;
  }
  for (R_xlen_t parameter = 0;
      parameter < state->parameter_count;
      ++parameter) {
    const parameter_plan_t *planned = &parameters[parameter];
    if (STRING_ELT(state->param_ids, parameter) != planned->id) {
      return FALSE;
    }
    if (planned->kind == DESIGN_TRAFO_NONE) {
      continue;
    }
    if (!cargo_has_exact_logscale_marker(
      VECTOR_ELT(state->cargo, parameter)
    )) {
      return FALSE;
    }
    if (planned->kind == DESIGN_TRAFO_EXP) {
      if (planned->callback != base_exp) {
        return FALSE;
      }
    } else {
      parameter_plan_t current = {
        planned->id,
        planned->callback,
        R_NilValue,
        R_NilValue,
        R_NilValue,
        0.0,
        0.0,
        DESIGN_TRAFO_NONE
      };
      if (!exact_integer_logscale_callback(planned->callback, &current) ||
          current.callback_environment != planned->callback_environment ||
          current.lower_source != planned->lower_source ||
          current.upper_source != planned->upper_source ||
          !same_double(current.lower, planned->lower) ||
          !same_double(current.upper, planned->upper)) {
        return FALSE;
      }
    }
  }
  for (R_xlen_t trafo = 0; trafo < state->trafo_count; ++trafo) {
    SEXP id = STRING_ELT(state->trafo_ids, trafo);
    R_xlen_t parameter = parameter_for_name(
      parameters,
      state->parameter_count,
      id
    );
    if (parameter == R_XLEN_T_MAX ||
        VECTOR_ELT(state->callbacks, trafo) !=
          parameters[parameter].callback) {
      return FALSE;
    }
  }
  return TRUE;
}

static int final_storage_surface_matches(const trafo_state_t *state) {
  if (TYPEOF(state->params_table) != VECSXP ||
      XLENGTH(state->params_table) <= PARADOX_DOMAIN_CARGO ||
      VECTOR_ELT(state->params_table, PARADOX_DOMAIN_ID) !=
        state->param_ids ||
      VECTOR_ELT(state->params_table, PARADOX_DOMAIN_CARGO) != state->cargo ||
      TYPEOF(state->param_ids) != STRSXP || ALTREP(state->param_ids) ||
      TYPEOF(state->cargo) != VECSXP || ALTREP(state->cargo) ||
      XLENGTH(state->param_ids) != state->parameter_count ||
      XLENGTH(state->cargo) != state->parameter_count) {
    return FALSE;
  }

  SEXP table = state->trafos_table;
  if (TYPEOF(table) != VECSXP || ALTREP(table) || XLENGTH(table) != 2 ||
      VECTOR_ELT(table, 0) != state->trafo_ids ||
      VECTOR_ELT(table, 1) != state->callbacks ||
      TYPEOF(state->trafo_ids) != STRSXP || ALTREP(state->trafo_ids) ||
      TYPEOF(state->callbacks) != VECSXP || ALTREP(state->callbacks) ||
      XLENGTH(state->trafo_ids) != state->trafo_count ||
      XLENGTH(state->callbacks) != state->trafo_count ||
      !paradox_api_has_no_attributes(state->trafo_ids) ||
      !paradox_api_has_no_attributes(state->callbacks)) {
    return FALSE;
  }
  SEXP names = Rf_getAttrib(table, R_NamesSymbol);
  SEXP classes = Rf_getAttrib(table, R_ClassSymbol);
  SEXP sorted = Rf_getAttrib(table, Rf_install("sorted"));
  return TYPEOF(names) == STRSXP && !ALTREP(names) &&
    XLENGTH(names) == 2 && paradox_api_has_no_attributes(names) &&
    paradox_domain_string_is(STRING_ELT(names, 0), "id") &&
    paradox_domain_string_is(STRING_ELT(names, 1), "trafo") &&
    TYPEOF(classes) == STRSXP && !ALTREP(classes) &&
    XLENGTH(classes) == 2 && paradox_api_has_no_attributes(classes) &&
    paradox_domain_string_is(STRING_ELT(classes, 0), "data.table") &&
    paradox_domain_string_is(STRING_ELT(classes, 1), "data.frame") &&
    TYPEOF(sorted) == STRSXP && !ALTREP(sorted) && XLENGTH(sorted) == 1 &&
    paradox_api_has_no_attributes(sorted) &&
    paradox_domain_string_is(STRING_ELT(sorted, 0), "id");
}

static int final_state_matches(const trafo_state_t *initial,
    const parameter_plan_t *parameters, SEXP roots, R_xlen_t *work) {
  SEXP enclosure = paradox_domain_local_value(
    initial->param_set,
    ".__enclos_env__"
  );
  SEXP private_environment = enclosure == R_UnboundValue
    ? R_UnboundValue
    : paradox_domain_local_value(enclosure, "private");
  if (enclosure != initial->enclosure ||
      private_environment != initial->private_environment) {
    return FALSE;
  }
  SEXP params_table = paradox_domain_local_value(
    private_environment,
    ".params"
  );
  SEXP trafos_table = paradox_domain_local_value(
    private_environment,
    ".trafos"
  );
  if (params_table != initial->params_table ||
      trafos_table != initial->trafos_table) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, FINAL_ROOT_PARAMS, params_table);
  SET_VECTOR_ELT(roots, FINAL_ROOT_TRAFOS, trafos_table);

  paradox_domain_params_t params;
  R_xlen_t unused = 0;
  if (!paradox_domain_validate_params(
      params_table,
      R_NilValue,
      TRUE,
      &params,
      &unused,
      work
    )) {
    return FALSE;
  }
  SEXP cargo = VECTOR_ELT(params_table, PARADOX_DOMAIN_CARGO);
  SET_VECTOR_ELT(roots, FINAL_ROOT_PARAM_IDS, params.ids);
  SET_VECTOR_ELT(roots, FINAL_ROOT_CARGO, cargo);

  paradox_domain_trafos_t trafos;
  if (!paradox_domain_validate_trafos(trafos_table, &trafos, work)) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, FINAL_ROOT_TRAFO_IDS, trafos.ids);
  SET_VECTOR_ELT(roots, FINAL_ROOT_CALLBACKS, trafos.values);

  trafo_state_t current = *initial;
  current.params_table = params_table;
  current.trafos_table = trafos_table;
  current.param_ids = params.ids;
  current.cargo = cargo;
  current.trafo_ids = trafos.ids;
  current.callbacks = trafos.values;
  current.parameter_count = params.row_count;
  current.trafo_count = trafos.row_count;

  /* The auth call allocates its root plan before inspecting the R6 surface.
   * The pointer/content comparisons below then catch any table or input
   * mutation caused by that allocation. */
  return paradox_param_set_design_trafo_auth(initial->param_set) &&
    paradox_domain_local_value(
      private_environment,
      ".extra_trafo"
    ) == R_NilValue &&
    paradox_domain_local_value(private_environment, ".params") ==
      params_table &&
    paradox_domain_local_value(private_environment, ".trafos") ==
      trafos_table &&
    current.parameter_count == initial->parameter_count &&
    current.trafo_count == initial->trafo_count &&
    current.param_ids == initial->param_ids &&
    current.cargo == initial->cargo &&
    current.trafo_ids == initial->trafo_ids &&
    current.callbacks == initial->callbacks &&
    final_storage_surface_matches(&current) &&
    parameter_plan_matches(&current, parameters);
}

SEXP paradox_design_transpose_logscale_builtin(SEXP xs, SEXP param_set) {
  if (TYPEOF(xs) != VECSXP || ALTREP(xs) || Rf_isObject(xs) ||
      !paradox_api_has_no_attributes(xs)) {
    return R_NilValue;
  }

  SEXP initial_roots = PROTECT(Rf_allocVector(
    VECSXP,
    INITIAL_ROOT_COUNT
  ));
  R_xlen_t work = 0;
  trafo_state_t state;
  if (!load_initial_state(param_set, &state, initial_roots, &work)) {
    UNPROTECT(1);
    return R_NilValue;
  }

  parameter_plan_t *parameters = paradox_temporary_alloc(
    state.parameter_count,
    sizeof(*parameters)
  );
  if (state.parameter_count > R_XLEN_T_MAX / 4) {
    UNPROTECT(1);
    return R_NilValue;
  }
  SEXP parameter_roots = PROTECT(Rf_allocVector(
    VECSXP,
    state.parameter_count * 4
  ));
  if (!initialize_parameter_plan(
      &state,
      parameters,
      parameter_roots,
      &work
    )) {
    UNPROTECT(2);
    return R_NilValue;
  }

  const R_xlen_t row_count = XLENGTH(xs);
  if (row_count > DESIGN_TRAFO_MAX_PLAN_ROWS) {
    UNPROTECT(2);
    return R_NilValue;
  }
  row_plan_t *rows = paradox_temporary_alloc(row_count, sizeof(*rows));
  R_xlen_t total_elements = 0;
  for (R_xlen_t row = 0; row < row_count; ++row) {
    SEXP value = VECTOR_ELT(xs, row);
    if (TYPEOF(value) != VECSXP || ALTREP(value)) {
      UNPROTECT(2);
      return R_NilValue;
    }
    const R_xlen_t size = XLENGTH(value);
    if (size > R_XLEN_T_MAX - total_elements) {
      UNPROTECT(2);
      return R_NilValue;
    }
    total_elements += size;
    if (total_elements > DESIGN_TRAFO_MAX_PLAN_ELEMENTS) {
      UNPROTECT(2);
      return R_NilValue;
    }
  }
  element_plan_t *elements = paradox_temporary_alloc(
    total_elements,
    sizeof(*elements)
  );
  if (row_count > R_XLEN_T_MAX / 2 ||
      total_elements > R_XLEN_T_MAX - row_count * 2) {
    UNPROTECT(2);
    return R_NilValue;
  }
  SEXP input_roots = PROTECT(Rf_allocVector(
    VECSXP,
    row_count * 2 + total_elements
  ));
  if (!initialize_input_plan(
      xs,
      parameters,
      state.parameter_count,
      rows,
      elements,
      total_elements,
      row_count,
      input_roots,
      &work
    )) {
    UNPROTECT(3);
    return R_NilValue;
  }

  SEXP result = PROTECT(allocate_output(row_count, rows, elements));
  SEXP final_roots = PROTECT(Rf_allocVector(VECSXP, FINAL_ROOT_COUNT));
  if (!final_state_matches(&state, parameters, final_roots, &work) ||
      !input_plan_matches(
        xs,
        rows,
        elements,
        row_count,
        total_elements
      )) {
    UNPROTECT(5);
    return R_NilValue;
  }

  for (R_xlen_t element = 0; element < total_elements; ++element) {
    if (elements[element].kind == DESIGN_TRAFO_EXP) {
      SET_REAL_ELT(
        elements[element].output,
        0,
        elements[element].real_result
      );
    } else if (elements[element].kind ==
        DESIGN_TRAFO_BOUNDED_INTEGER_EXP) {
      SET_INTEGER_ELT(
        elements[element].output,
        0,
        elements[element].integer_result
      );
    }
  }
  UNPROTECT(5);
  return result;
}
