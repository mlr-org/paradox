#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "paramset_domain_common.h"
#include "paramset_params_internal.h"
#include "r_api_compat.h"
#include "r_utils.h"

typedef struct {
  SEXP params;
  SEXP ids;
  R_xlen_t size;
} value_param_state_t;

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

static int canonical_wrapper_environment(SEXP function, SEXP self,
    SEXP private_environment, SEXP namespace_environment,
    const char *target) {
  SEXP environment = paradox_api_closure_environment(function);
  return TYPEOF(environment) == ENVSXP &&
    paradox_api_parent_environment(environment) == namespace_environment &&
    !R_existsVarInFrame(environment, Rf_install("super")) &&
    !R_existsVarInFrame(environment, Rf_install(target)) &&
    paradox_domain_local_value(environment, "self") == self &&
    paradox_domain_local_value(environment, "private") ==
      private_environment;
}

static int exact_missing_formal(SEXP formal, const char *name) {
  return formal != R_NilValue && TAG(formal) == Rf_install(name) &&
    CAR(formal) == R_MissingArg;
}

static int exact_flag_formal(SEXP formal, const char *name, int expected) {
  SEXP value = formal == R_NilValue ? R_NilValue : CAR(formal);
  return formal != R_NilValue && TAG(formal) == Rf_install(name) &&
    TYPEOF(value) == LGLSXP && !ALTREP(value) && XLENGTH(value) == 1 &&
    paradox_api_has_no_attributes(value) &&
    LOGICAL_ELT(value, 0) == expected;
}

static int exact_string_formal(SEXP formal, const char *name,
    const char *expected) {
  SEXP value = formal == R_NilValue ? R_NilValue : CAR(formal);
  return formal != R_NilValue && TAG(formal) == Rf_install(name) &&
    TYPEOF(value) == STRSXP && !ALTREP(value) && XLENGTH(value) == 1 &&
    paradox_api_has_no_attributes(value) &&
    paradox_domain_string_is(STRING_ELT(value, 0), expected);
}

static int exact_vname_formal(SEXP formal) {
  if (formal == R_NilValue || TAG(formal) != Rf_install(".var.name")) {
    return FALSE;
  }
  SEXP value = CAR(formal);
  return TYPEOF(value) == LANGSXP && CAR(value) == Rf_install("vname") &&
    paradox_api_has_no_attributes(value) &&
    CDR(value) != R_NilValue && CAR(CDR(value)) == Rf_install("xs") &&
    CDR(CDR(value)) == R_NilValue;
}

static int exact_assert_formals(SEXP function) {
  SEXP formal = paradox_api_closure_formals(function);
  if (!exact_missing_formal(formal, "xs")) return FALSE;
  formal = CDR(formal);
  if (!exact_flag_formal(formal, "check_strict", TRUE)) return FALSE;
  formal = CDR(formal);
  if (!exact_string_formal(formal, "presence", "none")) return FALSE;
  formal = CDR(formal);
  if (!exact_vname_formal(formal)) return FALSE;
  formal = CDR(formal);
  if (!exact_flag_formal(formal, "sanitize", FALSE)) return FALSE;
  formal = CDR(formal);
  return exact_flag_formal(formal, "allow_token", TRUE) &&
    CDR(formal) == R_NilValue;
}

static int exact_check_formals(SEXP function) {
  SEXP formal = paradox_api_closure_formals(function);
  if (!exact_missing_formal(formal, "xs")) return FALSE;
  formal = CDR(formal);
  if (!exact_flag_formal(formal, "check_strict", TRUE)) return FALSE;
  formal = CDR(formal);
  if (!exact_flag_formal(formal, "sanitize", FALSE)) return FALSE;
  formal = CDR(formal);
  if (!exact_string_formal(formal, "presence", "none")) return FALSE;
  formal = CDR(formal);
  return exact_flag_formal(formal, "allow_token", TRUE) &&
    CDR(formal) == R_NilValue;
}

static int exact_test_constraint_formals(SEXP function) {
  SEXP formal = paradox_api_closure_formals(function);
  if (!exact_missing_formal(formal, "x")) return FALSE;
  formal = CDR(formal);
  return exact_flag_formal(formal, "assert_value", TRUE) &&
    CDR(formal) == R_NilValue;
}

static int exact_xs_formals(SEXP function) {
  SEXP formal = paradox_api_closure_formals(function);
  return exact_missing_formal(formal, "xs") &&
    CDR(formal) == R_NilValue;
}

static int canonical_locked_method(SEXP container, SEXP self,
    SEXP private_environment, SEXP namespace_environment,
    const char *binding_name, const char *target,
    const char *const *arguments, R_xlen_t argument_count,
    int (*formals_are_exact)(SEXP)) {
  SEXP symbol = Rf_install(binding_name);
  if (!R_existsVarInFrame(container, symbol) ||
      R_BindingIsActive(symbol, container) ||
      !R_BindingIsLocked(symbol, container)) {
    return FALSE;
  }
  SEXP function = PROTECT(paradox_domain_local_value(
    container,
    binding_name
  ));
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

static int canonical_active_member(SEXP self, SEXP private_environment,
    SEXP namespace_environment, const char *member_name,
    const char *method_name, const char *argument_name,
    R_xlen_t *work_since_interrupt) {
  if (!paradox_params_canonical_active_member(
      self,
      private_environment,
      member_name,
      method_name,
      argument_name,
      NULL,
      work_since_interrupt
    )) {
    return FALSE;
  }
  SEXP function = PROTECT(R_ActiveBindingFunction(
    Rf_install(member_name),
    self
  ));
  SEXP environment = TYPEOF(function) == CLOSXP
    ? paradox_api_closure_environment(function)
    : R_NilValue;
  const int exact = TYPEOF(environment) == ENVSXP &&
    paradox_api_parent_environment(environment) == namespace_environment &&
    !R_existsVarInFrame(environment, Rf_install("super")) &&
    !R_existsVarInFrame(environment, Rf_install(method_name));
  UNPROTECT(1);
  return exact;
}

static int canonical_checked_assignment_surface(SEXP self,
    SEXP private_environment, R_xlen_t *work_since_interrupt) {
  static const char *const assert_arguments[] = {
    "self", "private", "super", "xs", "check_strict", "presence",
    ".var.name", "sanitize", "allow_token"
  };
  static const char *const check_arguments[] = {
    "self", "private", "super", "xs", "check_strict", "sanitize",
    "presence", "allow_token"
  };
  static const char *const test_constraint_arguments[] = {
    "self", "private", "super", "x", "assert_value"
  };
  static const char *const check_dependencies_arguments[] = {
    "self", "private", "super", "xs"
  };
  static const char *const store_arguments[] = {
    "self", "private", "super", "xs"
  };
  SEXP namespace_environment = paradox_api_registered_namespace("paradox");
  return TYPEOF(namespace_environment) == ENVSXP &&
    canonical_active_member(
      self,
      private_environment,
      namespace_environment,
      "values",
      ".__ParamSet__values",
      "xs",
      work_since_interrupt
    ) && canonical_active_member(
      self,
      private_environment,
      namespace_environment,
      "deps",
      ".__ParamSet__deps",
      "v",
      work_since_interrupt
    ) && canonical_active_member(
      self,
      private_environment,
      namespace_environment,
      "constraint",
      ".__ParamSet__constraint",
      "f",
      work_since_interrupt
    ) && canonical_locked_method(
      self,
      self,
      private_environment,
      namespace_environment,
      "assert",
      ".__ParamSet__assert",
      assert_arguments,
      9,
      exact_assert_formals
    ) && canonical_locked_method(
      self,
      self,
      private_environment,
      namespace_environment,
      "check",
      ".__ParamSet__check",
      check_arguments,
      8,
      exact_check_formals
    ) && canonical_locked_method(
      self,
      self,
      private_environment,
      namespace_environment,
      "test_constraint",
      ".__ParamSet__test_constraint",
      test_constraint_arguments,
      5,
      exact_test_constraint_formals
    ) && canonical_locked_method(
      self,
      self,
      private_environment,
      namespace_environment,
      "check_dependencies",
      ".__ParamSet__check_dependencies",
      check_dependencies_arguments,
      4,
      exact_xs_formals
    ) && canonical_locked_method(
      private_environment,
      self,
      private_environment,
      namespace_environment,
      ".store_values",
      ".__ParamSet__.store_values",
      store_arguments,
      4,
      exact_xs_formals
    );
}

static int exact_flag(SEXP value, int *result) {
  if (TYPEOF(value) != LGLSXP || ALTREP(value) || XLENGTH(value) != 1 ||
      !paradox_api_has_no_attributes(value)) {
    return FALSE;
  }
  const int flag = LOGICAL_ELT(value, 0);
  if (flag == NA_LOGICAL) {
    return FALSE;
  }
  *result = flag;
  return TRUE;
}

static int plain_list(SEXP value, SEXP *names, R_xlen_t *size) {
  if (TYPEOF(value) != VECSXP || Rf_isObject(value)) {
    return FALSE;
  }
  *size = XLENGTH(value);
  SEXP observed_names = PROTECT(ALTREP(value)
    ? paradox_stored_attribute(value, R_NamesSymbol)
    : Rf_getAttrib(value, R_NamesSymbol));
  *names = observed_names;
  int plain;
  if (observed_names == R_NilValue) {
    plain = *size == 0 && paradox_api_has_no_attributes(value);
  } else {
    plain = TYPEOF(observed_names) == STRSXP &&
      XLENGTH(observed_names) == *size &&
      paradox_params_names_are_only_attribute(value);
  }
  UNPROTECT(1);
  return plain;
}

/* Checked assignment must not execute arbitrary code after authenticating the
 * live ParamSet surface.  An ordinary list shell, ordinary names, and ordinary
 * scalar children make the built-in checker callback-free; unusual ALTREP
 * values deliberately fall through to the R implementation. */
static int callback_free_values(SEXP values) {
  if (TYPEOF(values) != VECSXP || ALTREP(values) || Rf_isObject(values)) {
    return FALSE;
  }
  const R_xlen_t size = XLENGTH(values);
  SEXP names = Rf_getAttrib(values, R_NamesSymbol);
  if ((names == R_NilValue && size != 0) ||
      (names != R_NilValue &&
        (TYPEOF(names) != STRSXP || ALTREP(names) ||
          XLENGTH(names) != size))) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    if (ALTREP(VECTOR_ELT(values, index))) {
      return FALSE;
    }
  }
  return TRUE;
}

static SEXP snapshot_character_vector(SEXP values,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(values) != STRSXP || ALTREP(values)) {
    return R_NilValue;
  }
  const R_xlen_t size = XLENGTH(values);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    SET_STRING_ELT(result, index, STRING_ELT(values, index));
  }
  UNPROTECT(1);
  return result;
}

static int same_character_vector(SEXP left, SEXP right,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(left) != STRSXP || TYPEOF(right) != STRSXP ||
      ALTREP(left) || ALTREP(right)) {
    return FALSE;
  }
  const R_xlen_t size = XLENGTH(left);
  if (XLENGTH(right) != size) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if (!paradox_domain_strings_equal(
        STRING_ELT(left, index),
        STRING_ELT(right, index)
      )) {
      return FALSE;
    }
  }
  return TRUE;
}

static int ordinary_plain_list(SEXP value, SEXP *names, R_xlen_t *size) {
  if (TYPEOF(value) != VECSXP || ALTREP(value) ||
      !plain_list(value, names, size)) {
    return FALSE;
  }
  return *names == R_NilValue || !ALTREP(*names);
}

/* Materialize an ordinary list shell and names vector. The copied shell is a
 * stable decision source for multi-pass merge/store planning and independently
 * roots every element returned by an ALTREP list method. */
static SEXP snapshot_plain_list(SEXP value, SEXP names, R_xlen_t size,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(value) != VECSXP ||
      (names != R_NilValue && TYPEOF(names) != STRSXP)) {
    return R_NilValue;
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  SEXP stable_names = R_NilValue;
  int protected_count = 1;
  if (names != R_NilValue) {
    stable_names = PROTECT(Rf_allocVector(STRSXP, size));
    ++protected_count;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP element = PROTECT(VECTOR_ELT(value, index));
    SET_VECTOR_ELT(result, index, element);
    UNPROTECT(1);
    if (stable_names != R_NilValue) {
      SET_STRING_ELT(stable_names, index, STRING_ELT(names, index));
    }
  }
  if (stable_names != R_NilValue) {
    Rf_setAttrib(result, R_NamesSymbol, stable_names);
  }
  UNPROTECT(protected_count);
  return result;
}

static int names_are_unique(SEXP names, R_xlen_t size,
    R_xlen_t *work_since_interrupt) {
  if (size == 0) {
    return TRUE;
  }
  if (TYPEOF(names) != STRSXP || XLENGTH(names) != size) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if (STRING_ELT(names, index) == NA_STRING) {
      return FALSE;
    }
  }
  return Rf_any_duplicated(names, FALSE) == 0;
}

static R_xlen_t find_name(SEXP names, SEXP sought,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t size = names == R_NilValue ? 0 : XLENGTH(names);
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if (paradox_domain_strings_equal(STRING_ELT(names, index), sought)) {
      return index;
    }
  }
  return -1;
}

static SEXP update_name(SEXP dot_names, SEXP value_names,
    R_xlen_t dot_size, R_xlen_t index) {
  return index < dot_size
    ? STRING_ELT(dot_names, index)
    : STRING_ELT(value_names, index - dot_size);
}

static SEXP update_value(SEXP dots, SEXP values,
    R_xlen_t dot_size, R_xlen_t index) {
  return index < dot_size
    ? VECTOR_ELT(dots, index)
    : VECTOR_ELT(values, index - dot_size);
}

static int disjoint_names(SEXP left, SEXP right,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t left_size = left == R_NilValue ? 0 : XLENGTH(left);
  for (R_xlen_t left_index = 0;
      left_index < left_size;
      ++left_index) {
    if (find_name(
        right,
        STRING_ELT(left, left_index),
        work_since_interrupt
      ) >= 0) {
      return FALSE;
    }
  }
  return TRUE;
}

static int exact_row_names(SEXP table, R_xlen_t row_count,
    R_xlen_t *work_since_interrupt) {
  SEXP row_names = Rf_getAttrib(table, R_RowNamesSymbol);
  if (row_count > INT_MAX || TYPEOF(row_names) != INTSXP ||
      XLENGTH(row_names) != row_count ||
      !paradox_api_has_no_attributes(row_names)) {
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

static int load_exact_param_state(SEXP private_environment, SEXP self,
    value_param_state_t *state, R_xlen_t *work_since_interrupt) {
  if (!paradox_params_exact_base_param_set(self, work_since_interrupt) ||
      TYPEOF(private_environment) != ENVSXP ||
      !paradox_domain_owns_private_environment(self, private_environment)) {
    return FALSE;
  }
  state->params = PROTECT(paradox_domain_local_value(
    private_environment,
    ".params"
  ));
  SEXP current_values = PROTECT(paradox_domain_local_value(
    private_environment,
    ".values"
  ));
  paradox_domain_params_t checked;
  R_xlen_t unused_row = 0;
  const int exact = state->params != R_UnboundValue &&
    current_values != R_UnboundValue &&
    paradox_params_supported_table_attributes(state->params, FALSE) &&
    paradox_domain_validate_params(
      state->params,
      R_NilValue,
      TRUE,
      &checked,
      &unused_row,
      work_since_interrupt
    ) && exact_row_names(
      state->params,
      checked.row_count,
      work_since_interrupt
    ) && checked.row_count <= INT_MAX;
  if (!exact) {
    UNPROTECT(2);
    return FALSE;
  }
  state->ids = checked.ids;
  state->size = checked.row_count;
  UNPROTECT(2);
  return TRUE;
}

static SEXP ordered_values(SEXP ids, SEXP values,
    R_xlen_t *work_since_interrupt) {
  SEXP value_names = R_NilValue;
  R_xlen_t original_size = 0;
  if (!plain_list(values, &value_names, &original_size)) {
    return R_NilValue;
  }
  PROTECT(value_names);

  if (value_names == R_NilValue || original_size > INT_MAX) {
    UNPROTECT(1);
    return R_NilValue;
  }

  SEXP stable_values = PROTECT(snapshot_plain_list(
    values,
    value_names,
    original_size,
    work_since_interrupt
  ));
  if (stable_values == R_NilValue ||
      XLENGTH(stable_values) != original_size) {
    UNPROTECT(2);
    return R_NilValue;
  }
  values = stable_values;
  value_names = Rf_getAttrib(values, R_NamesSymbol);
  const R_xlen_t id_size = XLENGTH(ids);

  SEXP matches = PROTECT(Rf_match(value_names, ids, 0));
  if (TYPEOF(matches) != INTSXP || XLENGTH(matches) != id_size) {
    UNPROTECT(3);
    return R_NilValue;
  }
  R_xlen_t output_size = 0;
  for (R_xlen_t row = 0; row < id_size; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    if (INTEGER_ELT(matches, row) > 0) {
      ++output_size;
    }
  }
  SEXP result = PROTECT(Rf_allocVector(VECSXP, output_size));
  SEXP result_names = PROTECT(Rf_allocVector(STRSXP, output_size));
  R_xlen_t output = 0;
  for (R_xlen_t row = 0; row < id_size; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    const int matched = INTEGER_ELT(matches, row);
    if (matched > 0) {
      const R_xlen_t source = (R_xlen_t) matched - 1;
      if (source >= original_size || output >= output_size) {
        UNPROTECT(5);
        Rf_error("Internal error: ordered values exceeded its capacity");
      }
      SET_VECTOR_ELT(result, output, VECTOR_ELT(values, source));
      SET_STRING_ELT(
        result_names,
        output,
        STRING_ELT(value_names, source)
      );
      ++output;
    }
  }
  if (output != output_size) {
    UNPROTECT(5);
    Rf_error("Internal error: incomplete ordered values output");
  }
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  UNPROTECT(5);
  return result;
}

SEXP paradox_param_set_values_merge(SEXP dots, SEXP values,
    SEXP current, SEXP insert_sexp) {
  R_xlen_t work_since_interrupt = 0;
  int insert = FALSE;
  int protected_count = 0;
  SEXP dot_names = R_NilValue;
  SEXP value_names = R_NilValue;
  SEXP current_names = R_NilValue;
  R_xlen_t original_dot_size = 0;
  R_xlen_t original_value_size = 0;
  R_xlen_t current_size = 0;
  if (!exact_flag(insert_sexp, &insert) ||
      !plain_list(dots, &dot_names, &original_dot_size)) {
    return R_NilValue;
  }
  PROTECT(dot_names);
  ++protected_count;
  if (!plain_list(values, &value_names, &original_value_size)) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  PROTECT(value_names);
  ++protected_count;
  if (insert) {
    if (!plain_list(current, &current_names, &current_size)) {
      UNPROTECT(protected_count);
      return R_NilValue;
    }
    PROTECT(current_names);
    ++protected_count;
  }

  SEXP stable_dots = PROTECT(snapshot_plain_list(
    dots,
    dot_names,
    original_dot_size,
    &work_since_interrupt
  ));
  ++protected_count;
  SEXP stable_values = PROTECT(snapshot_plain_list(
    values,
    value_names,
    original_value_size,
    &work_since_interrupt
  ));
  ++protected_count;
  if (stable_dots == R_NilValue || stable_values == R_NilValue ||
      XLENGTH(stable_dots) != original_dot_size ||
      XLENGTH(stable_values) != original_value_size) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  dots = stable_dots;
  values = stable_values;
  dot_names = Rf_getAttrib(dots, R_NamesSymbol);
  value_names = Rf_getAttrib(values, R_NamesSymbol);
  if (!names_are_unique(
      dot_names,
      XLENGTH(dots),
      &work_since_interrupt
    ) || !names_are_unique(
      value_names,
      XLENGTH(values),
      &work_since_interrupt
    ) || !disjoint_names(
      dot_names,
      value_names,
      &work_since_interrupt
    )) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }

  if (insert) {
    SEXP stable_current = PROTECT(snapshot_plain_list(
      current,
      current_names,
      current_size,
      &work_since_interrupt
    ));
    ++protected_count;
    if (stable_current == R_NilValue) {
      UNPROTECT(protected_count);
      return R_NilValue;
    }
    current = stable_current;
    current_names = Rf_getAttrib(current, R_NamesSymbol);
    if (!names_are_unique(
        current_names,
        XLENGTH(current),
        &work_since_interrupt
      )) {
      UNPROTECT(protected_count);
      return R_NilValue;
    }
  }
  const R_xlen_t dot_size = original_dot_size;
  const R_xlen_t value_size = original_value_size;
  if (dot_size > R_XLEN_T_MAX - value_size) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  const R_xlen_t update_size = dot_size + value_size;

  if (!insert) {
    SEXP result = PROTECT(Rf_allocVector(VECSXP, update_size));
    if (update_size == 0) {
      UNPROTECT(protected_count + 1);
      return result;
    }
    SEXP result_names = PROTECT(Rf_allocVector(STRSXP, update_size));
    for (R_xlen_t index = 0; index < update_size; ++index) {
      paradox_domain_account_work(&work_since_interrupt);
      SET_VECTOR_ELT(
        result,
        index,
        update_value(dots, values, dot_size, index)
      );
      SET_STRING_ELT(
        result_names,
        index,
        update_name(dot_names, value_names, dot_size, index)
      );
    }
    Rf_setAttrib(result, R_NamesSymbol, result_names);
    UNPROTECT(protected_count + 2);
    return result;
  }

  if (current_size > R_XLEN_T_MAX - update_size) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  R_xlen_t output_size = 0;
  for (R_xlen_t index = 0; index < current_size; ++index) {
    paradox_domain_account_work(&work_since_interrupt);
    const R_xlen_t update = find_name(
      dot_names,
      STRING_ELT(current_names, index),
      &work_since_interrupt
    );
    const R_xlen_t second_update = update >= 0 ? -1 : find_name(
      value_names,
      STRING_ELT(current_names, index),
      &work_since_interrupt
    );
    const R_xlen_t combined = update >= 0
      ? update
      : second_update >= 0 ? dot_size + second_update : -1;
    if (combined < 0 ||
        update_value(dots, values, dot_size, combined) != R_NilValue) {
      ++output_size;
    }
  }
  for (R_xlen_t update = 0; update < update_size; ++update) {
    paradox_domain_account_work(&work_since_interrupt);
    if (update_value(dots, values, dot_size, update) != R_NilValue &&
        find_name(
          current_names,
          update_name(dot_names, value_names, dot_size, update),
          &work_since_interrupt
        ) < 0) {
      ++output_size;
    }
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, output_size));
  SEXP result_names = PROTECT(Rf_allocVector(STRSXP, output_size));
  R_xlen_t output = 0;
  for (R_xlen_t index = 0; index < current_size; ++index) {
    paradox_domain_account_work(&work_since_interrupt);
    R_xlen_t update = find_name(
      dot_names,
      STRING_ELT(current_names, index),
      &work_since_interrupt
    );
    if (update < 0) {
      const R_xlen_t value_update = find_name(
        value_names,
        STRING_ELT(current_names, index),
        &work_since_interrupt
      );
      if (value_update >= 0) {
        update = dot_size + value_update;
      }
    }
    SEXP replacement = update >= 0
      ? update_value(dots, values, dot_size, update)
      : VECTOR_ELT(current, index);
    if (update < 0 || replacement != R_NilValue) {
      if (output >= output_size) {
        UNPROTECT(protected_count + 2);
        Rf_error("Internal error: values merge exceeded its capacity");
      }
      SET_VECTOR_ELT(result, output, replacement);
      SET_STRING_ELT(
        result_names,
        output,
        STRING_ELT(current_names, index)
      );
      ++output;
    }
  }
  for (R_xlen_t update = 0; update < update_size; ++update) {
    paradox_domain_account_work(&work_since_interrupt);
    SEXP name = update_name(dot_names, value_names, dot_size, update);
    SEXP replacement = update_value(dots, values, dot_size, update);
    if (replacement != R_NilValue && find_name(
        current_names,
        name,
        &work_since_interrupt
      ) < 0) {
      if (output >= output_size) {
        UNPROTECT(protected_count + 2);
        Rf_error("Internal error: values merge exceeded its capacity");
      }
      SET_VECTOR_ELT(result, output, replacement);
      SET_STRING_ELT(result_names, output, name);
      ++output;
    }
  }
  if (output != output_size) {
    UNPROTECT(protected_count + 2);
    Rf_error("Internal error: incomplete values merge output");
  }
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  UNPROTECT(protected_count + 2);
  return result;
}

SEXP paradox_param_set_store_values(SEXP private_environment, SEXP self,
    SEXP values) {
  R_xlen_t work_since_interrupt = 0;
  value_param_state_t state;
  if (!load_exact_param_state(
      private_environment,
      self,
      &state,
      &work_since_interrupt
    )) {
    return R_NilValue;
  }
  PROTECT(state.params);
  SEXP admitted_ids = PROTECT(snapshot_character_vector(
    state.ids,
    &work_since_interrupt
  ));
  if (admitted_ids == R_NilValue) {
    UNPROTECT(2);
    return R_NilValue;
  }
  SEXP stored = PROTECT(ordered_values(
    admitted_ids,
    values,
    &work_since_interrupt
  ));
  if (stored == R_NilValue) {
    UNPROTECT(3);
    return R_NilValue;
  }

  /* Input ALTREP methods may replace or mutate `.params` while values are
   * materialized.  Commit only if a fresh exact-state read has the same ID
   * surface used to order this result; otherwise let the R path retry against
   * the live state. */
  value_param_state_t live_state;
  if (!load_exact_param_state(
      private_environment,
      self,
      &live_state,
      &work_since_interrupt
    )) {
    UNPROTECT(3);
    return R_NilValue;
  }
  PROTECT(live_state.params);
  if (!same_character_vector(
      admitted_ids,
      live_state.ids,
      &work_since_interrupt
    )) {
    UNPROTECT(4);
    return R_NilValue;
  }
  Rf_defineVar(Rf_install(".values"), stored, private_environment);
  UNPROTECT(4);
  return stored;
}

SEXP paradox_param_set_assign_values_checked(SEXP private_environment,
    SEXP self, SEXP values) {
  R_xlen_t work_since_interrupt = 0;
  if (!callback_free_values(values)) {
    return R_NilValue;
  }
  value_param_state_t state;
  if (!load_exact_param_state(
      private_environment,
      self,
      &state,
      &work_since_interrupt
    )) {
    return R_NilValue;
  }
  PROTECT(state.params);
  if (!canonical_checked_assignment_surface(
      self,
      private_environment,
      &work_since_interrupt
    )) {
    UNPROTECT(1);
    return R_NilValue;
  }
  SEXP dependencies = PROTECT(paradox_domain_local_value(
    private_environment,
    ".deps"
  ));
  SEXP constraint = paradox_domain_local_value(
    private_environment,
    ".constraint"
  );
  paradox_domain_dependencies_t checked_dependencies;
  if (dependencies == R_UnboundValue || constraint != R_NilValue ||
      !paradox_params_supported_table_attributes(dependencies, TRUE) ||
      !paradox_domain_validate_dependencies(
        dependencies,
        &checked_dependencies,
        &work_since_interrupt
      ) || checked_dependencies.row_count != 0) {
    UNPROTECT(2);
    return R_NilValue;
  }

  SEXP sanitize = PROTECT(Rf_ScalarLogical(TRUE));
  SEXP checked = PROTECT(paradox_param_set_check_builtin(
    state.params,
    values,
    sanitize
  ));
  if (checked == R_NilValue || TYPEOF(checked) != LGLSXP ||
      XLENGTH(checked) != 1 || LOGICAL_ELT(checked, 0) != TRUE) {
    UNPROTECT(4);
    return R_NilValue;
  }
  SEXP sanitized = Rf_getAttrib(checked, Rf_install("sanitized"));
  if (TYPEOF(sanitized) != VECSXP) {
    UNPROTECT(4);
    return R_NilValue;
  }
  SEXP stored = PROTECT(ordered_values(
    state.ids,
    sanitized,
    &work_since_interrupt
  ));
  if (stored == R_NilValue) {
    UNPROTECT(5);
    return R_NilValue;
  }
  Rf_defineVar(Rf_install(".values"), stored, private_environment);
  UNPROTECT(5);
  return sanitized;
}

static int exact_collection(SEXP self,
    R_xlen_t *work_since_interrupt) {
  static const char *const classes[] = {
    "ParamSetCollection", "ParamSet", "R6"
  };
  if (TYPEOF(self) != ENVSXP) {
    return FALSE;
  }
  SEXP observed_classes = PROTECT(Rf_getAttrib(self, R_ClassSymbol));
  const int exact = paradox_domain_exact_string_vector(
    observed_classes,
    classes,
    3,
    work_since_interrupt
  );
  UNPROTECT(1);
  return exact;
}

static int supported_ascii(SEXP value) {
  if (value == NA_STRING || Rf_getCharCE(value) == CE_BYTES) {
    return FALSE;
  }
  const unsigned char *text = (const unsigned char *) CHAR(value);
  for (; *text != '\0'; ++text) {
    if (*text >= 0x80U) {
      return FALSE;
    }
  }
  return TRUE;
}

static int translated_id_matches(SEXP translated, SEXP original,
    SEXP owner, int postfix) {
  if (!supported_ascii(translated) || !supported_ascii(original) ||
      !supported_ascii(owner)) {
    return FALSE;
  }
  const char *translated_text = CHAR(translated);
  const char *original_text = CHAR(original);
  const char *owner_text = CHAR(owner);
  const size_t original_size = strlen(original_text);
  const size_t owner_size = strlen(owner_text);
  if (owner_size == 0) {
    return strcmp(translated_text, original_text) == 0;
  }
  const size_t translated_size = strlen(translated_text);
  if (original_size > SIZE_MAX - owner_size - 1U ||
      translated_size != original_size + owner_size + 1U) {
    return FALSE;
  }
  if (postfix) {
    return memcmp(translated_text, original_text, original_size) == 0 &&
      translated_text[original_size] == '.' &&
      memcmp(
        translated_text + original_size + 1U,
        owner_text,
        owner_size
      ) == 0;
  }
  return memcmp(translated_text, owner_text, owner_size) == 0 &&
    translated_text[owner_size] == '.' &&
    memcmp(
      translated_text + owner_size + 1U,
      original_text,
      original_size
    ) == 0;
}

static int exact_translation(SEXP translation, SEXP sets, int postfix,
    R_xlen_t *work_since_interrupt) {
  static const char *const column_names[] = {
    "id", "original_id", "owner_ps_index", "owner_name"
  };
  static const char *const classes[] = {"data.table", "data.frame"};
  static const char *const key[] = {"id"};
  if (TYPEOF(translation) != VECSXP || ALTREP(translation) ||
      XLENGTH(translation) != 4 ||
      !paradox_params_supported_table_attributes(translation, TRUE) ||
      !paradox_domain_exact_string_vector(
        Rf_getAttrib(translation, R_NamesSymbol),
        column_names,
        4,
        work_since_interrupt
      ) || !paradox_domain_exact_string_vector(
        Rf_getAttrib(translation, R_ClassSymbol),
        classes,
        2,
        work_since_interrupt
      ) || !paradox_domain_exact_string_vector(
        Rf_getAttrib(translation, Rf_install("sorted")),
        key,
        1,
        work_since_interrupt
      )) {
    return FALSE;
  }
  SEXP ids = PROTECT(VECTOR_ELT(translation, 0));
  SEXP originals = PROTECT(VECTOR_ELT(translation, 1));
  SEXP owners = PROTECT(VECTOR_ELT(translation, 2));
  SEXP owner_names = PROTECT(VECTOR_ELT(translation, 3));
  const R_xlen_t row_count = XLENGTH(ids);
  if (TYPEOF(ids) != STRSXP || TYPEOF(originals) != STRSXP ||
      TYPEOF(owners) != INTSXP || TYPEOF(owner_names) != STRSXP ||
      ALTREP(ids) || ALTREP(originals) || ALTREP(owners) ||
      ALTREP(owner_names) ||
      !paradox_api_has_no_attributes(ids) ||
      !paradox_api_has_no_attributes(originals) ||
      !paradox_api_has_no_attributes(owners) ||
      !paradox_api_has_no_attributes(owner_names) ||
      XLENGTH(originals) != row_count || XLENGTH(owners) != row_count ||
      XLENGTH(owner_names) != row_count || row_count > INT_MAX ||
      Rf_any_duplicated(ids, FALSE) != 0) {
    UNPROTECT(4);
    return FALSE;
  }
  const R_xlen_t set_count = XLENGTH(sets);
  SEXP set_names = PROTECT(Rf_getAttrib(sets, R_NamesSymbol));
  if (TYPEOF(set_names) != STRSXP || ALTREP(set_names) ||
      XLENGTH(set_names) != set_count) {
    UNPROTECT(5);
    return FALSE;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    const int owner = INTEGER_ELT(owners, row);
    if (owner == NA_INTEGER || owner < 1 ||
        (R_xlen_t) owner > set_count ||
        STRING_ELT(owner_names, row) == NA_STRING ||
        !paradox_domain_strings_equal(
          STRING_ELT(owner_names, row),
          STRING_ELT(set_names, (R_xlen_t) owner - 1)
        ) || !translated_id_matches(
          STRING_ELT(ids, row),
          STRING_ELT(originals, row),
          STRING_ELT(owner_names, row),
          postfix
        )) {
      UNPROTECT(5);
      return FALSE;
    }
    if (row > 0 && strcmp(
        CHAR(STRING_ELT(ids, row - 1)),
        CHAR(STRING_ELT(ids, row))
      ) > 0) {
      UNPROTECT(5);
      return FALSE;
    }
  }
  UNPROTECT(5);
  return TRUE;
}

/* Revalidate and materialize only the translation columns used by store
 * planning. Every later count/fill operation reads these ordinary snapshots,
 * never the callback-capable table that was admitted above. */
static SEXP snapshot_store_translation(SEXP translation, SEXP set_names,
    R_xlen_t child_count, int postfix,
    R_xlen_t *work_since_interrupt) {
  SEXP ids = PROTECT(VECTOR_ELT(translation, 0));
  SEXP originals = PROTECT(VECTOR_ELT(translation, 1));
  SEXP owners = PROTECT(VECTOR_ELT(translation, 2));
  SEXP owner_names = PROTECT(VECTOR_ELT(translation, 3));
  const R_xlen_t row_count = XLENGTH(ids);
  if (TYPEOF(ids) != STRSXP || TYPEOF(originals) != STRSXP ||
      TYPEOF(owners) != INTSXP || TYPEOF(owner_names) != STRSXP ||
      XLENGTH(originals) != row_count || XLENGTH(owners) != row_count ||
      XLENGTH(owner_names) != row_count || row_count > INT_MAX) {
    UNPROTECT(4);
    return R_NilValue;
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP stable_ids = PROTECT(Rf_allocVector(STRSXP, row_count));
  SET_VECTOR_ELT(result, 0, stable_ids);
  UNPROTECT(1);
  SEXP stable_originals = PROTECT(Rf_allocVector(STRSXP, row_count));
  SET_VECTOR_ELT(result, 1, stable_originals);
  UNPROTECT(1);
  SEXP stable_owners = PROTECT(Rf_allocVector(INTSXP, row_count));
  SET_VECTOR_ELT(result, 2, stable_owners);
  UNPROTECT(1);

  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP id = STRING_ELT(ids, row);
    SEXP original = STRING_ELT(originals, row);
    SEXP owner_name = STRING_ELT(owner_names, row);
    const int owner = INTEGER_ELT(owners, row);
    if (id == NA_STRING || original == NA_STRING || owner_name == NA_STRING ||
        owner == NA_INTEGER || owner < 1 ||
        (R_xlen_t) owner > child_count ||
        !paradox_domain_strings_equal(
          owner_name,
          STRING_ELT(set_names, (R_xlen_t) owner - 1)
        ) || !translated_id_matches(
          id,
          original,
          owner_name,
          postfix
        ) || (row != 0 && strcmp(
          CHAR(STRING_ELT(stable_ids, row - 1)),
          CHAR(id)
        ) >= 0)) {
      UNPROTECT(5);
      return R_NilValue;
    }
    SET_STRING_ELT(stable_ids, row, id);
    SET_STRING_ELT(stable_originals, row, original);
    SET_INTEGER_ELT(stable_owners, row, owner);
  }

  UNPROTECT(5);
  return result;
}

SEXP paradox_param_set_collection_store_plan(SEXP private_environment,
    SEXP self, SEXP sets, SEXP values) {
  R_xlen_t work_since_interrupt = 0;
  int protected_count = 0;
  SEXP set_names = R_NilValue;
  SEXP value_names = R_NilValue;
  R_xlen_t original_child_count = 0;
  R_xlen_t original_value_count = 0;
  int postfix = FALSE;
  if (!exact_collection(self, &work_since_interrupt) ||
      TYPEOF(private_environment) != ENVSXP ||
      !paradox_domain_owns_private_environment(self, private_environment) ||
      !ordinary_plain_list(sets, &set_names, &original_child_count) ||
      set_names == R_NilValue) {
    return R_NilValue;
  }
  PROTECT(set_names);
  ++protected_count;
  if (original_child_count > INT_MAX ||
      !plain_list(values, &value_names, &original_value_count)) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  PROTECT(value_names);
  ++protected_count;
  if (original_value_count > INT_MAX) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }

  SEXP current_sets = PROTECT(paradox_domain_local_value(
    private_environment,
    ".sets"
  ));
  ++protected_count;
  SEXP translation = PROTECT(paradox_domain_local_value(
    private_environment,
    ".translation"
  ));
  ++protected_count;
  SEXP postfix_sexp = PROTECT(paradox_domain_local_value(
    private_environment,
    ".postfix"
  ));
  ++protected_count;
  if (current_sets != sets || translation == R_UnboundValue ||
      !exact_flag(postfix_sexp, &postfix)) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  if (
      !exact_translation(
        translation,
        sets,
        postfix,
        &work_since_interrupt
      )) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }

  SEXP stable_values = PROTECT(snapshot_plain_list(
    values,
    value_names,
    original_value_count,
    &work_since_interrupt
  ));
  ++protected_count;
  if (stable_values == R_NilValue ||
      XLENGTH(stable_values) != original_value_count) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  values = stable_values;
  value_names = Rf_getAttrib(values, R_NamesSymbol);

  /* Value ALTREP methods are allowed during the input snapshot.  Authenticate
   * the collection state again afterwards, because the R wrapper will apply
   * this plan to its original `sets` local.  A replaced set list must fall
   * through; a replacement translation can be admitted only after complete
   * validation against that still-live list. */
  SEXP live_sets = PROTECT(paradox_domain_local_value(
    private_environment,
    ".sets"
  ));
  ++protected_count;
  SEXP live_translation = PROTECT(paradox_domain_local_value(
    private_environment,
    ".translation"
  ));
  ++protected_count;
  SEXP live_postfix = PROTECT(paradox_domain_local_value(
    private_environment,
    ".postfix"
  ));
  ++protected_count;
  SEXP live_set_names = R_NilValue;
  R_xlen_t child_count = 0;
  int current_postfix = FALSE;
  if (live_sets != sets || live_translation == R_UnboundValue ||
      !ordinary_plain_list(
        live_sets,
        &live_set_names,
        &child_count
      ) ||
      live_set_names == R_NilValue) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  PROTECT(live_set_names);
  ++protected_count;
  if (!exact_flag(live_postfix, &current_postfix)) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  if (child_count > INT_MAX || !exact_translation(
      live_translation,
      live_sets,
      current_postfix,
      &work_since_interrupt
    )) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }

  SEXP stable_sets = PROTECT(snapshot_plain_list(
    live_sets,
    live_set_names,
    child_count,
    &work_since_interrupt
  ));
  ++protected_count;
  if (stable_sets == R_NilValue || XLENGTH(stable_sets) != child_count) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  sets = stable_sets;
  set_names = Rf_getAttrib(sets, R_NamesSymbol);
  translation = live_translation;
  postfix = current_postfix;

  for (R_xlen_t child = 0; child < child_count; ++child) {
    paradox_domain_account_work(&work_since_interrupt);
    SEXP child_set = VECTOR_ELT(sets, child);
    if (TYPEOF(child_set) != ENVSXP ||
        !Rf_inherits(child_set, "ParamSet")) {
      UNPROTECT(protected_count);
      return R_NilValue;
    }
  }

  SEXP translation_snapshot = PROTECT(snapshot_store_translation(
    translation,
    set_names,
    child_count,
    postfix,
    &work_since_interrupt
  ));
  ++protected_count;
  if (translation_snapshot == R_NilValue) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  SEXP translation_ids = VECTOR_ELT(translation_snapshot, 0);
  SEXP originals = VECTOR_ELT(translation_snapshot, 1);
  SEXP owners = VECTOR_ELT(translation_snapshot, 2);
  const R_xlen_t translation_rows = XLENGTH(translation_ids);
  const R_xlen_t value_count = XLENGTH(values);

  SEXP matches;
  if (value_names == R_NilValue) {
    matches = PROTECT(Rf_allocVector(INTSXP, value_count));
    for (R_xlen_t index = 0; index < value_count; ++index) {
      SET_INTEGER_ELT(matches, index, 0);
    }
  } else {
    matches = PROTECT(Rf_match(
      translation_ids,
      value_names,
      0
    ));
  }
  ++protected_count;
  if (TYPEOF(matches) != INTSXP ||
      XLENGTH(matches) != value_count) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }

  R_xlen_t *counts = paradox_temporary_alloc(
    child_count == 0 ? 1 : child_count,
    sizeof(*counts)
  );
  R_xlen_t *positions = paradox_temporary_alloc(
    child_count == 0 ? 1 : child_count,
    sizeof(*positions)
  );
  R_xlen_t *filled = paradox_temporary_alloc(
    child_count == 0 ? 1 : child_count,
    sizeof(*filled)
  );
  for (R_xlen_t child = 0; child < child_count; ++child) {
    counts[child] = 0;
    positions[child] = 0;
    filled[child] = 0;
  }
  for (R_xlen_t index = 0; index < value_count; ++index) {
    paradox_domain_account_work(&work_since_interrupt);
    const int matched = INTEGER_ELT(matches, index);
    if (matched > 0) {
      const R_xlen_t row = (R_xlen_t) matched - 1;
      if (row >= translation_rows) {
        UNPROTECT(protected_count);
        Rf_error("Internal error: invalid collection store match");
      }
      const int owner = INTEGER_ELT(owners, row);
      if (owner < 1 || (R_xlen_t) owner > child_count) {
        UNPROTECT(protected_count);
        Rf_error("Internal error: invalid collection store owner");
      }
      ++counts[(R_xlen_t) owner - 1];
    }
  }

  SEXP order = PROTECT(Rf_allocVector(INTSXP, child_count));
  ++protected_count;
  SEXP assignments = PROTECT(Rf_allocVector(VECSXP, child_count));
  ++protected_count;
  R_xlen_t output = 0;
  for (int touched = TRUE; touched >= FALSE; --touched) {
    for (R_xlen_t child = 0; child < child_count; ++child) {
      paradox_domain_account_work(&work_since_interrupt);
      if ((counts[child] != 0) == touched) {
        if (output >= child_count) {
          UNPROTECT(protected_count);
          Rf_error("Internal error: collection store plan exceeded capacity");
        }
        positions[child] = output;
        SET_INTEGER_ELT(order, output, (int) child + 1);
        SEXP assignment = PROTECT(Rf_allocVector(
          VECSXP,
          counts[child]
        ));
        SEXP assignment_names = PROTECT(Rf_allocVector(
          STRSXP,
          counts[child]
        ));
        Rf_setAttrib(assignment, R_NamesSymbol, assignment_names);
        SET_VECTOR_ELT(assignments, output, assignment);
        UNPROTECT(2);
        ++output;
      }
    }
  }
  if (output != child_count) {
    UNPROTECT(protected_count);
    Rf_error("Internal error: incomplete collection store child plan");
  }

  for (R_xlen_t index = 0; index < value_count; ++index) {
    paradox_domain_account_work(&work_since_interrupt);
    const int matched = INTEGER_ELT(matches, index);
    if (matched > 0) {
      const R_xlen_t row = (R_xlen_t) matched - 1;
      if (row >= translation_rows) {
        UNPROTECT(protected_count);
        Rf_error("Internal error: invalid collection store match");
      }
      const int owner = INTEGER_ELT(owners, row);
      if (owner < 1 || (R_xlen_t) owner > child_count) {
        UNPROTECT(protected_count);
        Rf_error("Internal error: invalid collection store owner");
      }
      const R_xlen_t child = (R_xlen_t) owner - 1;
      SEXP assignment = VECTOR_ELT(assignments, positions[child]);
      SEXP assignment_names = Rf_getAttrib(assignment, R_NamesSymbol);
      const R_xlen_t destination = filled[child];
      if (destination >= counts[child]) {
        UNPROTECT(protected_count);
        Rf_error("Internal error: collection child assignment overflow");
      }
      SET_VECTOR_ELT(
        assignment,
        destination,
        VECTOR_ELT(values, index)
      );
      SET_STRING_ELT(
        assignment_names,
        destination,
        STRING_ELT(originals, row)
      );
      ++filled[child];
    }
  }
  for (R_xlen_t child = 0; child < child_count; ++child) {
    if (filled[child] != counts[child]) {
      UNPROTECT(protected_count);
      Rf_error("Internal error: incomplete collection child assignment");
    }
  }

  SEXP plan = PROTECT(Rf_allocVector(VECSXP, 2));
  ++protected_count;
  SET_VECTOR_ELT(plan, 0, order);
  SET_VECTOR_ELT(plan, 1, assignments);
  UNPROTECT(protected_count);
  return plan;
}
