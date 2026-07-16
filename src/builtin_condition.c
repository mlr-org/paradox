#include <stddef.h>
#include <string.h>

#include "builtin_condition.h"
#include "paramset_domain_common.h"
#include "r_api_compat.h"

static int exact_missing_formal(SEXP formal, const char *name) {
  return formal != R_NilValue && TAG(formal) == Rf_install(name) &&
    CAR(formal) == R_MissingArg;
}

static int exact_scalar_string(SEXP value, const char *expected) {
  return TYPEOF(value) == STRSXP && !ALTREP(value) && XLENGTH(value) == 1 &&
    paradox_domain_string_is(STRING_ELT(value, 0), expected);
}

static SEXP single_body_expression(SEXP body) {
  if (TYPEOF(body) != LANGSXP || CAR(body) != Rf_install("{") ||
      CDR(body) == R_NilValue || CDDR(body) != R_NilValue) {
    return R_UnboundValue;
  }
  return CADR(body);
}

static int exact_unary_call(SEXP call, const char *function, SEXP argument) {
  return TYPEOF(call) == LANGSXP && CAR(call) == Rf_install(function) &&
    CDR(call) != R_NilValue && CDDR(call) == R_NilValue &&
    CADR(call) == argument;
}

static int exact_two_missing_formals(SEXP function) {
  SEXP formal = paradox_api_closure_formals(function);
  if (!exact_missing_formal(formal, "cond")) {
    return FALSE;
  }
  formal = CDR(formal);
  return exact_missing_formal(formal, "x") && CDR(formal) == R_NilValue;
}

static int exact_condition_generic_body(SEXP function) {
  SEXP expression = single_body_expression(
    paradox_api_closure_expression(function)
  );
  if (TYPEOF(expression) != LANGSXP ||
      CAR(expression) != Rf_install("UseMethod") ||
      CDR(expression) == R_NilValue || CDDR(expression) != R_NilValue) {
    return FALSE;
  }
  SEXP generic = CADR(expression);
  return exact_scalar_string(generic, "condition_test") &&
    paradox_api_has_no_attributes(generic);
}

static int exact_condition_method_body(SEXP function,
    const char *relation) {
  SEXP expression = single_body_expression(
    paradox_api_closure_expression(function)
  );
  if (TYPEOF(expression) != LANGSXP || CAR(expression) != Rf_install("&") ||
      CDR(expression) == R_NilValue || CDDR(expression) == R_NilValue ||
      CDDDR(expression) != R_NilValue) {
    return FALSE;
  }

  SEXP x = Rf_install("x");
  SEXP cond = Rf_install("cond");
  SEXP missing = CADR(expression);
  SEXP comparison = CADDR(expression);
  if (TYPEOF(missing) != LANGSXP || CAR(missing) != Rf_install("!") ||
      CDR(missing) == R_NilValue || CDDR(missing) != R_NilValue ||
      !exact_unary_call(CADR(missing), "is.na", x)) {
    return FALSE;
  }

  SEXP rhs = TYPEOF(comparison) == LANGSXP &&
      CDR(comparison) != R_NilValue && CDDR(comparison) != R_NilValue
    ? CADDR(comparison)
    : R_UnboundValue;
  return TYPEOF(comparison) == LANGSXP &&
    CAR(comparison) == Rf_install(relation) &&
    CDR(comparison) != R_NilValue &&
    CDDR(comparison) != R_NilValue && CDDDR(comparison) == R_NilValue &&
    CADR(comparison) == x && TYPEOF(rhs) == LANGSXP &&
    CAR(rhs) == Rf_install("$") && CDR(rhs) != R_NilValue &&
    CDDR(rhs) != R_NilValue && CDDDR(rhs) == R_NilValue &&
    CADR(rhs) == cond && CADDR(rhs) == Rf_install("rhs");
}

static int canonical_condition_binding(SEXP namespace_environment,
    const char *name, const char *relation) {
  SEXP symbol = Rf_install(name);
  if (!R_existsVarInFrame(namespace_environment, symbol) ||
      R_BindingIsActive(symbol, namespace_environment) ||
      !R_BindingIsLocked(symbol, namespace_environment)) {
    return FALSE;
  }
  SEXP function = PROTECT(paradox_api_stable_local_value(
    namespace_environment,
    symbol
  ));
  const int exact = TYPEOF(function) == CLOSXP &&
    paradox_api_closure_environment(function) == namespace_environment &&
    exact_two_missing_formals(function) &&
    (relation == NULL
      ? exact_condition_generic_body(function)
      : exact_condition_method_body(function, relation));
  UNPROTECT(1);
  return exact;
}

/* The canonical methods read cond$rhs. The primitive still performs S3
 * dispatch for classed lists, so an otherwise untouched paradox namespace is
 * insufficient when a package has registered one of these methods in base. */
static int canonical_condition_dollar_dispatch(void) {
  SEXP table_symbol = Rf_install(".__S3MethodsTable__.");
  if (TYPEOF(R_BaseNamespace) != ENVSXP ||
      !R_existsVarInFrame(R_BaseNamespace, table_symbol) ||
      R_BindingIsActive(table_symbol, R_BaseNamespace) ||
      !R_BindingIsLocked(table_symbol, R_BaseNamespace)) {
    return FALSE;
  }
  SEXP table = PROTECT(paradox_api_stable_local_value(
    R_BaseNamespace,
    table_symbol
  ));
  if (TYPEOF(table) != ENVSXP || Rf_isObject(table)) {
    UNPROTECT(1);
    return FALSE;
  }
  static const char *const methods[] = {
    "$.CondEqual",
    "$.CondAnyOf",
    "$.Condition",
    "$.default"
  };
  for (size_t index = 0;
       index < sizeof(methods) / sizeof(methods[0]);
       ++index) {
    if (R_existsVarInFrame(table, Rf_install(methods[index]))) {
      UNPROTECT(1);
      return FALSE;
    }
  }
  UNPROTECT(1);
  return TRUE;
}

/* S3 registrations live in the generic namespace's method table.  Replacing
 * one with registerS3method() does not replace the same-named namespace
 * binding, so authenticating only the visible closures would bypass a live
 * replacement that the historical UseMethod() call observes. */
static int canonical_condition_method_table(SEXP namespace_environment) {
  SEXP table_symbol = Rf_install(".__S3MethodsTable__.");
  if (!R_existsVarInFrame(namespace_environment, table_symbol) ||
      R_BindingIsActive(table_symbol, namespace_environment) ||
      !R_BindingIsLocked(table_symbol, namespace_environment)) {
    return FALSE;
  }
  SEXP table = PROTECT(paradox_api_stable_local_value(
    namespace_environment,
    table_symbol
  ));
  if (TYPEOF(table) != ENVSXP || Rf_isObject(table)) {
    UNPROTECT(1);
    return FALSE;
  }

  static const char *const methods[] = {
    "condition_test.CondEqual",
    "condition_test.CondAnyOf"
  };
  for (size_t index = 0;
       index < sizeof(methods) / sizeof(methods[0]);
       ++index) {
    SEXP symbol = Rf_install(methods[index]);
    if (!R_existsVarInFrame(table, symbol) ||
        R_BindingIsActive(symbol, table) ||
        !R_existsVarInFrame(namespace_environment, symbol) ||
        R_BindingIsActive(symbol, namespace_environment) ||
        !R_BindingIsLocked(symbol, namespace_environment)) {
      UNPROTECT(1);
      return FALSE;
    }
    SEXP registered = PROTECT(paradox_api_stable_local_value(table, symbol));
    SEXP canonical = PROTECT(paradox_api_stable_local_value(
      namespace_environment,
      symbol
    ));
    const int exact = registered != R_UnboundValue &&
      registered == canonical && TYPEOF(registered) == CLOSXP;
    UNPROTECT(2);
    if (!exact) {
      UNPROTECT(1);
      return FALSE;
    }
  }
  UNPROTECT(1);
  return TRUE;
}

int paradox_builtin_condition_dispatch_is_canonical(
    SEXP namespace_environment) {
  return TYPEOF(namespace_environment) == ENVSXP &&
    canonical_condition_binding(
      namespace_environment,
      "condition_test",
      NULL
    ) && canonical_condition_binding(
      namespace_environment,
      "condition_test.CondEqual",
      "=="
    ) && canonical_condition_binding(
      namespace_environment,
      "condition_test.CondAnyOf",
      "%in%"
    ) && canonical_condition_method_table(namespace_environment) &&
    canonical_condition_dollar_dispatch();
}

static int condition_rhs_is_plain(SEXP rhs,
    paradox_builtin_condition_kind_t kind,
    R_xlen_t *work_since_interrupt) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(rhs);
  if ((type != LGLSXP && type != INTSXP && type != REALSXP &&
       type != STRSXP) || ALTREP(rhs) || Rf_isObject(rhs) ||
      !paradox_api_has_no_attributes(rhs)) {
    return FALSE;
  }
  const R_xlen_t size = XLENGTH(rhs);
  if ((kind == PARADOX_BUILTIN_CONDITION_EQUAL && size != 1) ||
      (kind == PARADOX_BUILTIN_CONDITION_ANY_OF && size == 0)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if ((type == LGLSXP && LOGICAL_ELT(rhs, index) == NA_LOGICAL) ||
        (type == INTSXP && INTEGER_ELT(rhs, index) == NA_INTEGER) ||
        (type == REALSXP && ISNAN(REAL_ELT(rhs, index))) ||
        (type == STRSXP &&
         (STRING_ELT(rhs, index) == NA_STRING ||
          Rf_getCharCE(STRING_ELT(rhs, index)) == CE_BYTES))) {
      return FALSE;
    }
  }
  return TRUE;
}

int paradox_builtin_condition_exact(SEXP condition,
    paradox_builtin_condition_kind_t *kind, SEXP *rhs,
    R_xlen_t *work_since_interrupt) {
  static const char *const names[] = {"rhs", "condition_format_string"};
  static const char *const equal_classes[] = {"CondEqual", "Condition"};
  static const char *const any_of_classes[] = {"CondAnyOf", "Condition"};
  static const char *const attributes[] = {"names", "class"};
  if (TYPEOF(condition) != VECSXP || ALTREP(condition) ||
      XLENGTH(condition) != 2 || !paradox_api_has_only_attributes(
        condition,
        attributes,
        2
      )) {
    return FALSE;
  }

  PROTECT(condition);
  SEXP condition_names = PROTECT(Rf_getAttrib(condition, R_NamesSymbol));
  SEXP classes = PROTECT(Rf_getAttrib(condition, R_ClassSymbol));
  if (!paradox_api_has_no_attributes(condition_names) ||
      !paradox_api_has_no_attributes(classes) ||
      !paradox_domain_exact_string_vector(
        condition_names,
        names,
        2,
        work_since_interrupt
      )) {
    UNPROTECT(3);
    return FALSE;
  }

  const char *format_text;
  if (paradox_domain_exact_string_vector(
      classes,
      equal_classes,
      2,
      work_since_interrupt
    )) {
    *kind = PARADOX_BUILTIN_CONDITION_EQUAL;
    format_text = "%s == %s";
  } else if (paradox_domain_exact_string_vector(
      classes,
      any_of_classes,
      2,
      work_since_interrupt
    )) {
    *kind = PARADOX_BUILTIN_CONDITION_ANY_OF;
    format_text = "%s %%in%% {%s}";
  } else {
    UNPROTECT(3);
    return FALSE;
  }

  SEXP format = PROTECT(VECTOR_ELT(condition, 1));
  SEXP candidate_rhs = PROTECT(VECTOR_ELT(condition, 0));
  const int exact = exact_scalar_string(format, format_text) &&
    paradox_api_has_no_attributes(format) && condition_rhs_is_plain(
      candidate_rhs,
      *kind,
      work_since_interrupt
    );
  if (exact) {
    *rhs = candidate_rhs;
  }
  UNPROTECT(5);
  return exact;
}

static int compatible_operand_types(SEXPTYPE value_type,
    SEXPTYPE rhs_type) {
  const int value_numeric = value_type == LGLSXP ||
    value_type == INTSXP || value_type == REALSXP;
  const int rhs_numeric = rhs_type == LGLSXP ||
    rhs_type == INTSXP || rhs_type == REALSXP;
  return (value_numeric && rhs_numeric) ||
    (value_type == STRSXP && rhs_type == STRSXP);
}

int paradox_builtin_condition_scalar_supported(SEXP value, SEXP rhs) {
  if (value == R_NilValue || Rf_inherits(value, "TuneToken")) {
    return TRUE;
  }
  const SEXPTYPE value_type = (SEXPTYPE) TYPEOF(value);
  const SEXPTYPE rhs_type = (SEXPTYPE) TYPEOF(rhs);
  if (!compatible_operand_types(value_type, rhs_type) || ALTREP(value) ||
      XLENGTH(value) != 1 || Rf_isObject(value) ||
      !paradox_api_has_no_attributes(value)) {
    return FALSE;
  }
  return value_type != STRSXP || STRING_ELT(value, 0) == NA_STRING ||
    Rf_getCharCE(STRING_ELT(value, 0)) != CE_BYTES;
}

int paradox_builtin_condition_column_supported(SEXP column, SEXP rhs,
    R_xlen_t *work_since_interrupt) {
  const SEXPTYPE column_type = (SEXPTYPE) TYPEOF(column);
  const SEXPTYPE rhs_type = (SEXPTYPE) TYPEOF(rhs);
  if (!compatible_operand_types(column_type, rhs_type) || ALTREP(column) ||
      Rf_isObject(column) || !paradox_api_has_no_attributes(column)) {
    return FALSE;
  }
  if (column_type != STRSXP) {
    return TRUE;
  }

  /* Once a Design plan is accepted, applying its masks must not allocate or
   * run finalizers. Mixed encodings require R's translating comparison and
   * therefore decline here to the established R condition path. */
  const R_xlen_t rows = XLENGTH(column);
  const R_xlen_t rhs_size = XLENGTH(rhs);
  for (R_xlen_t row = 0; row < rows; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP value = STRING_ELT(column, row);
    if (value == NA_STRING) {
      continue;
    }
    const cetype_t encoding = Rf_getCharCE(value);
    if (encoding == CE_BYTES) {
      return FALSE;
    }
    for (R_xlen_t index = 0; index < rhs_size; ++index) {
      SEXP candidate = STRING_ELT(rhs, index);
      if (candidate != value && Rf_getCharCE(candidate) != encoding) {
        return FALSE;
      }
    }
  }
  return TRUE;
}

int paradox_builtin_condition_element_matches(SEXP values,
    R_xlen_t index, SEXP rhs, R_xlen_t *work_since_interrupt) {
  const SEXPTYPE value_type = (SEXPTYPE) TYPEOF(values);
  const SEXPTYPE rhs_type = (SEXPTYPE) TYPEOF(rhs);
  if ((value_type == LGLSXP && LOGICAL_ELT(values, index) == NA_LOGICAL) ||
      (value_type == INTSXP && INTEGER_ELT(values, index) == NA_INTEGER) ||
      (value_type == REALSXP && ISNAN(REAL_ELT(values, index))) ||
      (value_type == STRSXP && STRING_ELT(values, index) == NA_STRING)) {
    return FALSE;
  }

  double real_value = 0.0;
  int integer_value = 0;
  if (value_type == REALSXP) {
    real_value = REAL_ELT(values, index);
  } else if (value_type == INTSXP) {
    integer_value = INTEGER_ELT(values, index);
    real_value = (double) integer_value;
  } else if (value_type == LGLSXP) {
    integer_value = LOGICAL_ELT(values, index);
    real_value = (double) integer_value;
  }

  const R_xlen_t size = XLENGTH(rhs);
  for (R_xlen_t rhs_index = 0; rhs_index < size; ++rhs_index) {
    paradox_domain_account_work(work_since_interrupt);
    if ((rhs_type == LGLSXP &&
         LOGICAL_ELT(rhs, rhs_index) == NA_LOGICAL) ||
        (rhs_type == INTSXP &&
         INTEGER_ELT(rhs, rhs_index) == NA_INTEGER) ||
        (rhs_type == REALSXP && ISNAN(REAL_ELT(rhs, rhs_index))) ||
        (rhs_type == STRSXP &&
         STRING_ELT(rhs, rhs_index) == NA_STRING)) {
      continue;
    }
    int equal = FALSE;
    if (value_type == STRSXP) {
      SEXP left = STRING_ELT(values, index);
      SEXP right = STRING_ELT(rhs, rhs_index);
      if (left == right) {
        equal = TRUE;
      } else if (Rf_getCharCE(left) == Rf_getCharCE(right)) {
        equal = strcmp(CHAR(left), CHAR(right)) == 0;
      } else {
        equal = paradox_domain_strings_equal(left, right);
      }
    } else if (value_type == REALSXP || rhs_type == REALSXP) {
      double rhs_value;
      if (rhs_type == REALSXP) {
        rhs_value = REAL_ELT(rhs, rhs_index);
      } else if (rhs_type == INTSXP) {
        const int integer_rhs = INTEGER_ELT(rhs, rhs_index);
        rhs_value = (double) integer_rhs;
      } else {
        const int logical_rhs = LOGICAL_ELT(rhs, rhs_index);
        rhs_value = (double) logical_rhs;
      }
      equal = real_value == rhs_value;
    } else {
      const int rhs_value = rhs_type == INTSXP
        ? INTEGER_ELT(rhs, rhs_index)
        : LOGICAL_ELT(rhs, rhs_index);
      equal = integer_value == rhs_value;
    }
    if (equal) {
      return TRUE;
    }
  }
  return FALSE;
}
