#include "r_api_compat.h"

static SEXP binding_snapshot_result(int ok, SEXP value) {
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SEXP flag = PROTECT(Rf_ScalarLogical(ok));

  SET_STRING_ELT(names, 0, Rf_mkChar("ok"));
  SET_STRING_ELT(names, 1, Rf_mkChar("value"));
  SET_VECTOR_ELT(result, 0, flag);
  SET_VECTOR_ELT(result, 1, ok ? value : R_NilValue);
  Rf_setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(3);
  return result;
}

SEXP paradox_plain_binding_snapshot(SEXP environment, SEXP name) {
  if (TYPEOF(environment) != ENVSXP || Rf_isS4(environment)) {
    Rf_error("`environment` must be an ordinary environment");
  }
  if (TYPEOF(name) != STRSXP || ALTREP(name) || Rf_isS4(name) ||
      Rf_isObject(name) || !paradox_api_has_no_attributes(name) ||
      XLENGTH(name) != 1) {
    Rf_error("`name` must be an ordinary non-empty character scalar");
  }

  SEXP label = STRING_ELT(name, 0);
  if (label == NA_STRING || Rf_getCharCE(label) == CE_BYTES ||
      CHAR(label)[0] == '\0') {
    Rf_error("`name` must be an ordinary non-empty character scalar");
  }

  SEXP value = PROTECT(paradox_api_plain_binding_snapshot(
    environment,
    Rf_installChar(label)
  ));
  SEXP result = binding_snapshot_result(value != R_UnboundValue, value);
  UNPROTECT(1);
  return result;
}
