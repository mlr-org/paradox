#include <stddef.h>
#include <string.h>

#include "r_api_compat.h"

#define PARADOX_API_MAX_ALLOWED_ATTRIBUTES ((size_t) 16)

#if R_VERSION < R_Version(4, 5, 0)
static SEXP evaluate_base_unary(const char *name, SEXP value) {
  PROTECT(value);
  SEXP call = PROTECT(Rf_lang2(Rf_install(name), value));
  SEXP result = Rf_eval(call, R_BaseEnv);
  UNPROTECT(2);
  return result;
}
#endif

SEXP paradox_api_closure_expression(SEXP closure) {
#if R_VERSION >= R_Version(4, 5, 0)
  return R_ClosureExpr(closure);
#else
  return evaluate_base_unary("body", closure);
#endif
}

SEXP paradox_api_closure_formals(SEXP closure) {
#if R_VERSION >= R_Version(4, 5, 0)
  return R_ClosureFormals(closure);
#else
  return evaluate_base_unary("formals", closure);
#endif
}

SEXP paradox_api_closure_environment(SEXP closure) {
#if R_VERSION >= R_Version(4, 5, 0)
  return R_ClosureEnv(closure);
#else
  return evaluate_base_unary("environment", closure);
#endif
}

SEXP paradox_api_parent_environment(SEXP environment) {
  if (environment == R_EmptyEnv) {
    return R_NilValue;
  }
#if R_VERSION >= R_Version(4, 5, 0)
  return R_ParentEnv(environment);
#else
  return evaluate_base_unary("parent.env", environment);
#endif
}

SEXP paradox_api_registered_namespace(const char *name) {
#if R_VERSION >= R_Version(4, 6, 0)
  return R_getRegisteredNamespace(name);
#else
  SEXP specification = PROTECT(Rf_mkString(name));
  SEXP loaded_call = PROTECT(Rf_lang2(
    Rf_install("isNamespaceLoaded"),
    specification
  ));
  SEXP loaded = PROTECT(Rf_eval(loaded_call, R_BaseEnv));
  if (TYPEOF(loaded) != LGLSXP || XLENGTH(loaded) != 1 ||
      LOGICAL_ELT(loaded, 0) != TRUE) {
    UNPROTECT(3);
    return R_NilValue;
  }
  SEXP get_call = PROTECT(Rf_lang2(
    Rf_install("getNamespace"),
    specification
  ));
  SEXP environment = Rf_eval(get_call, R_BaseEnv);
  UNPROTECT(4);
  return environment;
#endif
}

#if R_VERSION < R_Version(4, 6, 0)
static int snapshot_attributes_match(SEXP value,
    const char *const *allowed_names, size_t allowed_count,
    R_xlen_t required_count) {
  PROTECT(value);
  SEXP carrier = PROTECT(R_MakeExternalPtr(NULL, R_NilValue, R_NilValue));
  SHALLOW_DUPLICATE_ATTRIB(carrier, value);
  SEXP call = PROTECT(Rf_lang2(Rf_install("attributes"), carrier));
  SEXP attributes = PROTECT(Rf_eval(call, R_BaseEnv));
  if (attributes == R_NilValue) {
    UNPROTECT(4);
    return required_count < 0 || required_count == 0;
  }
  if (TYPEOF(attributes) != VECSXP || ALTREP(attributes)) {
    UNPROTECT(4);
    return FALSE;
  }
  const R_xlen_t count = XLENGTH(attributes);
  if ((required_count >= 0 && count != required_count) ||
      count > (R_xlen_t) allowed_count) {
    UNPROTECT(4);
    return FALSE;
  }
  SEXP names = PROTECT(Rf_getAttrib(attributes, R_NamesSymbol));
  if (TYPEOF(names) != STRSXP || ALTREP(names) || XLENGTH(names) != count) {
    UNPROTECT(5);
    return FALSE;
  }

  unsigned char seen[PARADOX_API_MAX_ALLOWED_ATTRIBUTES] = {0};
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP name = STRING_ELT(names, index);
    if (name == NA_STRING || Rf_getCharCE(name) == CE_BYTES) {
      UNPROTECT(5);
      return FALSE;
    }
    size_t allowed = 0;
    while (allowed < allowed_count &&
        strcmp(CHAR(name), allowed_names[allowed]) != 0) {
      ++allowed;
    }
    if (allowed == allowed_count || seen[allowed]) {
      UNPROTECT(5);
      return FALSE;
    }
    seen[allowed] = 1;
  }
  UNPROTECT(5);
  return TRUE;
}
#endif

int paradox_api_has_no_attributes(SEXP value) {
#if R_VERSION >= R_Version(4, 5, 0)
  return !ANY_ATTRIB(value);
#else
  return snapshot_attributes_match(value, NULL, 0, 0);
#endif
}

int paradox_api_has_single_attribute(SEXP value, const char *name) {
#if R_VERSION >= R_Version(4, 6, 0)
  PROTECT(value);
  SEXP symbol = Rf_install(name);
  const int exact = R_getAttribCount(value) == 1 &&
    R_hasAttrib(value, symbol);
  UNPROTECT(1);
  return exact;
#else
  const char *const allowed[] = {name};
  return snapshot_attributes_match(value, allowed, 1, 1);
#endif
}

int paradox_api_has_only_attributes(SEXP value,
    const char *const *allowed_names, size_t allowed_count) {
  if (allowed_count > PARADOX_API_MAX_ALLOWED_ATTRIBUTES ||
      (allowed_count != 0 && allowed_names == NULL)) {
    Rf_error("Internal error: invalid public attribute allow-list");
  }
#if R_VERSION >= R_Version(4, 6, 0)
  PROTECT(value);
  SEXP symbols[PARADOX_API_MAX_ALLOWED_ATTRIBUTES];
  for (size_t index = 0; index < allowed_count; ++index) {
    symbols[index] = Rf_install(allowed_names[index]);
  }
  const R_xlen_t count = R_getAttribCount(value);
  if (count > (R_xlen_t) allowed_count) {
    UNPROTECT(1);
    return FALSE;
  }
  R_xlen_t present = 0;
  for (size_t index = 0; index < allowed_count; ++index) {
    present += R_hasAttrib(value, symbols[index]);
  }
  const int exact = count == present;
  UNPROTECT(1);
  return exact;
#else
  return snapshot_attributes_match(
    value,
    allowed_names,
    allowed_count,
    -1
  );
#endif
}

SEXP paradox_api_local_value(SEXP environment, SEXP symbol) {
  if (TYPEOF(environment) != ENVSXP || TYPEOF(symbol) != SYMSXP) {
    return R_UnboundValue;
  }
#if R_VERSION >= R_Version(4, 6, 0)
  if (R_GetBindingType(symbol, environment) != R_BindingTypeValue) {
    return R_UnboundValue;
  }
  SEXP value = R_getVarEx(symbol, environment, FALSE, R_UnboundValue);
  if (value == R_UnboundValue || value == R_MissingArg ||
      TYPEOF(value) == PROMSXP) {
    return R_UnboundValue;
  }
  return value;
#else
  /* Before R 4.6 the public API cannot distinguish an ordinary value from a
   * delayed binding without forcing it.  Authentication must be
   * observationally inert, so disable these native fast paths and let their
   * R implementations handle the object. */
  return R_UnboundValue;
#endif
}

SEXP paradox_api_stable_local_value(SEXP environment, SEXP symbol) {
  if (TYPEOF(environment) != ENVSXP || TYPEOF(symbol) != SYMSXP) {
    return R_UnboundValue;
  }
#if R_VERSION >= R_Version(4, 6, 0)
  const R_BindingType_t binding_type = R_GetBindingType(
    symbol,
    environment
  );
  if (binding_type != R_BindingTypeValue &&
      binding_type != R_BindingTypeForced) {
    return R_UnboundValue;
  }
  SEXP value = R_getVarEx(symbol, environment, FALSE, R_UnboundValue);
  if (value == R_UnboundValue || value == R_MissingArg ||
      TYPEOF(value) == PROMSXP) {
    return R_UnboundValue;
  }
  return value;
#else
  return R_UnboundValue;
#endif
}

SEXP paradox_api_evaluated_local_value(SEXP environment, SEXP symbol) {
  if (TYPEOF(environment) != ENVSXP || TYPEOF(symbol) != SYMSXP ||
      !R_existsVarInFrame(environment, symbol)) {
    return R_UnboundValue;
  }
#if R_VERSION >= R_Version(4, 5, 0)
  return R_getVarEx(symbol, environment, FALSE, R_UnboundValue);
#else
  return Rf_eval(symbol, environment);
#endif
}
