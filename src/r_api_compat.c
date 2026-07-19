#include <stddef.h>
#include <string.h>

#include "r_api_compat.h"

#define PARADOX_API_MAX_ALLOWED_ATTRIBUTES ((size_t) 16)

SEXP paradox_api_closure_formals(SEXP closure) {
#if R_VERSION >= R_Version(4, 5, 0)
  return R_ClosureFormals(closure);
#else
  return FORMALS(closure);
#endif
}

#if R_VERSION < R_Version(4, 6, 0)
static int attributes_match(SEXP value,
    const char *const *allowed_names, size_t allowed_count,
    R_xlen_t required_count) {
  unsigned char seen[PARADOX_API_MAX_ALLOWED_ATTRIBUTES] = {0};
  R_xlen_t count = 0;
  SEXP attributes = ATTRIB(value);
  while (attributes != R_NilValue) {
    if (TYPEOF(attributes) != LISTSXP ||
        count >= (R_xlen_t) allowed_count) {
      return FALSE;
    }
    SEXP tag = TAG(attributes);
    if (TYPEOF(tag) != SYMSXP) {
      return FALSE;
    }
    const char *name = CHAR(PRINTNAME(tag));
    size_t allowed = 0;
    while (allowed < allowed_count &&
        strcmp(name, allowed_names[allowed]) != 0) {
      ++allowed;
    }
    if (allowed == allowed_count || seen[allowed]) {
      return FALSE;
    }
    seen[allowed] = 1;
    ++count;
    attributes = CDR(attributes);
  }
  return required_count < 0 || count == required_count;
}
#endif

int paradox_api_has_no_attributes(SEXP value) {
#if R_VERSION >= R_Version(4, 5, 0)
  return !ANY_ATTRIB(value);
#else
  return ATTRIB(value) == R_NilValue;
#endif
}

int paradox_api_has_single_attribute(SEXP value, const char *name) {
  if (name == NULL) {
    Rf_error("Internal error: missing public attribute name");
  }
#if R_VERSION >= R_Version(4, 6, 0)
  SEXP symbol = Rf_install(name);
  return R_getAttribCount(value) == 1 && R_hasAttrib(value, symbol);
#else
  const char *const allowed[] = {name};
  return attributes_match(value, allowed, 1, 1);
#endif
}

int paradox_api_has_only_attributes(SEXP value,
    const char *const *allowed_names, size_t allowed_count) {
  if (allowed_count > PARADOX_API_MAX_ALLOWED_ATTRIBUTES ||
      (allowed_count != 0 && allowed_names == NULL)) {
    Rf_error("Internal error: invalid public attribute allow-list");
  }
#if R_VERSION >= R_Version(4, 6, 0)
  SEXP symbols[PARADOX_API_MAX_ALLOWED_ATTRIBUTES];
  for (size_t index = 0; index < allowed_count; ++index) {
    symbols[index] = Rf_install(allowed_names[index]);
  }
  const R_xlen_t count = R_getAttribCount(value);
  if (count > (R_xlen_t) allowed_count) {
    return FALSE;
  }
  R_xlen_t present = 0;
  for (size_t index = 0; index < allowed_count; ++index) {
    present += R_hasAttrib(value, symbols[index]);
  }
  return count == present;
#else
  return attributes_match(
    value,
    allowed_names,
    allowed_count,
    -1
  );
#endif
}

static int plain_binding_boundary(SEXP environment, SEXP symbol) {
  return TYPEOF(environment) == ENVSXP && !Rf_isS4(environment) &&
    TYPEOF(symbol) == SYMSXP &&
    R_existsVarInFrame(environment, symbol) &&
    !R_BindingIsActive(symbol, environment);
}

SEXP paradox_api_plain_binding_snapshot(SEXP environment, SEXP symbol) {
  if (!plain_binding_boundary(environment, symbol)) {
    return R_UnboundValue;
  }
#if R_VERSION >= R_Version(4, 6, 0)
  if (R_GetBindingType(symbol, environment) != R_BindingTypeValue) {
    return R_UnboundValue;
  }
  return Rf_eval(symbol, environment);
#else
  SEXP result = Rf_findVarInFrame(environment, symbol);
  return result == R_UnboundValue || TYPEOF(result) == PROMSXP
    ? R_UnboundValue
    : result;
#endif
}

SEXP paradox_api_plain_binding_scan(SEXP environment, SEXP symbol) {
  return paradox_api_plain_binding_snapshot(environment, symbol);
}
