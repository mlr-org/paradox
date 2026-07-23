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

#if R_VERSION >= R_Version(4, 6, 0)
typedef struct {
  SEXP symbol;
  SEXP value;
  int found;
} raw_attribute_state_t;

static SEXP select_raw_attribute(SEXP tag, SEXP value, void *data) {
  raw_attribute_state_t *state = data;
  if (tag != state->symbol) return NULL;
  state->value = value;
  state->found = TRUE;
  /* R_mapAttrib() uses a C NULL as its continue sentinel. A symbol is an
   * allocation-free, non-NULL stop value even when the stored value is NULL. */
  return tag;
}
#endif

SEXP paradox_api_raw_attribute(SEXP value, SEXP symbol) {
  if (TYPEOF(symbol) != SYMSXP) {
    Rf_error("Internal error: attribute selector must be a symbol");
  }
#if R_VERSION >= R_Version(4, 6, 0)
  raw_attribute_state_t state = {symbol, R_NilValue, FALSE};
  (void) R_mapAttrib(value, select_raw_attribute, &state);
  return state.found ? state.value : R_NilValue;
#else
  for (SEXP attributes = ATTRIB(value);
      attributes != R_NilValue;
      attributes = CDR(attributes)) {
    if (TYPEOF(attributes) != LISTSXP || TYPEOF(TAG(attributes)) != SYMSXP) {
      Rf_error("Internal error: malformed attribute pairlist");
    }
    if (TAG(attributes) == symbol) return CAR(attributes);
  }
  return R_NilValue;
#endif
}

#if R_VERSION >= R_Version(4, 6, 0)
typedef struct {
  paradox_api_attribute_callback_t callback;
  void *data;
  R_xlen_t count;
} attribute_map_state_t;

static SEXP map_stored_attribute(SEXP tag, SEXP value, void *data) {
  attribute_map_state_t *state = data;
  if (state->callback != NULL) {
    state->callback(tag, value, state->data);
  }
  ++state->count;
  return NULL;
}
#endif

R_xlen_t paradox_api_stored_attribute_count(SEXP value) {
#if R_VERSION >= R_Version(4, 6, 0)
  attribute_map_state_t state = {NULL, NULL, 0};
  (void) R_mapAttrib(value, map_stored_attribute, &state);
  return state.count;
#else
  R_xlen_t count = 0;
  for (SEXP attributes = ATTRIB(value);
      attributes != R_NilValue;
      attributes = CDR(attributes)) {
    if (TYPEOF(attributes) != LISTSXP) {
      Rf_error("Internal error: malformed attribute pairlist");
    }
    if (count == R_XLEN_T_MAX) {
      Rf_error("Internal error: too many stored attributes");
    }
    ++count;
  }
  return count;
#endif
}

void paradox_api_map_stored_attributes(
    SEXP value,
    paradox_api_attribute_callback_t callback,
    void *data) {
  if (callback == NULL) {
    Rf_error("Internal error: missing attribute callback");
  }
#if R_VERSION >= R_Version(4, 6, 0)
  attribute_map_state_t state = {callback, data, 0};
  (void) R_mapAttrib(value, map_stored_attribute, &state);
#else
  for (SEXP attributes = ATTRIB(value);
      attributes != R_NilValue;
      attributes = CDR(attributes)) {
    if (TYPEOF(attributes) != LISTSXP) {
      Rf_error("Internal error: malformed attribute pairlist");
    }
    callback(TAG(attributes), CAR(attributes), data);
  }
#endif
}

static int plain_binding_boundary(SEXP environment, SEXP symbol) {
  return TYPEOF(environment) == ENVSXP && !Rf_isS4(environment) &&
    TYPEOF(symbol) == SYMSXP &&
    R_existsVarInFrame(environment, symbol) &&
    !R_BindingIsActive(symbol, environment);
}

#if R_VERSION < R_Version(4, 6, 0)
SEXP paradox_api_stored_binding_snapshot(
    SEXP environment, SEXP symbol) {
  if (!plain_binding_boundary(environment, symbol)) {
    return R_UnboundValue;
  }
  return Rf_findVarInFrame(environment, symbol);
}
#endif

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
  SEXP result = paradox_api_stored_binding_snapshot(environment, symbol);
  return result == R_UnboundValue || TYPEOF(result) == PROMSXP
    ? R_UnboundValue
    : result;
#endif
}

SEXP paradox_api_plain_binding_scan(SEXP environment, SEXP symbol) {
  return paradox_api_plain_binding_snapshot(environment, symbol);
}

#if R_VERSION < R_Version(4, 6, 0)
void paradox_api_promise_snapshot(
    SEXP promise, paradox_api_promise_snapshot_t *snapshot) {
  if (TYPEOF(promise) != PROMSXP || snapshot == NULL) {
    Rf_error("Internal error: invalid promise snapshot request");
  }
  snapshot->expression = R_PromiseExpr(promise);
  snapshot->environment = PRENV(promise);
  snapshot->value = PRVALUE(promise);
  snapshot->forced = snapshot->value != R_UnboundValue;
}
#endif
