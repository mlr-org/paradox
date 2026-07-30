#include <stddef.h>
#include <string.h>

#include "r_api_compat.h"

#define PARADOX_API_MAX_ALLOWED_ATTRIBUTES ((size_t) 16)

#if R_VERSION < R_Version(4, 5, 0)
/* Cold public-R bridge for accessors that became supported native API in
 * R 4.5. Compile it out entirely once every caller selects that direct API. */
static SEXP evaluate_base_unary(const char *name, SEXP argument) {
  if (name == NULL) {
    Rf_error("Internal error: missing base function name");
  }
  PROTECT(argument);
  SEXP function = PROTECT(Rf_findFun(Rf_install(name), R_BaseEnv));
  SEXP call = PROTECT(Rf_lang2(function, argument));
  SEXP result = PROTECT(Rf_eval(call, R_BaseEnv));
  UNPROTECT(4);
  return result;
}
#endif

SEXP paradox_api_closure_formals(SEXP closure) {
#if R_VERSION >= R_Version(4, 5, 0)
  return R_ClosureFormals(closure);
#else
  return FORMALS(closure);
#endif
}

SEXP paradox_api_closure_expression(SEXP closure) {
#if R_VERSION >= R_Version(4, 5, 0)
  return R_ClosureExpr(closure);
#else
  return evaluate_base_unary("body", closure);
#endif
}

SEXP paradox_api_bytecode_expression(SEXP bytecode) {
#if R_VERSION >= R_Version(4, 5, 0)
  return R_BytecodeExpr(bytecode);
#else
  /*
   * Old R has no public direct-BCODESXP expression accessor.  Its public
   * as.function.default() accepts bytecode as the sole body element without
   * executing it; public body() then returns the underlying expression. This
   * is a cold recursive-migration edge, not an ordinary package hot path.
   */
  PROTECT(bytecode);
  SEXP definition = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(definition, 0, bytecode);
  SEXP function = PROTECT(Rf_findFun(
    Rf_install("as.function.default"),
    R_BaseEnv
  ));
  SEXP call = PROTECT(Rf_lang3(function, definition, R_EmptyEnv));
  SET_TAG(CDDR(call), Rf_install("envir"));
  SEXP closure = PROTECT(Rf_eval(call, R_BaseEnv));
  SEXP result = PROTECT(evaluate_base_unary("body", closure));
  UNPROTECT(6);
  return result;
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
#if R_VERSION >= R_Version(4, 5, 0)
  return R_ParentEnv(environment);
#else
  return evaluate_base_unary("parent.env", environment);
#endif
}

SEXP paradox_api_option_snapshot(SEXP symbol) {
  if (TYPEOF(symbol) != SYMSXP) {
    Rf_error("Internal error: option selector must be a symbol");
  }
#if R_VERSION >= R_Version(4, 5, 0)
  return Rf_GetOption1(symbol);
#else
  /*
   * Rf_GetOption1 is exported and header-declared here, but Writing R
   * Extensions first documents it as API in R 4.5. Keep the old-runtime
   * branch on the public R spelling rather than enlarging the exceptional
   * native-API ledger for a performance-only operation.
   */
  SEXP label = PROTECT(Rf_ScalarString(PRINTNAME(symbol)));
  SEXP function = PROTECT(Rf_findFun(Rf_install("getOption"), R_BaseEnv));
  SEXP call = PROTECT(Rf_lang2(function, label));
  SEXP result = PROTECT(Rf_eval(call, R_BaseEnv));
  UNPROTECT(4);
  return result;
#endif
}

#if R_VERSION < R_Version(4, 6, 0)
/*
 * R 4.6 provides public attribute iteration.  Earlier supported headers have
 * no API operation that can count or enumerate the raw stored attributes
 * without evaluating R or changing compact row.names.  Keep their one
 * reviewed, header-declared ATTRIB use at this compatibility boundary.
 */
static inline SEXP stored_attributes_unchecked(SEXP value) {
  return ATTRIB(value);
}

static int attributes_match(SEXP value,
    const char *const *allowed_names, size_t allowed_count,
    R_xlen_t required_count) {
  unsigned char seen[PARADOX_API_MAX_ALLOWED_ATTRIBUTES] = {0};
  R_xlen_t count = 0;
  SEXP attributes = stored_attributes_unchecked(value);
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
  return stored_attributes_unchecked(value) == R_NilValue;
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
  for (SEXP attributes = stored_attributes_unchecked(value);
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
  for (SEXP attributes = stored_attributes_unchecked(value);
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
  for (SEXP attributes = stored_attributes_unchecked(value);
      attributes != R_NilValue;
      attributes = CDR(attributes)) {
    if (TYPEOF(attributes) != LISTSXP) {
      Rf_error("Internal error: malformed attribute pairlist");
    }
    callback(TAG(attributes), CAR(attributes), data);
  }
#endif
}

int paradox_api_ordinary_class_snapshot(SEXP value, SEXP *classes) {
  if (classes == NULL) {
    Rf_error("Internal error: missing class snapshot destination");
  }
  *classes = paradox_api_raw_attribute(value, R_ClassSymbol);
  if (*classes == R_NilValue) {
    return TRUE;
  }
  if (TYPEOF(*classes) != STRSXP || ALTREP(*classes) ||
      Rf_isS4(*classes) || !paradox_api_has_no_attributes(*classes)) {
    return FALSE;
  }
  const R_xlen_t size = XLENGTH(*classes);
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP label = STRING_ELT(*classes, index);
    if (label == NA_STRING || Rf_getCharCE(label) == CE_BYTES ||
        CHAR(label)[0] == '\0') {
      return FALSE;
    }
  }
  return TRUE;
}

int paradox_api_ordinary_class_contains(SEXP classes, const char *label) {
  if (TYPEOF(classes) != STRSXP || ALTREP(classes) ||
      Rf_isS4(classes) || label == NULL) {
    return FALSE;
  }
  const R_xlen_t size = XLENGTH(classes);
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP candidate = STRING_ELT(classes, index);
    if (candidate != NA_STRING && Rf_getCharCE(candidate) != CE_BYTES &&
        strcmp(CHAR(candidate), label) == 0) {
      return TRUE;
    }
  }
  return FALSE;
}

static int valid_binding_request(SEXP environment, SEXP symbol) {
  if (TYPEOF(environment) != ENVSXP || Rf_isS4(environment) ||
      TYPEOF(symbol) != SYMSXP) {
    return FALSE;
  }
  if (!Rf_isObject(environment)) {
    return TRUE;
  }

  /*
   * R_ObjectTable environments route binding APIs through callbacks. More
   * importantly, old R_HasFancyBindings() assumes the ordinary HASHTAB layout
   * and is not valid for their external-pointer-backed table. Never use
   * Rf_inherits() here: an arbitrary environment may carry callback-capable
   * ALTREP class metadata. Exact inert class inspection both recognizes the
   * object-table boundary and fails closed on malformed object metadata.
   */
  SEXP classes;
  return paradox_api_ordinary_class_snapshot(environment, &classes) &&
    !paradox_api_ordinary_class_contains(classes, "UserDefinedDatabase");
}

#if R_VERSION < R_Version(4, 2, 0)
static int evaluated_frame_has_binding(SEXP environment, SEXP symbol) {
  /*
   * R 3.6--4.1 has no public non-evaluating single-binding existence API.
   * base::exists(mode = "any", inherits = FALSE) is specifically implemented
   * without invoking active bindings. This is deliberately only the optional
   * existence operation. Admitted ordinary bindings use the allocation-free
   * snapshot operation below.
   */
  SEXP label = PROTECT(Rf_ScalarString(PRINTNAME(symbol)));
  SEXP inherits = PROTECT(Rf_ScalarLogical(FALSE));
  SEXP call = PROTECT(Rf_lang4(
    Rf_install("exists"),
    label,
    environment,
    inherits
  ));
  SET_TAG(CDDR(call), Rf_install("envir"));
  SET_TAG(CDDDR(call), Rf_install("inherits"));
  SEXP result = PROTECT(Rf_eval(call, R_BaseEnv));
  if (TYPEOF(result) != LGLSXP || XLENGTH(result) != 1 ||
      LOGICAL_ELT(result, 0) == NA_LOGICAL) {
    UNPROTECT(4);
    Rf_error("Internal error: unexpected result from base::exists()");
  }
  const int found = LOGICAL_ELT(result, 0);
  UNPROTECT(4);
  return found;
}
#endif

#if R_VERSION < R_Version(4, 6, 0)
/*
 * Keep the one reviewed non-public stored-cell selector at a single source
 * location. Both the pre-4.2 terminal topology scan and the pre-4.6 required
 * binding snapshot use this helper after their respective ordinary-frame
 * boundaries have been established.
 */
static inline SEXP stored_binding_snapshot_unchecked(
    SEXP environment, SEXP symbol) {
  return Rf_findVarInFrame(environment, symbol);
}
#endif

int paradox_api_frame_has_binding(SEXP environment, SEXP symbol) {
  if (!valid_binding_request(environment, symbol)) {
    return FALSE;
  }
#if R_VERSION >= R_Version(4, 2, 0)
  return R_existsVarInFrame(environment, symbol) != FALSE;
#else
  return evaluated_frame_has_binding(environment, symbol);
#endif
}

int paradox_api_frame_has_binding_scan(SEXP environment, SEXP symbol) {
  if (!valid_binding_request(environment, symbol)) {
    return TRUE;
  }
#if R_VERSION >= R_Version(4, 2, 0)
  return R_existsVarInFrame(environment, symbol) != FALSE;
#else
  /*
   * R_HasFancyBindings is the only header-declared, exported old-R operation
   * that lets the terminal receipt remain allocation-free without risking an
   * active-binding callback. Treating a fancy frame as occupied fails closed.
   * This spelling is confined to R 3.6--4.1 and to this compatibility facade.
   */
  return R_HasFancyBindings(environment) != FALSE ||
    stored_binding_snapshot_unchecked(environment, symbol) != R_UnboundValue;
#endif
}

static int plain_binding_boundary(SEXP environment, SEXP symbol) {
  if (!valid_binding_request(environment, symbol)) {
    return FALSE;
  }
#if R_VERSION >= R_Version(4, 2, 0)
  if (!R_existsVarInFrame(environment, symbol)) {
    return FALSE;
  }
#endif
  /*
   * Before R 4.2 there is no public allocation-free existence query.
   * Snapshot callers operate on required/admitted bindings, so a missing cell
   * is corrupt and R_BindingIsActive's error is the correct fail-closed
   * outcome. Crucially, this never evaluates an active binding and preserves
   * the allocation-free authenticated ordinary-frame second-scan barrier.
   */
  return R_BindingIsActive(symbol, environment) == FALSE;
}

#if R_VERSION < R_Version(4, 6, 0)
SEXP paradox_api_stored_binding_snapshot(
    SEXP environment, SEXP symbol) {
  if (!plain_binding_boundary(environment, symbol)) {
    return R_UnboundValue;
  }
  return stored_binding_snapshot_unchecked(environment, symbol);
}
#endif

SEXP paradox_api_plain_binding_snapshot(SEXP environment, SEXP symbol) {
  if (!plain_binding_boundary(environment, symbol)) {
    return R_UnboundValue;
  }
  SEXP result;
#if R_VERSION >= R_Version(4, 6, 0)
  if (R_GetBindingType(symbol, environment) != R_BindingTypeValue) {
    return R_UnboundValue;
  }
  /*
   * Binding kind is already known to be a direct value. R_getVar() retrieves
   * it without entering the evaluator (and therefore without evaluator
   * interrupt/finalizer checkpoints during an authenticated ordinary-frame
   * generation scan).
   */
  result = R_getVar(symbol, environment, FALSE);
#else
  /*
   * plain_binding_boundary() already admitted this exact cell. Repeating the
   * public boundary would double every old-runtime active/class check without
   * adding a receipt: the ordinary-frame selector below cannot allocate or
   * invoke a binding.
   */
  result = stored_binding_snapshot_unchecked(environment, symbol);
#endif
  return result == R_UnboundValue || result == R_MissingArg ||
      TYPEOF(result) == PROMSXP
    ? R_UnboundValue
    : result;
}

SEXP paradox_api_plain_binding_scan(SEXP environment, SEXP symbol) {
  return paradox_api_plain_binding_snapshot(environment, symbol);
}

SEXP paradox_api_optional_plain_binding_snapshot(
    SEXP environment, SEXP symbol) {
#if R_VERSION >= R_Version(4, 2, 0)
  /*
   * The current plain snapshot already performs the public existence check.
   * Its admitted ordinary-frame fast path is allocation-free, so do not pay
   * for the same frame lookup twice on every authenticated R6 gateway.
   * Recognized user-database environments were rejected before this point.
   * Gateway callers nevertheless keep their uniform rooting proof across all
   * supported API branches because hostile class metadata inspected at entry
   * can itself be callback-capable.
   */
  return paradox_api_plain_binding_snapshot(environment, symbol);
#else
  return paradox_api_frame_has_binding(environment, symbol)
    ? paradox_api_plain_binding_snapshot(environment, symbol)
    : R_UnboundValue;
#endif
}

SEXP paradox_api_active_binding_function(
    SEXP environment, SEXP symbol) {
  if (!valid_binding_request(environment, symbol) ||
      !R_BindingIsActive(symbol, environment)) {
    return R_UnboundValue;
  }
#if R_VERSION >= R_Version(4, 0, 0)
  return R_ActiveBindingFunction(symbol, environment);
#else
  return R_UnboundValue;
#endif
}

#if R_VERSION < R_Version(4, 5, 0)
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
