#include <stddef.h>
#include <string.h>

#include "r_api_compat.h"

#define PARADOX_API_MAX_ALLOWED_ATTRIBUTES ((size_t) 16)
#define PARADOX_API_MAX_STORED_ATTRIBUTES ((size_t) 64)

#if R_VERSION < R_Version(4, 5, 0)
/* Cold public-R bridge for the remaining operations without an admitted
 * allocation-free old-runtime accessor. Compile it out entirely once every
 * caller selects the direct R >= 4.5 API. */
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
  /*
   * Parentheses suppress the old function-like macro and select the
   * header-declared exported accessor.  It is the same narrow pre-4.5
   * exception already used for callback-formal admission, without exposing
   * R's object layout.
   */
  return (FORMALS)(closure);
#endif
}

SEXP paradox_api_closure_expression(SEXP closure) {
  /*
   * Header-declared and exported on every supported runtime; documented as
   * API from R 4.5.  The old-runtime use is confined here because the graph
   * crawler needs one allocation-free closure generation.
   */
  return R_ClosureExpr(closure);
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
  /*
   * As for FORMALS above, suppress the historical macro and call the
   * header-declared exported accessor.  This exception is compiled out once
   * R_ClosureEnv is public.
   */
  return (CLOENV)(closure);
#endif
}

int paradox_api_closure_formal_matches(
    SEXP closure, SEXP sought, int *matches) {
  if (matches == NULL || TYPEOF(sought) != SYMSXP) {
    Rf_error("Internal error: invalid closure-formal query");
  }
  *matches = FALSE;
  if (TYPEOF(closure) != CLOSXP) return TRUE;

  SEXP slow = paradox_api_closure_formals(closure);
  SEXP fast = slow;
  while (slow != R_NilValue) {
    if (TYPEOF(slow) != LISTSXP || TYPEOF(TAG(slow)) != SYMSXP) {
      *matches = FALSE;
      return FALSE;
    }
    if (TAG(slow) == sought) *matches = TRUE;
    slow = CDR(slow);

    for (int step = 0; step < 2 && fast != R_NilValue; ++step) {
      if (TYPEOF(fast) != LISTSXP) {
        *matches = FALSE;
        return FALSE;
      }
      fast = CDR(fast);
    }
    if (fast != R_NilValue && slow == fast) {
      *matches = FALSE;
      return FALSE;
    }
  }
  return TRUE;
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
#endif

typedef struct {
  SEXP symbols[PARADOX_API_MAX_ALLOWED_ATTRIBUTES];
  unsigned char seen[PARADOX_API_MAX_ALLOWED_ATTRIBUTES];
  size_t allowed_count;
  R_xlen_t count;
  int valid;
} allowed_attribute_match_state_t;

static void match_allowed_attribute(SEXP tag, SEXP value, void *data) {
  (void) value;
  allowed_attribute_match_state_t *state = data;
  if (!state->valid || TYPEOF(tag) != SYMSXP) {
    state->valid = FALSE;
    return;
  }
  size_t allowed = 0;
  while (allowed < state->allowed_count &&
      tag != state->symbols[allowed]) {
    ++allowed;
  }
  if (allowed == state->allowed_count || state->seen[allowed]) {
    state->valid = FALSE;
    return;
  }
  state->seen[allowed] = 1U;
  ++state->count;
}

static int attributes_match(SEXP value,
    const char *const *allowed_names, size_t allowed_count,
    R_xlen_t required_count) {
  allowed_attribute_match_state_t state = {
    {R_NilValue},
    {0},
    allowed_count,
    0,
    TRUE
  };
  for (size_t index = 0; index < allowed_count; ++index) {
    if (allowed_names[index] == NULL) {
      Rf_error("Internal error: missing public attribute name");
    }
    state.symbols[index] = Rf_install(allowed_names[index]);
  }
  R_xlen_t count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
      value,
      (R_xlen_t) allowed_count,
      match_allowed_attribute,
      &state,
      &count
    ) || !state.valid || state.count != count) {
    return FALSE;
  }
  return required_count < 0 || count == required_count;
}

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
  const char *const allowed[] = {name};
  return attributes_match(value, allowed, 1, 1);
}

int paradox_api_has_only_attributes(SEXP value,
    const char *const *allowed_names, size_t allowed_count) {
  if (allowed_count > PARADOX_API_MAX_ALLOWED_ATTRIBUTES ||
      (allowed_count != 0 && allowed_names == NULL)) {
    Rf_error("Internal error: invalid public attribute allow-list");
  }
  return attributes_match(
    value,
    allowed_names,
    allowed_count,
    -1
  );
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
  R_xlen_t limit;
  R_xlen_t count;
  int overflow;
} bounded_attribute_map_state_t;

static SEXP map_bounded_stored_attribute(
    SEXP tag, SEXP attribute, void *data) {
  bounded_attribute_map_state_t *state = data;
  if (state->count >= state->limit) {
    state->overflow = TRUE;
    /*
     * Any non-NULL SEXP stops R_mapAttrib().  A malformed raw cell may have a
     * NULL tag, so use R's permanently rooted names symbol rather than the
     * selected tag.  This keeps even a cyclic malformed spine bounded.
     */
    return R_NamesSymbol;
  }
  if (state->callback != NULL) {
    state->callback(tag, attribute, state->data);
  }
  ++state->count;
  return NULL;
}
#endif

int paradox_api_map_bounded_stored_attributes(
    SEXP value,
    R_xlen_t limit,
    paradox_api_attribute_callback_t callback,
    void *data,
    R_xlen_t *count) {
  if (limit < 0 || count == NULL) {
    Rf_error("Internal error: invalid bounded attribute-map request");
  }
#if R_VERSION >= R_Version(4, 6, 0)
  bounded_attribute_map_state_t state = {
    callback,
    data,
    limit,
    0,
    FALSE
  };
  (void) R_mapAttrib(value, map_bounded_stored_attribute, &state);
  *count = state.count;
  return !state.overflow;
#else
  R_xlen_t selected = 0;
  for (SEXP attributes = stored_attributes_unchecked(value);
      attributes != R_NilValue;
      attributes = CDR(attributes)) {
    if (TYPEOF(attributes) != LISTSXP || selected >= limit) {
      *count = selected;
      return FALSE;
    }
    if (callback != NULL) {
      callback(TAG(attributes), CAR(attributes), data);
    }
    ++selected;
  }
  *count = selected;
  return TRUE;
#endif
}

typedef struct {
  SEXP tags[PARADOX_API_MAX_STORED_ATTRIBUTES];
  SEXP classes;
  R_xlen_t count;
  int found;
  int valid;
} ordinary_class_snapshot_state_t;

static void capture_ordinary_class_attribute(
    SEXP tag, SEXP value, void *data) {
  ordinary_class_snapshot_state_t *state = data;
  if (!state->valid || TYPEOF(tag) != SYMSXP ||
      value == R_NilValue ||
      state->count >= (R_xlen_t) PARADOX_API_MAX_STORED_ATTRIBUTES) {
    state->valid = FALSE;
    return;
  }
  for (R_xlen_t index = 0; index < state->count; ++index) {
    if (state->tags[index] == tag) {
      state->valid = FALSE;
      return;
    }
  }
  state->tags[state->count] = tag;
  ++state->count;
  if (tag == R_ClassSymbol) {
    state->classes = value;
    state->found = TRUE;
  }
}

int paradox_api_ordinary_class_snapshot(SEXP value, SEXP *classes) {
  if (classes == NULL) {
    Rf_error("Internal error: missing class snapshot destination");
  }
  if (paradox_api_has_no_attributes(value)) {
    *classes = R_NilValue;
    return TRUE;
  }
  ordinary_class_snapshot_state_t state = {
    {R_NilValue},
    R_NilValue,
    0,
    FALSE,
    TRUE
  };
  R_xlen_t count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
      value,
      (R_xlen_t) PARADOX_API_MAX_STORED_ATTRIBUTES,
      capture_ordinary_class_attribute,
      &state,
      &count
    ) || !state.valid || state.count != count) {
    *classes = R_NilValue;
    return FALSE;
  }
  *classes = state.found ? state.classes : R_NilValue;
  if (!state.found) return TRUE;
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

int paradox_api_opaque_leaf_class_snapshot(SEXP value, SEXP *classes) {
  if (classes == NULL) {
    Rf_error("Internal error: missing opaque-leaf class snapshot destination");
  }
  if (paradox_api_ordinary_class_snapshot(value, classes)) return TRUE;
  if (!Rf_isS4(value)) return FALSE;
  *classes = R_NilValue;
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

int paradox_api_opaque_leaf_class_matches(
    SEXP value, const char *label, int *matches) {
  if (label == NULL || matches == NULL) {
    Rf_error("Internal error: invalid opaque-leaf class-membership request");
  }
  *matches = FALSE;
  if (!Rf_isObject(value)) return TRUE;
  SEXP classes = R_NilValue;
  if (!paradox_api_opaque_leaf_class_snapshot(value, &classes)) return FALSE;
  *matches = paradox_api_ordinary_class_contains(classes, label);
  return TRUE;
}

int paradox_api_ordinary_class_matches(
    SEXP value, const char *label, int *matches) {
  if (label == NULL || matches == NULL) {
    Rf_error("Internal error: invalid ordinary class-membership request");
  }
  *matches = FALSE;
  /*
   * R's object bit is the allocation-free negative inheritance certificate.
   * Preserve opaque unclassed values (including ParamUty environments with
   * arbitrary unrelated attributes) without walking metadata that cannot
   * contribute a class.
   */
  if (!Rf_isObject(value)) return TRUE;
  SEXP classes = R_NilValue;
  if (!paradox_api_ordinary_class_snapshot(value, &classes)) {
    return FALSE;
  }
  *matches = paradox_api_ordinary_class_contains(classes, label);
  return TRUE;
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
