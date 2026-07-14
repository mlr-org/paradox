/* Internal adversarial-test support.
 *
 * This deliberately stateful ALTREP class is registered only as a native test
 * symbol. It lets the package's subprocess tests prove that production kernels
 * snapshot callback-capable vectors instead of trusting repeated Length/Elt
 * observations. It is not part of the exported R API.
 */

#include <limits.h>

#include "paradox.h"
#include <R_ext/Altrep.h>
#include <R_ext/Rdynload.h>

enum test_state_slot {
  TEST_STATE_FIRST = 0,
  TEST_STATE_LATER,
  TEST_STATE_CALLBACK,
  TEST_STATE_SIZE
};

enum test_config_slot {
  TEST_CONFIG_ELT_CALLS = 0,
  TEST_CONFIG_LENGTH_CALLS,
  TEST_CONFIG_ELT_SWITCH_AFTER,
  TEST_CONFIG_LENGTH_SWITCH_AFTER,
  TEST_CONFIG_ELT_CALLBACK_AFTER,
  TEST_CONFIG_LENGTH_CALLBACK_AFTER,
  TEST_CONFIG_SIZE
};

static R_altrep_class_t test_string_class;
static R_altrep_class_t test_list_class;
static R_altrep_class_t test_integer_class;
static R_altrep_class_t test_real_class;
static R_altrep_class_t test_logical_class;

static int scalar_control(SEXP value, const char *name) {
  if (TYPEOF(value) != INTSXP || ALTREP(value) || XLENGTH(value) != 1) {
    Rf_error("`%s` must be an integer scalar", name);
  }
  const int result = INTEGER_ELT(value, 0);
  if (result != NA_INTEGER && result < 0) {
    Rf_error("`%s` must be non-negative or NA", name);
  }
  return result;
}

static void callback_controls(SEXP value, int *elt_after,
    int *length_after) {
  if (TYPEOF(value) != INTSXP || ALTREP(value)) {
    Rf_error("`callback_after` must be an integer scalar or pair");
  }
  const R_xlen_t size = XLENGTH(value);
  if (size != 1 && size != 2) {
    Rf_error("`callback_after` must be an integer scalar or pair");
  }
  const int elt = INTEGER_ELT(value, 0);
  const int length = size == 1 ? NA_INTEGER : INTEGER_ELT(value, 1);
  if ((elt != NA_INTEGER && elt < 0) ||
      (length != NA_INTEGER && length < 0)) {
    Rf_error("`callback_after` values must be non-negative or NA");
  }
  *elt_after = elt;
  *length_after = length;
}

static int next_call(SEXP config, enum test_config_slot slot) {
  int *values = INTEGER(config);
  const int current = values[slot];
  if (current < INT_MAX) {
    values[slot] = current + 1;
  }
  return current;
}

static int use_later(int call, int switch_after) {
  return switch_after != NA_INTEGER && call >= switch_after;
}

static int relative_callback_call(int current, int offset) {
  if (offset == NA_INTEGER) {
    return NA_INTEGER;
  }
  if (current > INT_MAX - offset) {
    Rf_error("`callback_after` exceeds the test fixture call range");
  }
  return current + offset;
}

static void run_callback_once(SEXP state, SEXP config, int call,
    enum test_config_slot callback_slot) {
  int *values = INTEGER(config);
  if (values[callback_slot] == NA_INTEGER ||
      call != values[callback_slot]) {
    return;
  }

  /* Disable first so a callback which recursively touches the ALTREP cannot
   * invoke itself indefinitely. */
  values[callback_slot] = NA_INTEGER;
  SEXP callback = PROTECT(VECTOR_ELT(state, TEST_STATE_CALLBACK));
  if (callback != R_NilValue) {
    SEXP call_expression = PROTECT(Rf_lang1(callback));
    SEXP ignored = PROTECT(Rf_eval(call_expression, R_BaseEnv));
    (void) ignored;
    UNPROTECT(2);
  }
  UNPROTECT(1);
}

/* Returns with state, config, and the selected source protected; each Elt
 * method releases all three after copying its scalar result. */
static SEXP begin_elt(SEXP x) {
  SEXP state = PROTECT(R_altrep_data1(x));
  SEXP config = PROTECT(R_altrep_data2(x));
  const int call = next_call(config, TEST_CONFIG_ELT_CALLS);
  const int later = use_later(
    call,
    INTEGER(config)[TEST_CONFIG_ELT_SWITCH_AFTER]
  );
  SEXP source = PROTECT(VECTOR_ELT(
    state,
    later ? TEST_STATE_LATER : TEST_STATE_FIRST
  ));
  run_callback_once(
    state,
    config,
    call,
    TEST_CONFIG_ELT_CALLBACK_AFTER
  );
  return source;
}

static R_xlen_t test_length(SEXP x) {
  SEXP state = PROTECT(R_altrep_data1(x));
  SEXP config = PROTECT(R_altrep_data2(x));
  const int call = next_call(config, TEST_CONFIG_LENGTH_CALLS);
  const int later = use_later(
    call,
    INTEGER(config)[TEST_CONFIG_LENGTH_SWITCH_AFTER]
  );
  SEXP source = PROTECT(VECTOR_ELT(
    state,
    later ? TEST_STATE_LATER : TEST_STATE_FIRST
  ));
  run_callback_once(
    state,
    config,
    call,
    TEST_CONFIG_LENGTH_CALLBACK_AFTER
  );
  const R_xlen_t result = XLENGTH(source);
  UNPROTECT(3);
  return result;
}

static SEXP test_string_elt(SEXP x, R_xlen_t index) {
  SEXP source = begin_elt(x);
  SEXP result = index >= 0 && index < XLENGTH(source)
    ? STRING_ELT(source, index)
    : NA_STRING;
  UNPROTECT(3);
  return result;
}

static SEXP test_list_elt(SEXP x, R_xlen_t index) {
  SEXP source = begin_elt(x);
  SEXP result = index >= 0 && index < XLENGTH(source)
    ? VECTOR_ELT(source, index)
    : R_NilValue;
  UNPROTECT(3);
  return result;
}

static int test_integer_elt(SEXP x, R_xlen_t index) {
  SEXP source = begin_elt(x);
  const int result = index >= 0 && index < XLENGTH(source)
    ? INTEGER_ELT(source, index)
    : NA_INTEGER;
  UNPROTECT(3);
  return result;
}

static double test_real_elt(SEXP x, R_xlen_t index) {
  SEXP source = begin_elt(x);
  const double result = index >= 0 && index < XLENGTH(source)
    ? REAL_ELT(source, index)
    : NA_REAL;
  UNPROTECT(3);
  return result;
}

static int test_logical_elt(SEXP x, R_xlen_t index) {
  SEXP source = begin_elt(x);
  const int result = index >= 0 && index < XLENGTH(source)
    ? LOGICAL_ELT(source, index)
    : NA_LOGICAL;
  UNPROTECT(3);
  return result;
}

void attribute_hidden paradox_test_altrep_initialize(DllInfo *dll) {
  test_string_class = R_make_altstring_class(
    "paradox_test_stateful_string",
    "paradox",
    dll
  );
  R_set_altrep_Length_method(test_string_class, test_length);
  R_set_altstring_Elt_method(test_string_class, test_string_elt);

  test_list_class = R_make_altlist_class(
    "paradox_test_stateful_list",
    "paradox",
    dll
  );
  R_set_altrep_Length_method(test_list_class, test_length);
  R_set_altlist_Elt_method(test_list_class, test_list_elt);

  test_integer_class = R_make_altinteger_class(
    "paradox_test_stateful_integer",
    "paradox",
    dll
  );
  R_set_altrep_Length_method(test_integer_class, test_length);
  R_set_altinteger_Elt_method(test_integer_class, test_integer_elt);

  test_real_class = R_make_altreal_class(
    "paradox_test_stateful_real",
    "paradox",
    dll
  );
  R_set_altrep_Length_method(test_real_class, test_length);
  R_set_altreal_Elt_method(test_real_class, test_real_elt);

  test_logical_class = R_make_altlogical_class(
    "paradox_test_stateful_logical",
    "paradox",
    dll
  );
  R_set_altrep_Length_method(test_logical_class, test_length);
  R_set_altlogical_Elt_method(test_logical_class, test_logical_elt);
}

SEXP paradox_test_stateful_altrep(SEXP first, SEXP later,
    SEXP elt_switch_after, SEXP length_switch_after, SEXP callback,
    SEXP callback_after) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(first);
  if (type != (SEXPTYPE) TYPEOF(later) || ALTREP(first) || ALTREP(later) ||
      (type != STRSXP && type != VECSXP && type != INTSXP &&
        type != REALSXP && type != LGLSXP) ||
      (callback != R_NilValue && !Rf_isFunction(callback))) {
    Rf_error("Unsupported stateful ALTREP test fixture arguments");
  }
  const int elt_switch = scalar_control(
    elt_switch_after,
    "elt_switch_after"
  );
  const int length_switch = scalar_control(
    length_switch_after,
    "length_switch_after"
  );
  int elt_callback_at;
  int length_callback_at;
  callback_controls(
    callback_after,
    &elt_callback_at,
    &length_callback_at
  );

  SEXP state = PROTECT(Rf_allocVector(VECSXP, TEST_STATE_SIZE));
  SET_VECTOR_ELT(state, TEST_STATE_FIRST, first);
  SET_VECTOR_ELT(state, TEST_STATE_LATER, later);
  SET_VECTOR_ELT(state, TEST_STATE_CALLBACK, callback);
  SEXP config = PROTECT(Rf_allocVector(INTSXP, TEST_CONFIG_SIZE));
  /* Returning an ALTREP list through the R helper performs one unavoidable
   * delivery Length probe. Shift list Length controls so their documented
   * zero-based offsets begin with the first observation made by the test. */
  const int length_delivery_offset = type == VECSXP ? 1 : 0;
  INTEGER(config)[TEST_CONFIG_ELT_CALLS] = 0;
  INTEGER(config)[TEST_CONFIG_LENGTH_CALLS] = 0;
  INTEGER(config)[TEST_CONFIG_ELT_SWITCH_AFTER] = elt_switch;
  INTEGER(config)[TEST_CONFIG_LENGTH_SWITCH_AFTER] = relative_callback_call(
    length_delivery_offset,
    length_switch
  );
  INTEGER(config)[TEST_CONFIG_ELT_CALLBACK_AFTER] = NA_INTEGER;
  INTEGER(config)[TEST_CONFIG_LENGTH_CALLBACK_AFTER] = NA_INTEGER;

  R_altrep_class_t class;
  switch (type) {
  case STRSXP:
    class = test_string_class;
    break;
  case VECSXP:
    class = test_list_class;
    break;
  case INTSXP:
    class = test_integer_class;
    break;
  case REALSXP:
    class = test_real_class;
    break;
  case LGLSXP:
    class = test_logical_class;
    break;
  default:
    UNPROTECT(2);
    Rf_error("Unsupported stateful ALTREP test fixture type");
  }

  SEXP result = PROTECT(R_new_altrep(class, state, config));
  DUPLICATE_ATTRIB(result, first);
  /* Attribute copying may query Length internally; expose deterministic
   * zero-based counters to the test which receives the completed object. */
  INTEGER(config)[TEST_CONFIG_ELT_CALLS] = 0;
  INTEGER(config)[TEST_CONFIG_LENGTH_CALLS] = 0;
  INTEGER(config)[TEST_CONFIG_ELT_CALLBACK_AFTER] = elt_callback_at;
  INTEGER(config)[TEST_CONFIG_LENGTH_CALLBACK_AFTER] = relative_callback_call(
    length_delivery_offset,
    length_callback_at
  );
  /* Avoid the .Call return probe by nesting the ALTREP in an ordinary shell.
   * The R helper's own list return still probes Length once; list controls
   * above include that known delivery offset. */
  SEXP wrapper = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(wrapper, 0, result);
  UNPROTECT(4);
  return wrapper;
}

SEXP paradox_test_stateful_altrep_rearm(SEXP value, SEXP callback_after) {
  if (!ALTREP(value) ||
      (!R_altrep_inherits(value, test_string_class) &&
        !R_altrep_inherits(value, test_list_class) &&
        !R_altrep_inherits(value, test_integer_class) &&
        !R_altrep_inherits(value, test_real_class) &&
        !R_altrep_inherits(value, test_logical_class))) {
    Rf_error("`value` must be a stateful ALTREP test fixture");
  }

  int elt_after;
  int length_after;
  callback_controls(callback_after, &elt_after, &length_after);
  SEXP config = PROTECT(R_altrep_data2(value));
  if (TYPEOF(config) != INTSXP || ALTREP(config) ||
      XLENGTH(config) != TEST_CONFIG_SIZE) {
    UNPROTECT(1);
    Rf_error("Corrupt stateful ALTREP test fixture configuration");
  }
  int *values = INTEGER(config);
  values[TEST_CONFIG_ELT_CALLBACK_AFTER] = relative_callback_call(
    values[TEST_CONFIG_ELT_CALLS],
    elt_after
  );
  values[TEST_CONFIG_LENGTH_CALLBACK_AFTER] = relative_callback_call(
    values[TEST_CONFIG_LENGTH_CALLS],
    length_after
  );
  UNPROTECT(1);
  return R_NilValue;
}

static void mutate_column_at_gc(SEXP pointer) {
  if (R_ExternalPtrAddr(pointer) == NULL) {
    return;
  }
  SEXP state = R_ExternalPtrProtected(pointer);
  if (TYPEOF(state) == VECSXP && XLENGTH(state) == 3) {
    SEXP table = VECTOR_ELT(state, 0);
    SEXP column = VECTOR_ELT(state, 1);
    SEXP replacement = VECTOR_ELT(state, 2);
    if (TYPEOF(table) == VECSXP && TYPEOF(column) == INTSXP &&
        XLENGTH(column) == 1) {
      const int selected = INTEGER_ELT(column, 0);
      if (selected >= 0 && (R_xlen_t) selected < XLENGTH(table)) {
        SET_VECTOR_ELT(table, selected, replacement);
      }
    }
  }
  R_ClearExternalPtr(pointer);
}

SEXP paradox_test_gc_column_mutator(SEXP table, SEXP column,
    SEXP replacement) {
  if (TYPEOF(table) != VECSXP || ALTREP(table) ||
      TYPEOF(column) != INTSXP || ALTREP(column) ||
      XLENGTH(column) != 1) {
    Rf_error("Invalid GC column-mutator test fixture arguments");
  }
  const int selected = INTEGER_ELT(column, 0);
  if (selected == NA_INTEGER || selected < 0 ||
      (R_xlen_t) selected >= XLENGTH(table)) {
    Rf_error("Invalid GC column-mutator test column");
  }

  SEXP state = PROTECT(Rf_allocVector(VECSXP, 3));
  SET_VECTOR_ELT(state, 0, table);
  SET_VECTOR_ELT(state, 1, column);
  SET_VECTOR_ELT(state, 2, replacement);
  SEXP pointer = PROTECT(R_MakeExternalPtr(
    (void *) table,
    R_NilValue,
    state
  ));
  R_RegisterCFinalizerEx(pointer, mutate_column_at_gc, FALSE);
  UNPROTECT(2);
  /* Returning the pointer lets adversarial tests retain it until the exact
   * callback boundary they intend to probe.  Discarding it here makes the
   * finalizer eligible before native admission, so an unrelated earlier GC
   * can turn a post-admission lifetime test into a precondition failure. */
  return pointer;
}
