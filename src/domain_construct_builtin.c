#include <float.h>
#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"
#include "r_api_compat.h"

/*
 * A deliberately narrow direct lane for standalone calls to p_dbl(),
 * p_int(), and p_lgl().  The ordinary R constructors remain the authority for
 * every unsupported call.  Admission is complete before an expression is
 * evaluated, and values observed during admission remain memoized in their
 * function frame, so falling back never evaluates a user expression twice.
 */

#define BUILTIN_FORMAL_COUNT ((size_t) 13)
#define BUILTIN_ROOT_CAPACITY ((R_xlen_t) 64)
#define BUILTIN_MAX_STRINGS ((R_xlen_t) 16)
#define BUILTIN_MAX_STRING_BYTES ((size_t) 80)

enum builtin_kind {
  BUILTIN_DBL = 0,
  BUILTIN_INT,
  BUILTIN_FCT,
  BUILTIN_LGL,
  BUILTIN_KIND_COUNT
};

enum builtin_runtime_slot {
  BUILTIN_RUNTIME_CONSTRUCTORS = 0,
  BUILTIN_RUNTIME_NO_DEFAULT,
  BUILTIN_RUNTIME_NAMESPACE,
  BUILTIN_RUNTIME_SORT,
  BUILTIN_RUNTIME_SLOT_COUNT
};

enum builtin_value {
  BUILTIN_LOWER = 0,
  BUILTIN_UPPER,
  BUILTIN_TAGS,
  BUILTIN_TOLERANCE,
  BUILTIN_VALUE_COUNT
};

struct builtin_spec {
  const char *name;
  const char *class_name;
  const char *grouping;
  const char *storage;
  const char *const *formals;
  size_t formal_count;
  unsigned int allowed;
  int required;
};

struct builtin_plan {
  enum builtin_kind kind;
  SEXP head;
  SEXP expressions[BUILTIN_FORMAL_COUNT];
  unsigned int present;
};

struct scalar_snapshot {
  SEXP value;
  SEXPTYPE type;
  union {
    int integer;
    double real;
  } content;
};

struct strings_snapshot {
  SEXP value;
  R_xlen_t size;
  SEXP elements[BUILTIN_MAX_STRINGS];
  int lengths[BUILTIN_MAX_STRINGS];
  cetype_t encodings[BUILTIN_MAX_STRINGS];
  size_t offsets[BUILTIN_MAX_STRINGS];
  unsigned char bytes[BUILTIN_MAX_STRING_BYTES];
  size_t byte_count;
};

struct delayed_formals_snapshot {
  SEXP expressions[BUILTIN_FORMAL_COUNT];
  SEXP environments[BUILTIN_FORMAL_COUNT];
  unsigned int captured;
};

static const char *const numeric_formals[] = {
  "lower", "upper", "special_vals", "default", "tags", "tolerance",
  "depends", "trafo", "logscale", "init", "aggr", "in_tune_fn",
  "disable_in_tune"
};

static const char *const factor_formals[] = {
  "levels", "special_vals", "default", "tags", "depends", "trafo",
  "init", "aggr", "in_tune_fn", "disable_in_tune"
};

static const char *const logical_formals[] = {
  "special_vals", "default", "tags", "depends", "trafo", "init",
  "aggr", "in_tune_fn", "disable_in_tune"
};

#define FORMAL_BIT(index) (1U << (index))

static const struct builtin_spec builtin_specs[BUILTIN_KIND_COUNT] = {
  {
    "p_dbl", "ParamDbl", "ParamDbl", "numeric",
    numeric_formals, sizeof(numeric_formals) / sizeof(numeric_formals[0]),
    FORMAL_BIT(0) | FORMAL_BIT(1) | FORMAL_BIT(4) | FORMAL_BIT(5), -1
  },
  {
    "p_int", "ParamInt", "ParamInt", "integer",
    numeric_formals, sizeof(numeric_formals) / sizeof(numeric_formals[0]),
    FORMAL_BIT(0) | FORMAL_BIT(1) | FORMAL_BIT(4) | FORMAL_BIT(5), -1
  },
  {
    "p_fct", "ParamFct", NULL, "character",
    factor_formals, sizeof(factor_formals) / sizeof(factor_formals[0]),
    FORMAL_BIT(0) | FORMAL_BIT(3), 0
  },
  {
    "p_lgl", "ParamLgl", "ParamLgl", "logical",
    logical_formals, sizeof(logical_formals) / sizeof(logical_formals[0]),
    FORMAL_BIT(2), -1
  }
};

static SEXP builtin_runtime_root = NULL;
static SEXP builtin_constructor_symbols[BUILTIN_KIND_COUNT];
static SEXP builtin_formal_symbols[BUILTIN_KIND_COUNT][BUILTIN_FORMAL_COUNT];
static SEXP builtin_value_symbols[BUILTIN_VALUE_COUNT];
static SEXP builtin_sort_symbol = NULL;
static SEXP builtin_no_default_symbol = NULL;

void paradox_domain_builtin_release(void) {
  if (builtin_runtime_root != NULL) {
    R_ReleaseObject(builtin_runtime_root);
    builtin_runtime_root = NULL;
  }
}

static int string_is(SEXP value, const char *expected) {
  return value != NA_STRING && strcmp(CHAR(value), expected) == 0;
}

static int plain_node(SEXP value) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  return (type == LANGSXP || type == LISTSXP) &&
    !Rf_isObject(value) && !Rf_isS4(value) &&
    paradox_api_has_no_attributes(value);
}

static int exact_strings(SEXP value, const char *const *expected,
    R_xlen_t size) {
  if (TYPEOF(value) != STRSXP || ALTREP(value) || Rf_isObject(value) ||
      !paradox_api_has_no_attributes(value) || XLENGTH(value) != size) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    if (!string_is(STRING_ELT(value, index), expected[index])) {
      return FALSE;
    }
  }
  return TRUE;
}

static int runtime_arguments_match(SEXP constructors, SEXP no_default,
    SEXP sort_function) {
  if (builtin_runtime_root == NULL || TYPEOF(constructors) != VECSXP ||
      ALTREP(constructors) || XLENGTH(constructors) != BUILTIN_KIND_COUNT ||
      no_default != VECTOR_ELT(
        builtin_runtime_root,
        BUILTIN_RUNTIME_NO_DEFAULT
      ) || sort_function != VECTOR_ELT(
        builtin_runtime_root,
        BUILTIN_RUNTIME_SORT
      )) {
    return FALSE;
  }
  SEXP configured = VECTOR_ELT(
    builtin_runtime_root,
    BUILTIN_RUNTIME_CONSTRUCTORS
  );
  for (R_xlen_t index = 0; index < BUILTIN_KIND_COUNT; ++index) {
    if (VECTOR_ELT(constructors, index) != VECTOR_ELT(configured, index)) {
      return FALSE;
    }
  }
  return TRUE;
}

SEXP paradox_domain_builtin_runtime(SEXP constructors, SEXP no_default,
    SEXP sort_function) {
  if (builtin_runtime_root != NULL) {
    return Rf_ScalarLogical(runtime_arguments_match(
      constructors,
      no_default,
      sort_function
    ));
  }

  static const char *const constructor_names[] = {
    "p_dbl", "p_int", "p_fct", "p_lgl"
  };
  static const char *const no_default_class[] = {"NoDefault"};
  if (TYPEOF(constructors) != VECSXP || ALTREP(constructors) ||
      Rf_isObject(constructors) || XLENGTH(constructors) !=
        BUILTIN_KIND_COUNT ||
      !paradox_api_has_single_attribute(constructors, "names") ||
      TYPEOF(no_default) != VECSXP || ALTREP(no_default) ||
      XLENGTH(no_default) != 0 ||
      !paradox_api_has_single_attribute(no_default, "class") ||
      TYPEOF(sort_function) != CLOSXP || Rf_isObject(sort_function) ||
      !paradox_api_has_no_attributes(sort_function)) {
    return Rf_ScalarLogical(FALSE);
  }

  SEXP names = PROTECT(Rf_getAttrib(constructors, R_NamesSymbol));
  SEXP no_default_classes = PROTECT(Rf_getAttrib(
    no_default,
    R_ClassSymbol
  ));
  SEXP namespace_environment = PROTECT(
    paradox_api_registered_namespace("paradox")
  );
  if (!exact_strings(
      names,
      constructor_names,
      BUILTIN_KIND_COUNT
    ) || !exact_strings(no_default_classes, no_default_class, 1) ||
      TYPEOF(namespace_environment) != ENVSXP) {
    UNPROTECT(3);
    return Rf_ScalarLogical(FALSE);
  }

  builtin_sort_symbol = Rf_install("sort");
  if (paradox_api_stable_local_value(
      R_BaseEnv,
      builtin_sort_symbol
    ) != sort_function) {
    UNPROTECT(3);
    return Rf_ScalarLogical(FALSE);
  }

  builtin_no_default_symbol = Rf_install("NO_DEF");
  int valid = paradox_api_stable_local_value(
    namespace_environment,
    builtin_no_default_symbol
  ) == no_default;
  for (R_xlen_t index = 0; valid && index < BUILTIN_KIND_COUNT; ++index) {
    builtin_constructor_symbols[index] = Rf_install(constructor_names[index]);
    SEXP closure = VECTOR_ELT(constructors, index);
    static const char *const closure_attributes[] = {"srcref"};
    valid = TYPEOF(closure) == CLOSXP && !Rf_isObject(closure) &&
      paradox_api_has_only_attributes(closure, closure_attributes, 1) &&
      paradox_api_closure_environment(closure) == namespace_environment &&
      paradox_api_stable_local_value(
        namespace_environment,
        builtin_constructor_symbols[index]
      ) == closure;
    const struct builtin_spec *spec = &builtin_specs[index];
    for (size_t formal = 0; formal < spec->formal_count; ++formal) {
      builtin_formal_symbols[index][formal] = Rf_install(
        spec->formals[formal]
      );
    }
  }
  if (!valid) {
    UNPROTECT(3);
    return Rf_ScalarLogical(FALSE);
  }

  builtin_value_symbols[BUILTIN_LOWER] = Rf_install("lower");
  builtin_value_symbols[BUILTIN_UPPER] = Rf_install("upper");
  builtin_value_symbols[BUILTIN_TAGS] = Rf_install("tags");
  builtin_value_symbols[BUILTIN_TOLERANCE] = Rf_install("tolerance");
  SEXP root = PROTECT(Rf_allocVector(VECSXP, BUILTIN_RUNTIME_SLOT_COUNT));
  SET_VECTOR_ELT(root, BUILTIN_RUNTIME_CONSTRUCTORS, constructors);
  SET_VECTOR_ELT(root, BUILTIN_RUNTIME_NO_DEFAULT, no_default);
  SET_VECTOR_ELT(root, BUILTIN_RUNTIME_NAMESPACE, namespace_environment);
  SET_VECTOR_ELT(root, BUILTIN_RUNTIME_SORT, sort_function);
  R_PreserveObject(root);
  builtin_runtime_root = root;
  UNPROTECT(4);
  return Rf_ScalarLogical(TRUE);
}

static int kind_argument(SEXP value, enum builtin_kind *kind) {
  if (TYPEOF(value) != INTSXP || ALTREP(value) || Rf_isObject(value) ||
      !paradox_api_has_no_attributes(value) || XLENGTH(value) != 1) {
    return FALSE;
  }
  const int input = INTEGER_ELT(value, 0);
  if (input < BUILTIN_DBL || input >= BUILTIN_KIND_COUNT) {
    return FALSE;
  }
  *kind = (enum builtin_kind) input;
  return TRUE;
}

static int ascii_syntactic_name(SEXP symbol) {
  if (TYPEOF(symbol) != SYMSXP) {
    return FALSE;
  }
  SEXP name = PRINTNAME(symbol);
  if (name == NA_STRING || Rf_getCharCE(name) != CE_NATIVE ||
      LENGTH(name) == 0) {
    return FALSE;
  }
  const unsigned char *bytes = (const unsigned char *) CHAR(name);
  const int size = LENGTH(name);
  int index = 0;
  if (bytes[0] == '.') {
    index = 1;
    if (index < size && bytes[index] >= '0' && bytes[index] <= '9') {
      return FALSE;
    }
  } else if (!((bytes[0] >= 'a' && bytes[0] <= 'z') ||
      (bytes[0] >= 'A' && bytes[0] <= 'Z'))) {
    return FALSE;
  } else {
    index = 1;
  }
  for (; index < size; ++index) {
    const unsigned char byte = bytes[index];
    if (!((byte >= 'a' && byte <= 'z') ||
        (byte >= 'A' && byte <= 'Z') ||
        (byte >= '0' && byte <= '9') || byte == '.' || byte == '_')) {
      return FALSE;
    }
  }
  static const char *const reserved[] = {
    "if", "else", "repeat", "while", "function", "for", "in",
    "next", "break", "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA",
    "NA_integer_", "NA_real_", "NA_complex_", "NA_character_"
  };
  for (size_t keyword = 0;
      keyword < sizeof(reserved) / sizeof(reserved[0]); ++keyword) {
    if (strcmp(CHAR(name), reserved[keyword]) == 0) {
      return FALSE;
    }
  }
  return TRUE;
}

static int formal_index(const struct builtin_spec *spec, SEXP tag) {
  if (TYPEOF(tag) != SYMSXP || Rf_getCharCE(PRINTNAME(tag)) != CE_NATIVE) {
    return -1;
  }
  const char *name = CHAR(PRINTNAME(tag));
  for (size_t index = 0; index < spec->formal_count; ++index) {
    if (strcmp(name, spec->formals[index]) == 0) {
      return (int) index;
    }
  }
  return -1;
}

static int plan_call(SEXP call, enum builtin_kind kind,
    struct builtin_plan *plan) {
  if (TYPEOF(call) != LANGSXP || !plain_node(call) ||
      !ascii_syntactic_name(CAR(call))) {
    return FALSE;
  }
  const struct builtin_spec *spec = &builtin_specs[kind];
  plan->kind = kind;
  plan->head = CAR(call);
  plan->present = 0U;
  for (size_t index = 0; index < BUILTIN_FORMAL_COUNT; ++index) {
    plan->expressions[index] = R_MissingArg;
  }

  SEXP node = CDR(call);
  while (node != R_NilValue) {
    if (TYPEOF(node) != LISTSXP || !plain_node(node) ||
        CAR(node) == R_MissingArg) {
      return FALSE;
    }
    if (TAG(node) != R_NilValue) {
      const int index = formal_index(spec, TAG(node));
      if (index < 0 || plan->expressions[index] != R_MissingArg) {
        return FALSE;
      }
      plan->expressions[index] = CAR(node);
      plan->present |= FORMAL_BIT((unsigned int) index);
    }
    node = CDR(node);
  }

  size_t positional = 0;
  for (node = CDR(call); node != R_NilValue; node = CDR(node)) {
    if (TAG(node) == R_NilValue) {
      while (positional < spec->formal_count &&
          plan->expressions[positional] != R_MissingArg) {
        ++positional;
      }
      if (positional == spec->formal_count) {
        return FALSE;
      }
      plan->expressions[positional] = CAR(node);
      plan->present |= FORMAL_BIT((unsigned int) positional);
      ++positional;
    }
  }
  return (plan->present & ~spec->allowed) == 0U &&
    (spec->required < 0 ||
      (plan->present & FORMAL_BIT((unsigned int) spec->required)) != 0U);
}

static int runtime_matches(enum builtin_kind kind, SEXP wrapper) {
  if (builtin_runtime_root == NULL || TYPEOF(wrapper) != CLOSXP) {
    return FALSE;
  }
  SEXP namespace_environment = VECTOR_ELT(
    builtin_runtime_root,
    BUILTIN_RUNTIME_NAMESPACE
  );
  SEXP expected = VECTOR_ELT(
    VECTOR_ELT(builtin_runtime_root, BUILTIN_RUNTIME_CONSTRUCTORS),
    (R_xlen_t) kind
  );
  return wrapper == expected &&
    paradox_api_stable_local_value(
      namespace_environment,
      builtin_constructor_symbols[kind]
    ) == expected &&
    paradox_api_stable_local_value(
      namespace_environment,
      builtin_no_default_symbol
    ) == VECTOR_ELT(builtin_runtime_root, BUILTIN_RUNTIME_NO_DEFAULT);
}

#if R_VERSION >= R_Version(4, 6, 0)
static int lookup_is(SEXP environment, SEXP symbol, SEXP expected) {
  if (TYPEOF(environment) != ENVSXP || TYPEOF(symbol) != SYMSXP) {
    return FALSE;
  }
  SEXP current = environment;
  while (current != R_EmptyEnv) {
    if (R_existsVarInFrame(current, symbol)) {
      return paradox_api_stable_local_value(current, symbol) == expected;
    }
    current = paradox_api_parent_environment(current);
    if (TYPEOF(current) != ENVSXP) {
      return FALSE;
    }
  }
  return FALSE;
}

static int init_formal_index(enum builtin_kind kind) {
  switch (kind) {
  case BUILTIN_DBL:
  case BUILTIN_INT:
    return 9;
  case BUILTIN_FCT:
    return 6;
  case BUILTIN_LGL:
    return 5;
  case BUILTIN_KIND_COUNT:
    return -1;
  }
  return -1;
}

static int forced_slot_for_formal(enum builtin_kind kind, size_t formal) {
  if (kind == BUILTIN_DBL || kind == BUILTIN_INT) {
    if (formal == 0) return BUILTIN_LOWER;
    if (formal == 1) return BUILTIN_UPPER;
    if (formal == 4) return BUILTIN_TAGS;
    if (formal == 5) return BUILTIN_TOLERANCE;
  } else if (kind == BUILTIN_FCT && formal == 3) {
    return BUILTIN_TAGS;
  } else if (kind == BUILTIN_LGL && formal == 2) {
    return BUILTIN_TAGS;
  }
  return -1;
}
#endif

/* R does not expose the dynamic call-stack owner of an environment.  The
 * public, allocation-free proof available here constrains the candidate frame
 * to the canonical closure namespace and exact formal-binding states, while
 * the simple call head must resolve inertly to the preserved wrapper in the
 * supplied caller environment.  This is an internal fast-path boundary, not a
 * security boundary against a caller deliberately forging a registered .Call. */
static int canonical_frame_surface(SEXP frame, SEXP caller_environment,
    SEXP wrapper, const struct builtin_plan *plan, unsigned int forced) {
#if R_VERSION < R_Version(4, 6, 0)
  (void) frame;
  (void) caller_environment;
  (void) wrapper;
  (void) plan;
  (void) forced;
  return FALSE;
#else
  SEXP namespace_environment = VECTOR_ELT(
    builtin_runtime_root,
    BUILTIN_RUNTIME_NAMESPACE
  );
  if (paradox_api_parent_environment(frame) != namespace_environment ||
      !lookup_is(caller_environment, plan->head, wrapper)) {
    return FALSE;
  }
  const struct builtin_spec *spec = &builtin_specs[plan->kind];
  const int init_index = init_formal_index(plan->kind);
  for (size_t formal = 0; formal < spec->formal_count; ++formal) {
    const R_BindingType_t observed = R_GetBindingType(
      builtin_formal_symbols[plan->kind][formal],
      frame
    );
    const int slot = forced_slot_for_formal(plan->kind, formal);
    const R_BindingType_t expected = slot >= 0 &&
      (forced & (1U << (unsigned int) slot)) != 0U ?
      R_BindingTypeForced :
      ((int) formal == init_index ?
        R_BindingTypeMissing : R_BindingTypeDelayed);
    if (observed != expected) {
      return FALSE;
    }
  }
  return TRUE;
#endif
}

static SEXP root_value(SEXP roots, R_xlen_t *cursor, SEXP value) {
  if (*cursor >= XLENGTH(roots)) {
    Rf_error("Internal error: direct Domain root capacity exceeded");
  }
  SET_VECTOR_ELT(roots, *cursor, value);
  ++*cursor;
  return value;
}

static int protect_delayed_formals(SEXP frame, SEXP wrapper,
    SEXP caller_environment, const struct builtin_plan *plan,
    struct delayed_formals_snapshot *snapshot, int *protect_count) {
#if R_VERSION < R_Version(4, 6, 0)
  (void) frame;
  (void) wrapper;
  (void) caller_environment;
  (void) plan;
  (void) snapshot;
  (void) protect_count;
  return FALSE;
#else
  snapshot->captured = 0U;
  *protect_count = 0;
  const struct builtin_spec *spec = &builtin_specs[plan->kind];
  const int init_index = init_formal_index(plan->kind);
  SEXP formal_node = paradox_api_closure_formals(wrapper);
  for (size_t formal = 0; formal < spec->formal_count; ++formal) {
    SEXP symbol = builtin_formal_symbols[plan->kind][formal];
    if (TYPEOF(formal_node) != LISTSXP || TAG(formal_node) != symbol) {
      return FALSE;
    }
    const R_BindingType_t binding = R_GetBindingType(symbol, frame);
    if ((int) formal == init_index) {
      if (binding != R_BindingTypeMissing || CAR(formal_node) != R_MissingArg) {
        return FALSE;
      }
      formal_node = CDR(formal_node);
      continue;
    }
    if (binding != R_BindingTypeDelayed) {
      return FALSE;
    }
    SEXP expression = R_DelayedBindingExpression(symbol, frame);
    SEXP environment = R_DelayedBindingEnvironment(symbol, frame);
    const int supplied =
      (plan->present & FORMAL_BIT((unsigned int) formal)) != 0U;
    SEXP expected_expression = supplied
      ? plan->expressions[formal]
      : CAR(formal_node);
    SEXP expected_environment = supplied ? caller_environment : frame;
    if (expression != expected_expression || environment != expected_environment) {
      return FALSE;
    }
    snapshot->expressions[formal] = PROTECT(expression);
    ++*protect_count;
    snapshot->environments[formal] = PROTECT(environment);
    ++*protect_count;
    snapshot->captured |= FORMAL_BIT((unsigned int) formal);
    formal_node = CDR(formal_node);
  }
  return formal_node == R_NilValue;
#endif
}

static int delayed_formals_match(SEXP frame,
    const struct builtin_plan *plan, unsigned int forced,
    const struct delayed_formals_snapshot *snapshot) {
#if R_VERSION < R_Version(4, 6, 0)
  (void) frame;
  (void) plan;
  (void) forced;
  (void) snapshot;
  return FALSE;
#else
  const struct builtin_spec *spec = &builtin_specs[plan->kind];
  for (size_t formal = 0; formal < spec->formal_count; ++formal) {
    if ((snapshot->captured & FORMAL_BIT((unsigned int) formal)) == 0U) {
      continue;
    }
    const int slot = forced_slot_for_formal(plan->kind, formal);
    if (slot >= 0 &&
        (forced & (1U << (unsigned int) slot)) != 0U) {
      continue;
    }
    SEXP symbol = builtin_formal_symbols[plan->kind][formal];
    if (R_GetBindingType(symbol, frame) != R_BindingTypeDelayed ||
        R_DelayedBindingExpression(symbol, frame) !=
          snapshot->expressions[formal] ||
        R_DelayedBindingEnvironment(symbol, frame) !=
          snapshot->environments[formal]) {
      return FALSE;
    }
  }
  return TRUE;
#endif
}

static SEXP scalar_string(const char *value, SEXP roots,
    R_xlen_t *cursor) {
  SEXP result = Rf_mkString(value);
  return root_value(roots, cursor, result);
}

static int plain_number(SEXP value, double *number) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  if ((type != INTSXP && type != REALSXP) || ALTREP(value) ||
      Rf_isObject(value) || Rf_isS4(value) ||
      !paradox_api_has_no_attributes(value) || XLENGTH(value) != 1) {
    return FALSE;
  }
  if (type == INTSXP) {
    const int input = INTEGER_ELT(value, 0);
    if (input == NA_INTEGER) {
      return FALSE;
    }
    *number = (double) input;
    return TRUE;
  }
  if (type == REALSXP) {
    const double input = REAL_ELT(value, 0);
    if (ISNAN(input)) {
      return FALSE;
    }
    *number = input;
    return TRUE;
  }
  return FALSE;
}

static int integer_bound(double value) {
  return !R_FINITE(value) ||
    (value >= -(double) INT_MAX && value <= (double) INT_MAX &&
      value == (double) ((int) value));
}

static int plain_strings(SEXP value) {
  if (TYPEOF(value) != STRSXP || ALTREP(value) || Rf_isObject(value) ||
      Rf_isS4(value) || !paradox_api_has_no_attributes(value)) {
    return FALSE;
  }
  const R_xlen_t size = XLENGTH(value);
  if (size > BUILTIN_MAX_STRINGS) {
    return FALSE;
  }
  size_t byte_count = 0;
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP current = STRING_ELT(value, index);
    const int length = current == NA_STRING ? -1 : LENGTH(current);
    if (length < 0 || (size_t) length >
        BUILTIN_MAX_STRING_BYTES - byte_count) {
      return FALSE;
    }
    byte_count += (size_t) length;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP current = STRING_ELT(value, index);
    for (R_xlen_t previous = 0; previous < index; ++previous) {
      SEXP other = STRING_ELT(value, previous);
      if (current == other || strcmp(CHAR(current), CHAR(other)) == 0) {
        return FALSE;
      }
    }
  }
  return TRUE;
}

static int capture_scalar(SEXP value, struct scalar_snapshot *snapshot,
    double *number) {
  if (!plain_number(value, number)) {
    return FALSE;
  }
  snapshot->value = value;
  snapshot->type = (SEXPTYPE) TYPEOF(value);
  if (snapshot->type == INTSXP) {
    snapshot->content.integer = INTEGER_ELT(value, 0);
  } else {
    snapshot->content.real = REAL_ELT(value, 0);
  }
  return TRUE;
}

static int scalar_matches(const struct scalar_snapshot *snapshot) {
  SEXP value = snapshot->value;
  if ((SEXPTYPE) TYPEOF(value) != snapshot->type || ALTREP(value) ||
      Rf_isObject(value) || Rf_isS4(value) ||
      !paradox_api_has_no_attributes(value) || XLENGTH(value) != 1) {
    return FALSE;
  }
  if (snapshot->type == INTSXP) {
    return INTEGER_ELT(value, 0) == snapshot->content.integer;
  }
  const double current = REAL_ELT(value, 0);
  return memcmp(
    &current,
    &snapshot->content.real,
    sizeof(current)
  ) == 0;
}

static int capture_strings(SEXP value, struct strings_snapshot *snapshot) {
  if (!plain_strings(value)) {
    return FALSE;
  }
  const R_xlen_t size = XLENGTH(value);
  size_t offset = 0;
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP element = STRING_ELT(value, index);
    const int length = LENGTH(element);
    if (length < 0 || offset > BUILTIN_MAX_STRING_BYTES ||
        (size_t) length > BUILTIN_MAX_STRING_BYTES - offset) {
      return FALSE;
    }
    snapshot->elements[index] = element;
    snapshot->lengths[index] = length;
    snapshot->encodings[index] = Rf_getCharCE(element);
    snapshot->offsets[index] = offset;
    memcpy(snapshot->bytes + offset, CHAR(element), (size_t) length);
    offset += (size_t) length;
  }
  snapshot->value = value;
  snapshot->size = size;
  snapshot->byte_count = offset;
  return TRUE;
}

static int strings_match(const struct strings_snapshot *snapshot) {
  SEXP value = snapshot->value;
  if (TYPEOF(value) != STRSXP || ALTREP(value) || Rf_isObject(value) ||
      Rf_isS4(value) || !paradox_api_has_no_attributes(value) ||
      XLENGTH(value) != snapshot->size) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < snapshot->size; ++index) {
    SEXP element = STRING_ELT(value, index);
    const int length = snapshot->lengths[index];
    const size_t offset = snapshot->offsets[index];
    if (element != snapshot->elements[index] || LENGTH(element) != length ||
        Rf_getCharCE(element) != snapshot->encodings[index] ||
        offset > snapshot->byte_count ||
        (size_t) length > snapshot->byte_count - offset ||
        memcmp(
          CHAR(element),
          snapshot->bytes + offset,
          (size_t) length
        ) != 0) {
      return FALSE;
    }
  }
  return TRUE;
}

static int force_frame_value(SEXP frame, enum builtin_value slot,
    SEXP roots, R_xlen_t *cursor, SEXP *value) {
  SEXP symbol = builtin_value_symbols[slot];
  if (!R_existsVarInFrame(frame, symbol) || R_BindingIsActive(symbol, frame)) {
    return FALSE;
  }
  *value = Rf_eval(symbol, frame);
  root_value(roots, cursor, *value);
  return TRUE;
}

static int stable_frame_value(SEXP frame, enum builtin_value slot,
    SEXP expected) {
  return paradox_api_stable_local_value(
    frame,
    builtin_value_symbols[slot]
  ) == expected;
}

static SEXP build_representation(const struct builtin_plan *plan,
    SEXP const *resolved, SEXP roots, R_xlen_t *cursor) {
  const struct builtin_spec *spec = &builtin_specs[plan->kind];
  SEXP arguments = R_NilValue;
  for (size_t reverse = spec->formal_count; reverse != 0; --reverse) {
    const size_t index = reverse - 1;
    if ((plan->present & FORMAL_BIT((unsigned int) index)) == 0U) {
      continue;
    }
    SEXP value = resolved[index];
    if (value == R_MissingArg) {
      return R_NilValue;
    }
    SEXP node = Rf_cons(value, arguments);
    root_value(roots, cursor, node);
    SET_TAG(node, Rf_install(spec->formals[index]));
    arguments = node;
  }
  SEXP result = Rf_lcons(plan->head, arguments);
  return root_value(roots, cursor, result);
}

static SEXP representation_id(const struct builtin_plan *plan,
    SEXP representation, SEXP roots, R_xlen_t *cursor) {
  SEXP canonical = Rf_duplicate(representation);
  root_value(roots, cursor, canonical);
  SETCAR(canonical, builtin_constructor_symbols[plan->kind]);
  SEXP canonical_id = paradox_domain_simple_repr_id(canonical);
  if (canonical_id == R_NilValue) {
    canonical_id = paradox_domain_relaxed_repr_id(canonical);
  }
  if (canonical_id == R_NilValue) {
    return R_NilValue;
  }
  root_value(roots, cursor, canonical_id);
  if (plan->head == builtin_constructor_symbols[plan->kind]) {
    return canonical_id;
  }

  SEXP text = STRING_ELT(canonical_id, 0);
  SEXP canonical_name = PRINTNAME(builtin_constructor_symbols[plan->kind]);
  SEXP alias_name = PRINTNAME(plan->head);
  const int text_size = LENGTH(text);
  const int canonical_size = LENGTH(canonical_name);
  const int alias_size = LENGTH(alias_name);
  if (text_size < canonical_size ||
      memcmp(CHAR(text), CHAR(canonical_name), (size_t) canonical_size) != 0 ||
      alias_size > 80 - (text_size - canonical_size)) {
    return R_NilValue;
  }
  const int output_size = alias_size + text_size - canonical_size;
  char *buffer = R_alloc((size_t) output_size + 1, sizeof(*buffer));
  memcpy(buffer, CHAR(alias_name), (size_t) alias_size);
  memcpy(
    buffer + alias_size,
    CHAR(text) + canonical_size,
    (size_t) (text_size - canonical_size)
  );
  buffer[output_size] = '\0';
  SEXP encoded = PROTECT(Rf_mkCharLen(buffer, output_size));
  SEXP result = Rf_ScalarString(encoded);
  root_value(roots, cursor, result);
  UNPROTECT(1);
  return result;
}

static SEXP domain_class(const char *class_name, SEXP roots,
    R_xlen_t *cursor) {
  static const char *const suffix[] = {
    "Domain", "data.table", "data.frame"
  };
  SEXP result = Rf_allocVector(STRSXP, 4);
  root_value(roots, cursor, result);
  SET_STRING_ELT(result, 0, Rf_mkChar(class_name));
  for (R_xlen_t index = 0; index < 3; ++index) {
    SET_STRING_ELT(result, index + 1, Rf_mkChar(suffix[index]));
  }
  return result;
}

static int same_plan(const struct builtin_plan *first,
    const struct builtin_plan *second) {
  if (first->kind != second->kind || first->head != second->head ||
      first->present != second->present) {
    return FALSE;
  }
  const size_t count = builtin_specs[first->kind].formal_count;
  for (size_t index = 0; index < count; ++index) {
    if (first->expressions[index] != second->expressions[index]) {
      return FALSE;
    }
  }
  return TRUE;
}

SEXP paradox_domain_construct_builtin(SEXP frame, SEXP wrapper, SEXP call,
    SEXP caller_environment, SEXP kind_value) {
  enum builtin_kind kind;
  if (builtin_runtime_root == NULL || TYPEOF(frame) != ENVSXP ||
      Rf_isObject(frame) || TYPEOF(caller_environment) != ENVSXP ||
      !kind_argument(kind_value, &kind) || !runtime_matches(kind, wrapper)) {
    return R_NilValue;
  }
  if (kind == BUILTIN_FCT) {
    return R_NilValue;
  }

  struct builtin_plan plan;
  if (!plan_call(call, kind, &plan) ||
      !canonical_frame_surface(
        frame,
        caller_environment,
        wrapper,
        &plan,
        0U
      )) {
    return R_NilValue;
  }

  struct delayed_formals_snapshot delayed_formals;
  int delayed_protect_count = 0;
  if (!protect_delayed_formals(
      frame,
      wrapper,
      caller_environment,
      &plan,
      &delayed_formals,
      &delayed_protect_count
    )) {
    UNPROTECT(delayed_protect_count);
    return R_NilValue;
  }

  SEXP roots = PROTECT(Rf_allocVector(VECSXP, BUILTIN_ROOT_CAPACITY));
  R_xlen_t root_cursor = 0;
  struct builtin_plan allocation_plan;
  if (!delayed_formals_match(frame, &plan, 0U, &delayed_formals) ||
      !plan_call(call, kind, &allocation_plan) ||
      !same_plan(&plan, &allocation_plan) ||
      !canonical_frame_surface(
        frame,
        caller_environment,
        wrapper,
        &plan,
        0U
      ) || !runtime_matches(kind, wrapper)) {
    UNPROTECT(delayed_protect_count + 1);
    return R_NilValue;
  }
  SEXP resolved[BUILTIN_FORMAL_COUNT];
  for (size_t index = 0; index < BUILTIN_FORMAL_COUNT; ++index) {
    resolved[index] = R_MissingArg;
  }
  unsigned int forced = 0U;

  SEXP lower;
  SEXP upper;
  SEXP tolerance;
  SEXP levels = R_NilValue;
  SEXP tags;
  SEXP grouping;
  double lower_number = NA_REAL;
  double upper_number = NA_REAL;
  double tolerance_number = NA_REAL;
  struct scalar_snapshot lower_snapshot;
  struct scalar_snapshot upper_snapshot;
  struct scalar_snapshot tolerance_snapshot;
  struct strings_snapshot tags_snapshot;

  if (kind == BUILTIN_DBL || kind == BUILTIN_INT) {
    if ((plan.present & FORMAL_BIT(5)) != 0U) {
      if (!force_frame_value(
          frame, BUILTIN_TOLERANCE, roots, &root_cursor, &tolerance
        )) {
        goto decline;
      }
      forced |= 1U << BUILTIN_TOLERANCE;
    } else {
      tolerance = root_value(
        roots,
        &root_cursor,
        Rf_ScalarReal(sqrt(DBL_EPSILON))
      );
    }
    resolved[5] = tolerance;
    if (!plain_number(tolerance, &tolerance_number) ||
        tolerance_number < 0.0 ||
        (kind == BUILTIN_INT && tolerance_number > 0.5)) {
      goto decline;
    }

    if ((plan.present & FORMAL_BIT(0)) != 0U) {
      if (!force_frame_value(
          frame, BUILTIN_LOWER, roots, &root_cursor, &lower
        )) {
        goto decline;
      }
      forced |= 1U << BUILTIN_LOWER;
    } else {
      lower = root_value(roots, &root_cursor, Rf_ScalarReal(R_NegInf));
    }
    resolved[0] = lower;
    if (!plain_number(lower, &lower_number) ||
        (kind == BUILTIN_INT && !integer_bound(lower_number))) {
      goto decline;
    }

    if ((plan.present & FORMAL_BIT(1)) != 0U) {
      if (!force_frame_value(
          frame, BUILTIN_UPPER, roots, &root_cursor, &upper
        )) {
        goto decline;
      }
      forced |= 1U << BUILTIN_UPPER;
    } else {
      upper = root_value(roots, &root_cursor, Rf_ScalarReal(R_PosInf));
    }
    resolved[1] = upper;
    if (!plain_number(upper, &upper_number) ||
        (kind == BUILTIN_INT && !integer_bound(upper_number)) ||
        lower_number > upper_number) {
      goto decline;
    }
  } else {
    lower = root_value(roots, &root_cursor, Rf_ScalarReal(NA_REAL));
    upper = root_value(roots, &root_cursor, Rf_ScalarReal(NA_REAL));
    tolerance = root_value(roots, &root_cursor, Rf_ScalarReal(NA_REAL));
  }
  grouping = scalar_string(
    builtin_specs[kind].grouping,
    roots,
    &root_cursor
  );
  if (kind == BUILTIN_LGL) {
    levels = Rf_allocVector(LGLSXP, 2);
    root_value(roots, &root_cursor, levels);
    SET_LOGICAL_ELT(levels, 0, TRUE);
    SET_LOGICAL_ELT(levels, 1, FALSE);
  }
  const int tags_formal = kind == BUILTIN_LGL ? 2 : 4;
  if ((plan.present & FORMAL_BIT((unsigned int) tags_formal)) != 0U) {
    if (!force_frame_value(
        frame, BUILTIN_TAGS, roots, &root_cursor, &tags
      )) {
      goto decline;
    }
    forced |= 1U << BUILTIN_TAGS;
  } else {
    tags = Rf_allocVector(STRSXP, 0);
    root_value(roots, &root_cursor, tags);
  }
  resolved[tags_formal] = tags;
  if (!capture_strings(tags, &tags_snapshot)) {
    goto decline;
  }
  /* Snapshot numeric values only after forcing and rooting the tags.  That
   * work may allocate and run a finalizer that changes an earlier forced
   * scalar in place; repr and Domain need one coherent later view. */
  if (kind == BUILTIN_DBL || kind == BUILTIN_INT) {
    if (!capture_scalar(lower, &lower_snapshot, &lower_number) ||
        !capture_scalar(upper, &upper_snapshot, &upper_number) ||
        !capture_scalar(
          tolerance,
          &tolerance_snapshot,
          &tolerance_number
        ) || lower_number > upper_number || tolerance_number < 0.0 ||
        (kind == BUILTIN_INT &&
          (tolerance_number > 0.5 || !integer_bound(lower_number) ||
           !integer_bound(upper_number)))) {
      goto decline;
    }
  }

  SEXP representation = build_representation(
    &plan,
    resolved,
    roots,
    &root_cursor
  );
  if (representation == R_NilValue) {
    goto decline;
  }
  SEXP id = representation_id(&plan, representation, roots, &root_cursor);
  if (id == R_NilValue) {
    goto decline;
  }
  SEXP class_name = scalar_string(
    builtin_specs[kind].class_name,
    roots,
    &root_cursor
  );
  SEXP storage = scalar_string(
    builtin_specs[kind].storage,
    roots,
    &root_cursor
  );
  SEXP special_values = Rf_allocVector(VECSXP, 0);
  root_value(roots, &root_cursor, special_values);
  SEXP init_given = Rf_ScalarLogical(FALSE);
  root_value(roots, &root_cursor, init_given);
  SEXP domain = paradox_domain_construct(
    class_name,
    grouping,
    R_NilValue,
    lower,
    upper,
    tolerance,
    levels,
    special_values,
    VECTOR_ELT(builtin_runtime_root, BUILTIN_RUNTIME_NO_DEFAULT),
    tags,
    R_NilValue,
    storage,
    init_given,
    R_NilValue
  );
  if (domain == R_NilValue) {
    goto decline;
  }
  root_value(roots, &root_cursor, domain);
  SET_VECTOR_ELT(domain, 0, id);
  SEXP classes = domain_class(
    builtin_specs[kind].class_name,
    roots,
    &root_cursor
  );
  Rf_setAttrib(domain, R_ClassSymbol, classes);
  Rf_setAttrib(domain, Rf_install("repr"), representation);

  /* Commit audit.  Everything that follows is allocation-free. */
  if ((kind == BUILTIN_DBL || kind == BUILTIN_INT) &&
      (!scalar_matches(&lower_snapshot) ||
       !scalar_matches(&upper_snapshot) ||
       !scalar_matches(&tolerance_snapshot))) {
    goto decline;
  }
  if (!strings_match(&tags_snapshot)) {
    goto decline;
  }
  for (int slot = 0; slot < BUILTIN_VALUE_COUNT; ++slot) {
    if ((forced & (1U << (unsigned int) slot)) != 0U) {
      SEXP expected = slot == BUILTIN_LOWER ? lower :
        (slot == BUILTIN_UPPER ? upper :
        (slot == BUILTIN_TAGS ? tags : tolerance));
      if (!stable_frame_value(frame, (enum builtin_value) slot, expected)) {
        goto decline;
      }
    }
  }
  struct builtin_plan final_plan;
  if (!plan_call(call, kind, &final_plan) ||
      !same_plan(&plan, &final_plan) ||
      !delayed_formals_match(frame, &plan, forced, &delayed_formals) ||
      !canonical_frame_surface(
        frame,
        caller_environment,
        wrapper,
        &plan,
        forced
      ) || !runtime_matches(kind, wrapper)) {
    goto decline;
  }

  UNPROTECT(delayed_protect_count + 1);
  return domain;

decline:
  UNPROTECT(delayed_protect_count + 1);
  return R_NilValue;
}
