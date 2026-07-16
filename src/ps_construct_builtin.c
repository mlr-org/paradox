#include <float.h>
#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <string.h>

#include "paradox.h"
#include "r_api_compat.h"
#include "r_utils.h"

/* `ps()` commonly receives literal calls to the four ordinary parameter
 * constructors.  Building full, printable Domain objects for those temporary
 * values is expensive: ParamSet immediately discards their representation and
 * private columns.  This file recognizes a deliberately small, callback-free
 * call grammar and builds anonymous Domain-shaped rows for the existing
 * ParamSet$new() path.  Anything outside that grammar is declined before an
 * argument expression is evaluated. */

#define PS_BUILTIN_MAX_DOMAINS ((R_xlen_t) 65536)

enum ps_runtime_root {
  PS_RUNTIME_CONSTRUCTORS = 0,
  PS_RUNTIME_NO_DEFAULT,
  PS_RUNTIME_ROOT_COUNT
};

enum ps_constructor_kind {
  PS_KIND_DBL = 0,
  PS_KIND_INT,
  PS_KIND_FCT,
  PS_KIND_LGL,
  PS_KIND_COUNT
};

enum ps_value_slot {
  PS_VALUE_LOWER = 0,
  PS_VALUE_UPPER,
  PS_VALUE_TOLERANCE,
  PS_VALUE_LEVELS,
  PS_VALUE_TAGS,
  PS_VALUE_SLOT_COUNT
};

struct ps_constructor_plan {
  enum ps_constructor_kind kind;
  SEXP symbol;
  SEXP expressions[PS_VALUE_SLOT_COUNT];
};

struct ps_constructor_spec {
  const char *name;
  const char *const *formals;
  const int *slots;
  size_t formal_count;
  int required_slot;
};

static const char *const dbl_formals[] = {
  "lower", "upper", "special_vals", "default", "tags", "tolerance",
  "depends", "trafo", "logscale", "init", "aggr", "in_tune_fn",
  "disable_in_tune"
};

static const int dbl_slots[] = {
  PS_VALUE_LOWER, PS_VALUE_UPPER, -1, -1, PS_VALUE_TAGS,
  PS_VALUE_TOLERANCE, -1, -1, -1, -1, -1, -1, -1
};

static const char *const fct_formals[] = {
  "levels", "special_vals", "default", "tags", "depends", "trafo",
  "init", "aggr", "in_tune_fn", "disable_in_tune"
};

static const int fct_slots[] = {
  PS_VALUE_LEVELS, -1, -1, PS_VALUE_TAGS, -1, -1, -1, -1, -1, -1
};

static const char *const lgl_formals[] = {
  "special_vals", "default", "tags", "depends", "trafo", "init",
  "aggr", "in_tune_fn", "disable_in_tune"
};

static const int lgl_slots[] = {
  -1, -1, PS_VALUE_TAGS, -1, -1, -1, -1, -1, -1
};

static const struct ps_constructor_spec constructor_specs[PS_KIND_COUNT] = {
  {
    "p_dbl", dbl_formals, dbl_slots,
    sizeof(dbl_formals) / sizeof(dbl_formals[0]), -1
  },
  {
    "p_int", dbl_formals, dbl_slots,
    sizeof(dbl_formals) / sizeof(dbl_formals[0]), -1
  },
  {
    "p_fct", fct_formals, fct_slots,
    sizeof(fct_formals) / sizeof(fct_formals[0]), PS_VALUE_LEVELS
  },
  {
    "p_lgl", lgl_formals, lgl_slots,
    sizeof(lgl_formals) / sizeof(lgl_formals[0]), -1
  }
};

static SEXP ps_runtime_root = NULL;

void paradox_ps_builtin_release(void) {
  if (ps_runtime_root != NULL) {
    R_ReleaseObject(ps_runtime_root);
    ps_runtime_root = NULL;
  }
}

static int string_is(SEXP string, const char *expected) {
  return string != NA_STRING && strcmp(CHAR(string), expected) == 0;
}

static int exact_string_vector(SEXP value,
    const char *const *expected, R_xlen_t size) {
  if (TYPEOF(value) != STRSXP || ALTREP(value) || XLENGTH(value) != size) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    if (!string_is(STRING_ELT(value, index), expected[index])) {
      return FALSE;
    }
  }
  return TRUE;
}

static int runtime_arguments_match(SEXP constructors, SEXP no_default) {
  if (ps_runtime_root == NULL) {
    return FALSE;
  }
  SEXP configured = VECTOR_ELT(
    ps_runtime_root,
    PS_RUNTIME_CONSTRUCTORS
  );
  if (TYPEOF(constructors) != VECSXP || ALTREP(constructors) ||
      XLENGTH(constructors) != PS_KIND_COUNT ||
      no_default != VECTOR_ELT(ps_runtime_root, PS_RUNTIME_NO_DEFAULT)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < PS_KIND_COUNT; ++index) {
    if (VECTOR_ELT(constructors, index) != VECTOR_ELT(configured, index)) {
      return FALSE;
    }
  }
  return TRUE;
}

SEXP paradox_ps_builtin_runtime(SEXP constructors, SEXP no_default) {
  if (ps_runtime_root != NULL) {
    return Rf_ScalarLogical(runtime_arguments_match(
      constructors,
      no_default
    ));
  }

  static const char *const constructor_names[] = {
    "p_dbl", "p_int", "p_fct", "p_lgl"
  };
  static const char *const no_default_class[] = {"NoDefault"};
  if (TYPEOF(constructors) != VECSXP || ALTREP(constructors) ||
      XLENGTH(constructors) != PS_KIND_COUNT ||
      !paradox_api_has_single_attribute(constructors, "names")) {
    return Rf_ScalarLogical(FALSE);
  }
  SEXP names = PROTECT(Rf_getAttrib(constructors, R_NamesSymbol));
  if (!exact_string_vector(names, constructor_names, PS_KIND_COUNT)) {
    UNPROTECT(1);
    return Rf_ScalarLogical(FALSE);
  }

  SEXP namespace_environment = PROTECT(
    paradox_api_registered_namespace("paradox")
  );
  if (TYPEOF(namespace_environment) != ENVSXP ||
      TYPEOF(no_default) != VECSXP || ALTREP(no_default) ||
      XLENGTH(no_default) != 0 ||
      !paradox_api_has_single_attribute(no_default, "class")) {
    UNPROTECT(2);
    return Rf_ScalarLogical(FALSE);
  }
  SEXP no_default_classes = PROTECT(Rf_getAttrib(
    no_default,
    R_ClassSymbol
  ));
  if (!exact_string_vector(no_default_classes, no_default_class, 1)) {
    UNPROTECT(3);
    return Rf_ScalarLogical(FALSE);
  }

  int valid = TRUE;
  for (R_xlen_t index = 0; index < PS_KIND_COUNT; ++index) {
    SEXP closure = VECTOR_ELT(constructors, index);
    SEXP symbol = Rf_install(constructor_names[index]);
    if (TYPEOF(closure) != CLOSXP ||
        paradox_api_closure_environment(closure) != namespace_environment ||
        paradox_api_stable_local_value(namespace_environment, symbol) !=
          closure) {
      valid = FALSE;
      break;
    }
  }
  if (valid && paradox_api_stable_local_value(
      namespace_environment,
      Rf_install("NO_DEF")
    ) != no_default) {
    valid = FALSE;
  }
  if (!valid) {
    UNPROTECT(3);
    return Rf_ScalarLogical(FALSE);
  }

  SEXP root = PROTECT(Rf_allocVector(VECSXP, PS_RUNTIME_ROOT_COUNT));
  SET_VECTOR_ELT(root, PS_RUNTIME_CONSTRUCTORS, constructors);
  SET_VECTOR_ELT(root, PS_RUNTIME_NO_DEFAULT, no_default);
  R_PreserveObject(root);
  ps_runtime_root = root;
  UNPROTECT(4);
  return Rf_ScalarLogical(TRUE);
}

static int plain_pairlist(SEXP node) {
  return (TYPEOF(node) == LISTSXP || TYPEOF(node) == LANGSXP) &&
    !Rf_isObject(node) && !Rf_isS4(node) &&
    paradox_api_has_no_attributes(node);
}

static int strict_ascii_id(SEXP value) {
  if (value == R_NilValue || TYPEOF(value) != SYMSXP) {
    return FALSE;
  }
  SEXP name = PRINTNAME(value);
  if (name == NA_STRING || Rf_getCharCE(name) != CE_NATIVE ||
      LENGTH(name) == 0) {
    return FALSE;
  }
  const unsigned char *bytes = (const unsigned char *) CHAR(name);
  const int size = LENGTH(name);
  int index = 0;
  while (index < size && bytes[index] == '.') {
    ++index;
  }
  if (index == size ||
      !((bytes[index] >= 'a' && bytes[index] <= 'z') ||
        (bytes[index] >= 'A' && bytes[index] <= 'Z'))) {
    return FALSE;
  }
  for (++index; index < size; ++index) {
    const unsigned char byte = bytes[index];
    if (!((byte >= 'a' && byte <= 'z') ||
          (byte >= 'A' && byte <= 'Z') ||
          (byte >= '0' && byte <= '9') || byte == '.' || byte == '_')) {
      return FALSE;
    }
  }
  return TRUE;
}

/* Resolve an unqualified function name without invoking an active binding or
 * forcing a delayed binding.  Encountering either kind before the canonical
 * closure is a decline, not permission to continue into the parent. */
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

static SEXP canonical_constructor(enum ps_constructor_kind kind) {
  return VECTOR_ELT(
    VECTOR_ELT(ps_runtime_root, PS_RUNTIME_CONSTRUCTORS),
    (R_xlen_t) kind
  );
}

static int constructor_kind(SEXP symbol,
    enum ps_constructor_kind *kind) {
  if (TYPEOF(symbol) != SYMSXP || Rf_getCharCE(PRINTNAME(symbol)) != CE_NATIVE) {
    return FALSE;
  }
  const char *name = CHAR(PRINTNAME(symbol));
  for (int index = 0; index < PS_KIND_COUNT; ++index) {
    if (strcmp(name, constructor_specs[index].name) == 0) {
      *kind = (enum ps_constructor_kind) index;
      return TRUE;
    }
  }
  return FALSE;
}

static int formal_index(const struct ps_constructor_spec *spec,
    SEXP tag) {
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

static SEXP base_function(const char *name) {
  return paradox_api_stable_local_value(R_BaseEnv, Rf_install(name));
}

static int numeric_literal_surface(SEXP expression, SEXP eval_environment) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(expression);
  if ((type == INTSXP || type == REALSXP) && !ALTREP(expression) &&
      !Rf_isObject(expression) && !Rf_isS4(expression) &&
      paradox_api_has_no_attributes(expression) &&
      XLENGTH(expression) == 1) {
    return TRUE;
  }
  if (type != LANGSXP || !plain_pairlist(expression)) {
    return FALSE;
  }
  SEXP head = CAR(expression);
  if (TYPEOF(head) != SYMSXP ||
      (!string_is(PRINTNAME(head), "-") &&
       !string_is(PRINTNAME(head), "+"))) {
    return FALSE;
  }
  SEXP arguments = CDR(expression);
  if (arguments == R_NilValue || TYPEOF(arguments) != LISTSXP ||
      !plain_pairlist(arguments) || TAG(arguments) != R_NilValue ||
      CDR(arguments) != R_NilValue) {
    return FALSE;
  }
  SEXP operand = CAR(arguments);
  const SEXPTYPE operand_type = (SEXPTYPE) TYPEOF(operand);
  if ((operand_type != INTSXP && operand_type != REALSXP) ||
      ALTREP(operand) || Rf_isObject(operand) || Rf_isS4(operand) ||
      !paradox_api_has_no_attributes(operand) || XLENGTH(operand) != 1) {
    return FALSE;
  }
  SEXP canonical = base_function(CHAR(PRINTNAME(head)));
  return canonical != R_UnboundValue &&
    lookup_is(eval_environment, head, canonical);
}

static int ascii_character(SEXP value) {
  if (value == NA_STRING || Rf_getCharCE(value) != CE_NATIVE) {
    return FALSE;
  }
  const unsigned char *bytes = (const unsigned char *) CHAR(value);
  const int size = LENGTH(value);
  for (int index = 0; index < size; ++index) {
    if (bytes[index] == '\0' || bytes[index] >= 0x80) {
      return FALSE;
    }
  }
  return TRUE;
}

static int direct_character_surface(SEXP expression) {
  if (TYPEOF(expression) != STRSXP || ALTREP(expression) ||
      Rf_isObject(expression) || Rf_isS4(expression) ||
      !paradox_api_has_no_attributes(expression)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(expression); ++index) {
    if (!ascii_character(STRING_ELT(expression, index))) {
      return FALSE;
    }
  }
  return TRUE;
}

static int character_literal_surface(SEXP expression,
    SEXP eval_environment) {
  if (direct_character_surface(expression)) {
    return TRUE;
  }
  if (TYPEOF(expression) != LANGSXP || !plain_pairlist(expression)) {
    return FALSE;
  }
  SEXP head = CAR(expression);
  if (TYPEOF(head) != SYMSXP) {
    return FALSE;
  }
  if (string_is(PRINTNAME(head), "character")) {
    if (CDR(expression) != R_NilValue) {
      return FALSE;
    }
    SEXP canonical = base_function("character");
    return canonical != R_UnboundValue &&
      lookup_is(eval_environment, head, canonical);
  }
  if (!string_is(PRINTNAME(head), "c")) {
    return FALSE;
  }
  SEXP canonical = base_function("c");
  if (canonical == R_UnboundValue ||
      !lookup_is(eval_environment, head, canonical)) {
    return FALSE;
  }
  SEXP node = CDR(expression);
  if (node == R_NilValue) {
    return FALSE;
  }
  while (node != R_NilValue) {
    if (TYPEOF(node) != LISTSXP || !plain_pairlist(node) ||
        TAG(node) != R_NilValue) {
      return FALSE;
    }
    SEXP value = CAR(node);
    if (TYPEOF(value) != STRSXP || XLENGTH(value) != 1 ||
        !direct_character_surface(value)) {
      return FALSE;
    }
    node = CDR(node);
  }
  return TRUE;
}

static int value_surfaces_are_literal(
    const struct ps_constructor_plan *plan,
    SEXP eval_environment) {
  if (plan->expressions[PS_VALUE_LOWER] != R_MissingArg &&
      !numeric_literal_surface(
        plan->expressions[PS_VALUE_LOWER],
        eval_environment
      )) {
    return FALSE;
  }
  if (plan->expressions[PS_VALUE_UPPER] != R_MissingArg &&
      !numeric_literal_surface(
        plan->expressions[PS_VALUE_UPPER],
        eval_environment
      )) {
    return FALSE;
  }
  if (plan->expressions[PS_VALUE_TOLERANCE] != R_MissingArg &&
      !numeric_literal_surface(
        plan->expressions[PS_VALUE_TOLERANCE],
        eval_environment
      )) {
    return FALSE;
  }
  if (plan->expressions[PS_VALUE_LEVELS] != R_MissingArg &&
      !character_literal_surface(
        plan->expressions[PS_VALUE_LEVELS],
        eval_environment
      )) {
    return FALSE;
  }
  return plan->expressions[PS_VALUE_TAGS] == R_MissingArg ||
    character_literal_surface(
      plan->expressions[PS_VALUE_TAGS],
      eval_environment
    );
}

static int plan_constructor(SEXP call, SEXP eval_environment,
    struct ps_constructor_plan *plan) {
  if (TYPEOF(call) != LANGSXP || !plain_pairlist(call) ||
      !constructor_kind(CAR(call), &plan->kind)) {
    return FALSE;
  }
  plan->symbol = CAR(call);
  if (!lookup_is(
      eval_environment,
      plan->symbol,
      canonical_constructor(plan->kind)
    )) {
    return FALSE;
  }
  for (int slot = 0; slot < PS_VALUE_SLOT_COUNT; ++slot) {
    plan->expressions[slot] = R_MissingArg;
  }

  const struct ps_constructor_spec *spec = &constructor_specs[plan->kind];
  SEXP matched[sizeof(dbl_formals) / sizeof(dbl_formals[0])];
  if (spec->formal_count > sizeof(matched) / sizeof(matched[0])) {
    Rf_error("Internal error: ps() constructor formal table is too large");
  }
  for (size_t index = 0; index < spec->formal_count; ++index) {
    matched[index] = R_MissingArg;
  }

  SEXP node = CDR(call);
  while (node != R_NilValue) {
    if (TYPEOF(node) != LISTSXP || !plain_pairlist(node)) {
      return FALSE;
    }
    if (TAG(node) != R_NilValue) {
      const int index = formal_index(spec, TAG(node));
      if (index < 0 || spec->slots[index] < 0 ||
          matched[index] != R_MissingArg || CAR(node) == R_MissingArg) {
        return FALSE;
      }
      matched[index] = CAR(node);
    }
    node = CDR(node);
  }

  size_t positional = 0;
  node = CDR(call);
  while (node != R_NilValue) {
    if (TAG(node) == R_NilValue) {
      while (positional < spec->formal_count &&
          matched[positional] != R_MissingArg) {
        ++positional;
      }
      if (positional == spec->formal_count ||
          spec->slots[positional] < 0 || CAR(node) == R_MissingArg) {
        return FALSE;
      }
      matched[positional] = CAR(node);
      ++positional;
    }
    node = CDR(node);
  }

  for (size_t index = 0; index < spec->formal_count; ++index) {
    if (matched[index] != R_MissingArg) {
      plan->expressions[spec->slots[index]] = matched[index];
    }
  }
  if (spec->required_slot >= 0 &&
      plan->expressions[spec->required_slot] == R_MissingArg) {
    return FALSE;
  }
  return value_surfaces_are_literal(plan, eval_environment);
}

static SEXP decode_numeric_literal(SEXP expression) {
  if (TYPEOF(expression) == INTSXP || TYPEOF(expression) == REALSXP) {
    return expression;
  }
  SEXP head = CAR(expression);
  SEXP operand = CAR(CDR(expression));
  const int negative = string_is(PRINTNAME(head), "-");
  if (TYPEOF(operand) == INTSXP) {
    const int value = INTEGER_ELT(operand, 0);
    if (value == NA_INTEGER || (negative && value == INT_MIN)) {
      return R_NilValue;
    }
    return Rf_ScalarInteger(negative ? -value : value);
  }
  const double value = REAL_ELT(operand, 0);
  return Rf_ScalarReal(negative ? -value : value);
}

static SEXP decode_character_literal(SEXP expression) {
  if (TYPEOF(expression) == STRSXP) {
    const R_xlen_t size = XLENGTH(expression);
    SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
    for (R_xlen_t index = 0; index < size; ++index) {
      SET_STRING_ELT(result, index, STRING_ELT(expression, index));
    }
    UNPROTECT(1);
    return result;
  }
  if (string_is(PRINTNAME(CAR(expression)), "character")) {
    return Rf_allocVector(STRSXP, 0);
  }
  R_xlen_t size = 0;
  for (SEXP node = CDR(expression); node != R_NilValue; node = CDR(node)) {
    ++size;
  }
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  R_xlen_t index = 0;
  for (SEXP node = CDR(expression); node != R_NilValue; node = CDR(node)) {
    SET_STRING_ELT(result, index++, STRING_ELT(CAR(node), 0));
  }
  UNPROTECT(1);
  return result;
}

static int scalar_numeric_value(SEXP value, double *number) {
  if (TYPEOF(value) == INTSXP) {
    const int integer = INTEGER_ELT(value, 0);
    if (integer == NA_INTEGER) {
      return FALSE;
    }
    *number = (double) integer;
    return TRUE;
  }
  const double real = REAL_ELT(value, 0);
  if (ISNAN(real)) {
    return FALSE;
  }
  *number = real;
  return TRUE;
}

static int integer_bound(double value) {
  return !R_FINITE(value) ||
    (value >= -(double) INT_MAX && value <= (double) INT_MAX &&
      value == (double) ((int) value));
}

static int unique_strings(SEXP value) {
  return Rf_any_duplicated(value, FALSE) == 0;
}

static SEXP anchor_value(SEXP anchor, R_xlen_t *cursor, SEXP value) {
  if (*cursor >= XLENGTH(anchor)) {
    Rf_error("Internal error: ps() construction root capacity exceeded");
  }
  SET_VECTOR_ELT(anchor, *cursor, value);
  ++*cursor;
  return value;
}

static SEXP scalar_string(const char *value, SEXP anchor,
    R_xlen_t *cursor) {
  SEXP result = PROTECT(Rf_mkString(value));
  anchor_value(anchor, cursor, result);
  UNPROTECT(1);
  return result;
}

static SEXP domain_class(const char *kind, SEXP anchor,
    R_xlen_t *cursor) {
  static const char *const suffix[] = {
    "Domain", "data.table", "data.frame"
  };
  SEXP result = PROTECT(Rf_allocVector(STRSXP, 4));
  SET_STRING_ELT(result, 0, Rf_mkChar(kind));
  for (R_xlen_t index = 0; index < 3; ++index) {
    SET_STRING_ELT(result, index + 1, Rf_mkChar(suffix[index]));
  }
  anchor_value(anchor, cursor, result);
  UNPROTECT(1);
  return result;
}

struct ps_builtin_constants {
  SEXP classes[PS_KIND_COUNT];
  SEXP class_names[PS_KIND_COUNT];
  SEXP storage[PS_KIND_COUNT];
  SEXP no_default;
  SEXP default_tolerance;
  SEXP negative_infinity;
  SEXP positive_infinity;
  SEXP missing_real;
  SEXP false_value;
};

struct ps_builtin_row {
  SEXP lower;
  SEXP upper;
  SEXP tolerance;
  SEXP levels;
  SEXP tags;
  SEXP grouping;
};

static int count_builtin_domains(SEXP expressions, R_xlen_t *count) {
  *count = 0;
  SEXP node = CDR(expressions);
  while (node != R_NilValue) {
    if (TYPEOF(node) != LISTSXP || !plain_pairlist(node) ||
        !strict_ascii_id(TAG(node)) || *count == PS_BUILTIN_MAX_DOMAINS) {
      return FALSE;
    }
    ++*count;
    node = CDR(node);
  }
  return *count != 0;
}

static int plan_builtin_domains(SEXP expressions, SEXP eval_environment,
    SEXP names, struct ps_constructor_plan *plans, R_xlen_t count) {
  SEXP node = CDR(expressions);
  for (R_xlen_t index = 0; index < count; ++index, node = CDR(node)) {
    SET_STRING_ELT(names, index, PRINTNAME(TAG(node)));
    if (!plan_constructor(CAR(node), eval_environment, &plans[index])) {
      return FALSE;
    }
  }
  return Rf_any_duplicated(names, FALSE) == 0;
}

static void initialize_builtin_constants(struct ps_builtin_constants *values,
    SEXP roots, R_xlen_t *root_cursor) {
  values->classes[PS_KIND_DBL] = domain_class(
    "ParamDbl", roots, root_cursor
  );
  values->classes[PS_KIND_INT] = domain_class(
    "ParamInt", roots, root_cursor
  );
  values->classes[PS_KIND_FCT] = domain_class(
    "ParamFct", roots, root_cursor
  );
  values->classes[PS_KIND_LGL] = domain_class(
    "ParamLgl", roots, root_cursor
  );

  values->class_names[PS_KIND_DBL] = scalar_string(
    "ParamDbl", roots, root_cursor
  );
  values->class_names[PS_KIND_INT] = scalar_string(
    "ParamInt", roots, root_cursor
  );
  values->class_names[PS_KIND_FCT] = R_NilValue;
  values->class_names[PS_KIND_LGL] = scalar_string(
    "ParamLgl", roots, root_cursor
  );

  values->storage[PS_KIND_DBL] = scalar_string(
    "numeric", roots, root_cursor
  );
  values->storage[PS_KIND_INT] = scalar_string(
    "integer", roots, root_cursor
  );
  values->storage[PS_KIND_FCT] = scalar_string(
    "character", roots, root_cursor
  );
  values->storage[PS_KIND_LGL] = scalar_string(
    "logical", roots, root_cursor
  );
  values->no_default = VECTOR_ELT(
    ps_runtime_root,
    PS_RUNTIME_NO_DEFAULT
  );

  values->default_tolerance = PROTECT(Rf_ScalarReal(sqrt(DBL_EPSILON)));
  anchor_value(roots, root_cursor, values->default_tolerance);
  UNPROTECT(1);
  values->negative_infinity = PROTECT(Rf_ScalarReal(R_NegInf));
  anchor_value(roots, root_cursor, values->negative_infinity);
  UNPROTECT(1);
  values->positive_infinity = PROTECT(Rf_ScalarReal(R_PosInf));
  anchor_value(roots, root_cursor, values->positive_infinity);
  UNPROTECT(1);
  values->missing_real = PROTECT(Rf_ScalarReal(NA_REAL));
  anchor_value(roots, root_cursor, values->missing_real);
  UNPROTECT(1);
  values->false_value = PROTECT(Rf_ScalarLogical(FALSE));
  anchor_value(roots, root_cursor, values->false_value);
  UNPROTECT(1);
}

static int build_builtin_tags(const struct ps_constructor_plan *plan,
    SEXP roots, R_xlen_t *root_cursor, SEXP *tags) {
  if (plan->expressions[PS_VALUE_TAGS] == R_MissingArg) {
    *tags = PROTECT(Rf_allocVector(STRSXP, 0));
  } else {
    *tags = PROTECT(decode_character_literal(
      plan->expressions[PS_VALUE_TAGS]
    ));
  }
  anchor_value(roots, root_cursor, *tags);
  UNPROTECT(1);
  return unique_strings(*tags);
}

static int build_builtin_numeric(SEXP expression, SEXP fallback,
    SEXP roots, R_xlen_t *root_cursor, SEXP *result) {
  if (expression == R_MissingArg) {
    *result = fallback;
    return TRUE;
  }
  *result = PROTECT(decode_numeric_literal(expression));
  if (*result == R_NilValue) {
    UNPROTECT(1);
    return FALSE;
  }
  anchor_value(roots, root_cursor, *result);
  UNPROTECT(1);
  return TRUE;
}

static int build_builtin_numeric_values(
    const struct ps_constructor_plan *plan,
    const struct ps_builtin_constants *constants,
    SEXP roots, R_xlen_t *root_cursor, struct ps_builtin_row *row) {
  if (!build_builtin_numeric(
      plan->expressions[PS_VALUE_LOWER],
      constants->negative_infinity,
      roots,
      root_cursor,
      &row->lower
    ) || !build_builtin_numeric(
      plan->expressions[PS_VALUE_UPPER],
      constants->positive_infinity,
      roots,
      root_cursor,
      &row->upper
    ) || !build_builtin_numeric(
      plan->expressions[PS_VALUE_TOLERANCE],
      constants->default_tolerance,
      roots,
      root_cursor,
      &row->tolerance
    )) {
    return FALSE;
  }

  double lower_number;
  double upper_number;
  double tolerance_number;
  if (!scalar_numeric_value(row->lower, &lower_number) ||
      !scalar_numeric_value(row->upper, &upper_number) ||
      !scalar_numeric_value(row->tolerance, &tolerance_number) ||
      tolerance_number < 0.0 || lower_number > upper_number) {
    return FALSE;
  }
  return plan->kind != PS_KIND_INT ||
    (tolerance_number <= 0.5 && integer_bound(lower_number) &&
      integer_bound(upper_number));
}

static int build_builtin_levels(const struct ps_constructor_plan *plan,
    SEXP roots, R_xlen_t *root_cursor, SEXP *levels) {
  if (plan->kind == PS_KIND_FCT) {
    *levels = PROTECT(decode_character_literal(
      plan->expressions[PS_VALUE_LEVELS]
    ));
    anchor_value(roots, root_cursor, *levels);
    UNPROTECT(1);
    return unique_strings(*levels);
  }
  if (plan->kind == PS_KIND_LGL) {
    *levels = PROTECT(Rf_allocVector(LGLSXP, 2));
    SET_LOGICAL_ELT(*levels, 0, TRUE);
    SET_LOGICAL_ELT(*levels, 1, FALSE);
    anchor_value(roots, root_cursor, *levels);
    UNPROTECT(1);
  }
  return TRUE;
}

static int build_builtin_grouping(const struct ps_constructor_plan *plan,
    const struct ps_builtin_constants *constants,
    SEXP roots, R_xlen_t *root_cursor, SEXP levels, SEXP *grouping) {
  if (plan->kind != PS_KIND_FCT) {
    *grouping = constants->class_names[plan->kind];
    return TRUE;
  }

  SEXP sort_call = PROTECT(Rf_lang2(Rf_install("sort"), levels));
  SEXP sorted = PROTECT(Rf_eval(sort_call, R_BaseEnv));
  *grouping = PROTECT(paradox_domain_fct_grouping(sorted));
  if (*grouping == R_NilValue) {
    UNPROTECT(3);
    return FALSE;
  }
  anchor_value(roots, root_cursor, *grouping);
  UNPROTECT(3);
  return TRUE;
}

static void initialize_builtin_row(const struct ps_builtin_constants *constants,
    struct ps_builtin_row *row) {
  row->lower = constants->missing_real;
  row->upper = constants->missing_real;
  row->tolerance = constants->missing_real;
  row->levels = R_NilValue;
  row->tags = R_NilValue;
  row->grouping = R_NilValue;
}

static int construct_builtin_row(const struct ps_constructor_plan *plan,
    const struct ps_builtin_constants *constants,
    SEXP eval_environment, SEXP roots, R_xlen_t *root_cursor,
    SEXP names, R_xlen_t index, SEXP result) {
  /* Recheck immediately before allocating this row.  A GC finalizer may
   * have changed a later lookup after the allocation-only preflight. */
  if (!lookup_is(
      eval_environment,
      plan->symbol,
      canonical_constructor(plan->kind)
    )) {
    return FALSE;
  }

  struct ps_builtin_row row;
  initialize_builtin_row(constants, &row);
  if (!build_builtin_tags(plan, roots, root_cursor, &row.tags)) {
    return FALSE;
  }
  if ((plan->kind == PS_KIND_DBL || plan->kind == PS_KIND_INT) &&
      !build_builtin_numeric_values(
        plan,
        constants,
        roots,
        root_cursor,
        &row
      )) {
    return FALSE;
  }
  if (!build_builtin_levels(plan, roots, root_cursor, &row.levels) ||
      !build_builtin_grouping(
        plan,
        constants,
        roots,
        root_cursor,
        row.levels,
        &row.grouping
      )) {
    return FALSE;
  }

  SEXP special_values = PROTECT(Rf_allocVector(VECSXP, 0));
  anchor_value(roots, root_cursor, special_values);
  SEXP cls = constants->class_names[plan->kind];
  if (plan->kind == PS_KIND_FCT) {
    cls = scalar_string("ParamFct", roots, root_cursor);
  }
  PROTECT(cls);

  SEXP domain = PROTECT(paradox_domain_construct(
    cls,
    row.grouping,
    R_NilValue,
    row.lower,
    row.upper,
    row.tolerance,
    row.levels,
    special_values,
    constants->no_default,
    row.tags,
    R_NilValue,
    constants->storage[plan->kind],
    constants->false_value,
    R_NilValue
  ));
  if (domain == R_NilValue) {
    UNPROTECT(3);
    return FALSE;
  }
  SET_STRING_ELT(VECTOR_ELT(domain, 0), 0, STRING_ELT(names, index));
  Rf_setAttrib(domain, R_ClassSymbol, constants->classes[plan->kind]);
  SET_VECTOR_ELT(result, index, domain);
  UNPROTECT(3);
  return TRUE;
}

SEXP paradox_ps_builtin_domains(SEXP expressions, SEXP eval_environment) {
  if (ps_runtime_root == NULL || TYPEOF(eval_environment) != ENVSXP ||
      TYPEOF(expressions) != LANGSXP || !plain_pairlist(expressions) ||
      TYPEOF(CAR(expressions)) != SYMSXP ||
      !string_is(PRINTNAME(CAR(expressions)), "list")) {
    return R_NilValue;
  }

  R_xlen_t count;
  if (!count_builtin_domains(expressions, &count)) {
    return R_NilValue;
  }

  SEXP names = PROTECT(Rf_allocVector(STRSXP, count));
  struct ps_constructor_plan *plans = paradox_temporary_alloc(
    count,
    sizeof(*plans)
  );
  if (!plan_builtin_domains(
      expressions,
      eval_environment,
      names,
      plans,
      count
    )) {
    UNPROTECT(1);
    return R_NilValue;
  }

  if (count > (R_XLEN_T_MAX - 32) / (PS_VALUE_SLOT_COUNT + 8)) {
    UNPROTECT(1);
    return R_NilValue;
  }
  const R_xlen_t root_capacity =
    count * (PS_VALUE_SLOT_COUNT + 8) + 32;
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, root_capacity));
  R_xlen_t root_cursor = 0;
  SEXP result = PROTECT(Rf_allocVector(VECSXP, count));

  struct ps_builtin_constants constants;
  initialize_builtin_constants(&constants, roots, &root_cursor);

  for (R_xlen_t index = 0; index < count; ++index) {
    if (!construct_builtin_row(
        &plans[index],
        &constants,
        eval_environment,
        roots,
        &root_cursor,
        names,
        index,
        result
      )) {
      UNPROTECT(3);
      return R_NilValue;
    }
  }

  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(3);
  return result;
}
