#include <stddef.h>
#include <string.h>

#include "paradox.h"

#include "paramset_bulk_shell_internal.h"
#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

/* The canonical object is constructed while the namespace and its R6
 * generator are still pristine.  It is never returned to R: it is a graph
 * schema, not a reusable instance. */
static SEXP canonical_prototype = NULL;
static SEXP canonical_generator_snapshot = NULL;

enum generator_snapshot_slot {
  GENERATOR_ENVIRONMENT = 0,
  GENERATOR_PARENT,
  GENERATOR_NAMES,
  GENERATOR_SYMBOLS,
  GENERATOR_VALUES,
  GENERATOR_LIST_SNAPSHOTS,
  GENERATOR_BINDING_TYPES,
  GENERATOR_BINDING_LOCKS,
  GENERATOR_NAME_ATTRIBUTE,
  GENERATOR_CLASS_ATTRIBUTE,
  GENERATOR_SNAPSHOT_SIZE
};

static void release_preserved(SEXP *object) {
  if (*object != NULL) {
    R_ReleaseObject(*object);
    *object = NULL;
  }
}

void paradox_param_set_bulk_shell_release(void) {
  release_preserved(&canonical_generator_snapshot);
  release_preserved(&canonical_prototype);
}

#if R_VERSION >= R_Version(4, 6, 0)

static SEXP owned_list_snapshot(SEXP source) {
  if (TYPEOF(source) != VECSXP || ALTREP(source)) {
    return R_NilValue;
  }
  SEXP result = PROTECT(Rf_allocVector(VECSXP, XLENGTH(source)));
  for (R_xlen_t index = 0; index < XLENGTH(source); ++index) {
    SEXP value = VECTOR_ELT(source, index);
    if (TYPEOF(value) == VECSXP) {
      SEXP child = PROTECT(owned_list_snapshot(value));
      if (child == R_NilValue) {
        UNPROTECT(2);
        return R_NilValue;
      }
      SET_VECTOR_ELT(result, index, child);
      UNPROTECT(1);
    } else if (Rf_isVectorAtomic(value)) {
      SEXP child = PROTECT(Rf_duplicate(value));
      SET_VECTOR_ELT(result, index, child);
      UNPROTECT(1);
    } else {
      SET_VECTOR_ELT(result, index, value);
    }
  }
  DUPLICATE_ATTRIB(result, source);
  UNPROTECT(1);
  return result;
}

static int snapshot_value_matches(SEXP value, SEXP snapshot);

static int snapshot_attributes_match(SEXP value, SEXP snapshot) {
  static const char *const attribute_names[] = {
    "names", "class", "row.names", ".internal.selfref", "sorted", "index"
  };
  const R_xlen_t expected_count = R_getAttribCount(snapshot);
  if (R_getAttribCount(value) != expected_count) {
    return FALSE;
  }
  R_xlen_t matched = 0;
  for (size_t index = 0;
      index < sizeof(attribute_names) / sizeof(attribute_names[0]); ++index) {
    SEXP symbol = Rf_install(attribute_names[index]);
    const int expected = R_hasAttrib(snapshot, symbol);
    if (expected != R_hasAttrib(value, symbol)) {
      return FALSE;
    }
    if (expected) {
      ++matched;
      SEXP value_attribute = PROTECT(Rf_getAttrib(value, symbol));
      SEXP snapshot_attribute = PROTECT(Rf_getAttrib(snapshot, symbol));
      const int attributes_match = snapshot_value_matches(
        value_attribute,
        snapshot_attribute
      );
      UNPROTECT(2);
      if (!attributes_match) {
        return FALSE;
      }
    }
  }
  return matched == expected_count;
}

static int snapshot_value_matches(SEXP value, SEXP snapshot) {
  if (TYPEOF(value) != TYPEOF(snapshot)) {
    return FALSE;
  }
  const int type = TYPEOF(value);
  if (type == VECSXP) {
    if (ALTREP(value) || ALTREP(snapshot) ||
        XLENGTH(value) != XLENGTH(snapshot)) {
      return FALSE;
    }
    for (R_xlen_t index = 0; index < XLENGTH(value); ++index) {
      if (!snapshot_value_matches(
          VECTOR_ELT(value, index),
          VECTOR_ELT(snapshot, index)
        )) {
        return FALSE;
      }
    }
    return snapshot_attributes_match(value, snapshot);
  }
  if (type == STRSXP) {
    if (ALTREP(value) || ALTREP(snapshot) ||
        XLENGTH(value) != XLENGTH(snapshot)) {
      return FALSE;
    }
    for (R_xlen_t index = 0; index < XLENGTH(value); ++index) {
      if (STRING_ELT(value, index) != STRING_ELT(snapshot, index)) {
        return FALSE;
      }
    }
    return snapshot_attributes_match(value, snapshot);
  }
  if (type == INTSXP || type == LGLSXP) {
    if (ALTREP(value) || ALTREP(snapshot) ||
        XLENGTH(value) != XLENGTH(snapshot)) {
      return FALSE;
    }
    const int *value_data = type == INTSXP ? INTEGER(value) : LOGICAL(value);
    const int *snapshot_data = type == INTSXP
      ? INTEGER(snapshot)
      : LOGICAL(snapshot);
    return memcmp(
        value_data,
        snapshot_data,
        (size_t) XLENGTH(value) * sizeof(int)
      ) == 0 && snapshot_attributes_match(value, snapshot);
  }
  if (type == REALSXP) {
    return !ALTREP(value) && !ALTREP(snapshot) &&
      XLENGTH(value) == XLENGTH(snapshot) &&
      memcmp(
        REAL(value),
        REAL(snapshot),
        (size_t) XLENGTH(value) * sizeof(double)
      ) == 0 && snapshot_attributes_match(value, snapshot);
  }
  if (type == CPLXSXP) {
    return !ALTREP(value) && !ALTREP(snapshot) &&
      XLENGTH(value) == XLENGTH(snapshot) &&
      memcmp(
        COMPLEX(value),
        COMPLEX(snapshot),
        (size_t) XLENGTH(value) * sizeof(Rcomplex)
      ) == 0 && snapshot_attributes_match(value, snapshot);
  }
  if (type == RAWSXP) {
    return !ALTREP(value) && !ALTREP(snapshot) &&
      XLENGTH(value) == XLENGTH(snapshot) &&
      memcmp(
        RAW(value),
        RAW(snapshot),
        (size_t) XLENGTH(value)
      ) == 0 && snapshot_attributes_match(value, snapshot);
  }
  return value == snapshot;
}

static int exact_generator_attribute(SEXP generator, SEXP snapshot,
    const char *name) {
  SEXP symbol = Rf_install(name);
  if (!R_hasAttrib(generator, symbol)) {
    return FALSE;
  }
  SEXP value = PROTECT(Rf_getAttrib(generator, symbol));
  const int matches = snapshot_value_matches(value, snapshot);
  UNPROTECT(1);
  return matches;
}

SEXP paradox_r6_generator_snapshot(SEXP generator) {
  static const char *const generator_attributes[] = {"name", "class"};
  if (TYPEOF(generator) != ENVSXP || R_EnvironmentIsLocked(generator) ||
      !paradox_api_has_only_attributes(
        generator,
        generator_attributes,
        2
      )) {
    return R_NilValue;
  }
  SEXP names = PROTECT(R_lsInternal3(generator, TRUE, TRUE));
  const R_xlen_t count = XLENGTH(names);
  SEXP root = PROTECT(Rf_allocVector(VECSXP, GENERATOR_SNAPSHOT_SIZE));
  SEXP symbols = PROTECT(Rf_allocVector(VECSXP, count));
  SEXP values = PROTECT(Rf_allocVector(VECSXP, count));
  SEXP lists = PROTECT(Rf_allocVector(VECSXP, count));
  SEXP types = PROTECT(Rf_allocVector(INTSXP, count));
  SEXP locks = PROTECT(Rf_allocVector(LGLSXP, count));
  SEXP name_attribute = PROTECT(Rf_duplicate(Rf_getAttrib(
    generator,
    Rf_install("name")
  )));
  SEXP class_attribute = PROTECT(Rf_duplicate(Rf_getAttrib(
    generator,
    R_ClassSymbol
  )));
  SET_VECTOR_ELT(root, GENERATOR_ENVIRONMENT, generator);
  SET_VECTOR_ELT(root, GENERATOR_PARENT, paradox_api_parent_environment(generator));
  SET_VECTOR_ELT(root, GENERATOR_NAMES, names);
  SET_VECTOR_ELT(root, GENERATOR_SYMBOLS, symbols);
  SET_VECTOR_ELT(root, GENERATOR_VALUES, values);
  SET_VECTOR_ELT(root, GENERATOR_LIST_SNAPSHOTS, lists);
  SET_VECTOR_ELT(root, GENERATOR_BINDING_TYPES, types);
  SET_VECTOR_ELT(root, GENERATOR_BINDING_LOCKS, locks);
  SET_VECTOR_ELT(root, GENERATOR_NAME_ATTRIBUTE, name_attribute);
  SET_VECTOR_ELT(root, GENERATOR_CLASS_ATTRIBUTE, class_attribute);

  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP symbol = Rf_installChar(STRING_ELT(names, index));
    SET_VECTOR_ELT(symbols, index, symbol);
    const R_BindingType_t type = R_GetBindingType(symbol, generator);
    if (type != R_BindingTypeValue) {
      UNPROTECT(9);
      return R_NilValue;
    }
    SEXP value = paradox_api_local_value(generator, symbol);
    if (value == R_UnboundValue) {
      UNPROTECT(9);
      return R_NilValue;
    }
    SET_VECTOR_ELT(values, index, value);
    INTEGER(types)[index] = (int) type;
    LOGICAL(locks)[index] = R_BindingIsLocked(symbol, generator);
    if (TYPEOF(value) == VECSXP) {
      SEXP snapshot = PROTECT(owned_list_snapshot(value));
      if (snapshot == R_NilValue || !snapshot_value_matches(value, snapshot)) {
        UNPROTECT(10);
        return R_NilValue;
      }
      SET_VECTOR_ELT(lists, index, snapshot);
      UNPROTECT(1);
    }
  }
  UNPROTECT(9);
  return root;
}

int paradox_r6_generator_matches(SEXP generator, SEXP snapshot,
    SEXP names) {
  if (snapshot == NULL || TYPEOF(generator) != ENVSXP ||
      generator != VECTOR_ELT(snapshot, GENERATOR_ENVIRONMENT) ||
      R_EnvironmentIsLocked(generator) ||
      paradox_api_parent_environment(generator) !=
        VECTOR_ELT(snapshot, GENERATOR_PARENT) ||
      R_getAttribCount(generator) != 2 ||
      !exact_generator_attribute(
        generator,
        VECTOR_ELT(snapshot, GENERATOR_NAME_ATTRIBUTE),
        "name"
      ) || !exact_generator_attribute(
        generator,
        VECTOR_ELT(snapshot, GENERATOR_CLASS_ATTRIBUTE),
        "class"
      )) {
    return FALSE;
  }
  SEXP expected_names = VECTOR_ELT(snapshot, GENERATOR_NAMES);
  if (!snapshot_value_matches(names, expected_names)) {
    return FALSE;
  }
  SEXP values = VECTOR_ELT(snapshot, GENERATOR_VALUES);
  SEXP symbols = VECTOR_ELT(snapshot, GENERATOR_SYMBOLS);
  SEXP lists = VECTOR_ELT(snapshot, GENERATOR_LIST_SNAPSHOTS);
  SEXP types = VECTOR_ELT(snapshot, GENERATOR_BINDING_TYPES);
  SEXP locks = VECTOR_ELT(snapshot, GENERATOR_BINDING_LOCKS);
  for (R_xlen_t index = 0; index < XLENGTH(names); ++index) {
    SEXP symbol = VECTOR_ELT(symbols, index);
    const R_BindingType_t binding_type = R_GetBindingType(
      symbol,
      generator
    );
    if (binding_type != (R_BindingType_t) INTEGER(types)[index] ||
        R_BindingIsLocked(symbol, generator) != LOGICAL(locks)[index]) {
      return FALSE;
    }
    SEXP value = PROTECT(paradox_api_local_value(generator, symbol));
    SEXP expected = VECTOR_ELT(values, index);
    if ((expected == VECTOR_ELT(snapshot, GENERATOR_ENVIRONMENT)
          ? value != generator
          : value != expected) ||
        (TYPEOF(expected) == VECSXP && !snapshot_value_matches(
          value,
          VECTOR_ELT(lists, index)
        ))) {
      UNPROTECT(1);
      return FALSE;
    }
    UNPROTECT(1);
  }
  return TRUE;
}

int paradox_param_set_bulk_generator_matches(SEXP generator, SEXP names) {
  return paradox_r6_generator_matches(
    generator,
    canonical_generator_snapshot,
    names
  );
}

#endif

SEXP paradox_param_set_bulk_shell_register(SEXP prototype,
    SEXP generator) {
  if (canonical_prototype != NULL || canonical_generator_snapshot != NULL ||
      TYPEOF(prototype) != ENVSXP || TYPEOF(generator) != ENVSXP) {
    return Rf_ScalarLogical(FALSE);
  }

#if R_VERSION >= R_Version(4, 6, 0)
  SEXP snapshot = PROTECT(paradox_r6_generator_snapshot(generator));
  if (snapshot == R_NilValue) {
    UNPROTECT(1);
    return Rf_ScalarLogical(FALSE);
  }
#else
  SEXP snapshot = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(snapshot, 0, generator);
#endif
  R_PreserveObject(prototype);
  canonical_prototype = prototype;
  R_PreserveObject(snapshot);
  canonical_generator_snapshot = snapshot;
  UNPROTECT(1);
  return Rf_ScalarLogical(TRUE);
}

SEXP paradox_param_set_bulk_generator_auth(SEXP generator) {
#if R_VERSION >= R_Version(4, 6, 0)
  if (TYPEOF(generator) != ENVSXP) {
    return Rf_ScalarLogical(FALSE);
  }
  SEXP names = PROTECT(R_lsInternal3(generator, TRUE, TRUE));
  const int exact = paradox_param_set_bulk_generator_matches(generator, names);
  UNPROTECT(1);
  return Rf_ScalarLogical(exact);
#else
  (void) generator;
  return Rf_ScalarLogical(FALSE);
#endif
}

#if R_VERSION >= R_Version(4, 6, 0)

typedef struct {
  SEXP self;
  SEXP enclosure;
  SEXP private_environment;
  SEXP active_registry;
} param_set_graph_t;

static int exact_param_set_classes(SEXP object) {
  static const char *const classes[] = {"ParamSet", "R6"};
  R_xlen_t work_since_interrupt = 0;
  return paradox_api_has_single_attribute(object, "class") &&
    paradox_domain_exact_string_vector(
      Rf_getAttrib(object, R_ClassSymbol),
      classes,
      2,
      &work_since_interrupt
    );
}

static int load_graph(SEXP self, param_set_graph_t *graph) {
  if (TYPEOF(self) != ENVSXP || !exact_param_set_classes(self) ||
      paradox_api_parent_environment(self) != R_EmptyEnv ||
      !R_EnvironmentIsLocked(self)) {
    return FALSE;
  }

  graph->self = self;
  graph->enclosure = paradox_api_local_value(
    self,
    Rf_install(".__enclos_env__")
  );
  if (TYPEOF(graph->enclosure) != ENVSXP ||
      R_EnvironmentIsLocked(graph->enclosure) ||
      paradox_api_parent_environment(graph->enclosure) !=
        paradox_api_registered_namespace("paradox") ||
      paradox_api_local_value(graph->enclosure, Rf_install("self")) !=
        self) {
    return FALSE;
  }

  graph->private_environment = paradox_api_local_value(
    graph->enclosure,
    Rf_install("private")
  );
  graph->active_registry = paradox_api_local_value(
    graph->enclosure,
    Rf_install(".__active__")
  );
  return TYPEOF(graph->private_environment) == ENVSXP &&
    paradox_api_parent_environment(graph->private_environment) == R_EmptyEnv &&
    R_EnvironmentIsLocked(graph->private_environment) &&
    TYPEOF(graph->active_registry) == VECSXP &&
    !ALTREP(graph->active_registry);
}

static R_xlen_t name_position(SEXP names, SEXP name) {
  for (R_xlen_t index = 0; index < XLENGTH(names); ++index) {
    if (STRING_ELT(names, index) == name) {
      return index;
    }
  }
  return R_XLEN_T_MAX;
}

static SEXP clone_closure(SEXP source, SEXP enclosure) {
  SEXP duplicated = PROTECT(Rf_duplicate(source));
  SEXP result = PROTECT(R_mkClosure(
    paradox_api_closure_formals(duplicated),
    paradox_api_closure_expression(duplicated),
    enclosure
  ));
  SHALLOW_DUPLICATE_ATTRIB(result, duplicated);
  UNPROTECT(2);
  return result;
}

static int private_state_field(SEXP name) {
  static const char *const fields[] = {
    ".params", ".tags", ".trafos", ".deps", ".values",
    ".extra_trafo", ".constraint"
  };
  for (size_t index = 0; index < sizeof(fields) / sizeof(fields[0]); ++index) {
    if (strcmp(CHAR(name), fields[index]) == 0) {
      return TRUE;
    }
  }
  return FALSE;
}

static void define_with_lock(SEXP environment, SEXP symbol, SEXP value,
    int locked) {
  Rf_defineVar(symbol, value, environment);
  if (locked) {
    R_LockBinding(symbol, environment);
  }
}

static int build_shell(const param_set_graph_t *source,
    param_set_graph_t *result) {
  SEXP namespace_environment = paradox_api_registered_namespace("paradox");
  result->self = PROTECT(R_NewEnv(R_EmptyEnv, FALSE, 0));
  result->enclosure = PROTECT(R_NewEnv(namespace_environment, FALSE, 0));
  result->private_environment = PROTECT(R_NewEnv(R_EmptyEnv, FALSE, 0));

  SEXP active_names = PROTECT(Rf_getAttrib(
    source->active_registry,
    R_NamesSymbol
  ));
  result->active_registry = PROTECT(Rf_allocVector(
    VECSXP,
    XLENGTH(source->active_registry)
  ));
  SEXP active_names_copy = PROTECT(Rf_duplicate(active_names));
  Rf_setAttrib(result->active_registry, R_NamesSymbol, active_names_copy);

  SEXP self_names = PROTECT(R_lsInternal3(source->self, TRUE, FALSE));
  for (R_xlen_t remaining = XLENGTH(self_names); remaining > 0; --remaining) {
    const R_xlen_t index = remaining - 1;
    SEXP name = STRING_ELT(self_names, index);
    SEXP symbol = Rf_installChar(name);
    const R_BindingType_t type = R_GetBindingType(symbol, source->self);
    if (type == R_BindingTypeActive) {
      SEXP source_closure = PROTECT(R_ActiveBindingFunction(
        symbol,
        source->self
      ));
      SEXP closure = PROTECT(clone_closure(
        source_closure,
        result->enclosure
      ));
      R_MakeActiveBinding(symbol, closure, result->self);
      const R_xlen_t position = name_position(active_names, name);
      if (position == R_XLEN_T_MAX) {
        UNPROTECT(9);
        return FALSE;
      }
      SET_VECTOR_ELT(result->active_registry, position, closure);
      UNPROTECT(2);
      continue;
    }

    SEXP source_value = PROTECT(paradox_api_local_value(
      source->self,
      symbol
    ));
    SEXP value;
    if (source_value == source->enclosure) {
      value = PROTECT(result->enclosure);
    } else if (TYPEOF(source_value) == CLOSXP) {
      value = PROTECT(clone_closure(source_value, result->enclosure));
    } else {
      value = PROTECT(Rf_duplicate(source_value));
    }
    define_with_lock(
      result->self,
      symbol,
      value,
      R_BindingIsLocked(symbol, source->self)
    );
    UNPROTECT(2);
  }

  SEXP private_names = PROTECT(R_lsInternal3(
    source->private_environment,
    TRUE,
    FALSE
  ));
  for (R_xlen_t remaining = XLENGTH(private_names);
      remaining > 0; --remaining) {
    const R_xlen_t index = remaining - 1;
    SEXP name = STRING_ELT(private_names, index);
    SEXP symbol = Rf_installChar(name);
    SEXP source_value = PROTECT(paradox_api_local_value(
      source->private_environment,
      symbol
    ));
    SEXP value;
    if (TYPEOF(source_value) == CLOSXP) {
      value = PROTECT(clone_closure(source_value, result->enclosure));
    } else if (private_state_field(name)) {
      value = PROTECT(R_NilValue);
    } else {
      UNPROTECT(9);
      return FALSE;
    }
    define_with_lock(
      result->private_environment,
      symbol,
      value,
      R_BindingIsLocked(symbol, source->private_environment)
    );
    UNPROTECT(2);
  }

  define_with_lock(
    result->enclosure,
    Rf_install("self"),
    result->self,
    FALSE
  );
  define_with_lock(
    result->enclosure,
    Rf_install("private"),
    result->private_environment,
    FALSE
  );
  define_with_lock(
    result->enclosure,
    Rf_install(".__active__"),
    result->active_registry,
    FALSE
  );
  SEXP classes = PROTECT(Rf_duplicate(Rf_getAttrib(
    source->self,
    R_ClassSymbol
  )));
  Rf_setAttrib(result->self, R_ClassSymbol, classes);
  UNPROTECT(9);
  return TRUE;
}

static int exact_plan(SEXP plan, SEXP *token) {
  static const char *const expected_names[] = {
    "missing_parents", "state"
  };
  if (TYPEOF(plan) != VECSXP || ALTREP(plan) || XLENGTH(plan) != 2 ||
      !paradox_api_has_single_attribute(plan, "names")) {
    return FALSE;
  }
  SEXP names = Rf_getAttrib(plan, R_NamesSymbol);
  R_xlen_t work_since_interrupt = 0;
  SEXP missing = VECTOR_ELT(plan, 0);
  SEXP state = VECTOR_ELT(plan, 1);
  if (!paradox_domain_exact_string_vector(
        names,
        expected_names,
        2,
        &work_since_interrupt
      ) || TYPEOF(missing) != STRSXP || ALTREP(missing) ||
      XLENGTH(missing) != 0 || !paradox_api_has_no_attributes(missing) ||
      !paradox_param_set_subset_state_is_singleton(state)) {
    return FALSE;
  }
  *token = state;
  return TRUE;
}

SEXP paradox_param_set_bulk_generator_names(SEXP generator) {
  if (TYPEOF(generator) != ENVSXP) {
    return R_NilValue;
  }
  return R_lsInternal3(generator, TRUE, TRUE);
}

SEXP paradox_param_set_bulk_prepare(SEXP generator, SEXP plans,
    R_xlen_t *count) {
  if (canonical_prototype == NULL || TYPEOF(generator) != ENVSXP ||
      TYPEOF(plans) != VECSXP || ALTREP(plans) ||
      !paradox_api_has_only_attributes(
        plans,
        (const char *const[]) {"names"},
        1
      )) {
    return R_NilValue;
  }
  SEXP generator_names = PROTECT(
    paradox_param_set_bulk_generator_names(generator)
  );
  const int generator_admitted =
    paradox_param_set_bulk_generator_matches(generator, generator_names);
  UNPROTECT(1);
  if (!generator_admitted) {
    return R_NilValue;
  }

  *count = XLENGTH(plans);
  SEXP plan_names = PROTECT(Rf_getAttrib(plans, R_NamesSymbol));
  if (TYPEOF(plan_names) != STRSXP || ALTREP(plan_names) ||
      XLENGTH(plan_names) != *count ||
      !paradox_api_has_no_attributes(plan_names)) {
    UNPROTECT(1);
    return R_NilValue;
  }
  SEXP tokens = PROTECT(Rf_allocVector(VECSXP, *count));
  SEXP names = PROTECT(Rf_duplicate(plan_names));
  Rf_setAttrib(tokens, R_NamesSymbol, names);
  for (R_xlen_t index = 0; index < *count; ++index) {
    SEXP token = R_NilValue;
    if (!exact_plan(VECTOR_ELT(plans, index), &token)) {
      UNPROTECT(3);
      return R_NilValue;
    }
    SET_VECTOR_ELT(tokens, index, token);
    for (R_xlen_t earlier = 0; earlier < index; ++earlier) {
      if (VECTOR_ELT(tokens, earlier) == token) {
        UNPROTECT(3);
        return R_NilValue;
      }
    }
  }
  UNPROTECT(3);
  return tokens;
}

int paradox_param_set_bulk_build_shells(SEXP result,
    SEXP private_environments, R_xlen_t count) {
  if (TYPEOF(result) != VECSXP || XLENGTH(result) != count ||
      TYPEOF(private_environments) != VECSXP ||
      XLENGTH(private_environments) != count) {
    return FALSE;
  }
  param_set_graph_t canonical_graph;
  if (!load_graph(canonical_prototype, &canonical_graph)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < count; ++index) {
    param_set_graph_t shell;
    if (!build_shell(&canonical_graph, &shell)) {
      return FALSE;
    }
    SET_VECTOR_ELT(result, index, shell.self);
    SET_VECTOR_ELT(
      private_environments,
      index,
      shell.private_environment
    );
  }
  return TRUE;
}

int paradox_param_set_bulk_tokens_match(SEXP tokens, int sampler_safe) {
  if (TYPEOF(tokens) != VECSXP || ALTREP(tokens)) {
    return FALSE;
  }
  const R_xlen_t count = XLENGTH(tokens);
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP token = VECTOR_ELT(tokens, index);
    const int exact = sampler_safe
      ? paradox_param_set_subset_state_is_sampler_safe(token)
      : paradox_param_set_subset_state_is_singleton(token);
    if (!exact) {
      return FALSE;
    }
  }
  return TRUE;
}

int paradox_param_set_bulk_destinations_ready(SEXP result,
    SEXP private_environments, R_xlen_t count) {
  if (TYPEOF(result) != VECSXP || XLENGTH(result) != count ||
      TYPEOF(private_environments) != VECSXP ||
      XLENGTH(private_environments) != count) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < count; ++index) {
    if (!paradox_param_set_subset_destination_ready(
        VECTOR_ELT(private_environments, index)
      )) {
      return FALSE;
    }
  }
  return TRUE;
}

int paradox_param_set_bulk_commit(SEXP result, SEXP private_environments,
    SEXP tokens) {
  if (TYPEOF(tokens) != VECSXP || ALTREP(tokens)) {
    return FALSE;
  }
  const R_xlen_t count = XLENGTH(tokens);
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP private_environment = VECTOR_ELT(private_environments, index);
    if (!paradox_param_set_adopt_subset_state_internal(
        private_environment,
        VECTOR_ELT(tokens, index)
      )) {
      return FALSE;
    }
    R_LockEnvironment(private_environment, FALSE);
    R_LockEnvironment(VECTOR_ELT(result, index), FALSE);
  }
  return TRUE;
}

SEXP paradox_param_set_bulk_shells(SEXP generator, SEXP plans) {
  R_xlen_t count = 0;
  SEXP tokens = PROTECT(paradox_param_set_bulk_prepare(
      generator,
      plans,
      &count
    ));
  if (tokens == R_NilValue) {
    UNPROTECT(1);
    return R_NilValue;
  }
  if (count == 0) {
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 0));
    SEXP names = PROTECT(Rf_getAttrib(tokens, R_NamesSymbol));
    if (names != R_NilValue) {
      Rf_setAttrib(result, R_NamesSymbol, names);
    }
    UNPROTECT(3);
    return result;
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, count));
  SEXP private_environments = PROTECT(Rf_allocVector(VECSXP, count));
  if (!paradox_param_set_bulk_build_shells(
      result,
      private_environments,
      count
    )) {
    UNPROTECT(3);
    return R_NilValue;
  }

  SEXP names = PROTECT(Rf_getAttrib(tokens, R_NamesSymbol));
  if (names != R_NilValue) {
    Rf_setAttrib(result, R_NamesSymbol, names);
  }

  /* Allocating either shells or result attributes can run pending finalizers.
   * The fresh name inventory is the final allocation; everything below is an
   * inert audit or a no-allocation state handoff. */
  SEXP final_generator_names = PROTECT(
    paradox_param_set_bulk_generator_names(generator)
  );
  if (!paradox_param_set_bulk_generator_matches(
        generator,
        final_generator_names
      ) || !paradox_param_set_bulk_tokens_match(tokens, FALSE) ||
      !paradox_param_set_bulk_destinations_ready(
        result,
        private_environments,
        count
      )) {
    UNPROTECT(5);
    return R_NilValue;
  }

  /* No allocations or callbacks occur after the first token is consumed.
   * Exact token, graph, and owner preflight above therefore makes the handoff
   * atomic for the complete batch. */
  if (!paradox_param_set_bulk_commit(
      result,
      private_environments,
      tokens
    )) {
    UNPROTECT(5);
    Rf_error("Internal error: preflighted ParamSet state was not adoptable");
  }
  UNPROTECT(5);
  return result;
}

#else

SEXP paradox_param_set_bulk_shells(SEXP generator, SEXP plans) {
  (void) generator;
  (void) plans;
  return R_NilValue;
}

#endif
