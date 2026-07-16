#include <stddef.h>
#include <string.h>

#include "paradox.h"

#include "paramset_bulk_shell_internal.h"
#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

/* This is intentionally a specialized copier for the package-owned
 * Sampler1DUnif -> Sampler1D -> Sampler graph.  It is not an R6 cloning API. */
#define SAMPLER_SLICE_COUNT 3
#define SAMPLER_GENERATOR_COUNT 4
#define SAMPLER_CLOSURE_LIMIT 64

static SEXP canonical_sampler_prototype = NULL;
static SEXP canonical_sampler_schema = NULL;
static SEXP canonical_sampler_generators = NULL;
static SEXP canonical_r6_capsule = NULL;
static SEXP canonical_namespace_targets = NULL;

static void release_preserved(SEXP *object) {
  if (*object != NULL) {
    R_ReleaseObject(*object);
    *object = NULL;
  }
}

void paradox_sampler_1d_unif_bulk_release(void) {
  release_preserved(&canonical_namespace_targets);
  release_preserved(&canonical_r6_capsule);
  release_preserved(&canonical_sampler_generators);
  release_preserved(&canonical_sampler_schema);
  release_preserved(&canonical_sampler_prototype);
}

#if R_VERSION >= R_Version(4, 6, 0)

enum sampler_schema_slot {
  SAMPLER_SELF_NAMES_FIRST = 0,
  SAMPLER_ENCLOSURE_NAMES_FIRST = SAMPLER_SLICE_COUNT,
  SAMPLER_PRIVATE_NAMES = 2 * SAMPLER_SLICE_COUNT,
  SAMPLER_SCHEMA_SIZE
};

enum capsule_snapshot_slot {
  CAPSULE_ENVIRONMENT = 0,
  CAPSULE_PARENT,
  CAPSULE_NAME_ATTRIBUTE,
  CAPSULE_NAMES,
  CAPSULE_SYMBOLS,
  CAPSULE_VALUES,
  CAPSULE_SNAPSHOT_SIZE
};

enum namespace_snapshot_slot {
  NAMESPACE_ENVIRONMENT = 0,
  NAMESPACE_SYMBOLS,
  NAMESPACE_VALUES,
  NAMESPACE_TYPES,
  NAMESPACE_SNAPSHOT_SIZE
};

typedef struct {
  SEXP self[SAMPLER_SLICE_COUNT];
  SEXP enclosure[SAMPLER_SLICE_COUNT];
  SEXP active[SAMPLER_SLICE_COUNT];
  SEXP private_environment;
} sampler_graph_t;

typedef struct {
  SEXP source[SAMPLER_CLOSURE_LIMIT];
  SEXP target[SAMPLER_CLOSURE_LIMIT];
  R_xlen_t size;
  const sampler_graph_t *source_graph;
  sampler_graph_t *target_graph;
} sampler_closure_map_t;

static SEXP symbol_enclosure = NULL;
static SEXP symbol_active = NULL;
static SEXP symbol_private = NULL;
static SEXP symbol_self = NULL;
static SEXP symbol_super = NULL;
static SEXP symbol_param_set = NULL;
static SEXP symbol_name = NULL;

static int exact_named_generators(SEXP generators) {
  static const char *const names[] = {
    "ParamSet", "Sampler1DUnif", "Sampler1D", "Sampler"
  };
  if (TYPEOF(generators) != VECSXP || ALTREP(generators) ||
      XLENGTH(generators) != SAMPLER_GENERATOR_COUNT ||
      !paradox_api_has_only_attributes(
        generators,
        (const char *const[]) {"names"},
        1
      )) {
    return FALSE;
  }
  R_xlen_t work_since_interrupt = 0;
  return paradox_domain_exact_string_vector(
    Rf_getAttrib(generators, R_NamesSymbol),
    names,
    SAMPLER_GENERATOR_COUNT,
    &work_since_interrupt
  );
}

static int exact_sampler_classes(SEXP object) {
  static const char *const classes[] = {
    "Sampler1DUnif", "Sampler1D", "Sampler", "R6"
  };
  R_xlen_t work_since_interrupt = 0;
  return paradox_api_has_single_attribute(object, "class") &&
    paradox_domain_exact_string_vector(
      Rf_getAttrib(object, R_ClassSymbol),
      classes,
      4,
      &work_since_interrupt
    );
}

static int name_is(SEXP name, const char *expected) {
  return TYPEOF(name) == CHARSXP && strcmp(CHAR(name), expected) == 0;
}

static R_xlen_t name_position(SEXP names, SEXP name) {
  for (R_xlen_t index = 0; index < XLENGTH(names); ++index) {
    if (STRING_ELT(names, index) == name) {
      return index;
    }
  }
  return R_XLEN_T_MAX;
}

static int exact_string_snapshot(SEXP value, SEXP snapshot) {
  if (TYPEOF(value) != STRSXP || TYPEOF(snapshot) != STRSXP ||
      ALTREP(value) || ALTREP(snapshot) ||
      XLENGTH(value) != XLENGTH(snapshot) ||
      R_getAttribCount(value) != R_getAttribCount(snapshot)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(value); ++index) {
    if (STRING_ELT(value, index) != STRING_ELT(snapshot, index)) {
      return FALSE;
    }
  }
  return R_getAttribCount(value) == 0;
}

static int load_sampler_graph(SEXP root, sampler_graph_t *graph) {
  SEXP namespace_environment = paradox_api_registered_namespace("paradox");
  graph->self[0] = root;
  graph->private_environment = R_NilValue;
  for (R_xlen_t slice = 0; slice < SAMPLER_SLICE_COUNT; ++slice) {
    SEXP self = graph->self[slice];
    if (TYPEOF(self) != ENVSXP ||
        paradox_api_parent_environment(self) != R_EmptyEnv ||
        R_EnvironmentIsLocked(self) != (slice == 0) ||
        (slice == 0 ? !exact_sampler_classes(self)
          : R_getAttribCount(self) != 0)) {
      return FALSE;
    }
    SEXP enclosure = paradox_api_local_value(self, symbol_enclosure);
    if (TYPEOF(enclosure) != ENVSXP || R_EnvironmentIsLocked(enclosure) ||
        paradox_api_parent_environment(enclosure) != namespace_environment ||
        R_getAttribCount(enclosure) != 0 ||
        paradox_api_local_value(enclosure, symbol_self) != root) {
      return FALSE;
    }
    graph->enclosure[slice] = enclosure;
    SEXP private_environment = paradox_api_local_value(
      enclosure,
      symbol_private
    );
    if (TYPEOF(private_environment) != ENVSXP ||
        paradox_api_parent_environment(private_environment) != R_EmptyEnv ||
        !R_EnvironmentIsLocked(private_environment) ||
        R_getAttribCount(private_environment) != 0) {
      return FALSE;
    }
    if (slice == 0) {
      graph->private_environment = private_environment;
    } else if (private_environment != graph->private_environment) {
      return FALSE;
    }
    SEXP active = paradox_api_local_value(enclosure, symbol_active);
    if (TYPEOF(active) != VECSXP || ALTREP(active)) {
      return FALSE;
    }
    graph->active[slice] = active;
    SEXP next = paradox_api_local_value(enclosure, symbol_super);
    if (slice + 1 < SAMPLER_SLICE_COUNT) {
      if (TYPEOF(next) != ENVSXP) {
        return FALSE;
      }
      graph->self[slice + 1] = next;
    } else if (next != R_UnboundValue) {
      return FALSE;
    }
  }
  return TRUE;
}

static int closure_uses_graph_enclosure(SEXP closure,
    const sampler_graph_t *graph) {
  if (TYPEOF(closure) != CLOSXP) {
    return FALSE;
  }
  SEXP environment = paradox_api_closure_environment(closure);
  for (R_xlen_t slice = 0; slice < SAMPLER_SLICE_COUNT; ++slice) {
    if (environment == graph->enclosure[slice]) {
      return TRUE;
    }
  }
  return FALSE;
}

static int validate_self_bindings(const sampler_graph_t *graph,
    R_xlen_t slice, SEXP names) {
  if (TYPEOF(names) != STRSXP || ALTREP(names) ||
      XLENGTH(names) == 0 || XLENGTH(names) > 32) {
    return FALSE;
  }
  SEXP active_names = Rf_getAttrib(graph->active[slice], R_NamesSymbol);
  if (XLENGTH(graph->active[slice]) == 0) {
    if (active_names != R_NilValue || R_getAttribCount(graph->active[slice])) {
      return FALSE;
    }
  } else if (TYPEOF(active_names) != STRSXP || ALTREP(active_names) ||
      XLENGTH(active_names) != XLENGTH(graph->active[slice]) ||
      R_getAttribCount(graph->active[slice]) != 1) {
    return FALSE;
  }
  R_xlen_t seen_enclosure = 0;
  R_xlen_t seen_param_set = 0;
  R_xlen_t seen_active = 0;
  for (R_xlen_t index = 0; index < XLENGTH(names); ++index) {
    SEXP name = STRING_ELT(names, index);
    SEXP symbol = Rf_installChar(name);
    const R_BindingType_t type = R_GetBindingType(
      symbol,
      graph->self[slice]
    );
    if (type == R_BindingTypeActive) {
      SEXP closure = R_ActiveBindingFunction(symbol, graph->self[slice]);
      const R_xlen_t position = name_position(active_names, name);
      if (!closure_uses_graph_enclosure(closure, graph) ||
          position == R_XLEN_T_MAX ||
          VECTOR_ELT(graph->active[slice], position) != closure) {
        return FALSE;
      }
      ++seen_active;
      continue;
    }
    if (type != R_BindingTypeValue) {
      return FALSE;
    }
    SEXP value = paradox_api_local_value(graph->self[slice], symbol);
    if (value == graph->enclosure[slice]) {
      if (!name_is(name, ".__enclos_env__")) {
        return FALSE;
      }
      ++seen_enclosure;
    } else if (TYPEOF(value) == CLOSXP) {
      if (!closure_uses_graph_enclosure(value, graph)) {
        return FALSE;
      }
    } else if (slice == 0 && symbol == symbol_param_set &&
        TYPEOF(value) == ENVSXP) {
      ++seen_param_set;
    } else {
      return FALSE;
    }
  }
  return seen_enclosure == 1 && seen_param_set == (slice == 0) &&
    seen_active == XLENGTH(graph->active[slice]);
}

static int validate_enclosure_bindings(const sampler_graph_t *graph,
    R_xlen_t slice, SEXP names) {
  if (TYPEOF(names) != STRSXP || ALTREP(names) ||
      XLENGTH(names) != (slice + 1 < SAMPLER_SLICE_COUNT ? 4 : 3)) {
    return FALSE;
  }
  int seen_active = FALSE;
  int seen_private = FALSE;
  int seen_self = FALSE;
  int seen_super = FALSE;
  for (R_xlen_t index = 0; index < XLENGTH(names); ++index) {
    SEXP symbol = Rf_installChar(STRING_ELT(names, index));
    if (R_GetBindingType(symbol, graph->enclosure[slice]) !=
          R_BindingTypeValue ||
        R_BindingIsLocked(symbol, graph->enclosure[slice])) {
      return FALSE;
    }
    SEXP value = paradox_api_local_value(graph->enclosure[slice], symbol);
    if (symbol == symbol_active && value == graph->active[slice]) {
      seen_active = TRUE;
    } else if (symbol == symbol_private &&
        value == graph->private_environment) {
      seen_private = TRUE;
    } else if (symbol == symbol_self && value == graph->self[0]) {
      seen_self = TRUE;
    } else if (symbol == symbol_super &&
        slice + 1 < SAMPLER_SLICE_COUNT &&
        value == graph->self[slice + 1]) {
      seen_super = TRUE;
    } else {
      return FALSE;
    }
  }
  return seen_active && seen_private && seen_self &&
    (seen_super == (slice + 1 < SAMPLER_SLICE_COUNT));
}

static SEXP sampler_graph_schema(const sampler_graph_t *graph) {
  SEXP schema = PROTECT(Rf_allocVector(VECSXP, SAMPLER_SCHEMA_SIZE));
  for (R_xlen_t slice = 0; slice < SAMPLER_SLICE_COUNT; ++slice) {
    SEXP self_names = PROTECT(R_lsInternal3(
      graph->self[slice],
      TRUE,
      FALSE
    ));
    SEXP enclosure_names = PROTECT(R_lsInternal3(
      graph->enclosure[slice],
      TRUE,
      FALSE
    ));
    if (!validate_self_bindings(graph, slice, self_names) ||
        !validate_enclosure_bindings(graph, slice, enclosure_names)) {
      UNPROTECT(3);
      return R_NilValue;
    }
    SET_VECTOR_ELT(schema, SAMPLER_SELF_NAMES_FIRST + slice, self_names);
    SET_VECTOR_ELT(
      schema,
      SAMPLER_ENCLOSURE_NAMES_FIRST + slice,
      enclosure_names
    );
    UNPROTECT(2);
  }
  SEXP private_names = PROTECT(R_lsInternal3(
    graph->private_environment,
    TRUE,
    FALSE
  ));
  if (TYPEOF(private_names) != STRSXP || ALTREP(private_names) ||
      XLENGTH(private_names) == 0 || XLENGTH(private_names) > 16) {
    UNPROTECT(2);
    return R_NilValue;
  }
  for (R_xlen_t index = 0; index < XLENGTH(private_names); ++index) {
    SEXP symbol = Rf_installChar(STRING_ELT(private_names, index));
    SEXP value = PROTECT(paradox_api_local_value(
      graph->private_environment,
      symbol
    ));
    if (R_GetBindingType(symbol, graph->private_environment) !=
          R_BindingTypeValue ||
        !R_BindingIsLocked(symbol, graph->private_environment) ||
        !closure_uses_graph_enclosure(value, graph)) {
      UNPROTECT(3);
      return R_NilValue;
    }
    UNPROTECT(1);
  }
  SET_VECTOR_ELT(schema, SAMPLER_PRIVATE_NAMES, private_names);
  UNPROTECT(2);
  return schema;
}

static SEXP capsule_snapshot(SEXP capsule) {
  static const char *const helper_names[] = {
    "all_named",
    "assign_func_envs",
    "create_super_env",
    "get_functions",
    "get_nonfunctions",
    "get_superclassnames",
    "list2env2",
    "merge_vectors"
  };
  if (TYPEOF(capsule) != ENVSXP || R_EnvironmentIsLocked(capsule) ||
      paradox_api_parent_environment(capsule) !=
        paradox_api_registered_namespace("R6") ||
      R_getAttribCount(capsule) != 1 || !R_hasAttrib(capsule, symbol_name)) {
    return R_NilValue;
  }
  SEXP names = PROTECT(R_lsInternal3(capsule, TRUE, TRUE));
  R_xlen_t work_since_interrupt = 0;
  if (TYPEOF(names) != STRSXP || ALTREP(names) || XLENGTH(names) != 8 ||
      R_getAttribCount(names) != 0 ||
      !paradox_domain_exact_string_vector(
        names,
        helper_names,
        8,
        &work_since_interrupt
      )) {
    UNPROTECT(1);
    return R_NilValue;
  }
  SEXP root = PROTECT(Rf_allocVector(VECSXP, CAPSULE_SNAPSHOT_SIZE));
  SEXP symbols = PROTECT(Rf_allocVector(VECSXP, XLENGTH(names)));
  SEXP values = PROTECT(Rf_allocVector(VECSXP, XLENGTH(names)));
  SEXP name_attribute = PROTECT(Rf_duplicate(Rf_getAttrib(
    capsule,
    symbol_name
  )));
  SET_VECTOR_ELT(root, CAPSULE_ENVIRONMENT, capsule);
  SET_VECTOR_ELT(root, CAPSULE_PARENT, paradox_api_parent_environment(capsule));
  SET_VECTOR_ELT(root, CAPSULE_NAME_ATTRIBUTE, name_attribute);
  SET_VECTOR_ELT(root, CAPSULE_NAMES, names);
  SET_VECTOR_ELT(root, CAPSULE_SYMBOLS, symbols);
  SET_VECTOR_ELT(root, CAPSULE_VALUES, values);
  for (R_xlen_t index = 0; index < XLENGTH(names); ++index) {
    SEXP symbol = Rf_installChar(STRING_ELT(names, index));
    SEXP value = PROTECT(paradox_api_local_value(capsule, symbol));
    if (R_GetBindingType(symbol, capsule) != R_BindingTypeValue ||
        R_BindingIsLocked(symbol, capsule) || TYPEOF(value) != CLOSXP) {
      UNPROTECT(6);
      return R_NilValue;
    }
    SET_VECTOR_ELT(symbols, index, symbol);
    SET_VECTOR_ELT(values, index, value);
    UNPROTECT(1);
  }
  UNPROTECT(5);
  return root;
}

static int capsule_matches(SEXP names) {
  SEXP snapshot = canonical_r6_capsule;
  if (snapshot == NULL) {
    return FALSE;
  }
  SEXP capsule = VECTOR_ELT(snapshot, CAPSULE_ENVIRONMENT);
  if (TYPEOF(capsule) != ENVSXP || R_EnvironmentIsLocked(capsule) ||
      paradox_api_parent_environment(capsule) !=
        VECTOR_ELT(snapshot, CAPSULE_PARENT) ||
      R_getAttribCount(capsule) != 1 ||
      !R_hasAttrib(capsule, symbol_name) ||
      !exact_string_snapshot(
        Rf_getAttrib(capsule, symbol_name),
        VECTOR_ELT(snapshot, CAPSULE_NAME_ATTRIBUTE)
      ) || !exact_string_snapshot(
        names,
        VECTOR_ELT(snapshot, CAPSULE_NAMES)
      )) {
    return FALSE;
  }
  SEXP symbols = VECTOR_ELT(snapshot, CAPSULE_SYMBOLS);
  SEXP values = VECTOR_ELT(snapshot, CAPSULE_VALUES);
  for (R_xlen_t index = 0; index < XLENGTH(names); ++index) {
    SEXP symbol = VECTOR_ELT(symbols, index);
    if (R_GetBindingType(symbol, capsule) != R_BindingTypeValue ||
        R_BindingIsLocked(symbol, capsule) ||
        paradox_api_local_value(capsule, symbol) !=
          VECTOR_ELT(values, index)) {
      return FALSE;
    }
  }
  return TRUE;
}

static SEXP namespace_target_snapshot(SEXP generators) {
  static const char *const target_names[] = {
    ".__ParamSet__initialize",
    ".__ParamSet__extra_trafo",
    ".__Sampler1D__initialize",
    ".__Sampler1DUnif__initialize",
    ".__Sampler__initialize",
    "ParamSet",
    "Sampler1DUnif",
    "Sampler1D",
    "Sampler"
  };
  SEXP namespace_environment = paradox_api_registered_namespace("paradox");
  /* .onLoad runs before R seals this package namespace.  Individual target
   * bindings are already locked; the runtime matcher additionally requires
   * the completed namespace seal before admitting the factory. */
  if (TYPEOF(namespace_environment) != ENVSXP) {
    return R_NilValue;
  }
  const R_xlen_t count = (R_xlen_t) (
    sizeof(target_names) / sizeof(target_names[0])
  );
  SEXP root = PROTECT(Rf_allocVector(VECSXP, NAMESPACE_SNAPSHOT_SIZE));
  SEXP symbols = PROTECT(Rf_allocVector(VECSXP, count));
  SEXP values = PROTECT(Rf_allocVector(VECSXP, count));
  SEXP types = PROTECT(Rf_allocVector(INTSXP, count));
  SET_VECTOR_ELT(root, NAMESPACE_ENVIRONMENT, namespace_environment);
  SET_VECTOR_ELT(root, NAMESPACE_SYMBOLS, symbols);
  SET_VECTOR_ELT(root, NAMESPACE_VALUES, values);
  SET_VECTOR_ELT(root, NAMESPACE_TYPES, types);
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP symbol = Rf_install(target_names[index]);
    const R_BindingType_t binding_type = R_GetBindingType(
      symbol,
      namespace_environment
    );
    SEXP value = paradox_api_stable_local_value(
      namespace_environment,
      symbol
    );
    const int generator_index = index >= 5 ? (int) index - 5 : -1;
    if ((binding_type != R_BindingTypeValue &&
          binding_type != R_BindingTypeForced) ||
        (generator_index < 0 ? TYPEOF(value) != CLOSXP
          : value != VECTOR_ELT(generators, generator_index))) {
      UNPROTECT(4);
      return R_NilValue;
    }
    SET_VECTOR_ELT(symbols, index, symbol);
    SET_VECTOR_ELT(values, index, value);
    INTEGER(types)[index] = (int) binding_type;
  }
  UNPROTECT(4);
  return root;
}

static int namespace_targets_match(void) {
  if (canonical_namespace_targets == NULL) {
    return FALSE;
  }
  SEXP namespace_environment = VECTOR_ELT(
    canonical_namespace_targets,
    NAMESPACE_ENVIRONMENT
  );
  if (namespace_environment != paradox_api_registered_namespace("paradox") ||
      !R_EnvironmentIsLocked(namespace_environment)) {
    return FALSE;
  }
  SEXP symbols = VECTOR_ELT(
    canonical_namespace_targets,
    NAMESPACE_SYMBOLS
  );
  SEXP values = VECTOR_ELT(canonical_namespace_targets, NAMESPACE_VALUES);
  SEXP types = VECTOR_ELT(canonical_namespace_targets, NAMESPACE_TYPES);
  for (R_xlen_t index = 0; index < XLENGTH(symbols); ++index) {
    SEXP symbol = VECTOR_ELT(symbols, index);
    if (R_GetBindingType(symbol, namespace_environment) !=
          (R_BindingType_t) INTEGER(types)[index] ||
        !R_BindingIsLocked(symbol, namespace_environment) ||
        paradox_api_stable_local_value(namespace_environment, symbol) !=
          VECTOR_ELT(values, index)) {
      return FALSE;
    }
  }
  return TRUE;
}

static int generators_match(SEXP generators, SEXP names) {
  if (!exact_named_generators(generators) || TYPEOF(names) != VECSXP ||
      XLENGTH(names) != SAMPLER_GENERATOR_COUNT ||
      canonical_sampler_generators == NULL) {
    return FALSE;
  }
  SEXP capsule = VECTOR_ELT(canonical_r6_capsule, CAPSULE_ENVIRONMENT);
  for (R_xlen_t index = 0; index < SAMPLER_GENERATOR_COUNT; ++index) {
    SEXP generator = VECTOR_ELT(generators, index);
    if (TYPEOF(generator) != ENVSXP ||
        paradox_api_parent_environment(generator) != capsule ||
        !paradox_r6_generator_matches(
          generator,
          VECTOR_ELT(canonical_sampler_generators, index),
          VECTOR_ELT(names, index)
        )) {
      return FALSE;
    }
  }
  return TRUE;
}

static int sampler_surfaces_match_allocating(SEXP generators) {
  if (canonical_r6_capsule == NULL) {
    return FALSE;
  }
  SEXP generator_names = PROTECT(Rf_allocVector(
    VECSXP,
    SAMPLER_GENERATOR_COUNT
  ));
  for (R_xlen_t index = 0; index < SAMPLER_GENERATOR_COUNT; ++index) {
    SEXP generator = VECTOR_ELT(generators, index);
    if (TYPEOF(generator) != ENVSXP) {
      UNPROTECT(1);
      return FALSE;
    }
    SEXP names = PROTECT(R_lsInternal3(generator, TRUE, TRUE));
    SET_VECTOR_ELT(generator_names, index, names);
    UNPROTECT(1);
  }
  SEXP capsule_names = PROTECT(R_lsInternal3(
    VECTOR_ELT(canonical_r6_capsule, CAPSULE_ENVIRONMENT),
    TRUE,
    TRUE
  ));
  const int admitted = generators_match(generators, generator_names) &&
    paradox_param_set_bulk_generator_matches(
      VECTOR_ELT(generators, 0),
      VECTOR_ELT(generator_names, 0)
    ) && capsule_matches(capsule_names) && namespace_targets_match();
  UNPROTECT(2);
  return admitted;
}

static SEXP clone_sampler_closure(sampler_closure_map_t *map,
    SEXP source) {
  for (R_xlen_t index = 0; index < map->size; ++index) {
    if (map->source[index] == source) {
      PROTECT(map->target[index]);
      return map->target[index];
    }
  }
  if (map->size >= SAMPLER_CLOSURE_LIMIT || TYPEOF(source) != CLOSXP) {
    PROTECT(R_NilValue);
    return R_NilValue;
  }
  SEXP source_environment = paradox_api_closure_environment(source);
  SEXP target_environment = R_NilValue;
  for (R_xlen_t slice = 0; slice < SAMPLER_SLICE_COUNT; ++slice) {
    if (source_environment == map->source_graph->enclosure[slice]) {
      target_environment = map->target_graph->enclosure[slice];
      break;
    }
  }
  if (target_environment == R_NilValue) {
    PROTECT(R_NilValue);
    return R_NilValue;
  }
  SEXP closure_copy = PROTECT(Rf_duplicate(source));
  SEXP target = PROTECT(R_mkClosure(
    paradox_api_closure_formals(closure_copy),
    paradox_api_closure_expression(closure_copy),
    target_environment
  ));
  SHALLOW_DUPLICATE_ATTRIB(target, closure_copy);
  map->source[map->size] = source;
  map->target[map->size] = target;
  ++map->size;
  UNPROTECT(1);
  return target;
}

static void define_with_lock(SEXP environment, SEXP symbol, SEXP value,
    int locked) {
  Rf_defineVar(symbol, value, environment);
  if (locked) {
    R_LockBinding(symbol, environment);
  }
}

static int build_sampler_shell(const sampler_graph_t *source,
    SEXP param_set, SEXP schema, SEXP *result) {
  enum root_slot {
    ROOT_SELF_FIRST = 0,
    ROOT_ENCLOSURE_FIRST = ROOT_SELF_FIRST + SAMPLER_SLICE_COUNT,
    ROOT_ACTIVE_FIRST = ROOT_ENCLOSURE_FIRST + SAMPLER_SLICE_COUNT,
    ROOT_PRIVATE = ROOT_ACTIVE_FIRST + SAMPLER_SLICE_COUNT,
    ROOT_COUNT
  };
  sampler_graph_t target;
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, ROOT_COUNT));
  SEXP namespace_environment = paradox_api_registered_namespace("paradox");
  for (R_xlen_t slice = 0; slice < SAMPLER_SLICE_COUNT; ++slice) {
    SEXP self = PROTECT(R_NewEnv(R_EmptyEnv, FALSE, 0));
    SET_VECTOR_ELT(roots, ROOT_SELF_FIRST + slice, self);
    target.self[slice] = self;
    UNPROTECT(1);
    SEXP enclosure = PROTECT(R_NewEnv(
      namespace_environment,
      FALSE,
      0
    ));
    SET_VECTOR_ELT(roots, ROOT_ENCLOSURE_FIRST + slice, enclosure);
    target.enclosure[slice] = enclosure;
    UNPROTECT(1);
    SEXP active = PROTECT(Rf_allocVector(
      VECSXP,
      XLENGTH(source->active[slice])
    ));
    DUPLICATE_ATTRIB(active, source->active[slice]);
    SET_VECTOR_ELT(roots, ROOT_ACTIVE_FIRST + slice, active);
    target.active[slice] = active;
    UNPROTECT(1);
  }
  SEXP private_environment = PROTECT(R_NewEnv(R_EmptyEnv, FALSE, 0));
  SET_VECTOR_ELT(roots, ROOT_PRIVATE, private_environment);
  target.private_environment = private_environment;
  UNPROTECT(1);

  sampler_closure_map_t map = {
    {R_NilValue}, {R_NilValue}, 0, source, &target
  };
  for (R_xlen_t slice = 0; slice < SAMPLER_SLICE_COUNT; ++slice) {
    SEXP names = VECTOR_ELT(schema, SAMPLER_SELF_NAMES_FIRST + slice);
    SEXP active_names = Rf_getAttrib(source->active[slice], R_NamesSymbol);
    for (R_xlen_t remaining = XLENGTH(names); remaining > 0; --remaining) {
      const R_xlen_t index = remaining - 1;
      SEXP name = STRING_ELT(names, index);
      SEXP symbol = Rf_installChar(name);
      const R_BindingType_t type = R_GetBindingType(
        symbol,
        source->self[slice]
      );
      if (type == R_BindingTypeActive) {
        SEXP source_closure = R_ActiveBindingFunction(
          symbol,
          source->self[slice]
        );
        SEXP closure = clone_sampler_closure(&map, source_closure);
        const R_xlen_t position = name_position(active_names, name);
        if (closure == R_NilValue || position == R_XLEN_T_MAX) {
          UNPROTECT(2);
          return FALSE;
        }
        R_MakeActiveBinding(symbol, closure, target.self[slice]);
        if (R_BindingIsLocked(symbol, source->self[slice])) {
          R_LockBinding(symbol, target.self[slice]);
        }
        SET_VECTOR_ELT(target.active[slice], position, closure);
        UNPROTECT(1);
        continue;
      }
      SEXP source_value = paradox_api_local_value(
        source->self[slice],
        symbol
      );
      SEXP value;
      if (source_value == source->enclosure[slice]) {
        value = target.enclosure[slice];
        PROTECT(value);
      } else if (TYPEOF(source_value) == CLOSXP) {
        value = clone_sampler_closure(&map, source_value);
      } else if (slice == 0 && symbol == symbol_param_set &&
          TYPEOF(param_set) == ENVSXP) {
        value = param_set;
        PROTECT(value);
      } else {
        UNPROTECT(1);
        return FALSE;
      }
      if (value == R_NilValue) {
        UNPROTECT(2);
        return FALSE;
      }
      define_with_lock(
        target.self[slice],
        symbol,
        value,
        R_BindingIsLocked(symbol, source->self[slice])
      );
      UNPROTECT(1);
    }
    DUPLICATE_ATTRIB(target.self[slice], source->self[slice]);
  }

  SEXP private_names = VECTOR_ELT(schema, SAMPLER_PRIVATE_NAMES);
  for (R_xlen_t remaining = XLENGTH(private_names);
      remaining > 0; --remaining) {
    const R_xlen_t index = remaining - 1;
    SEXP symbol = Rf_installChar(STRING_ELT(private_names, index));
    SEXP source_value = paradox_api_local_value(
      source->private_environment,
      symbol
    );
    SEXP value = clone_sampler_closure(&map, source_value);
    if (value == R_NilValue) {
      UNPROTECT(2);
      return FALSE;
    }
    define_with_lock(
      target.private_environment,
      symbol,
      value,
      R_BindingIsLocked(symbol, source->private_environment)
    );
    UNPROTECT(1);
  }

  for (R_xlen_t slice = 0; slice < SAMPLER_SLICE_COUNT; ++slice) {
    SEXP names = VECTOR_ELT(
      schema,
      SAMPLER_ENCLOSURE_NAMES_FIRST + slice
    );
    for (R_xlen_t remaining = XLENGTH(names); remaining > 0; --remaining) {
      const R_xlen_t index = remaining - 1;
      SEXP symbol = Rf_installChar(STRING_ELT(names, index));
      SEXP source_value = paradox_api_local_value(
        source->enclosure[slice],
        symbol
      );
      SEXP value;
      if (source_value == source->active[slice]) {
        value = target.active[slice];
      } else if (source_value == source->private_environment) {
        value = target.private_environment;
      } else if (source_value == source->self[0]) {
        value = target.self[0];
      } else if (slice + 1 < SAMPLER_SLICE_COUNT &&
          source_value == source->self[slice + 1]) {
        value = target.self[slice + 1];
      } else {
        UNPROTECT(1);
        return FALSE;
      }
      define_with_lock(
        target.enclosure[slice],
        symbol,
        value,
        R_BindingIsLocked(symbol, source->enclosure[slice])
      );
    }
    DUPLICATE_ATTRIB(target.enclosure[slice], source->enclosure[slice]);
    if (R_EnvironmentIsLocked(source->self[slice])) {
      R_LockEnvironment(target.self[slice], FALSE);
    }
  }
  DUPLICATE_ATTRIB(
    target.private_environment,
    source->private_environment
  );
  R_LockEnvironment(target.private_environment, FALSE);
  *result = target.self[0];
  UNPROTECT(1);
  return TRUE;
}

SEXP paradox_sampler_1d_unif_bulk_register(SEXP prototype,
    SEXP generators) {
  if (canonical_sampler_prototype != NULL ||
      canonical_sampler_schema != NULL ||
      canonical_sampler_generators != NULL || canonical_r6_capsule != NULL ||
      canonical_namespace_targets != NULL ||
      TYPEOF(prototype) != ENVSXP || !exact_named_generators(generators)) {
    return Rf_ScalarLogical(FALSE);
  }
  symbol_enclosure = Rf_install(".__enclos_env__");
  symbol_active = Rf_install(".__active__");
  symbol_private = Rf_install("private");
  symbol_self = Rf_install("self");
  symbol_super = Rf_install("super");
  symbol_param_set = Rf_install("param_set");
  symbol_name = Rf_install("name");

  sampler_graph_t graph;
  if (!load_sampler_graph(prototype, &graph)) {
    return Rf_ScalarLogical(FALSE);
  }
  SEXP schema = PROTECT(sampler_graph_schema(&graph));
  if (schema == R_NilValue) {
    UNPROTECT(1);
    return Rf_ScalarLogical(FALSE);
  }
  SEXP snapshots = PROTECT(Rf_allocVector(
    VECSXP,
    SAMPLER_GENERATOR_COUNT
  ));
  SEXP capsule = paradox_api_parent_environment(VECTOR_ELT(generators, 0));
  for (R_xlen_t index = 0; index < SAMPLER_GENERATOR_COUNT; ++index) {
    SEXP generator = VECTOR_ELT(generators, index);
    if (TYPEOF(generator) != ENVSXP ||
        paradox_api_parent_environment(generator) != capsule) {
      UNPROTECT(2);
      return Rf_ScalarLogical(FALSE);
    }
    SEXP snapshot = PROTECT(paradox_r6_generator_snapshot(generator));
    if (snapshot == R_NilValue) {
      UNPROTECT(3);
      return Rf_ScalarLogical(FALSE);
    }
    SET_VECTOR_ELT(snapshots, index, snapshot);
    UNPROTECT(1);
  }
  SEXP capsule_state = PROTECT(capsule_snapshot(capsule));
  SEXP namespace_state = PROTECT(namespace_target_snapshot(generators));
  if (capsule_state == R_NilValue || namespace_state == R_NilValue) {
    UNPROTECT(4);
    return Rf_ScalarLogical(FALSE);
  }

  R_PreserveObject(prototype);
  canonical_sampler_prototype = prototype;
  R_PreserveObject(schema);
  canonical_sampler_schema = schema;
  R_PreserveObject(snapshots);
  canonical_sampler_generators = snapshots;
  R_PreserveObject(capsule_state);
  canonical_r6_capsule = capsule_state;
  R_PreserveObject(namespace_state);
  canonical_namespace_targets = namespace_state;
  UNPROTECT(4);
  return Rf_ScalarLogical(TRUE);
}

SEXP paradox_sampler_1d_unif_bulk_auth(SEXP generators) {
  return Rf_ScalarLogical(
    exact_named_generators(generators) &&
    sampler_surfaces_match_allocating(generators)
  );
}

SEXP paradox_sampler_1d_unif_bulk_shells(SEXP param_generator,
    SEXP generators, SEXP plans, SEXP values) {
  /* values is the public snapshot from which every one-row token was made.
   * Keeping the exact ordinary list as a .Call argument roots all of its
   * children until the all-or-nothing handoff has completed. */
  if (canonical_sampler_prototype == NULL ||
      TYPEOF(values) != VECSXP || ALTREP(values) ||
      !exact_named_generators(generators) ||
      VECTOR_ELT(generators, 0) != param_generator ||
      !sampler_surfaces_match_allocating(generators)) {
    return R_NilValue;
  }
  R_xlen_t count = 0;
  SEXP tokens = PROTECT(paradox_param_set_bulk_prepare(
    param_generator,
    plans,
    &count
  ));
  if (tokens == R_NilValue ||
      !paradox_param_set_bulk_tokens_match(tokens, TRUE)) {
    UNPROTECT(1);
    return R_NilValue;
  }
  SEXP param_sets = PROTECT(Rf_allocVector(VECSXP, count));
  SEXP param_privates = PROTECT(Rf_allocVector(VECSXP, count));
  SEXP samplers = PROTECT(Rf_allocVector(VECSXP, count));
  if (!paradox_param_set_bulk_build_shells(
      param_sets,
      param_privates,
      count
    )) {
    UNPROTECT(4);
    return R_NilValue;
  }
  sampler_graph_t source;
  if (!load_sampler_graph(canonical_sampler_prototype, &source)) {
    UNPROTECT(4);
    return R_NilValue;
  }
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP sampler = R_NilValue;
    if (!build_sampler_shell(
        &source,
        VECTOR_ELT(param_sets, index),
        canonical_sampler_schema,
        &sampler
      )) {
      UNPROTECT(4);
      return R_NilValue;
    }
    SET_VECTOR_ELT(samplers, index, sampler);
  }
  SEXP result_names = PROTECT(Rf_duplicate(Rf_getAttrib(
    tokens,
    R_NamesSymbol
  )));
  Rf_setAttrib(samplers, R_NamesSymbol, result_names);

  /* These are the last allocation-capable inventories.  The audit below is
   * pointer/type/lock comparison only; token adoption starts afterwards. */
  SEXP generator_names = PROTECT(Rf_allocVector(
    VECSXP,
    SAMPLER_GENERATOR_COUNT
  ));
  for (R_xlen_t index = 0; index < SAMPLER_GENERATOR_COUNT; ++index) {
    SEXP names = PROTECT(R_lsInternal3(
      VECTOR_ELT(generators, index),
      TRUE,
      TRUE
    ));
    SET_VECTOR_ELT(generator_names, index, names);
    UNPROTECT(1);
  }
  SEXP capsule_names = PROTECT(R_lsInternal3(
    VECTOR_ELT(canonical_r6_capsule, CAPSULE_ENVIRONMENT),
    TRUE,
    TRUE
  ));
  if (!generators_match(generators, generator_names) ||
      !paradox_param_set_bulk_generator_matches(
        param_generator,
        VECTOR_ELT(generator_names, 0)
      ) || !capsule_matches(capsule_names) ||
      !namespace_targets_match() ||
      !paradox_param_set_bulk_tokens_match(tokens, TRUE) ||
      !paradox_param_set_bulk_destinations_ready(
        param_sets,
        param_privates,
        count
      )) {
    UNPROTECT(7);
    return R_NilValue;
  }
  if (!paradox_param_set_bulk_commit(
      param_sets,
      param_privates,
      tokens
    )) {
    UNPROTECT(7);
    Rf_error("Internal error: preflighted sampler state was not adoptable");
  }
  UNPROTECT(7);
  return samplers;
}

#else

SEXP paradox_sampler_1d_unif_bulk_register(SEXP prototype,
    SEXP generators) {
  (void) prototype;
  (void) generators;
  return Rf_ScalarLogical(FALSE);
}

SEXP paradox_sampler_1d_unif_bulk_auth(SEXP generators) {
  (void) generators;
  return Rf_ScalarLogical(FALSE);
}

SEXP paradox_sampler_1d_unif_bulk_shells(SEXP param_generator,
    SEXP generators, SEXP plans, SEXP values) {
  (void) param_generator;
  (void) generators;
  (void) plans;
  (void) values;
  return R_NilValue;
}

#endif
