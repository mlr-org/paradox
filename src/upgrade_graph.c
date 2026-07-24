#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "core_state.h"
#include "r_api_compat.h"
#include "r_utils.h"
#include "upgrade_graph.h"

typedef struct paradox_upgrade_path {
  const struct paradox_upgrade_path *parent;
  const char *segment;
  size_t segment_size;
  size_t total_size;
} paradox_upgrade_path_t;

typedef struct {
  SEXP node;
  const paradox_upgrade_path_t *path;
} paradox_upgrade_work_t;

typedef struct {
  paradox_upgrade_work_t *items;
  size_t size;
  size_t capacity;
  SEXP roots;
  PROTECT_INDEX roots_index;
} paradox_upgrade_stack_t;

typedef struct {
  SEXP *keys;
  size_t size;
  size_t capacity;
  SEXP roots;
  R_xlen_t root_count;
  R_xlen_t root_capacity;
  PROTECT_INDEX roots_index;
} paradox_upgrade_seen_t;

typedef struct {
  SEXP shell;
  const paradox_upgrade_path_t *path;
} paradox_upgrade_candidate_t;

typedef struct {
  paradox_upgrade_candidate_t *items;
  size_t size;
  size_t capacity;
} paradox_upgrade_candidates_t;

typedef struct {
  SEXP *items;
  size_t size;
  size_t capacity;
} paradox_upgrade_boundaries_t;

typedef struct {
  SEXP tag;
  SEXP value;
} paradox_upgrade_attribute_t;

typedef struct {
  paradox_upgrade_seen_t seen;
  paradox_upgrade_stack_t stack;
  paradox_upgrade_candidates_t candidates;
  paradox_upgrade_boundaries_t boundaries;
  R_xlen_t work_since_interrupt;
} paradox_upgrade_walker_t;

static void *temporary_size_alloc(size_t count, size_t element_size) {
  if (count > (size_t) R_XLEN_T_MAX) {
    Rf_error("Object graph is too large to inspect");
  }
  return paradox_temporary_alloc((R_xlen_t) count, element_size);
}

static size_t checked_double_capacity(size_t capacity) {
  if (capacity > SIZE_MAX / 2) {
    Rf_error("Object graph is too large to inspect");
  }
  return capacity * 2;
}

static void account_work(paradox_upgrade_walker_t *walker) {
  ++walker->work_since_interrupt;
  if (walker->work_since_interrupt >= PARADOX_INTERRUPT_CHECK_INTERVAL) {
    R_CheckUserInterrupt();
    walker->work_since_interrupt = 0;
  }
}

static paradox_upgrade_path_t *new_path(
    const paradox_upgrade_path_t *parent,
    const char *segment,
    size_t segment_size) {
  if (segment == NULL ||
      (parent != NULL && parent->total_size > SIZE_MAX - segment_size)) {
    Rf_error("Object graph path is too large to report");
  }
  paradox_upgrade_path_t *path = temporary_size_alloc(1, sizeof(*path));
  path->parent = parent;
  path->segment = segment;
  path->segment_size = segment_size;
  path->total_size = segment_size +
    (parent == NULL ? 0 : parent->total_size);
  return path;
}

static paradox_upgrade_path_t *literal_path(
    const paradox_upgrade_path_t *parent, const char *literal) {
  const size_t size = strlen(literal);
  char *owned = temporary_size_alloc(size + 1, sizeof(*owned));
  memcpy(owned, literal, size + 1);
  return new_path(parent, owned, size);
}

static paradox_upgrade_path_t *indexed_path(
    const paradox_upgrade_path_t *parent,
    const char *prefix,
    R_xlen_t index,
    const char *suffix) {
  char digits[32];
  const int written = snprintf(
    digits,
    sizeof(digits),
    "%lld",
    (long long) (index + 1)
  );
  if (written < 0 || (size_t) written >= sizeof(digits)) {
    Rf_error("Object graph index is too large to report");
  }
  const size_t prefix_size = strlen(prefix);
  const size_t suffix_size = strlen(suffix);
  const size_t digits_size = (size_t) written;
  if (prefix_size > SIZE_MAX - suffix_size ||
      prefix_size + suffix_size > SIZE_MAX - digits_size) {
    Rf_error("Object graph path is too large to report");
  }
  const size_t size = prefix_size + digits_size + suffix_size;
  char *segment = temporary_size_alloc(size + 1, sizeof(*segment));
  memcpy(segment, prefix, prefix_size);
  memcpy(segment + prefix_size, digits, digits_size);
  memcpy(segment + prefix_size + digits_size, suffix, suffix_size + 1);
  return new_path(parent, segment, size);
}

static int path_plain_byte(unsigned char byte) {
  return (byte >= (unsigned char) 'a' && byte <= (unsigned char) 'z') ||
    (byte >= (unsigned char) 'A' && byte <= (unsigned char) 'Z') ||
    (byte >= (unsigned char) '0' && byte <= (unsigned char) '9') ||
    byte == (unsigned char) '_' || byte == (unsigned char) '.' ||
    byte == (unsigned char) '-';
}

static paradox_upgrade_path_t *named_path(
    const paradox_upgrade_path_t *parent,
    const char *prefix,
    SEXP name,
    const char *suffix) {
  if (TYPEOF(name) != CHARSXP || name == NA_STRING) {
    return literal_path(parent, "@unnamed");
  }
  const char *bytes = CHAR(name);
  const size_t byte_count = strlen(bytes);
  const size_t prefix_size = strlen(prefix);
  const size_t suffix_size = strlen(suffix);
  if (byte_count > (SIZE_MAX - prefix_size - suffix_size) / 4) {
    Rf_error("Object graph path is too large to report");
  }
  const size_t capacity = prefix_size + suffix_size + byte_count * 4;
  char *segment = temporary_size_alloc(capacity + 1, sizeof(*segment));
  memcpy(segment, prefix, prefix_size);
  size_t cursor = prefix_size;
  static const char hexadecimal[] = "0123456789ABCDEF";
  for (size_t index = 0; index < byte_count; ++index) {
    const unsigned char byte = (unsigned char) bytes[index];
    if (path_plain_byte(byte)) {
      segment[cursor++] = (char) byte;
    } else if (byte == (unsigned char) '"' ||
        byte == (unsigned char) '\\') {
      segment[cursor++] = '\\';
      segment[cursor++] = (char) byte;
    } else {
      segment[cursor++] = '\\';
      segment[cursor++] = 'x';
      segment[cursor++] = hexadecimal[byte >> 4];
      segment[cursor++] = hexadecimal[byte & 15U];
    }
  }
  memcpy(segment + cursor, suffix, suffix_size + 1);
  cursor += suffix_size;
  return new_path(parent, segment, cursor);
}

static SEXP render_path(const paradox_upgrade_path_t *path) {
  if (path == NULL) {
    Rf_error("Object graph path is too large to report");
    return R_NilValue;
  }
  if (path->total_size > (size_t) R_XLEN_T_MAX) {
    Rf_error("Object graph path is too large to report");
    return R_NilValue;
  }
  char *buffer = temporary_size_alloc(path->total_size + 1, sizeof(*buffer));
  size_t cursor = path->total_size;
  buffer[cursor] = '\0';
  for (const paradox_upgrade_path_t *part = path;
      part != NULL;
      part = part->parent) {
    if (cursor < part->segment_size) {
      Rf_error("Internal error: malformed object graph path");
    }
    cursor -= part->segment_size;
    memcpy(buffer + cursor, part->segment, part->segment_size);
  }
  if (cursor != 0) {
    Rf_error("Internal error: malformed object graph path");
  }
  return Rf_mkCharCE(buffer, CE_UTF8);
}

static size_t pointer_hash(SEXP key) {
  uintptr_t value = (uintptr_t) key;
#if UINTPTR_MAX > UINT32_MAX
  value ^= value >> 33;
  value *= UINT64_C(0xff51afd7ed558ccd);
  value ^= value >> 33;
  value *= UINT64_C(0xc4ceb9fe1a85ec53);
  value ^= value >> 33;
#else
  value ^= value >> 16;
  value *= UINT32_C(0x7feb352d);
  value ^= value >> 15;
  value *= UINT32_C(0x846ca68b);
  value ^= value >> 16;
#endif
  return (size_t) value;
}

static void insert_seen_key(SEXP *keys, size_t capacity, SEXP key) {
  size_t slot = pointer_hash(key) & (capacity - 1);
  while (keys[slot] != NULL) {
    slot = (slot + 1) & (capacity - 1);
  }
  keys[slot] = key;
}

static void grow_seen_hash(paradox_upgrade_seen_t *seen) {
  const size_t capacity = checked_double_capacity(seen->capacity);
  SEXP *keys = temporary_size_alloc(capacity, sizeof(*keys));
  memset(keys, 0, capacity * sizeof(*keys));
  for (size_t index = 0; index < seen->capacity; ++index) {
    if (seen->keys[index] != NULL) {
      insert_seen_key(keys, capacity, seen->keys[index]);
    }
  }
  seen->keys = keys;
  seen->capacity = capacity;
}

static void grow_seen_roots(paradox_upgrade_seen_t *seen) {
  if (seen->root_capacity > R_XLEN_T_MAX / 2) {
    Rf_error("Object graph is too large to inspect");
  }
  const R_xlen_t capacity = seen->root_capacity * 2;
  SEXP replacement = PROTECT(Rf_allocVector(VECSXP, capacity));
  for (R_xlen_t index = 0; index < seen->root_count; ++index) {
    SET_VECTOR_ELT(replacement, index, VECTOR_ELT(seen->roots, index));
  }
  seen->roots = replacement;
  REPROTECT(seen->roots, seen->roots_index);
  seen->root_capacity = capacity;
  UNPROTECT(1);
}

static int remember_node(paradox_upgrade_seen_t *seen, SEXP node) {
  if (node == R_NilValue || node == R_UnboundValue ||
      node == R_MissingArg) {
    return FALSE;
  }
  size_t slot = pointer_hash(node) & (seen->capacity - 1);
  while (seen->keys[slot] != NULL) {
    if (seen->keys[slot] == node) return FALSE;
    slot = (slot + 1) & (seen->capacity - 1);
  }

  PROTECT(node);
  if ((seen->size + 1) * 4 > seen->capacity * 3) {
    grow_seen_hash(seen);
    slot = pointer_hash(node) & (seen->capacity - 1);
    while (seen->keys[slot] != NULL) {
      slot = (slot + 1) & (seen->capacity - 1);
    }
  }
  if (seen->root_count == seen->root_capacity) {
    grow_seen_roots(seen);
  }
  seen->keys[slot] = node;
  ++seen->size;
  SET_VECTOR_ELT(seen->roots, seen->root_count, node);
  ++seen->root_count;
  UNPROTECT(1);
  return TRUE;
}

static void grow_stack(paradox_upgrade_stack_t *stack) {
  const size_t capacity = checked_double_capacity(stack->capacity);
  if (capacity > (size_t) R_XLEN_T_MAX) {
    Rf_error("Object graph work stack is too large");
  }
  paradox_upgrade_work_t *items = temporary_size_alloc(
    capacity,
    sizeof(*items)
  );
  memcpy(items, stack->items, stack->size * sizeof(*items));
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, (R_xlen_t) capacity));
  for (size_t index = 0; index < stack->size; ++index) {
    SET_VECTOR_ELT(
      roots,
      (R_xlen_t) index,
      VECTOR_ELT(stack->roots, (R_xlen_t) index)
    );
  }
  stack->roots = roots;
  REPROTECT(stack->roots, stack->roots_index);
  UNPROTECT(1);
  stack->items = items;
  stack->capacity = capacity;
}

static void schedule_node(
    paradox_upgrade_walker_t *walker,
    SEXP node,
    const paradox_upgrade_path_t *path) {
  account_work(walker);
  if (node == R_NilValue || node == R_UnboundValue ||
      node == R_MissingArg) {
    return;
  }
  if (walker->stack.size == walker->stack.capacity) {
    grow_stack(&walker->stack);
  }
  walker->stack.items[walker->stack.size] =
    (paradox_upgrade_work_t) {node, path};
  SET_VECTOR_ELT(
    walker->stack.roots,
    (R_xlen_t) walker->stack.size,
    node
  );
  ++walker->stack.size;
}

static paradox_upgrade_work_t pop_node(paradox_upgrade_stack_t *stack) {
  if (stack->size == 0) {
    Rf_error("Internal error: empty object graph work stack");
  }
  return stack->items[--stack->size];
}

static void grow_candidates(paradox_upgrade_candidates_t *candidates) {
  const size_t capacity = checked_double_capacity(candidates->capacity);
  paradox_upgrade_candidate_t *items = temporary_size_alloc(
    capacity,
    sizeof(*items)
  );
  memcpy(items, candidates->items, candidates->size * sizeof(*items));
  candidates->items = items;
  candidates->capacity = capacity;
}

static void append_candidate(
    paradox_upgrade_candidates_t *candidates,
    SEXP shell,
    const paradox_upgrade_path_t *path) {
  if (candidates->size == candidates->capacity) {
    grow_candidates(candidates);
  }
  candidates->items[candidates->size++] =
    (paradox_upgrade_candidate_t) {shell, path};
}

static int scalar_string_equal(SEXP string, const char *expected) {
  return string != NA_STRING && strcmp(CHAR(string), expected) == 0;
}

static int candidate_kind(
    SEXP environment) {
  SEXP classes = paradox_api_raw_attribute(environment, R_ClassSymbol);
  if (TYPEOF(classes) != STRSXP || ALTREP(classes) || Rf_isS4(classes)) {
    return FALSE;
  }
  const R_xlen_t count = XLENGTH(classes);
  int has_param_set = FALSE;
  int has_r6 = FALSE;
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP label = STRING_ELT(classes, index);
    has_param_set |= scalar_string_equal(label, "ParamSet");
    has_r6 |= scalar_string_equal(label, "R6");
  }
  return has_param_set && has_r6 && count != 0;
}

static int current_param_set_shell(SEXP shell) {
  SEXP enclosure = PROTECT(paradox_api_plain_binding_snapshot(
    shell,
    Rf_install(".__enclos_env__")
  ));
  if (TYPEOF(enclosure) != ENVSXP || Rf_isS4(enclosure)) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP self = PROTECT(paradox_api_plain_binding_snapshot(
    enclosure,
    Rf_install("self")
  ));
  SEXP private_environment = PROTECT(paradox_api_plain_binding_snapshot(
    enclosure,
    Rf_install("private")
  ));
  const int current = self == shell &&
    TYPEOF(private_environment) == ENVSXP &&
    !Rf_isS4(private_environment) &&
    paradox_core_from_private(private_environment) != R_UnboundValue;
  UNPROTECT(3);
  return current;
}

static void grow_boundaries(paradox_upgrade_boundaries_t *boundaries) {
  const size_t capacity = checked_double_capacity(boundaries->capacity);
  SEXP *items = temporary_size_alloc(capacity, sizeof(*items));
  memcpy(items, boundaries->items, boundaries->size * sizeof(*items));
  boundaries->items = items;
  boundaries->capacity = capacity;
}

static int boundary_contains(
    const paradox_upgrade_boundaries_t *boundaries, SEXP environment) {
  for (size_t index = 0; index < boundaries->size; ++index) {
    if (boundaries->items[index] == environment) return TRUE;
  }
  return FALSE;
}

static void append_boundary(
    paradox_upgrade_boundaries_t *boundaries, SEXP environment) {
  if (boundary_contains(boundaries, environment)) return;
  if (boundaries->size == boundaries->capacity) {
    grow_boundaries(boundaries);
  }
  boundaries->items[boundaries->size++] = environment;
}

static SEXP evaluate_base_unary(const char *name, SEXP argument) {
  PROTECT(argument);
  SEXP function = PROTECT(Rf_findFun(Rf_install(name), R_BaseEnv));
  SEXP call = PROTECT(Rf_lang2(function, argument));
  SEXP result = PROTECT(Rf_eval(call, R_BaseEnv));
  UNPROTECT(4);
  return result;
}

static SEXP parent_environment(SEXP environment) {
#if R_VERSION >= R_Version(4, 5, 0)
  return R_ParentEnv(environment);
#else
  return evaluate_base_unary("parent.env", environment);
#endif
}

static void initialize_search_boundaries(
    paradox_upgrade_boundaries_t *boundaries) {
  SEXP environment = R_GlobalEnv;
  while (environment != R_EmptyEnv) {
    if (TYPEOF(environment) != ENVSXP ||
        boundary_contains(boundaries, environment)) {
      Rf_error("Internal error: malformed R search path");
    }
    append_boundary(boundaries, environment);
    environment = parent_environment(environment);
  }
  append_boundary(boundaries, R_EmptyEnv);
  append_boundary(boundaries, R_BaseNamespace);
}

static int imports_environment(SEXP environment) {
  SEXP name = PROTECT(evaluate_base_unary(
    "environmentName",
    environment
  ));
  const int imports = TYPEOF(name) == STRSXP && XLENGTH(name) == 1 &&
    STRING_ELT(name, 0) != NA_STRING &&
    strncmp(CHAR(STRING_ELT(name, 0)), "imports:", 8) == 0;
  UNPROTECT(1);
  return imports;
}

static int environment_boundary(
    const paradox_upgrade_walker_t *walker, SEXP environment) {
  return boundary_contains(&walker->boundaries, environment) ||
    R_IsNamespaceEnv(environment) ||
    R_IsPackageEnv(environment) ||
    imports_environment(environment);
}

static SEXP environment_names(SEXP environment) {
  PROTECT(environment);
  SEXP function = PROTECT(Rf_findFun(Rf_install("ls"), R_BaseEnv));
  SEXP true_value = PROTECT(Rf_ScalarLogical(TRUE));
  SEXP call = PROTECT(Rf_lang4(
    function,
    environment,
    true_value,
    true_value
  ));
  SET_TAG(CDR(call), Rf_install("envir"));
  SET_TAG(CDDR(call), Rf_install("all.names"));
  SET_TAG(CDDDR(call), Rf_install("sorted"));
  SEXP result = PROTECT(Rf_eval(call, R_BaseEnv));
  if (TYPEOF(result) != STRSXP || ALTREP(result)) {
    UNPROTECT(5);
    Rf_error("Internal error: environment name enumeration failed");
  }
  UNPROTECT(5);
  return result;
}

#if R_VERSION < R_Version(4, 6, 0)
static void schedule_promise_edges(
    paradox_upgrade_walker_t *walker,
    SEXP promise,
    const paradox_upgrade_path_t *path) {
  paradox_api_promise_snapshot_t snapshot;
  paradox_api_promise_snapshot(promise, &snapshot);
  PROTECT(snapshot.expression);
  PROTECT(snapshot.environment);
  PROTECT(snapshot.value);
  if (snapshot.forced) {
    schedule_node(
      walker,
      snapshot.value,
      literal_path(path, ".promise.value")
    );
  } else {
    schedule_node(
      walker,
      snapshot.environment,
      literal_path(path, ".promise.environment")
    );
  }
  schedule_node(
    walker,
    snapshot.expression,
    literal_path(path, ".promise.expression")
  );
  UNPROTECT(3);
}
#endif

static void schedule_binding(
    paradox_upgrade_walker_t *walker,
    SEXP environment,
    SEXP symbol,
    const paradox_upgrade_path_t *path) {
  if (R_BindingIsActive(symbol, environment)) {
    SEXP function = PROTECT(R_ActiveBindingFunction(symbol, environment));
    schedule_node(
      walker,
      function,
      literal_path(path, ".active")
    );
    UNPROTECT(1);
    return;
  }

#if R_VERSION >= R_Version(4, 6, 0)
  if (symbol == R_DotsSymbol && R_DotsExist(environment)) {
    const int count = R_DotsLength(environment);
    for (int index = count; index > 0; --index) {
      const paradox_upgrade_path_t *element_path = indexed_path(
        path,
        "[[",
        (R_xlen_t) (index - 1),
        "]]"
      );
      const R_DotType_t type = R_GetDotType(index, environment);
      switch (type) {
      case R_DotTypeValue: {
        SEXP value = PROTECT(R_DotsElt(index, environment));
        schedule_node(walker, value, element_path);
        UNPROTECT(1);
        break;
      }
      case R_DotTypeDelayed: {
        SEXP expression = PROTECT(R_DotDelayedExpression(
          index,
          environment
        ));
        SEXP evaluation_environment = PROTECT(R_DotDelayedEnvironment(
          index,
          environment
        ));
        schedule_node(
          walker,
          evaluation_environment,
          literal_path(element_path, ".promise.environment")
        );
        schedule_node(
          walker,
          expression,
          literal_path(element_path, ".promise.expression")
        );
        UNPROTECT(2);
        break;
      }
      case R_DotTypeForced: {
        SEXP expression = PROTECT(R_DotForcedExpression(
          index,
          environment
        ));
        /* R_DotsElt() evaluates delayed elements, but this branch has already
         * authenticated an existing forced value. */
        SEXP value = PROTECT(R_DotsElt(index, environment));
        schedule_node(
          walker,
          value,
          literal_path(element_path, ".promise.value")
        );
        schedule_node(
          walker,
          expression,
          literal_path(element_path, ".promise.expression")
        );
        UNPROTECT(2);
        break;
      }
      case R_DotTypeMissing:
        break;
      }
    }
    return;
  }

  const R_BindingType_t type = R_GetBindingType(symbol, environment);
  switch (type) {
  case R_BindingTypeValue: {
    SEXP value = PROTECT(R_getVar(symbol, environment, FALSE));
    schedule_node(walker, value, path);
    UNPROTECT(1);
    return;
  }
  case R_BindingTypeDelayed: {
    SEXP expression = PROTECT(R_DelayedBindingExpression(
      symbol,
      environment
    ));
    SEXP evaluation_environment = PROTECT(R_DelayedBindingEnvironment(
      symbol,
      environment
    ));
    schedule_node(
      walker,
      evaluation_environment,
      literal_path(path, ".promise.environment")
    );
    schedule_node(
      walker,
      expression,
      literal_path(path, ".promise.expression")
    );
    UNPROTECT(2);
    return;
  }
  case R_BindingTypeForced: {
    SEXP expression = PROTECT(R_ForcedBindingExpression(
      symbol,
      environment
    ));
    SEXP value = PROTECT(R_getVar(symbol, environment, FALSE));
    schedule_node(
      walker,
      value,
      literal_path(path, ".promise.value")
    );
    schedule_node(
      walker,
      expression,
      literal_path(path, ".promise.expression")
    );
    UNPROTECT(2);
    return;
  }
  case R_BindingTypeActive:
    Rf_error("Object graph binding changed during inspection");
  case R_BindingTypeUnbound:
  case R_BindingTypeMissing:
    return;
  }
  Rf_error("Internal error: unknown R binding type");
#else
  SEXP value = PROTECT(paradox_api_stored_binding_snapshot(
    environment,
    symbol
  ));
  if (value != R_UnboundValue) {
    if (TYPEOF(value) == PROMSXP) {
      schedule_promise_edges(walker, value, path);
    } else {
      schedule_node(walker, value, path);
    }
  }
  UNPROTECT(1);
#endif
}

static void schedule_environment(
    paradox_upgrade_walker_t *walker,
    SEXP environment,
    const paradox_upgrade_path_t *path) {
  if (environment_boundary(walker, environment)) return;

  if (candidate_kind(environment) && !current_param_set_shell(environment)) {
    append_candidate(
      &walker->candidates,
      environment,
      path
    );
  }

  SEXP parent = PROTECT(parent_environment(environment));
  schedule_node(
    walker,
    parent,
    literal_path(path, ".parent")
  );
  UNPROTECT(1);

  SEXP names = PROTECT(environment_names(environment));
  for (R_xlen_t index = XLENGTH(names); index > 0; --index) {
    SEXP name = STRING_ELT(names, index - 1);
    if (name == NA_STRING) {
      UNPROTECT(1);
      Rf_error("Internal error: missing environment binding name");
    }
    SEXP symbol = Rf_installChar(name);
    schedule_binding(
      walker,
      environment,
      symbol,
      named_path(path, "[[\"", name, "\"]]")
    );
  }
  UNPROTECT(1);
}

typedef struct {
  paradox_upgrade_attribute_t *items;
  R_xlen_t count;
  R_xlen_t capacity;
} paradox_upgrade_attribute_map_t;

static void record_attribute(SEXP tag, SEXP value, void *data) {
  paradox_upgrade_attribute_map_t *map = data;
  if (map->count >= map->capacity) {
    Rf_error("Internal error: attribute count changed during inspection");
  }
  map->items[map->count++] =
    (paradox_upgrade_attribute_t) {tag, value};
}

static void schedule_attributes(
    paradox_upgrade_walker_t *walker,
    SEXP node,
    const paradox_upgrade_path_t *path) {
  const R_xlen_t count = paradox_api_stored_attribute_count(node);
  if (count == 0) return;
  paradox_upgrade_attribute_t *attributes = paradox_temporary_alloc(
    count,
    sizeof(*attributes)
  );
  paradox_upgrade_attribute_map_t map = {attributes, 0, count};
  paradox_api_map_stored_attributes(node, record_attribute, &map);
  if (map.count != count) {
    Rf_error("Internal error: attribute count changed during inspection");
  }

  for (R_xlen_t index = count; index > 0; --index) {
    const paradox_upgrade_attribute_t attribute = attributes[index - 1];
    const paradox_upgrade_path_t *attribute_path =
      TYPEOF(attribute.tag) == SYMSXP
        ? named_path(
            path,
            "@attr[[\"",
            PRINTNAME(attribute.tag),
            "\"]]"
          )
        : indexed_path(path, "@attributes[[", index - 1, "]]");
    schedule_node(walker, attribute.value, attribute_path);
  }
}

static void schedule_vector(
    paradox_upgrade_walker_t *walker,
    SEXP vector,
    const paradox_upgrade_path_t *path) {
  PROTECT(vector);
  SEXP source = vector;
  if (ALTREP(vector)) {
    source = PROTECT(Rf_duplicate(vector));
  }
  const R_xlen_t count = XLENGTH(source);
  for (R_xlen_t index = count; index > 0; --index) {
    schedule_node(
      walker,
      VECTOR_ELT(source, index - 1),
      indexed_path(path, "[[", index - 1, "]]")
    );
  }
  if (source != vector) UNPROTECT(1);
  UNPROTECT(1);
}

static SEXP closure_environment(SEXP closure) {
#if R_VERSION >= R_Version(4, 5, 0)
  return R_ClosureEnv(closure);
#else
  return evaluate_base_unary("environment", closure);
#endif
}

static void schedule_closure(
    paradox_upgrade_walker_t *walker,
    SEXP closure,
    const paradox_upgrade_path_t *path) {
  SEXP formals = PROTECT(paradox_api_closure_formals(closure));
  SEXP expression = PROTECT(R_ClosureExpr(closure));
  SEXP environment = PROTECT(closure_environment(closure));
  schedule_node(
    walker,
    environment,
    literal_path(path, ".environment")
  );
  schedule_node(
    walker,
    expression,
    literal_path(path, ".body")
  );
  schedule_node(
    walker,
    formals,
    literal_path(path, ".formals")
  );
  UNPROTECT(3);
}

static void schedule_pairlist(
    paradox_upgrade_walker_t *walker,
    SEXP cell,
    const paradox_upgrade_path_t *path) {
  schedule_node(walker, CDR(cell), literal_path(path, ".cdr"));
  schedule_node(walker, TAG(cell), literal_path(path, ".tag"));
  schedule_node(walker, CAR(cell), literal_path(path, ".car"));
}

static void inspect_node(
    paradox_upgrade_walker_t *walker,
    paradox_upgrade_work_t work) {
  SEXP node = work.node;
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(node);
  if (type == ENVSXP && environment_boundary(walker, node)) return;

  /* Attributes, including S4 slots, are ordinary graph edges. Schedule them
   * before primary children so LIFO processing visits primary structure first. */
  schedule_attributes(walker, node, work.path);

  switch (type) {
  case VECSXP:
  case EXPRSXP:
    schedule_vector(walker, node, work.path);
    return;
  case LISTSXP:
  case LANGSXP:
  case DOTSXP:
    schedule_pairlist(walker, node, work.path);
    return;
  case ENVSXP:
    schedule_environment(walker, node, work.path);
    return;
  case CLOSXP:
    schedule_closure(walker, node, work.path);
    return;
  case PROMSXP:
#if R_VERSION < R_Version(4, 6, 0)
    schedule_promise_edges(walker, node, work.path);
#endif
    return;
  case BCODESXP: {
    SEXP expression = PROTECT(R_BytecodeExpr(node));
    schedule_node(
      walker,
      expression,
      literal_path(work.path, ".expression")
    );
    UNPROTECT(1);
    return;
  }
  case EXTPTRSXP:
    if (paradox_core_is_canonical(node)) {
      SEXP payload = PROTECT(R_ExternalPtrProtected(node));
      schedule_node(
        walker,
        payload,
        literal_path(work.path, ".protected")
      );
      UNPROTECT(1);
    }
    return;
  case WEAKREFSXP:
    return;
  default:
    return;
  }
}

static SEXP build_result(const paradox_upgrade_walker_t *walker) {
  if (walker->candidates.size > (size_t) R_XLEN_T_MAX) {
    Rf_error("Too many Paradox objects were found");
  }
  const R_xlen_t count = (R_xlen_t) walker->candidates.size;
  SEXP objects = PROTECT(Rf_allocVector(VECSXP, count));
  SEXP paths = PROTECT(Rf_allocVector(STRSXP, count));
  for (R_xlen_t index = 0; index < count; ++index) {
    const paradox_upgrade_candidate_t *candidate =
      &walker->candidates.items[(size_t) index];
    SET_VECTOR_ELT(objects, index, candidate->shell);
    SET_STRING_ELT(paths, index, render_path(candidate->path));
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_VECTOR_ELT(result, 0, objects);
  SET_VECTOR_ELT(result, 1, paths);
  SET_STRING_ELT(names, 0, Rf_mkChar("objects"));
  SET_STRING_ELT(names, 1, Rf_mkChar("paths"));
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(4);
  return result;
}

SEXP paradox_upgrade_graph_discover(SEXP root) {
  PROTECT(root);
  paradox_upgrade_walker_t walker = {0};

  walker.seen.capacity = 1024;
  walker.seen.keys = temporary_size_alloc(
    walker.seen.capacity,
    sizeof(*walker.seen.keys)
  );
  memset(
    walker.seen.keys,
    0,
    walker.seen.capacity * sizeof(*walker.seen.keys)
  );
  walker.seen.root_capacity = 1024;
  PROTECT_WITH_INDEX(
    walker.seen.roots = Rf_allocVector(
      VECSXP,
      walker.seen.root_capacity
    ),
    &walker.seen.roots_index
  );

  walker.stack.capacity = 1024;
  walker.stack.items = temporary_size_alloc(
    walker.stack.capacity,
    sizeof(*walker.stack.items)
  );
  PROTECT_WITH_INDEX(
    walker.stack.roots = Rf_allocVector(
      VECSXP,
      (R_xlen_t) walker.stack.capacity
    ),
    &walker.stack.roots_index
  );
  walker.candidates.capacity = 16;
  walker.candidates.items = temporary_size_alloc(
    walker.candidates.capacity,
    sizeof(*walker.candidates.items)
  );
  walker.boundaries.capacity = 32;
  walker.boundaries.items = temporary_size_alloc(
    walker.boundaries.capacity,
    sizeof(*walker.boundaries.items)
  );
  initialize_search_boundaries(&walker.boundaries);

  const paradox_upgrade_path_t *root_path = literal_path(NULL, "x");
  schedule_node(&walker, root, root_path);
  while (walker.stack.size != 0) {
    account_work(&walker);
    const paradox_upgrade_work_t work = pop_node(&walker.stack);
    if (remember_node(&walker.seen, work.node)) {
      inspect_node(&walker, work);
    }
  }

  SEXP result = PROTECT(build_result(&walker));
  UNPROTECT(4);
  return result;
}
