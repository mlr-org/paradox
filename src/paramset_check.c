#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "paradox.h"

#include <R_ext/Arith.h>
#include <R_ext/Memory.h>
#include <R_ext/Utils.h>

#include "builtin_value.h"
#include "builtin_condition.h"
#include "core_state.h"
#include "domain_admission.h"
#include "paramset_activity.h"
#include "paramset_domain_common.h"
#include "paramset_shadow.h"
#include "parameter_suggestion.h"
#include "r_api_compat.h"
#include "r_utils.h"
#include "shell_auth.h"

/*
 * ParamSet checking is deliberately one engine.  The R6 object is only the
 * operation boundary: this translation unit snapshots the complete core graph
 * and every callback before invoking user code, then uses the same row kernel
 * for list and data-frame inputs.  There is no R replay path and no NULL
 * admission sentinel for a supported current capsule.
 */

typedef enum {
  VALUE_DBL = 0,
  VALUE_INT,
  VALUE_FCT,
  VALUE_LGL,
  VALUE_UTY
} value_kind_t;

typedef enum {
  PRESENCE_NONE = 0,
  PRESENCE_ALL,
  PRESENCE_REQUIRED
} presence_t;

typedef struct {
  value_kind_t kind;
  double lower;
  double upper;
  double tolerance;
  SEXP id;
  SEXP levels;
  SEXP special_values;
  SEXP custom_check;
} value_spec_t;

static SEXP validate_tune_token(const value_spec_t *spec, SEXP token,
  R_xlen_t *work_since_interrupt, SEXP *receipt_result);

typedef struct {
  uint64_t hash;
  R_xlen_t row_plus_one;
} id_slot_t;

typedef struct {
  id_slot_t *slots;
  R_xlen_t capacity;
  SEXP ids;
} id_map_t;

enum node_root_slot {
  NODE_ROOT_SELF = 0,
  NODE_ROOT_CLASS,
  NODE_ROOT_ENCLOSURE,
  NODE_ROOT_PRIVATE,
  NODE_ROOT_ASSERT_VALUES,
  NODE_ROOT_SELECTED_CORE,
  NODE_ROOT_CORE,
  NODE_ROOT_STATE,
  NODE_ROOT_PARAMS,
  NODE_ROOT_VALUES,
  NODE_ROOT_TAGS,
  NODE_ROOT_DEPS,
  NODE_ROOT_TRAFOS,
  NODE_ROOT_EXTRA_TRAFO,
  NODE_ROOT_CONSTRAINT,
  NODE_ROOT_SETS,
  NODE_ROOT_SET_NAMES,
  NODE_ROOT_TRANSLATION,
  NODE_ROOT_ROOT_IDS,
  NODE_ROOT_COUNT
};

typedef struct {
  SEXP roots;
  SEXP self;
  SEXP classes;
  SEXP enclosure;
  SEXP private_environment;
  SEXP assert_values;
  SEXP selected_core;
  SEXP state;
  SEXP params;
  SEXP values;
  SEXP tags;
  SEXP dependencies;
  SEXP trafos;
  SEXP extra_trafo;
  SEXP constraint;
  SEXP sets;
  SEXP set_names;
  SEXP translation;
  SEXP root_ids;
  paradox_core_kind_t kind;
  paradox_domain_params_t checked_params;
  paradox_domain_values_t checked_values;
  paradox_domain_tags_t checked_tags;
  paradox_domain_trafos_t checked_trafos;
  paradox_domain_dependencies_t checked_dependencies;
  R_xlen_t parent;
  R_xlen_t child_position;
  R_xlen_t next_child;
  /* TRUE once this node or anything below it exposes a parameter, a
   * dependency, a stored value, or a callback.  A shared node whose whole
   * subtree is barren has nothing that can differ between two occurrences of
   * it, so the builder stops re-expanding it; see `barren_registry_t`. */
  int subtree_contributes;
  int postfix;
  int semantic;
} check_node_t;

typedef struct {
  check_node_t *nodes;
  R_xlen_t *path;
  R_xlen_t count;
  R_xlen_t capacity;
} check_graph_t;

typedef struct {
  check_graph_t graph;
  value_spec_t *specs;
  id_map_t root_ids;
  R_xlen_t parameter_count;
  SEXP root_params;
  SEXP root_tags;
  SEXP defaults;
  int activity_mapping_ready;
  R_xlen_t dependency_count;
  unsigned char *has_dependency;
  R_xlen_t *dependency_child;
  R_xlen_t *dependency_parent;
  SEXP *dependency_condition;
  SEXP *dependency_rhs;
  SEXP *dependency_exposed_id;
  SEXP *dependency_exposed_on;
} check_plan_t;

typedef struct {
  SEXP values;
  SEXP names;
  R_xlen_t size;
  R_xlen_t *param_rows;
  R_xlen_t *value_for_param;
} point_t;

typedef struct {
  int ready;
  int retain_reasons;
  paradox_activity_result_t result;
} point_activity_t;

#if defined(R_PRINTF_FORMAT)
# define PARADOX_PRINTF_FORMAT(format_index, first_argument) \
    R_PRINTF_FORMAT(format_index, first_argument)
#elif defined(__GNUC__) || defined(__clang__)
# define PARADOX_PRINTF_FORMAT(format_index, first_argument) \
    __attribute__((format(printf, format_index, first_argument)))
#else
# define PARADOX_PRINTF_FORMAT(format_index, first_argument)
#endif

static SEXP check_message(const char *format, ...)
  PARADOX_PRINTF_FORMAT(1, 2);

static SEXP check_message(const char *format, ...) {
  /*
   * Rtools35 ultimately targets the historical MSVCRT formatter.  Its
   * `vsnprintf` compatibility path is not required to implement the C99
   * null-buffer sizing extension, and returns -1 rather than the required
   * length when an actual buffer is too small.  Render into a real buffer on
   * every attempt and handle both return conventions.
   *
   * Every current caller emits a short package-owned diagnostic.  The generous
   * ceiling therefore changes no supported message, while bounding a repeated
   * negative return caused by an encoding/formatter failure instead of growing
   * until R exhausts the process address space.  The small local first buffer
   * also keeps ordinary failure formatting allocation-free until mkChar.
   */
  enum {
    INITIAL_CAPACITY = 32,
    MAXIMUM_CAPACITY = 65536
  };
  char local_buffer[INITIAL_CAPACITY];
  char *buffer = local_buffer;
  size_t capacity = sizeof(local_buffer);

  va_list arguments;
  va_start(arguments, format);
  for (;;) {
    va_list attempt;
    va_copy(attempt, arguments);
    const int written = vsnprintf(buffer, capacity, format, attempt);
    va_end(attempt);

    if (written >= 0 && (size_t) written < capacity) {
      va_end(arguments);
      return Rf_mkString(buffer);
    }

    size_t next_capacity;
    if (written >= 0) {
      const size_t required = (size_t) written + 1U;
      if (required <= capacity || required > (size_t) MAXIMUM_CAPACITY) {
        va_end(arguments);
        Rf_error("Internal error while formatting a ParamSet diagnostic");
      }
      next_capacity = required;
    } else {
      if (capacity >= (size_t) MAXIMUM_CAPACITY) {
        va_end(arguments);
        Rf_error("Internal error while formatting a ParamSet diagnostic");
      }
      next_capacity = capacity > (size_t) MAXIMUM_CAPACITY / 2U
        ? (size_t) MAXIMUM_CAPACITY
        : capacity * 2U;
    }

    buffer = R_alloc(next_capacity, sizeof(*buffer));
    capacity = next_capacity;
  }
}

static SEXP format_public_number(double value) {
  if (value == R_PosInf) return Rf_mkString("Inf");
  if (value == R_NegInf) return Rf_mkString("-Inf");
  return check_message("%g", value);
}

static SEXP utf8_message_1(const char *prefix, SEXP first,
    const char *suffix) {
  SEXP safe_first = PROTECT(paradox_diagnostic_charsxp(first));
  paradox_utf8_piece_t pieces[] = {
    paradox_utf8_ascii_piece(prefix),
    paradox_utf8_charsxp_piece(safe_first),
    paradox_utf8_ascii_piece(suffix)
  };
  SEXP result = PROTECT(paradox_utf8_message(pieces, 3));
  UNPROTECT(2);
  return result;
}

static SEXP utf8_message_2(const char *prefix, SEXP first,
    const char *middle, SEXP second, const char *suffix) {
  SEXP safe_first = PROTECT(paradox_diagnostic_charsxp(first));
  SEXP safe_second = PROTECT(paradox_diagnostic_charsxp(second));
  paradox_utf8_piece_t pieces[] = {
    paradox_utf8_ascii_piece(prefix),
    paradox_utf8_charsxp_piece(safe_first),
    paradox_utf8_ascii_piece(middle),
    paradox_utf8_charsxp_piece(safe_second),
    paradox_utf8_ascii_piece(suffix)
  };
  SEXP result = PROTECT(paradox_utf8_message(pieces, 5));
  UNPROTECT(3);
  return result;
}

static SEXP utf8_message_3(const char *prefix, SEXP first,
    const char *middle_1, SEXP second, const char *middle_2, SEXP third,
    const char *suffix) {
  SEXP safe_first = PROTECT(paradox_diagnostic_charsxp(first));
  SEXP safe_second = PROTECT(paradox_diagnostic_charsxp(second));
  SEXP safe_third = PROTECT(paradox_diagnostic_charsxp(third));
  paradox_utf8_piece_t pieces[] = {
    paradox_utf8_ascii_piece(prefix),
    paradox_utf8_charsxp_piece(safe_first),
    paradox_utf8_ascii_piece(middle_1),
    paradox_utf8_charsxp_piece(safe_second),
    paradox_utf8_ascii_piece(middle_2),
    paradox_utf8_charsxp_piece(safe_third),
    paradox_utf8_ascii_piece(suffix)
  };
  SEXP result = PROTECT(paradox_utf8_message(pieces, 7));
  UNPROTECT(4);
  return result;
}

static SEXP utf8_message_4(const char *prefix, SEXP first,
    const char *middle_1, SEXP second, const char *middle_2, SEXP third,
    const char *middle_3, SEXP fourth, const char *suffix) {
  SEXP safe_first = PROTECT(paradox_diagnostic_charsxp(first));
  SEXP safe_second = PROTECT(paradox_diagnostic_charsxp(second));
  SEXP safe_third = PROTECT(paradox_diagnostic_charsxp(third));
  SEXP safe_fourth = PROTECT(paradox_diagnostic_charsxp(fourth));
  paradox_utf8_piece_t pieces[] = {
    paradox_utf8_ascii_piece(prefix),
    paradox_utf8_charsxp_piece(safe_first),
    paradox_utf8_ascii_piece(middle_1),
    paradox_utf8_charsxp_piece(safe_second),
    paradox_utf8_ascii_piece(middle_2),
    paradox_utf8_charsxp_piece(safe_third),
    paradox_utf8_ascii_piece(middle_3),
    paradox_utf8_charsxp_piece(safe_fourth),
    paradox_utf8_ascii_piece(suffix)
  };
  SEXP result = PROTECT(paradox_utf8_message(pieces, 9));
  UNPROTECT(5);
  return result;
}

static int exact_flag(SEXP value, const char *name) {
  if (TYPEOF(value) != LGLSXP || ALTREP(value) || Rf_isS4(value) ||
      XLENGTH(value) != 1 ||
      LOGICAL_ELT(value, 0) == NA_LOGICAL) {
    Rf_error("`%s` must be TRUE or FALSE", name);
  }
  return LOGICAL_ELT(value, 0);
}

static presence_t exact_presence(SEXP value) {
  if (TYPEOF(value) != STRSXP || ALTREP(value) || Rf_isS4(value) ||
      XLENGTH(value) != 1 ||
      STRING_ELT(value, 0) == NA_STRING) {
    Rf_error("`presence` must be one of 'none', 'all', or 'required'");
  }
  const char *text = CHAR(STRING_ELT(value, 0));
  if (strcmp(text, "none") == 0) return PRESENCE_NONE;
  if (strcmp(text, "all") == 0) return PRESENCE_ALL;
  if (strcmp(text, "required") == 0) return PRESENCE_REQUIRED;
  Rf_error("`presence` must be one of 'none', 'all', or 'required'");
  return PRESENCE_NONE;
}

static int no_attributes(SEXP value) {
  return paradox_api_has_no_attributes(value);
}

static SEXP named_list_element(SEXP value, const char *target) {
  if (TYPEOF(value) != VECSXP || ALTREP(value)) return R_UnboundValue;
  SEXP names = PROTECT(Rf_getAttrib(value, R_NamesSymbol));
  if (TYPEOF(names) != STRSXP || ALTREP(names) ||
      XLENGTH(names) != XLENGTH(value)) {
    UNPROTECT(1);
    return R_UnboundValue;
  }
  SEXP result = R_UnboundValue;
  int matches = 0;
  for (R_xlen_t index = 0; index < XLENGTH(value); ++index) {
    SEXP name = STRING_ELT(names, index);
    if (name != NA_STRING && strcmp(CHAR(name), target) == 0) {
      result = VECTOR_ELT(value, index);
      ++matches;
    }
  }
  UNPROTECT(1);
  return matches == 1 ? result : R_UnboundValue;
}

static uint64_t hash_bytes(const unsigned char *text, uint64_t hash) {
  while (*text != '\0') {
    hash ^= (uint64_t) *text;
    hash *= UINT64_C(1099511628211);
    ++text;
  }
  return hash;
}

static uint64_t hash_string(SEXP string) {
  uint64_t hash = UINT64_C(14695981039346656037);
  if (Rf_getCharCE(string) == CE_BYTES) {
    hash ^= UINT64_C(0xff);
    hash *= UINT64_C(1099511628211);
    return hash_bytes((const unsigned char *) CHAR(string), hash);
  }
  PROTECT(string);
  const void *vmax = vmaxget();
  const char *text = Rf_translateCharUTF8(string);
  hash = hash_bytes((const unsigned char *) text, hash);
  vmaxset(vmax);
  UNPROTECT(1);
  return hash;
}

static void initialize_id_map(SEXP ids, id_map_t *map) {
  const R_xlen_t size = XLENGTH(ids);
  if (size > R_XLEN_T_MAX / 2) {
    Rf_error("ParamSet contains too many parameters");
  }
  R_xlen_t capacity = 1;
  const R_xlen_t needed = size == 0 ? 1 : size * 2;
  while (capacity < needed) {
    if (capacity > R_XLEN_T_MAX / 2) {
      Rf_error("ParamSet identifier index exceeds platform bounds");
    }
    capacity *= 2;
  }
  id_slot_t *slots = paradox_temporary_alloc(capacity, sizeof(*slots));
  memset(slots, 0, (size_t) capacity * sizeof(*slots));
  const R_xlen_t mask = capacity - 1;
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t row = 0; row < size; ++row) {
    paradox_account_work(&work_since_interrupt);
    SEXP id = STRING_ELT(ids, row);
    const uint64_t hash = hash_string(id);
    R_xlen_t slot = (R_xlen_t) (hash & (uint64_t) mask);
    while (slots[slot].row_plus_one != 0) {
      const R_xlen_t present = slots[slot].row_plus_one - 1;
      if (slots[slot].hash == hash && paradox_domain_strings_equal(
          STRING_ELT(ids, present), id
        )) {
        Rf_error("Corrupt ParamSet state: duplicate parameter identifier");
      }
      slot = (slot + 1) & mask;
    }
    slots[slot].hash = hash;
    slots[slot].row_plus_one = row + 1;
  }
  map->slots = slots;
  map->capacity = capacity;
  map->ids = ids;
}

static int find_id(const id_map_t *map, SEXP id, R_xlen_t *row,
    R_xlen_t *work_since_interrupt) {
  if (id == NA_STRING) return FALSE;
  const uint64_t hash = hash_string(id);
  const R_xlen_t mask = map->capacity - 1;
  R_xlen_t slot = (R_xlen_t) (hash & (uint64_t) mask);
  while (map->slots[slot].row_plus_one != 0) {
    paradox_account_work(work_since_interrupt);
    const R_xlen_t present = map->slots[slot].row_plus_one - 1;
    if (map->slots[slot].hash == hash && paradox_domain_strings_equal(
        STRING_ELT(map->ids, present), id
      )) {
      *row = present;
      return TRUE;
    }
    slot = (slot + 1) & mask;
  }
  return FALSE;
}

static R_xlen_t local_param_row(const check_node_t *node, SEXP id,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t row = 0; row < node->checked_params.row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    if (paradox_domain_strings_equal(
        STRING_ELT(node->checked_params.ids, row), id
      )) return row;
  }
  return R_XLEN_T_MAX;
}

/* A dependency parent that names nothing in its own node is resolved outward
 * through the enclosing namespaces, then looked up in the root's exposed
 * table. This is the same walk the dependency getter performs
 * (`paradox_collection_translate_dependency_id`), expressed on the check
 * graph's precomputed root spellings; the two must agree, because a collection
 * that answers `$deps` with a resolved parent has to enforce it as well. A name
 * no ancestor knows stays verbatim and is left to the root lookup, so a
 * dangling parent that a later sibling supplies starts being enforced at the
 * next read. */
static SEXP outward_dependency_parent(const check_graph_t *graph,
    R_xlen_t node_index, SEXP on, R_xlen_t *work_since_interrupt) {
  for (R_xlen_t ancestor = graph->nodes[node_index].parent;
      ancestor != R_XLEN_T_MAX;
      ancestor = graph->nodes[ancestor].parent) {
    const R_xlen_t row = local_param_row(
      &graph->nodes[ancestor],
      on,
      work_since_interrupt
    );
    if (row != R_XLEN_T_MAX) {
      return STRING_ELT(graph->nodes[ancestor].root_ids, row);
    }
  }
  return on;
}

static SEXP append_node_roots(SEXP *root_plan,
    PROTECT_INDEX root_plan_index, SEXP self, SEXP private_environment) {
  PROTECT(self);
  PROTECT(private_environment);
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, NODE_ROOT_COUNT));
  SET_VECTOR_ELT(roots, NODE_ROOT_SELF, self);
  SET_VECTOR_ELT(roots, NODE_ROOT_PRIVATE, private_environment);
  SEXP expanded = PROTECT(Rf_cons(roots, *root_plan));
  REPROTECT(expanded, root_plan_index);
  *root_plan = expanded;
  UNPROTECT(4);
  return roots;
}

static SEXP check_enclosure_symbol = NULL;
static SEXP check_self_symbol = NULL;
static SEXP check_private_symbol = NULL;
static SEXP check_core_symbol = NULL;
static SEXP check_assert_values_symbol = NULL;

static void initialize_check_binding_symbols(void) {
  if (check_enclosure_symbol != NULL) return;
  check_enclosure_symbol = Rf_install(".__enclos_env__");
  check_self_symbol = Rf_install("self");
  check_private_symbol = Rf_install("private");
  check_core_symbol = Rf_install(".core");
  check_assert_values_symbol = Rf_install("assert_values");
}

static void initialize_graph(check_graph_t *graph) {
  graph->capacity = 8;
  graph->nodes = paradox_temporary_alloc(
    graph->capacity, sizeof(*graph->nodes)
  );
  graph->path = paradox_temporary_alloc(
    graph->capacity, sizeof(*graph->path)
  );
  graph->count = 0;
}

static void reserve_graph(check_graph_t *graph) {
  if (graph->count < graph->capacity) return;
  if (graph->capacity > R_XLEN_T_MAX / 2) {
    Rf_error("ParamSet graph exceeds platform bounds");
  }
  const R_xlen_t capacity = graph->capacity * 2;
  check_node_t *nodes = paradox_temporary_alloc(capacity, sizeof(*nodes));
  R_xlen_t *path = paradox_temporary_alloc(capacity, sizeof(*path));
  memcpy(nodes, graph->nodes, (size_t) graph->count * sizeof(*nodes));
  memcpy(path, graph->path, (size_t) graph->count * sizeof(*path));
  graph->nodes = nodes;
  graph->path = path;
  graph->capacity = capacity;
}

static int exact_sets(SEXP sets, SEXP *names,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isObject(sets)) {
    return FALSE;
  }
  SEXP observed_names = PROTECT(Rf_getAttrib(sets, R_NamesSymbol));
  if (TYPEOF(observed_names) != STRSXP || ALTREP(observed_names) ||
      !no_attributes(observed_names) ||
      XLENGTH(observed_names) != XLENGTH(sets)) {
    UNPROTECT(1);
    return FALSE;
  }
  for (R_xlen_t right = 0; right < XLENGTH(sets); ++right) {
    paradox_account_work(work_since_interrupt);
    SEXP right_name = STRING_ELT(observed_names, right);
    if (right_name == NA_STRING || Rf_getCharCE(right_name) == CE_BYTES) {
      UNPROTECT(1);
      return FALSE;
    }
    if (CHAR(right_name)[0] == '\0') continue;
    for (R_xlen_t left = 0; left < right; ++left) {
      if (paradox_domain_strings_equal(
          STRING_ELT(observed_names, left), right_name
        )) {
        UNPROTECT(1);
        return FALSE;
      }
    }
  }
  *names = observed_names;
  UNPROTECT(1);
  return TRUE;
}

static int exact_translation(SEXP translation,
    const paradox_domain_params_t *params, SEXP sets, SEXP set_names,
    int postfix, R_xlen_t *work_since_interrupt) {
  static const char *const names[] = {
    "id", "original_id", "owner_ps_index", "owner_name"
  };
  R_xlen_t rows = 0;
  if (!paradox_domain_exact_plain_table(
      translation, names, 4, &rows, work_since_interrupt
    ) || rows != params->row_count) {
    return FALSE;
  }
  SEXP ids = PROTECT(VECTOR_ELT(translation, 0));
  SEXP originals = PROTECT(VECTOR_ELT(translation, 1));
  SEXP owners = PROTECT(VECTOR_ELT(translation, 2));
  SEXP owner_names = PROTECT(VECTOR_ELT(translation, 3));
  int valid = TYPEOF(ids) == STRSXP && TYPEOF(originals) == STRSXP &&
    TYPEOF(owners) == INTSXP && TYPEOF(owner_names) == STRSXP &&
    !ALTREP(ids) && !ALTREP(originals) && !ALTREP(owners) &&
    !ALTREP(owner_names) && no_attributes(ids) && no_attributes(originals) &&
    no_attributes(owners) && no_attributes(owner_names) &&
    Rf_any_duplicated(ids, FALSE) == 0;
  id_map_t parameter_ids;
  if (valid) {
    initialize_id_map(params->ids, &parameter_ids);
  }
  for (R_xlen_t row = 0; valid && row < rows; ++row) {
    paradox_account_work(work_since_interrupt);
    SEXP id = STRING_ELT(ids, row);
    SEXP original = STRING_ELT(originals, row);
    SEXP owner_name = STRING_ELT(owner_names, row);
    const int owner = INTEGER_ELT(owners, row);
    R_xlen_t parameter_row = R_XLEN_T_MAX;
    if (id == NA_STRING || original == NA_STRING || owner_name == NA_STRING ||
        owner == NA_INTEGER || owner < 1 || owner > XLENGTH(sets) ||
        !find_id(
          &parameter_ids, id, &parameter_row, work_since_interrupt
        ) ||
        !paradox_domain_strings_equal(
          owner_name, STRING_ELT(set_names, owner - 1)
        )) {
      valid = FALSE;
      break;
    }
    const char *owner_text = CHAR(owner_name);
    const char *original_text = CHAR(original);
    const char *id_text = CHAR(id);
    if (owner_text[0] == '\0') {
      valid = strcmp(id_text, original_text) == 0;
      continue;
    }
    const size_t owner_size = strlen(owner_text);
    const size_t original_size = strlen(original_text);
    if (owner_size > SIZE_MAX - original_size - 2U) {
      valid = FALSE;
      break;
    }
    const size_t expected_size = owner_size + original_size + 1U;
    if (strlen(id_text) != expected_size) {
      valid = FALSE;
      break;
    }
    if (postfix) {
      valid = memcmp(id_text, original_text, original_size) == 0 &&
        id_text[original_size] == '.' &&
        memcmp(id_text + original_size + 1U, owner_text, owner_size) == 0;
    } else {
      valid = memcmp(id_text, owner_text, owner_size) == 0 &&
        id_text[owner_size] == '.' &&
        memcmp(id_text + owner_size + 1U, original_text, original_size) == 0;
    }
  }
  UNPROTECT(4);
  return valid;
}

static int supported_value_kind(SEXP cls, SEXP storage,
    value_kind_t *result) {
  if (paradox_domain_string_is(cls, "ParamDbl") &&
      paradox_domain_string_is(storage, "numeric")) {
    *result = VALUE_DBL;
    return TRUE;
  }
  if (paradox_domain_string_is(cls, "ParamInt") &&
      paradox_domain_string_is(storage, "integer")) {
    *result = VALUE_INT;
    return TRUE;
  }
  if (paradox_domain_string_is(cls, "ParamFct") &&
      paradox_domain_string_is(storage, "character")) {
    *result = VALUE_FCT;
    return TRUE;
  }
  if (paradox_domain_string_is(cls, "ParamLgl") &&
      paradox_domain_string_is(storage, "logical")) {
    *result = VALUE_LGL;
    return TRUE;
  }
  if (paradox_domain_string_is(cls, "ParamUty") &&
      paradox_domain_string_is(storage, "list")) {
    *result = VALUE_UTY;
    return TRUE;
  }
  return FALSE;
}

static value_kind_t value_kind(SEXP cls, SEXP storage) {
  value_kind_t result;
  if (supported_value_kind(cls, storage, &result)) return result;
  Rf_error("Corrupt ParamSet state: unsupported built-in parameter kind");
  return VALUE_UTY;
}

static SEXP utility_callback(SEXP cargo) {
  if (cargo == R_NilValue) return R_NilValue;
  if (TYPEOF(cargo) != VECSXP || ALTREP(cargo)) {
    Rf_error("Corrupt ParamSet state: ParamUty cargo must be a list or NULL");
  }
  SEXP callback = named_list_element(cargo, "custom_check");
  if (callback == R_UnboundValue) callback = R_NilValue;
  if (callback != R_NilValue && !Rf_isFunction(callback)) {
    Rf_error("Corrupt ParamSet state: ParamUty custom_check must be a function or NULL");
  }
  return callback;
}

static int semantic_value_is_ordinary(SEXP value) {
  R_xlen_t capacity = 16;
  R_xlen_t size = 1;
  R_xlen_t seen_count = 0;
  SEXP *stack = paradox_temporary_alloc(capacity, sizeof(*stack));
  SEXP *seen = paradox_temporary_alloc(capacity, sizeof(*seen));
  stack[0] = value;
  R_xlen_t work_since_interrupt = 0;
  while (size != 0) {
    paradox_account_work(&work_since_interrupt);
    SEXP current = stack[--size];
    if (ALTREP(current)) return FALSE;
    if (TYPEOF(current) != VECSXP) continue;
    int visited = FALSE;
    for (R_xlen_t index = 0; index < seen_count; ++index) {
      if (seen[index] == current) {
        visited = TRUE;
        break;
      }
    }
    if (visited) continue;
    if (seen_count == capacity) {
      if (capacity > R_XLEN_T_MAX / 2) return FALSE;
      const R_xlen_t expanded = capacity * 2;
      SEXP *new_stack = paradox_temporary_alloc(expanded, sizeof(*new_stack));
      SEXP *new_seen = paradox_temporary_alloc(expanded, sizeof(*new_seen));
      memcpy(new_stack, stack, (size_t) size * sizeof(*stack));
      memcpy(new_seen, seen, (size_t) seen_count * sizeof(*seen));
      stack = new_stack;
      seen = new_seen;
      capacity = expanded;
    }
    seen[seen_count++] = current;
    const R_xlen_t children = XLENGTH(current);
    if (children > R_XLEN_T_MAX - size) return FALSE;
    while (size + children > capacity) {
      if (capacity > R_XLEN_T_MAX / 2) return FALSE;
      const R_xlen_t expanded = capacity * 2;
      SEXP *new_stack = paradox_temporary_alloc(expanded, sizeof(*new_stack));
      SEXP *new_seen = paradox_temporary_alloc(expanded, sizeof(*new_seen));
      memcpy(new_stack, stack, (size_t) size * sizeof(*stack));
      memcpy(new_seen, seen, (size_t) seen_count * sizeof(*seen));
      stack = new_stack;
      seen = new_seen;
      capacity = expanded;
    }
    for (R_xlen_t index = 0; index < children; ++index) {
      stack[size++] = VECTOR_ELT(current, index);
    }
  }
  return TRUE;
}

static void validate_node_schema(check_node_t *node,
    R_xlen_t *work_since_interrupt) {
  SEXP params = node->params;
  SEXP classes = VECTOR_ELT(params, PARADOX_DOMAIN_CLS);
  SEXP cargos = VECTOR_ELT(params, PARADOX_DOMAIN_CARGO);
  SEXP lower = VECTOR_ELT(params, PARADOX_DOMAIN_LOWER);
  SEXP upper = VECTOR_ELT(params, PARADOX_DOMAIN_UPPER);
  SEXP tolerance = VECTOR_ELT(params, PARADOX_DOMAIN_TOLERANCE);
  SEXP storage = VECTOR_ELT(params, PARADOX_DOMAIN_STORAGE_TYPE);
  for (R_xlen_t row = 0; row < node->checked_params.row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    const value_kind_t kind = value_kind(
      STRING_ELT(classes, row), STRING_ELT(storage, row)
    );
    if (kind == VALUE_DBL || kind == VALUE_INT) {
      const double row_lower = paradox_numeric_elt(lower, row);
      const double row_upper = paradox_numeric_elt(upper, row);
      const double row_tolerance = paradox_numeric_elt(tolerance, row);
      if (ISNAN(row_lower) || ISNAN(row_upper) ||
          ISNAN(row_tolerance) || row_lower > row_upper ||
          !R_FINITE(row_tolerance) || row_tolerance < 0.0 ||
          (kind == VALUE_INT && row_tolerance > 0.5)) {
        Rf_error("Corrupt ParamSet state: invalid numeric bounds or tolerance");
      }
    }
    if (kind == VALUE_UTY) {
      (void) utility_callback(VECTOR_ELT(cargos, row));
    }
  }

  for (R_xlen_t row = 0; row < node->checked_tags.row_count; ++row) {
    if (local_param_row(
        node,
        STRING_ELT(node->checked_tags.ids, row),
        work_since_interrupt
      ) == R_XLEN_T_MAX) {
      Rf_error("Corrupt ParamSet state: tag refers to an unknown parameter");
    }
  }
  for (R_xlen_t row = 0; row < node->checked_values.size; ++row) {
    SEXP stored_id = STRING_ELT(node->checked_values.names, row);
    const R_xlen_t parameter_row = local_param_row(
      node, stored_id, work_since_interrupt
    );
    if (parameter_row == R_XLEN_T_MAX) {
      Rf_error("Corrupt ParamSet state: invalid stored parameter value");
    }
    SEXP stored_value = VECTOR_ELT(node->checked_values.values, row);
    const value_kind_t kind = value_kind(
      STRING_ELT(classes, parameter_row),
      STRING_ELT(storage, parameter_row)
    );
    /* ParamUty is deliberately opaque.  Its value shell and nested leaves can
     * include ALTREP vectors, environments, external pointers, and objects
     * whose identity belongs to the caller; the engine never indexes them.
     * TuneToken metadata remains semantic even when assigned to ParamUty and
     * therefore still has to be ordinary canonical storage. */
    if ((kind != VALUE_UTY || Rf_inherits(stored_value, "TuneToken")) &&
        !semantic_value_is_ordinary(stored_value)) {
      Rf_error("Corrupt ParamSet state: invalid stored parameter value");
    }
  }
  for (R_xlen_t row = 0; row < node->checked_trafos.row_count; ++row) {
    if (local_param_row(
        node,
        STRING_ELT(node->checked_trafos.ids, row),
        work_since_interrupt
      ) == R_XLEN_T_MAX) {
      Rf_error("Corrupt ParamSet state: transformation refers to an unknown parameter");
    }
  }
  if (Rf_any_duplicated(node->checked_trafos.ids, FALSE) != 0) {
    Rf_error("Corrupt ParamSet state: duplicate parameter transformation");
  }
  for (R_xlen_t row = 0;
      row < node->checked_dependencies.row_count; ++row) {
    if (local_param_row(
        node,
        STRING_ELT(node->checked_dependencies.ids, row),
        work_since_interrupt
      ) == R_XLEN_T_MAX) {
      Rf_error("Corrupt ParamSet state: dependency target is unknown");
    }
    paradox_builtin_condition_kind_t condition_kind;
    SEXP rhs = R_NilValue;
    if (!paradox_builtin_condition_exact(
        VECTOR_ELT(node->checked_dependencies.conditions, row),
        &condition_kind,
        &rhs,
        work_since_interrupt
      )) {
      Rf_error("Corrupt ParamSet state: malformed built-in dependency condition");
    }
  }
}

static SEXP child_root_ids(const check_graph_t *graph, R_xlen_t parent_index,
    R_xlen_t child_position, const paradox_domain_params_t *child_params,
    R_xlen_t *work_since_interrupt) {
  const check_node_t *parent = &graph->nodes[parent_index];
  SEXP translation = parent->translation;
  SEXP exposed = VECTOR_ELT(translation, 0);
  SEXP originals = VECTOR_ELT(translation, 1);
  SEXP owners = VECTOR_ELT(translation, 2);
  const R_xlen_t owner = child_position + 1;
  R_xlen_t owner_rows = 0;
  for (R_xlen_t row = 0; row < XLENGTH(exposed); ++row) {
    if (INTEGER_ELT(owners, row) == owner) ++owner_rows;
  }
  if (owner_rows != child_params->row_count) {
    Rf_error("Corrupt ParamSetCollection translation: child schema size mismatch");
  }

  SEXP result = PROTECT(Rf_allocVector(STRSXP, child_params->row_count));
  for (R_xlen_t child_row = 0;
      child_row < child_params->row_count; ++child_row) {
    paradox_account_work(work_since_interrupt);
    SEXP child_id = STRING_ELT(child_params->ids, child_row);
    R_xlen_t match = R_XLEN_T_MAX;
    for (R_xlen_t row = 0; row < XLENGTH(exposed); ++row) {
      if (INTEGER_ELT(owners, row) == owner &&
          paradox_domain_strings_equal(
            STRING_ELT(originals, row), child_id
          )) {
        if (match != R_XLEN_T_MAX) {
          UNPROTECT(1);
          Rf_error("Corrupt ParamSetCollection translation: duplicate child identifier");
        }
        match = row;
      }
    }
    if (match == R_XLEN_T_MAX) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSetCollection translation: missing child identifier");
    }
    R_xlen_t parent_row = R_XLEN_T_MAX;
    for (R_xlen_t row = 0; row < parent->checked_params.row_count; ++row) {
      if (paradox_domain_strings_equal(
          STRING_ELT(parent->checked_params.ids, row),
          STRING_ELT(exposed, match)
        )) {
        parent_row = row;
        break;
      }
    }
    if (parent_row == R_XLEN_T_MAX) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSetCollection translation: exposed identifier is unknown");
    }
    SET_STRING_ELT(result, child_row, STRING_ELT(parent->root_ids, parent_row));
  }
  UNPROTECT(1);
  return result;
}

static void initialize_node(SEXP self, SEXP private_environment,
    R_xlen_t parent, R_xlen_t child_position, int semantic,
    check_graph_t *graph,
    check_node_t *node, SEXP *root_plan, PROTECT_INDEX root_plan_index,
    R_xlen_t *work_since_interrupt, SEXP selected_core,
    int heal) {
  node->roots = append_node_roots(
    root_plan, root_plan_index, self, private_environment
  );
  node->self = self;
  node->private_environment = private_environment;
  node->classes = R_NilValue;
  node->assert_values = R_NilValue;
  node->parent = parent;
  node->child_position = child_position;
  node->next_child = 0;
  node->subtree_contributes = FALSE;
  node->postfix = FALSE;
  node->semantic = semantic;

  if (TYPEOF(self) != ENVSXP || TYPEOF(private_environment) != ENVSXP) {
    Rf_error("Corrupt ParamSet graph: node shell is not an environment");
  }
  SEXP classes = R_NilValue;
  paradox_core_kind_t class_kind = 0;
  if (!heal) {
    class_kind = paradox_param_set_class_kind_raw(self, &classes);
    if (class_kind == 0) {
      Rf_error("Corrupt ParamSet graph: invalid ParamSet-family R6 class");
    }
    node->classes = classes;
    SET_VECTOR_ELT(node->roots, NODE_ROOT_CLASS, classes);
  }

  SEXP enclosure = paradox_api_plain_binding_snapshot(
    self,
    check_enclosure_symbol
  );
  if (TYPEOF(enclosure) != ENVSXP || Rf_isS4(enclosure)) {
    Rf_error("Corrupt ParamSet shell ownership");
  }
  node->enclosure = enclosure;
  SET_VECTOR_ELT(node->roots, NODE_ROOT_ENCLOSURE, enclosure);

  SEXP owned_self = paradox_api_plain_binding_snapshot(
    enclosure,
    check_self_symbol
  );
  SEXP owned_private = paradox_api_plain_binding_snapshot(
    enclosure,
    check_private_symbol
  );
  if (owned_self != self || owned_private != private_environment) {
    Rf_error("Corrupt ParamSet shell ownership");
  }

  if (!heal) {
    SEXP assert_values = paradox_api_plain_binding_snapshot(
      self,
      check_assert_values_symbol
    );
    if (!paradox_param_set_assert_values_is_exact(assert_values)) {
      Rf_error("Corrupt ParamSet shell: invalid assert_values policy");
    }
    node->assert_values = assert_values;
    SET_VECTOR_ELT(node->roots, NODE_ROOT_ASSERT_VALUES, assert_values);
  }

  SEXP bound_core = paradox_api_plain_binding_snapshot(
    private_environment,
    check_core_symbol
  );
  SEXP core = selected_core == R_NilValue ? bound_core : selected_core;
  if (core == R_UnboundValue || bound_core != core) {
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }
  node->selected_core = core;
  SET_VECTOR_ELT(node->roots, NODE_ROOT_SELECTED_CORE, core);
  if (!paradox_core_is_canonical(core)) {
    Rf_error("Corrupt ParamSet state: noncanonical core capsule");
  }
  node->kind = paradox_core_kind(core);
  if (node->kind != PARADOX_CORE_BASE &&
      node->kind != PARADOX_CORE_COLLECTION &&
      node->kind != PARADOX_CORE_SHADOW) {
    Rf_error("Corrupt ParamSet state: unknown core node kind");
  }
  if (!heal && node->kind != class_kind) {
    Rf_error("Corrupt ParamSet state: capsule kind disagrees with shell class");
  }
  if (node->kind == PARADOX_CORE_SHADOW &&
      !paradox_shadow_metadata_is_exact(core)) {
    Rf_error("Corrupt ParamSetShadow native snapshot metadata");
  }
  if (!heal && paradox_collection_flatten_is_stale(core)) {
    /* Migration preflight may not install anything into a current shell, so
     * it cannot re-flatten this node the way every ordinary read does. Say
     * what is actually wrong rather than reporting the mismatch downstream as
     * corruption. */
    Rf_error(
      "ParamSetCollection schema is out of date with its contained sets; "
      "read it once (for example with `$ids()`) before this operation"
    );
  }
  if (node->kind == PARADOX_CORE_SHADOW) {
    if (heal) {
      /* A shadow's visible schema, effective values, dependencies, and
       * constraint are all a package-owned live view of its origin. Refresh
       * exactly once at an ordinary operation boundary, before snapshotting or
       * invoking user code. */
      core = paradox_core_refresh(self, private_environment);
    } else {
      /* Migration needs the same authoritative live projection, including
       * cross-shadow dependency checks, but cannot mutate any current shell
       * before every candidate has passed preflight. */
      core = paradox_shadow_preview_authoritative(self, private_environment);
    }
    if (!paradox_core_is_canonical(core) ||
        paradox_core_kind(core) != PARADOX_CORE_SHADOW ||
        !paradox_shadow_metadata_is_exact(core)) {
      Rf_error("Corrupt ParamSetShadow refreshed core capsule");
    }
  }
  SET_VECTOR_ELT(node->roots, NODE_ROOT_CORE, core);
  node->state = paradox_core_payload(core);
  SET_VECTOR_ELT(node->roots, NODE_ROOT_STATE, node->state);
  node->params = VECTOR_ELT(node->state, PARADOX_CORE_PARAMS);
  node->values = VECTOR_ELT(node->state, PARADOX_CORE_VALUES);
  node->tags = VECTOR_ELT(node->state, PARADOX_CORE_TAGS);
  node->dependencies = VECTOR_ELT(node->state, PARADOX_CORE_DEPS);
  node->trafos = VECTOR_ELT(node->state, PARADOX_CORE_TRAFOS);
  node->extra_trafo = VECTOR_ELT(node->state, PARADOX_CORE_EXTRA_TRAFO);
  node->constraint = VECTOR_ELT(node->state, PARADOX_CORE_CONSTRAINT);
  node->sets = VECTOR_ELT(node->state, PARADOX_CORE_SETS);
  node->translation = VECTOR_ELT(node->state, PARADOX_CORE_TRANSLATION);
  SET_VECTOR_ELT(node->roots, NODE_ROOT_PARAMS, node->params);
  SET_VECTOR_ELT(node->roots, NODE_ROOT_VALUES, node->values);
  SET_VECTOR_ELT(node->roots, NODE_ROOT_TAGS, node->tags);
  SET_VECTOR_ELT(node->roots, NODE_ROOT_DEPS, node->dependencies);
  SET_VECTOR_ELT(node->roots, NODE_ROOT_TRAFOS, node->trafos);
  SET_VECTOR_ELT(node->roots, NODE_ROOT_EXTRA_TRAFO, node->extra_trafo);
  SET_VECTOR_ELT(node->roots, NODE_ROOT_CONSTRAINT, node->constraint);
  SET_VECTOR_ELT(node->roots, NODE_ROOT_SETS, node->sets);
  SET_VECTOR_ELT(node->roots, NODE_ROOT_TRANSLATION, node->translation);

  R_xlen_t unused_row = 0;
  if (!paradox_domain_validate_params(
      node->params, R_NilValue, TRUE, &node->checked_params,
      &unused_row, work_since_interrupt
    ) || !paradox_domain_validate_values(
      node->values, &node->checked_values, work_since_interrupt
    ) || !paradox_domain_validate_tags(
      node->tags, &node->checked_tags, work_since_interrupt
    ) || !paradox_domain_validate_trafos(
      node->trafos, &node->checked_trafos, work_since_interrupt
    ) || !paradox_domain_validate_dependencies(
      node->dependencies, &node->checked_dependencies,
      work_since_interrupt
    )) {
    Rf_error("Corrupt ParamSet state: invalid canonical schema tables");
  }
  if (node->constraint != R_NilValue && !Rf_isFunction(node->constraint)) {
    Rf_error("Corrupt ParamSet state: constraint must be a function or NULL");
  }
  if (node->extra_trafo != R_NilValue && !Rf_isFunction(node->extra_trafo)) {
    Rf_error("Corrupt ParamSet state: extra_trafo must be a function or NULL");
  }
  validate_node_schema(node, work_since_interrupt);

  if (parent == R_XLEN_T_MAX ||
      graph->nodes[parent].kind == PARADOX_CORE_SHADOW) {
    SEXP mapping = PROTECT(Rf_duplicate(node->checked_params.ids));
    node->root_ids = mapping;
    SET_VECTOR_ELT(node->roots, NODE_ROOT_ROOT_IDS, mapping);
    UNPROTECT(1);
  } else {
    SEXP mapping = PROTECT(child_root_ids(
      graph, parent, child_position, &node->checked_params,
      work_since_interrupt
    ));
    node->root_ids = mapping;
    SET_VECTOR_ELT(node->roots, NODE_ROOT_ROOT_IDS, mapping);
    UNPROTECT(1);
  }

  if (node->kind == PARADOX_CORE_COLLECTION) {
    SEXP postfix = VECTOR_ELT(node->state, PARADOX_CORE_POSTFIX);
    if (node->constraint != R_NilValue || node->extra_trafo != R_NilValue ||
        node->checked_values.size != 0 || TYPEOF(postfix) != LGLSXP ||
        ALTREP(postfix) || !no_attributes(postfix) || XLENGTH(postfix) != 1 ||
        LOGICAL_ELT(postfix, 0) == NA_LOGICAL ||
        !exact_sets(node->sets, &node->set_names, work_since_interrupt)) {
      Rf_error("Corrupt ParamSetCollection state");
    }
    node->postfix = LOGICAL_ELT(postfix, 0);
    SET_VECTOR_ELT(node->roots, NODE_ROOT_SET_NAMES, node->set_names);
    if (!exact_translation(
        node->translation, &node->checked_params, node->sets,
        node->set_names, node->postfix, work_since_interrupt
      )) {
      Rf_error("Corrupt ParamSetCollection translation table");
    }
  } else if (node->kind == PARADOX_CORE_BASE) {
    SEXP postfix = VECTOR_ELT(node->state, PARADOX_CORE_POSTFIX);
    if (node->sets != R_NilValue || node->translation != R_NilValue ||
        TYPEOF(postfix) != LGLSXP || ALTREP(postfix) ||
        !no_attributes(postfix) || XLENGTH(postfix) != 1 ||
        LOGICAL_ELT(postfix, 0) != FALSE) {
      Rf_error("Corrupt ParamSet state: BASE node contains graph metadata");
    }
  } else {
    SEXP postfix = VECTOR_ELT(node->state, PARADOX_CORE_POSTFIX);
    if (TYPEOF(node->sets) != VECSXP || ALTREP(node->sets) ||
        XLENGTH(node->sets) != 1 ||
        TYPEOF(VECTOR_ELT(node->sets, 0)) != ENVSXP ||
        paradox_domain_private_environment(VECTOR_ELT(node->sets, 0)) ==
          R_UnboundValue || VECTOR_ELT(node->sets, 0) == self ||
        node->translation != R_NilValue ||
        TYPEOF(postfix) != LGLSXP || ALTREP(postfix) ||
        !no_attributes(postfix) || XLENGTH(postfix) != 1 ||
        LOGICAL_ELT(postfix, 0) != FALSE) {
      Rf_error("Corrupt ParamSetShadow state: invalid origin edge");
    }
  }

  node->subtree_contributes = node->checked_params.row_count != 0 ||
    node->checked_dependencies.row_count != 0 ||
    node->checked_values.size != 0 ||
    node->constraint != R_NilValue || node->extra_trafo != R_NilValue;
}

/* Shells whose entire subtree turned out to expose nothing the plan can use.
 * Every registered shell is rooted for the whole build by its own node's
 * `roots` entry in the protected root plan, so holding the raw pointer in
 * unscanned R_alloc storage cannot observe a collected-and-reused address.
 * The registry stays empty for a graph without a barren subtree, which is why
 * the ordinary build pays nothing for the lookup below. */
typedef struct {
  SEXP *shells;
  R_xlen_t count;
  R_xlen_t capacity;
} barren_registry_t;

static int barren_registry_contains(const barren_registry_t *registry,
    SEXP shell, R_xlen_t *work_since_interrupt) {
  for (R_xlen_t index = 0; index < registry->count; ++index) {
    paradox_account_work(work_since_interrupt);
    if (registry->shells[index] == shell) {
      return TRUE;
    }
  }
  return FALSE;
}

static void barren_registry_add(barren_registry_t *registry, SEXP shell) {
  if (registry->count == registry->capacity) {
    if (registry->capacity > R_XLEN_T_MAX / 2) {
      Rf_error("ParamSet graph is too large to validate");
    }
    const R_xlen_t expanded =
      registry->capacity == 0 ? 8 : registry->capacity * 2;
    SEXP *shells = paradox_temporary_alloc(expanded, sizeof(*shells));
    if (registry->count != 0) {
      memcpy(
        shells,
        registry->shells,
        (size_t) registry->count * sizeof(*shells)
      );
    }
    registry->shells = shells;
    registry->capacity = expanded;
  }
  registry->shells[registry->count++] = shell;
}

static void build_graph(SEXP private_environment, SEXP self,
    check_graph_t *graph, SEXP *root_plan, PROTECT_INDEX root_plan_index,
    int heal, SEXP selected_root_core) {
  initialize_check_binding_symbols();
  /* Heal before the first snapshot: this traversal validates every cached
   * flatten against its children, so an ancestor of a set that grew must be
   * brought up to date first. Migration preflight installs nothing and is
   * excluded, as is a caller that pinned an exact generation. */
  if (heal && selected_root_core == R_NilValue) {
    SEXP selected = paradox_core_from_private(private_environment);
    if (selected != R_UnboundValue && !paradox_core_is_verified(selected)) {
      (void) paradox_core_refresh(self, private_environment);
    }
  }
  initialize_graph(graph);
  R_xlen_t work_since_interrupt = 0;
  initialize_node(
    self, private_environment, R_XLEN_T_MAX, R_XLEN_T_MAX, TRUE, graph,
    &graph->nodes[0], root_plan, root_plan_index, &work_since_interrupt,
    selected_root_core, heal
  );
  graph->count = 1;
  graph->path[0] = 0;
  R_xlen_t depth = 1;
  barren_registry_t barren = {NULL, 0, 0};

  while (depth != 0) {
    const R_xlen_t node_index = graph->path[depth - 1];
    check_node_t *node = &graph->nodes[node_index];
    if ((node->kind == PARADOX_CORE_COLLECTION ||
         node->kind == PARADOX_CORE_SHADOW) &&
        node->next_child < XLENGTH(node->sets)) {
      const R_xlen_t child_position = node->next_child;
      SEXP child = PROTECT(VECTOR_ELT(node->sets, child_position));
      if (TYPEOF(child) != ENVSXP) {
        UNPROTECT(1);
        Rf_error("Corrupt ParamSet graph: collection child is not a ParamSet shell");
      }
      for (R_xlen_t ancestor = 0; ancestor < depth; ++ancestor) {
        paradox_account_work(&work_since_interrupt);
        if (graph->nodes[graph->path[ancestor]].self == child) {
          UNPROTECT(1);
          Rf_error("ParamSet graph contains a cycle");
        }
      }
      /* This shell was already expanded once through another path and its
       * whole subtree turned out to be barren, so a second expansion can only
       * reproduce the same nothing.  Re-expanding it is what made an
       * alternating shared graph cost Theta(2^depth) node snapshots for an
       * empty result; a subtree that does contribute is still expanded once
       * per occurrence, because each occurrence exposes its own affixed IDs. */
      if (barren_registry_contains(&barren, child, &work_since_interrupt)) {
        UNPROTECT(1);
        ++graph->nodes[node_index].next_child;
        continue;
      }
      SEXP child_private = PROTECT(paradox_domain_private_environment(child));
      if (child_private == R_UnboundValue) {
        UNPROTECT(2);
        Rf_error("Corrupt ParamSet graph: child has no versioned core capsule");
      }
      reserve_graph(graph);
      const R_xlen_t child_index = graph->count;
      initialize_node(
        child, child_private, node_index, child_position,
        node->kind == PARADOX_CORE_COLLECTION ? node->semantic : FALSE,
        graph,
        &graph->nodes[child_index], root_plan, root_plan_index,
        &work_since_interrupt, R_NilValue, heal
      );
      UNPROTECT(2);
      ++graph->nodes[node_index].next_child;
      ++graph->count;
      graph->path[depth] = child_index;
      ++depth;
      continue;
    }
    {
      check_node_t *finished = &graph->nodes[node_index];
      if (finished->parent != R_XLEN_T_MAX && finished->subtree_contributes) {
        graph->nodes[finished->parent].subtree_contributes = TRUE;
      }
      if (!finished->subtree_contributes) {
        barren_registry_add(&barren, finished->self);
      }
    }
    --depth;
  }
}

static int graph_receipts_are_current(const check_graph_t *graph) {
  for (R_xlen_t index = 0; index < graph->count; ++index) {
    const check_node_t *node = &graph->nodes[index];
    if (node->classes == R_NilValue ||
        Rf_getAttrib(node->self, R_ClassSymbol) != node->classes ||
        paradox_param_set_class_kind_raw(node->self, NULL) != node->kind) {
      return FALSE;
    }
    SEXP enclosure = paradox_api_plain_binding_scan(
      node->self,
      check_enclosure_symbol
    );
    if (enclosure != node->enclosure ||
        paradox_api_plain_binding_scan(enclosure, check_self_symbol) !=
          node->self ||
        paradox_api_plain_binding_scan(enclosure, check_private_symbol) !=
          node->private_environment ||
        paradox_api_plain_binding_scan(
          node->self,
          check_assert_values_symbol
        ) != node->assert_values ||
        paradox_api_plain_binding_scan(
          node->private_environment,
          check_core_symbol
        ) != node->selected_core) {
      return FALSE;
    }
  }
  return TRUE;
}

static value_spec_t load_spec(SEXP params, R_xlen_t row) {
  value_spec_t spec;
  spec.id = STRING_ELT(VECTOR_ELT(params, PARADOX_DOMAIN_ID), row);
  spec.kind = value_kind(
    STRING_ELT(VECTOR_ELT(params, PARADOX_DOMAIN_CLS), row),
    STRING_ELT(VECTOR_ELT(params, PARADOX_DOMAIN_STORAGE_TYPE), row)
  );
  spec.lower = paradox_numeric_elt(VECTOR_ELT(params, PARADOX_DOMAIN_LOWER), row);
  spec.upper = paradox_numeric_elt(VECTOR_ELT(params, PARADOX_DOMAIN_UPPER), row);
  spec.tolerance = paradox_numeric_elt(
    VECTOR_ELT(params, PARADOX_DOMAIN_TOLERANCE), row
  );
  spec.levels = VECTOR_ELT(VECTOR_ELT(params, PARADOX_DOMAIN_LEVELS), row);
  spec.special_values = VECTOR_ELT(
    VECTOR_ELT(params, PARADOX_DOMAIN_SPECIAL_VALS), row
  );
  spec.custom_check = spec.kind == VALUE_UTY
    ? utility_callback(VECTOR_ELT(
        VECTOR_ELT(params, PARADOX_DOMAIN_CARGO), row
      ))
    : R_NilValue;
  return spec;
}

static void initialize_activity_mapping(check_plan_t *plan) {
  if (plan->activity_mapping_ready) return;
  R_xlen_t dependency_count = 0;
  for (R_xlen_t node_index = 0;
      node_index < plan->graph.count; ++node_index) {
    const check_node_t *node = &plan->graph.nodes[node_index];
    if (!node->semantic) continue;
    if (node->checked_dependencies.row_count >
        R_XLEN_T_MAX - dependency_count) {
      Rf_error("ParamSet dependency graph is too large");
    }
    dependency_count += node->checked_dependencies.row_count;
  }

  plan->dependency_count = dependency_count;
  plan->has_dependency = paradox_temporary_alloc(
    plan->parameter_count == 0 ? 1 : plan->parameter_count,
    sizeof(*plan->has_dependency)
  );
  memset(
    plan->has_dependency,
    0,
    (size_t) (plan->parameter_count == 0 ? 1 : plan->parameter_count)
  );
  const R_xlen_t allocation_count = dependency_count == 0
    ? 1
    : dependency_count;
  plan->dependency_child = paradox_temporary_alloc(
    allocation_count,
    sizeof(*plan->dependency_child)
  );
  plan->dependency_parent = paradox_temporary_alloc(
    allocation_count,
    sizeof(*plan->dependency_parent)
  );
  plan->dependency_condition = paradox_temporary_alloc(
    allocation_count,
    sizeof(*plan->dependency_condition)
  );
  plan->dependency_rhs = paradox_temporary_alloc(
    allocation_count,
    sizeof(*plan->dependency_rhs)
  );
  plan->dependency_exposed_id = paradox_temporary_alloc(
    allocation_count,
    sizeof(*plan->dependency_exposed_id)
  );
  plan->dependency_exposed_on = paradox_temporary_alloc(
    allocation_count,
    sizeof(*plan->dependency_exposed_on)
  );

  R_xlen_t output = 0;
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t node_index = 0;
      node_index < plan->graph.count; ++node_index) {
    const check_node_t *node = &plan->graph.nodes[node_index];
    if (!node->semantic) continue;
    for (R_xlen_t local_dependency = 0;
        local_dependency < node->checked_dependencies.row_count;
        ++local_dependency) {
      paradox_account_work(&work_since_interrupt);
      SEXP id = STRING_ELT(
        node->checked_dependencies.ids,
        local_dependency
      );
      SEXP on = STRING_ELT(
        node->checked_dependencies.on,
        local_dependency
      );
      const R_xlen_t local_child = local_param_row(
        node,
        id,
        &work_since_interrupt
      );
      if (local_child == R_XLEN_T_MAX) {
        Rf_error("Corrupt ParamSet state: dependency target is unknown");
      }
      R_xlen_t child = R_XLEN_T_MAX;
      if (!find_id(
          &plan->root_ids,
          STRING_ELT(node->root_ids, local_child),
          &child,
          &work_since_interrupt
        )) {
        Rf_error("Corrupt ParamSet graph: dependency target is not exposed");
      }

      const R_xlen_t local_parent = local_param_row(
        node,
        on,
        &work_since_interrupt
      );
      R_xlen_t parent = R_XLEN_T_MAX;
      SEXP exposed_on = on;
      if (local_parent != R_XLEN_T_MAX) {
        exposed_on = STRING_ELT(node->root_ids, local_parent);
        if (!find_id(
            &plan->root_ids,
            exposed_on,
            &parent,
            &work_since_interrupt
          )) {
          Rf_error("Corrupt ParamSet graph: dependency parent is not exposed");
        }
      } else if (node->parent != R_XLEN_T_MAX) {
        /* Only a parent this node does not know pays for the outward walk. A
         * name that no enclosing namespace supplies either stays unresolved
         * and its edge remains never-satisfiable, exactly as before. */
        exposed_on = outward_dependency_parent(
          &plan->graph,
          node_index,
          on,
          &work_since_interrupt
        );
        (void) find_id(
          &plan->root_ids,
          exposed_on,
          &parent,
          &work_since_interrupt
        );
      }

      SEXP condition = VECTOR_ELT(
        node->checked_dependencies.conditions,
        local_dependency
      );
      paradox_builtin_condition_kind_t condition_kind;
      SEXP rhs = R_NilValue;
      if (!paradox_builtin_condition_exact(
          condition,
          &condition_kind,
          &rhs,
          &work_since_interrupt
        )) {
        Rf_error(
          "Corrupt ParamSet state: malformed built-in dependency condition"
        );
      }
      plan->dependency_child[output] = child;
      plan->has_dependency[child] = TRUE;
      plan->dependency_parent[output] = parent;
      plan->dependency_condition[output] = condition;
      plan->dependency_rhs[output] = rhs;
      plan->dependency_exposed_id[output] =
        STRING_ELT(node->root_ids, local_child);
      plan->dependency_exposed_on[output] = exposed_on;
      ++output;
    }
  }
  if (output != dependency_count) {
    Rf_error("Internal error: incomplete ParamSet activity mapping");
  }
  plan->activity_mapping_ready = TRUE;
}

static void initialize_check_plan(check_plan_t *plan) {
  check_node_t *root = &plan->graph.nodes[0];
  plan->root_params = root->params;
  plan->root_tags = root->tags;
  plan->parameter_count = root->checked_params.row_count;
  plan->defaults = VECTOR_ELT(plan->root_params, PARADOX_DOMAIN_DEFAULT);
  plan->activity_mapping_ready = FALSE;
  if (TYPEOF(plan->defaults) != VECSXP ||
      XLENGTH(plan->defaults) != plan->parameter_count) {
    Rf_error("Corrupt ParamSet state: malformed defaults");
  }
  plan->specs = paradox_temporary_alloc(
    plan->parameter_count == 0 ? 1 : plan->parameter_count,
    sizeof(*plan->specs)
  );
  for (R_xlen_t row = 0; row < plan->parameter_count; ++row) {
    plan->specs[row] = load_spec(plan->root_params, row);
  }
  initialize_id_map(root->checked_params.ids, &plan->root_ids);
}

static void build_check_plan(SEXP private_environment, SEXP self,
    check_plan_t *plan, SEXP *root_plan, PROTECT_INDEX root_plan_index) {
  build_graph(
    private_environment, self, &plan->graph, root_plan, root_plan_index, TRUE,
    R_NilValue
  );
  initialize_check_plan(plan);
}

/* ObjectTuneToken ParamSet admission is closed to one exact BASE node. Its
 * shell was authenticated before entry, so validate the already selected,
 * rooted capsule directly. Generic graph construction would independently
 * reread `.core`, allowing allocation finalizers to switch generations (or
 * install an active binding) between selection and semantic admission. */
static void build_exact_base_check_plan(SEXP private_environment, SEXP self,
    SEXP selected_core, check_plan_t *plan, SEXP *root_plan,
    PROTECT_INDEX root_plan_index) {
  initialize_check_binding_symbols();
  initialize_graph(&plan->graph);
  R_xlen_t work_since_interrupt = 0;
  initialize_node(
    self,
    private_environment,
    R_XLEN_T_MAX,
    R_XLEN_T_MAX,
    TRUE,
    &plan->graph,
    &plan->graph.nodes[0],
    root_plan,
    root_plan_index,
    &work_since_interrupt,
    selected_core,
    TRUE
  );
  plan->graph.count = 1;
  plan->graph.path[0] = 0;
  if (plan->graph.nodes[0].kind != PARADOX_CORE_BASE) {
    Rf_error("ObjectTuneToken ParamSet content must be an exact BASE ParamSet");
  }
  initialize_check_plan(plan);
}

typedef enum {
  TOKEN_PARAM_SET_ADMITTED = 0,
  TOKEN_PARAM_SET_MALFORMED,
  TOKEN_PARAM_SET_UNBOUNDED
} token_param_set_admission_t;

/* Symbols are interned before candidate admission can invoke any callback.
 * R symbols are immortal, so these cached references need no preservation and
 * let the final generation scan avoid an allocating Rf_install() boundary. */
static SEXP token_enclosure_symbol = NULL;
static SEXP token_self_symbol = NULL;
static SEXP token_private_symbol = NULL;
static SEXP token_core_symbol = NULL;

static void initialize_token_binding_symbols(void) {
  if (token_enclosure_symbol != NULL) return;
  token_enclosure_symbol = Rf_install(".__enclos_env__");
  token_self_symbol = Rf_install("self");
  token_private_symbol = Rf_install("private");
  token_core_symbol = Rf_install(".core");
}

static int exact_base_param_set_shell(SEXP content,
    SEXP *private_environment, SEXP *core_result) {
  static const char *const classes[] = {"ParamSet", "R6"};
  initialize_token_binding_symbols();
  if (TYPEOF(content) != ENVSXP || Rf_isS4(content)) return FALSE;
  R_xlen_t work_since_interrupt = 0;
  SEXP observed_classes = PROTECT(Rf_getAttrib(content, R_ClassSymbol));
  const int exact_class = !Rf_isS4(observed_classes) &&
    paradox_api_has_no_attributes(observed_classes) &&
    paradox_domain_exact_string_vector(
      observed_classes,
      classes,
      2,
      &work_since_interrupt
    );
  UNPROTECT(1);
  if (!exact_class) return FALSE;

  SEXP enclosure = PROTECT(paradox_api_optional_plain_binding_snapshot(
    content,
    token_enclosure_symbol
  ));
  if (enclosure == R_UnboundValue || TYPEOF(enclosure) != ENVSXP ||
      Rf_isS4(enclosure)) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP owner = PROTECT(paradox_api_optional_plain_binding_snapshot(enclosure, token_self_symbol));
  SEXP candidate_private = PROTECT(paradox_api_optional_plain_binding_snapshot(
    enclosure, token_private_symbol
  ));
  if (owner != content || TYPEOF(candidate_private) != ENVSXP ||
      Rf_isS4(candidate_private)) {
    UNPROTECT(3);
    return FALSE;
  }
  SEXP raw_core = PROTECT(paradox_api_optional_plain_binding_snapshot(
    candidate_private,
    token_core_symbol
  ));
  if (raw_core == R_UnboundValue) {
    UNPROTECT(4);
    return FALSE;
  }
  const int owned = paradox_core_is_canonical(raw_core) &&
    paradox_core_kind(raw_core) == PARADOX_CORE_BASE;
  if (!owned) {
    UNPROTECT(4);
    return FALSE;
  }
  if (private_environment != NULL) *private_environment = candidate_private;
  if (core_result != NULL) *core_result = raw_core;
  UNPROTECT(4);
  return TRUE;
}

/* Admit one exact BASE candidate without invoking a
 * candidate transformation or compatibility callback.  Collection, Shadow,
 * and additive-subclass graphs are deliberately outside the Paradox-2 Object
 * TuneToken contract. */
static token_param_set_admission_t admit_token_param_set_base(
    SEXP content, SEXP *private_result, SEXP *core_result,
    R_xlen_t *work_since_interrupt) {
  SEXP private_environment = R_NilValue;
  SEXP selected_core = R_NilValue;
  if (!exact_base_param_set_shell(
      content,
      &private_environment,
      &selected_core
    )) {
    PROTECT(private_environment);
    UNPROTECT(1);
    return TOKEN_PARAM_SET_MALFORMED;
  }
  PROTECT(private_environment);
  PROTECT(selected_core);

  PROTECT_INDEX candidate_plan_index;
  SEXP candidate_plan_root;
  PROTECT_WITH_INDEX(
    candidate_plan_root = R_NilValue,
    &candidate_plan_index
  );
  check_plan_t candidate_plan;
  build_exact_base_check_plan(
    private_environment,
    content,
    selected_core,
    &candidate_plan,
    &candidate_plan_root,
    candidate_plan_index
  );
  if (candidate_plan.graph.count == 0 ||
      TYPEOF(candidate_plan.graph.nodes[0].roots) != VECSXP ||
      XLENGTH(candidate_plan.graph.nodes[0].roots) != NODE_ROOT_COUNT) {
    UNPROTECT(3);
    Rf_error("Internal error: ObjectTuneToken admission lost its root node");
  }
  SEXP observed_private = R_NilValue;
  SEXP observed_core = R_NilValue;
  if (!exact_base_param_set_shell(
      content,
      &observed_private,
      &observed_core
    ) ||
      observed_private != private_environment ||
      observed_core != selected_core) {
    UNPROTECT(3);
    Rf_error(
      "ObjectTuneToken ParamSet candidate changed during admission; "
      "nested mutation was preserved"
    );
  }
  int bounded = candidate_plan.parameter_count != 0;
  for (R_xlen_t row = 0;
      row < candidate_plan.parameter_count && bounded; ++row) {
    paradox_account_work(work_since_interrupt);
    const value_spec_t *candidate = &candidate_plan.specs[row];
    bounded = candidate->kind == VALUE_FCT ||
      candidate->kind == VALUE_LGL ||
      ((candidate->kind == VALUE_DBL || candidate->kind == VALUE_INT) &&
        R_FINITE(candidate->lower) && R_FINITE(candidate->upper));
  }
  if (bounded && private_result != NULL) {
    *private_result = private_environment;
  }
  if (bounded && core_result != NULL) {
    *core_result = selected_core;
  }
  UNPROTECT(3);
  return bounded ? TOKEN_PARAM_SET_ADMITTED : TOKEN_PARAM_SET_UNBOUNDED;
}

static int exact_base_receipt_unchanged(SEXP receipt) {
  if (TYPEOF(receipt) != VECSXP || ALTREP(receipt) ||
      XLENGTH(receipt) != 3 || !paradox_api_has_no_attributes(receipt) ||
      token_enclosure_symbol == NULL || token_self_symbol == NULL ||
      token_private_symbol == NULL || token_core_symbol == NULL) {
    return FALSE;
  }
  SEXP shell = VECTOR_ELT(receipt, 0);
  SEXP expected_private = VECTOR_ELT(receipt, 1);
  SEXP expected_core = VECTOR_ELT(receipt, 2);
  if (TYPEOF(shell) != ENVSXP || Rf_isS4(shell) ||
      TYPEOF(expected_private) != ENVSXP || Rf_isS4(expected_private) ||
      TYPEOF(expected_core) != EXTPTRSXP || Rf_isS4(expected_core)) {
    return FALSE;
  }
  SEXP classes = Rf_getAttrib(shell, R_ClassSymbol);
  if (TYPEOF(classes) != STRSXP || ALTREP(classes) || Rf_isS4(classes) ||
      XLENGTH(classes) != 2 || !paradox_api_has_no_attributes(classes) ||
      STRING_ELT(classes, 0) == NA_STRING ||
      STRING_ELT(classes, 1) == NA_STRING ||
      strcmp(CHAR(STRING_ELT(classes, 0)), "ParamSet") != 0 ||
      strcmp(CHAR(STRING_ELT(classes, 1)), "R6") != 0) {
    return FALSE;
  }
  SEXP enclosure = paradox_api_plain_binding_scan(shell, token_enclosure_symbol);
  if (TYPEOF(enclosure) != ENVSXP || Rf_isS4(enclosure) ||
      paradox_api_plain_binding_scan(enclosure, token_self_symbol) != shell ||
      paradox_api_plain_binding_scan(enclosure, token_private_symbol) !=
        expected_private ||
      paradox_api_plain_binding_scan(expected_private, token_core_symbol) !=
        expected_core) {
    return FALSE;
  }
  return TRUE;
}

void paradox_param_set_scan_token_receipts(SEXP receipts) {
  if (receipts == R_NilValue) return;
  if (TYPEOF(receipts) != VECSXP || ALTREP(receipts) ||
      !paradox_api_has_no_attributes(receipts)) {
    Rf_error("Internal error: malformed ObjectTuneToken receipt set");
  }
  for (R_xlen_t index = 0; index < XLENGTH(receipts); ++index) {
    SEXP receipt = VECTOR_ELT(receipts, index);
    if (receipt != R_NilValue && !exact_base_receipt_unchanged(receipt)) {
      Rf_error(
        "ObjectTuneToken ParamSet candidate changed during validation; "
        "nested mutation was preserved"
      );
    }
  }
}

/* Receipts are internal rooted capabilities, never public ParamSet state. Each
 * non-NULL element records the exact shell/private/capsule generation admitted
 * before a validation callback ran. Reauthentication is deliberately limited
 * to ordinary, non-forcing bindings and pointer identity: it cannot invoke a
 * candidate method or replay candidate admission. */
static void verify_token_receipts(SEXP receipts) {
  if (receipts == R_NilValue) return;
  if (TYPEOF(receipts) != VECSXP || ALTREP(receipts)) {
    Rf_error("Internal error: malformed ObjectTuneToken receipt set");
  }
  for (R_xlen_t index = 0; index < XLENGTH(receipts); ++index) {
    SEXP receipt = VECTOR_ELT(receipts, index);
    if (receipt == R_NilValue) continue;
    if (TYPEOF(receipt) != VECSXP || ALTREP(receipt) ||
        XLENGTH(receipt) != 3) {
      Rf_error("Internal error: malformed ObjectTuneToken receipt");
    }
    SEXP shell = VECTOR_ELT(receipt, 0);
    SEXP expected_private = VECTOR_ELT(receipt, 1);
    SEXP expected_core = VECTOR_ELT(receipt, 2);
    SEXP observed_private = R_NilValue;
    SEXP observed_core = R_NilValue;
    if (TYPEOF(shell) != ENVSXP || Rf_isS4(shell) ||
        TYPEOF(expected_private) != ENVSXP || Rf_isS4(expected_private) ||
        TYPEOF(expected_core) != EXTPTRSXP || Rf_isS4(expected_core) ||
        !exact_base_param_set_shell(
          shell,
          &observed_private,
          &observed_core
        ) ||
        observed_private != expected_private ||
        observed_core != expected_core) {
      Rf_error(
        "ObjectTuneToken ParamSet candidate changed during validation; "
        "nested mutation was preserved"
      );
    }
  }
  /* The full check above may allocate while authenticating a later receipt.
   * Finish with one non-forcing, non-installing pointer scan over every
   * receipt so no finalizer can change an earlier generation unnoticed. */
  paradox_param_set_scan_token_receipts(receipts);
}

static SEXP materialize_atomic(SEXP value) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  const R_xlen_t size = XLENGTH(value);
  SEXP result = PROTECT(Rf_allocVector(type, size));
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_account_work(&work_since_interrupt);
    switch (type) {
    case LGLSXP:
      SET_LOGICAL_ELT(result, index, LOGICAL_ELT(value, index));
      break;
    case INTSXP:
      SET_INTEGER_ELT(result, index, INTEGER_ELT(value, index));
      break;
    case REALSXP:
      SET_REAL_ELT(result, index, REAL_ELT(value, index));
      break;
    case CPLXSXP:
      paradox_api_set_complex_elt(
        result,
        index,
        COMPLEX_ELT(value, index)
      );
      break;
    case STRSXP:
      SET_STRING_ELT(result, index, STRING_ELT(value, index));
      break;
    case RAWSXP:
      paradox_api_set_raw_elt(result, index, RAW_ELT(value, index));
      break;
    default:
      UNPROTECT(1);
      Rf_error("Unsupported ALTREP parameter value type");
    }
  }
  SHALLOW_DUPLICATE_ATTRIB(result, value);
  UNPROTECT(1);
  return result;
}

static SEXP snapshot_parameter_leaf(SEXP value) {
  /* S4 parameter leaves are opaque at snapshot admission. Typed Domains may
   * accept one only through exact identity with an admitted S4 special, so a
   * duplicate here would make that documented exception impossible. */
  if (Rf_isS4(value)) return value;
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  if (type == LGLSXP || type == INTSXP || type == REALSXP ||
      type == CPLXSXP || type == STRSXP || type == RAWSXP) {
    if (ALTREP(value)) return materialize_atomic(value);
    return Rf_duplicate(value);
  }
  if (type == VECSXP) {
    const R_xlen_t size = XLENGTH(value);
    SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
    for (R_xlen_t index = 0; index < size; ++index) {
      SET_VECTOR_ELT(result, index, VECTOR_ELT(value, index));
    }
    SHALLOW_DUPLICATE_ATTRIB(result, value);
    UNPROTECT(1);
    return result;
  }
  return value;
}

typedef enum {
  TOKEN_SNAPSHOT_FULL = 0,
  TOKEN_SNAPSHOT_RANGE,
  TOKEN_SNAPSHOT_OBJECT,
  TOKEN_SNAPSHOT_INTERNAL_FULL,
  TOKEN_SNAPSHOT_INTERNAL_RANGE
} token_snapshot_kind_t;

static int exact_token_string_vector(SEXP value,
    const char *const *expected, R_xlen_t size,
    R_xlen_t *work_since_interrupt) {
  return TYPEOF(value) == STRSXP && !ALTREP(value) &&
    !Rf_isS4(value) && !Rf_isObject(value) && no_attributes(value) &&
    paradox_domain_exact_string_vector(
      value,
      expected,
      size,
      work_since_interrupt
    );
}

static token_snapshot_kind_t exact_token_kind(SEXP token) {
  static const char *const attributes[] = {"names", "class"};
  static const char *const names[] = {"content", "call"};
  static const char *const full_classes[] = {
    "FullTuneToken", "TuneToken"
  };
  static const char *const range_classes[] = {
    "RangeTuneToken", "TuneToken"
  };
  static const char *const object_classes[] = {
    "ObjectTuneToken", "TuneToken"
  };
  static const char *const internal_full_classes[] = {
    "InternalTuneToken", "FullTuneToken", "TuneToken"
  };
  static const char *const internal_range_classes[] = {
    "InternalTuneToken", "RangeTuneToken", "TuneToken"
  };
  if (TYPEOF(token) != VECSXP || ALTREP(token) || Rf_isS4(token) ||
      XLENGTH(token) != 2 ||
      !paradox_api_has_only_attributes(token, attributes, 2)) {
    Rf_error(
      "Malformed TuneToken: expected the fixed {content, call} list"
    );
  }
  R_xlen_t work_since_interrupt = 0;
  SEXP observed_names = PROTECT(Rf_getAttrib(token, R_NamesSymbol));
  SEXP classes = PROTECT(Rf_getAttrib(token, R_ClassSymbol));
  if (!exact_token_string_vector(
        observed_names,
        names,
        2,
        &work_since_interrupt
      )) {
    UNPROTECT(2);
    Rf_error(
      "Malformed TuneToken: expected the fixed {content, call} list"
    );
  }
  token_snapshot_kind_t kind;
  if (exact_token_string_vector(
      classes,
      full_classes,
      2,
      &work_since_interrupt
    )) {
    kind = TOKEN_SNAPSHOT_FULL;
  } else if (exact_token_string_vector(
      classes,
      range_classes,
      2,
      &work_since_interrupt
    )) {
    kind = TOKEN_SNAPSHOT_RANGE;
  } else if (exact_token_string_vector(
      classes,
      object_classes,
      2,
      &work_since_interrupt
    )) {
    kind = TOKEN_SNAPSHOT_OBJECT;
  } else if (exact_token_string_vector(
      classes,
      internal_full_classes,
      3,
      &work_since_interrupt
    )) {
    kind = TOKEN_SNAPSHOT_INTERNAL_FULL;
  } else if (exact_token_string_vector(
      classes,
      internal_range_classes,
      3,
      &work_since_interrupt
    )) {
    kind = TOKEN_SNAPSHOT_INTERNAL_RANGE;
  } else {
    UNPROTECT(2);
    Rf_error("Malformed TuneToken: unsupported class vector");
  }
  UNPROTECT(2);
  return kind;
}

static int token_content_names(SEXP content,
    const char *const *expected, R_xlen_t size) {
  if (TYPEOF(content) != VECSXP || ALTREP(content) || Rf_isS4(content) ||
      Rf_isObject(content) || XLENGTH(content) != size ||
      !paradox_api_has_single_attribute(content, "names")) {
    return FALSE;
  }
  R_xlen_t work_since_interrupt = 0;
  SEXP names = PROTECT(Rf_getAttrib(content, R_NamesSymbol));
  const int exact = exact_token_string_vector(
    names,
    expected,
    size,
    &work_since_interrupt
  );
  UNPROTECT(1);
  return exact;
}

static int plain_token_logical(SEXP value) {
  if (TYPEOF(value) != LGLSXP || ALTREP(value) || Rf_isS4(value) ||
      Rf_isObject(value) || XLENGTH(value) != 1 ||
      LOGICAL_ELT(value, 0) == NA_LOGICAL) {
    return FALSE;
  }
  if (no_attributes(value)) return TRUE;
  if (!paradox_api_has_single_attribute(value, "names")) return FALSE;
  SEXP names = PROTECT(Rf_getAttrib(value, R_NamesSymbol));
  const int valid_names = TYPEOF(names) == STRSXP && !ALTREP(names) &&
    !Rf_isS4(names) && !Rf_isObject(names) && no_attributes(names) &&
    XLENGTH(names) == 1 &&
    STRING_ELT(names, 0) != NA_STRING;
  UNPROTECT(1);
  return valid_names;
}

static int token_number_or_null(SEXP value) {
  if (value == R_NilValue) return TRUE;
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  if ((type != INTSXP && type != REALSXP) || ALTREP(value) ||
      Rf_isS4(value) || Rf_isObject(value) || XLENGTH(value) != 1) {
    return FALSE;
  }
  if (!no_attributes(value)) {
    if (!paradox_api_has_single_attribute(value, "names")) return FALSE;
    SEXP names = PROTECT(Rf_getAttrib(value, R_NamesSymbol));
    const int valid_names = TYPEOF(names) == STRSXP && !ALTREP(names) &&
      !Rf_isS4(names) && !Rf_isObject(names) && no_attributes(names) &&
      XLENGTH(names) == 1 &&
      STRING_ELT(names, 0) != NA_STRING;
    UNPROTECT(1);
    if (!valid_names) return FALSE;
  }
  return type == REALSXP
    ? !ISNAN(REAL_ELT(value, 0))
    : INTEGER_ELT(value, 0) != NA_INTEGER;
}

static void validate_token_content(SEXP content, token_snapshot_kind_t kind) {
  static const char *const full_names[] = {"logscale"};
  static const char *const full_aggr_names[] = {"logscale", "aggr"};
  static const char *const range_names[] = {
    "lower", "upper", "logscale"
  };
  static const char *const range_aggr_names[] = {
    "lower", "upper", "logscale", "aggr"
  };
  if (kind == TOKEN_SNAPSHOT_OBJECT) {
    if (Rf_isS4(content) ||
        (!Rf_inherits(content, "Domain") && TYPEOF(content) != ENVSXP)) {
      Rf_error(
        "Malformed ObjectTuneToken: content must be a Domain or ParamSet"
      );
    }
    return;
  }

  const int internal = kind == TOKEN_SNAPSHOT_INTERNAL_FULL ||
    kind == TOKEN_SNAPSHOT_INTERNAL_RANGE;
  const int range = kind == TOKEN_SNAPSHOT_RANGE ||
    kind == TOKEN_SNAPSHOT_INTERNAL_RANGE;
  const R_xlen_t ordinary_size = range ? 3 : 1;
  const R_xlen_t size = TYPEOF(content) == VECSXP && !ALTREP(content)
    ? XLENGTH(content)
    : 0;
  const int has_aggr = internal && size == ordinary_size + 1;
  const char *const *expected_names = range
    ? (has_aggr ? range_aggr_names : range_names)
    : (has_aggr ? full_aggr_names : full_names);
  if ((!internal && size != ordinary_size) ||
      (internal && size != ordinary_size && size != ordinary_size + 1) ||
      !token_content_names(content, expected_names, size)) {
    Rf_error("Malformed TuneToken content");
  }
  if (range && (!token_number_or_null(VECTOR_ELT(content, 0)) ||
      !token_number_or_null(VECTOR_ELT(content, 1)))) {
    Rf_error("Malformed RangeTuneToken bounds");
  }
  const R_xlen_t logscale_index = range ? 2 : 0;
  SEXP logscale = VECTOR_ELT(content, logscale_index);
  if (!plain_token_logical(logscale) ||
      (internal && LOGICAL_ELT(logscale, 0) != FALSE) ||
      (has_aggr && (Rf_isS4(VECTOR_ELT(content, size - 1)) ||
        !Rf_isFunction(VECTOR_ELT(content, size - 1))))) {
    Rf_error("Malformed TuneToken content");
  }
}

static void validate_token_call(SEXP call) {
  if (TYPEOF(call) != STRSXP || ALTREP(call) || Rf_isS4(call) ||
      Rf_isObject(call) || !no_attributes(call) || XLENGTH(call) != 1 ||
      STRING_ELT(call, 0) == NA_STRING) {
    Rf_error("Malformed TuneToken call");
  }
}

static SEXP snapshot_tune_token_impl(
    SEXP token, SEXP mutation_column, SEXP mutation_replacement) {
  const token_snapshot_kind_t kind = exact_token_kind(token);
  SEXP content = PROTECT(VECTOR_ELT(token, 0));
  SEXP call = PROTECT(VECTOR_ELT(token, 1));
  validate_token_call(call);
  validate_token_content(content, kind);

  if (mutation_column != R_NilValue) {
    PROTECT(paradox_test_gc_column_mutator(
      content,
      mutation_column,
      mutation_replacement
    ));
    UNPROTECT(1);
    const int selected = INTEGER_ELT(mutation_column, 0);
    /* This is a test-only deterministic finalizer barrier. R_gc() and
     * R_RunPendingFinalizers() are public APIs; invoking both here guarantees
     * that the mutation occurs after exact admission and before the owned
     * copy, instead of relying on an allocation to drain the pending queue. */
    R_gc();
    R_RunPendingFinalizers();
    if (VECTOR_ELT(content, selected) != mutation_replacement) {
      UNPROTECT(2);
      Rf_error("TuneToken GC-mutation test fixture did not run");
    }
  }

  SEXP stable_content;
  if (kind == TOKEN_SNAPSHOT_OBJECT) {
    if (TYPEOF(content) == VECSXP) {
      paradox_domain_field_t failed_field = PARADOX_DOMAIN_FIELD_NONE;
      stable_content = PROTECT(paradox_snapshot_builtin_domain(
        content,
        &failed_field
      ));
      if (stable_content == R_NilValue) {
        UNPROTECT(3);
        if (failed_field != PARADOX_DOMAIN_FIELD_NONE) {
          Rf_error(
            "Malformed ObjectTuneToken Domain field `%s`",
            paradox_domain_field_name(failed_field)
          );
        }
        Rf_error("Malformed ObjectTuneToken Domain candidate");
      }
      SEXP domain_class = VECTOR_ELT(
        stable_content,
        PARADOX_DOMAIN_CLS
      );
      SEXP domain_levels = VECTOR_ELT(
        stable_content,
        PARADOX_DOMAIN_LEVELS
      );
      if (paradox_domain_string_is(
            STRING_ELT(domain_class, 0),
            "ParamFct"
          ) &&
          XLENGTH(VECTOR_ELT(domain_levels, 0)) == 0) {
        UNPROTECT(3);
        Rf_error(
          "Malformed ObjectTuneToken Domain candidate: categorical tuning "
          "Domains must contain at least one level"
        );
      }
    } else {
      /* ParamSet environments intentionally remain live in stored public
       * tokens.  Exact BASE admission and sealed detachment happen again at
       * each explicit search boundary. */
      stable_content = PROTECT(content);
    }
  } else {
    stable_content = PROTECT(Rf_allocVector(VECSXP, XLENGTH(content)));
    const R_xlen_t representation_scalar_count =
      kind == TOKEN_SNAPSHOT_FULL || kind == TOKEN_SNAPSHOT_INTERNAL_FULL
      ? 1
      : 3;
    for (R_xlen_t index = 0; index < XLENGTH(content); ++index) {
      SEXP element = PROTECT(snapshot_parameter_leaf(
        VECTOR_ELT(content, index)
      ));
      /* Ordinary R arithmetic and subsetting commonly attach a name to a
       * scalar.  Bounds and logscale admit that representation, but their
       * owned snapshot is canonical.  The optional aggregation callback is
       * deliberately opaque and must retain its identity and attributes. */
      if (index < representation_scalar_count && element != R_NilValue) {
        Rf_setAttrib(element, R_NamesSymbol, R_NilValue);
      }
      SET_VECTOR_ELT(stable_content, index, element);
      UNPROTECT(1);
    }
    SHALLOW_DUPLICATE_ATTRIB(stable_content, content);
  }
  SEXP stable_call = PROTECT(Rf_duplicate(call));
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, stable_content);
  SET_VECTOR_ELT(result, 1, stable_call);
  SHALLOW_DUPLICATE_ATTRIB(result, token);
  /* Exact admission is about the owned snapshot, not a live source that may
   * have changed while an allocation ran a pending finalizer. Revalidating the
   * final detached root closes that interval without a parallel R validator. */
  const token_snapshot_kind_t stable_kind = exact_token_kind(result);
  if (stable_kind != kind) {
    UNPROTECT(5);
    Rf_error("TuneToken changed while its exact snapshot was constructed");
  }
  validate_token_call(VECTOR_ELT(result, 1));
  validate_token_content(VECTOR_ELT(result, 0), stable_kind);
  UNPROTECT(5);
  return result;
}

static SEXP snapshot_tune_token(SEXP token) {
  return snapshot_tune_token_impl(token, R_NilValue, R_NilValue);
}

SEXP paradox_test_tune_token_gc_mutation_snapshot(
    SEXP token, SEXP column, SEXP replacement) {
  if (TYPEOF(token) != VECSXP || ALTREP(token) || XLENGTH(token) < 1) {
    Rf_error("Invalid TuneToken GC-mutation test fixture token");
  }
  SEXP content = VECTOR_ELT(token, 0);
  if (TYPEOF(content) != VECSXP || ALTREP(content)) {
    Rf_error("Invalid TuneToken GC-mutation test fixture content");
  }
  return snapshot_tune_token_impl(token, column, replacement);
}

static SEXP snapshot_parameter_value(SEXP value) {
  if (Rf_inherits(value, "TuneToken")) {
    return snapshot_tune_token(value);
  }
  return snapshot_parameter_leaf(value);
}

static int representation_only_s3_classes(SEXP value) {
  SEXP classes = PROTECT(Rf_getAttrib(value, R_ClassSymbol));
  if (classes == R_NilValue) {
    UNPROTECT(1);
    return TRUE;
  }
  if (TYPEOF(classes) != STRSXP || ALTREP(classes) || Rf_isS4(classes) ||
      Rf_isObject(classes) || !no_attributes(classes) ||
      XLENGTH(classes) == 0) {
    UNPROTECT(1);
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(classes); ++index) {
    SEXP cls = STRING_ELT(classes, index);
    if (cls == NA_STRING || CHAR(cls)[0] == '\0') {
      UNPROTECT(1);
      return FALSE;
    }
  }
  UNPROTECT(1);
  return TRUE;
}

static int ordinary_names_vector(SEXP value, R_xlen_t expected,
    int allow_absent) {
  if (value == R_NilValue) return allow_absent;
  return TYPEOF(value) == STRSXP && !ALTREP(value) &&
    !Rf_isS4(value) && !Rf_isObject(value) && no_attributes(value) &&
    XLENGTH(value) == expected;
}

static SEXP snapshot_named_list(SEXP values) {
  static const char *const allowed_attributes[] = {"names", "class"};
  if (TYPEOF(values) != VECSXP) {
    return check_message(
      "Must be a list, not '%s'",
      Rf_type2char((SEXPTYPE) TYPEOF(values))
    );
  }
  if (ALTREP(values) || Rf_isS4(values) ||
      !paradox_api_has_only_attributes(values, allowed_attributes, 2) ||
      !representation_only_s3_classes(values)) {
    return check_message("Must be an ordinary named list");
  }
  const R_xlen_t size = XLENGTH(values);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  SEXP names = PROTECT(Rf_getAttrib(values, R_NamesSymbol));
  if (names == R_NilValue && size != 0) {
    UNPROTECT(2);
    return check_message("Must be a named list");
  }
  if (!ordinary_names_vector(names, size, size == 0)) {
    UNPROTECT(2);
    return check_message(
      "Must be a named list. Names must be an ordinary character vector"
    );
  }
  /* `list()` is the canonical public spelling of an empty point.  R does not
   * attach a zero-length names vector to it, so normalize that sole unnamed
   * case before entering the common point kernel. */
  SEXP stable_names = PROTECT(names == R_NilValue
    ? Rf_allocVector(STRSXP, 0)
    : Rf_duplicate(names));
  if (TYPEOF(stable_names) != STRSXP || ALTREP(stable_names) ||
      Rf_isS4(stable_names) || XLENGTH(stable_names) != size ||
      !no_attributes(stable_names)) {
    UNPROTECT(3);
    return check_message("Names must be an ordinary character vector");
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    if (STRING_ELT(stable_names, index) == NA_STRING ||
        CHAR(STRING_ELT(stable_names, index))[0] == '\0') {
      UNPROTECT(3);
      return check_message("Names must be non-missing and non-empty");
    }
    SEXP value = PROTECT(VECTOR_ELT(values, index));
    SEXP copy = PROTECT(snapshot_parameter_value(value));
    SET_VECTOR_ELT(result, index, copy);
    UNPROTECT(2);
  }
  if (Rf_any_duplicated(stable_names, FALSE) != 0) {
    UNPROTECT(3);
    return check_message("Names must be unique");
  }
  Rf_setAttrib(result, R_NamesSymbol, stable_names);
  UNPROTECT(3);
  return result;
}

/* Publishes the complete admitted name vector through `value_names` beside the
 * TuneToken subset it returns. Both cross the return boundary unrooted, so the
 * caller must root them before anything that can allocate. */
static SEXP snapshot_tune_tokens_from_values(SEXP values, SEXP *value_names) {
  static const char *const allowed_attributes[] = {"names", "class"};
  if (TYPEOF(values) != VECSXP || ALTREP(values) || Rf_isS4(values) ||
      !paradox_api_has_only_attributes(values, allowed_attributes, 2)) {
    Rf_error("Search-space values must be an ordinary named list");
  }
  if (!representation_only_s3_classes(values)) {
    Rf_error("Search-space value container has a malformed S3 class");
  }
  const R_xlen_t size = XLENGTH(values);
  SEXP names = PROTECT(Rf_getAttrib(values, R_NamesSymbol));
  if (!ordinary_names_vector(names, size, size == 0)) {
    UNPROTECT(1);
    Rf_error("Search-space values must have ordinary character names");
  }
  SEXP stable_names = PROTECT(names == R_NilValue
    ? Rf_allocVector(STRSXP, 0)
    : Rf_duplicate(names));
  if (TYPEOF(stable_names) != STRSXP || ALTREP(stable_names) ||
      Rf_isS4(stable_names) || Rf_isObject(stable_names) ||
      XLENGTH(stable_names) != size || !no_attributes(stable_names)) {
    UNPROTECT(2);
    Rf_error("Search-space value names must be ordinary characters");
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP name = STRING_ELT(stable_names, index);
    if (name == NA_STRING || CHAR(name)[0] == '\0') {
      UNPROTECT(2);
      Rf_error("Search-space value names must be non-missing and non-empty");
    }
  }
  if (Rf_any_duplicated(stable_names, FALSE) != 0) {
    UNPROTECT(2);
    Rf_error("Search-space value names must be unique");
  }

  R_xlen_t token_count = 0;
  unsigned char *selected = paradox_temporary_alloc(
    size == 0 ? 1 : size,
    sizeof(*selected)
  );
  for (R_xlen_t index = 0; index < size; ++index) {
    const int is_token = Rf_inherits(
      VECTOR_ELT(values, index), "TuneToken"
    ) != FALSE;
    selected[index] = is_token ? 1U : 0U;
    token_count += selected[index] != 0;
  }
  SEXP result = PROTECT(Rf_allocVector(VECSXP, token_count));
  SEXP result_names = PROTECT(Rf_allocVector(STRSXP, token_count));
  R_xlen_t output = 0;
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP value = VECTOR_ELT(values, index);
    if (selected[index] == 0) continue;
    SEXP token = PROTECT(snapshot_tune_token(value));
    SET_VECTOR_ELT(result, output, token);
    SET_STRING_ELT(result_names, output, STRING_ELT(stable_names, index));
    ++output;
    UNPROTECT(1);
  }
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  *value_names = stable_names;
  UNPROTECT(4);
  return result;
}

static void search_space_parameter_unavailable(SEXP id, SEXP candidate_ids) {
  PROTECT(id);
  if (Rf_getCharCE(id) == CE_BYTES) {
    UNPROTECT(1);
    Rf_error("Search-space values name an unknown bytes-encoded parameter ID");
  }
  SEXP message = PROTECT(paradox_parameter_unavailable_diagnostic(
    id,
    candidate_ids,
    ""
  ));
  paradox_error_from_scalar_string(message);
}

static SEXP select_tune_target_domains(SEXP all_domains, SEXP value_names,
    SEXP token_names, R_xlen_t *work_since_interrupt) {
  if (TYPEOF(all_domains) != VECSXP) {
    Rf_error("Internal error: invalid target Domain snapshot");
  }
  SEXP all_names = PROTECT(Rf_getAttrib(all_domains, R_NamesSymbol));
  if (TYPEOF(all_names) != STRSXP ||
      XLENGTH(all_names) != XLENGTH(all_domains)) {
    UNPROTECT(1);
    Rf_error("Internal error: unnamed target Domain snapshot");
  }
  const R_xlen_t value_count = XLENGTH(value_names);
  const R_xlen_t size = XLENGTH(token_names);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  SEXP result_names = PROTECT(Rf_duplicate(token_names));
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  if (value_count != 0) {
    /* One index over the parameter IDs serves both loops below.  It replaces
     * a per-name linear scan, so admitting the complete value container costs
     * less than resolving the TuneToken subset alone used to.  An empty
     * container resolves nothing and skips the index entirely. */
    id_map_t parameter_ids;
    initialize_id_map(all_names, &parameter_ids);
    /* Paradox 1 asserted `names(values)` to be a subset of `$ids()`.  Without
     * it a misspelled entry that is not a TuneToken is silently dropped and
     * the caller receives an empty search space instead of an error.  The
     * assertion named the offending entry, so the replacement diagnostic
     * names it as well. */
    for (R_xlen_t index = 0; index < value_count; ++index) {
      R_xlen_t found = R_XLEN_T_MAX;
      if (!find_id(
          &parameter_ids,
          STRING_ELT(value_names, index),
          &found,
          work_since_interrupt
        )) {
        search_space_parameter_unavailable(
          STRING_ELT(value_names, index),
          all_names
        );
      }
    }
    for (R_xlen_t output = 0; output < size; ++output) {
      R_xlen_t found = R_XLEN_T_MAX;
      if (!find_id(
          &parameter_ids,
          STRING_ELT(token_names, output),
          &found,
          work_since_interrupt
        )) {
        search_space_parameter_unavailable(
          STRING_ELT(token_names, output),
          all_names
        );
      }
      SET_VECTOR_ELT(result, output, VECTOR_ELT(all_domains, found));
    }
  }
  UNPROTECT(3);
  return result;
}

static value_spec_t target_domain_spec(SEXP domain) {
  if (TYPEOF(domain) != VECSXP || XLENGTH(domain) !=
      PARADOX_DOMAIN_COLUMN_COUNT) {
    Rf_error("Internal error: malformed admitted target Domain");
  }
  SEXP id = VECTOR_ELT(domain, PARADOX_DOMAIN_ID);
  SEXP cls = VECTOR_ELT(domain, PARADOX_DOMAIN_CLS);
  SEXP storage = VECTOR_ELT(domain, PARADOX_DOMAIN_STORAGE_TYPE);
  SEXP lower = VECTOR_ELT(domain, PARADOX_DOMAIN_LOWER);
  SEXP upper = VECTOR_ELT(domain, PARADOX_DOMAIN_UPPER);
  SEXP tolerance = VECTOR_ELT(domain, PARADOX_DOMAIN_TOLERANCE);
  SEXP cargo_column = VECTOR_ELT(domain, PARADOX_DOMAIN_CARGO);
  SEXP levels_column = VECTOR_ELT(domain, PARADOX_DOMAIN_LEVELS);
  SEXP specials_column = VECTOR_ELT(domain, PARADOX_DOMAIN_SPECIAL_VALS);
  if (TYPEOF(id) != STRSXP || TYPEOF(cls) != STRSXP ||
      TYPEOF(storage) != STRSXP || XLENGTH(id) != 1 || XLENGTH(cls) != 1 ||
      XLENGTH(storage) != 1 ||
      (TYPEOF(lower) != INTSXP && TYPEOF(lower) != REALSXP) ||
      (TYPEOF(upper) != INTSXP && TYPEOF(upper) != REALSXP) ||
      (TYPEOF(tolerance) != INTSXP && TYPEOF(tolerance) != REALSXP) ||
      XLENGTH(lower) != 1 || XLENGTH(upper) != 1 ||
      XLENGTH(tolerance) != 1 || TYPEOF(cargo_column) != VECSXP ||
      TYPEOF(levels_column) != VECSXP || TYPEOF(specials_column) != VECSXP ||
      XLENGTH(cargo_column) != 1 || XLENGTH(levels_column) != 1 ||
      XLENGTH(specials_column) != 1) {
    Rf_error("Internal error: malformed admitted target Domain columns");
  }
  value_spec_t result;
  result.id = STRING_ELT(id, 0);
  result.kind = value_kind(
    STRING_ELT(cls, 0),
    STRING_ELT(storage, 0)
  );
  result.lower = paradox_numeric_elt(lower, 0);
  result.upper = paradox_numeric_elt(upper, 0);
  result.tolerance = paradox_numeric_elt(tolerance, 0);
  result.levels = VECTOR_ELT(levels_column, 0);
  result.special_values = VECTOR_ELT(specials_column, 0);
  result.custom_check = result.kind == VALUE_UTY
    ? utility_callback(VECTOR_ELT(cargo_column, 0))
    : R_NilValue;
  return result;
}

SEXP paradox_tune_token_snapshot_list(SEXP private_environment, SEXP self,
    SEXP values) {
  SEXP value_names = R_NilValue;
  SEXP tokens = PROTECT(snapshot_tune_tokens_from_values(values, &value_names));
  PROTECT(value_names);
  SEXP token_names = PROTECT(Rf_getAttrib(tokens, R_NamesSymbol));
  R_xlen_t work_since_interrupt = 0;
  SEXP all_domains = PROTECT(paradox_param_set_domains(
    private_environment,
    self
  ));
  SEXP targets = PROTECT(select_tune_target_domains(
    all_domains,
    value_names,
    token_names,
    &work_since_interrupt
  ));
  for (R_xlen_t index = 0; index < XLENGTH(tokens); ++index) {
    SEXP token = VECTOR_ELT(tokens, index);
    const token_snapshot_kind_t token_kind = exact_token_kind(token);
    if (token_kind == TOKEN_SNAPSHOT_OBJECT &&
        TYPEOF(VECTOR_ELT(token, 0)) == ENVSXP) {
      SEXP content = VECTOR_ELT(token, 0);
      SEXP candidate_private = R_NilValue;
      if (!exact_base_param_set_shell(
          content,
          &candidate_private,
          NULL
        )) {
        UNPROTECT(5);
        Rf_error("Malformed ObjectTuneToken: malformed exact BASE ParamSet");
      }
      PROTECT(candidate_private);
      SEXP capability = PROTECT(paradox_param_set_base_snapshot_state(
        candidate_private,
        content
      ));
      SET_VECTOR_ELT(token, 0, capability);
      UNPROTECT(2);
      continue;
    }
    value_spec_t spec = target_domain_spec(VECTOR_ELT(targets, index));
    SEXP diagnostic = PROTECT(validate_tune_token(
      &spec,
      token,
      &work_since_interrupt,
      NULL
    ));
    if (diagnostic != R_NilValue) {
      paradox_error_from_scalar_string(diagnostic);
    }
    UNPROTECT(1);
  }

  /* This one capsule-backed Domain projection is the complete target graph
   * snapshot consumed after candidate callbacks begin. In particular, each
   * Domain carries detached `.requirements`, so R never rereads `self$deps`.
   */
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, tokens);
  SET_VECTOR_ELT(result, 1, targets);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, Rf_mkChar("tokens"));
  SET_STRING_ELT(names, 1, Rf_mkChar("targets"));
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(7);
  return result;
}

static SEXP initialize_point(SEXP stable_values, const check_plan_t *plan,
    point_t *point) {
  point->values = stable_values;
  point->names = Rf_getAttrib(stable_values, R_NamesSymbol);
  point->size = XLENGTH(stable_values);
  point->param_rows = paradox_temporary_alloc(
    point->size == 0 ? 1 : point->size, sizeof(*point->param_rows)
  );
  point->value_for_param = paradox_temporary_alloc(
    plan->parameter_count == 0 ? 1 : plan->parameter_count,
    sizeof(*point->value_for_param)
  );
  for (R_xlen_t row = 0; row < plan->parameter_count; ++row) {
    point->value_for_param[row] = R_XLEN_T_MAX;
  }
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t index = 0; index < point->size; ++index) {
    R_xlen_t row = R_XLEN_T_MAX;
    SEXP id = STRING_ELT(point->names, index);
    if (!find_id(&plan->root_ids, id, &row, &work_since_interrupt)) {
      return paradox_parameter_unavailable_diagnostic(
        id,
        plan->root_ids.ids,
        ""
      );
    }
    if (point->value_for_param[row] != R_XLEN_T_MAX) {
      return check_message("Names must be unique");
    }
    point->param_rows[index] = row;
    point->value_for_param[row] = index;
  }
  return R_NilValue;
}

static int scalar_number(SEXP value, double *result) {
  /* Length/ALTREP predicates are not defined for every SEXP type.  In
   * particular, a named NULL is a valid list element at this boundary: it
   * must reach ordinary Domain rejection (or special-value admission), not
   * raise an internal R C-API error. */
  if (Rf_isS4(value) || Rf_isObject(value) ||
      (TYPEOF(value) != REALSXP && TYPEOF(value) != INTSXP)) {
    return FALSE;
  }
  if (ALTREP(value) || XLENGTH(value) != 1) {
    return FALSE;
  }
  if (TYPEOF(value) == REALSXP) {
    *result = REAL_ELT(value, 0);
    return !ISNAN(*result);
  }
  if (TYPEOF(value) == INTSXP) {
    const int integer = INTEGER_ELT(value, 0);
    if (integer == NA_INTEGER) return FALSE;
    *result = (double) integer;
    return TRUE;
  }
  return FALSE;
}

static paradox_builtin_domain_kind_t builtin_value_kind(value_kind_t kind) {
  switch (kind) {
  case VALUE_DBL: return PARADOX_BUILTIN_DOMAIN_DBL;
  case VALUE_INT: return PARADOX_BUILTIN_DOMAIN_INT;
  case VALUE_FCT: return PARADOX_BUILTIN_DOMAIN_FCT;
  case VALUE_LGL: return PARADOX_BUILTIN_DOMAIN_LGL;
  case VALUE_UTY: return PARADOX_BUILTIN_DOMAIN_UTY;
  default:
    Rf_error("Internal error: unknown ParamSet Domain kind");
  }
  return PARADOX_BUILTIN_DOMAIN_UNKNOWN;
}

static paradox_builtin_value_spec_t builtin_value_spec(
    const value_spec_t *spec) {
  const paradox_builtin_value_spec_t result = {
    builtin_value_kind(spec->kind),
    spec->lower,
    spec->upper,
    spec->tolerance,
    spec->levels,
    spec->special_values
  };
  return result;
}

static SEXP validate_ordinary_value(const value_spec_t *spec, SEXP value,
    int sanitize, SEXP *replacement,
    R_xlen_t *work_since_interrupt) {
  *replacement = value;
  const paradox_builtin_value_spec_t builtin = builtin_value_spec(spec);
  const paradox_builtin_value_result_t checked = paradox_builtin_value_check(
    &builtin,
    value,
    TRUE,
    work_since_interrupt
  );
  if (checked.special) return R_NilValue;

  if (spec->kind == VALUE_UTY) {
    if (spec->custom_check == R_NilValue) return R_NilValue;
    SEXP call = PROTECT(paradox_unary_callback_call(
      spec->custom_check,
      value
    ));
    SEXP answer = PROTECT(Rf_eval(call, R_BaseEnv));
    if (TYPEOF(answer) == LGLSXP && XLENGTH(answer) == 1 &&
        LOGICAL_ELT(answer, 0) == TRUE) {
      UNPROTECT(2);
      return R_NilValue;
    }
    SEXP result;
    if (TYPEOF(answer) == STRSXP && XLENGTH(answer) == 1 &&
        STRING_ELT(answer, 0) != NA_STRING) {
      result = PROTECT(utf8_message_2(
        "", spec->id, ": ", STRING_ELT(answer, 0), ""
      ));
    } else {
      result = PROTECT(utf8_message_1(
        "", spec->id,
        ": `custom_check` must return TRUE or one non-missing string"
      ));
    }
    UNPROTECT(3);
    return result;
  }
  if (checked.failure != PARADOX_BUILTIN_VALUE_OK) {
    return paradox_builtin_value_diagnostic(
      spec->id,
      &builtin,
      value,
      &checked
    );
  }
  if (sanitize && spec->kind == VALUE_DBL) {
    double clamped = checked.number;
    if (clamped < spec->lower) clamped = spec->lower;
    if (clamped > spec->upper) clamped = spec->upper;
    *replacement = Rf_ScalarReal(clamped);
  } else if (sanitize && spec->kind == VALUE_INT) {
    *replacement = Rf_ScalarInteger((int) checked.canonical_number);
  }
  return R_NilValue;
}

static SEXP token_call(SEXP token) {
  SEXP call = named_list_element(token, "call");
  return TYPEOF(call) == STRSXP && !ALTREP(call) && XLENGTH(call) == 1 &&
      STRING_ELT(call, 0) != NA_STRING
    ? STRING_ELT(call, 0)
    : Rf_mkChar("TuneToken");
}

static SEXP short_value(SEXP value) {
  if (TYPEOF(value) == STRSXP && XLENGTH(value) == 1 &&
      STRING_ELT(value, 0) != NA_STRING) {
    return utf8_message_1("\"", STRING_ELT(value, 0), "\"");
  }
  if ((TYPEOF(value) == LGLSXP || TYPEOF(value) == INTSXP ||
       TYPEOF(value) == REALSXP) && XLENGTH(value) == 1) {
    SEXP text = PROTECT(Rf_asChar(value));
    SEXP result = PROTECT(Rf_ScalarString(text));
    UNPROTECT(2);
    return result;
  }
  return Rf_mkString("<value>");
}

static int scalar_logical_or(SEXP value, int fallback, int *valid) {
  if (value == R_UnboundValue || value == R_NilValue) {
    *valid = TRUE;
    return fallback;
  }
  *valid = TYPEOF(value) == LGLSXP && !ALTREP(value) &&
    XLENGTH(value) == 1 && LOGICAL_ELT(value, 0) != NA_LOGICAL;
  return *valid ? LOGICAL_ELT(value, 0) : fallback;
}

static int optional_number(SEXP content, const char *name, double *result,
    int *present) {
  SEXP value = named_list_element(content, name);
  if (value == R_UnboundValue || value == R_NilValue) {
    *present = FALSE;
    return TRUE;
  }
  *present = TRUE;
  return scalar_number(value, result);
}

static SEXP validate_object_token(const value_spec_t *spec, SEXP content,
    R_xlen_t *work_since_interrupt, SEXP *receipt_result) {
  if (receipt_result != NULL) *receipt_result = R_NilValue;
  if (Rf_inherits(content, "Domain")) {
    /* Object Domain content was deeply owned, structurally admitted, and
     * bounded by snapshot_tune_token(). Candidate transformation and target
     * compatibility intentionally belong only to search-space conversion. */
    return R_NilValue;
  }
  if (TYPEOF(content) == ENVSXP) {
    SEXP candidate_private = R_NilValue;
    SEXP candidate_core = R_NilValue;
    const token_param_set_admission_t admission =
      admit_token_param_set_base(
        content,
        &candidate_private,
        &candidate_core,
        work_since_interrupt
    );
    if (admission == TOKEN_PARAM_SET_MALFORMED) {
      return utf8_message_1(
        "", spec->id,
        ": tune token invalid: malformed ParamSet candidate"
      );
    }
    if (admission == TOKEN_PARAM_SET_UNBOUNDED) {
      return utf8_message_1(
        "", spec->id,
        ": tune token invalid: ParamSet candidate must be nonempty and bounded"
      );
    }
    if (receipt_result != NULL) {
      PROTECT(content);
      PROTECT(candidate_private);
      PROTECT(candidate_core);
      SEXP receipt = PROTECT(Rf_allocVector(VECSXP, 3));
      SET_VECTOR_ELT(receipt, 0, content);
      SET_VECTOR_ELT(receipt, 1, candidate_private);
      SET_VECTOR_ELT(receipt, 2, candidate_core);
      *receipt_result = receipt;
      UNPROTECT(4);
    }
    return R_NilValue;
  }
  return utf8_message_1(
    "", spec->id, ": tune token invalid: unsupported candidate"
  );
}

static SEXP validate_tune_token(const value_spec_t *spec, SEXP token,
    R_xlen_t *work_since_interrupt, SEXP *receipt_result) {
  if (receipt_result != NULL) *receipt_result = R_NilValue;
  if (TYPEOF(token) != VECSXP || ALTREP(token)) {
    return utf8_message_1(
      "", spec->id, ": tune token invalid: malformed token"
    );
  }
  SEXP content = named_list_element(token, "content");
  if (content == R_UnboundValue) {
    return utf8_message_1(
      "", spec->id, ": tune token invalid: missing content"
    );
  }
  SEXP call = token_call(token);

  if (Rf_inherits(token, "RangeTuneToken")) {
    if (spec->kind != VALUE_DBL && spec->kind != VALUE_INT) {
      return utf8_message_1(
        "", call,
        " for non-numeric param must have zero or one argument."
      );
    }
    if (TYPEOF(content) != VECSXP || ALTREP(content)) {
      return utf8_message_1(
        "", spec->id, ": tune token invalid: malformed range"
      );
    }
    double lower = spec->lower;
    double upper = spec->upper;
    int has_lower = FALSE;
    int has_upper = FALSE;
    if (!optional_number(content, "lower", &lower, &has_lower) ||
        !optional_number(content, "upper", &upper, &has_upper)) {
      return utf8_message_1(
        "", spec->id,
        ": tune token invalid: range bounds must be numeric"
      );
    }
    int valid_logscale = FALSE;
    (void) scalar_logical_or(
      named_list_element(content, "logscale"), FALSE, &valid_logscale
    );
    if (!valid_logscale) {
      return utf8_message_1(
        "", spec->id,
        ": tune token invalid: logscale must be TRUE or FALSE"
      );
    }
    if ((has_lower && lower < spec->lower) ||
        (has_upper && upper > spec->upper) || lower > upper) {
      SEXP bounds = PROTECT(check_message(
        "%g, upper %g", lower, upper
      ));
      SEXP result = PROTECT(utf8_message_3(
        "", call, " range not compatible with param ", spec->id,
        ": lower ", STRING_ELT(bounds, 0), ""
      ));
      UNPROTECT(2);
      return result;
    }
    if (!R_FINITE(lower) || !R_FINITE(upper)) {
      SEXP shown_lower = PROTECT(format_public_number(lower));
      SEXP shown_upper = PROTECT(format_public_number(upper));
      SEXP result = PROTECT(utf8_message_3(
        "", spec->id, " range must be bounded, but is [",
        STRING_ELT(shown_lower, 0), ", ",
        STRING_ELT(shown_upper, 0), "]"
      ));
      UNPROTECT(3);
      return result;
    }
    return R_NilValue;
  }

  if (Rf_inherits(token, "FullTuneToken")) {
    int bounded = spec->kind == VALUE_FCT || spec->kind == VALUE_LGL ||
      ((spec->kind == VALUE_DBL || spec->kind == VALUE_INT) &&
       R_FINITE(spec->lower) && R_FINITE(spec->upper));
    if (!bounded) {
      return utf8_message_2(
        "", call, " must give a range for unbounded parameter ",
        spec->id, "."
      );
    }
    int valid_logscale = FALSE;
    const int logscale = scalar_logical_or(
      TYPEOF(content) == VECSXP
        ? named_list_element(content, "logscale")
        : R_UnboundValue,
      FALSE,
      &valid_logscale
    );
    if (!valid_logscale || (logscale && spec->kind != VALUE_DBL &&
        spec->kind != VALUE_INT)) {
      return utf8_message_1(
        "", call,
        ": logscale only valid for numeric / integer parameters."
      );
    }
    return R_NilValue;
  }

  if (Rf_inherits(token, "ObjectTuneToken")) {
    return validate_object_token(
      spec,
      content,
      work_since_interrupt,
      receipt_result
    );
  }
  return utf8_message_1(
    "", spec->id, ": tune token invalid: unknown token kind"
  );
}

static int has_tag(const check_plan_t *plan, SEXP id, const char *tag,
    R_xlen_t *work_since_interrupt) {
  SEXP ids = VECTOR_ELT(plan->root_tags, 0);
  SEXP tags = VECTOR_ELT(plan->root_tags, 1);
  for (R_xlen_t row = 0; row < XLENGTH(ids); ++row) {
    paradox_account_work(work_since_interrupt);
    if (paradox_domain_strings_equal(STRING_ELT(ids, row), id) &&
        paradox_domain_string_is(STRING_ELT(tags, row), tag)) {
      return TRUE;
    }
  }
  return FALSE;
}

static SEXP local_values(const check_node_t *node, const check_plan_t *plan,
    const point_t *point, const unsigned char *active) {
  R_xlen_t size = 0;
  R_xlen_t work_since_interrupt = 0;
  const R_xlen_t row_count = node->checked_params.row_count;
  R_xlen_t *root_rows = paradox_temporary_alloc(
    row_count == 0 ? 1 : row_count,
    sizeof(*root_rows)
  );
  for (R_xlen_t row = 0; row < row_count; ++row) {
    R_xlen_t root_row = R_XLEN_T_MAX;
    if (!find_id(
        &plan->root_ids, STRING_ELT(node->root_ids, row), &root_row,
        &work_since_interrupt
      )) {
      Rf_error("Corrupt ParamSet graph: node-to-root identifier is unknown");
    }
    root_rows[row] = root_row;
    if (point->value_for_param[root_row] != R_XLEN_T_MAX &&
        (active == NULL || active[root_row])) {
      ++size;
    }
  }
  SEXP values = PROTECT(Rf_allocVector(VECSXP, size));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, size));
  R_xlen_t output = 0;
  for (R_xlen_t row = 0; row < row_count; ++row) {
    const R_xlen_t root_row = root_rows[row];
    const R_xlen_t input = point->value_for_param[root_row];
    if (input == R_XLEN_T_MAX || (active != NULL && !active[root_row])) {
      continue;
    }
    SET_VECTOR_ELT(values, output, VECTOR_ELT(point->values, input));
    SET_STRING_ELT(
      names, output, STRING_ELT(node->checked_params.ids, row)
    );
    ++output;
  }
  Rf_setAttrib(values, R_NamesSymbol, names);
  UNPROTECT(2);
  return values;
}

static void ensure_point_activity(check_plan_t *plan,
    const point_t *point, point_activity_t *activity) {
  if (activity->ready) return;
  initialize_activity_mapping(plan);
  activity->result.active = paradox_temporary_alloc(
    plan->parameter_count == 0 ? 1 : plan->parameter_count,
    sizeof(*activity->result.active)
  );
  activity->result.reasons = activity->retain_reasons
    ? paradox_temporary_alloc(
        plan->dependency_count == 0 ? 1 : plan->dependency_count,
        sizeof(*activity->result.reasons)
      )
    : NULL;
  const paradox_activity_plan_t activity_plan = {
    plan->parameter_count,
    plan->defaults,
    point->values,
    point->value_for_param,
    plan->dependency_count,
    plan->dependency_child,
    plan->dependency_parent,
    (SEXP const *) plan->dependency_rhs
  };
  R_xlen_t work_since_interrupt = 0;
  paradox_activity_evaluate(
    &activity_plan,
    &activity->result,
    &work_since_interrupt
  );
  activity->ready = TRUE;
}

static SEXP collapsed_ids(SEXP ids, const unsigned char *selected,
    R_xlen_t selected_count) {
  if (selected_count == 0) return Rf_mkString("");
  if (selected_count > R_XLEN_T_MAX / 2 + 1) {
    Rf_error("ParamSet presence diagnostic exceeds native vector bounds");
  }
  R_xlen_t *rows = paradox_temporary_alloc(selected_count, sizeof(*rows));
  R_xlen_t output = 0;
  for (R_xlen_t row = 0; row < XLENGTH(ids); ++row) {
    if (selected[row]) rows[output++] = row;
  }
  for (R_xlen_t right = 1; right < selected_count; ++right) {
    const R_xlen_t candidate = rows[right];
    R_xlen_t left = right;
    while (left > 0 && strcmp(
        CHAR(STRING_ELT(ids, rows[left - 1])),
        CHAR(STRING_ELT(ids, candidate))
      ) > 0) {
      rows[left] = rows[left - 1];
      --left;
    }
    rows[left] = candidate;
  }
  const R_xlen_t piece_count = selected_count * 2 - 1;
  paradox_utf8_piece_t *pieces = paradox_temporary_alloc(
    piece_count, sizeof(*pieces)
  );
  R_xlen_t piece = 0;
  for (R_xlen_t index = 0; index < selected_count; ++index) {
    if (index != 0) {
      pieces[piece++] = paradox_utf8_ascii_piece(", ");
    }
    pieces[piece++] = paradox_utf8_charsxp_piece(
      STRING_ELT(ids, rows[index])
    );
  }
  if (piece != piece_count) {
    Rf_error("Internal error while building a ParamSet presence diagnostic");
  }
  return paradox_utf8_message(pieces, piece_count);
}

static SEXP check_presence(check_plan_t *plan, const point_t *point,
    presence_t presence, point_activity_t *activity) {
  if (presence == PRESENCE_NONE) return R_NilValue;
  ensure_point_activity(plan, point, activity);
  const R_xlen_t count = plan->parameter_count;
  unsigned char *plain_missing = paradox_temporary_alloc(
    count == 0 ? 1 : count, sizeof(*plain_missing)
  );
  unsigned char *active_missing = paradox_temporary_alloc(
    count == 0 ? 1 : count, sizeof(*active_missing)
  );
  memset(plain_missing, 0, (size_t) (count == 0 ? 1 : count));
  memset(active_missing, 0, (size_t) (count == 0 ? 1 : count));
  R_xlen_t plain_count = 0;
  R_xlen_t active_count = 0;
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t row = 0; row < count; ++row) {
    if (point->value_for_param[row] != R_XLEN_T_MAX) continue;
    SEXP id = STRING_ELT(plan->root_ids.ids, row);
    if (presence == PRESENCE_REQUIRED &&
        !has_tag(plan, id, "required", &work_since_interrupt)) continue;
    if (!plan->has_dependency[row]) {
      plain_missing[row] = 1;
      ++plain_count;
    } else if (activity->result.active[row]) {
      active_missing[row] = 1;
      ++active_count;
    }
  }
  if (plain_count != 0) {
    SEXP ids = PROTECT(collapsed_ids(
      plan->root_ids.ids, plain_missing, plain_count
    ));
    SEXP result = PROTECT(utf8_message_1(
      "All parameters must be present. Missing parameters: ",
      STRING_ELT(ids, 0), ""
    ));
    UNPROTECT(2);
    return result;
  }
  if (active_count != 0) {
    SEXP ids = PROTECT(collapsed_ids(
      plan->root_ids.ids, active_missing, active_count
    ));
    SEXP result = PROTECT(utf8_message_1(
      "All parameters must be present. "
      "Missing parameters with satisfied dependencies: ",
      STRING_ELT(ids, 0), ""
    ));
    UNPROTECT(2);
    return result;
  }
  return R_NilValue;
}

static SEXP condition_description(SEXP condition, SEXP on,
    R_xlen_t *work_since_interrupt) {
  paradox_builtin_condition_kind_t kind;
  SEXP rhs = R_NilValue;
  if (!paradox_builtin_condition_exact(
      condition, &kind, &rhs, work_since_interrupt
    )) {
    Rf_error("Corrupt ParamSet state: malformed built-in dependency condition");
  }
  /* Fill a temporary scalar without coercion or dispatch. */
  SEXP scalar = PROTECT(Rf_allocVector((SEXPTYPE) TYPEOF(rhs), 1));
  switch ((SEXPTYPE) TYPEOF(rhs)) {
  case LGLSXP:
    SET_LOGICAL_ELT(scalar, 0, LOGICAL_ELT(rhs, 0));
    break;
  case INTSXP:
    SET_INTEGER_ELT(scalar, 0, INTEGER_ELT(rhs, 0));
    break;
  case REALSXP:
    SET_REAL_ELT(scalar, 0, REAL_ELT(rhs, 0));
    break;
  case STRSXP:
    SET_STRING_ELT(scalar, 0, STRING_ELT(rhs, 0));
    break;
  default:
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet state: unsupported Condition rhs type");
  }
  SEXP actual_shown = PROTECT(short_value(scalar));
  SEXP result;
  if (kind == PARADOX_BUILTIN_CONDITION_EQUAL) {
    result = PROTECT(utf8_message_2(
      "", on, " == ", STRING_ELT(actual_shown, 0), ""
    ));
  } else {
    result = PROTECT(utf8_message_2(
      "", on, " %in% {", STRING_ELT(actual_shown, 0),
      XLENGTH(rhs) > 1 ? ", ...}" : "}"
    ));
  }
  UNPROTECT(3);
  return result;
}

static SEXP check_constraints(check_plan_t *plan,
    const point_t *point, point_activity_t *activity) {
  int any_constraint = FALSE;
  int any_dependency = FALSE;
  for (R_xlen_t node_index = 0;
      node_index < plan->graph.count; ++node_index) {
    const check_node_t *node = &plan->graph.nodes[node_index];
    if (!node->semantic) continue;
    any_constraint =
      any_constraint || node->constraint != R_NilValue;
    any_dependency =
      any_dependency || node->checked_dependencies.row_count != 0;
  }
  if (!any_constraint) return R_NilValue;
  if (any_dependency) {
    ensure_point_activity(plan, point, activity);
  }
  const unsigned char *active = any_dependency
    ? activity->result.active
    : NULL;

  for (R_xlen_t node_index = 0;
      node_index < plan->graph.count; ++node_index) {
    const check_node_t *node = &plan->graph.nodes[node_index];
    if (!node->semantic) continue;
    if (node->constraint == R_NilValue) continue;
    SEXP values = PROTECT(local_values(
      node,
      plan,
      point,
      active
    ));
    SEXP call = PROTECT(Rf_lang2(node->constraint, values));
    SEXP answer = PROTECT(Rf_eval(call, R_BaseEnv));
    if (TYPEOF(answer) != LGLSXP || XLENGTH(answer) != 1 ||
        LOGICAL_ELT(answer, 0) == NA_LOGICAL) {
      UNPROTECT(3);
      Rf_error("ParamSet constraint must return one non-missing logical value");
    }
    const int accepted = LOGICAL_ELT(answer, 0);
    UNPROTECT(3);
    if (!accepted) return Rf_mkString("Constraint not fulfilled.");
  }
  return R_NilValue;
}

static int plan_has_dependencies(const check_plan_t *plan) {
  for (R_xlen_t node_index = 0;
      node_index < plan->graph.count; ++node_index) {
    const check_node_t *node = &plan->graph.nodes[node_index];
    if (node->semantic && node->checked_dependencies.row_count != 0) {
      return TRUE;
    }
  }
  return FALSE;
}

static SEXP check_dependencies(check_plan_t *plan,
    const point_t *point, point_activity_t *activity) {
  ensure_point_activity(plan, point, activity);
  if (plan->dependency_count != 0 &&
      activity->result.reasons == NULL) {
    Rf_error("Internal error: dependency diagnostics were not retained");
  }
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t dependency = 0;
      dependency < plan->dependency_count;
      ++dependency) {
    paradox_account_work(&work_since_interrupt);
    const R_xlen_t child = plan->dependency_child[dependency];
    const R_xlen_t child_value = point->value_for_param[child];
    if (child_value == R_XLEN_T_MAX ||
        Rf_inherits(
          VECTOR_ELT(point->values, child_value),
          "TuneToken"
        )) {
      continue;
    }
    const paradox_activity_reason_t reason =
      activity->result.reasons[dependency];
    if (paradox_activity_reason_is_satisfied(reason)) continue;

    SEXP exposed_id = plan->dependency_exposed_id[dependency];
    SEXP exposed_on = plan->dependency_exposed_on[dependency];
    if (reason == PARADOX_ACTIVITY_VALUE_UNSUPPORTED ||
        reason == PARADOX_ACTIVITY_DEFAULT_UNSUPPORTED) {
      return utf8_message_1(
        "Dependency comparison for '",
        exposed_on,
        "' requires a plain scalar logical, integer, double, or character value."
      );
    }

    SEXP description = PROTECT(condition_description(
      plan->dependency_condition[dependency],
      exposed_on,
      &work_since_interrupt
    ));
    SEXP result;
    if (reason == PARADOX_ACTIVITY_PARENT_INACTIVE) {
      result = PROTECT(utf8_message_3(
        "",
        exposed_id,
        ": can only be set if the following condition is met '",
        STRING_ELT(description, 0),
        "'. Instead the parameter '",
        exposed_on,
        "' is inactive because its own dependencies are not satisfied"
      ));
    } else if (reason == PARADOX_ACTIVITY_PARENT_ABSENT) {
      result = PROTECT(utf8_message_4(
        "",
        exposed_id,
        ": can only be set if the following condition is met '",
        STRING_ELT(description, 0),
        "'. Instead the parameter value for '",
        exposed_on,
        "' is not set at all. Try setting '",
        exposed_on,
        "' to a value that satisfies the condition"
      ));
    } else {
      const R_xlen_t parent = plan->dependency_parent[dependency];
      const int from_default =
        reason == PARADOX_ACTIVITY_DEFAULT_MISMATCH;
      const R_xlen_t parent_value = parent == R_XLEN_T_MAX
        ? R_XLEN_T_MAX
        : point->value_for_param[parent];
      SEXP effective = from_default
        ? VECTOR_ELT(plan->defaults, parent)
        : VECTOR_ELT(point->values, parent_value);
      SEXP shown = PROTECT(short_value(effective));
      result = from_default
        ? PROTECT(utf8_message_4(
            "",
            exposed_id,
            ": can only be set if the following condition is met '",
            STRING_ELT(description, 0),
            "'. Instead the parameter value for '",
            exposed_on,
            "' is not set at all and its default is: ",
            STRING_ELT(shown, 0),
            ""
          ))
        : PROTECT(utf8_message_4(
            "",
            exposed_id,
            ": can only be set if the following condition is met '",
            STRING_ELT(description, 0),
            "'. Instead the current parameter value is: ",
            exposed_on,
            " == ",
            STRING_ELT(shown, 0),
            ""
          ));
      UNPROTECT(1);
    }
    UNPROTECT(2);
    return result;
  }
  return R_NilValue;
}

static SEXP validate_initialized_point(check_plan_t *plan,
    point_t *point, int check_constraints_flag,
    int check_dependencies_flag, int sanitize, presence_t presence,
    int allow_token, SEXP *receipts_result) {
  if (receipts_result != NULL) *receipts_result = R_NilValue;
  point_activity_t activity = {
    .ready = FALSE,
    .retain_reasons = check_dependencies_flag,
    .result = {NULL, NULL}
  };
  for (R_xlen_t index = 0; index < point->size; ++index) {
    if (!allow_token && Rf_inherits(
        VECTOR_ELT(point->values, index), "TuneToken"
      )) {
      return Rf_mkString("TuneTokens are not allowed to be present.");
    }
  }

  SEXP presence_failure = PROTECT(check_presence(
    plan,
    point,
    presence,
    &activity
  ));
  if (presence_failure != R_NilValue) {
    UNPROTECT(1);
    return presence_failure;
  }
  UNPROTECT(1);

  /* Admit every live ObjectTuneToken ParamSet and root its selected capsule
   * generation before any user callback runs. Non-Object tokens leave a NULL
   * slot, keeping the receipt index identical to the point index without a
   * second counting/allocation pass. */
  SEXP receipts = R_NilValue;
  int protected_count = 0;
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t index = 0; index < point->size; ++index) {
    paradox_account_work(&work_since_interrupt);
    SEXP value = VECTOR_ELT(point->values, index);
    if (!Rf_inherits(value, "TuneToken")) continue;
    const value_spec_t *spec = &plan->specs[point->param_rows[index]];
    if (Rf_inherits(value, "InternalTuneToken") && !has_tag(
        plan, spec->id, "internal_tuning", &work_since_interrupt
      )) {
      UNPROTECT(protected_count);
      SEXP diagnostic = PROTECT(utf8_message_1(
        "Trying to assign InternalTuneToken to parameter '", spec->id,
        "' which is not tagged with 'internal_tuning'."
      ));
      paradox_error_from_scalar_string(diagnostic);
    }
    SEXP receipt = R_NilValue;
    SEXP failure = validate_tune_token(
      spec,
      value,
      &work_since_interrupt,
      &receipt
    );
    if (failure != R_NilValue) {
      UNPROTECT(protected_count);
      return failure;
    }
    if (receipt != R_NilValue) {
      if (receipts == R_NilValue) {
        /* UNPROTECT is strictly LIFO, and the receipt set, not the
         * individual receipt, is the root this operation retains until it
         * hands the set to its caller. Protect the receipt only across the
         * one allocation, then leave the set itself on the stack. */
        PROTECT(receipt);
        receipts = Rf_allocVector(VECSXP, point->size);
        SET_VECTOR_ELT(receipts, index, receipt);
        UNPROTECT(1);
        PROTECT(receipts);
        ++protected_count;
      } else {
        SET_VECTOR_ELT(receipts, index, receipt);
      }
    }
  }
  verify_token_receipts(receipts);

  SEXP sanitized = R_NilValue;
  if (sanitize) {
    sanitized = PROTECT(Rf_shallow_duplicate(point->values));
    ++protected_count;
  }
  for (R_xlen_t index = 0; index < point->size; ++index) {
    paradox_account_work(&work_since_interrupt);
    const R_xlen_t row = point->param_rows[index];
    const value_spec_t *spec = &plan->specs[row];
    SEXP value = VECTOR_ELT(point->values, index);
    SEXP failure = R_NilValue;
    if (!Rf_inherits(value, "TuneToken")) {
      SEXP replacement = value;
      failure = validate_ordinary_value(
        spec, value, sanitize, &replacement, &work_since_interrupt
      );
      if (failure == R_NilValue && sanitize && replacement != value) {
        PROTECT(replacement);
        SET_VECTOR_ELT(sanitized, index, replacement);
        UNPROTECT(1);
      }
    }
    if (failure != R_NilValue) {
      PROTECT(failure);
      verify_token_receipts(receipts);
      UNPROTECT(1);
      UNPROTECT(protected_count);
      return failure;
    }
  }

  if (check_constraints_flag || check_dependencies_flag) {
    SEXP failure = check_constraints_flag
      ? PROTECT(check_constraints(plan, point, &activity))
      : PROTECT(R_NilValue);
    if (failure == R_NilValue && check_dependencies_flag) {
      UNPROTECT(1);
      failure = PROTECT(check_dependencies(plan, point, &activity));
    }
    if (failure != R_NilValue) {
      verify_token_receipts(receipts);
      UNPROTECT(protected_count + 1);
      return failure;
    }
    UNPROTECT(1);
  }

  verify_token_receipts(receipts);

  SEXP result = PROTECT(Rf_allocVector(LGLSXP, 1));
  LOGICAL(result)[0] = TRUE;
  if (sanitize) Rf_setAttrib(result, Rf_install("sanitized"), sanitized);
  /* Result allocation and attribute installation may run a pending finalizer.
   * Recheck once afterward so public check returns and checked assignment
   * receives receipts for the exact generation that survived all callbacks
   * and allocations in this operation. */
  verify_token_receipts(receipts);
  if (receipts_result != NULL) *receipts_result = receipts;
  UNPROTECT(protected_count + 1);
  return result;
}

static SEXP validate_point(check_plan_t *plan, SEXP stable_values,
    int check_constraints_flag, int check_dependencies_flag,
    int sanitize, presence_t presence, int allow_token,
    SEXP *receipts_result) {
  point_t point;
  SEXP structural_failure = PROTECT(initialize_point(
    stable_values, plan, &point
  ));
  if (structural_failure != R_NilValue) {
    UNPROTECT(1);
    return structural_failure;
  }
  UNPROTECT(1);
  return validate_initialized_point(
    plan,
    &point,
    check_constraints_flag,
    check_dependencies_flag,
    sanitize,
    presence,
    allow_token,
    receipts_result
  );
}

static SEXP snapshot_table_column(SEXP column) {
  if (TYPEOF(column) != VECSXP) return snapshot_parameter_value(column);
  const R_xlen_t size = XLENGTH(column);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  for (R_xlen_t row = 0; row < size; ++row) {
    SEXP value = PROTECT(VECTOR_ELT(column, row));
    SEXP copy = PROTECT(snapshot_parameter_value(value));
    SET_VECTOR_ELT(result, row, copy);
    UNPROTECT(2);
  }
  SHALLOW_DUPLICATE_ATTRIB(result, column);
  UNPROTECT(1);
  return result;
}

static int ordinary_table_class(SEXP table, const char *required_class) {
  if (TYPEOF(table) != VECSXP || ALTREP(table) || Rf_isS4(table)) {
    return FALSE;
  }
  const paradox_public_table_kind_t kind = paradox_public_table_kind(table);
  return strcmp(required_class, "data.table") == 0
    ? kind == PARADOX_PUBLIC_DATA_TABLE
    : kind != PARADOX_PUBLIC_TABLE_NONE;
}

static SEXP snapshot_table(SEXP table, R_xlen_t *row_count) {
  if (!ordinary_table_class(table, "data.frame")) {
    return check_message("Must be a data.frame or data.table");
  }
  const R_xlen_t columns = XLENGTH(table);
  SEXP names = PROTECT(Rf_getAttrib(table, R_NamesSymbol));
  if (names != R_NilValue &&
      (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
        Rf_isObject(names) || !no_attributes(names) ||
        XLENGTH(names) != columns)) {
    UNPROTECT(1);
    return check_message("Table columns must be named");
  }
  if (names == R_NilValue && columns != 0) {
    UNPROTECT(1);
    return check_message("Table columns must be named");
  }
  SEXP stable_names = PROTECT(names == R_NilValue
    ? Rf_allocVector(STRSXP, 0)
    : Rf_duplicate(names));
  for (R_xlen_t column = 0; column < columns; ++column) {
    if (STRING_ELT(stable_names, column) == NA_STRING ||
        CHAR(STRING_ELT(stable_names, column))[0] == '\0') {
      UNPROTECT(2);
      return check_message("Table column names must be non-missing and non-empty");
    }
  }
  if (Rf_any_duplicated(stable_names, FALSE) != 0) {
    UNPROTECT(2);
    return check_message("Table column names must be unique");
  }

  R_xlen_t rows = 0;
  if (!paradox_public_table_row_count(table, &rows)) {
    UNPROTECT(2);
    return check_message("Invalid data.frame row names");
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, columns));
  for (R_xlen_t column = 0; column < columns; ++column) {
    SEXP source = PROTECT(VECTOR_ELT(table, column));
    SEXP stable = PROTECT(snapshot_table_column(source));
    const SEXPTYPE type = (SEXPTYPE) TYPEOF(stable);
    if (type != LGLSXP && type != INTSXP && type != REALSXP &&
        type != CPLXSXP && type != STRSXP && type != RAWSXP &&
        type != VECSXP) {
      UNPROTECT(5);
      return check_message("Unsupported table column type '%s'",
        Rf_type2char(type));
    }
    if (XLENGTH(stable) != rows) {
      UNPROTECT(5);
      return check_message(column == 0
        ? "Invalid data.frame row names"
        : "Table columns must have equal lengths");
    }
    SET_VECTOR_ELT(result, column, stable);
    UNPROTECT(2);
  }
  Rf_setAttrib(result, R_NamesSymbol, stable_names);
  *row_count = rows;
  UNPROTECT(3);
  return result;
}

static int table_cell_missing(SEXP column, R_xlen_t row) {
  switch ((SEXPTYPE) TYPEOF(column)) {
  case LGLSXP:
    return LOGICAL_ELT(column, row) == NA_LOGICAL;
  case INTSXP:
    return INTEGER_ELT(column, row) == NA_INTEGER;
  case REALSXP:
    return ISNAN(REAL_ELT(column, row));
  case CPLXSXP: {
    const Rcomplex value = COMPLEX_ELT(column, row);
    return ISNAN(value.r) || ISNAN(value.i);
  }
  case STRSXP:
    return STRING_ELT(column, row) == NA_STRING;
  default:
    return FALSE;
  }
}

static SEXP table_cell(SEXP column, R_xlen_t row) {
  SEXP result;
  /* Ordinary columns carry no attributes and take the allocation-free path
   * below. An attributed column needs its own one-element cell: the copied
   * set may contain the column's own length (`names`, `dim`, `dimnames`),
   * and Rf_ScalarLogical() hands out R's shared TRUE/FALSE/NA singletons,
   * which must never receive attributes. */
  const int attributed = !paradox_api_has_no_attributes(column);
  switch ((SEXPTYPE) TYPEOF(column)) {
  case LGLSXP:
    if (attributed) {
      result = PROTECT(Rf_allocVector(LGLSXP, 1));
      SET_LOGICAL_ELT(result, 0, LOGICAL_ELT(column, row));
    } else {
      result = PROTECT(Rf_ScalarLogical(LOGICAL_ELT(column, row)));
    }
    break;
  case INTSXP:
    result = PROTECT(Rf_ScalarInteger(INTEGER_ELT(column, row)));
    break;
  case REALSXP:
    result = PROTECT(Rf_ScalarReal(REAL_ELT(column, row)));
    break;
  case CPLXSXP:
    result = PROTECT(Rf_ScalarComplex(COMPLEX_ELT(column, row)));
    break;
  case STRSXP:
    result = PROTECT(Rf_ScalarString(STRING_ELT(column, row)));
    break;
  case RAWSXP: {
    result = PROTECT(Rf_allocVector(RAWSXP, 1));
    paradox_api_set_raw_elt(result, 0, RAW_ELT(column, row));
    break;
  }
  case VECSXP:
    return VECTOR_ELT(column, row);
  default:
    Rf_error("Internal error: unsupported table column type");
  }
  if (attributed) {
    SHALLOW_DUPLICATE_ATTRIB(result, column);
    Rf_setAttrib(result, R_NamesSymbol, R_NilValue);
    Rf_setAttrib(result, R_DimSymbol, R_NilValue);
    Rf_setAttrib(result, R_DimNamesSymbol, R_NilValue);
  }
  UNPROTECT(1);
  return result;
}

static SEXP table_point(SEXP table, R_xlen_t row) {
  const R_xlen_t columns = XLENGTH(table);
  SEXP table_names = Rf_getAttrib(table, R_NamesSymbol);
  R_xlen_t present = 0;
  for (R_xlen_t column = 0; column < columns; ++column) {
    if (!table_cell_missing(VECTOR_ELT(table, column), row)) ++present;
  }
  SEXP values = PROTECT(Rf_allocVector(VECSXP, present));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, present));
  R_xlen_t output = 0;
  for (R_xlen_t column = 0; column < columns; ++column) {
    SEXP source = VECTOR_ELT(table, column);
    if (table_cell_missing(source, row)) continue;
    SEXP value = PROTECT(table_cell(source, row));
    SET_VECTOR_ELT(values, output, value);
    SET_STRING_ELT(names, output, STRING_ELT(table_names, column));
    UNPROTECT(1);
    ++output;
  }
  Rf_setAttrib(values, R_NamesSymbol, names);
  UNPROTECT(2);
  return values;
}

SEXP paradox_param_set_validate_current_graph(
    SEXP private_environment, SEXP self, SEXP selected_core) {
  /*
   * Migration preflight must validate current nodes without refreshing a
   * stale SHADOW into its private environment. The stored SHADOW generation
   * and metadata are valid current state; ordinary operations may refresh it
   * later. This read-only plan still admits the complete capsule graph and all
   * canonical tables, callbacks, translations, and edges.
   */
  SEXP result = PROTECT(Rf_ScalarLogical(TRUE));
  PROTECT_INDEX root_plan_index;
  SEXP root_plan;
  PROTECT_WITH_INDEX(root_plan = R_NilValue, &root_plan_index);
  check_plan_t plan;
  build_graph(
    private_environment,
    self,
    &plan.graph,
    &root_plan,
    root_plan_index,
    FALSE,
    selected_core
  );
  initialize_check_plan(&plan);
  if (!graph_receipts_are_current(&plan.graph)) {
    UNPROTECT(2);
    Rf_error("ParamSet capsule graph changed during read-only validation");
  }
  UNPROTECT(2);
  return result;
}

SEXP paradox_param_set_validate_current_roots(SEXP selves) {
  /*
   * This is the migration session's final all-roots barrier. Every selected
   * shell generation remains in one shared protected root plan until all
   * graphs have been admitted; only then do allocation-free receipt scans
   * certify the complete set simultaneously.
   */
  if (TYPEOF(selves) != VECSXP || ALTREP(selves) || Rf_isS4(selves)) {
    Rf_error("Current ParamSet roots must be an ordinary list");
  }
  initialize_check_binding_symbols();
  SEXP result = PROTECT(Rf_ScalarLogical(TRUE));
  PROTECT_INDEX root_plan_index;
  SEXP root_plan;
  PROTECT_WITH_INDEX(root_plan = R_NilValue, &root_plan_index);
  const R_xlen_t count = XLENGTH(selves);
  check_plan_t *plans = paradox_temporary_alloc(
    count == 0 ? 1 : count,
    sizeof(*plans)
  );

  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP self = VECTOR_ELT(selves, index);
    if (TYPEOF(self) != ENVSXP || Rf_isS4(self)) {
      UNPROTECT(2);
      Rf_error("Current ParamSet root is not an ordinary environment");
    }
    SEXP enclosure = PROTECT(paradox_api_plain_binding_snapshot(
      self,
      check_enclosure_symbol
    ));
    SEXP private_environment = PROTECT(
      TYPEOF(enclosure) == ENVSXP && !Rf_isS4(enclosure)
        ? paradox_api_plain_binding_snapshot(
            enclosure,
            check_private_symbol
          )
        : R_UnboundValue
    );
    SEXP selected_core = PROTECT(
      TYPEOF(private_environment) == ENVSXP &&
        !Rf_isS4(private_environment)
        ? paradox_api_plain_binding_snapshot(
            private_environment,
            check_core_symbol
          )
        : R_UnboundValue
    );
    build_graph(
      private_environment,
      self,
      &plans[index].graph,
      &root_plan,
      root_plan_index,
      FALSE,
      selected_core
    );
    initialize_check_plan(&plans[index]);
    UNPROTECT(3);
  }
  for (R_xlen_t index = 0; index < count; ++index) {
    if (!graph_receipts_are_current(&plans[index].graph)) {
      UNPROTECT(2);
      Rf_error(
        "ParamSet roots changed during joint read-only validation"
      );
    }
  }
  UNPROTECT(2);
  return result;
}

SEXP paradox_param_set_check_builtin_with_receipts(
    SEXP private_environment, SEXP self,
    SEXP values, SEXP check_strict, SEXP sanitize, SEXP presence,
    SEXP allow_token, int enforce_dependencies,
    SEXP *receipts_result) {
  if (receipts_result != NULL) *receipts_result = R_NilValue;
  const int strict = exact_flag(check_strict, "check_strict");
  const int do_sanitize = exact_flag(sanitize, "sanitize");
  const presence_t required_presence = exact_presence(presence);
  const int tokens = exact_flag(allow_token, "allow_token");

  SEXP stable_values = PROTECT(snapshot_named_list(values));
  if (TYPEOF(stable_values) != VECSXP) {
    UNPROTECT(1);
    return stable_values;
  }
  PROTECT_INDEX root_plan_index;
  SEXP root_plan;
  PROTECT_WITH_INDEX(root_plan = R_NilValue, &root_plan_index);
  check_plan_t plan;
  build_check_plan(
    private_environment, self, &plan, &root_plan, root_plan_index
  );
  SEXP result = PROTECT(validate_point(
    &plan,
    stable_values,
    strict,
    strict && enforce_dependencies,
    do_sanitize,
    required_presence,
    tokens,
    receipts_result
  ));
  UNPROTECT(3);
  return result;
}

SEXP paradox_param_set_check_builtin(SEXP private_environment, SEXP self,
    SEXP values, SEXP check_strict, SEXP sanitize, SEXP presence,
    SEXP allow_token) {
  return paradox_param_set_check_builtin_with_receipts(
    private_environment,
    self,
    values,
    check_strict,
    sanitize,
    presence,
    allow_token,
    TRUE,
    NULL
  );
}

SEXP paradox_param_set_check_dependencies_builtin(
    SEXP private_environment, SEXP self, SEXP values) {
  static const char *const allowed_attributes[] = {"names"};
  if (TYPEOF(values) != VECSXP || ALTREP(values) || Rf_isObject(values) ||
      !paradox_api_has_only_attributes(values, allowed_attributes, 1)) {
    return Rf_mkString("Must be an ordinary named list");
  }

  SEXP stable_values = PROTECT(snapshot_named_list(values));
  if (TYPEOF(stable_values) != VECSXP) {
    UNPROTECT(1);
    return stable_values;
  }
  PROTECT_INDEX root_plan_index;
  SEXP root_plan;
  PROTECT_WITH_INDEX(root_plan = R_NilValue, &root_plan_index);
  check_plan_t plan;
  build_check_plan(
    private_environment, self, &plan, &root_plan, root_plan_index
  );

  point_t point;
  point_activity_t activity = {
    .ready = FALSE,
    .retain_reasons = TRUE,
    .result = {NULL, NULL}
  };
  SEXP result = PROTECT(initialize_point(stable_values, &plan, &point));
  if (result == R_NilValue) {
    UNPROTECT(1);
    result = PROTECT(check_dependencies(&plan, &point, &activity));
  }
  if (result == R_NilValue) {
    UNPROTECT(1);
    result = PROTECT(Rf_ScalarLogical(TRUE));
  }
  UNPROTECT(3);
  return result;
}

static void constraint_input_error(SEXP failure, const char *argument) {
  if (TYPEOF(failure) != STRSXP || XLENGTH(failure) != 1 ||
      STRING_ELT(failure, 0) == NA_STRING) {
    Rf_error("Internal error while validating a ParamSet constraint input");
  }
  SEXP prefix = PROTECT(check_message("Assertion on '%s' failed: ", argument));
  SEXP message = PROTECT(utf8_message_2(
    "", STRING_ELT(prefix, 0), "", STRING_ELT(failure, 0),
    paradox_charsxp_ends_sentence(STRING_ELT(failure, 0)) ? "" : "."
  ));
  paradox_error_from_scalar_string(message);
}

static void initialize_constraint_point(SEXP values,
    check_plan_t *plan, point_t *point, int assert_value,
    const char *argument) {
  SEXP structural_failure = PROTECT(initialize_point(values, plan, point));
  if (structural_failure != R_NilValue) {
    constraint_input_error(structural_failure, argument);
  }
  UNPROTECT(1);
  if (!assert_value) return;

  SEXP validation = PROTECT(validate_initialized_point(
    plan,
    point,
    FALSE,
    FALSE,
    FALSE,
    PRESENCE_NONE,
    TRUE,
    NULL
  ));
  if (TYPEOF(validation) != LGLSXP || XLENGTH(validation) != 1 ||
      LOGICAL_ELT(validation, 0) != TRUE) {
    constraint_input_error(validation, argument);
  }
  UNPROTECT(1);
}

static int plan_has_constraint(const check_plan_t *plan) {
  for (R_xlen_t node_index = 0;
      node_index < plan->graph.count; ++node_index) {
    const check_node_t *node = &plan->graph.nodes[node_index];
    if (node->semantic && node->constraint != R_NilValue) return TRUE;
  }
  return FALSE;
}

SEXP paradox_param_set_test_constraint_builtin(
    SEXP private_environment, SEXP self, SEXP values, SEXP assert_value) {
  const int validate = exact_flag(assert_value, "assert_value");
  SEXP stable_values = PROTECT(snapshot_named_list(values));
  if (TYPEOF(stable_values) != VECSXP) {
    constraint_input_error(stable_values, "x");
  }

  PROTECT_INDEX root_plan_index;
  SEXP root_plan;
  PROTECT_WITH_INDEX(root_plan = R_NilValue, &root_plan_index);
  check_plan_t plan;
  build_check_plan(
    private_environment, self, &plan, &root_plan, root_plan_index
  );
  if (!validate && !plan_has_constraint(&plan)) {
    SEXP result = PROTECT(Rf_ScalarLogical(TRUE));
    UNPROTECT(3);
    return result;
  }
  point_t point;
  point_activity_t activity = {
    .ready = FALSE,
    .retain_reasons = FALSE,
    .result = {NULL, NULL}
  };
  initialize_constraint_point(
    stable_values, &plan, &point, validate, "x"
  );
  SEXP failure = PROTECT(check_constraints(&plan, &point, &activity));
  SEXP result = PROTECT(Rf_ScalarLogical(failure == R_NilValue));
  UNPROTECT(4);
  return result;
}

SEXP paradox_param_set_test_constraint_dt_builtin(
    SEXP private_environment, SEXP self, SEXP table, SEXP assert_value) {
  SEXP table_shell = PROTECT(paradox_materialize_public_table_shell(table));
  if (!ordinary_table_class(table_shell, "data.table")) {
    UNPROTECT(1);
    Rf_error("Assertion on 'x' failed: Must be a data.table.");
  }
  const int validate = exact_flag(assert_value, "assert_value");
  R_xlen_t rows = 0;
  SEXP stable_table = PROTECT(snapshot_table(table_shell, &rows));
  if (TYPEOF(stable_table) != VECSXP) {
    constraint_input_error(stable_table, "x");
  }

  PROTECT_INDEX root_plan_index;
  SEXP root_plan;
  PROTECT_WITH_INDEX(root_plan = R_NilValue, &root_plan_index);
  check_plan_t plan;
  build_check_plan(
    private_environment, self, &plan, &root_plan, root_plan_index
  );

  if (validate) {
    const void *row_watermark = vmaxget();
    for (R_xlen_t row = 0; row < rows; ++row) {
      if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
        R_CheckUserInterrupt();
      }
      SEXP values = PROTECT(table_point(stable_table, row));
      point_t point;
      initialize_constraint_point(values, &plan, &point, TRUE, "x");
      UNPROTECT(1);
      vmaxset(row_watermark);
    }
  }

  SEXP result = PROTECT(Rf_allocVector(LGLSXP, rows));
  if (!plan_has_constraint(&plan)) {
    for (R_xlen_t row = 0; row < rows; ++row) LOGICAL(result)[row] = TRUE;
    UNPROTECT(4);
    return result;
  }

  /*
   * The per-row watermark must never reclaim topology cached in `plan`.
   * Materialize that operation-lifetime mapping before taking the watermark;
   * only row-local activity/value scratch is then released between callbacks.
   */
  if (plan_has_dependencies(&plan)) {
    initialize_activity_mapping(&plan);
  }
  const void *row_watermark = vmaxget();
  for (R_xlen_t row = 0; row < rows; ++row) {
    if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP values = PROTECT(table_point(stable_table, row));
    point_t point;
    point_activity_t activity = {
      .ready = FALSE,
      .retain_reasons = FALSE,
      .result = {NULL, NULL}
    };
    initialize_constraint_point(values, &plan, &point, FALSE, "x");
    SEXP failure = PROTECT(check_constraints(
      &plan,
      &point,
      &activity
    ));
    LOGICAL(result)[row] = failure == R_NilValue;
    UNPROTECT(2);
    vmaxset(row_watermark);
  }
  UNPROTECT(4);
  return result;
}

SEXP paradox_param_set_check_dt_builtin(SEXP private_environment, SEXP self,
    SEXP table, SEXP check_strict, SEXP presence, SEXP allow_token) {
  const int strict = exact_flag(check_strict, "check_strict");
  const presence_t required_presence = exact_presence(presence);
  const int tokens = exact_flag(allow_token, "allow_token");

  SEXP table_shell = PROTECT(paradox_materialize_public_table_shell(table));
  R_xlen_t rows = 0;
  SEXP stable_table = PROTECT(snapshot_table(table_shell, &rows));
  if (TYPEOF(stable_table) != VECSXP) {
    UNPROTECT(2);
    return stable_table;
  }
  PROTECT_INDEX root_plan_index;
  SEXP root_plan;
  PROTECT_WITH_INDEX(root_plan = R_NilValue, &root_plan_index);
  check_plan_t plan;
  build_check_plan(
    private_environment, self, &plan, &root_plan, root_plan_index
  );
  /*
   * Keep operation-lifetime topology below the per-row watermark. Point,
   * mask, and DFS scratch can then be reclaimed after every row instead of
   * growing linearly with a wide check table.
   */
  if (strict || required_presence != PRESENCE_NONE) {
    initialize_activity_mapping(&plan);
  }
  const void *row_watermark = vmaxget();
  for (R_xlen_t row = 0; row < rows; ++row) {
    if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP point = PROTECT(table_point(stable_table, row));
    SEXP result = PROTECT(validate_point(
      &plan,
      point,
      strict,
      strict,
      FALSE,
      required_presence,
      tokens,
      NULL
    ));
    if (TYPEOF(result) != LGLSXP || XLENGTH(result) != 1 ||
        LOGICAL_ELT(result, 0) != TRUE) {
      vmaxset(row_watermark);
      UNPROTECT(5);
      return result;
    }
    UNPROTECT(2);
    vmaxset(row_watermark);
  }
  SEXP result = PROTECT(Rf_ScalarLogical(TRUE));
  UNPROTECT(4);
  return result;
}
