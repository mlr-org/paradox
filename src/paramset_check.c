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
#include "paramset_params_internal.h"
#include "paramset_shadow.h"
#include "parameter_suggestion.h"
#include "r_api_compat.h"
#include "r_utils.h"
#include "shell_auth.h"
#include "upgrade_graph.h"

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

typedef paradox_domain_id_map_t id_map_t;

enum node_root_slot {
  NODE_ROOT_SELF = 0,
  NODE_ROOT_CLASS,
  NODE_ROOT_ENCLOSURE,
  NODE_ROOT_PRIVATE,
  NODE_ROOT_ASSERT_VALUES,
  NODE_ROOT_SELECTED_CORE,
  NODE_ROOT_SHADOW_SIGNATURE,
  NODE_ROOT_SHADOW_SIGNATURE_CONTENT,
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
  SEXP shadow_signature;
  SEXP shadow_signature_content;
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
  if (TYPEOF(value) != VECSXP || ALTREP(value) || Rf_isS4(value)) {
    return R_UnboundValue;
  }
  int has_names = FALSE;
  if (!paradox_bounded_metadata_has_tag(
      value,
      R_NamesSymbol,
      &has_names
    ) || !has_names) {
    return R_UnboundValue;
  }
  SEXP names = PROTECT(paradox_api_raw_attribute(
    value,
    R_NamesSymbol
  ));
  if (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
      Rf_isObject(names) || !no_attributes(names) ||
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

/* A duplicate identifier reaching the check engine is corrupt capsule state;
 * the shared index reports the condition and this engine keeps its wording. */
static void initialize_id_map(SEXP ids, id_map_t *map) {
  switch (paradox_domain_id_map_init(ids, map)) {
  case PARADOX_DOMAIN_ID_MAP_OK:
    return;
  case PARADOX_DOMAIN_ID_MAP_TOO_MANY:
    Rf_error("ParamSet contains too many parameters");
  case PARADOX_DOMAIN_ID_MAP_CAPACITY:
    Rf_error("ParamSet identifier index exceeds platform bounds");
  case PARADOX_DOMAIN_ID_MAP_DUPLICATE:
    Rf_error("Corrupt ParamSet state: duplicate parameter identifier");
  }
}

static int find_id(const id_map_t *map, SEXP id, R_xlen_t *row,
    R_xlen_t *work_since_interrupt) {
  return paradox_domain_id_map_find(map, id, row, work_since_interrupt);
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
  if (TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isS4(sets) ||
      Rf_isObject(sets) ||
      !paradox_api_has_single_attribute(sets, "names")) {
    return FALSE;
  }
  SEXP observed_names = PROTECT(paradox_api_raw_attribute(
    sets,
    R_NamesSymbol
  ));
  if (TYPEOF(observed_names) != STRSXP || ALTREP(observed_names) ||
      Rf_isS4(observed_names) || Rf_isObject(observed_names) ||
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
  if (TYPEOF(cargo) != VECSXP || ALTREP(cargo) || Rf_isS4(cargo) ||
      Rf_isObject(cargo)) {
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
  /*
   * Atomic values are overwhelmingly common here.  They have no descendants
   * to retain, so avoid allocating traversal state on their valid path.
   * S4 values are opaque identity tokens at this boundary, including an
   * S4-classed VECSXP: neither their payload nor their class contract belongs
   * to the built-in value engine.
   */
  if (Rf_isS4(value)) return TRUE;
  if (ALTREP(value)) return FALSE;
  if (TYPEOF(value) != VECSXP) return TRUE;

  R_xlen_t capacity = 16;
  R_xlen_t size = 1;
  R_xlen_t seen_count = 0;
  /*
   * Both halves of this carrier are scanned by the collector: [0, capacity)
   * is the pending DFS stack and [capacity, 2 * capacity) is the visited set.
   * Raw SEXP arrays allocated with R_alloc() are not GC roots.  In
   * particular, growing such an array can run a pending finalizer after a
   * descendant was detached from its owning list, leaving only an unscanned
   * raw pointer.  One indexed carrier keeps every exact selected descendant
   * alive through growth while retaining allocation-free cycle handling.
   */
  PROTECT_INDEX carrier_index;
  SEXP carrier;
  PROTECT_WITH_INDEX(
    carrier = Rf_allocVector(VECSXP, capacity * 2),
    &carrier_index
  );
  SET_VECTOR_ELT(carrier, 0, value);
  int valid = TRUE;
  R_xlen_t work_since_interrupt = 0;
  while (size != 0) {
    paradox_account_work(&work_since_interrupt);
    SEXP current = VECTOR_ELT(carrier, --size);
    if (Rf_isS4(current)) continue;
    if (ALTREP(current)) {
      valid = FALSE;
      break;
    }
    if (TYPEOF(current) != VECSXP) continue;
    int visited = FALSE;
    for (R_xlen_t index = 0; index < seen_count; ++index) {
      if (VECTOR_ELT(carrier, capacity + index) == current) {
        visited = TRUE;
        break;
      }
    }
    if (visited) continue;
    const R_xlen_t children = XLENGTH(current);
    if (children > R_XLEN_T_MAX - size ||
        seen_count == R_XLEN_T_MAX) {
      valid = FALSE;
      break;
    }
    R_xlen_t required = size + children;
    if (seen_count + 1 > required) required = seen_count + 1;
    if (required > capacity) {
      const R_xlen_t maximum_capacity = R_XLEN_T_MAX / 2;
      if (required > maximum_capacity) {
        valid = FALSE;
        break;
      }
      R_xlen_t expanded = capacity;
      while (expanded < required) {
        expanded = expanded > maximum_capacity / 2
          ? maximum_capacity
          : expanded * 2;
      }
      SEXP replacement = Rf_allocVector(VECSXP, expanded * 2);
      for (R_xlen_t index = 0; index < size; ++index) {
        SET_VECTOR_ELT(replacement, index, VECTOR_ELT(carrier, index));
      }
      for (R_xlen_t index = 0; index < seen_count; ++index) {
        SET_VECTOR_ELT(
          replacement,
          expanded + index,
          VECTOR_ELT(carrier, capacity + index)
        );
      }
      SET_VECTOR_ELT(replacement, expanded + seen_count, current);
      REPROTECT(carrier = replacement, carrier_index);
      capacity = expanded;
    } else {
      SET_VECTOR_ELT(carrier, capacity + seen_count, current);
    }
    ++seen_count;
    for (R_xlen_t index = 0; index < children; ++index) {
      SET_VECTOR_ELT(carrier, size++, VECTOR_ELT(current, index));
    }
  }
  UNPROTECT(1);
  return valid;
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
      if (!paradox_domain_numeric_capsule_is_canonical(
          kind == VALUE_INT,
          paradox_numeric_elt(lower, row),
          paradox_numeric_elt(upper, row),
          paradox_numeric_elt(tolerance, row)
        )) {
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
    int stored_token = FALSE;
    if (kind == VALUE_UTY) {
      if (!paradox_api_opaque_leaf_class_matches(
          stored_value,
          "TuneToken",
          &stored_token
        )) {
        Rf_error("Corrupt ParamSet state: invalid stored parameter value");
      }
    }
    if ((kind != VALUE_UTY || stored_token) &&
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
    int heal, int retain_receipt) {
  node->roots = append_node_roots(
    root_plan, root_plan_index, self, private_environment
  );
  node->self = self;
  node->private_environment = private_environment;
  node->classes = R_NilValue;
  node->assert_values = R_NilValue;
  node->shadow_signature = R_NilValue;
  node->shadow_signature_content = R_NilValue;
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
  if (!heal || retain_receipt) {
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

  if (!heal || retain_receipt) {
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
  if ((!heal || retain_receipt) && node->kind != class_kind) {
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
    /*
     * A read-only Shadow preview is a fresh capsule that is deliberately not
     * installed in the shell.  Root it before the signature-content snapshot
     * below allocates: retaining only the stored generation in
     * NODE_ROOT_SELECTED_CORE does not retain this authoritative preview.
     * Committed refreshes are already reachable through the private binding,
     * but using the same immediate handoff keeps the two paths uniform.
     */
    SET_VECTOR_ELT(node->roots, NODE_ROOT_CORE, core);
    if (!paradox_core_is_canonical(core) ||
        paradox_core_kind(core) != PARADOX_CORE_SHADOW ||
        !paradox_shadow_metadata_is_exact(core)) {
      Rf_error("Corrupt ParamSetShadow refreshed core capsule");
    }
    if (retain_receipt) {
      /*
       * Refresh may install a new authoritative projection.  A receipt owns
       * the generation selected after that installation, not the stale
       * binding observed before refresh.
       */
      node->selected_core = core;
      SET_VECTOR_ELT(node->roots, NODE_ROOT_SELECTED_CORE, core);
    }
  }
  if (node->kind == PARADOX_CORE_SHADOW &&
      (retain_receipt || !heal)) {
    node->shadow_signature = paradox_shadow_metadata_signature(
      node->selected_core
    );
    if (node->shadow_signature == R_UnboundValue) {
      Rf_error("Corrupt ParamSetShadow native snapshot metadata");
    }
    SET_VECTOR_ELT(
      node->roots,
      NODE_ROOT_SHADOW_SIGNATURE,
      node->shadow_signature
    );
    SEXP signature_content = PROTECT(paradox_shadow_signature_content_snapshot(
      node->shadow_signature
    ));
    if (signature_content == R_NilValue) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSetShadow native snapshot metadata");
    }
    node->shadow_signature_content = signature_content;
    SET_VECTOR_ELT(
      node->roots,
      NODE_ROOT_SHADOW_SIGNATURE_CONTENT,
      signature_content
    );
    UNPROTECT(1);
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
        ALTREP(postfix) || Rf_isS4(postfix) ||
        !no_attributes(postfix) || XLENGTH(postfix) != 1 ||
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
        TYPEOF(postfix) != LGLSXP || ALTREP(postfix) || Rf_isS4(postfix) ||
        !no_attributes(postfix) || XLENGTH(postfix) != 1 ||
        LOGICAL_ELT(postfix, 0) != FALSE) {
      Rf_error("Corrupt ParamSet state: BASE node contains graph metadata");
    }
  } else {
    SEXP postfix = VECTOR_ELT(node->state, PARADOX_CORE_POSTFIX);
    SEXP origin = TYPEOF(node->sets) == VECSXP && !ALTREP(node->sets) &&
        !Rf_isS4(node->sets) && XLENGTH(node->sets) == 1
      ? VECTOR_ELT(node->sets, 0)
      : R_NilValue;
    if (TYPEOF(node->sets) != VECSXP || ALTREP(node->sets) ||
        Rf_isS4(node->sets) || Rf_isObject(node->sets) ||
        !no_attributes(node->sets) || XLENGTH(node->sets) != 1 ||
        TYPEOF(origin) != ENVSXP || Rf_isS4(origin) ||
        paradox_domain_private_environment(origin) ==
          R_UnboundValue || VECTOR_ELT(node->sets, 0) == self ||
        node->translation != R_NilValue ||
        TYPEOF(postfix) != LGLSXP || ALTREP(postfix) || Rf_isS4(postfix) ||
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
    int heal, int retain_receipt, SEXP selected_root_core) {
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
  /*
   * The selected capsules below are immutable and individually rooted, but
   * initializing later nodes allocates. A pending finalizer can therefore
   * perform a supported mutation between two selections and otherwise leave
   * an ordinary (non-receipted) check plan describing generations that never
   * coexisted. Every semantic installation advances the session-wide state
   * epoch; cache refreshes deliberately do not because they preserve the
   * graph's denotation. One terminal comparison closes this build without a
   * per-node receipt scan on the hot check path.
   */
  const uintptr_t entry_epoch = paradox_core_state_epoch_value();
  initialize_graph(graph);
  R_xlen_t work_since_interrupt = 0;
  initialize_node(
    self, private_environment, R_XLEN_T_MAX, R_XLEN_T_MAX, TRUE, graph,
    &graph->nodes[0], root_plan, root_plan_index, &work_since_interrupt,
    selected_root_core, heal, retain_receipt
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
        &work_since_interrupt, R_NilValue, heal, retain_receipt
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
  if (heal && !retain_receipt &&
      paradox_core_state_epoch_value() != entry_epoch) {
    Rf_error("ParamSet graph changed while a native operation was being constructed");
  }
}

static int graph_receipts_are_current(const check_graph_t *graph) {
  for (R_xlen_t index = 0; index < graph->count; ++index) {
    const check_node_t *node = &graph->nodes[index];
    SEXP current_classes = R_NilValue;
    const paradox_core_kind_t current_kind =
      paradox_param_set_class_kind_raw(node->self, &current_classes);
    if (node->classes == R_NilValue ||
        current_classes != node->classes ||
        current_kind != node->kind ||
        (node->kind == PARADOX_CORE_SHADOW &&
          !paradox_shadow_signature_receipt_is_current(
            node->selected_core,
            node->shadow_signature,
            node->shadow_signature_content
          ))) {
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

static int internal_tuning_node_receipt_is_current(SEXP roots) {
  if (TYPEOF(roots) != VECSXP || ALTREP(roots) || Rf_isS4(roots) ||
      Rf_isObject(roots) || !paradox_api_has_no_attributes(roots) ||
      XLENGTH(roots) != NODE_ROOT_COUNT ||
      check_enclosure_symbol == NULL || check_self_symbol == NULL ||
      check_private_symbol == NULL || check_core_symbol == NULL ||
      check_assert_values_symbol == NULL) {
    return FALSE;
  }

  SEXP self = VECTOR_ELT(roots, NODE_ROOT_SELF);
  SEXP classes = VECTOR_ELT(roots, NODE_ROOT_CLASS);
  SEXP expected_enclosure = VECTOR_ELT(roots, NODE_ROOT_ENCLOSURE);
  SEXP expected_private = VECTOR_ELT(roots, NODE_ROOT_PRIVATE);
  SEXP expected_assert_values =
    VECTOR_ELT(roots, NODE_ROOT_ASSERT_VALUES);
  SEXP expected_core = VECTOR_ELT(roots, NODE_ROOT_SELECTED_CORE);
  SEXP expected_shadow_signature =
    VECTOR_ELT(roots, NODE_ROOT_SHADOW_SIGNATURE);
  SEXP expected_shadow_signature_content =
    VECTOR_ELT(roots, NODE_ROOT_SHADOW_SIGNATURE_CONTENT);
  SEXP admitted_core = VECTOR_ELT(roots, NODE_ROOT_CORE);
  SEXP expected_state = VECTOR_ELT(roots, NODE_ROOT_STATE);
  const paradox_core_kind_t expected_kind =
    paradox_core_kind(expected_core);
  SEXP current_classes = R_NilValue;
  const paradox_core_kind_t current_kind =
    paradox_param_set_class_kind_raw(self, &current_classes);
  if (TYPEOF(self) != ENVSXP || Rf_isS4(self) ||
      TYPEOF(expected_enclosure) != ENVSXP || Rf_isS4(expected_enclosure) ||
      TYPEOF(expected_private) != ENVSXP || Rf_isS4(expected_private) ||
      admitted_core != expected_core ||
      !paradox_param_set_assert_values_is_exact(expected_assert_values) ||
      !paradox_core_is_canonical(expected_core) ||
      (expected_kind == PARADOX_CORE_SHADOW &&
        !paradox_shadow_signature_receipt_is_current(
          expected_core,
          expected_shadow_signature,
          expected_shadow_signature_content
        )) ||
      (expected_kind != PARADOX_CORE_SHADOW &&
        (expected_shadow_signature != R_NilValue ||
         expected_shadow_signature_content != R_NilValue)) ||
      paradox_core_payload(expected_core) != expected_state ||
      current_classes != classes ||
      current_kind != expected_kind) {
    return FALSE;
  }

  if (VECTOR_ELT(roots, NODE_ROOT_PARAMS) !=
        VECTOR_ELT(expected_state, PARADOX_CORE_PARAMS) ||
      VECTOR_ELT(roots, NODE_ROOT_VALUES) !=
        VECTOR_ELT(expected_state, PARADOX_CORE_VALUES) ||
      VECTOR_ELT(roots, NODE_ROOT_TAGS) !=
        VECTOR_ELT(expected_state, PARADOX_CORE_TAGS) ||
      VECTOR_ELT(roots, NODE_ROOT_DEPS) !=
        VECTOR_ELT(expected_state, PARADOX_CORE_DEPS) ||
      VECTOR_ELT(roots, NODE_ROOT_TRAFOS) !=
        VECTOR_ELT(expected_state, PARADOX_CORE_TRAFOS) ||
      VECTOR_ELT(roots, NODE_ROOT_EXTRA_TRAFO) !=
        VECTOR_ELT(expected_state, PARADOX_CORE_EXTRA_TRAFO) ||
      VECTOR_ELT(roots, NODE_ROOT_CONSTRAINT) !=
        VECTOR_ELT(expected_state, PARADOX_CORE_CONSTRAINT) ||
      VECTOR_ELT(roots, NODE_ROOT_SETS) !=
        VECTOR_ELT(expected_state, PARADOX_CORE_SETS) ||
      VECTOR_ELT(roots, NODE_ROOT_TRANSLATION) !=
        VECTOR_ELT(expected_state, PARADOX_CORE_TRANSLATION)) {
    return FALSE;
  }

  SEXP enclosure = paradox_api_plain_binding_scan(
    self,
    check_enclosure_symbol
  );
  return enclosure == expected_enclosure &&
    paradox_api_plain_binding_scan(enclosure, check_self_symbol) == self &&
    paradox_api_plain_binding_scan(enclosure, check_private_symbol) ==
      expected_private &&
    paradox_api_plain_binding_scan(self, check_assert_values_symbol) ==
      expected_assert_values &&
    paradox_api_plain_binding_scan(expected_private, check_core_symbol) ==
      expected_core;
}

void paradox_param_set_scan_internal_tuning_receipt(SEXP receipt) {
  if (TYPEOF(receipt) != VECSXP || ALTREP(receipt) ||
      Rf_isS4(receipt) || Rf_isObject(receipt) ||
      !paradox_api_has_no_attributes(receipt) ||
      XLENGTH(receipt) == 0) {
    Rf_error("Internal error: malformed internal-tuning graph receipt");
  }
  for (R_xlen_t index = 0; index < XLENGTH(receipt); ++index) {
    if (!internal_tuning_node_receipt_is_current(
        VECTOR_ELT(receipt, index)
      )) {
      Rf_error("ParamSet graph changed during internal-tuning operation");
    }
  }
}

void paradox_param_set_scan_internal_tuning_receipts(SEXP receipts) {
  if (TYPEOF(receipts) != VECSXP || ALTREP(receipts) ||
      Rf_isS4(receipts) || Rf_isObject(receipts) ||
      !paradox_api_has_no_attributes(receipts) ||
      XLENGTH(receipts) == 0) {
    Rf_error("Internal error: malformed internal-tuning receipt set");
  }
  for (R_xlen_t index = 0; index < XLENGTH(receipts); ++index) {
    paradox_param_set_scan_internal_tuning_receipt(
      VECTOR_ELT(receipts, index)
    );
  }
}

SEXP paradox_param_set_internal_tuning_receipt(SEXP receipt) {
  paradox_param_set_scan_internal_tuning_receipt(receipt);
  /*
   * This entry is also the terminal source-generation barrier for cold
   * Design generators. Returning R_NilValue is part of that contract: unlike
   * constructing a scalar result, it cannot run a finalizer after the exact
   * allocation-free scan and before the caller performs one plain-field
   * ownership handoff.
   */
  return R_NilValue;
}

static SEXP snapshot_internal_tuning_ids(SEXP ids) {
  if (TYPEOF(ids) != STRSXP || ALTREP(ids) || Rf_isS4(ids) ||
      Rf_isObject(ids) || !paradox_api_has_no_attributes(ids)) {
    Rf_error("Internal-tuning parameter IDs must be an ordinary character vector");
  }
  const R_xlen_t size = XLENGTH(ids);
  SEXP stable = PROTECT(Rf_allocVector(STRSXP, size));
  /*
   * The allocation above may run a pending finalizer. Recheck the caller
   * carrier, then copy the exact post-allocation identity sequence without
   * another allocation boundary.
   */
  if (TYPEOF(ids) != STRSXP || ALTREP(ids) || Rf_isS4(ids) ||
      Rf_isObject(ids) || !paradox_api_has_no_attributes(ids) ||
      XLENGTH(ids) != size) {
    UNPROTECT(1);
    Rf_error("Internal-tuning parameter IDs changed while being admitted");
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP id = STRING_ELT(ids, index);
    if (id == NA_STRING || Rf_getCharCE(id) == CE_BYTES ||
        CHAR(id)[0] == '\0') {
      UNPROTECT(1);
      Rf_error("Internal-tuning parameter IDs must be non-missing, non-empty strings");
    }
    SET_STRING_ELT(stable, index, id);
  }
  if (Rf_any_duplicated(stable, FALSE) != 0) {
    UNPROTECT(1);
    Rf_error("Internal-tuning parameter IDs must be unique");
  }
  UNPROTECT(1);
  return stable;
}

static R_xlen_t internal_tuning_child_node(
    const check_graph_t *graph, R_xlen_t parent, R_xlen_t child_position) {
  R_xlen_t child = R_XLEN_T_MAX;
  for (R_xlen_t index = parent + 1; index < graph->count; ++index) {
    const check_node_t *candidate = &graph->nodes[index];
    if (candidate->parent == parent &&
        candidate->child_position == child_position) {
      if (child != R_XLEN_T_MAX) {
        Rf_error("Corrupt ParamSet graph: duplicate internal-tuning route");
      }
      child = index;
    }
  }
  if (child == R_XLEN_T_MAX) {
    Rf_error("Corrupt ParamSet graph: incomplete internal-tuning route");
  }
  return child;
}

static R_xlen_t internal_tuning_terminal(
    const check_graph_t *graph, SEXP *id, R_xlen_t *work_since_interrupt) {
  R_xlen_t node_index = 0;
  for (;;) {
    const check_node_t *node = &graph->nodes[node_index];
    if (node->kind == PARADOX_CORE_BASE) {
      const R_xlen_t row = local_param_row(
        node,
        *id,
        work_since_interrupt
      );
      if (row == R_XLEN_T_MAX) {
        Rf_error("Corrupt ParamSet graph: terminal route lost its parameter");
      }
      *id = STRING_ELT(node->checked_params.ids, row);
      return node_index;
    }
    if (node->kind == PARADOX_CORE_SHADOW) {
      node_index = internal_tuning_child_node(graph, node_index, 0);
      continue;
    }
    if (node->kind != PARADOX_CORE_COLLECTION) {
      Rf_error("Corrupt ParamSet graph: unsupported internal-tuning node");
    }

    SEXP exposed = VECTOR_ELT(node->translation, 0);
    SEXP originals = VECTOR_ELT(node->translation, 1);
    SEXP owners = VECTOR_ELT(node->translation, 2);
    R_xlen_t translation_row = R_XLEN_T_MAX;
    for (R_xlen_t row = 0; row < XLENGTH(exposed); ++row) {
      paradox_account_work(work_since_interrupt);
      if (paradox_domain_strings_equal(STRING_ELT(exposed, row), *id)) {
        translation_row = row;
        break;
      }
    }
    if (translation_row == R_XLEN_T_MAX) {
      Rf_error("Corrupt ParamSetCollection internal-tuning translation");
    }
    const int owner = INTEGER_ELT(owners, translation_row);
    if (owner <= 0) {
      Rf_error("Corrupt ParamSetCollection internal-tuning owner");
    }
    *id = STRING_ELT(originals, translation_row);
    node_index = internal_tuning_child_node(
      graph,
      node_index,
      (R_xlen_t) owner - 1
    );
  }
}

static int internal_tuning_row_has_tag(
    const check_node_t *root, R_xlen_t parameter_row, const char *tag,
    R_xlen_t *work_since_interrupt) {
  SEXP id = STRING_ELT(root->checked_params.ids, parameter_row);
  for (R_xlen_t row = 0; row < root->checked_tags.row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    if (paradox_domain_strings_equal(
          STRING_ELT(root->checked_tags.ids, row),
          id
        ) && paradox_domain_string_is(
          STRING_ELT(root->checked_tags.values, row),
          tag
        )) {
      return TRUE;
    }
  }
  return FALSE;
}

SEXP paradox_param_set_internal_tuning_snapshot(
    SEXP private_environment, SEXP self, SEXP ids,
    SEXP include_root_values) {
  const int capture_root_values = exact_flag(
    include_root_values,
    "include_root_values"
  );
  const int select_all = ids == R_NilValue;
  PROTECT_INDEX stable_ids_index;
  SEXP stable_ids;
  PROTECT_WITH_INDEX(
    stable_ids = select_all
      ? R_NilValue
      : snapshot_internal_tuning_ids(ids),
    &stable_ids_index
  );

  PROTECT_INDEX root_plan_index;
  SEXP root_plan;
  PROTECT_WITH_INDEX(root_plan = R_NilValue, &root_plan_index);
  check_graph_t graph;
  build_graph(
    private_environment,
    self,
    &graph,
    &root_plan,
    root_plan_index,
    TRUE,
    TRUE,
    R_NilValue
  );
  const check_node_t *root = &graph.nodes[0];
  R_xlen_t work_since_interrupt = 0;
  if (select_all) {
    /*
     * Root the destination before the interruptible copy.  The old code kept
     * the freshly allocated STRSXP only in a C local until after this loop;
     * `paradox_account_work()` may enter `R_CheckUserInterrupt()`, across
     * which an unprotected allocation must not be retained.
     */
    REPROTECT(
      stable_ids = Rf_allocVector(
        STRSXP,
        root->checked_params.row_count
      ),
      stable_ids_index
    );
    for (R_xlen_t row = 0;
        row < root->checked_params.row_count;
        ++row) {
      paradox_account_work(&work_since_interrupt);
      SET_STRING_ELT(
        stable_ids,
        row,
        STRING_ELT(root->checked_params.ids, row)
      );
    }
  }
  const R_xlen_t size = XLENGTH(stable_ids);

  R_xlen_t *terminal_nodes = paradox_temporary_alloc(
    size == 0 ? 1 : size,
    sizeof(*terminal_nodes)
  );
  R_xlen_t *route_nodes = paradox_temporary_alloc(
    size == 0 ? 1 : size,
    sizeof(*route_nodes)
  );
  R_xlen_t *owner_nodes = paradox_temporary_alloc(
    size == 0 ? 1 : size,
    sizeof(*owner_nodes)
  );
  R_xlen_t route_count = 0;
  R_xlen_t owner_count = 0;

  SEXP original_ids = PROTECT(Rf_allocVector(STRSXP, size));
  SEXP route_indices = PROTECT(Rf_allocVector(INTSXP, size));
  SEXP owner_indices = PROTECT(Rf_allocVector(INTSXP, size));
  SEXP cargo = PROTECT(Rf_allocVector(VECSXP, size));
  SEXP internal_tuning = PROTECT(Rf_allocVector(LGLSXP, size));
  SEXP root_cargo = VECTOR_ELT(root->params, PARADOX_DOMAIN_CARGO);

  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP exposed_id = STRING_ELT(stable_ids, index);
    const R_xlen_t root_row = local_param_row(
      root,
      exposed_id,
      &work_since_interrupt
    );
    if (root_row == R_XLEN_T_MAX) {
      UNPROTECT(7);
      Rf_error("Unknown internal-tuning parameter");
    }
    SEXP original_id = exposed_id;
    const R_xlen_t terminal = internal_tuning_terminal(
      &graph,
      &original_id,
      &work_since_interrupt
    );
    terminal_nodes[index] = terminal;
    SET_STRING_ELT(original_ids, index, original_id);
    SET_VECTOR_ELT(cargo, index, VECTOR_ELT(root_cargo, root_row));
    SET_LOGICAL_ELT(
      internal_tuning,
      index,
      internal_tuning_row_has_tag(
        root,
        root_row,
        "internal_tuning",
        &work_since_interrupt
      )
    );

    R_xlen_t route = 0;
    while (route < route_count && route_nodes[route] != terminal) ++route;
    if (route == route_count) route_nodes[route_count++] = terminal;
    if (route >= INT_MAX) {
      UNPROTECT(7);
      Rf_error("Internal-tuning route count exceeds R integer bounds");
    }
    SET_INTEGER_ELT(route_indices, index, (int) (route + 1));

    R_xlen_t owner = 0;
    while (owner < owner_count &&
        graph.nodes[owner_nodes[owner]].self != graph.nodes[terminal].self) {
      ++owner;
    }
    if (owner == owner_count) {
      owner_nodes[owner_count++] = terminal;
    } else if (graph.nodes[owner_nodes[owner]].selected_core !=
        graph.nodes[terminal].selected_core) {
      UNPROTECT(7);
      Rf_error("ParamSet owner changed during internal-tuning snapshot");
    }
    if (owner >= INT_MAX) {
      UNPROTECT(7);
      Rf_error("Internal-tuning owner count exceeds R integer bounds");
    }
    SET_INTEGER_ELT(owner_indices, index, (int) (owner + 1));
  }

  SEXP root_values = R_NilValue;
  if (!capture_root_values) {
    PROTECT(root_values);
  } else {
    if (size != root->checked_params.row_count) {
      UNPROTECT(7);
      Rf_error("Internal-tuning root-value snapshot requires every parameter");
    }
    for (R_xlen_t row = 0; row < size; ++row) {
      if (!paradox_domain_strings_equal(
          STRING_ELT(stable_ids, row),
          STRING_ELT(root->checked_params.ids, row)
        )) {
        UNPROTECT(7);
        Rf_error("Internal-tuning root-value snapshot has reordered parameters");
      }
    }
    if (root->kind != PARADOX_CORE_COLLECTION) {
      root_values = root->values;
      PROTECT(root_values);
    } else {
      R_xlen_t stored_count = 0;
      for (R_xlen_t row = 0; row < size; ++row) {
        const check_node_t *terminal = &graph.nodes[terminal_nodes[row]];
        SEXP local_id = STRING_ELT(original_ids, row);
        for (R_xlen_t stored = 0;
            stored < terminal->checked_values.size;
            ++stored) {
          paradox_account_work(&work_since_interrupt);
          if (paradox_domain_strings_equal(
              STRING_ELT(terminal->checked_values.names, stored),
              local_id
            )) {
            ++stored_count;
            break;
          }
        }
      }
      root_values = PROTECT(Rf_allocVector(VECSXP, stored_count));
      SEXP root_value_names = PROTECT(Rf_allocVector(
        STRSXP,
        stored_count
      ));
      R_xlen_t output = 0;
      for (R_xlen_t row = 0; row < size; ++row) {
        const check_node_t *terminal = &graph.nodes[terminal_nodes[row]];
        SEXP local_id = STRING_ELT(original_ids, row);
        for (R_xlen_t stored = 0;
            stored < terminal->checked_values.size;
            ++stored) {
          paradox_account_work(&work_since_interrupt);
          if (!paradox_domain_strings_equal(
              STRING_ELT(terminal->checked_values.names, stored),
              local_id
            )) {
            continue;
          }
          if (output >= stored_count) {
            UNPROTECT(9);
            Rf_error("Internal error: root-value snapshot overflow");
          }
          SET_VECTOR_ELT(
            root_values,
            output,
            VECTOR_ELT(terminal->checked_values.values, stored)
          );
          SET_STRING_ELT(
            root_value_names,
            output,
            STRING_ELT(stable_ids, row)
          );
          ++output;
          break;
        }
      }
      if (output != stored_count) {
        UNPROTECT(9);
        Rf_error("Internal error: incomplete root-value snapshot");
      }
      Rf_setAttrib(root_values, R_NamesSymbol, root_value_names);
      UNPROTECT(1);
    }
  }

  SEXP owners = PROTECT(Rf_allocVector(VECSXP, owner_count));
  SEXP owner_values = PROTECT(Rf_allocVector(VECSXP, owner_count));
  SEXP owner_ids = PROTECT(Rf_allocVector(VECSXP, owner_count));
  for (R_xlen_t owner = 0; owner < owner_count; ++owner) {
    const check_node_t *node = &graph.nodes[owner_nodes[owner]];
    SET_VECTOR_ELT(owners, owner, node->self);
    /*
     * `owner_values` crosses the package boundary: it is the `param_vals`
     * argument of every documented `in_tune_fn` cargo callback. Publish the
     * public accessor's outward form -- fresh list and name carriers, typed
     * leaves detached, ParamUty leaves identity-preserving -- instead of the
     * capsule's own `.values` list, which a by-reference callback mutation
     * would silently corrupt.
     */
    const R_xlen_t stored_count = node->checked_values.size;
    SEXP outward_values = PROTECT(Rf_allocVector(VECSXP, stored_count));
    SEXP outward_names = PROTECT(Rf_allocVector(STRSXP, stored_count));
    for (R_xlen_t stored = 0; stored < stored_count; ++stored) {
      paradox_account_work(&work_since_interrupt);
      SEXP stored_name = STRING_ELT(node->checked_values.names, stored);
      int typed = TRUE;
      for (R_xlen_t row = 0; row < node->checked_params.row_count; ++row) {
        if (paradox_domain_strings_equal(
            STRING_ELT(node->checked_params.ids, row),
            stored_name
          )) {
          typed = !paradox_domain_string_is(
            STRING_ELT(node->checked_params.classes, row),
            "ParamUty"
          );
          break;
        }
      }
      SEXP detached = PROTECT(paradox_detach_stored_value_leaf(
        VECTOR_ELT(node->checked_values.values, stored),
        typed
      ));
      SET_VECTOR_ELT(outward_values, stored, detached);
      SET_STRING_ELT(outward_names, stored, stored_name);
      UNPROTECT(1);
    }
    Rf_setAttrib(outward_values, R_NamesSymbol, outward_names);
    SET_VECTOR_ELT(owner_values, owner, outward_values);
    UNPROTECT(2);
    SET_VECTOR_ELT(owner_ids, owner, node->checked_params.ids);
  }

  SEXP receipt = PROTECT(Rf_allocVector(VECSXP, graph.count));
  for (R_xlen_t index = 0; index < graph.count; ++index) {
    SET_VECTOR_ELT(receipt, index, graph.nodes[index].roots);
  }
  int any_constraint = FALSE;
  for (R_xlen_t index = 0; index < graph.count; ++index) {
    const check_node_t *node = &graph.nodes[index];
    if (node->semantic && node->constraint != R_NilValue) {
      any_constraint = TRUE;
      break;
    }
  }
  SEXP has_constraint = PROTECT(Rf_ScalarLogical(any_constraint));

  static const char *const snapshot_names[] = {
    "id", "original_id", "route_index", "owner_ps_index", "owners",
    "owner_values", "owner_ids", "root_values", "cargo",
    "internal_tuning", "has_constraint", "assert_values", "receipt"
  };
  SEXP result = PROTECT(Rf_allocVector(
    VECSXP,
    PARADOX_INTERNAL_TUNING_SNAPSHOT_COUNT
  ));
  SEXP result_names = PROTECT(Rf_allocVector(
    STRSXP,
    PARADOX_INTERNAL_TUNING_SNAPSHOT_COUNT
  ));
  SET_VECTOR_ELT(result, PARADOX_INTERNAL_TUNING_SNAPSHOT_ID, stable_ids);
  SET_VECTOR_ELT(
    result,
    PARADOX_INTERNAL_TUNING_SNAPSHOT_ORIGINAL_ID,
    original_ids
  );
  SET_VECTOR_ELT(
    result,
    PARADOX_INTERNAL_TUNING_SNAPSHOT_ROUTE_INDEX,
    route_indices
  );
  SET_VECTOR_ELT(
    result,
    PARADOX_INTERNAL_TUNING_SNAPSHOT_OWNER_INDEX,
    owner_indices
  );
  SET_VECTOR_ELT(result, PARADOX_INTERNAL_TUNING_SNAPSHOT_OWNERS, owners);
  SET_VECTOR_ELT(
    result,
    PARADOX_INTERNAL_TUNING_SNAPSHOT_OWNER_VALUES,
    owner_values
  );
  SET_VECTOR_ELT(
    result,
    PARADOX_INTERNAL_TUNING_SNAPSHOT_OWNER_IDS,
    owner_ids
  );
  SET_VECTOR_ELT(
    result,
    PARADOX_INTERNAL_TUNING_SNAPSHOT_ROOT_VALUES,
    root_values
  );
  SET_VECTOR_ELT(
    result,
    PARADOX_INTERNAL_TUNING_SNAPSHOT_CARGO,
    cargo
  );
  SET_VECTOR_ELT(
    result,
    PARADOX_INTERNAL_TUNING_SNAPSHOT_TAG,
    internal_tuning
  );
  SET_VECTOR_ELT(
    result,
    PARADOX_INTERNAL_TUNING_SNAPSHOT_HAS_CONSTRAINT,
    has_constraint
  );
  SET_VECTOR_ELT(
    result,
    PARADOX_INTERNAL_TUNING_SNAPSHOT_ASSERT_VALUES,
    root->assert_values
  );
  SET_VECTOR_ELT(
    result,
    PARADOX_INTERNAL_TUNING_SNAPSHOT_RECEIPT,
    receipt
  );
  for (int field = 0;
      field < PARADOX_INTERNAL_TUNING_SNAPSHOT_COUNT;
      ++field) {
    SET_STRING_ELT(result_names, field, Rf_mkChar(snapshot_names[field]));
  }
  Rf_setAttrib(result, R_NamesSymbol, result_names);

  /*
   * All result allocations are complete. This terminal nonallocating scan
   * proves that cargo, translations, owner stores, and shell generations were
   * simultaneously current at one instant before the snapshot escapes.
   */
  paradox_param_set_scan_internal_tuning_receipt(receipt);
  UNPROTECT(15);
  return result;
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
    FALSE, R_NilValue
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
    TRUE,
    FALSE
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
  SEXP observed_classes = R_NilValue;
  if (!paradox_api_ordinary_class_snapshot(content, &observed_classes)) {
    return FALSE;
  }
  PROTECT(observed_classes);
  const int exact_class =
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
  if (TYPEOF(receipt) != VECSXP || ALTREP(receipt) || Rf_isS4(receipt) ||
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
  SEXP classes = R_NilValue;
  if (!paradox_api_ordinary_class_snapshot(shell, &classes)) return FALSE;
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
  if (TYPEOF(receipts) != VECSXP || ALTREP(receipts) || Rf_isS4(receipts) ||
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
  if (TYPEOF(receipts) != VECSXP || ALTREP(receipts) || Rf_isS4(receipts)) {
    Rf_error("Internal error: malformed ObjectTuneToken receipt set");
  }
  for (R_xlen_t index = 0; index < XLENGTH(receipts); ++index) {
    SEXP receipt = VECTOR_ELT(receipts, index);
    if (receipt == R_NilValue) continue;
    if (TYPEOF(receipt) != VECSXP || ALTREP(receipt) || Rf_isS4(receipt) ||
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

static SEXP snapshot_ordinary_list_leaf(SEXP value) {
  /*
   * A typed list value is interpreted only as an outer semantic carrier:
   * its cells remain opaque identities. Structural ALTREP has no supported
   * contract here and must be rejected before Length/Elt can dispatch.
   */
  if (TYPEOF(value) != VECSXP || ALTREP(value) || Rf_isS4(value)) {
    Rf_error("Typed parameter list values must use ordinary list storage");
  }

  PROTECT(value);
  /*
   * Allocate an attribute-free destination before selecting the opaque cells.
   * The package-owned bounded attribute copier below never hands the
   * caller-owned pairlist to R's recursive shallow duplicator.  Its terminal
   * receipt plus the final payload comparison reject a finalizer splice
   * between cell and metadata selection.
   */
  const R_xlen_t size = XLENGTH(value);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  if (TYPEOF(value) != VECSXP || ALTREP(value) || Rf_isS4(value) ||
      XLENGTH(value) != size) {
    UNPROTECT(2);
    Rf_error("Typed parameter list value changed while being snapshotted");
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    SET_VECTOR_ELT(result, index, VECTOR_ELT(value, index));
  }
  paradox_copy_bounded_shallow_attributes(
    result,
    value,
    PARADOX_SHALLOW_ATTRIBUTES_ALL,
    "Typed parameter list metadata must use a bounded attribute set"
  );
  if (!paradox_ordinary_vector_payload_equal(value, result)) {
    UNPROTECT(2);
    Rf_error("Typed parameter list value changed while being snapshotted");
  }
  UNPROTECT(2);
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
    return paradox_snapshot_builtin_value_leaf(value);
  }
  if (type == VECSXP) {
    return snapshot_ordinary_list_leaf(value);
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

typedef struct {
  const char *const *names;
  R_xlen_t size;
  R_xlen_t representation_scalar_count;
  int internal;
  int range;
  int has_aggr;
} token_content_layout_t;

static int token_content_layout(token_snapshot_kind_t kind, R_xlen_t size,
    token_content_layout_t *layout) {
  static const char *const full_names[] = {"logscale"};
  static const char *const full_aggr_names[] = {"logscale", "aggr"};
  static const char *const range_names[] = {
    "lower", "upper", "logscale"
  };
  static const char *const range_aggr_names[] = {
    "lower", "upper", "logscale", "aggr"
  };
  if (kind == TOKEN_SNAPSHOT_OBJECT) return FALSE;

  layout->internal = kind == TOKEN_SNAPSHOT_INTERNAL_FULL ||
    kind == TOKEN_SNAPSHOT_INTERNAL_RANGE;
  layout->range = kind == TOKEN_SNAPSHOT_RANGE ||
    kind == TOKEN_SNAPSHOT_INTERNAL_RANGE;
  const R_xlen_t ordinary_size = layout->range ? 3 : 1;
  layout->has_aggr = layout->internal && size == ordinary_size + 1;
  if ((!layout->internal && size != ordinary_size) ||
      (layout->internal && size != ordinary_size &&
        size != ordinary_size + 1)) {
    return FALSE;
  }
  layout->names = layout->range
    ? (layout->has_aggr ? range_aggr_names : range_names)
    : (layout->has_aggr ? full_aggr_names : full_names);
  layout->size = size;
  layout->representation_scalar_count = ordinary_size;
  return TRUE;
}

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

/* The closed set of admitted TuneToken class vectors, shared by the exact
 * classifier, the erroring admission below, and owned snapshot construction.
 * One table set keeps those three from drifting apart. */
typedef struct {
  const char *const *names;
  R_xlen_t size;
  token_snapshot_kind_t kind;
} token_class_vector_t;

static const char *const token_full_classes[] = {
  "FullTuneToken", "TuneToken"
};
static const char *const token_range_classes[] = {
  "RangeTuneToken", "TuneToken"
};
static const char *const token_object_classes[] = {
  "ObjectTuneToken", "TuneToken"
};
static const char *const token_internal_full_classes[] = {
  "InternalTuneToken", "FullTuneToken", "TuneToken"
};
static const char *const token_internal_range_classes[] = {
  "InternalTuneToken", "RangeTuneToken", "TuneToken"
};
static const char *const token_shell_names[] = {"content", "call"};

#define TOKEN_CLASS_VECTOR_COUNT 5
static const token_class_vector_t
    token_class_vectors[TOKEN_CLASS_VECTOR_COUNT] = {
  {token_full_classes, 2, TOKEN_SNAPSHOT_FULL},
  {token_range_classes, 2, TOKEN_SNAPSHOT_RANGE},
  {token_object_classes, 2, TOKEN_SNAPSHOT_OBJECT},
  {token_internal_full_classes, 3, TOKEN_SNAPSHOT_INTERNAL_FULL},
  {token_internal_range_classes, 3, TOKEN_SNAPSHOT_INTERNAL_RANGE}
};

static int token_kind_from_classes(SEXP classes,
    token_snapshot_kind_t *kind, R_xlen_t *work_since_interrupt) {
  for (int index = 0; index < TOKEN_CLASS_VECTOR_COUNT; ++index) {
    if (exact_token_string_vector(
        classes,
        token_class_vectors[index].names,
        token_class_vectors[index].size,
        work_since_interrupt
      )) {
      *kind = token_class_vectors[index].kind;
      return TRUE;
    }
  }
  return FALSE;
}

typedef enum {
  /* An ordinary value: never selected as a TuneToken. */
  TOKEN_CLAIM_NONE = 0,
  /* Exactly one of the five admitted class vectors; `kind` is reported. */
  TOKEN_CLAIM_EXACT,
  /* Claims to be a TuneToken without being one: selected, then rejected by
   * the erroring admission so its established diagnostic is unchanged. */
  TOKEN_CLAIM_MALFORMED
} token_claim_t;

/*
 * Allocation-free and callback-free classification. Exact admission requires
 * one of the five plain, attribute-free character class vectors above. Other
 * ordinary character carriers are inspected only for a literal "TuneToken"
 * claim, which is selected and rejected as malformed; without that claim they
 * remain ordinary values. A non-character or ALTREP class carrier cannot be
 * proved to be an ordinary non-token under this closed contract, so it fails
 * closed as malformed without observing any ALTREP element.
 */
static token_claim_t exact_token_claim(SEXP value,
    token_snapshot_kind_t *kind) {
  *kind = TOKEN_SNAPSHOT_FULL;
  if (!Rf_isObject(value)) return TOKEN_CLAIM_NONE;
  SEXP classes = R_NilValue;
  if (!paradox_api_ordinary_class_snapshot(value, &classes)) {
    return TOKEN_CLAIM_MALFORMED;
  }
  if (classes == R_NilValue) return TOKEN_CLAIM_NONE;
  /* These carriers cannot describe an admitted token, but neither can they be
   * safely classified as ordinary non-tokens. Select them as malformed claims
   * so the erroring admission rejects them; never dispatch through ALTREP
   * element access merely to refine that diagnostic. */
  if (TYPEOF(classes) != STRSXP || ALTREP(classes)) {
    return TOKEN_CLAIM_MALFORMED;
  }
  R_xlen_t work_since_interrupt = 0;
  if (token_kind_from_classes(classes, kind, &work_since_interrupt)) {
    return TOKEN_CLAIM_EXACT;
  }
  for (R_xlen_t index = 0; index < XLENGTH(classes); ++index) {
    SEXP entry = STRING_ELT(classes, index);
    if (entry != NA_STRING && strcmp(CHAR(entry), "TuneToken") == 0) {
      return TOKEN_CLAIM_MALFORMED;
    }
  }
  return TOKEN_CLAIM_NONE;
}

/* An S4-marked ordinary TuneToken remains malformed interpreted structure,
 * while a formal S4 value is an opaque semantic leaf. Distinguish those cases
 * only on the S4 cold path. Ordinary values retain the allocation-free exact
 * token classifier unchanged. */
static token_claim_t exact_semantic_leaf_token_claim(
    SEXP value, token_snapshot_kind_t *kind) {
  *kind = TOKEN_SNAPSHOT_FULL;
  /* Preserve the former hot path exactly: the object bit is the complete
   * negative certificate for ordinary scalar values. Only classed values need
   * the S4 distinction below. */
  if (!Rf_isObject(value)) return TOKEN_CLAIM_NONE;
  if (Rf_isS4(value)) {
    SEXP classes = R_NilValue;
    if (!paradox_api_opaque_leaf_class_snapshot(value, &classes)) {
      return TOKEN_CLAIM_MALFORMED;
    }
    if (paradox_api_ordinary_class_contains(classes, "TuneToken")) {
      return exact_token_claim(value, kind);
    }
    return TOKEN_CLAIM_NONE;
  }
  return exact_token_claim(value, kind);
}

static int semantic_leaf_is_tune_token(SEXP value) {
  token_snapshot_kind_t ignored;
  return exact_semantic_leaf_token_claim(value, &ignored) != TOKEN_CLAIM_NONE;
}

/* Own the snapshot's shell metadata instead of sharing the caller's vectors.
 * `SHALLOW_DUPLICATE_ATTRIB` copies only the attribute pairlist spine, so a
 * shared class vector would remain the dispatch authority for every later
 * reader of the supposedly detached snapshot. */
static SEXP owned_token_class(token_snapshot_kind_t kind) {
  for (int index = 0; index < TOKEN_CLASS_VECTOR_COUNT; ++index) {
    if (token_class_vectors[index].kind == kind) {
      return paradox_domain_character_vector(
        token_class_vectors[index].names,
        token_class_vectors[index].size
      );
    }
  }
  Rf_error("Internal error: unknown TuneToken kind");
  return R_NilValue;
}

static SEXP owned_token_names(void) {
  return paradox_domain_character_vector(token_shell_names, 2);
}

static token_snapshot_kind_t exact_token_kind(SEXP token) {
  static const char *const attributes[] = {"names", "class"};
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
        token_shell_names,
        2,
        &work_since_interrupt
      )) {
    UNPROTECT(2);
    Rf_error(
      "Malformed TuneToken: expected the fixed {content, call} list"
    );
  }
  token_snapshot_kind_t kind;
  if (!token_kind_from_classes(classes, &kind, &work_since_interrupt)) {
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
  if (kind == TOKEN_SNAPSHOT_OBJECT) {
    if (TYPEOF(content) == ENVSXP && !Rf_isS4(content)) return;
    int domain = FALSE;
    if (TYPEOF(content) != VECSXP || ALTREP(content) ||
        Rf_isS4(content) ||
        !paradox_api_ordinary_class_matches(
          content,
          "Domain",
          &domain
        ) || !domain) {
      Rf_error(
        "Malformed ObjectTuneToken: content must be a Domain or ParamSet"
      );
    }
    return;
  }

  const R_xlen_t size = TYPEOF(content) == VECSXP && !ALTREP(content)
    ? XLENGTH(content)
    : 0;
  token_content_layout_t layout;
  if (!token_content_layout(kind, size, &layout) ||
      !token_content_names(content, layout.names, size)) {
    Rf_error("Malformed TuneToken content");
  }
  if (layout.range && (!token_number_or_null(VECTOR_ELT(content, 0)) ||
      !token_number_or_null(VECTOR_ELT(content, 1)))) {
    Rf_error("Malformed RangeTuneToken bounds");
  }
  const R_xlen_t logscale_index = layout.range ? 2 : 0;
  SEXP logscale = VECTOR_ELT(content, logscale_index);
  if (!plain_token_logical(logscale) ||
      (layout.internal && LOGICAL_ELT(logscale, 0) != FALSE) ||
      (layout.has_aggr && (Rf_isS4(VECTOR_ELT(content, size - 1)) ||
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

/* Boundaries a test fixture may select. `TOKEN_TEST_PHASE_NONE` is what every
 * production caller passes, so the accepted path pays one comparison. */
#define TOKEN_TEST_PHASE_NONE (-1)
#define TOKEN_TEST_PHASE_CONTENT 0
#define TOKEN_TEST_PHASE_TOKEN 1
#define TOKEN_TEST_PHASE_SELECTION 2

static void run_token_test_barrier(int active, SEXP source, SEXP column,
    SEXP replacement) {
  if (!active) return;
  if (column != R_NilValue) {
    PROTECT(paradox_test_gc_column_mutator(source, column, replacement));
    UNPROTECT(1);
  }
  /*
   * This is a test-only deterministic finalizer barrier. R_gc() and
   * R_RunPendingFinalizers() are public APIs; invoking both here guarantees
   * that a pending mutation occurs at the exact snapshot boundary under test.
   */
  R_gc();
  R_RunPendingFinalizers();
  if (column != R_NilValue) {
    const int selected = INTEGER_ELT(column, 0);
    if (VECTOR_ELT(source, selected) != replacement) {
      Rf_error("TuneToken GC-mutation test fixture did not run");
    }
  }
}

static SEXP snapshot_search_space_value_carrier(SEXP values);
static SEXP snapshot_tune_tokens_from_value_carrier_impl(
  SEXP stable_values, SEXP *value_names, int phase,
  SEXP mutation_column, SEXP mutation_replacement
);

/* The widest representation-scalar prefix any admitted token content has:
 * `{lower, upper, logscale}`. The optional `aggr` callback is opaque and is
 * never copied. */
#define TOKEN_CONTENT_MAX_SCALARS 3

static SEXP snapshot_tune_token_impl(SEXP token, int phase,
    SEXP mutation_column, SEXP mutation_replacement) {
  const token_snapshot_kind_t kind = exact_token_kind(token);
  SEXP content = PROTECT(VECTOR_ELT(token, 0));
  SEXP call = PROTECT(VECTOR_ELT(token, 1));
  validate_token_call(call);

  SEXP stable_content;
  if (kind == TOKEN_SNAPSHOT_OBJECT) {
    validate_token_content(content, kind);
    run_token_test_barrier(
      phase == TOKEN_TEST_PHASE_CONTENT,
      content,
      mutation_column,
      mutation_replacement
    );
    run_token_test_barrier(
      phase == TOKEN_TEST_PHASE_TOKEN,
      token,
      mutation_column,
      mutation_replacement
    );
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
    const R_xlen_t size = TYPEOF(content) == VECSXP && !ALTREP(content)
      ? XLENGTH(content)
      : 0;
    token_content_layout_t layout;
    if (!token_content_layout(kind, size, &layout)) {
      UNPROTECT(2);
      Rf_error("Malformed TuneToken content");
    }
    /*
     * Capture the content spine, admit the captured leaves, and allocate one
     * canonical destination per representation scalar before any payload is
     * read.  The copy below is then a single allocation-free pass, so no
     * instant exists at which one admitted field has been taken from an older
     * generation than another.  The token shapes are tiny and closed, so this
     * replaces per-leaf duplication rather than adding a recursive receipt.
     */
    stable_content = PROTECT(Rf_allocVector(VECSXP, size));
    SEXP stable_names = PROTECT(Rf_allocVector(STRSXP, size));
    Rf_setAttrib(stable_content, R_NamesSymbol, stable_names);

    /* Both carriers exist, so select every name beside its exact matching
     * element in one allocation-free pass. From here the source spine is
     * never read again. */
    R_xlen_t work_since_interrupt = 0;
    if (TYPEOF(content) != VECSXP || ALTREP(content) ||
        Rf_isS4(content) || Rf_isObject(content) ||
        XLENGTH(content) != size ||
        !paradox_api_has_single_attribute(content, "names")) {
      UNPROTECT(4);
      Rf_error("Malformed TuneToken content");
    }
    SEXP observed_names = paradox_api_raw_attribute(
      content,
      R_NamesSymbol
    );
    if (!exact_token_string_vector(
        observed_names,
        layout.names,
        size,
        &work_since_interrupt
      )) {
      UNPROTECT(4);
      Rf_error("Malformed TuneToken content");
    }
    for (R_xlen_t index = 0; index < size; ++index) {
      SET_STRING_ELT(
        stable_names,
        index,
        STRING_ELT(observed_names, index)
      );
      SET_VECTOR_ELT(stable_content, index, VECTOR_ELT(content, index));
    }

    validate_token_content(stable_content, kind);
    SEXPTYPE destination_types[TOKEN_CONTENT_MAX_SCALARS];
    for (R_xlen_t index = 0;
        index < layout.representation_scalar_count;
        ++index) {
      destination_types[index] =
        (SEXPTYPE) TYPEOF(VECTOR_ELT(stable_content, index));
    }
    SEXP destinations = PROTECT(Rf_allocVector(
      VECSXP,
      layout.representation_scalar_count
    ));
    for (R_xlen_t index = 0;
        index < layout.representation_scalar_count;
        ++index) {
      if (destination_types[index] == NILSXP) continue;
      SEXP carrier = PROTECT(Rf_allocVector(destination_types[index], 1));
      SET_VECTOR_ELT(destinations, index, carrier);
      UNPROTECT(1);
    }

    run_token_test_barrier(
      phase == TOKEN_TEST_PHASE_CONTENT,
      content,
      mutation_column,
      mutation_replacement
    );
    run_token_test_barrier(
      phase == TOKEN_TEST_PHASE_TOKEN,
      token,
      mutation_column,
      mutation_replacement
    );

    /*
     * One allocation-free pass over the retained cells: re-admit every leaf
     * and copy its payload into the destination allocated for it. Because
     * nothing between the first and the last copy can allocate, the admitted
     * fields always come from one generation.
     */
    for (R_xlen_t index = 0; index < size; ++index) {
      SEXP element = VECTOR_ELT(stable_content, index);
      if (index >= layout.representation_scalar_count) {
        /* The optional aggregation callback is deliberately opaque and must
         * retain its identity and attributes. */
        if (Rf_isS4(element) || !Rf_isFunction(element)) {
          UNPROTECT(5);
          Rf_error("Malformed TuneToken content");
        }
        SET_VECTOR_ELT(stable_content, index, element);
        continue;
      }
      const int logscale_slot = layout.range ? index == 2 : index == 0;
      if (logscale_slot
          ? (!plain_token_logical(element) ||
            (layout.internal && LOGICAL_ELT(element, 0) != FALSE))
          : !token_number_or_null(element)) {
        UNPROTECT(5);
        Rf_error("Malformed TuneToken content");
      }
      if ((SEXPTYPE) TYPEOF(element) != destination_types[index]) {
        UNPROTECT(5);
        Rf_error("TuneToken changed while its exact snapshot was constructed");
      }
      /* Ordinary R arithmetic and subsetting commonly attach a name to a
       * scalar.  Bounds and logscale admit that representation, but the owned
       * destination is canonical and therefore attribute-free by
       * construction. */
      SEXP destination = VECTOR_ELT(destinations, index);
      switch (destination_types[index]) {
      case NILSXP:
        break;
      case REALSXP:
        SET_REAL_ELT(destination, 0, REAL_ELT(element, 0));
        break;
      case INTSXP:
        SET_INTEGER_ELT(destination, 0, INTEGER_ELT(element, 0));
        break;
      case LGLSXP:
        SET_LOGICAL_ELT(destination, 0, LOGICAL_ELT(element, 0));
        break;
      default:
        UNPROTECT(5);
        Rf_error("Malformed TuneToken content");
      }
      SET_VECTOR_ELT(stable_content, index, destination);
    }
    UNPROTECT(2);
  }
  SEXP stable_call = PROTECT(Rf_allocVector(STRSXP, 1));
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP result_names = PROTECT(owned_token_names());
  SEXP result_class = PROTECT(owned_token_class(kind));
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  Rf_setAttrib(result, R_ClassSymbol, result_class);
  /*
   * Terminal, allocation-free.  The snapshot owns its shell metadata, so this
   * barrier compares the live token instead of re-reading a class vector the
   * caller would still share; a replacement of that attribute is now visible
   * where the shared-pointer form could not see it.
   */
  validate_token_call(call);
  SET_STRING_ELT(stable_call, 0, STRING_ELT(call, 0));
  SET_VECTOR_ELT(result, 0, stable_content);
  SET_VECTOR_ELT(result, 1, stable_call);
  token_snapshot_kind_t observed_kind = TOKEN_SNAPSHOT_FULL;
  if (exact_token_claim(token, &observed_kind) != TOKEN_CLAIM_EXACT ||
      observed_kind != kind) {
    UNPROTECT(7);
    Rf_error("TuneToken changed while its exact snapshot was constructed");
  }
  validate_token_content(VECTOR_ELT(result, 0), kind);
  UNPROTECT(7);
  return result;
}

static SEXP snapshot_tune_token(SEXP token) {
  return snapshot_tune_token_impl(
    token,
    TOKEN_TEST_PHASE_NONE,
    R_NilValue,
    R_NilValue
  );
}

static int test_token_phase(SEXP phase) {
  if (phase == R_NilValue) return TOKEN_TEST_PHASE_CONTENT;
  if (TYPEOF(phase) != STRSXP || XLENGTH(phase) != 1 ||
      STRING_ELT(phase, 0) == NA_STRING) {
    Rf_error("Invalid TuneToken GC-mutation test fixture phase");
  }
  const char *selected = CHAR(STRING_ELT(phase, 0));
  if (strcmp(selected, "content") == 0) return TOKEN_TEST_PHASE_CONTENT;
  if (strcmp(selected, "token") == 0) return TOKEN_TEST_PHASE_TOKEN;
  if (strcmp(selected, "selection") == 0) return TOKEN_TEST_PHASE_SELECTION;
  Rf_error("Invalid TuneToken GC-mutation test fixture phase");
  return TOKEN_TEST_PHASE_NONE;
}

SEXP paradox_test_tune_token_gc_mutation_snapshot(
    SEXP token, SEXP column, SEXP replacement, SEXP phase) {
  const int selected_phase = test_token_phase(phase);
  if (TYPEOF(token) != VECSXP || ALTREP(token) || XLENGTH(token) < 1) {
    Rf_error("Invalid TuneToken GC-mutation test fixture token");
  }
  token_snapshot_kind_t token_kind = TOKEN_SNAPSHOT_FULL;
  const token_claim_t claim = exact_token_claim(token, &token_kind);
  if (claim == TOKEN_CLAIM_NONE) {
    SEXP stable_values = PROTECT(snapshot_search_space_value_carrier(token));
    run_token_test_barrier(
      selected_phase == TOKEN_TEST_PHASE_CONTENT,
      token,
      column,
      replacement
    );
    SEXP value_names = R_NilValue;
    SEXP result = PROTECT(snapshot_tune_tokens_from_value_carrier_impl(
      stable_values,
      &value_names,
      selected_phase,
      selected_phase == TOKEN_TEST_PHASE_SELECTION ? column : R_NilValue,
      replacement
    ));
    UNPROTECT(2);
    return result;
  }
  if (claim != TOKEN_CLAIM_EXACT) {
    Rf_error("Invalid TuneToken GC-mutation test fixture token");
  }
  SEXP content = VECTOR_ELT(token, 0);
  if (TYPEOF(content) != VECSXP || ALTREP(content)) {
    Rf_error("Invalid TuneToken GC-mutation test fixture content");
  }
  return snapshot_tune_token_impl(
    token,
    selected_phase,
    column,
    replacement
  );
}

static SEXP snapshot_parameter_value(SEXP value) {
  if (semantic_leaf_is_tune_token(value)) {
    return snapshot_tune_token(value);
  }
  return snapshot_parameter_leaf(value);
}

static SEXP snapshot_value_for_spec(
    SEXP value, const value_spec_t *spec) {
  /*
   * ParamUty is the package's opaque escape hatch.  Its ordinary values are
   * identity-bearing leaves, not interpreted vectors: in particular, merely
   * checking or storing one must not call an ALTREP method or duplicate an
   * ordinary atomic object.  TuneTokens remain semantic syntax regardless of
   * the target Domain and therefore retain their one canonical snapshot.
   */
  if (semantic_leaf_is_tune_token(value)) {
    return snapshot_tune_token(value);
  }
  return spec->kind == VALUE_UTY
    ? value
    : snapshot_parameter_leaf(value);
}

static int representation_only_s3_classes(SEXP value) {
  SEXP classes = R_NilValue;
  if (!paradox_api_ordinary_class_snapshot(value, &classes)) {
    return FALSE;
  }
  PROTECT(classes);
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

static int named_list_shell_is_current(SEXP values, R_xlen_t size) {
  static const char *const allowed_attributes[] = {"names", "class"};
  return TYPEOF(values) == VECSXP && !ALTREP(values) &&
    !Rf_isS4(values) && XLENGTH(values) == size &&
    paradox_api_has_only_attributes(values, allowed_attributes, 2) &&
    representation_only_s3_classes(values);
}

static int ordinary_names_vector(SEXP value, R_xlen_t expected,
    int allow_absent) {
  if (value == R_NilValue) return allow_absent;
  return TYPEOF(value) == STRSXP && !ALTREP(value) &&
    !Rf_isS4(value) && !Rf_isObject(value) && no_attributes(value) &&
    XLENGTH(value) == expected;
}

static SEXP snapshot_named_list(SEXP values) {
  if (TYPEOF(values) != VECSXP) {
    return check_message(
      "Must be a list, not '%s'",
      Rf_type2char((SEXPTYPE) TYPEOF(values))
    );
  }
  if (ALTREP(values) || Rf_isS4(values)) {
    return check_message("Must be an ordinary named list");
  }
  const R_xlen_t size = XLENGTH(values);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  if (!named_list_shell_is_current(values, size)) {
    UNPROTECT(1);
    return check_message("Must be an ordinary named list");
  }
  SEXP names = PROTECT(paradox_api_raw_attribute(
    values,
    R_NamesSymbol
  ));
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
  SEXP stable_names = PROTECT(Rf_allocVector(STRSXP, size));
  /*
   * The allocations are complete.  Re-select names now, then capture each
   * name beside the exact matching leaf in one allocation-free pass.  A
   * pending finalizer may have rewritten the caller-owned list while either
   * carrier was allocated; it may not leave a pre-callback name paired with a
   * post-callback value.
   */
  if (!named_list_shell_is_current(values, size)) {
    UNPROTECT(3);
    return check_message("Must be an ordinary named list");
  }
  names = paradox_api_raw_attribute(values, R_NamesSymbol);
  if (TYPEOF(stable_names) != STRSXP || ALTREP(stable_names) ||
      Rf_isS4(stable_names) || XLENGTH(stable_names) != size ||
      !no_attributes(stable_names) ||
      !ordinary_names_vector(names, size, size == 0)) {
    UNPROTECT(3);
    return check_message("Names must be an ordinary character vector");
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP name = STRING_ELT(names, index);
    if (name == NA_STRING || CHAR(name)[0] == '\0') {
      UNPROTECT(3);
      return check_message("Names must be non-missing and non-empty");
    }
    SET_STRING_ELT(stable_names, index, name);
    SET_VECTOR_ELT(result, index, VECTOR_ELT(values, index));
  }
  if (Rf_any_duplicated(stable_names, FALSE) != 0) {
    UNPROTECT(3);
    return check_message("Names must be unique");
  }
  Rf_setAttrib(result, R_NamesSymbol, stable_names);
  UNPROTECT(3);
  return result;
}

/*
 * Own one exact outer values generation before TuneToken selection begins.
 * The returned ordinary named list is the sole authority for both names and
 * leaves after this function returns.
 */
static SEXP snapshot_search_space_value_carrier(SEXP values) {
  static const char *const allowed_attributes[] = {"names", "class"};
  if (TYPEOF(values) != VECSXP || ALTREP(values) || Rf_isS4(values) ||
      !paradox_api_has_only_attributes(values, allowed_attributes, 2)) {
    Rf_error("Search-space values must be an ordinary named list");
  }
  if (!representation_only_s3_classes(values)) {
    Rf_error("Search-space value container has a malformed S3 class");
  }
  const R_xlen_t size = XLENGTH(values);
  SEXP stable_values = PROTECT(Rf_allocVector(VECSXP, size));
  SEXP stable_names = PROTECT(Rf_allocVector(STRSXP, size));

  /*
   * Either allocation above may run a pending finalizer. Reauthenticate the
   * source shell, then capture every name beside its matching leaf without
   * another allocation or callback. This prevents a name from one generation
   * being paired with a value selected from another.
   */
  if (TYPEOF(values) != VECSXP || ALTREP(values) || Rf_isS4(values) ||
      XLENGTH(values) != size ||
      !paradox_api_has_only_attributes(values, allowed_attributes, 2)) {
    UNPROTECT(2);
    Rf_error("Search-space values must be an ordinary named list");
  }
  if (!representation_only_s3_classes(values)) {
    UNPROTECT(2);
    Rf_error("Search-space value container has a malformed S3 class");
  }
  SEXP names = Rf_getAttrib(values, R_NamesSymbol);
  if (!ordinary_names_vector(names, size, size == 0)) {
    UNPROTECT(2);
    Rf_error("Search-space values must have ordinary character names");
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP name = STRING_ELT(names, index);
    if (name == NA_STRING || CHAR(name)[0] == '\0') {
      UNPROTECT(2);
      Rf_error("Search-space value names must be non-missing and non-empty");
    }
    SET_STRING_ELT(stable_names, index, name);
    SET_VECTOR_ELT(stable_values, index, VECTOR_ELT(values, index));
  }
  if (Rf_any_duplicated(stable_names, FALSE) != 0) {
    UNPROTECT(2);
    Rf_error("Search-space value names must be unique");
  }
  Rf_setAttrib(stable_values, R_NamesSymbol, stable_names);
  UNPROTECT(2);
  return stable_values;
}

/* One byte per value: 0 is an ordinary value, 1 a malformed token claim, and
 * `2 + kind` an exact token of that kind. Retaining the kind makes the
 * terminal pass reject a same-object kind change as well as a token/non-token
 * change, at no extra cost. */
static unsigned char token_selection_code(token_claim_t claim,
    token_snapshot_kind_t kind) {
  if (claim == TOKEN_CLAIM_NONE) return 0U;
  if (claim == TOKEN_CLAIM_MALFORMED) return 1U;
  return (unsigned char) (2U + (unsigned int) kind);
}

/* Publishes the complete admitted name vector through `value_names` beside the
 * TuneToken subset it returns. Both cross the return boundary unrooted, so the
 * caller must root them before anything that can allocate. */
static SEXP snapshot_tune_tokens_from_value_carrier_impl(
    SEXP stable_values, SEXP *value_names, int phase,
    SEXP mutation_column, SEXP mutation_replacement) {
  const R_xlen_t size = XLENGTH(stable_values);
  SEXP stable_names = PROTECT(Rf_getAttrib(
    stable_values,
    R_NamesSymbol
  ));
  R_xlen_t token_count = 0;
  unsigned char *selected = paradox_temporary_alloc(
    size == 0 ? 1 : size,
    sizeof(*selected)
  );
  for (R_xlen_t index = 0; index < size; ++index) {
    token_snapshot_kind_t kind = TOKEN_SNAPSHOT_FULL;
    const token_claim_t claim = exact_semantic_leaf_token_claim(
      VECTOR_ELT(stable_values, index),
      &kind
    );
    selected[index] = token_selection_code(claim, kind);
    token_count += selected[index] != 0;
  }
  run_token_test_barrier(
    phase == TOKEN_TEST_PHASE_SELECTION,
    stable_values,
    mutation_column,
    mutation_replacement
  );
  SEXP result = PROTECT(Rf_allocVector(VECSXP, token_count));
  SEXP result_names = PROTECT(Rf_allocVector(STRSXP, token_count));
  R_xlen_t output = 0;
  for (R_xlen_t index = 0; index < size; ++index) {
    if (selected[index] == 0) continue;
    SEXP token = PROTECT(snapshot_tune_token(
      VECTOR_ELT(stable_values, index)
    ));
    SET_VECTOR_ELT(result, output, token);
    SET_STRING_ELT(result_names, output, STRING_ELT(stable_names, index));
    ++output;
    UNPROTECT(1);
  }
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  /*
   * Terminal, allocation-free: reclassify every retained value. Selection
   * precedes both result carriers and every per-token snapshot, all of which
   * allocate; an in-place class rewrite of a value that was not selected
   * would otherwise omit that parameter from the search space with no
   * diagnostic at all.
   */
  for (R_xlen_t index = 0; index < size; ++index) {
    token_snapshot_kind_t kind = TOKEN_SNAPSHOT_FULL;
    const token_claim_t claim = exact_semantic_leaf_token_claim(
      VECTOR_ELT(stable_values, index),
      &kind
    );
    if (token_selection_code(claim, kind) != selected[index]) {
      UNPROTECT(3);
      Rf_error(
        "Search-space values changed while their TuneToken snapshot was "
        "constructed"
      );
    }
  }
  *value_names = stable_names;
  UNPROTECT(3);
  return result;
}

static SEXP snapshot_tune_tokens_from_value_carrier(
    SEXP stable_values, SEXP *value_names) {
  return snapshot_tune_tokens_from_value_carrier_impl(
    stable_values,
    value_names,
    TOKEN_TEST_PHASE_NONE,
    R_NilValue,
    R_NilValue
  );
}

static SEXP snapshot_tune_tokens_from_values(SEXP values, SEXP *value_names) {
  SEXP stable_values = PROTECT(snapshot_search_space_value_carrier(values));
  SEXP result = PROTECT(snapshot_tune_tokens_from_value_carrier(
    stable_values,
    value_names
  ));
  UNPROTECT(2);
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

static SEXP finish_tune_token_snapshot(
    SEXP tokens, SEXP value_names, SEXP all_domains) {
  SEXP token_names = PROTECT(Rf_getAttrib(tokens, R_NamesSymbol));
  R_xlen_t work_since_interrupt = 0;
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
        UNPROTECT(2);
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
  UNPROTECT(4);
  return result;
}

SEXP paradox_tune_token_snapshot_list(SEXP private_environment, SEXP self,
    SEXP values) {
  SEXP value_names = R_NilValue;
  SEXP tokens = PROTECT(snapshot_tune_tokens_from_values(values, &value_names));
  PROTECT(value_names);
  SEXP all_domains = PROTECT(paradox_param_set_domains(
    private_environment,
    self
  ));
  SEXP result = PROTECT(finish_tune_token_snapshot(
    tokens,
    value_names,
    all_domains
  ));
  UNPROTECT(4);
  return result;
}

SEXP paradox_tune_token_snapshot_current(
    SEXP private_environment, SEXP self) {
  SEXP source = PROTECT(paradox_param_set_domains_and_values(
    private_environment,
    self
  ));
  SEXP values = VECTOR_ELT(source, 0);
  SEXP all_domains = VECTOR_ELT(source, 1);
  SEXP value_names = R_NilValue;
  SEXP tokens = PROTECT(snapshot_tune_tokens_from_values(values, &value_names));
  PROTECT(value_names);
  SEXP result = PROTECT(finish_tune_token_snapshot(
    tokens,
    value_names,
    all_domains
  ));
  UNPROTECT(4);
  return result;
}

static SEXP initialize_point(SEXP stable_values, const check_plan_t *plan,
    point_t *point, int values_are_stable) {
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
  if (!values_are_stable) {
    /*
     * Name admission above is callback-free and complete before a semantic
     * leaf is observed.  The owned point shell can now be specialized by the
     * exact selected Domain without a schema-blind R/C fallback.
     */
    for (R_xlen_t index = 0; index < point->size; ++index) {
      SEXP value = PROTECT(VECTOR_ELT(point->values, index));
      SEXP stable = PROTECT(snapshot_value_for_spec(
        value,
        &plan->specs[point->param_rows[index]]
      ));
      SET_VECTOR_ELT(point->values, index, stable);
      UNPROTECT(2);
    }
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
    SEXP reason = TYPEOF(answer) == STRSXP && XLENGTH(answer) == 1
      ? STRING_ELT(answer, 0)
      : NA_STRING;
    if (reason != NA_STRING) {
      result = PROTECT(utf8_message_2(
        "", spec->id, ": ", reason, ""
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
  if (TYPEOF(content) == VECSXP) {
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
  if (TYPEOF(token) != VECSXP || ALTREP(token) || Rf_isS4(token)) {
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
  token_snapshot_kind_t kind = TOKEN_SNAPSHOT_FULL;
  if (exact_token_claim(token, &kind) != TOKEN_CLAIM_EXACT) {
    return utf8_message_1(
      "", spec->id, ": tune token invalid: malformed token"
    );
  }

  if (kind == TOKEN_SNAPSHOT_RANGE ||
      kind == TOKEN_SNAPSHOT_INTERNAL_RANGE) {
    if (spec->kind != VALUE_DBL && spec->kind != VALUE_INT) {
      return utf8_message_1(
        "", call,
        " for non-numeric param must have zero or one argument."
      );
    }
    if (TYPEOF(content) != VECSXP || ALTREP(content) || Rf_isS4(content)) {
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

  if (kind == TOKEN_SNAPSHOT_FULL ||
      kind == TOKEN_SNAPSHOT_INTERNAL_FULL) {
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

  if (kind == TOKEN_SNAPSHOT_OBJECT) {
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
    if (TYPEOF(answer) != LGLSXP || XLENGTH(answer) != 1) {
      UNPROTECT(3);
      Rf_error("ParamSet constraint must return one non-missing logical value");
    }
    const int accepted = LOGICAL_ELT(answer, 0);
    if (accepted == NA_LOGICAL) {
      UNPROTECT(3);
      Rf_error("ParamSet constraint must return one non-missing logical value");
    }
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
        semantic_leaf_is_tune_token(
          VECTOR_ELT(point->values, child_value)
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
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t index = 0; index < point->size; ++index) {
    if (!allow_token && semantic_leaf_is_tune_token(
        VECTOR_ELT(point->values, index)
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
  for (R_xlen_t index = 0; index < point->size; ++index) {
    paradox_account_work(&work_since_interrupt);
    SEXP value = VECTOR_ELT(point->values, index);
    token_snapshot_kind_t token_kind = TOKEN_SNAPSHOT_FULL;
    const token_claim_t token_claim = exact_semantic_leaf_token_claim(
      value,
      &token_kind
    );
    if (token_claim == TOKEN_CLAIM_NONE) continue;
    if (token_claim != TOKEN_CLAIM_EXACT) {
      UNPROTECT(protected_count);
      return utf8_message_1(
        "", plan->specs[point->param_rows[index]].id,
        ": tune token invalid: malformed token"
      );
    }
    const value_spec_t *spec = &plan->specs[point->param_rows[index]];
    const int internal =
      token_kind == TOKEN_SNAPSHOT_INTERNAL_FULL ||
      token_kind == TOKEN_SNAPSHOT_INTERNAL_RANGE;
    if (internal && !has_tag(
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
    if (!semantic_leaf_is_tune_token(value)) {
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
    SEXP *receipts_result, int values_are_stable) {
  point_t point;
  SEXP structural_failure = PROTECT(initialize_point(
    stable_values, plan, &point, values_are_stable
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
  /* The list-column shell is interpreted structure even though its cells are
   * opaque semantic leaves. Preserve S4 leaf identity, but do not normalize
   * an S4 carrier into an apparently ordinary private snapshot. */
  if (TYPEOF(column) == VECSXP && Rf_isS4(column)) {
    return R_UnboundValue;
  }
  if (TYPEOF(column) == VECSXP && ALTREP(column)) {
    return R_MissingArg;
  }
  if (TYPEOF(column) != VECSXP) return snapshot_parameter_value(column);
  return snapshot_ordinary_list_leaf(column);
}

static void stabilize_table_list_columns(
    SEXP table, const check_plan_t *plan) {
  SEXP names = Rf_getAttrib(table, R_NamesSymbol);
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t column = 0; column < XLENGTH(table); ++column) {
    SEXP values = VECTOR_ELT(table, column);
    if (TYPEOF(values) != VECSXP) continue;

    R_xlen_t parameter = R_XLEN_T_MAX;
    if (!find_id(
          &plan->root_ids,
          STRING_ELT(names, column),
          &parameter,
          &work_since_interrupt
        )) {
      /* initialize_point() owns the ordinary unavailable-ID diagnostic. */
      continue;
    }
    const value_spec_t *spec = &plan->specs[parameter];
    for (R_xlen_t row = 0; row < XLENGTH(values); ++row) {
      paradox_account_work(&work_since_interrupt);
      SEXP value = PROTECT(VECTOR_ELT(values, row));
      SEXP stable = PROTECT(snapshot_value_for_spec(value, spec));
      SET_VECTOR_ELT(values, row, stable);
      UNPROTECT(2);
    }
  }
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
  if (TYPEOF(table) != VECSXP || ALTREP(table) || Rf_isS4(table)) {
    return check_message("Must be a data.frame or data.table");
  }
  const R_xlen_t columns = XLENGTH(table);
  SEXP stable_names = PROTECT(Rf_allocVector(STRSXP, columns));
  SEXP result = PROTECT(Rf_allocVector(VECSXP, columns));
  if (!ordinary_table_class(table, "data.frame")) {
    UNPROTECT(2);
    return check_message("Must be a data.frame or data.table");
  }
  if (!paradox_capture_list_identities(
      table,
      stable_names,
      result
    )) {
    UNPROTECT(2);
    return check_message("Table columns must be named");
  }
  /*
   * Capture row-name authority in the same post-allocation observation
   * window as names and columns. Name diagnostics and duplicate detection may
   * allocate; delaying this read could pair an earlier column generation with
   * later row names after a pending finalizer.
   */
  R_xlen_t rows = 0;
  if (!paradox_public_table_row_count(table, &rows)) {
    UNPROTECT(2);
    return check_message("Invalid data.frame row names");
  }
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

  for (R_xlen_t column = 0; column < columns; ++column) {
    SEXP source = PROTECT(VECTOR_ELT(result, column));
    SEXP stable = PROTECT(snapshot_table_column(source));
    if (stable == R_UnboundValue) {
      UNPROTECT(4);
      return check_message("Table list-column shells must not be S4");
    }
    if (stable == R_MissingArg) {
      UNPROTECT(4);
      return check_message("Table list-column shells must use ordinary storage");
    }
    const SEXPTYPE type = (SEXPTYPE) TYPEOF(stable);
    if (type != LGLSXP && type != INTSXP && type != REALSXP &&
        type != CPLXSXP && type != STRSXP && type != RAWSXP &&
        type != VECSXP) {
      UNPROTECT(4);
      return check_message("Unsupported table column type '%s'",
        Rf_type2char(type));
    }
    if (XLENGTH(stable) != rows) {
      UNPROTECT(4);
      return check_message(column == 0
        ? "Invalid data.frame row names"
        : "Table columns must have equal lengths");
    }
    SET_VECTOR_ELT(result, column, stable);
    UNPROTECT(2);
  }
  Rf_setAttrib(result, R_NamesSymbol, stable_names);
  *row_count = rows;
  UNPROTECT(2);
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

SEXP paradox_param_set_validate_current_roots(
    SEXP selves, SEXP public_receipts) {
  /*
   * This is the migration session's final all-roots barrier. Every selected
   * shell generation remains in one shared protected root plan until all
   * graphs have been admitted; only then do allocation-free graph receipt
   * scans, followed immediately by the optional complete public-shell receipt
   * scan, certify the complete set simultaneously.
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
  if (public_receipts != R_NilValue) {
    paradox_validate_upgrade_public_binding_receipts(public_receipts);
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
    receipts_result,
    FALSE
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
  if (TYPEOF(values) != VECSXP || ALTREP(values) || Rf_isS4(values) ||
      Rf_isObject(values) ||
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
  SEXP result = PROTECT(initialize_point(
    stable_values, &plan, &point, FALSE
  ));
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
    const char *argument, int values_are_stable) {
  SEXP structural_failure = PROTECT(initialize_point(
    values, plan, point, values_are_stable
  ));
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
    stable_values, &plan, &point, validate, "x", FALSE
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
  stabilize_table_list_columns(stable_table, &plan);

  if (validate) {
    const void *row_watermark = vmaxget();
    for (R_xlen_t row = 0; row < rows; ++row) {
      if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
        R_CheckUserInterrupt();
      }
      SEXP values = PROTECT(table_point(stable_table, row));
      point_t point;
      initialize_constraint_point(
        values, &plan, &point, TRUE, "x", TRUE
      );
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
    initialize_constraint_point(
      values, &plan, &point, FALSE, "x", TRUE
    );
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
  stabilize_table_list_columns(stable_table, &plan);
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
      NULL,
      TRUE
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
