#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Utils.h>

#include "core_state.h"
#include "paramset_domain_common.h"
#include "r_utils.h"

typedef struct {
  R_xlen_t row_plus_one;
} match_slot_t;

enum {
  MATCH_LINEAR_THRESHOLD = 8
};

enum ids_root_slot {
  IDS_ROOT_CLASS_FILTER = 0,
  IDS_ROOT_ALL_TAGS,
  IDS_ROOT_ANY_TAGS,
  IDS_ROOT_PARAM_IDS,
  IDS_ROOT_PARAM_CLASSES,
  IDS_ROOT_TAG_IDS,
  IDS_ROOT_TAG_VALUES,
  IDS_ROOT_MATCH_TABLE,
  IDS_ROOT_MATCH_VALUE,
  IDS_ROOT_COUNT
};

static inline void periodic_interrupt(R_xlen_t iteration) {
  if (iteration != 0 &&
      iteration % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
    R_CheckUserInterrupt();
  }
}

static void require_callback_free_match_operand(SEXP value) {
  if (TYPEOF(value) != STRSXP || ALTREP(value) || Rf_isObject(value)) {
    Rf_error(
      "Corrupt ParamSet storage: matching columns must use callback-free "
      "character representations"
    );
  }
}

static uint64_t hash_bytes(const unsigned char *text, uint64_t hash) {
  while (*text != '\0') {
    hash ^= (uint64_t) *text;
    hash *= UINT64_C(1099511628211);
    ++text;
  }
  return hash;
}

static uint64_t raw_string_hash(SEXP string) {
  if (string == NA_STRING) {
    return UINT64_C(0x9e3779b97f4a7c15);
  }

  uint64_t hash = UINT64_C(14695981039346656037);
  hash ^= Rf_getCharCE(string) == CE_BYTES
    ? UINT64_C(0xff)
    : UINT64_C(0x01);
  hash *= UINT64_C(1099511628211);
  return hash_bytes((const unsigned char *) CHAR(string), hash);
}

static uint64_t canonical_string_hash(SEXP string) {
  if (string == NA_STRING || Rf_getCharCE(string) == CE_BYTES) {
    return raw_string_hash(string);
  }

  uint64_t hash = UINT64_C(14695981039346656037);
  hash ^= UINT64_C(0x01);
  hash *= UINT64_C(1099511628211);
  const void *vmax = vmaxget();
  const char *text = Rf_translateCharUTF8(string);
  hash = hash_bytes((const unsigned char *) text, hash);
  vmaxset(vmax);
  return hash;
}

static unsigned int string_encoding_bit(SEXP string) {
  if (string == NA_STRING || Rf_getCharCE(string) == CE_BYTES) {
    return 0;
  }

  /* R's character cache discards declared encodings for ASCII strings, so a
   * public-API-created ASCII CHARSXP reports CE_NATIVE here. */
  switch (Rf_getCharCE(string)) {
  case CE_NATIVE:
    return 1U;
  case CE_UTF8:
    return 2U;
  case CE_LATIN1:
    return 4U;
  default:
    return 8U;
  }
}

static int needs_canonical_matching(SEXP table, SEXP value) {
  unsigned int encodings = 0;
  const R_xlen_t table_size = XLENGTH(table);
  const R_xlen_t value_size = XLENGTH(value);
  for (R_xlen_t row = 0; row < table_size; ++row) {
    periodic_interrupt(row);
    encodings |= string_encoding_bit(STRING_ELT(table, row));
  }
  for (R_xlen_t row = 0; row < value_size; ++row) {
    periodic_interrupt(row);
    encodings |= string_encoding_bit(STRING_ELT(value, row));
  }
  return encodings != 0 && (encodings & (encodings - 1U)) != 0;
}

static uint64_t pointer_hash(SEXP string) {
  uint64_t value = (uint64_t) (uintptr_t) string;
  value = (value >> 3) ^ (value >> 35);
  return value * UINT64_C(11400714819323198485);
}

static R_xlen_t match_capacity(R_xlen_t table_size) {
  if (table_size > R_XLEN_T_MAX / 2) {
    Rf_error("Unable to allocate temporary native workspace");
  }

  const R_xlen_t needed = table_size == 0 ? 1 : table_size * 2;
  R_xlen_t capacity = 1;
  while (capacity < needed) {
    if (capacity > R_XLEN_T_MAX / 2) {
      Rf_error("Unable to allocate temporary native workspace");
    }
    capacity *= 2;
  }
  return capacity;
}

static void snapshot_match_operands(SEXP table, SEXP value, SEXP roots,
    SEXP *stable_table, SEXP *stable_value) {
  const R_xlen_t table_size = XLENGTH(table);
  const R_xlen_t value_size = XLENGTH(value);

  SEXP table_copy = PROTECT(Rf_allocVector(STRSXP, table_size));
  SET_VECTOR_ELT(roots, IDS_ROOT_MATCH_TABLE, table_copy);
  UNPROTECT(1);
  SEXP value_copy = PROTECT(Rf_allocVector(STRSXP, value_size));
  SET_VECTOR_ELT(roots, IDS_ROOT_MATCH_VALUE, value_copy);
  UNPROTECT(1);

  /* Both allocations happen before either exposed operand is validated.
   * The adjacent guards therefore observe a finalizer mutation caused by
   * either allocation, and copying itself cannot allocate or dispatch. */
  require_callback_free_match_operand(table);
  require_callback_free_match_operand(value);
  if (XLENGTH(table) != table_size || XLENGTH(value) != value_size) {
    Rf_error("ParamSet matching column changed length during native ids()");
  }
  for (R_xlen_t row = 0; row < table_size; ++row) {
    periodic_interrupt(row);
    SET_STRING_ELT(table_copy, row, STRING_ELT(table, row));
  }
  for (R_xlen_t row = 0; row < value_size; ++row) {
    periodic_interrupt(row);
    SET_STRING_ELT(value_copy, row, STRING_ELT(value, row));
  }

  *stable_table = table_copy;
  *stable_value = value_copy;
}

static R_xlen_t *native_character_match(SEXP table, SEXP value, SEXP roots) {
  const R_xlen_t table_size = XLENGTH(table);
  const R_xlen_t value_size = XLENGTH(value);
  R_xlen_t *matches = paradox_temporary_alloc(value_size, sizeof(*matches));
  R_xlen_t capacity = 0;
  match_slot_t *slots = NULL;
  if (table_size > MATCH_LINEAR_THRESHOLD) {
    capacity = match_capacity(table_size);
    slots = paradox_temporary_alloc(capacity, sizeof(*slots));
  }

  /* R_alloc() may collect and run finalizers. Authenticate the two exposed
   * operands only after all common-path workspace allocations have finished. */
  require_callback_free_match_operand(table);
  require_callback_free_match_operand(value);
  if (XLENGTH(table) != table_size || XLENGTH(value) != value_size) {
    Rf_error("ParamSet matching column changed length during native ids()");
  }

  int canonical = needs_canonical_matching(table, value);
  if (canonical) {
    snapshot_match_operands(
      table,
      value,
      roots,
      &table,
      &value
    );
  }

  /* These are the actual matcher operands. In the canonical case they are
   * invocation-private snapshots; in the common case no allocation or R
   * callback remains between these guards and the complete native match.
   * Use element access throughout: retaining STRING_PTR_RO() across an
   * interrupt poll is outside the native pointer-lifetime contract, and the
   * canonical branch also translates strings and allocates transient copies. */
  require_callback_free_match_operand(table);
  require_callback_free_match_operand(value);

  if (table_size <= MATCH_LINEAR_THRESHOLD) {
    for (R_xlen_t row = 0; row < value_size; ++row) {
      periodic_interrupt(row);
      R_xlen_t matched = 0;
      for (R_xlen_t candidate = 0; candidate < table_size; ++candidate) {
        if (canonical
            ? paradox_domain_strings_equal(
                STRING_ELT(table, candidate),
                STRING_ELT(value, row)
              )
            : STRING_ELT(table, candidate) == STRING_ELT(value, row)) {
          matched = candidate + 1;
          break;
        }
      }
      matches[row] = matched;
    }
    return matches;
  }

  R_xlen_t work_since_interrupt = 0;
  const R_xlen_t mask = capacity - 1;
  if (canonical) {
    for (R_xlen_t slot = 0; slot < capacity; ++slot) {
      paradox_account_work(&work_since_interrupt);
      slots[slot].row_plus_one = 0;
    }
    for (R_xlen_t row = 0; row < table_size; ++row) {
      paradox_account_work(&work_since_interrupt);
      const uint64_t hash = canonical_string_hash(STRING_ELT(table, row));
      R_xlen_t slot = (R_xlen_t) (hash & (uint64_t) mask);
      while (slots[slot].row_plus_one != 0) {
        paradox_account_work(&work_since_interrupt);
        const R_xlen_t present = slots[slot].row_plus_one - 1;
        if (paradox_domain_strings_equal(
            STRING_ELT(table, present),
            STRING_ELT(table, row)
          )) {
          break;
        }
        slot = (slot + 1) & mask;
      }
      if (slots[slot].row_plus_one == 0) {
        slots[slot].row_plus_one = row + 1;
      }
    }
    for (R_xlen_t row = 0; row < value_size; ++row) {
      paradox_account_work(&work_since_interrupt);
      const uint64_t hash = canonical_string_hash(STRING_ELT(value, row));
      R_xlen_t slot = (R_xlen_t) (hash & (uint64_t) mask);
      while (slots[slot].row_plus_one != 0) {
        paradox_account_work(&work_since_interrupt);
        const R_xlen_t present = slots[slot].row_plus_one - 1;
        if (paradox_domain_strings_equal(
            STRING_ELT(table, present),
            STRING_ELT(value, row)
          )) {
          break;
        }
        slot = (slot + 1) & mask;
      }
      matches[row] = slots[slot].row_plus_one;
    }
    return matches;
  }

  /* Public-API-created strings are interned, with their declared encoding as
   * part of the cache key. Mixed non-byte encodings took the canonical branch
   * above, so CHARSXP identity is exact for these ordinary operands. */
  for (R_xlen_t slot = 0; slot < capacity; ++slot) {
    paradox_account_work(&work_since_interrupt);
    slots[slot].row_plus_one = 0;
  }
  for (R_xlen_t row = 0; row < table_size; ++row) {
    paradox_account_work(&work_since_interrupt);
    R_xlen_t slot = (R_xlen_t) (
      pointer_hash(STRING_ELT(table, row)) & (uint64_t) mask
    );
    while (slots[slot].row_plus_one != 0) {
      paradox_account_work(&work_since_interrupt);
      const R_xlen_t present = slots[slot].row_plus_one - 1;
      if (STRING_ELT(table, present) == STRING_ELT(table, row)) {
        break;
      }
      slot = (slot + 1) & mask;
    }
    if (slots[slot].row_plus_one == 0) {
      slots[slot].row_plus_one = row + 1;
    }
  }

  for (R_xlen_t row = 0; row < value_size; ++row) {
    paradox_account_work(&work_since_interrupt);
    R_xlen_t slot = (R_xlen_t) (
      pointer_hash(STRING_ELT(value, row)) & (uint64_t) mask
    );
    while (slots[slot].row_plus_one != 0) {
      paradox_account_work(&work_since_interrupt);
      const R_xlen_t present = slots[slot].row_plus_one - 1;
      if (STRING_ELT(table, present) == STRING_ELT(value, row)) {
        break;
      }
      slot = (slot + 1) & mask;
    }
    matches[row] = slots[slot].row_plus_one;
  }

  return matches;
}

static R_xlen_t ordinary_character_column_size(SEXP column,
    const char *column_name) {
  if (TYPEOF(column) != STRSXP || ALTREP(column)) {
    /* Retain the historical observed-length diagnostic for ordinary corrupt
     * columns. Never ask a callback-capable column for its Length merely to
     * construct an error message. */
    const R_xlen_t diagnostic_size = ALTREP(column) ? 0 : XLENGTH(column);
    paradox_require_column(column, STRSXP, diagnostic_size, column_name);
    return 0;
  }
  return XLENGTH(column);
}

/* Matching may consult an ALTSTRING's Length and Elt methods more than once.
 * Materialize each public filter once so every native capacity and every
 * subsequent match is based on one ordinary, independently rooted vector. */
static SEXP snapshot_filter(SEXP value, const char *argument_name) {
  if (value == R_NilValue) {
    return R_NilValue;
  }

  paradox_require_character_argument_type(value, argument_name);
  const R_xlen_t size = XLENGTH(value);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP element = PROTECT(STRING_ELT(value, index));
    SET_STRING_ELT(result, index, element);
    UNPROTECT(1);
  }
  /* Validate the captured ordinary observation rather than rescanning the
   * callback-capable provider or passing a captured NA into Rf_match(). */
  paradox_require_character_argument(result, argument_name);
  UNPROTECT(1);
  return result;
}

static SEXP param_set_ids_impl(
  SEXP params,
  SEXP tag_table,
  SEXP class_filter,
  SEXP all_tags,
  SEXP any_tags,
  int filters_are_stable
);

static void check_tag_values(SEXP tag_ids, SEXP tag_values) {
  const R_xlen_t size = XLENGTH(tag_ids);
  for (R_xlen_t row = 0; row < size; ++row) {
    periodic_interrupt(row);
    if (STRING_ELT(tag_ids, row) == NA_STRING) {
      Rf_error("Corrupt ParamSet storage: `.tags$id` contains a missing value");
    }
    if (STRING_ELT(tag_values, row) == NA_STRING) {
      Rf_error("Corrupt ParamSet storage: `.tags$tag` contains a missing value");
    }
  }
}

static SEXP selected_ids(SEXP ids, const int *selected, R_xlen_t size) {
  R_xlen_t result_size = 0;
  for (R_xlen_t row = 0; row < size; ++row) {
    periodic_interrupt(row);
    result_size += selected[row] != 0;
  }

  SEXP result = PROTECT(Rf_allocVector(STRSXP, result_size));
  R_xlen_t result_row = 0;
  for (R_xlen_t row = 0; row < size; ++row) {
    periodic_interrupt(row);
    if (selected[row] != 0) {
      SET_STRING_ELT(result, result_row, STRING_ELT(ids, row));
      ++result_row;
    }
  }
  UNPROTECT(1);
  return result;
}

SEXP paradox_param_set_filter_argument(SEXP frame,
    const char *argument_name) {
  if (TYPEOF(frame) != ENVSXP) {
    Rf_error("Internal error: ParamSet filter frame must be an environment");
  }
  SEXP value = PROTECT(Rf_eval(Rf_install(argument_name), frame));
  SEXP result = PROTECT(snapshot_filter(value, argument_name));
  UNPROTECT(2);
  return result;
}

SEXP paradox_param_set_ids_lazy(SEXP private_environment, SEXP self,
    SEXP frame) {
  if (TYPEOF(private_environment) != ENVSXP || TYPEOF(frame) != ENVSXP) {
    Rf_error(
      "Internal error: ParamSet ID filtering requires private and method "
      "environments"
    );
  }

  /* Keep the promises in the public method frame until each earlier filter is
   * known to be valid.  Passing all three as .Call arguments would force a
   * later promise before native validation can report an invalid earlier one. */
  SEXP class_filter = PROTECT(paradox_param_set_filter_argument(
    frame,
    "class"
  ));
  SEXP all_tags = PROTECT(paradox_param_set_filter_argument(frame, "tags"));
  SEXP any_tags = PROTECT(paradox_param_set_filter_argument(
    frame,
    "any_tags"
  ));

  /* Filters are the public callback boundary. Snapshot the one authoritative
   * capsule only after all three promises have been forced, so a nested
   * mutation is observed without consulting retired private tables or
   * replaying the operation in R. */
  SEXP selected = paradox_core_from_private(private_environment);
  if (selected != R_UnboundValue && !paradox_core_is_verified(selected)) {
    selected = paradox_core_refresh(self, private_environment);
  }
  SEXP state = PROTECT(paradox_core_state_from_core(selected));
  if (state == R_UnboundValue) {
    UNPROTECT(4);
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }
  SEXP params = VECTOR_ELT(state, PARADOX_CORE_PARAMS);
  SEXP tag_table = VECTOR_ELT(state, PARADOX_CORE_TAGS);

  SEXP result = param_set_ids_impl(
    params,
    tag_table,
    class_filter,
    all_tags,
    any_tags,
    TRUE
  );
  UNPROTECT(4);
  return result;
}

static SEXP param_set_ids_impl(SEXP params, SEXP tag_table,
    SEXP class_filter, SEXP all_tags, SEXP any_tags,
    int filters_are_stable) {
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, IDS_ROOT_COUNT));
  SEXP stable_class_filter = PROTECT(filters_are_stable
    ? class_filter
    : snapshot_filter(class_filter, "class"));
  SET_VECTOR_ELT(roots, IDS_ROOT_CLASS_FILTER, stable_class_filter);
  UNPROTECT(1);
  SEXP stable_all_tags = PROTECT(filters_are_stable
    ? all_tags
    : snapshot_filter(all_tags, "tags"));
  SET_VECTOR_ELT(roots, IDS_ROOT_ALL_TAGS, stable_all_tags);
  UNPROTECT(1);
  SEXP stable_any_tags = PROTECT(filters_are_stable
    ? any_tags
    : snapshot_filter(any_tags, "any_tags"));
  SET_VECTOR_ELT(roots, IDS_ROOT_ANY_TAGS, stable_any_tags);
  UNPROTECT(1);
  class_filter = stable_class_filter;
  all_tags = stable_all_tags;
  any_tags = stable_any_tags;

  SEXP ids = paradox_get_named_column(params, ".params", "id");
  SET_VECTOR_ELT(roots, IDS_ROOT_PARAM_IDS, ids);
  const R_xlen_t size = ordinary_character_column_size(ids, "id");
  SEXP classes = paradox_get_named_column(params, ".params", "cls");
  SET_VECTOR_ELT(roots, IDS_ROOT_PARAM_CLASSES, classes);
  paradox_require_column(ids, STRSXP, size, "id");
  paradox_require_column(classes, STRSXP, size, "cls");

  const R_xlen_t all_tag_count = all_tags == R_NilValue
    ? 0
    : XLENGTH(all_tags);
  const R_xlen_t any_tag_count = any_tags == R_NilValue
    ? 0
    : XLENGTH(any_tags);

  if (class_filter == R_NilValue && all_tags == R_NilValue &&
      any_tags == R_NilValue) {
    UNPROTECT(1);
    return ids;
  }

  int *selected = paradox_temporary_alloc(size, sizeof(*selected));

  if (class_filter == R_NilValue) {
    for (R_xlen_t row = 0; row < size; ++row) {
      periodic_interrupt(row);
      selected[row] = TRUE;
    }
  } else {
    const R_xlen_t *class_matches = native_character_match(
      class_filter,
      classes,
      roots
    );
    for (R_xlen_t row = 0; row < size; ++row) {
      periodic_interrupt(row);
      selected[row] = class_matches[row] != 0;
    }
  }

  if (all_tags == R_NilValue && any_tags == R_NilValue) {
    SEXP result = selected_ids(ids, selected, size);
    UNPROTECT(1);
    return result;
  }

  /* Upstream returned NULL for this case. Keep its empty selection while
   * repairing the documented return type to character(0). */
  if (all_tags != R_NilValue && all_tag_count == 0 &&
      any_tags == R_NilValue) {
    for (R_xlen_t row = 0; row < size; ++row) {
      periodic_interrupt(row);
      selected[row] = FALSE;
    }
    SEXP result = selected_ids(ids, selected, size);
    UNPROTECT(1);
    return result;
  }

  SEXP tag_ids = paradox_get_named_column(tag_table, ".tags", "id");
  SET_VECTOR_ELT(roots, IDS_ROOT_TAG_IDS, tag_ids);
  const R_xlen_t n_tag_rows = ordinary_character_column_size(
    tag_ids,
    ".tags$id"
  );
  SEXP tag_values = paradox_get_named_column(tag_table, ".tags", "tag");
  SET_VECTOR_ELT(roots, IDS_ROOT_TAG_VALUES, tag_values);
  paradox_require_column(tag_ids, STRSXP, n_tag_rows, ".tags$id");
  paradox_require_column(tag_values, STRSXP, n_tag_rows, ".tags$tag");
  check_tag_values(tag_ids, tag_values);

  R_xlen_t *owner = native_character_match(ids, tag_ids, roots);
  for (R_xlen_t tag_row = 0; tag_row < n_tag_rows; ++tag_row) {
    periodic_interrupt(tag_row);
    const R_xlen_t matched_owner = owner[tag_row];
    if (matched_owner == 0 || matched_owner > size) {
      Rf_error(
        "Corrupt ParamSet storage: `.tags$id` contains an unknown parameter ID"
      );
    }
    owner[tag_row] = matched_owner - 1;
  }

  /* A single required tag with no other filter is by far the most frequent
   * tagged query. It needs neither grouping nor a per-requirement workspace. */
  if (class_filter == R_NilValue && all_tags != R_NilValue &&
      all_tag_count == 1 && any_tags == R_NilValue) {
    const R_xlen_t *tag_matches = native_character_match(
      all_tags,
      tag_values,
      roots
    );
    for (R_xlen_t row = 0; row < size; ++row) {
      periodic_interrupt(row);
      selected[row] = FALSE;
    }
    for (R_xlen_t tag_row = 0; tag_row < n_tag_rows; ++tag_row) {
      periodic_interrupt(tag_row);
      if (tag_matches[tag_row] != 0) {
        selected[owner[tag_row]] = TRUE;
      }
    }

    SEXP result = selected_ids(ids, selected, size);
    UNPROTECT(1);
    return result;
  }

  if (size == R_XLEN_T_MAX) {
    Rf_error("Unable to allocate temporary native workspace");
  }
  R_xlen_t *offset = paradox_temporary_alloc(size + 1, sizeof(*offset));
  for (R_xlen_t row = 0; row <= size; ++row) {
    periodic_interrupt(row);
    offset[row] = 0;
  }
  for (R_xlen_t tag_row = 0; tag_row < n_tag_rows; ++tag_row) {
    periodic_interrupt(tag_row);
    ++offset[owner[tag_row] + 1];
  }
  for (R_xlen_t row = 1; row <= size; ++row) {
    periodic_interrupt(row);
    offset[row] += offset[row - 1];
  }

  R_xlen_t *cursor = paradox_temporary_alloc(size, sizeof(*cursor));
  R_xlen_t *tag_order = paradox_temporary_alloc(
    n_tag_rows,
    sizeof(*tag_order)
  );
  for (R_xlen_t row = 0; row < size; ++row) {
    periodic_interrupt(row);
    cursor[row] = offset[row];
  }
  for (R_xlen_t tag_row = 0; tag_row < n_tag_rows; ++tag_row) {
    periodic_interrupt(tag_row);
    tag_order[cursor[owner[tag_row]]] = tag_row;
    ++cursor[owner[tag_row]];
  }

  const R_xlen_t *all_matches = NULL;
  R_xlen_t required_tag_count = 0;
  R_xlen_t *seen_tag = NULL;
  if (all_tags != R_NilValue && all_tag_count > 0) {
    all_matches = native_character_match(
      all_tags,
      tag_values,
      roots
    );
    const R_xlen_t *self_matches = native_character_match(
      all_tags,
      all_tags,
      roots
    );
    for (R_xlen_t tag = 0; tag < all_tag_count; ++tag) {
      periodic_interrupt(tag);
      const R_xlen_t matched = self_matches[tag];
      if (matched > all_tag_count) {
        Rf_error("Internal error: invalid result from R's matching primitive");
      }
      required_tag_count += matched == tag + 1;
    }
    seen_tag = paradox_temporary_alloc(all_tag_count, sizeof(*seen_tag));
    for (R_xlen_t tag = 0; tag < all_tag_count; ++tag) {
      periodic_interrupt(tag);
      seen_tag[tag] = 0;
    }
  }

  const R_xlen_t *any_matches = NULL;
  if (any_tags != R_NilValue && any_tag_count > 0) {
    any_matches = native_character_match(
      any_tags,
      tag_values,
      roots
    );
  }

  for (R_xlen_t row = 0; row < size; ++row) {
    periodic_interrupt(row);

    R_xlen_t matched_all = 0;
    int matched_any = FALSE;
    for (R_xlen_t position = offset[row]; position < offset[row + 1];
        ++position) {
      periodic_interrupt(position);
      const R_xlen_t tag_row = tag_order[position];
      if (all_matches != NULL) {
        const R_xlen_t required_tag = all_matches[tag_row];
        if (required_tag > all_tag_count) {
          Rf_error("Internal error: invalid result from R's matching primitive");
        }
        if (required_tag != 0 && seen_tag[required_tag - 1] != row + 1) {
          seen_tag[required_tag - 1] = row + 1;
          ++matched_all;
        }
      }
      if (any_matches != NULL && any_matches[tag_row] != 0) {
        matched_any = TRUE;
      }
    }

    const int satisfies_all = all_tags == R_NilValue ||
      all_tag_count == 0 || matched_all == required_tag_count;
    const int satisfies_any = any_tags == R_NilValue ||
      (any_tag_count > 0 && matched_any);
    selected[row] = selected[row] && satisfies_all && satisfies_any;
  }

  SEXP result = selected_ids(ids, selected, size);
  UNPROTECT(1);
  return result;
}

SEXP paradox_param_set_ids(SEXP params, SEXP tag_table, SEXP class_filter,
    SEXP all_tags, SEXP any_tags) {
  return param_set_ids_impl(
    params,
    tag_table,
    class_filter,
    all_tags,
    any_tags,
    FALSE
  );
}
