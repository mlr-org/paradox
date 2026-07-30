#include <limits.h>
#include <stdint.h>
#include <string.h>

#include "core_state.h"
#include "r_api_compat.h"
#include "r_utils.h"
#include "shell_auth.h"
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

SEXP paradox_upgrade_structural_list_exact(SEXP source) {
  return Rf_ScalarLogical(
    TYPEOF(source) == VECSXP && !ALTREP(source) && !Rf_isS4(source)
  );
}

SEXP paradox_upgrade_carrier_list_snapshot(SEXP source) {
  static const char *const allowed_attributes[] = {"names"};
  if (TYPEOF(source) != VECSXP || ALTREP(source) || Rf_isS4(source) ||
      Rf_isObject(source) ||
      !paradox_api_has_only_attributes(source, allowed_attributes, 1)) {
    return R_NilValue;
  }

  const R_xlen_t size = XLENGTH(source);
  const int has_names =
    paradox_api_raw_attribute(source, R_NamesSymbol) != R_NilValue;
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  int protect_count = 1;
  SEXP stable_names = R_NilValue;
  if (has_names) {
    stable_names = PROTECT(Rf_allocVector(STRSXP, size));
    ++protect_count;
  }

  /*
   * Both destination carriers now exist.  Either allocation may have run a
   * pending finalizer that rewrote the caller-owned list or its names, so
   * re-admit the shell and then capture each name beside its exact element in
   * one allocation-free pass.  The former names-then-elements sequence could
   * create a legacy callback carrier generation that never existed.
   */
  SEXP source_names = paradox_api_raw_attribute(source, R_NamesSymbol);
  if (TYPEOF(source) != VECSXP || ALTREP(source) || Rf_isS4(source) ||
      Rf_isObject(source) || XLENGTH(source) != size ||
      (source_names != R_NilValue) != has_names ||
      !paradox_api_has_only_attributes(source, allowed_attributes, 1) ||
      !paradox_capture_list_identities(
        source,
        stable_names,
        result
      )) {
    UNPROTECT(protect_count);
    return R_NilValue;
  }

  if (stable_names != R_NilValue) {
    for (R_xlen_t index = 0; index < size; ++index) {
      SEXP name = STRING_ELT(stable_names, index);
      if (name == NA_STRING || Rf_getCharCE(name) == CE_BYTES) {
        UNPROTECT(protect_count);
        return R_NilValue;
      }
    }
    Rf_setAttrib(result, R_NamesSymbol, stable_names);
  }

  UNPROTECT(protect_count);
  return result;
}

typedef struct {
  SEXP internal_selfref_symbol;
  SEXP sorted_symbol;
  SEXP index_symbol;
  SEXP repr_symbol;
  unsigned int seen;
  int allow_repr;
  int valid;
  SEXP row_names;
  SEXP repr;
} paradox_upgrade_table_attributes_t;

enum {
  UPGRADE_TABLE_ATTRIBUTE_NAMES = 1U << 0,
  UPGRADE_TABLE_ATTRIBUTE_ROWS = 1U << 1,
  UPGRADE_TABLE_ATTRIBUTE_CLASS = 1U << 2,
  UPGRADE_TABLE_ATTRIBUTE_SELFREF = 1U << 3,
  UPGRADE_TABLE_ATTRIBUTE_SORTED = 1U << 4,
  UPGRADE_TABLE_ATTRIBUTE_INDEX = 1U << 5,
  UPGRADE_TABLE_ATTRIBUTE_REPR = 1U << 6
};

static void capture_upgrade_table_attribute(
    SEXP tag, SEXP value, void *data) {
  paradox_upgrade_table_attributes_t *state = data;
  unsigned int bit = 0U;
  if (tag == R_NamesSymbol) {
    bit = UPGRADE_TABLE_ATTRIBUTE_NAMES;
  } else if (tag == R_RowNamesSymbol) {
    bit = UPGRADE_TABLE_ATTRIBUTE_ROWS;
    state->row_names = value;
  } else if (tag == R_ClassSymbol) {
    bit = UPGRADE_TABLE_ATTRIBUTE_CLASS;
  } else if (tag == state->internal_selfref_symbol) {
    bit = UPGRADE_TABLE_ATTRIBUTE_SELFREF;
  } else if (tag == state->sorted_symbol) {
    bit = UPGRADE_TABLE_ATTRIBUTE_SORTED;
  } else if (tag == state->index_symbol) {
    bit = UPGRADE_TABLE_ATTRIBUTE_INDEX;
  } else if (state->allow_repr && tag == state->repr_symbol) {
    bit = UPGRADE_TABLE_ATTRIBUTE_REPR;
    state->repr = value;
  } else {
    state->valid = FALSE;
    return;
  }
  if ((state->seen & bit) != 0U) {
    state->valid = FALSE;
    return;
  }
  state->seen |= bit;
}

static int exact_upgrade_table_classes(SEXP observed, SEXP expected) {
  if (TYPEOF(observed) != STRSXP || ALTREP(observed) ||
      Rf_isS4(observed) || Rf_isObject(observed) ||
      !paradox_api_has_no_attributes(observed) ||
      TYPEOF(expected) != STRSXP || ALTREP(expected) ||
      Rf_isS4(expected) || Rf_isObject(expected) ||
      !paradox_api_has_no_attributes(expected) ||
      XLENGTH(observed) != XLENGTH(expected)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(expected); ++index) {
    SEXP left = STRING_ELT(observed, index);
    SEXP right = STRING_ELT(expected, index);
    if (left == NA_STRING || right == NA_STRING ||
        Rf_getCharCE(left) == CE_BYTES || Rf_getCharCE(right) == CE_BYTES ||
        strcmp(CHAR(left), CHAR(right)) != 0) {
      return FALSE;
    }
  }
  return TRUE;
}

static int upgrade_table_snapshot_is_current(SEXP source, SEXP snapshot) {
  if (TYPEOF(source) != VECSXP || ALTREP(source) || Rf_isS4(source) ||
      TYPEOF(snapshot) != VECSXP || ALTREP(snapshot) ||
      Rf_isS4(snapshot) || Rf_isObject(snapshot) ||
      XLENGTH(source) != XLENGTH(snapshot)) {
    return FALSE;
  }
  SEXP source_names = paradox_api_raw_attribute(source, R_NamesSymbol);
  SEXP snapshot_names = paradox_api_raw_attribute(snapshot, R_NamesSymbol);
  if ((source_names == R_NilValue) != (snapshot_names == R_NilValue)) {
    return FALSE;
  }
  if (source_names != R_NilValue) {
    if (TYPEOF(source_names) != STRSXP || ALTREP(source_names) ||
        Rf_isS4(source_names) || Rf_isObject(source_names) ||
        !paradox_api_has_no_attributes(source_names) ||
        TYPEOF(snapshot_names) != STRSXP || ALTREP(snapshot_names) ||
        Rf_isS4(snapshot_names) || Rf_isObject(snapshot_names) ||
        !paradox_api_has_no_attributes(snapshot_names) ||
        XLENGTH(source_names) != XLENGTH(source) ||
        XLENGTH(snapshot_names) != XLENGTH(snapshot)) {
      return FALSE;
    }
  }
  for (R_xlen_t index = 0; index < XLENGTH(source); ++index) {
    if (VECTOR_ELT(source, index) != VECTOR_ELT(snapshot, index) ||
        (source_names != R_NilValue &&
          STRING_ELT(source_names, index) !=
            STRING_ELT(snapshot_names, index))) {
      return FALSE;
    }
  }
  return TRUE;
}

static int exact_upgrade_table_row_names(
    SEXP row_names, R_xlen_t row_count) {
  if (row_count > INT_MAX || TYPEOF(row_names) != INTSXP ||
      ALTREP(row_names) || Rf_isS4(row_names) ||
      Rf_isObject(row_names) ||
      !paradox_api_has_no_attributes(row_names)) {
    return FALSE;
  }
  if (row_count == 0) return XLENGTH(row_names) == 0;

  const R_xlen_t encoded_size = XLENGTH(row_names);
  if (encoded_size == 2 &&
      INTEGER_ELT(row_names, 0) == NA_INTEGER) {
    const int encoded = INTEGER_ELT(row_names, 1);
    const int expected = (int) row_count;
    return encoded == expected || encoded == -expected;
  }
  if (encoded_size != row_count) return FALSE;
  for (R_xlen_t row = 0; row < row_count; ++row) {
    if (INTEGER_ELT(row_names, row) != (int) row + 1) return FALSE;
  }
  return TRUE;
}

static int exact_upgrade_table_rows(
    const paradox_upgrade_table_attributes_t *attributes,
    R_xlen_t row_count) {
  if ((attributes->seen & UPGRADE_TABLE_ATTRIBUTE_ROWS) == 0U) {
    /*
     * Paradox 1/data.table legitimately omitted row.names from some empty
     * keyed internal tables.  That historical spelling is unambiguous only
     * after the selected columns have proved that the table has zero rows.
     */
    return row_count == 0;
  }
  return exact_upgrade_table_row_names(attributes->row_names, row_count);
}

SEXP paradox_upgrade_table_list_snapshot(SEXP source,
    SEXP expected_classes, SEXP allow_repr) {
  if (TYPEOF(source) != VECSXP || ALTREP(source) || Rf_isS4(source) ||
      TYPEOF(allow_repr) != LGLSXP || ALTREP(allow_repr) ||
      Rf_isS4(allow_repr) || Rf_isObject(allow_repr) ||
      !paradox_api_has_no_attributes(allow_repr) ||
      XLENGTH(allow_repr) != 1 ||
      LOGICAL_ELT(allow_repr, 0) == NA_LOGICAL) {
    return R_NilValue;
  }

  /*
   * Intern every attribute tag and allocate the outward carrier before
   * selecting source state. A pending finalizer during any of that work is
   * therefore part of the generation snapshotted below.
   */
  paradox_upgrade_table_attributes_t attributes = {
    Rf_install(".internal.selfref"),
    Rf_install("sorted"),
    Rf_install("index"),
    Rf_install("repr"),
    0U,
    LOGICAL_ELT(allow_repr, 0),
    TRUE,
    R_NilValue,
    R_NilValue
  };
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP result_names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(result_names, 0, Rf_mkChar("table"));
  SET_STRING_ELT(result_names, 1, Rf_mkChar("repr"));
  Rf_setAttrib(result, R_NamesSymbol, result_names);

  /*
   * The shared semantic-vector primitive allocates its own destinations first
   * and then copies every ordinary list name beside its exact column pointer
   * in one callback-free pass. It deliberately discards the source class and
   * cache attributes from the returned table shell.
   */
  SEXP table = PROTECT(paradox_snapshot_semantic_vector(source));
  R_xlen_t row_count = 0;
  if (XLENGTH(table) != 0) {
    SEXP first_column = VECTOR_ELT(table, 0);
    if (!Rf_isVector(first_column)) {
      UNPROTECT(3);
      return R_NilValue;
    }
    /*
     * A stable ALTREP column may observe Length here. Attribute capture and
     * the exact source-shell receipt deliberately follow that observation, so
     * a supported reentrant change is either the selected later generation or
     * a terminal mismatch, never old columns paired with unrelated row names.
     */
    row_count = XLENGTH(first_column);
  }
  SEXP observed_classes = paradox_api_raw_attribute(source, R_ClassSymbol);
  paradox_api_map_stored_attributes(
    source,
    capture_upgrade_table_attribute,
    &attributes
  );
  if (!upgrade_table_snapshot_is_current(source, table) ||
      !attributes.valid ||
      (attributes.seen & UPGRADE_TABLE_ATTRIBUTE_NAMES) == 0U ||
      (attributes.seen & UPGRADE_TABLE_ATTRIBUTE_CLASS) == 0U ||
      !exact_upgrade_table_rows(&attributes, row_count) ||
      !exact_upgrade_table_classes(observed_classes, expected_classes)) {
    UNPROTECT(3);
    return R_NilValue;
  }

  SET_VECTOR_ELT(result, 0, table);
  SET_VECTOR_ELT(result, 1, attributes.repr);
  UNPROTECT(3);
  return result;
}

enum {
  UPGRADE_BINDING_RECEIPT_OWNER = 0,
  UPGRADE_BINDING_RECEIPT_CLASS,
  UPGRADE_BINDING_RECEIPT_SYMBOLS,
  UPGRADE_BINDING_RECEIPT_VALUES,
  UPGRADE_BINDING_RECEIPT_ACTIVE,
  UPGRADE_BINDING_RECEIPT_LOCKED,
  UPGRADE_BINDING_RECEIPT_ENVIRONMENT_LOCKED,
  UPGRADE_BINDING_RECEIPT_SIZE
};

static int exact_upgrade_receipt_list(SEXP value, R_xlen_t size) {
  return TYPEOF(value) == VECSXP && !ALTREP(value) &&
    !Rf_isS4(value) && !Rf_isObject(value) &&
    paradox_api_has_no_attributes(value) && XLENGTH(value) == size;
}

static int exact_upgrade_receipt_flags(SEXP value, R_xlen_t size) {
  if (TYPEOF(value) != LGLSXP || ALTREP(value) ||
      Rf_isS4(value) || Rf_isObject(value) ||
      !paradox_api_has_no_attributes(value) || XLENGTH(value) != size) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    if (LOGICAL_ELT(value, index) == NA_LOGICAL) return FALSE;
  }
  return TRUE;
}

void paradox_validate_upgrade_public_binding_receipts(SEXP receipts) {
  if (TYPEOF(receipts) != VECSXP || ALTREP(receipts) ||
      Rf_isS4(receipts) || Rf_isObject(receipts) ||
      !paradox_api_has_no_attributes(receipts)) {
    Rf_error("Invalid Paradox migration binding receipt carrier");
  }

  /*
   * Every carrier and binding symbol was constructed before entry. The scan
   * below allocates nothing, invokes no active binding, and forces no promise.
   * It is therefore the terminal generation barrier after the last joint
   * capsule validation: a pending finalizer may run before this call, but
   * cannot make one transplanted shell combine binding values or lock bits
   * from different generations.
   */
  for (R_xlen_t receipt_index = 0;
      receipt_index < XLENGTH(receipts);
      ++receipt_index) {
    SEXP receipt = VECTOR_ELT(receipts, receipt_index);
    if (!exact_upgrade_receipt_list(
        receipt,
        UPGRADE_BINDING_RECEIPT_SIZE
      )) {
      Rf_error("Invalid Paradox migration binding receipt");
    }
    SEXP owner = VECTOR_ELT(receipt, UPGRADE_BINDING_RECEIPT_OWNER);
    SEXP expected_class = VECTOR_ELT(
      receipt,
      UPGRADE_BINDING_RECEIPT_CLASS
    );
    SEXP symbols = VECTOR_ELT(
      receipt,
      UPGRADE_BINDING_RECEIPT_SYMBOLS
    );
    SEXP values = VECTOR_ELT(receipt, UPGRADE_BINDING_RECEIPT_VALUES);
    SEXP active = VECTOR_ELT(receipt, UPGRADE_BINDING_RECEIPT_ACTIVE);
    SEXP locked = VECTOR_ELT(receipt, UPGRADE_BINDING_RECEIPT_LOCKED);
    SEXP environment_locked = VECTOR_ELT(
      receipt,
      UPGRADE_BINDING_RECEIPT_ENVIRONMENT_LOCKED
    );
    if (TYPEOF(owner) != ENVSXP || Rf_isS4(owner) ||
        !exact_upgrade_receipt_flags(environment_locked, 1)) {
      Rf_error("Invalid Paradox migration binding receipt owner");
    }
    if (paradox_api_raw_attribute(owner, R_ClassSymbol) != expected_class ||
        (R_EnvironmentIsLocked(owner) != FALSE) !=
          (LOGICAL_ELT(environment_locked, 0) != FALSE)) {
      Rf_error("Paradox migration public shell changed during commit");
    }
    if (TYPEOF(symbols) != VECSXP || ALTREP(symbols) ||
        Rf_isS4(symbols) || Rf_isObject(symbols) ||
        !paradox_api_has_no_attributes(symbols)) {
      Rf_error("Invalid Paradox migration binding receipt symbols");
    }
    const R_xlen_t binding_count = XLENGTH(symbols);
    if (!exact_upgrade_receipt_list(values, binding_count) ||
        !exact_upgrade_receipt_flags(active, binding_count) ||
        !exact_upgrade_receipt_flags(locked, binding_count)) {
      Rf_error("Invalid Paradox migration binding receipt fields");
    }

    for (R_xlen_t binding_index = 0;
        binding_index < binding_count;
        ++binding_index) {
      SEXP symbol = VECTOR_ELT(symbols, binding_index);
      SEXP expected_value = VECTOR_ELT(values, binding_index);
      const int expected_active =
        LOGICAL_ELT(active, binding_index) != FALSE;
      const int expected_locked =
        LOGICAL_ELT(locked, binding_index) != FALSE;
      if (TYPEOF(symbol) != SYMSXP ||
          (R_BindingIsActive(symbol, owner) != FALSE) != expected_active ||
          (R_BindingIsLocked(symbol, owner) != FALSE) != expected_locked ||
          (expected_active
            ? paradox_api_active_binding_function(owner, symbol)
            : paradox_api_plain_binding_scan(owner, symbol)) !=
              expected_value) {
        Rf_error("Paradox migration public shell changed during commit");
      }
    }
  }
}

SEXP paradox_upgrade_public_binding_receipts(SEXP receipts) {
  paradox_validate_upgrade_public_binding_receipts(receipts);
  return R_NilValue;
}

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
  paradox_account_work(&walker->work_since_interrupt);
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
  if (index < 0 || index >= R_XLEN_T_MAX) {
    Rf_error("Object graph index is too large to report");
    /* `Rf_error()` does not return; keep static analyzers on that path. */
    return NULL;
  }
  R_xlen_t value = index + 1;
  size_t digits_size = 0;
  do {
    if (digits_size >= sizeof(digits)) {
      Rf_error("Object graph index is too large to report");
      /* `Rf_error()` does not return; keep static analyzers on that path. */
      return NULL;
    }
    digits[digits_size++] =
      (char) ('0' + (int) (value % (R_xlen_t) 10));
    value /= (R_xlen_t) 10;
  } while (value != 0);
  for (size_t left = 0, right = digits_size - 1;
      left < right;
      ++left, --right) {
    const char temporary = digits[left];
    digits[left] = digits[right];
    digits[right] = temporary;
  }
  const size_t prefix_size = strlen(prefix);
  const size_t suffix_size = strlen(suffix);
  if (prefix_size > SIZE_MAX - suffix_size ||
      prefix_size + suffix_size >= SIZE_MAX - digits_size) {
    Rf_error("Object graph path is too large to report");
    /* `Rf_error()` does not return; keep static analyzers on that path. */
    return NULL;
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
    Rf_error("Internal error: malformed object graph path");
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

static int is_candidate_shell(SEXP environment) {
  SEXP classes;
  if (!paradox_api_ordinary_class_snapshot(environment, &classes) ||
      classes == R_NilValue) {
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
  return has_param_set && has_r6;
}

SEXP paradox_upgrade_class_snapshot(SEXP value) {
  PROTECT(value);
  SEXP classes;
  if (!paradox_api_ordinary_class_snapshot(value, &classes)) {
    SEXP result = PROTECT(Rf_ScalarLogical(FALSE));
    UNPROTECT(2);
    return result;
  }
  if (classes == R_NilValue) {
    UNPROTECT(1);
    return R_NilValue;
  }
  SEXP result = PROTECT(Rf_duplicate(classes));
  if (TYPEOF(result) != STRSXP || ALTREP(result) || Rf_isS4(result) ||
      !paradox_api_has_no_attributes(result)) {
    UNPROTECT(2);
    Rf_error("Internal error: could not own ordinary class metadata");
  }
  UNPROTECT(2);
  return result;
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

static void initialize_search_boundaries(
    paradox_upgrade_boundaries_t *boundaries) {
  SEXP environment = R_GlobalEnv;
  while (environment != R_EmptyEnv) {
    if (TYPEOF(environment) != ENVSXP ||
        boundary_contains(boundaries, environment)) {
      Rf_error("Internal error: malformed R search path");
    }
    append_boundary(boundaries, environment);
    environment = paradox_api_parent_environment(environment);
  }
  append_boundary(boundaries, R_EmptyEnv);
  append_boundary(boundaries, R_BaseNamespace);
}

static int imports_environment(SEXP environment) {
  /*
   * A namespace imports frame is not identified by its printable name alone.
   * Ordinary user environments may carry the same `imports:` name. Mirror
   * R's invariant through public operations: a safe scalar raw `name`
   * attribute plus R_BaseNamespace as the direct parent. Requiring ordinary
   * metadata also keeps a hostile ALTREP/classed name from becoming a
   * traversal boundary merely by exposing the prefix.
   */
  SEXP name = paradox_api_raw_attribute(environment, R_NameSymbol);
  if (TYPEOF(name) != STRSXP || ALTREP(name) || Rf_isS4(name) ||
      Rf_isObject(name) || !paradox_api_has_no_attributes(name) ||
      XLENGTH(name) != 1) {
    return FALSE;
  }
  SEXP label = STRING_ELT(name, 0);
  if (label == NA_STRING || Rf_getCharCE(label) == CE_BYTES ||
      strncmp(CHAR(label), "imports:", 8) != 0) {
    return FALSE;
  }
  SEXP parent = PROTECT(paradox_api_parent_environment(environment));
  const int imports = parent == R_BaseNamespace;
  UNPROTECT(1);
  return imports;
}

static int user_database_environment(SEXP environment) {
  if (!Rf_isObject(environment)) {
    return FALSE;
  }
  SEXP classes;
  if (!paradox_api_ordinary_class_snapshot(environment, &classes)) {
    return TRUE;
  }
  return paradox_api_ordinary_class_contains(
    classes,
    "UserDefinedDatabase"
  );
}

static int package_environment(SEXP environment) {
  SEXP name = paradox_api_raw_attribute(environment, R_NameSymbol);
  if (TYPEOF(name) != STRSXP || ALTREP(name) || Rf_isS4(name) ||
      Rf_isObject(name) || !paradox_api_has_no_attributes(name) ||
      XLENGTH(name) != 1) {
    return FALSE;
  }
  SEXP label = STRING_ELT(name, 0);
  return label != NA_STRING && Rf_getCharCE(label) != CE_BYTES &&
    strncmp(CHAR(label), "package:", 8) == 0 && CHAR(label)[8] != '\0';
}

static int namespace_environment(SEXP environment) {
  SEXP marker = PROTECT(paradox_api_optional_plain_binding_snapshot(
    environment,
    Rf_install(".__NAMESPACE__.")
  ));
  if (TYPEOF(marker) != ENVSXP || Rf_isS4(marker)) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP spec = PROTECT(paradox_api_plain_binding_snapshot(
    marker,
    Rf_install("spec")
  ));
  static const char *const spec_attributes[] = {"names"};
  if (TYPEOF(spec) != STRSXP || ALTREP(spec) || Rf_isS4(spec) ||
      !paradox_api_has_only_attributes(spec, spec_attributes, 1) ||
      XLENGTH(spec) < 1) {
    UNPROTECT(2);
    return FALSE;
  }
  SEXP name = STRING_ELT(spec, 0);
  if (name == NA_STRING || Rf_getCharCE(name) == CE_BYTES ||
      CHAR(name)[0] == '\0') {
    UNPROTECT(2);
    return FALSE;
  }
  SEXP scalar_name = PROTECT(Rf_ScalarString(name));
  SEXP registered = PROTECT(R_FindNamespace(scalar_name));
  const int result = registered == environment;
  UNPROTECT(4);
  return result;
}

static int environment_boundary(
    const paradox_upgrade_walker_t *walker, SEXP environment) {
  return boundary_contains(&walker->boundaries, environment) ||
    /*
     * Object-table environments route enumeration and binding access through
     * arbitrary callbacks and do not have an ordinary frame layout. They are
     * traversal boundaries, just like namespaces and package environments.
     * This predicate must precede the namespace/package predicates: old R
     * implements those through an object-table lookup.
     */
    user_database_environment(environment) ||
    namespace_environment(environment) ||
    package_environment(environment) ||
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

#if R_VERSION < R_Version(4, 5, 0)
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
#elif R_VERSION < R_Version(4, 6, 0)
static void fail_opaque_promise(const paradox_upgrade_path_t *path) {
  SEXP location = PROTECT(render_path(path));
  Rf_error(
    "Recursive Paradox object upgrade cannot inspect a promise on R 4.5 "
    "(at `%s`); load and upgrade this object under R 4.0--4.4 or R >= 4.6",
    CHAR(location)
  );
}
#endif

static void schedule_binding(
    paradox_upgrade_walker_t *walker,
    SEXP environment,
    SEXP symbol,
    const paradox_upgrade_path_t *path) {
  if (R_BindingIsActive(symbol, environment)) {
    SEXP function = PROTECT(paradox_api_active_binding_function(
      environment,
      symbol
    ));
    if (function == R_UnboundValue) {
      UNPROTECT(1);
      SEXP location = PROTECT(render_path(path));
      Rf_error(
        "Recursive Paradox object upgrade cannot inspect an active binding "
        "on R 3.6 (at `%s`); load and upgrade this object under R >= 4.0",
        CHAR(location)
      );
    }
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
#if R_VERSION < R_Version(4, 5, 0)
      schedule_promise_edges(walker, value, path);
#elif R_VERSION < R_Version(4, 6, 0)
      fail_opaque_promise(path);
#endif
    } else {
      schedule_node(walker, value, path);
    }
  }
  UNPROTECT(1);
#endif
}

#if R_VERSION < R_Version(4, 0, 0)
static void schedule_builtin_current_binding(
    paradox_upgrade_walker_t *walker,
    SEXP environment,
    SEXP symbol,
    const paradox_upgrade_path_t *path) {
  if (symbol == Rf_install(".__enclos_env__") ||
      R_BindingIsActive(symbol, environment)) {
    return;
  }
  SEXP value = PROTECT(paradox_api_stored_binding_snapshot(
    environment,
    symbol
  ));
  /*
   * An exact built-in R6 shell is locked against new public members. Its
   * locked ordinary closures are treated as package-generated methods;
   * following their R6 enclosure would only rediscover active facades already
   * represented by the authenticated core. An unlocked replacement closure
   * and every non-function public value remain graph edges. Replacing and then
   * relocking a method is unsupported and indistinguishable on R 3.6, so that
   * closure is opaque just like a replaced package active facade.
   */
  if (value != R_UnboundValue &&
      (TYPEOF(value) != CLOSXP ||
        !R_BindingIsLocked(symbol, environment))) {
    if (TYPEOF(value) == PROMSXP) {
      schedule_promise_edges(walker, value, path);
    } else {
      schedule_node(walker, value, path);
    }
  }
  UNPROTECT(1);
}
#endif

static void schedule_environment(
    paradox_upgrade_walker_t *walker,
    SEXP environment,
    const paradox_upgrade_path_t *path) {
  if (environment_boundary(walker, environment)) return;

#if R_VERSION < R_Version(4, 0, 0)
  int current_builtin = FALSE;
#endif
  /*
   * Current shells are candidates as well as legacy shells.  R preflight
   * distinguishes them and performs the complete callback-free capsule graph
   * validation before any legacy shell is changed.  A shallow carrier/schema
   * check here would otherwise let a semantically corrupt current capsule hide
   * inside a mixed graph and violate the all-roots-before-commit guarantee.
   */
  if (is_candidate_shell(environment)) {
    append_candidate(
      &walker->candidates,
      environment,
      path
    );
#if R_VERSION < R_Version(4, 0, 0)
    /*
     * R 3.6 cannot retrieve an active binding's closure. Exact built-in
     * current shells have already authenticated every R6 topology receipt,
     * and their active facades expose only state held by the canonical core.
     * Schedule that authority directly and continue through ordinary public
     * fields. Additive/custom shells do not enter this exception.
     */
    SEXP core = PROTECT(paradox_builtin_current_core_snapshot(environment));
    if (core != R_UnboundValue) {
      current_builtin = TRUE;
      schedule_node(
        walker,
        core,
        literal_path(path, ".core")
      );
    }
    UNPROTECT(1);
#endif
  }

  SEXP parent = PROTECT(paradox_api_parent_environment(environment));
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
#if R_VERSION < R_Version(4, 0, 0)
    if (current_builtin) {
      schedule_builtin_current_binding(
        walker,
        environment,
        symbol,
        named_path(path, "[[\"", name, "\"]]")
      );
      continue;
    }
#endif
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
  SEXP roots;
  R_xlen_t count;
  R_xlen_t capacity;
} paradox_upgrade_attribute_map_t;

static void record_attribute(SEXP tag, SEXP value, void *data) {
  paradox_upgrade_attribute_map_t *map = data;
  if (map->count >= map->capacity) {
    Rf_error("Internal error: attribute count changed during inspection");
  }
  map->items[map->count] =
    (paradox_upgrade_attribute_t) {tag, value};
  SET_VECTOR_ELT(map->roots, map->count, value);
  ++map->count;
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
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, count));
  paradox_upgrade_attribute_map_t map = {attributes, roots, 0, count};
  paradox_api_map_stored_attributes(node, record_attribute, &map);
  if (map.count != count) {
    UNPROTECT(1);
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
  UNPROTECT(1);
}

static void schedule_vector(
    paradox_upgrade_walker_t *walker,
    SEXP vector,
    const paradox_upgrade_path_t *path) {
  SEXP source = vector;
  PROTECT_INDEX source_index;
  PROTECT_WITH_INDEX(source, &source_index);
  const SEXPTYPE source_type = (SEXPTYPE) TYPEOF(source);
  const R_xlen_t count = XLENGTH(source);
  /*
   * Path construction allocates.  Copy every child identity into one ordinary
   * root carrier before constructing the first path, so a pending finalizer
   * cannot make one discovery pass combine elements from different
   * generations of an otherwise ordinary caller-owned list.  Allocate the
   * carrier first: if that allocation changes the source, the receipt below
   * rejects a changed length/type and the allocation-free copy observes only
   * the post-allocation generation.
   *
   * An ALTREP list is duplicated once, retaining the existing stable-provider
   * contract and the self-returning Duplicate-method protection invariant,
   * before its elements are materialized into the same carrier.
   */
  SEXP children = PROTECT(Rf_allocVector(VECSXP, count));
  if (ALTREP(source)) {
    REPROTECT(source = Rf_duplicate(source), source_index);
  }
  if ((SEXPTYPE) TYPEOF(source) != source_type ||
      XLENGTH(source) != count) {
    UNPROTECT(2);
    Rf_error("Object graph vector changed during inspection");
  }
  for (R_xlen_t index = 0; index < count; ++index) {
    SET_VECTOR_ELT(children, index, VECTOR_ELT(source, index));
  }
  for (R_xlen_t index = count; index > 0; --index) {
    SEXP child = VECTOR_ELT(children, index - 1);
    const paradox_upgrade_path_t *child_path = indexed_path(
      path,
      "[[",
      index - 1,
      "]]"
    );
    schedule_node(
      walker,
      child,
      child_path
    );
  }
  UNPROTECT(2);
}

static void schedule_closure(
    paradox_upgrade_walker_t *walker,
    SEXP closure,
    const paradox_upgrade_path_t *path) {
  /*
   * Before R 4.5 the public body()/environment() bridge evaluates small base
   * calls and may therefore run a pending finalizer between field reads.
   * Duplicate the closure shell once first; the private shell cannot then be
   * rewired into a formals/body/environment combination that never existed.
   * Current R exposes all three direct accessors, so retain its allocation-free
   * path.
   */
#if R_VERSION < R_Version(4, 5, 0)
  SEXP stable_closure = PROTECT(Rf_duplicate(closure));
  if (TYPEOF(stable_closure) != CLOSXP || stable_closure == closure) {
    UNPROTECT(1);
    Rf_error("Object graph closure could not be snapshotted");
  }
#else
  SEXP stable_closure = closure;
  PROTECT(stable_closure);
#endif
  SEXP formals = PROTECT(paradox_api_closure_formals(stable_closure));
  SEXP expression = PROTECT(paradox_api_closure_expression(stable_closure));
  SEXP environment = PROTECT(paradox_api_closure_environment(stable_closure));
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
  UNPROTECT(4);
}

static void schedule_pairlist(
    paradox_upgrade_walker_t *walker,
    SEXP cell,
    const paradox_upgrade_path_t *path) {
  SEXP cdr = PROTECT(CDR(cell));
  SEXP tag = PROTECT(TAG(cell));
  SEXP car = PROTECT(CAR(cell));
  const paradox_upgrade_path_t *cdr_path = literal_path(path, ".cdr");
  schedule_node(walker, cdr, cdr_path);
  const paradox_upgrade_path_t *tag_path = literal_path(path, ".tag");
  schedule_node(walker, tag, tag_path);
  const paradox_upgrade_path_t *car_path = literal_path(path, ".car");
  schedule_node(walker, car, car_path);
  UNPROTECT(3);
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
#if R_VERSION < R_Version(4, 5, 0)
    schedule_promise_edges(walker, node, work.path);
#elif R_VERSION < R_Version(4, 6, 0)
    fail_opaque_promise(work.path);
#endif
    return;
  case BCODESXP: {
    SEXP expression = PROTECT(paradox_api_bytecode_expression(node));
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
