#include <limits.h>
#include <stdint.h>
#include <string.h>

#include "core_state.h"
#include "builtin_condition.h"
#include "domain_admission.h"
#include "paramset_domain_common.h"
#include "paramset_params_internal.h"
#include "r_api_compat.h"
#include "r_utils.h"
#include "shell_auth.h"
#include "upgrade_graph.h"

#define PARADOX_UPGRADE_MAX_ATTRIBUTES ((R_xlen_t) 65536)

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
  SEXP roots;
  PROTECT_INDEX roots_index;
} paradox_upgrade_boundaries_t;

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
  if (TYPEOF(source) != VECSXP || ALTREP(source) || Rf_isS4(source) ||
      Rf_isObject(source) || XLENGTH(source) != size ||
      !paradox_api_has_only_attributes(source, allowed_attributes, 1)) {
    UNPROTECT(protect_count);
    return R_NilValue;
  }
  SEXP source_names = paradox_api_raw_attribute(source, R_NamesSymbol);
  if ((source_names != R_NilValue) != has_names ||
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
  if (row_count > INT_MAX) return FALSE;
  if ((attributes->seen & UPGRADE_TABLE_ATTRIBUTE_ROWS) == 0U) {
    /*
     * Paradox 1 constructed keyed internal tables as a classed list followed
     * by data.table::setkeyv().  That spelling legitimately omits row.names
     * even when the columns are populated.  Absence supplies no competing row
     * count: the exact selected columns define it, and the R-side migration
     * validator subsequently requires all native-owned columns to have that
     * same length.  A present row.names attribute, by contrast, must agree
     * exactly with the selected first-column length below.
     */
    return TRUE;
  }
  return exact_upgrade_table_row_names(attributes->row_names, row_count);
}

static int upgrade_table_column_is_supported(SEXP column) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(column);
  const int atomic = type == LGLSXP || type == INTSXP || type == REALSXP ||
    type == CPLXSXP || type == STRSXP || type == RAWSXP;
  return (atomic || (type == VECSXP && !ALTREP(column))) &&
    !Rf_isS4(column) && !Rf_isObject(column) &&
    paradox_api_has_no_attributes(column);
}

static int upgrade_table_leaf_is_owned_atomic(SEXP value) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  return !Rf_isS4(value) &&
    (type == LGLSXP || type == INTSXP || type == REALSXP ||
      type == CPLXSXP || type == STRSXP || type == RAWSXP);
}

typedef enum {
  UPGRADE_TABLE_SCHEMA_GENERIC = 0,
  UPGRADE_TABLE_SCHEMA_DOMAIN,
  UPGRADE_TABLE_SCHEMA_DEPENDENCIES
} upgrade_table_schema_t;

typedef enum {
  UPGRADE_TABLE_LEAF_BUILTIN = 0,
  UPGRADE_TABLE_LEAF_OPAQUE,
  UPGRADE_TABLE_LEAF_CONDITION,
  UPGRADE_TABLE_LEAF_DOMAIN_CARGO,
  UPGRADE_TABLE_LEAF_DOMAIN_LEVELS,
  UPGRADE_TABLE_LEAF_DOMAIN_SPECIAL_VALUES,
  UPGRADE_TABLE_LEAF_DOMAIN_DEFAULT,
  UPGRADE_TABLE_LEAF_DOMAIN_INIT,
  UPGRADE_TABLE_LEAF_DOMAIN_REQUIREMENTS
} upgrade_table_leaf_policy_t;

static int upgrade_table_has_exact_names(SEXP table,
    const char *const *expected, R_xlen_t count) {
  SEXP names = paradox_api_raw_attribute(table, R_NamesSymbol);
  if (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
      Rf_isObject(names) || !paradox_api_has_no_attributes(names) ||
      XLENGTH(table) != count || XLENGTH(names) != count) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP name = STRING_ELT(names, index);
    if (name == NA_STRING || Rf_getCharCE(name) == CE_BYTES ||
        strcmp(CHAR(name), expected[index]) != 0) {
      return FALSE;
    }
  }
  return TRUE;
}

static upgrade_table_schema_t upgrade_table_schema(SEXP table) {
  static const char *const dependency_names[] = {"id", "on", "cond"};
  if (upgrade_table_has_exact_names(
      table,
      paradox_domain_column_names,
      PARADOX_DOMAIN_COLUMN_COUNT
    ) || upgrade_table_has_exact_names(
      table,
      paradox_domain_column_names,
      PARADOX_DOMAIN_PERMANENT_COLUMNS
    )) {
    return UPGRADE_TABLE_SCHEMA_DOMAIN;
  }
  if (upgrade_table_has_exact_names(table, dependency_names, 3)) {
    return UPGRADE_TABLE_SCHEMA_DEPENDENCIES;
  }
  return UPGRADE_TABLE_SCHEMA_GENERIC;
}

static int upgrade_table_domain_class_is_typed(SEXP table,
    R_xlen_t row) {
  SEXP classes = VECTOR_ELT(table, PARADOX_DOMAIN_CLS);
  if (TYPEOF(classes) != STRSXP || ALTREP(classes) || Rf_isS4(classes) ||
      Rf_isObject(classes) || !paradox_api_has_no_attributes(classes) ||
      row >= XLENGTH(classes)) {
    Rf_error("legacy Domain class column is malformed");
  }
  SEXP cls = STRING_ELT(classes, row);
  if (cls == NA_STRING || Rf_getCharCE(cls) == CE_BYTES) {
    Rf_error("legacy Domain class column is malformed");
  }
  if (strcmp(CHAR(cls), "ParamUty") == 0) return FALSE;
  if (strcmp(CHAR(cls), "ParamDbl") == 0 ||
      strcmp(CHAR(cls), "ParamInt") == 0 ||
      strcmp(CHAR(cls), "ParamFct") == 0 ||
      strcmp(CHAR(cls), "ParamLgl") == 0) {
    return TRUE;
  }
  Rf_error("legacy Domain class column is malformed");
}

static upgrade_table_leaf_policy_t upgrade_table_leaf_policy(
    upgrade_table_schema_t schema, R_xlen_t column) {
  if (schema == UPGRADE_TABLE_SCHEMA_DEPENDENCIES && column == 2) {
    return UPGRADE_TABLE_LEAF_CONDITION;
  }
  if (schema != UPGRADE_TABLE_SCHEMA_DOMAIN) {
    return UPGRADE_TABLE_LEAF_BUILTIN;
  }
  switch ((enum paradox_domain_column) column) {
  case PARADOX_DOMAIN_CARGO:
    return UPGRADE_TABLE_LEAF_DOMAIN_CARGO;
  case PARADOX_DOMAIN_LEVELS:
    return UPGRADE_TABLE_LEAF_DOMAIN_LEVELS;
  case PARADOX_DOMAIN_SPECIAL_VALS:
    return UPGRADE_TABLE_LEAF_DOMAIN_SPECIAL_VALUES;
  case PARADOX_DOMAIN_DEFAULT:
    return UPGRADE_TABLE_LEAF_DOMAIN_DEFAULT;
  case PARADOX_DOMAIN_INIT:
    return UPGRADE_TABLE_LEAF_DOMAIN_INIT;
  case PARADOX_DOMAIN_REQUIREMENTS:
    return UPGRADE_TABLE_LEAF_DOMAIN_REQUIREMENTS;
  case PARADOX_DOMAIN_TRAFO:
    return UPGRADE_TABLE_LEAF_OPAQUE;
  default:
    return UPGRADE_TABLE_LEAF_BUILTIN;
  }
}

NORET static void upgrade_table_column_error(
    SEXP table, R_xlen_t index, const char *problem) {
  SEXP names = paradox_api_raw_attribute(table, R_NamesSymbol);
  if (TYPEOF(names) == STRSXP && !ALTREP(names) && !Rf_isS4(names) &&
      !Rf_isObject(names) && paradox_api_has_no_attributes(names) &&
      XLENGTH(names) == XLENGTH(table)) {
    SEXP name = STRING_ELT(names, index);
    if (name != NA_STRING && Rf_getCharCE(name) != CE_BYTES) {
      Rf_error("table column `%s` %s", CHAR(name), problem);
    }
  }
  Rf_error("table column %.0f %s", (double) (index + 1), problem);
}

static void admit_upgrade_table_column(SEXP table, R_xlen_t index) {
  SEXP column = VECTOR_ELT(table, index);
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(column);
  const int atomic = type == LGLSXP || type == INTSXP || type == REALSXP ||
    type == CPLXSXP || type == STRSXP || type == RAWSXP;
  if (Rf_isS4(column) || (!atomic && type != VECSXP) ||
      (type == VECSXP && ALTREP(column))) {
    upgrade_table_column_error(
      table,
      index,
      "has unsupported structural representation"
    );
  }
  if (Rf_isObject(column) || !paradox_api_has_no_attributes(column)) {
    upgrade_table_column_error(
      table,
      index,
      "has unsupported attributes"
    );
  }
  if (XLENGTH(column) > INT_MAX) {
    upgrade_table_column_error(table, index, "is too large");
  }
}

/*
 * The ordinary table snapshot owns its names and column-pointer carrier, but
 * a data.table by-reference write can still mutate an aliased ordinary column
 * payload after the native receipt.  Own every top-level column before that
 * receipt. Atomic ALTREP is materialized by the shared semantic primitive. An
 * interpreted list column receives both a private identity receipt and a fresh
 * outward carrier; every non-S4 atomic leaf is independently snapshotted there
 * too, while genuinely opaque leaves retain identity.
 */
static SEXP snapshot_upgrade_table_leaf(SEXP source,
    upgrade_table_leaf_policy_t policy, int typed,
    R_xlen_t *work_since_interrupt) {
  switch (policy) {
  case UPGRADE_TABLE_LEAF_OPAQUE:
    return source;
  case UPGRADE_TABLE_LEAF_CONDITION:
    return paradox_builtin_condition_snapshot(
      source,
      work_since_interrupt
    );
  case UPGRADE_TABLE_LEAF_DOMAIN_CARGO:
    return paradox_detach_domain_row_field(
      source,
      PARADOX_DOMAIN_CARGO,
      typed,
      work_since_interrupt
    );
  case UPGRADE_TABLE_LEAF_DOMAIN_LEVELS:
    return paradox_detach_domain_row_field(
      source,
      PARADOX_DOMAIN_LEVELS,
      typed,
      work_since_interrupt
    );
  case UPGRADE_TABLE_LEAF_DOMAIN_SPECIAL_VALUES:
    return paradox_detach_domain_row_field(
      source,
      PARADOX_DOMAIN_SPECIAL_VALS,
      typed,
      work_since_interrupt
    );
  case UPGRADE_TABLE_LEAF_DOMAIN_DEFAULT:
    return paradox_detach_domain_row_field(
      source,
      PARADOX_DOMAIN_DEFAULT,
      typed,
      work_since_interrupt
    );
  case UPGRADE_TABLE_LEAF_DOMAIN_INIT:
    return paradox_detach_domain_row_field(
      source,
      PARADOX_DOMAIN_INIT,
      typed,
      work_since_interrupt
    );
  case UPGRADE_TABLE_LEAF_DOMAIN_REQUIREMENTS:
    return paradox_detach_domain_row_field(
      source,
      PARADOX_DOMAIN_REQUIREMENTS,
      typed,
      work_since_interrupt
    );
  case UPGRADE_TABLE_LEAF_BUILTIN:
    return paradox_snapshot_builtin_value_leaf(source);
  default:
    Rf_error("Internal error: invalid legacy-table leaf policy");
  }
}

static SEXP snapshot_upgrade_table_list_column(
    SEXP source, SEXP list_receipts, R_xlen_t column_index,
    upgrade_table_schema_t schema, SEXP owned_table) {
  SEXP captured = PROTECT(paradox_snapshot_semantic_vector(source));
  SET_VECTOR_ELT(list_receipts, column_index, captured);
  SEXP owned = PROTECT(paradox_snapshot_semantic_vector(captured));
  const upgrade_table_leaf_policy_t policy =
    upgrade_table_leaf_policy(schema, column_index);
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t index = 0; index < XLENGTH(captured); ++index) {
    paradox_account_work(&work_since_interrupt);
    SEXP leaf = VECTOR_ELT(captured, index);
    const int typed = schema != UPGRADE_TABLE_SCHEMA_DOMAIN ||
      upgrade_table_domain_class_is_typed(owned_table, index);
    SEXP leaf_snapshot = PROTECT(snapshot_upgrade_table_leaf(
      leaf,
      policy,
      typed,
      &work_since_interrupt
    ));
    if (leaf_snapshot == R_UnboundValue) {
      UNPROTECT(3);
      upgrade_table_column_error(
        owned_table,
        column_index,
        "contains malformed interpreted structure"
      );
    }
    SET_VECTOR_ELT(owned, index, leaf_snapshot);
    UNPROTECT(1);
  }
  UNPROTECT(2);
  return owned;
}

static SEXP snapshot_upgrade_table_columns(
    SEXP selected, SEXP list_receipts) {
  const R_xlen_t count = XLENGTH(selected);
  for (R_xlen_t index = 0; index < count; ++index) {
    admit_upgrade_table_column(selected, index);
  }

  SEXP owned = PROTECT(paradox_snapshot_semantic_vector(selected));
  const upgrade_table_schema_t schema = upgrade_table_schema(selected);
  /* Own every atomic selector first. Domain list-leaf policy is derived from
   * this private `cls` generation, never by rereading a caller-owned column
   * after nested allocations have begun. */
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP source_column = VECTOR_ELT(selected, index);
    if (TYPEOF(source_column) == VECSXP) continue;
    SEXP column = PROTECT(paradox_snapshot_semantic_vector(source_column));
    SET_VECTOR_ELT(owned, index, column);
    UNPROTECT(1);
  }
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP source_column = VECTOR_ELT(selected, index);
    if (TYPEOF(source_column) != VECSXP) continue;
    SEXP column = PROTECT(snapshot_upgrade_table_list_column(
      source_column,
      list_receipts,
      index,
      schema,
      owned
    ));
    SET_VECTOR_ELT(owned, index, column);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return owned;
}

static int upgrade_table_owned_column_structure_is_current(
    SEXP source, SEXP owned) {
  return upgrade_table_column_is_supported(source) &&
    TYPEOF(source) == TYPEOF(owned) && !ALTREP(owned) &&
    !Rf_isS4(owned) && !Rf_isObject(owned) &&
    paradox_api_has_no_attributes(owned);
}

static int upgrade_table_list_column_shape_is_current(
    SEXP value, R_xlen_t size) {
  return TYPEOF(value) == VECSXP && !ALTREP(value) &&
    !Rf_isS4(value) && !Rf_isObject(value) &&
    paradox_api_has_no_attributes(value) && XLENGTH(value) == size;
}

/* Complete every stable nested ALTREP Length callback before any ordinary
 * payload or metadata receipt. A callback may rewrite a different cell by
 * reference; the following callback-free phase reauthenticates all of them. */
static int upgrade_table_builtin_leaf_lengths_are_current(
    SEXP source, SEXP owned) {
  if (!upgrade_table_leaf_is_owned_atomic(source)) {
    return source == owned;
  }
  if (!ALTREP(source)) return TRUE;
  if (TYPEOF(source) != TYPEOF(owned) || ALTREP(owned) ||
      Rf_isS4(owned)) {
    return FALSE;
  }
  /* Nested special-value carriers can rewrite their own cells from a Length
   * callback.  The outer shallow receipt then no longer retains this exact
   * leaf, so root it directly for the whole dispatch. */
  PROTECT(source);
  PROTECT(owned);
  const int current = XLENGTH(source) == XLENGTH(owned);
  UNPROTECT(2);
  return current;
}

static int exact_upgrade_no_default(SEXP value) {
  static const char *const allowed[] = {"class"};
  /* Bound the raw selector first; the shared Domain classifier remains the
   * sole semantic owner of the marker spelling (including formal-S4
   * exclusion). */
  if (!paradox_api_has_only_attributes(value, allowed, 1)) {
    return FALSE;
  }
  SEXP classes = paradox_api_raw_attribute(value, R_ClassSymbol);
  return paradox_domain_exact_no_default_marker(value, classes) == 1;
}

static int upgrade_table_special_lengths_are_current(
    SEXP source, SEXP owned, int typed) {
  if (TYPEOF(source) != VECSXP || TYPEOF(owned) != VECSXP ||
      ALTREP(source) || ALTREP(owned) || Rf_isS4(source) ||
      Rf_isS4(owned) || XLENGTH(source) != XLENGTH(owned)) {
    return FALSE;
  }
  if (!typed) return TRUE;
  for (R_xlen_t index = 0; index < XLENGTH(source); ++index) {
    if (!upgrade_table_builtin_leaf_lengths_are_current(
        VECTOR_ELT(source, index),
        VECTOR_ELT(owned, index)
      )) {
      return FALSE;
    }
  }
  return TRUE;
}

static int upgrade_table_leaf_lengths_are_current(SEXP source,
    SEXP owned, upgrade_table_leaf_policy_t policy, int typed) {
  switch (policy) {
  case UPGRADE_TABLE_LEAF_OPAQUE:
    return source == owned;
  case UPGRADE_TABLE_LEAF_CONDITION:
    return paradox_builtin_condition_snapshot_lengths_current(
      source,
      owned
    );
  case UPGRADE_TABLE_LEAF_DOMAIN_CARGO:
    return TRUE;
  case UPGRADE_TABLE_LEAF_DOMAIN_LEVELS:
    return source == R_NilValue
      ? owned == R_NilValue
      : upgrade_table_builtin_leaf_lengths_are_current(source, owned);
  case UPGRADE_TABLE_LEAF_DOMAIN_SPECIAL_VALUES:
    return upgrade_table_special_lengths_are_current(
      source,
      owned,
      typed
    );
  case UPGRADE_TABLE_LEAF_DOMAIN_DEFAULT:
  case UPGRADE_TABLE_LEAF_DOMAIN_INIT:
    if (source != owned && exact_upgrade_no_default(owned)) {
      return exact_upgrade_no_default(source);
    }
    return typed
      ? upgrade_table_builtin_leaf_lengths_are_current(source, owned)
      : source == owned;
  case UPGRADE_TABLE_LEAF_DOMAIN_REQUIREMENTS:
    return paradox_builtin_requirements_snapshot_lengths_current(
      source,
      owned
    );
  case UPGRADE_TABLE_LEAF_BUILTIN:
    return upgrade_table_builtin_leaf_lengths_are_current(source, owned);
  default:
    return FALSE;
  }
}

static int upgrade_table_list_leaf_lengths_are_current(
    SEXP source, SEXP captured, SEXP owned,
    upgrade_table_schema_t schema, R_xlen_t column_index,
    SEXP owned_table) {
  const R_xlen_t size = XLENGTH(captured);
  if (!upgrade_table_list_column_shape_is_current(source, size) ||
      !upgrade_table_list_column_shape_is_current(captured, size) ||
      !upgrade_table_list_column_shape_is_current(owned, size)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP source_leaf = VECTOR_ELT(source, index);
    SEXP captured_leaf = VECTOR_ELT(captured, index);
    SEXP owned_leaf = VECTOR_ELT(owned, index);
    if (source_leaf != captured_leaf) return FALSE;
    const upgrade_table_leaf_policy_t policy =
      upgrade_table_leaf_policy(schema, column_index);
    const int typed = schema != UPGRADE_TABLE_SCHEMA_DOMAIN ||
      upgrade_table_domain_class_is_typed(owned_table, index);
    if (!upgrade_table_leaf_lengths_are_current(
        source_leaf,
        owned_leaf,
        policy,
        typed
      )) {
      return FALSE;
    }
  }
  return TRUE;
}

static int upgrade_table_leaf_is_current(SEXP source, SEXP owned,
    upgrade_table_leaf_policy_t policy, int typed) {
  switch (policy) {
  case UPGRADE_TABLE_LEAF_OPAQUE:
    return source == owned;
  case UPGRADE_TABLE_LEAF_CONDITION:
    return paradox_builtin_condition_snapshot_is_current(source, owned);
  case UPGRADE_TABLE_LEAF_DOMAIN_CARGO:
    return paradox_domain_cargo_snapshot_is_current(source, owned);
  case UPGRADE_TABLE_LEAF_DOMAIN_LEVELS:
    return source == R_NilValue
      ? owned == R_NilValue
      : paradox_builtin_value_leaf_receipt_current(source, owned);
  case UPGRADE_TABLE_LEAF_DOMAIN_SPECIAL_VALUES:
    return paradox_domain_special_values_snapshot_is_current(
      source,
      owned,
      typed
    );
  case UPGRADE_TABLE_LEAF_DOMAIN_DEFAULT:
  case UPGRADE_TABLE_LEAF_DOMAIN_INIT:
    if (source != owned && exact_upgrade_no_default(owned)) {
      return exact_upgrade_no_default(source);
    }
    return typed
      ? (ALTREP(source)
        ? paradox_altrep_builtin_value_leaf_metadata_is_current(
            source,
            owned
          )
        : paradox_builtin_value_leaf_receipt_current(source, owned))
      : source == owned;
  case UPGRADE_TABLE_LEAF_DOMAIN_REQUIREMENTS:
    return paradox_builtin_requirements_snapshot_is_current(source, owned);
  case UPGRADE_TABLE_LEAF_BUILTIN:
    if (!upgrade_table_leaf_is_owned_atomic(source)) {
      return source == owned;
    }
    return ALTREP(source)
      ? paradox_altrep_builtin_value_leaf_metadata_is_current(source, owned)
      : paradox_builtin_value_leaf_receipt_current(source, owned);
  default:
    return FALSE;
  }
}

static int upgrade_table_list_leaves_are_current(
    SEXP source, SEXP captured, SEXP owned,
    upgrade_table_schema_t schema, R_xlen_t column_index,
    SEXP owned_table) {
  const R_xlen_t size = XLENGTH(captured);
  if (!upgrade_table_list_column_shape_is_current(source, size) ||
      !upgrade_table_list_column_shape_is_current(captured, size) ||
      !upgrade_table_list_column_shape_is_current(owned, size)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP source_leaf = VECTOR_ELT(source, index);
    SEXP captured_leaf = VECTOR_ELT(captured, index);
    SEXP owned_leaf = VECTOR_ELT(owned, index);
    if (source_leaf != captured_leaf) return FALSE;
    const upgrade_table_leaf_policy_t policy =
      upgrade_table_leaf_policy(schema, column_index);
    const int typed = schema != UPGRADE_TABLE_SCHEMA_DOMAIN ||
      upgrade_table_domain_class_is_typed(owned_table, index);
    if (!upgrade_table_leaf_is_current(
        source_leaf,
        owned_leaf,
        policy,
        typed
      )) {
      return FALSE;
    }
  }
  return TRUE;
}

static int upgrade_table_owned_columns_are_current(
    SEXP selected, SEXP owned, SEXP list_receipts) {
  if (TYPEOF(selected) != VECSXP || ALTREP(selected) || Rf_isS4(selected) ||
      TYPEOF(owned) != VECSXP || ALTREP(owned) || Rf_isS4(owned) ||
      Rf_isObject(owned) || TYPEOF(list_receipts) != VECSXP ||
      ALTREP(list_receipts) || Rf_isS4(list_receipts) ||
      Rf_isObject(list_receipts) ||
      !paradox_api_has_no_attributes(list_receipts) ||
      XLENGTH(selected) != XLENGTH(owned) ||
      XLENGTH(selected) != XLENGTH(list_receipts)) {
    return FALSE;
  }
  const R_xlen_t count = XLENGTH(selected);
  const upgrade_table_schema_t schema = upgrade_table_schema(owned);

  /* Finish every callback-capable stable-ALTREP Length observation first. A
   * later column's Length method may rewrite an earlier ordinary source
   * payload by reference, so interleaving these observations with payload
   * comparisons would not make the latter a terminal receipt. */
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP source_column = VECTOR_ELT(selected, index);
    SEXP owned_column = VECTOR_ELT(owned, index);
    SEXP list_receipt = VECTOR_ELT(list_receipts, index);
    if (!upgrade_table_owned_column_structure_is_current(
        source_column,
        owned_column
      )) {
      return FALSE;
    }
    if (TYPEOF(source_column) == VECSXP) {
      if (!upgrade_table_list_leaf_lengths_are_current(
          source_column,
          list_receipt,
          owned_column,
          schema,
          index,
          owned
        )) {
        return FALSE;
      }
    } else if (list_receipt != R_NilValue ||
        (ALTREP(source_column) &&
          XLENGTH(source_column) != XLENGTH(owned_column))) {
      return FALSE;
    }
  }

  /* No supported observation below can allocate or invoke R. Re-admit every
   * shell after the final Length callback, then compare every ordinary
   * payload before the table attributes and outer identities are selected. */
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP source_column = VECTOR_ELT(selected, index);
    SEXP owned_column = VECTOR_ELT(owned, index);
    SEXP list_receipt = VECTOR_ELT(list_receipts, index);
    if (!upgrade_table_owned_column_structure_is_current(
        source_column,
        owned_column
      ) || (TYPEOF(source_column) == VECSXP
        ? !upgrade_table_list_leaves_are_current(
            source_column,
            list_receipt,
            owned_column,
            schema,
            index,
            owned
          )
        : (!ALTREP(source_column) &&
            !paradox_ordinary_vector_payload_equal(
              source_column,
              owned_column
            )))) {
      return FALSE;
    }
  }
  return TRUE;
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
  SEXP list_receipts = PROTECT(Rf_allocVector(VECSXP, XLENGTH(source)));
  SEXP selected_table = PROTECT(paradox_snapshot_semantic_vector(source));
  SEXP table = PROTECT(snapshot_upgrade_table_columns(
    selected_table,
    list_receipts
  ));
  if (table == R_NilValue) {
    UNPROTECT(5);
    return R_NilValue;
  }
  R_xlen_t row_count = 0;
  if (XLENGTH(table) != 0) {
    SEXP first_column = VECTOR_ELT(table, 0);
    /*
     * Every top-level column is ordinary and independently owned now.  Its
     * length cannot dispatch or expose source state while the terminal table
     * and attribute receipts select one exact legacy generation below.
     */
    row_count = XLENGTH(first_column);
  }
  /* A stable ALTREP source receives its final bounded Length observation
   * before attribute capture.  A callback may therefore precede the selected
   * source generation or make the terminal pointer/name receipt fail, but it
   * cannot run between attribute capture and that receipt. */
  if (!upgrade_table_owned_columns_are_current(
      selected_table,
      table,
      list_receipts
    )) {
    UNPROTECT(5);
    return R_NilValue;
  }
  R_xlen_t attribute_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
      source,
      7,
      capture_upgrade_table_attribute,
      &attributes,
      &attribute_count
    ) || !attributes.valid || attribute_count > 7) {
    UNPROTECT(5);
    return R_NilValue;
  }
  /*
   * The bounded pass proved the raw spine finite and callbacks cannot run
   * before the terminal checks, so the direct class/name selectors below
   * cannot encounter a later cyclic generation.
   */
  SEXP observed_classes = paradox_api_raw_attribute(source, R_ClassSymbol);
  if (!upgrade_table_snapshot_is_current(source, selected_table) ||
      !attributes.valid ||
      (attributes.seen & UPGRADE_TABLE_ATTRIBUTE_NAMES) == 0U ||
      (attributes.seen & UPGRADE_TABLE_ATTRIBUTE_CLASS) == 0U ||
      !exact_upgrade_table_rows(&attributes, row_count) ||
      !exact_upgrade_table_classes(observed_classes, expected_classes)) {
    UNPROTECT(5);
    return R_NilValue;
  }

  /*
   * `repr` content stays deliberately opaque -- it is print-only Domain
   * metadata that migration never interprets -- but its shape is not free:
   * Domain construction admits only an ordinary non-ALTREP/non-S4 carrier, so
   * capturing any other shape would produce migration output that no longer
   * builds. Enforce that one rule here, after the table receipts, so the
   * failure is a migration diagnostic rather than a later construction error.
   */
  if (attributes.repr != R_NilValue &&
      (ALTREP(attributes.repr) || Rf_isS4(attributes.repr))) {
    UNPROTECT(5);
    Rf_error(
      "legacy Domain `repr` metadata must be an ordinary non-ALTREP/non-S4 "
      "object"
    );
  }

  SET_VECTOR_ELT(result, 0, table);
  SET_VECTOR_ELT(result, 1, attributes.repr);
  UNPROTECT(5);
  return result;
}

static int exact_upgrade_values_parameter_vectors(
    SEXP ids, SEXP classes) {
  if (TYPEOF(ids) != STRSXP || ALTREP(ids) || Rf_isS4(ids) ||
      Rf_isObject(ids) || !paradox_api_has_no_attributes(ids) ||
      TYPEOF(classes) != STRSXP || ALTREP(classes) || Rf_isS4(classes) ||
      Rf_isObject(classes) || !paradox_api_has_no_attributes(classes) ||
      XLENGTH(ids) != XLENGTH(classes)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(ids); ++index) {
    SEXP id = STRING_ELT(ids, index);
    SEXP cls = STRING_ELT(classes, index);
    if (id == NA_STRING || cls == NA_STRING || CHAR(id)[0] == '\0' ||
        Rf_getCharCE(id) == CE_BYTES || Rf_getCharCE(cls) == CE_BYTES ||
        (strcmp(CHAR(cls), "ParamDbl") != 0 &&
          strcmp(CHAR(cls), "ParamInt") != 0 &&
          strcmp(CHAR(cls), "ParamFct") != 0 &&
          strcmp(CHAR(cls), "ParamLgl") != 0 &&
          strcmp(CHAR(cls), "ParamUty") != 0)) {
      return FALSE;
    }
    for (R_xlen_t prior = 0; prior < index; ++prior) {
      if (paradox_domain_strings_equal(id, STRING_ELT(ids, prior))) {
        return FALSE;
      }
    }
  }
  return TRUE;
}

static int exact_upgrade_values_shell(SEXP value) {
  static const char *const allowed[] = {"names"};
  if (TYPEOF(value) != VECSXP || ALTREP(value) || Rf_isS4(value) ||
      Rf_isObject(value) ||
      !paradox_api_has_only_attributes(value, allowed, 1)) {
    return FALSE;
  }
  SEXP names = paradox_api_raw_attribute(value, R_NamesSymbol);
  return names == R_NilValue
    ? XLENGTH(value) == 0
    : TYPEOF(names) == STRSXP && !ALTREP(names) && !Rf_isS4(names) &&
      !Rf_isObject(names) && paradox_api_has_no_attributes(names) &&
      XLENGTH(names) == XLENGTH(value);
}

static int upgrade_values_outer_is_current(SEXP source, SEXP captured) {
  if (!exact_upgrade_values_shell(source) ||
      !exact_upgrade_values_shell(captured) ||
      XLENGTH(source) != XLENGTH(captured)) {
    return FALSE;
  }
  SEXP source_names = paradox_api_raw_attribute(source, R_NamesSymbol);
  SEXP captured_names = paradox_api_raw_attribute(captured, R_NamesSymbol);
  if ((source_names == R_NilValue) != (captured_names == R_NilValue) ||
      (source_names != R_NilValue &&
        !paradox_ordinary_vector_payload_equal(
          source_names,
          captured_names
        ))) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(source); ++index) {
    if (VECTOR_ELT(source, index) != VECTOR_ELT(captured, index)) {
      return FALSE;
    }
  }
  return TRUE;
}

static int upgrade_value_owner_is_typed(SEXP id, SEXP ids,
    SEXP classes, int *typed) {
  R_xlen_t match = R_XLEN_T_MAX;
  for (R_xlen_t index = 0; index < XLENGTH(ids); ++index) {
    if (!paradox_domain_strings_equal(id, STRING_ELT(ids, index))) {
      continue;
    }
    if (match != R_XLEN_T_MAX) return FALSE;
    match = index;
  }
  if (match == R_XLEN_T_MAX) return FALSE;
  *typed = strcmp(CHAR(STRING_ELT(classes, match)), "ParamUty") != 0;
  return TRUE;
}

SEXP paradox_upgrade_values_snapshot(SEXP source,
    SEXP ids, SEXP classes) {
  PROTECT(source);
  PROTECT(ids);
  PROTECT(classes);
  if (!exact_upgrade_values_parameter_vectors(ids, classes) ||
      !exact_upgrade_values_shell(source)) {
    UNPROTECT(3);
    return R_NilValue;
  }

  SEXP captured = PROTECT(paradox_snapshot_semantic_vector(source));
  SEXP owned = PROTECT(paradox_snapshot_semantic_vector(captured));
  SEXP typed = PROTECT(Rf_allocVector(LGLSXP, XLENGTH(captured)));
  if (!upgrade_values_outer_is_current(source, captured)) {
    UNPROTECT(6);
    return R_NilValue;
  }
  SEXP names = paradox_api_raw_attribute(captured, R_NamesSymbol);
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t index = 0; index < XLENGTH(captured); ++index) {
    paradox_account_work(&work_since_interrupt);
    int typed_leaf = FALSE;
    if (!upgrade_value_owner_is_typed(
        STRING_ELT(names, index),
        ids,
        classes,
        &typed_leaf
      )) {
      UNPROTECT(6);
      /* The outer carrier is canonical, but this name cannot select a
       * parameter kind. Keep that semantic failure distinct from malformed
       * list structure so the R migration boundary can retain its useful
       * invalid-parameter-names diagnostic without attempting to own a leaf
       * under an invented kind. */
      return Rf_ScalarLogical(FALSE);
    }
    LOGICAL(typed)[index] = typed_leaf;
    SEXP detached = PROTECT(paradox_detach_stored_value_leaf(
      VECTOR_ELT(captured, index),
      typed_leaf
    ));
    SET_VECTOR_ELT(owned, index, detached);
    UNPROTECT(1);
  }

  /* Finish every nested stable-ALTREP Length before the callback-free source
   * generation and payload receipts. */
  if (!upgrade_values_outer_is_current(source, captured)) {
    UNPROTECT(6);
    return R_NilValue;
  }
  for (R_xlen_t index = 0; index < XLENGTH(captured); ++index) {
    if (LOGICAL(typed)[index] &&
        !upgrade_table_builtin_leaf_lengths_are_current(
          VECTOR_ELT(captured, index),
          VECTOR_ELT(owned, index)
        )) {
      UNPROTECT(6);
      return R_NilValue;
    }
  }
  if (!upgrade_values_outer_is_current(source, captured)) {
    UNPROTECT(6);
    return R_NilValue;
  }
  for (R_xlen_t index = 0; index < XLENGTH(captured); ++index) {
    SEXP source_leaf = VECTOR_ELT(captured, index);
    SEXP owned_leaf = VECTOR_ELT(owned, index);
    if (LOGICAL(typed)[index]
        ? !upgrade_table_leaf_is_current(
            source_leaf,
            owned_leaf,
            UPGRADE_TABLE_LEAF_BUILTIN,
            TRUE
          )
        : source_leaf != owned_leaf) {
      UNPROTECT(6);
      return R_NilValue;
    }
  }
  UNPROTECT(6);
  return owned;
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
    SEXP observed_class = R_NilValue;
    if (!paradox_api_ordinary_class_snapshot(owner, &observed_class) ||
        observed_class != expected_class ||
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

static int ordinary_class_value(SEXP classes) {
  if (classes == R_NilValue) return TRUE;
  if (TYPEOF(classes) != STRSXP ||
      ALTREP(classes) || Rf_isS4(classes) ||
      !paradox_api_has_no_attributes(classes)) {
    return FALSE;
  }
  const R_xlen_t count = XLENGTH(classes);
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP label = STRING_ELT(classes, index);
    if (label == NA_STRING || Rf_getCharCE(label) == CE_BYTES ||
        CHAR(label)[0] == '\0') {
      return FALSE;
    }
  }
  return TRUE;
}

static int candidate_classes(SEXP classes) {
  if (classes == R_NilValue || !ordinary_class_value(classes)) return FALSE;
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

#if R_VERSION < R_Version(4, 0, 0)
static int is_candidate_shell(SEXP environment) {
  SEXP classes;
  return paradox_api_ordinary_class_snapshot(environment, &classes) &&
    candidate_classes(classes);
}
#endif

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
  if (capacity > (size_t) R_XLEN_T_MAX) {
    Rf_error("Object graph boundary set is too large");
  }
  SEXP *items = temporary_size_alloc(capacity, sizeof(*items));
  memcpy(items, boundaries->items, boundaries->size * sizeof(*items));
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, (R_xlen_t) capacity));
  for (size_t index = 0; index < boundaries->size; ++index) {
    SET_VECTOR_ELT(
      roots,
      (R_xlen_t) index,
      VECTOR_ELT(boundaries->roots, (R_xlen_t) index)
    );
  }
  boundaries->roots = roots;
  REPROTECT(boundaries->roots, boundaries->roots_index);
  UNPROTECT(1);
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
  PROTECT(environment);
  if (boundaries->size == boundaries->capacity) {
    grow_boundaries(boundaries);
  }
  boundaries->items[boundaries->size] = environment;
  SET_VECTOR_ELT(
    boundaries->roots,
    (R_xlen_t) boundaries->size,
    environment
  );
  ++boundaries->size;
  UNPROTECT(1);
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
  int has_name = FALSE;
  if (!paradox_bounded_metadata_has_tag(
      environment,
      R_NameSymbol,
      &has_name
    ) || !has_name) {
    return FALSE;
  }
  SEXP name = PROTECT(paradox_api_raw_attribute(
    environment,
    R_NameSymbol
  ));
  if (TYPEOF(name) != STRSXP || ALTREP(name) || Rf_isS4(name) ||
      Rf_isObject(name) || !paradox_api_has_no_attributes(name) ||
      XLENGTH(name) != 1) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP label = STRING_ELT(name, 0);
  if (label == NA_STRING || Rf_getCharCE(label) == CE_BYTES ||
      strncmp(CHAR(label), "imports:", 8) != 0) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP parent = PROTECT(paradox_api_parent_environment(environment));
  int current_has_name = FALSE;
  const int imports = parent == R_BaseNamespace &&
    paradox_bounded_metadata_has_tag(
      environment,
      R_NameSymbol,
      &current_has_name
    ) &&
    current_has_name &&
    paradox_api_raw_attribute(environment, R_NameSymbol) == name;
  UNPROTECT(2);
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
  int has_name = FALSE;
  if (!paradox_bounded_metadata_has_tag(
      environment,
      R_NameSymbol,
      &has_name
    ) || !has_name) {
    return FALSE;
  }
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

#if R_VERSION >= R_Version(4, 5, 0) && \
    R_VERSION < R_Version(4, 6, 0)
static void fail_opaque_promise(const paradox_upgrade_path_t *path) {
  SEXP location = PROTECT(render_path(path));
  Rf_error(
    "Recursive Paradox object upgrade cannot inspect a promise on R 4.5 "
    "(at `%s`); load and upgrade this object under R 4.0--4.4 or R >= 4.6",
    CHAR(location)
  );
}
#endif

enum upgrade_edge_snapshot_slot {
  UPGRADE_EDGE_ATTRIBUTES = 0,
  UPGRADE_EDGE_PRIMARY,
  UPGRADE_EDGE_SLOT_COUNT
};

typedef struct {
  SEXP roots;
  R_xlen_t count;
  R_xlen_t capacity;
  int current;
} paradox_upgrade_attribute_capture_t;

static void capture_upgrade_attribute(SEXP tag, SEXP value, void *data) {
  paradox_upgrade_attribute_capture_t *capture = data;
  if (!capture->current || capture->count >= capture->capacity ||
      TYPEOF(tag) != SYMSXP || value == R_NilValue) {
    capture->current = FALSE;
    return;
  }
  const R_xlen_t offset = 2 * capture->count;
  SET_VECTOR_ELT(capture->roots, offset, tag);
  SET_VECTOR_ELT(capture->roots, offset + 1, value);
  ++capture->count;
}

static SEXP allocate_edge_snapshot(
    R_xlen_t attribute_count, R_xlen_t primary_count) {
  if (attribute_count < 0 ||
      attribute_count > R_XLEN_T_MAX / 2 ||
      primary_count < 0) {
    Rf_error("Object graph is too large to inspect");
  }
  SEXP snapshot = PROTECT(Rf_allocVector(VECSXP, UPGRADE_EDGE_SLOT_COUNT));
  SEXP attributes = PROTECT(Rf_allocVector(
    VECSXP,
    2 * attribute_count
  ));
  SET_VECTOR_ELT(snapshot, UPGRADE_EDGE_ATTRIBUTES, attributes);
  UNPROTECT(1);
  SEXP primary = PROTECT(Rf_allocVector(VECSXP, primary_count));
  SET_VECTOR_ELT(snapshot, UPGRADE_EDGE_PRIMARY, primary);
  UNPROTECT(2);
  return snapshot;
}

static R_xlen_t upgrade_attribute_count(SEXP node) {
  R_xlen_t count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
      node,
      PARADOX_UPGRADE_MAX_ATTRIBUTES,
      NULL,
      NULL,
      &count
    )) {
    Rf_error(
      "Object graph attribute set is malformed or too large to inspect"
    );
  }
  return count;
}

/*
 * Every destination already exists when this function starts. Raw stored
 * attribute enumeration performs no allocation or dispatch, so the captured
 * tag/value sequence belongs to the same selected node generation as the
 * callback-free primary-edge reads immediately beside it.
 */
static int capture_attributes_into(
    SEXP node, SEXP roots, R_xlen_t expected_count) {
  if (TYPEOF(roots) != VECSXP || ALTREP(roots) ||
      expected_count < 0 ||
      expected_count > PARADOX_UPGRADE_MAX_ATTRIBUTES ||
      XLENGTH(roots) != 2 * expected_count) {
    return FALSE;
  }
  paradox_upgrade_attribute_capture_t capture = {
    roots,
    0,
    expected_count,
    TRUE
  };
  R_xlen_t observed_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
    node,
    expected_count,
    capture_upgrade_attribute,
    &capture,
    &observed_count
  )) {
    return FALSE;
  }
  return capture.current && capture.count == expected_count &&
    observed_count == expected_count;
}

static int attribute_snapshots_equal(SEXP left, SEXP right) {
  if (TYPEOF(left) != VECSXP || ALTREP(left) ||
      TYPEOF(right) != VECSXP || ALTREP(right) ||
      XLENGTH(left) != XLENGTH(right)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(left); ++index) {
    if (VECTOR_ELT(left, index) != VECTOR_ELT(right, index)) return FALSE;
  }
  return TRUE;
}

#if R_VERSION < R_Version(4, 5, 0)
static int edge_snapshots_equal(SEXP left, SEXP right) {
  if (TYPEOF(left) != VECSXP || ALTREP(left) ||
      TYPEOF(right) != VECSXP || ALTREP(right) ||
      XLENGTH(left) != UPGRADE_EDGE_SLOT_COUNT ||
      XLENGTH(right) != UPGRADE_EDGE_SLOT_COUNT) {
    return FALSE;
  }
  SEXP left_attributes = VECTOR_ELT(left, UPGRADE_EDGE_ATTRIBUTES);
  SEXP right_attributes = VECTOR_ELT(right, UPGRADE_EDGE_ATTRIBUTES);
  SEXP left_primary = VECTOR_ELT(left, UPGRADE_EDGE_PRIMARY);
  SEXP right_primary = VECTOR_ELT(right, UPGRADE_EDGE_PRIMARY);
  if (!attribute_snapshots_equal(left_attributes, right_attributes) ||
      TYPEOF(left_primary) != VECSXP || ALTREP(left_primary) ||
      TYPEOF(right_primary) != VECSXP || ALTREP(right_primary) ||
      XLENGTH(left_primary) != XLENGTH(right_primary)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(left_primary); ++index) {
    if (VECTOR_ELT(left_primary, index) !=
        VECTOR_ELT(right_primary, index)) {
      return FALSE;
    }
  }
  return TRUE;
}
#endif

static void schedule_snapshot_attributes(
    paradox_upgrade_walker_t *walker,
    SEXP attributes,
    const paradox_upgrade_path_t *path) {
  if (TYPEOF(attributes) != VECSXP || ALTREP(attributes) ||
      XLENGTH(attributes) % 2 != 0) {
    Rf_error("Internal error: malformed object graph attribute snapshot");
  }
  const R_xlen_t count = XLENGTH(attributes) / 2;
  for (R_xlen_t index = count; index > 0; --index) {
    SEXP tag = VECTOR_ELT(attributes, 2 * (index - 1));
    SEXP value = VECTOR_ELT(attributes, 2 * (index - 1) + 1);
    const paradox_upgrade_path_t *attribute_path =
      TYPEOF(tag) == SYMSXP
        ? named_path(
            path,
            "@attr[[\"",
            PRINTNAME(tag),
            "\"]]"
          )
        : indexed_path(path, "@attributes[[", index - 1, "]]");
    schedule_node(walker, value, attribute_path);
  }
}

static void schedule_attributes_only(
    paradox_upgrade_walker_t *walker,
    SEXP node,
    const paradox_upgrade_path_t *path) {
  PROTECT(node);
  const R_xlen_t attribute_count =
    upgrade_attribute_count(node);
  SEXP snapshot = PROTECT(allocate_edge_snapshot(attribute_count, 0));
  SEXP attributes = VECTOR_ELT(snapshot, UPGRADE_EDGE_ATTRIBUTES);
  if (!capture_attributes_into(node, attributes, attribute_count)) {
    UNPROTECT(2);
    Rf_error("Object graph attributes changed during inspection");
  }
  schedule_snapshot_attributes(walker, attributes, path);
  UNPROTECT(2);
}

static void schedule_vector(
    paradox_upgrade_walker_t *walker,
    SEXP vector,
    const paradox_upgrade_path_t *path) {
  /*
   * Lists and expression vectors are graph structure, not semantic vectors.
   * Their ALTREP methods could evaluate arbitrary code while the crawler is
   * selecting edges, and duplicating an ALTREP shell does not make a
   * stateful provider coherent. `inspect_node()` rejects that boundary before
   * even scheduling attributes; keep this local guard so the helper cannot
   * accidentally regain an observing ALTREP path.
   */
  if (ALTREP(vector)) {
    Rf_error(
      "Object graph structural list/expression vectors must not use ALTREP; "
      "rebuild the container with an ordinary copy such as x[seq_along(x)] "
      "before migration"
    );
  }
  SEXP source = PROTECT(vector);
  const SEXPTYPE source_type = (SEXPTYPE) TYPEOF(source);
  const R_xlen_t count = XLENGTH(source);
  const R_xlen_t attribute_count =
    upgrade_attribute_count(source);
  /*
   * Path construction allocates. Allocate both attribute and child carriers
   * first, then capture the complete edge set in one callback-free pass. This
   * prevents a finalizer from pairing attributes from one list generation
   * with elements from another.
   */
  SEXP snapshot = PROTECT(allocate_edge_snapshot(
    attribute_count,
    count
  ));
  SEXP attributes = VECTOR_ELT(snapshot, UPGRADE_EDGE_ATTRIBUTES);
  SEXP children = VECTOR_ELT(snapshot, UPGRADE_EDGE_PRIMARY);
  if (ALTREP(source) || (SEXPTYPE) TYPEOF(source) != source_type ||
      XLENGTH(source) != count ||
      !capture_attributes_into(source, attributes, attribute_count)) {
    UNPROTECT(2);
    Rf_error("Object graph vector changed during inspection");
  }
  for (R_xlen_t index = 0; index < count; ++index) {
    SET_VECTOR_ELT(children, index, VECTOR_ELT(source, index));
  }
  schedule_snapshot_attributes(walker, attributes, path);
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

enum upgrade_closure_primary_slot {
  UPGRADE_CLOSURE_FORMALS = 0,
  UPGRADE_CLOSURE_EXPRESSION,
  UPGRADE_CLOSURE_ENVIRONMENT,
  UPGRADE_CLOSURE_PRIMARY_COUNT
};

static int capture_closure_edges(SEXP closure, SEXP snapshot) {
  SEXP attributes = VECTOR_ELT(snapshot, UPGRADE_EDGE_ATTRIBUTES);
  SEXP primary = VECTOR_ELT(snapshot, UPGRADE_EDGE_PRIMARY);
  const R_xlen_t attribute_count = XLENGTH(attributes) / 2;
  if (TYPEOF(closure) != CLOSXP ||
      TYPEOF(primary) != VECSXP || ALTREP(primary) ||
      XLENGTH(primary) != UPGRADE_CLOSURE_PRIMARY_COUNT ||
      !capture_attributes_into(closure, attributes, attribute_count)) {
    return FALSE;
  }
  SEXP formals = PROTECT(paradox_api_closure_formals(closure));
  SEXP expression = PROTECT(paradox_api_closure_expression(closure));
  SEXP environment = PROTECT(paradox_api_closure_environment(closure));
  SET_VECTOR_ELT(primary, UPGRADE_CLOSURE_FORMALS, formals);
  SET_VECTOR_ELT(primary, UPGRADE_CLOSURE_EXPRESSION, expression);
  SET_VECTOR_ELT(primary, UPGRADE_CLOSURE_ENVIRONMENT, environment);
  UNPROTECT(3);
  return TRUE;
}

static SEXP closure_edge_snapshot(SEXP closure) {
  const R_xlen_t attribute_count =
    upgrade_attribute_count(closure);
  SEXP snapshot = PROTECT(allocate_edge_snapshot(
    attribute_count,
    UPGRADE_CLOSURE_PRIMARY_COUNT
  ));
  if (!capture_closure_edges(closure, snapshot)) {
    UNPROTECT(1);
    Rf_error("Object graph closure changed during inspection");
  }
  UNPROTECT(1);
  return snapshot;
}

static void schedule_closure(
    paradox_upgrade_walker_t *walker,
    SEXP closure,
    const paradox_upgrade_path_t *path) {
  /*
   * Every supported runtime now reaches its closure fields through the
   * allocation-free compatibility facade.  Allocate the complete root carrier
   * first; capture_attributes_into() then proves the post-allocation attribute
   * generation and all three primary edges are selected beside it without a
   * callback or allocation.  This avoids both the old evaluating R bridge and
   * R's unbounded shallow attribute duplicator.
   */
  PROTECT(closure);
  SEXP second = PROTECT(closure_edge_snapshot(closure));
  SEXP attributes = VECTOR_ELT(second, UPGRADE_EDGE_ATTRIBUTES);
  SEXP primary = VECTOR_ELT(second, UPGRADE_EDGE_PRIMARY);
  SEXP formals = VECTOR_ELT(primary, UPGRADE_CLOSURE_FORMALS);
  SEXP expression = VECTOR_ELT(primary, UPGRADE_CLOSURE_EXPRESSION);
  SEXP environment = VECTOR_ELT(primary, UPGRADE_CLOSURE_ENVIRONMENT);
  schedule_snapshot_attributes(walker, attributes, path);
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
  UNPROTECT(2);
}

static void schedule_pairlist(
    paradox_upgrade_walker_t *walker,
    SEXP cell,
    const paradox_upgrade_path_t *path) {
  PROTECT(cell);
  const R_xlen_t attribute_count =
    upgrade_attribute_count(cell);
  SEXP snapshot = PROTECT(allocate_edge_snapshot(attribute_count, 3));
  SEXP attributes = VECTOR_ELT(snapshot, UPGRADE_EDGE_ATTRIBUTES);
  SEXP primary = VECTOR_ELT(snapshot, UPGRADE_EDGE_PRIMARY);
  if (!capture_attributes_into(cell, attributes, attribute_count)) {
    UNPROTECT(2);
    Rf_error("Object graph pairlist changed during inspection");
  }
  SET_VECTOR_ELT(primary, 0, CDR(cell));
  SET_VECTOR_ELT(primary, 1, TAG(cell));
  SET_VECTOR_ELT(primary, 2, CAR(cell));
  SEXP cdr = VECTOR_ELT(primary, 0);
  SEXP tag = VECTOR_ELT(primary, 1);
  SEXP car = VECTOR_ELT(primary, 2);
  schedule_snapshot_attributes(walker, attributes, path);
  const paradox_upgrade_path_t *cdr_path = literal_path(path, ".cdr");
  schedule_node(walker, cdr, cdr_path);
  const paradox_upgrade_path_t *tag_path = literal_path(path, ".tag");
  schedule_node(walker, tag, tag_path);
  const paradox_upgrade_path_t *car_path = literal_path(path, ".car");
  schedule_node(walker, car, car_path);
  UNPROTECT(2);
}

#if R_VERSION < R_Version(4, 5, 0)
enum upgrade_promise_primary_slot {
  UPGRADE_PROMISE_EXPRESSION = 0,
  UPGRADE_PROMISE_ENVIRONMENT,
  UPGRADE_PROMISE_VALUE,
  UPGRADE_PROMISE_PRIMARY_COUNT
};

static void schedule_promise_node(
    paradox_upgrade_walker_t *walker,
    SEXP promise,
    const paradox_upgrade_path_t *path) {
  PROTECT(promise);
  const R_xlen_t attribute_count =
    upgrade_attribute_count(promise);
  SEXP snapshot = PROTECT(allocate_edge_snapshot(
    attribute_count,
    UPGRADE_PROMISE_PRIMARY_COUNT
  ));
  SEXP attributes = VECTOR_ELT(snapshot, UPGRADE_EDGE_ATTRIBUTES);
  SEXP primary = VECTOR_ELT(snapshot, UPGRADE_EDGE_PRIMARY);
  paradox_api_promise_snapshot_t promise_snapshot;
  if (!capture_attributes_into(promise, attributes, attribute_count)) {
    UNPROTECT(2);
    Rf_error("Object graph promise changed during inspection");
  }
  paradox_api_promise_snapshot(promise, &promise_snapshot);
  SET_VECTOR_ELT(
    primary,
    UPGRADE_PROMISE_EXPRESSION,
    promise_snapshot.expression
  );
  SET_VECTOR_ELT(
    primary,
    UPGRADE_PROMISE_ENVIRONMENT,
    promise_snapshot.environment
  );
  SET_VECTOR_ELT(primary, UPGRADE_PROMISE_VALUE, promise_snapshot.value);

  schedule_snapshot_attributes(walker, attributes, path);
  if (promise_snapshot.forced) {
    schedule_node(
      walker,
      VECTOR_ELT(primary, UPGRADE_PROMISE_VALUE),
      literal_path(path, ".promise.value")
    );
  } else {
    schedule_node(
      walker,
      VECTOR_ELT(primary, UPGRADE_PROMISE_ENVIRONMENT),
      literal_path(path, ".promise.environment")
    );
  }
  schedule_node(
    walker,
    VECTOR_ELT(primary, UPGRADE_PROMISE_EXPRESSION),
    literal_path(path, ".promise.expression")
  );
  UNPROTECT(2);
}
#endif

static SEXP bytecode_edge_snapshot(SEXP bytecode) {
  const R_xlen_t attribute_count =
    upgrade_attribute_count(bytecode);
  SEXP snapshot = PROTECT(allocate_edge_snapshot(attribute_count, 1));
  SEXP attributes = VECTOR_ELT(snapshot, UPGRADE_EDGE_ATTRIBUTES);
  SEXP primary = VECTOR_ELT(snapshot, UPGRADE_EDGE_PRIMARY);
#if R_VERSION >= R_Version(4, 5, 0)
  if (!capture_attributes_into(bytecode, attributes, attribute_count)) {
    UNPROTECT(1);
    Rf_error("Object graph bytecode changed during inspection");
  }
  SET_VECTOR_ELT(
    primary,
    0,
    paradox_api_bytecode_expression(bytecode)
  );
#else
  /*
   * The old public bridge allocates while recovering the expression. A pair
   * of independently rooted complete observations below detects a finalizer
   * transition without adding another old-R internal accessor.
   */
  SEXP expression = PROTECT(paradox_api_bytecode_expression(bytecode));
  if (!capture_attributes_into(bytecode, attributes, attribute_count)) {
    UNPROTECT(2);
    Rf_error("Object graph bytecode changed during inspection");
  }
  SET_VECTOR_ELT(primary, 0, expression);
  UNPROTECT(1);
#endif
  UNPROTECT(1);
  return snapshot;
}

static void schedule_bytecode(
    paradox_upgrade_walker_t *walker,
    SEXP bytecode,
    const paradox_upgrade_path_t *path) {
  PROTECT(bytecode);
#if R_VERSION < R_Version(4, 5, 0)
  SEXP first = PROTECT(bytecode_edge_snapshot(bytecode));
  SEXP second = PROTECT(bytecode_edge_snapshot(bytecode));
  if (!edge_snapshots_equal(first, second)) {
    UNPROTECT(3);
    Rf_error("Object graph bytecode changed during inspection");
  }
#else
  SEXP second = PROTECT(bytecode_edge_snapshot(bytecode));
#endif
  SEXP attributes = VECTOR_ELT(second, UPGRADE_EDGE_ATTRIBUTES);
  SEXP primary = VECTOR_ELT(second, UPGRADE_EDGE_PRIMARY);
  schedule_snapshot_attributes(walker, attributes, path);
  schedule_node(
    walker,
    VECTOR_ELT(primary, 0),
    literal_path(path, ".expression")
  );
#if R_VERSION < R_Version(4, 5, 0)
  UNPROTECT(3);
#else
  UNPROTECT(2);
#endif
}

static void schedule_external_pointer(
    paradox_upgrade_walker_t *walker,
    SEXP pointer,
    const paradox_upgrade_path_t *path) {
  PROTECT(pointer);
  const R_xlen_t attribute_count =
    upgrade_attribute_count(pointer);
  SEXP snapshot = PROTECT(allocate_edge_snapshot(attribute_count, 1));
  SEXP attributes = VECTOR_ELT(snapshot, UPGRADE_EDGE_ATTRIBUTES);
  SEXP primary = VECTOR_ELT(snapshot, UPGRADE_EDGE_PRIMARY);
  if (!capture_attributes_into(pointer, attributes, attribute_count)) {
    UNPROTECT(2);
    Rf_error("Object graph external pointer changed during inspection");
  }
  SET_VECTOR_ELT(
    primary,
    0,
    paradox_core_is_canonical(pointer)
      ? R_ExternalPtrProtected(pointer)
      : R_NilValue
  );
  schedule_snapshot_attributes(walker, attributes, path);
  if (VECTOR_ELT(primary, 0) != R_NilValue) {
    schedule_node(
      walker,
      VECTOR_ELT(primary, 0),
      literal_path(path, ".protected")
    );
  }
  UNPROTECT(2);
}

enum upgrade_environment_snapshot_slot {
  UPGRADE_ENVIRONMENT_ATTRIBUTES = 0,
  UPGRADE_ENVIRONMENT_PARENT,
  UPGRADE_ENVIRONMENT_NAMES,
  UPGRADE_ENVIRONMENT_KINDS,
  UPGRADE_ENVIRONMENT_FIRST,
  UPGRADE_ENVIRONMENT_SECOND,
  UPGRADE_ENVIRONMENT_EXTRA,
  UPGRADE_ENVIRONMENT_BINDING_LOCKED,
  UPGRADE_ENVIRONMENT_FLAGS,
  UPGRADE_ENVIRONMENT_SLOT_COUNT
};

enum upgrade_environment_flag {
  UPGRADE_ENVIRONMENT_LOCKED = 0,
  UPGRADE_ENVIRONMENT_S4,
  UPGRADE_ENVIRONMENT_OBJECT,
  UPGRADE_ENVIRONMENT_FLAG_COUNT
};

enum upgrade_binding_kind {
  UPGRADE_BINDING_MISSING = 0,
  UPGRADE_BINDING_UNBOUND,
  UPGRADE_BINDING_VALUE,
  UPGRADE_BINDING_ACTIVE,
  UPGRADE_BINDING_DELAYED,
  UPGRADE_BINDING_FORCED,
  UPGRADE_BINDING_DOTS,
  UPGRADE_BINDING_SKIP_ENCLOSURE,
  UPGRADE_BINDING_SKIP_ACTIVE,
  UPGRADE_BINDING_SKIP_LOCKED_CLOSURE
};

enum upgrade_dots_snapshot_slot {
  UPGRADE_DOTS_KINDS = 0,
  UPGRADE_DOTS_FIRST,
  UPGRADE_DOTS_SECOND,
  UPGRADE_DOTS_SLOT_COUNT
};

static void set_snapshot_vector_slot(
    SEXP owner, R_xlen_t slot, SEXPTYPE type, R_xlen_t size) {
  SEXP value = PROTECT(Rf_allocVector(type, size));
  SET_VECTOR_ELT(owner, slot, value);
  UNPROTECT(1);
}

static SEXP allocate_environment_snapshot(
    SEXP names, R_xlen_t attribute_count) {
  if (TYPEOF(names) != STRSXP || ALTREP(names) ||
      Rf_isS4(names) || Rf_isObject(names) ||
      !paradox_api_has_no_attributes(names) ||
      attribute_count < 0 || attribute_count > R_XLEN_T_MAX / 2) {
    Rf_error("Object graph environment could not be snapshotted");
  }
  const R_xlen_t binding_count = XLENGTH(names);
  SEXP snapshot = PROTECT(Rf_allocVector(
    VECSXP,
    UPGRADE_ENVIRONMENT_SLOT_COUNT
  ));
  set_snapshot_vector_slot(
    snapshot,
    UPGRADE_ENVIRONMENT_ATTRIBUTES,
    VECSXP,
    2 * attribute_count
  );
  SET_VECTOR_ELT(snapshot, UPGRADE_ENVIRONMENT_NAMES, names);
  set_snapshot_vector_slot(
    snapshot,
    UPGRADE_ENVIRONMENT_KINDS,
    INTSXP,
    binding_count
  );
  set_snapshot_vector_slot(
    snapshot,
    UPGRADE_ENVIRONMENT_FIRST,
    VECSXP,
    binding_count
  );
  set_snapshot_vector_slot(
    snapshot,
    UPGRADE_ENVIRONMENT_SECOND,
    VECSXP,
    binding_count
  );
  set_snapshot_vector_slot(
    snapshot,
    UPGRADE_ENVIRONMENT_EXTRA,
    VECSXP,
    binding_count
  );
  set_snapshot_vector_slot(
    snapshot,
    UPGRADE_ENVIRONMENT_BINDING_LOCKED,
    LGLSXP,
    binding_count
  );
  set_snapshot_vector_slot(
    snapshot,
    UPGRADE_ENVIRONMENT_FLAGS,
    LGLSXP,
    UPGRADE_ENVIRONMENT_FLAG_COUNT
  );
  UNPROTECT(1);
  return snapshot;
}

static void active_binding_inspection_error(
    const paradox_upgrade_path_t *path) {
  SEXP location = PROTECT(render_path(path));
  Rf_error(
    "Recursive Paradox object upgrade cannot inspect an active binding "
    "on R 3.6 (at `%s`); load and upgrade this object under R >= 4.0",
    CHAR(location)
  );
}

#if R_VERSION >= R_Version(4, 6, 0)
static SEXP capture_dots_snapshot(SEXP environment) {
  const int count = R_DotsLength(environment);
  if (count < 0) {
    Rf_error("Object graph dots binding changed during inspection");
  }
  SEXP snapshot = PROTECT(Rf_allocVector(VECSXP, UPGRADE_DOTS_SLOT_COUNT));
  set_snapshot_vector_slot(
    snapshot,
    UPGRADE_DOTS_KINDS,
    INTSXP,
    (R_xlen_t) count
  );
  set_snapshot_vector_slot(
    snapshot,
    UPGRADE_DOTS_FIRST,
    VECSXP,
    (R_xlen_t) count
  );
  set_snapshot_vector_slot(
    snapshot,
    UPGRADE_DOTS_SECOND,
    VECSXP,
    (R_xlen_t) count
  );
  if (!R_DotsExist(environment) || R_DotsLength(environment) != count) {
    UNPROTECT(1);
    Rf_error("Object graph dots binding changed during inspection");
  }

  SEXP kinds = VECTOR_ELT(snapshot, UPGRADE_DOTS_KINDS);
  SEXP first = VECTOR_ELT(snapshot, UPGRADE_DOTS_FIRST);
  SEXP second = VECTOR_ELT(snapshot, UPGRADE_DOTS_SECOND);
  for (int index = 1; index <= count; ++index) {
    switch (R_GetDotType(index, environment)) {
    case R_DotTypeValue: {
      SEXP value = PROTECT(R_DotsElt(index, environment));
      INTEGER(kinds)[index - 1] = UPGRADE_BINDING_VALUE;
      SET_VECTOR_ELT(first, index - 1, value);
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
      INTEGER(kinds)[index - 1] = UPGRADE_BINDING_DELAYED;
      SET_VECTOR_ELT(first, index - 1, expression);
      SET_VECTOR_ELT(second, index - 1, evaluation_environment);
      UNPROTECT(2);
      break;
    }
    case R_DotTypeForced: {
      SEXP expression = PROTECT(R_DotForcedExpression(
        index,
        environment
      ));
      /* The type classifier proves that R_DotsElt() cannot force this cell. */
      SEXP value = PROTECT(R_DotsElt(index, environment));
      INTEGER(kinds)[index - 1] = UPGRADE_BINDING_FORCED;
      SET_VECTOR_ELT(first, index - 1, expression);
      SET_VECTOR_ELT(second, index - 1, value);
      UNPROTECT(2);
      break;
    }
    case R_DotTypeMissing:
      INTEGER(kinds)[index - 1] = UPGRADE_BINDING_MISSING;
      break;
    default:
      UNPROTECT(1);
      Rf_error("Internal error: unknown R dots binding type");
    }
  }
  UNPROTECT(1);
  return snapshot;
}
#endif

static void capture_environment_binding(
    SEXP environment,
    SEXP symbol,
    R_xlen_t index,
    SEXP snapshot,
    const paradox_upgrade_path_t *path,
    int current_builtin,
    SEXP enclosure_symbol) {
  SEXP kinds = VECTOR_ELT(snapshot, UPGRADE_ENVIRONMENT_KINDS);
  SEXP first = VECTOR_ELT(snapshot, UPGRADE_ENVIRONMENT_FIRST);
#if R_VERSION < R_Version(4, 5, 0) || \
    R_VERSION >= R_Version(4, 6, 0)
  SEXP second = VECTOR_ELT(snapshot, UPGRADE_ENVIRONMENT_SECOND);
  SEXP extra = VECTOR_ELT(snapshot, UPGRADE_ENVIRONMENT_EXTRA);
#endif
  SEXP locked = VECTOR_ELT(
    snapshot,
    UPGRADE_ENVIRONMENT_BINDING_LOCKED
  );
  LOGICAL(locked)[index] =
    R_BindingIsLocked(symbol, environment) != FALSE;

#if R_VERSION < R_Version(4, 0, 0)
  if (current_builtin && symbol == enclosure_symbol) {
    if (R_BindingIsActive(symbol, environment)) {
      INTEGER(kinds)[index] = UPGRADE_BINDING_SKIP_ACTIVE;
    } else {
      SEXP value = PROTECT(paradox_api_stored_binding_snapshot(
        environment,
        symbol
      ));
      INTEGER(kinds)[index] = UPGRADE_BINDING_SKIP_ENCLOSURE;
      SET_VECTOR_ELT(first, index, value);
      UNPROTECT(1);
    }
    return;
  }
#else
  (void) current_builtin;
  (void) enclosure_symbol;
#endif

  if (R_BindingIsActive(symbol, environment)) {
#if R_VERSION < R_Version(4, 0, 0)
    if (current_builtin) {
      INTEGER(kinds)[index] = UPGRADE_BINDING_SKIP_ACTIVE;
      return;
    }
#endif
    SEXP function = PROTECT(paradox_api_active_binding_function(
      environment,
      symbol
    ));
    if (function == R_UnboundValue) {
      UNPROTECT(1);
      active_binding_inspection_error(path);
    }
    INTEGER(kinds)[index] = UPGRADE_BINDING_ACTIVE;
    SET_VECTOR_ELT(first, index, function);
    UNPROTECT(1);
    return;
  }

#if R_VERSION >= R_Version(4, 6, 0)
  if (symbol == R_DotsSymbol && R_DotsExist(environment)) {
    SEXP dots = PROTECT(capture_dots_snapshot(environment));
    INTEGER(kinds)[index] = UPGRADE_BINDING_DOTS;
    SET_VECTOR_ELT(extra, index, dots);
    UNPROTECT(1);
    return;
  }

  switch (R_GetBindingType(symbol, environment)) {
  case R_BindingTypeValue: {
    SEXP value = PROTECT(R_getVar(symbol, environment, FALSE));
    INTEGER(kinds)[index] = UPGRADE_BINDING_VALUE;
    SET_VECTOR_ELT(first, index, value);
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
    INTEGER(kinds)[index] = UPGRADE_BINDING_DELAYED;
    SET_VECTOR_ELT(first, index, expression);
    SET_VECTOR_ELT(second, index, evaluation_environment);
    UNPROTECT(2);
    return;
  }
  case R_BindingTypeForced: {
    SEXP expression = PROTECT(R_ForcedBindingExpression(
      symbol,
      environment
    ));
    SEXP value = PROTECT(R_getVar(symbol, environment, FALSE));
    INTEGER(kinds)[index] = UPGRADE_BINDING_FORCED;
    SET_VECTOR_ELT(first, index, expression);
    SET_VECTOR_ELT(second, index, value);
    UNPROTECT(2);
    return;
  }
  case R_BindingTypeUnbound:
    INTEGER(kinds)[index] = UPGRADE_BINDING_UNBOUND;
    return;
  case R_BindingTypeMissing:
    INTEGER(kinds)[index] = UPGRADE_BINDING_MISSING;
    return;
  case R_BindingTypeActive:
    Rf_error("Object graph binding changed during inspection");
  }
  Rf_error("Internal error: unknown R binding type");
#else
  SEXP value = PROTECT(paradox_api_stored_binding_snapshot(
    environment,
    symbol
  ));
  if (value == R_UnboundValue) {
    INTEGER(kinds)[index] = UPGRADE_BINDING_UNBOUND;
    UNPROTECT(1);
    return;
  }
  if (value == R_MissingArg) {
    INTEGER(kinds)[index] = UPGRADE_BINDING_MISSING;
    UNPROTECT(1);
    return;
  }
#if R_VERSION < R_Version(4, 0, 0)
  if (current_builtin && TYPEOF(value) == CLOSXP &&
      R_BindingIsLocked(symbol, environment)) {
    INTEGER(kinds)[index] = UPGRADE_BINDING_SKIP_LOCKED_CLOSURE;
    SET_VECTOR_ELT(first, index, value);
    UNPROTECT(1);
    return;
  }
#endif
  if (TYPEOF(value) != PROMSXP) {
    INTEGER(kinds)[index] = UPGRADE_BINDING_VALUE;
    SET_VECTOR_ELT(first, index, value);
    UNPROTECT(1);
    return;
  }
#if R_VERSION < R_Version(4, 5, 0)
  paradox_api_promise_snapshot_t promise;
  paradox_api_promise_snapshot(value, &promise);
  INTEGER(kinds)[index] = promise.forced
    ? UPGRADE_BINDING_FORCED
    : UPGRADE_BINDING_DELAYED;
  SET_VECTOR_ELT(first, index, promise.expression);
  SET_VECTOR_ELT(
    second,
    index,
    promise.forced ? promise.value : promise.environment
  );
  /* Promise identity is part of the exact old-runtime binding receipt. */
  SET_VECTOR_ELT(extra, index, value);
  UNPROTECT(1);
  return;
#else
  UNPROTECT(1);
  fail_opaque_promise(path);
#endif
#endif
}

static SEXP capture_environment_snapshot(
    SEXP environment,
    const paradox_upgrade_path_t *path,
    int current_builtin) {
  SEXP names = PROTECT(environment_names(environment));
  const R_xlen_t attribute_count =
    upgrade_attribute_count(environment);
  SEXP snapshot = PROTECT(allocate_environment_snapshot(
    names,
    attribute_count
  ));
  SEXP parent = PROTECT(paradox_api_parent_environment(environment));
  SET_VECTOR_ELT(snapshot, UPGRADE_ENVIRONMENT_PARENT, parent);
  UNPROTECT(1);

  SEXP enclosure_symbol = Rf_install(".__enclos_env__");
  const R_xlen_t binding_count = XLENGTH(names);
  for (R_xlen_t index = 0; index < binding_count; ++index) {
    SEXP name = STRING_ELT(names, index);
    if (name == NA_STRING) {
      UNPROTECT(2);
      Rf_error("Internal error: missing environment binding name");
    }
    SEXP symbol = Rf_installChar(name);
    const paradox_upgrade_path_t *binding_path =
      named_path(path, "[[\"", name, "\"]]");
    capture_environment_binding(
      environment,
      symbol,
      index,
      snapshot,
      binding_path,
      current_builtin,
      enclosure_symbol
    );
  }

  SEXP attributes = VECTOR_ELT(
    snapshot,
    UPGRADE_ENVIRONMENT_ATTRIBUTES
  );
  SEXP flags = VECTOR_ELT(snapshot, UPGRADE_ENVIRONMENT_FLAGS);
  if (!capture_attributes_into(
      environment,
      attributes,
      attribute_count
    )) {
    UNPROTECT(2);
    Rf_error("Object graph environment changed during inspection");
  }
  LOGICAL(flags)[UPGRADE_ENVIRONMENT_LOCKED] =
    R_EnvironmentIsLocked(environment) != FALSE;
  LOGICAL(flags)[UPGRADE_ENVIRONMENT_S4] =
    Rf_isS4(environment) != FALSE;
  LOGICAL(flags)[UPGRADE_ENVIRONMENT_OBJECT] =
    Rf_isObject(environment) != FALSE;

  UNPROTECT(2);
  return snapshot;
}

static int exact_snapshot_integer(SEXP left, SEXP right) {
  if (TYPEOF(left) != INTSXP || ALTREP(left) ||
      TYPEOF(right) != INTSXP || ALTREP(right) ||
      XLENGTH(left) != XLENGTH(right)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(left); ++index) {
    if (INTEGER(left)[index] != INTEGER(right)[index]) return FALSE;
  }
  return TRUE;
}

static int exact_snapshot_logical(SEXP left, SEXP right) {
  if (TYPEOF(left) != LGLSXP || ALTREP(left) ||
      TYPEOF(right) != LGLSXP || ALTREP(right) ||
      XLENGTH(left) != XLENGTH(right)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(left); ++index) {
    if (LOGICAL(left)[index] != LOGICAL(right)[index]) return FALSE;
  }
  return TRUE;
}

static int exact_snapshot_list(SEXP left, SEXP right) {
  if (TYPEOF(left) != VECSXP || ALTREP(left) ||
      TYPEOF(right) != VECSXP || ALTREP(right) ||
      XLENGTH(left) != XLENGTH(right)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(left); ++index) {
    if (VECTOR_ELT(left, index) != VECTOR_ELT(right, index)) return FALSE;
  }
  return TRUE;
}

static int exact_snapshot_names(SEXP left, SEXP right) {
  if (TYPEOF(left) != STRSXP || ALTREP(left) ||
      TYPEOF(right) != STRSXP || ALTREP(right) ||
      XLENGTH(left) != XLENGTH(right)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(left); ++index) {
    if (STRING_ELT(left, index) != STRING_ELT(right, index)) return FALSE;
  }
  return TRUE;
}

static int dots_snapshots_equal(SEXP left, SEXP right) {
  if (TYPEOF(left) != VECSXP || ALTREP(left) ||
      TYPEOF(right) != VECSXP || ALTREP(right) ||
      XLENGTH(left) != UPGRADE_DOTS_SLOT_COUNT ||
      XLENGTH(right) != UPGRADE_DOTS_SLOT_COUNT) {
    return FALSE;
  }
  return exact_snapshot_integer(
      VECTOR_ELT(left, UPGRADE_DOTS_KINDS),
      VECTOR_ELT(right, UPGRADE_DOTS_KINDS)
    ) && exact_snapshot_list(
      VECTOR_ELT(left, UPGRADE_DOTS_FIRST),
      VECTOR_ELT(right, UPGRADE_DOTS_FIRST)
    ) && exact_snapshot_list(
      VECTOR_ELT(left, UPGRADE_DOTS_SECOND),
      VECTOR_ELT(right, UPGRADE_DOTS_SECOND)
    );
}

static int environment_snapshots_equal(SEXP left, SEXP right) {
  if (TYPEOF(left) != VECSXP || ALTREP(left) ||
      TYPEOF(right) != VECSXP || ALTREP(right) ||
      XLENGTH(left) != UPGRADE_ENVIRONMENT_SLOT_COUNT ||
      XLENGTH(right) != UPGRADE_ENVIRONMENT_SLOT_COUNT ||
      !attribute_snapshots_equal(
        VECTOR_ELT(left, UPGRADE_ENVIRONMENT_ATTRIBUTES),
        VECTOR_ELT(right, UPGRADE_ENVIRONMENT_ATTRIBUTES)
      ) ||
      VECTOR_ELT(left, UPGRADE_ENVIRONMENT_PARENT) !=
        VECTOR_ELT(right, UPGRADE_ENVIRONMENT_PARENT) ||
      !exact_snapshot_names(
        VECTOR_ELT(left, UPGRADE_ENVIRONMENT_NAMES),
        VECTOR_ELT(right, UPGRADE_ENVIRONMENT_NAMES)
      ) ||
      !exact_snapshot_integer(
        VECTOR_ELT(left, UPGRADE_ENVIRONMENT_KINDS),
        VECTOR_ELT(right, UPGRADE_ENVIRONMENT_KINDS)
      ) ||
      !exact_snapshot_list(
        VECTOR_ELT(left, UPGRADE_ENVIRONMENT_FIRST),
        VECTOR_ELT(right, UPGRADE_ENVIRONMENT_FIRST)
      ) ||
      !exact_snapshot_list(
        VECTOR_ELT(left, UPGRADE_ENVIRONMENT_SECOND),
        VECTOR_ELT(right, UPGRADE_ENVIRONMENT_SECOND)
      ) ||
      !exact_snapshot_logical(
        VECTOR_ELT(left, UPGRADE_ENVIRONMENT_BINDING_LOCKED),
        VECTOR_ELT(right, UPGRADE_ENVIRONMENT_BINDING_LOCKED)
      ) ||
      !exact_snapshot_logical(
        VECTOR_ELT(left, UPGRADE_ENVIRONMENT_FLAGS),
        VECTOR_ELT(right, UPGRADE_ENVIRONMENT_FLAGS)
      )) {
    return FALSE;
  }

  SEXP kinds = VECTOR_ELT(left, UPGRADE_ENVIRONMENT_KINDS);
  SEXP left_extra = VECTOR_ELT(left, UPGRADE_ENVIRONMENT_EXTRA);
  SEXP right_extra = VECTOR_ELT(right, UPGRADE_ENVIRONMENT_EXTRA);
  if (TYPEOF(left_extra) != VECSXP || ALTREP(left_extra) ||
      TYPEOF(right_extra) != VECSXP || ALTREP(right_extra) ||
      XLENGTH(left_extra) != XLENGTH(kinds) ||
      XLENGTH(right_extra) != XLENGTH(kinds)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(kinds); ++index) {
    SEXP left_value = VECTOR_ELT(left_extra, index);
    SEXP right_value = VECTOR_ELT(right_extra, index);
    if (INTEGER(kinds)[index] == UPGRADE_BINDING_DOTS) {
      if (!dots_snapshots_equal(left_value, right_value)) return FALSE;
    } else if (left_value != right_value) {
      return FALSE;
    }
  }
  return TRUE;
}

static SEXP snapshot_attribute_value(SEXP attributes, SEXP tag) {
  if (TYPEOF(attributes) != VECSXP || ALTREP(attributes) ||
      XLENGTH(attributes) % 2 != 0) {
    return R_UnboundValue;
  }
  for (R_xlen_t index = 0; index < XLENGTH(attributes); index += 2) {
    if (VECTOR_ELT(attributes, index) == tag) {
      return VECTOR_ELT(attributes, index + 1);
    }
  }
  return R_NilValue;
}

static int environment_snapshot_is_candidate(SEXP snapshot) {
  SEXP classes = snapshot_attribute_value(
    VECTOR_ELT(snapshot, UPGRADE_ENVIRONMENT_ATTRIBUTES),
    R_ClassSymbol
  );
  return classes != R_UnboundValue && candidate_classes(classes);
}

static int environment_snapshot_has_name_prefix(
    SEXP snapshot, const char *prefix, int require_suffix) {
  SEXP name = snapshot_attribute_value(
    VECTOR_ELT(snapshot, UPGRADE_ENVIRONMENT_ATTRIBUTES),
    R_NameSymbol
  );
  if (name == R_UnboundValue || TYPEOF(name) != STRSXP ||
      ALTREP(name) || Rf_isS4(name) || Rf_isObject(name) ||
      !paradox_api_has_no_attributes(name) || XLENGTH(name) != 1) {
    return FALSE;
  }
  SEXP label = STRING_ELT(name, 0);
  if (label == NA_STRING || Rf_getCharCE(label) == CE_BYTES) return FALSE;
  const size_t prefix_size = strlen(prefix);
  return strncmp(CHAR(label), prefix, prefix_size) == 0 &&
    (!require_suffix || CHAR(label)[prefix_size] != '\0');
}

/*
 * `environment_boundary()` owns identities and namespace registry
 * authentication before any environment observation. Package/import/user
 * database boundaries additionally depend only on fields already retained in
 * the selected complete snapshot. Recheck those fields without allocating so
 * a finalizer cannot make the crawler apply an old non-boundary decision to a
 * new boundary generation.
 */
static int environment_snapshot_is_boundary(SEXP snapshot) {
  SEXP attributes = VECTOR_ELT(
    snapshot,
    UPGRADE_ENVIRONMENT_ATTRIBUTES
  );
  SEXP flags = VECTOR_ELT(snapshot, UPGRADE_ENVIRONMENT_FLAGS);
  if (LOGICAL(flags)[UPGRADE_ENVIRONMENT_OBJECT] != FALSE) {
    SEXP classes = snapshot_attribute_value(attributes, R_ClassSymbol);
    if (classes == R_UnboundValue || !ordinary_class_value(classes)) {
      return TRUE;
    }
    if (paradox_api_ordinary_class_contains(
        classes,
        "UserDefinedDatabase"
      )) {
      return TRUE;
    }
  }
  if (environment_snapshot_has_name_prefix(
      snapshot,
      "package:",
      TRUE
    )) {
    return TRUE;
  }
  return environment_snapshot_has_name_prefix(
      snapshot,
      "imports:",
      FALSE
    ) && VECTOR_ELT(snapshot, UPGRADE_ENVIRONMENT_PARENT) ==
      R_BaseNamespace;
}

static void schedule_binding_edge(
    paradox_upgrade_walker_t *walker,
    int kind,
    SEXP first,
    SEXP second,
    const paradox_upgrade_path_t *path);

static void schedule_dots_edges(
    paradox_upgrade_walker_t *walker,
    SEXP snapshot,
    const paradox_upgrade_path_t *path) {
  SEXP kinds = VECTOR_ELT(snapshot, UPGRADE_DOTS_KINDS);
  SEXP first = VECTOR_ELT(snapshot, UPGRADE_DOTS_FIRST);
  SEXP second = VECTOR_ELT(snapshot, UPGRADE_DOTS_SECOND);
  for (R_xlen_t index = XLENGTH(kinds); index > 0; --index) {
    const paradox_upgrade_path_t *element_path = indexed_path(
      path,
      "[[",
      index - 1,
      "]]"
    );
    schedule_binding_edge(
      walker,
      INTEGER(kinds)[index - 1],
      VECTOR_ELT(first, index - 1),
      VECTOR_ELT(second, index - 1),
      element_path
    );
  }
}

static void schedule_binding_edge(
    paradox_upgrade_walker_t *walker,
    int kind,
    SEXP first,
    SEXP second,
    const paradox_upgrade_path_t *path) {
  switch (kind) {
  case UPGRADE_BINDING_VALUE:
    schedule_node(walker, first, path);
    return;
  case UPGRADE_BINDING_ACTIVE:
    schedule_node(
      walker,
      first,
      literal_path(path, ".active")
    );
    return;
  case UPGRADE_BINDING_DELAYED:
    schedule_node(
      walker,
      second,
      literal_path(path, ".promise.environment")
    );
    schedule_node(
      walker,
      first,
      literal_path(path, ".promise.expression")
    );
    return;
  case UPGRADE_BINDING_FORCED:
    schedule_node(
      walker,
      second,
      literal_path(path, ".promise.value")
    );
    schedule_node(
      walker,
      first,
      literal_path(path, ".promise.expression")
    );
    return;
  case UPGRADE_BINDING_MISSING:
  case UPGRADE_BINDING_UNBOUND:
  case UPGRADE_BINDING_SKIP_ENCLOSURE:
  case UPGRADE_BINDING_SKIP_ACTIVE:
  case UPGRADE_BINDING_SKIP_LOCKED_CLOSURE:
    return;
  case UPGRADE_BINDING_DOTS:
    Rf_error("Internal error: nested dots graph snapshot");
  }
  Rf_error("Internal error: unknown object graph binding snapshot");
}

static void schedule_environment(
    paradox_upgrade_walker_t *walker,
    SEXP environment,
    const paradox_upgrade_path_t *path) {
  if (environment_boundary(walker, environment)) return;
  PROTECT(environment);

  int current_builtin = FALSE;
#if R_VERSION < R_Version(4, 0, 0)
  SEXP selected_core = R_NilValue;
  const int preliminary_candidate = is_candidate_shell(environment);
  int core_protected = FALSE;
  if (preliminary_candidate) {
    selected_core = PROTECT(paradox_builtin_current_core_snapshot(
      environment
    ));
    core_protected = TRUE;
    current_builtin = selected_core != R_UnboundValue;
  }
#endif

  SEXP first_snapshot = PROTECT(capture_environment_snapshot(
    environment,
    path,
    current_builtin
  ));
  SEXP second_snapshot = PROTECT(capture_environment_snapshot(
    environment,
    path,
    current_builtin
  ));
  if (!environment_snapshots_equal(first_snapshot, second_snapshot)) {
#if R_VERSION < R_Version(4, 0, 0)
    UNPROTECT(3 + core_protected);
#else
    UNPROTECT(3);
#endif
    Rf_error("Object graph environment changed during inspection");
  }

  const int selected_candidate =
    environment_snapshot_is_candidate(second_snapshot);
#if R_VERSION < R_Version(4, 0, 0)
  if (selected_candidate != preliminary_candidate) {
    UNPROTECT(3 + core_protected);
    Rf_error("Object graph environment changed during inspection");
  }
  if (current_builtin) {
    SEXP terminal_core = PROTECT(paradox_builtin_current_core_snapshot(
      environment
    ));
    if (terminal_core != selected_core) {
      UNPROTECT(4 + core_protected);
      Rf_error("Object graph environment changed during inspection");
    }
    UNPROTECT(1);
  }
#endif

  if (environment_snapshot_is_boundary(second_snapshot)) {
#if R_VERSION < R_Version(4, 0, 0)
    UNPROTECT(3 + core_protected);
#else
    UNPROTECT(3);
#endif
    return;
  }

  SEXP attributes = VECTOR_ELT(
    second_snapshot,
    UPGRADE_ENVIRONMENT_ATTRIBUTES
  );
  schedule_snapshot_attributes(walker, attributes, path);
  if (selected_candidate) {
    append_candidate(&walker->candidates, environment, path);
#if R_VERSION < R_Version(4, 0, 0)
    if (current_builtin) {
      schedule_node(
        walker,
        selected_core,
        literal_path(path, ".core")
      );
    }
#endif
  }

  schedule_node(
    walker,
    VECTOR_ELT(second_snapshot, UPGRADE_ENVIRONMENT_PARENT),
    literal_path(path, ".parent")
  );
  SEXP names = VECTOR_ELT(second_snapshot, UPGRADE_ENVIRONMENT_NAMES);
  SEXP kinds = VECTOR_ELT(second_snapshot, UPGRADE_ENVIRONMENT_KINDS);
  SEXP first = VECTOR_ELT(second_snapshot, UPGRADE_ENVIRONMENT_FIRST);
  SEXP second = VECTOR_ELT(second_snapshot, UPGRADE_ENVIRONMENT_SECOND);
  SEXP extra = VECTOR_ELT(second_snapshot, UPGRADE_ENVIRONMENT_EXTRA);
  for (R_xlen_t index = XLENGTH(names); index > 0; --index) {
    SEXP name = STRING_ELT(names, index - 1);
    const paradox_upgrade_path_t *binding_path =
      named_path(path, "[[\"", name, "\"]]");
    const int kind = INTEGER(kinds)[index - 1];
    if (kind == UPGRADE_BINDING_DOTS) {
      schedule_dots_edges(
        walker,
        VECTOR_ELT(extra, index - 1),
        binding_path
      );
    } else {
      schedule_binding_edge(
        walker,
        kind,
        VECTOR_ELT(first, index - 1),
        VECTOR_ELT(second, index - 1),
        binding_path
      );
    }
  }

#if R_VERSION < R_Version(4, 0, 0)
  UNPROTECT(3 + core_protected);
#else
  UNPROTECT(3);
#endif
}

static void inspect_node(
    paradox_upgrade_walker_t *walker,
    paradox_upgrade_work_t work) {
  SEXP node = work.node;
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(node);
  /*
   * A base `names<-` wrapper and a hostile stateful provider are the same
   * structural list ALTREP here: telling them apart, or unwrapping either one,
   * requires observing the object through non-API methods that may evaluate R
   * code while the crawler is selecting edges. The rejection therefore stays
   * unconditional, and the R migration boundary materializes the one carrier
   * it owns -- the caller-supplied top-level container -- before this walk
   * begins. The remedy for every other carrier belongs in the message.
   */
  if ((type == VECSXP || type == EXPRSXP) && ALTREP(node)) {
    Rf_error(
      "Object graph structural list/expression vectors must not use ALTREP; "
      "rebuild the container with an ordinary copy such as x[seq_along(x)] "
      "before migration"
    );
  }

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
    schedule_promise_node(walker, node, work.path);
#elif R_VERSION < R_Version(4, 6, 0)
    fail_opaque_promise(work.path);
#else
    schedule_attributes_only(walker, node, work.path);
#endif
    return;
  case BCODESXP:
    schedule_bytecode(walker, node, work.path);
    return;
  case EXTPTRSXP:
    schedule_external_pointer(walker, node, work.path);
    return;
  case WEAKREFSXP:
    schedule_attributes_only(walker, node, work.path);
    return;
  default:
    schedule_attributes_only(walker, node, work.path);
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

static SEXP upgrade_graph_discover_with_boundary_hook(
    SEXP root, SEXP boundary_hook) {
  PROTECT(root);
  PROTECT(boundary_hook);
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
  PROTECT_WITH_INDEX(
    walker.boundaries.roots = Rf_allocVector(
      VECSXP,
      (R_xlen_t) walker.boundaries.capacity
    ),
    &walker.boundaries.roots_index
  );
  initialize_search_boundaries(&walker.boundaries);
  if (boundary_hook != R_NilValue) {
    SEXP call = PROTECT(Rf_lang1(boundary_hook));
    SEXP hook_result = PROTECT(Rf_eval(call, R_BaseEnv));
    (void) hook_result;
    UNPROTECT(2);
  }

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
  UNPROTECT(6);
  return result;
}

SEXP paradox_upgrade_graph_discover(SEXP root) {
  return upgrade_graph_discover_with_boundary_hook(root, R_NilValue);
}

SEXP paradox_test_upgrade_graph_boundary_lifetime(SEXP root, SEXP hook) {
  if (!Rf_isFunction(hook)) {
    Rf_error("Boundary lifetime test hook must be a function");
  }
  return upgrade_graph_discover_with_boundary_hook(root, hook);
}
