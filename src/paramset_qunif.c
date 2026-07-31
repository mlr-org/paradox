#include <math.h>
#include <limits.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Arith.h>
#include <R_ext/Utils.h>

#include "builtin_condition.h"
#include "core_state.h"
#include "dependency_graph.h"
#include "generation_receipt.h"
#include "paramset_collection_readers.h"
#include "paramset_domain_common.h"
#include "paramset_shadow.h"
#include "r_api_compat.h"
#include "r_utils.h"

enum qunif_column_root {
  QUNIF_ROOT_COLUMNS = 0,
  QUNIF_ROOT_COUNT = PARADOX_DOMAIN_TAGS
};

enum qunif_input_root {
  QUNIF_INPUT_NAMES = 0,
  QUNIF_INPUT_VALUES,
  QUNIF_INPUT_SOURCE,
  QUNIF_INPUT_ROOT_COUNT
};

enum qunif_operation_root {
  QUNIF_OPERATION_CORE = 0,
  QUNIF_OPERATION_STATE,
  QUNIF_OPERATION_ROOT_COUNT
};

typedef enum {
  QUNIF_KIND_UNKNOWN = 0,
  QUNIF_KIND_DBL,
  QUNIF_KIND_INT,
  QUNIF_KIND_FCT,
  QUNIF_KIND_LGL
} qunif_kind_t;

typedef struct {
  R_xlen_t size;
  SEXP ids;
  SEXP classes;
  SEXP grouping;
  SEXP lower;
  SEXP upper;
  SEXP tolerance;
  SEXP levels;
  SEXP storage_types;
} param_columns_t;

typedef struct {
  uint64_t hash;
  R_xlen_t row_plus_one;
} id_slot_t;

typedef struct {
  id_slot_t *slots;
  R_xlen_t capacity;
  SEXP ids;
} id_map_t;

typedef struct {
  qunif_kind_t kind;
  double lower;
  double upper;
  SEXP levels;
} qunif_spec_t;

typedef struct {
  R_xlen_t rows;
  R_xlen_t columns;
  SEXP column_names;
  SEXP values;
} qunif_input_t;

static R_xlen_t qunif_column_root(enum paradox_domain_column column) {
  return (R_xlen_t) QUNIF_ROOT_COLUMNS + (R_xlen_t) column;
}

static int load_param_columns(SEXP params, param_columns_t *columns,
    SEXP roots) {
  paradox_domain_params_t checked;
  R_xlen_t unused_row = 0;
  R_xlen_t work_since_interrupt = 0;
  if (!paradox_domain_validate_params(
      params,
      R_NilValue,
      TRUE,
      &checked,
      &unused_row,
      &work_since_interrupt
    )) {
    return FALSE;
  }

  for (R_xlen_t column = 0; column < PARADOX_DOMAIN_TAGS; ++column) {
    SEXP value = PROTECT(VECTOR_ELT(params, column));
    SET_VECTOR_ELT(
      roots,
      qunif_column_root((enum paradox_domain_column) column),
      value
    );
    UNPROTECT(1);
  }

  columns->ids = VECTOR_ELT(
    roots,
    qunif_column_root(PARADOX_DOMAIN_ID)
  );
  columns->size = checked.row_count;

  columns->classes = VECTOR_ELT(
    roots,
    qunif_column_root(PARADOX_DOMAIN_CLS)
  );
  columns->grouping = VECTOR_ELT(
    roots,
    qunif_column_root(PARADOX_DOMAIN_GROUPING)
  );
  columns->lower = VECTOR_ELT(
    roots,
    qunif_column_root(PARADOX_DOMAIN_LOWER)
  );
  columns->upper = VECTOR_ELT(
    roots,
    qunif_column_root(PARADOX_DOMAIN_UPPER)
  );
  columns->tolerance = VECTOR_ELT(
    roots,
    qunif_column_root(PARADOX_DOMAIN_TOLERANCE)
  );
  columns->levels = VECTOR_ELT(
    roots,
    qunif_column_root(PARADOX_DOMAIN_LEVELS)
  );
  columns->storage_types = VECTOR_ELT(
    roots,
    qunif_column_root(PARADOX_DOMAIN_STORAGE_TYPE)
  );

  return TRUE;
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

static int initialize_id_map(SEXP ids, id_map_t *map) {
  const R_xlen_t size = XLENGTH(ids);
  if (size > R_XLEN_T_MAX / 2) {
    return FALSE;
  }

  R_xlen_t capacity = 1;
  const R_xlen_t needed = size == 0 ? 1 : size * 2;
  while (capacity < needed) {
    if (capacity > R_XLEN_T_MAX / 2) {
      return FALSE;
    }
    capacity *= 2;
  }

  id_slot_t *slots = paradox_temporary_alloc(capacity, sizeof(*slots));
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t slot = 0; slot < capacity; ++slot) {
    paradox_account_work(&work_since_interrupt);
    slots[slot].hash = 0;
    slots[slot].row_plus_one = 0;
  }

  const R_xlen_t mask = capacity - 1;
  for (R_xlen_t row = 0; row < size; ++row) {
    paradox_account_work(&work_since_interrupt);
    SEXP id = PROTECT(STRING_ELT(ids, row));
    const uint64_t hash = hash_string(id);
    R_xlen_t slot = (R_xlen_t) (hash & (uint64_t) mask);
    while (slots[slot].row_plus_one != 0) {
      paradox_account_work(&work_since_interrupt);
      const R_xlen_t present = slots[slot].row_plus_one - 1;
      if (slots[slot].hash == hash &&
          paradox_domain_strings_equal(STRING_ELT(ids, present), id)) {
        UNPROTECT(1);
        return FALSE;
      }
      slot = (slot + 1) & mask;
    }
    slots[slot].hash = hash;
    slots[slot].row_plus_one = row + 1;
    UNPROTECT(1);
  }

  map->slots = slots;
  map->capacity = capacity;
  map->ids = ids;
  return TRUE;
}

static int find_id(const id_map_t *map, SEXP id, R_xlen_t *row,
    R_xlen_t *work_since_interrupt) {
  const uint64_t hash = hash_string(id);
  const R_xlen_t mask = map->capacity - 1;
  R_xlen_t slot = (R_xlen_t) (hash & (uint64_t) mask);
  while (map->slots[slot].row_plus_one != 0) {
    paradox_account_work(work_since_interrupt);
    const R_xlen_t present = map->slots[slot].row_plus_one - 1;
    if (map->slots[slot].hash == hash &&
        paradox_domain_strings_equal(STRING_ELT(map->ids, present), id)) {
      *row = present;
      return TRUE;
    }
    slot = (slot + 1) & mask;
  }
  return FALSE;
}

static void require_unit_interval(double unit) {
  if (ISNAN(unit)) {
    Rf_error("Values in `x` must not be missing or NaN");
  }
  if (!R_FINITE(unit)) {
    Rf_error("Values in `x` must be finite");
  }
  if (unit < 0.0 || unit > 1.0) {
    Rf_error("Values in `x` must be between zero and one");
  }
}

static int ordinary_character_metadata(SEXP value, int allow_altrep) {
  return TYPEOF(value) == STRSXP && (allow_altrep || !ALTREP(value)) &&
    !Rf_isS4(value) && !Rf_isObject(value) &&
    paradox_api_has_no_attributes(value);
}

static void snapshot_input_names(SEXP source_names, R_xlen_t columns,
    int allow_altrep, SEXP roots, R_xlen_t *work_since_interrupt) {
  PROTECT(source_names);
  if (!ordinary_character_metadata(source_names, allow_altrep) ||
      XLENGTH(source_names) != columns) {
    UNPROTECT(1);
    Rf_error("`x` must have one column name for every column");
  }

  SEXP stable_names = PROTECT(Rf_allocVector(STRSXP, columns));
  for (R_xlen_t column = 0; column < columns; ++column) {
    paradox_account_work(work_since_interrupt);
    SEXP name = STRING_ELT(source_names, column);
    if (name == NA_STRING) {
      UNPROTECT(2);
      Rf_error("Column names of `x` must not be missing");
    }
    SET_STRING_ELT(stable_names, column, name);
  }
  SET_VECTOR_ELT(roots, QUNIF_INPUT_NAMES, stable_names);
  UNPROTECT(2);
}

static R_xlen_t checked_input_size(R_xlen_t rows, R_xlen_t columns) {
  if (rows < 0 || columns <= 0 ||
      (rows != 0 && columns > R_XLEN_T_MAX / rows)) {
    Rf_error("`x` dimensions are too large");
  }
  return rows * columns;
}

static int matrix_metadata_current(
    SEXP x, SEXP dimensions, SEXP dimension_names) {
  int has_class = FALSE;
  if (!paradox_bounded_metadata_has_tag(
      x,
      R_ClassSymbol,
      &has_class
    ) || has_class) {
    return FALSE;
  }
  /*
   * The complete bounded tag scan is allocation-free and immediately
   * precedes these two raw selectors, so even a cyclic caller-owned spine
   * cannot reach an unbounded lookup.
   */
  return paradox_stored_attribute(x, R_DimSymbol) == dimensions &&
    paradox_stored_attribute(x, R_DimNamesSymbol) == dimension_names;
}

static void snapshot_matrix_input(SEXP x, qunif_input_t *info, SEXP roots,
    R_xlen_t *work_since_interrupt) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(x);
  if ((type != REALSXP && type != INTSXP) || Rf_isS4(x) ||
      Rf_isObject(x)) {
    Rf_error("`x` must be a numeric matrix or data.frame");
  }

  int has_class = FALSE;
  if (!paradox_bounded_metadata_has_tag(
      x,
      R_ClassSymbol,
      &has_class
    )) {
    Rf_error("`x` must have ordinary, acyclic, bounded metadata");
  }
  if (has_class) {
    Rf_error("`x` must be a numeric matrix or data.frame");
  }
  SEXP dimensions = PROTECT(paradox_stored_attribute(x, R_DimSymbol));
  SEXP dimension_names = PROTECT(paradox_stored_attribute(
    x,
    R_DimNamesSymbol
  ));
  if (TYPEOF(dimensions) != INTSXP || ALTREP(dimensions) ||
      Rf_isS4(dimensions) || Rf_isObject(dimensions) ||
      !paradox_api_has_no_attributes(dimensions) ||
      XLENGTH(dimensions) != 2) {
    UNPROTECT(2);
    Rf_error("`x` must be a numeric matrix or data.frame");
  }
  const int row_count = INTEGER_ELT(dimensions, 0);
  const int column_count = INTEGER_ELT(dimensions, 1);
  if (row_count < 0 || column_count <= 0) {
    UNPROTECT(2);
    Rf_error("`x` must have at least one column");
  }
  info->rows = (R_xlen_t) row_count;
  info->columns = (R_xlen_t) column_count;
  const R_xlen_t size = checked_input_size(info->rows, info->columns);
  if (XLENGTH(x) != size ||
      !matrix_metadata_current(x, dimensions, dimension_names)) {
    UNPROTECT(2);
    Rf_error("`x` has inconsistent matrix dimensions");
  }

  static const char *const names_only[] = {"names"};
  if (TYPEOF(dimension_names) != VECSXP || ALTREP(dimension_names) ||
      Rf_isS4(dimension_names) || Rf_isObject(dimension_names) ||
      !paradox_api_has_only_attributes(dimension_names, names_only, 1) ||
      XLENGTH(dimension_names) != 2) {
    UNPROTECT(2);
    Rf_error("`x` must have one column name for every column");
  }
  SEXP dimension_labels = PROTECT(Rf_getAttrib(
    dimension_names,
    R_NamesSymbol
  ));
  if (dimension_labels != R_NilValue &&
      (!ordinary_character_metadata(dimension_labels, FALSE) ||
        XLENGTH(dimension_labels) != 2)) {
    UNPROTECT(3);
    Rf_error("`x` has invalid matrix dimnames metadata");
  }
  SEXP source_names = PROTECT(VECTOR_ELT(dimension_names, 1));
  snapshot_input_names(
    source_names,
    info->columns,
    TRUE,
    roots,
    work_since_interrupt
  );
  if (!matrix_metadata_current(x, dimensions, dimension_names) ||
      VECTOR_ELT(dimension_names, 1) != source_names) {
    UNPROTECT(4);
    Rf_error("`x` matrix metadata changed while being snapshotted");
  }
  SET_VECTOR_ELT(roots, QUNIF_INPUT_SOURCE, x);

  SEXP stable_values = PROTECT(Rf_allocVector(REALSXP, size));
  if (!matrix_metadata_current(x, dimensions, dimension_names) ||
      VECTOR_ELT(dimension_names, 1) != source_names) {
    UNPROTECT(5);
    Rf_error("`x` matrix metadata changed while being snapshotted");
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_account_work(work_since_interrupt);
    const double unit = paradox_numeric_elt(x, index);
    require_unit_interval(unit);
    SET_REAL_ELT(stable_values, index, unit);
  }
  if (!matrix_metadata_current(x, dimensions, dimension_names) ||
      VECTOR_ELT(dimension_names, 1) != source_names) {
    UNPROTECT(5);
    Rf_error("`x` matrix metadata changed while being snapshotted");
  }
  SET_VECTOR_ELT(roots, QUNIF_INPUT_VALUES, stable_values);
  info->column_names = VECTOR_ELT(roots, QUNIF_INPUT_NAMES);
  info->values = VECTOR_ELT(roots, QUNIF_INPUT_VALUES);
  UNPROTECT(5);
}

static int ordinary_frame_shell(SEXP x) {
  if (TYPEOF(x) != VECSXP || ALTREP(x) || Rf_isS4(x)) return FALSE;
  int valid = paradox_public_table_kind(x) != PARADOX_PUBLIC_TABLE_NONE;
  if (valid) {
    SEXP names = PROTECT(paradox_api_raw_attribute(x, R_NamesSymbol));
    valid = ordinary_character_metadata(names, FALSE) &&
      XLENGTH(names) == XLENGTH(x);
    UNPROTECT(1);
  }
  return valid;
}

typedef enum {
  QUNIF_FRAME_COLUMN_CURRENT = 0,
  QUNIF_FRAME_COLUMN_WRONG_TYPE,
  QUNIF_FRAME_COLUMN_WRONG_LENGTH
} qunif_frame_column_status_t;

/*
 * A data-frame snapshot owns each exact column identity, but deliberately
 * does not copy its payload before the bulk REALSXP materialization.  Length
 * may dispatch for a stable atomic ALTREP and a later column's Elt method may
 * mutate an earlier column's metadata.  Each admission/receipt pass therefore
 * observes Length first, then bounds the complete attribute spine and ends
 * with allocation-free type/class flags.  Ordinary vectors receive one final
 * callback-free length receipt as well.
 */
static qunif_frame_column_status_t frame_numeric_column_status(
    SEXP source, SEXPTYPE captured_type, R_xlen_t rows) {
  const int source_altrep = ALTREP(source);
  if (XLENGTH(source) != rows) {
    return QUNIF_FRAME_COLUMN_WRONG_LENGTH;
  }
  int has_class = FALSE;
  if (!paradox_bounded_metadata_has_tag(
      source,
      R_ClassSymbol,
      &has_class
    ) || has_class ||
      (SEXPTYPE) TYPEOF(source) != captured_type ||
      (captured_type != REALSXP && captured_type != INTSXP) ||
      Rf_isS4(source) || Rf_isObject(source)) {
    return QUNIF_FRAME_COLUMN_WRONG_TYPE;
  }
  if (!source_altrep && XLENGTH(source) != rows) {
    return QUNIF_FRAME_COLUMN_WRONG_LENGTH;
  }
  return QUNIF_FRAME_COLUMN_CURRENT;
}

static void require_frame_columns_current(
    SEXP source_columns, const SEXPTYPE *captured_types,
    R_xlen_t columns, R_xlen_t rows) {
  for (R_xlen_t column = 0; column < columns; ++column) {
    if (frame_numeric_column_status(
        VECTOR_ELT(source_columns, column),
        captured_types[column],
        rows
      ) != QUNIF_FRAME_COLUMN_CURRENT) {
      Rf_error("Columns of `x` changed while being snapshotted");
    }
  }
}

static void snapshot_frame_input(SEXP x, qunif_input_t *info, SEXP roots,
    R_xlen_t *work_since_interrupt) {
  if (!ordinary_frame_shell(x)) {
    Rf_error("`x` must be a numeric matrix or data.frame");
  }
  info->columns = XLENGTH(x);
  if (info->columns <= 0) {
    Rf_error("`x` must have at least one column");
  }

  /* Allocate both snapshot holders before reading anything out of `x`.
   * Allocation may run a pending finalizer, and the row-name Length method
   * below may reenter R outright; either can replace or permute the caller's
   * columns. Names and column identities are therefore copied afterwards in
   * one pass that cannot allocate, so they are one consistent generation. A
   * name paired with another generation's column would silently map every
   * value through the wrong parameter's Domain. */
  SEXP stable_names = PROTECT(Rf_allocVector(STRSXP, info->columns));
  SEXP source_columns = PROTECT(Rf_allocVector(VECSXP, info->columns));
  if (XLENGTH(x) != info->columns ||
      !paradox_capture_list_identities(
        x,
        stable_names,
        source_columns
      )) {
    UNPROTECT(2);
    Rf_error("`x` must have one column name for every column");
  }
  for (R_xlen_t column = 0; column < info->columns; ++column) {
    SEXP name = STRING_ELT(stable_names, column);
    if (name == NA_STRING) {
      UNPROTECT(2);
      Rf_error("Column names of `x` must not be missing");
    }
  }

  /*
   * Select the dimension carrier while this allocation-free snapshot window
   * still denotes the same table generation as the names and columns above.
   * A later allocation or interrupt may run a finalizer that mutates the
   * caller's table shell by reference.
   */
  if (!paradox_public_table_row_count(x, &info->rows)) {
    UNPROTECT(2);
    Rf_error("`x` has invalid data.frame row names");
  }

  SET_VECTOR_ELT(roots, QUNIF_INPUT_NAMES, stable_names);
  SET_VECTOR_ELT(roots, QUNIF_INPUT_SOURCE, source_columns);
  UNPROTECT(2);

  if (info->rows > INT_MAX) {
    Rf_error("`x` has too many rows for a data.frame result");
  }

  SEXPTYPE *captured_types = paradox_temporary_alloc(
    info->columns,
    sizeof(*captured_types)
  );

  /* Validate the owned columns. These reads may dispatch an ALTREP Length,
   * but they observe this operation's own snapshot. */
  for (R_xlen_t column = 0; column < info->columns; ++column) {
    paradox_account_work(work_since_interrupt);
    SEXP source = VECTOR_ELT(source_columns, column);
    captured_types[column] = (SEXPTYPE) TYPEOF(source);
    const qunif_frame_column_status_t status =
      frame_numeric_column_status(
        source,
        captured_types[column],
        info->rows
      );
    if (status == QUNIF_FRAME_COLUMN_WRONG_TYPE) {
      Rf_error("Every column of `x` must be an unclassed numeric vector");
    }
    if (status == QUNIF_FRAME_COLUMN_WRONG_LENGTH) {
      Rf_error(column == 0
        ? "`x` has invalid data.frame row names"
        : "Columns of `x` must have equal lengths");
    }
  }
  const R_xlen_t size = checked_input_size(info->rows, info->columns);

  SEXP stable_values = PROTECT(Rf_allocVector(REALSXP, size));
  require_frame_columns_current(
    source_columns,
    captured_types,
    info->columns,
    info->rows
  );
  for (R_xlen_t column = 0; column < info->columns; ++column) {
    SEXP source = VECTOR_ELT(source_columns, column);
    for (R_xlen_t row = 0; row < info->rows; ++row) {
      paradox_account_work(work_since_interrupt);
      const double unit = paradox_numeric_elt(source, row);
      require_unit_interval(unit);
      SET_REAL_ELT(
        stable_values,
        column * info->rows + row,
        unit
      );
    }
  }
  require_frame_columns_current(
    source_columns,
    captured_types,
    info->columns,
    info->rows
  );
  SET_VECTOR_ELT(roots, QUNIF_INPUT_VALUES, stable_values);
  info->column_names = VECTOR_ELT(roots, QUNIF_INPUT_NAMES);
  info->values = VECTOR_ELT(roots, QUNIF_INPUT_VALUES);
  UNPROTECT(1);
}

static void snapshot_qunif_input(SEXP x, qunif_input_t *info, SEXP roots) {
  R_xlen_t work_since_interrupt = 0;
  SEXP stable_shell = PROTECT(paradox_materialize_public_table_shell(x));
  if (ordinary_frame_shell(stable_shell)) {
    snapshot_frame_input(stable_shell, info, roots, &work_since_interrupt);
  } else {
    snapshot_matrix_input(stable_shell, info, roots, &work_since_interrupt);
  }
  UNPROTECT(1);
}

static SEXP snapshot_factor_levels(SEXP levels,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(levels) != STRSXP) {
    return R_NilValue;
  }
  const R_xlen_t size = XLENGTH(levels);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t level = 0; level < size; ++level) {
    paradox_account_work(work_since_interrupt);
    SEXP value = STRING_ELT(levels, level);
    if (value == NA_STRING) {
      UNPROTECT(1);
      return R_NilValue;
    }
    SET_STRING_ELT(result, level, value);
  }
  UNPROTECT(1);
  return result;
}

static int load_spec(const param_columns_t *columns, R_xlen_t row,
    qunif_spec_t *spec, SEXP roots, R_xlen_t root_index,
    R_xlen_t *work_since_interrupt) {
  SEXP class_name = STRING_ELT(columns->classes, row);
  SEXP storage_type = STRING_ELT(columns->storage_types, row);
  spec->kind = QUNIF_KIND_UNKNOWN;
  spec->levels = R_NilValue;
  SEXP source_levels = PROTECT(VECTOR_ELT(columns->levels, row));

  if (paradox_domain_string_is(class_name, "ParamDbl") &&
      paradox_domain_string_is(storage_type, "numeric")) {
    spec->kind = QUNIF_KIND_DBL;
  } else if (paradox_domain_string_is(class_name, "ParamInt") &&
      paradox_domain_string_is(storage_type, "integer")) {
    spec->kind = QUNIF_KIND_INT;
  } else if (paradox_domain_string_is(class_name, "ParamFct") &&
      paradox_domain_string_is(storage_type, "character")) {
    SEXP stable_levels = PROTECT(snapshot_factor_levels(
      source_levels,
      work_since_interrupt
    ));
    if (stable_levels == R_NilValue) {
      UNPROTECT(2);
      return FALSE;
    }
    spec->kind = QUNIF_KIND_FCT;
    spec->levels = stable_levels;
    SET_VECTOR_ELT(roots, root_index, stable_levels);
    UNPROTECT(2);
    return TRUE;
  } else if (paradox_domain_string_is(class_name, "ParamLgl") &&
      paradox_domain_string_is(storage_type, "logical") &&
      TYPEOF(source_levels) == LGLSXP && XLENGTH(source_levels) == 2 &&
      LOGICAL_ELT(source_levels, 0) == TRUE &&
      LOGICAL_ELT(source_levels, 1) == FALSE) {
    spec->kind = QUNIF_KIND_LGL;
    UNPROTECT(1);
    return TRUE;
  } else {
    UNPROTECT(1);
    return FALSE;
  }

  spec->lower = paradox_numeric_elt(columns->lower, row);
  spec->upper = paradox_numeric_elt(columns->upper, row);
  const double tolerance = paradox_numeric_elt(columns->tolerance, row);
  const int valid = !ISNAN(spec->lower) && !ISNAN(spec->upper) &&
    !ISNAN(tolerance) && R_FINITE(tolerance) && tolerance >= 0.0 &&
    spec->lower <= spec->upper;
  UNPROTECT(1);
  return valid;
}

static SEXPTYPE output_type(qunif_kind_t kind) {
  switch (kind) {
  case QUNIF_KIND_DBL:
    return REALSXP;
  case QUNIF_KIND_INT:
    return INTSXP;
  case QUNIF_KIND_FCT:
    return STRSXP;
  case QUNIF_KIND_LGL:
    return LGLSXP;
  case QUNIF_KIND_UNKNOWN:
    break;
  }
  return NILSXP;
}

static SEXP copy_column_names(SEXP names,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t size = XLENGTH(names);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_account_work(work_since_interrupt);
    SET_STRING_ELT(result, index, STRING_ELT(names, index));
  }
  UNPROTECT(1);
  return result;
}

static SEXP set_table_attributes(SEXP table, SEXP names, R_xlen_t rows) {
  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(classes, 0, Rf_mkChar("data.table"));
  SET_STRING_ELT(classes, 1, Rf_mkChar("data.frame"));

  SEXP row_names;
  if (rows == 0) {
    row_names = PROTECT(Rf_allocVector(INTSXP, 0));
  } else {
    row_names = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(row_names)[0] = NA_INTEGER;
    INTEGER(row_names)[1] = -(int) rows;
  }

  /* Keep one native construction order on every supported R runtime.
   * Historical data.table::CJ() pairlist order varied with R itself; that
   * incidental order is not part of the public grid contract. */
  Rf_setAttrib(table, R_RowNamesSymbol, row_names);
  Rf_setAttrib(table, R_ClassSymbol, classes);
  Rf_setAttrib(table, R_NamesSymbol, names);
  SEXP result = PROTECT(paradox_prepare_data_table(table, TRUE));
  UNPROTECT(3);
  return result;
}

static int fill_column(SEXP output, SEXP x, R_xlen_t input_offset,
    R_xlen_t rows, const qunif_spec_t *spec,
    int *warn_integer_range, R_xlen_t *work_since_interrupt) {
  for (R_xlen_t row = 0; row < rows; ++row) {
    paradox_account_work(work_since_interrupt);
    const double unit = paradox_numeric_elt(x, input_offset + row);
    switch (spec->kind) {
    case QUNIF_KIND_DBL:
      REAL(output)[row] = paradox_qunif_double_value(
        unit,
        spec->lower,
        spec->upper
      );
      break;
    case QUNIF_KIND_INT:
      if (!paradox_qunif_integer_value(
            unit,
            spec->lower,
            spec->upper,
            &INTEGER(output)[row]
          )) {
        INTEGER(output)[row] = NA_INTEGER;
        *warn_integer_range = TRUE;
      }
      break;
    case QUNIF_KIND_FCT: {
      const R_xlen_t level = paradox_qunif_level_index(
        unit,
        XLENGTH(spec->levels)
      );
      if (level == R_XLEN_T_MAX) {
        return FALSE;
      }
      SET_STRING_ELT(
        output,
        row,
        STRING_ELT(spec->levels, level)
      );
      break;
    }
    case QUNIF_KIND_LGL:
      LOGICAL(output)[row] = unit < 0.5;
      break;
    case QUNIF_KIND_UNKNOWN:
      return FALSE;
    }
  }
  return TRUE;
}

static int grid_resolution_at(SEXP resolutions, R_xlen_t index,
    int *result) {
  if (TYPEOF(resolutions) == INTSXP) {
    const int value = INTEGER_ELT(resolutions, index);
    if (value == NA_INTEGER || value < 0) {
      return FALSE;
    }
    *result = value;
    return TRUE;
  }

  const double value = REAL_ELT(resolutions, index);
  if (!R_FINITE(value) || value < 0.0 || value > (double) INT_MAX ||
      floor(value) != value) {
    return FALSE;
  }
  *result = (int) value;
  return TRUE;
}

static int parse_grid_resolution(SEXP resolution, int *supplied) {
  *supplied = resolution != R_NilValue;
  if (resolution == R_NilValue) return 0;
  if (Rf_isS4(resolution) || Rf_isObject(resolution) ||
      ALTREP(resolution) || !paradox_api_has_no_attributes(resolution) ||
      XLENGTH(resolution) != 1) {
    Rf_error("`resolution` must be one non-negative whole number");
  }
  int result;
  if (!grid_resolution_at(resolution, 0, &result)) {
    Rf_error("`resolution` must be one non-negative whole number");
  }
  return result;
}

static R_xlen_t snapshot_grid_resolution_overrides(
    SEXP param_resolutions, SEXP stable_names, SEXP stable_counts) {
  if (param_resolutions == R_NilValue) return 0;
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(param_resolutions);
  if ((type != INTSXP && type != REALSXP) ||
      ALTREP(param_resolutions) || Rf_isS4(param_resolutions) ||
      Rf_isObject(param_resolutions)) {
    Rf_error(
      "`param_resolutions` must be a named numeric vector of non-negative whole numbers"
    );
  }
  const R_xlen_t size = XLENGTH(param_resolutions);
  if (XLENGTH(stable_names) != size || XLENGTH(stable_counts) != size ||
      !paradox_api_has_single_attribute(param_resolutions, "names")) {
    Rf_error("`param_resolutions` must have one name per value");
  }
  SEXP names = Rf_getAttrib(param_resolutions, R_NamesSymbol);
  if (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
      Rf_isObject(names) || !paradox_api_has_no_attributes(names) ||
      XLENGTH(names) != size) {
    Rf_error("`param_resolutions` must have ordinary character names");
  }

  /*
   * Both destinations already exist.  This allocation-free pass owns the
   * complete caller control generation before the ParamSet graph is selected,
   * so an ALTREP/finalizer cannot pair one override's name with another
   * generation's count.
   */
  for (R_xlen_t index = 0; index < size; ++index) {
    int count;
    SEXP name = STRING_ELT(names, index);
    if (name == NA_STRING || CHAR(name)[0] == '\0') {
      Rf_error("`param_resolutions` names must be non-empty and non-missing");
    }
    if (!grid_resolution_at(param_resolutions, index, &count)) {
      Rf_error(
        "`param_resolutions` must contain non-negative whole numbers no greater than INT_MAX"
      );
    }
    SET_STRING_ELT(stable_names, index, name);
    SET_INTEGER_ELT(stable_counts, index, count);
  }
  return size;
}

static void load_grid_specs(const param_columns_t *columns,
    const id_map_t *id_map, int global_supplied, int global_count,
    SEXP override_names, SEXP override_counts,
    SEXP resolution_names, qunif_spec_t *specs, int *counts,
    unsigned char *overridden, SEXP spec_roots,
    R_xlen_t *work_since_interrupt) {
  int has_numeric = FALSE;
  for (R_xlen_t row = 0; row < columns->size; ++row) {
    paradox_account_work(work_since_interrupt);
    overridden[row] = 0;
    if (!load_spec(
          columns,
          row,
          &specs[row],
          spec_roots,
          row,
          work_since_interrupt
        )) {
      if (paradox_domain_string_is(
          STRING_ELT(columns->classes, row),
          "ParamUty"
        )) {
        Rf_error("Grid generation is undefined for ParamUty");
      }
      Rf_error("Corrupt ParamSet grid quantile state");
    }
    switch (specs[row].kind) {
    case QUNIF_KIND_DBL:
    case QUNIF_KIND_INT:
      has_numeric = TRUE;
      counts[row] = global_supplied ? global_count : -1;
      break;
    case QUNIF_KIND_FCT:
      if (XLENGTH(specs[row].levels) > INT_MAX) {
        Rf_error("Factor parameter has too many levels for grid generation");
      }
      counts[row] = (int) XLENGTH(specs[row].levels);
      break;
    case QUNIF_KIND_LGL:
      counts[row] = 2;
      break;
    case QUNIF_KIND_UNKNOWN:
      Rf_error("Corrupt ParamSet grid quantile state");
    }
  }

  /*
   * Categorical-only and zero-dimensional spaces have no resolution control
   * to normalize.  This preserves the public convention that these arguments
   * are irrelevant there while still admitting the closed schema once.
   */
  if (!has_numeric) {
    for (R_xlen_t row = 0; row < columns->size; ++row) {
      SET_STRING_ELT(
        resolution_names,
        row,
        STRING_ELT(columns->ids, row)
      );
    }
    return;
  }
  if (!global_supplied && XLENGTH(override_names) == 0) {
    Rf_error("You must specify 'resolution' or 'param_resolutions'!");
  }

  R_xlen_t output = 0;
  for (R_xlen_t index = 0; index < XLENGTH(override_names); ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP name = STRING_ELT(override_names, index);
    R_xlen_t row;
    if (!find_id(id_map, name, &row, work_since_interrupt) ||
        (specs[row].kind != QUNIF_KIND_DBL &&
          specs[row].kind != QUNIF_KIND_INT)) {
      Rf_error(
        "`param_resolutions` names must name numerical ParamSet parameters"
      );
    }
    if (overridden[row]) {
      Rf_error("`param_resolutions` names must be unique");
    }
    counts[row] = INTEGER_ELT(override_counts, index);
    overridden[row] = 1;
    if (!global_supplied) {
      SET_STRING_ELT(resolution_names, output, name);
      ++output;
    }
  }

  for (R_xlen_t row = 0; row < columns->size; ++row) {
    if (counts[row] < 0) {
      Rf_error(
        "Resolution setting missing for numerical parameter '%s'",
        CHAR(STRING_ELT(columns->ids, row))
      );
    }
    if (global_supplied || !overridden[row]) {
      SET_STRING_ELT(
        resolution_names,
        output,
        STRING_ELT(columns->ids, row)
      );
      ++output;
    }
  }
  if (output != columns->size) {
    Rf_error("Internal error: incomplete grid axis order");
  }
}

static double grid_unit_value(R_xlen_t level, int resolution) {
  if (level == 0 || resolution <= 1) {
    return 0.0;
  }
  if (level == (R_xlen_t) resolution - 1) {
    return 1.0;
  }
  return (double) level * (1.0 / (double) (resolution - 1));
}

SEXP paradox_param_set_qunif_builtin(SEXP private_environment, SEXP self,
    SEXP x) {
  /* Materialize callback-capable input before selecting the ParamSet capsule.
   * A reentrant ALTREP callback sees and may replace current state; this outer
   * operation then consistently uses the post-materialization generation. */
  qunif_input_t input;
  SEXP input_roots = PROTECT(Rf_allocVector(
    VECSXP,
    QUNIF_INPUT_ROOT_COUNT
  ));
  snapshot_qunif_input(x, &input, input_roots);

  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet shell ownership");
  }
  SEXP operation_roots = PROTECT(Rf_allocVector(
    VECSXP,
    QUNIF_OPERATION_ROOT_COUNT
  ));
  SEXP core = paradox_core_from_private(private_environment);
  if (core == R_UnboundValue) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSet quantile state: missing core capsule");
  }
  if (!paradox_core_is_verified(core)) {
    core = paradox_core_refresh(self, private_environment);
  }
  const paradox_core_kind_t kind = paradox_core_kind(core);
  if (kind != PARADOX_CORE_BASE && kind != PARADOX_CORE_COLLECTION &&
      kind != PARADOX_CORE_SHADOW) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSet quantile state: unknown core kind");
  }
  if (!paradox_core_has_exact_schema(core)) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSet quantile state: invalid core schema");
  }
  SET_VECTOR_ELT(operation_roots, QUNIF_OPERATION_CORE, core);
  SEXP state = paradox_core_payload(core);
  if (state == R_UnboundValue) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSet quantile state: invalid core payload");
  }
  SET_VECTOR_ELT(operation_roots, QUNIF_OPERATION_STATE, state);
  SEXP params = VECTOR_ELT(state, PARADOX_CORE_PARAMS);

  param_columns_t columns;
  SEXP column_roots = PROTECT(Rf_allocVector(VECSXP, QUNIF_ROOT_COUNT));
  if (!load_param_columns(params, &columns, column_roots)) {
    UNPROTECT(3);
    Rf_error("Corrupt ParamSet quantile state: invalid parameter schema");
  }

  id_map_t id_map;
  if (!initialize_id_map(columns.ids, &id_map)) {
    UNPROTECT(3);
    Rf_error("Corrupt ParamSet quantile state: duplicate parameter IDs");
  }

  qunif_spec_t *specs = paradox_temporary_alloc(
    input.columns,
    sizeof(*specs)
  );
  SEXP spec_roots = PROTECT(Rf_allocVector(VECSXP, input.columns));
  unsigned char *selected = paradox_temporary_alloc(
    columns.size == 0 ? 1 : columns.size,
    sizeof(*selected)
  );
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t row = 0; row < columns.size; ++row) {
    paradox_account_work(&work_since_interrupt);
    selected[row] = 0;
  }
  for (R_xlen_t column = 0; column < input.columns; ++column) {
    paradox_account_work(&work_since_interrupt);
    R_xlen_t param_row;
    if (!find_id(
          &id_map,
          STRING_ELT(input.column_names, column),
          &param_row,
          &work_since_interrupt
        )) {
      UNPROTECT(4);
      Rf_error("Column names of `x` must be a subset of ParamSet IDs");
    }
    if (selected[param_row]) {
      UNPROTECT(4);
      Rf_error("Column names of `x` must be unique");
    }
    if (!load_spec(
          &columns,
          param_row,
          &specs[column],
          spec_roots,
          column,
          &work_since_interrupt
        )) {
      if (paradox_domain_string_is(
          STRING_ELT(columns.classes, param_row),
          "ParamUty"
        )) {
        UNPROTECT(4);
        Rf_error("ParamSet$qunif() is undefined for ParamUty");
      }
      UNPROTECT(4);
      Rf_error("Corrupt ParamSet quantile state for selected parameter");
    }
    selected[param_row] = 1;
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, input.columns));
  SEXP result_names = PROTECT(copy_column_names(
    input.column_names,
    &work_since_interrupt
  ));
  int warn_integer_range = FALSE;
  for (R_xlen_t column = 0; column < input.columns; ++column) {
    paradox_account_work(&work_since_interrupt);
    if (input.rows != 0 && specs[column].kind == QUNIF_KIND_FCT &&
        XLENGTH(specs[column].levels) == 0) {
      UNPROTECT(6);
      Rf_error("Cannot map quantiles for a factor parameter with no levels");
    }
    SEXP output = PROTECT(Rf_allocVector(
      output_type(specs[column].kind),
      input.rows
    ));
    SET_VECTOR_ELT(result, column, output);
    if (!fill_column(
          output,
          input.values,
          column * input.rows,
          input.rows,
          &specs[column],
          &warn_integer_range,
          &work_since_interrupt
        )) {
      UNPROTECT(7);
      Rf_error("Corrupt ParamSet quantile mapping state");
    }
    UNPROTECT(1);
  }
  if (warn_integer_range) {
    Rf_warning("NAs introduced by coercion to integer range");
  }
  SEXP prepared = PROTECT(set_table_attributes(
    result,
    result_names,
    input.rows
  ));
  UNPROTECT(7);
  return prepared;
}

enum grid_state_root {
  GRID_STATE_PRIVATE = 0,
  GRID_STATE_SELF,
  GRID_STATE_CORE,
  GRID_STATE_PARAMS,
  GRID_STATE_VALUES,
  GRID_STATE_DEPENDENCIES,
  GRID_STATE_RECEIPT,
  GRID_STATE_ROOT_COUNT
};

typedef struct {
  SEXP params;
  SEXP values;
  SEXP dependencies;
  SEXP receipt;
  paradox_domain_values_t values_data;
  paradox_domain_dependencies_t dependencies_data;
} grid_state_t;

typedef union {
  double real;
  int integer;
  SEXP string;
} grid_scalar_t;

typedef struct {
  qunif_spec_t spec;
  SEXP values;
  int *first_levels;
  R_xlen_t param_row;
  int count;
  int fixed;
} grid_axis_t;

typedef struct {
  grid_scalar_t *values;
  int *first_levels;
  int size;
  int capacity;
} grid_axis_builder_t;

static void load_grid_state(SEXP private_environment, SEXP self,
    grid_state_t *state, SEXP state_roots, SEXP *graph_roots,
    PROTECT_INDEX graph_roots_index, R_xlen_t *work_since_interrupt) {
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("Corrupt ParamSet shell ownership in grid generation");
  }
  SET_VECTOR_ELT(state_roots, GRID_STATE_PRIVATE, private_environment);
  SET_VECTOR_ELT(state_roots, GRID_STATE_SELF, self);

  SEXP core = paradox_core_from_private(private_environment);
  if (core == R_UnboundValue) {
    Rf_error("Corrupt ParamSet grid state: missing core capsule");
  }
  paradox_core_kind_t kind = paradox_core_kind(core);
  /* Collection graph admission owns its one refresh traversal. */
  if (kind != PARADOX_CORE_COLLECTION &&
      !paradox_core_is_verified(core)) {
    core = paradox_core_refresh(self, private_environment);
    kind = paradox_core_kind(core);
  }
  if (kind != PARADOX_CORE_BASE && kind != PARADOX_CORE_COLLECTION &&
      kind != PARADOX_CORE_SHADOW) {
    Rf_error("Corrupt ParamSet grid state: unknown core kind");
  }
  SET_VECTOR_ELT(state_roots, GRID_STATE_CORE, core);

  if (kind == PARADOX_CORE_COLLECTION) {
    paradox_collection_graph_t graph;
    paradox_collection_graph_build_receipted(
      private_environment,
      self,
      &graph,
      graph_roots,
      graph_roots_index,
      work_since_interrupt
    );
    core = graph.nodes[0].core;
    SET_VECTOR_ELT(state_roots, GRID_STATE_CORE, core);
    state->params = graph.nodes[0].params.table;
    SET_VECTOR_ELT(state_roots, GRID_STATE_PARAMS, state->params);

    SEXP values = PROTECT(paradox_collection_values_from_graph(
      &graph,
      work_since_interrupt
    ));
    state->values = values;
    SET_VECTOR_ELT(state_roots, GRID_STATE_VALUES, state->values);
    UNPROTECT(1);

    SEXP dependencies = PROTECT(paradox_collection_dependencies_from_graph(
      &graph,
      work_since_interrupt
    ));
    state->dependencies = dependencies;
    SET_VECTOR_ELT(
      state_roots,
      GRID_STATE_DEPENDENCIES,
      state->dependencies
    );
    UNPROTECT(1);
    state->receipt = PROTECT(paradox_generation_receipt_graph(&graph));
    SET_VECTOR_ELT(
      state_roots,
      GRID_STATE_RECEIPT,
      state->receipt
    );
    UNPROTECT(1);
  } else {
    SEXP payload = paradox_core_payload(core);
    if (payload == R_UnboundValue) {
      Rf_error("Corrupt ParamSet grid state: invalid core payload");
    }
    state->params = VECTOR_ELT(payload, PARADOX_CORE_PARAMS);
    state->values = VECTOR_ELT(payload, PARADOX_CORE_VALUES);
    state->dependencies = VECTOR_ELT(payload, PARADOX_CORE_DEPS);
    SET_VECTOR_ELT(state_roots, GRID_STATE_PARAMS, state->params);
    SET_VECTOR_ELT(state_roots, GRID_STATE_VALUES, state->values);
    SET_VECTOR_ELT(
      state_roots,
      GRID_STATE_DEPENDENCIES,
      state->dependencies
    );
    if (kind == PARADOX_CORE_SHADOW) {
      SEXP signature = PROTECT(paradox_shadow_metadata_signature(core));
      if (signature == R_UnboundValue) {
        UNPROTECT(1);
        Rf_error("Corrupt ParamSetShadow signature in grid generation");
      }
      SEXP content = PROTECT(
        paradox_shadow_signature_content_snapshot(signature)
      );
      if (content == R_NilValue ||
          !paradox_shadow_signature_receipt_is_current(
            core,
            signature,
            content
          )) {
        UNPROTECT(2);
        Rf_error("ParamSetShadow changed during grid generation");
      }
      state->receipt = PROTECT(paradox_generation_receipt_single(
        private_environment,
        core,
        signature,
        content
      ));
      SET_VECTOR_ELT(
        state_roots,
        GRID_STATE_RECEIPT,
        state->receipt
      );
      UNPROTECT(3);
    } else {
      state->receipt = PROTECT(paradox_generation_receipt_single(
        private_environment,
        core,
        R_NilValue,
        R_NilValue
      ));
      SET_VECTOR_ELT(
        state_roots,
        GRID_STATE_RECEIPT,
        state->receipt
      );
      UNPROTECT(1);
    }
  }

  if (!paradox_domain_validate_values(
        state->values,
        &state->values_data,
        work_since_interrupt
      ) || !paradox_domain_validate_dependencies(
        state->dependencies,
        &state->dependencies_data,
        work_since_interrupt
      )) {
    Rf_error("Corrupt ParamSet grid value or dependency state");
  }
}

static SEXP grid_result_with_receipt(SEXP prepared, SEXP receipt) {
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, prepared);
  SET_VECTOR_ELT(result, 1, receipt);
  /*
   * The bundle allocation is the last callback-capable operation in native
   * grid construction. Require the selected graph at this exact boundary;
   * the R wrapper performs the same scan after installing the caller-facing
   * Design shell.
   */
  paradox_generation_receipt_scan(receipt);
  UNPROTECT(1);
  return result;
}

static void initialize_axis_builder(grid_axis_builder_t *builder,
    int maximum_size) {
  int capacity = maximum_size < 16 ? maximum_size : 16;
  if (capacity < 1) capacity = 1;
  builder->values = paradox_temporary_alloc(
    (R_xlen_t) capacity,
    sizeof(*builder->values)
  );
  builder->first_levels = paradox_temporary_alloc(
    (R_xlen_t) capacity,
    sizeof(*builder->first_levels)
  );
  builder->size = 0;
  builder->capacity = capacity;
}

static void grow_axis_builder(grid_axis_builder_t *builder,
    int maximum_size) {
  if (builder->size < builder->capacity) return;
  int capacity = builder->capacity;
  if (capacity > maximum_size / 2) {
    capacity = maximum_size;
  } else {
    capacity *= 2;
  }
  if (capacity <= builder->capacity) {
    Rf_error("Realized grid axis is too large");
  }
  grid_scalar_t *values = paradox_temporary_alloc(
    (R_xlen_t) capacity,
    sizeof(*values)
  );
  int *first_levels = paradox_temporary_alloc(
    (R_xlen_t) capacity,
    sizeof(*first_levels)
  );
  memcpy(
    values,
    builder->values,
    (size_t) builder->size * sizeof(*values)
  );
  memcpy(
    first_levels,
    builder->first_levels,
    (size_t) builder->size * sizeof(*first_levels)
  );
  builder->values = values;
  builder->first_levels = first_levels;
  builder->capacity = capacity;
}

static int same_realized_double(double left, double right) {
  if (left == right) return TRUE;
  if (R_IsNA(left) || R_IsNA(right)) {
    return R_IsNA(left) && R_IsNA(right);
  }
  return R_IsNaN(left) && R_IsNaN(right);
}

static int axis_builder_contains_last(const grid_axis_builder_t *builder,
    qunif_kind_t kind, grid_scalar_t value) {
  if (builder->size == 0) return FALSE;
  const grid_scalar_t previous = builder->values[builder->size - 1];
  switch (kind) {
  case QUNIF_KIND_DBL:
    return same_realized_double(previous.real, value.real);
  case QUNIF_KIND_INT:
  case QUNIF_KIND_LGL:
    return previous.integer == value.integer;
  case QUNIF_KIND_FCT:
    return paradox_domain_strings_equal(previous.string, value.string);
  case QUNIF_KIND_UNKNOWN:
    break;
  }
  Rf_error("Internal error: unknown realized grid kind");
  return FALSE;
}

static void append_axis_value(grid_axis_builder_t *builder,
    qunif_kind_t kind, grid_scalar_t value, int first_level,
    int maximum_size) {
  /*
   * Every built-in quantile map is monotone in its unit argument. Equal
   * realized values are consequently adjacent, including integer rounding,
   * fixed numeric bounds, signed zero, and the NaN interior of (-Inf, Inf).
   * Comparing only the last retained value keeps axis construction O(r).
   */
  if (axis_builder_contains_last(builder, kind, value)) return;
  grow_axis_builder(builder, maximum_size);
  builder->values[builder->size] = value;
  builder->first_levels[builder->size] = first_level;
  ++builder->size;
}

static SEXP realized_axis_vector(const grid_axis_builder_t *builder,
    qunif_kind_t kind, R_xlen_t *work_since_interrupt) {
  SEXP result = PROTECT(Rf_allocVector(
    output_type(kind),
    (R_xlen_t) builder->size
  ));
  for (R_xlen_t index = 0;
      index < (R_xlen_t) builder->size;
      ++index) {
    paradox_account_work(work_since_interrupt);
    switch (kind) {
    case QUNIF_KIND_DBL:
      REAL(result)[index] = builder->values[index].real;
      break;
    case QUNIF_KIND_INT:
      INTEGER(result)[index] = builder->values[index].integer;
      break;
    case QUNIF_KIND_LGL:
      LOGICAL(result)[index] = builder->values[index].integer;
      break;
    case QUNIF_KIND_FCT:
      SET_STRING_ELT(result, index, builder->values[index].string);
      break;
    case QUNIF_KIND_UNKNOWN:
      UNPROTECT(1);
      Rf_error("Internal error: unknown realized grid kind");
    }
  }
  UNPROTECT(1);
  return result;
}

static void build_realized_axis(grid_axis_t *axis, int resolution,
    int *warn_integer_range, R_xlen_t *work_since_interrupt) {
  grid_axis_builder_t builder;
  initialize_axis_builder(&builder, resolution);
  for (R_xlen_t level = 0;
      level < (R_xlen_t) resolution;
      ++level) {
    paradox_account_work(work_since_interrupt);
    const double unit = grid_unit_value((R_xlen_t) level, resolution);
    grid_scalar_t value;
    switch (axis->spec.kind) {
    case QUNIF_KIND_DBL:
      value.real = paradox_qunif_double_value(
        unit,
        axis->spec.lower,
        axis->spec.upper
      );
      break;
    case QUNIF_KIND_INT:
      if (!paradox_qunif_integer_value(
            unit,
            axis->spec.lower,
            axis->spec.upper,
            &value.integer
          )) {
        value.integer = NA_INTEGER;
        *warn_integer_range = TRUE;
      }
      break;
    case QUNIF_KIND_FCT: {
      const R_xlen_t choice = paradox_qunif_level_index(
        unit,
        XLENGTH(axis->spec.levels)
      );
      if (choice == R_XLEN_T_MAX) {
        Rf_error("Corrupt ParamSet realized grid mapping state");
      }
      value.string = STRING_ELT(axis->spec.levels, choice);
      break;
    }
    case QUNIF_KIND_LGL:
      value.integer = unit < 0.5;
      break;
    case QUNIF_KIND_UNKNOWN:
      Rf_error("Corrupt ParamSet realized grid mapping state");
    }
    append_axis_value(
      &builder,
      axis->spec.kind,
      value,
      (int) level,
      resolution
    );
  }
  axis->values = realized_axis_vector(
    &builder,
    axis->spec.kind,
    work_since_interrupt
  );
  axis->count = builder.size;
  axis->first_levels = builder.first_levels;
  axis->fixed = FALSE;
}

static SEXP fixed_axis_vector(SEXP value, SEXPTYPE storage_type) {
  int is_tune_token = FALSE;
  if (!paradox_api_opaque_leaf_class_matches(
      value,
      "TuneToken",
      &is_tune_token
    )) {
    Rf_error(
      "Grid generation cannot inspect a fixed value with malformed class metadata"
    );
  }
  if (is_tune_token) {
    Rf_error(
      "Grid generation cannot materialize a stored TuneToken value"
    );
  }
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  if (type == storage_type && !Rf_isObject(value) && !Rf_isS4(value) &&
      XLENGTH(value) == 1 && (type == LGLSXP || type == INTSXP ||
        type == REALSXP || type == STRSXP)) {
    SEXP result = PROTECT(Rf_allocVector(type, 1));
    switch (type) {
    case LGLSXP:
      LOGICAL(result)[0] = LOGICAL_ELT(value, 0);
      break;
    case INTSXP:
      INTEGER(result)[0] = INTEGER_ELT(value, 0);
      break;
    case REALSXP:
      REAL(result)[0] = REAL_ELT(value, 0);
      break;
    case STRSXP:
      SET_STRING_ELT(result, 0, STRING_ELT(value, 0));
      break;
    default:
      UNPROTECT(1);
      Rf_error("Internal error: invalid fixed grid scalar");
    }
    UNPROTECT(1);
    return result;
  }

  /*
   * A TuneToken, NULL, S4 special value, or other opaque admitted leaf is one
   * parameter value, irrespective of its representation's internal length.
   * Store it as one list-column cell instead of reproducing data.table's
   * accidental unlisting/coercion behavior from the old post-hoc overwrite.
   */
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(result, 0, value);
  UNPROTECT(1);
  return result;
}

static int map_fixed_values(const grid_state_t *state,
    const id_map_t *id_map, R_xlen_t parameter_count,
    R_xlen_t *fixed_by_parameter, R_xlen_t *work_since_interrupt) {
  for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
    fixed_by_parameter[parameter] = R_XLEN_T_MAX;
  }
  for (R_xlen_t value = 0; value < state->values_data.size; ++value) {
    paradox_account_work(work_since_interrupt);
    R_xlen_t parameter;
    if (!find_id(
          id_map,
          STRING_ELT(state->values_data.names, value),
          &parameter,
          work_since_interrupt
        ) || fixed_by_parameter[parameter] != R_XLEN_T_MAX) {
      return FALSE;
    }
    fixed_by_parameter[parameter] = value;
  }
  return TRUE;
}

static void build_grid_axes(const grid_state_t *state,
    const param_columns_t *columns, const id_map_t *id_map,
    SEXP resolution_names, const int *counts, qunif_spec_t *specs,
    const R_xlen_t *fixed_by_parameter, grid_axis_t *axes,
    R_xlen_t *axis_by_parameter, SEXP axis_roots,
    int *warn_integer_range, R_xlen_t *work_since_interrupt) {
  for (R_xlen_t parameter = 0; parameter < columns->size; ++parameter) {
    axis_by_parameter[parameter] = R_XLEN_T_MAX;
  }
  for (R_xlen_t column = 0; column < columns->size; ++column) {
    R_xlen_t parameter;
    if (!find_id(
          id_map,
          STRING_ELT(resolution_names, column),
          &parameter,
          work_since_interrupt
        )) {
      Rf_error("Internal error: unresolved grid parameter");
    }
    axes[column].spec = specs[parameter];
    axes[column].param_row = parameter;
    axis_by_parameter[parameter] = column;
    const R_xlen_t fixed = fixed_by_parameter[parameter];
    if (fixed != R_XLEN_T_MAX) {
      SEXP value = PROTECT(fixed_axis_vector(
        VECTOR_ELT(state->values_data.values, fixed),
        output_type(specs[parameter].kind)
      ));
      axes[column].values = value;
      axes[column].count = 1;
      axes[column].first_levels = paradox_temporary_alloc(
        1,
        sizeof(*axes[column].first_levels)
      );
      axes[column].first_levels[0] = 0;
      axes[column].fixed = TRUE;
      SET_VECTOR_ELT(axis_roots, column, value);
      UNPROTECT(1);
    } else {
      build_realized_axis(
        &axes[column],
        counts[parameter],
        warn_integer_range,
        work_since_interrupt
      );
      SET_VECTOR_ELT(axis_roots, column, axes[column].values);
    }
  }
}

static int parse_grid_upper_limit(SEXP upper_limit, int *supplied) {
  *supplied = upper_limit != R_NilValue;
  if (upper_limit == R_NilValue) return INT_MAX;
  if (Rf_isS4(upper_limit) || Rf_isObject(upper_limit) ||
      ALTREP(upper_limit) || !paradox_api_has_no_attributes(upper_limit) ||
      XLENGTH(upper_limit) != 1) {
    Rf_error("`upper_limit` must be NULL or one non-negative whole number");
  }
  if (TYPEOF(upper_limit) == INTSXP) {
    const int value = INTEGER_ELT(upper_limit, 0);
    if (value == NA_INTEGER || value < 0) {
      Rf_error("`upper_limit` must be NULL or one non-negative whole number");
    }
    return value;
  }
  if (TYPEOF(upper_limit) == REALSXP) {
    const double value = REAL_ELT(upper_limit, 0);
    if (!R_FINITE(value) || value < 0.0 || value > (double) INT_MAX ||
        floor(value) != value) {
      Rf_error("`upper_limit` must be NULL or one non-negative whole number");
    }
    return (int) value;
  }
  Rf_error("`upper_limit` must be NULL or one non-negative whole number");
  return 0;
}

static void grid_limit_error(R_xlen_t limit) {
  Rf_error(
    "Realized grid exceeds `upper_limit` of %.0f rows",
    (double) limit
  );
}

static void copy_axis_element(SEXP output, R_xlen_t output_row,
    SEXP values, R_xlen_t value_row) {
  switch (TYPEOF(values)) {
  case LGLSXP:
    LOGICAL(output)[output_row] = LOGICAL_ELT(values, value_row);
    break;
  case INTSXP:
    INTEGER(output)[output_row] = INTEGER_ELT(values, value_row);
    break;
  case REALSXP:
    REAL(output)[output_row] = REAL_ELT(values, value_row);
    break;
  case STRSXP:
    SET_STRING_ELT(output, output_row, STRING_ELT(values, value_row));
    break;
  case VECSXP:
    SET_VECTOR_ELT(output, output_row, VECTOR_ELT(values, value_row));
    break;
  default:
    Rf_error("Internal error: unsupported realized grid axis type");
  }
}

static SEXP build_empty_grid(const grid_axis_t *axes,
    R_xlen_t column_count, SEXP names,
    R_xlen_t *work_since_interrupt) {
  SEXP result = PROTECT(Rf_allocVector(VECSXP, column_count));
  for (R_xlen_t column = 0; column < column_count; ++column) {
    paradox_account_work(work_since_interrupt);
    SEXP values = axes == NULL
      ? R_NilValue
      : axes[column].values;
    const SEXPTYPE type = values == R_NilValue
      ? output_type(axes[column].spec.kind)
      : (SEXPTYPE) TYPEOF(values);
    SEXP output = PROTECT(Rf_allocVector(type, 0));
    SET_VECTOR_ELT(result, column, output);
    UNPROTECT(1);
  }
  SEXP prepared = PROTECT(set_table_attributes(result, names, 0));
  UNPROTECT(2);
  return prepared;
}

static SEXP build_independent_grid(const grid_axis_t *axes,
    R_xlen_t column_count, SEXP names, int upper_limit,
    int upper_limit_supplied, R_xlen_t *work_since_interrupt) {
  R_xlen_t rows = 1;
  for (R_xlen_t column = 0; column < column_count; ++column) {
    paradox_account_work(work_since_interrupt);
    const int count = axes[column].count;
    if (count == 0) {
      rows = 0;
      break;
    }
    if (rows > (R_xlen_t) upper_limit / (R_xlen_t) count) {
      if (upper_limit_supplied) grid_limit_error(upper_limit);
      Rf_error("Grid product exceeds the maximum data.frame row count");
    }
    rows *= (R_xlen_t) count;
  }
  if (rows == 0) {
    return build_empty_grid(axes, column_count, names, work_since_interrupt);
  }

  R_xlen_t *strides = paradox_temporary_alloc(
    column_count,
    sizeof(*strides)
  );
  R_xlen_t stride = 1;
  for (R_xlen_t remaining = column_count; remaining > 0; --remaining) {
    const R_xlen_t column = remaining - 1;
    strides[column] = stride;
    stride *= (R_xlen_t) axes[column].count;
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, column_count));
  for (R_xlen_t column = 0; column < column_count; ++column) {
    paradox_account_work(work_since_interrupt);
    SEXP values = axes[column].values;
    SEXP output = PROTECT(Rf_allocVector(
      (SEXPTYPE) TYPEOF(values),
      rows
    ));
    const R_xlen_t column_stride = strides[column];
    for (R_xlen_t row = 0; row < rows; ++row) {
      paradox_account_work(work_since_interrupt);
      const R_xlen_t value = (row / column_stride) %
        (R_xlen_t) axes[column].count;
      copy_axis_element(output, row, values, value);
    }
    SET_VECTOR_ELT(result, column, output);
    UNPROTECT(1);
  }
  SEXP prepared = PROTECT(set_table_attributes(result, names, rows));
  UNPROTECT(2);
  return prepared;
}

#define GRID_CHOICE_INACTIVE (-1)
#define GRID_CHOICE_UNASSIGNED (-2)

static int grid_condition_compatible(SEXP values, SEXP rhs) {
  const SEXPTYPE value_type = (SEXPTYPE) TYPEOF(values);
  const SEXPTYPE rhs_type = (SEXPTYPE) TYPEOF(rhs);
  const int value_numeric = value_type == LGLSXP ||
    value_type == INTSXP || value_type == REALSXP;
  const int rhs_numeric = rhs_type == LGLSXP ||
    rhs_type == INTSXP || rhs_type == REALSXP;
  return (value_numeric && rhs_numeric) ||
    (value_type == STRSXP && rhs_type == STRSXP);
}

static int grid_parameter_is_active(
    const paradox_dependency_graph_plan_t *plan,
    R_xlen_t parameter, const grid_axis_t *axes,
    const R_xlen_t *axis_by_parameter, const int *choices,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t incoming = plan->incoming_start[parameter];
      incoming < plan->incoming_start[parameter + 1];
      ++incoming) {
    paradox_account_work(work_since_interrupt);
    const paradox_dependency_graph_edge_t *edge =
      &plan->edges[plan->incoming_edges[incoming]];
    if (edge->parent == R_XLEN_T_MAX) return FALSE;
    if (edge->parent >= plan->parameter_count) {
      Rf_error("Corrupt ParamSet dependency topology");
    }
    const int choice = choices[edge->parent];
    if (choice == GRID_CHOICE_INACTIVE) return FALSE;
    if (choice == GRID_CHOICE_UNASSIGNED) {
      Rf_error("Internal error: dependency parent was not assigned");
    }
    const grid_axis_t *parent =
      &axes[axis_by_parameter[edge->parent]];
    if (choice < 0 || choice >= parent->count) return FALSE;

    SEXP values = parent->values;
    R_xlen_t value_index = (R_xlen_t) choice;
    if (TYPEOF(values) == VECSXP) {
      /*
       * Cross-storage fixed specials are represented as one list-column cell
       * to preserve their exact leaf. Compare the admitted scalar leaf through
       * the same built-in Condition authority; opaque/NULL/S4 leaves simply do
       * not satisfy a built-in predicate.
       */
      values = VECTOR_ELT(values, value_index);
      value_index = 0;
      if (values == R_NilValue ||
          !paradox_builtin_condition_scalar_supported(values, edge->rhs)) {
        return FALSE;
      }
    } else if (!grid_condition_compatible(values, edge->rhs)) {
      return FALSE;
    }
    if (!paradox_builtin_condition_element_matches(
        values,
        value_index,
        edge->rhs,
        work_since_interrupt
      )) {
      return FALSE;
    }
  }
  return TRUE;
}

static R_xlen_t enumerate_dependent_grid(
    const paradox_dependency_graph_plan_t *plan,
    const R_xlen_t *topological_order, const grid_axis_t *axes,
    const R_xlen_t *axis_by_parameter, R_xlen_t row_capacity,
    int capacity_is_user_limit, int *choice_matrix,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t parameter_count = plan->parameter_count;
  int *choices = paradox_temporary_alloc(
    parameter_count,
    sizeof(*choices)
  );
  int *next_branch = paradox_temporary_alloc(
    parameter_count,
    sizeof(*next_branch)
  );
  int *branch_count = paradox_temporary_alloc(
    parameter_count,
    sizeof(*branch_count)
  );
  unsigned char *active = paradox_temporary_alloc(
    parameter_count,
    sizeof(*active)
  );
  for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
    choices[parameter] = GRID_CHOICE_UNASSIGNED;
    next_branch[parameter] = GRID_CHOICE_UNASSIGNED;
  }

  R_xlen_t rows = 0;
  R_xlen_t depth = 0;
  while (TRUE) {
    paradox_account_work(work_since_interrupt);
    if (depth == parameter_count) {
      if (rows >= row_capacity) {
        if (choice_matrix != NULL) {
          Rf_error("Internal error: dependent grid count changed");
        }
        if (capacity_is_user_limit) grid_limit_error(row_capacity);
        Rf_error("Grid product exceeds the maximum data.frame row count");
      }
      if (choice_matrix != NULL) {
        for (R_xlen_t parameter = 0;
            parameter < parameter_count;
            ++parameter) {
          paradox_account_work(work_since_interrupt);
          choice_matrix[parameter * row_capacity + rows] =
            choices[parameter];
        }
      }
      ++rows;
      --depth;
      continue;
    }

    const R_xlen_t parameter = topological_order[depth];
    if (next_branch[depth] == GRID_CHOICE_UNASSIGNED) {
      active[depth] = (unsigned char) grid_parameter_is_active(
        plan,
        parameter,
        axes,
        axis_by_parameter,
        choices,
        work_since_interrupt
      );
      branch_count[depth] = active[depth]
        ? axes[axis_by_parameter[parameter]].count
        : 1;
      next_branch[depth] = 0;
    }

    if (next_branch[depth] < branch_count[depth]) {
      choices[parameter] = active[depth]
        ? next_branch[depth]
        : GRID_CHOICE_INACTIVE;
      ++next_branch[depth];
      ++depth;
      if (depth < parameter_count) {
        next_branch[depth] = GRID_CHOICE_UNASSIGNED;
      }
      continue;
    }

    choices[parameter] = GRID_CHOICE_UNASSIGNED;
    next_branch[depth] = GRID_CHOICE_UNASSIGNED;
    if (depth == 0) break;
    --depth;
  }
  return rows;
}

static int compare_grid_rows(R_xlen_t left, R_xlen_t right,
    const int *choice_matrix, R_xlen_t row_count,
    const grid_axis_t *axes, R_xlen_t column_count) {
  for (R_xlen_t column = 0; column < column_count; ++column) {
    const grid_axis_t *axis = &axes[column];
    const R_xlen_t parameter = axis->param_row;
    const int left_choice =
      choice_matrix[parameter * row_count + left];
    const int right_choice =
      choice_matrix[parameter * row_count + right];
    const int left_rank = left_choice < 0 || axis->fixed
      ? 0
      : axis->first_levels[left_choice];
    const int right_rank = right_choice < 0 || axis->fixed
      ? 0
      : axis->first_levels[right_choice];
    if (left_rank < right_rank) return -1;
    if (left_rank > right_rank) return 1;
  }
  return 0;
}

static R_xlen_t *stable_grid_row_order(const int *choice_matrix,
    R_xlen_t row_count, const grid_axis_t *axes,
    R_xlen_t column_count, R_xlen_t *work_since_interrupt) {
  R_xlen_t *order = paradox_temporary_alloc(
    row_count == 0 ? 1 : row_count,
    sizeof(*order)
  );
  R_xlen_t *scratch = paradox_temporary_alloc(
    row_count == 0 ? 1 : row_count,
    sizeof(*scratch)
  );
  for (R_xlen_t row = 0; row < row_count; ++row) {
    order[row] = row;
  }

  R_xlen_t *source = order;
  R_xlen_t *target = scratch;
  for (R_xlen_t width = 1; width < row_count;) {
    for (R_xlen_t start = 0; start < row_count;) {
      const R_xlen_t middle = width > row_count - start
        ? row_count
        : start + width;
      const R_xlen_t remaining = row_count - middle;
      const R_xlen_t end = width > remaining
        ? row_count
        : middle + width;
      R_xlen_t left = start;
      R_xlen_t right = middle;
      R_xlen_t output = start;
      while (left < middle || right < end) {
        paradox_account_work(work_since_interrupt);
        if (right == end || (left < middle && compare_grid_rows(
              source[left],
              source[right],
              choice_matrix,
              row_count,
              axes,
              column_count
            ) <= 0)) {
          target[output++] = source[left++];
        } else {
          target[output++] = source[right++];
        }
      }
      start = end;
    }
    R_xlen_t *swap = source;
    source = target;
    target = swap;
    if (width > row_count / 2) {
      width = row_count;
    } else {
      width *= 2;
    }
  }
  if (source != order && row_count != 0) {
    memcpy(order, source, (size_t) row_count * sizeof(*order));
  }
  return order;
}

static void set_grid_missing(SEXP output, R_xlen_t row,
    SEXP list_missing) {
  switch (TYPEOF(output)) {
  case LGLSXP:
    LOGICAL(output)[row] = NA_LOGICAL;
    break;
  case INTSXP:
    INTEGER(output)[row] = NA_INTEGER;
    break;
  case REALSXP:
    REAL(output)[row] = NA_REAL;
    break;
  case STRSXP:
    SET_STRING_ELT(output, row, NA_STRING);
    break;
  case VECSXP:
    SET_VECTOR_ELT(output, row, list_missing);
    break;
  default:
    Rf_error("Internal error: unsupported dependent grid column type");
  }
}

static SEXP build_dependent_grid(
    const paradox_dependency_graph_plan_t *plan,
    const grid_axis_t *axes, const R_xlen_t *axis_by_parameter,
    SEXP names, int upper_limit, int upper_limit_supplied,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t parameter_count = plan->parameter_count;
  R_xlen_t *branch_factors = paradox_temporary_alloc(
    parameter_count,
    sizeof(*branch_factors)
  );
  R_xlen_t *topological_order = paradox_temporary_alloc(
    parameter_count,
    sizeof(*topological_order)
  );
  for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
    const R_xlen_t axis = axis_by_parameter[parameter];
    if (axis == R_XLEN_T_MAX) {
      Rf_error("Internal error: grid parameter has no realized axis");
    }
    branch_factors[parameter] = (R_xlen_t) axes[axis].count;
  }
  paradox_dependency_graph_topological_order(
    plan,
    branch_factors,
    topological_order,
    work_since_interrupt
  );

  /*
   * Count exactly before allocating. Unlike the celecx prototype, the ceiling
   * is checked only at complete leaves, so a later empty active branch can
   * still reduce the final design. Nominal zero axes have already taken the
   * stronger legacy short-circuit above.
   */
  const R_xlen_t row_count = enumerate_dependent_grid(
    plan,
    topological_order,
    axes,
    axis_by_parameter,
    (R_xlen_t) upper_limit,
    upper_limit_supplied,
    NULL,
    work_since_interrupt
  );
  if (row_count == 0) {
    return build_empty_grid(
      axes,
      parameter_count,
      names,
      work_since_interrupt
    );
  }
  if (parameter_count > R_XLEN_T_MAX / row_count) {
    Rf_error("Dependent grid choice matrix is too large");
  }
  int *choice_matrix = paradox_temporary_alloc(
    parameter_count * row_count,
    sizeof(*choice_matrix)
  );
  const R_xlen_t filled = enumerate_dependent_grid(
    plan,
    topological_order,
    axes,
    axis_by_parameter,
    row_count,
    FALSE,
    choice_matrix,
    work_since_interrupt
  );
  if (filled != row_count) {
    Rf_error("Internal error: dependent grid count changed");
  }
  /*
   * When DFS assigned parameters in public axis order, its rows are already
   * ordered by the retained first nominal level. This is the common case for
   * dependency chains and avoids an O(n log n) compatibility sort plus two
   * row-sized index vectors. A branch-factor reordering still takes the exact
   * stable sort below.
   */
  int public_order = TRUE;
  for (R_xlen_t column = 0; column < parameter_count; ++column) {
    if (topological_order[column] != axes[column].param_row) {
      public_order = FALSE;
      break;
    }
  }
  R_xlen_t *order = public_order
    ? NULL
    : stable_grid_row_order(
        choice_matrix,
        row_count,
        axes,
        parameter_count,
        work_since_interrupt
      );

  SEXP result = PROTECT(Rf_allocVector(VECSXP, parameter_count));
  for (R_xlen_t column = 0; column < parameter_count; ++column) {
    paradox_account_work(work_since_interrupt);
    const grid_axis_t *axis = &axes[column];
    SEXP output = PROTECT(Rf_allocVector(
      (SEXPTYPE) TYPEOF(axis->values),
      row_count
    ));
    SEXP list_missing = R_NilValue;
    if (TYPEOF(output) == VECSXP) {
      list_missing = PROTECT(Rf_ScalarLogical(NA_LOGICAL));
    }
    for (R_xlen_t row = 0; row < row_count; ++row) {
      paradox_account_work(work_since_interrupt);
      const R_xlen_t generated_row = order == NULL ? row : order[row];
      const int choice = choice_matrix[
        axis->param_row * row_count + generated_row
      ];
      if (choice == GRID_CHOICE_INACTIVE) {
        set_grid_missing(output, row, list_missing);
      } else if (choice >= 0 && choice < axis->count) {
        copy_axis_element(
          output,
          row,
          axis->values,
          (R_xlen_t) choice
        );
      } else {
        if (TYPEOF(output) == VECSXP) UNPROTECT(1);
        UNPROTECT(2);
        Rf_error("Internal error: invalid dependent grid choice");
      }
    }
    SET_VECTOR_ELT(result, column, output);
    if (TYPEOF(output) == VECSXP) UNPROTECT(1);
    UNPROTECT(1);
  }
  SEXP prepared = PROTECT(set_table_attributes(
    result,
    names,
    row_count
  ));
  UNPROTECT(2);
  return prepared;
}

SEXP paradox_generate_design_grid_builtin(SEXP private_environment, SEXP self,
    SEXP controls, SEXP upper_limit) {
  int protected_count = 0;
  PROTECT(private_environment);
  ++protected_count;
  PROTECT(self);
  ++protected_count;
  PROTECT(controls);
  ++protected_count;
  PROTECT(upper_limit);
  ++protected_count;

  if (TYPEOF(controls) != VECSXP || ALTREP(controls) ||
      Rf_isS4(controls) || Rf_isObject(controls) ||
      XLENGTH(controls) != 2 ||
      !paradox_api_has_single_attribute(controls, "names")) {
    Rf_error("Invalid grid resolution controls");
  }
  SEXP control_names = paradox_api_raw_attribute(
    controls,
    R_NamesSymbol
  );
  if (TYPEOF(control_names) != STRSXP || ALTREP(control_names) ||
      Rf_isS4(control_names) || Rf_isObject(control_names) ||
      !paradox_api_has_no_attributes(control_names) ||
      XLENGTH(control_names) != 2 ||
      !paradox_domain_string_is(
        STRING_ELT(control_names, 0),
        "resolution"
      ) ||
      !paradox_domain_string_is(
        STRING_ELT(control_names, 1),
        "param_resolutions"
      )) {
    Rf_error("Invalid grid resolution controls");
  }
  SEXP resolution = VECTOR_ELT(controls, 0);
  SEXP param_resolutions = VECTOR_ELT(controls, 1);
  /*
   * `param_resolutions` is used after the two destination allocations below.
   * Root the selected control generation itself: a pending finalizer may
   * rewrite the caller-owned controls list during either allocation and
   * otherwise leave this local pointer unreachable.
   */
  PROTECT(param_resolutions);
  ++protected_count;
  int global_supplied = FALSE;
  const int global_count = parse_grid_resolution(
    resolution,
    &global_supplied
  );
  R_xlen_t override_count = 0;
  if (param_resolutions != R_NilValue) {
    const SEXPTYPE type = (SEXPTYPE) TYPEOF(param_resolutions);
    if ((type != INTSXP && type != REALSXP) ||
        ALTREP(param_resolutions) || Rf_isS4(param_resolutions) ||
        Rf_isObject(param_resolutions)) {
      Rf_error(
        "`param_resolutions` must be a named numeric vector of non-negative whole numbers"
      );
    }
    override_count = XLENGTH(param_resolutions);
  }
  /*
   * Allocate both resolution destinations before observing the paired public
   * names/values. All callback-capable controls are thereby settled before
   * selecting the ParamSet generation used by the rest of the operation.
   */
  SEXP override_names = PROTECT(Rf_allocVector(STRSXP, override_count));
  ++protected_count;
  SEXP override_counts = PROTECT(Rf_allocVector(INTSXP, override_count));
  ++protected_count;
  snapshot_grid_resolution_overrides(
    param_resolutions,
    override_names,
    override_counts
  );

  int upper_limit_supplied = FALSE;
  const int maximum_rows = parse_grid_upper_limit(
    upper_limit,
    &upper_limit_supplied
  );
  R_xlen_t work_since_interrupt = 0;
  SEXP state_roots = PROTECT(Rf_allocVector(
    VECSXP,
    GRID_STATE_ROOT_COUNT
  ));
  ++protected_count;
  PROTECT_INDEX graph_roots_index;
  SEXP graph_roots;
  PROTECT_WITH_INDEX(graph_roots = R_NilValue, &graph_roots_index);
  ++protected_count;
  grid_state_t state = {0};
  load_grid_state(
    private_environment,
    self,
    &state,
    state_roots,
    &graph_roots,
    graph_roots_index,
    &work_since_interrupt
  );

  param_columns_t columns;
  SEXP column_roots = PROTECT(Rf_allocVector(VECSXP, QUNIF_ROOT_COUNT));
  ++protected_count;
  if (!load_param_columns(state.params, &columns, column_roots)) {
    Rf_error("Corrupt ParamSet grid state: invalid parameter schema");
  }
  paradox_dependency_graph_plan_t dependency_plan = {0};
  if (columns.size == 0) {
    if (state.dependencies_data.row_count != 0) {
      /*
       * A canonical zero-dimensional schema cannot own an edge. Route the
       * impossible state through the same mapper so empty-grid handling does
       * not conceal an unknown child or another dependency corruption.
       */
      paradox_dependency_graph_plan_build(
        columns.ids,
        &state.dependencies_data,
        &dependency_plan,
        &work_since_interrupt
      );
    }
    SEXP stable_names = PROTECT(Rf_allocVector(STRSXP, 0));
    ++protected_count;
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 0));
    ++protected_count;
    SEXP prepared = PROTECT(set_table_attributes(result, stable_names, 0));
    ++protected_count;
    SEXP bundled = PROTECT(grid_result_with_receipt(
      prepared,
      state.receipt
    ));
    ++protected_count;
    UNPROTECT(protected_count);
    return bundled;
  }

  id_map_t id_map;
  if (!initialize_id_map(columns.ids, &id_map)) {
    Rf_error("Corrupt ParamSet grid state: duplicate parameter IDs");
  }

  qunif_spec_t *specs = paradox_temporary_alloc(
    columns.size,
    sizeof(*specs)
  );
  int *counts = paradox_temporary_alloc(columns.size, sizeof(*counts));
  unsigned char *overridden = paradox_temporary_alloc(
    columns.size,
    sizeof(*overridden)
  );
  SEXP spec_roots = PROTECT(Rf_allocVector(VECSXP, columns.size));
  ++protected_count;
  SEXP stable_resolution_names = PROTECT(Rf_allocVector(
    STRSXP,
    columns.size
  ));
  ++protected_count;
  load_grid_specs(
    &columns,
    &id_map,
    global_supplied,
    global_count,
    override_names,
    override_counts,
    stable_resolution_names,
    specs,
    counts,
    overridden,
    spec_roots,
    &work_since_interrupt
  );
  /*
   * A global resolution retains canonical ParamSet order: per-axis controls
   * merely override counts. With no global control, the historical Cartesian
   * order follows the explicit numeric control order and then the remaining
   * categorical axes. A nominally empty axis is the one exception: typed
   * empty designs have always exposed canonical schema order.
   */
  int nominally_empty = FALSE;
  for (R_xlen_t row = 0; row < columns.size; ++row) {
    if (counts[row] == 0) {
      nominally_empty = TRUE;
      break;
    }
  }
  if (nominally_empty) {
    for (R_xlen_t row = 0; row < columns.size; ++row) {
      SET_STRING_ELT(
        stable_resolution_names,
        row,
        STRING_ELT(columns.ids, row)
      );
    }
  }
  if (state.dependencies_data.row_count != 0) {
    /*
     * Topology is part of ParamSet admission, not a consequence of producing
     * rows. Validate it before the nominal-zero return so an empty grid cannot
     * hide an admitted dependency cycle.
     */
    paradox_dependency_graph_plan_build(
      columns.ids,
      &state.dependencies_data,
      &dependency_plan,
      &work_since_interrupt
    );
  }

  grid_axis_t *axes = paradox_temporary_alloc(
    columns.size,
    sizeof(*axes)
  );
  for (R_xlen_t column = 0; column < columns.size; ++column) {
    R_xlen_t parameter;
    if (!find_id(
          &id_map,
          STRING_ELT(stable_resolution_names, column),
          &parameter,
          &work_since_interrupt
        )) {
      Rf_error("Internal error: unresolved grid axis order");
    }
    axes[column] = (grid_axis_t) {
      .spec = specs[parameter],
      .values = R_NilValue,
      .first_levels = NULL,
      .param_row = parameter,
      .count = counts[parameter],
      .fixed = FALSE
    };
  }
  SEXP result_names = PROTECT(copy_column_names(
    stable_resolution_names,
    &work_since_interrupt
  ));
  ++protected_count;

  /*
   * Preserve the established nominal-zero contract before consulting fixed
   * values: a requested zero-resolution axis or zero-level factor makes the
   * complete grid empty even when that parameter has a stored value.
   */
  for (R_xlen_t column = 0; column < columns.size; ++column) {
    if (counts[column] == 0) {
      SEXP prepared = PROTECT(build_empty_grid(
        axes,
        columns.size,
        result_names,
        &work_since_interrupt
      ));
      ++protected_count;
      SEXP bundled = PROTECT(grid_result_with_receipt(
        prepared,
        state.receipt
      ));
      ++protected_count;
      UNPROTECT(protected_count);
      return bundled;
    }
  }

  R_xlen_t *fixed_by_parameter = paradox_temporary_alloc(
    columns.size,
    sizeof(*fixed_by_parameter)
  );
  if (!map_fixed_values(
        &state,
        &id_map,
        columns.size,
        fixed_by_parameter,
        &work_since_interrupt
      )) {
    Rf_error("Corrupt ParamSet grid state: invalid stored value IDs");
  }
  R_xlen_t *axis_by_parameter = paradox_temporary_alloc(
    columns.size,
    sizeof(*axis_by_parameter)
  );
  SEXP axis_roots = PROTECT(Rf_allocVector(VECSXP, columns.size));
  ++protected_count;
  int warn_integer_range = FALSE;
  build_grid_axes(
    &state,
    &columns,
    &id_map,
    stable_resolution_names,
    counts,
    specs,
    fixed_by_parameter,
    axes,
    axis_by_parameter,
    axis_roots,
    &warn_integer_range,
    &work_since_interrupt
  );

  SEXP prepared = PROTECT(state.dependencies_data.row_count == 0
    ? build_independent_grid(
        axes,
        columns.size,
        result_names,
        maximum_rows,
        upper_limit_supplied,
        &work_since_interrupt
      )
    : build_dependent_grid(
        &dependency_plan,
        axes,
        axis_by_parameter,
        result_names,
        maximum_rows,
        upper_limit_supplied,
        &work_since_interrupt
      ));
  ++protected_count;
  if (warn_integer_range) {
    Rf_warning("NAs introduced by coercion to integer range");
  }
  SEXP bundled = PROTECT(grid_result_with_receipt(
    prepared,
    state.receipt
  ));
  ++protected_count;
  UNPROTECT(protected_count);
  return bundled;
}
