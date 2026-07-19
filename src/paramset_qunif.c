#include <math.h>
#include <limits.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Arith.h>
#include <R_ext/Utils.h>

#include "core_state.h"
#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

enum param_column {
  PARAM_ID = 0,
  PARAM_CLS,
  PARAM_GROUPING,
  PARAM_CARGO,
  PARAM_LOWER,
  PARAM_UPPER,
  PARAM_TOLERANCE,
  PARAM_LEVELS,
  PARAM_SPECIAL_VALS,
  PARAM_DEFAULT,
  PARAM_STORAGE_TYPE,
  PARAM_COLUMN_COUNT
};

enum qunif_column_root {
  QUNIF_ROOT_COLUMNS = 0,
  QUNIF_ROOT_COUNT = PARAM_COLUMN_COUNT
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

static inline void account_work(R_xlen_t *work_since_interrupt) {
  ++*work_since_interrupt;
  if (*work_since_interrupt >= PARADOX_INTERRUPT_CHECK_INTERVAL) {
    R_CheckUserInterrupt();
    *work_since_interrupt = 0;
  }
}

static int exact_string(SEXP string, const char *expected) {
  return string != NA_STRING && strcmp(CHAR(string), expected) == 0;
}

static char *copy_utf8_string(SEXP string) {
  const size_t size = strlen(Rf_translateCharUTF8(string));
  if (size >= (size_t) R_XLEN_T_MAX) {
    Rf_error("Unable to copy a canonical string");
  }
  char *copy = paradox_temporary_alloc(
    (R_xlen_t) size + 1,
    sizeof(*copy)
  );
  memcpy(copy, Rf_translateCharUTF8(string), size + 1U);
  return copy;
}

static int strings_equal(SEXP left, SEXP right) {
  if (left == right) {
    return TRUE;
  }
  if (left == NA_STRING || right == NA_STRING) {
    return FALSE;
  }

  const cetype_t left_encoding = Rf_getCharCE(left);
  const cetype_t right_encoding = Rf_getCharCE(right);
  if (left_encoding == CE_BYTES || right_encoding == CE_BYTES) {
    return left_encoding == CE_BYTES && right_encoding == CE_BYTES &&
      strcmp(CHAR(left), CHAR(right)) == 0;
  }

  PROTECT(left);
  PROTECT(right);
  const void *vmax = vmaxget();
  const char *left_text = copy_utf8_string(left);
  const char *right_text = Rf_translateCharUTF8(right);
  const int equal = strcmp(left_text, right_text) == 0;
  vmaxset(vmax);
  UNPROTECT(2);
  return equal;
}

static R_xlen_t qunif_column_root(enum param_column column) {
  return (R_xlen_t) QUNIF_ROOT_COLUMNS + (R_xlen_t) column;
}

static double numeric_at(SEXP column, R_xlen_t index) {
  if (TYPEOF(column) == REALSXP) {
    return REAL_ELT(column, index);
  }
  const int value = INTEGER_ELT(column, index);
  return value == NA_INTEGER ? NA_REAL : (double) value;
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

  for (R_xlen_t column = 0; column < PARAM_COLUMN_COUNT; ++column) {
    SEXP value = PROTECT(VECTOR_ELT(params, column));
    SET_VECTOR_ELT(
      roots,
      qunif_column_root((enum param_column) column),
      value
    );
    UNPROTECT(1);
  }

  columns->ids = VECTOR_ELT(
    roots,
    qunif_column_root(PARAM_ID)
  );
  columns->size = checked.row_count;

  columns->classes = VECTOR_ELT(
    roots,
    qunif_column_root(PARAM_CLS)
  );
  columns->grouping = VECTOR_ELT(
    roots,
    qunif_column_root(PARAM_GROUPING)
  );
  columns->lower = VECTOR_ELT(
    roots,
    qunif_column_root(PARAM_LOWER)
  );
  columns->upper = VECTOR_ELT(
    roots,
    qunif_column_root(PARAM_UPPER)
  );
  columns->tolerance = VECTOR_ELT(
    roots,
    qunif_column_root(PARAM_TOLERANCE)
  );
  columns->levels = VECTOR_ELT(
    roots,
    qunif_column_root(PARAM_LEVELS)
  );
  columns->storage_types = VECTOR_ELT(
    roots,
    qunif_column_root(PARAM_STORAGE_TYPE)
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
    account_work(&work_since_interrupt);
    slots[slot].hash = 0;
    slots[slot].row_plus_one = 0;
  }

  const R_xlen_t mask = capacity - 1;
  for (R_xlen_t row = 0; row < size; ++row) {
    account_work(&work_since_interrupt);
    SEXP id = PROTECT(STRING_ELT(ids, row));
    const uint64_t hash = hash_string(id);
    R_xlen_t slot = (R_xlen_t) (hash & (uint64_t) mask);
    while (slots[slot].row_plus_one != 0) {
      account_work(&work_since_interrupt);
      const R_xlen_t present = slots[slot].row_plus_one - 1;
      if (slots[slot].hash == hash &&
          strings_equal(STRING_ELT(ids, present), id)) {
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
    account_work(work_since_interrupt);
    const R_xlen_t present = map->slots[slot].row_plus_one - 1;
    if (map->slots[slot].hash == hash &&
        strings_equal(STRING_ELT(map->ids, present), id)) {
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
    account_work(work_since_interrupt);
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

static void snapshot_matrix_input(SEXP x, qunif_input_t *info, SEXP roots,
    R_xlen_t *work_since_interrupt) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(x);
  if ((type != REALSXP && type != INTSXP) || Rf_isS4(x) ||
      Rf_isObject(x)) {
    Rf_error("`x` must be a numeric matrix or data.frame");
  }

  SEXP dimensions = PROTECT(ALTREP(x)
    ? paradox_stored_attribute(x, R_DimSymbol)
    : Rf_getAttrib(x, R_DimSymbol));
  if (TYPEOF(dimensions) != INTSXP || ALTREP(dimensions) ||
      Rf_isS4(dimensions) || Rf_isObject(dimensions) ||
      !paradox_api_has_no_attributes(dimensions) ||
      XLENGTH(dimensions) != 2) {
    UNPROTECT(1);
    Rf_error("`x` must be a numeric matrix or data.frame");
  }
  const int row_count = INTEGER_ELT(dimensions, 0);
  const int column_count = INTEGER_ELT(dimensions, 1);
  if (row_count < 0 || column_count <= 0) {
    UNPROTECT(1);
    Rf_error("`x` must have at least one column");
  }
  info->rows = (R_xlen_t) row_count;
  info->columns = (R_xlen_t) column_count;
  const R_xlen_t size = checked_input_size(info->rows, info->columns);
  if (XLENGTH(x) != size) {
    UNPROTECT(1);
    Rf_error("`x` has inconsistent matrix dimensions");
  }

  SEXP dimension_names = PROTECT(ALTREP(x)
    ? paradox_stored_attribute(x, R_DimNamesSymbol)
    : Rf_getAttrib(x, R_DimNamesSymbol));
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
  SET_VECTOR_ELT(roots, QUNIF_INPUT_SOURCE, x);

  SEXP stable_values = PROTECT(Rf_allocVector(REALSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    account_work(work_since_interrupt);
    const double unit = numeric_at(x, index);
    require_unit_interval(unit);
    SET_REAL_ELT(stable_values, index, unit);
  }
  SET_VECTOR_ELT(roots, QUNIF_INPUT_VALUES, stable_values);
  info->column_names = VECTOR_ELT(roots, QUNIF_INPUT_NAMES);
  info->values = VECTOR_ELT(roots, QUNIF_INPUT_VALUES);
  UNPROTECT(5);
}

static int ordinary_frame_shell(SEXP x) {
  if (TYPEOF(x) != VECSXP || ALTREP(x) || Rf_isS4(x)) return FALSE;
  SEXP classes = PROTECT(Rf_getAttrib(x, R_ClassSymbol));
  const R_xlen_t count = ordinary_character_metadata(classes, FALSE)
    ? XLENGTH(classes)
    : 0;
  const int data_frame = count == 1 && paradox_domain_string_is(
    STRING_ELT(classes, 0),
    "data.frame"
  );
  const int data_table = count == 2 && paradox_domain_string_is(
      STRING_ELT(classes, 0),
      "data.table"
    ) && paradox_domain_string_is(
      STRING_ELT(classes, 1),
      "data.frame"
    );
  static const char *const frame_attributes[] = {
    "names", "row.names", "class"
  };
  static const char *const table_attributes[] = {
    "names", "row.names", "class", ".internal.selfref", "sorted", "index"
  };
  int valid = (data_frame && paradox_api_has_only_attributes(
      x,
      frame_attributes,
      3
    )) || (data_table && paradox_api_has_only_attributes(
      x,
      table_attributes,
      6
    ));
  UNPROTECT(1);
  if (valid) {
    SEXP names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
    valid = ordinary_character_metadata(names, FALSE) &&
      XLENGTH(names) == XLENGTH(x);
    UNPROTECT(1);
  }
  return valid;
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

  /* Own every observed column before invoking an ALTREP Length/Elt method.
   * A callback may replace a data.frame column, but this operation must finish
   * from the single input generation selected at entry. */
  SEXP source_columns = PROTECT(Rf_allocVector(VECSXP, info->columns));
  for (R_xlen_t column = 0; column < info->columns; ++column) {
    account_work(work_since_interrupt);
    SEXP source = PROTECT(VECTOR_ELT(x, column));
    const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
    if ((type != REALSXP && type != INTSXP) || Rf_isObject(source)) {
      UNPROTECT(2);
      Rf_error("Every column of `x` must be an unclassed numeric vector");
    }
    SET_VECTOR_ELT(source_columns, column, source);
    UNPROTECT(1);
  }
  SET_VECTOR_ELT(roots, QUNIF_INPUT_SOURCE, source_columns);

  info->rows = XLENGTH(VECTOR_ELT(source_columns, 0));
  if (info->rows > INT_MAX) {
    UNPROTECT(1);
    Rf_error("`x` has too many rows for a data.frame result");
  }
  for (R_xlen_t column = 1; column < info->columns; ++column) {
    account_work(work_since_interrupt);
    if (XLENGTH(VECTOR_ELT(source_columns, column)) != info->rows) {
      UNPROTECT(1);
      Rf_error("Columns of `x` must have equal lengths");
    }
  }
  const R_xlen_t size = checked_input_size(info->rows, info->columns);

  SEXP source_names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
  snapshot_input_names(
    source_names,
    info->columns,
    FALSE,
    roots,
    work_since_interrupt
  );

  SEXP stable_values = PROTECT(Rf_allocVector(REALSXP, size));
  for (R_xlen_t column = 0; column < info->columns; ++column) {
    SEXP source = VECTOR_ELT(source_columns, column);
    for (R_xlen_t row = 0; row < info->rows; ++row) {
      account_work(work_since_interrupt);
      const double unit = numeric_at(source, row);
      require_unit_interval(unit);
      SET_REAL_ELT(
        stable_values,
        column * info->rows + row,
        unit
      );
    }
  }
  SET_VECTOR_ELT(roots, QUNIF_INPUT_VALUES, stable_values);
  info->column_names = VECTOR_ELT(roots, QUNIF_INPUT_NAMES);
  info->values = VECTOR_ELT(roots, QUNIF_INPUT_VALUES);
  UNPROTECT(3);
}

static void snapshot_qunif_input(SEXP x, qunif_input_t *info, SEXP roots) {
  R_xlen_t work_since_interrupt = 0;
  if (ordinary_frame_shell(x)) {
    snapshot_frame_input(x, info, roots, &work_since_interrupt);
  } else {
    snapshot_matrix_input(x, info, roots, &work_since_interrupt);
  }
}

static SEXP snapshot_factor_levels(SEXP levels,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(levels) != STRSXP) {
    return R_NilValue;
  }
  const R_xlen_t size = XLENGTH(levels);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t level = 0; level < size; ++level) {
    account_work(work_since_interrupt);
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

  if (exact_string(class_name, "ParamDbl") &&
      exact_string(storage_type, "numeric")) {
    spec->kind = QUNIF_KIND_DBL;
  } else if (exact_string(class_name, "ParamInt") &&
      exact_string(storage_type, "integer")) {
    spec->kind = QUNIF_KIND_INT;
  } else if (exact_string(class_name, "ParamFct") &&
      exact_string(storage_type, "character")) {
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
  } else if (exact_string(class_name, "ParamLgl") &&
      exact_string(storage_type, "logical") &&
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

  spec->lower = numeric_at(columns->lower, row);
  spec->upper = numeric_at(columns->upper, row);
  const double tolerance = numeric_at(columns->tolerance, row);
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
    account_work(work_since_interrupt);
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
    account_work(work_since_interrupt);
    const double unit = numeric_at(x, input_offset + row);
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

static void snapshot_grid_resolutions(SEXP resolutions, SEXP stable_names,
    int *counts) {
  if (!paradox_api_has_single_attribute(resolutions, "names")) {
    Rf_error("`resolutions` must have exactly one `names` attribute");
  }
  SEXP names = Rf_getAttrib(resolutions, R_NamesSymbol);
  if (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isObject(names) ||
      XLENGTH(names) != XLENGTH(resolutions) ||
      !paradox_api_has_no_attributes(names)) {
    Rf_error("`resolutions` must have ordinary character names");
  }

  /* Everything above and below this loop is allocation-free. Once copied,
   * later factor-level materialization and character translation use only
   * these stable names and counts, so a GC finalizer cannot create a grid from
   * a mixture of pre- and post-callback resolution state. */
  for (R_xlen_t index = 0; index < XLENGTH(resolutions); ++index) {
    int count;
    SEXP name = STRING_ELT(names, index);
    if (name == NA_STRING) {
      Rf_error("`resolutions` names must not be missing");
    }
    if (!grid_resolution_at(resolutions, index, &count)) {
      Rf_error(
        "`resolutions` must contain non-negative whole numbers no greater than INT_MAX"
      );
    }
    counts[index] = count;
    SET_STRING_ELT(stable_names, index, name);
  }
}

static void load_grid_specs(const param_columns_t *columns,
    const id_map_t *id_map, SEXP resolution_names,
    qunif_spec_t *specs, const int *counts, R_xlen_t *strides,
    unsigned char *selected, SEXP spec_roots, R_xlen_t *rows,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t row = 0; row < columns->size; ++row) {
    account_work(work_since_interrupt);
    selected[row] = 0;
  }

  for (R_xlen_t column = 0; column < columns->size; ++column) {
    account_work(work_since_interrupt);
    const SEXP name = STRING_ELT(resolution_names, column);
    const int count = counts[column];
    R_xlen_t param_row;
    if (!find_id(
          id_map,
          name,
          &param_row,
          work_since_interrupt
        )) {
      Rf_error("`resolutions` names must match ParamSet IDs");
    }
    if (selected[param_row]) {
      Rf_error("`resolutions` names must be unique");
    }
    if (!load_spec(
          columns,
          param_row,
          &specs[column],
          spec_roots,
          column,
          work_since_interrupt
        )) {
      if (exact_string(
          STRING_ELT(columns->classes, param_row),
          "ParamUty"
        )) {
        Rf_error("Grid generation is undefined for ParamUty");
      }
      Rf_error("Corrupt ParamSet grid quantile state");
    }
    if ((specs[column].kind == QUNIF_KIND_FCT &&
          (R_xlen_t) count != XLENGTH(specs[column].levels)) ||
        (specs[column].kind == QUNIF_KIND_LGL && count != 2)) {
      Rf_error(
        "Categorical grid resolution must equal the number of levels"
      );
    }
    selected[param_row] = 1;
  }

  R_xlen_t total = 1;
  for (R_xlen_t remaining = columns->size; remaining > 0; --remaining) {
    const R_xlen_t column = remaining - 1;
    account_work(work_since_interrupt);
    strides[column] = total;
    if (counts[column] == 0) {
      total = 0;
      continue;
    }
    if (total > (R_xlen_t) INT_MAX / (R_xlen_t) counts[column]) {
      Rf_error("Grid product exceeds the maximum data.frame row count");
    }
    total *= (R_xlen_t) counts[column];
  }
  *rows = total;
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

static int fill_grid_column(SEXP output, R_xlen_t rows, int resolution,
    R_xlen_t stride, const qunif_spec_t *spec,
    int *warn_integer_range, R_xlen_t *work_since_interrupt) {
  if (rows != 0 && (resolution <= 0 || stride <= 0)) {
    return FALSE;
  }

  for (R_xlen_t row = 0; row < rows; ++row) {
    account_work(work_since_interrupt);
    const R_xlen_t level = (row / stride) % (R_xlen_t) resolution;
    const double unit = grid_unit_value(level, resolution);
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
      const R_xlen_t choice = paradox_qunif_level_index(
        unit,
        XLENGTH(spec->levels)
      );
      if (choice == R_XLEN_T_MAX) {
        return FALSE;
      }
      SET_STRING_ELT(output, row, STRING_ELT(spec->levels, choice));
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
  const paradox_core_kind_t kind = paradox_core_kind(core);
  if (kind == PARADOX_CORE_SHADOW) {
    core = paradox_core_refresh_shadow(self, private_environment);
  } else if (kind != PARADOX_CORE_BASE &&
      kind != PARADOX_CORE_COLLECTION) {
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
    account_work(&work_since_interrupt);
    selected[row] = 0;
  }
  for (R_xlen_t column = 0; column < input.columns; ++column) {
    account_work(&work_since_interrupt);
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
      if (exact_string(
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
    account_work(&work_since_interrupt);
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

SEXP paradox_generate_design_grid_builtin(SEXP params, SEXP resolutions) {
  const SEXPTYPE resolution_type = (SEXPTYPE) TYPEOF(resolutions);
  if ((resolution_type != INTSXP && resolution_type != REALSXP) ||
      ALTREP(resolutions) || Rf_isObject(resolutions)) {
    Rf_error("`resolutions` must be an ordinary named numeric vector");
  }

  param_columns_t columns;
  SEXP column_roots = PROTECT(Rf_allocVector(VECSXP, QUNIF_ROOT_COUNT));
  if (!load_param_columns(params, &columns, column_roots)) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet grid state: invalid parameter schema");
  }
  if (columns.size == 0) {
    if (XLENGTH(resolutions) != 0) {
      UNPROTECT(1);
      Rf_error("`resolutions` must contain one value per parameter");
    }
    SEXP stable_names = PROTECT(Rf_allocVector(STRSXP, 0));
    int unused_count = 0;
    snapshot_grid_resolutions(resolutions, stable_names, &unused_count);
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 0));
    SEXP prepared = PROTECT(set_table_attributes(result, stable_names, 0));
    UNPROTECT(4);
    return prepared;
  }
  if (XLENGTH(resolutions) != columns.size) {
    UNPROTECT(1);
    Rf_error("`resolutions` must contain one value per parameter");
  }

  id_map_t id_map;
  if (!initialize_id_map(columns.ids, &id_map)) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet grid state: duplicate parameter IDs");
  }

  qunif_spec_t *specs = paradox_temporary_alloc(
    columns.size,
    sizeof(*specs)
  );
  int *counts = paradox_temporary_alloc(columns.size, sizeof(*counts));
  R_xlen_t *strides = paradox_temporary_alloc(
    columns.size,
    sizeof(*strides)
  );
  unsigned char *selected = paradox_temporary_alloc(
    columns.size,
    sizeof(*selected)
  );
  SEXP spec_roots = PROTECT(Rf_allocVector(VECSXP, columns.size));
  SEXP stable_resolution_names = PROTECT(Rf_allocVector(
    STRSXP,
    columns.size
  ));
  R_xlen_t rows;
  R_xlen_t work_since_interrupt = 0;
  snapshot_grid_resolutions(
    resolutions,
    stable_resolution_names,
    counts
  );
  load_grid_specs(
    &columns,
    &id_map,
    stable_resolution_names,
    specs,
    counts,
    strides,
    selected,
    spec_roots,
    &rows,
    &work_since_interrupt
  );

  SEXP result = PROTECT(Rf_allocVector(VECSXP, columns.size));
  SEXP result_names = PROTECT(copy_column_names(
    stable_resolution_names,
    &work_since_interrupt
  ));
  int warn_integer_range = FALSE;
  for (R_xlen_t column = 0; column < columns.size; ++column) {
    account_work(&work_since_interrupt);
    SEXP output = PROTECT(Rf_allocVector(
      output_type(specs[column].kind),
      rows
    ));
    SET_VECTOR_ELT(result, column, output);
    if (!fill_grid_column(
          output,
          rows,
          counts[column],
          strides[column],
          &specs[column],
          &warn_integer_range,
          &work_since_interrupt
        )) {
      UNPROTECT(6);
      Rf_error("Corrupt ParamSet grid mapping state");
    }
    UNPROTECT(1);
  }

  if (warn_integer_range) {
    Rf_warning("NAs introduced by coercion to integer range");
  }

  SEXP prepared = PROTECT(set_table_attributes(result, result_names, rows));
  UNPROTECT(6);
  return prepared;
}
