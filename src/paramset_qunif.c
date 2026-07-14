#include <math.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Arith.h>
#include <R_ext/Utils.h>

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
  QUNIF_ROOT_CLASSES = 0,
  QUNIF_ROOT_NAMES,
  QUNIF_ROOT_COLUMNS,
  QUNIF_ROOT_COUNT = (int) QUNIF_ROOT_COLUMNS + (int) PARAM_COLUMN_COUNT
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
} matrix_info_t;

static const char *const param_column_names[PARAM_COLUMN_COUNT] = {
  "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
  "levels", "special_vals", "default", "storage_type"
};

static const SEXPTYPE param_column_types[PARAM_COLUMN_COUNT] = {
  STRSXP, STRSXP, STRSXP, VECSXP, REALSXP, REALSXP, REALSXP, VECSXP,
  VECSXP, VECSXP, STRSXP
};

static inline void periodic_interrupt(R_xlen_t iteration) {
  if (iteration != 0 &&
      iteration % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
    R_CheckUserInterrupt();
  }
}

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

  const void *vmax = vmaxget();
  const char *left_text = Rf_translateCharUTF8(left);
  const char *right_text = Rf_translateCharUTF8(right);
  const int equal = strcmp(left_text, right_text) == 0;
  vmaxset(vmax);
  return equal;
}

static int numeric_column(SEXP column, R_xlen_t size) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(column);
  return (type == REALSXP || type == INTSXP) && XLENGTH(column) == size;
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
  if (TYPEOF(params) != VECSXP || ALTREP(params) ||
      XLENGTH(params) != PARAM_COLUMN_COUNT) {
    return FALSE;
  }

  SEXP classes = PROTECT(Rf_getAttrib(params, R_ClassSymbol));
  SET_VECTOR_ELT(roots, QUNIF_ROOT_CLASSES, classes);
  UNPROTECT(1);
  if (TYPEOF(classes) != STRSXP || ALTREP(classes) ||
      XLENGTH(classes) != 2) {
    return FALSE;
  }

  SEXP names = PROTECT(Rf_getAttrib(params, R_NamesSymbol));
  SET_VECTOR_ELT(roots, QUNIF_ROOT_NAMES, names);
  UNPROTECT(1);
  if (TYPEOF(names) != STRSXP || ALTREP(names) ||
      XLENGTH(names) != PARAM_COLUMN_COUNT) {
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
  if (TYPEOF(columns->ids) != STRSXP || ALTREP(columns->ids)) {
    return FALSE;
  }
  columns->size = XLENGTH(columns->ids);
  if (!exact_string(STRING_ELT(classes, 0), "data.table") ||
      !exact_string(STRING_ELT(classes, 1), "data.frame")) {
    return FALSE;
  }
  for (R_xlen_t column = 0; column < PARAM_COLUMN_COUNT; ++column) {
    if (!exact_string(STRING_ELT(names, column), param_column_names[column])) {
      return FALSE;
    }
    SEXP value = VECTOR_ELT(
      roots,
      qunif_column_root((enum param_column) column)
    );
    const int is_numeric = column == PARAM_LOWER || column == PARAM_UPPER ||
      column == PARAM_TOLERANCE;
    if (ALTREP(value) || (is_numeric
          ? !numeric_column(value, columns->size)
          : (SEXPTYPE) TYPEOF(value) != param_column_types[column] ||
            XLENGTH(value) != columns->size)) {
      return FALSE;
    }
  }

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

  for (R_xlen_t row = 0; row < columns->size; ++row) {
    periodic_interrupt(row);
    if (STRING_ELT(columns->ids, row) == NA_STRING ||
        STRING_ELT(columns->classes, row) == NA_STRING ||
        STRING_ELT(columns->grouping, row) == NA_STRING ||
        STRING_ELT(columns->storage_types, row) == NA_STRING) {
      return FALSE;
    }
  }
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

  const void *vmax = vmaxget();
  const char *text = Rf_translateCharUTF8(string);
  hash = hash_bytes((const unsigned char *) text, hash);
  vmaxset(vmax);
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
    SEXP id = STRING_ELT(ids, row);
    const uint64_t hash = hash_string(id);
    R_xlen_t slot = (R_xlen_t) (hash & (uint64_t) mask);
    while (slots[slot].row_plus_one != 0) {
      account_work(&work_since_interrupt);
      const R_xlen_t present = slots[slot].row_plus_one - 1;
      if (slots[slot].hash == hash &&
          strings_equal(STRING_ELT(ids, present), id)) {
        return FALSE;
      }
      slot = (slot + 1) & mask;
    }
    slots[slot].hash = hash;
    slots[slot].row_plus_one = row + 1;
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

static int load_matrix_info(SEXP x, matrix_info_t *info, SEXP roots) {
  if (Rf_isObject(x) || (TYPEOF(x) != REALSXP && TYPEOF(x) != INTSXP)) {
    return FALSE;
  }

  SEXP dimensions = PROTECT(ALTREP(x)
    ? paradox_stored_attribute(x, R_DimSymbol)
    : Rf_getAttrib(x, R_DimSymbol));
  if (TYPEOF(dimensions) != INTSXP || XLENGTH(dimensions) != 2) {
    UNPROTECT(1);
    return FALSE;
  }
  const int row_count = INTEGER_ELT(dimensions, 0);
  const int column_count = INTEGER_ELT(dimensions, 1);
  if (row_count < 0 || column_count <= 0) {
    UNPROTECT(1);
    return FALSE;
  }
  info->rows = (R_xlen_t) row_count;
  info->columns = (R_xlen_t) column_count;
  if (info->rows != 0 &&
      info->columns > R_XLEN_T_MAX / info->rows) {
    UNPROTECT(1);
    return FALSE;
  }
  const R_xlen_t size = info->rows * info->columns;
  if (XLENGTH(x) != size) {
    UNPROTECT(1);
    return FALSE;
  }

  SEXP dimension_names = PROTECT(ALTREP(x)
    ? paradox_stored_attribute(x, R_DimNamesSymbol)
    : Rf_getAttrib(x, R_DimNamesSymbol));
  if (TYPEOF(dimension_names) != VECSXP || XLENGTH(dimension_names) != 2) {
    UNPROTECT(2);
    return FALSE;
  }
  SEXP source_names = PROTECT(VECTOR_ELT(dimension_names, 1));
  if (TYPEOF(source_names) != STRSXP ||
      XLENGTH(source_names) != info->columns) {
    UNPROTECT(3);
    return FALSE;
  }

  R_xlen_t work_since_interrupt = 0;
  SEXP stable_names = PROTECT(Rf_allocVector(STRSXP, info->columns));
  for (R_xlen_t column = 0; column < info->columns; ++column) {
    account_work(&work_since_interrupt);
    SEXP name = STRING_ELT(source_names, column);
    if (name == NA_STRING) {
      UNPROTECT(4);
      return FALSE;
    }
    SET_STRING_ELT(stable_names, column, name);
  }
  SET_VECTOR_ELT(roots, 0, stable_names);
  UNPROTECT(1);

  SEXP stable_values = PROTECT(Rf_allocVector(REALSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    account_work(&work_since_interrupt);
    const double unit = numeric_at(x, index);
    if (!R_FINITE(unit) || unit < 0.0 || unit > 1.0) {
      UNPROTECT(4);
      return FALSE;
    }
    SET_REAL_ELT(stable_values, index, unit);
  }
  SET_VECTOR_ELT(roots, 1, stable_values);
  info->column_names = VECTOR_ELT(roots, 0);
  info->values = VECTOR_ELT(roots, 1);
  UNPROTECT(4);
  return TRUE;
}

static SEXP snapshot_factor_levels(SEXP levels,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(levels) != STRSXP) {
    return R_NilValue;
  }
  const R_xlen_t size = XLENGTH(levels);
  if (size == 0) {
    return R_NilValue;
  }
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

  /* `as.data.table(set_names(...))`, which assembled the historical result,
   * leaves these visible attributes in row.names/class/names order. Attribute
   * order is observable through `attributes()` and is therefore retained. */
  Rf_setAttrib(table, R_RowNamesSymbol, row_names);
  Rf_setAttrib(table, R_ClassSymbol, classes);
  Rf_setAttrib(table, R_NamesSymbol, names);
  SEXP result = PROTECT(paradox_prepare_data_table(table, TRUE));
  UNPROTECT(3);
  return result;
}

static int fill_column(SEXP output, SEXP x, R_xlen_t input_offset,
    R_xlen_t rows, const qunif_spec_t *spec,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t row = 0; row < rows; ++row) {
    account_work(work_since_interrupt);
    const double unit = numeric_at(x, input_offset + row);
    if (!R_FINITE(unit) || unit < 0.0 || unit > 1.0) {
      return FALSE;
    }
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
        return FALSE;
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

SEXP paradox_param_set_qunif_builtin(SEXP params, SEXP x) {
  param_columns_t columns;
  matrix_info_t matrix;
  SEXP column_roots = PROTECT(Rf_allocVector(VECSXP, QUNIF_ROOT_COUNT));
  if (!load_param_columns(params, &columns, column_roots)) {
    UNPROTECT(1);
    return R_NilValue;
  }

  SEXP matrix_roots = PROTECT(Rf_allocVector(VECSXP, 2));
  if (!load_matrix_info(x, &matrix, matrix_roots)) {
    UNPROTECT(2);
    return R_NilValue;
  }

  id_map_t id_map;
  if (!initialize_id_map(columns.ids, &id_map)) {
    UNPROTECT(2);
    return R_NilValue;
  }

  qunif_spec_t *specs = paradox_temporary_alloc(
    matrix.columns,
    sizeof(*specs)
  );
  SEXP spec_roots = PROTECT(Rf_allocVector(VECSXP, matrix.columns));
  unsigned char *selected = paradox_temporary_alloc(
    columns.size == 0 ? 1 : columns.size,
    sizeof(*selected)
  );
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t row = 0; row < columns.size; ++row) {
    account_work(&work_since_interrupt);
    selected[row] = 0;
  }
  for (R_xlen_t column = 0; column < matrix.columns; ++column) {
    account_work(&work_since_interrupt);
    R_xlen_t param_row;
    if (!find_id(
          &id_map,
          STRING_ELT(matrix.column_names, column),
          &param_row,
          &work_since_interrupt
        ) || selected[param_row] ||
        !load_spec(
          &columns,
          param_row,
          &specs[column],
          spec_roots,
          column,
          &work_since_interrupt
        )) {
      UNPROTECT(3);
      return R_NilValue;
    }
    selected[param_row] = 1;
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, matrix.columns));
  SEXP result_names = PROTECT(copy_column_names(
    matrix.column_names,
    &work_since_interrupt
  ));
  for (R_xlen_t column = 0; column < matrix.columns; ++column) {
    account_work(&work_since_interrupt);
    SEXP output = PROTECT(Rf_allocVector(
      output_type(specs[column].kind),
      matrix.rows
    ));
    SET_VECTOR_ELT(result, column, output);
    if (!fill_column(
          output,
          matrix.values,
          column * matrix.rows,
          matrix.rows,
          &specs[column],
          &work_since_interrupt
        )) {
      UNPROTECT(6);
      return R_NilValue;
    }
    UNPROTECT(1);
  }
  SEXP prepared = PROTECT(set_table_attributes(
    result,
    result_names,
    matrix.rows
  ));
  UNPROTECT(6);
  return prepared;
}
