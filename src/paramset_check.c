#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Arith.h>
#include <R_ext/Utils.h>

#include "r_utils.h"

typedef enum {
  PARAM_KIND_UNKNOWN = 0,
  PARAM_KIND_DBL,
  PARAM_KIND_INT,
  PARAM_KIND_FCT,
  PARAM_KIND_LGL
} param_kind_t;

typedef struct {
  R_xlen_t size;
  SEXP ids;
  SEXP classes;
  SEXP lower;
  SEXP upper;
  SEXP tolerance;
  SEXP levels;
  SEXP special_values;
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
  param_kind_t kind;
  double lower;
  double upper;
  double tolerance;
  SEXP levels;
} param_spec_t;

enum check_root_slot {
  CHECK_ROOT_PARAM_NAMES = 0,
  CHECK_ROOT_PARAM_TABLE_CLASS,
  CHECK_ROOT_PARAM_IDS,
  CHECK_ROOT_PARAM_CLASSES,
  CHECK_ROOT_PARAM_LOWER,
  CHECK_ROOT_PARAM_UPPER,
  CHECK_ROOT_PARAM_TOLERANCE,
  CHECK_ROOT_PARAM_LEVELS,
  CHECK_ROOT_PARAM_SPECIAL_VALUES,
  CHECK_ROOT_PARAM_STORAGE_TYPES,
  CHECK_ROOT_INPUT_NAMES,
  CHECK_ROOT_INPUT_CLASS,
  CHECK_ROOT_INPUT_VALUES,
  CHECK_ROOT_INPUT_COLUMNS,
  CHECK_ROOT_CURRENT_VALUE,
  CHECK_ROOT_CURRENT_LEVELS,
  CHECK_ROOT_CURRENT_SPECIAL_VALUES,
  CHECK_ROOT_COUNT
};

static inline void account_work(R_xlen_t *work_since_interrupt) {
  ++*work_since_interrupt;
  if (*work_since_interrupt >= PARADOX_INTERRUPT_CHECK_INTERVAL) {
    R_CheckUserInterrupt();
    *work_since_interrupt = 0;
  }
}

static SEXP named_column(SEXP table, SEXP names, R_xlen_t n_columns,
    const char *column_name) {
  SEXP result = R_NilValue;
  int matches = 0;
  for (R_xlen_t column = 0; column < n_columns; ++column) {
    SEXP name = STRING_ELT(names, column);
    if (name != NA_STRING && strcmp(CHAR(name), column_name) == 0) {
      result = VECTOR_ELT(table, column);
      ++matches;
    }
  }
  return matches == 1 ? result : R_NilValue;
}

static int numeric_column(SEXP column, R_xlen_t size) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(column);
  return (type == REALSXP || type == INTSXP) && !ALTREP(column) &&
    XLENGTH(column) == size;
}

static int load_param_columns(SEXP params, param_columns_t *columns,
    SEXP roots) {
  if (TYPEOF(params) != VECSXP || ALTREP(params)) {
    return FALSE;
  }
  if (Rf_isObject(params)) {
    SEXP table_class = Rf_getAttrib(params, R_ClassSymbol);
    SET_VECTOR_ELT(roots, CHECK_ROOT_PARAM_TABLE_CLASS, table_class);
    if (TYPEOF(table_class) != STRSXP || ALTREP(table_class) ||
        !Rf_inherits(params, "data.frame")) {
      return FALSE;
    }
  }

  const R_xlen_t n_columns = XLENGTH(params);
  SEXP names = Rf_getAttrib(params, R_NamesSymbol);
  SET_VECTOR_ELT(roots, CHECK_ROOT_PARAM_NAMES, names);
  if (TYPEOF(names) != STRSXP || ALTREP(names) ||
      XLENGTH(names) != n_columns) {
    return FALSE;
  }

  columns->ids = named_column(params, names, n_columns, "id");
  SET_VECTOR_ELT(roots, CHECK_ROOT_PARAM_IDS, columns->ids);
  columns->classes = named_column(params, names, n_columns, "cls");
  SET_VECTOR_ELT(roots, CHECK_ROOT_PARAM_CLASSES, columns->classes);
  columns->lower = named_column(params, names, n_columns, "lower");
  SET_VECTOR_ELT(roots, CHECK_ROOT_PARAM_LOWER, columns->lower);
  columns->upper = named_column(params, names, n_columns, "upper");
  SET_VECTOR_ELT(roots, CHECK_ROOT_PARAM_UPPER, columns->upper);
  columns->tolerance = named_column(
    params,
    names,
    n_columns,
    "tolerance"
  );
  SET_VECTOR_ELT(roots, CHECK_ROOT_PARAM_TOLERANCE, columns->tolerance);
  columns->levels = named_column(params, names, n_columns, "levels");
  SET_VECTOR_ELT(roots, CHECK_ROOT_PARAM_LEVELS, columns->levels);
  columns->special_values = named_column(
    params,
    names,
    n_columns,
    "special_vals"
  );
  SET_VECTOR_ELT(
    roots,
    CHECK_ROOT_PARAM_SPECIAL_VALUES,
    columns->special_values
  );
  columns->storage_types = named_column(
    params,
    names,
    n_columns,
    "storage_type"
  );
  SET_VECTOR_ELT(
    roots,
    CHECK_ROOT_PARAM_STORAGE_TYPES,
    columns->storage_types
  );

  if (columns->ids == R_NilValue || columns->classes == R_NilValue ||
      columns->lower == R_NilValue || columns->upper == R_NilValue ||
      columns->tolerance == R_NilValue || columns->levels == R_NilValue ||
      columns->special_values == R_NilValue ||
      columns->storage_types == R_NilValue) {
    return FALSE;
  }

  if (TYPEOF(columns->ids) != STRSXP || ALTREP(columns->ids) ||
      TYPEOF(columns->classes) != STRSXP || ALTREP(columns->classes) ||
      TYPEOF(columns->levels) != VECSXP || ALTREP(columns->levels) ||
      TYPEOF(columns->special_values) != VECSXP ||
      ALTREP(columns->special_values) ||
      TYPEOF(columns->storage_types) != STRSXP ||
      ALTREP(columns->storage_types)) {
    return FALSE;
  }

  columns->size = XLENGTH(columns->ids);
  const R_xlen_t size = columns->size;
  return numeric_column(columns->lower, size) &&
    numeric_column(columns->upper, size) &&
    numeric_column(columns->tolerance, size) &&
    XLENGTH(columns->classes) == size &&
    XLENGTH(columns->levels) == size &&
    XLENGTH(columns->special_values) == size &&
    XLENGTH(columns->storage_types) == size;
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
  const char *left_text = Rf_translateCharUTF8(left);
  const char *right_text = Rf_translateCharUTF8(right);
  const int equal = strcmp(left_text, right_text) == 0;
  vmaxset(vmax);
  UNPROTECT(2);
  return equal;
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

static int initialize_id_map(SEXP ids, R_xlen_t size, id_map_t *map) {
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
    if (id == NA_STRING) {
      UNPROTECT(1);
      return FALSE;
    }
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
  if (id == NA_STRING) {
    return FALSE;
  }
  PROTECT(id);
  const uint64_t hash = hash_string(id);
  const R_xlen_t mask = map->capacity - 1;
  R_xlen_t slot = (R_xlen_t) (hash & (uint64_t) mask);
  while (map->slots[slot].row_plus_one != 0) {
    account_work(work_since_interrupt);
    const R_xlen_t present = map->slots[slot].row_plus_one - 1;
    if (map->slots[slot].hash == hash &&
        strings_equal(STRING_ELT(map->ids, present), id)) {
      *row = present;
      UNPROTECT(1);
      return TRUE;
    }
    slot = (slot + 1) & mask;
  }
  UNPROTECT(1);
  return FALSE;
}

static double numeric_at(SEXP column, R_xlen_t row) {
  if (TYPEOF(column) == REALSXP) {
    return REAL_ELT(column, row);
  }
  const int value = INTEGER_ELT(column, row);
  return value == NA_INTEGER ? NA_REAL : (double) value;
}

static int exact_string(SEXP string, const char *expected) {
  return string != NA_STRING && strcmp(CHAR(string), expected) == 0;
}

static int load_param_spec(const param_columns_t *columns, R_xlen_t row,
    param_spec_t *spec, SEXP roots, R_xlen_t *work_since_interrupt) {
  SEXP special_values = VECTOR_ELT(columns->special_values, row);
  SET_VECTOR_ELT(
    roots,
    CHECK_ROOT_CURRENT_SPECIAL_VALUES,
    special_values
  );
  if (TYPEOF(special_values) != VECSXP || ALTREP(special_values) ||
      XLENGTH(special_values) != 0) {
    return FALSE;
  }

  SEXP class_name = STRING_ELT(columns->classes, row);
  SEXP storage_type = STRING_ELT(columns->storage_types, row);
  spec->kind = PARAM_KIND_UNKNOWN;
  if (exact_string(class_name, "ParamDbl") &&
      exact_string(storage_type, "numeric")) {
    spec->kind = PARAM_KIND_DBL;
  } else if (exact_string(class_name, "ParamInt") &&
      exact_string(storage_type, "integer")) {
    spec->kind = PARAM_KIND_INT;
  } else if (exact_string(class_name, "ParamFct") &&
      exact_string(storage_type, "character")) {
    spec->kind = PARAM_KIND_FCT;
  } else if (exact_string(class_name, "ParamLgl") &&
      exact_string(storage_type, "logical")) {
    spec->kind = PARAM_KIND_LGL;
  } else {
    return FALSE;
  }

  spec->levels = VECTOR_ELT(columns->levels, row);
  SET_VECTOR_ELT(roots, CHECK_ROOT_CURRENT_LEVELS, spec->levels);
  if (ALTREP(spec->levels)) {
    return FALSE;
  }
  if (spec->kind == PARAM_KIND_FCT) {
    if (TYPEOF(spec->levels) != STRSXP) {
      return FALSE;
    }
    const R_xlen_t n_levels = XLENGTH(spec->levels);
    for (R_xlen_t level = 0; level < n_levels; ++level) {
      account_work(work_since_interrupt);
      if (STRING_ELT(spec->levels, level) == NA_STRING) {
        return FALSE;
      }
    }
    return TRUE;
  }

  if (spec->kind == PARAM_KIND_LGL) {
    return TRUE;
  }

  spec->lower = numeric_at(columns->lower, row);
  spec->upper = numeric_at(columns->upper, row);
  spec->tolerance = numeric_at(columns->tolerance, row);
  if (ISNAN(spec->lower) || ISNAN(spec->upper) ||
      ISNAN(spec->tolerance) || !R_FINITE(spec->tolerance) ||
      spec->tolerance < 0.0 || spec->lower > spec->upper) {
    return FALSE;
  }
  if (spec->kind == PARAM_KIND_INT && spec->tolerance > 0.5) {
    return FALSE;
  }
  return TRUE;
}

static int scalar_numeric(SEXP value, double *number) {
  if (ALTREP(value) || Rf_isObject(value)) {
    return FALSE;
  }
  if (TYPEOF(value) == REALSXP) {
    if (XLENGTH(value) != 1) {
      return FALSE;
    }
    *number = REAL_ELT(value, 0);
    return !ISNAN(*number);
  }
  if (TYPEOF(value) == INTSXP) {
    if (XLENGTH(value) != 1) {
      return FALSE;
    }
    const int integer = INTEGER_ELT(value, 0);
    if (integer == NA_INTEGER) {
      return FALSE;
    }
    *number = (double) integer;
    return TRUE;
  }
  return FALSE;
}

static int valid_double(double value, const param_spec_t *spec) {
  const double lower = paradox_accepted_lower(spec->lower, spec->tolerance);
  const double upper = paradox_accepted_upper(spec->upper, spec->tolerance);
  return !ISNAN(lower) && !ISNAN(upper) && value >= lower && value <= upper;
}

static int valid_integer(double value, const param_spec_t *spec,
    double *rounded) {
  if (!R_FINITE(value)) {
    return FALSE;
  }
  *rounded = nearbyint(value);
  return fabs(value - *rounded) <= spec->tolerance &&
    *rounded >= spec->lower && *rounded <= spec->upper;
}

static int valid_factor(SEXP value, SEXP levels,
    R_xlen_t *work_since_interrupt) {
  if (ALTREP(value) || Rf_isObject(value) || TYPEOF(value) != STRSXP ||
      XLENGTH(value) != 1) {
    return FALSE;
  }
  SEXP selected = STRING_ELT(value, 0);
  if (selected == NA_STRING) {
    return FALSE;
  }
  PROTECT(selected);

  int found = FALSE;
  const R_xlen_t size = XLENGTH(levels);
  for (R_xlen_t level = 0; level < size; ++level) {
    account_work(work_since_interrupt);
    SEXP choice = STRING_ELT(levels, level);
    if (choice == NA_STRING) {
      UNPROTECT(1);
      return FALSE;
    }
    if (strings_equal(selected, choice)) {
      found = TRUE;
    }
  }
  UNPROTECT(1);
  return found;
}

static int valid_scalar(SEXP value, const param_spec_t *spec,
    double *numeric_result, R_xlen_t *work_since_interrupt) {
  switch (spec->kind) {
  case PARAM_KIND_DBL:
    return scalar_numeric(value, numeric_result) &&
      valid_double(*numeric_result, spec);
  case PARAM_KIND_INT:
    return scalar_numeric(value, numeric_result) &&
      valid_integer(*numeric_result, spec, numeric_result);
  case PARAM_KIND_FCT:
    return valid_factor(value, spec->levels, work_since_interrupt);
  case PARAM_KIND_LGL:
    return !ALTREP(value) && !Rf_isObject(value) && TYPEOF(value) == LGLSXP &&
      XLENGTH(value) == 1 && LOGICAL_ELT(value, 0) != NA_LOGICAL;
  case PARAM_KIND_UNKNOWN:
    return FALSE;
  }
  return FALSE;
}

static SEXP valid_check_result(SEXP xs, int sanitize) {
  if (!sanitize) {
    return Rf_ScalarLogical(TRUE);
  }

  SEXP sanitized = PROTECT(Rf_shallow_duplicate(xs));
  /* ScalarLogical(TRUE) may be R's shared singleton.  An attributed result
   * must always be freshly allocated rather than mutating that singleton. */
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, 1));
  LOGICAL(result)[0] = TRUE;
  Rf_setAttrib(result, Rf_install("sanitized"), sanitized);
  UNPROTECT(2);
  return result;
}

SEXP paradox_param_set_check_builtin(SEXP params, SEXP xs, SEXP sanitize) {
  if (TYPEOF(sanitize) != LGLSXP || ALTREP(sanitize) ||
      XLENGTH(sanitize) != 1 ||
      LOGICAL_ELT(sanitize, 0) == NA_LOGICAL || TYPEOF(xs) != VECSXP ||
      ALTREP(xs) || Rf_isObject(xs)) {
    return R_NilValue;
  }
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, CHECK_ROOT_COUNT));
  const int do_sanitize = LOGICAL_ELT(sanitize, 0);
  const R_xlen_t n_values = XLENGTH(xs);
  if (n_values == 0) {
    SEXP result = PROTECT(valid_check_result(xs, do_sanitize));
    UNPROTECT(2);
    return result;
  }

  SEXP input_values = PROTECT(Rf_allocVector(VECSXP, n_values));
  SET_VECTOR_ELT(roots, CHECK_ROOT_INPUT_VALUES, input_values);
  UNPROTECT(1);
  SEXP value_names = Rf_getAttrib(xs, R_NamesSymbol);
  SET_VECTOR_ELT(roots, CHECK_ROOT_INPUT_NAMES, value_names);
  if (TYPEOF(value_names) != STRSXP || ALTREP(value_names) ||
      XLENGTH(value_names) != n_values) {
    UNPROTECT(1);
    return R_NilValue;
  }
  for (R_xlen_t index = 0; index < n_values; ++index) {
    SEXP value = PROTECT(VECTOR_ELT(xs, index));
    SET_VECTOR_ELT(input_values, index, value);
    UNPROTECT(1);
  }

  param_columns_t columns;
  if (!load_param_columns(params, &columns, roots)) {
    UNPROTECT(1);
    return R_NilValue;
  }
  id_map_t map;
  if (!initialize_id_map(columns.ids, columns.size, &map)) {
    UNPROTECT(1);
    return R_NilValue;
  }

  const R_xlen_t seen_size = columns.size == 0 ? 1 : columns.size;
  unsigned char *seen = paradox_temporary_alloc(
    seen_size,
    sizeof(*seen)
  );
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t row = 0; row < seen_size; ++row) {
    account_work(&work_since_interrupt);
    seen[row] = 0;
  }

  SEXP sanitized = R_NilValue;
  int protected_count = 1;
  if (do_sanitize) {
    sanitized = PROTECT(Rf_shallow_duplicate(xs));
    ++protected_count;
  }

  for (R_xlen_t index = 0; index < n_values; ++index) {
    account_work(&work_since_interrupt);
    R_xlen_t row;
    if (!find_id(
          &map,
          STRING_ELT(value_names, index),
          &row,
          &work_since_interrupt
        ) || seen[row]) {
      UNPROTECT(protected_count);
      return R_NilValue;
    }
    seen[row] = 1;

    param_spec_t spec;
    double numeric_result = 0.0;
    SEXP value = VECTOR_ELT(input_values, index);
    SET_VECTOR_ELT(roots, CHECK_ROOT_CURRENT_VALUE, value);
    if (!load_param_spec(
          &columns,
          row,
          &spec,
          roots,
          &work_since_interrupt
        ) ||
        !valid_scalar(value, &spec, &numeric_result, &work_since_interrupt)) {
      UNPROTECT(protected_count);
      return R_NilValue;
    }

    if (do_sanitize && spec.kind == PARAM_KIND_DBL) {
      double clamped = numeric_result;
      if (clamped < spec.lower) {
        clamped = spec.lower;
      }
      if (clamped > spec.upper) {
        clamped = spec.upper;
      }
      SEXP replacement = PROTECT(Rf_ScalarReal(clamped));
      SET_VECTOR_ELT(sanitized, index, replacement);
      UNPROTECT(1);
    } else if (do_sanitize && spec.kind == PARAM_KIND_INT) {
      if (numeric_result <= (double) INT_MIN ||
          numeric_result > (double) INT_MAX) {
        UNPROTECT(protected_count);
        return R_NilValue;
      }
      SEXP replacement = PROTECT(Rf_ScalarInteger((int) numeric_result));
      SET_VECTOR_ELT(sanitized, index, replacement);
      UNPROTECT(1);
    }
    SET_VECTOR_ELT(roots, CHECK_ROOT_CURRENT_VALUE, R_NilValue);
    SET_VECTOR_ELT(roots, CHECK_ROOT_CURRENT_LEVELS, R_NilValue);
    SET_VECTOR_ELT(
      roots,
      CHECK_ROOT_CURRENT_SPECIAL_VALUES,
      R_NilValue
    );
  }

  if (!do_sanitize) {
    SEXP result = PROTECT(Rf_ScalarLogical(TRUE));
    UNPROTECT(2);
    return result;
  }
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, 1));
  LOGICAL(result)[0] = TRUE;
  Rf_setAttrib(result, Rf_install("sanitized"), sanitized);
  UNPROTECT(protected_count + 1);
  return result;
}

static int atomic_missing(SEXP column, R_xlen_t row) {
  switch ((SEXPTYPE) TYPEOF(column)) {
  case REALSXP:
    return ISNAN(REAL_ELT(column, row));
  case INTSXP:
    return INTEGER_ELT(column, row) == NA_INTEGER;
  case LGLSXP:
    return LOGICAL_ELT(column, row) == NA_LOGICAL;
  case STRSXP:
    return STRING_ELT(column, row) == NA_STRING;
  default:
    return FALSE;
  }
}

static int valid_double_column(SEXP column, R_xlen_t size,
    const param_spec_t *spec, int require_complete, int *all_complete,
    R_xlen_t *work_since_interrupt) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(column);
  if (type != REALSXP && type != INTSXP) {
    for (R_xlen_t row = 0; row < size; ++row) {
      account_work(work_since_interrupt);
      if (!atomic_missing(column, row)) {
        return FALSE;
      }
      if (require_complete) {
        return FALSE;
      }
      if (all_complete != NULL) {
        *all_complete = FALSE;
      }
    }
    return TRUE;
  }

  for (R_xlen_t row = 0; row < size; ++row) {
    account_work(work_since_interrupt);
    double value;
    if (type == REALSXP) {
      value = REAL_ELT(column, row);
      if (ISNAN(value)) {
        if (require_complete) {
          return FALSE;
        }
        if (all_complete != NULL) {
          *all_complete = FALSE;
        }
        continue;
      }
    } else {
      const int integer = INTEGER_ELT(column, row);
      if (integer == NA_INTEGER) {
        if (require_complete) {
          return FALSE;
        }
        if (all_complete != NULL) {
          *all_complete = FALSE;
        }
        continue;
      }
      value = (double) integer;
    }
    if (!valid_double(value, spec)) {
      return FALSE;
    }
  }
  return TRUE;
}

static int valid_integer_column(SEXP column, R_xlen_t size,
    const param_spec_t *spec, int require_complete, int *all_complete,
    R_xlen_t *work_since_interrupt) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(column);
  if (type != REALSXP && type != INTSXP) {
    for (R_xlen_t row = 0; row < size; ++row) {
      account_work(work_since_interrupt);
      if (!atomic_missing(column, row)) {
        return FALSE;
      }
      if (require_complete) {
        return FALSE;
      }
      if (all_complete != NULL) {
        *all_complete = FALSE;
      }
    }
    return TRUE;
  }

  for (R_xlen_t row = 0; row < size; ++row) {
    account_work(work_since_interrupt);
    double value;
    if (type == REALSXP) {
      value = REAL_ELT(column, row);
      if (ISNAN(value)) {
        if (require_complete) {
          return FALSE;
        }
        if (all_complete != NULL) {
          *all_complete = FALSE;
        }
        continue;
      }
    } else {
      const int integer = INTEGER_ELT(column, row);
      if (integer == NA_INTEGER) {
        if (require_complete) {
          return FALSE;
        }
        if (all_complete != NULL) {
          *all_complete = FALSE;
        }
        continue;
      }
      value = (double) integer;
    }
    double rounded;
    if (!valid_integer(value, spec, &rounded)) {
      return FALSE;
    }
  }
  return TRUE;
}

static int valid_factor_column(SEXP column, R_xlen_t size, SEXP levels,
    int require_complete, int *all_complete,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(column) != STRSXP) {
    for (R_xlen_t row = 0; row < size; ++row) {
      account_work(work_since_interrupt);
      if (!atomic_missing(column, row)) {
        return FALSE;
      }
      if (require_complete) {
        return FALSE;
      }
      if (all_complete != NULL) {
        *all_complete = FALSE;
      }
    }
    return TRUE;
  }

  for (R_xlen_t row = 0; row < size; ++row) {
    account_work(work_since_interrupt);
    SEXP selected = STRING_ELT(column, row);
    if (selected == NA_STRING) {
      if (require_complete) {
        return FALSE;
      }
      if (all_complete != NULL) {
        *all_complete = FALSE;
      }
      continue;
    }
    PROTECT(selected);
    int found = FALSE;
    const R_xlen_t n_levels = XLENGTH(levels);
    for (R_xlen_t level = 0; level < n_levels; ++level) {
      account_work(work_since_interrupt);
      SEXP choice = STRING_ELT(levels, level);
      if (strings_equal(selected, choice)) {
        found = TRUE;
      }
    }
    UNPROTECT(1);
    if (!found) {
      return FALSE;
    }
  }
  return TRUE;
}

static int valid_logical_column(SEXP column, R_xlen_t size,
    int require_complete, int *all_complete,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(column) != LGLSXP) {
    for (R_xlen_t row = 0; row < size; ++row) {
      account_work(work_since_interrupt);
      if (!atomic_missing(column, row)) {
        return FALSE;
      }
      if (require_complete) {
        return FALSE;
      }
      if (all_complete != NULL) {
        *all_complete = FALSE;
      }
    }
    return TRUE;
  }
  if (require_complete || all_complete != NULL) {
    for (R_xlen_t row = 0; row < size; ++row) {
      account_work(work_since_interrupt);
      if (LOGICAL_ELT(column, row) == NA_LOGICAL) {
        if (require_complete) {
          return FALSE;
        }
        *all_complete = FALSE;
      }
    }
  }
  return TRUE;
}

static int valid_builtin_column(SEXP column, R_xlen_t size,
    const param_spec_t *spec, int require_complete, int *all_complete,
    R_xlen_t *work_since_interrupt) {
  if (ALTREP(column) || Rf_isObject(column)) {
    return FALSE;
  }
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(column);
  if (type != REALSXP && type != INTSXP && type != LGLSXP &&
      type != STRSXP) {
    return FALSE;
  }

  switch (spec->kind) {
  case PARAM_KIND_DBL:
    return valid_double_column(
      column,
      size,
      spec,
      require_complete,
      all_complete,
      work_since_interrupt
    );
  case PARAM_KIND_INT:
    return valid_integer_column(
      column,
      size,
      spec,
      require_complete,
      all_complete,
      work_since_interrupt
    );
  case PARAM_KIND_FCT:
    return valid_factor_column(
      column,
      size,
      spec->levels,
      require_complete,
      all_complete,
      work_since_interrupt
    );
  case PARAM_KIND_LGL:
    return valid_logical_column(
      column,
      size,
      require_complete,
      all_complete,
      work_since_interrupt
    );
  case PARAM_KIND_UNKNOWN:
    return FALSE;
  }
  return FALSE;
}

enum check_dt_plan_flag {
  CHECK_DT_PLAN_COMPLETE = 1,
  CHECK_DT_PLAN_ALL_PARAMS = 2
};

static SEXP valid_check_dt_result(int report_plan, int all_complete,
    int all_params) {
  if (!report_plan) {
    return Rf_ScalarLogical(TRUE);
  }
  return Rf_ScalarInteger(
    (all_complete ? CHECK_DT_PLAN_COMPLETE : 0) |
      (all_params ? CHECK_DT_PLAN_ALL_PARAMS : 0)
  );
}

static SEXP check_dt_builtin(SEXP params, SEXP xdt,
    int require_nonmissing, int require_all_params, int report_plan) {
  if (TYPEOF(xdt) != VECSXP || ALTREP(xdt)) {
    return R_NilValue;
  }
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, CHECK_ROOT_COUNT));
  SEXP input_class = Rf_getAttrib(xdt, R_ClassSymbol);
  SET_VECTOR_ELT(roots, CHECK_ROOT_INPUT_CLASS, input_class);
  if (TYPEOF(input_class) != STRSXP || ALTREP(input_class) ||
      !Rf_inherits(xdt, "data.frame")) {
    UNPROTECT(1);
    return R_NilValue;
  }
  const R_xlen_t n_columns = XLENGTH(xdt);
  if (n_columns == 0) {
    SEXP result = PROTECT(require_all_params
      ? R_NilValue
      : valid_check_dt_result(report_plan, TRUE, FALSE));
    UNPROTECT(2);
    return result;
  }

  SEXP input_columns = PROTECT(Rf_allocVector(VECSXP, n_columns));
  SET_VECTOR_ELT(roots, CHECK_ROOT_INPUT_COLUMNS, input_columns);
  UNPROTECT(1);
  for (R_xlen_t column = 0; column < n_columns; ++column) {
    SEXP values = PROTECT(VECTOR_ELT(xdt, column));
    SET_VECTOR_ELT(input_columns, column, values);
    UNPROTECT(1);
  }

  SEXP first_column = VECTOR_ELT(input_columns, 0);
  if (ALTREP(first_column)) {
    UNPROTECT(1);
    return R_NilValue;
  }
  const R_xlen_t n_rows = XLENGTH(first_column);
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t column = 1; column < n_columns; ++column) {
    account_work(&work_since_interrupt);
    SEXP values = VECTOR_ELT(input_columns, column);
    if (ALTREP(values) || XLENGTH(values) != n_rows) {
      UNPROTECT(1);
      return R_NilValue;
    }
  }
  if (n_rows == 0) {
    SEXP result = PROTECT(valid_check_dt_result(
      report_plan,
      TRUE,
      FALSE
    ));
    UNPROTECT(2);
    return result;
  }

  SEXP column_names = Rf_getAttrib(xdt, R_NamesSymbol);
  SET_VECTOR_ELT(roots, CHECK_ROOT_INPUT_NAMES, column_names);
  if (TYPEOF(column_names) != STRSXP || ALTREP(column_names) ||
      XLENGTH(column_names) != n_columns) {
    UNPROTECT(1);
    return R_NilValue;
  }
  param_columns_t columns;
  if (!load_param_columns(params, &columns, roots)) {
    UNPROTECT(1);
    return R_NilValue;
  }
  if (require_all_params && n_columns != columns.size) {
    UNPROTECT(1);
    return R_NilValue;
  }
  id_map_t map;
  if (!initialize_id_map(columns.ids, columns.size, &map)) {
    UNPROTECT(1);
    return R_NilValue;
  }

  const R_xlen_t seen_size = columns.size == 0 ? 1 : columns.size;
  unsigned char *seen = paradox_temporary_alloc(
    seen_size,
    sizeof(*seen)
  );
  for (R_xlen_t row = 0; row < seen_size; ++row) {
    account_work(&work_since_interrupt);
    seen[row] = 0;
  }
  int all_complete = TRUE;

  for (R_xlen_t column = 0; column < n_columns; ++column) {
    account_work(&work_since_interrupt);
    R_xlen_t row;
    if (!find_id(
          &map,
          STRING_ELT(column_names, column),
          &row,
          &work_since_interrupt
        ) || seen[row]) {
      UNPROTECT(1);
      return R_NilValue;
    }
    seen[row] = 1;

    param_spec_t spec;
    SEXP values = VECTOR_ELT(input_columns, column);
    SET_VECTOR_ELT(roots, CHECK_ROOT_CURRENT_VALUE, values);
    if (!load_param_spec(
          &columns,
          row,
          &spec,
          roots,
          &work_since_interrupt
        ) ||
        !valid_builtin_column(
          values,
          n_rows,
          &spec,
          require_nonmissing,
          report_plan ? &all_complete : NULL,
          &work_since_interrupt
        )) {
      UNPROTECT(1);
      return R_NilValue;
    }
    SET_VECTOR_ELT(roots, CHECK_ROOT_CURRENT_VALUE, R_NilValue);
    SET_VECTOR_ELT(roots, CHECK_ROOT_CURRENT_LEVELS, R_NilValue);
    SET_VECTOR_ELT(
      roots,
      CHECK_ROOT_CURRENT_SPECIAL_VALUES,
      R_NilValue
    );
  }
  SEXP result = PROTECT(valid_check_dt_result(
    report_plan,
    all_complete,
    n_columns == columns.size
  ));
  UNPROTECT(2);
  return result;
}

SEXP paradox_param_set_check_dt_builtin(SEXP params, SEXP xdt) {
  return check_dt_builtin(params, xdt, FALSE, FALSE, FALSE);
}

static SEXP materialize_altrep_list(SEXP value) {
  const R_xlen_t size = XLENGTH(value);
  SEXP materialized = PROTECT(Rf_allocVector(VECSXP, size));
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t index = 0; index < size; ++index) {
    account_work(&work_since_interrupt);
    SEXP element = PROTECT(VECTOR_ELT(value, index));
    SET_VECTOR_ELT(materialized, index, element);
    UNPROTECT(1);
  }
  /* Retain the post-materialization attribute surface.  An ALTREP element
   * callback may legally allocate or mutate reachable R state; the returned
   * ordinary shell is therefore also the sole input to any later R fallback,
   * so no callback is replayed after native admission has inspected it. */
  SHALLOW_DUPLICATE_ATTRIB(materialized, value);
  UNPROTECT(1);
  return materialized;
}

SEXP paradox_param_set_check_dt_plan_builtin(SEXP params, SEXP xdt) {
  if (TYPEOF(xdt) != VECSXP || !ALTREP(xdt)) {
    return check_dt_builtin(params, xdt, FALSE, FALSE, TRUE);
  }

  SEXP input = PROTECT(materialize_altrep_list(xdt));
  SEXP plan = PROTECT(check_dt_builtin(
    params,
    input,
    FALSE,
    FALSE,
    TRUE
  ));
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, plan);
  SET_VECTOR_ELT(result, 1, input);
  UNPROTECT(3);
  return result;
}

SEXP paradox_param_set_check_dt_complete_builtin(SEXP params, SEXP xdt) {
  return check_dt_builtin(params, xdt, TRUE, FALSE, FALSE);
}

SEXP paradox_param_set_check_dt_all_builtin(SEXP params, SEXP xdt) {
  return check_dt_builtin(params, xdt, TRUE, TRUE, FALSE);
}
