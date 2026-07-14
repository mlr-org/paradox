#include <string.h>

#include "paramset_domain_common.h"
#include <R_ext/Utils.h>

#include "r_api_compat.h"
#include "r_utils.h"

typedef enum {
  DOMAIN_KIND_UNKNOWN = 0,
  DOMAIN_KIND_DBL,
  DOMAIN_KIND_INT,
  DOMAIN_KIND_FCT,
  DOMAIN_KIND_LGL,
  DOMAIN_KIND_UTY
} domain_kind_t;

static const char *const domain_column_names[PARADOX_DOMAIN_COLUMN_COUNT] = {
  "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
  "levels", "special_vals", "default", "storage_type", ".tags",
  ".trafo", ".requirements", ".init_given", ".init"
};

static const char *const permanent_column_names[PARADOX_DOMAIN_TAGS] = {
  "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
  "levels", "special_vals", "default", "storage_type"
};

static const SEXPTYPE permanent_column_types[PARADOX_DOMAIN_TAGS] = {
  STRSXP, STRSXP, STRSXP, VECSXP, REALSXP, REALSXP, REALSXP, VECSXP,
  VECSXP, VECSXP, STRSXP
};

void paradox_domain_account_work(R_xlen_t *work_since_interrupt) {
  ++*work_since_interrupt;
  if (*work_since_interrupt >= PARADOX_INTERRUPT_CHECK_INTERVAL) {
    R_CheckUserInterrupt();
    *work_since_interrupt = 0;
  }
}

int paradox_domain_string_is(SEXP string, const char *expected) {
  return string != NA_STRING && strcmp(CHAR(string), expected) == 0;
}

static int has_no_attributes(SEXP value) {
  return paradox_api_has_no_attributes(value);
}

int paradox_domain_strings_equal(SEXP left, SEXP right) {
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

int paradox_domain_exact_string_vector(SEXP value,
    const char *const *expected, R_xlen_t size,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(value) != STRSXP || ALTREP(value)) {
    return FALSE;
  }
  const R_xlen_t observed_size = XLENGTH(value);
  if (observed_size != size) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if (!paradox_domain_string_is(STRING_ELT(value, index), expected[index])) {
      return FALSE;
    }
  }
  return TRUE;
}

static int exact_data_table(SEXP table, const char *const *column_names,
    R_xlen_t column_count, R_xlen_t *work_since_interrupt) {
  static const char *const table_classes[] = {"data.table", "data.frame"};
  if (TYPEOF(table) != VECSXP || ALTREP(table)) {
    return FALSE;
  }
  PROTECT(table);
  const R_xlen_t observed_column_count = XLENGTH(table);
  SEXP names = PROTECT(Rf_getAttrib(table, R_NamesSymbol));
  SEXP classes = PROTECT(Rf_getAttrib(table, R_ClassSymbol));
  const int valid = observed_column_count == column_count &&
    has_no_attributes(names) &&
    has_no_attributes(classes) &&
    paradox_domain_exact_string_vector(
      names,
      column_names,
      column_count,
      work_since_interrupt
    ) &&
    paradox_domain_exact_string_vector(
      classes,
      table_classes,
      2,
      work_since_interrupt
    );
  UNPROTECT(3);
  return valid;
}

static int keyed_by_id(SEXP table, R_xlen_t *work_since_interrupt) {
  static const char *const key[] = {"id"};
  PROTECT(table);
  SEXP sorted = PROTECT(Rf_getAttrib(table, Rf_install("sorted")));
  const int valid = has_no_attributes(sorted) &&
    paradox_domain_exact_string_vector(
    sorted,
    key,
    1,
    work_since_interrupt
  );
  UNPROTECT(2);
  return valid;
}

SEXP paradox_domain_local_value(SEXP environment, const char *name) {
  if (TYPEOF(environment) != ENVSXP) {
    return R_UnboundValue;
  }
  SEXP symbol = Rf_install(name);
  /* The facade accepts only an ordinary direct binding. It never evaluates
   * active or delayed bindings, and fails closed on R releases that cannot
   * classify binding kinds through the public API. */
  return paradox_api_local_value(environment, symbol);
}

int paradox_domain_owns_private_environment(SEXP self,
    SEXP private_environment) {
  if (TYPEOF(self) != ENVSXP || TYPEOF(private_environment) != ENVSXP) {
    return FALSE;
  }
  SEXP enclosure = PROTECT(paradox_domain_local_value(
    self,
    ".__enclos_env__"
  ));
  if (TYPEOF(enclosure) != ENVSXP) {
    UNPROTECT(1);
    return FALSE;
  }
  const int owned = paradox_domain_local_value(enclosure, "private") ==
    private_environment;
  UNPROTECT(1);
  return owned;
}

static domain_kind_t domain_kind(SEXP cls, SEXP storage_type) {
  if (paradox_domain_string_is(cls, "ParamDbl") &&
      paradox_domain_string_is(storage_type, "numeric")) {
    return DOMAIN_KIND_DBL;
  }
  if (paradox_domain_string_is(cls, "ParamInt") &&
      paradox_domain_string_is(storage_type, "integer")) {
    return DOMAIN_KIND_INT;
  }
  if (paradox_domain_string_is(cls, "ParamFct") &&
      paradox_domain_string_is(storage_type, "character")) {
    return DOMAIN_KIND_FCT;
  }
  if (paradox_domain_string_is(cls, "ParamLgl") &&
      paradox_domain_string_is(storage_type, "logical")) {
    return DOMAIN_KIND_LGL;
  }
  if (paradox_domain_string_is(cls, "ParamUty") &&
      paradox_domain_string_is(storage_type, "list")) {
    return DOMAIN_KIND_UTY;
  }
  return DOMAIN_KIND_UNKNOWN;
}

static int canonical_levels(domain_kind_t kind, SEXP levels,
    R_xlen_t *work_since_interrupt) {
  switch (kind) {
  case DOMAIN_KIND_DBL:
  case DOMAIN_KIND_INT:
  case DOMAIN_KIND_UTY:
    return levels == R_NilValue;
  case DOMAIN_KIND_LGL:
    if (TYPEOF(levels) != LGLSXP || ALTREP(levels) ||
        !has_no_attributes(levels)) {
      return FALSE;
    }
    return XLENGTH(levels) == 2 && LOGICAL_ELT(levels, 0) == TRUE &&
      LOGICAL_ELT(levels, 1) == FALSE;
  case DOMAIN_KIND_FCT:
    if (TYPEOF(levels) != STRSXP || ALTREP(levels) ||
        !has_no_attributes(levels)) {
      return FALSE;
    }
    const R_xlen_t level_count = XLENGTH(levels);
    for (R_xlen_t level = 0; level < level_count; ++level) {
      paradox_domain_account_work(work_since_interrupt);
      if (STRING_ELT(levels, level) == NA_STRING) {
        return FALSE;
      }
    }
    return Rf_any_duplicated(levels, FALSE) == 0;
  case DOMAIN_KIND_UNKNOWN:
    return FALSE;
  }
  return FALSE;
}

static int validate_params_rooted(SEXP params, SEXP selected_id,
    int validate_all_rows, paradox_domain_params_t *result,
    R_xlen_t *selected_row, R_xlen_t *work_since_interrupt, SEXP roots) {
  SEXP ids = VECTOR_ELT(roots, PARADOX_DOMAIN_ID);
  if (TYPEOF(ids) != STRSXP || ALTREP(ids) || !has_no_attributes(ids)) {
    return FALSE;
  }
  const R_xlen_t row_count = XLENGTH(ids);
  for (enum paradox_domain_column column = PARADOX_DOMAIN_CLS;
      column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP value = VECTOR_ELT(roots, column);
    const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
    const int numeric = column == PARADOX_DOMAIN_LOWER ||
      column == PARADOX_DOMAIN_UPPER ||
      column == PARADOX_DOMAIN_TOLERANCE;
    if (ALTREP(value) || (numeric
          ? type != REALSXP && type != INTSXP
          : type != permanent_column_types[column]) ||
        !has_no_attributes(value) || XLENGTH(value) != row_count) {
      return FALSE;
    }
  }

  SEXP classes = VECTOR_ELT(roots, PARADOX_DOMAIN_CLS);
  SEXP grouping = VECTOR_ELT(roots, PARADOX_DOMAIN_GROUPING);
  SEXP storage_types = VECTOR_ELT(roots, PARADOX_DOMAIN_STORAGE_TYPE);
  R_xlen_t match_count = 0;
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    if (STRING_ELT(ids, row) == NA_STRING ||
        STRING_ELT(grouping, row) == NA_STRING ||
        domain_kind(
          STRING_ELT(classes, row),
          STRING_ELT(storage_types, row)
        ) == DOMAIN_KIND_UNKNOWN) {
      return FALSE;
    }
    if (selected_id != R_NilValue && paradox_domain_strings_equal(
        STRING_ELT(ids, row),
        STRING_ELT(selected_id, 0)
      )) {
      *selected_row = row;
      ++match_count;
    }
  }
  if (Rf_any_duplicated(ids, FALSE) != 0 ||
      (selected_id != R_NilValue && match_count != 1)) {
    return FALSE;
  }

  SEXP levels = VECTOR_ELT(roots, PARADOX_DOMAIN_LEVELS);
  SEXP special_values = VECTOR_ELT(roots, PARADOX_DOMAIN_SPECIAL_VALS);
  if (validate_all_rows) {
    for (R_xlen_t row = 0; row < row_count; ++row) {
      paradox_domain_account_work(work_since_interrupt);
      const domain_kind_t kind = domain_kind(
        STRING_ELT(classes, row),
        STRING_ELT(storage_types, row)
      );
      SEXP row_levels = PROTECT(VECTOR_ELT(levels, row));
      SEXP row_special_values = PROTECT(VECTOR_ELT(special_values, row));
      SEXP special_classes = PROTECT(Rf_getAttrib(
        row_special_values,
        R_ClassSymbol
      ));
      const int valid = canonical_levels(
          kind,
          row_levels,
          work_since_interrupt
        ) && TYPEOF(row_special_values) == VECSXP &&
          !ALTREP(row_special_values) &&
          (special_classes == R_NilValue ||
           (TYPEOF(special_classes) == STRSXP && !ALTREP(special_classes))) &&
          !Rf_inherits(row_special_values, "data.frame");
      UNPROTECT(3);
      if (!valid) {
        return FALSE;
      }
    }
  } else {
    if (selected_id == R_NilValue) {
      return FALSE;
    }
    const domain_kind_t kind = domain_kind(
      STRING_ELT(classes, *selected_row),
      STRING_ELT(storage_types, *selected_row)
    );
    SEXP row_levels = PROTECT(VECTOR_ELT(levels, *selected_row));
    SEXP row_special_values = PROTECT(VECTOR_ELT(
      special_values,
      *selected_row
    ));
    SEXP special_classes = PROTECT(Rf_getAttrib(
      row_special_values,
      R_ClassSymbol
    ));
    const int valid = canonical_levels(
        kind,
        row_levels,
        work_since_interrupt
      ) && TYPEOF(row_special_values) == VECSXP &&
        !ALTREP(row_special_values) &&
        (special_classes == R_NilValue ||
         (TYPEOF(special_classes) == STRSXP && !ALTREP(special_classes))) &&
        !Rf_inherits(row_special_values, "data.frame");
    UNPROTECT(3);
    if (!valid) {
      return FALSE;
    }
  }

  result->table = params;
  result->ids = ids;
  result->classes = classes;
  result->row_count = row_count;
  return TRUE;
}

int paradox_domain_validate_params(SEXP params, SEXP selected_id,
    int validate_all_rows, paradox_domain_params_t *result,
    R_xlen_t *selected_row, R_xlen_t *work_since_interrupt) {
  /* Allocate the root plan before inspecting any table attribute. A GC
   * finalizer may replace a user-visible data.table column during allocation;
   * validation must consistently observe and retain the post-allocation
   * children rather than leave earlier borrowed children bare. */
  PROTECT(params);
  PROTECT(selected_id);
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, PARADOX_DOMAIN_TAGS));
  if (!exact_data_table(
      params,
      permanent_column_names,
      PARADOX_DOMAIN_TAGS,
      work_since_interrupt
    ) || (selected_id != R_NilValue &&
      (TYPEOF(selected_id) != STRSXP || ALTREP(selected_id) ||
       XLENGTH(selected_id) != 1 || STRING_ELT(selected_id, 0) == NA_STRING))) {
    UNPROTECT(3);
    return FALSE;
  }

  for (enum paradox_domain_column column = PARADOX_DOMAIN_ID;
      column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    SEXP child = PROTECT(VECTOR_ELT(params, column));
    SET_VECTOR_ELT(roots, column, child);
    UNPROTECT(1);
  }
  const int valid = validate_params_rooted(
    params,
    selected_id,
    validate_all_rows,
    result,
    selected_row,
    work_since_interrupt,
    roots
  );
  UNPROTECT(3);
  return valid;
}

int paradox_domain_validate_tags(SEXP tags, paradox_domain_tags_t *result,
    R_xlen_t *work_since_interrupt) {
  static const char *const column_names[] = {"id", "tag"};
  PROTECT(tags);
  if (!exact_data_table(tags, column_names, 2, work_since_interrupt) ||
      !keyed_by_id(tags, work_since_interrupt)) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP ids = PROTECT(VECTOR_ELT(tags, 0));
  SEXP values = PROTECT(VECTOR_ELT(tags, 1));
  if (TYPEOF(ids) != STRSXP || TYPEOF(values) != STRSXP || ALTREP(ids) ||
      ALTREP(values) || !has_no_attributes(ids) ||
      !has_no_attributes(values)) {
    UNPROTECT(3);
    return FALSE;
  }
  const R_xlen_t row_count = XLENGTH(ids);
  const R_xlen_t value_count = XLENGTH(values);
  if (value_count != row_count) {
    UNPROTECT(3);
    return FALSE;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    if (STRING_ELT(ids, row) == NA_STRING ||
        STRING_ELT(values, row) == NA_STRING) {
      UNPROTECT(3);
      return FALSE;
    }
  }
  result->ids = ids;
  result->values = values;
  result->row_count = row_count;
  UNPROTECT(3);
  return TRUE;
}

int paradox_domain_validate_trafos(SEXP trafos,
    paradox_domain_trafos_t *result,
    R_xlen_t *work_since_interrupt) {
  static const char *const column_names[] = {"id", "trafo"};
  PROTECT(trafos);
  if (!exact_data_table(trafos, column_names, 2, work_since_interrupt) ||
      !keyed_by_id(trafos, work_since_interrupt)) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP ids = PROTECT(VECTOR_ELT(trafos, 0));
  SEXP values = PROTECT(VECTOR_ELT(trafos, 1));
  if (TYPEOF(ids) != STRSXP || TYPEOF(values) != VECSXP || ALTREP(ids) ||
      ALTREP(values) || !has_no_attributes(ids) ||
      !has_no_attributes(values)) {
    UNPROTECT(3);
    return FALSE;
  }
  const R_xlen_t row_count = XLENGTH(ids);
  const R_xlen_t value_count = XLENGTH(values);
  if (value_count != row_count) {
    UNPROTECT(3);
    return FALSE;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    if (STRING_ELT(ids, row) == NA_STRING ||
        !Rf_isFunction(VECTOR_ELT(values, row))) {
      UNPROTECT(3);
      return FALSE;
    }
  }
  result->ids = ids;
  result->values = values;
  result->row_count = row_count;
  UNPROTECT(3);
  return TRUE;
}

int paradox_domain_validate_dependencies(SEXP dependencies,
    paradox_domain_dependencies_t *result,
    R_xlen_t *work_since_interrupt) {
  static const char *const column_names[] = {"id", "on", "cond"};
  PROTECT(dependencies);
  if (!exact_data_table(
      dependencies,
      column_names,
      3,
      work_since_interrupt
    )) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP ids = PROTECT(VECTOR_ELT(dependencies, 0));
  SEXP on = PROTECT(VECTOR_ELT(dependencies, 1));
  SEXP conditions = PROTECT(VECTOR_ELT(dependencies, 2));
  if (TYPEOF(ids) != STRSXP || TYPEOF(on) != STRSXP ||
      TYPEOF(conditions) != VECSXP || ALTREP(ids) || ALTREP(on) ||
      ALTREP(conditions)) {
    UNPROTECT(4);
    return FALSE;
  }
  const R_xlen_t row_count = XLENGTH(ids);
  const R_xlen_t on_count = XLENGTH(on);
  const R_xlen_t condition_count = XLENGTH(conditions);
  if (!has_no_attributes(ids) ||
      !has_no_attributes(on) || !has_no_attributes(conditions) ||
      on_count != row_count || condition_count != row_count) {
    UNPROTECT(4);
    return FALSE;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP condition = PROTECT(VECTOR_ELT(conditions, row));
    SEXP condition_classes = PROTECT(Rf_getAttrib(
      condition,
      R_ClassSymbol
    ));
    if (STRING_ELT(ids, row) == NA_STRING ||
        STRING_ELT(on, row) == NA_STRING ||
        ALTREP(condition) ||
        (condition_classes != R_NilValue &&
         (TYPEOF(condition_classes) != STRSXP || ALTREP(condition_classes))) ||
        !Rf_inherits(condition, "Condition")) {
      UNPROTECT(6);
      return FALSE;
    }
    UNPROTECT(2);
  }
  result->ids = ids;
  result->on = on;
  result->conditions = conditions;
  result->row_count = row_count;
  UNPROTECT(4);
  return TRUE;
}

int paradox_domain_validate_values(SEXP values,
    paradox_domain_values_t *result,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(values) != VECSXP || ALTREP(values) || Rf_isObject(values)) {
    return FALSE;
  }
  PROTECT(values);
  const R_xlen_t value_count = XLENGTH(values);
  SEXP names = PROTECT(Rf_getAttrib(values, R_NamesSymbol));
  if (TYPEOF(names) != STRSXP || ALTREP(names) ||
      !has_no_attributes(names)) {
    UNPROTECT(2);
    return FALSE;
  }
  const R_xlen_t name_count = XLENGTH(names);
  if (name_count != value_count) {
    UNPROTECT(2);
    return FALSE;
  }
  for (R_xlen_t index = 0; index < name_count; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if (STRING_ELT(names, index) == NA_STRING) {
      UNPROTECT(2);
      return FALSE;
    }
  }
  if (Rf_any_duplicated(names, FALSE) != 0) {
    UNPROTECT(2);
    return FALSE;
  }
  result->values = values;
  result->names = names;
  result->size = value_count;
  UNPROTECT(2);
  return TRUE;
}

static void set_scalar_column(SEXP result,
    enum paradox_domain_column output_column, SEXP source,
    R_xlen_t source_row) {
  PROTECT(source);
  SEXP column = PROTECT(Rf_allocVector((SEXPTYPE) TYPEOF(source), 1));
  switch (TYPEOF(source)) {
  case STRSXP:
    SET_STRING_ELT(column, 0, STRING_ELT(source, source_row));
    break;
  case VECSXP:
    SET_VECTOR_ELT(column, 0, VECTOR_ELT(source, source_row));
    break;
  case REALSXP:
    SET_REAL_ELT(column, 0, REAL_ELT(source, source_row));
    break;
  case INTSXP:
    SET_INTEGER_ELT(column, 0, INTEGER_ELT(source, source_row));
    break;
  default:
    Rf_error("Internal error: unsupported ParamSet Domain column type");
  }
  SET_VECTOR_ELT(result, output_column, column);
  UNPROTECT(2);
}

static SEXP set_domain_attributes(SEXP result, SEXP cls,
    R_xlen_t *work_since_interrupt) {
  PROTECT(cls);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, PARADOX_DOMAIN_COLUMN_COUNT));
  for (R_xlen_t column = 0;
      column < PARADOX_DOMAIN_COLUMN_COUNT;
      ++column) {
    paradox_domain_account_work(work_since_interrupt);
    SET_STRING_ELT(names, column, Rf_mkChar(domain_column_names[column]));
  }
  Rf_setAttrib(result, R_NamesSymbol, names);

  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 4));
  SET_STRING_ELT(classes, 0, cls);
  SET_STRING_ELT(classes, 1, Rf_mkChar("Domain"));
  SET_STRING_ELT(classes, 2, Rf_mkChar("data.table"));
  SET_STRING_ELT(classes, 3, Rf_mkChar("data.frame"));
  Rf_setAttrib(result, R_ClassSymbol, classes);

  SEXP row_names = PROTECT(Rf_allocVector(INTSXP, 2));
  SET_INTEGER_ELT(row_names, 0, NA_INTEGER);
  SET_INTEGER_ELT(row_names, 1, -1);
  Rf_setAttrib(result, R_RowNamesSymbol, row_names);

  SEXP prepared = PROTECT(paradox_prepare_data_table(result, TRUE));
  SEXP prepared_names = PROTECT(Rf_getAttrib(prepared, R_NamesSymbol));
  Rf_setAttrib(prepared, R_NamesSymbol, R_NilValue);
  Rf_setAttrib(prepared, R_NamesSymbol, prepared_names);
  UNPROTECT(6);
  return prepared;
}

SEXP paradox_domain_fill(SEXP domain, const paradox_domain_row_t *row,
    R_xlen_t *work_since_interrupt) {
  paradox_domain_account_work(work_since_interrupt);
  for (enum paradox_domain_column column = PARADOX_DOMAIN_ID;
      column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    paradox_domain_account_work(work_since_interrupt);
    set_scalar_column(
      domain,
      column,
      VECTOR_ELT(row->params->table, column),
      row->parameter_row
    );
  }

  SEXP selected_tags = PROTECT(Rf_allocVector(STRSXP, row->tag_count));
  for (R_xlen_t index = 0; index < row->tag_count; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    SET_STRING_ELT(
      selected_tags,
      index,
      STRING_ELT(row->tags->values, row->tag_rows[index])
    );
  }
  SEXP tag_column = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(tag_column, 0, selected_tags);
  SET_VECTOR_ELT(domain, PARADOX_DOMAIN_TAGS, tag_column);
  UNPROTECT(2);

  SEXP trafo_column = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(trafo_column, 0, row->trafo);
  SET_VECTOR_ELT(domain, PARADOX_DOMAIN_TRAFO, trafo_column);
  UNPROTECT(1);

  SEXP requirements_column = PROTECT(Rf_allocVector(VECSXP, 1));
  if (row->dependency_count == 0) {
    SET_VECTOR_ELT(requirements_column, 0, R_NilValue);
  } else {
    SEXP requirements = PROTECT(Rf_allocVector(
      VECSXP,
      row->dependency_count
    ));
    for (R_xlen_t index = 0; index < row->dependency_count; ++index) {
      paradox_domain_account_work(work_since_interrupt);
      const R_xlen_t dependency_row = row->dependency_rows[index];
      SEXP requirement = PROTECT(Rf_allocVector(VECSXP, 2));
      SEXP on = PROTECT(Rf_allocVector(STRSXP, 1));
      SET_STRING_ELT(
        on,
        0,
        STRING_ELT(row->dependencies->on, dependency_row)
      );
      SET_VECTOR_ELT(requirement, 0, on);
      SET_VECTOR_ELT(
        requirement,
        1,
        VECTOR_ELT(row->dependencies->conditions, dependency_row)
      );
      SEXP requirement_names = PROTECT(Rf_allocVector(STRSXP, 2));
      SET_STRING_ELT(requirement_names, 0, Rf_mkChar("on"));
      SET_STRING_ELT(requirement_names, 1, Rf_mkChar("cond"));
      Rf_setAttrib(requirement, R_NamesSymbol, requirement_names);
      SET_VECTOR_ELT(requirements, index, requirement);
      UNPROTECT(3);
    }
    SET_VECTOR_ELT(requirements_column, 0, requirements);
    UNPROTECT(1);
  }
  SET_VECTOR_ELT(domain, PARADOX_DOMAIN_REQUIREMENTS, requirements_column);
  UNPROTECT(1);

  SEXP init_given_column = PROTECT(Rf_allocVector(LGLSXP, 1));
  SET_LOGICAL_ELT(init_given_column, 0, row->init_given);
  SET_VECTOR_ELT(domain, PARADOX_DOMAIN_INIT_GIVEN, init_given_column);
  SEXP init_column = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(init_column, 0, row->init_value);
  SET_VECTOR_ELT(domain, PARADOX_DOMAIN_INIT, init_column);
  UNPROTECT(2);

  return set_domain_attributes(
    domain,
    STRING_ELT(row->params->classes, row->parameter_row),
    work_since_interrupt
  );
}
