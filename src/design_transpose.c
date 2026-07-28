#include "paradox.h"

#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

/*
 * Snapshot Design$data once and emit independently owned named row shells.
 * Atomic scalars retain ordinary column classes (factor, Date, and similar
 * base representations); list-column leaves retain identity.  We deliberately
 * do not invoke S3 `[[` or `is.na` methods for arbitrary column classes.
 */

static SEXP snapshot_strings(SEXP source, const char *description,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(source) != STRSXP || ALTREP(source) || Rf_isS4(source) ||
      Rf_isObject(source) || !paradox_api_has_no_attributes(source)) {
    Rf_error("%s must be a character vector", description);
  }
  const R_xlen_t count = XLENGTH(source);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, count));
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP value = STRING_ELT(source, index);
    if (value == NA_STRING || Rf_getCharCE(value) == CE_BYTES ||
        CHAR(value)[0] == '\0') {
      UNPROTECT(1);
      Rf_error("%s contains an unsupported name", description);
    }
    SET_STRING_ELT(result, index, value);
  }
  UNPROTECT(1);
  return result;
}

static int ordinary_design_shell(SEXP data) {
  static const char *const names_only[] = {"names"};
  if (TYPEOF(data) != VECSXP || ALTREP(data) || Rf_isS4(data)) {
    return FALSE;
  }
  int valid_shell = FALSE;
  if (!Rf_isObject(data)) {
    valid_shell = paradox_api_has_only_attributes(data, names_only, 1);
  } else {
    valid_shell = paradox_public_table_kind(data) !=
      PARADOX_PUBLIC_TABLE_NONE;
  }
  if (!valid_shell) return FALSE;
  SEXP names = PROTECT(paradox_api_raw_attribute(data, R_NamesSymbol));
  const int valid_names = names == R_NilValue
    ? XLENGTH(data) == 0
    : TYPEOF(names) == STRSXP && !ALTREP(names) && !Rf_isS4(names) &&
      !Rf_isObject(names) && paradox_api_has_no_attributes(names) &&
      XLENGTH(names) == XLENGTH(data);
  UNPROTECT(1);
  return valid_names;
}

static int supported_column_type(SEXPTYPE type) {
  return type == LGLSXP || type == INTSXP || type == REALSXP ||
    type == CPLXSXP || type == STRSXP || type == RAWSXP ||
    type == VECSXP;
}

static void validate_attributes(SEXP source) {
  static const char *const allowed[] = {
    "names", "class", "levels", "tzone", "units"
  };
  if (!paradox_api_has_only_attributes(source, allowed, 5)) {
    Rf_error("Design columns have unsupported structural attributes");
  }
  const SEXP attributes[] = {
    Rf_getAttrib(source, R_NamesSymbol),
    Rf_getAttrib(source, R_ClassSymbol),
    Rf_getAttrib(source, R_LevelsSymbol),
    Rf_getAttrib(source, Rf_install("tzone")),
    Rf_getAttrib(source, Rf_install("units"))
  };
  for (size_t index = 0; index < 5; ++index) {
    if (attributes[index] != R_NilValue &&
        (ALTREP(attributes[index]) || Rf_isS4(attributes[index]) ||
          Rf_isObject(attributes[index]))) {
      Rf_error("Design columns must have ordinary structural attributes");
    }
  }
}

static SEXP snapshot_column(SEXP source,
    R_xlen_t *work_since_interrupt) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
  if (Rf_isS4(source) || !supported_column_type(type)) {
    Rf_error("Design contains an unsupported column type");
  }
  validate_attributes(source);

  const R_xlen_t count = XLENGTH(source);
  SEXP result = PROTECT(Rf_allocVector(type, count));
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_account_work(work_since_interrupt);
    switch (type) {
    case LGLSXP:
      SET_LOGICAL_ELT(result, index, LOGICAL_ELT(source, index));
      break;
    case INTSXP:
      SET_INTEGER_ELT(result, index, INTEGER_ELT(source, index));
      break;
    case REALSXP:
      SET_REAL_ELT(result, index, REAL_ELT(source, index));
      break;
    case CPLXSXP:
      paradox_api_set_complex_elt(
        result,
        index,
        COMPLEX_ELT(source, index)
      );
      break;
    case STRSXP:
      SET_STRING_ELT(result, index, STRING_ELT(source, index));
      break;
    case RAWSXP:
      paradox_api_set_raw_elt(result, index, RAW_ELT(source, index));
      break;
    case VECSXP:
      SET_VECTOR_ELT(result, index, VECTOR_ELT(source, index));
      break;
    default:
      UNPROTECT(1);
      Rf_error("Internal error: unsupported Design column type");
    }
  }
  DUPLICATE_ATTRIB(result, source);
  UNPROTECT(1);
  return result;
}

static int scalar_is_na(SEXP value) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  if (type != LGLSXP && type != INTSXP && type != REALSXP &&
      type != CPLXSXP && type != STRSXP) {
    return FALSE;
  }
  if (ALTREP(value) || XLENGTH(value) != 1) {
    return FALSE;
  }
  switch (type) {
  case LGLSXP:
    return LOGICAL_ELT(value, 0) == NA_LOGICAL;
  case INTSXP:
    return INTEGER_ELT(value, 0) == NA_INTEGER;
  case REALSXP:
    return ISNAN(REAL_ELT(value, 0));
  case CPLXSXP: {
    const Rcomplex scalar = COMPLEX_ELT(value, 0);
    return ISNAN(scalar.r) || ISNAN(scalar.i);
  }
  case STRSXP:
    return STRING_ELT(value, 0) == NA_STRING;
  default:
    return FALSE;
  }
}

static SEXP atomic_scalar(SEXP column, R_xlen_t row) {
  SEXP result;
  switch (TYPEOF(column)) {
  case LGLSXP:
    result = PROTECT(Rf_ScalarLogical(LOGICAL_ELT(column, row)));
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
  case RAWSXP:
    result = PROTECT(Rf_allocVector(RAWSXP, 1));
    paradox_api_set_raw_elt(result, 0, RAW_ELT(column, row));
    break;
  default:
    Rf_error("Internal error: non-atomic Design column");
    return R_NilValue;
  }
  DUPLICATE_ATTRIB(result, column);
  Rf_setAttrib(result, R_NamesSymbol, R_NilValue);
  Rf_setAttrib(result, R_DimSymbol, R_NilValue);
  Rf_setAttrib(result, R_DimNamesSymbol, R_NilValue);
  UNPROTECT(1);
  return result;
}

static SEXP scalar_from_column(SEXP column, R_xlen_t row) {
  return TYPEOF(column) == VECSXP
    ? VECTOR_ELT(column, row)
    : atomic_scalar(column, row);
}

static int parse_flag(SEXP value) {
  if (TYPEOF(value) != LGLSXP || XLENGTH(value) != 1) {
    Rf_error("`filter_na` must be one TRUE or FALSE value");
  }
  const int result = LOGICAL_ELT(value, 0);
  if (result == NA_LOGICAL) {
    Rf_error("`filter_na` must be one TRUE or FALSE value");
  }
  return result;
}

SEXP paradox_design_transpose(SEXP data, SEXP filter_na) {
  data = PROTECT(paradox_materialize_public_table_shell(data));
  PROTECT(filter_na);
  const int do_filter = parse_flag(filter_na);
  if (!ordinary_design_shell(data)) {
    UNPROTECT(2);
    Rf_error("Design$data must be a list-like data frame");
  }

  R_xlen_t work_since_interrupt = 0;
  const R_xlen_t column_count = XLENGTH(data);
  SEXP source_names = PROTECT(paradox_api_raw_attribute(
    data,
    R_NamesSymbol
  ));
  SEXP names = column_count == 0
    ? PROTECT(Rf_allocVector(STRSXP, 0))
    : PROTECT(snapshot_strings(
        source_names,
        "Design column names",
        &work_since_interrupt
      ));
  if (XLENGTH(names) != column_count ||
      Rf_any_duplicated(names, FALSE) != 0) {
    UNPROTECT(4);
    Rf_error("Design$data must have unique column names");
  }

  const int table_input = Rf_isObject(data) != FALSE;
  R_xlen_t table_rows = 0;
  if (table_input &&
      !paradox_public_table_row_count(data, &table_rows)) {
    UNPROTECT(4);
    Rf_error("Design$data has invalid data.frame row names");
  }

  SEXP columns = PROTECT(Rf_allocVector(VECSXP, column_count));
  R_xlen_t row_count = table_input ? table_rows : 0;
  for (R_xlen_t column = 0; column < column_count; ++column) {
    paradox_account_work(&work_since_interrupt);
    SEXP source = PROTECT(VECTOR_ELT(data, column));
    SEXP frozen = PROTECT(snapshot_column(source, &work_since_interrupt));
    const R_xlen_t rows = XLENGTH(frozen);
    if (rows != row_count && (table_input || column != 0)) {
      UNPROTECT(7);
      Rf_error(column == 0
        ? "Design$data has invalid data.frame row names"
        : "Design$data columns have inconsistent lengths");
    }
    if (!table_input && column == 0) {
      row_count = rows;
    }
    SET_VECTOR_ELT(columns, column, frozen);
    UNPROTECT(2);
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, row_count));
  for (R_xlen_t row_index = 0; row_index < row_count; ++row_index) {
    paradox_account_work(&work_since_interrupt);
    SEXP row = PROTECT(Rf_allocVector(VECSXP, column_count));
    SEXP row_names = PROTECT(Rf_allocVector(STRSXP, column_count));
    R_xlen_t target = 0;
    for (R_xlen_t column = 0; column < column_count; ++column) {
      paradox_account_work(&work_since_interrupt);
      SEXP value = PROTECT(scalar_from_column(
        VECTOR_ELT(columns, column),
        row_index
      ));
      if (!do_filter || !scalar_is_na(value)) {
        SET_VECTOR_ELT(row, target, value);
        SET_STRING_ELT(row_names, target, STRING_ELT(names, column));
        ++target;
      }
      UNPROTECT(1);
    }

    SEXP final_row = row;
    SEXP final_names = row_names;
    int trim_protects = 0;
    if (target != column_count) {
      final_row = PROTECT(Rf_xlengthgets(row, target));
      ++trim_protects;
      final_names = PROTECT(Rf_xlengthgets(row_names, target));
      ++trim_protects;
    }
    Rf_setAttrib(final_row, R_NamesSymbol, final_names);
    SET_VECTOR_ELT(result, row_index, final_row);
    UNPROTECT(2 + trim_protects);
  }

  UNPROTECT(6);
  return result;
}
