#include <string.h>

#include "paradox.h"

/*
 * Turn the columns of an ordinary data frame into a list of named row lists.
 *
 * A classed atomic vector may have S3 subsetting semantics (factor, Date,
 * integer64, and user classes), so this kernel deliberately returns NULL for
 * those inputs. The R wrapper then uses mlr3misc::transpose_list(), retaining
 * historical dispatch. List-column elements are reused exactly; when NA
 * filtering could dispatch on a classed scalar element, the operation likewise
 * falls back.
 */

static inline void account_work(R_xlen_t *work_since_interrupt) {
  ++*work_since_interrupt;
  if (*work_since_interrupt >= PARADOX_INTERRUPT_CHECK_INTERVAL) {
    R_CheckUserInterrupt();
    *work_since_interrupt = 0;
  }
}

static int supported_container(SEXP data) {
  /* The R fallback is the sole observer of callback-capable containers.  In
   * particular, do not authenticate an ALTREP shell and then let Length or
   * Elt callbacks invalidate that decision. */
  if (ALTREP(data)) {
    return 0;
  }
  if (!Rf_isObject(data)) {
    return 1;
  }

  SEXP classes = PROTECT(Rf_getAttrib(data, R_ClassSymbol));
  int supported = TYPEOF(classes) == STRSXP && !ALTREP(classes) &&
    XLENGTH(classes) == 2;
  if (supported) {
    SEXP first = STRING_ELT(classes, 0);
    SEXP second = STRING_ELT(classes, 1);
    supported = first != NA_STRING && second != NA_STRING &&
      strcmp(CHAR(first), "data.table") == 0 &&
      strcmp(CHAR(second), "data.frame") == 0;
  }
  UNPROTECT(1);
  return supported;
}

static int supported_column(SEXP column) {
  /* Reading either the length or an element of an ALTREP column can execute
   * arbitrary R code.  Decline before either operation so fallback cannot
   * replay a partially observed source. */
  if (ALTREP(column)) {
    return 0;
  }
  if (Rf_getAttrib(column, R_NamesSymbol) != R_NilValue) {
    return 0;
  }

  switch (TYPEOF(column)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:
      return !Rf_isObject(column);
    case VECSXP:
      return !Rf_isObject(column);
    default:
      return 0;
  }
}

static int atomic_scalar_na(SEXP value, int *supported) {
  *supported = 1;
  if (ALTREP(value) || Rf_isObject(value)) {
    *supported = 0;
    return 0;
  }
  switch (TYPEOF(value)) {
    case LGLSXP:
      if (XLENGTH(value) != 1) {
        return 0;
      }
      return LOGICAL_ELT(value, 0) == NA_LOGICAL;
    case INTSXP:
      if (XLENGTH(value) != 1) {
        return 0;
      }
      return INTEGER_ELT(value, 0) == NA_INTEGER;
    case REALSXP:
      if (XLENGTH(value) != 1) {
        return 0;
      }
      return ISNAN(REAL_ELT(value, 0));
    case CPLXSXP: {
      Rcomplex scalar;
      if (XLENGTH(value) != 1) {
        return 0;
      }
      scalar = COMPLEX_ELT(value, 0);
      return ISNAN(scalar.r) || ISNAN(scalar.i);
    }
    case STRSXP:
      if (XLENGTH(value) != 1) {
        return 0;
      }
      return STRING_ELT(value, 0) == NA_STRING;
    case RAWSXP:
      if (XLENGTH(value) != 1) {
        return 0;
      }
      return 0;
    default:
      return 0;
  }
}

static SEXP scalar_from_column(SEXP column, R_xlen_t row) {
  switch (TYPEOF(column)) {
    case LGLSXP:
      return Rf_ScalarLogical(LOGICAL_ELT(column, row));
    case INTSXP:
      return Rf_ScalarInteger(INTEGER_ELT(column, row));
    case REALSXP:
      return Rf_ScalarReal(REAL_ELT(column, row));
    case CPLXSXP:
      return Rf_ScalarComplex(COMPLEX_ELT(column, row));
    case STRSXP:
      return Rf_ScalarString(STRING_ELT(column, row));
    case RAWSXP: {
      SEXP scalar = Rf_allocVector(RAWSXP, 1);
      RAW(scalar)[0] = RAW_ELT(column, row);
      return scalar;
    }
    case VECSXP:
      return VECTOR_ELT(column, row);
    default:
      return R_NilValue;
  }
}

SEXP paradox_design_transpose(SEXP data, SEXP filter_na) {
  R_xlen_t column_count;
  R_xlen_t row_count;
  R_xlen_t column_index;
  R_xlen_t row_index;
  int do_filter;
  SEXP column_names;
  SEXP output_names;
  SEXP result;
  R_xlen_t work_since_interrupt = 0;

  if (TYPEOF(data) != VECSXP || ALTREP(data) ||
      TYPEOF(filter_na) != LGLSXP ||
      ALTREP(filter_na) ||
      XLENGTH(filter_na) != 1) {
    return R_NilValue;
  }
  do_filter = LOGICAL_ELT(filter_na, 0);
  if (do_filter == NA_LOGICAL || !supported_container(data)) {
    return R_NilValue;
  }
  column_count = XLENGTH(data);
  if (column_count == 0) {
    return Rf_allocVector(VECSXP, 0);
  }

  PROTECT(column_names = Rf_getAttrib(data, R_NamesSymbol));
  if (TYPEOF(column_names) != STRSXP || ALTREP(column_names) ||
      XLENGTH(column_names) != column_count) {
    UNPROTECT(1);
    return R_NilValue;
  }
  /* Own the names used by every output row. */
  PROTECT(output_names = Rf_allocVector(STRSXP, column_count));
  for (column_index = 0; column_index < column_count; ++column_index) {
    account_work(&work_since_interrupt);
    SEXP name = STRING_ELT(column_names, column_index);
    if (name == NA_STRING || CHAR(name)[0] == '\0') {
      UNPROTECT(2);
      return R_NilValue;
    }
    SET_STRING_ELT(output_names, column_index, name);
  }

  /* Keep every validated child independently rooted while output allocation
   * can trigger collection. */
  SEXP columns = PROTECT(Rf_allocVector(VECSXP, column_count));
  row_count = 0;
  for (column_index = 0; column_index < column_count; ++column_index) {
    account_work(&work_since_interrupt);
    SEXP column = PROTECT(VECTOR_ELT(data, column_index));
    if (!supported_column(column)) {
      UNPROTECT(4);
      return R_NilValue;
    }
    const R_xlen_t column_size = XLENGTH(column);
    if (column_index != 0 && column_size != row_count) {
      UNPROTECT(4);
      return R_NilValue;
    }
    if (column_index == 0) {
      row_count = column_size;
    }
    SET_VECTOR_ELT(columns, column_index, column);
    UNPROTECT(1);
  }

  PROTECT(result = Rf_allocVector(VECSXP, row_count));
  for (row_index = 0; row_index < row_count; ++row_index) {
    R_xlen_t target_index = 0;
    SEXP row;
    SEXP row_names;

    account_work(&work_since_interrupt);
    PROTECT(row = Rf_allocVector(VECSXP, column_count));
    PROTECT(row_names = Rf_allocVector(STRSXP, column_count));
    for (column_index = 0; column_index < column_count; ++column_index) {
      account_work(&work_since_interrupt);
      SEXP column = VECTOR_ELT(columns, column_index);
      SEXP value = PROTECT(scalar_from_column(column, row_index));
      if (do_filter) {
        int supported;
        const int is_na = atomic_scalar_na(value, &supported);
        if (!supported) {
          UNPROTECT(7);
          return R_NilValue;
        }
        if (is_na) {
          UNPROTECT(1);
          continue;
        }
      }
      if (target_index >= column_count) {
        UNPROTECT(7);
        Rf_error("Internal error: transpose row exceeded its capacity");
      }
      SET_VECTOR_ELT(row, target_index, value);
      SET_STRING_ELT(
        row_names, target_index, STRING_ELT(output_names, column_index)
      );
      ++target_index;
      UNPROTECT(1);
    }

    SEXP final_row = row;
    SEXP final_names = row_names;
    int trimmed_protects = 0;
    if (target_index != column_count) {
      final_row = PROTECT(Rf_xlengthgets(row, target_index));
      ++trimmed_protects;
      final_names = PROTECT(Rf_xlengthgets(row_names, target_index));
      ++trimmed_protects;
    }
    if (XLENGTH(final_row) != target_index ||
        XLENGTH(final_names) != target_index) {
      UNPROTECT(6 + trimmed_protects);
      Rf_error("Internal error: incomplete transpose row");
    }
    Rf_setAttrib(final_row, R_NamesSymbol, final_names);
    SET_VECTOR_ELT(result, row_index, final_row);
    UNPROTECT(2 + trimmed_protects);
  }
  UNPROTECT(4);
  return result;
}
