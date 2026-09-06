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

static void validate_snapshot_names(SEXP names, const char *description,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t count = XLENGTH(names);
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP value = STRING_ELT(names, index);
    if (value == NA_STRING || Rf_getCharCE(value) == CE_BYTES ||
        CHAR(value)[0] == '\0') {
      Rf_error("%s contains an unsupported name", description);
    }
  }
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

static SEXP design_tzone_symbol = NULL;
static SEXP design_units_symbol = NULL;

enum design_attribute {
  DESIGN_ATTRIBUTE_NAMES = 0,
  DESIGN_ATTRIBUTE_CLASS,
  DESIGN_ATTRIBUTE_LEVELS,
  DESIGN_ATTRIBUTE_TZONE,
  DESIGN_ATTRIBUTE_UNITS,
  DESIGN_ATTRIBUTE_COUNT
};

static void initialize_design_attribute_symbols(void) {
  if (design_tzone_symbol != NULL) return;
  design_tzone_symbol = Rf_install("tzone");
  design_units_symbol = Rf_install("units");
}

static SEXP design_attribute_symbol(enum design_attribute attribute) {
  switch (attribute) {
  case DESIGN_ATTRIBUTE_NAMES:
    return R_NamesSymbol;
  case DESIGN_ATTRIBUTE_CLASS:
    return R_ClassSymbol;
  case DESIGN_ATTRIBUTE_LEVELS:
    return R_LevelsSymbol;
  case DESIGN_ATTRIBUTE_TZONE:
    return design_tzone_symbol;
  case DESIGN_ATTRIBUTE_UNITS:
    return design_units_symbol;
  default:
    Rf_error("Internal error: invalid Design attribute");
  }
  return R_NilValue;
}

static int ordinary_design_attribute(SEXP value) {
  return value == R_NilValue ||
    (TYPEOF(value) == STRSXP && !ALTREP(value) && !Rf_isS4(value) &&
      !Rf_isObject(value) && paradox_api_has_no_attributes(value));
}

static int bounded_design_attribute_count(
    SEXP source, R_xlen_t *count) {
  if (paradox_api_has_no_attributes(source)) {
    *count = 0;
    return TRUE;
  }
  return paradox_api_map_bounded_stored_attributes(
    source,
    DESIGN_ATTRIBUTE_COUNT,
    NULL,
    NULL,
    count
  );
}

static R_xlen_t capture_design_attributes(SEXP source, SEXP snapshot,
    R_xlen_t column_size) {
  R_xlen_t stored_count = 0;
  if (!bounded_design_attribute_count(source, &stored_count)) {
    Rf_error("Design columns have unsupported structural attributes");
  }
  R_xlen_t present_count = 0;
  for (enum design_attribute attribute = DESIGN_ATTRIBUTE_NAMES;
      attribute < DESIGN_ATTRIBUTE_COUNT;
      attribute = (enum design_attribute) (attribute + 1)) {
    SEXP value = paradox_api_raw_attribute(
      source,
      design_attribute_symbol(attribute)
    );
    if (!ordinary_design_attribute(value) ||
        (attribute == DESIGN_ATTRIBUTE_NAMES &&
          value != R_NilValue && XLENGTH(value) != column_size)) {
      Rf_error("Design columns must have ordinary structural attributes");
    }
    if (value != R_NilValue) ++present_count;
    SET_VECTOR_ELT(snapshot, attribute, value);
  }
  if (stored_count != present_count) {
    Rf_error("Design columns have unsupported structural attributes");
  }
  return stored_count;
}

static int design_attributes_current(SEXP source, SEXP snapshot,
    R_xlen_t column_size, R_xlen_t stored_count) {
  R_xlen_t observed_count = 0;
  if (stored_count > DESIGN_ATTRIBUTE_COUNT ||
      !bounded_design_attribute_count(source, &observed_count) ||
      observed_count != stored_count) {
    return FALSE;
  }
  R_xlen_t present_count = 0;
  for (enum design_attribute attribute = DESIGN_ATTRIBUTE_NAMES;
      attribute < DESIGN_ATTRIBUTE_COUNT;
      attribute = (enum design_attribute) (attribute + 1)) {
    SEXP value = paradox_api_raw_attribute(
      source,
      design_attribute_symbol(attribute)
    );
    if (value != VECTOR_ELT(snapshot, attribute) ||
        !ordinary_design_attribute(value) ||
        (attribute == DESIGN_ATTRIBUTE_NAMES &&
          value != R_NilValue && XLENGTH(value) != column_size)) {
      return FALSE;
    }
    if (value != R_NilValue) ++present_count;
  }
  return present_count == stored_count;
}

static int design_attribute_copies_current(SEXP source, SEXP owned) {
  for (enum design_attribute attribute = DESIGN_ATTRIBUTE_NAMES;
      attribute < DESIGN_ATTRIBUTE_COUNT;
      attribute = (enum design_attribute) (attribute + 1)) {
    SEXP original = VECTOR_ELT(source, attribute);
    SEXP snapshot = VECTOR_ELT(owned, attribute);
    if (original == R_NilValue) {
      if (snapshot != R_NilValue) return FALSE;
    } else if (!ordinary_design_attribute(original) ||
        !ordinary_design_attribute(snapshot) ||
        original == snapshot ||
        !paradox_ordinary_vector_payload_equal(original, snapshot)) {
      return FALSE;
    }
  }
  return TRUE;
}

static void install_design_attributes(SEXP target, SEXP attributes,
    int include_names) {
  for (enum design_attribute attribute = DESIGN_ATTRIBUTE_NAMES;
      attribute < DESIGN_ATTRIBUTE_COUNT;
      attribute = (enum design_attribute) (attribute + 1)) {
    if (!include_names && attribute == DESIGN_ATTRIBUTE_NAMES) continue;
    SEXP value = VECTOR_ELT(attributes, attribute);
    if (value != R_NilValue) {
      Rf_setAttrib(target, design_attribute_symbol(attribute), value);
    }
  }
}

static SEXP snapshot_column(SEXP source,
    R_xlen_t *work_since_interrupt) {
  /*
   * Intern the two nonstandard attribute tags before selecting the source
   * generation. The first symbol installation may allocate; every structural
   * read after the result allocation below is then allocation-free.
   */
  initialize_design_attribute_symbols();
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
  if (Rf_isS4(source) || !supported_column_type(type)) {
    Rf_error("Design contains an unsupported column type");
  }
  const R_xlen_t count = XLENGTH(source);
  SEXP result = PROTECT(Rf_allocVector(type, count));
  /*
   * The result allocation may run a pending finalizer that rewrites the
   * caller-owned column. Recheck the non-attribute shell before observing its
   * payload. Structural attributes are copied only after the element pass and
   * admitted on the owned result below: an ALTREP Elt callback may replace
   * the source attribute pairlist, so validating it here would authenticate a
   * different generation from the one ultimately retained.
   */
  if ((SEXPTYPE) TYPEOF(source) != type || Rf_isS4(source) ||
      XLENGTH(source) != count) {
    UNPROTECT(1);
    Rf_error("Design column changed while being snapshotted");
  }
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
  R_xlen_t observed_attribute_count = 0;
  if (!bounded_design_attribute_count(
      source,
      &observed_attribute_count
    )) {
    UNPROTECT(1);
    Rf_error("Design columns have unsupported structural attributes");
  }
  if (observed_attribute_count == 0) {
    /*
     * This is the canonical hot path. No allocation or callback follows the
     * payload pass, so the already selected ordinary result can return
     * directly without an attribute carrier or terminal replay.
     */
    if ((SEXPTYPE) TYPEOF(source) != type || Rf_isS4(source) ||
        XLENGTH(source) != count) {
      UNPROTECT(1);
      Rf_error("Design column changed while being snapshotted");
    }
    UNPROTECT(1);
    return result;
  }

  /*
   * Design interprets exactly five flat character metadata fields. Allocate
   * both root carriers before selecting their generation, own every present
   * value independently, and install only those known fields. This avoids R's
   * recursive attribute duplicator entirely.
   */
  SEXP selected_attributes = PROTECT(Rf_allocVector(
    VECSXP,
    DESIGN_ATTRIBUTE_COUNT
  ));
  SEXP owned_attributes = PROTECT(Rf_allocVector(
    VECSXP,
    DESIGN_ATTRIBUTE_COUNT
  ));
  const R_xlen_t selected_attribute_count = capture_design_attributes(
    source,
    selected_attributes,
    count
  );
  for (enum design_attribute attribute = DESIGN_ATTRIBUTE_NAMES;
      attribute < DESIGN_ATTRIBUTE_COUNT;
      attribute = (enum design_attribute) (attribute + 1)) {
    SEXP selected = VECTOR_ELT(selected_attributes, attribute);
    if (selected != R_NilValue) {
      SEXP owned = PROTECT(Rf_duplicate(selected));
      SET_VECTOR_ELT(owned_attributes, attribute, owned);
      UNPROTECT(1);
    }
  }
  install_design_attributes(result, owned_attributes, TRUE);

  if ((SEXPTYPE) TYPEOF(source) != type || Rf_isS4(source) ||
      XLENGTH(source) != count ||
      !design_attributes_current(
        source,
        selected_attributes,
        count,
        selected_attribute_count
      ) ||
      !design_attribute_copies_current(
        selected_attributes,
        owned_attributes
      ) ||
      (!ALTREP(source) &&
        !paradox_ordinary_vector_payload_equal(source, result))) {
    UNPROTECT(3);
    Rf_error("Design column changed while being snapshotted");
  }
  if ((SEXPTYPE) TYPEOF(result) != type || Rf_isS4(result) ||
      XLENGTH(result) != count) {
    UNPROTECT(3);
    Rf_error("Design column changed while being snapshotted");
  }
  UNPROTECT(3);
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

static void install_design_scalar_attributes(SEXP target, SEXP column) {
  for (enum design_attribute attribute = DESIGN_ATTRIBUTE_CLASS;
      attribute < DESIGN_ATTRIBUTE_COUNT;
      attribute = (enum design_attribute) (attribute + 1)) {
    SEXP value = paradox_api_raw_attribute(
      column,
      design_attribute_symbol(attribute)
    );
    if (value != R_NilValue) {
      /*
       * The frozen column owns one flat, attribute-free character value.
       * Duplicate that value for every public scalar so rows cannot share a
       * mutable levels/class/tzone/units carrier.
       *
       * This is O(rows x carrier length) per attributed column and dominates
       * transposition of wide-levelled factor or `POSIXct` designs. The cost
       * is accepted: isolation here has to hold against `data.table::setattr()`
       * and every other by-reference attribute write, which bypass R's
       * copy-on-write barrier entirely, so marking one shared carrier
       * immutable would not preserve it. Sharing would make one row's
       * attribute write visible in its siblings.
       */
      SEXP owned = PROTECT(Rf_duplicate(value));
      Rf_setAttrib(target, design_attribute_symbol(attribute), owned);
      UNPROTECT(1);
    }
  }
}

static SEXP atomic_scalar(SEXP column, R_xlen_t row) {
  SEXP result;
  /* Ordinary columns carry no attributes and take the allocation-free path
   * below. An attributed column needs its own one-element cell: the copied
   * set may contain the column's own length (`names`, `dim`, `dimnames`),
   * and Rf_ScalarLogical() hands out R's shared TRUE/FALSE/NA singletons,
   * which must never receive attributes. */
  const int attributed = !paradox_api_has_no_attributes(column);
  switch (TYPEOF(column)) {
  case LGLSXP:
    if (attributed) {
      result = PROTECT(Rf_allocVector(LGLSXP, 1));
      SET_LOGICAL_ELT(result, 0, LOGICAL_ELT(column, row));
    } else {
      result = PROTECT(Rf_ScalarLogical(LOGICAL_ELT(column, row)));
    }
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
  if (attributed) {
    install_design_scalar_attributes(result, column);
  }
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
  if (TYPEOF(data) != VECSXP || ALTREP(data) || Rf_isS4(data)) {
    UNPROTECT(2);
    Rf_error("Design$data must be a list-like data frame");
  }

  R_xlen_t work_since_interrupt = 0;
  const R_xlen_t column_count = XLENGTH(data);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, column_count));
  SEXP columns = PROTECT(Rf_allocVector(VECSXP, column_count));
  if (!ordinary_design_shell(data)) {
    UNPROTECT(4);
    Rf_error("Design$data must be a list-like data frame");
  }
  if (!paradox_capture_list_identities(data, names, columns)) {
    UNPROTECT(4);
    Rf_error("Design$data must have unique column names");
  }

  /*
   * Capture the dimension carrier in the same allocation-free observation
   * window as names and columns.  Rf_any_duplicated() below may allocate and
   * run a pending finalizer; delaying this read could otherwise pair the
   * already captured columns with a later row.names generation.
   */
  const int table_input = Rf_isObject(data) != FALSE;
  R_xlen_t table_rows = 0;
  if (table_input &&
      !paradox_public_table_row_count(data, &table_rows)) {
    UNPROTECT(4);
    Rf_error("Design$data has invalid data.frame row names");
  }

  validate_snapshot_names(
    names,
    "Design column names",
    &work_since_interrupt
  );
  if (Rf_any_duplicated(names, FALSE) != 0) {
    UNPROTECT(4);
    Rf_error("Design$data must have unique column names");
  }

  R_xlen_t row_count = table_input ? table_rows : 0;
  for (R_xlen_t column = 0; column < column_count; ++column) {
    paradox_account_work(&work_since_interrupt);
    SEXP source = PROTECT(VECTOR_ELT(columns, column));
    SEXP frozen = PROTECT(snapshot_column(source, &work_since_interrupt));
    const R_xlen_t rows = XLENGTH(frozen);
    if (rows != row_count && (table_input || column != 0)) {
      UNPROTECT(6);
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

  UNPROTECT(5);
  return result;
}
