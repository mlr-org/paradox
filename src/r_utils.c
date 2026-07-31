#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Utils.h>

#include "r_api_compat.h"
#include "r_utils.h"

SEXP paradox_stored_attribute(SEXP object, SEXP symbol) {
  return paradox_api_raw_attribute(object, symbol);
}

static int strict_id_tail_byte(unsigned char byte) {
  return (byte >= 'a' && byte <= 'z') ||
    (byte >= 'A' && byte <= 'Z') ||
    (byte >= '0' && byte <= '9') || byte == '.' || byte == '_';
}

int paradox_string_is_strict_id(SEXP value) {
  if (!paradox_charsxp_is_ordinary(value) || LENGTH(value) == 0) {
    return FALSE;
  }

  const unsigned char *bytes = (const unsigned char *) CHAR(value);
  const int size = LENGTH(value);
  int index = 0;
  while (index < size && bytes[index] == '.') {
    ++index;
  }
  if (index == size ||
      !((bytes[index] >= 'a' && bytes[index] <= 'z') ||
        (bytes[index] >= 'A' && bytes[index] <= 'Z'))) {
    return FALSE;
  }

  for (++index; index < size; ++index) {
    if (!strict_id_tail_byte(bytes[index])) {
      return FALSE;
    }
  }
  return TRUE;
}

int paradox_string_is_strict_id_tail(SEXP value) {
  if (!paradox_charsxp_is_ordinary(value) || LENGTH(value) == 0) {
    return FALSE;
  }
  const unsigned char *bytes = (const unsigned char *) CHAR(value);
  const int size = LENGTH(value);
  for (int index = 0; index < size; ++index) {
    if (!strict_id_tail_byte(bytes[index])) {
      return FALSE;
    }
  }
  return TRUE;
}

static int public_table_class_name(SEXP value, const char *expected) {
  return value != NA_STRING && Rf_getCharCE(value) != CE_BYTES &&
    strcmp(CHAR(value), expected) == 0;
}

static paradox_public_table_kind_t public_table_class_kind(SEXP classes) {
  if (TYPEOF(classes) != STRSXP || ALTREP(classes) || Rf_isS4(classes) ||
      Rf_isObject(classes) || !paradox_api_has_no_attributes(classes)) {
    return PARADOX_PUBLIC_TABLE_NONE;
  }

  const R_xlen_t count = XLENGTH(classes);
  if (count == 0) return PARADOX_PUBLIC_TABLE_NONE;

  /* Keep the two overwhelmingly common exact spellings on a fixed, minimal
   * path. Prefix validation below is paid only by additive subclasses. */
  if (count == 1 && public_table_class_name(
      STRING_ELT(classes, 0), "data.frame")) {
    return PARADOX_PUBLIC_DATA_FRAME;
  }
  if (count == 2 && public_table_class_name(
      STRING_ELT(classes, 0), "data.table") &&
      public_table_class_name(STRING_ELT(classes, 1), "data.frame")) {
    return PARADOX_PUBLIC_DATA_TABLE;
  }

  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP label = STRING_ELT(classes, index);
    if (label == NA_STRING || Rf_getCharCE(label) == CE_BYTES ||
        CHAR(label)[0] == '\0') {
      return PARADOX_PUBLIC_TABLE_NONE;
    }
  }
  const int data_frame = public_table_class_name(
    STRING_ELT(classes, count - 1),
    "data.frame"
  );
  if (!data_frame) return PARADOX_PUBLIC_TABLE_NONE;

  const int data_table = count >= 2 && public_table_class_name(
    STRING_ELT(classes, count - 2),
    "data.table"
  );
  const R_xlen_t prefix_count = count - (data_table ? 2 : 1);
  for (R_xlen_t index = 0; index < prefix_count; ++index) {
    SEXP label = STRING_ELT(classes, index);
    if (public_table_class_name(label, "data.frame") ||
        public_table_class_name(label, "data.table")) {
      return PARADOX_PUBLIC_TABLE_NONE;
    }
    /* Leading labels are normally one element long (for example
     * bmr_aggregate). Keep their uniqueness check allocation-free rather than
     * paying Rf_any_duplicated()'s hash table on every public-table ingress. */
    for (R_xlen_t previous = 0; previous < index; ++previous) {
      if (strcmp(CHAR(label), CHAR(STRING_ELT(classes, previous))) == 0) {
        return PARADOX_PUBLIC_TABLE_NONE;
      }
    }
  }
  return data_table
    ? PARADOX_PUBLIC_DATA_TABLE
    : PARADOX_PUBLIC_DATA_FRAME;
}

static SEXP canonical_public_table_class(paradox_public_table_kind_t kind) {
  const R_xlen_t count = kind == PARADOX_PUBLIC_DATA_TABLE ? 2 : 1;
  SEXP classes = PROTECT(Rf_allocVector(STRSXP, count));
  if (kind == PARADOX_PUBLIC_DATA_TABLE) {
    SET_STRING_ELT(classes, 0, Rf_mkChar("data.table"));
  }
  SET_STRING_ELT(classes, count - 1, Rf_mkChar("data.frame"));
  UNPROTECT(1);
  return classes;
}

static int ordinary_ignored_data_table_metadata(
    SEXP table, SEXP self_reference_symbol, SEXP sorted_symbol,
    SEXP index_symbol) {
  SEXP self_reference = PROTECT(paradox_api_raw_attribute(
    table,
    self_reference_symbol
  ));
  SEXP sorted = PROTECT(paradox_api_raw_attribute(
    table,
    sorted_symbol
  ));
  SEXP index = PROTECT(paradox_api_raw_attribute(
    table,
    index_symbol
  ));
  const int valid_self_reference = self_reference == R_NilValue ||
    (TYPEOF(self_reference) == EXTPTRSXP && !Rf_isS4(self_reference) &&
      !Rf_isObject(self_reference) &&
      paradox_api_has_no_attributes(self_reference));
  const int valid_sorted = sorted == R_NilValue ||
    (TYPEOF(sorted) == STRSXP && !ALTREP(sorted) && !Rf_isS4(sorted) &&
      !Rf_isObject(sorted) && paradox_api_has_no_attributes(sorted));
  /* data.table stores secondary-index payloads below attributes of one
   * ordinary integer(0) carrier. Paradox never consumes those caches: only
   * the carrier's representation is structural, and it is dropped from the
   * owned public-input snapshot. */
  const int valid_index = index == R_NilValue ||
    (TYPEOF(index) == INTSXP && !ALTREP(index) && !Rf_isS4(index) &&
      !Rf_isObject(index) && XLENGTH(index) == 0);
  UNPROTECT(3);
  return valid_self_reference && valid_sorted && valid_index;
}

paradox_public_table_kind_t paradox_public_table_kind(SEXP table) {
  if (TYPEOF(table) != VECSXP || Rf_isS4(table)) {
    return PARADOX_PUBLIC_TABLE_NONE;
  }
  if (paradox_api_has_no_attributes(table)) {
    return PARADOX_PUBLIC_TABLE_NONE;
  }
  static const char *const frame_attributes[] = {
    "names", "row.names", "class"
  };
  static const char *const table_attributes[] = {
    "names", "row.names", "class", ".internal.selfref", "sorted", "index"
  };
  /*
   * Intern every non-global tag before selecting the caller-owned generation.
   * One exact data.table-superset allow-list scan then proves the complete
   * spine finite and validates every tag before the raw selectors below.
   * Nothing between that proof and the final classifier allocates.
   */
  SEXP self_reference_symbol = Rf_install(".internal.selfref");
  SEXP sorted_symbol = Rf_install("sorted");
  SEXP index_symbol = Rf_install("index");
  if (!paradox_api_has_only_attributes(
      table,
      table_attributes,
      6
    )) {
    return PARADOX_PUBLIC_TABLE_NONE;
  }
  SEXP classes = PROTECT(paradox_api_raw_attribute(
    table,
    R_ClassSymbol
  ));
  SEXP names = PROTECT(paradox_api_raw_attribute(
    table,
    R_NamesSymbol
  ));
  /* Base structure(list(), class = "data.frame", row.names = ...) is a
   * long-standing valid zero-column spelling with no names attribute. The
   * operation-level length check admits that case only when the shell is
   * actually empty. */
  const int ordinary_names = names == R_NilValue ||
    (TYPEOF(names) == STRSXP && !ALTREP(names) && !Rf_isS4(names) &&
      !Rf_isObject(names) && paradox_api_has_no_attributes(names));
  const paradox_public_table_kind_t kind = public_table_class_kind(classes);
  const int recognized_frame = ordinary_names &&
    kind == PARADOX_PUBLIC_DATA_FRAME &&
    paradox_api_has_only_attributes(
      table,
      frame_attributes,
      3
    );
  const int recognized_table = ordinary_names &&
    kind == PARADOX_PUBLIC_DATA_TABLE &&
    ordinary_ignored_data_table_metadata(
      table,
      self_reference_symbol,
      sorted_symbol,
      index_symbol
    );
  UNPROTECT(2);
  return recognized_table
    ? PARADOX_PUBLIC_DATA_TABLE
    : recognized_frame
      ? PARADOX_PUBLIC_DATA_FRAME
      : PARADOX_PUBLIC_TABLE_NONE;
}

int paradox_public_row_names_count(SEXP row_names, R_xlen_t *row_count) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(row_names);
  if ((type != INTSXP && type != STRSXP) || Rf_isS4(row_names) ||
      Rf_isObject(row_names) || !paradox_api_has_no_attributes(row_names)) {
    return FALSE;
  }

  R_xlen_t rows;
  if (ALTREP(row_names)) {
    /* Modern base R legitimately retains compact integer sequences and
     * deferred strings as explicit row names. Their labels are irrelevant to
     * every admitted operation, so select one stable Length and no elements. */
    rows = XLENGTH(row_names);
  } else if (type == INTSXP && XLENGTH(row_names) == 2 &&
      INTEGER_ELT(row_names, 0) == NA_INTEGER) {
    const int encoded = INTEGER_ELT(row_names, 1);
    if (encoded == NA_INTEGER) return FALSE;
    rows = encoded < 0 ? (R_xlen_t) -encoded : (R_xlen_t) encoded;
  } else {
    rows = XLENGTH(row_names);
  }
  *row_count = rows;
  return TRUE;
}

int paradox_public_table_row_count(SEXP table, R_xlen_t *row_count) {
  if (row_count == NULL) {
    Rf_error("Internal error: missing public table row-count destination");
  }
  int has_row_names = FALSE;
  if (!paradox_bounded_metadata_has_tag(
      table,
      R_RowNamesSymbol,
      &has_row_names
    )) {
    return FALSE;
  }
  /* The bounded scan immediately precedes this allocation-free raw lookup. */
  SEXP row_names = PROTECT(has_row_names
    ? paradox_api_raw_attribute(table, R_RowNamesSymbol)
    : R_NilValue);
  const int valid = paradox_public_row_names_count(row_names, row_count);
  UNPROTECT(1);
  return valid;
}

int paradox_capture_list_identities(SEXP source, SEXP stable_names,
    SEXP stable_values) {
  if (TYPEOF(source) != VECSXP || ALTREP(source) || Rf_isS4(source) ||
      TYPEOF(stable_values) != VECSXP || ALTREP(stable_values) ||
      Rf_isS4(stable_values) || Rf_isObject(stable_values) ||
      !paradox_api_has_no_attributes(stable_values)) {
    return FALSE;
  }
  const R_xlen_t count = XLENGTH(source);
  if (XLENGTH(stable_values) != count) return FALSE;

  int has_source_names = FALSE;
  if (!paradox_bounded_metadata_has_tag(
      source,
      R_NamesSymbol,
      &has_source_names
    )) {
    return FALSE;
  }
  /* The complete bounded scan and this raw lookup are both allocation-free. */
  SEXP source_names = has_source_names
    ? paradox_api_raw_attribute(source, R_NamesSymbol)
    : R_NilValue;
  if (stable_names == R_NilValue) {
    if (source_names != R_NilValue) return FALSE;
  } else {
    if (TYPEOF(stable_names) != STRSXP || ALTREP(stable_names) ||
        Rf_isS4(stable_names) || Rf_isObject(stable_names) ||
        !paradox_api_has_no_attributes(stable_names) ||
        XLENGTH(stable_names) != count) {
      return FALSE;
    }
    if (source_names == R_NilValue) {
      if (count != 0) return FALSE;
    } else if (TYPEOF(source_names) != STRSXP || ALTREP(source_names) ||
        Rf_isS4(source_names) || Rf_isObject(source_names) ||
        !paradox_api_has_no_attributes(source_names) ||
        XLENGTH(source_names) != count) {
      return FALSE;
    }
  }

  /* No allocation, evaluator, ALTREP observation, or interrupt check may be
   * introduced into this loop.  Its purpose is one indivisible observation
   * of the ordinary source shell. */
  for (R_xlen_t index = 0; index < count; ++index) {
    if (stable_names != R_NilValue) {
      SET_STRING_ELT(stable_names, index, STRING_ELT(source_names, index));
    }
    SET_VECTOR_ELT(stable_values, index, VECTOR_ELT(source, index));
  }
  return TRUE;
}

SEXP paradox_test_public_row_names_count(SEXP row_names) {
  PROTECT(row_names);
  R_xlen_t rows = 0;
  if (!paradox_public_row_names_count(row_names, &rows)) {
    UNPROTECT(1);
    Rf_error("Test row-name metadata is invalid");
  }
  SEXP result = PROTECT(Rf_ScalarReal((double) rows));
  UNPROTECT(2);
  return result;
}

SEXP paradox_materialize_public_table_shell(SEXP table) {
  if (TYPEOF(table) != VECSXP || !ALTREP(table) || Rf_isS4(table)) {
    return table;
  }
  const paradox_public_table_kind_t entry_kind =
    paradox_public_table_kind(table);
  if (entry_kind == PARADOX_PUBLIC_TABLE_NONE) return table;

  /*
   * Allocate the two spine carriers before selecting interpreted metadata.
   * A pending finalizer may run during either allocation; afterwards the
   * source names, row-name carrier, and exact columns are selected without
   * another allocation in between.  Row-name Length is deliberately observed
   * only after the columns: it may be ALTREP and re-enter R, but by then the
   * matching name/column generation is already independently rooted.
   *
   * A top-level ALTREP necessarily supplies one Length and one Elt per
   * column. Stable shells (including base's lazy attribute-only duplicate)
   * therefore materialize once. A hostile Elt method can still mutate its own
   * not-yet-observed backing elements; that behavior is outside the stable
   * ALTREP contract, but it cannot leave an unrooted pointer here.
   */
  PROTECT(table);
  const R_xlen_t count = XLENGTH(table);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, count));
  SEXP stable_names = PROTECT(Rf_allocVector(STRSXP, count));

  const paradox_public_table_kind_t kind =
    paradox_public_table_kind(table);
  if (kind == PARADOX_PUBLIC_TABLE_NONE || kind != entry_kind) {
    UNPROTECT(3);
    return table;
  }
  SEXP source_names = PROTECT(paradox_api_raw_attribute(
    table,
    R_NamesSymbol
  ));
  SEXP source_row_names = PROTECT(paradox_api_raw_attribute(
    table,
    R_RowNamesSymbol
  ));
  const int names_absent = source_names == R_NilValue;
  if (!names_absent && (TYPEOF(source_names) != STRSXP ||
      ALTREP(source_names) || Rf_isS4(source_names) ||
      Rf_isObject(source_names) ||
      !paradox_api_has_no_attributes(source_names))) {
    UNPROTECT(5);
    return table;
  }
  const R_xlen_t name_count = names_absent ? 0 : XLENGTH(source_names);
  if (name_count != count || (names_absent && count != 0)) {
    UNPROTECT(5);
    return table;
  }
  for (R_xlen_t column = 0; column < count; ++column) {
    SET_STRING_ELT(stable_names, column, STRING_ELT(source_names, column));
  }
  for (R_xlen_t column = 0; column < count; ++column) {
    if (column != 0 &&
        column % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP value = PROTECT(VECTOR_ELT(table, column));
    SET_VECTOR_ELT(result, column, value);
    UNPROTECT(1);
  }

  R_xlen_t rows = 0;
  if (!paradox_public_row_names_count(source_row_names, &rows) ||
      rows > INT_MAX) {
    UNPROTECT(5);
    return table;
  }
  SEXP stable_classes = PROTECT(canonical_public_table_class(kind));
  SEXP stable_row_names = PROTECT(Rf_allocVector(
    INTSXP,
    rows == 0 ? 0 : 2
  ));
  if (rows != 0) {
    SET_INTEGER_ELT(stable_row_names, 0, NA_INTEGER);
    SET_INTEGER_ELT(stable_row_names, 1, -(int) rows);
  }
  Rf_setAttrib(result, R_NamesSymbol, stable_names);
  Rf_setAttrib(result, R_RowNamesSymbol, stable_row_names);
  Rf_setAttrib(result, R_ClassSymbol, stable_classes);
  UNPROTECT(7);
  return result;
}

static SEXP argument_class(SEXP value) {
  if (!Rf_isObject(value)) {
    return R_NilValue;
  }

  /*
   * This cold diagnostic accepts a caller-owned object.  Do not let its class
   * lookup hand an unbounded or cyclic attribute spine to R's compatibility
   * accessor on an old runtime; the shared snapshot validates the complete
   * spine and returns the exact ordinary class carrier without allocation.
   */
  SEXP classes = R_NilValue;
  if (!paradox_api_ordinary_class_snapshot(value, &classes)) {
    return R_NilValue;
  }
  PROTECT(classes);
  SEXP result = R_NilValue;
  if (TYPEOF(classes) == STRSXP && !ALTREP(classes) &&
      paradox_api_has_no_attributes(classes) && XLENGTH(classes) > 0) {
    SEXP first_class = STRING_ELT(classes, 0);
    if (first_class != NA_STRING && Rf_getCharCE(first_class) != CE_BYTES) {
      result = first_class;
    }
  }
  UNPROTECT(1);
  return result;
}

SEXP paradox_get_named_column_checked(SEXP table, const char *corrupt_context,
    const char *storage_name, const char *column_name) {
  if (TYPEOF(table) != VECSXP || Rf_isS4(table)) {
    Rf_error("Corrupt %s: `%s` must be a list", corrupt_context, storage_name);
  }
  if (ALTREP(table)) {
    Rf_error(
      "Corrupt %s: `%s` must use an ordinary list representation",
      corrupt_context,
      storage_name
    );
  }

  int has_names = FALSE;
  if (!paradox_bounded_metadata_has_tag(
      table,
      R_NamesSymbol,
      &has_names
    ) || !has_names) {
    Rf_error(
      "Corrupt %s: `%s` must be a named list",
      corrupt_context,
      storage_name
    );
  }
  /* The complete bounded scan immediately precedes this allocation-free raw
   * lookup.  A malformed old-R attribute pairlist therefore cannot enter an
   * unbounded compatibility traversal. */
  SEXP names = PROTECT(paradox_api_raw_attribute(
    table,
    R_NamesSymbol
  ));
  const R_xlen_t n_columns = XLENGTH(table);
  if (TYPEOF(names) != STRSXP || Rf_isS4(names) ||
      Rf_isObject(names) || !paradox_api_has_no_attributes(names)) {
    Rf_error(
      "Corrupt %s: `%s` must be a named list",
      corrupt_context,
      storage_name
    );
  }
  if (ALTREP(names)) {
    Rf_error(
      "Corrupt %s: `%s` names must use an ordinary character representation",
      corrupt_context,
      storage_name
    );
  }
  if (XLENGTH(names) != n_columns) {
    Rf_error(
      "Corrupt %s: `%s` must be a named list",
      corrupt_context,
      storage_name
    );
  }

  SEXP result = R_NilValue;
  int matches = 0;
  for (R_xlen_t column = 0; column < n_columns; ++column) {
    SEXP name = STRING_ELT(names, column);
    if (name != NA_STRING && strcmp(CHAR(name), column_name) == 0) {
      result = VECTOR_ELT(table, column);
      ++matches;
    }
  }

  if (matches == 1) {
    UNPROTECT(1);
    return result;
  }
  if (matches > 1) {
    Rf_error(
      "Corrupt %s: `%s` has more than one `%s` column",
      corrupt_context,
      storage_name,
      column_name
    );
  }

  Rf_error(
    "Corrupt %s: `%s` has no `%s` column",
    corrupt_context,
    storage_name,
    column_name
  );
  return R_NilValue;
}

SEXP paradox_get_named_column(SEXP table, const char *storage_name,
    const char *column_name) {
  return paradox_get_named_column_checked(
    table,
    "ParamSet storage",
    storage_name,
    column_name
  );
}

void paradox_require_column_checked(SEXP column, SEXPTYPE type,
    R_xlen_t size, const char *corrupt_context, const char *column_name) {
  if ((SEXPTYPE) TYPEOF(column) != type) {
    Rf_error(
      "Corrupt %s: `%s` must have type `%s` and length %.0f",
      corrupt_context,
      column_name,
      Rf_type2char(type),
      (double) size
    );
  }
  /* Canonical table columns are ordinary, unclassed, attribute-free vectors.
   * Reject their complete structural shell before asking Length anything: a
   * hostile or merely stateful ALTREP implementation may call back into R or
   * report a different size on the second observation. */
  if (ALTREP(column) || Rf_isS4(column) || Rf_isObject(column) ||
      !paradox_api_has_no_attributes(column)) {
    Rf_error(
      "Corrupt %s: `%s` must use an ordinary %s representation",
      corrupt_context,
      column_name,
      Rf_type2char(type)
    );
  }
  if (XLENGTH(column) != size) {
    Rf_error(
      "Corrupt %s: `%s` must have type `%s` and length %.0f",
      corrupt_context,
      column_name,
      Rf_type2char(type),
      (double) size
    );
  }
}

void paradox_require_column(SEXP column, SEXPTYPE type, R_xlen_t size,
    const char *column_name) {
  paradox_require_column_checked(
    column,
    type,
    size,
    "ParamSet storage",
    column_name
  );
}

void paradox_require_numeric_column(SEXP column,
    R_xlen_t size, const char *corrupt_context, const char *column_name) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(column);
  if (type != REALSXP && type != INTSXP) {
    Rf_error(
      "Corrupt %s: `%s` must be numeric and have length %.0f",
      corrupt_context,
      column_name,
      (double) size
    );
  }
  if (ALTREP(column) || Rf_isS4(column) || Rf_isObject(column) ||
      !paradox_api_has_no_attributes(column)) {
    Rf_error(
      "Corrupt %s: `%s` must use an ordinary numeric representation",
      corrupt_context,
      column_name
    );
  }
  if (XLENGTH(column) != size) {
    Rf_error(
      "Corrupt %s: `%s` must be numeric and have length %.0f",
      corrupt_context,
      column_name,
      (double) size
    );
  }
}

double paradox_accepted_lower(double bound, double tolerance) {
  if (tolerance == 0.0 || !R_FINITE(bound)) {
    return bound;
  }
  return bound - tolerance * fmax(1.0, fabs(bound));
}

double paradox_accepted_upper(double bound, double tolerance) {
  if (tolerance == 0.0 || !R_FINITE(bound)) {
    return bound;
  }
  return bound + tolerance * fmax(1.0, fabs(bound));
}

int paradox_within_integer_tolerance(double value, double rounded,
    double tolerance) {
  const double subtraction_allowance =
    fabs(value) * (2.0 * DBL_EPSILON);
  return fabs(value - rounded) <= tolerance + subtraction_allowance;
}

void paradox_require_character_argument_type(SEXP value,
    const char *argument_name) {
  if (value == R_NilValue) {
    return;
  }
  if (TYPEOF(value) != STRSXP) {
    SEXP class_name = PROTECT(argument_class(value));
    paradox_utf8_piece_t pieces[] = {
      paradox_utf8_ascii_piece("Assertion on '"),
      paradox_utf8_ascii_piece(argument_name),
      paradox_utf8_ascii_piece(
        "' failed: Must be of type 'character' (or 'NULL'), not '"
      ),
      class_name == R_NilValue
        ? paradox_utf8_ascii_piece(Rf_type2char((SEXPTYPE) TYPEOF(value)))
        : paradox_utf8_charsxp_piece(class_name),
      paradox_utf8_ascii_piece("'.")
    };
    SEXP message = PROTECT(paradox_utf8_message(pieces, 5));
    paradox_error_from_scalar_string(message);
  }
}

void paradox_require_character_argument(SEXP value, const char *argument_name) {
  paradox_require_character_argument_type(value, argument_name);
  if (value == R_NilValue) {
    return;
  }

  const R_xlen_t size = XLENGTH(value);
  for (R_xlen_t index = 0; index < size; ++index) {
    if (index != 0 && index % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    if (STRING_ELT(value, index) == NA_STRING) {
      Rf_error(
        "Assertion on '%s' failed: Contains missing values (element %.0f).",
        argument_name,
        (double) (index + 1)
      );
    }
  }
}

void *paradox_temporary_alloc(R_xlen_t count, size_t element_size) {
  if (count < 0 || element_size == 0 || element_size > (size_t) INT_MAX ||
      (uintmax_t) count > (uintmax_t) (SIZE_MAX / element_size)) {
    Rf_error("Unable to allocate temporary native workspace");
  }

  return (void *) R_alloc((size_t) count, (int) element_size);
}

char *paradox_temporary_utf8_copy(SEXP string, size_t *size) {
  if (TYPEOF(string) != CHARSXP || string == NA_STRING) {
    Rf_error("Internal error: cannot copy invalid UTF-8 text");
  }

  PROTECT(string);
  const void *translation_watermark = vmaxget();
  const size_t measured = strlen(Rf_translateCharUTF8(string));
  vmaxset(translation_watermark);
  if (measured >= (size_t) R_XLEN_T_MAX) {
    UNPROTECT(1);
    Rf_error("Internal error: UTF-8 text exceeds native string limits");
  }

  char *copy = paradox_temporary_alloc(
    (R_xlen_t) measured + 1,
    sizeof(*copy)
  );
  /*
   * Rf_translateCharUTF8() returns transient vmax storage, and R_alloc() above
   * may run a finalizer which both invalidates that storage and changes the
   * native locale used for a CE_NATIVE string.  Reacquire after allocation
   * and reject a changed byte count before copying into the measured buffer.
   */
  translation_watermark = vmaxget();
  const char *translated = Rf_translateCharUTF8(string);
  const size_t current = strlen(translated);
  if (current != measured) {
    vmaxset(translation_watermark);
    UNPROTECT(1);
    Rf_error("UTF-8 text changed while being copied");
  }
  memcpy(copy, translated, measured + 1U);
  vmaxset(translation_watermark);
  UNPROTECT(1);
  if (size != NULL) {
    *size = measured;
  }
  return copy;
}

paradox_utf8_piece_t paradox_utf8_ascii_piece(const char *ascii) {
  const paradox_utf8_piece_t result = {
    .kind = PARADOX_UTF8_PIECE_ASCII,
    .ascii = ascii,
    .string = R_NilValue
  };
  return result;
}

paradox_utf8_piece_t paradox_utf8_charsxp_piece(SEXP string) {
  const paradox_utf8_piece_t result = {
    .kind = PARADOX_UTF8_PIECE_CHARSXP,
    .ascii = NULL,
    .string = string
  };
  return result;
}

static void utf8_piece_interrupt(R_xlen_t index) {
  if (index != 0 && index % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
    R_CheckUserInterrupt();
  }
}

static size_t ascii_piece_size(const char *text) {
  if (text != NULL) {
    const size_t size = strlen(text);
    for (size_t index = 0; index < size; ++index) {
      if ((unsigned char) text[index] >= 0x80U) {
        Rf_error("Internal error: a UTF-8 message literal is not ASCII");
      }
    }
    return size;
  }
  Rf_error("Internal error: a UTF-8 message has a NULL ASCII fragment");
}

static int utf8_continuation(unsigned char byte) {
  return byte >= 0x80U && byte <= 0xbfU;
}

static int valid_utf8_fragment(const char *text, size_t size) {
  const unsigned char *bytes = (const unsigned char *) text;
  size_t index = 0;
  while (index < size) {
    const unsigned char first = bytes[index];
    if (first <= 0x7fU) {
      ++index;
      continue;
    }
    if (first >= 0xc2U && first <= 0xdfU) {
      if (size - index < 2U || !utf8_continuation(bytes[index + 1U])) {
        return FALSE;
      }
      index += 2U;
      continue;
    }
    if (first >= 0xe0U && first <= 0xefU) {
      if (size - index < 3U ||
          !utf8_continuation(bytes[index + 2U])) {
        return FALSE;
      }
      const unsigned char second = bytes[index + 1U];
      if ((first == 0xe0U && (second < 0xa0U || second > 0xbfU)) ||
          (first == 0xedU && (second < 0x80U || second > 0x9fU)) ||
          (first != 0xe0U && first != 0xedU &&
           !utf8_continuation(second))) {
        return FALSE;
      }
      index += 3U;
      continue;
    }
    if (first >= 0xf0U && first <= 0xf4U) {
      if (size - index < 4U ||
          !utf8_continuation(bytes[index + 2U]) ||
          !utf8_continuation(bytes[index + 3U])) {
        return FALSE;
      }
      const unsigned char second = bytes[index + 1U];
      if ((first == 0xf0U && (second < 0x90U || second > 0xbfU)) ||
          (first == 0xf4U && (second < 0x80U || second > 0x8fU)) ||
          (first != 0xf0U && first != 0xf4U &&
           !utf8_continuation(second))) {
        return FALSE;
      }
      index += 4U;
      continue;
    }
    return FALSE;
  }
  return TRUE;
}

static size_t translated_piece_size(SEXP string) {
  const char *text = Rf_translateCharUTF8(string);
  const size_t size = strlen(text);
  if (!valid_utf8_fragment(text, size)) {
    Rf_error("Internal error: a message fragment is not valid UTF-8");
  }
  return size;
}

SEXP paradox_utf8_message(const paradox_utf8_piece_t *pieces,
    R_xlen_t piece_count) {
  if (piece_count < 0 ||
      (piece_count != 0 && pieces == NULL) ||
      (uintmax_t) piece_count >
        (uintmax_t) PTRDIFF_MAX / sizeof(*pieces)) {
    Rf_error("Internal error: invalid UTF-8 message piece array");
  }

  R_xlen_t string_count = 0;
  /* Callers retain each CHARSXP through this non-allocating admission pass.
   * One STRSXP then becomes the sole root for every allocating phase below. */
  for (R_xlen_t index = 0; index < piece_count; ++index) {
    utf8_piece_interrupt(index);
    const paradox_utf8_piece_t *piece = &pieces[index];
    if (piece->kind == PARADOX_UTF8_PIECE_ASCII) {
      (void) ascii_piece_size(piece->ascii);
      continue;
    }
    if (piece->kind != PARADOX_UTF8_PIECE_CHARSXP ||
        TYPEOF(piece->string) != CHARSXP || piece->string == NA_STRING ||
        Rf_getCharCE(piece->string) == CE_BYTES) {
      Rf_error("Internal error: invalid UTF-8 message string fragment");
    }
    ++string_count;
  }

  SEXP roots = PROTECT(Rf_allocVector(STRSXP, string_count));
  R_xlen_t string_index = 0;
  for (R_xlen_t index = 0; index < piece_count; ++index) {
    utf8_piece_interrupt(index);
    if (pieces[index].kind == PARADOX_UTF8_PIECE_CHARSXP) {
      SET_STRING_ELT(roots, string_index, pieces[index].string);
      ++string_index;
    }
  }
  if (string_index != string_count) {
    UNPROTECT(1);
    Rf_error("Internal error: incomplete UTF-8 message root vector");
  }

  size_t output_size = 0;
  string_index = 0;
  /* Translation workspaces are observed only long enough to validate and
   * measure them.  No translated address survives into output allocation. */
  for (R_xlen_t index = 0; index < piece_count; ++index) {
    utf8_piece_interrupt(index);
    const size_t piece_size = pieces[index].kind == PARADOX_UTF8_PIECE_ASCII
      ? ascii_piece_size(pieces[index].ascii)
      : translated_piece_size(STRING_ELT(roots, string_index++));
    if (piece_size > (size_t) INT_MAX - output_size) {
      UNPROTECT(1);
      Rf_error("Internal error: UTF-8 message exceeds R's string limit");
    }
    output_size += piece_size;
  }

  char *output = paradox_temporary_alloc(
    (R_xlen_t) output_size + 1,
    sizeof(*output)
  );
  size_t offset = 0;
  string_index = 0;
  for (R_xlen_t index = 0; index < piece_count; ++index) {
    utf8_piece_interrupt(index);
    const char *text;
    size_t piece_size;
    if (pieces[index].kind == PARADOX_UTF8_PIECE_ASCII) {
      text = pieces[index].ascii;
      piece_size = ascii_piece_size(text);
    } else {
      SEXP string = STRING_ELT(roots, string_index++);
      /* Reacquire after the sole output allocation and consume immediately,
       * before the next interrupt check or translating call. */
      text = Rf_translateCharUTF8(string);
      piece_size = strlen(text);
      if (!valid_utf8_fragment(text, piece_size)) {
        UNPROTECT(1);
        Rf_error("Internal error: a message fragment is not valid UTF-8");
      }
    }
    if (piece_size > output_size - offset) {
      UNPROTECT(1);
      Rf_error("Internal error: UTF-8 message fragment changed while copying");
    }
    memcpy(output + offset, text, piece_size);
    offset += piece_size;
  }
  if (offset != output_size || string_index != string_count) {
    UNPROTECT(1);
    Rf_error("Internal error: incomplete UTF-8 message construction");
  }
  output[output_size] = '\0';

  SEXP string = PROTECT(Rf_mkCharLenCE(
    output,
    (int) output_size,
    CE_UTF8
  ));
  SEXP result = PROTECT(Rf_ScalarString(string));
  UNPROTECT(3);
  return result;
}

SEXP paradox_diagnostic_charsxp(SEXP string) {
  if (TYPEOF(string) != CHARSXP || string == NA_STRING) {
    Rf_error("Internal error: invalid diagnostic string fragment");
  }
  PROTECT(string);
  const int bytes = Rf_getCharCE(string) == CE_BYTES;
  size_t source_size;
  if (bytes) {
    source_size = strlen(CHAR(string));
  } else {
    /* A string whose UTF-8 translation is not valid UTF-8 -- a native or
     * mismarked identifier carrying foreign bytes -- cannot enter the UTF-8
     * message builder. Escape it exactly like bytes rather than turning an
     * ordinary "not available" diagnostic into an internal error. */
    const void *vmax = vmaxget();
    const char *translated = Rf_translateCharUTF8(string);
    const size_t translated_size = strlen(translated);
    if (valid_utf8_fragment(translated, translated_size)) {
      vmaxset(vmax);
      UNPROTECT(1);
      return string;
    }
    source_size = translated_size;
    vmaxset(vmax);
  }

  static const char hex[] = "0123456789abcdef";
  if (source_size > (size_t) INT_MAX / 4U) {
    Rf_error("Diagnostic string fragment exceeds R's string limit");
  }
  char *output = paradox_temporary_alloc(
    (R_xlen_t) (source_size * 4U) + 1,
    sizeof(*output)
  );
  /* Translation buffers live in R's transient vmax arena, while R_alloc()
   * above may reuse that arena. Reacquire only after the output allocation
   * and verify that the immutable CHARSXP still translates to the measured
   * size before consuming the bytes. */
  const unsigned char *source;
  if (bytes) {
    source = (const unsigned char *) CHAR(string);
  } else {
    const char *translated = Rf_translateCharUTF8(string);
    if (strlen(translated) != source_size) {
      UNPROTECT(1);
      Rf_error("Diagnostic string translation changed while being escaped");
    }
    source = (const unsigned char *) translated;
  }
  size_t output_size = 0;
  for (size_t index = 0; index < source_size; ++index) {
    const unsigned char byte = source[index];
    if (byte >= 0x20U && byte <= 0x7eU && byte != (unsigned char) '\\') {
      output[output_size++] = (char) byte;
    } else {
      output[output_size++] = '\\';
      output[output_size++] = 'x';
      output[output_size++] = hex[byte >> 4U];
      output[output_size++] = hex[byte & 0x0fU];
    }
  }
  output[output_size] = '\0';
  SEXP result = PROTECT(Rf_mkCharLenCE(
    output,
    (int) output_size,
    CE_UTF8
  ));
  UNPROTECT(2);
  return result;
}

SEXP paradox_unary_callback_call(SEXP callback, SEXP value) {
  switch (TYPEOF(value)) {
  case SYMSXP:
  case LANGSXP:
  case PROMSXP:
  case BCODESXP: {
    /* These four are the R values that Rf_eval() interprets instead of
     * returning. `quote` is a base primitive and cannot be shadowed in the
     * base environment the callbacks are evaluated in. */
    SEXP quoted = PROTECT(Rf_lang2(R_QuoteSymbol, value));
    SEXP call = Rf_lang2(callback, quoted);
    UNPROTECT(1);
    return call;
  }
  default:
    return Rf_lang2(callback, value);
  }
}

NORET void paradox_error_from_scalar_string(SEXP message) {
  PROTECT(message);
  if (TYPEOF(message) != STRSXP || ALTREP(message) ||
      XLENGTH(message) != 1 || STRING_ELT(message, 0) == NA_STRING ||
      Rf_getCharCE(STRING_ELT(message, 0)) == CE_BYTES) {
    UNPROTECT(1);
    Rf_error("Internal error: expected one non-missing textual diagnostic");
  }
  SEXP string = PROTECT(STRING_ELT(message, 0));
  const size_t size = strlen(Rf_translateChar(string));
  if ((uintmax_t) size >= (uintmax_t) R_XLEN_T_MAX) {
    UNPROTECT(2);
    Rf_error("Internal error: diagnostic exceeds native string limits");
  }
  char *owned = paradox_temporary_alloc(
    (R_xlen_t) size + 1,
    sizeof(*owned)
  );
  const char *translated = Rf_translateChar(string);
  if (strlen(translated) != size) {
    UNPROTECT(2);
    Rf_error("Internal error: diagnostic changed while being copied");
  }
  memcpy(owned, translated, size + 1U);
  UNPROTECT(2);
  Rf_error("%s", owned);
}

NORET void paradox_assertion_error(const char *variable, SEXP diagnostic) {
  if (variable == NULL || strchr(variable, '\'') != NULL ||
      TYPEOF(diagnostic) != STRSXP || ALTREP(diagnostic) ||
      XLENGTH(diagnostic) != 1 ||
      STRING_ELT(diagnostic, 0) == NA_STRING) {
    Rf_error("Internal error: invalid assertion diagnostic");
  }
  SEXP text = PROTECT(paradox_diagnostic_charsxp(
    STRING_ELT(diagnostic, 0)
  ));
  paradox_utf8_piece_t pieces[] = {
    paradox_utf8_ascii_piece("Assertion on '"),
    paradox_utf8_ascii_piece(variable),
    paradox_utf8_ascii_piece("' failed: "),
    paradox_utf8_charsxp_piece(text),
    paradox_utf8_ascii_piece(
      paradox_charsxp_ends_sentence(text) ? "" : "."
    )
  };
  SEXP message = PROTECT(paradox_utf8_message(pieces, 5));
  paradox_error_from_scalar_string(message);
}

#define PARADOX_BUILTIN_METADATA_MAX_DEPTH 64U
#define PARADOX_BUILTIN_METADATA_MAX_NODES 65536U

static int bounded_metadata_attribute_count(
    SEXP value, R_xlen_t *count) {
  if (paradox_api_has_no_attributes(value)) {
    *count = 0;
    return TRUE;
  }
  return paradox_api_map_bounded_stored_attributes(
    value,
    (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
    NULL,
    NULL,
    count
  );
}

static int bounded_metadata_attribute_count_is(
    SEXP value, R_xlen_t expected) {
  R_xlen_t count = 0;
  return bounded_metadata_attribute_count(value, &count) &&
    count == expected;
}

static int builtin_metadata_has_no_attributes(SEXP value) {
  return paradox_api_has_no_attributes(value);
}

typedef struct {
  SEXP selected;
  SEXP tags[PARADOX_BUILTIN_METADATA_MAX_DEPTH];
  R_xlen_t count;
  int found;
  int valid;
} bounded_metadata_tag_query_t;

static void select_bounded_metadata_tag(
    SEXP tag, SEXP value, void *data) {
  bounded_metadata_tag_query_t *query = data;
  if (!query->valid || TYPEOF(tag) != SYMSXP ||
      value == R_NilValue ||
      query->count >=
        (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH) {
    query->valid = FALSE;
    return;
  }
  for (R_xlen_t index = 0; index < query->count; ++index) {
    if (query->tags[index] == tag) {
      query->valid = FALSE;
      return;
    }
  }
  query->tags[query->count] = tag;
  ++query->count;
  if (tag == query->selected) query->found = TRUE;
}

int paradox_bounded_metadata_has_tag(
    SEXP value, SEXP tag, int *found) {
  if (TYPEOF(tag) != SYMSXP || found == NULL) {
    Rf_error("Internal error: invalid bounded metadata query");
  }
  if (paradox_api_has_no_attributes(value)) {
    *found = FALSE;
    return TRUE;
  }
  bounded_metadata_tag_query_t query = {
    tag,
    {R_NilValue},
    0,
    FALSE,
    TRUE
  };
  R_xlen_t count = 0;
  const int bounded = paradox_api_map_bounded_stored_attributes(
    value,
    (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
    select_bounded_metadata_tag,
    &query,
    &count
  );
  *found = query.found;
  return bounded && query.valid && query.count == count;
}

SEXP paradox_snapshot_semantic_vector(SEXP value) {
  if (value == R_NilValue) {
    return value;
  }

  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  if (type != LGLSXP && type != INTSXP && type != REALSXP &&
      type != CPLXSXP && type != STRSXP && type != RAWSXP &&
      type != VECSXP) {
    Rf_error(
      "Cannot snapshot semantic value of type `%s`",
      Rf_type2char(type)
    );
  }
  if (Rf_isS4(value)) {
    Rf_error("Cannot snapshot an S4 semantic vector shell");
  }

  const R_xlen_t size = XLENGTH(value);
  /* Rf_isObject() changed between an int and Rboolean declaration across
   * supported R headers. Normalize the predicate before retaining it so the
   * exact-generation comparisons below stay warning-free on every runtime. */
  const int source_object = Rf_isObject(value) != FALSE;
  R_xlen_t source_attribute_count = 0;
  if (!bounded_metadata_attribute_count(
      value,
      &source_attribute_count
    )) {
    Rf_error("Semantic vector metadata is not bounded");
  }
  /*
   * Names identify the meaning of semantic elements. Allocate both
   * destinations before selecting either side, then capture an ordinary
   * source's name and value together in one callback-free pass. A pending
   * finalizer may mutate the caller-owned vector during either allocation,
   * but cannot leave this snapshot with names and values from different
   * generations. Stable ALTREP values retain their established contract:
   * structural names are owned first, then each semantic element is observed
   * exactly once.
   */
  int has_names = FALSE;
  if (!paradox_bounded_metadata_has_tag(
      value,
      R_NamesSymbol,
      &has_names
    )) {
    Rf_error("Semantic vector metadata is not ordinary and bounded");
  }
  SEXP result = PROTECT(Rf_allocVector(type, size));
  int protect_count = 1;
  SEXP stable_names = R_NilValue;
  if (has_names) {
    stable_names = PROTECT(Rf_allocVector(STRSXP, size));
    ++protect_count;
  }
  int selected_has_names = FALSE;
  if (Rf_isS4(value) ||
      (Rf_isObject(value) != FALSE) != source_object ||
      !bounded_metadata_attribute_count_is(
        value,
        source_attribute_count
      ) ||
      !paradox_bounded_metadata_has_tag(
        value,
        R_NamesSymbol,
        &selected_has_names
      ) ||
      selected_has_names != has_names) {
    UNPROTECT(protect_count);
    Rf_error("Semantic vector structure changed while being snapshotted");
  }
  /*
   * The bounded tag scan is allocation-free and immediately precedes this raw
   * selection.  It therefore proves the live spine finite before the selector
   * can walk it; PROTECT itself performs no intervening allocation.
   */
  SEXP source_names = PROTECT(selected_has_names
    ? paradox_api_raw_attribute(value, R_NamesSymbol)
    : R_NilValue);
  ++protect_count;
  /* Deferred-string and wrapper ALTREP names are ordinary base-R output
   * (`names(x) <- as.character(...)`), so the names shell admits ALTREP; the
   * capture below observes each name exactly once, before any semantic
   * element, so a dispatching name read can only precede value capture. */
  if ((source_names != R_NilValue) != has_names ||
      (has_names && (TYPEOF(source_names) != STRSXP ||
        Rf_isS4(source_names) ||
        Rf_isObject(source_names) ||
        !paradox_api_has_no_attributes(source_names) ||
        XLENGTH(source_names) != size))) {
    UNPROTECT(protect_count);
    Rf_error("Invalid names on semantic vector");
  }

  const int semantic_altrep = ALTREP(value);
  if (!semantic_altrep && XLENGTH(value) != size) {
    UNPROTECT(protect_count);
    Rf_error("Semantic vector changed length while being snapshotted");
  }

  if (type == VECSXP && !semantic_altrep) {
    if (!paradox_capture_list_identities(
        value,
        stable_names,
        result
      )) {
      UNPROTECT(protect_count);
      Rf_error("Invalid names on semantic vector");
    }
    if (stable_names != R_NilValue) {
      Rf_setAttrib(result, R_NamesSymbol, stable_names);
    }
    UNPROTECT(protect_count);
    return result;
  }

  /* Names are captured completely before the first semantic element, for
   * every representation: an ALTREP name read may dispatch, so confining all
   * name observations to this stage means the element capture below always
   * reads one post-names generation. */
  if (stable_names != R_NilValue) {
    for (R_xlen_t index = 0; index < size; ++index) {
      SET_STRING_ELT(
        stable_names,
        index,
        STRING_ELT(source_names, index)
      );
    }
    if ((SEXPTYPE) TYPEOF(value) != type || Rf_isS4(value) ||
        (!semantic_altrep &&
          (ALTREP(value) || XLENGTH(value) != size))) {
      UNPROTECT(protect_count);
      Rf_error("Semantic vector changed while being snapshotted");
    }
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    if (index != 0 &&
        index % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    switch (type) {
    case LGLSXP:
      SET_LOGICAL_ELT(result, index, LOGICAL_ELT(value, index));
      break;
    case INTSXP:
      SET_INTEGER_ELT(result, index, INTEGER_ELT(value, index));
      break;
    case REALSXP:
      SET_REAL_ELT(result, index, REAL_ELT(value, index));
      break;
    case CPLXSXP:
      paradox_api_set_complex_elt(
        result,
        index,
        COMPLEX_ELT(value, index)
      );
      break;
    case STRSXP:
      SET_STRING_ELT(result, index, STRING_ELT(value, index));
      break;
    case RAWSXP:
      paradox_api_set_raw_elt(result, index, RAW_ELT(value, index));
      break;
    case VECSXP:
      SET_VECTOR_ELT(result, index, VECTOR_ELT(value, index));
      break;
    default:
      UNPROTECT(protect_count);
      Rf_error("Internal error: unsupported semantic vector type");
    }
  }
  /*
   * An ALTREP Elt method may re-enter R.  Keep the source generation's
   * structural receipt closed across that observation too.  Ordinary vector
   * element reads above cannot allocate or evaluate, so they need no third
   * attribute traversal.
   */
  if (semantic_altrep &&
      (Rf_isS4(value) ||
        (Rf_isObject(value) != FALSE) != source_object ||
        !bounded_metadata_attribute_count_is(
          value,
          source_attribute_count
        ))) {
    UNPROTECT(protect_count);
    Rf_error("Semantic vector structure changed while being snapshotted");
  }
  if (stable_names != R_NilValue) {
    Rf_setAttrib(result, R_NamesSymbol, stable_names);
  }
  UNPROTECT(protect_count);
  return result;
}

typedef struct {
  SEXP tags;
  SEXP values;
  R_xlen_t capacity;
  R_xlen_t count;
  int valid;
} atomic_leaf_attribute_capture_t;

static void capture_atomic_leaf_attribute(SEXP tag, SEXP value, void *data) {
  atomic_leaf_attribute_capture_t *capture = data;
  if (!capture->valid || TYPEOF(tag) != SYMSXP ||
      capture->count >= capture->capacity) {
    capture->valid = FALSE;
    return;
  }
  SET_VECTOR_ELT(capture->tags, capture->count, tag);
  SET_VECTOR_ELT(capture->values, capture->count, value);
  ++capture->count;
}

typedef struct {
  SEXP tags;
  SEXP values;
  R_xlen_t expected;
  R_xlen_t count;
  int current;
} atomic_leaf_attribute_receipt_t;

static void compare_atomic_leaf_attribute(SEXP tag, SEXP value, void *data) {
  atomic_leaf_attribute_receipt_t *receipt = data;
  if (!receipt->current || receipt->count >= receipt->expected ||
      VECTOR_ELT(receipt->tags, receipt->count) != tag ||
      VECTOR_ELT(receipt->values, receipt->count) != value) {
    receipt->current = FALSE;
    return;
  }
  ++receipt->count;
}

int paradox_ordinary_vector_payload_equal(SEXP left, SEXP right) {
  if (TYPEOF(left) != TYPEOF(right) || ALTREP(left) || ALTREP(right) ||
      XLENGTH(left) != XLENGTH(right) ||
      Rf_isS4(left) != Rf_isS4(right)) {
    return FALSE;
  }
  const R_xlen_t size = XLENGTH(left);
  switch ((SEXPTYPE) TYPEOF(left)) {
  case LGLSXP:
    return (uintmax_t) size <= SIZE_MAX / sizeof(int) &&
      memcmp(
        LOGICAL(left),
        LOGICAL(right),
        (size_t) size * sizeof(int)
      ) == 0;
  case INTSXP:
    return (uintmax_t) size <= SIZE_MAX / sizeof(int) &&
      memcmp(
        INTEGER(left),
        INTEGER(right),
        (size_t) size * sizeof(int)
      ) == 0;
  case REALSXP:
    return (uintmax_t) size <= SIZE_MAX / sizeof(double) &&
      memcmp(
        REAL(left),
        REAL(right),
        (size_t) size * sizeof(double)
      ) == 0;
  case CPLXSXP:
    return (uintmax_t) size <= SIZE_MAX / sizeof(Rcomplex) &&
      memcmp(
        COMPLEX(left),
        COMPLEX(right),
        (size_t) size * sizeof(Rcomplex)
      ) == 0;
  case RAWSXP:
    return (uintmax_t) size <= SIZE_MAX / sizeof(Rbyte) &&
      memcmp(
        RAW(left),
        RAW(right),
        (size_t) size * sizeof(Rbyte)
      ) == 0;
  case STRSXP:
    for (R_xlen_t index = 0; index < size; ++index) {
      if (STRING_ELT(left, index) != STRING_ELT(right, index)) return FALSE;
    }
    return TRUE;
  case VECSXP:
  case EXPRSXP:
    for (R_xlen_t index = 0; index < size; ++index) {
      if (VECTOR_ELT(left, index) != VECTOR_ELT(right, index)) return FALSE;
    }
    return TRUE;
  default:
    return FALSE;
  }
}

/*
 * Allocate and copy one ordinary vector payload without asking R to duplicate
 * its caller-owned attribute pairlist. VECSXP/EXPRSXP elements are opaque
 * payload identities here; callers separately own the bounded presentation
 * metadata graph when their boundary requires it.
 */
static inline SEXP snapshot_ordinary_vector_payload(
    SEXP source, int require_attribute_free) {
  if (ALTREP(source) || Rf_isS4(source)) {
    Rf_error("Cannot own a non-ordinary vector payload");
  }
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
  if (type != LGLSXP && type != INTSXP && type != REALSXP &&
      type != CPLXSXP && type != RAWSXP && type != STRSXP &&
      type != VECSXP && type != EXPRSXP) {
    Rf_error(
      "Cannot own vector payload of type `%s`",
      Rf_type2char(type)
    );
  }
  const R_xlen_t size = XLENGTH(source);
  /* Every private caller roots `source` across this sole allocation. */
  SEXP result = PROTECT(Rf_allocVector(type, size));
  if (ALTREP(source) || Rf_isS4(source) ||
      (SEXPTYPE) TYPEOF(source) != type || XLENGTH(source) != size ||
      (require_attribute_free &&
        (!builtin_metadata_has_no_attributes(source) ||
          Rf_isObject(source)))) {
    UNPROTECT(1);
    Rf_error("Vector payload changed while being snapshotted");
  }
  switch (type) {
  case LGLSXP:
    if ((uintmax_t) size > SIZE_MAX / sizeof(int)) {
      UNPROTECT(1);
      Rf_error("Vector payload is too large to own");
    }
    memcpy(
      LOGICAL(result),
      LOGICAL_RO(source),
      (size_t) size * sizeof(int)
    );
    break;
  case INTSXP:
    if ((uintmax_t) size > SIZE_MAX / sizeof(int)) {
      UNPROTECT(1);
      Rf_error("Vector payload is too large to own");
    }
    memcpy(
      INTEGER(result),
      INTEGER_RO(source),
      (size_t) size * sizeof(int)
    );
    break;
  case REALSXP:
    if ((uintmax_t) size > SIZE_MAX / sizeof(double)) {
      UNPROTECT(1);
      Rf_error("Vector payload is too large to own");
    }
    memcpy(
      REAL(result),
      REAL_RO(source),
      (size_t) size * sizeof(double)
    );
    break;
  case CPLXSXP:
    if ((uintmax_t) size > SIZE_MAX / sizeof(Rcomplex)) {
      UNPROTECT(1);
      Rf_error("Vector payload is too large to own");
    }
    memcpy(
      COMPLEX(result),
      COMPLEX_RO(source),
      (size_t) size * sizeof(Rcomplex)
    );
    break;
  case RAWSXP:
    if ((uintmax_t) size > SIZE_MAX / sizeof(Rbyte)) {
      UNPROTECT(1);
      Rf_error("Vector payload is too large to own");
    }
    memcpy(
      RAW(result),
      RAW_RO(source),
      (size_t) size * sizeof(Rbyte)
    );
    break;
  case STRSXP:
    for (R_xlen_t index = 0; index < size; ++index) {
      SET_STRING_ELT(result, index, STRING_ELT(source, index));
    }
    break;
  case VECSXP:
  case EXPRSXP:
    for (R_xlen_t index = 0; index < size; ++index) {
      SET_VECTOR_ELT(result, index, VECTOR_ELT(source, index));
    }
    break;
  default:
    UNPROTECT(1);
    Rf_error("Internal error: unsupported vector payload type");
  }
  UNPROTECT(1);
  return result;
}

/*
 * R's ordinary deep duplicator is itself recursive and does not detect
 * cycles. Built-in leaves deliberately retain arbitrary *ordinary* metadata,
 * so both the preflight and package-owned bounded copier below enforce the
 * same explicit depth, per-node attribute-count, and total-node limits.
 * Malformed native cycles or adversarially broad/deep metadata therefore
 * become a clean rejection even when a pending finalizer changes a shared
 * metadata node after preflight.
 *
 * These bounds are intentionally far above ordinary class/names/dim/
 * dimnames/levels metadata.  The node bound also caps a broad acyclic graph;
 * repeated nodes in a DAG are counted each time because R's deep duplicator
 * visits them each time too.
 */
typedef struct {
  SEXP path[PARADOX_BUILTIN_METADATA_MAX_DEPTH];
  size_t nodes;
} builtin_metadata_graph_t;

typedef struct {
  builtin_metadata_graph_t *graph;
  size_t depth;
  SEXP tags[PARADOX_BUILTIN_METADATA_MAX_DEPTH];
  size_t count;
  int valid;
} builtin_metadata_attribute_graph_t;

static int builtin_metadata_graph_is_ordinary(SEXP value,
  builtin_metadata_graph_t *graph,
  size_t depth
);

static int builtin_metadata_attribute_graph_is_ordinary(
  SEXP value,
  builtin_metadata_graph_t *graph,
  size_t depth
);

static void builtin_metadata_attribute_is_ordinary(SEXP tag, SEXP value,
    void *data) {
  builtin_metadata_attribute_graph_t *state = data;
  if (!state->valid || TYPEOF(tag) != SYMSXP || value == R_NilValue ||
      state->count >= PARADOX_BUILTIN_METADATA_MAX_DEPTH) {
    state->valid = FALSE;
    return;
  }
  for (size_t index = 0; index < state->count; ++index) {
    if (state->tags[index] == tag) {
      state->valid = FALSE;
      return;
    }
  }
  state->tags[state->count] = tag;
  ++state->count;
  if (state->graph->nodes >= PARADOX_BUILTIN_METADATA_MAX_NODES) {
    state->valid = FALSE;
    return;
  }
  /* Count the stored attribute edge separately from the value node reached
   * below, matching the copy and terminal-receipt budgets. */
  ++state->graph->nodes;
  if (!builtin_metadata_graph_is_ordinary(
        value,
        state->graph,
        state->depth
      )) {
    state->valid = FALSE;
  }
}

static int builtin_metadata_attribute_graph_is_ordinary(
    SEXP value, builtin_metadata_graph_t *graph, size_t depth) {
  builtin_metadata_attribute_graph_t attribute_state = {
    graph,
    depth,
    {R_NilValue},
    0U,
    TRUE
  };
  R_xlen_t attribute_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
    value,
    (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
    builtin_metadata_attribute_is_ordinary,
    &attribute_state,
    &attribute_count
  )) {
    return FALSE;
  }
  return attribute_state.valid &&
    attribute_state.count == (size_t) attribute_count;
}

static int builtin_metadata_graph_is_ordinary(SEXP value,
    builtin_metadata_graph_t *graph, size_t depth) {
  if (ALTREP(value) || Rf_isS4(value)) return FALSE;
  if (graph->nodes >= PARADOX_BUILTIN_METADATA_MAX_NODES) return FALSE;
  ++graph->nodes;

  /*
   * These are identity leaves for R's deep duplicator.  In particular, do
   * not follow an environment or external pointer into a graph it owns by
   * reference; Paradox's opaque-leaf policy retains that exact identity.
   */
  switch ((SEXPTYPE) TYPEOF(value)) {
  case NILSXP:
  case SYMSXP:
  case ENVSXP:
  case SPECIALSXP:
  case BUILTINSXP:
  case EXTPTRSXP:
  case BCODESXP:
  case WEAKREFSXP:
  case CHARSXP:
  case PROMSXP:
    return TRUE;
  default:
    break;
  }

  if (depth >= PARADOX_BUILTIN_METADATA_MAX_DEPTH) {
    return FALSE;
  }
  for (size_t index = 0; index < depth; ++index) {
    if (graph->path[index] == value) return FALSE;
  }
  graph->path[depth] = value;

  if (!builtin_metadata_attribute_graph_is_ordinary(
      value,
      graph,
      depth + 1U
    )) {
    return FALSE;
  }

  switch ((SEXPTYPE) TYPEOF(value)) {
  case VECSXP:
  case EXPRSXP:
    for (R_xlen_t index = 0; index < XLENGTH(value); ++index) {
      if (!builtin_metadata_graph_is_ordinary(
          VECTOR_ELT(value, index),
          graph,
          depth + 1U
        )) {
        return FALSE;
      }
    }
    return TRUE;
  case LISTSXP:
  case LANGSXP:
    return builtin_metadata_graph_is_ordinary(
        CAR(value),
        graph,
        depth + 1U
      ) && builtin_metadata_graph_is_ordinary(
        CDR(value),
        graph,
        depth + 1U
      );
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case RAWSXP:
  case STRSXP:
    return TRUE;
  case CLOSXP:
    /* R duplicates a closure shell but old supported R has no allocation-free
     * public formals/body/environment accessors for the terminal receipt. */
    return FALSE;
  case DOTSXP:
    /* No supported R release exposes a public allocator for a fresh DOTSXP
     * node. Promise-list internals are not ordinary presentation metadata. */
    return FALSE;
  default:
    return FALSE;
  }
}

static int builtin_metadata_attributes_are_ordinary(SEXP value) {
  if (Rf_isS4(value)) return FALSE;
  builtin_metadata_graph_t graph;
  /* Count the selected carrier exactly as the complete-graph preflight does,
   * but deliberately do not traverse its semantic payload. */
  graph.path[0] = value;
  graph.nodes = 1U;
  return builtin_metadata_attribute_graph_is_ordinary(
    value,
    &graph,
    1U
  );
}

typedef struct {
  SEXP source_path[PARADOX_BUILTIN_METADATA_MAX_DEPTH];
  size_t nodes;
  const char *failure_message;
} builtin_metadata_copy_t;

typedef struct {
  SEXP entries;
  R_xlen_t capacity;
  R_xlen_t count;
  int valid;
} builtin_metadata_copy_attribute_capture_t;

static void capture_builtin_metadata_copy_attribute(SEXP tag, SEXP value,
    void *data) {
  builtin_metadata_copy_attribute_capture_t *capture = data;
  if (!capture->valid || TYPEOF(tag) != SYMSXP ||
      value == R_NilValue || capture->count >= capture->capacity) {
    capture->valid = FALSE;
    return;
  }
  for (R_xlen_t index = 0; index < capture->count; ++index) {
    if (VECTOR_ELT(capture->entries, 3 * index) == tag) {
      capture->valid = FALSE;
      return;
    }
  }
  SET_VECTOR_ELT(capture->entries, 3 * capture->count, tag);
  SET_VECTOR_ELT(capture->entries, 3 * capture->count + 1, value);
  ++capture->count;
}

NORET static void builtin_metadata_copy_error(
    const builtin_metadata_copy_t *copy) {
  Rf_error("%s", copy->failure_message);
}

static SEXP copy_builtin_metadata_graph(
  SEXP source,
  builtin_metadata_copy_t *copy,
  size_t depth
);

typedef struct {
  SEXP tags;
  R_xlen_t capacity;
  R_xlen_t count;
  int valid;
} builtin_metadata_tag_capture_t;

static void capture_builtin_metadata_tag(SEXP tag, SEXP value, void *data) {
  (void) value;
  builtin_metadata_tag_capture_t *capture = data;
  if (!capture->valid || TYPEOF(tag) != SYMSXP ||
      capture->count >= capture->capacity) {
    capture->valid = FALSE;
    return;
  }
  for (R_xlen_t index = 0; index < capture->count; ++index) {
    if (VECTOR_ELT(capture->tags, index) == tag) {
      capture->valid = FALSE;
      return;
    }
  }
  SET_VECTOR_ELT(capture->tags, capture->count, tag);
  ++capture->count;
}

static void clear_builtin_metadata_attributes(
    SEXP destination, builtin_metadata_copy_t *copy) {
  R_xlen_t empty_count = 0;
  if (paradox_api_map_bounded_stored_attributes(
      destination,
      0,
      NULL,
      NULL,
      &empty_count
    )) {
    if (empty_count != 0 || Rf_isObject(destination)) {
      builtin_metadata_copy_error(copy);
    }
    return;
  }

  SEXP tags = PROTECT(Rf_allocVector(
    VECSXP,
    (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH
  ));
  builtin_metadata_tag_capture_t capture = {
    tags,
    (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
    0,
    TRUE
  };
  R_xlen_t selected_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
    destination,
    (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
    capture_builtin_metadata_tag,
    &capture,
    &selected_count
  )) {
    UNPROTECT(1);
    builtin_metadata_copy_error(copy);
  }
  if (!capture.valid || capture.count != selected_count) {
    UNPROTECT(1);
    builtin_metadata_copy_error(copy);
  }
  for (R_xlen_t index = 0; index < selected_count; ++index) {
    Rf_setAttrib(destination, VECTOR_ELT(tags, index), R_NilValue);
  }
  if (!builtin_metadata_has_no_attributes(destination) ||
      Rf_isObject(destination)) {
    UNPROTECT(1);
    builtin_metadata_copy_error(copy);
  }
  UNPROTECT(1);
}

static int builtin_metadata_attribute_priority(
    SEXP tag, SEXP comment_symbol) {
  if (tag == R_DimSymbol) return 0;
  if (tag == R_DimNamesSymbol) return 2;
  if (tag == R_ClassSymbol) return 3;
  /*
   * Names, row.names, tsp, comment, and general attributes all retain their
   * relative selected order after dimension installation. Naming these cases
   * explicitly documents that they intentionally use their public setters.
   */
  if (tag == R_NamesSymbol || tag == R_RowNamesSymbol ||
      tag == R_TspSymbol || tag == comment_symbol) {
    return 1;
  }
  return 1;
}

typedef struct {
  SEXP entries;
  R_xlen_t capacity;
  R_xlen_t count;
  int valid;
} bounded_shallow_attribute_capture_t;

static void capture_bounded_shallow_attribute(
    SEXP tag, SEXP value, void *data) {
  bounded_shallow_attribute_capture_t *capture = data;
  if (!capture->valid || TYPEOF(tag) != SYMSXP ||
      value == R_NilValue || capture->count >= capture->capacity) {
    capture->valid = FALSE;
    return;
  }
  for (R_xlen_t index = 0; index < capture->count; ++index) {
    if (VECTOR_ELT(capture->entries, 2 * index) == tag) {
      capture->valid = FALSE;
      return;
    }
  }
  SET_VECTOR_ELT(capture->entries, 2 * capture->count, tag);
  SET_VECTOR_ELT(capture->entries, 2 * capture->count + 1, value);
  ++capture->count;
}

typedef struct {
  SEXP entries;
  R_xlen_t expected;
  R_xlen_t count;
  int current;
} bounded_shallow_source_receipt_t;

static void compare_bounded_shallow_source_attribute(
    SEXP tag, SEXP value, void *data) {
  bounded_shallow_source_receipt_t *receipt = data;
  if (!receipt->current || receipt->count >= receipt->expected ||
      VECTOR_ELT(receipt->entries, 2 * receipt->count) != tag ||
      VECTOR_ELT(receipt->entries, 2 * receipt->count + 1) != value) {
    receipt->current = FALSE;
    return;
  }
  ++receipt->count;
}

typedef struct {
  SEXP tags[PARADOX_BUILTIN_METADATA_MAX_DEPTH];
  SEXP values[PARADOX_BUILTIN_METADATA_MAX_DEPTH];
  R_xlen_t count;
  int valid;
} shallow_container_metadata_t;

static void capture_shallow_container_attribute(
    SEXP tag, SEXP value, void *data) {
  shallow_container_metadata_t *metadata = data;
  if (!metadata->valid || TYPEOF(tag) != SYMSXP ||
      value == R_NilValue ||
      metadata->count >= PARADOX_BUILTIN_METADATA_MAX_DEPTH) {
    metadata->valid = FALSE;
    return;
  }
  for (R_xlen_t index = 0; index < metadata->count; ++index) {
    if (metadata->tags[index] == tag) {
      metadata->valid = FALSE;
      return;
    }
  }
  metadata->tags[metadata->count] = tag;
  metadata->values[metadata->count] = value;
  ++metadata->count;
}

typedef struct {
  const shallow_container_metadata_t *expected;
  unsigned char seen[PARADOX_BUILTIN_METADATA_MAX_DEPTH];
  R_xlen_t count;
  int current;
} shallow_container_metadata_receipt_t;

static void compare_shallow_container_attribute(
    SEXP tag, SEXP value, void *data) {
  shallow_container_metadata_receipt_t *receipt = data;
  if (!receipt->current || TYPEOF(tag) != SYMSXP ||
      value == R_NilValue) {
    receipt->current = FALSE;
    return;
  }
  R_xlen_t selected = 0;
  while (selected < receipt->expected->count &&
      receipt->expected->tags[selected] != tag) {
    ++selected;
  }
  if (selected == receipt->expected->count ||
      receipt->seen[selected] ||
      receipt->expected->values[selected] != value) {
    receipt->current = FALSE;
    return;
  }
  receipt->seen[selected] = 1U;
  ++receipt->count;
}

/*
 * R's public dimnames setter deliberately shallow-duplicates the outer list.
 * That normalization changes only the outer carrier: its elements and its
 * own bounded metadata values retain exact identity.  Admit precisely that
 * documented setter result while continuing to reject every other
 * normalization or mutation.
 */
static int shallow_dimnames_setter_result_matches(
    SEXP source, SEXP destination) {
  if (source == destination) return TRUE;
  if (TYPEOF(source) != VECSXP || TYPEOF(destination) != VECSXP ||
      ALTREP(source) || ALTREP(destination) ||
      Rf_isS4(source) || Rf_isS4(destination) ||
      (Rf_isObject(source) != FALSE) !=
        (Rf_isObject(destination) != FALSE) ||
      XLENGTH(source) != XLENGTH(destination)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(source); ++index) {
    if (VECTOR_ELT(source, index) != VECTOR_ELT(destination, index)) {
      return FALSE;
    }
  }

  shallow_container_metadata_t expected = {
    {R_NilValue},
    {R_NilValue},
    0,
    TRUE
  };
  R_xlen_t expected_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
      source,
      (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
      capture_shallow_container_attribute,
      &expected,
      &expected_count
    ) || !expected.valid || expected.count != expected_count) {
    return FALSE;
  }
  shallow_container_metadata_receipt_t receipt = {
    &expected,
    {0},
    0,
    TRUE
  };
  R_xlen_t destination_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
      destination,
      (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
      compare_shallow_container_attribute,
      &receipt,
      &destination_count
    ) || !receipt.current ||
      receipt.count != expected.count ||
      destination_count != expected_count) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < expected.count; ++index) {
    if (!receipt.seen[index]) return FALSE;
  }
  return TRUE;
}

static int compact_row_names_count(SEXP value, R_xlen_t *count) {
  if (TYPEOF(value) != INTSXP || XLENGTH(value) != 2 ||
      INTEGER_ELT(value, 0) != NA_INTEGER) {
    return FALSE;
  }
  const int encoded = INTEGER_ELT(value, 1);
  if (encoded == NA_INTEGER || encoded == INT_MIN) return FALSE;
  *count = encoded < 0
    ? (R_xlen_t) -encoded
    : (R_xlen_t) encoded;
  return TRUE;
}

static int explicit_row_names_sequence(SEXP value, R_xlen_t count) {
  if (TYPEOF(value) != INTSXP || XLENGTH(value) != count) return FALSE;
  if (count > INT_MAX) return FALSE;
  for (R_xlen_t index = 0; index < count; ++index) {
    if (INTEGER_ELT(value, index) != (int) index + 1) return FALSE;
  }
  return TRUE;
}

/*
 * The public row.names setter may duplicate a carrier and may interchange R's
 * compact c(NA, +/-n) representation with the explicit ordinary 1:n spelling.
 * Accept only those representation-preserving normalizations; arbitrary row
 * labels still require exact ordinary payload equality.
 */
static int shallow_row_names_setter_result_matches(
    SEXP source, SEXP destination) {
  if (source == destination) return TRUE;
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
  if ((type != INTSXP && type != STRSXP) ||
      (SEXPTYPE) TYPEOF(destination) != type ||
      ALTREP(source) || ALTREP(destination) ||
      Rf_isS4(source) || Rf_isS4(destination) ||
      Rf_isObject(source) || Rf_isObject(destination) ||
      !paradox_api_has_no_attributes(source) ||
      !paradox_api_has_no_attributes(destination)) {
    return FALSE;
  }
  if (paradox_ordinary_vector_payload_equal(source, destination)) {
    return TRUE;
  }
  if (type != INTSXP) return FALSE;
  R_xlen_t source_count = 0;
  R_xlen_t destination_count = 0;
  const int source_compact =
    compact_row_names_count(source, &source_count);
  const int destination_compact =
    compact_row_names_count(destination, &destination_count);
  if (source_compact && destination_compact) {
    return source_count == destination_count;
  }
  if (source_compact) {
    return explicit_row_names_sequence(destination, source_count);
  }
  return destination_compact &&
    explicit_row_names_sequence(source, destination_count);
}

static int bounded_shallow_attribute_is_selected(
    SEXP tag, paradox_shallow_attribute_policy_t policy, int has_dimensions) {
  if (policy == PARADOX_SHALLOW_ATTRIBUTES_ALL) return TRUE;
  return tag == R_DimSymbol || tag == R_DimNamesSymbol ||
    (tag == R_NamesSymbol && !has_dimensions);
}

typedef struct {
  SEXP entries;
  R_xlen_t captured;
  paradox_shallow_attribute_policy_t policy;
  int has_dimensions;
  unsigned char seen[PARADOX_BUILTIN_METADATA_MAX_DEPTH];
  R_xlen_t count;
  int current;
} bounded_shallow_destination_receipt_t;

static void compare_bounded_shallow_destination_attribute(
    SEXP tag, SEXP value, void *data) {
  bounded_shallow_destination_receipt_t *receipt = data;
  if (!receipt->current) return;
  R_xlen_t selected = receipt->captured;
  for (R_xlen_t index = 0; index < receipt->captured; ++index) {
    if (VECTOR_ELT(receipt->entries, 2 * index) == tag) {
      selected = index;
      break;
    }
  }
  if (selected == receipt->captured ||
      !bounded_shallow_attribute_is_selected(
        tag,
        receipt->policy,
        receipt->has_dimensions
      ) ||
      receipt->seen[selected] ||
      (VECTOR_ELT(receipt->entries, 2 * selected + 1) != value &&
        !((tag == R_DimNamesSymbol &&
          shallow_dimnames_setter_result_matches(
            VECTOR_ELT(receipt->entries, 2 * selected + 1),
            value
          )) ||
          (tag == R_RowNamesSymbol &&
            shallow_row_names_setter_result_matches(
              VECTOR_ELT(receipt->entries, 2 * selected + 1),
              value
            ))))) {
    receipt->current = FALSE;
    return;
  }
  receipt->seen[selected] = 1U;
  ++receipt->count;
}

void paradox_copy_bounded_shallow_attributes(
    SEXP destination, SEXP source,
    paradox_shallow_attribute_policy_t policy,
    const char *failure_message) {
  if (failure_message == NULL ||
      (policy != PARADOX_SHALLOW_ATTRIBUTES_ALL &&
        policy != PARADOX_SHALLOW_ATTRIBUTES_LOGICAL_STRUCTURE)) {
    Rf_error("Internal error: invalid bounded attribute-copy request");
  }
  if (Rf_isS4(destination) || Rf_isObject(destination) ||
      !builtin_metadata_has_no_attributes(destination)) {
    Rf_error("%s", failure_message);
  }

  R_xlen_t initial_count = 0;
  if (!bounded_metadata_attribute_count(source, &initial_count)) {
    Rf_error("%s", failure_message);
  }
  if (initial_count == 0) {
    if (Rf_isS4(source) || Rf_isObject(source)) {
      Rf_error("%s", failure_message);
    }
    return;
  }

  PROTECT(destination);
  PROTECT(source);
  /*
   * Intern the one non-global standard tag before selecting the generation.
   * The fixed scanned carrier is likewise allocated first.  Every subsequent
   * source observation and the complete capture are allocation-free.
   */
  SEXP comment_symbol = Rf_install("comment");
  SEXP entries = PROTECT(Rf_allocVector(
    VECSXP,
    (R_xlen_t) (2U * PARADOX_BUILTIN_METADATA_MAX_DEPTH)
  ));
  bounded_shallow_attribute_capture_t capture = {
    entries,
    (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
    0,
    TRUE
  };
  R_xlen_t selected_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
      source,
      (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
      capture_bounded_shallow_attribute,
      &capture,
      &selected_count
    ) || !capture.valid || capture.count != selected_count) {
    UNPROTECT(3);
    Rf_error("%s", failure_message);
  }
  const int source_object = Rf_isObject(source) != FALSE;
  if (Rf_isS4(source)) {
    UNPROTECT(3);
    Rf_error("%s", failure_message);
  }

  int has_dimensions = FALSE;
  int has_raw_class = FALSE;
  R_xlen_t expected_destination_count = 0;
  for (R_xlen_t index = 0; index < selected_count; ++index) {
    SEXP tag = VECTOR_ELT(entries, 2 * index);
    if (tag == R_DimSymbol) has_dimensions = TRUE;
    if (tag == R_ClassSymbol) has_raw_class = TRUE;
  }
  if (policy == PARADOX_SHALLOW_ATTRIBUTES_LOGICAL_STRUCTURE &&
      (source_object || has_raw_class)) {
    UNPROTECT(3);
    Rf_error("%s", failure_message);
  }
  for (R_xlen_t index = 0; index < selected_count; ++index) {
    if (bounded_shallow_attribute_is_selected(
        VECTOR_ELT(entries, 2 * index),
        policy,
        has_dimensions
      )) {
      ++expected_destination_count;
    }
  }

  /*
   * Use only public setters, in the same dependency-safe order as the deep
   * metadata owner.  The captured values remain exact shallow identities;
   * nested metadata is deliberately neither inspected nor duplicated.
   */
  for (int priority = 0; priority <= 3; ++priority) {
    for (R_xlen_t index = 0; index < selected_count; ++index) {
      SEXP tag = VECTOR_ELT(entries, 2 * index);
      if (!bounded_shallow_attribute_is_selected(
          tag,
          policy,
          has_dimensions
        ) || builtin_metadata_attribute_priority(
          tag,
          comment_symbol
        ) != priority) {
        continue;
      }
      Rf_setAttrib(
        destination,
        tag,
        VECTOR_ELT(entries, 2 * index + 1)
      );
    }
  }

  bounded_shallow_source_receipt_t source_receipt = {
    entries,
    selected_count,
    0,
    TRUE
  };
  R_xlen_t source_count = 0;
  bounded_shallow_destination_receipt_t destination_receipt = {
    entries,
    selected_count,
    policy,
    has_dimensions,
    {0},
    0,
    TRUE
  };
  R_xlen_t destination_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
      source,
      (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
      compare_bounded_shallow_source_attribute,
      &source_receipt,
      &source_count
    ) || !source_receipt.current ||
      source_receipt.count != selected_count ||
      source_count != selected_count ||
      Rf_isS4(source) ||
      (Rf_isObject(source) != FALSE) != source_object ||
      !paradox_api_map_bounded_stored_attributes(
        destination,
        (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
        compare_bounded_shallow_destination_attribute,
        &destination_receipt,
        &destination_count
      ) || !destination_receipt.current ||
      destination_receipt.count != expected_destination_count ||
      destination_count != expected_destination_count ||
      Rf_isS4(destination) ||
      (Rf_isObject(destination) != FALSE) !=
        (policy == PARADOX_SHALLOW_ATTRIBUTES_ALL && source_object)) {
    UNPROTECT(3);
    Rf_error("%s", failure_message);
  }
  for (R_xlen_t index = 0; index < selected_count; ++index) {
    if (bounded_shallow_attribute_is_selected(
        VECTOR_ELT(entries, 2 * index),
        policy,
        has_dimensions
      ) && !destination_receipt.seen[index]) {
      UNPROTECT(3);
      Rf_error("%s", failure_message);
    }
  }
  UNPROTECT(3);
}

/*
 * Copy one attribute graph without ever handing a caller-owned nested node to
 * R's recursive duplicator. The fixed source carrier is allocated before its
 * allocation-free tag/value capture, so every selected value remains rooted
 * even if a later allocation runs a finalizer that rewrites the live source.
 * Rf_setAttrib() sees only a package-owned bounded copy.
 */
static void copy_builtin_metadata_attributes(
    SEXP destination, SEXP source, builtin_metadata_copy_t *copy,
    size_t depth) {
  if (Rf_isS4(source)) {
    builtin_metadata_copy_error(copy);
  }
  R_xlen_t empty_count = 0;
  if (paradox_api_map_bounded_stored_attributes(
      source,
      0,
      NULL,
      NULL,
      &empty_count
    )) {
    if (empty_count != 0 ||
        !builtin_metadata_has_no_attributes(destination) ||
        (Rf_isObject(destination) != FALSE) !=
        (Rf_isObject(source) != FALSE)) {
      builtin_metadata_copy_error(copy);
    }
    return;
  }

  SEXP captured = PROTECT(Rf_allocVector(
    VECSXP,
    (R_xlen_t) (3U * PARADOX_BUILTIN_METADATA_MAX_DEPTH)
  ));
  builtin_metadata_copy_attribute_capture_t capture = {
    captured,
    (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
    0,
    TRUE
  };
  R_xlen_t selected_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
    source,
    (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
    capture_builtin_metadata_copy_attribute,
    &capture,
    &selected_count
  )) {
    UNPROTECT(1);
    builtin_metadata_copy_error(copy);
  }
  if (!capture.valid || capture.count != selected_count) {
    UNPROTECT(1);
    builtin_metadata_copy_error(copy);
  }
  if ((uintmax_t) selected_count >
      PARADOX_BUILTIN_METADATA_MAX_NODES - copy->nodes) {
    UNPROTECT(1);
    builtin_metadata_copy_error(copy);
  }
  copy->nodes += (size_t) selected_count;
  const int source_object = Rf_isObject(source) != FALSE;

  for (R_xlen_t index = 0; index < selected_count; ++index) {
    SEXP value = VECTOR_ELT(captured, 3 * index + 1);
    SEXP owned = PROTECT(copy_builtin_metadata_graph(
      value,
      copy,
      depth
    ));
    SET_VECTOR_ELT(captured, 3 * index + 2, owned);
    UNPROTECT(1);
  }
  clear_builtin_metadata_attributes(destination, copy);
  SEXP comment_symbol = Rf_install("comment");
  /*
   * Rf_setAttrib() is the sole cross-version public setter. Install
   * dependency-sensitive standard attributes in one fixed order: dim before
   * names/general metadata and dimnames, then class last so its object bit is
   * terminal. Names, row.names, tsp, comment, and general attributes retain
   * their relative selected order in the middle wave.
   * Unsupported raw spellings that public R normalizes are rejected by the
   * allocation-free receipt rather than reproduced through SET_ATTRIB.
   */
  for (int priority = 0; priority <= 3; ++priority) {
    for (R_xlen_t index = 0; index < selected_count; ++index) {
      SEXP tag = VECTOR_ELT(captured, 3 * index);
      if (builtin_metadata_attribute_priority(
          tag,
          comment_symbol
        ) != priority) {
        continue;
      }
      Rf_setAttrib(
        destination,
        tag,
        VECTOR_ELT(captured, 3 * index + 2)
      );
    }
  }
  if ((Rf_isObject(destination) != FALSE) != source_object) {
    UNPROTECT(1);
    builtin_metadata_copy_error(copy);
  }
  UNPROTECT(1);
}

static int builtin_metadata_copy_source_shape(
    SEXP source, SEXPTYPE type, R_xlen_t size) {
  return !ALTREP(source) && !Rf_isS4(source) &&
    (SEXPTYPE) TYPEOF(source) == type && XLENGTH(source) == size;
}

/*
 * Native recursion is hard-bounded to 64 frames by the same contract as the
 * preflight and terminal receipt. Every child edge is selected into a scanned
 * VECSXP before the first recursive allocation. Thus a finalizer may make the
 * operation reject at the terminal receipt, but it cannot redirect this walk
 * into an unbounded or cyclic live graph.
 */
static SEXP copy_builtin_metadata_graph(
    SEXP source, builtin_metadata_copy_t *copy, size_t depth) {
  PROTECT(source);
  if (ALTREP(source) || Rf_isS4(source) ||
      copy->nodes >= PARADOX_BUILTIN_METADATA_MAX_NODES) {
    UNPROTECT(1);
    builtin_metadata_copy_error(copy);
  }
  ++copy->nodes;

  switch ((SEXPTYPE) TYPEOF(source)) {
  case NILSXP:
  case SYMSXP:
  case ENVSXP:
  case SPECIALSXP:
  case BUILTINSXP:
  case EXTPTRSXP:
  case BCODESXP:
  case WEAKREFSXP:
  case CHARSXP:
  case PROMSXP:
    UNPROTECT(1);
    return source;
  default:
    break;
  }

  if (depth >= PARADOX_BUILTIN_METADATA_MAX_DEPTH) {
    UNPROTECT(1);
    builtin_metadata_copy_error(copy);
  }
  for (size_t index = 0; index < depth; ++index) {
    if (copy->source_path[index] == source) {
      UNPROTECT(1);
      builtin_metadata_copy_error(copy);
    }
  }
  copy->source_path[depth] = source;

  const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
  switch (type) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case RAWSXP:
  case STRSXP: {
    const R_xlen_t size = XLENGTH(source);
    SEXP result = PROTECT(Rf_allocVector(type, size));
    if (!builtin_metadata_copy_source_shape(source, type, size)) {
      UNPROTECT(2);
      builtin_metadata_copy_error(copy);
    }
    switch (type) {
    case LGLSXP:
      if ((uintmax_t) size > SIZE_MAX / sizeof(int)) {
        UNPROTECT(2);
        builtin_metadata_copy_error(copy);
      }
      memcpy(
        LOGICAL(result),
        LOGICAL_RO(source),
        (size_t) size * sizeof(int)
      );
      break;
    case INTSXP:
      if ((uintmax_t) size > SIZE_MAX / sizeof(int)) {
        UNPROTECT(2);
        builtin_metadata_copy_error(copy);
      }
      memcpy(
        INTEGER(result),
        INTEGER_RO(source),
        (size_t) size * sizeof(int)
      );
      break;
    case REALSXP:
      if ((uintmax_t) size > SIZE_MAX / sizeof(double)) {
        UNPROTECT(2);
        builtin_metadata_copy_error(copy);
      }
      memcpy(
        REAL(result),
        REAL_RO(source),
        (size_t) size * sizeof(double)
      );
      break;
    case CPLXSXP:
      if ((uintmax_t) size > SIZE_MAX / sizeof(Rcomplex)) {
        UNPROTECT(2);
        builtin_metadata_copy_error(copy);
      }
      memcpy(
        COMPLEX(result),
        COMPLEX_RO(source),
        (size_t) size * sizeof(Rcomplex)
      );
      break;
    case RAWSXP:
      if ((uintmax_t) size > SIZE_MAX / sizeof(Rbyte)) {
        UNPROTECT(2);
        builtin_metadata_copy_error(copy);
      }
      memcpy(
        RAW(result),
        RAW_RO(source),
        (size_t) size * sizeof(Rbyte)
      );
      break;
    case STRSXP:
      for (R_xlen_t index = 0; index < size; ++index) {
        SET_STRING_ELT(result, index, STRING_ELT(source, index));
      }
      break;
    default:
      UNPROTECT(2);
      Rf_error("Internal error: unreachable metadata vector type");
    }
    copy_builtin_metadata_attributes(
      result,
      source,
      copy,
      depth + 1U
    );
    if (!builtin_metadata_copy_source_shape(source, type, size)) {
      UNPROTECT(2);
      builtin_metadata_copy_error(copy);
    }
    UNPROTECT(2);
    return result;
  }
  case VECSXP:
  case EXPRSXP: {
    const R_xlen_t size = XLENGTH(source);
    if ((uintmax_t) size >
        PARADOX_BUILTIN_METADATA_MAX_NODES - copy->nodes) {
      UNPROTECT(1);
      builtin_metadata_copy_error(copy);
    }
    SEXP result = PROTECT(Rf_allocVector(type, size));
    SEXP selected = PROTECT(Rf_allocVector(VECSXP, size));
    if (!builtin_metadata_copy_source_shape(source, type, size)) {
      UNPROTECT(3);
      builtin_metadata_copy_error(copy);
    }
    for (R_xlen_t index = 0; index < size; ++index) {
      SET_VECTOR_ELT(selected, index, VECTOR_ELT(source, index));
    }
    for (R_xlen_t index = 0; index < size; ++index) {
      SEXP value = PROTECT(copy_builtin_metadata_graph(
        VECTOR_ELT(selected, index),
        copy,
        depth + 1U
      ));
      SET_VECTOR_ELT(result, index, value);
      UNPROTECT(1);
    }
    copy_builtin_metadata_attributes(
      result,
      source,
      copy,
      depth + 1U
    );
    if (!builtin_metadata_copy_source_shape(source, type, size)) {
      UNPROTECT(3);
      builtin_metadata_copy_error(copy);
    }
    UNPROTECT(3);
    return result;
  }
  case LISTSXP:
  case LANGSXP: {
    SEXP result = PROTECT(Rf_allocVector(type, 1));
    SEXP selected = PROTECT(Rf_allocVector(VECSXP, 3));
    if (ALTREP(source) || Rf_isS4(source) ||
        (SEXPTYPE) TYPEOF(source) != type ||
        (TAG(source) != R_NilValue && TYPEOF(TAG(source)) != SYMSXP)) {
      UNPROTECT(3);
      builtin_metadata_copy_error(copy);
    }
    SET_VECTOR_ELT(selected, 0, CAR(source));
    SET_VECTOR_ELT(selected, 1, CDR(source));
    SET_VECTOR_ELT(selected, 2, TAG(source));
    SEXP owned_car = PROTECT(copy_builtin_metadata_graph(
      VECTOR_ELT(selected, 0),
      copy,
      depth + 1U
    ));
    SEXP owned_cdr = PROTECT(copy_builtin_metadata_graph(
      VECTOR_ELT(selected, 1),
      copy,
      depth + 1U
    ));
    SETCAR(result, owned_car);
    SETCDR(result, owned_cdr);
    SET_TAG(result, VECTOR_ELT(selected, 2));
    UNPROTECT(2);
    copy_builtin_metadata_attributes(
      result,
      source,
      copy,
      depth + 1U
    );
    if (ALTREP(source) || Rf_isS4(source) ||
        (SEXPTYPE) TYPEOF(source) != type) {
      UNPROTECT(3);
      builtin_metadata_copy_error(copy);
    }
    UNPROTECT(3);
    return result;
  }
  case CLOSXP:
  case DOTSXP:
  default:
    UNPROTECT(1);
    builtin_metadata_copy_error(copy);
  }
}

static void own_builtin_metadata_attributes(
    SEXP destination, SEXP source, const char *failure_message) {
  builtin_metadata_copy_t copy;
  copy.source_path[0] = source;
  copy.nodes = 1U;
  copy.failure_message = failure_message;
  copy_builtin_metadata_attributes(destination, source, &copy, 1U);
}

typedef struct {
  SEXP source_path[PARADOX_BUILTIN_METADATA_MAX_DEPTH];
  SEXP snapshot_path[PARADOX_BUILTIN_METADATA_MAX_DEPTH];
  size_t nodes;
} builtin_metadata_receipt_t;

typedef struct {
  SEXP tags[PARADOX_BUILTIN_METADATA_MAX_DEPTH];
  SEXP values[PARADOX_BUILTIN_METADATA_MAX_DEPTH];
  size_t count;
  int valid;
} builtin_metadata_attribute_capture_t;

static void capture_builtin_metadata_attribute(SEXP tag, SEXP value,
    void *data) {
  builtin_metadata_attribute_capture_t *capture = data;
  if (!capture->valid || TYPEOF(tag) != SYMSXP || value == R_NilValue ||
      capture->count >= PARADOX_BUILTIN_METADATA_MAX_DEPTH) {
    capture->valid = FALSE;
    return;
  }
  for (size_t index = 0; index < capture->count; ++index) {
    if (capture->tags[index] == tag) {
      capture->valid = FALSE;
      return;
    }
  }
  capture->tags[capture->count] = tag;
  capture->values[capture->count] = value;
  ++capture->count;
}

typedef struct {
  const builtin_metadata_attribute_capture_t *snapshot;
  builtin_metadata_receipt_t *receipt;
  size_t depth;
  unsigned char seen[PARADOX_BUILTIN_METADATA_MAX_DEPTH];
  size_t count;
  int current;
} builtin_metadata_attribute_receipt_t;

static int builtin_metadata_receipt_current(SEXP source, SEXP snapshot,
  builtin_metadata_receipt_t *receipt,
  size_t depth
);

static void compare_builtin_metadata_attribute(SEXP tag, SEXP value,
    void *data) {
  builtin_metadata_attribute_receipt_t *state = data;
  if (!state->current || TYPEOF(tag) != SYMSXP ||
      value == R_NilValue || state->count >= state->snapshot->count) {
    state->current = FALSE;
    return;
  }
  size_t selected = 0U;
  while (selected < state->snapshot->count &&
      state->snapshot->tags[selected] != tag) {
    ++selected;
  }
  if (selected == state->snapshot->count || state->seen[selected] ||
      !builtin_metadata_receipt_current(
        value,
        state->snapshot->values[selected],
        state->receipt,
        state->depth
      )) {
    state->current = FALSE;
    return;
  }
  state->seen[selected] = 1U;
  ++state->count;
}

/*
 * Compare only the stored attribute graphs of two selected carriers.  The
 * Attribute tags and every recursively copied value have one exact
 * allocation-free receipt.
 */
static int builtin_metadata_attribute_graph_receipt_current(
    SEXP source, SEXP snapshot, builtin_metadata_receipt_t *receipt,
    size_t depth) {
  if (Rf_isS4(source) || Rf_isS4(snapshot)) {
    return FALSE;
  }
  builtin_metadata_attribute_capture_t attributes = {
    {R_NilValue},
    {R_NilValue},
    0U,
    TRUE
  };
  R_xlen_t snapshot_attribute_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
    snapshot,
    (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
    capture_builtin_metadata_attribute,
    &attributes,
    &snapshot_attribute_count
  )) {
    return FALSE;
  }
  if (!attributes.valid ||
      attributes.count != (size_t) snapshot_attribute_count) {
    return FALSE;
  }
  if ((uintmax_t) snapshot_attribute_count >
      PARADOX_BUILTIN_METADATA_MAX_NODES - receipt->nodes) {
    return FALSE;
  }
  receipt->nodes += (size_t) snapshot_attribute_count;
  builtin_metadata_attribute_receipt_t attribute_receipt = {
    &attributes,
    receipt,
    depth,
    {0},
    0U,
    TRUE
  };
  R_xlen_t source_attribute_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
    source,
    (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
    compare_builtin_metadata_attribute,
    &attribute_receipt,
    &source_attribute_count
  )) {
    return FALSE;
  }
  return attribute_receipt.current &&
    source_attribute_count == snapshot_attribute_count &&
    attribute_receipt.count == attributes.count;
}

static int builtin_metadata_receipt_current(SEXP source, SEXP snapshot,
    builtin_metadata_receipt_t *receipt, size_t depth) {
  if (ALTREP(source) || ALTREP(snapshot) ||
      Rf_isS4(source) || Rf_isS4(snapshot) ||
      TYPEOF(source) != TYPEOF(snapshot) ||
      (Rf_isObject(source) != FALSE) !=
        (Rf_isObject(snapshot) != FALSE)) {
    return FALSE;
  }
  if (receipt->nodes >= PARADOX_BUILTIN_METADATA_MAX_NODES) return FALSE;
  ++receipt->nodes;

  /*
   * These are exactly the identity leaves returned unchanged by R's ordinary
   * deep duplicator on every supported runtime.  Every mutable ordinary
   * vector or pairlist is freshly allocated, including zero-length vectors,
   * so pointer identity for one of those carriers is evidence of sharing, not
   * a successful receipt.
   */
  switch ((SEXPTYPE) TYPEOF(source)) {
  case NILSXP:
  case SYMSXP:
  case ENVSXP:
  case SPECIALSXP:
  case BUILTINSXP:
  case EXTPTRSXP:
  case BCODESXP:
  case WEAKREFSXP:
  case CHARSXP:
  case PROMSXP:
    return source == snapshot;
  default:
    break;
  }
  if (source == snapshot) return FALSE;

  if (depth >= PARADOX_BUILTIN_METADATA_MAX_DEPTH) {
    return FALSE;
  }
  for (size_t index = 0; index < depth; ++index) {
    if (receipt->source_path[index] == source ||
        receipt->snapshot_path[index] == snapshot) {
      return FALSE;
    }
  }
  receipt->source_path[depth] = source;
  receipt->snapshot_path[depth] = snapshot;

  if (!builtin_metadata_attribute_graph_receipt_current(
      source,
      snapshot,
      receipt,
      depth + 1U
    )) {
    return FALSE;
  }

  switch ((SEXPTYPE) TYPEOF(source)) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case RAWSXP:
  case STRSXP:
    return paradox_ordinary_vector_payload_equal(source, snapshot);
  case VECSXP:
  case EXPRSXP:
    if (XLENGTH(source) != XLENGTH(snapshot)) return FALSE;
    for (R_xlen_t index = 0; index < XLENGTH(source); ++index) {
      if (!builtin_metadata_receipt_current(
          VECTOR_ELT(source, index),
          VECTOR_ELT(snapshot, index),
          receipt,
          depth + 1U
        )) {
        return FALSE;
      }
    }
    return TRUE;
  case LISTSXP:
  case LANGSXP:
    return TAG(source) == TAG(snapshot) &&
      builtin_metadata_receipt_current(
        CAR(source),
        CAR(snapshot),
        receipt,
        depth + 1U
      ) && builtin_metadata_receipt_current(
        CDR(source),
        CDR(snapshot),
        receipt,
        depth + 1U
      );
  case CLOSXP:
  case DOTSXP:
    return FALSE;
  default:
    return FALSE;
  }
}

int paradox_builtin_value_leaf_receipt_current(SEXP source, SEXP snapshot) {
  /* Typed S4 leaves are documented opaque identity tokens. */
  if (Rf_isS4(source) || Rf_isS4(snapshot)) {
    return source == snapshot;
  }
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
  if (type != LGLSXP && type != INTSXP && type != REALSXP &&
      type != CPLXSXP && type != STRSXP && type != RAWSXP) {
    return source == snapshot;
  }
  builtin_metadata_receipt_t receipt;
  receipt.nodes = 0U;
  return builtin_metadata_receipt_current(
    source,
    snapshot,
    &receipt,
    0U
  );
}

static int builtin_metadata_attributes_receipt_current(
    SEXP source, SEXP snapshot) {
  builtin_metadata_receipt_t receipt;
  /* The preflight counts the selected attribute carrier itself. */
  receipt.source_path[0] = source;
  receipt.snapshot_path[0] = snapshot;
  receipt.nodes = 1U;
  return builtin_metadata_attribute_graph_receipt_current(
    source,
    snapshot,
    &receipt,
    1U
  );
}

int paradox_altrep_builtin_value_leaf_metadata_is_current(
    SEXP source, SEXP snapshot) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
  if (!ALTREP(source) || ALTREP(snapshot) || Rf_isS4(source) ||
      Rf_isS4(snapshot) || (SEXPTYPE) TYPEOF(snapshot) != type ||
      (type != LGLSXP && type != INTSXP && type != REALSXP &&
        type != CPLXSXP && type != STRSXP && type != RAWSXP) ||
      (Rf_isObject(source) != FALSE) !=
        (Rf_isObject(snapshot) != FALSE)) {
    return FALSE;
  }
  /* The snapshot helper has already materialized the stable semantic payload
   * and independently owned this complete ordinary metadata graph. Raw stored-
   * attribute traversal cannot invoke ALTREP methods, allocate, or evaluate R,
   * so it is safe in the callback-free terminal receipt phase. */
  return builtin_metadata_attributes_receipt_current(source, snapshot);
}

static void run_builtin_metadata_copy_hook(SEXP hook) {
  if (hook == R_NilValue) return;
  PROTECT(hook);
  SEXP call = PROTECT(Rf_lang1(hook));
  SEXP result = PROTECT(Rf_eval(call, R_BaseEnv));
  (void) result;
  UNPROTECT(3);
}

static SEXP snapshot_builtin_value_leaf(SEXP value, SEXP hook) {
  if (Rf_isS4(value)) {
    return value;
  }
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  if (type != LGLSXP && type != INTSXP && type != REALSXP &&
      type != CPLXSXP && type != STRSXP && type != RAWSXP) {
    return value;
  }

  PROTECT(value);
  if (!ALTREP(value)) {
    if (hook == R_NilValue &&
        builtin_metadata_has_no_attributes(value) &&
        !Rf_isObject(value)) {
      SEXP result = PROTECT(snapshot_ordinary_vector_payload(value, TRUE));
      UNPROTECT(2);
      return result;
    }
    /*
     * The package-owned graph copier allocates an attribute-free payload and
     * copies every supported metadata node itself.  The attribute-free hot
     * path is therefore one allocation plus one memcpy/pointer loop; it never
     * asks R's pairlist duplicator to traverse a caller-owned attribute spine.
     */
    if (!builtin_metadata_attributes_are_ordinary(value)) {
      UNPROTECT(1);
      Rf_error(
        "Built-in value metadata must be ordinary, acyclic, and bounded"
      );
    }
    /* The package-owned copier repeats the complete bound after this
     * test-only seam.  A pending finalizer can therefore make the operation
     * reject, but cannot redirect R's recursive pairlist duplicator. */
    run_builtin_metadata_copy_hook(hook);
    builtin_metadata_copy_t copy;
    copy.nodes = 0U;
    copy.failure_message =
      "Built-in value metadata must be ordinary, acyclic, and bounded";
    SEXP result = PROTECT(copy_builtin_metadata_graph(
      value,
      &copy,
      0U
    ));
    if (!paradox_ordinary_vector_payload_equal(value, result) ||
        !builtin_metadata_attributes_receipt_current(value, result)) {
      UNPROTECT(2);
      Rf_error("Built-in value changed while being snapshotted");
    }
    UNPROTECT(2);
    return result;
  }

  /*
   * A stable ALTREP leaf must be materialized element-by-element, so retain a
   * receipt for every arbitrary stored attribute around that observation.
   * Names are independently owned by the semantic-vector snapshot; the
   * remaining attribute values keep their ordinary shallow-copy identity.
   */
  R_xlen_t attribute_count = 0;
  if (!bounded_metadata_attribute_count(value, &attribute_count)) {
    UNPROTECT(1);
    Rf_error(
      "Built-in value metadata must be ordinary, acyclic, and bounded"
    );
  }
  SEXP attribute_tags = PROTECT(Rf_allocVector(VECSXP, attribute_count));
  SEXP attribute_values = PROTECT(Rf_allocVector(VECSXP, attribute_count));
  atomic_leaf_attribute_capture_t capture = {
    attribute_tags,
    attribute_values,
    attribute_count,
    0,
    TRUE
  };
  R_xlen_t captured_attribute_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
    value,
    (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
    capture_atomic_leaf_attribute,
    &capture,
    &captured_attribute_count
  ) || !capture.valid || capture.count != attribute_count ||
      captured_attribute_count != attribute_count) {
    UNPROTECT(3);
    Rf_error("Built-in value attributes changed before snapshot");
  }

  SEXP result = PROTECT(paradox_snapshot_semantic_vector(value));
  atomic_leaf_attribute_receipt_t receipt = {
    attribute_tags,
    attribute_values,
    attribute_count,
    0,
    TRUE
  };
  R_xlen_t receipt_attribute_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
    value,
    (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
    compare_atomic_leaf_attribute,
    &receipt,
    &receipt_attribute_count
  ) || !receipt.current || receipt.count != attribute_count ||
      receipt_attribute_count != attribute_count) {
    UNPROTECT(4);
    Rf_error("Built-in value attributes changed while being snapshotted");
  }

  if (attribute_count != 0) {
    /*
     * Copy directly from the receipted live carrier through the bounded
     * package-owned mapper. No R duplicator sees its attribute pairlist.
     */
    if (!builtin_metadata_attributes_are_ordinary(value)) {
      UNPROTECT(4);
      Rf_error(
        "Built-in value metadata must be ordinary, acyclic, and bounded"
      );
    }
    run_builtin_metadata_copy_hook(hook);
    own_builtin_metadata_attributes(
      result,
      value,
      "Built-in value metadata must be ordinary, acyclic, and bounded"
    );
    if (!builtin_metadata_attributes_receipt_current(
        value,
        result
      )) {
      UNPROTECT(4);
      Rf_error("Built-in value changed while being snapshotted");
    }
    atomic_leaf_attribute_receipt_t final_receipt = {
      attribute_tags,
      attribute_values,
      attribute_count,
      0,
      TRUE
    };
    R_xlen_t final_attribute_count = 0;
    if (!paradox_api_map_bounded_stored_attributes(
      value,
      (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
      compare_atomic_leaf_attribute,
      &final_receipt,
      &final_attribute_count
    ) || !final_receipt.current ||
        final_receipt.count != attribute_count ||
        final_attribute_count != attribute_count) {
      UNPROTECT(4);
      Rf_error("Built-in value changed while being snapshotted");
    }
  }
  UNPROTECT(4);
  return result;
}

SEXP paradox_snapshot_builtin_value_leaf(SEXP value) {
  return snapshot_builtin_value_leaf(value, R_NilValue);
}

SEXP paradox_test_builtin_metadata_copy_reentry(SEXP value, SEXP hook) {
  if (hook != R_NilValue && !Rf_isFunction(hook)) {
    Rf_error("Metadata-copy test hook must be a function or NULL");
  }
  return snapshot_builtin_value_leaf(value, hook);
}

SEXP paradox_own_builtin_special_value_leaves(SEXP special_values,
    int typed) {
  if (!typed) {
    return special_values;
  }
  PROTECT(special_values);
  for (R_xlen_t index = 0; index < XLENGTH(special_values); ++index) {
    SEXP leaf = PROTECT(paradox_snapshot_builtin_value_leaf(
      VECTOR_ELT(special_values, index)
    ));
    SET_VECTOR_ELT(special_values, index, leaf);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return special_values;
}

static SEXP snapshot_public_data_table_column(SEXP value);

static void own_named_data_table_columns(SEXP table) {
  PROTECT(table);
  for (R_xlen_t column = 0; column < XLENGTH(table); ++column) {
    SEXP value = VECTOR_ELT(table, column);
    int has_names = FALSE;
    if (!paradox_bounded_metadata_has_tag(
        value,
        R_NamesSymbol,
        &has_names
      )) {
      UNPROTECT(1);
      Rf_error(
        "Internal error: expected bounded package-owned column metadata"
      );
    }
    if (has_names) {
      /*
       * This is a rare normalization path for package-built columns. Use the
       * same bounded payload/metadata snapshot as the defensive public path
       * so a future producer cannot expose an unbounded attribute spine to
       * R's pairlist duplicator. Canonical unnamed columns stay allocation-
       * and protection-stack-free here.
       */
      SEXP owned = PROTECT(snapshot_public_data_table_column(value));
      if (owned == value) {
        Rf_error(
          "Internal error: cannot own named data.table column %.0f",
          (double) (column + 1)
        );
      }
      Rf_setAttrib(owned, R_NamesSymbol, R_NilValue);
      SET_VECTOR_ELT(table, column, owned);
      UNPROTECT(1);
    }
  }
  UNPROTECT(1);
}

/*
 * Own one public data.table column without recursively copying list-column
 * leaves. The payload and its arbitrary attribute metadata have deliberately
 * different ownership rules:
 *
 * - ordinary vector/list payloads use one package-owned allocation and
 *   memcpy/pointer loop, so a VECSXP column keeps the exact opaque elements
 *   selected from the source without duplicating its attribute pairlist;
 * - stable ALTREP payloads must be materialized through their typed Elt API
 *   because a legitimate class need not expose a Dataptr;
 * - every attribute carrier is presentation metadata and must be
 *   independently owned. The bounded package-owned copier detaches that graph
 *   without recursively touching VECSXP payload elements.
 *
 * For ALTREP, retain an exact tag/value receipt across materialization and
 * pass the live carrier directly to the bounded metadata copier. An Elt
 * callback may therefore make the operation reject, but it cannot pair one
 * payload generation with a different top-level attribute generation.
 */
static SEXP snapshot_public_data_table_column(SEXP value) {
  PROTECT(value);
  int protect_count = 1;
  const int source_altrep = ALTREP(value);
  R_xlen_t initial_attribute_count = 0;
  if (!bounded_metadata_attribute_count(
      value,
      &initial_attribute_count
    )) {
    UNPROTECT(protect_count);
    Rf_error(
      "data.table column metadata must be ordinary, acyclic, and bounded"
    );
  }

  /*
   * Canonical ordinary columns take exactly one payload allocation and one
   * memcpy/pointer loop.  The zero-limit raw mapper before and after that
   * allocation prevents an attribute spine introduced by a pending finalizer
   * from escaping the slow bounded-metadata path.
   */
  if (!source_altrep && initial_attribute_count == 0) {
    if (Rf_isObject(value)) {
      UNPROTECT(protect_count);
      Rf_error(
        "data.table column metadata must be ordinary, acyclic, and bounded"
      );
    }
    SEXP owned = PROTECT(snapshot_ordinary_vector_payload(value, TRUE));
    ++protect_count;
    UNPROTECT(protect_count);
    return owned;
  }

  if (source_altrep && initial_attribute_count == 0) {
    if (Rf_isObject(value)) {
      UNPROTECT(protect_count);
      Rf_error(
        "data.table column metadata must be ordinary, acyclic, and bounded"
      );
    }
    SEXP owned = PROTECT(paradox_snapshot_semantic_vector(value));
    ++protect_count;
    if (!builtin_metadata_has_no_attributes(value) ||
        Rf_isObject(value)) {
      UNPROTECT(protect_count);
      Rf_error("data.table column changed while being snapshotted");
    }
    UNPROTECT(protect_count);
    return owned;
  }

  SEXP attribute_tags = PROTECT(Rf_allocVector(
    VECSXP,
    (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH
  ));
  ++protect_count;
  SEXP attribute_values = PROTECT(Rf_allocVector(
    VECSXP,
    (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH
  ));
  ++protect_count;
  SEXP owned;
  R_xlen_t attribute_count = 0;
  atomic_leaf_attribute_capture_t capture = {
    attribute_tags,
    attribute_values,
    (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
    0,
    TRUE
  };

  if (source_altrep) {
    if (!paradox_api_map_bounded_stored_attributes(
        value,
        (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
        capture_atomic_leaf_attribute,
        &capture,
        &attribute_count
      ) || !capture.valid ||
        capture.count != attribute_count) {
      UNPROTECT(protect_count);
      Rf_error("data.table column attributes changed before snapshot");
    }
    owned = PROTECT(paradox_snapshot_semantic_vector(value));
    ++protect_count;
  } else {
    owned = PROTECT(snapshot_ordinary_vector_payload(value, FALSE));
    ++protect_count;
    if (!paradox_api_map_bounded_stored_attributes(
        value,
        (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
        capture_atomic_leaf_attribute,
        &capture,
        &attribute_count
      ) || !capture.valid ||
        capture.count != attribute_count) {
      UNPROTECT(protect_count);
      Rf_error("data.table column attributes changed before ownership");
    }
  }

  atomic_leaf_attribute_receipt_t selected_receipt = {
    attribute_tags,
    attribute_values,
    attribute_count,
    0,
    TRUE
  };
  R_xlen_t selected_attribute_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
      value,
      (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
      compare_atomic_leaf_attribute,
      &selected_receipt,
      &selected_attribute_count
    ) || !selected_receipt.current ||
      selected_receipt.count != attribute_count ||
      selected_attribute_count != attribute_count ||
      !builtin_metadata_attributes_are_ordinary(value)) {
    UNPROTECT(protect_count);
    Rf_error(
      "data.table column metadata must be ordinary, acyclic, and bounded"
    );
  }

  own_builtin_metadata_attributes(
    owned,
    value,
    "data.table column metadata must be ordinary, acyclic, and bounded"
  );
  if (!builtin_metadata_attributes_receipt_current(value, owned)) {
    UNPROTECT(protect_count);
    Rf_error("data.table column changed while being snapshotted");
  }
  atomic_leaf_attribute_receipt_t final_receipt = {
    attribute_tags,
    attribute_values,
    attribute_count,
    0,
    TRUE
  };
  R_xlen_t final_attribute_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
      value,
      (R_xlen_t) PARADOX_BUILTIN_METADATA_MAX_DEPTH,
      compare_atomic_leaf_attribute,
      &final_receipt,
      &final_attribute_count
    ) || !final_receipt.current ||
      final_receipt.count != attribute_count ||
      final_attribute_count != attribute_count ||
      (!source_altrep &&
        !paradox_ordinary_vector_payload_equal(value, owned))) {
    UNPROTECT(protect_count);
    Rf_error("data.table column changed while being snapshotted");
  }
  UNPROTECT(protect_count);
  return owned;
}

static void own_all_data_table_columns(SEXP table) {
  PROTECT(table);
  for (R_xlen_t column = 0; column < XLENGTH(table); ++column) {
    SEXP value = PROTECT(VECTOR_ELT(table, column));
    SEXP owned = PROTECT(snapshot_public_data_table_column(value));
    /* data.table may remove column names by reference while normalizing or
     * updating a facade.  The column is private now, so that normalization can
     * no longer reach the caller's source. */
    Rf_setAttrib(owned, R_NamesSymbol, R_NilValue);
    SET_VECTOR_ELT(table, column, owned);
    UNPROTECT(2);
  }
  UNPROTECT(1);
}

SEXP paradox_prepare_data_table(SEXP table, int growable) {
  (void) growable;
  /* Public facade columns must not retain names that data.table may later
   * remove by reference. Own only those unusual columns before normalization;
   * canonical unnamed columns remain shared with the detached facade shell. */
  own_named_data_table_columns(table);
  /* data.table's public object representation uses an external pointer whose
   * protected external pointer identifies the owning table and whose tag
   * identifies its names vector. Constructing the same representation solely
   * through R's public API keeps a native-built facade safe for subsequent
   * by-reference data.table operations without calling into data.table. */
  SEXP names = PROTECT(Rf_getAttrib(table, R_NamesSymbol));
  SEXP owner = PROTECT(R_MakeExternalPtr(
    (void *) table,
    R_NilValue,
    R_NilValue
  ));
  SEXP selfref = PROTECT(R_MakeExternalPtr(
    (void *) R_NilValue,
    names,
    owner
  ));
  Rf_setAttrib(table, Rf_install(".internal.selfref"), selfref);
  UNPROTECT(3);
  return table;
}

SEXP paradox_prepare_fresh_data_table(SEXP table) {
  PROTECT(table);
  SEXP result = PROTECT(paradox_prepare_data_table(table, TRUE));
  /* data.table ties the self-reference tag to this exact names vector.
   * Reattaching the already-owned names last matches its outward ownership
   * boundary without the defensive copies required for caller-owned input. */
  SEXP names = PROTECT(Rf_getAttrib(result, R_NamesSymbol));
  Rf_setAttrib(result, R_NamesSymbol, R_NilValue);
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(3);
  return result;
}

SEXP paradox_finalize_data_table(SEXP table) {
  if (TYPEOF(table) != VECSXP || ALTREP(table) || Rf_isS4(table)) {
    Rf_error("Internal error: expected an ordinary data.table shell");
  }
  SEXP selfref_symbol = Rf_install(".internal.selfref");

  /*
   * This is a registered entry point, so the input can have arbitrary aliases.
   * First bound the complete presentation-metadata graph without invoking S3
   * inheritance or R's pairlist duplicator. Then copy the outer column payload
   * and metadata through the same package-owned operations used for public
   * leaves. The terminal allocation-free receipt rejects a finalizer splice
   * before any selected column is independently owned.
   *
   * Attribute values, including names, row names, class, and data.table cache
   * carriers, are detached by the bounded copier. The stale input self-reference
   * is removed below and replaced only after every column spine is private.
   */
  PROTECT(table);
  if (!builtin_metadata_attributes_are_ordinary(table)) {
    UNPROTECT(1);
    Rf_error("Internal error: expected bounded data.table metadata");
  }
  SEXP shell = PROTECT(snapshot_ordinary_vector_payload(table, FALSE));
  own_builtin_metadata_attributes(
    shell,
    table,
    "Internal error: expected bounded data.table metadata"
  );
  if (!paradox_ordinary_vector_payload_equal(table, shell) ||
      !builtin_metadata_attributes_receipt_current(table, shell)) {
    UNPROTECT(2);
    Rf_error("data.table shell changed while being snapshotted");
  }
  const R_xlen_t entry_count = XLENGTH(shell);
  SEXP names = PROTECT(Rf_getAttrib(shell, R_NamesSymbol));
  if (TYPEOF(shell) != VECSXP || ALTREP(shell) || Rf_isS4(shell) ||
      !Rf_inherits(shell, "data.table") || XLENGTH(shell) != entry_count ||
      TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
      Rf_isObject(names) || !paradox_api_has_no_attributes(names) ||
      XLENGTH(names) != entry_count) {
    UNPROTECT(3);
    Rf_error("Internal error: expected ordinary data.table names");
  }
  /* An input facade may carry a copied or names-stale self-reference. Remove
   * it before installing the independently owned result reference. */
  Rf_setAttrib(shell, selfref_symbol, R_NilValue);
  own_all_data_table_columns(shell);
  SEXP result = PROTECT(paradox_prepare_data_table(shell, TRUE));
  SEXP result_names = PROTECT(Rf_getAttrib(result, R_NamesSymbol));
  Rf_setAttrib(result, R_NamesSymbol, R_NilValue);
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  UNPROTECT(5);
  return result;
}
