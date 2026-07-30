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

static int ordinary_ignored_data_table_metadata(SEXP table) {
  SEXP self_reference = PROTECT(paradox_api_raw_attribute(
    table,
    Rf_install(".internal.selfref")
  ));
  SEXP sorted = PROTECT(paradox_api_raw_attribute(
    table,
    Rf_install("sorted")
  ));
  SEXP index = PROTECT(paradox_api_raw_attribute(
    table,
    Rf_install("index")
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
  static const char *const frame_attributes[] = {
    "names", "row.names", "class"
  };
  static const char *const table_attributes[] = {
    "names", "row.names", "class", ".internal.selfref", "sorted", "index"
  };
  const int recognized_frame = ordinary_names &&
    kind == PARADOX_PUBLIC_DATA_FRAME &&
    paradox_api_has_only_attributes(
      table,
      frame_attributes,
      3
    );
  const int recognized_table = ordinary_names &&
    kind == PARADOX_PUBLIC_DATA_TABLE &&
    paradox_api_has_only_attributes(table, table_attributes, 6) &&
    ordinary_ignored_data_table_metadata(table);
  UNPROTECT(2);
  return recognized_table
    ? PARADOX_PUBLIC_DATA_TABLE
    : recognized_frame
      ? PARADOX_PUBLIC_DATA_FRAME
      : PARADOX_PUBLIC_TABLE_NONE;
}

static int public_row_names_count(SEXP row_names, R_xlen_t *row_count) {
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
  SEXP row_names = PROTECT(paradox_api_raw_attribute(
    table,
    R_RowNamesSymbol
  ));
  const int valid = public_row_names_count(row_names, row_count);
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

  SEXP source_names = paradox_api_raw_attribute(source, R_NamesSymbol);
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
  if (!public_row_names_count(row_names, &rows)) {
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
  if (!public_row_names_count(source_row_names, &rows) || rows > INT_MAX) {
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

  SEXP classes = PROTECT(Rf_getAttrib(value, R_ClassSymbol));
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

  SEXP names = PROTECT(Rf_getAttrib(table, R_NamesSymbol));
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
  const R_xlen_t source_attribute_count =
    paradox_api_stored_attribute_count(value);
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
  const int has_names =
    paradox_api_raw_attribute(value, R_NamesSymbol) != R_NilValue;
  SEXP result = PROTECT(Rf_allocVector(type, size));
  int protect_count = 1;
  SEXP stable_names = R_NilValue;
  if (has_names) {
    stable_names = PROTECT(Rf_allocVector(STRSXP, size));
    ++protect_count;
  }
  SEXP source_names = PROTECT(paradox_api_raw_attribute(
    value,
    R_NamesSymbol
  ));
  ++protect_count;
  if (Rf_isS4(value) ||
      (Rf_isObject(value) != FALSE) != source_object ||
      paradox_api_stored_attribute_count(value) != source_attribute_count) {
    UNPROTECT(protect_count);
    Rf_error("Semantic vector structure changed while being snapshotted");
  }
  if ((source_names != R_NilValue) != has_names ||
      (has_names && (TYPEOF(source_names) != STRSXP ||
        ALTREP(source_names) || Rf_isS4(source_names) ||
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

  if (semantic_altrep && stable_names != R_NilValue) {
    for (R_xlen_t index = 0; index < size; ++index) {
      SET_STRING_ELT(
        stable_names,
        index,
        STRING_ELT(source_names, index)
      );
    }
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    if (index != 0 &&
        index % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    if (!semantic_altrep && stable_names != R_NilValue) {
      SET_STRING_ELT(
        stable_names,
        index,
        STRING_ELT(source_names, index)
      );
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
        paradox_api_stored_attribute_count(value) !=
          source_attribute_count)) {
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

static int ordinary_vector_payload_equal(SEXP left, SEXP right) {
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

SEXP paradox_snapshot_builtin_value_leaf(SEXP value) {
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
    /*
     * For an ordinary atomic vector, one shallow duplicate selects payload
     * and the complete arbitrary attribute pairlist after its sole
     * allocation. A pending finalizer therefore runs before that indivisible
     * copy rather than between separate payload and attribute snapshots.
     * Attribute values are mutable presentation carriers in their own right
     * (factor levels and nested dimnames/custom metadata are common examples),
     * so deep-own that selected pairlist without recursively touching any
     * separate opaque leaf.
     */
    SEXP result = PROTECT(Rf_shallow_duplicate(value));
    if (result == value || (SEXPTYPE) TYPEOF(result) != type ||
        ALTREP(result)) {
      UNPROTECT(2);
      Rf_error("Cannot own an ordinary built-in value leaf");
    }
    const R_xlen_t attribute_count =
      paradox_api_stored_attribute_count(result);
    if (attribute_count != 0) {
      SEXP attribute_tags = PROTECT(Rf_allocVector(
        VECSXP,
        attribute_count
      ));
      SEXP attribute_values = PROTECT(Rf_allocVector(
        VECSXP,
        attribute_count
      ));
      atomic_leaf_attribute_capture_t capture = {
        attribute_tags,
        attribute_values,
        attribute_count,
        0,
        TRUE
      };
      paradox_api_map_stored_attributes(
        result,
        capture_atomic_leaf_attribute,
        &capture
      );
      if (!capture.valid || capture.count != attribute_count) {
        UNPROTECT(4);
        Rf_error("Built-in value attributes changed before ownership");
      }
      DUPLICATE_ATTRIB(result, result);
      atomic_leaf_attribute_receipt_t receipt = {
        attribute_tags,
        attribute_values,
        attribute_count,
        0,
        TRUE
      };
      paradox_api_map_stored_attributes(
        value,
        compare_atomic_leaf_attribute,
        &receipt
      );
      if (!receipt.current || receipt.count != attribute_count ||
          !ordinary_vector_payload_equal(value, result)) {
        UNPROTECT(4);
        Rf_error("Built-in value changed while being snapshotted");
      }
      UNPROTECT(2);
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
  const R_xlen_t attribute_count =
    paradox_api_stored_attribute_count(value);
  SEXP attribute_tags = PROTECT(Rf_allocVector(VECSXP, attribute_count));
  SEXP attribute_values = PROTECT(Rf_allocVector(VECSXP, attribute_count));
  atomic_leaf_attribute_capture_t capture = {
    attribute_tags,
    attribute_values,
    attribute_count,
    0,
    TRUE
  };
  paradox_api_map_stored_attributes(
    value,
    capture_atomic_leaf_attribute,
    &capture
  );
  if (!capture.valid || capture.count != attribute_count) {
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
  paradox_api_map_stored_attributes(
    value,
    compare_atomic_leaf_attribute,
    &receipt
  );
  if (!receipt.current || receipt.count != attribute_count) {
    UNPROTECT(4);
    Rf_error("Built-in value attributes changed while being snapshotted");
  }

  if (attribute_count != 0) {
    /*
     * Select the exact receipted attribute pairlist into an ordinary carrier,
     * then deep-own all of its mutable metadata. This preserves arbitrary
     * classes/attributes without sharing their nested vectors with either the
     * caller or a capsule.
     */
    SEXP attribute_source = PROTECT(Rf_allocVector(VECSXP, 0));
    SHALLOW_DUPLICATE_ATTRIB(attribute_source, value);
    atomic_leaf_attribute_receipt_t selected_receipt = {
      attribute_tags,
      attribute_values,
      attribute_count,
      0,
      TRUE
    };
    paradox_api_map_stored_attributes(
      attribute_source,
      compare_atomic_leaf_attribute,
      &selected_receipt
    );
    if (!selected_receipt.current ||
        selected_receipt.count != attribute_count) {
      UNPROTECT(5);
      Rf_error("Built-in value attributes changed before ownership");
    }
    DUPLICATE_ATTRIB(result, attribute_source);
    UNPROTECT(1);
    atomic_leaf_attribute_receipt_t final_receipt = {
      attribute_tags,
      attribute_values,
      attribute_count,
      0,
      TRUE
    };
    paradox_api_map_stored_attributes(
      value,
      compare_atomic_leaf_attribute,
      &final_receipt
    );
    if (!final_receipt.current ||
        final_receipt.count != attribute_count) {
      UNPROTECT(4);
      Rf_error("Built-in value changed while being snapshotted");
    }
  }
  UNPROTECT(4);
  return result;
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

static void own_named_data_table_columns(SEXP table) {
  PROTECT(table);
  for (R_xlen_t column = 0; column < XLENGTH(table); ++column) {
    SEXP value = VECTOR_ELT(table, column);
    if (Rf_getAttrib(value, R_NamesSymbol) != R_NilValue) {
      /*
       * `table` normally owns `value`, but the shallow duplicate below may
       * run a pending finalizer that replaces this very column.  Root the
       * selected column before that sole allocation; canonical unnamed
       * columns still pay no protection-stack traffic.
       */
      PROTECT(value);
      SEXP owned = PROTECT(Rf_shallow_duplicate(value));
      if (owned == value) {
        Rf_error(
          "Internal error: cannot own named data.table column %.0f",
          (double) (column + 1)
        );
      }
      Rf_setAttrib(owned, R_NamesSymbol, R_NilValue);
      SET_VECTOR_ELT(table, column, owned);
      UNPROTECT(2);
    }
  }
  UNPROTECT(1);
}

/*
 * Own one public data.table column without recursively copying list-column
 * leaves. The payload and its arbitrary attribute metadata have deliberately
 * different ownership rules:
 *
 * - ordinary vector/list payloads need one shallow duplicate, so a VECSXP
 *   column keeps the exact opaque elements selected from the source;
 * - stable ALTREP payloads must be materialized through their typed Elt API
 *   because a legitimate class need not expose a Dataptr to
 *   Rf_shallow_duplicate();
 * - every attribute carrier is presentation metadata and must be
 *   independently owned. DUPLICATE_ATTRIB() deep-copies that pairlist without
 *   recursively touching VECSXP payload elements.
 *
 * For ALTREP, retain an exact tag/value receipt across materialization and
 * select the attribute pairlist into an ordinary carrier before duplicating
 * it. An Elt callback may therefore make the operation reject, but it cannot
 * pair one payload generation with a different top-level attribute
 * generation.
 */
static SEXP snapshot_public_data_table_column(SEXP value) {
  PROTECT(value);
  int protect_count = 1;
  SEXP owned;
  SEXP attribute_source = R_NilValue;
  SEXP attribute_tags = R_NilValue;
  SEXP attribute_values = R_NilValue;
  R_xlen_t attribute_count;
  int source_altrep = ALTREP(value);

  if (source_altrep) {
    attribute_count = paradox_api_stored_attribute_count(value);
    attribute_tags = PROTECT(Rf_allocVector(
      VECSXP,
      attribute_count
    ));
    ++protect_count;
    attribute_values = PROTECT(Rf_allocVector(
      VECSXP,
      attribute_count
    ));
    ++protect_count;
    atomic_leaf_attribute_capture_t capture = {
      attribute_tags,
      attribute_values,
      attribute_count,
      0,
      TRUE
    };
    paradox_api_map_stored_attributes(
      value,
      capture_atomic_leaf_attribute,
      &capture
    );
    if (!capture.valid || capture.count != attribute_count) {
      UNPROTECT(protect_count);
      Rf_error("data.table column attributes changed before snapshot");
    }

    owned = PROTECT(paradox_snapshot_semantic_vector(value));
    ++protect_count;
    if (attribute_count != 0) {
      attribute_source = PROTECT(Rf_allocVector(VECSXP, 0));
      ++protect_count;
      SHALLOW_DUPLICATE_ATTRIB(attribute_source, value);

      atomic_leaf_attribute_receipt_t receipt = {
        attribute_tags,
        attribute_values,
        attribute_count,
        0,
        TRUE
      };
      paradox_api_map_stored_attributes(
        attribute_source,
        compare_atomic_leaf_attribute,
        &receipt
      );
      if (!receipt.current || receipt.count != attribute_count) {
        UNPROTECT(protect_count);
        Rf_error(
          "data.table column attributes changed while being snapshotted"
        );
      }
    }
  } else {
    owned = PROTECT(Rf_shallow_duplicate(value));
    ++protect_count;
    if (owned == value || ALTREP(owned) ||
        TYPEOF(owned) != TYPEOF(value) ||
        XLENGTH(owned) != XLENGTH(value)) {
      UNPROTECT(protect_count);
      Rf_error("Cannot own an ordinary data.table column");
    }
    attribute_count = paradox_api_stored_attribute_count(owned);
    if (attribute_count != 0) {
      attribute_tags = PROTECT(Rf_allocVector(
        VECSXP,
        attribute_count
      ));
      ++protect_count;
      attribute_values = PROTECT(Rf_allocVector(
        VECSXP,
        attribute_count
      ));
      ++protect_count;
      atomic_leaf_attribute_capture_t capture = {
        attribute_tags,
        attribute_values,
        attribute_count,
        0,
        TRUE
      };
      paradox_api_map_stored_attributes(
        owned,
        capture_atomic_leaf_attribute,
        &capture
      );
      if (!capture.valid || capture.count != attribute_count) {
        UNPROTECT(protect_count);
        Rf_error("data.table column attributes changed before ownership");
      }
      attribute_source = PROTECT(Rf_allocVector(VECSXP, 0));
      ++protect_count;
      SHALLOW_DUPLICATE_ATTRIB(attribute_source, owned);
    }
  }

  if (attribute_count != 0) {
    /*
     * `attribute_source` owns its pairlist and retains the exact selected
     * attribute values. Deep duplication here owns nested mutable metadata
     * such as factor levels, dimnames, and user class metadata. It does not
     * inspect or duplicate the column payload's list elements.
     *
     * Both selecting that pairlist and duplicating it allocate.  A terminal
     * content comparison closes the finalizer window between payload and
     * metadata selection.  Canonical attribute-free columns avoid all of this
     * work and return immediately after the shallow payload copy.
     */
    DUPLICATE_ATTRIB(owned, attribute_source);
    atomic_leaf_attribute_receipt_t receipt = {
      attribute_tags,
      attribute_values,
      attribute_count,
      0,
      TRUE
    };
    paradox_api_map_stored_attributes(
      value,
      compare_atomic_leaf_attribute,
      &receipt
    );
    if (!receipt.current || receipt.count != attribute_count ||
        (!source_altrep &&
          !ordinary_vector_payload_equal(value, owned))) {
      UNPROTECT(protect_count);
      Rf_error("data.table column changed while being snapshotted");
    }
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
  if (TYPEOF(table) != VECSXP || ALTREP(table) || Rf_isS4(table) ||
      !Rf_inherits(table, "data.table")) {
    Rf_error("Internal error: expected an ordinary data.table shell");
  }
  SEXP index_symbol = Rf_install("index");
  SEXP sorted_symbol = Rf_install("sorted");
  SEXP selfref_symbol = Rf_install(".internal.selfref");

  /* This is a registered entry point, so the input can have arbitrary aliases.
   * Allocate the independent names destination before duplicating the shell.
   * A pending finalizer may mutate the source during either allocation. The
   * shallow duplicate then selects one post-allocation column/attribute
   * generation, and the immediately following callback-free loop copies that
   * exact shell's names before any further allocation. Selecting names before
   * the shell duplicate could pair pre-finalizer names with post-finalizer
   * columns; allocating their duplicate afterwards would leave the inverse
   * tear available.
   *
   * The shell duplicate deliberately selects one exact source generation.
   * Every selected column is then shallow-duplicated at this defensive
   * caller-owned boundary. List-column leaves retain identity, but a later
   * `set()`/`:=` cannot mutate either an atomic source column or a list-column
   * spine.
   */
  PROTECT(table);
  const R_xlen_t entry_count = XLENGTH(table);
  SEXP shell_names = PROTECT(Rf_allocVector(STRSXP, entry_count));
  SEXP shell = PROTECT(Rf_shallow_duplicate(table));
  SEXP names = PROTECT(Rf_getAttrib(shell, R_NamesSymbol));
  if (TYPEOF(shell) != VECSXP || ALTREP(shell) || Rf_isS4(shell) ||
      !Rf_inherits(shell, "data.table") || XLENGTH(shell) != entry_count ||
      TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
      Rf_isObject(names) || !paradox_api_has_no_attributes(names) ||
      XLENGTH(names) != entry_count) {
    UNPROTECT(4);
    Rf_error("Internal error: expected ordinary data.table names");
  }
  for (R_xlen_t column = 0; column < entry_count; ++column) {
    SET_STRING_ELT(shell_names, column, STRING_ELT(names, column));
  }

  /* Select the detached shell's cache carriers before their copies allocate.
   * Replacement of the caller's attribute pairlist can no longer switch the
   * chosen generation. These are cache metadata only; semantic column/name
   * ownership was already closed above. */
  SEXP source_index = PROTECT(Rf_getAttrib(
    shell,
    index_symbol
  ));
  SEXP source_sorted = PROTECT(Rf_getAttrib(
    shell,
    sorted_symbol
  ));
  Rf_setAttrib(shell, R_NamesSymbol, shell_names);
  SEXP shell_index = PROTECT(Rf_shallow_duplicate(
    source_index
  ));
  SEXP shell_sorted = PROTECT(Rf_duplicate(
    source_sorted
  ));
  Rf_setAttrib(shell, index_symbol, shell_index);
  Rf_setAttrib(shell, sorted_symbol, shell_sorted);
  /* An input facade may carry a copied or names-stale self-reference. Remove
   * it before installing the independently owned result reference. */
  Rf_setAttrib(shell, selfref_symbol, R_NilValue);
  own_all_data_table_columns(shell);
  SEXP result = PROTECT(paradox_prepare_data_table(shell, TRUE));
  SEXP result_names = PROTECT(Rf_getAttrib(result, R_NamesSymbol));
  Rf_setAttrib(result, R_NamesSymbol, R_NilValue);
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  UNPROTECT(10);
  return result;
}
