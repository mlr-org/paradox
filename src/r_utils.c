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
  const paradox_public_table_kind_t kind = paradox_public_table_kind(table);
  if (kind == PARADOX_PUBLIC_TABLE_NONE) return table;

  /* Select and own interpreted metadata, including a canonical class suffix,
   * before any callback-capable Length or Elt observation. Ordinary admitted
   * shells are not copied merely to remove inert leading labels: every caller
   * remains in C and its semantic snapshot ignores them.
   * SHALLOW_DUPLICATE_ATTRIB is insufficient for ALTREP: an Elt callback can
   * mutate a shared names vector in place. */
  PROTECT(table);
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
    UNPROTECT(3);
    return table;
  }
  const R_xlen_t name_count = names_absent ? 0 : XLENGTH(source_names);
  SEXP stable_names = PROTECT(names_absent
    ? Rf_allocVector(STRSXP, 0)
    : Rf_duplicate(source_names));
  SEXP stable_classes = PROTECT(canonical_public_table_class(kind));

  R_xlen_t rows = 0;
  if (!public_row_names_count(source_row_names, &rows) || rows > INT_MAX) {
    UNPROTECT(5);
    return table;
  }

  /* Base's attribute-only duplicate wrapper is the common motivating case,
   * but every admitted shell gets one top-level Length followed by one Elt per
   * column. Metadata values above already belong to the selected generation. */
  const R_xlen_t count = XLENGTH(table);
  if (name_count != count) {
    UNPROTECT(5);
    return table;
  }
  SEXP result = PROTECT(Rf_allocVector(VECSXP, count));
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
  for (R_xlen_t column = 0; column < count; ++column) {
    if (column != 0 &&
        column % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP value = PROTECT(VECTOR_ELT(table, column));
    SET_VECTOR_ELT(result, column, value);
    UNPROTECT(1);
  }
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
  if (TYPEOF(table) != VECSXP) {
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
  if (TYPEOF(names) != STRSXP) {
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
      "Corrupt %s: `%s` must have type `%s` and length %lld",
      corrupt_context,
      column_name,
      Rf_type2char(type),
      (long long) size
    );
  }
  /* Canonical table columns are always ordinary vectors.  Reject an ALTREP
   * shell before asking its Length method anything: a hostile or merely
   * stateful implementation may call back into R or report a different size
   * on the second observation. */
  if (ALTREP(column)) {
    Rf_error(
      "Corrupt %s: `%s` must use an ordinary %s representation",
      corrupt_context,
      column_name,
      Rf_type2char(type)
    );
  }
  if (XLENGTH(column) != size) {
    Rf_error(
      "Corrupt %s: `%s` must have type `%s` and length %lld",
      corrupt_context,
      column_name,
      Rf_type2char(type),
      (long long) size
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

paradox_numeric_column_t paradox_get_numeric_column(SEXP column,
    R_xlen_t size, const char *corrupt_context, const char *column_name) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(column);
  if (type != REALSXP && type != INTSXP) {
    Rf_error(
      "Corrupt %s: `%s` must be numeric and have length %lld",
      corrupt_context,
      column_name,
      (long long) size
    );
  }
  if (ALTREP(column)) {
    Rf_error(
      "Corrupt %s: `%s` must use an ordinary numeric representation",
      corrupt_context,
      column_name
    );
  }
  if (XLENGTH(column) != size) {
    Rf_error(
      "Corrupt %s: `%s` must be numeric and have length %lld",
      corrupt_context,
      column_name,
      (long long) size
    );
  }

  paradox_numeric_column_t result = {
    type,
    type == REALSXP ? REAL_RO(column) : NULL,
    type == INTSXP ? INTEGER_RO(column) : NULL
  };
  return result;
}

double paradox_numeric_at(const paradox_numeric_column_t *column,
    R_xlen_t index) {
  if (column->type == REALSXP) {
    return column->real_values[index];
  }

  const int value = column->integer_values[index];
  return value == NA_INTEGER ? NA_REAL : (double) value;
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
        "Assertion on '%s' failed: Contains missing values (element %lld).",
        argument_name,
        (long long) (index + 1)
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
  if (Rf_getCharCE(string) != CE_BYTES) return string;

  static const char hex[] = "0123456789abcdef";
  const unsigned char *source = (const unsigned char *) CHAR(string);
  const size_t source_size = strlen((const char *) source);
  if (source_size > (size_t) INT_MAX / 4U) {
    Rf_error("Diagnostic string fragment exceeds R's string limit");
  }
  char *output = paradox_temporary_alloc(
    (R_xlen_t) (source_size * 4U) + 1,
    sizeof(*output)
  );
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
  return Rf_mkCharLenCE(output, (int) output_size, CE_UTF8);
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
  memcpy(owned, Rf_translateChar(string), size + 1U);
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
    paradox_utf8_ascii_piece(".")
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

  const R_xlen_t size = XLENGTH(value);
  SEXP result = PROTECT(Rf_allocVector(type, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    if (index != 0 && index % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
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
      UNPROTECT(1);
      Rf_error("Internal error: unsupported semantic vector type");
    }
  }

  SEXP names = PROTECT(Rf_getAttrib(value, R_NamesSymbol));
  if (names != R_NilValue) {
    if (TYPEOF(names) != STRSXP || XLENGTH(names) != size) {
      UNPROTECT(2);
      Rf_error("Invalid names on semantic vector");
    }
    SEXP stable_names = PROTECT(Rf_allocVector(STRSXP, size));
    for (R_xlen_t index = 0; index < size; ++index) {
      if (index != 0 && index % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
        R_CheckUserInterrupt();
      }
      SET_STRING_ELT(stable_names, index, STRING_ELT(names, index));
    }
    Rf_setAttrib(result, R_NamesSymbol, stable_names);
    UNPROTECT(1);
  }

  UNPROTECT(2);
  return result;
}

static void own_named_data_table_columns(SEXP table) {
  PROTECT(table);
  for (R_xlen_t column = 0; column < XLENGTH(table); ++column) {
    SEXP value = VECTOR_ELT(table, column);
    if (Rf_getAttrib(value, R_NamesSymbol) != R_NilValue) {
      SEXP owned = PROTECT(Rf_shallow_duplicate(value));
      if (owned == value) {
        Rf_error(
          "Internal error: cannot own named data.table column %lld",
          (long long) (column + 1)
        );
      }
      Rf_setAttrib(owned, R_NamesSymbol, R_NilValue);
      SET_VECTOR_ELT(table, column, owned);
      UNPROTECT(1);
    }
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
  if (TYPEOF(table) != VECSXP || ALTREP(table) ||
      !Rf_inherits(table, "data.table")) {
    Rf_error("Internal error: expected an ordinary data.table shell");
  }
  SEXP names = PROTECT(Rf_getAttrib(table, R_NamesSymbol));
  if (TYPEOF(names) != STRSXP || ALTREP(names) ||
      XLENGTH(names) != XLENGTH(table)) {
    Rf_error("Internal error: expected ordinary data.table names");
  }

  /* This is a registered entry point, so the input can have arbitrary aliases.
   * Own the outer vector and attribute pairlist before normalizing either one.
   * A shallow duplicate deliberately preserves the column vectors; duplicating
   * names separately prevents later by-reference renaming of the result from
   * reaching the input table through a shared STRSXP. */
  SEXP shell = PROTECT(Rf_shallow_duplicate(table));
  SEXP shell_names = PROTECT(Rf_shallow_duplicate(names));
  SEXP shell_index = PROTECT(Rf_shallow_duplicate(
    Rf_getAttrib(table, Rf_install("index"))
  ));
  SEXP shell_sorted = PROTECT(Rf_duplicate(
    Rf_getAttrib(table, Rf_install("sorted"))
  ));
  Rf_setAttrib(shell, R_NamesSymbol, shell_names);
  Rf_setAttrib(shell, Rf_install("index"), shell_index);
  Rf_setAttrib(shell, Rf_install("sorted"), shell_sorted);
  /* An input facade may carry a copied or names-stale self-reference. Remove
   * it before installing the independently owned result reference. */
  Rf_setAttrib(shell, Rf_install(".internal.selfref"), R_NilValue);
  SEXP result = PROTECT(paradox_prepare_data_table(shell, TRUE));
  SEXP result_names = PROTECT(Rf_getAttrib(result, R_NamesSymbol));
  Rf_setAttrib(result, R_NamesSymbol, R_NilValue);
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  UNPROTECT(7);
  return result;
}
