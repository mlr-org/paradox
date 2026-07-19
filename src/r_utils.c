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
  SEXP holder = PROTECT(R_MakeExternalPtr(
    NULL,
    R_NilValue,
    R_NilValue
  ));
  SHALLOW_DUPLICATE_ATTRIB(holder, object);
  SEXP result = PROTECT(Rf_getAttrib(holder, symbol));
  UNPROTECT(2);
  return result;
}

static int public_table_class_name(SEXP value, const char *expected) {
  return value != NA_STRING && Rf_getCharCE(value) != CE_BYTES &&
    strcmp(CHAR(value), expected) == 0;
}

static int recognized_public_table_shell(SEXP table) {
  SEXP classes = PROTECT(Rf_getAttrib(table, R_ClassSymbol));
  const int ordinary_classes = TYPEOF(classes) == STRSXP &&
    !ALTREP(classes) && !Rf_isS4(classes) && !Rf_isObject(classes) &&
    paradox_api_has_no_attributes(classes);
  const R_xlen_t count = ordinary_classes ? XLENGTH(classes) : 0;
  const int data_frame = count == 1 && public_table_class_name(
    STRING_ELT(classes, 0),
    "data.frame"
  );
  const int data_table = count == 2 && public_table_class_name(
      STRING_ELT(classes, 0),
      "data.table"
    ) && public_table_class_name(
      STRING_ELT(classes, 1),
      "data.frame"
    );
  static const char *const frame_attributes[] = {
    "names", "row.names", "class"
  };
  static const char *const table_attributes[] = {
    "names", "row.names", "class", ".internal.selfref", "sorted", "index"
  };
  const int recognized = (data_frame && paradox_api_has_only_attributes(
      table,
      frame_attributes,
      3
    )) || (data_table && paradox_api_has_only_attributes(
      table,
      table_attributes,
      6
    ));
  UNPROTECT(1);
  return recognized;
}

SEXP paradox_materialize_public_table_shell(SEXP table) {
  if (TYPEOF(table) != VECSXP || !ALTREP(table) || Rf_isS4(table) ||
      !recognized_public_table_shell(table)) {
    return table;
  }

  /* Base's attribute-only duplicate wrapper is the common motivating case,
   * but use only public ALTREP accessors and give every exact documented public
   * table shell the same one-observation boundary: one Length call followed by
   * one Elt call per column. Existing operation validators inspect the copied
   * attributes and columns afterward.
   */
  PROTECT(table);
  const R_xlen_t count = XLENGTH(table);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, count));
  /* Select metadata before an Elt accessor can reenter R. Attribute values are
   * retained exactly; the caller's existing validator decides whether every
   * structural value is ordinary and admissible. */
  SHALLOW_DUPLICATE_ATTRIB(result, table);
  for (R_xlen_t column = 0; column < count; ++column) {
    if (column != 0 &&
        column % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP value = PROTECT(VECTOR_ELT(table, column));
    SET_VECTOR_ELT(result, column, value);
    UNPROTECT(1);
  }
  UNPROTECT(2);
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
      SET_COMPLEX_ELT(result, index, COMPLEX_ELT(value, index));
      break;
    case STRSXP:
      SET_STRING_ELT(result, index, STRING_ELT(value, index));
      break;
    case RAWSXP:
      SET_RAW_ELT(result, index, RAW_ELT(value, index));
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
