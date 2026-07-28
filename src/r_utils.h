#ifndef PARADOX_R_UTILS_H
#define PARADOX_R_UTILS_H

#include <stddef.h>

#include "paradox.h"

/* Ordinary CHARSXP predicate shared by structural-string admission: rejects
 * the missing string and bytes-encoded storage, the two representations no
 * interpreted identifier or name position supports. */
static inline int paradox_charsxp_is_ordinary(SEXP value) {
  return value != NA_STRING && Rf_getCharCE(value) != CE_BYTES;
}

typedef enum {
  PARADOX_UTF8_PIECE_ASCII = 1,
  PARADOX_UTF8_PIECE_CHARSXP = 2
} paradox_utf8_piece_kind_t;

typedef struct {
  paradox_utf8_piece_kind_t kind;
  const char *ascii;
  SEXP string;
} paradox_utf8_piece_t;

/* Return one raw stored attribute without R's special row-name expansion or
 * dispatching the source ALTREP's Length method. The caller must root the
 * returned value before a callback-capable observation. */
attribute_hidden SEXP paradox_stored_attribute(SEXP object, SEXP symbol);

/* Named-column lookup rejects ALTREP list/name shells and returns an exact
 * child without protecting it.  The caller must install that child in a
 * protected root before any operation that can allocate or re-enter R. */

attribute_hidden SEXP paradox_get_named_column(
  SEXP table,
  const char *storage_name,
  const char *column_name
);
attribute_hidden SEXP paradox_get_named_column_checked(
  SEXP table,
  const char *corrupt_context,
  const char *storage_name,
  const char *column_name
);
/* Canonical table columns must use ordinary representations.  Validation
 * checks TYPEOF before ALTREP and rejects ALTREP before observing XLENGTH. */
attribute_hidden void paradox_require_column(
  SEXP column,
  SEXPTYPE type,
  R_xlen_t size,
  const char *column_name
);
attribute_hidden void paradox_require_column_checked(
  SEXP column,
  SEXPTYPE type,
  R_xlen_t size,
  const char *corrupt_context,
  const char *column_name
);
/* Canonical numeric capsule columns are ordinary REALSXP/INTSXP vectors. */
attribute_hidden void paradox_require_numeric_column(
  SEXP column,
  R_xlen_t size,
  const char *corrupt_context,
  const char *column_name
);

/* NA-aware element read of an already validated numeric column. */
static inline double paradox_numeric_elt(SEXP column, R_xlen_t index) {
  if (TYPEOF(column) == REALSXP) {
    return REAL_ELT(column, index);
  }
  const int value = INTEGER_ELT(column, index);
  return value == NA_INTEGER ? NA_REAL : (double) value;
}
attribute_hidden double paradox_accepted_lower(
  double bound,
  double tolerance
);
attribute_hidden double paradox_accepted_upper(
  double bound,
  double tolerance
);
/* Test an integer-valued numeric with an inclusive user tolerance.  The
 * machine-epsilon allowance compensates only for subtraction error at the
 * mathematical boundary (for example, 100.4 - 100 at tolerance 0.4). */
attribute_hidden int paradox_within_integer_tolerance(
  double value,
  double rounded,
  double tolerance
);
/* The type-only form preserves lazy argument-validation order without
 * dispatching an ALTREP Length/Elt method. */
attribute_hidden void paradox_require_character_argument_type(
  SEXP value,
  const char *argument_name
);
attribute_hidden void paradox_require_character_argument(
  SEXP value,
  const char *argument_name
);
attribute_hidden void *paradox_temporary_alloc(
  R_xlen_t count,
  size_t element_size
);

/* Build one marked UTF-8 scalar from alternating package-owned ASCII text and
 * semantic CHARSXPs.  A CHARSXP passed to the piece constructor must remain
 * rooted until paradox_utf8_message() has entered and built its one root
 * vector; the message builder retains no translated pointer across an
 * allocation or interrupt check. */
attribute_hidden paradox_utf8_piece_t paradox_utf8_ascii_piece(
  const char *ascii
);
attribute_hidden paradox_utf8_piece_t paradox_utf8_charsxp_piece(SEXP string);
attribute_hidden SEXP paradox_utf8_message(
  const paradox_utf8_piece_t *pieces,
  R_xlen_t piece_count
);

/* Convert one semantic CHARSXP into a diagnostic-safe UTF-8 fragment. Native,
 * UTF-8, and Latin-1 strings retain their text; bytes-marked strings are
 * rendered as deterministic `\xHH` escapes instead of being passed through an
 * invalid UTF-8 translation. */
attribute_hidden SEXP paradox_diagnostic_charsxp(SEXP string);

/* Rf_error() consumes text in the current locale.  Translate and own the
 * bytes before entering its allocating formatter. */
attribute_hidden NORET void paradox_error_from_scalar_string(SEXP message);

/* Raise the checkmate-style assertion wrapper used by public assert methods
 * and checked value assignment. `diagnostic` must be one non-missing string
 * and is treated as already formatted semantic text. */
attribute_hidden NORET void paradox_assertion_error(
  const char *variable,
  SEXP diagnostic
);

/* Return an independently owned, ordinary copy of an atomic or list vector.
 * Only the names attribute is semantic at this boundary; it is itself copied
 * to ordinary storage. List elements and other scalar leaves retain identity.
 * The result is unprotected and must be rooted by the caller before any
 * allocating operation. */
attribute_hidden SEXP paradox_snapshot_semantic_vector(SEXP value);

typedef enum {
  PARADOX_PUBLIC_TABLE_NONE = 0,
  PARADOX_PUBLIC_DATA_FRAME,
  PARADOX_PUBLIC_DATA_TABLE
} paradox_public_table_kind_t;

/* Classify an allowed-attribute public data.frame/data.table. Its ordinary
 * class vector may have unique, non-reserved representation labels before the
 * canonical "data.frame" or c("data.table", "data.frame") suffix. The
 * top-level VECSXP may be ALTREP; interpreted metadata must satisfy the shared
 * public-table contract. */
attribute_hidden paradox_public_table_kind_t paradox_public_table_kind(
  SEXP table
);

/* Capture the public table's structural row count without interpreting row
 * labels. The raw row.names value must be integer/character, non-S4,
 * non-object, and attribute-free. Stable row-name ALTREP pays exactly one
 * Length observation and no element observations. */
attribute_hidden int paradox_public_table_row_count(
  SEXP table,
  R_xlen_t *row_count
);

/* Normalize an admitted ALTREP public data.frame/data.table shell once.
 * Interpreted names metadata and a canonical class suffix are owned before
 * callback-capable Length/Elt observations, row labels are normalized to their
 * captured count, ignored data.table cache attributes are dropped, and column
 * identities are retained. Ordinary shells are not copied merely to remove
 * leading representation labels; every native semantic snapshot ignores
 * them. Unrecognized inputs are returned unchanged so operation-specific
 * validators remain authoritative. The result is unprotected and must be
 * rooted immediately by the caller. */
attribute_hidden SEXP paradox_materialize_public_table_shell(SEXP table);

attribute_hidden SEXP paradox_prepare_data_table(SEXP table, int growable);
/* Finish a freshly allocated table whose canonical names, class, and row
 * names are already installed. Unlike the registered defensive finalizer,
 * this does not duplicate the shell or metadata: callers must own the fresh
 * shell and names exclusively. */
attribute_hidden SEXP paradox_prepare_fresh_data_table(SEXP table);

#endif
