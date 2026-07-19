#ifndef PARADOX_R_UTILS_H
#define PARADOX_R_UTILS_H

#include <stddef.h>

#include "paradox.h"

typedef struct {
  SEXPTYPE type;
  const double *real_values;
  const int *integer_values;
} paradox_numeric_column_t;

typedef enum {
  PARADOX_UTF8_PIECE_ASCII = 1,
  PARADOX_UTF8_PIECE_CHARSXP = 2
} paradox_utf8_piece_kind_t;

typedef struct {
  paradox_utf8_piece_kind_t kind;
  const char *ascii;
  SEXP string;
} paradox_utf8_piece_t;

/* Copy the raw attribute pairlist to a non-vector holder, then return `symbol`
 * without dispatching the source ALTREP's Length method. Use only when an
 * already scoped public ALTREP snapshot must not be observed a second time. */
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
/* Numeric views reject ALTREP.  Their owner must remain independently rooted
 * for the complete lifetime of either retained read-only pointer. */
attribute_hidden paradox_numeric_column_t paradox_get_numeric_column(
  SEXP column,
  R_xlen_t size,
  const char *corrupt_context,
  const char *column_name
);
attribute_hidden double paradox_numeric_at(
  const paradox_numeric_column_t *column,
  R_xlen_t index
);
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

/* Rf_error() consumes text in the current locale.  Translate and own the
 * bytes before entering its allocating formatter. */
attribute_hidden NORET void paradox_error_from_scalar_string(SEXP message);

/* Return an independently owned, ordinary copy of an atomic or list vector.
 * Only the names attribute is semantic at this boundary; it is itself copied
 * to ordinary storage. List elements and other scalar leaves retain identity.
 * The result is unprotected and must be rooted by the caller before any
 * allocating operation. */
attribute_hidden SEXP paradox_snapshot_semantic_vector(SEXP value);

attribute_hidden SEXP paradox_prepare_data_table(SEXP table, int growable);

#endif
