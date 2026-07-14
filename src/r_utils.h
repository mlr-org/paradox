#ifndef PARADOX_R_UTILS_H
#define PARADOX_R_UTILS_H

#include <stddef.h>

#include "paradox.h"

typedef struct {
  SEXPTYPE type;
  const double *real_values;
  const int *integer_values;
} paradox_numeric_column_t;

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
attribute_hidden SEXP paradox_prepare_data_table(SEXP table, int growable);

#endif
