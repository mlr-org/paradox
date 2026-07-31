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

/* checkmate's convention, which every `$check()` fragment follows: the
 * fragment carries no sentence terminator and the `assert_*` wrapper supplies
 * one.  A fragment that legitimately ends a sentence itself -- the suggestion
 * clause "Did you mean 'x'?" -- must not be given a second terminator.  Every
 * continuation byte of a multibyte UTF-8 or Latin-1 character is >= 0x80, so
 * inspecting the last raw byte cannot misfire on a translated string. */
static inline int paradox_charsxp_ends_sentence(SEXP value) {
  const int size = LENGTH(value);
  if (size == 0) return FALSE;
  const char last = CHAR(value)[size - 1];
  return last == '.' || last == '?' || last == '!';
}

/* The documented checkmate `type = "strict"` grammar,
 * ^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$, that every interpreted parameter ID must
 * satisfy.  A ParamSet parameter name and a prepended ParamSetCollection set
 * name both own the leading position of an ID and admit through this one
 * predicate, so a collection cannot assemble an ID its own `$search_space()`
 * or `ParamSet$new()` would then reject. */
attribute_hidden int paradox_string_is_strict_id(SEXP value);

/* The continuation grammar of a strict ID, `[a-zA-Z0-9._]+`: everything a
 * strict ID may contain after its first letter.  A `ParamSetCollection` set
 * name that is appended rather than prepended lands in exactly that position,
 * so it is the rule such a name has to satisfy. */
attribute_hidden int paradox_string_is_strict_id_tail(SEXP value);

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

/* Named-column lookup rejects S4/ALTREP list shells and requires ordinary,
 * attribute-free names. It returns an exact child without protecting it. The
 * caller must install that child in a protected root before any operation
 * that can allocate or re-enter R. */

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
/* Canonical table columns must use ordinary unclassed, attribute-free
 * representations. Validation checks TYPEOF before structural admission and
 * rejects ALTREP/S4/object/attributes before observing XLENGTH. */
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

/* Equal infinities are the two admitted empty-at-infinity intervals: no
 * finite integer can equal either endpoint.  Every other interval with a
 * non-finite endpoint is genuinely unbounded.  Spell this once inline because
 * both Domain and ParamSet property readers are hot and must not drift. */
static inline double paradox_integer_domain_nlevels(
    double lower, double upper) {
  if (R_FINITE(lower) && R_FINITE(upper)) {
    return upper - lower + 1.0;
  }
  return lower == R_PosInf || upper == R_NegInf ? 0.0 : R_PosInf;
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

/* Copy one CHARSXP's current UTF-8 translation into R_alloc() storage.
 * Translation buffers themselves are transient and may be invalidated by the
 * destination allocation.  This helper therefore measures, allocates,
 * retranslates, verifies the byte count, and only then copies. */
attribute_hidden char *paradox_temporary_utf8_copy(
  SEXP string,
  size_t *size
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
 * invalid UTF-8 translation. The caller must root `string` for the whole
 * call: escaping holds a raw byte pointer into it across an R_alloc that can
 * collect. Escaping also allocates in the caller's R_alloc frame, so take any
 * `vmaxget()` watermark that is meant to release it before calling. */
attribute_hidden SEXP paradox_diagnostic_charsxp(SEXP string);

/* Build `callback(<value>)` for a documented one-argument R callback that
 * receives a semantic parameter value. Almost every R value is
 * self-evaluating and is spliced in directly; a symbol, call, promise, or
 * byte-code object is not, and is wrapped so that Rf_eval() hands the object
 * itself to the callback instead of evaluating it. The result is unprotected
 * and must be rooted by the caller. */
attribute_hidden SEXP paradox_unary_callback_call(SEXP callback, SEXP value);

/* Rf_error() consumes text in the current locale.  Translate and own the
 * bytes before entering its allocating formatter. */
NORET attribute_hidden void paradox_error_from_scalar_string(SEXP message);

/* Raise the checkmate-style assertion wrapper used by public assert methods
 * and checked value assignment. `diagnostic` must be one non-missing string
 * and is treated as already formatted semantic text. */
NORET attribute_hidden void paradox_assertion_error(
  const char *variable,
  SEXP diagnostic
);

/* Return an independently owned, ordinary copy of a non-S4 atomic or list
 * vector. Only the names attribute is semantic at this boundary; it is itself
 * copied to ordinary storage. List elements and other scalar leaves retain
 * identity. A structural receipt rejects object/S4/attribute-count changes
 * during destination allocation or ALTREP observation; together with the
 * exact names receipt, this keeps an ordinary names-only caller admission
 * from being laundered into the plain result. The result is unprotected and
 * must be rooted by the caller before any allocating operation. */
attribute_hidden SEXP paradox_snapshot_semantic_vector(SEXP value);
/* Snapshot a built-in typed value leaf with one coherent payload/attribute
 * generation. S4 and non-atomic opaque leaves retain exact identity. */
attribute_hidden SEXP paradox_snapshot_builtin_value_leaf(SEXP value);
/* Test-only post-preflight seam for the bounded metadata copier. */
attribute_hidden SEXP paradox_test_builtin_metadata_copy_reentry(
  SEXP value,
  SEXP hook
);
/*
 * Allocation-free terminal receipt for an ordinary built-in atomic leaf
 * returned by `paradox_snapshot_builtin_value_leaf()`.  The snapshot owns the
 * complete finite ordinary attribute graph; this comparison verifies that the
 * caller-owned source still has exactly that payload and metadata generation.
 * Exotic ALTREP/S4, cyclic, or overdeep metadata is outside this boundary and
 * returns false rather than recursing without a bound.
 */
attribute_hidden int paradox_builtin_value_leaf_receipt_current(
  SEXP source,
  SEXP snapshot
);
/* Allocation- and callback-free terminal metadata receipt for a stable ALTREP
 * atomic leaf returned by `paradox_snapshot_builtin_value_leaf()`. The caller
 * must finish its one admitted Length observation before entering this
 * receipt; semantic payload stability is the documented ALTREP contract, while
 * this function proves that the complete bounded attribute generation did not
 * move around that observation. */
attribute_hidden int paradox_altrep_builtin_value_leaf_metadata_is_current(
  SEXP source,
  SEXP snapshot
);
/* Allocation-free exact payload comparison for already ordinary vectors.
 * Attributes are deliberately excluded. */
attribute_hidden int paradox_ordinary_vector_payload_equal(
  SEXP left,
  SEXP right
);

typedef enum {
  PARADOX_SHALLOW_ATTRIBUTES_ALL = 0,
  PARADOX_SHALLOW_ATTRIBUTES_LOGICAL_STRUCTURE = 1
} paradox_shallow_attribute_policy_t;

/* Allocation-free bounded top-level metadata query. Every tag/value cell
 * delivered by the runtime mapper must be ordinary and unique; malformed
 * tags/values and cyclic or overlong pairlist spines return false. */
attribute_hidden int paradox_bounded_metadata_has_tag(
  SEXP value,
  SEXP tag,
  int *found
);

/*
 * Copy one caller-owned top-level attribute generation onto a fresh,
 * attribute-free destination without invoking R's recursive pairlist
 * duplicator.  The attr-free path is allocation-free.  Otherwise one bounded
 * tag/value capture roots the selected shallow metadata identities across
 * public-setter installation, and terminal source/destination receipts reject
 * mutation or normalization.  LOGICAL_STRUCTURE retains names only without
 * dimensions, plus dim/dimnames, and requires an unclassed non-S4 source; ALL
 * retains every attribute.  Nested metadata values deliberately retain exact
 * identity and are not traversed.
 */
attribute_hidden void paradox_copy_bounded_shallow_attributes(
  SEXP destination,
  SEXP source,
  paradox_shallow_attribute_policy_t policy,
  const char *failure_message
);

/* Complete ownership of a freshly owned built-in `special_vals` list.
 * Typed atomic leaves are detached; typed S4/non-atomic leaves and every
 * ParamUty leaf retain identity. The input list shell itself must already be
 * private to the caller. */
attribute_hidden SEXP paradox_own_builtin_special_value_leaves(
  SEXP special_values,
  int typed
);

/* Capture an ordinary list's exact name/element pairing into already
 * allocated carriers.  The scan is allocation-free and callback-free: once
 * it succeeds, a later row-name ALTREP observation or pending finalizer may
 * mutate the caller's shell without pairing names from one generation with
 * leaves from another. `stable_names` is either an ordinary STRSXP of the
 * same length or NULL when the source is required to be unnamed.
 * `stable_values` is an ordinary VECSXP of the same length. */
attribute_hidden int paradox_capture_list_identities(
  SEXP source,
  SEXP stable_names,
  SEXP stable_values
);

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

/* Count-only validation over a row.names carrier already captured from one
 * exact metadata generation. Stable ALTREP pays one Length observation and
 * no element observation; `row_count` is a required destination and the
 * caller owns any required post-callback generation recapture. */
attribute_hidden int paradox_public_row_names_count(
  SEXP row_names,
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
