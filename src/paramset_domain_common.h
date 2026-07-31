#ifndef PARADOX_PARAMSET_DOMAIN_COMMON_H
#define PARADOX_PARAMSET_DOMAIN_COMMON_H

#include "paradox.h"

enum paradox_domain_column {
  PARADOX_DOMAIN_ID = 0,
  PARADOX_DOMAIN_CLS,
  PARADOX_DOMAIN_GROUPING,
  PARADOX_DOMAIN_CARGO,
  PARADOX_DOMAIN_LOWER,
  PARADOX_DOMAIN_UPPER,
  PARADOX_DOMAIN_TOLERANCE,
  PARADOX_DOMAIN_LEVELS,
  PARADOX_DOMAIN_SPECIAL_VALS,
  PARADOX_DOMAIN_DEFAULT,
  PARADOX_DOMAIN_STORAGE_TYPE,
  PARADOX_DOMAIN_TAGS,
  PARADOX_DOMAIN_TRAFO,
  PARADOX_DOMAIN_REQUIREMENTS,
  PARADOX_DOMAIN_INIT_GIVEN,
  PARADOX_DOMAIN_INIT,
  PARADOX_DOMAIN_COLUMN_COUNT
};

/* The canonical 16-column Domain-table schema. `.tags` and later columns are
 * projection-only; the leading columns through `storage_type` are the
 * permanent columns shared by every parameter-table representation, so the
 * permanent name list is this array's prefix. */
#define PARADOX_DOMAIN_PERMANENT_COLUMNS ((R_xlen_t) PARADOX_DOMAIN_TAGS)
attribute_hidden extern const char *const
  paradox_domain_column_names[PARADOX_DOMAIN_COLUMN_COUNT];
attribute_hidden extern const SEXPTYPE
  paradox_domain_column_types[PARADOX_DOMAIN_COLUMN_COUNT];

/* Intern the canonical column names once at package load, so column selection
 * can decide a canonical name with one pointer comparison. */
attribute_hidden void paradox_domain_intern_column_names(void);

/* One exact, allocation-free capture of the complete supported outward
 * Domain metadata generation.  The mapper admits only names, class,
 * row.names, .internal.selfref, and repr, rejects duplicates and malformed
 * cells, and hard-bounds the complete attribute spine at five cells.  Every
 * member is borrowed from `domain`; `metadata` is a required destination and
 * callers must root retained members before any allocation or callback-capable
 * observation. */
typedef struct {
  SEXP names;
  SEXP classes;
  SEXP row_names;
  SEXP selfref;
  SEXP repr;
  R_xlen_t count;
  int valid;
} paradox_domain_outer_metadata_t;

attribute_hidden int paradox_domain_capture_outer_metadata(
  SEXP domain,
  paradox_domain_outer_metadata_t *metadata
);

/* The package-load-interned non-global metadata tag used by the separate
 * exact empty-Domain validator. */
attribute_hidden SEXP paradox_domain_selfref_symbol(void);

/* Select every requested canonical column of one outward table in a single
 * allocation-free pass over its names. `required_mask` holds one bit per
 * `enum paradox_domain_column`; requested slots of `columns` receive the exact
 * selected column, unrequested slots `R_NilValue`. Container diagnostics,
 * missing-column and duplicate-column rejection are byte-for-byte those of
 * `paradox_get_named_column_checked()`, applied in ascending column order. */
attribute_hidden void paradox_domain_select_columns(
  SEXP table,
  const char *corrupt_context,
  const char *storage_name,
  unsigned int required_mask,
  SEXP *columns
);

/* The same canonical selector with an optional exact physical-position
 * receipt. Every position slot is initialized to R_XLEN_T_MAX; requested
 * columns receive their selected R_xlen_t index. This is structural
 * operation-local evidence, not a second name-matching implementation. */
attribute_hidden void paradox_domain_select_columns_with_positions(
  SEXP table,
  const char *corrupt_context,
  const char *storage_name,
  unsigned int required_mask,
  SEXP *columns,
  R_xlen_t *positions
);

/* Generation-local selector companion. `names` must be the carrier returned
 * by the same exact outer-metadata capture as `table`; this entry therefore
 * performs no second attribute traversal. */
attribute_hidden void paradox_domain_select_captured_columns_with_positions(
  SEXP table,
  SEXP names,
  const char *corrupt_context,
  const char *storage_name,
  unsigned int required_mask,
  SEXP *columns,
  R_xlen_t *positions
);

/*
 * Allocation-free terminal companion to the selector above. The initial
 * selector has already proved that `positions` is a bijection from the
 * sixteen canonical names to one exact sixteen-column ordinary table.
 * Rechecking each physical position, canonical spelling, captured column
 * identity, and exact row-count/type shell therefore proves the complete
 * pairing and rectangularity without another table loop.
 */
attribute_hidden int paradox_domain_selected_columns_current(
  SEXP table,
  const SEXP *columns,
  const R_xlen_t *positions,
  R_xlen_t row_count
);

/* Terminal receipt over the names carrier captured in the same terminal
 * generation. It is allocation-free and never authenticates another
 * generation or a cached caller decision. */
attribute_hidden int paradox_domain_captured_columns_current(
  SEXP table,
  SEXP names,
  const SEXP *columns,
  const R_xlen_t *positions,
  R_xlen_t row_count
);

/* Exact callback-free shell predicate shared by initial admission and the
 * fused terminal selector receipt. Numeric schema columns admit either
 * ordinary integer or real storage; every other column has its fixed type. */
attribute_hidden int paradox_domain_column_shell_is_exact(
  SEXP column,
  enum paradox_domain_column selected,
  R_xlen_t row_count
);

typedef struct {
  SEXP table;
  SEXP ids;
  SEXP classes;
  R_xlen_t row_count;
} paradox_domain_params_t;

typedef struct {
  SEXP ids;
  SEXP values;
  R_xlen_t row_count;
} paradox_domain_tags_t;

typedef struct {
  SEXP ids;
  SEXP values;
  R_xlen_t row_count;
} paradox_domain_trafos_t;

typedef struct {
  SEXP ids;
  SEXP on;
  SEXP conditions;
  R_xlen_t row_count;
} paradox_domain_dependencies_t;

typedef struct {
  SEXP values;
  SEXP names;
  R_xlen_t size;
} paradox_domain_values_t;

typedef struct {
  const paradox_domain_params_t *params;
  R_xlen_t parameter_row;
  const paradox_domain_tags_t *tags;
  const R_xlen_t *tag_rows;
  R_xlen_t tag_count;
  SEXP trafo;
  const paradox_domain_dependencies_t *dependencies;
  const R_xlen_t *dependency_rows;
  R_xlen_t dependency_count;
  int init_given;
  SEXP init_value;
} paradox_domain_row_t;

/* Fresh ordinary character vector from constant ASCII labels. */
attribute_hidden SEXP paradox_domain_character_vector(
  const char *const *values,
  R_xlen_t size
);
/* Install canonical plain-data.frame metadata (names, "data.frame" class,
 * compact row.names) on a fresh package-owned column shell and return it. */
attribute_hidden SEXP paradox_domain_finish_plain_table(
  SEXP table,
  const char *const *column_names,
  R_xlen_t column_count,
  R_xlen_t row_count
);
/* Allocate a fresh canonical plain data.frame with the given column types. */
attribute_hidden SEXP paradox_domain_new_plain_table(
  const char *const *column_names,
  const SEXPTYPE *column_types,
  R_xlen_t column_count,
  R_xlen_t row_count
);
attribute_hidden int paradox_domain_string_is(
  SEXP string,
  const char *expected
);
attribute_hidden int paradox_domain_strings_equal(SEXP left, SEXP right);
/* Linear identifier search over an already admitted character vector, with
 * the pointer-identity fast path first. Returns the first matching index or
 * R_XLEN_T_MAX when absent. */
attribute_hidden R_xlen_t paradox_domain_find_string(
  SEXP strings,
  SEXP sought,
  R_xlen_t *work_since_interrupt
);
/* Derived tags with a node's own `$tags<-` answer applied: every ID the
 * override governs takes its rows from the override, every other ID keeps the
 * rows derived from the sets. `override` is `NULL` or the exact two-field
 * `{ids, tags}` record validated by the capsule schema. */
attribute_hidden SEXP paradox_domain_apply_tag_override(
  SEXP derived,
  SEXP override,
  R_xlen_t *work_since_interrupt
);
attribute_hidden int paradox_domain_string_in(
  SEXP strings,
  SEXP sought,
  R_xlen_t *work_since_interrupt
);
attribute_hidden int paradox_domain_exact_string_vector(
  SEXP value,
  const char *const *expected,
  R_xlen_t size,
  R_xlen_t *work_since_interrupt
);
/* Package-owned capsule tables are canonical plain data.frames.  They carry
 * exactly names/class/row.names metadata, never data.table's mutable
 * self-reference, key, index, or spare-capacity state.  The compact integer
 * row.names representation used by R is admitted; table columns and names
 * remain ordinary vectors and are validated by the schema-specific caller. */
attribute_hidden int paradox_domain_exact_plain_table(
  SEXP table,
  const char *const *column_names,
  R_xlen_t column_count,
  R_xlen_t *row_count,
  R_xlen_t *work_since_interrupt
);
attribute_hidden SEXP paradox_domain_plain_table_snapshot(
  SEXP source,
  const char *const *column_names,
  R_xlen_t column_count
);
/* Exact private-state validators reject ALTREP table shells, structural
 * attributes, columns, and inspected nested vectors. Validators root every
 * borrowed child across their own allocation-capable checks. Returned child
 * SEXPs are still borrowed: callers must root each retained child immediately
 * on return, before allocating or invoking R. Every reported size is captured
 * once during validation. */
attribute_hidden SEXP paradox_domain_local_value(
  SEXP environment,
  const char *name
);
attribute_hidden int paradox_domain_owns_private_environment(
  SEXP self,
  SEXP private_environment
);
/* Required package-operation topology lookup. Unlike the absence-tolerant
 * candidate classifier below, a missing binding is corrupt and may fail at
 * the old-R binding API; active/delayed bindings are still never invoked. */
attribute_hidden SEXP paradox_domain_required_private_environment(SEXP self);
attribute_hidden SEXP paradox_domain_private_environment(SEXP self);
attribute_hidden int paradox_domain_validate_params(
  SEXP params,
  SEXP selected_id,
  int validate_all_rows,
  paradox_domain_params_t *result,
  R_xlen_t *selected_row,
  R_xlen_t *work_since_interrupt
);
attribute_hidden int paradox_domain_validate_tags(
  SEXP tags,
  paradox_domain_tags_t *result,
  R_xlen_t *work_since_interrupt
);
attribute_hidden int paradox_domain_validate_trafos(
  SEXP trafos,
  paradox_domain_trafos_t *result,
  R_xlen_t *work_since_interrupt
);
attribute_hidden int paradox_domain_validate_dependencies(
  SEXP dependencies,
  paradox_domain_dependencies_t *result,
  R_xlen_t *work_since_interrupt
);
/* Validate the same exact dependency representation while retaining each
 * admitted built-in Condition RHS in operation-local workspace.  The
 * dependencies object remains the owner/root of every returned SEXP. */
attribute_hidden int paradox_domain_validate_dependencies_with_rhs(
  SEXP dependencies,
  paradox_domain_dependencies_t *result,
  SEXP **condition_rhs,
  R_xlen_t *work_since_interrupt
);
attribute_hidden int paradox_domain_validate_values(
  SEXP values,
  paradox_domain_values_t *result,
  R_xlen_t *work_since_interrupt
);
/* Install the canonical one-row built-in Domain facade on a fresh, owned
 * 16-column shell. The class scalar must be an admitted built-in Param class. */
attribute_hidden SEXP paradox_domain_prepare_facade(
  SEXP domain,
  SEXP cls,
  R_xlen_t *work_since_interrupt
);
attribute_hidden SEXP paradox_domain_fill(
  SEXP domain,
  const paradox_domain_row_t *row,
  R_xlen_t *work_since_interrupt
);

#endif
