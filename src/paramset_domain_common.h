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
