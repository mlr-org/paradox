#ifndef PARADOX_DOMAIN_ADMISSION_H
#define PARADOX_DOMAIN_ADMISSION_H

#include "paradox.h"
#include "builtin_value.h"
#include "paramset_domain_common.h"

typedef enum {
  PARADOX_DOMAIN_FIELD_NONE = 0,
  PARADOX_DOMAIN_FIELD_ID,
  PARADOX_DOMAIN_FIELD_CLASS_STORAGE,
  PARADOX_DOMAIN_FIELD_GROUPING,
  PARADOX_DOMAIN_FIELD_CARGO,
  PARADOX_DOMAIN_FIELD_BOUNDS,
  PARADOX_DOMAIN_FIELD_LEVELS,
  PARADOX_DOMAIN_FIELD_SPECIAL_VALUES,
  PARADOX_DOMAIN_FIELD_DEFAULT,
  PARADOX_DOMAIN_FIELD_REQUIRED_DEFAULT,
  PARADOX_DOMAIN_FIELD_DEFAULT_VALUE,
  PARADOX_DOMAIN_FIELD_TAGS,
  PARADOX_DOMAIN_FIELD_TAGS_DUPLICATE,
  PARADOX_DOMAIN_FIELD_TRAFO,
  PARADOX_DOMAIN_FIELD_REQUIREMENTS,
  PARADOX_DOMAIN_FIELD_INIT,
  PARADOX_DOMAIN_FIELD_INIT_TRAFO,
  PARADOX_DOMAIN_FIELD_INIT_VALUE,
  PARADOX_DOMAIN_FIELD_LEVELS_DUPLICATE,
  /* Cargo failures are reported per documented constructor argument so the
   * shared owner can name `aggr`/`in_tune_fn`/`disable_in_tune` instead of
   * the internal `cargo` container. */
  PARADOX_DOMAIN_FIELD_CARGO_AGGR,
  PARADOX_DOMAIN_FIELD_CARGO_IN_TUNE_FN,
  PARADOX_DOMAIN_FIELD_CARGO_DISABLE_IN_TUNE,
  PARADOX_DOMAIN_FIELD_CARGO_TUNING_TAG,
  PARADOX_DOMAIN_FIELD_CARGO_TUNING_PAIR,
  PARADOX_DOMAIN_FIELD_CARGO_TUNING_AGGR
} paradox_domain_field_t;

/*
 * Operation-local proof that the exact privately owned special-values shell
 * has passed the kind-dependent structural admission.  It is deliberately a
 * stack receipt rather than persistent metadata: the owning constructor keeps
 * `special_values` rooted and does not expose its shell between preparation
 * and complete row admission.
 */
typedef struct {
  SEXP special_values;
  int typed;
} paradox_special_values_receipt_t;

/*
 * Perform the special-values part of canonical Domain-row admission before a
 * typed default/init leaf can be materialized.  For the four typed kinds this
 * rejects every ALTREP leaf without observing an element.  Unknown
 * class/storage pairs are left for the row owner's earlier class diagnostic.
 */
attribute_hidden int paradox_prepare_builtin_special_values(
  SEXP cls,
  SEXP storage,
  SEXP special_values,
  paradox_special_values_receipt_t *receipt,
  R_xlen_t *work_since_interrupt
);

/*
 * What a Domain operation declares it interprets. The identity spine -- `id`,
 * `cls`, `grouping`, `storage_type`, and the closed kind they derive -- is
 * always admitted and has no bit. Callers declare only the fields their
 * operation reads; `paradox_domain_interpretation_closure()` expands the
 * declaration to every field those fields' rules reference, so mask
 * correctness lives with the rule owner, not at each call site.
 */
#define PARADOX_DOMAIN_INTERPRET_NONE 0U
#define PARADOX_DOMAIN_INTERPRET_BOUNDS (1U << 0)
#define PARADOX_DOMAIN_INTERPRET_LEVELS (1U << 1)
#define PARADOX_DOMAIN_INTERPRET_SPECIAL_VALUES (1U << 2)
#define PARADOX_DOMAIN_INTERPRET_CARGO (1U << 3)
#define PARADOX_DOMAIN_INTERPRET_TAGS (1U << 4)
#define PARADOX_DOMAIN_INTERPRET_TRAFO (1U << 5)
#define PARADOX_DOMAIN_INTERPRET_ALL 0x3FU

/* The rule-dependency closure of one interpretation declaration, owned by the
 * same translation unit that owns the rules themselves. Idempotent. */
attribute_hidden unsigned int paradox_domain_interpretation_closure(
  unsigned int interpreted
);

/* Intern the kind and storage names the row owner matches on every row, so
 * canonical tables resolve them by CHARSXP identity. Called once at package
 * load. */
attribute_hidden void paradox_domain_admission_intern(void);

/* Canonical semantic admission of the schema half of one built-in Domain row:
 * identity, closed kind, grouping, tags, cargo, transformation, special
 * values, bounds, and levels. This is the complete rule set for the fields an
 * operation on an existing Domain interprets. `interpreted` selects the
 * declared fields (closure-expanded internally); the identity spine is always
 * admitted, and `PARADOX_DOMAIN_INTERPRET_ALL` is the complete row. A
 * constructor additionally owns the default/requirement/initialization rules
 * and therefore calls `paradox_admit_builtin_domain_row()`, which is this
 * function at `ALL` plus that remainder -- never a second implementation of
 * these rules. `id` may be `R_NilValue` only while the native constructor is
 * building the row which Domain() names later. `special_receipt` may be NULL
 * exactly when special values are outside the closure. */
attribute_hidden int paradox_admit_builtin_domain_schema_row(
  SEXP id,
  SEXP cls,
  SEXP grouping,
  SEXP cargo,
  SEXP lower,
  SEXP upper,
  SEXP tolerance,
  SEXP levels,
  SEXP special_values,
  SEXP storage,
  SEXP tags,
  SEXP trafo,
  const paradox_special_values_receipt_t *special_receipt,
  unsigned int interpreted,
  paradox_builtin_domain_kind_t *kind,
  /* Optional three-element output: the exact numeric bounds and tolerance
   * this admission accepted, so a caller never rereads the caller-owned
   * scalars after the hash tables above could have run a finalizer. Only
   * published when bounds are inside the closure. */
  double *admitted_bounds,
  paradox_domain_field_t *failure,
  R_xlen_t *work_since_interrupt
);

/* Sole canonical semantic admission for one built-in Domain row. Opaque
 * default/init/special-value leaves retain identity; only their Paradox-owned
 * containers and marker state are interpreted. `special_receipt` must have
 * been prepared from the exact privately owned `special_values` shell before
 * any typed default/init snapshot. `id` may be NULL only while the native
 * constructor is building the row which Domain() names later. */
attribute_hidden int paradox_admit_builtin_domain_row(
  SEXP id,
  SEXP cls,
  SEXP grouping,
  SEXP cargo,
  SEXP lower,
  SEXP upper,
  SEXP tolerance,
  SEXP levels,
  SEXP special_values,
  SEXP default_value,
  SEXP storage,
  SEXP tags,
  SEXP trafo,
  SEXP requirements,
  SEXP init_given,
  SEXP init_value,
  const paradox_special_values_receipt_t *special_receipt,
  paradox_builtin_domain_kind_t *kind,
  paradox_domain_field_t *failure,
  paradox_builtin_value_result_t *value_failure,
  R_xlen_t *work_since_interrupt
);

attribute_hidden const char *paradox_domain_field_name(
  paradox_domain_field_t field
);

/* Own the complete callback-free dependency metadata of one Domain.  The
 * returned list contains freshly built requirement/Condition containers and
 * materialized RHS vectors while retaining no alias to caller-owned semantic
 * shells.  R_UnboundValue denotes malformed input. */
attribute_hidden SEXP paradox_snapshot_builtin_requirements(
  SEXP requirements,
  R_xlen_t *work_since_interrupt
);

/* Own one interpreted nested Domain field.  This is the single structural
 * snapshot authority shared by Domain/ParamSet admission and detached public
 * projections.  Only CARGO, LEVELS, SPECIAL_VALS, and REQUIREMENTS are
 * interpreted; other columns are returned unchanged. */
attribute_hidden SEXP paradox_snapshot_domain_nested(
  SEXP source,
  enum paradox_domain_column column,
  R_xlen_t *work_since_interrupt
);

/* Admit and own one exact built-in public Domain facade.  Opaque leaf objects
 * keep their identity, while every package-owned container (including
 * requirements and Condition RHS values) is detached. */
attribute_hidden SEXP paradox_snapshot_builtin_domain(
  SEXP domain,
  paradox_domain_field_t *failed_field
);

/* Interpreted per-row fields retained by the public Domain-operation adapter
 * below, in the order the owner receives them. */
enum paradox_admitted_domain_row_field {
  PARADOX_ADMITTED_LEVELS = 0,
  PARADOX_ADMITTED_SPECIAL_VALS,
  PARADOX_ADMITTED_CARGO,
  PARADOX_ADMITTED_TAGS,
  PARADOX_ADMITTED_TRAFO,
  PARADOX_ADMITTED_ROW_STRIDE
};

typedef struct {
  /* Single rooted carrier for everything below; the caller protects it. */
  SEXP bundle;
  /* The exact selected columns, indexed by `enum paradox_domain_column`. */
  SEXP columns;
  /* `PARADOX_ADMITTED_ROW_STRIDE` interpreted fields per admitted row. */
  SEXP rows;
  /* Admitted numeric schema, already widened and validated by the owner. */
  const double *lower;
  const double *upper;
  const double *tolerance;
  R_xlen_t row_count;
} paradox_admitted_domain_table_t;

static inline SEXP paradox_admitted_domain_field(
    const paradox_admitted_domain_table_t *table, R_xlen_t row,
    enum paradox_admitted_domain_row_field field) {
  return VECTOR_ELT(
    table->rows,
    row * PARADOX_ADMITTED_ROW_STRIDE + field
  );
}

/* Route one public built-in Domain table through the canonical row owner.
 * The adapter validates the complete outward column container -- presence,
 * uniqueness, and shape of every interpreted-capable column stay structural
 * duties of the boundary -- and then admits every row's declared fields
 * through the owner at the closure of `interpreted`; it restates no semantic
 * rule. `domain_check` passes `PARADOX_DOMAIN_INTERPRET_ALL`; the other
 * kernels declare what they read, so a corrupt field outside an operation's
 * closure is diagnosed by the first operation that interprets it (`check`
 * always does) rather than by every operation. The returned bundle must be
 * protected by the caller, which then reads the admitted columns, rows, and
 * numeric schema instead of reselecting them from the live table; row fields
 * and numeric pointers outside the closure are absent (`R_NilValue`/NULL)
 * and must not be read. */
attribute_hidden SEXP paradox_admit_public_domain_table(
  SEXP domain,
  paradox_builtin_domain_kind_t kind,
  R_xlen_t row_count,
  unsigned int interpreted,
  paradox_admitted_domain_table_t *table,
  R_xlen_t *work_since_interrupt
);

#endif
