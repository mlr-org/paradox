#ifndef PARADOX_DOMAIN_ADMISSION_H
#define PARADOX_DOMAIN_ADMISSION_H

#include "paradox.h"

typedef enum {
  PARADOX_BUILTIN_DOMAIN_UNKNOWN = 0,
  PARADOX_BUILTIN_DOMAIN_DBL,
  PARADOX_BUILTIN_DOMAIN_INT,
  PARADOX_BUILTIN_DOMAIN_FCT,
  PARADOX_BUILTIN_DOMAIN_LGL,
  PARADOX_BUILTIN_DOMAIN_UTY
} paradox_builtin_domain_kind_t;

/* Test whether one admitted special-value list contains `value`.  Built-in
 * typed Domains retain legacy structural identity for ordinary leaves, but an
 * S4 leaf is special only by exact pointer identity.  ParamUty treats every
 * leaf as opaque and therefore retains structural identity for S4 values too.
 * Both inputs remain rooted across identity comparison and long scans poll for
 * interrupts through the operation-local work counter. */
attribute_hidden int paradox_builtin_special_values_contain(
  paradox_builtin_domain_kind_t kind,
  SEXP special_values,
  SEXP value,
  R_xlen_t *work_since_interrupt
);

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
  PARADOX_DOMAIN_FIELD_LEVELS_DUPLICATE
} paradox_domain_field_t;

/* Sole canonical semantic admission for one built-in Domain row. Opaque
 * default/init/special-value leaves retain identity; only their Paradox-owned
 * containers and marker state are interpreted. `id` may be NULL only while
 * the native constructor is building the row which Domain() names later. */
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
  paradox_builtin_domain_kind_t *kind,
  paradox_domain_field_t *failure,
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

/* Admit and own one exact built-in public Domain facade.  Opaque leaf objects
 * keep their identity, while every package-owned container (including
 * requirements and Condition RHS values) is detached. */
attribute_hidden SEXP paradox_snapshot_builtin_domain(
  SEXP domain,
  paradox_domain_field_t *failed_field
);

#endif
