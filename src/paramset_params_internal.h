#ifndef PARADOX_PARAMSET_PARAMS_INTERNAL_H
#define PARADOX_PARAMSET_PARAMS_INTERNAL_H

#include "paramset_domain_common.h"

/* Parsed, callback-free state retained for one native `$params` call.  Its
 * SEXPs are rooted by the owning private environment only until arbitrary R
 * evaluation begins; a caller that retains any of them across callbacks must
 * anchor those sources explicitly.  Temporary row maps are allocated with
 * R_alloc() and therefore live until the registered entry point returns. */
typedef struct {
  SEXP params_sexp;
  SEXP tags_sexp;
  SEXP trafos_sexp;
  SEXP dependencies_sexp;
  SEXP values_sexp;
  SEXP source_index;
  paradox_domain_params_t params;
  /* Permanent columns are an admitted snapshot, not later table lookups.
   * The rooted loader anchors every entry before any subsequent allocation. */
  SEXP params_columns[PARADOX_DOMAIN_TAGS];
  paradox_domain_tags_t tags;
  paradox_domain_trafos_t trafos;
  paradox_domain_dependencies_t dependencies;
  paradox_domain_values_t values;
  R_xlen_t *tag_offsets;
  R_xlen_t *tag_order;
  R_xlen_t *trafo_index;
} paradox_params_state_t;

attribute_hidden int paradox_params_exact_base_param_set(
  SEXP self,
  R_xlen_t *work_since_interrupt
);
attribute_hidden int paradox_params_canonical_active_member(
  SEXP self,
  SEXP private_environment,
  const char *member_name,
  const char *method_name,
  const char *argument_name,
  const char *super_method_name,
  R_xlen_t *work_since_interrupt
);
attribute_hidden int paradox_params_canonical_private_getter(
  SEXP self,
  SEXP private_environment
);
attribute_hidden int paradox_params_supported_table_attributes(
  SEXP table,
  int allow_sorted
);
attribute_hidden int paradox_params_names_are_only_attribute(SEXP value);
attribute_hidden int paradox_params_load_private_state(
  SEXP private_environment,
  paradox_params_state_t *state,
  R_xlen_t *work_since_interrupt
);
attribute_hidden int paradox_params_load_private_state_rooted(
  SEXP private_environment,
  paradox_params_state_t *state,
  SEXP roots,
  R_xlen_t roots_offset,
  R_xlen_t *work_since_interrupt
);
attribute_hidden SEXP paradox_params_build_static(
  const paradox_params_state_t *state,
  R_xlen_t *work_since_interrupt
);
attribute_hidden int paradox_params_finish_dynamic(
  SEXP result,
  const paradox_domain_params_t *params,
  SEXP dependencies,
  SEXP values,
  R_xlen_t *work_since_interrupt
);

#endif
