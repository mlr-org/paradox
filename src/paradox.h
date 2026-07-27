#ifndef PARADOX_H
#define PARADOX_H

/* Recent R can expose compiler extensions in its public headers: a fixed-base
 * enum on some builds and, since R 4.3, an anonymous struct in Rcomplex when
 * compiling package C as C99. Keep pedantic diagnostics enabled for our code
 * while isolating those declarations. */
#if defined(__clang__)
# pragma clang diagnostic push
# if defined(__has_warning)
#  if __has_warning("-Wfixed-enum-extension")
#   pragma clang diagnostic ignored "-Wfixed-enum-extension"
#  elif __has_warning("-Wc23-extensions")
#   pragma clang diagnostic ignored "-Wc23-extensions"
#  endif
#  if __has_warning("-Wc11-extensions")
#   pragma clang diagnostic ignored "-Wc11-extensions"
#  endif
# endif
#elif defined(__GNUC__) && __GNUC__ >= 5
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wpedantic"
#endif
#include <R.h>
#include <R_ext/Complex.h>
#include <Rversion.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#if defined(__clang__)
# pragma clang diagnostic pop
#elif defined(__GNUC__) && __GNUC__ >= 5
# pragma GCC diagnostic pop
#endif

#define PARADOX_INTERRUPT_CHECK_INTERVAL ((R_xlen_t) 65536)

attribute_hidden SEXP paradox_domain_construct(
  SEXP cls,
  SEXP grouping,
  SEXP cargo,
  SEXP lower,
  SEXP upper,
  SEXP tolerance,
  SEXP levels,
  SEXP special_vals,
  SEXP default_value,
  SEXP tags,
  SEXP trafo,
  SEXP storage_type,
  SEXP init_given,
  SEXP init_value,
  SEXP numeric_source_kind,
  SEXP numeric_logscale,
  SEXP id,
  SEXP requirements
);
attribute_hidden SEXP paradox_domain_uty_check_result(SEXP result);
attribute_hidden SEXP paradox_domain_simple_repr_id(SEXP representation);
attribute_hidden SEXP paradox_param_set_construct(SEXP domains);
attribute_hidden SEXP paradox_param_set_collection_construct(
  SEXP sets,
  SEXP tag_sets,
  SEXP tag_params,
  SEXP postfix_names
);
attribute_hidden SEXP paradox_param_set_collection_add(
  SEXP private_environment,
  SEXP self,
  SEXP child,
  SEXP name,
  SEXP tag_sets,
  SEXP tag_params
);
attribute_hidden SEXP paradox_param_set_collection_detach_plan(
  SEXP private_environment,
  SEXP self,
  SEXP requested
);
attribute_hidden SEXP paradox_param_set_collection_has_callback(
  SEXP private_environment,
  SEXP self,
  SEXP selector
);
attribute_hidden SEXP paradox_param_set_collection_extra_trafo(
  SEXP private_environment,
  SEXP self,
  SEXP x
);
attribute_hidden SEXP paradox_param_set_collection_constraint(
  SEXP private_environment,
  SEXP self,
  SEXP x
);
attribute_hidden SEXP paradox_param_set_collection_detached_extra_trafo(
  SEXP plan,
  SEXP x
);
attribute_hidden SEXP paradox_param_set_collection_detached_constraint(
  SEXP plan,
  SEXP x
);
attribute_hidden SEXP paradox_param_set_collection_owner_subset_state(
  SEXP callback,
  SEXP source,
  SEXP ids
);
attribute_hidden SEXP paradox_param_set_ids(
  SEXP params,
  SEXP tag_table,
  SEXP class_filter,
  SEXP all_tags,
  SEXP any_tags
);
attribute_hidden SEXP paradox_param_set_ids_lazy(
  SEXP private_environment,
  SEXP frame
);
attribute_hidden SEXP paradox_param_set_get_values(
  SEXP private_environment,
  SEXP self,
  SEXP frame
);
attribute_hidden SEXP paradox_param_set_values_merge(
  SEXP dots,
  SEXP values,
  SEXP current,
  SEXP insert
);
attribute_hidden SEXP paradox_param_set_store_values(
  SEXP private_environment,
  SEXP self,
  SEXP values
);
attribute_hidden SEXP paradox_param_set_assign_values_checked(
  SEXP private_environment,
  SEXP self,
  SEXP values
);
attribute_hidden SEXP paradox_param_set_set_tags(
  SEXP private_environment,
  SEXP self,
  SEXP tags
);
attribute_hidden SEXP paradox_param_set_get_tags(
  SEXP private_environment,
  SEXP self
);
attribute_hidden SEXP paradox_param_set_dependency_table_snapshot(
  SEXP dependencies
);
/* Complete a fresh, owned, canonical dependency table as a detached public
 * data.table facade. This is shared by BASE/SHADOW and COLLECTION reads. */
attribute_hidden SEXP paradox_dependency_public_facade(SEXP table);
attribute_hidden SEXP paradox_param_set_dependencies(
  SEXP private_environment,
  SEXP self
);
attribute_hidden SEXP paradox_param_set_has_dependencies(
  SEXP private_environment,
  SEXP self
);
attribute_hidden SEXP paradox_param_set_set_dependencies(
  SEXP private_environment,
  SEXP self,
  SEXP dependencies
);
attribute_hidden SEXP paradox_param_set_add_dependency(
  SEXP private_environment,
  SEXP self,
  SEXP id,
  SEXP on,
  SEXP condition,
  SEXP allow_dangling
);
attribute_hidden SEXP paradox_param_set_set_callback(
  SEXP private_environment,
  SEXP self,
  SEXP callback,
  SEXP selector
);
attribute_hidden SEXP paradox_param_set_filter_argument(
  SEXP frame,
  const char *argument_name
);
attribute_hidden SEXP paradox_param_set_property(SEXP params, SEXP property);
attribute_hidden SEXP paradox_domain_check_builtin(
  SEXP param,
  SEXP values,
  SEXP internal
);
attribute_hidden SEXP paradox_domain_property_builtin(
  SEXP param,
  SEXP property
);
attribute_hidden SEXP paradox_domain_qunif_builtin(SEXP param, SEXP x);
attribute_hidden SEXP paradox_domain_sanitize_builtin(SEXP param, SEXP values);
attribute_hidden double paradox_qunif_double_value(
  double unit,
  double lower,
  double upper
);
attribute_hidden int paradox_qunif_integer_value(
  double unit,
  double lower,
  double upper,
  int *result
);
attribute_hidden R_xlen_t paradox_qunif_level_index(
  double unit,
  R_xlen_t level_count
);
attribute_hidden SEXP paradox_param_set_shadow_constraint(
  SEXP plan,
  SEXP visible_values
);
attribute_hidden SEXP paradox_param_set_check_builtin(
  SEXP private_environment,
  SEXP self,
  SEXP values,
  SEXP check_strict,
  SEXP sanitize,
  SEXP presence,
  SEXP allow_token
);
attribute_hidden SEXP paradox_param_set_validate_current_graph(
  SEXP private_environment,
  SEXP self,
  SEXP selected_core
);
attribute_hidden SEXP paradox_param_set_validate_current_roots(SEXP selves);
/* Internal checked-assignment entry: the semantic result is identical to the
 * registered operation, while successful live ObjectTuneToken admissions are
 * returned as rooted shell/private/core generation receipts. */
attribute_hidden SEXP paradox_param_set_check_builtin_with_receipts(
  SEXP private_environment,
  SEXP self,
  SEXP values,
  SEXP check_strict,
  SEXP sanitize,
  SEXP presence,
  SEXP allow_token,
  int enforce_dependencies,
  SEXP *receipts_result
);
attribute_hidden void paradox_param_set_verify_token_receipts(SEXP receipts);
/* Final allocation/callback-free pointer scan used immediately before the
 * atomic capsule binding wave. */
attribute_hidden void paradox_param_set_scan_token_receipts(SEXP receipts);
attribute_hidden SEXP paradox_tune_token_snapshot_list(
  SEXP private_environment,
  SEXP self,
  SEXP values
);
attribute_hidden SEXP paradox_param_set_check_dependencies_builtin(
  SEXP private_environment,
  SEXP self,
  SEXP values
);
attribute_hidden SEXP paradox_param_set_test_constraint_builtin(
  SEXP private_environment,
  SEXP self,
  SEXP values,
  SEXP assert_value
);
attribute_hidden SEXP paradox_param_set_test_constraint_dt_builtin(
  SEXP private_environment,
  SEXP self,
  SEXP table,
  SEXP assert_value
);
attribute_hidden SEXP paradox_param_set_check_dt_builtin(
  SEXP private_environment,
  SEXP self,
  SEXP table,
  SEXP check_strict,
  SEXP presence,
  SEXP allow_token
);
attribute_hidden SEXP paradox_condition_test_builtin(SEXP condition, SEXP x);
attribute_hidden SEXP paradox_param_set_qunif_builtin(
  SEXP private_environment,
  SEXP self,
  SEXP x
);
attribute_hidden SEXP paradox_sampler_unif_sample_builtin(
  SEXP param_set,
  SEXP n
);
attribute_hidden SEXP paradox_sampler_unif_subspace_handoffs(
  SEXP param_set,
  SEXP requested_ids,
  SEXP extra_trafo
);
attribute_hidden SEXP paradox_sampler_unif_take_subspace(SEXP handoff);
attribute_hidden SEXP paradox_design_dependency_plan(
  SEXP data,
  SEXP param_set
);
attribute_hidden SEXP paradox_generate_design_grid_builtin(
  SEXP private_environment,
  SEXP self,
  SEXP resolutions,
  SEXP upper_limit
);
attribute_hidden SEXP paradox_param_set_trafo(
  SEXP private_environment,
  SEXP self,
  SEXP x,
  SEXP param_set_argument
);
attribute_hidden SEXP paradox_param_set_get_domain(
  SEXP private_environment,
  SEXP self,
  SEXP id
);
attribute_hidden SEXP paradox_param_set_domains(
  SEXP private_environment,
  SEXP self
);
attribute_hidden SEXP paradox_param_set_domains_select(
  SEXP private_environment,
  SEXP self,
  SEXP id
);
attribute_hidden SEXP paradox_param_set_params(
  SEXP private_environment,
  SEXP self
);
attribute_hidden SEXP paradox_param_set_collection_params(
  SEXP private_environment,
  SEXP self
);
attribute_hidden SEXP paradox_param_set_collection_deps(
  SEXP private_environment,
  SEXP self
);
attribute_hidden SEXP paradox_param_set_collection_values(
  SEXP private_environment,
  SEXP self
);
attribute_hidden SEXP paradox_param_set_subset_state(
  SEXP private_environment,
  SEXP self,
  SEXP requested_ids,
  SEXP allow_dangling_dependencies,
  SEXP keep_constraint,
  SEXP constraint,
  SEXP extra_trafo,
  SEXP keep_trafo
);
attribute_hidden SEXP paradox_param_set_subspace_states(
  SEXP private_environment,
  SEXP self,
  SEXP requested_ids,
  SEXP extra_trafo
);
attribute_hidden SEXP paradox_param_set_base_snapshot_state(
  SEXP private_environment,
  SEXP self
);
attribute_hidden SEXP paradox_param_set_adopt_subset_state(
  SEXP private_environment,
  SEXP token
);
attribute_hidden SEXP paradox_plain_binding_snapshot(
  SEXP environment,
  SEXP name
);
attribute_hidden SEXP paradox_gateway_context_snapshot(
  SEXP self,
  SEXP expected_kind
);
attribute_hidden SEXP paradox_param_set_class_kind(SEXP self);
attribute_hidden SEXP paradox_param_set_assert_values_exact(SEXP value);
attribute_hidden SEXP paradox_design_transpose(SEXP data, SEXP filter_na);
attribute_hidden SEXP paradox_design_transpose_trafos(
  SEXP rows,
  SEXP param_set
);
attribute_hidden SEXP paradox_finalize_data_table(SEXP table);
attribute_hidden SEXP paradox_test_checked_affixed_size(
  SEXP owner_boundary,
  SEXP id_boundary
);
attribute_hidden SEXP paradox_test_stateful_altrep(
  SEXP first,
  SEXP later,
  SEXP elt_switch_after,
  SEXP length_switch_after,
  SEXP callback,
  SEXP callback_after
);
attribute_hidden SEXP paradox_test_stateful_altrep_rearm(
  SEXP value,
  SEXP callback_after
);
attribute_hidden SEXP paradox_test_stateful_altrep_row_names_rearm(
  SEXP table,
  SEXP callback_after
);
attribute_hidden SEXP paradox_test_public_row_names_count(SEXP row_names);
attribute_hidden SEXP paradox_test_gc_column_mutator(
  SEXP table,
  SEXP column,
  SEXP replacement
);
attribute_hidden SEXP paradox_test_tune_token_gc_mutation_snapshot(
  SEXP token,
  SEXP column,
  SEXP replacement
);
attribute_hidden void paradox_test_altrep_initialize(DllInfo *dll);

#endif
