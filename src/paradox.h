#ifndef PARADOX_H
#define PARADOX_H

/* R 4.6 can expose a compiler-extension fixed-base enum in its public C17
 * headers when R itself was built by a compiler that supports it. Keep
 * pedantic diagnostics enabled for our code while isolating that declaration. */
#if defined(__clang__)
# pragma clang diagnostic push
# if defined(__has_warning)
#  if __has_warning("-Wfixed-enum-extension")
#   pragma clang diagnostic ignored "-Wfixed-enum-extension"
#  elif __has_warning("-Wc23-extensions")
#   pragma clang diagnostic ignored "-Wc23-extensions"
#  endif
# endif
#elif defined(__GNUC__)
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wpedantic"
#endif
#include <R.h>
#include <Rversion.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#if defined(__clang__)
# pragma clang diagnostic pop
#elif defined(__GNUC__)
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
  SEXP init_value
);
attribute_hidden SEXP paradox_domain_construct_frame(SEXP frame);
attribute_hidden SEXP paradox_domain_builtin_runtime(
  SEXP constructors,
  SEXP no_default,
  SEXP sort_function
);
attribute_hidden void paradox_domain_builtin_release(void);
attribute_hidden SEXP paradox_domain_construct_builtin(
  SEXP frame,
  SEXP wrapper,
  SEXP call,
  SEXP caller_environment,
  SEXP kind_value
);
attribute_hidden SEXP paradox_domain_fct_grouping(SEXP levels);
attribute_hidden SEXP paradox_domain_numeric_bounds_admit(
  SEXP frame,
  SEXP integer_kind
);
attribute_hidden SEXP paradox_domain_uty_check_result(SEXP result);
attribute_hidden SEXP paradox_domain_simple_repr_id(SEXP representation);
attribute_hidden SEXP paradox_domain_relaxed_repr_id(SEXP representation);
attribute_hidden SEXP paradox_ps_builtin_runtime(
  SEXP constructors,
  SEXP no_default
);
attribute_hidden void paradox_ps_builtin_release(void);
attribute_hidden SEXP paradox_ps_builtin_domains(
  SEXP expressions,
  SEXP eval_environment
);
attribute_hidden SEXP paradox_param_set_index_layout(
  SEXP version,
  SEXP params_index,
  SEXP tags_index,
  SEXP identity_index,
  SEXP empty_index
);
attribute_hidden int paradox_param_set_attach_singleton_index(SEXP params);
attribute_hidden SEXP paradox_param_set_construct(SEXP domains);
attribute_hidden SEXP paradox_param_set_collection_construct(
  SEXP sets,
  SEXP tag_sets,
  SEXP tag_params,
  SEXP postfix_names
);
attribute_hidden SEXP paradox_param_set_collection_detach_plan(
  SEXP private_environment,
  SEXP self,
  SEXP requested
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
attribute_hidden SEXP paradox_param_set_collection_store_plan(
  SEXP private_environment,
  SEXP self,
  SEXP sets,
  SEXP values
);
attribute_hidden SEXP paradox_param_set_filter_argument(
  SEXP frame,
  const char *argument_name
);
attribute_hidden SEXP paradox_param_set_evaluated_local_value(
  SEXP environment,
  const char *name
);
attribute_hidden SEXP paradox_param_set_property(SEXP params, SEXP property);
attribute_hidden SEXP paradox_domain_check_builtin(SEXP param, SEXP values);
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
attribute_hidden SEXP paradox_param_set_check_builtin(
  SEXP params,
  SEXP xs,
  SEXP sanitize
);
attribute_hidden SEXP paradox_param_set_collection_check_builtin(
  SEXP private_environment,
  SEXP self,
  SEXP values,
  SEXP sanitize,
  SEXP check_strict
);
attribute_hidden SEXP paradox_param_set_check_dt_builtin(SEXP params, SEXP xdt);
attribute_hidden SEXP paradox_param_set_check_dt_plan_builtin(
  SEXP params,
  SEXP xdt
);
attribute_hidden SEXP paradox_param_set_check_dt_complete_builtin(
  SEXP params,
  SEXP xdt
);
attribute_hidden SEXP paradox_param_set_check_dt_all_builtin(
  SEXP params,
  SEXP xdt
);
attribute_hidden SEXP paradox_param_set_surface_auth(SEXP self, SEXP mode);
attribute_hidden int paradox_param_set_design_trafo_auth(SEXP self);
attribute_hidden int paradox_param_set_design_dependencies_auth(SEXP self);
attribute_hidden int paradox_param_set_collection_check_auth(SEXP self);
attribute_hidden int paradox_param_set_random_design_auth(SEXP self);
attribute_hidden SEXP paradox_param_set_qunif_builtin(SEXP params, SEXP x);
attribute_hidden SEXP paradox_sampler_unif_sample_builtin(
  SEXP self,
  SEXP param_set,
  SEXP samplers,
  SEXP n
);
attribute_hidden SEXP paradox_design_dependency_runtime(
  SEXP mlr3misc_version
);
attribute_hidden SEXP paradox_design_dependency_plan_builtin(
  SEXP data,
  SEXP param_set
);
attribute_hidden SEXP paradox_generate_design_grid_builtin(
  SEXP params,
  SEXP resolutions
);
attribute_hidden SEXP paradox_param_set_trafo_plan(SEXP x, SEXP trafos);
attribute_hidden SEXP paradox_param_set_get_domain(
  SEXP private_environment,
  SEXP self,
  SEXP id
);
attribute_hidden SEXP paradox_param_set_domains(
  SEXP private_environment,
  SEXP self
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
  SEXP check_dependencies
);
attribute_hidden SEXP paradox_param_set_subspace_state(
  SEXP private_environment,
  SEXP self,
  SEXP requested_ids,
  SEXP check_dependencies,
  SEXP expected_values
);
attribute_hidden SEXP paradox_param_set_subspace_states(
  SEXP private_environment,
  SEXP self,
  SEXP requested_ids,
  SEXP expected_values
);
attribute_hidden SEXP paradox_param_set_adopt_subset_state(
  SEXP private_environment,
  SEXP token
);
attribute_hidden int paradox_param_set_subset_state_is_singleton(SEXP token);
attribute_hidden int paradox_param_set_subset_state_is_sampler_safe(
  SEXP token
);
attribute_hidden int paradox_param_set_adopt_subset_state_internal(
  SEXP private_environment,
  SEXP token
);
attribute_hidden int paradox_param_set_subset_destination_ready(
  SEXP private_environment
);
attribute_hidden SEXP paradox_param_set_bulk_shell_register(
  SEXP prototype,
  SEXP generator_surface
);
attribute_hidden SEXP paradox_param_set_bulk_generator_auth(
  SEXP generator_surface
);
attribute_hidden SEXP paradox_param_set_bulk_shells(
  SEXP prototype,
  SEXP plans
);
attribute_hidden void paradox_param_set_bulk_shell_release(void);
attribute_hidden SEXP paradox_sampler_1d_unif_bulk_register(
  SEXP prototype,
  SEXP generators
);
attribute_hidden SEXP paradox_sampler_1d_unif_bulk_shells(
  SEXP param_generator,
  SEXP generators,
  SEXP plans,
  SEXP values
);
attribute_hidden SEXP paradox_sampler_1d_unif_bulk_auth(SEXP generators);
attribute_hidden void paradox_sampler_1d_unif_bulk_release(void);
attribute_hidden SEXP paradox_design_transpose(SEXP data, SEXP filter_na);
attribute_hidden SEXP paradox_design_transpose_logscale_builtin(
  SEXP xs,
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
attribute_hidden SEXP paradox_test_gc_column_mutator(
  SEXP table,
  SEXP column,
  SEXP replacement
);
#if defined(PARADOX_TEST_GC_ROW_NAMES_ROOTS)
enum paradox_test_gc_row_names_point {
  PARADOX_TEST_GC_ROW_NAMES_STORE = 0,
  PARADOX_TEST_GC_ROW_NAMES_COLLECTION_LOCAL,
  PARADOX_TEST_GC_ROW_NAMES_COLLECTION_CARRIER,
  PARADOX_TEST_GC_ROW_NAMES_POINT_COUNT
};
attribute_hidden void paradox_test_gc_row_names_barrier(
  SEXP value,
  enum paradox_test_gc_row_names_point point
);
attribute_hidden SEXP paradox_test_gc_row_names_barrier_counts(SEXP reset);
# define PARADOX_TEST_GC_ROW_NAMES_BARRIER(value, point) \
  paradox_test_gc_row_names_barrier((value), (point))
#else
# define PARADOX_TEST_GC_ROW_NAMES_BARRIER(value, point) ((void) 0)
#endif
attribute_hidden void paradox_test_altrep_initialize(DllInfo *dll);

#endif
