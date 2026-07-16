#include "paradox.h"
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

void attribute_visible R_init_paradox(DllInfo *dll);
void attribute_visible R_unload_paradox(DllInfo *dll);

/* R's registration ABI deliberately erases the concrete routine signature. */
#if defined(__clang__)
# pragma clang diagnostic push
# if defined(__has_warning)
#  if __has_warning("-Wcast-function-type-strict")
#   pragma clang diagnostic ignored "-Wcast-function-type-strict"
#  endif
#  if __has_warning("-Wcast-function-type-mismatch")
#   pragma clang diagnostic ignored "-Wcast-function-type-mismatch"
#  endif
# endif
#elif defined(__GNUC__)
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wcast-function-type"
#endif
static const R_CallMethodDef call_methods[] = {
  {"design_transpose", (DL_FUNC) &paradox_design_transpose, 2},
  {"design_transpose_logscale_builtin",
    (DL_FUNC) &paradox_design_transpose_logscale_builtin, 2},
  {"design_dependency_runtime",
    (DL_FUNC) &paradox_design_dependency_runtime, 1},
  {"design_dependency_plan_builtin",
    (DL_FUNC) &paradox_design_dependency_plan_builtin, 2},
  {"finalize_data_table", (DL_FUNC) &paradox_finalize_data_table, 1},
  {"domain_check_builtin", (DL_FUNC) &paradox_domain_check_builtin, 2},
  {"domain_construct", (DL_FUNC) &paradox_domain_construct, 14},
  {"domain_construct_frame", (DL_FUNC) &paradox_domain_construct_frame, 1},
  {"domain_builtin_runtime",
    (DL_FUNC) &paradox_domain_builtin_runtime, 3},
  {"domain_construct_builtin",
    (DL_FUNC) &paradox_domain_construct_builtin, 5},
  {"domain_fct_grouping", (DL_FUNC) &paradox_domain_fct_grouping, 1},
  {"domain_numeric_bounds_admit",
    (DL_FUNC) &paradox_domain_numeric_bounds_admit, 2},
  {"domain_uty_check_result",
    (DL_FUNC) &paradox_domain_uty_check_result, 1},
  {"domain_simple_repr_id",
    (DL_FUNC) &paradox_domain_simple_repr_id, 1},
  {"ps_builtin_runtime", (DL_FUNC) &paradox_ps_builtin_runtime, 2},
  {"ps_builtin_domains", (DL_FUNC) &paradox_ps_builtin_domains, 2},
  {"domain_qunif_builtin", (DL_FUNC) &paradox_domain_qunif_builtin, 2},
  {"domain_sanitize_builtin", (DL_FUNC) &paradox_domain_sanitize_builtin, 2},
  {"param_set_index_layout", (DL_FUNC) &paradox_param_set_index_layout, 5},
  {"param_set_construct", (DL_FUNC) &paradox_param_set_construct, 1},
  {"param_set_collection_construct",
    (DL_FUNC) &paradox_param_set_collection_construct, 4},
  {"param_set_collection_detach_plan",
    (DL_FUNC) &paradox_param_set_collection_detach_plan, 3},
  {"param_set_collection_check_builtin",
    (DL_FUNC) &paradox_param_set_collection_check_builtin, 5},
  {"param_set_check_builtin", (DL_FUNC) &paradox_param_set_check_builtin, 3},
  {"param_set_check_dt_builtin", (DL_FUNC) &paradox_param_set_check_dt_builtin, 2},
  {"param_set_check_dt_plan_builtin",
    (DL_FUNC) &paradox_param_set_check_dt_plan_builtin, 2},
  {"param_set_check_dt_complete_builtin",
    (DL_FUNC) &paradox_param_set_check_dt_complete_builtin, 2},
  {"param_set_check_dt_all_builtin",
    (DL_FUNC) &paradox_param_set_check_dt_all_builtin, 2},
  {"param_set_surface_auth", (DL_FUNC) &paradox_param_set_surface_auth, 2},
  {"param_set_ids", (DL_FUNC) &paradox_param_set_ids, 5},
  {"param_set_ids_lazy", (DL_FUNC) &paradox_param_set_ids_lazy, 2},
  {"param_set_get_values", (DL_FUNC) &paradox_param_set_get_values, 3},
  {"param_set_values_merge", (DL_FUNC) &paradox_param_set_values_merge, 4},
  {"param_set_store_values", (DL_FUNC) &paradox_param_set_store_values, 3},
  {"param_set_assign_values_checked",
    (DL_FUNC) &paradox_param_set_assign_values_checked, 3},
  {"param_set_collection_store_plan",
    (DL_FUNC) &paradox_param_set_collection_store_plan, 4},
  {"param_set_property", (DL_FUNC) &paradox_param_set_property, 2},
  {"param_set_qunif_builtin", (DL_FUNC) &paradox_param_set_qunif_builtin, 2},
  {"sampler_unif_sample_builtin",
    (DL_FUNC) &paradox_sampler_unif_sample_builtin, 4},
  {"generate_design_grid_builtin",
    (DL_FUNC) &paradox_generate_design_grid_builtin, 2},
  {"param_set_trafo_plan", (DL_FUNC) &paradox_param_set_trafo_plan, 2},
  {"param_set_get_domain", (DL_FUNC) &paradox_param_set_get_domain, 3},
  {"param_set_domains", (DL_FUNC) &paradox_param_set_domains, 2},
  {"param_set_params", (DL_FUNC) &paradox_param_set_params, 2},
  {"param_set_collection_params", (DL_FUNC) &paradox_param_set_collection_params, 2},
  {"param_set_collection_deps", (DL_FUNC) &paradox_param_set_collection_deps, 2},
  {"param_set_collection_values", (DL_FUNC) &paradox_param_set_collection_values, 2},
  {"param_set_subset_state", (DL_FUNC) &paradox_param_set_subset_state, 4},
  {"param_set_subspace_state", (DL_FUNC) &paradox_param_set_subspace_state, 5},
  {"param_set_subspace_states", (DL_FUNC) &paradox_param_set_subspace_states, 4},
  {"param_set_adopt_subset_state", (DL_FUNC) &paradox_param_set_adopt_subset_state, 2},
  {"param_set_bulk_shell_register",
    (DL_FUNC) &paradox_param_set_bulk_shell_register, 2},
  {"param_set_bulk_generator_auth",
    (DL_FUNC) &paradox_param_set_bulk_generator_auth, 1},
  {"param_set_bulk_shells", (DL_FUNC) &paradox_param_set_bulk_shells, 2},
  {"sampler_1d_unif_bulk_register",
    (DL_FUNC) &paradox_sampler_1d_unif_bulk_register, 2},
  {"sampler_1d_unif_bulk_auth",
    (DL_FUNC) &paradox_sampler_1d_unif_bulk_auth, 1},
  {"sampler_1d_unif_bulk_shells",
    (DL_FUNC) &paradox_sampler_1d_unif_bulk_shells, 4},
  {"test_checked_affixed_size",
    (DL_FUNC) &paradox_test_checked_affixed_size, 2},
  {"test_stateful_altrep", (DL_FUNC) &paradox_test_stateful_altrep, 6},
  {"test_stateful_altrep_rearm",
    (DL_FUNC) &paradox_test_stateful_altrep_rearm, 2},
  {"test_gc_column_mutator",
    (DL_FUNC) &paradox_test_gc_column_mutator, 3},
#if defined(PARADOX_TEST_GC_ROW_NAMES_ROOTS)
  {"test_gc_row_names_barrier_counts",
    (DL_FUNC) &paradox_test_gc_row_names_barrier_counts, 1},
#endif
  {NULL, NULL, 0}
};
#if defined(__clang__)
# pragma clang diagnostic pop
#elif defined(__GNUC__)
# pragma GCC diagnostic pop
#endif

void attribute_visible R_init_paradox(DllInfo *dll) {
  paradox_test_altrep_initialize(dll);
  R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

void attribute_visible R_unload_paradox(DllInfo *dll) {
  (void) dll;
  paradox_sampler_1d_unif_bulk_release();
  paradox_param_set_bulk_shell_release();
  paradox_domain_builtin_release();
  paradox_ps_builtin_release();
}
