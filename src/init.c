#include "paradox.h"
#include "r_utils.h"
#include "core_state.h"
#include "paramset_shadow.h"
#include "upgrade_graph.h"
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

void attribute_visible R_init_paradox(DllInfo *dll);

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
  {"param_set_core_new", (DL_FUNC) &paradox_param_set_core_new, 2},
  {"param_set_core_state", (DL_FUNC) &paradox_param_set_core_state, 1},
  {"param_set_core_replace", (DL_FUNC) &paradox_param_set_core_replace, 2},
  {"param_set_core_kind", (DL_FUNC) &paradox_param_set_core_kind, 1},
  {"param_set_shadow_core_new",
    (DL_FUNC) &paradox_param_set_shadow_core_new, 2},
  {"param_set_shadow_construct",
    (DL_FUNC) &paradox_param_set_shadow_construct, 2},
  {"param_set_shadow_refresh",
    (DL_FUNC) &paradox_shadow_refresh_authoritative, 2},
  {"param_set_shadow_constraint",
    (DL_FUNC) &paradox_param_set_shadow_constraint, 2},
  {"design_transpose", (DL_FUNC) &paradox_design_transpose, 2},
  {"design_transpose_trafos",
    (DL_FUNC) &paradox_design_transpose_trafos, 2},
  {"design_dependency_plan",
    (DL_FUNC) &paradox_design_dependency_plan, 2},
  {"finalize_data_table", (DL_FUNC) &paradox_finalize_data_table, 1},
  {"domain_check_builtin", (DL_FUNC) &paradox_domain_check_builtin, 3},
  {"domain_property_builtin", (DL_FUNC) &paradox_domain_property_builtin, 2},
  {"domain_construct", (DL_FUNC) &paradox_domain_construct, 18},
  {"domain_uty_check_result",
    (DL_FUNC) &paradox_domain_uty_check_result, 1},
  {"domain_simple_repr_id",
    (DL_FUNC) &paradox_domain_simple_repr_id, 1},
  {"domain_qunif_builtin", (DL_FUNC) &paradox_domain_qunif_builtin, 2},
  {"domain_sanitize_builtin", (DL_FUNC) &paradox_domain_sanitize_builtin, 2},
  {"param_set_construct", (DL_FUNC) &paradox_param_set_construct, 1},
  {"param_set_collection_construct",
    (DL_FUNC) &paradox_param_set_collection_construct, 4},
  {"param_set_collection_add",
    (DL_FUNC) &paradox_param_set_collection_add, 6},
  {"param_set_collection_detach_plan",
    (DL_FUNC) &paradox_param_set_collection_detach_plan, 3},
  {"param_set_collection_has_callback",
    (DL_FUNC) &paradox_param_set_collection_has_callback, 3},
  {"param_set_collection_extra_trafo",
    (DL_FUNC) &paradox_param_set_collection_extra_trafo, 3},
  {"param_set_collection_constraint",
    (DL_FUNC) &paradox_param_set_collection_constraint, 3},
  {"param_set_collection_detached_extra_trafo",
    (DL_FUNC) &paradox_param_set_collection_detached_extra_trafo, 2},
  {"param_set_collection_detached_constraint",
    (DL_FUNC) &paradox_param_set_collection_detached_constraint, 2},
  {"param_set_collection_owner_subset_state",
    (DL_FUNC) &paradox_param_set_collection_owner_subset_state, 3},
  {"param_set_check_builtin", (DL_FUNC) &paradox_param_set_check_builtin, 7},
  {"tune_token_snapshot_list",
    (DL_FUNC) &paradox_tune_token_snapshot_list, 3},
  {"param_set_check_dependencies_builtin",
    (DL_FUNC) &paradox_param_set_check_dependencies_builtin, 3},
  {"param_set_test_constraint_builtin",
    (DL_FUNC) &paradox_param_set_test_constraint_builtin, 4},
  {"param_set_test_constraint_dt_builtin",
    (DL_FUNC) &paradox_param_set_test_constraint_dt_builtin, 4},
  {"param_set_check_dt_builtin", (DL_FUNC) &paradox_param_set_check_dt_builtin, 6},
  {"condition_test_builtin", (DL_FUNC) &paradox_condition_test_builtin, 2},
  {"param_set_ids", (DL_FUNC) &paradox_param_set_ids, 5},
  {"param_set_ids_lazy", (DL_FUNC) &paradox_param_set_ids_lazy, 2},
  {"param_set_get_values", (DL_FUNC) &paradox_param_set_get_values, 3},
  {"param_set_values_merge", (DL_FUNC) &paradox_param_set_values_merge, 4},
  {"param_set_store_values", (DL_FUNC) &paradox_param_set_store_values, 3},
  {"param_set_assign_values_checked",
    (DL_FUNC) &paradox_param_set_assign_values_checked, 3},
  {"param_set_set_tags", (DL_FUNC) &paradox_param_set_set_tags, 3},
  {"param_set_get_tags", (DL_FUNC) &paradox_param_set_get_tags, 2},
  {"param_set_dependency_table_snapshot",
    (DL_FUNC) &paradox_param_set_dependency_table_snapshot, 1},
  {"param_set_dependencies", (DL_FUNC) &paradox_param_set_dependencies, 2},
  {"param_set_has_dependencies",
    (DL_FUNC) &paradox_param_set_has_dependencies, 2},
  {"param_set_set_dependencies",
    (DL_FUNC) &paradox_param_set_set_dependencies, 3},
  {"param_set_add_dependency",
    (DL_FUNC) &paradox_param_set_add_dependency, 6},
  {"param_set_set_callback",
    (DL_FUNC) &paradox_param_set_set_callback, 4},
  {"param_set_property", (DL_FUNC) &paradox_param_set_property, 2},
  {"param_set_qunif_builtin", (DL_FUNC) &paradox_param_set_qunif_builtin, 3},
  {"sampler_unif_sample_builtin",
    (DL_FUNC) &paradox_sampler_unif_sample_builtin, 2},
  {"generate_design_grid_builtin",
    (DL_FUNC) &paradox_generate_design_grid_builtin, 2},
  {"param_set_trafo", (DL_FUNC) &paradox_param_set_trafo, 4},
  {"param_set_get_domain", (DL_FUNC) &paradox_param_set_get_domain, 3},
  {"param_set_domains", (DL_FUNC) &paradox_param_set_domains, 2},
  {"param_set_params", (DL_FUNC) &paradox_param_set_params, 2},
  {"param_set_collection_params", (DL_FUNC) &paradox_param_set_collection_params, 2},
  {"param_set_collection_deps", (DL_FUNC) &paradox_param_set_collection_deps, 2},
  {"param_set_collection_values", (DL_FUNC) &paradox_param_set_collection_values, 2},
  {"param_set_subset_state", (DL_FUNC) &paradox_param_set_subset_state, 8},
  {"param_set_subspace_states", (DL_FUNC) &paradox_param_set_subspace_states, 4},
  {"param_set_adopt_subset_state", (DL_FUNC) &paradox_param_set_adopt_subset_state, 2},
  {"upgrade_graph_discover", (DL_FUNC) &paradox_upgrade_graph_discover, 1},
  {"test_checked_affixed_size",
    (DL_FUNC) &paradox_test_checked_affixed_size, 2},
  {"test_stateful_altrep", (DL_FUNC) &paradox_test_stateful_altrep, 6},
  {"test_stateful_altrep_rearm",
    (DL_FUNC) &paradox_test_stateful_altrep_rearm, 2},
  {"test_stateful_altrep_row_names_rearm",
    (DL_FUNC) &paradox_test_stateful_altrep_row_names_rearm, 2},
  {"test_public_row_names_count",
    (DL_FUNC) &paradox_test_public_row_names_count, 1},
  {"test_materialize_public_table_shell",
    (DL_FUNC) &paradox_materialize_public_table_shell, 1},
  {"test_gc_column_mutator",
    (DL_FUNC) &paradox_test_gc_column_mutator, 3},
  {"test_tune_token_gc_mutation_snapshot",
    (DL_FUNC) &paradox_test_tune_token_gc_mutation_snapshot, 3},
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
