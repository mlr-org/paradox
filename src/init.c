#include "paradox.h"
#include "r_utils.h"
#include "core_state.h"
#include "domain_admission.h"
#include "paramset_domain_common.h"
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
#elif defined(__GNUC__) && __GNUC__ >= 8
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wcast-function-type"
#endif
static const R_CallMethodDef call_methods[] = {
  {"param_set_core_new", (DL_FUNC) &paradox_param_set_core_new, 2},
  {"param_set_core_state", (DL_FUNC) &paradox_param_set_core_state, 2},
  {"param_set_core_replace", (DL_FUNC) &paradox_param_set_core_replace, 2},
  {"param_set_core_kind", (DL_FUNC) &paradox_param_set_core_kind, 1},
  {"param_set_deep_clone_receipt",
    (DL_FUNC) &paradox_param_set_deep_clone_receipt, 5},
  {"param_set_deep_clone_shadow_receipt",
    (DL_FUNC) &paradox_param_set_deep_clone_shadow_receipt, 1},
  {"param_set_shadow_core_new",
    (DL_FUNC) &paradox_param_set_shadow_core_new, 2},
  {"param_set_shadow_construct",
    (DL_FUNC) &paradox_param_set_shadow_construct, 2},
  {"param_set_shadow_origin",
    (DL_FUNC) &paradox_param_set_shadow_origin, 2},
  {"param_set_core_refresh",
    (DL_FUNC) &paradox_core_refresh, 2},
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
  {"domain_uty_validate_custom_check",
    (DL_FUNC) &paradox_domain_uty_validate_custom_check, 1},
  {"domain_simple_repr_id",
    (DL_FUNC) &paradox_domain_simple_repr_id, 1},
  {"domain_qunif_builtin", (DL_FUNC) &paradox_domain_qunif_builtin, 2},
  {"domain_sanitize_builtin", (DL_FUNC) &paradox_domain_sanitize_builtin, 2},
  {"param_set_construct", (DL_FUNC) &paradox_param_set_construct, 2},
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
  {"param_set_validate_current_graph",
    (DL_FUNC) &paradox_param_set_validate_current_graph, 3},
  {"param_set_validate_current_roots",
    (DL_FUNC) &paradox_param_set_validate_current_roots, 2},
  {"param_set_internal_tuning_snapshot",
    (DL_FUNC) &paradox_param_set_internal_tuning_snapshot, 4},
  {"param_set_internal_tuning_receipt",
    (DL_FUNC) &paradox_param_set_internal_tuning_receipt, 1},
  {"param_set_generation_receipt",
    (DL_FUNC) &paradox_param_set_generation_receipt, 1},
  {"param_set_check_builtin", (DL_FUNC) &paradox_param_set_check_builtin, 7},
  {"tune_token_snapshot_list",
    (DL_FUNC) &paradox_tune_token_snapshot_list, 3},
  {"tune_token_snapshot_current",
    (DL_FUNC) &paradox_tune_token_snapshot_current, 2},
  {"param_set_check_dependencies_builtin",
    (DL_FUNC) &paradox_param_set_check_dependencies_builtin, 3},
  {"param_set_test_constraint_builtin",
    (DL_FUNC) &paradox_param_set_test_constraint_builtin, 4},
  {"param_set_test_constraint_dt_builtin",
    (DL_FUNC) &paradox_param_set_test_constraint_dt_builtin, 4},
  {"param_set_check_dt_builtin", (DL_FUNC) &paradox_param_set_check_dt_builtin, 6},
  {"condition_test_builtin", (DL_FUNC) &paradox_condition_test_builtin, 2},
  {"param_set_ids", (DL_FUNC) &paradox_param_set_ids, 5},
  {"param_set_ids_lazy", (DL_FUNC) &paradox_param_set_ids_lazy, 3},
  {"param_set_get_values", (DL_FUNC) &paradox_param_set_get_values, 3},
  {"param_set_values_merge", (DL_FUNC) &paradox_param_set_values_merge, 4},
  {"param_set_set_values", (DL_FUNC) &paradox_param_set_set_values, 5},
  {"param_set_assign_values",
    (DL_FUNC) &paradox_param_set_assign_values, 3},
  {"param_set_store_values", (DL_FUNC) &paradox_param_set_store_values, 3},
  {"param_set_assign_values_checked",
    (DL_FUNC) &paradox_param_set_assign_values_checked, 3},
  {"param_set_internal_tuning_store",
    (DL_FUNC) &paradox_param_set_internal_tuning_store, 5},
  {"param_set_internal_tuning_store_owners",
    (DL_FUNC) &paradox_param_set_internal_tuning_store_owners, 4},
  {"param_set_set_tags", (DL_FUNC) &paradox_param_set_set_tags, 3},
  {"param_set_get_tags", (DL_FUNC) &paradox_param_set_get_tags, 2},
  {"param_set_dependency_table_snapshot",
    (DL_FUNC) &paradox_param_set_dependency_table_snapshot, 1},
  {"param_set_dependencies", (DL_FUNC) &paradox_param_set_dependencies, 2},
  {"param_set_has_dependencies",
    (DL_FUNC) &paradox_param_set_has_dependencies, 2},
  {"param_set_assertion_state",
    (DL_FUNC) &paradox_param_set_assertion_state, 2},
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
  {"sampler_unif_subspace_handoffs",
    (DL_FUNC) &paradox_sampler_unif_subspace_handoffs, 1},
  {"sampler_unif_take_subspace",
    (DL_FUNC) &paradox_sampler_unif_take_subspace, 1},
  {"generate_design_grid_builtin",
    (DL_FUNC) &paradox_generate_design_grid_builtin, 4},
  {"param_set_trafo", (DL_FUNC) &paradox_param_set_trafo, 4},
  {"param_set_get_domain", (DL_FUNC) &paradox_param_set_get_domain, 3},
  {"param_set_domains", (DL_FUNC) &paradox_param_set_domains, 2},
  {"param_set_params", (DL_FUNC) &paradox_param_set_params, 2},
  {"param_set_collection_params", (DL_FUNC) &paradox_param_set_collection_params, 2},
  {"param_set_collection_deps", (DL_FUNC) &paradox_param_set_collection_deps, 2},
  {"param_set_collection_values", (DL_FUNC) &paradox_param_set_collection_values, 2},
  {"param_set_subset_state", (DL_FUNC) &paradox_param_set_subset_state, 6},
  {"param_set_flatten_state", (DL_FUNC) &paradox_param_set_flatten_state, 2},
  {"test_param_set_subset_reentry",
    (DL_FUNC) &paradox_test_param_set_subset_reentry, 7},
  {"param_set_subspace_states", (DL_FUNC) &paradox_param_set_subspace_states, 3},
  {"param_set_all_subspace_states",
    (DL_FUNC) &paradox_param_set_all_subspace_states, 2},
  {"param_set_adopt_subset_state", (DL_FUNC) &paradox_param_set_adopt_subset_state, 2},
  {"upgrade_graph_discover", (DL_FUNC) &paradox_upgrade_graph_discover, 1},
  {"test_upgrade_graph_boundary_lifetime",
    (DL_FUNC) &paradox_test_upgrade_graph_boundary_lifetime, 2},
  {"upgrade_class_snapshot", (DL_FUNC) &paradox_upgrade_class_snapshot, 1},
  {"upgrade_structural_list_exact",
    (DL_FUNC) &paradox_upgrade_structural_list_exact, 1},
  {"upgrade_carrier_list_snapshot",
    (DL_FUNC) &paradox_upgrade_carrier_list_snapshot, 1},
  {"upgrade_table_list_snapshot",
    (DL_FUNC) &paradox_upgrade_table_list_snapshot, 3},
  {"upgrade_values_snapshot",
    (DL_FUNC) &paradox_upgrade_values_snapshot, 3},
  {"upgrade_public_binding_receipts",
    (DL_FUNC) &paradox_upgrade_public_binding_receipts, 1},
  {"plain_binding_snapshot", (DL_FUNC) &paradox_plain_binding_snapshot, 2},
  {"gateway_context_snapshot",
    (DL_FUNC) &paradox_gateway_context_snapshot, 2},
  {"param_set_class_kind", (DL_FUNC) &paradox_param_set_class_kind, 1},
  {"param_set_assert_values_exact",
    (DL_FUNC) &paradox_param_set_assert_values_exact, 1},
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
    (DL_FUNC) &paradox_test_tune_token_gc_mutation_snapshot, 4},
  {"test_gc_attribute_mutator",
    (DL_FUNC) &paradox_test_gc_attribute_mutator, 3},
  {"test_builtin_metadata_copy_reentry",
    (DL_FUNC) &paradox_test_builtin_metadata_copy_reentry, 2},
  {"test_param_set_collection_add_reentry",
    (DL_FUNC) &paradox_test_param_set_collection_add_reentry, 8},
  {"test_param_set_collection_construct_reentry",
    (DL_FUNC) &paradox_test_param_set_collection_construct_reentry, 5},
  {"test_domain_interpretation_closure",
    (DL_FUNC) &paradox_test_domain_interpretation_closure, 1},
  {"test_domain_admission_reentry",
    (DL_FUNC) &paradox_test_domain_admission_reentry, 4},
#if defined(PARADOX_TEST_CORE_GRAPH_ROOTS)
  {"test_core_graph_root_barrier_counts",
    (DL_FUNC) &paradox_test_core_graph_root_barrier_counts, 1},
#endif
  {NULL, NULL, 0}
};
#if defined(__clang__)
# pragma clang diagnostic pop
#elif defined(__GNUC__) && __GNUC__ >= 8
# pragma GCC diagnostic pop
#endif

void attribute_visible R_init_paradox(DllInfo *dll) {
  paradox_domain_intern_column_names();
  paradox_domain_admission_intern();
  paradox_test_altrep_initialize(dll);
  R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
