#include "paradox.h"
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
  {"design_transpose", (DL_FUNC) &paradox_design_transpose, 2},
  {"finalize_data_table", (DL_FUNC) &paradox_finalize_data_table, 1},
  {"domain_check_builtin", (DL_FUNC) &paradox_domain_check_builtin, 2},
  {"domain_construct", (DL_FUNC) &paradox_domain_construct, 14},
  {"domain_construct_frame", (DL_FUNC) &paradox_domain_construct_frame, 1},
  {"domain_qunif_builtin", (DL_FUNC) &paradox_domain_qunif_builtin, 2},
  {"domain_sanitize_builtin", (DL_FUNC) &paradox_domain_sanitize_builtin, 2},
  {"param_set_construct", (DL_FUNC) &paradox_param_set_construct, 1},
  {"param_set_collection_construct",
    (DL_FUNC) &paradox_param_set_collection_construct, 4},
  {"param_set_check_builtin", (DL_FUNC) &paradox_param_set_check_builtin, 3},
  {"param_set_check_dt_builtin", (DL_FUNC) &paradox_param_set_check_dt_builtin, 2},
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
  {"param_set_trafo_plan", (DL_FUNC) &paradox_param_set_trafo_plan, 2},
  {"param_set_get_domain", (DL_FUNC) &paradox_param_set_get_domain, 3},
  {"param_set_domains", (DL_FUNC) &paradox_param_set_domains, 2},
  {"param_set_params", (DL_FUNC) &paradox_param_set_params, 2},
  {"param_set_collection_params", (DL_FUNC) &paradox_param_set_collection_params, 2},
  {"param_set_collection_deps", (DL_FUNC) &paradox_param_set_collection_deps, 2},
  {"param_set_collection_values", (DL_FUNC) &paradox_param_set_collection_values, 2},
  {"param_set_subset_state", (DL_FUNC) &paradox_param_set_subset_state, 4},
  {"param_set_adopt_subset_state", (DL_FUNC) &paradox_param_set_adopt_subset_state, 2},
  {"test_checked_affixed_size",
    (DL_FUNC) &paradox_test_checked_affixed_size, 2},
  {"test_stateful_altrep", (DL_FUNC) &paradox_test_stateful_altrep, 6},
  {"test_stateful_altrep_rearm",
    (DL_FUNC) &paradox_test_stateful_altrep_rearm, 2},
  {"test_gc_column_mutator",
    (DL_FUNC) &paradox_test_gc_column_mutator, 3},
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
