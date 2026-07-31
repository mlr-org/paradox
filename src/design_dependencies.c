#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "builtin_condition.h"
#include "core_state.h"
#include "dependency_graph.h"
#include "generation_receipt.h"
#include "paramset_collection_readers.h"
#include "paramset_domain_common.h"
#include "paramset_shadow.h"
#include "r_api_compat.h"
#include "r_utils.h"

/* Build the complete dependency mask for one public Design$data snapshot.
 * Freeze the package-owned capsule graph and every Design column once,
 * validate the closed Condition objects, walk one stable topological order,
 * and return the outward mutation plan. Invalid current state errors; there is
 * no alternate execution engine. This vector engine consumes complete sampled
 * rows, so every dependency parent already has a column value and recorded
 * defaults can never participate. It is therefore deliberately separate from,
 * but comparator-equivalent to, the list-basis activity kernel used by
 * checks, stored-value reads, and constraints. */

#define DESIGN_DEPENDENCY_MASK_LIMIT_BYTES \
  ((size_t) 128U * (size_t) 1024U * (size_t) 1024U)

typedef struct {
  SEXP params;
  SEXP dependencies;
  SEXP values;
  SEXP columns;
  SEXP data_names;
  paradox_domain_params_t params_data;
  paradox_domain_dependencies_t dependencies_data;
  paradox_domain_values_t values_data;
  R_xlen_t parameter_count;
  R_xlen_t dependency_count;
  R_xlen_t row_count;
  R_xlen_t *column_by_parameter;
  R_xlen_t *fixed_by_parameter;
  paradox_dependency_graph_plan_t graph;
  paradox_collection_graph_t collection_graph;
  int has_collection_graph;
  SEXP selected_private;
  SEXP selected_core;
  SEXP selected_shadow_signature;
  SEXP selected_shadow_signature_content;
} dependency_snapshot_t;

static SEXP snapshot_string_vector(SEXP source,
    const char *description, R_xlen_t *work_since_interrupt) {
  if (TYPEOF(source) != STRSXP || ALTREP(source) || Rf_isS4(source) ||
      Rf_isObject(source) || !paradox_api_has_no_attributes(source)) {
    Rf_error("%s must be a character vector", description);
  }
  const R_xlen_t count = XLENGTH(source);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, count));
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP value = STRING_ELT(source, index);
    if (value == NA_STRING || Rf_getCharCE(value) == CE_BYTES) {
      UNPROTECT(1);
      Rf_error("%s contains an unsupported name", description);
    }
    SET_STRING_ELT(result, index, value);
  }
  UNPROTECT(1);
  return result;
}

static void validate_snapshot_names(SEXP names,
    const char *description, R_xlen_t *work_since_interrupt) {
  const R_xlen_t count = XLENGTH(names);
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP value = STRING_ELT(names, index);
    if (value == NA_STRING || Rf_getCharCE(value) == CE_BYTES) {
      Rf_error("%s contains an unsupported name", description);
    }
  }
}

static int ordinary_design_shell(SEXP data) {
  static const char *const names_only[] = {"names"};
  if (TYPEOF(data) != VECSXP || ALTREP(data) || Rf_isS4(data)) {
    return FALSE;
  }
  int valid_shell = FALSE;
  if (!Rf_isObject(data)) {
    valid_shell = paradox_api_has_only_attributes(data, names_only, 1);
  } else {
    valid_shell = paradox_public_table_kind(data) !=
      PARADOX_PUBLIC_TABLE_NONE;
  }
  if (!valid_shell) return FALSE;
  SEXP names = PROTECT(paradox_api_raw_attribute(data, R_NamesSymbol));
  const int valid_names = names == R_NilValue
    ? XLENGTH(data) == 0
    : TYPEOF(names) == STRSXP && !ALTREP(names) && !Rf_isS4(names) &&
      !Rf_isObject(names) && paradox_api_has_no_attributes(names) &&
      XLENGTH(names) == XLENGTH(data);
  UNPROTECT(1);
  return valid_names;
}

#define DESIGN_COLUMN_METADATA_LIMIT ((R_xlen_t) 64)

typedef struct {
  SEXP entries;
  R_xlen_t count;
  SEXP classes;
  SEXP levels;
  int valid;
} design_column_metadata_t;

static void capture_design_column_attribute(
    SEXP tag, SEXP value, void *data) {
  design_column_metadata_t *metadata = data;
  if (!metadata->valid || TYPEOF(tag) != SYMSXP ||
      value == R_NilValue ||
      metadata->count >= DESIGN_COLUMN_METADATA_LIMIT) {
    metadata->valid = FALSE;
    return;
  }
  for (R_xlen_t index = 0; index < metadata->count; ++index) {
    if (VECTOR_ELT(metadata->entries, 2 * index) == tag) {
      metadata->valid = FALSE;
      return;
    }
  }
  SET_VECTOR_ELT(metadata->entries, 2 * metadata->count, tag);
  SET_VECTOR_ELT(metadata->entries, 2 * metadata->count + 1, value);
  if (tag == R_ClassSymbol) metadata->classes = value;
  if (tag == R_LevelsSymbol) metadata->levels = value;
  ++metadata->count;
}

typedef struct {
  SEXP entries;
  R_xlen_t expected;
  R_xlen_t count;
  int current;
} design_column_metadata_receipt_t;

static void compare_design_column_attribute(
    SEXP tag, SEXP value, void *data) {
  design_column_metadata_receipt_t *receipt = data;
  if (!receipt->current || receipt->count >= receipt->expected ||
      VECTOR_ELT(receipt->entries, 2 * receipt->count) != tag ||
      VECTOR_ELT(receipt->entries, 2 * receipt->count + 1) != value) {
    receipt->current = FALSE;
    return;
  }
  ++receipt->count;
}

static int capture_design_column_metadata(
    SEXP source, SEXP entries, design_column_metadata_t *metadata) {
  *metadata = (design_column_metadata_t) {
    entries,
    0,
    R_NilValue,
    R_NilValue,
    TRUE
  };
  R_xlen_t count = 0;
  return paradox_api_map_bounded_stored_attributes(
      source,
      DESIGN_COLUMN_METADATA_LIMIT,
      capture_design_column_attribute,
      metadata,
      &count
    ) && metadata->valid && metadata->count == count;
}

static int design_column_metadata_is_current(
    SEXP source, const design_column_metadata_t *metadata) {
  design_column_metadata_receipt_t receipt = {
    metadata->entries,
    metadata->count,
    0,
    TRUE
  };
  R_xlen_t count = 0;
  return paradox_api_map_bounded_stored_attributes(
      source,
      DESIGN_COLUMN_METADATA_LIMIT,
      compare_design_column_attribute,
      &receipt,
      &count
    ) && receipt.current && receipt.count == metadata->count &&
    count == metadata->count;
}

static int ordinary_design_classes(SEXP classes) {
  if (classes == R_NilValue) return TRUE;
  return TYPEOF(classes) == STRSXP && !ALTREP(classes) &&
    !Rf_isS4(classes) && !Rf_isObject(classes) &&
    paradox_api_has_no_attributes(classes) &&
    XLENGTH(classes) != 0;
}

static SEXP snapshot_column(SEXP source,
    R_xlen_t *work_since_interrupt) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
  if (type != LGLSXP && type != INTSXP && type != REALSXP &&
      type != STRSXP && type != VECSXP) {
    Rf_error("Design columns must be logical, integer, numeric, character, factor, or list vectors");
  }
  const R_xlen_t count = XLENGTH(source);
  SEXP result = PROTECT(Rf_allocVector(type, count));
  /*
   * The destination allocation may run a finalizer that changes the public
   * column. Validate the exact post-allocation structure whose elements are
   * copied below; otherwise unsupported class metadata from a later
   * generation could bypass admission.
   */
  if ((SEXPTYPE) TYPEOF(source) != type || Rf_isS4(source) ||
      XLENGTH(source) != count) {
    UNPROTECT(1);
    Rf_error("Design column changed while being snapshotted");
  }
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_account_work(work_since_interrupt);
    switch (type) {
    case LGLSXP:
      SET_LOGICAL_ELT(result, index, LOGICAL_ELT(source, index));
      break;
    case INTSXP:
      SET_INTEGER_ELT(result, index, INTEGER_ELT(source, index));
      break;
    case REALSXP:
      SET_REAL_ELT(result, index, REAL_ELT(source, index));
      break;
    case STRSXP:
      SET_STRING_ELT(result, index, STRING_ELT(source, index));
      break;
    case VECSXP:
      /* List-column elements are opaque values, not recursively copied. */
      SET_VECTOR_ELT(result, index, VECTOR_ELT(source, index));
      break;
    default:
      UNPROTECT(1);
      Rf_error("Internal error: unsupported Design column type");
    }
  }

  if ((SEXPTYPE) TYPEOF(source) != type || Rf_isS4(source) ||
      XLENGTH(source) != count) {
    UNPROTECT(1);
    Rf_error("Design column changed while being snapshotted");
  }
  if (paradox_api_has_no_attributes(source)) {
    UNPROTECT(1);
    return result;
  }

  /*
   * An ALTREP element method may replace the source class/levels carriers.
   * Allocate their root carrier only for attributed columns, then select the
   * complete bounded post-observation metadata generation into it. Every
   * factor decision below uses this single rooted generation.
   */
  SEXP metadata_entries = PROTECT(Rf_allocVector(
    VECSXP,
    2 * DESIGN_COLUMN_METADATA_LIMIT
  ));
  design_column_metadata_t metadata;
  if ((SEXPTYPE) TYPEOF(source) != type || Rf_isS4(source) ||
      XLENGTH(source) != count ||
      !capture_design_column_metadata(
        source,
        metadata_entries,
        &metadata
      ) ||
      !ordinary_design_classes(metadata.classes) ||
      (!ALTREP(source) &&
        !paradox_ordinary_vector_payload_equal(source, result))) {
    UNPROTECT(2);
    Rf_error("Design columns must have ordinary class metadata");
  }

  if (paradox_api_ordinary_class_contains(metadata.classes, "factor")) {
    if (type != INTSXP) {
      UNPROTECT(2);
      Rf_error("A factor Design column has invalid storage");
    }
    SEXP stable_source_classes = PROTECT(snapshot_string_vector(
      metadata.classes,
      "Design column classes",
      work_since_interrupt
    ));
    SEXP levels = PROTECT(snapshot_string_vector(
      metadata.levels,
      "Factor levels",
      work_since_interrupt
    ));
    if (Rf_any_duplicated(levels, FALSE) != 0) {
      UNPROTECT(4);
      Rf_error("A factor Design column has duplicate levels");
    }
    SEXP classes = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(classes, 0, Rf_mkChar("factor"));
    Rf_setAttrib(result, R_LevelsSymbol, levels);
    Rf_setAttrib(result, R_ClassSymbol, classes);
    for (R_xlen_t index = 0; index < count; ++index) {
      const int code = INTEGER_ELT(result, index);
      if (code != NA_INTEGER &&
          (code <= 0 || (R_xlen_t) code > XLENGTH(levels))) {
        UNPROTECT(5);
        Rf_error("A factor Design column contains an invalid level code");
      }
    }
    if ((SEXPTYPE) TYPEOF(source) != type || Rf_isS4(source) ||
        XLENGTH(source) != count ||
        !design_column_metadata_is_current(source, &metadata) ||
        (!ALTREP(source) &&
          !paradox_ordinary_vector_payload_equal(source, result)) ||
        !paradox_ordinary_vector_payload_equal(
          metadata.classes,
          stable_source_classes
        ) ||
        !paradox_ordinary_vector_payload_equal(
          metadata.levels,
          levels
        )) {
      UNPROTECT(5);
      Rf_error("Design column changed while being snapshotted");
    }
    UNPROTECT(3);
  }
  UNPROTECT(2);
  return result;
}

static SEXP private_environment(SEXP param_set) {
  SEXP private = paradox_domain_private_environment(param_set);
  if (private == R_UnboundValue) {
    Rf_error("Corrupt ParamSet shell in Design dependency operation");
  }
  return private;
}

static void load_param_state(SEXP param_set, SEXP private,
    SEXP *graph_roots, PROTECT_INDEX graph_roots_index,
    dependency_snapshot_t *snapshot, SEXP roots,
    R_xlen_t *work_since_interrupt) {
  SEXP core = PROTECT(paradox_core_from_private(private));
  if (core == R_UnboundValue) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet state in Design dependency operation");
  }
  paradox_core_kind_t kind = paradox_core_kind(core);
  /*
   * The complete collection graph builder owns its refresh/admission gate.
   * Running the root gate here as well would duplicate a stale subtree walk
   * and Shadow fingerprint before immediately selecting the graph again.
   */
  if (kind != PARADOX_CORE_COLLECTION &&
      !paradox_core_is_verified(core)) {
    core = paradox_core_refresh(param_set, private);
    kind = paradox_core_kind(core);
  }
  snapshot->selected_private = private;
  snapshot->selected_core = core;
  SET_VECTOR_ELT(roots, 5, core);

  if (kind == PARADOX_CORE_COLLECTION) {
    paradox_collection_graph_build_receipted(
      private,
      param_set,
      &snapshot->collection_graph,
      graph_roots,
      graph_roots_index,
      work_since_interrupt
    );
    snapshot->has_collection_graph = TRUE;
    snapshot->selected_core =
      snapshot->collection_graph.nodes[0].source_core;
    SET_VECTOR_ELT(roots, 5, snapshot->selected_core);
    snapshot->params = snapshot->collection_graph.nodes[0].params.table;
    snapshot->dependencies = PROTECT(
      paradox_collection_dependencies_from_graph(
        &snapshot->collection_graph,
        work_since_interrupt
      )
    );
    snapshot->values = PROTECT(
      paradox_collection_values_from_graph(
        &snapshot->collection_graph,
        work_since_interrupt
      )
    );
    SET_VECTOR_ELT(roots, 0, snapshot->params);
    SET_VECTOR_ELT(roots, 1, snapshot->dependencies);
    SET_VECTOR_ELT(roots, 4, snapshot->values);
    UNPROTECT(3);
  } else if (kind == PARADOX_CORE_BASE || kind == PARADOX_CORE_SHADOW) {
    SEXP payload = paradox_core_payload(core);
    if (payload == R_UnboundValue) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSet capsule in Design dependency operation");
    }
    snapshot->params = VECTOR_ELT(payload, PARADOX_CORE_PARAMS);
    snapshot->dependencies = VECTOR_ELT(payload, PARADOX_CORE_DEPS);
    snapshot->values = VECTOR_ELT(payload, PARADOX_CORE_VALUES);
    SET_VECTOR_ELT(roots, 0, snapshot->params);
    SET_VECTOR_ELT(roots, 1, snapshot->dependencies);
    SET_VECTOR_ELT(roots, 4, snapshot->values);
    UNPROTECT(1);
    if (kind == PARADOX_CORE_SHADOW) {
      snapshot->selected_shadow_signature = PROTECT(
        paradox_shadow_metadata_signature(core)
      );
      if (snapshot->selected_shadow_signature == R_UnboundValue) {
        UNPROTECT(1);
        Rf_error("Corrupt ParamSetShadow signature in Design operation");
      }
      snapshot->selected_shadow_signature_content = PROTECT(
        paradox_shadow_signature_content_snapshot(
          snapshot->selected_shadow_signature
        )
      );
      if (snapshot->selected_shadow_signature_content == R_NilValue ||
          !paradox_shadow_signature_receipt_is_current(
            core,
            snapshot->selected_shadow_signature,
            snapshot->selected_shadow_signature_content
          )) {
        UNPROTECT(2);
        Rf_error("ParamSetShadow changed during Design construction");
      }
      SET_VECTOR_ELT(roots, 6, snapshot->selected_shadow_signature);
      SET_VECTOR_ELT(
        roots,
        7,
        snapshot->selected_shadow_signature_content
      );
      UNPROTECT(2);
    }
  } else {
    UNPROTECT(1);
    Rf_error("Unknown ParamSet node kind in Design dependency operation");
  }

  R_xlen_t unused_row = 0;
  if (!paradox_domain_validate_params(
      snapshot->params,
      R_NilValue,
      TRUE,
      &snapshot->params_data,
      &unused_row,
      work_since_interrupt
    ) || !paradox_domain_validate_dependencies(
      snapshot->dependencies,
      &snapshot->dependencies_data,
      work_since_interrupt
    ) || !paradox_domain_validate_values(
      snapshot->values,
      &snapshot->values_data,
      work_since_interrupt
    )) {
    Rf_error("Corrupt ParamSet parameter, value, or dependency capsule");
  }
  snapshot->parameter_count = snapshot->params_data.row_count;
  snapshot->dependency_count = snapshot->dependencies_data.row_count;
  snapshot->fixed_by_parameter = paradox_temporary_alloc(
    snapshot->parameter_count,
    sizeof(*snapshot->fixed_by_parameter)
  );
  for (R_xlen_t parameter = 0;
      parameter < snapshot->parameter_count;
      ++parameter) {
    snapshot->fixed_by_parameter[parameter] = R_XLEN_T_MAX;
  }
  for (R_xlen_t value = 0;
      value < snapshot->values_data.size;
      ++value) {
    R_xlen_t parameter = paradox_domain_find_string(
      snapshot->params_data.ids,
      STRING_ELT(snapshot->values_data.names, value),
      work_since_interrupt
    );
    if (parameter == R_XLEN_T_MAX ||
        snapshot->fixed_by_parameter[parameter] != R_XLEN_T_MAX) {
      Rf_error("Corrupt ParamSet stored value IDs in Design operation");
    }
    snapshot->fixed_by_parameter[parameter] = value;
  }
}

static void snapshot_design_shell(SEXP data, dependency_snapshot_t *snapshot,
    SEXP roots, R_xlen_t *work_since_interrupt) {
  const R_xlen_t column_count = XLENGTH(data);

  snapshot->data_names = PROTECT(Rf_allocVector(STRSXP, column_count));
  snapshot->columns = PROTECT(Rf_allocVector(VECSXP, column_count));
  SET_VECTOR_ELT(roots, 2, snapshot->data_names);
  SET_VECTOR_ELT(roots, 3, snapshot->columns);
  if (!ordinary_design_shell(data)) {
    UNPROTECT(2);
    Rf_error("Design$data must be a list-like data frame");
  }
  if (!paradox_capture_list_identities(
      data,
      snapshot->data_names,
      snapshot->columns
    )) {
    UNPROTECT(2);
    Rf_error("Design$data must have unique column names");
  }

  /*
   * Freeze the dimension carrier in the same allocation-free observation
   * window as the column names and identities.  Name validation and
   * Rf_any_duplicated() below may allocate and run a pending finalizer; a
   * later row.names read could otherwise combine a new table dimension with
   * the already captured old columns.
   */
  const int table_input = Rf_isObject(data) != FALSE;
  snapshot->row_count = 0;
  if (table_input &&
      !paradox_public_table_row_count(data, &snapshot->row_count)) {
    UNPROTECT(2);
    Rf_error("Design$data has invalid data.frame row names");
  }

  validate_snapshot_names(
    snapshot->data_names,
    "Design column names",
    work_since_interrupt
  );
  if (Rf_any_duplicated(snapshot->data_names, FALSE) != 0) {
    UNPROTECT(2);
    Rf_error("Design$data must have unique column names");
  }

  UNPROTECT(2);

  if (snapshot->row_count > INT_MAX) {
    Rf_error("Design dependency result exceeds data.frame row limits");
  }
}

static void snapshot_design_columns(dependency_snapshot_t *snapshot,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t column = 0; column < XLENGTH(snapshot->columns); ++column) {
    paradox_account_work(work_since_interrupt);
    SEXP source = PROTECT(VECTOR_ELT(snapshot->columns, column));
    SEXP frozen = PROTECT(snapshot_column(source, work_since_interrupt));
    if (XLENGTH(frozen) != snapshot->row_count) {
      UNPROTECT(2);
      Rf_error(column == 0
        ? "Design$data has invalid data.frame row names"
        : "Design$data columns have inconsistent lengths");
    }
    SET_VECTOR_ELT(snapshot->columns, column, frozen);
    UNPROTECT(2);
  }
}

static void map_design_columns(dependency_snapshot_t *snapshot,
    R_xlen_t *work_since_interrupt) {
  if (XLENGTH(snapshot->columns) != snapshot->parameter_count) {
    Rf_error("Design$data must have one column for every parameter");
  }
  snapshot->column_by_parameter = paradox_temporary_alloc(
    snapshot->parameter_count,
    sizeof(*snapshot->column_by_parameter)
  );
  for (R_xlen_t parameter = 0;
      parameter < snapshot->parameter_count;
      ++parameter) {
    R_xlen_t column = paradox_domain_find_string(
      snapshot->data_names,
      STRING_ELT(snapshot->params_data.ids, parameter),
      work_since_interrupt
    );
    if (column == R_XLEN_T_MAX) {
      Rf_error("Design$data column names do not match the ParamSet");
    }
    snapshot->column_by_parameter[parameter] = column;
  }
}

static int numeric_type(SEXPTYPE type) {
  return type == LGLSXP || type == INTSXP || type == REALSXP;
}

static int factor_column(SEXP column) {
  return TYPEOF(column) == INTSXP && Rf_inherits(column, "factor");
}

static int fixed_value_is_plain(SEXP value, SEXP storage_type) {
  if (paradox_domain_string_is(storage_type, "list") ||
      !Rf_isVectorAtomic(value) || Rf_isObject(value) ||
      XLENGTH(value) != 1 || !paradox_api_has_no_attributes(value)) {
    return FALSE;
  }
  if (paradox_domain_string_is(storage_type, "numeric")) {
    return TYPEOF(value) == REALSXP || TYPEOF(value) == INTSXP;
  }
  if (paradox_domain_string_is(storage_type, "integer")) {
    return TYPEOF(value) == INTSXP;
  }
  if (paradox_domain_string_is(storage_type, "character")) {
    return TYPEOF(value) == STRSXP;
  }
  if (paradox_domain_string_is(storage_type, "logical")) {
    return TYPEOF(value) == LGLSXP;
  }
  return FALSE;
}

static SEXP fixed_value(const dependency_snapshot_t *snapshot,
    R_xlen_t parameter) {
  const R_xlen_t value = snapshot->fixed_by_parameter[parameter];
  return value == R_XLEN_T_MAX
    ? R_UnboundValue
    : VECTOR_ELT(snapshot->values_data.values, value);
}

static int design_value_is_tune_token(SEXP value) {
  int token = FALSE;
  if (!paradox_api_opaque_leaf_class_matches(
      value,
      "TuneToken",
      &token
    )) {
    Rf_error(
      "Design value class metadata must be ordinary and bounded"
    );
  }
  return token;
}

static int parameter_value_is_tune_token(
    const dependency_snapshot_t *snapshot,
    R_xlen_t parameter, R_xlen_t row) {
  SEXP fixed = fixed_value(snapshot, parameter);
  if (fixed != R_UnboundValue) {
    return design_value_is_tune_token(fixed);
  }
  SEXP column = VECTOR_ELT(
    snapshot->columns,
    snapshot->column_by_parameter[parameter]
  );
  return TYPEOF(column) == VECSXP &&
    design_value_is_tune_token(VECTOR_ELT(column, row));
}

static int rhs_matches_string(SEXP value, SEXP rhs,
    R_xlen_t *work_since_interrupt) {
  if (value == NA_STRING) {
    return FALSE;
  }
  const R_xlen_t count = XLENGTH(rhs);
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_account_work(work_since_interrupt);
    if (paradox_domain_strings_equal(value, STRING_ELT(rhs, index))) {
      return TRUE;
    }
  }
  return FALSE;
}

static int condition_matches(const paradox_dependency_graph_edge_t *edge,
    SEXP column,
    R_xlen_t row, R_xlen_t *work_since_interrupt) {
  if (TYPEOF(column) == VECSXP) {
    SEXP value = VECTOR_ELT(column, row);
    /* Complete Design rows use list columns for opaque and cross-storage
     * scalar leaves. TuneToken parents skip their edge just as they do in the
     * shared list-basis activity kernel; other supported scalar leaves enter
     * the same built-in comparator as native grid traversal. */
    if (design_value_is_tune_token(value)) {
      return TRUE;
    }
    if (value == R_NilValue ||
        !paradox_builtin_condition_scalar_supported(value, edge->rhs)) {
      return FALSE;
    }
    return paradox_builtin_condition_element_matches(
      value,
      0,
      edge->rhs,
      work_since_interrupt
    );
  }
  if (factor_column(column)) {
    if (TYPEOF(edge->rhs) != STRSXP) {
      return FALSE;
    }
    const int code = INTEGER_ELT(column, row);
    if (code == NA_INTEGER) {
      return FALSE;
    }
    SEXP levels = Rf_getAttrib(column, R_LevelsSymbol);
    return rhs_matches_string(
      STRING_ELT(levels, (R_xlen_t) code - 1),
      edge->rhs,
      work_since_interrupt
    );
  }

  const SEXPTYPE column_type = (SEXPTYPE) TYPEOF(column);
  const SEXPTYPE rhs_type = (SEXPTYPE) TYPEOF(edge->rhs);
  if (!((numeric_type(column_type) && numeric_type(rhs_type)) ||
        (column_type == STRSXP && rhs_type == STRSXP))) {
    return FALSE;
  }
  return paradox_builtin_condition_element_matches(
    column,
    row,
    edge->rhs,
    work_since_interrupt
  );
}

static int parameter_condition_matches(
    const dependency_snapshot_t *snapshot,
    R_xlen_t parameter, R_xlen_t row,
    const paradox_dependency_graph_edge_t *edge,
    R_xlen_t *work_since_interrupt) {
  SEXP fixed = fixed_value(snapshot, parameter);
  if (fixed == R_UnboundValue) {
    return condition_matches(
      edge,
      VECTOR_ELT(
        snapshot->columns,
        snapshot->column_by_parameter[parameter]
      ),
      row,
      work_since_interrupt
    );
  }
  SEXP storage_type = STRING_ELT(
    VECTOR_ELT(snapshot->params, PARADOX_DOMAIN_STORAGE_TYPE),
    parameter
  );
  if (!fixed_value_is_plain(fixed, storage_type)) {
    if (design_value_is_tune_token(fixed)) {
      return TRUE;
    }
    if (fixed == R_NilValue) {
      return FALSE;
    }
  }
  /* Plain and non-plain leaves take the same comparator admission: a stored
   * value whose type the right-hand side can never equal is an ordinary
   * unsatisfied predicate -- the child masks inactive, exactly as the shared
   * list-basis activity kernel decides it -- never a typed re-read of the
   * wrong representation. */
  if (!paradox_builtin_condition_scalar_supported(fixed, edge->rhs)) {
    return FALSE;
  }
  return paradox_builtin_condition_element_matches(
    fixed,
    0,
    edge->rhs,
    work_since_interrupt
  );
}

static int value_is_missing(SEXP column, R_xlen_t row) {
  switch (TYPEOF(column)) {
  case LGLSXP:
    return LOGICAL_ELT(column, row) == NA_LOGICAL;
  case INTSXP:
    return INTEGER_ELT(column, row) == NA_INTEGER;
  case REALSXP:
    return ISNAN(REAL_ELT(column, row));
  case STRSXP:
    return STRING_ELT(column, row) == NA_STRING;
  default:
    return FALSE;
  }
}

static int parameter_value_is_missing(
    const dependency_snapshot_t *snapshot,
    R_xlen_t parameter, R_xlen_t row) {
  SEXP fixed = fixed_value(snapshot, parameter);
  if (fixed == R_UnboundValue) {
    return value_is_missing(
      VECTOR_ELT(
        snapshot->columns,
        snapshot->column_by_parameter[parameter]
      ),
      row
    );
  }
  SEXP storage_type = STRING_ELT(
    VECTOR_ELT(snapshot->params, PARADOX_DOMAIN_STORAGE_TYPE),
    parameter
  );
  return fixed_value_is_plain(fixed, storage_type)
    ? value_is_missing(fixed, 0)
    : FALSE;
}

static size_t mask_offset(const dependency_snapshot_t *snapshot,
    R_xlen_t parameter, R_xlen_t row) {
  return (size_t) parameter * (size_t) snapshot->row_count + (size_t) row;
}

static int mask_get(const dependency_snapshot_t *snapshot,
    const unsigned char *mask, R_xlen_t parameter, R_xlen_t row) {
  const size_t bit = mask_offset(snapshot, parameter, row);
  return (mask[bit >> 3] & (unsigned char) (1U << (bit & 7U))) != 0;
}

static void mask_set(const dependency_snapshot_t *snapshot,
    unsigned char *mask, R_xlen_t parameter, R_xlen_t row) {
  const size_t bit = mask_offset(snapshot, parameter, row);
  mask[bit >> 3] |= (unsigned char) (1U << (bit & 7U));
}

static SEXP typed_missing(SEXP storage) {
  if (paradox_domain_string_is(storage, "numeric")) {
    return Rf_ScalarReal(NA_REAL);
  }
  if (paradox_domain_string_is(storage, "integer")) {
    return Rf_ScalarInteger(NA_INTEGER);
  }
  if (paradox_domain_string_is(storage, "character")) {
    return Rf_ScalarString(NA_STRING);
  }
  if (paradox_domain_string_is(storage, "logical") ||
      paradox_domain_string_is(storage, "list")) {
    return Rf_ScalarLogical(NA_LOGICAL);
  }
  Rf_error("Corrupt ParamSet storage type in Design dependency operation");
  return R_NilValue;
}

static SEXP build_output(dependency_snapshot_t *snapshot,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t parameter_count = snapshot->parameter_count;
  const R_xlen_t row_count = snapshot->row_count;
  const R_xlen_t mask_rows =
    snapshot->dependency_count == 0 ? 0 : row_count;
  if (parameter_count != 0 &&
      mask_rows > R_XLEN_T_MAX / parameter_count) {
    Rf_error("Design dependency mask is too large");
  }
  const R_xlen_t bit_count = parameter_count * mask_rows;
  if ((uint64_t) bit_count > (uint64_t) SIZE_MAX - 7U) {
    Rf_error("Design dependency mask is too large");
  }
  const size_t byte_count = ((size_t) bit_count + 7U) >> 3;
  if (byte_count > DESIGN_DEPENDENCY_MASK_LIMIT_BYTES ||
      byte_count > (size_t) R_XLEN_T_MAX) {
    Rf_error("Design dependency mask exceeds the supported memory limit");
  }
  unsigned char *inactive = paradox_temporary_alloc(
    (R_xlen_t) byte_count,
    sizeof(*inactive)
  );
  if (byte_count != 0) {
    memset(inactive, 0, byte_count);
  }

  R_xlen_t patch_count = 0;
  R_xlen_t *inactive_counts = paradox_temporary_alloc(
    parameter_count,
    sizeof(*inactive_counts)
  );
  for (R_xlen_t position = 0; position < parameter_count; ++position) {
    paradox_account_work(work_since_interrupt);
    const R_xlen_t child = snapshot->graph.topological_order[position];
    inactive_counts[child] = 0;
    if (snapshot->graph.incoming_count[child] == 0) {
      continue;
    }
    ++patch_count;
    for (R_xlen_t row = 0; row < row_count; ++row) {
      /*
       * Account at the complete-row boundary as well as inside the comparator:
       * TuneToken children and dangling parents deliberately short-circuit
       * before any Condition element is observed.
       */
      paradox_account_work(work_since_interrupt);
      int child_inactive = FALSE;
      /*
       * TuneToken children skip all incoming edges, after the shared graph
       * planner above has already admitted topology and rejected cycles.
       */
      if (parameter_value_is_tune_token(snapshot, child, row)) {
        continue;
      }
      for (R_xlen_t incoming = snapshot->graph.incoming_start[child];
          incoming < snapshot->graph.incoming_start[child + 1] &&
            !child_inactive;
          ++incoming) {
        const paradox_dependency_graph_edge_t *dependency =
          &snapshot->graph.edges[
          snapshot->graph.incoming_edges[incoming]
        ];
        if (dependency->parent == R_XLEN_T_MAX) {
          child_inactive = TRUE;
          continue;
        }
        if (dependency->parent >= parameter_count) {
          Rf_error("Corrupt ParamSet dependency topology");
        }
        child_inactive = mask_get(
          snapshot,
          inactive,
          dependency->parent,
          row
        ) || parameter_value_is_missing(
          snapshot,
          dependency->parent,
          row
        ) || !parameter_condition_matches(
          snapshot,
          dependency->parent,
          row,
          dependency,
          work_since_interrupt
        );
      }
      if (child_inactive) {
        mask_set(snapshot, inactive, child, row);
        ++inactive_counts[child];
      }
    }
  }

  const R_xlen_t fixed_count = snapshot->values_data.size;
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 7));
  SEXP result_names = PROTECT(Rf_allocVector(STRSXP, 7));
  SEXP fixed_columns = PROTECT(Rf_allocVector(STRSXP, fixed_count));
  SEXP fixed_values = PROTECT(Rf_allocVector(VECSXP, fixed_count));
  SEXP fixed_plain = PROTECT(Rf_allocVector(LGLSXP, fixed_count));
  SEXP rows = PROTECT(Rf_allocVector(VECSXP, patch_count));
  SEXP columns = PROTECT(Rf_allocVector(STRSXP, patch_count));
  SEXP values = PROTECT(Rf_allocVector(VECSXP, patch_count));
  SET_STRING_ELT(result_names, 0, Rf_mkChar("fixed_columns"));
  SET_STRING_ELT(result_names, 1, Rf_mkChar("fixed_values"));
  SET_STRING_ELT(result_names, 2, Rf_mkChar("fixed_plain"));
  SET_STRING_ELT(result_names, 3, Rf_mkChar("rows"));
  SET_STRING_ELT(result_names, 4, Rf_mkChar("columns"));
  SET_STRING_ELT(result_names, 5, Rf_mkChar("values"));
  SET_STRING_ELT(result_names, 6, Rf_mkChar("receipt"));
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  SET_VECTOR_ELT(result, 0, fixed_columns);
  SET_VECTOR_ELT(result, 1, fixed_values);
  SET_VECTOR_ELT(result, 2, fixed_plain);
  SET_VECTOR_ELT(result, 3, rows);
  SET_VECTOR_ELT(result, 4, columns);
  SET_VECTOR_ELT(result, 5, values);

  for (R_xlen_t value = 0; value < fixed_count; ++value) {
    paradox_account_work(work_since_interrupt);
    SEXP id = STRING_ELT(snapshot->values_data.names, value);
    SEXP fixed = VECTOR_ELT(snapshot->values_data.values, value);
    R_xlen_t parameter = paradox_domain_find_string(
      snapshot->params_data.ids,
      id,
      work_since_interrupt
    );
    if (parameter == R_XLEN_T_MAX) {
      UNPROTECT(8);
      Rf_error("Corrupt ParamSet stored value ID in Design plan");
    }
    if (design_value_is_tune_token(fixed)) {
      UNPROTECT(8);
      Rf_error(
        "Design generation cannot materialize the stored TuneToken value of parameter '%s'.",
        CHAR(id)
      );
    }
    SET_STRING_ELT(fixed_columns, value, id);
    SET_VECTOR_ELT(fixed_values, value, fixed);
    SET_LOGICAL_ELT(
      fixed_plain,
      value,
      fixed_value_is_plain(
        fixed,
        STRING_ELT(
          VECTOR_ELT(snapshot->params, PARADOX_DOMAIN_STORAGE_TYPE),
          parameter
        )
      )
    );
  }

  R_xlen_t output = 0;
  for (R_xlen_t position = 0; position < parameter_count; ++position) {
    paradox_account_work(work_since_interrupt);
    const R_xlen_t child = snapshot->graph.topological_order[position];
    if (snapshot->graph.incoming_count[child] == 0) {
      continue;
    }
    SEXP indices = PROTECT(Rf_allocVector(
      INTSXP,
      inactive_counts[child]
    ));
    R_xlen_t index = 0;
    for (R_xlen_t row = 0; row < row_count; ++row) {
      paradox_account_work(work_since_interrupt);
      if (mask_get(snapshot, inactive, child, row)) {
        SET_INTEGER_ELT(indices, index, (int) row + 1);
        ++index;
      }
    }
    SEXP missing = PROTECT(typed_missing(STRING_ELT(
      VECTOR_ELT(snapshot->params, PARADOX_DOMAIN_STORAGE_TYPE),
      child
    )));
    SET_VECTOR_ELT(rows, output, indices);
    SET_STRING_ELT(
      columns,
      output,
      STRING_ELT(snapshot->params_data.ids, child)
    );
    SET_VECTOR_ELT(values, output, missing);
    ++output;
    UNPROTECT(2);
  }
  if (output != patch_count) {
    UNPROTECT(8);
    Rf_error("Internal error: incomplete Design dependency plan");
  }
  SEXP receipt = PROTECT(snapshot->has_collection_graph
    ? paradox_generation_receipt_graph(&snapshot->collection_graph)
    : paradox_generation_receipt_single(
        snapshot->selected_private,
        snapshot->selected_core,
        paradox_core_kind(snapshot->selected_core) ==
          PARADOX_CORE_SHADOW
          ? snapshot->selected_shadow_signature
          : R_NilValue,
        paradox_core_kind(snapshot->selected_core) ==
          PARADOX_CORE_SHADOW
          ? snapshot->selected_shadow_signature_content
          : R_NilValue
      ));
  SET_VECTOR_ELT(result, 6, receipt);
  UNPROTECT(9);
  return result;
}

static void require_current_param_generation(
    const dependency_snapshot_t *snapshot,
    R_xlen_t *work_since_interrupt) {
  if (!snapshot->has_collection_graph) {
    if (paradox_core_from_private(snapshot->selected_private) !=
        snapshot->selected_core ||
        (paradox_core_kind(snapshot->selected_core) ==
          PARADOX_CORE_SHADOW &&
          !paradox_shadow_signature_receipt_is_current(
            snapshot->selected_core,
            snapshot->selected_shadow_signature,
            snapshot->selected_shadow_signature_content
          ))) {
      Rf_error("ParamSet changed during Design construction");
    }
    return;
  }
  /*
   * A rooted capsule generation is immutable under every supported public
   * operation, but a Design keeps the live ParamSet shell after this plan
   * returns. Require every selected shell still to name that exact generation
   * in one allocation-free terminal wave. Unsupported in-place writes to
   * private capsule tables are intentionally outside this boundary.
   */
  for (R_xlen_t index = 0;
      index < snapshot->collection_graph.count;
      ++index) {
    paradox_account_work(work_since_interrupt);
    const paradox_collection_graph_node_t *node =
      &snapshot->collection_graph.nodes[index];
    if (paradox_core_from_private(node->private_environment) !=
        node->source_core) {
      Rf_error("ParamSet graph changed during Design construction");
    }
  }
  if (!paradox_collection_graph_snapshot_is_intact(
      &snapshot->collection_graph,
      work_since_interrupt
    )) {
    Rf_error("ParamSet graph changed during Design construction");
  }
}

SEXP paradox_design_dependency_plan(SEXP data, SEXP param_set) {
  data = PROTECT(paradox_materialize_public_table_shell(data));
  PROTECT(param_set);
  if (TYPEOF(data) != VECSXP || ALTREP(data) || Rf_isS4(data)) {
    UNPROTECT(2);
    Rf_error("Design$data must be a list-like data frame");
  }
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, 8));
  PROTECT_INDEX graph_roots_index;
  SEXP graph_roots;
  PROTECT_WITH_INDEX(graph_roots = R_NilValue, &graph_roots_index);
  SEXP private = PROTECT(private_environment(param_set));

  dependency_snapshot_t snapshot = {0};
  R_xlen_t work_since_interrupt = 0;
  /*
   * Public columns may materialize ALTREP or execute class metadata methods.
   * Settle and own that complete input before selecting the ParamSet graph, so
   * supported reentry consistently chooses the post-callback generation.
   */
  snapshot_design_shell(
    data,
    &snapshot,
    roots,
    &work_since_interrupt
  );
  load_param_state(
    param_set,
    private,
    &graph_roots,
    graph_roots_index,
    &snapshot,
    roots,
    &work_since_interrupt
  );
  map_design_columns(&snapshot, &work_since_interrupt);
  if (snapshot.dependency_count != 0) {
    snapshot_design_columns(
      &snapshot,
      &work_since_interrupt
    );
  }
  paradox_dependency_graph_plan_build(
    snapshot.params_data.ids,
    &snapshot.dependencies_data,
    &snapshot.graph,
    &work_since_interrupt
  );
  SEXP result = PROTECT(build_output(
    &snapshot,
    &work_since_interrupt
  ));
  require_current_param_generation(
    &snapshot,
    &work_since_interrupt
  );
  UNPROTECT(6);
  return result;
}
