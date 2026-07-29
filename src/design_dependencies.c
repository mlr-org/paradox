#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "builtin_condition.h"
#include "core_state.h"
#include "dependency_graph.h"
#include "paramset_collection_readers.h"
#include "paramset_domain_common.h"
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
  SEXP columns;
  SEXP data_names;
  paradox_domain_params_t params_data;
  paradox_domain_dependencies_t dependencies_data;
  R_xlen_t parameter_count;
  R_xlen_t dependency_count;
  R_xlen_t row_count;
  R_xlen_t *column_by_parameter;
  paradox_dependency_graph_plan_t graph;
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

static int ordinary_optional_classes(SEXP source) {
  SEXP classes = PROTECT(Rf_getAttrib(source, R_ClassSymbol));
  const int valid = classes == R_NilValue ||
    (TYPEOF(classes) == STRSXP && !ALTREP(classes) &&
      !Rf_isS4(classes) && !Rf_isObject(classes) &&
      paradox_api_has_no_attributes(classes) && XLENGTH(classes) != 0);
  UNPROTECT(1);
  return valid;
}

static SEXP snapshot_factor_levels(SEXP source,
    R_xlen_t *work_since_interrupt) {
  SEXP levels = PROTECT(Rf_getAttrib(source, R_LevelsSymbol));
  if (TYPEOF(levels) != STRSXP) {
    UNPROTECT(1);
    Rf_error("A factor Design column has invalid levels");
  }
  SEXP result = PROTECT(snapshot_string_vector(
    levels,
    "Factor levels",
    work_since_interrupt
  ));
  if (Rf_any_duplicated(result, FALSE) != 0) {
    UNPROTECT(2);
    Rf_error("A factor Design column has duplicate levels");
  }
  UNPROTECT(2);
  return result;
}

static SEXP snapshot_column(SEXP source,
    R_xlen_t *work_since_interrupt) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
  if (type != LGLSXP && type != INTSXP && type != REALSXP &&
      type != STRSXP && type != VECSXP) {
    Rf_error("Design columns must be logical, integer, numeric, character, factor, or list vectors");
  }
  if (Rf_isS4(source) || !ordinary_optional_classes(source)) {
    Rf_error("Design columns must have ordinary class metadata");
  }

  const R_xlen_t count = XLENGTH(source);
  SEXP result = PROTECT(Rf_allocVector(type, count));
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

  if (Rf_inherits(source, "factor")) {
    if (type != INTSXP) {
      UNPROTECT(1);
      Rf_error("A factor Design column has invalid storage");
    }
    SEXP levels = PROTECT(snapshot_factor_levels(
      source,
      work_since_interrupt
    ));
    SEXP classes = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(classes, 0, Rf_mkChar("factor"));
    Rf_setAttrib(result, R_LevelsSymbol, levels);
    Rf_setAttrib(result, R_ClassSymbol, classes);
    for (R_xlen_t index = 0; index < count; ++index) {
      const int code = INTEGER_ELT(result, index);
      if (code != NA_INTEGER &&
          (code <= 0 || (R_xlen_t) code > XLENGTH(levels))) {
        UNPROTECT(3);
        Rf_error("A factor Design column contains an invalid level code");
      }
    }
    UNPROTECT(2);
  }
  UNPROTECT(1);
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
  if (!paradox_core_is_verified(core)) {
    core = paradox_core_refresh(param_set, private);
  }
  const paradox_core_kind_t kind = paradox_core_kind(core);

  if (kind == PARADOX_CORE_COLLECTION) {
    paradox_collection_graph_t graph;
    paradox_collection_graph_build(
      private,
      param_set,
      &graph,
      graph_roots,
      graph_roots_index,
      work_since_interrupt
    );
    snapshot->params = graph.nodes[0].params.table;
    snapshot->dependencies = PROTECT(
      paradox_collection_dependencies_from_graph(
        &graph,
        work_since_interrupt
      )
    );
    SET_VECTOR_ELT(roots, 0, snapshot->params);
    SET_VECTOR_ELT(roots, 1, snapshot->dependencies);
    UNPROTECT(2);
  } else if (kind == PARADOX_CORE_BASE || kind == PARADOX_CORE_SHADOW) {
    SEXP payload = paradox_core_payload(core);
    if (payload == R_UnboundValue) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSet capsule in Design dependency operation");
    }
    snapshot->params = VECTOR_ELT(payload, PARADOX_CORE_PARAMS);
    snapshot->dependencies = VECTOR_ELT(payload, PARADOX_CORE_DEPS);
    SET_VECTOR_ELT(roots, 0, snapshot->params);
    SET_VECTOR_ELT(roots, 1, snapshot->dependencies);
    UNPROTECT(1);
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
    )) {
    Rf_error("Corrupt ParamSet parameter or dependency capsule");
  }
  snapshot->parameter_count = snapshot->params_data.row_count;
  snapshot->dependency_count = snapshot->dependencies_data.row_count;
}

static void snapshot_design_data(SEXP data, dependency_snapshot_t *snapshot,
    SEXP roots, R_xlen_t *work_since_interrupt) {
  const R_xlen_t column_count = XLENGTH(data);
  if (column_count != snapshot->parameter_count) {
    Rf_error("Design$data must have one column for every parameter");
  }

  SEXP source_names = PROTECT(paradox_api_raw_attribute(
    data,
    R_NamesSymbol
  ));
  snapshot->data_names = PROTECT(snapshot_string_vector(
    source_names,
    "Design column names",
    work_since_interrupt
  ));
  if (XLENGTH(snapshot->data_names) != column_count ||
      Rf_any_duplicated(snapshot->data_names, FALSE) != 0) {
    UNPROTECT(2);
    Rf_error("Design$data must have unique column names");
  }
  snapshot->columns = PROTECT(Rf_allocVector(VECSXP, column_count));
  SET_VECTOR_ELT(roots, 2, snapshot->data_names);
  SET_VECTOR_ELT(roots, 3, snapshot->columns);

  const int table_input = Rf_isObject(data) != FALSE;
  snapshot->row_count = 0;
  if (table_input &&
      !paradox_public_table_row_count(data, &snapshot->row_count)) {
    UNPROTECT(3);
    Rf_error("Design$data has invalid data.frame row names");
  }
  for (R_xlen_t column = 0; column < column_count; ++column) {
    paradox_account_work(work_since_interrupt);
    SEXP source = PROTECT(VECTOR_ELT(data, column));
    SEXP frozen = PROTECT(snapshot_column(source, work_since_interrupt));
    const R_xlen_t rows = XLENGTH(frozen);
    if (rows != snapshot->row_count && (table_input || column != 0)) {
      UNPROTECT(5);
      Rf_error(column == 0
        ? "Design$data has invalid data.frame row names"
        : "Design$data columns have inconsistent lengths");
    }
    if (!table_input && column == 0) {
      snapshot->row_count = rows;
    }
    SET_VECTOR_ELT(snapshot->columns, column, frozen);
    UNPROTECT(2);
  }
  UNPROTECT(3);

  if (snapshot->row_count > INT_MAX) {
    Rf_error("Design dependency result exceeds data.frame row limits");
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
    if (Rf_inherits(value, "TuneToken")) {
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
  if (parameter_count != 0 && row_count > R_XLEN_T_MAX / parameter_count) {
    Rf_error("Design dependency mask is too large");
  }
  const R_xlen_t bit_count = parameter_count * row_count;
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
      SEXP child_column = VECTOR_ELT(
        snapshot->columns,
        snapshot->column_by_parameter[child]
      );
      /*
       * TuneToken children skip all incoming edges, after the shared graph
       * planner above has already admitted topology and rejected cycles.
       */
      if (TYPEOF(child_column) == VECSXP &&
          Rf_inherits(VECTOR_ELT(child_column, row), "TuneToken")) {
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
        SEXP parent_column = VECTOR_ELT(
          snapshot->columns,
          snapshot->column_by_parameter[dependency->parent]
        );
        child_inactive = mask_get(
          snapshot,
          inactive,
          dependency->parent,
          row
        ) || value_is_missing(parent_column, row) || !condition_matches(
          dependency,
          parent_column,
          row,
          work_since_interrupt
        );
      }
      if (child_inactive) {
        mask_set(snapshot, inactive, child, row);
        ++inactive_counts[child];
      }
    }
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP result_names = PROTECT(Rf_allocVector(STRSXP, 3));
  SEXP rows = PROTECT(Rf_allocVector(VECSXP, patch_count));
  SEXP columns = PROTECT(Rf_allocVector(STRSXP, patch_count));
  SEXP values = PROTECT(Rf_allocVector(VECSXP, patch_count));
  SET_STRING_ELT(result_names, 0, Rf_mkChar("rows"));
  SET_STRING_ELT(result_names, 1, Rf_mkChar("columns"));
  SET_STRING_ELT(result_names, 2, Rf_mkChar("values"));
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  SET_VECTOR_ELT(result, 0, rows);
  SET_VECTOR_ELT(result, 1, columns);
  SET_VECTOR_ELT(result, 2, values);

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
    UNPROTECT(5);
    Rf_error("Internal error: incomplete Design dependency plan");
  }
  UNPROTECT(5);
  return result;
}

static SEXP empty_output(void) {
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, Rf_mkChar("rows"));
  SET_STRING_ELT(names, 1, Rf_mkChar("columns"));
  SET_STRING_ELT(names, 2, Rf_mkChar("values"));
  Rf_setAttrib(result, R_NamesSymbol, names);
  SET_VECTOR_ELT(result, 0, Rf_allocVector(VECSXP, 0));
  SET_VECTOR_ELT(result, 1, Rf_allocVector(STRSXP, 0));
  SET_VECTOR_ELT(result, 2, Rf_allocVector(VECSXP, 0));
  UNPROTECT(2);
  return result;
}

SEXP paradox_design_dependency_plan(SEXP data, SEXP param_set) {
  data = PROTECT(paradox_materialize_public_table_shell(data));
  PROTECT(param_set);
  if (!ordinary_design_shell(data)) {
    UNPROTECT(2);
    Rf_error("Design$data must be a list-like data frame");
  }
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, 4));
  PROTECT_INDEX graph_roots_index;
  SEXP graph_roots;
  PROTECT_WITH_INDEX(graph_roots = R_NilValue, &graph_roots_index);
  SEXP private = PROTECT(private_environment(param_set));

  dependency_snapshot_t snapshot = {0};
  R_xlen_t work_since_interrupt = 0;
  load_param_state(
    param_set,
    private,
    &graph_roots,
    graph_roots_index,
    &snapshot,
    roots,
    &work_since_interrupt
  );
  if (snapshot.dependency_count == 0) {
    if (Rf_isObject(data)) {
      R_xlen_t ignored_rows = 0;
      if (!paradox_public_table_row_count(data, &ignored_rows)) {
        UNPROTECT(5);
        Rf_error("Design$data has invalid data.frame row names");
      }
    }
    SEXP result = PROTECT(empty_output());
    UNPROTECT(6);
    return result;
  }
  snapshot_design_data(
    data,
    &snapshot,
    roots,
    &work_since_interrupt
  );
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
  UNPROTECT(6);
  return result;
}
