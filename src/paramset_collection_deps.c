#include <limits.h>

#include "paradox.h"

#include "paramset_collection_readers.h"
#include "paramset_domain_common.h"
#include "r_utils.h"

static SEXP set_plain_dependencies_attributes(SEXP result,
    R_xlen_t row_count) {
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, Rf_mkChar("id"));
  SET_STRING_ELT(names, 1, Rf_mkChar("on"));
  SET_STRING_ELT(names, 2, Rf_mkChar("cond"));
  Rf_setAttrib(result, R_NamesSymbol, names);

  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(classes, 0, Rf_mkChar("data.frame"));
  Rf_setAttrib(result, R_ClassSymbol, classes);

  SEXP row_names = PROTECT(Rf_allocVector(
    INTSXP,
    row_count == 0 ? 0 : 2
  ));
  if (row_count != 0) {
    SET_INTEGER_ELT(row_names, 0, NA_INTEGER);
    SET_INTEGER_ELT(row_names, 1, -(int) row_count);
  }
  Rf_setAttrib(result, R_RowNamesSymbol, row_names);
  UNPROTECT(3);
  return result;
}

static R_xlen_t find_id(SEXP ids, SEXP sought,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t count = XLENGTH(ids);
  for (R_xlen_t row = 0; row < count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP candidate = STRING_ELT(ids, row);
    if (candidate == sought || paradox_domain_strings_equal(candidate, sought)) {
      return row;
    }
  }
  return R_XLEN_T_MAX;
}

static SEXP translate_dependency_id(
    const paradox_collection_graph_t *graph,
    R_xlen_t node_index,
    SEXP input,
    R_xlen_t *work_since_interrupt) {
  SEXP current = input;
  while (graph->nodes[node_index].parent != R_XLEN_T_MAX) {
    const paradox_collection_graph_node_t *node = &graph->nodes[node_index];
    const paradox_collection_graph_node_t *parent =
      &graph->nodes[node->parent];
    const R_xlen_t row = find_id(
      node->params.ids,
      current,
      work_since_interrupt
    );
    if (row != R_XLEN_T_MAX) {
      if (node->parent_param_start > parent->params.row_count ||
          row >= parent->params.row_count - node->parent_param_start) {
        Rf_error("Internal error: invalid dependency translation path");
      }
      current = STRING_ELT(
        parent->params.ids,
        node->parent_param_start + row
      );
    }
    node_index = node->parent;
  }
  return current;
}

SEXP paradox_collection_dependencies_from_graph(
    const paradox_collection_graph_t *graph,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t row_count = graph->nodes[0].subtree_dependencies;
  if (row_count > INT_MAX) {
    Rf_error("ParamSetCollection dependency result exceeds data.frame limits");
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP ids = PROTECT(Rf_allocVector(STRSXP, row_count));
  SEXP on = PROTECT(Rf_allocVector(STRSXP, row_count));
  SEXP conditions = PROTECT(Rf_allocVector(VECSXP, row_count));
  SET_VECTOR_ELT(result, 0, ids);
  SET_VECTOR_ELT(result, 1, on);
  SET_VECTOR_ELT(result, 2, conditions);

  R_xlen_t output = 0;
  for (R_xlen_t order = 0; order < graph->postorder_count; ++order) {
    const R_xlen_t node_index = graph->postorder[order];
    const paradox_collection_graph_node_t *node = &graph->nodes[node_index];
    for (R_xlen_t row = 0; row < node->dependencies.row_count; ++row) {
      paradox_domain_account_work(work_since_interrupt);
      if (output >= row_count) {
        UNPROTECT(4);
        Rf_error("Internal error: dependency output exceeded its snapshot");
      }
      SET_STRING_ELT(
        ids,
        output,
        translate_dependency_id(
          graph,
          node_index,
          STRING_ELT(node->dependencies.ids, row),
          work_since_interrupt
        )
      );
      SET_STRING_ELT(
        on,
        output,
        translate_dependency_id(
          graph,
          node_index,
          STRING_ELT(node->dependencies.on, row),
          work_since_interrupt
        )
      );
      SEXP condition = PROTECT(Rf_duplicate(
        VECTOR_ELT(node->dependencies.conditions, row)
      ));
      SET_VECTOR_ELT(conditions, output, condition);
      UNPROTECT(1);
      ++output;
    }
  }
  if (output != row_count) {
    UNPROTECT(4);
    Rf_error("Internal error: incomplete dependency output snapshot");
  }
  set_plain_dependencies_attributes(result, row_count);
  UNPROTECT(4);
  return result;
}

static SEXP public_dependency_facade(SEXP table) {
  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(classes, 0, Rf_mkChar("data.table"));
  SET_STRING_ELT(classes, 1, Rf_mkChar("data.frame"));
  Rf_setAttrib(table, R_ClassSymbol, classes);
  SEXP result = PROTECT(paradox_prepare_data_table(table, TRUE));
  /* Reinstall the independently allocated names attribute after selfref
   * construction, matching the public data.table ownership boundary. */
  SEXP names = PROTECT(Rf_getAttrib(result, R_NamesSymbol));
  Rf_setAttrib(result, R_NamesSymbol, R_NilValue);
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(3);
  return result;
}

SEXP paradox_param_set_collection_deps(SEXP private_environment, SEXP self) {
  R_xlen_t work_since_interrupt = 0;
  PROTECT_INDEX roots_index;
  SEXP roots;
  PROTECT_WITH_INDEX(roots = R_NilValue, &roots_index);
  paradox_collection_graph_t graph;
  paradox_collection_graph_build(
    private_environment,
    self,
    &graph,
    &roots,
    roots_index,
    &work_since_interrupt
  );
  SEXP plain = PROTECT(paradox_collection_dependencies_from_graph(
    &graph,
    &work_since_interrupt
  ));
  SEXP result = PROTECT(public_dependency_facade(plain));
  UNPROTECT(3);
  return result;
}
