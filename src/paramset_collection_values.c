#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "core_state.h"
#include "paramset_collection_readers.h"
#include "paramset_domain_common.h"
#include "paramset_params_internal.h"
#include "paramset_shadow.h"
#include "r_api_compat.h"
#include "r_utils.h"

static void retain_root(SEXP value, SEXP *roots,
    PROTECT_INDEX roots_index) {
  SEXP expanded = PROTECT(Rf_cons(value, *roots));
  REPROTECT(expanded, roots_index);
  *roots = expanded;
  UNPROTECT(1);
}

static int exact_flag(SEXP value, int *flag) {
  if (TYPEOF(value) != LGLSXP || ALTREP(value) || Rf_isS4(value) ||
      Rf_isObject(value) || XLENGTH(value) != 1 ||
      !paradox_api_has_no_attributes(value)) {
    return FALSE;
  }
  const int observed = LOGICAL_ELT(value, 0);
  if (observed == NA_LOGICAL) {
    return FALSE;
  }
  *flag = observed;
  return TRUE;
}

static int read_set_names(SEXP sets, SEXP *names) {
  if (TYPEOF(sets) != VECSXP || ALTREP(sets)) return FALSE;
  SEXP observed = Rf_getAttrib(sets, R_NamesSymbol);
  if (TYPEOF(observed) != STRSXP || ALTREP(observed) ||
      XLENGTH(observed) != XLENGTH(sets)) return FALSE;
  *names = observed;
  return TRUE;
}

static int read_translation(SEXP table, R_xlen_t child_count,
    R_xlen_t parameter_count, paradox_collection_translation_t *translation,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(table) != VECSXP || ALTREP(table) || XLENGTH(table) != 4) {
    return FALSE;
  }
  for (int column = 0; column < 4; ++column) {
    SEXP value = VECTOR_ELT(table, column);
    if (TYPEOF(value) != (column == 2 ? INTSXP : STRSXP) ||
        ALTREP(value) || XLENGTH(value) != parameter_count) return FALSE;
  }
  SEXP owners = VECTOR_ELT(table, 2);
  for (R_xlen_t row = 0; row < parameter_count; ++row) {
    paradox_account_work(work_since_interrupt);
    const int owner = INTEGER_ELT(owners, row);
    if (owner <= 0 || (R_xlen_t) owner > child_count) return FALSE;
  }
  *translation = (paradox_collection_translation_t) {
    table, VECTOR_ELT(table, 0), VECTOR_ELT(table, 1), owners,
    VECTOR_ELT(table, 3), parameter_count
  };
  return TRUE;
}

static int read_dynamic_state(paradox_collection_graph_node_t *node,
    unsigned int fields, R_xlen_t *work_since_interrupt) {
  const unsigned int columns = (fields & PARADOX_GRAPH_SCHEMA)
    ? PARADOX_PARAMS_COLUMNS_ALL
    : (fields & PARADOX_GRAPH_VALUES) ? 1U << PARADOX_DOMAIN_CLS : 0U;
  if (!paradox_domain_read_params(
      VECTOR_ELT(node->state, PARADOX_CORE_PARAMS), columns, &node->params)) {
    return FALSE;
  }
  if (fields & PARADOX_GRAPH_DEPENDENCIES) {
    SEXP dependencies = VECTOR_ELT(node->state, PARADOX_CORE_DEPS);
    const int valid = fields & PARADOX_GRAPH_SCHEMA
      ? paradox_domain_validate_dependencies(dependencies,
          &node->dependencies, work_since_interrupt)
      : paradox_domain_read_dependencies(dependencies, &node->dependencies);
    if (!valid) return FALSE;
  }
  if ((fields & PARADOX_GRAPH_VALUES) &&
      !paradox_domain_read_values(
        VECTOR_ELT(node->state, PARADOX_CORE_VALUES),
        &node->values, work_since_interrupt)) return FALSE;
  return TRUE;
}

static void reserve_graph(paradox_collection_graph_t *graph,
    R_xlen_t required) {
  if (required <= graph->capacity) {
    return;
  }
  R_xlen_t expanded_capacity = graph->capacity;
  while (expanded_capacity < required) {
    if (expanded_capacity > R_XLEN_T_MAX / 2) {
      Rf_error("ParamSetCollection capsule graph is too large");
    }
    expanded_capacity *= 2;
  }
  paradox_collection_graph_node_t *nodes = paradox_temporary_alloc(
    expanded_capacity,
    sizeof(*nodes)
  );
  R_xlen_t *path = paradox_temporary_alloc(
    expanded_capacity,
    sizeof(*path)
  );
  R_xlen_t *postorder = paradox_temporary_alloc(
    expanded_capacity,
    sizeof(*postorder)
  );
  memcpy(nodes, graph->nodes, (size_t) graph->count * sizeof(*nodes));
  memcpy(path, graph->path, (size_t) graph->count * sizeof(*path));
  memcpy(
    postorder,
    graph->postorder,
    (size_t) graph->postorder_count * sizeof(*postorder)
  );
  graph->nodes = nodes;
  graph->path = path;
  graph->postorder = postorder;
  graph->capacity = expanded_capacity;
}

static void initialize_graph(paradox_collection_graph_t *graph) {
  graph->capacity = PARADOX_COLLECTION_GRAPH_INLINE_CAPACITY;
  graph->nodes = graph->inline_nodes;
  graph->path = graph->inline_path;
  graph->postorder = graph->inline_postorder;
  graph->count = 0;
  graph->postorder_count = 0;
}

static int initialize_new_node(SEXP self, SEXP private_environment,
    SEXP operation_core, SEXP source_core, R_xlen_t parent,
    R_xlen_t parent_child,
    paradox_collection_graph_node_t *node, SEXP *roots,
    PROTECT_INDEX roots_index, R_xlen_t *work_since_interrupt,
    int retain_receipt, unsigned int fields) {
  if (!paradox_core_is_valid(operation_core) ||
      !paradox_core_is_valid(source_core)) {
    return FALSE;
  }
  PROTECT(operation_core);
  retain_root(operation_core, roots, roots_index);
  if (source_core != operation_core) {
    PROTECT(source_core);
    retain_root(source_core, roots, roots_index);
    UNPROTECT(1);
  }
  UNPROTECT(1);

  SEXP state = paradox_core_payload(operation_core);
  const paradox_core_kind_t kind = paradox_core_kind(operation_core);
  *node = (paradox_collection_graph_node_t) {
    .self = self,
    .private_environment = private_environment,
    .core = operation_core,
    .source_core = source_core,
    .shadow_signature = R_NilValue,
    .shadow_signature_content = R_NilValue,
    .state = state,
    .kind = kind,
    .value_param_rows = R_NilValue,
    .values = {R_NilValue, R_NilValue, 0},
    .dependencies = {R_NilValue, R_NilValue, R_NilValue, 0},
    .sets = R_NilValue,
    .set_names = R_NilValue,
    .translation = {R_NilValue, R_NilValue, R_NilValue, R_NilValue,
      R_NilValue, 0},
    .parent = parent,
    .parent_child = parent_child,
    .parent_param_start = 0,
    .next_child = 0,
    .consumed_params = 0,
    .subtree_dependencies = 0,
    .subtree_contributes = FALSE,
    .postfix = FALSE
  };
  if (kind != PARADOX_CORE_BASE && kind != PARADOX_CORE_COLLECTION &&
      kind != PARADOX_CORE_SHADOW) {
    return FALSE;
  }
  /*
   * A receipted operation must select the Shadow carrier before admitting the
   * dynamic payload. Otherwise allocation while copying the receipt could run
   * a finalizer that rewrites the carrier after admission, and the terminal
   * receipt would incorrectly bless that later generation. Capturing first
   * makes a mutation during admission visible to the terminal comparison;
   * a mutation during the receipt allocation itself is instead included in
   * the generation subsequently admitted below.
   */
  if (kind == PARADOX_CORE_SHADOW && retain_receipt) {
    SEXP signature = paradox_shadow_metadata_signature(source_core);
    if (signature == R_UnboundValue) {
      return FALSE;
    }
    PROTECT(signature);
    retain_root(signature, roots, roots_index);
    UNPROTECT(1);
    node->shadow_signature = signature;
    SEXP content = PROTECT(
      paradox_shadow_signature_content_snapshot(signature)
    );
    if (content == R_NilValue) {
      UNPROTECT(1);
      return FALSE;
    }
    retain_root(content, roots, roots_index);
    UNPROTECT(1);
    node->shadow_signature_content = content;
  }
  if (!read_dynamic_state(node, fields, work_since_interrupt)) {
    return FALSE;
  }
  if ((fields & PARADOX_GRAPH_VALUES) && kind != PARADOX_CORE_COLLECTION) {
    SEXP rows = PROTECT(paradox_domain_value_rows(
      node->params.ids, node->values.names, work_since_interrupt));
    if (rows != R_NilValue) retain_root(rows, roots, roots_index);
    node->value_param_rows = rows;
    UNPROTECT(1);
  }
  node->subtree_dependencies = node->dependencies.row_count;
  /* A callback is a contribution even on a set with no parameters: it runs
   * once per occurrence, so pruning a node that carries one would silently
   * drop those calls. */
  node->subtree_contributes = node->params.row_count != 0 ||
    node->dependencies.row_count != 0 || node->values.size != 0 ||
    VECTOR_ELT(state, PARADOX_CORE_EXTRA_TRAFO) != R_NilValue ||
    VECTOR_ELT(state, PARADOX_CORE_CONSTRAINT) != R_NilValue;

  if (kind != PARADOX_CORE_COLLECTION) return TRUE;
  SEXP sets = VECTOR_ELT(state, PARADOX_CORE_SETS);
  if (TYPEOF(sets) != VECSXP || ALTREP(sets) || XLENGTH(sets) > INT_MAX) {
    return FALSE;
  }
  node->sets = sets;
  if (fields & PARADOX_GRAPH_SCHEMA) {
    if (!read_set_names(sets, &node->set_names) ||
        !exact_flag(VECTOR_ELT(state, PARADOX_CORE_POSTFIX), &node->postfix) ||
        !read_translation(VECTOR_ELT(state, PARADOX_CORE_TRANSLATION),
          XLENGTH(sets), node->params.row_count, &node->translation,
          work_since_interrupt)) return FALSE;
  }
  return TRUE;
}

void paradox_collection_validate_single_node(SEXP private_environment,
    SEXP self, SEXP *roots, PROTECT_INDEX roots_index,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(private_environment) != ENVSXP ||
      Rf_isS4(private_environment) ||
      TYPEOF(self) != ENVSXP || Rf_isS4(self) ||
      !paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("Corrupt ParamSet child shell");
  }
  PROTECT_INDEX core_index;
  SEXP core;
  PROTECT_WITH_INDEX(
    core = paradox_core_from_private(private_environment),
    &core_index
  );
  if (!paradox_core_is_verified(core)) {
    REPROTECT(
      core = paradox_core_refresh(self, private_environment),
      core_index
    );
  }
  paradox_collection_graph_node_t node;
  if ((paradox_core_kind(core) != PARADOX_CORE_BASE &&
       paradox_core_kind(core) != PARADOX_CORE_SHADOW) ||
      !initialize_new_node(
        self,
        private_environment,
        core,
        core,
        R_XLEN_T_MAX,
        R_XLEN_T_MAX,
        &node,
        roots,
        roots_index,
        work_since_interrupt,
        FALSE, PARADOX_GRAPH_ALL
      )) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet child capsule state");
  }
  UNPROTECT(1);
}

/* The read-only sibling of the validator above: it admits the node's current
 * generation -- previewing a SHADOW's authoritative projection instead of
 * committing a refresh -- and installs nothing. Committing here is not only
 * unnecessary for admission, it is what turned a shared alternating
 * shadow/collection graph exponential: every install invalidates the
 * neighboring signatures, so each admission re-healed the complete subtree. */

static int initialize_node(SEXP self, SEXP private_environment,
    SEXP operation_core, SEXP source_core, R_xlen_t parent,
    R_xlen_t parent_child, R_xlen_t previous,
    const paradox_collection_graph_t *graph,
    paradox_collection_graph_node_t *node, SEXP *roots,
    PROTECT_INDEX roots_index, R_xlen_t *work_since_interrupt,
    int retain_receipt, unsigned int fields) {
  if (previous != R_XLEN_T_MAX) {
    if (previous >= graph->count || graph->nodes[previous].self != self) {
      return FALSE;
    }
    *node = graph->nodes[previous];
    node->parent = parent;
    node->parent_child = parent_child;
    node->parent_param_start = 0;
    node->next_child = 0;
    node->consumed_params = 0;
    node->subtree_dependencies = node->dependencies.row_count;
    node->subtree_contributes = node->params.row_count != 0 ||
      node->dependencies.row_count != 0 || node->values.size != 0 ||
      VECTOR_ELT(node->state, PARADOX_CORE_EXTRA_TRAFO) != R_NilValue ||
      VECTOR_ELT(node->state, PARADOX_CORE_CONSTRAINT) != R_NilValue;
    return TRUE;
  }
  return initialize_new_node(
    self,
    private_environment,
    operation_core,
    source_core,
    parent,
    parent_child,
    node,
    roots,
    roots_index,
    work_since_interrupt,
    retain_receipt, fields
  );
}

static int read_edge(paradox_collection_graph_node_t *parent,
    paradox_collection_graph_node_t *child) {
  const R_xlen_t child_rows = child->params.row_count;
  if (parent->consumed_params > parent->params.row_count ||
      child_rows > parent->params.row_count - parent->consumed_params) {
    return FALSE;
  }
  /* Public schema mutation maintains this concatenation. Its content is not
   * re-proved here; these range checks are what native consumers need. */
  child->parent_param_start = parent->consumed_params;
  parent->consumed_params += child_rows;
  return TRUE;
}

static void collection_graph_build(SEXP private_environment, SEXP self,
    paradox_collection_graph_t *graph, SEXP *roots,
    PROTECT_INDEX roots_index, R_xlen_t *work_since_interrupt,
    int commit_shadow_refreshes, int retain_receipt, unsigned int fields) {
  if (TYPEOF(private_environment) != ENVSXP ||
      Rf_isS4(private_environment) ||
      TYPEOF(self) != ENVSXP || Rf_isS4(self)) {
    Rf_error("Corrupt ParamSetCollection shell");
  }

  /* Public child changes must be reflected before selecting operation views.
   * A verified graph needs no refresh. Non-installing cold previews leave
   * the live shells untouched and select offside Shadow projections below. */
  SEXP selected = paradox_core_from_private(private_environment);
  if (commit_shadow_refreshes && selected != R_UnboundValue &&
      !paradox_core_is_verified(selected)) {
    selected = paradox_core_refresh(self, private_environment);
  }
  /*
   * Every selected capsule is immutable and rooted, but admitting a later
   * child allocates and may run a pending finalizer. Without one session-wide
   * generation barrier, the resulting graph could combine child generations
   * from supported mutations that never coexisted. Semantic installations
   * advance this epoch; authoritative cache refreshes do not because they
   * preserve the graph's denotation.
   */
  const uintptr_t entry_epoch = paradox_core_state_epoch_value();

  /* Choose and root the root capsule before any shell traversal or callback.
   * The private argument comes directly from the package active binding. */
  SEXP root_core = PROTECT(selected);
  if (root_core == R_UnboundValue ||
      paradox_core_kind(root_core) != PARADOX_CORE_COLLECTION) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSetCollection root capsule");
  }
  retain_root(root_core, roots, roots_index);
  SEXP owned_private = PROTECT(
    paradox_domain_required_private_environment(self)
  );
  if (owned_private != private_environment) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSetCollection shell ownership");
  }
  retain_root(self, roots, roots_index);

  initialize_graph(graph);
  if (!initialize_new_node(
      self,
      private_environment,
      root_core,
      root_core,
      R_XLEN_T_MAX,
      R_XLEN_T_MAX,
      &graph->nodes[0],
      roots,
      roots_index,
      work_since_interrupt,
      retain_receipt, fields
    )) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSetCollection root state");
  }
  UNPROTECT(2);
  graph->count = 1;
  graph->path[0] = 0;
  R_xlen_t depth = 1;

  while (depth != 0) {
    const R_xlen_t node_index = graph->path[depth - 1];
    paradox_collection_graph_node_t *node = &graph->nodes[node_index];
    if (node->kind == PARADOX_CORE_COLLECTION &&
        node->next_child < XLENGTH(node->sets)) {
      const R_xlen_t child_position = node->next_child;
      SEXP child_self = PROTECT(VECTOR_ELT(node->sets, child_position));
      if (TYPEOF(child_self) != ENVSXP || Rf_isS4(child_self)) {
        UNPROTECT(1);
        Rf_error("Corrupt ParamSetCollection child reference");
      }
      for (R_xlen_t ancestor = 0; ancestor < depth; ++ancestor) {
        paradox_account_work(work_since_interrupt);
        if (graph->nodes[graph->path[ancestor]].self == child_self) {
          UNPROTECT(1);
          Rf_error("ParamSetCollection capsule graph contains a cycle");
        }
      }

      reserve_graph(graph, graph->count + 1);
      const R_xlen_t child_node_index = graph->count;
      R_xlen_t previous_node = R_XLEN_T_MAX;
      for (R_xlen_t previous = 0; previous < graph->count; ++previous) {
        paradox_account_work(work_since_interrupt);
        if (graph->nodes[previous].self == child_self) {
          previous_node = previous;
          break;
        }
      }
      const int reused = previous_node != R_XLEN_T_MAX;
      /* A shared subtree that exposes no parameter, no dependency, and no
       * value contributes nothing that can differ between two occurrences of
       * it, and the first occurrence already validated every node in it.
       * Re-descending it is what made an alternating shared graph cost
       * Theta(2^depth) nodes -- and therefore memory -- to produce an empty
       * result.  A subtree that does contribute is still expanded per
       * occurrence, because each occurrence exposes its own affixed IDs. */
      if (reused && !graph->nodes[previous_node].subtree_contributes) {
        ++graph->nodes[node_index].next_child;
        UNPROTECT(1);
        continue;
      }
      SEXP child_private = R_UnboundValue;
      SEXP child_source_core = R_UnboundValue;
      SEXP child_operation_core = R_UnboundValue;
      if (!reused) {
        child_private = PROTECT(
          paradox_domain_required_private_environment(child_self)
        );
        if (child_private == R_UnboundValue) {
          UNPROTECT(2);
          Rf_error("Corrupt ParamSetCollection child shell");
        }
        child_source_core = PROTECT(paradox_core_from_private(child_private));
        PROTECT_INDEX operation_core_index;
        PROTECT_WITH_INDEX(
          child_operation_core = child_source_core,
          &operation_core_index
        );
        if (paradox_core_kind(child_source_core) == PARADOX_CORE_SHADOW) {
          SEXP authoritative_core = commit_shadow_refreshes
            ? paradox_core_refresh(child_self, child_private)
            : paradox_shadow_preview_authoritative(child_self, child_private);
          REPROTECT(
            child_operation_core = authoritative_core,
            operation_core_index
          );
          if (commit_shadow_refreshes) {
            /* A committed refresh deliberately changed the selected binding:
             * its replacement is now both the source receipt and the semantic
             * generation. A read-only preview must retain the old source. */
            child_source_core = child_operation_core;
          }
        }
      }
      const int valid = initialize_node(
          child_self,
          child_private,
          child_operation_core,
          child_source_core,
          node_index,
          child_position,
          previous_node,
          graph,
          &graph->nodes[child_node_index],
          roots,
          roots_index,
          work_since_interrupt,
          retain_receipt, fields
        ) && read_edge(
          &graph->nodes[node_index],
          &graph->nodes[child_node_index]
        );
      if (!reused) {
        UNPROTECT(3);
      }
      UNPROTECT(1);
      if (!valid) {
        Rf_error("Corrupt ParamSetCollection child capsule state");
      }
      ++graph->nodes[node_index].next_child;
      ++graph->count;
      graph->path[depth] = child_node_index;
      ++depth;
      continue;
    }

    if (node->kind == PARADOX_CORE_COLLECTION &&
        node->consumed_params != node->params.row_count) {
      Rf_error("Corrupt ParamSetCollection translation coverage");
    }
    if (node->parent != R_XLEN_T_MAX) {
      paradox_collection_graph_node_t *parent = &graph->nodes[node->parent];
      if (node->subtree_dependencies >
          R_XLEN_T_MAX - parent->subtree_dependencies) {
        Rf_error("ParamSetCollection dependency result is too large");
      }
      parent->subtree_dependencies += node->subtree_dependencies;
      if (node->subtree_contributes) {
        parent->subtree_contributes = TRUE;
      }
    }
    graph->postorder[graph->postorder_count++] = node_index;
    --depth;
  }
  if (graph->nodes[0].subtree_dependencies > INT_MAX) {
    Rf_error("ParamSetCollection dependency result exceeds data.frame limits");
  }
  if (paradox_core_state_epoch_value() != entry_epoch) {
    Rf_error("ParamSetCollection graph changed while being snapshotted");
  }
}

void paradox_collection_graph_build(SEXP private_environment, SEXP self,
    unsigned int fields,
    paradox_collection_graph_t *graph, SEXP *roots,
    PROTECT_INDEX roots_index, R_xlen_t *work_since_interrupt) {
  collection_graph_build(
    private_environment,
    self,
    graph,
    roots,
    roots_index,
    work_since_interrupt,
    TRUE,
    FALSE, fields
  );
}

void paradox_collection_graph_build_receipted(
    SEXP private_environment, SEXP self,
    paradox_collection_graph_t *graph, SEXP *roots,
    PROTECT_INDEX roots_index, R_xlen_t *work_since_interrupt) {
  collection_graph_build(
    private_environment,
    self,
    graph,
    roots,
    roots_index,
    work_since_interrupt,
    TRUE,
    TRUE, PARADOX_GRAPH_ALL
  );
}


void paradox_collection_graph_build_readonly_receipted(
    SEXP private_environment, SEXP self,
    paradox_collection_graph_t *graph, SEXP *roots,
    PROTECT_INDEX roots_index, R_xlen_t *work_since_interrupt) {
  collection_graph_build(
    private_environment,
    self,
    graph,
    roots,
    roots_index,
    work_since_interrupt,
    FALSE,
    TRUE, PARADOX_GRAPH_ALL
  );
}

int paradox_collection_graph_snapshot_is_intact(
    const paradox_collection_graph_t *graph,
    R_xlen_t *work_since_interrupt) {
  if (graph == NULL || graph->count == 0) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < graph->count; ++index) {
    paradox_account_work(work_since_interrupt);
    const paradox_collection_graph_node_t *node = &graph->nodes[index];
    if (paradox_core_kind(node->core) != node->kind ||
        paradox_core_payload(node->core) != node->state ||
        (node->kind == PARADOX_CORE_SHADOW &&
          !paradox_shadow_signature_receipt_is_current(
            node->source_core,
            node->shadow_signature,
            node->shadow_signature_content
          )) ||
        (node->kind != PARADOX_CORE_SHADOW &&
          (node->shadow_signature != R_NilValue ||
           node->shadow_signature_content != R_NilValue))) {
      return FALSE;
    }
  }
  return TRUE;
}

/* Stored names have already been resolved to local parameter rows. Public
 * construction concatenates child rows; the walker checks the extents needed
 * to follow that layout, not private parent/child semantic equality. */
static SEXP translate_value_row(const paradox_collection_graph_t *graph,
    R_xlen_t node_index, R_xlen_t row) {
  while (graph->nodes[node_index].parent != R_XLEN_T_MAX) {
    const paradox_collection_graph_node_t *node = &graph->nodes[node_index];
    const paradox_collection_graph_node_t *parent =
      &graph->nodes[node->parent];
    if (row >= node->params.row_count ||
        node->parent_param_start > parent->params.row_count ||
        row >= parent->params.row_count - node->parent_param_start) {
      Rf_error("Internal error: invalid collection value translation path");
    }
    row += node->parent_param_start;
    node_index = node->parent;
  }
  const paradox_collection_graph_node_t *root = &graph->nodes[node_index];
  if (row >= root->params.row_count) {
    Rf_error("Internal error: invalid collection value translation row");
  }
  return STRING_ELT(root->params.ids, row);
}

SEXP paradox_collection_values_from_graph(
    const paradox_collection_graph_t *graph,
    int detach,
    R_xlen_t *work_since_interrupt) {
  R_xlen_t output_size = 0;
  for (R_xlen_t node_index = 0; node_index < graph->count; ++node_index) {
    const paradox_collection_graph_node_t *node = &graph->nodes[node_index];
    if (node->kind != PARADOX_CORE_COLLECTION) {
      if (node->values.size > R_XLEN_T_MAX - output_size) {
        Rf_error("ParamSetCollection value result is too large");
      }
      output_size += node->values.size;
    }
  }

  /* Detaching a typed leaf can enter an ALTREP provider. Retain the selected
   * carriers independently of their mutable private owners across that call.
   * The internal collector only moves ordinary list/string elements and does
   * not need this additional snapshot. Row maps are already graph roots. */
  SEXP selected = R_NilValue;
  if (detach) {
    if (graph->count > (R_XLEN_T_MAX - 1) / 2) {
      Rf_error("ParamSetCollection value snapshot is too large");
    }
    selected = PROTECT(Rf_allocVector(VECSXP, 2 * graph->count + 1));
    SET_VECTOR_ELT(selected, 0, graph->nodes[0].params.ids);
    for (R_xlen_t index = 0; index < graph->count; ++index) {
      SET_VECTOR_ELT(selected, 2 * index + 1, graph->nodes[index].values.values);
      SET_VECTOR_ELT(selected, 2 * index + 2, graph->nodes[index].params.classes);
    }
  } else {
    PROTECT(selected);
  }
  SEXP result = PROTECT(Rf_allocVector(VECSXP, output_size));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, output_size));
  R_xlen_t output = 0;
  for (R_xlen_t node_index = 0; node_index < graph->count; ++node_index) {
    const paradox_collection_graph_node_t *node = &graph->nodes[node_index];
    if (node->kind == PARADOX_CORE_COLLECTION) {
      continue;
    }
    for (R_xlen_t index = 0; index < node->values.size; ++index) {
      paradox_account_work(work_since_interrupt);
      const R_xlen_t row = paradox_domain_value_row(node->value_param_rows, index);
      SEXP value = VECTOR_ELT(node->values.values, index);
      if (detach) {
        value = paradox_detach_stored_value_leaf(value,
          !paradox_domain_string_is(STRING_ELT(node->params.classes, row),
            "ParamUty"));
      }
      PROTECT(value);
      SET_VECTOR_ELT(result, output, value);
      SET_STRING_ELT(
        names,
        output,
        translate_value_row(
          graph,
          node_index,
          row
        )
      );
      UNPROTECT(1);
      ++output;
    }
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(3);
  return result;
}

SEXP paradox_param_set_collection_values(SEXP private_environment, SEXP self) {
  R_xlen_t work_since_interrupt = 0;
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("ParamSet value reader called with a foreign private environment");
  }
  PROTECT_INDEX core_index;
  SEXP core;
  PROTECT_WITH_INDEX(
    core = paradox_core_from_private(private_environment),
    &core_index
  );
  if (core == R_UnboundValue) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet value state: missing core capsule");
  }
  paradox_core_kind_t kind = paradox_core_kind(core);
  if (kind != PARADOX_CORE_COLLECTION &&
      !paradox_core_is_verified(core)) {
    REPROTECT(
      core = paradox_core_refresh(self, private_environment),
      core_index
    );
    kind = paradox_core_kind(core);
  }
  if (kind != PARADOX_CORE_BASE && kind != PARADOX_CORE_COLLECTION &&
      kind != PARADOX_CORE_SHADOW) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet value capsule kind");
  }

  if (kind != PARADOX_CORE_COLLECTION) {
    SEXP state = paradox_core_payload(core);
    paradox_domain_params_t params;
    paradox_domain_values_t values;
    if (state == R_UnboundValue ||
        !paradox_domain_read_params(
          VECTOR_ELT(state, PARADOX_CORE_PARAMS),
          1U << PARADOX_DOMAIN_CLS, &params)) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSet value capsule state");
    }
    PROTECT(params.ids);
    PROTECT(params.classes);
    if (!paradox_domain_read_values(
          VECTOR_ELT(state, PARADOX_CORE_VALUES),
          &values,
          &work_since_interrupt
        )) {
      UNPROTECT(3);
      Rf_error("Corrupt ParamSet value capsule state");
    }
    SEXP result = PROTECT(paradox_detach_named_values(
      &values,
      &params,
      &work_since_interrupt
    ));
    UNPROTECT(4);
    return result;
  }

  PROTECT_INDEX roots_index;
  SEXP roots;
  PROTECT_WITH_INDEX(roots = R_NilValue, &roots_index);
  paradox_collection_graph_t graph;
  paradox_collection_graph_build(
    private_environment,
    self,
    PARADOX_GRAPH_VALUES,
    &graph,
    &roots,
    roots_index,
    &work_since_interrupt
  );
  SEXP result = PROTECT(paradox_collection_values_from_graph(
    &graph,
    TRUE,
    &work_since_interrupt
  ));
  UNPROTECT(3);
  return result;
}
