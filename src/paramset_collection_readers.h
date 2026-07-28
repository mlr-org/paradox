#ifndef PARADOX_PARAMSET_COLLECTION_READERS_H
#define PARADOX_PARAMSET_COLLECTION_READERS_H

#include "core_state.h"
#include "paramset_domain_common.h"

typedef struct {
  SEXP table;
  SEXP ids;
  SEXP original_ids;
  SEXP owner_indices;
  SEXP owner_names;
  R_xlen_t row_count;
} paradox_collection_translation_t;

typedef struct {
  SEXP self;
  SEXP private_environment;
  /* `core` is the operation-local semantic generation. During a read-only
   * migration preflight it may be an offside authoritative SHADOW preview.
   * `source_core` is the exact generation selected from the live private
   * binding and is therefore the generation used by mutation receipts. */
  SEXP core;
  SEXP source_core;
  SEXP state;
  paradox_core_kind_t kind;
  paradox_domain_params_t params;
  paradox_domain_dependencies_t dependencies;
  paradox_domain_values_t values;
  /* Operation-local rows proven while admitting `values`. Readers reuse
   * them instead of searching the same parameter IDs a second time. */
  R_xlen_t *value_param_rows;
  SEXP sets;
  SEXP set_names;
  paradox_collection_translation_t translation;
  R_xlen_t *translation_by_param;
  R_xlen_t parent;
  R_xlen_t parent_child;
  R_xlen_t parent_param_start;
  R_xlen_t next_child;
  R_xlen_t consumed_params;
  R_xlen_t subtree_dependencies;
  int postfix;
} paradox_collection_graph_node_t;

#define PARADOX_COLLECTION_GRAPH_INLINE_CAPACITY 16

typedef struct {
  paradox_collection_graph_node_t *nodes;
  R_xlen_t *path;
  R_xlen_t *postorder;
  R_xlen_t count;
  R_xlen_t postorder_count;
  R_xlen_t capacity;
  /* The common graph fits entirely on the caller's C stack.  The graph
   * builder switches the three public pointers to operation-local R_alloc
   * storage together when this capacity is exceeded; consumers never need to
   * distinguish the two representations. */
  paradox_collection_graph_node_t
    inline_nodes[PARADOX_COLLECTION_GRAPH_INLINE_CAPACITY];
  R_xlen_t inline_path[PARADOX_COLLECTION_GRAPH_INLINE_CAPACITY];
  R_xlen_t inline_postorder[PARADOX_COLLECTION_GRAPH_INLINE_CAPACITY];
} paradox_collection_graph_t;

/* The caller owns one PROTECT_WITH_INDEX slot for `roots`. Every selected
 * source capsule and every operation-local capsule preview is retained there,
 * so a callback that replaces a live SHADOW core cannot change either the
 * receipt or the semantic snapshot used by the enclosing operation. */
attribute_hidden void paradox_collection_graph_build(
  SEXP private_environment,
  SEXP self,
  paradox_collection_graph_t *graph,
  SEXP *roots,
  PROTECT_INDEX roots_index,
  R_xlen_t *work_since_interrupt
);

/* Migration preflight uses the same complete graph admission while previewing
 * stale SHADOW children offside instead of installing their refreshed cores. */
attribute_hidden void paradox_collection_graph_build_readonly(
  SEXP private_environment,
  SEXP self,
  paradox_collection_graph_t *graph,
  SEXP *roots,
  PROTECT_INDEX roots_index,
  R_xlen_t *work_since_interrupt
);

/* Admit one exact BASE/SHADOW node through the same dynamic-state validator
 * used by a collection graph. COLLECTION roots should use the complete graph
 * builder above so every translation edge is checked as well. */
attribute_hidden void paradox_collection_validate_single_node(
  SEXP private_environment,
  SEXP self,
  SEXP *roots,
  PROTECT_INDEX roots_index,
  R_xlen_t *work_since_interrupt
);

/* Both emitters consume only the frozen graph. The dependency result is a
 * canonical plain data.frame for internal composition; the public wrapper
 * installs the detached data.table facade exactly once at the boundary. */
attribute_hidden SEXP paradox_collection_values_from_graph(
  const paradox_collection_graph_t *graph,
  R_xlen_t *work_since_interrupt
);
attribute_hidden SEXP paradox_collection_dependencies_from_graph(
  const paradox_collection_graph_t *graph,
  R_xlen_t *work_since_interrupt
);

/* Field layout of the callback-detachment plan produced below and consumed
 * by the Shadow adapter builder. */
enum paradox_collection_detach_field {
  PARADOX_COLLECTION_DETACH_TRANSLATION = 0,
  PARADOX_COLLECTION_DETACH_CONSTRAINT_INDICES,
  PARADOX_COLLECTION_DETACH_CONSTRAINT_SETS,
  PARADOX_COLLECTION_DETACH_TRAFO_INDICES,
  PARADOX_COLLECTION_DETACH_TRAFO_SETS,
  PARADOX_COLLECTION_DETACH_POSTFIX,
  PARADOX_COLLECTION_DETACH_FIELD_COUNT
};

/* Construct the callback-detachment plan from the exact admitted graph.
 * This is the sole plan constructor: the registered live entry point routes
 * through the canonical graph build above and then enters here, so detachment
 * has no separate admission mode. In particular, migration preflight can
 * consume offside SHADOW previews without traversing live shells again or
 * committing a nested refresh. */
attribute_hidden SEXP paradox_param_set_collection_detach_plan_from_graph(
  const paradox_collection_graph_t *graph,
  SEXP requested
);

#endif
