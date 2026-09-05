#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "core_state.h"
#include "paramset_collection_readers.h"
#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

enum detach_translation_column {
  DETACH_TRANSLATION_ID = 0,
  DETACH_TRANSLATION_ORIGINAL_ID,
  DETACH_TRANSLATION_OWNER,
  DETACH_TRANSLATION_OWNER_NAME,
  DETACH_TRANSLATION_PREFIX,
  DETACH_TRANSLATION_SUFFIX,
  DETACH_TRANSLATION_COLUMN_COUNT
};

typedef struct {
  SEXP self;
  SEXP private_environment;
  SEXP core;
  SEXP payload;
  paradox_core_kind_t kind;
  paradox_domain_params_t params;
  SEXP sets;
  SEXP set_names;
  SEXP translation;
  SEXP translation_ids;
  SEXP translation_original_ids;
  SEXP translation_owners;
  SEXP translation_owner_names;
  R_xlen_t translation_rows;
  int postfix;
} detach_node_t;

typedef struct {
  detach_node_t *nodes;
  R_xlen_t count;
  R_xlen_t capacity;
} detach_graph_t;

typedef struct {
  R_xlen_t leaf;
  R_xlen_t depth;
  R_xlen_t *collection_nodes;
  R_xlen_t *owners;
  SEXP prefix;
  SEXP suffix;
  SEXP constraint;
  SEXP trafo;
} detach_unit_t;

static void append_root(SEXP *roots, PROTECT_INDEX roots_index, SEXP value) {
  PROTECT(value);
  SEXP expanded = PROTECT(Rf_cons(value, *roots));
  REPROTECT(expanded, roots_index);
  *roots = expanded;
  UNPROTECT(2);
}

static R_xlen_t find_node(const detach_graph_t *graph, SEXP self) {
  for (R_xlen_t index = 0; index < graph->count; ++index) {
    if (graph->nodes[index].self == self) {
      return index;
    }
  }
  return R_XLEN_T_MAX;
}

static void import_admitted_graph(detach_graph_t *graph,
    const paradox_collection_graph_t *source, SEXP *roots,
    PROTECT_INDEX roots_index) {
  if (source == NULL) {
    Rf_error("Corrupt admitted ParamSetCollection graph");
    /* `Rf_error()` does not return; keep static analyzers on that path. */
    return;
  }
  const R_xlen_t count = source->count;
  if (count == 0 ||
      (size_t) count > SIZE_MAX / sizeof(*graph->nodes)) {
    Rf_error("Corrupt admitted ParamSetCollection graph");
  }
  graph->capacity = count;
  graph->nodes = paradox_temporary_alloc(
    graph->capacity,
    sizeof(*graph->nodes)
  );
  graph->count = 0;

  for (R_xlen_t index = 0; index < count; ++index) {
    const paradox_collection_graph_node_t *selected =
      &source->nodes[index];
    const R_xlen_t existing = find_node(graph, selected->self);
    if (existing != R_XLEN_T_MAX) {
      const detach_node_t *retained = &graph->nodes[existing];
      if (retained->private_environment != selected->private_environment ||
          retained->core != selected->core ||
          retained->payload != selected->state ||
          retained->kind != selected->kind) {
        Rf_error("Corrupt shared ParamSetCollection graph snapshot");
      }
      continue;
    }

    SEXP constraint = VECTOR_ELT(
      selected->state,
      PARADOX_CORE_CONSTRAINT
    );
    SEXP trafo = VECTOR_ELT(
      selected->state,
      PARADOX_CORE_EXTRA_TRAFO
    );
    if ((constraint != R_NilValue && !Rf_isFunction(constraint)) ||
        (trafo != R_NilValue && !Rf_isFunction(trafo)) ||
        (selected->kind == PARADOX_CORE_COLLECTION &&
         (constraint != R_NilValue || trafo != R_NilValue))) {
      Rf_error("Corrupt ParamSet callback capsule");
    }

    append_root(roots, roots_index, selected->self);
    append_root(roots, roots_index, selected->private_environment);
    append_root(roots, roots_index, selected->core);
    if (selected->source_core != selected->core) {
      append_root(roots, roots_index, selected->source_core);
    }
    detach_node_t *node = &graph->nodes[graph->count++];
    *node = (detach_node_t) {
      .self = selected->self,
      .private_environment = selected->private_environment,
      .core = selected->core,
      .payload = selected->state,
      .kind = selected->kind,
      .params = selected->params,
      .sets = selected->sets,
      .set_names = selected->set_names,
      .translation = selected->translation.table,
      .translation_ids = selected->translation.ids,
      .translation_original_ids = selected->translation.original_ids,
      .translation_owners = selected->translation.owner_indices,
      .translation_owner_names = selected->translation.owner_names,
      .translation_rows = selected->translation.row_count,
      .postfix = selected->postfix
    };
  }
}

static SEXP materialize_requested(SEXP requested) {
  if (requested == R_NilValue) {
    return R_NilValue;
  }
  if (TYPEOF(requested) != STRSXP) {
    Rf_error("`ids` must be a character vector");
  }
  const R_xlen_t size = XLENGTH(requested);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP id = STRING_ELT(requested, index);
    if (!paradox_charsxp_is_ordinary(id)) {
      UNPROTECT(1);
      Rf_error("`ids` must not contain missing or bytes-encoded strings");
    }
    SET_STRING_ELT(result, index, id);
  }
  UNPROTECT(1);
  return result;
}

static int id_selected(SEXP selected_ids, SEXP id) {
  if (selected_ids == R_NilValue) {
    return TRUE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(selected_ids); ++index) {
    if (paradox_domain_strings_equal(STRING_ELT(selected_ids, index), id)) {
      return TRUE;
    }
  }
  return FALSE;
}

static R_xlen_t translation_row(const detach_node_t *node, SEXP id) {
  for (R_xlen_t row = 0; row < node->translation_rows; ++row) {
    if (paradox_domain_strings_equal(
        STRING_ELT(node->translation_ids, row),
        id
      )) {
      return row;
    }
  }
  Rf_error("Corrupt ParamSetCollection route: unknown parameter id");
}

static int same_unit(const detach_unit_t *unit, R_xlen_t leaf,
    const R_xlen_t *collection_nodes, const R_xlen_t *owners,
    R_xlen_t depth) {
  if (unit->leaf != leaf || unit->depth != depth) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < depth; ++index) {
    if (unit->collection_nodes[index] != collection_nodes[index] ||
        unit->owners[index] != owners[index]) {
      return FALSE;
    }
  }
  return TRUE;
}

static SEXP make_affix(const detach_graph_t *graph,
    const R_xlen_t *collection_nodes, const R_xlen_t *owners,
    R_xlen_t depth, int prefix) {
  size_t *measured = paradox_temporary_alloc(
    depth == 0 ? 1 : depth,
    sizeof(*measured)
  );
  size_t bytes = 0;
  const void *vmax = vmaxget();
  for (R_xlen_t step = 0; step < depth; ++step) {
    measured[step] = 0;
    const R_xlen_t path = prefix ? step : depth - step - 1;
    const detach_node_t *node = &graph->nodes[collection_nodes[path]];
    if (node->postfix == prefix) {
      continue;
    }
    SEXP owner = STRING_ELT(node->set_names, owners[path]);
    if (paradox_domain_string_is(owner, "")) {
      continue;
    }
    const size_t size = strlen(Rf_translateCharUTF8(owner));
    if (size > (size_t) INT_MAX || bytes > (size_t) INT_MAX - size - 1) {
      vmaxset(vmax);
      Rf_error("ParamSetCollection affix is too long");
    }
    measured[step] = size;
    bytes += size + 1;
  }
  char *buffer = paradox_temporary_alloc(
    (R_xlen_t) bytes + 1,
    sizeof(*buffer)
  );
  size_t output = 0;
  for (R_xlen_t step = 0; step < depth; ++step) {
    const R_xlen_t path = prefix ? step : depth - step - 1;
    const detach_node_t *node = &graph->nodes[collection_nodes[path]];
    if (node->postfix == prefix) {
      continue;
    }
    SEXP owner = STRING_ELT(node->set_names, owners[path]);
    if (paradox_domain_string_is(owner, "")) {
      continue;
    }
    const char *text = Rf_translateCharUTF8(owner);
    const size_t size = strlen(text);
    if (size != measured[step] || output >= bytes ||
        size > bytes - output - 1U) {
      vmaxset(vmax);
      Rf_error("ParamSetCollection affix changed while being copied");
    }
    if (prefix) {
      memcpy(buffer + output, text, size);
      output += size;
      buffer[output++] = '.';
    } else {
      buffer[output++] = '.';
      memcpy(buffer + output, text, size);
      output += size;
    }
  }
  if (output != bytes) {
    vmaxset(vmax);
    Rf_error("ParamSetCollection affix changed while being copied");
  }
  buffer[output] = '\0';
  SEXP result = Rf_mkCharLenCE(buffer, (int) output, CE_UTF8);
  vmaxset(vmax);
  return result;
}

static int expected_root_id(SEXP root_id, SEXP prefix, SEXP local_id,
    SEXP suffix) {
  PROTECT(root_id);
  PROTECT(prefix);
  PROTECT(local_id);
  PROTECT(suffix);
  const void *vmax = vmaxget();
  const size_t prefix_size = strlen(Rf_translateCharUTF8(prefix));
  const size_t local_size = strlen(Rf_translateCharUTF8(local_id));
  const size_t suffix_size = strlen(Rf_translateCharUTF8(suffix));
  if (prefix_size > (size_t) INT_MAX || local_size > (size_t) INT_MAX ||
      suffix_size > (size_t) INT_MAX ||
      prefix_size > (size_t) INT_MAX - local_size ||
      prefix_size + local_size > (size_t) INT_MAX - suffix_size) {
    vmaxset(vmax);
    UNPROTECT(4);
    return FALSE;
  }
  const size_t size = prefix_size + local_size + suffix_size;
  char *buffer = paradox_temporary_alloc(
    (R_xlen_t) size + 1,
    sizeof(*buffer)
  );
  const char *translated = Rf_translateCharUTF8(prefix);
  if (strlen(translated) != prefix_size) {
    vmaxset(vmax);
    UNPROTECT(4);
    return FALSE;
  }
  memcpy(buffer, translated, prefix_size);
  translated = Rf_translateCharUTF8(local_id);
  if (strlen(translated) != local_size) {
    vmaxset(vmax);
    UNPROTECT(4);
    return FALSE;
  }
  memcpy(buffer + prefix_size, translated, local_size);
  translated = Rf_translateCharUTF8(suffix);
  if (strlen(translated) != suffix_size) {
    vmaxset(vmax);
    UNPROTECT(4);
    return FALSE;
  }
  memcpy(buffer + prefix_size + local_size, translated, suffix_size);
  buffer[size] = '\0';
  SEXP expected = PROTECT(Rf_mkCharLenCE(buffer, (int) size, CE_UTF8));
  const int equal = paradox_domain_strings_equal(root_id, expected);
  vmaxset(vmax);
  UNPROTECT(5);
  return equal;
}

static SEXP allocate_translation(R_xlen_t size) {
  static const char *const column_names[] = {
    "id", "original_id", "owner_ps_index", "owner_name", ".prefix",
    ".suffix"
  };
  if (size > (R_xlen_t) INT_MAX) {
    Rf_error("ParamSetCollection detachment plan is too large");
  }
  SEXP result = PROTECT(Rf_allocVector(
    VECSXP,
    DETACH_TRANSLATION_COLUMN_COUNT
  ));
  SET_VECTOR_ELT(result, DETACH_TRANSLATION_ID, Rf_allocVector(STRSXP, size));
  SET_VECTOR_ELT(
    result,
    DETACH_TRANSLATION_ORIGINAL_ID,
    Rf_allocVector(STRSXP, size)
  );
  SET_VECTOR_ELT(
    result,
    DETACH_TRANSLATION_OWNER,
    Rf_allocVector(INTSXP, size)
  );
  SET_VECTOR_ELT(
    result,
    DETACH_TRANSLATION_OWNER_NAME,
    Rf_allocVector(STRSXP, size)
  );
  SET_VECTOR_ELT(
    result,
    DETACH_TRANSLATION_PREFIX,
    Rf_allocVector(STRSXP, size)
  );
  SET_VECTOR_ELT(
    result,
    DETACH_TRANSLATION_SUFFIX,
    Rf_allocVector(STRSXP, size)
  );
  SEXP names = PROTECT(Rf_allocVector(
    STRSXP,
    DETACH_TRANSLATION_COLUMN_COUNT
  ));
  for (R_xlen_t column = 0;
      column < DETACH_TRANSLATION_COLUMN_COUNT;
      ++column) {
    SET_STRING_ELT(names, column, Rf_mkChar(column_names[column]));
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  SEXP classes = PROTECT(Rf_mkString("data.frame"));
  Rf_setAttrib(result, R_ClassSymbol, classes);
  SEXP row_names = PROTECT(Rf_allocVector(INTSXP, size));
  for (R_xlen_t row = 0; row < size; ++row) {
    INTEGER(row_names)[row] = (int) (row + 1);
  }
  Rf_setAttrib(result, R_RowNamesSymbol, row_names);
  UNPROTECT(4);
  return result;
}

static SEXP allocate_carriers(R_xlen_t size, const char *field,
    int include_source) {
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, size));
  Rf_setAttrib(result, R_NamesSymbol, names);
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP carrier = PROTECT(Rf_allocVector(
      VECSXP,
      include_source ? 2 : 1
    ));
    SEXP carrier_names = PROTECT(Rf_allocVector(
      STRSXP,
      include_source ? 2 : 1
    ));
    SET_STRING_ELT(carrier_names, 0, Rf_mkChar(field));
    if (include_source) {
      SET_STRING_ELT(carrier_names, 1, Rf_mkChar(".core"));
    }
    Rf_setAttrib(carrier, R_NamesSymbol, carrier_names);
    SET_VECTOR_ELT(result, index, carrier);
    SET_STRING_ELT(names, index, R_BlankString);
    UNPROTECT(2);
  }
  UNPROTECT(2);
  return result;
}

static SEXP collection_detach_plan(SEXP requested,
    const paradox_collection_graph_t *admitted_graph) {
  PROTECT_INDEX roots_index;
  SEXP roots;
  PROTECT_WITH_INDEX(roots = R_NilValue, &roots_index);
  SEXP requested_snapshot = PROTECT(materialize_requested(requested));

  detach_graph_t graph;
  import_admitted_graph(
    &graph,
    admitted_graph,
    &roots,
    roots_index
  );
  if (graph.nodes[0].kind != PARADOX_CORE_COLLECTION) {
    UNPROTECT(2);
    Rf_error("Detachment requires a ParamSetCollection capsule");
  }
  const detach_node_t *root = &graph.nodes[0];
  if (requested_snapshot != R_NilValue) {
    for (R_xlen_t request = 0;
        request < XLENGTH(requested_snapshot);
        ++request) {
      if (!id_selected(
          root->params.ids,
          STRING_ELT(requested_snapshot, request)
        )) {
        UNPROTECT(2);
        Rf_error("Unknown ParamSetCollection subset id");
      }
    }
  }

  R_xlen_t selected_count = 0;
  for (R_xlen_t row = 0; row < root->params.row_count; ++row) {
    selected_count += id_selected(
      requested_snapshot,
      STRING_ELT(root->params.ids, row)
    );
  }
  if (selected_count > INT_MAX) {
    UNPROTECT(2);
    Rf_error("ParamSetCollection detachment plan is too large");
  }
  R_xlen_t *selected_rows = paradox_temporary_alloc(
    selected_count,
    sizeof(*selected_rows)
  );
  R_xlen_t *selected_units = paradox_temporary_alloc(
    selected_count,
    sizeof(*selected_units)
  );
  SEXP *selected_local_ids = paradox_temporary_alloc(
    selected_count,
    sizeof(*selected_local_ids)
  );
  detach_unit_t *units = paradox_temporary_alloc(
    selected_count,
    sizeof(*units)
  );
  /* One route scratch pair for the whole scan. Each iteration rewrites both
   * from index zero before reading them, and an admitted unit keeps its own
   * copy, so allocating them per selected row only raised the retained peak
   * to selected_count times the node count. */
  R_xlen_t *collection_nodes = paradox_temporary_alloc(
    graph.count,
    sizeof(*collection_nodes)
  );
  R_xlen_t *owners = paradox_temporary_alloc(
    graph.count,
    sizeof(*owners)
  );
  R_xlen_t unit_count = 0;
  R_xlen_t output = 0;
  for (R_xlen_t root_row = 0;
      root_row < root->params.row_count;
      ++root_row) {
    SEXP root_id = STRING_ELT(root->params.ids, root_row);
    if (!id_selected(requested_snapshot, root_id)) {
      continue;
    }
    R_xlen_t current = 0;
    SEXP local_id = root_id;
    R_xlen_t depth = 0;
    while (graph.nodes[current].kind == PARADOX_CORE_COLLECTION) {
      const detach_node_t *node = &graph.nodes[current];
      const R_xlen_t row = translation_row(node, local_id);
      const R_xlen_t owner =
        (R_xlen_t) INTEGER_ELT(node->translation_owners, row) - 1;
      collection_nodes[depth] = current;
      owners[depth] = owner;
      ++depth;
      local_id = STRING_ELT(node->translation_original_ids, row);
      current = find_node(&graph, VECTOR_ELT(node->sets, owner));
      if (current == R_XLEN_T_MAX) {
        UNPROTECT(2);
        Rf_error("Corrupt ParamSetCollection route plan");
      }
    }

    R_xlen_t unit = 0;
    for (; unit < unit_count; ++unit) {
      if (same_unit(
          &units[unit],
          current,
          collection_nodes,
          owners,
          depth
        )) {
        break;
      }
    }
    if (unit == unit_count) {
      detach_unit_t *created = &units[unit_count];
      created->leaf = current;
      created->depth = depth;
      created->collection_nodes = paradox_temporary_alloc(
        depth,
        sizeof(*created->collection_nodes)
      );
      created->owners = paradox_temporary_alloc(
        depth,
        sizeof(*created->owners)
      );
      memcpy(
        created->collection_nodes,
        collection_nodes,
        (size_t) depth * sizeof(*created->collection_nodes)
      );
      memcpy(
        created->owners,
        owners,
        (size_t) depth * sizeof(*created->owners)
      );
      /* make_affix() returns a fresh unprotected CHARSXP, and `created` lives
       * in R_alloc memory that the GC does not scan. Root each affix before
       * the next allocation: R sweeps its string cache on every collection,
       * so an unmarked fresh CHARSXP parked here would be evicted and freed. */
      created->prefix = make_affix(
        &graph,
        collection_nodes,
        owners,
        depth,
        TRUE
      );
      append_root(&roots, roots_index, created->prefix);
      created->suffix = make_affix(
        &graph,
        collection_nodes,
        owners,
        depth,
        FALSE
      );
      append_root(&roots, roots_index, created->suffix);
      const detach_node_t *leaf = &graph.nodes[current];
      created->constraint = VECTOR_ELT(
        leaf->payload,
        PARADOX_CORE_CONSTRAINT
      );
      created->trafo = VECTOR_ELT(
        leaf->payload,
        PARADOX_CORE_EXTRA_TRAFO
      );
      ++unit_count;
    }
    if (!expected_root_id(
        root_id,
        units[unit].prefix,
        local_id,
        units[unit].suffix
      )) {
      UNPROTECT(2);
      Rf_error("Corrupt ParamSetCollection affix translation");
    }
    selected_rows[output] = root_row;
    selected_units[output] = unit;
    selected_local_ids[output] = local_id;
    ++output;
  }
  if (output != selected_count) {
    UNPROTECT(2);
    Rf_error("Internal error: incomplete detachment route plan");
  }

  R_xlen_t constraint_count = 0;
  R_xlen_t trafo_count = 0;
  for (R_xlen_t unit = 0; unit < unit_count; ++unit) {
    constraint_count += units[unit].constraint != R_NilValue;
    trafo_count += units[unit].trafo != R_NilValue;
  }
  SEXP plan = PROTECT(Rf_allocVector(VECSXP, PARADOX_COLLECTION_DETACH_FIELD_COUNT));
  SEXP plan_names = PROTECT(Rf_allocVector(
    STRSXP,
    PARADOX_COLLECTION_DETACH_FIELD_COUNT
  ));
  static const char *const field_names[] = {
    "translation", "constraint_indices", "constraint_sets",
    "trafo_indices", "trafo_sets", "postfix"
  };
  for (R_xlen_t field = 0; field < PARADOX_COLLECTION_DETACH_FIELD_COUNT; ++field) {
    SET_STRING_ELT(plan_names, field, Rf_mkChar(field_names[field]));
  }
  Rf_setAttrib(plan, R_NamesSymbol, plan_names);
  SEXP translation = PROTECT(allocate_translation(selected_count));
  SEXP constraint_indices = PROTECT(Rf_allocVector(
    INTSXP,
    constraint_count
  ));
  SEXP constraint_sets = PROTECT(allocate_carriers(
    constraint_count,
    "constraint",
    FALSE
  ));
  SEXP trafo_indices = PROTECT(Rf_allocVector(INTSXP, trafo_count));
  SEXP trafo_sets = PROTECT(allocate_carriers(
    trafo_count,
    "extra_trafo",
    TRUE
  ));
  SEXP postfix = PROTECT(Rf_ScalarLogical(root->postfix));
  SET_VECTOR_ELT(plan, PARADOX_COLLECTION_DETACH_TRANSLATION, translation);
  SET_VECTOR_ELT(plan, PARADOX_COLLECTION_DETACH_CONSTRAINT_INDICES, constraint_indices);
  SET_VECTOR_ELT(plan, PARADOX_COLLECTION_DETACH_CONSTRAINT_SETS, constraint_sets);
  SET_VECTOR_ELT(plan, PARADOX_COLLECTION_DETACH_TRAFO_INDICES, trafo_indices);
  SET_VECTOR_ELT(plan, PARADOX_COLLECTION_DETACH_TRAFO_SETS, trafo_sets);
  SET_VECTOR_ELT(plan, PARADOX_COLLECTION_DETACH_POSTFIX, postfix);

  SEXP output_ids = VECTOR_ELT(translation, DETACH_TRANSLATION_ID);
  SEXP output_original_ids = VECTOR_ELT(
    translation,
    DETACH_TRANSLATION_ORIGINAL_ID
  );
  SEXP output_owners = VECTOR_ELT(
    translation,
    DETACH_TRANSLATION_OWNER
  );
  SEXP output_owner_names = VECTOR_ELT(
    translation,
    DETACH_TRANSLATION_OWNER_NAME
  );
  SEXP output_prefixes = VECTOR_ELT(
    translation,
    DETACH_TRANSLATION_PREFIX
  );
  SEXP output_suffixes = VECTOR_ELT(
    translation,
    DETACH_TRANSLATION_SUFFIX
  );
  for (R_xlen_t row = 0; row < selected_count; ++row) {
    const R_xlen_t root_row = selected_rows[row];
    const R_xlen_t unit = selected_units[row];
    SET_STRING_ELT(
      output_ids,
      row,
      STRING_ELT(root->params.ids, root_row)
    );
    SET_STRING_ELT(output_original_ids, row, selected_local_ids[row]);
    INTEGER(output_owners)[row] = (int) (unit + 1);
    SET_STRING_ELT(output_owner_names, row, R_BlankString);
    SET_STRING_ELT(output_prefixes, row, units[unit].prefix);
    SET_STRING_ELT(output_suffixes, row, units[unit].suffix);
  }

  R_xlen_t constraint_output = 0;
  R_xlen_t trafo_output = 0;
  for (R_xlen_t unit = 0; unit < unit_count; ++unit) {
    if (units[unit].constraint != R_NilValue) {
      INTEGER(constraint_indices)[constraint_output] = (int) (unit + 1);
      SET_VECTOR_ELT(
        VECTOR_ELT(constraint_sets, constraint_output),
        0,
        units[unit].constraint
      );
      ++constraint_output;
    }
    if (units[unit].trafo != R_NilValue) {
      INTEGER(trafo_indices)[trafo_output] = (int) (unit + 1);
      SEXP carrier = VECTOR_ELT(trafo_sets, trafo_output);
      SET_VECTOR_ELT(carrier, 0, units[unit].trafo);
      SET_VECTOR_ELT(carrier, 1, graph.nodes[units[unit].leaf].core);
      ++trafo_output;
    }
  }
  if (constraint_output != constraint_count || trafo_output != trafo_count) {
    UNPROTECT(10);
    Rf_error("Internal error: incomplete callback detachment plan");
  }

  UNPROTECT(10);
  return plan;
}

SEXP paradox_param_set_collection_detach_plan(SEXP private_environment,
    SEXP self, SEXP requested) {
  /* The live entry performs the same complete canonical graph admission as
   * every other collection operation; the plan constructor consumes only
   * admitted graphs. The former detach-only parallel admission is gone. */
  R_xlen_t work_since_interrupt = 0;
  PROTECT_INDEX graph_roots_index;
  SEXP graph_roots;
  PROTECT_WITH_INDEX(graph_roots = R_NilValue, &graph_roots_index);
  paradox_collection_graph_t graph;
  paradox_collection_graph_build(
    private_environment,
    self,
    PARADOX_GRAPH_ALL,
    &graph,
    &graph_roots,
    graph_roots_index,
    &work_since_interrupt
  );
  SEXP plan = collection_detach_plan(requested, &graph);
  UNPROTECT(1);
  return plan;
}

SEXP paradox_param_set_collection_detach_plan_from_graph(
    const paradox_collection_graph_t *graph, SEXP requested) {
  return collection_detach_plan(requested, graph);
}
