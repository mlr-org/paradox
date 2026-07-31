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

static int ordinary_table_columns(SEXP table, R_xlen_t column_count) {
  if (TYPEOF(table) != VECSXP || ALTREP(table) || Rf_isS4(table) ||
      XLENGTH(table) != column_count) {
    return FALSE;
  }
  for (R_xlen_t column = 0; column < column_count; ++column) {
    if (ALTREP(VECTOR_ELT(table, column)) ||
        Rf_isS4(VECTOR_ELT(table, column)) ||
        Rf_isObject(VECTOR_ELT(table, column))) {
      return FALSE;
    }
  }
  return TRUE;
}

static int exact_set_names(SEXP sets, SEXP *names,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isS4(sets) ||
      Rf_isObject(sets) ||
      !paradox_api_has_only_attributes(sets, (const char *const[]) {"names"}, 1)) {
    return FALSE;
  }
  SEXP observed = PROTECT(Rf_getAttrib(sets, R_NamesSymbol));
  const R_xlen_t count = XLENGTH(sets);
  int valid = TYPEOF(observed) == STRSXP && !ALTREP(observed) &&
    !Rf_isS4(observed) && !Rf_isObject(observed) &&
    paradox_api_has_no_attributes(observed) && XLENGTH(observed) == count;
  for (R_xlen_t right = 0; valid && right < count; ++right) {
    paradox_account_work(work_since_interrupt);
    SEXP right_name = STRING_ELT(observed, right);
    if (!paradox_charsxp_is_ordinary(right_name)) {
      valid = FALSE;
      break;
    }
    if (CHAR(right_name)[0] == '\0') {
      continue;
    }
    for (R_xlen_t left = 0; left < right; ++left) {
      paradox_account_work(work_since_interrupt);
      SEXP left_name = STRING_ELT(observed, left);
      if (CHAR(left_name)[0] != '\0' &&
          paradox_domain_strings_equal(left_name, right_name)) {
        valid = FALSE;
        break;
      }
    }
  }
  if (valid) {
    *names = observed;
  }
  UNPROTECT(1);
  return valid;
}

static int exact_translation(SEXP table, SEXP set_names,
    R_xlen_t child_count, R_xlen_t parameter_count,
    paradox_collection_translation_t *translation,
    R_xlen_t **translation_by_param, SEXP parameter_ids,
    R_xlen_t *work_since_interrupt) {
  static const char *const column_names[] = {
    "id", "original_id", "owner_ps_index", "owner_name"
  };
  R_xlen_t row_count = 0;
  if (!ordinary_table_columns(table, 4) ||
      !paradox_domain_exact_plain_table(
        table,
        column_names,
        4,
        &row_count,
        work_since_interrupt
      ) || row_count != parameter_count) {
    return FALSE;
  }

  SEXP ids = VECTOR_ELT(table, 0);
  SEXP original_ids = VECTOR_ELT(table, 1);
  SEXP owner_indices = VECTOR_ELT(table, 2);
  SEXP owner_names = VECTOR_ELT(table, 3);
  if (TYPEOF(ids) != STRSXP || TYPEOF(original_ids) != STRSXP ||
      TYPEOF(owner_indices) != INTSXP || TYPEOF(owner_names) != STRSXP ||
      ALTREP(ids) || ALTREP(original_ids) || ALTREP(owner_indices) ||
      ALTREP(owner_names) || !paradox_api_has_no_attributes(ids) ||
      !paradox_api_has_no_attributes(original_ids) ||
      !paradox_api_has_no_attributes(owner_indices) ||
      !paradox_api_has_no_attributes(owner_names)) {
    return FALSE;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    const int owner = INTEGER_ELT(owner_indices, row);
    if (!paradox_charsxp_is_ordinary(STRING_ELT(ids, row)) ||
        !paradox_charsxp_is_ordinary(STRING_ELT(original_ids, row)) ||
        !paradox_charsxp_is_ordinary(STRING_ELT(owner_names, row)) || owner <= 0 ||
        (R_xlen_t) owner > child_count ||
        !paradox_domain_strings_equal(
          STRING_ELT(owner_names, row),
          STRING_ELT(set_names, (R_xlen_t) owner - 1)
        )) {
      return FALSE;
    }
  }
  if (Rf_any_duplicated(ids, FALSE) != 0) {
    return FALSE;
  }

  SEXP matches = PROTECT(Rf_match(ids, parameter_ids, 0));
  if ((TYPEOF(matches) != INTSXP && TYPEOF(matches) != REALSXP) ||
      ALTREP(matches) || XLENGTH(matches) != parameter_count) {
    UNPROTECT(1);
    Rf_error("Internal error: invalid collection translation match");
  }
  R_xlen_t *by_param = paradox_temporary_alloc(
    parameter_count,
    sizeof(*by_param)
  );
  for (R_xlen_t row = 0; row < parameter_count; ++row) {
    paradox_account_work(work_since_interrupt);
    R_xlen_t position = 0;
    if (TYPEOF(matches) == INTSXP) {
      const int value = INTEGER_ELT(matches, row);
      position = value == NA_INTEGER || value <= 0 ? 0 : (R_xlen_t) value;
    } else {
      const double value = REAL_ELT(matches, row);
      position = !R_FINITE(value) || value <= 0.0 ||
          value > (double) R_XLEN_T_MAX
        ? 0
        : (R_xlen_t) value;
    }
    if (position == 0 || position > row_count) {
      UNPROTECT(1);
      return FALSE;
    }
    by_param[row] = position - 1;
  }
  UNPROTECT(1);

  *translation = (paradox_collection_translation_t) {
    table, ids, original_ids, owner_indices, owner_names, row_count
  };
  *translation_by_param = by_param;
  return TRUE;
}

static int exact_dynamic_state(paradox_collection_graph_node_t *node,
    R_xlen_t *work_since_interrupt) {
  SEXP params = VECTOR_ELT(node->state, PARADOX_CORE_PARAMS);
  SEXP dependencies = VECTOR_ELT(node->state, PARADOX_CORE_DEPS);
  SEXP values = VECTOR_ELT(node->state, PARADOX_CORE_VALUES);
  SEXP tags = VECTOR_ELT(node->state, PARADOX_CORE_TAGS);
  SEXP trafos = VECTOR_ELT(node->state, PARADOX_CORE_TRAFOS);
  R_xlen_t unused_row = 0;
  if (!ordinary_table_columns(params, PARADOX_DOMAIN_TAGS) ||
      !ordinary_table_columns(dependencies, 3) ||
      !ordinary_table_columns(tags, 2) ||
      !ordinary_table_columns(trafos, 2) ||
      !paradox_domain_validate_params(
        params,
        R_NilValue,
        TRUE,
        &node->params,
        &unused_row,
        work_since_interrupt
      ) || !paradox_domain_validate_dependencies(
        dependencies,
        &node->dependencies,
        work_since_interrupt
      ) || !paradox_domain_validate_values(
        values,
        &node->values,
        work_since_interrupt
      )) {
    return FALSE;
  }

  node->value_param_rows = node->values.size == 0
    ? NULL
    : paradox_temporary_alloc(
        node->values.size,
        sizeof(*node->value_param_rows)
      );
  for (R_xlen_t index = 0; index < node->values.size; ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP name = STRING_ELT(node->values.names, index);
    SEXP value = VECTOR_ELT(node->values.values, index);
    if (!paradox_charsxp_is_ordinary(name) || value == R_UnboundValue ||
        value == R_MissingArg || TYPEOF(value) == PROMSXP) {
      return FALSE;
    }
    const R_xlen_t parameter_row = paradox_domain_find_string(
      node->params.ids,
      name,
      work_since_interrupt
    );
    if (parameter_row == R_XLEN_T_MAX) {
      return FALSE;
    }
    node->value_param_rows[index] = parameter_row;
  }
  return TRUE;
}

static int numeric_equal(SEXP left, R_xlen_t left_row,
    SEXP right, R_xlen_t right_row) {
  const SEXPTYPE left_type = (SEXPTYPE) TYPEOF(left);
  const SEXPTYPE right_type = (SEXPTYPE) TYPEOF(right);
  if ((left_type != INTSXP && left_type != REALSXP) ||
      (right_type != INTSXP && right_type != REALSXP)) {
    return FALSE;
  }
  const int left_integer = left_type == INTSXP
    ? INTEGER_ELT(left, left_row)
    : NA_INTEGER;
  const int right_integer = right_type == INTSXP
    ? INTEGER_ELT(right, right_row)
    : NA_INTEGER;
  double left_value = left_type == INTSXP
    ? (left_integer == NA_INTEGER ? NA_REAL : (double) left_integer)
    : REAL_ELT(left, left_row);
  double right_value = right_type == INTSXP
    ? (right_integer == NA_INTEGER ? NA_REAL : (double) right_integer)
    : REAL_ELT(right, right_row);
  if (ISNA(left_value) || ISNA(right_value)) {
    return ISNA(left_value) && ISNA(right_value);
  }
  if (ISNAN(left_value) || ISNAN(right_value)) {
    return ISNAN(left_value) && ISNAN(right_value);
  }
  return left_value == right_value;
}

static int permanent_rows_equal(const paradox_domain_params_t *parent,
    R_xlen_t parent_row, const paradox_domain_params_t *child,
    R_xlen_t child_row, R_xlen_t *work_since_interrupt) {
  for (enum paradox_domain_column column = PARADOX_DOMAIN_CLS;
      column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    paradox_account_work(work_since_interrupt);
    SEXP left = VECTOR_ELT(parent->table, column);
    SEXP right = VECTOR_ELT(child->table, column);
    switch (TYPEOF(left)) {
    case STRSXP:
      if (TYPEOF(right) != STRSXP || !paradox_domain_strings_equal(
          STRING_ELT(left, parent_row),
          STRING_ELT(right, child_row)
        )) {
        return FALSE;
      }
      break;
    case VECSXP: {
      if (TYPEOF(right) != VECSXP) {
        return FALSE;
      }
      SEXP left_value = PROTECT(VECTOR_ELT(left, parent_row));
      SEXP right_value = PROTECT(VECTOR_ELT(right, child_row));
      const int equal = left_value == right_value || R_compute_identical(
          left_value,
          right_value,
          paradox_api_identical_default_flags()
        );
      UNPROTECT(2);
      if (!equal) {
        return FALSE;
      }
      break;
    }
    case INTSXP:
    case REALSXP:
      if (!numeric_equal(left, parent_row, right, child_row)) {
        return FALSE;
      }
      break;
    default:
      return FALSE;
    }
  }
  return TRUE;
}

static char *utf8_copy(SEXP value, size_t *size) {
  return paradox_temporary_utf8_copy(value, size);
}

static int ascii_size(const char *text, size_t *size) {
  const unsigned char *cursor = (const unsigned char *) text;
  while (*cursor != '\0') {
    if (*cursor >= 0x80U) {
      return FALSE;
    }
    ++cursor;
  }
  *size = (size_t) (cursor - (const unsigned char *) text);
  return TRUE;
}

static int raw_affixed_id_equal(SEXP outer, SEXP owner, SEXP inner,
    int postfix, int *known) {
  const cetype_t encoding = Rf_getCharCE(outer);
  if (Rf_getCharCE(owner) != encoding || Rf_getCharCE(inner) != encoding ||
      (encoding != CE_NATIVE && encoding != CE_UTF8 &&
       encoding != CE_LATIN1)) {
    *known = FALSE;
    return FALSE;
  }

  const char *outer_text = CHAR(outer);
  const char *owner_text = CHAR(owner);
  const char *inner_text = CHAR(inner);
  size_t outer_size = 0;
  size_t owner_size = 0;
  size_t inner_size = 0;
  if (encoding == CE_NATIVE) {
    if (!ascii_size(outer_text, &outer_size) ||
        !ascii_size(owner_text, &owner_size) ||
        !ascii_size(inner_text, &inner_size)) {
      *known = FALSE;
      return FALSE;
    }
  } else {
    outer_size = strlen(outer_text);
    owner_size = strlen(owner_text);
    inner_size = strlen(inner_text);
  }

  *known = TRUE;
  if (owner_size > SIZE_MAX - inner_size - 1U ||
      outer_size != owner_size + inner_size + 1U) {
    return FALSE;
  }
  if (postfix) {
    return memcmp(outer_text, inner_text, inner_size) == 0 &&
      outer_text[inner_size] == '.' &&
      memcmp(outer_text + inner_size + 1U, owner_text, owner_size) == 0;
  }
  return memcmp(outer_text, owner_text, owner_size) == 0 &&
    outer_text[owner_size] == '.' &&
    memcmp(outer_text + owner_size + 1U, inner_text, inner_size) == 0;
}

static int affixed_id_equal(SEXP outer, SEXP owner, SEXP inner, int postfix) {
  if (!paradox_charsxp_is_ordinary(outer) || !paradox_charsxp_is_ordinary(owner) ||
      !paradox_charsxp_is_ordinary(inner)) {
    return FALSE;
  }
  if (CHAR(owner)[0] == '\0') {
    return paradox_domain_strings_equal(outer, inner);
  }
  int raw_known = FALSE;
  const int raw_equal = raw_affixed_id_equal(
    outer,
    owner,
    inner,
    postfix,
    &raw_known
  );
  if (raw_known) {
    return raw_equal;
  }
  PROTECT(outer);
  PROTECT(owner);
  PROTECT(inner);
  const void *vmax = vmaxget();
  size_t outer_size = 0;
  size_t owner_size = 0;
  size_t inner_size = 0;
  char *outer_text = utf8_copy(outer, &outer_size);
  char *owner_text = utf8_copy(owner, &owner_size);
  char *inner_text = utf8_copy(inner, &inner_size);
  int equal = FALSE;
  if (outer_text != NULL && owner_text != NULL && inner_text != NULL &&
      owner_size <= SIZE_MAX - inner_size - 1U &&
      outer_size == owner_size + inner_size + 1U) {
    if (postfix) {
      equal = memcmp(outer_text, inner_text, inner_size) == 0 &&
        outer_text[inner_size] == '.' &&
        memcmp(
          outer_text + inner_size + 1U,
          owner_text,
          owner_size
        ) == 0;
    } else {
      equal = memcmp(outer_text, owner_text, owner_size) == 0 &&
        outer_text[owner_size] == '.' &&
        memcmp(
          outer_text + owner_size + 1U,
          inner_text,
          inner_size
        ) == 0;
    }
  }
  vmaxset(vmax);
  UNPROTECT(3);
  return equal;
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
    int retain_receipt) {
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
    .value_param_rows = NULL,
    .sets = R_NilValue,
    .set_names = R_NilValue,
    .translation = {R_NilValue, R_NilValue, R_NilValue, R_NilValue,
      R_NilValue, 0},
    .translation_by_param = NULL,
    .parent = parent,
    .parent_child = parent_child,
    .parent_param_start = 0,
    .next_child = 0,
    .consumed_params = 0,
    .subtree_dependencies = 0,
    .subtree_contributes = FALSE,
    .postfix = FALSE
  };
  if ((kind != PARADOX_CORE_BASE && kind != PARADOX_CORE_COLLECTION &&
       kind != PARADOX_CORE_SHADOW) ||
      !paradox_core_state_exact_schema(state)) {
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
  if (!exact_dynamic_state(node, work_since_interrupt)) {
    return FALSE;
  }
  node->subtree_dependencies = node->dependencies.row_count;
  /* A callback is a contribution even on a set with no parameters: it runs
   * once per occurrence, so pruning a node that carries one would silently
   * drop those calls. */
  node->subtree_contributes = node->params.row_count != 0 ||
    node->dependencies.row_count != 0 || node->values.size != 0 ||
    VECTOR_ELT(state, PARADOX_CORE_EXTRA_TRAFO) != R_NilValue ||
    VECTOR_ELT(state, PARADOX_CORE_CONSTRAINT) != R_NilValue;

  SEXP sets = VECTOR_ELT(state, PARADOX_CORE_SETS);
  SEXP translation = VECTOR_ELT(state, PARADOX_CORE_TRANSLATION);
  SEXP postfix = VECTOR_ELT(state, PARADOX_CORE_POSTFIX);
  int parsed_postfix = FALSE;
  if (!exact_flag(postfix, &parsed_postfix)) {
    return FALSE;
  }
  node->postfix = parsed_postfix;

  if (kind == PARADOX_CORE_BASE) {
    return sets == R_NilValue && translation == R_NilValue && !parsed_postfix;
  }
  if (kind == PARADOX_CORE_SHADOW) {
    return TYPEOF(sets) == VECSXP && !ALTREP(sets) &&
      !Rf_isS4(sets) && !Rf_isObject(sets) &&
      paradox_api_has_no_attributes(sets) && XLENGTH(sets) == 1 &&
      TYPEOF(VECTOR_ELT(sets, 0)) == ENVSXP &&
      !Rf_isS4(VECTOR_ELT(sets, 0)) &&
      translation == R_NilValue && !parsed_postfix;
  }

  node->sets = sets;
  if (node->values.size != 0 ||
      !exact_set_names(sets, &node->set_names, work_since_interrupt) ||
      XLENGTH(sets) > INT_MAX || !exact_translation(
        translation,
        node->set_names,
        XLENGTH(sets),
        node->params.row_count,
        &node->translation,
        &node->translation_by_param,
        node->params.ids,
        work_since_interrupt
      )) {
    return FALSE;
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
        FALSE
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
void paradox_collection_validate_single_node_readonly(
    SEXP private_environment, SEXP self, SEXP *roots,
    PROTECT_INDEX roots_index, R_xlen_t *work_since_interrupt) {
  if (TYPEOF(private_environment) != ENVSXP ||
      Rf_isS4(private_environment) ||
      TYPEOF(self) != ENVSXP || Rf_isS4(self) ||
      !paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("Corrupt ParamSet child shell");
  }
  SEXP source_core = PROTECT(paradox_core_from_private(private_environment));
  PROTECT_INDEX operation_index;
  SEXP operation_core;
  PROTECT_WITH_INDEX(operation_core = source_core, &operation_index);
  if (paradox_core_kind(source_core) == PARADOX_CORE_SHADOW) {
    REPROTECT(
      operation_core = paradox_shadow_preview_authoritative(
        self,
        private_environment
      ),
      operation_index
    );
  }
  paradox_collection_graph_node_t node;
  if ((paradox_core_kind(operation_core) != PARADOX_CORE_BASE &&
       paradox_core_kind(operation_core) != PARADOX_CORE_SHADOW) ||
      !initialize_new_node(
        self,
        private_environment,
        operation_core,
        source_core,
        R_XLEN_T_MAX,
        R_XLEN_T_MAX,
        &node,
        roots,
        roots_index,
        work_since_interrupt,
        FALSE
      )) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSet child capsule state");
  }
  UNPROTECT(2);
}

static int initialize_node(SEXP self, SEXP private_environment,
    SEXP operation_core, SEXP source_core, R_xlen_t parent,
    R_xlen_t parent_child, R_xlen_t previous,
    const paradox_collection_graph_t *graph,
    paradox_collection_graph_node_t *node, SEXP *roots,
    PROTECT_INDEX roots_index, R_xlen_t *work_since_interrupt,
    int retain_receipt) {
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
    retain_receipt
  );
}

static int validate_edge(paradox_collection_graph_node_t *parent,
    paradox_collection_graph_node_t *child, R_xlen_t child_index,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t child_rows = child->params.row_count;
  if (parent->kind != PARADOX_CORE_COLLECTION ||
      parent->consumed_params > parent->params.row_count ||
      child_rows > parent->params.row_count - parent->consumed_params) {
    return FALSE;
  }
  const R_xlen_t start = parent->consumed_params;
  SEXP owner_name = STRING_ELT(parent->set_names, child_index);
  for (R_xlen_t child_row = 0; child_row < child_rows; ++child_row) {
    paradox_account_work(work_since_interrupt);
    const R_xlen_t parent_row = start + child_row;
    const R_xlen_t translation_row =
      parent->translation_by_param[parent_row];
    SEXP parent_id = STRING_ELT(parent->params.ids, parent_row);
    SEXP child_id = STRING_ELT(child->params.ids, child_row);
    if (translation_row >= parent->translation.row_count ||
        !affixed_id_equal(parent_id, owner_name, child_id, parent->postfix) ||
        INTEGER_ELT(parent->translation.owner_indices, translation_row) !=
          (int) (child_index + 1) ||
        !paradox_domain_strings_equal(
          STRING_ELT(parent->translation.ids, translation_row),
          parent_id
        ) || !paradox_domain_strings_equal(
          STRING_ELT(parent->translation.original_ids, translation_row),
          child_id
        ) || !paradox_domain_strings_equal(
          STRING_ELT(parent->translation.owner_names, translation_row),
          owner_name
        ) || !permanent_rows_equal(
          &parent->params,
          parent_row,
          &child->params,
          child_row,
          work_since_interrupt
        )) {
      return FALSE;
    }
  }
  child->parent_param_start = start;
  parent->consumed_params += child_rows;
  return TRUE;
}

static void collection_graph_build(SEXP private_environment, SEXP self,
    paradox_collection_graph_t *graph, SEXP *roots,
    PROTECT_INDEX roots_index, R_xlen_t *work_since_interrupt,
    int commit_shadow_refreshes, int retain_receipt) {
  if (TYPEOF(private_environment) != ENVSXP ||
      Rf_isS4(private_environment) ||
      TYPEOF(self) != ENVSXP || Rf_isS4(self)) {
    Rf_error("Corrupt ParamSetCollection shell");
  }

  /* A traversal re-proves that every cached flatten agrees with its children,
   * so the graph must be healed before it is admitted -- otherwise a schema
   * change below an ancestor is reported as corruption instead of being
   * absorbed. A verified graph pays one comparison for this. Migration
   * preflight is deliberately excluded: it may not install anything into a
   * current shell, so a stale collection still fails it, exactly as before. */
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
      retain_receipt
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
          retain_receipt
        ) && validate_edge(
          &graph->nodes[node_index],
          &graph->nodes[child_node_index],
          child_position,
          work_since_interrupt
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
    FALSE
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
    TRUE
  );
}

void paradox_collection_graph_build_readonly(
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
    FALSE
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
    TRUE
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

/* Value admission has already resolved each stored name to one local
 * parameter row. Every admitted collection edge then proves that child row
 * `r` is represented by parent row `parent_param_start + r`. Reusing those
 * facts avoids replaying a linear ID search for every emitted value while
 * preserving the complete graph and corruption validation. */
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

  SEXP result = PROTECT(Rf_allocVector(VECSXP, output_size));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, output_size));
  R_xlen_t output = 0;
  for (R_xlen_t node_index = 0; node_index < graph->count; ++node_index) {
    const paradox_collection_graph_node_t *node = &graph->nodes[node_index];
    if (node->kind == PARADOX_CORE_COLLECTION) {
      continue;
    }
    if (node->values.size != 0 && node->value_param_rows == NULL) {
      Rf_error("Internal error: missing collection value row snapshot");
    }
    for (R_xlen_t index = 0; index < node->values.size; ++index) {
      paradox_account_work(work_since_interrupt);
      SET_VECTOR_ELT(result, output, VECTOR_ELT(node->values.values, index));
      SET_STRING_ELT(
        names,
        output,
        translate_value_row(
          graph,
          node_index,
          node->value_param_rows[index]
        )
      );
      ++output;
    }
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
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
    R_xlen_t unused_row = 0;
    if (state == R_UnboundValue ||
        !paradox_domain_validate_params(
          VECTOR_ELT(state, PARADOX_CORE_PARAMS),
          R_NilValue,
          TRUE,
          &params,
          &unused_row,
          &work_since_interrupt
        ) || !paradox_domain_validate_values(
          VECTOR_ELT(state, PARADOX_CORE_VALUES),
          &values,
          &work_since_interrupt
        )) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSet value capsule state");
    }
    SEXP result = PROTECT(paradox_detach_named_values(
      values.values,
      &params,
      &work_since_interrupt
    ));
    UNPROTECT(2);
    return result;
  }

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
  SEXP values = PROTECT(paradox_collection_values_from_graph(
    &graph,
    &work_since_interrupt
  ));
  SEXP result = PROTECT(paradox_detach_named_values(
    values,
    &graph.nodes[0].params,
    &work_since_interrupt
  ));
  UNPROTECT(4);
  return result;
}
