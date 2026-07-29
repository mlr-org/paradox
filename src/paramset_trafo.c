#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "builtin_condition.h"
#include "core_state.h"
#include "paramset_activity.h"
#include "paramset_collection_readers.h"
#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

/*
 * One transformation engine serves ParamSet$trafo() and batched Design rows.
 * Inputs are frozen before the capsule graph, all individual and extra
 * callbacks are then selected once, and every row finishes against that
 * snapshot.  Callback evaluation is the only R execution in the engine.
 */

typedef struct {
  SEXP callback;
  SEXP callback_owner;
  R_xlen_t node_index;
  int accepts_param_set;
} extra_trafo_plan_t;

typedef struct {
  SEXP self;
  SEXP param_set_argument;
  SEXP root_state;
  SEXP params_table;
  SEXP trafos_table;
  paradox_domain_params_t params;
  paradox_domain_trafos_t trafos;
  paradox_collection_graph_t graph;
  R_xlen_t **root_parameter_by_local;
  extra_trafo_plan_t *extra_plans;
  R_xlen_t extra_count;
  int is_collection;
} trafo_snapshot_t;

typedef enum {
  TRAFO_LIST_INPUT = 0,
  TRAFO_LIST_PLAIN
} trafo_list_policy_t;

static int ordinary_string_vector(SEXP value) {
  return TYPEOF(value) == STRSXP && !ALTREP(value) &&
    !Rf_isS4(value) && !Rf_isObject(value) &&
    paradox_api_has_no_attributes(value);
}

static int valid_list_shell(SEXP source, trafo_list_policy_t policy) {
  static const char *const names_only[] = {"names"};
  if (TYPEOF(source) != VECSXP || ALTREP(source) || Rf_isS4(source)) {
    return FALSE;
  }
  if (!Rf_isObject(source)) {
    return paradox_api_has_only_attributes(source, names_only, 1);
  }
  if (policy != TRAFO_LIST_INPUT) return FALSE;
  return paradox_public_table_kind(source) != PARADOX_PUBLIC_TABLE_NONE;
}

static SEXP snapshot_names(SEXP source, R_xlen_t expected,
    const char *description, R_xlen_t *work_since_interrupt) {
  if (source == R_NilValue && expected == 0) {
    return Rf_allocVector(STRSXP, 0);
  }
  if (!ordinary_string_vector(source) || XLENGTH(source) != expected) {
    Rf_error("%s must have one name for every element", description);
  }
  SEXP result = PROTECT(Rf_allocVector(STRSXP, expected));
  for (R_xlen_t index = 0; index < expected; ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP name = STRING_ELT(source, index);
    if (name == NA_STRING || Rf_getCharCE(name) == CE_BYTES ||
        CHAR(name)[0] == '\0') {
      UNPROTECT(1);
      Rf_error("%s contains an unsupported name", description);
    }
    SET_STRING_ELT(result, index, name);
  }
  if (Rf_any_duplicated(result, FALSE) != 0) {
    UNPROTECT(1);
    Rf_error("%s must have unique names", description);
  }
  UNPROTECT(1);
  return result;
}

static SEXP snapshot_list(SEXP source, int require_names,
    trafo_list_policy_t policy, const char *description,
    R_xlen_t *work_since_interrupt) {
  if (!valid_list_shell(source, policy)) {
    Rf_error(
      "%s must be an ordinary%s list",
      description,
      require_names ? " named" : ""
    );
  }
  const R_xlen_t count = XLENGTH(source);
  SEXP source_names = PROTECT(Rf_getAttrib(source, R_NamesSymbol));
  int protect_count = 1;
  SEXP names = R_NilValue;
  if (require_names || source_names != R_NilValue) {
    names = PROTECT(snapshot_names(
      source_names,
      count,
      description,
      work_since_interrupt
    ));
    ++protect_count;
  }
  if (Rf_isObject(source) && policy == TRAFO_LIST_INPUT) {
    R_xlen_t ignored_rows = 0;
    if (!paradox_public_table_row_count(source, &ignored_rows)) {
      UNPROTECT(protect_count);
      Rf_error("%s has invalid data.frame row names", description);
    }
  }
  SEXP result = PROTECT(Rf_allocVector(VECSXP, count));
  ++protect_count;
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP value = VECTOR_ELT(source, index);
    if (value == R_UnboundValue || value == R_MissingArg ||
        TYPEOF(value) == PROMSXP) {
      UNPROTECT(protect_count);
      Rf_error("%s contains an invalid value", description);
    }
    /* Values are opaque callback leaves and intentionally retain identity. */
    SET_VECTOR_ELT(result, index, value);
  }
  if (names != R_NilValue) {
    Rf_setAttrib(result, R_NamesSymbol, names);
  }
  UNPROTECT(protect_count);
  return result;
}

static SEXP snapshot_row(SEXP source,
    R_xlen_t *work_since_interrupt) {
  return snapshot_list(
    source,
    TRUE,
    TRAFO_LIST_INPUT,
    "A ParamSet transformation input",
    work_since_interrupt
  );
}

static SEXP snapshot_plain_row(SEXP source,
    R_xlen_t *work_since_interrupt) {
  return snapshot_list(
    source,
    TRUE,
    TRAFO_LIST_PLAIN,
    "A Design transformation row",
    work_since_interrupt
  );
}

static int exact_batch_shell(SEXP source) {
  return TYPEOF(source) == VECSXP && !ALTREP(source) &&
    !Rf_isS4(source) && !Rf_isObject(source) &&
    paradox_api_has_no_attributes(source);
}

static SEXP snapshot_batch(SEXP source,
    R_xlen_t *work_since_interrupt) {
  if (!exact_batch_shell(source)) {
    Rf_error("Design transformation rows must be an ordinary list");
  }
  const R_xlen_t count = XLENGTH(source);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, count));
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP row = PROTECT(snapshot_plain_row(
      VECTOR_ELT(source, index),
      work_since_interrupt
    ));
    SET_VECTOR_ELT(result, index, row);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return result;
}

static int callback_accepts_param_set(SEXP callback) {
  if (TYPEOF(callback) != CLOSXP) {
    return FALSE;
  }
  for (SEXP formal = paradox_api_closure_formals(callback);
      formal != R_NilValue;
      formal = CDR(formal)) {
    if (TYPEOF(formal) != LISTSXP) {
      Rf_error("A ParamSet extra_trafo has malformed formals");
    }
    if (TAG(formal) == Rf_install("param_set") ||
        TAG(formal) == R_DotsSymbol) {
      return TRUE;
    }
  }
  return FALSE;
}

static void validate_trafo_table(trafo_snapshot_t *snapshot,
    R_xlen_t *work_since_interrupt) {
  R_xlen_t unused_row = 0;
  if (!paradox_domain_validate_params(
      snapshot->params_table,
      R_NilValue,
      TRUE,
      &snapshot->params,
      &unused_row,
      work_since_interrupt
    ) || !paradox_domain_validate_trafos(
      snapshot->trafos_table,
      &snapshot->trafos,
      work_since_interrupt
    )) {
    Rf_error("Corrupt ParamSet transformation capsule");
  }
  if (Rf_any_duplicated(snapshot->trafos.ids, FALSE) != 0) {
    Rf_error("Corrupt ParamSet transformation capsule: duplicate IDs");
  }
  for (R_xlen_t index = 0; index < snapshot->trafos.row_count; ++index) {
    if (paradox_domain_find_string(
        snapshot->params.ids,
        STRING_ELT(snapshot->trafos.ids, index),
        work_since_interrupt
      ) == R_XLEN_T_MAX) {
      Rf_error("Corrupt ParamSet transformation capsule: unknown ID");
    }
  }
}

static void build_collection_mappings(trafo_snapshot_t *snapshot) {
  const R_xlen_t node_count = snapshot->graph.count;
  snapshot->root_parameter_by_local = paradox_temporary_alloc(
    node_count,
    sizeof(*snapshot->root_parameter_by_local)
  );
  for (R_xlen_t node_index = 0; node_index < node_count; ++node_index) {
    const paradox_collection_graph_node_t *node =
      &snapshot->graph.nodes[node_index];
    const R_xlen_t parameter_count = node->params.row_count;
    R_xlen_t *mapping = paradox_temporary_alloc(
      parameter_count,
      sizeof(*mapping)
    );
    snapshot->root_parameter_by_local[node_index] = mapping;
    if (node_index == 0) {
      for (R_xlen_t parameter = 0;
          parameter < parameter_count;
          ++parameter) {
        mapping[parameter] = parameter;
      }
      continue;
    }
    if (node->parent >= node_index) {
      Rf_error("Corrupt ParamSetCollection transformation graph order");
    }
    const paradox_collection_graph_node_t *parent =
      &snapshot->graph.nodes[node->parent];
    R_xlen_t *parent_mapping =
      snapshot->root_parameter_by_local[node->parent];
    if (node->parent_param_start > parent->params.row_count ||
        parameter_count > parent->params.row_count - node->parent_param_start) {
      Rf_error("Corrupt ParamSetCollection transformation mapping");
    }
    for (R_xlen_t parameter = 0;
        parameter < parameter_count;
        ++parameter) {
      mapping[parameter] = parent_mapping[
        node->parent_param_start + parameter
      ];
    }
  }
}

static void build_extra_plans(trafo_snapshot_t *snapshot) {
  const R_xlen_t node_count = snapshot->is_collection
    ? snapshot->graph.count
    : 1;
  R_xlen_t count = 0;
  for (R_xlen_t node_index = 0; node_index < node_count; ++node_index) {
    SEXP state = snapshot->is_collection
      ? snapshot->graph.nodes[node_index].state
      : snapshot->root_state;
    paradox_core_kind_t kind = snapshot->is_collection
      ? snapshot->graph.nodes[node_index].kind
      : PARADOX_CORE_BASE;
    if (snapshot->is_collection && kind == PARADOX_CORE_COLLECTION) {
      if (VECTOR_ELT(state, PARADOX_CORE_EXTRA_TRAFO) != R_NilValue) {
        Rf_error("Corrupt ParamSetCollection capsule: stored aggregate trafo");
      }
      continue;
    }
    SEXP callback = VECTOR_ELT(state, PARADOX_CORE_EXTRA_TRAFO);
    if (callback != R_NilValue) {
      if (!Rf_isFunction(callback)) {
        Rf_error("Corrupt ParamSet capsule: extra_trafo is not a function");
      }
      ++count;
    }
  }

  snapshot->extra_plans = paradox_temporary_alloc(
    count,
    sizeof(*snapshot->extra_plans)
  );
  snapshot->extra_count = count;
  R_xlen_t output = 0;
  for (R_xlen_t node_index = 0; node_index < node_count; ++node_index) {
    SEXP state = snapshot->is_collection
      ? snapshot->graph.nodes[node_index].state
      : snapshot->root_state;
    paradox_core_kind_t kind = snapshot->is_collection
      ? snapshot->graph.nodes[node_index].kind
      : PARADOX_CORE_BASE;
    if (snapshot->is_collection && kind == PARADOX_CORE_COLLECTION) {
      continue;
    }
    SEXP callback = VECTOR_ELT(state, PARADOX_CORE_EXTRA_TRAFO);
    if (callback == R_NilValue) {
      continue;
    }
    snapshot->extra_plans[output] = (extra_trafo_plan_t) {
      callback,
      snapshot->is_collection
        ? snapshot->graph.nodes[node_index].self
        : snapshot->param_set_argument,
      node_index,
      callback_accepts_param_set(callback)
    };
    ++output;
  }
  if (output != count) {
    Rf_error("Internal error: incomplete ParamSet extra_trafo plan");
  }
}

static void load_snapshot(SEXP private_environment, SEXP self,
    SEXP param_set_argument, trafo_snapshot_t *snapshot,
    SEXP *graph_roots, PROTECT_INDEX graph_roots_index,
    SEXP roots, R_xlen_t *work_since_interrupt) {
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("Corrupt ParamSet shell ownership in transformation");
  }
  SEXP core = PROTECT(paradox_core_from_private(private_environment));
  if (core == R_UnboundValue) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet transformation capsule");
  }
  if (!paradox_core_is_verified(core)) {
    core = paradox_core_refresh(self, private_environment);
  }
  const paradox_core_kind_t kind = paradox_core_kind(core);

  *snapshot = (trafo_snapshot_t) {
    .self = self,
    .param_set_argument = param_set_argument,
    .root_state = R_NilValue,
    .params_table = R_NilValue,
    .trafos_table = R_NilValue,
    .root_parameter_by_local = NULL,
    .extra_plans = NULL,
    .extra_count = 0,
    .is_collection = kind == PARADOX_CORE_COLLECTION
  };

  if (snapshot->is_collection) {
    paradox_collection_graph_build(
      private_environment,
      self,
      &snapshot->graph,
      graph_roots,
      graph_roots_index,
      work_since_interrupt
    );
    snapshot->params_table = snapshot->graph.nodes[0].params.table;
    snapshot->root_state = snapshot->graph.nodes[0].state;
    snapshot->trafos_table = VECTOR_ELT(
      snapshot->graph.nodes[0].state,
      PARADOX_CORE_TRAFOS
    );
    build_collection_mappings(snapshot);
  } else if (kind == PARADOX_CORE_BASE || kind == PARADOX_CORE_SHADOW) {
    SEXP payload = paradox_core_payload(core);
    if (payload == R_UnboundValue) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSet transformation capsule");
    }
    SET_VECTOR_ELT(roots, 0, core);
    snapshot->root_state = payload;
    snapshot->params_table = VECTOR_ELT(payload, PARADOX_CORE_PARAMS);
    snapshot->trafos_table = VECTOR_ELT(payload, PARADOX_CORE_TRAFOS);
  } else {
    UNPROTECT(1);
    Rf_error("Unknown ParamSet node kind in transformation");
  }
  SET_VECTOR_ELT(roots, 1, snapshot->params_table);
  SET_VECTOR_ELT(roots, 2, snapshot->trafos_table);
  UNPROTECT(1);

  validate_trafo_table(snapshot, work_since_interrupt);
  build_extra_plans(snapshot);
}

static SEXP evaluate_unary(SEXP callback, SEXP value) {
  SEXP call = PROTECT(paradox_unary_callback_call(callback, value));
  SEXP result = PROTECT(Rf_eval(call, R_BaseEnv));
  UNPROTECT(2);
  return result;
}

static SEXP evaluate_extra(const extra_trafo_plan_t *plan, SEXP input) {
  SEXP call;
  if (plan->accepts_param_set) {
    call = PROTECT(Rf_lang3(plan->callback, input, plan->callback_owner));
    SET_TAG(CDR(call), Rf_install("x"));
    SET_TAG(CDDR(call), Rf_install("param_set"));
  } else {
    call = PROTECT(Rf_lang2(plan->callback, input));
  }
  SEXP result = PROTECT(Rf_eval(call, R_BaseEnv));
  UNPROTECT(2);
  return result;
}

static SEXP apply_individual_trafos(SEXP row,
    const trafo_snapshot_t *snapshot,
    R_xlen_t *work_since_interrupt) {
  SEXP names = Rf_getAttrib(row, R_NamesSymbol);
  const R_xlen_t count = XLENGTH(row);
  SEXP matches = PROTECT(Rf_match(snapshot->trafos.ids, names, 0));
  const SEXPTYPE match_type = (SEXPTYPE) TYPEOF(matches);
  if ((match_type != INTSXP && match_type != REALSXP) ||
      XLENGTH(matches) != count) {
    UNPROTECT(1);
    Rf_error("Internal error: invalid transformation match result");
  }
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_account_work(work_since_interrupt);
    R_xlen_t position = 0;
    if (match_type == INTSXP) {
      const int value = INTEGER_ELT(matches, index);
      position = value == NA_INTEGER || value <= 0
        ? 0
        : (R_xlen_t) value;
    } else {
      const double value = REAL_ELT(matches, index);
      position = !R_FINITE(value) || value <= 0.0 ||
          value > (double) R_XLEN_T_MAX
        ? 0
        : (R_xlen_t) value;
    }
    if (position == 0) {
      continue;
    }
    if (position > snapshot->trafos.row_count) {
      UNPROTECT(1);
      Rf_error("Internal error: transformation match is out of range");
    }
    const R_xlen_t trafo = position - 1;
    SEXP transformed = PROTECT(evaluate_unary(
      VECTOR_ELT(snapshot->trafos.values, trafo),
      VECTOR_ELT(row, index)
    ));
    SET_VECTOR_ELT(row, index, transformed);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return row;
}

static SEXP local_input_for_names(SEXP row, SEXP root_names,
    SEXP local_names, R_xlen_t *work_since_interrupt) {
  if (TYPEOF(root_names) != STRSXP || TYPEOF(local_names) != STRSXP ||
      XLENGTH(root_names) != XLENGTH(local_names)) {
    Rf_error("Internal error: invalid callback input mapping");
  }
  SEXP row_names = Rf_getAttrib(row, R_NamesSymbol);
  R_xlen_t present = 0;
  for (R_xlen_t parameter = 0;
      parameter < XLENGTH(root_names);
      ++parameter) {
    if (paradox_domain_find_string(
        row_names,
        STRING_ELT(root_names, parameter),
        work_since_interrupt
      ) != R_XLEN_T_MAX) {
      ++present;
    }
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, present));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, present));
  R_xlen_t output = 0;
  for (R_xlen_t parameter = 0;
      parameter < XLENGTH(root_names);
      ++parameter) {
    const R_xlen_t input = paradox_domain_find_string(
      row_names,
      STRING_ELT(root_names, parameter),
      work_since_interrupt
    );
    if (input == R_XLEN_T_MAX) {
      continue;
    }
    SET_VECTOR_ELT(result, output, VECTOR_ELT(row, input));
    SET_STRING_ELT(
      names,
      output,
      STRING_ELT(local_names, parameter)
    );
    ++output;
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
}

static SEXP local_extra_input(SEXP row, const trafo_snapshot_t *snapshot,
    R_xlen_t node_index, R_xlen_t *work_since_interrupt) {
  const paradox_collection_graph_node_t *node =
    &snapshot->graph.nodes[node_index];
  SEXP root_names = PROTECT(Rf_allocVector(
    STRSXP,
    node->params.row_count
  ));
  for (R_xlen_t parameter = 0;
      parameter < node->params.row_count;
      ++parameter) {
    const R_xlen_t root_parameter =
      snapshot->root_parameter_by_local[node_index][parameter];
    if (root_parameter >= snapshot->params.row_count) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSetCollection transformation mapping");
    }
    SET_STRING_ELT(
      root_names,
      parameter,
      STRING_ELT(snapshot->params.ids, root_parameter)
    );
  }
  SEXP result = PROTECT(local_input_for_names(
    row,
    root_names,
    node->params.ids,
    work_since_interrupt
  ));
  UNPROTECT(2);
  return result;
}

static SEXP affix_name(SEXP id, SEXP owner, int postfix) {
  if (owner == NA_STRING || id == NA_STRING ||
      Rf_getCharCE(owner) == CE_BYTES || Rf_getCharCE(id) == CE_BYTES) {
    Rf_error("An extra_trafo returned an unsupported name");
  }
  if (CHAR(owner)[0] == '\0') {
    return id;
  }
  PROTECT(id);
  PROTECT(owner);
  const void *vmax = vmaxget();
  const size_t id_size = strlen(Rf_translateCharUTF8(id));
  if (id_size >= (size_t) R_XLEN_T_MAX) {
    UNPROTECT(2);
    Rf_error("An extra_trafo output name is too long");
  }
  char *id_text = paradox_temporary_alloc(
    (R_xlen_t) id_size + 1,
    sizeof(*id_text)
  );
  memcpy(id_text, Rf_translateCharUTF8(id), id_size + 1U);
  const size_t owner_size = strlen(Rf_translateCharUTF8(owner));
  if (owner_size >= (size_t) R_XLEN_T_MAX) {
    UNPROTECT(2);
    Rf_error("An extra_trafo output name is too long");
  }
  char *owner_text = paradox_temporary_alloc(
    (R_xlen_t) owner_size + 1,
    sizeof(*owner_text)
  );
  memcpy(owner_text, Rf_translateCharUTF8(owner), owner_size + 1U);
  if (id_size > SIZE_MAX - owner_size - 2U ||
      id_size + owner_size + 2U > (size_t) R_XLEN_T_MAX) {
    UNPROTECT(2);
    Rf_error("An extra_trafo output name is too long");
  }
  const size_t size = id_size + owner_size + 1U;
  char *buffer = paradox_temporary_alloc(
    (R_xlen_t) size + 1,
    sizeof(*buffer)
  );
  if (postfix) {
    memcpy(buffer, id_text, id_size);
    buffer[id_size] = '.';
    memcpy(buffer + id_size + 1U, owner_text, owner_size);
  } else {
    memcpy(buffer, owner_text, owner_size);
    buffer[owner_size] = '.';
    memcpy(buffer + owner_size + 1U, id_text, id_size);
  }
  buffer[size] = '\0';
  SEXP result = Rf_mkCharCE(buffer, CE_UTF8);
  /* Name translation is a row-level hot path.  Release its transient copies
   * now rather than retaining three buffers per translated output until the
   * enclosing .Call returns.  The interned CHARSXP result is independent of
   * the transient allocation watermark. */
  vmaxset(vmax);
  UNPROTECT(2);
  return result;
}

static SEXP translate_extra_name(const trafo_snapshot_t *snapshot,
    R_xlen_t node_index, SEXP name) {
  SEXP current = name;
  while (snapshot->graph.nodes[node_index].parent != R_XLEN_T_MAX) {
    const paradox_collection_graph_node_t *node =
      &snapshot->graph.nodes[node_index];
    const paradox_collection_graph_node_t *parent =
      &snapshot->graph.nodes[node->parent];
    if (node->parent_child >= XLENGTH(parent->set_names)) {
      Rf_error("Corrupt ParamSetCollection transformation path");
    }
    current = affix_name(
      current,
      STRING_ELT(parent->set_names, node->parent_child),
      parent->postfix
    );
    node_index = node->parent;
  }
  return current;
}

static SEXP merge_extra_unit(SEXP row, SEXP update, SEXP owned_names,
    SEXP translated_names, R_xlen_t *work_since_interrupt) {
  SEXP row_names = PROTECT(Rf_getAttrib(row, R_NamesSymbol));
  const R_xlen_t row_count = XLENGTH(row);
  const R_xlen_t update_count = XLENGTH(update);
  R_xlen_t retained = 0;
  for (R_xlen_t index = 0; index < row_count; ++index) {
    paradox_account_work(work_since_interrupt);
    if (paradox_domain_find_string(
        owned_names,
        STRING_ELT(row_names, index),
        work_since_interrupt
      ) == R_XLEN_T_MAX) {
      ++retained;
    }
  }
  if (Rf_any_duplicated(translated_names, FALSE) != 0) {
    UNPROTECT(1);
    Rf_error("An extra_trafo produced duplicate translated names");
  }
  for (R_xlen_t output = 0; output < update_count; ++output) {
    SEXP output_name = STRING_ELT(translated_names, output);
    for (R_xlen_t input = 0; input < row_count; ++input) {
      paradox_account_work(work_since_interrupt);
      SEXP input_name = STRING_ELT(row_names, input);
      if (paradox_domain_find_string(
          owned_names,
          input_name,
          work_since_interrupt
        ) == R_XLEN_T_MAX && paradox_domain_strings_equal(input_name, output_name)) {
        UNPROTECT(1);
        Rf_error(
          "An extra_trafo output collides with a retained value"
        );
      }
    }
  }
  if (update_count > R_XLEN_T_MAX - retained) {
    UNPROTECT(1);
    Rf_error("A transformed row is too large");
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, retained + update_count));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, retained + update_count));
  R_xlen_t next = 0;
  for (R_xlen_t index = 0; index < row_count; ++index) {
    if (paradox_domain_find_string(
        owned_names,
        STRING_ELT(row_names, index),
        work_since_interrupt
      ) != R_XLEN_T_MAX) {
      continue;
    }
    SET_VECTOR_ELT(result, next, VECTOR_ELT(row, index));
    SET_STRING_ELT(names, next, STRING_ELT(row_names, index));
    ++next;
  }
  for (R_xlen_t index = 0; index < update_count; ++index) {
    SET_VECTOR_ELT(result, next, VECTOR_ELT(update, index));
    SET_STRING_ELT(names, next, STRING_ELT(translated_names, index));
    ++next;
  }
  if (next != retained + update_count) {
    UNPROTECT(3);
    Rf_error("Internal error: incomplete transformed row merge");
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(3);
  return result;
}

static SEXP merge_collection_extra(SEXP row, SEXP update,
    const trafo_snapshot_t *snapshot, R_xlen_t node_index,
    R_xlen_t *work_since_interrupt) {
  const paradox_collection_graph_node_t *node =
    &snapshot->graph.nodes[node_index];
  SEXP owned_names = PROTECT(Rf_allocVector(
    STRSXP,
    node->params.row_count
  ));
  for (R_xlen_t parameter = 0;
      parameter < node->params.row_count;
      ++parameter) {
    const R_xlen_t root_parameter =
      snapshot->root_parameter_by_local[node_index][parameter];
    if (root_parameter >= snapshot->params.row_count) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSetCollection transformation mapping");
    }
    SET_STRING_ELT(
      owned_names,
      parameter,
      STRING_ELT(snapshot->params.ids, root_parameter)
    );
  }

  SEXP update_names = PROTECT(Rf_getAttrib(update, R_NamesSymbol));
  SEXP translated_names = PROTECT(Rf_allocVector(
    STRSXP,
    XLENGTH(update)
  ));
  for (R_xlen_t index = 0; index < XLENGTH(update); ++index) {
    paradox_account_work(work_since_interrupt);
    SET_STRING_ELT(
      translated_names,
      index,
      translate_extra_name(
        snapshot,
        node_index,
        STRING_ELT(update_names, index)
      )
    );
  }
  SEXP result = PROTECT(merge_extra_unit(
    row,
    update,
    owned_names,
    translated_names,
    work_since_interrupt
  ));
  UNPROTECT(4);
  return result;
}

static SEXP apply_extra_trafos(SEXP row,
    const trafo_snapshot_t *snapshot,
    R_xlen_t *work_since_interrupt) {
  PROTECT_INDEX current_index;
  SEXP current;
  PROTECT_WITH_INDEX(current = row, &current_index);
  for (R_xlen_t index = 0; index < snapshot->extra_count; ++index) {
    paradox_account_work(work_since_interrupt);
    const extra_trafo_plan_t *plan = &snapshot->extra_plans[index];
    SEXP input = snapshot->is_collection
      ? PROTECT(local_extra_input(
          current,
          snapshot,
          plan->node_index,
          work_since_interrupt
        ))
      : PROTECT(snapshot_plain_row(current, work_since_interrupt));
    SEXP callback_result = PROTECT(evaluate_extra(plan, input));
    SEXP update = PROTECT(snapshot_list(
      callback_result,
      snapshot->is_collection,
      TRAFO_LIST_PLAIN,
      "A ParamSet extra_trafo result",
      work_since_interrupt
    ));
    SEXP replacement = snapshot->is_collection
      ? PROTECT(merge_collection_extra(
          current,
          update,
          snapshot,
          plan->node_index,
          work_since_interrupt
        ))
      : PROTECT(update);
    REPROTECT(current = replacement, current_index);
    UNPROTECT(4);
  }
  UNPROTECT(1);
  return current;
}

static SEXP transform_batch(SEXP rows, const trafo_snapshot_t *snapshot,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t count = XLENGTH(rows);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, count));
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_account_work(work_since_interrupt);
    /* `rows` is the operation-owned snapshot; mutate its private row shell
     * directly instead of copying it a second time. */
    SEXP row = PROTECT(VECTOR_ELT(rows, index));
    apply_individual_trafos(row, snapshot, work_since_interrupt);
    SEXP transformed = PROTECT(apply_extra_trafos(
      row,
      snapshot,
      work_since_interrupt
    ));
    SET_VECTOR_ELT(result, index, transformed);
    UNPROTECT(2);
  }
  UNPROTECT(1);
  return result;
}

static SEXP run_batch(SEXP frozen_rows, SEXP private_environment,
    SEXP self, SEXP param_set_argument,
    R_xlen_t *work_since_interrupt) {
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, 3));
  PROTECT_INDEX graph_roots_index;
  SEXP graph_roots;
  PROTECT_WITH_INDEX(graph_roots = R_NilValue, &graph_roots_index);
  trafo_snapshot_t snapshot;
  load_snapshot(
    private_environment,
    self,
    param_set_argument,
    &snapshot,
    &graph_roots,
    graph_roots_index,
    roots,
    work_since_interrupt
  );
  SEXP result = PROTECT(transform_batch(
    frozen_rows,
    &snapshot,
    work_since_interrupt
  ));
  UNPROTECT(3);
  return result;
}

SEXP paradox_param_set_trafo(SEXP private_environment, SEXP self,
    SEXP x, SEXP param_set_argument) {
  PROTECT(private_environment);
  PROTECT(self);
  x = PROTECT(paradox_materialize_public_table_shell(x));
  PROTECT(param_set_argument);
  R_xlen_t work_since_interrupt = 0;
  SEXP frozen_row = PROTECT(snapshot_row(x, &work_since_interrupt));
  SEXP rows = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(rows, 0, frozen_row);
  SEXP result = PROTECT(run_batch(
    rows,
    private_environment,
    self,
    param_set_argument,
    &work_since_interrupt
  ));
  SEXP transformed = PROTECT(VECTOR_ELT(result, 0));
  UNPROTECT(8);
  return transformed;
}

SEXP paradox_design_transpose_trafos(SEXP rows, SEXP param_set) {
  PROTECT(rows);
  PROTECT(param_set);
  if (!exact_batch_shell(rows)) {
    UNPROTECT(2);
    Rf_error("Design transformation rows must be an ordinary list");
  }
  R_xlen_t work_since_interrupt = 0;
  SEXP private_environment = PROTECT(
    paradox_domain_private_environment(param_set)
  );
  if (private_environment == R_UnboundValue) {
    UNPROTECT(3);
    Rf_error("Corrupt ParamSet shell in Design transformation");
  }

  /* The rows came directly from paradox_design_transpose(): their semantic
   * vectors are already ordinary, rooted snapshots. Select the capsule once
   * here so an empty transformation graph can return them without a second
   * O(rows * columns) copy. Registered direct calls are internal and are not
   * an extension surface. */
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, 3));
  PROTECT_INDEX graph_roots_index;
  SEXP graph_roots;
  PROTECT_WITH_INDEX(graph_roots = R_NilValue, &graph_roots_index);
  trafo_snapshot_t snapshot;
  load_snapshot(
    private_environment,
    param_set,
    param_set,
    &snapshot,
    &graph_roots,
    graph_roots_index,
    roots,
    &work_since_interrupt
  );
  if (snapshot.trafos.row_count == 0 && snapshot.extra_count == 0) {
    UNPROTECT(5);
    return rows;
  }

  SEXP frozen_rows = PROTECT(snapshot_batch(rows, &work_since_interrupt));
  SEXP result = PROTECT(transform_batch(
    frozen_rows,
    &snapshot,
    &work_since_interrupt
  ));
  UNPROTECT(7);
  return result;
}

enum detached_plan_field {
  DETACHED_PLAN_TRANSLATION = 0,
  DETACHED_PLAN_INDICES,
  DETACHED_PLAN_SETS,
  DETACHED_PLAN_FIELD_COUNT
};

enum detached_translation_column {
  DETACHED_TRANSLATION_ID = 0,
  DETACHED_TRANSLATION_ORIGINAL_ID,
  DETACHED_TRANSLATION_OWNER,
  DETACHED_TRANSLATION_OWNER_NAME,
  DETACHED_TRANSLATION_PREFIX,
  DETACHED_TRANSLATION_SUFFIX,
  DETACHED_TRANSLATION_COLUMN_COUNT
};

typedef struct {
  SEXP translation;
  SEXP ids;
  SEXP original_ids;
  SEXP owners;
  SEXP prefixes;
  SEXP suffixes;
  SEXP indices;
  SEXP sets;
  R_xlen_t row_count;
  R_xlen_t callback_count;
  int extra_trafo;
} detached_callback_plan_t;

static int supported_detached_string(SEXP value, int allow_empty) {
  return value != NA_STRING && Rf_getCharCE(value) != CE_BYTES &&
    (allow_empty || CHAR(value)[0] != '\0');
}

static int exact_named_plain_list(SEXP value,
    const char *const *expected_names, R_xlen_t count) {
  if (TYPEOF(value) != VECSXP || ALTREP(value) || Rf_isObject(value) ||
      XLENGTH(value) != count ||
      !paradox_api_has_single_attribute(value, "names")) {
    return FALSE;
  }
  R_xlen_t work = 0;
  SEXP names = Rf_getAttrib(value, R_NamesSymbol);
  return paradox_api_has_no_attributes(names) &&
    paradox_domain_exact_string_vector(
      names,
      expected_names,
      count,
      &work
    );
}

static SEXP affix_detached_name(SEXP prefix, SEXP id, SEXP suffix) {
  if (!supported_detached_string(prefix, TRUE) ||
      !supported_detached_string(id, FALSE) ||
      !supported_detached_string(suffix, TRUE)) {
    Rf_error("A detached extra_trafo produced an unsupported name");
  }
  PROTECT(prefix);
  PROTECT(id);
  PROTECT(suffix);
  const void *vmax = vmaxget();

  const size_t prefix_size = strlen(Rf_translateCharUTF8(prefix));
  if (prefix_size >= (size_t) R_XLEN_T_MAX) {
    vmaxset(vmax);
    UNPROTECT(3);
    Rf_error("A detached extra_trafo output name is too long");
  }
  char *prefix_copy = paradox_temporary_alloc(
    (R_xlen_t) prefix_size + 1,
    sizeof(*prefix_copy)
  );
  memcpy(prefix_copy, Rf_translateCharUTF8(prefix), prefix_size + 1U);

  const size_t id_size = strlen(Rf_translateCharUTF8(id));
  if (id_size >= (size_t) R_XLEN_T_MAX) {
    vmaxset(vmax);
    UNPROTECT(3);
    Rf_error("A detached extra_trafo output name is too long");
  }
  char *id_copy = paradox_temporary_alloc(
    (R_xlen_t) id_size + 1,
    sizeof(*id_copy)
  );
  memcpy(id_copy, Rf_translateCharUTF8(id), id_size + 1U);

  const size_t suffix_size = strlen(Rf_translateCharUTF8(suffix));
  if (suffix_size >= (size_t) R_XLEN_T_MAX ||
      prefix_size > SIZE_MAX - id_size ||
      prefix_size + id_size > SIZE_MAX - suffix_size ||
      prefix_size + id_size + suffix_size > (size_t) R_XLEN_T_MAX) {
    vmaxset(vmax);
    UNPROTECT(3);
    Rf_error("A detached extra_trafo output name is too long");
  }
  char *suffix_copy = paradox_temporary_alloc(
    (R_xlen_t) suffix_size + 1,
    sizeof(*suffix_copy)
  );
  memcpy(suffix_copy, Rf_translateCharUTF8(suffix), suffix_size + 1U);

  const size_t size = prefix_size + id_size + suffix_size;
  char *buffer = paradox_temporary_alloc(
    (R_xlen_t) size + 1,
    sizeof(*buffer)
  );
  memcpy(buffer, prefix_copy, prefix_size);
  memcpy(buffer + prefix_size, id_copy, id_size);
  memcpy(buffer + prefix_size + id_size, suffix_copy, suffix_size);
  buffer[size] = '\0';
  SEXP result = Rf_mkCharCE(buffer, CE_UTF8);
  vmaxset(vmax);
  UNPROTECT(3);
  return result;
}

static int exact_detached_callback_owner(SEXP owner) {
  if (TYPEOF(owner) != ENVSXP) {
    return FALSE;
  }
  SEXP private_environment = paradox_domain_private_environment(owner);
  if (private_environment == R_UnboundValue ||
      !paradox_domain_owns_private_environment(owner, private_environment)) {
    return FALSE;
  }
  SEXP core = paradox_core_from_private(private_environment);
  return paradox_core_is_canonical(core) &&
    paradox_core_kind(core) == PARADOX_CORE_BASE;
}

static void load_detached_callback_plan(SEXP plan, int extra_trafo,
    detached_callback_plan_t *output,
    R_xlen_t *work_since_interrupt) {
  static const char *const plan_names[] = {
    "translation", "indices", "sets"
  };
  static const char *const translation_names[] = {
    "id", "original_id", "owner_ps_index", "owner_name", ".prefix",
    ".suffix"
  };
  static const char *const constraint_carrier_names[] = {"constraint"};
  static const char *const trafo_carrier_names[] = {
    "extra_trafo", "param_set"
  };
  if (!exact_named_plain_list(plan, plan_names, DETACHED_PLAN_FIELD_COUNT)) {
    Rf_error("Corrupt detached ParamSetCollection callback plan");
  }
  SEXP translation = VECTOR_ELT(plan, DETACHED_PLAN_TRANSLATION);
  SEXP indices = VECTOR_ELT(plan, DETACHED_PLAN_INDICES);
  SEXP sets = VECTOR_ELT(plan, DETACHED_PLAN_SETS);
  R_xlen_t row_count = 0;
  if (!paradox_domain_exact_plain_table(
        translation,
        translation_names,
        DETACHED_TRANSLATION_COLUMN_COUNT,
        &row_count,
        work_since_interrupt
      ) || TYPEOF(indices) != INTSXP || ALTREP(indices) ||
      !paradox_api_has_no_attributes(indices) || TYPEOF(sets) != VECSXP ||
      ALTREP(sets) || Rf_isObject(sets) ||
      !paradox_api_has_single_attribute(sets, "names") ||
      XLENGTH(indices) != XLENGTH(sets)) {
    Rf_error("Corrupt detached ParamSetCollection callback plan");
  }
  SEXP set_names = Rf_getAttrib(sets, R_NamesSymbol);
  if (TYPEOF(set_names) != STRSXP || ALTREP(set_names) ||
      !paradox_api_has_no_attributes(set_names) ||
      XLENGTH(set_names) != XLENGTH(sets)) {
    Rf_error("Corrupt detached ParamSetCollection callback carriers");
  }
  for (R_xlen_t index = 0; index < XLENGTH(set_names); ++index) {
    if (STRING_ELT(set_names, index) != R_BlankString) {
      Rf_error("Corrupt detached ParamSetCollection callback carriers");
    }
  }

  SEXP ids = VECTOR_ELT(translation, DETACHED_TRANSLATION_ID);
  SEXP original_ids = VECTOR_ELT(
    translation,
    DETACHED_TRANSLATION_ORIGINAL_ID
  );
  SEXP owners = VECTOR_ELT(translation, DETACHED_TRANSLATION_OWNER);
  SEXP owner_names = VECTOR_ELT(
    translation,
    DETACHED_TRANSLATION_OWNER_NAME
  );
  SEXP prefixes = VECTOR_ELT(translation, DETACHED_TRANSLATION_PREFIX);
  SEXP suffixes = VECTOR_ELT(translation, DETACHED_TRANSLATION_SUFFIX);
  if (TYPEOF(ids) != STRSXP || ALTREP(ids) ||
      !paradox_api_has_no_attributes(ids) ||
      TYPEOF(original_ids) != STRSXP || ALTREP(original_ids) ||
      !paradox_api_has_no_attributes(original_ids) ||
      TYPEOF(owners) != INTSXP || ALTREP(owners) ||
      !paradox_api_has_no_attributes(owners) ||
      TYPEOF(owner_names) != STRSXP || ALTREP(owner_names) ||
      !paradox_api_has_no_attributes(owner_names) ||
      TYPEOF(prefixes) != STRSXP || ALTREP(prefixes) ||
      !paradox_api_has_no_attributes(prefixes) ||
      TYPEOF(suffixes) != STRSXP || ALTREP(suffixes) ||
      !paradox_api_has_no_attributes(suffixes) ||
      XLENGTH(original_ids) != row_count || XLENGTH(owners) != row_count ||
      XLENGTH(owner_names) != row_count || XLENGTH(prefixes) != row_count ||
      XLENGTH(suffixes) != row_count ||
      Rf_any_duplicated(ids, FALSE) != 0) {
    Rf_error("Corrupt detached ParamSetCollection callback translation");
  }

  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    SEXP id = STRING_ELT(ids, row);
    SEXP original_id = STRING_ELT(original_ids, row);
    SEXP prefix = STRING_ELT(prefixes, row);
    SEXP suffix = STRING_ELT(suffixes, row);
    if (!supported_detached_string(id, FALSE) ||
        !supported_detached_string(original_id, FALSE) ||
        !supported_detached_string(STRING_ELT(owner_names, row), TRUE) ||
        !supported_detached_string(prefix, TRUE) ||
        !supported_detached_string(suffix, TRUE) ||
        INTEGER_ELT(owners, row) <= 0) {
      Rf_error("Corrupt detached ParamSetCollection callback translation");
    }
    SEXP expected = PROTECT(affix_detached_name(
      prefix,
      original_id,
      suffix
    ));
    const int equal = paradox_domain_strings_equal(id, expected);
    UNPROTECT(1);
    if (!equal) {
      Rf_error("Corrupt detached ParamSetCollection callback translation");
    }
  }

  int previous_unit = 0;
  for (R_xlen_t callback = 0;
      callback < XLENGTH(indices);
      ++callback) {
    paradox_account_work(work_since_interrupt);
    const int unit = INTEGER_ELT(indices, callback);
    if (unit <= previous_unit) {
      Rf_error("Corrupt detached ParamSetCollection callback order");
    }
    int found = FALSE;
    for (R_xlen_t row = 0; row < row_count; ++row) {
      if (INTEGER_ELT(owners, row) == unit) {
        found = TRUE;
        break;
      }
    }
    if (!found) {
      Rf_error("Corrupt detached ParamSetCollection callback unit");
    }
    SEXP carrier = VECTOR_ELT(sets, callback);
    if (extra_trafo) {
      if (!exact_named_plain_list(carrier, trafo_carrier_names, 2) ||
          !Rf_isFunction(VECTOR_ELT(carrier, 0))) {
        Rf_error("Corrupt detached ParamSetCollection trafo carrier");
      }
      const int accepts_param_set = callback_accepts_param_set(
        VECTOR_ELT(carrier, 0)
      );
      SEXP owner = VECTOR_ELT(carrier, 1);
      if ((accepts_param_set && !exact_detached_callback_owner(owner)) ||
          (!accepts_param_set && owner != R_NilValue)) {
        Rf_error("Corrupt detached ParamSetCollection trafo owner");
      }
    } else if (!exact_named_plain_list(
        carrier,
        constraint_carrier_names,
        1
      ) || !Rf_isFunction(VECTOR_ELT(carrier, 0))) {
      Rf_error("Corrupt detached ParamSetCollection constraint carrier");
    }
    previous_unit = unit;
  }

  *output = (detached_callback_plan_t) {
    translation,
    ids,
    original_ids,
    owners,
    prefixes,
    suffixes,
    indices,
    sets,
    row_count,
    XLENGTH(indices),
    extra_trafo
  };
}

static R_xlen_t detached_unit_size(const detached_callback_plan_t *plan,
    int unit) {
  R_xlen_t result = 0;
  for (R_xlen_t row = 0; row < plan->row_count; ++row) {
    result += INTEGER_ELT(plan->owners, row) == unit;
  }
  return result;
}

static void detached_unit_names(const detached_callback_plan_t *plan,
    int unit, SEXP root_names, SEXP local_names, SEXP *prefix,
    SEXP *suffix, R_xlen_t *work_since_interrupt) {
  R_xlen_t output = 0;
  *prefix = R_NilValue;
  *suffix = R_NilValue;
  for (R_xlen_t row = 0; row < plan->row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    if (INTEGER_ELT(plan->owners, row) != unit) {
      continue;
    }
    SEXP row_prefix = STRING_ELT(plan->prefixes, row);
    SEXP row_suffix = STRING_ELT(plan->suffixes, row);
    if (*prefix == R_NilValue) {
      *prefix = row_prefix;
      *suffix = row_suffix;
    } else if (!paradox_domain_strings_equal(*prefix, row_prefix) ||
        !paradox_domain_strings_equal(*suffix, row_suffix)) {
      Rf_error("Corrupt detached ParamSetCollection callback affix");
    }
    SET_STRING_ELT(root_names, output, STRING_ELT(plan->ids, row));
    SET_STRING_ELT(
      local_names,
      output,
      STRING_ELT(plan->original_ids, row)
    );
    ++output;
  }
  if (output != XLENGTH(root_names) || *prefix == R_NilValue ||
      *suffix == R_NilValue) {
    Rf_error("Corrupt detached ParamSetCollection callback unit");
  }
}

static SEXP detached_update_names(SEXP update,
    SEXP prefix, SEXP suffix, R_xlen_t *work_since_interrupt) {
  SEXP input_names = PROTECT(Rf_getAttrib(update, R_NamesSymbol));
  SEXP result = PROTECT(Rf_allocVector(STRSXP, XLENGTH(update)));
  for (R_xlen_t index = 0; index < XLENGTH(update); ++index) {
    paradox_account_work(work_since_interrupt);
    SET_STRING_ELT(
      result,
      index,
      affix_detached_name(
        prefix,
        STRING_ELT(input_names, index),
        suffix
      )
    );
  }
  UNPROTECT(2);
  return result;
}

static SEXP run_detached_extra_trafos(SEXP row,
    const detached_callback_plan_t *plan,
    R_xlen_t *work_since_interrupt) {
  PROTECT_INDEX current_index;
  SEXP current;
  PROTECT_WITH_INDEX(current = row, &current_index);
  for (R_xlen_t callback = 0;
      callback < plan->callback_count;
      ++callback) {
    paradox_account_work(work_since_interrupt);
    const int unit = INTEGER_ELT(plan->indices, callback);
    const R_xlen_t unit_size = detached_unit_size(plan, unit);
    SEXP root_names = PROTECT(Rf_allocVector(STRSXP, unit_size));
    SEXP local_names = PROTECT(Rf_allocVector(STRSXP, unit_size));
    SEXP prefix = R_NilValue;
    SEXP suffix = R_NilValue;
    detached_unit_names(
      plan,
      unit,
      root_names,
      local_names,
      &prefix,
      &suffix,
      work_since_interrupt
    );
    SEXP input = PROTECT(local_input_for_names(
      current,
      root_names,
      local_names,
      work_since_interrupt
    ));
    SEXP carrier = VECTOR_ELT(plan->sets, callback);
    extra_trafo_plan_t callback_plan = {
      VECTOR_ELT(carrier, 0),
      VECTOR_ELT(carrier, 1),
      0,
      callback_accepts_param_set(VECTOR_ELT(carrier, 0))
    };
    SEXP callback_result = PROTECT(evaluate_extra(&callback_plan, input));
    SEXP update = PROTECT(snapshot_list(
      callback_result,
      TRUE,
      TRAFO_LIST_PLAIN,
      "A ParamSetCollection extra_trafo result",
      work_since_interrupt
    ));
    SEXP translated_names = PROTECT(detached_update_names(
      update,
      prefix,
      suffix,
      work_since_interrupt
    ));
    SEXP replacement = PROTECT(merge_extra_unit(
      current,
      update,
      root_names,
      translated_names,
      work_since_interrupt
    ));
    REPROTECT(current = replacement, current_index);
    UNPROTECT(7);
  }
  UNPROTECT(1);
  return current;
}

static SEXP run_detached_constraints(SEXP row,
    const detached_callback_plan_t *plan,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t callback = 0;
      callback < plan->callback_count;
      ++callback) {
    paradox_account_work(work_since_interrupt);
    const int unit = INTEGER_ELT(plan->indices, callback);
    const R_xlen_t unit_size = detached_unit_size(plan, unit);
    SEXP root_names = PROTECT(Rf_allocVector(STRSXP, unit_size));
    SEXP local_names = PROTECT(Rf_allocVector(STRSXP, unit_size));
    SEXP prefix = R_NilValue;
    SEXP suffix = R_NilValue;
    detached_unit_names(
      plan,
      unit,
      root_names,
      local_names,
      &prefix,
      &suffix,
      work_since_interrupt
    );
    (void) prefix;
    (void) suffix;
    SEXP input = PROTECT(local_input_for_names(
      row,
      root_names,
      local_names,
      work_since_interrupt
    ));
    SEXP answer = PROTECT(evaluate_unary(
      VECTOR_ELT(VECTOR_ELT(plan->sets, callback), 0),
      input
    ));
    if (TYPEOF(answer) != LGLSXP || XLENGTH(answer) != 1 ||
        LOGICAL_ELT(answer, 0) == NA_LOGICAL) {
      UNPROTECT(4);
      Rf_error(
        "ParamSet constraint must return one non-missing logical value"
      );
    }
    const int accepted = LOGICAL_ELT(answer, 0);
    UNPROTECT(4);
    if (!accepted) {
      return Rf_ScalarLogical(FALSE);
    }
  }
  return Rf_ScalarLogical(TRUE);
}

SEXP paradox_param_set_collection_has_callback(SEXP private_environment,
    SEXP self, SEXP selector) {
  PROTECT(private_environment);
  PROTECT(self);
  PROTECT(selector);
  if (TYPEOF(selector) != INTSXP || ALTREP(selector) ||
      !paradox_api_has_no_attributes(selector) || XLENGTH(selector) != 1 ||
      (INTEGER_ELT(selector, 0) != 0 && INTEGER_ELT(selector, 0) != 1)) {
    UNPROTECT(3);
    Rf_error("Invalid ParamSetCollection callback selector");
  }
  const int field = INTEGER_ELT(selector, 0) == 0
    ? PARADOX_CORE_EXTRA_TRAFO
    : PARADOX_CORE_CONSTRAINT;
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
  int found = FALSE;
  for (R_xlen_t index = 0; index < graph.count; ++index) {
    const paradox_collection_graph_node_t *node = &graph.nodes[index];
    SEXP callback = VECTOR_ELT(node->state, field);
    if (node->kind == PARADOX_CORE_COLLECTION) {
      if (callback != R_NilValue) {
        UNPROTECT(4);
        Rf_error("Corrupt ParamSetCollection stored aggregate callback");
      }
    } else if (callback != R_NilValue) {
      if (!Rf_isFunction(callback)) {
        UNPROTECT(4);
        Rf_error("Corrupt ParamSetCollection callback capsule");
      }
      found = TRUE;
    }
  }
  UNPROTECT(4);
  return Rf_ScalarLogical(found);
}

SEXP paradox_param_set_collection_extra_trafo(SEXP private_environment,
    SEXP self, SEXP x) {
  PROTECT(private_environment);
  PROTECT(self);
  PROTECT(x);
  R_xlen_t work_since_interrupt = 0;
  SEXP frozen = PROTECT(snapshot_row(x, &work_since_interrupt));
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, 3));
  PROTECT_INDEX graph_roots_index;
  SEXP graph_roots;
  PROTECT_WITH_INDEX(graph_roots = R_NilValue, &graph_roots_index);
  trafo_snapshot_t snapshot;
  load_snapshot(
    private_environment,
    self,
    self,
    &snapshot,
    &graph_roots,
    graph_roots_index,
    roots,
    &work_since_interrupt
  );
  if (!snapshot.is_collection) {
    UNPROTECT(6);
    Rf_error("Collection callback requires a ParamSetCollection capsule");
  }
  SEXP result = PROTECT(apply_extra_trafos(
    frozen,
    &snapshot,
    &work_since_interrupt
  ));
  UNPROTECT(7);
  return result;
}

static SEXP collection_active_constraint_row(SEXP row,
    const trafo_snapshot_t *snapshot,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t parameter_count = snapshot->params.row_count;
  R_xlen_t dependency_count = 0;
  for (R_xlen_t node_index = 0;
      node_index < snapshot->graph.count;
      ++node_index) {
    const R_xlen_t local_count =
      snapshot->graph.nodes[node_index].dependencies.row_count;
    if (local_count > R_XLEN_T_MAX - dependency_count) {
      Rf_error("ParamSetCollection constraint dependency graph is too large");
    }
    dependency_count += local_count;
  }
  if (dependency_count == 0) {
    return row;
  }

  R_xlen_t *value_by_parameter = paradox_temporary_alloc(
    parameter_count == 0 ? 1 : parameter_count,
    sizeof(*value_by_parameter)
  );
  for (R_xlen_t parameter = 0;
      parameter < parameter_count;
      ++parameter) {
    value_by_parameter[parameter] = R_XLEN_T_MAX;
  }
  SEXP row_names = Rf_getAttrib(row, R_NamesSymbol);
  for (R_xlen_t value = 0; value < XLENGTH(row); ++value) {
    const R_xlen_t parameter = paradox_domain_find_string(
      snapshot->params.ids,
      STRING_ELT(row_names, value),
      work_since_interrupt
    );
    if (parameter != R_XLEN_T_MAX) {
      value_by_parameter[parameter] = value;
    }
  }

  R_xlen_t *dependency_child = paradox_temporary_alloc(
    dependency_count == 0 ? 1 : dependency_count,
    sizeof(*dependency_child)
  );
  R_xlen_t *dependency_parent = paradox_temporary_alloc(
    dependency_count == 0 ? 1 : dependency_count,
    sizeof(*dependency_parent)
  );
  SEXP *dependency_rhs = paradox_temporary_alloc(
    dependency_count == 0 ? 1 : dependency_count,
    sizeof(*dependency_rhs)
  );
  R_xlen_t output = 0;
  for (R_xlen_t node_index = 0;
      node_index < snapshot->graph.count;
      ++node_index) {
    const paradox_collection_graph_node_t *node =
      &snapshot->graph.nodes[node_index];
    for (R_xlen_t dependency = 0;
        dependency < node->dependencies.row_count;
        ++dependency) {
      const R_xlen_t local_child = paradox_domain_find_string(
        node->params.ids,
        STRING_ELT(node->dependencies.ids, dependency),
        work_since_interrupt
      );
      const R_xlen_t local_parent = paradox_domain_find_string(
        node->params.ids,
        STRING_ELT(node->dependencies.on, dependency),
        work_since_interrupt
      );
      if (local_child == R_XLEN_T_MAX) {
        Rf_error("Corrupt ParamSetCollection constraint dependency target");
      }
      dependency_child[output] =
        snapshot->root_parameter_by_local[node_index][local_child];
      dependency_parent[output] = local_parent == R_XLEN_T_MAX
        ? R_XLEN_T_MAX
        : snapshot->root_parameter_by_local[node_index][local_parent];
      paradox_builtin_condition_kind_t kind;
      SEXP rhs = R_NilValue;
      if (!paradox_builtin_condition_exact(
          VECTOR_ELT(node->dependencies.conditions, dependency),
          &kind,
          &rhs,
          work_since_interrupt
        )) {
        Rf_error("Corrupt ParamSetCollection constraint dependency");
      }
      dependency_rhs[output] = rhs;
      ++output;
    }
  }
  if (output != dependency_count) {
    Rf_error("Internal error: incomplete collection constraint activity plan");
  }

  paradox_activity_result_t activity = {
    paradox_temporary_alloc(
      parameter_count == 0 ? 1 : parameter_count,
      sizeof(*activity.active)
    ),
    NULL
  };
  const paradox_activity_plan_t plan = {
    parameter_count,
    VECTOR_ELT(snapshot->params.table, PARADOX_DOMAIN_DEFAULT),
    row,
    value_by_parameter,
    dependency_count,
    dependency_child,
    dependency_parent,
    (SEXP const *) dependency_rhs
  };
  paradox_activity_evaluate(
    &plan,
    &activity,
    work_since_interrupt
  );

  R_xlen_t kept = 0;
  for (R_xlen_t value = 0; value < XLENGTH(row); ++value) {
    const R_xlen_t parameter = paradox_domain_find_string(
      snapshot->params.ids,
      STRING_ELT(row_names, value),
      work_since_interrupt
    );
    kept += parameter != R_XLEN_T_MAX && activity.active[parameter];
  }
  SEXP result = PROTECT(Rf_allocVector(VECSXP, kept));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, kept));
  output = 0;
  for (R_xlen_t value = 0; value < XLENGTH(row); ++value) {
    const R_xlen_t parameter = paradox_domain_find_string(
      snapshot->params.ids,
      STRING_ELT(row_names, value),
      work_since_interrupt
    );
    if (parameter == R_XLEN_T_MAX || !activity.active[parameter]) continue;
    SET_VECTOR_ELT(result, output, VECTOR_ELT(row, value));
    SET_STRING_ELT(names, output, STRING_ELT(row_names, value));
    ++output;
  }
  if (output != kept) {
    UNPROTECT(2);
    Rf_error("Internal error: incomplete collection constraint input");
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
}

SEXP paradox_param_set_collection_constraint(SEXP private_environment,
    SEXP self, SEXP x) {
  PROTECT(private_environment);
  PROTECT(self);
  PROTECT(x);
  R_xlen_t work_since_interrupt = 0;
  SEXP frozen = PROTECT(snapshot_row(x, &work_since_interrupt));
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, 3));
  PROTECT_INDEX graph_roots_index;
  SEXP graph_roots;
  PROTECT_WITH_INDEX(graph_roots = R_NilValue, &graph_roots_index);
  trafo_snapshot_t snapshot;
  load_snapshot(
    private_environment,
    self,
    self,
    &snapshot,
    &graph_roots,
    graph_roots_index,
    roots,
    &work_since_interrupt
  );
  if (!snapshot.is_collection) {
    UNPROTECT(6);
    Rf_error("Collection callback requires a ParamSetCollection capsule");
  }
  SEXP active_row = PROTECT(collection_active_constraint_row(
    frozen,
    &snapshot,
    &work_since_interrupt
  ));
  for (R_xlen_t node_index = 0;
      node_index < snapshot.graph.count;
      ++node_index) {
    const paradox_collection_graph_node_t *node =
      &snapshot.graph.nodes[node_index];
    SEXP callback = VECTOR_ELT(node->state, PARADOX_CORE_CONSTRAINT);
    if (node->kind == PARADOX_CORE_COLLECTION) {
      if (callback != R_NilValue) {
        UNPROTECT(7);
        Rf_error("Corrupt ParamSetCollection stored aggregate constraint");
      }
      continue;
    }
    if (callback == R_NilValue) {
      continue;
    }
    if (!Rf_isFunction(callback)) {
      UNPROTECT(7);
      Rf_error("Corrupt ParamSetCollection constraint capsule");
    }
    SEXP input = PROTECT(local_extra_input(
      active_row,
      &snapshot,
      node_index,
      &work_since_interrupt
    ));
    SEXP answer = PROTECT(evaluate_unary(callback, input));
    if (TYPEOF(answer) != LGLSXP || XLENGTH(answer) != 1 ||
        LOGICAL_ELT(answer, 0) == NA_LOGICAL) {
      UNPROTECT(9);
      Rf_error(
        "ParamSet constraint must return one non-missing logical value"
      );
    }
    const int accepted = LOGICAL_ELT(answer, 0);
    UNPROTECT(2);
    if (!accepted) {
      UNPROTECT(7);
      return Rf_ScalarLogical(FALSE);
    }
  }
  UNPROTECT(7);
  return Rf_ScalarLogical(TRUE);
}

SEXP paradox_param_set_collection_detached_extra_trafo(SEXP plan, SEXP x) {
  PROTECT(plan);
  PROTECT(x);
  R_xlen_t work_since_interrupt = 0;
  detached_callback_plan_t snapshot;
  load_detached_callback_plan(
    plan,
    TRUE,
    &snapshot,
    &work_since_interrupt
  );
  SEXP row = PROTECT(snapshot_row(x, &work_since_interrupt));
  SEXP result = PROTECT(run_detached_extra_trafos(
    row,
    &snapshot,
    &work_since_interrupt
  ));
  UNPROTECT(4);
  return result;
}

SEXP paradox_param_set_collection_detached_constraint(SEXP plan, SEXP x) {
  PROTECT(plan);
  PROTECT(x);
  R_xlen_t work_since_interrupt = 0;
  detached_callback_plan_t snapshot;
  load_detached_callback_plan(
    plan,
    FALSE,
    &snapshot,
    &work_since_interrupt
  );
  SEXP row = PROTECT(snapshot_row(x, &work_since_interrupt));
  SEXP result = PROTECT(run_detached_constraints(
    row,
    &snapshot,
    &work_since_interrupt
  ));
  UNPROTECT(4);
  return result;
}

SEXP paradox_param_set_collection_owner_subset_state(SEXP callback,
    SEXP source, SEXP ids) {
  PROTECT(callback);
  PROTECT(source);
  PROTECT(ids);
  if (!Rf_isFunction(callback)) {
    UNPROTECT(3);
    Rf_error("Corrupt ParamSetCollection callback owner function");
  }
  if (!callback_accepts_param_set(callback)) {
    UNPROTECT(3);
    return R_NilValue;
  }
  SEXP private_environment = PROTECT(
    paradox_domain_private_environment(source)
  );
  if (private_environment == R_UnboundValue) {
    UNPROTECT(4);
    Rf_error("Corrupt ParamSetCollection callback owner shell");
  }
  SEXP true_flag = PROTECT(Rf_ScalarLogical(TRUE));
  SEXP false_flag = PROTECT(Rf_ScalarLogical(FALSE));
  SEXP token = PROTECT(paradox_param_set_subset_state(
    private_environment,
    source,
    ids,
    true_flag,
    false_flag,
    R_NilValue,
    R_NilValue,
    true_flag
  ));
  UNPROTECT(7);
  return token;
}
