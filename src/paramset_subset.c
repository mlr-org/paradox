#include <limits.h>
#include <stddef.h>

#include "paradox.h"

#include "builtin_condition.h"
#include "core_state.h"
#include "paramset_collection_readers.h"
#include "paramset_domain_common.h"
#include "paramset_shadow.h"
#include "r_api_compat.h"
#include "r_utils.h"

/*
 * Subsetting is a capsule operation.  It snapshots the current node once,
 * validates the closed built-in schema, slices ordinary storage, and returns a
 * single-use capability protecting a fresh BASE core.  R6 construction is the
 * consumer of that capability; this file never inspects or clones generated
 * R6 closures, classes, or private-table layouts.
 */

static const char *const tags_columns[] = {"id", "tag"};
static const char *const trafos_columns[] = {"id", "trafo"};
static const char *const deps_columns[] = {"id", "on", "cond"};

static unsigned char subset_token_identity;

typedef struct {
  R_xlen_t *offsets;
  R_xlen_t *rows;
} row_groups_t;

typedef struct {
  SEXP params_table;
  SEXP tags_table;
  SEXP trafos_table;
  SEXP deps_table;
  SEXP values_list;
  paradox_domain_params_t params;
  paradox_domain_tags_t tags;
  paradox_domain_trafos_t trafos;
  paradox_domain_dependencies_t deps;
  paradox_domain_values_t values;
  row_groups_t tag_groups;
  row_groups_t trafo_groups;
  row_groups_t dependency_groups;
  R_xlen_t *dependency_parents;
  R_xlen_t *values_by_parameter;
  SEXP constraint;
  SEXP extra_trafo;
  paradox_core_kind_t kind;
} subset_source_t;

enum source_root {
  SOURCE_CORE = 0,
  SOURCE_STATE,
  SOURCE_PARAMS,
  SOURCE_TAGS,
  SOURCE_TRAFOS,
  SOURCE_DEPS,
  SOURCE_VALUES,
  SOURCE_CONSTRAINT,
  SOURCE_EXTRA_TRAFO,
  SOURCE_SHADOW_SIGNATURE,
  SOURCE_SHADOW_SIGNATURE_CONTENT,
  SOURCE_ROOT_COUNT
};

enum subset_bundle_field {
  SUBSET_BUNDLE_TOKEN = 0,
  SUBSET_BUNDLE_DETACH,
  SUBSET_BUNDLE_KEEP_CONSTRAINT,
  SUBSET_BUNDLE_KEEP_TRAFO,
  SUBSET_BUNDLE_FIELD_COUNT
};

static int scalar_flag(SEXP value, const char *name) {
  if (TYPEOF(value) != LGLSXP || ALTREP(value) || XLENGTH(value) != 1 ||
      !paradox_api_has_no_attributes(value)) {
    Rf_error("`%s` must be one unclassed non-missing logical value", name);
  }
  const int result = LOGICAL_ELT(value, 0);
  if (result == NA_LOGICAL) {
    Rf_error("`%s` must be one non-missing logical value", name);
  }
  return result;
}

static void require_callback(SEXP callback, const char *name) {
  if (callback != R_NilValue && !Rf_isFunction(callback)) {
    Rf_error("Corrupt ParamSet state: `%s` must be NULL or a function", name);
  }
}

static SEXP snapshot_ids(SEXP ids) {
  if (TYPEOF(ids) != STRSXP) {
    Rf_error("`ids` must be a character vector");
  }
  const R_xlen_t size = XLENGTH(ids);
  if (size > INT_MAX) {
    Rf_error("`ids` is too long for a ParamSet");
  }
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    if (index != 0 && index % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP id = STRING_ELT(ids, index);
    if (id == NA_STRING) {
      UNPROTECT(1);
      Rf_error("`ids` must not contain missing values");
    }
    SET_STRING_ELT(result, index, id);
  }
  UNPROTECT(1);
  return result;
}

static R_xlen_t match_position(SEXP matches, R_xlen_t index) {
  if (TYPEOF(matches) == INTSXP) {
    const int value = INTEGER_ELT(matches, index);
    return value == NA_INTEGER || value <= 0
      ? R_XLEN_T_MAX
      : (R_xlen_t) value - 1;
  }
  if (TYPEOF(matches) == REALSXP) {
    const double value = REAL_ELT(matches, index);
    return ISNAN(value) || value <= 0.0
      ? R_XLEN_T_MAX
      : (R_xlen_t) value - 1;
  }
  Rf_error("Internal error: unexpected character-match result");
  return R_XLEN_T_MAX;
}

static void corrupt_unknown_parameter(SEXP id, const char *storage_name) {
  PROTECT(id);
  if (Rf_getCharCE(id) == CE_BYTES) {
    UNPROTECT(1);
    Rf_error(
      "Corrupt ParamSet state: `%s` refers to an unknown bytes-encoded parameter ID",
      storage_name
    );
  }
  const paradox_utf8_piece_t pieces[] = {
    paradox_utf8_ascii_piece("Corrupt ParamSet state: `"),
    paradox_utf8_ascii_piece(storage_name),
    paradox_utf8_ascii_piece("` refers to unknown parameter '"),
    paradox_utf8_charsxp_piece(id),
    paradox_utf8_ascii_piece("'")
  };
  SEXP message = PROTECT(paradox_utf8_message(pieces, 5));
  paradox_error_from_scalar_string(message);
}

static void unknown_requested_parameter(SEXP id) {
  PROTECT(id);
  if (Rf_getCharCE(id) == CE_BYTES) {
    UNPROTECT(1);
    Rf_error("Unknown bytes-encoded parameter ID");
  }
  /* An id whose bytes are not valid UTF-8 in this locale is escaped rather
   * than failing the message builder with an internal error. */
  SEXP safe_id = PROTECT(paradox_diagnostic_charsxp(id));
  const paradox_utf8_piece_t pieces[] = {
    paradox_utf8_ascii_piece("`ids` contains unknown parameter '"),
    paradox_utf8_charsxp_piece(safe_id),
    paradox_utf8_ascii_piece("'")
  };
  SEXP message = PROTECT(paradox_utf8_message(pieces, 3));
  paradox_error_from_scalar_string(message);
}

static R_xlen_t *match_owners(SEXP parameter_ids, SEXP ids,
    int missing_allowed, const char *storage_name) {
  const R_xlen_t size = XLENGTH(ids);
  R_xlen_t *owners = paradox_temporary_alloc(size, sizeof(*owners));
  SEXP matches = PROTECT(Rf_match(parameter_ids, ids, 0));
  if (XLENGTH(matches) != size) {
    UNPROTECT(1);
    Rf_error("Internal error: invalid ParamSet match result");
  }
  for (R_xlen_t row = 0; row < size; ++row) {
    const R_xlen_t owner = match_position(matches, row);
    if (!missing_allowed && owner == R_XLEN_T_MAX) {
      SEXP id = STRING_ELT(ids, row);
      UNPROTECT(1);
      corrupt_unknown_parameter(id, storage_name);
    }
    owners[row] = owner;
  }
  UNPROTECT(1);
  return owners;
}

static row_groups_t group_rows(const R_xlen_t *owners, R_xlen_t row_count,
    R_xlen_t parameter_count) {
  row_groups_t result = {
    paradox_temporary_alloc(parameter_count + 1, sizeof(*result.offsets)),
    paradox_temporary_alloc(row_count, sizeof(*result.rows))
  };
  for (R_xlen_t parameter = 0; parameter <= parameter_count; ++parameter) {
    result.offsets[parameter] = 0;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    if (owners[row] >= parameter_count) {
      Rf_error("Internal error: invalid ParamSet row owner");
    }
    ++result.offsets[owners[row] + 1];
  }
  for (R_xlen_t parameter = 1; parameter <= parameter_count; ++parameter) {
    result.offsets[parameter] += result.offsets[parameter - 1];
  }
  R_xlen_t *next = paradox_temporary_alloc(parameter_count, sizeof(*next));
  for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
    next[parameter] = result.offsets[parameter];
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    result.rows[next[owners[row]]++] = row;
  }
  return result;
}

static void validate_source_fields(SEXP params_table, SEXP tags_table,
    SEXP trafos_table, SEXP deps_table, SEXP values_list,
    subset_source_t *source) {
  R_xlen_t work = 0;
  R_xlen_t unused = 0;
  if (!paradox_domain_validate_params(
        params_table,
        R_NilValue,
        TRUE,
        &source->params,
        &unused,
        &work
      ) || !paradox_domain_validate_tags(
        tags_table,
        &source->tags,
        &work
      ) || !paradox_domain_validate_trafos(
        trafos_table,
        &source->trafos,
        &work
      ) || !paradox_domain_validate_dependencies(
        deps_table,
        &source->deps,
        &work
      ) || !paradox_domain_validate_values(
        values_list,
        &source->values,
        &work
      ) || source->params.row_count > INT_MAX) {
    Rf_error("Corrupt ParamSet state: invalid canonical subset storage");
  }

  source->params_table = params_table;
  source->tags_table = tags_table;
  source->trafos_table = trafos_table;
  source->deps_table = deps_table;
  source->values_list = values_list;

  R_xlen_t *tag_owners = match_owners(
    source->params.ids,
    source->tags.ids,
    FALSE,
    "tags"
  );
  R_xlen_t *trafo_owners = match_owners(
    source->params.ids,
    source->trafos.ids,
    FALSE,
    "transformations"
  );
  R_xlen_t *dependency_owners = match_owners(
    source->params.ids,
    source->deps.ids,
    FALSE,
    "dependencies"
  );
  source->dependency_parents = match_owners(
    source->params.ids,
    source->deps.on,
    TRUE,
    "dependency parents"
  );
  R_xlen_t *value_owners = match_owners(
    source->params.ids,
    source->values.names,
    FALSE,
    "values"
  );

  if (Rf_any_duplicated(source->trafos.ids, FALSE) != 0) {
    Rf_error("Corrupt ParamSet state: duplicate transformations");
  }
  for (R_xlen_t row = 0; row < source->deps.row_count; ++row) {
    paradox_builtin_condition_kind_t kind;
    SEXP rhs = R_NilValue;
    if (!paradox_builtin_condition_exact(
        VECTOR_ELT(source->deps.conditions, row),
        &kind,
        &rhs,
        &work
      )) {
      Rf_error("Corrupt ParamSet state: invalid built-in dependency condition");
    }
  }

  const R_xlen_t parameter_count = source->params.row_count;
  source->tag_groups = group_rows(
    tag_owners,
    source->tags.row_count,
    parameter_count
  );
  source->trafo_groups = group_rows(
    trafo_owners,
    source->trafos.row_count,
    parameter_count
  );
  source->dependency_groups = group_rows(
    dependency_owners,
    source->deps.row_count,
    parameter_count
  );
  source->values_by_parameter = paradox_temporary_alloc(
    parameter_count,
    sizeof(*source->values_by_parameter)
  );
  for (R_xlen_t parameter = 0; parameter < parameter_count; ++parameter) {
    source->values_by_parameter[parameter] = R_XLEN_T_MAX;
  }
  for (R_xlen_t row = 0; row < source->values.size; ++row) {
    const R_xlen_t owner = value_owners[row];
    if (source->values_by_parameter[owner] != R_XLEN_T_MAX) {
      Rf_error("Corrupt ParamSet state: duplicate parameter values");
    }
    source->values_by_parameter[owner] = row;
  }
}

static void install_source_fields(SEXP core, SEXP state, SEXP roots,
    SEXP values, SEXP dependencies, subset_source_t *source) {
  const paradox_core_kind_t kind = paradox_core_kind(core);
  SET_VECTOR_ELT(roots, SOURCE_CORE, core);
  SET_VECTOR_ELT(roots, SOURCE_STATE, state);
  SET_VECTOR_ELT(roots, SOURCE_PARAMS, VECTOR_ELT(state, PARADOX_CORE_PARAMS));
  SET_VECTOR_ELT(roots, SOURCE_TAGS, VECTOR_ELT(state, PARADOX_CORE_TAGS));
  SET_VECTOR_ELT(roots, SOURCE_TRAFOS, VECTOR_ELT(state, PARADOX_CORE_TRAFOS));
  SET_VECTOR_ELT(roots, SOURCE_DEPS, dependencies);
  SET_VECTOR_ELT(roots, SOURCE_VALUES, values);
  SET_VECTOR_ELT(
    roots,
    SOURCE_CONSTRAINT,
    VECTOR_ELT(state, PARADOX_CORE_CONSTRAINT)
  );
  SET_VECTOR_ELT(
    roots,
    SOURCE_EXTRA_TRAFO,
    VECTOR_ELT(state, PARADOX_CORE_EXTRA_TRAFO)
  );

  source->kind = kind;
  source->constraint = VECTOR_ELT(roots, SOURCE_CONSTRAINT);
  source->extra_trafo = VECTOR_ELT(roots, SOURCE_EXTRA_TRAFO);
  require_callback(source->constraint, "constraint");
  require_callback(source->extra_trafo, "extra_trafo");
  if (kind == PARADOX_CORE_COLLECTION &&
      (source->constraint != R_NilValue ||
       source->extra_trafo != R_NilValue)) {
    Rf_error("Corrupt ParamSetCollection stored aggregate callback");
  }
  if (kind == PARADOX_CORE_SHADOW) {
    SEXP signature = paradox_shadow_metadata_signature(core);
    if (signature == R_UnboundValue) {
      Rf_error("Corrupt ParamSetShadow native snapshot metadata");
    }
    SET_VECTOR_ELT(roots, SOURCE_SHADOW_SIGNATURE, signature);
    SEXP content = PROTECT(
      paradox_shadow_signature_content_snapshot(signature)
    );
    if (content == R_NilValue) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSetShadow native snapshot metadata");
    }
    SET_VECTOR_ELT(roots, SOURCE_SHADOW_SIGNATURE_CONTENT, content);
    UNPROTECT(1);
  }

  validate_source_fields(
    VECTOR_ELT(roots, SOURCE_PARAMS),
    VECTOR_ELT(roots, SOURCE_TAGS),
    VECTOR_ELT(roots, SOURCE_TRAFOS),
    VECTOR_ELT(roots, SOURCE_DEPS),
    VECTOR_ELT(roots, SOURCE_VALUES),
    source
  );
}

static void load_source(SEXP private_environment, SEXP self, SEXP roots,
    subset_source_t *source, paradox_collection_graph_t *graph,
    SEXP *graph_roots, PROTECT_INDEX graph_roots_index, int *has_graph) {
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("Corrupt ParamSet shell ownership");
  }
  PROTECT_INDEX core_index;
  SEXP core;
  PROTECT_WITH_INDEX(
    core = paradox_core_from_private(private_environment),
    &core_index
  );
  if (core == R_UnboundValue) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }
  const paradox_core_kind_t initial_kind = paradox_core_kind(core);
  if (initial_kind == PARADOX_CORE_COLLECTION) {
    R_xlen_t work_since_interrupt = 0;
    paradox_collection_graph_build_receipted(
      private_environment,
      self,
      graph,
      graph_roots,
      graph_roots_index,
      &work_since_interrupt
    );
    *has_graph = TRUE;
    core = graph->nodes[0].core;
  } else {
    if (!paradox_core_is_verified(core)) {
      REPROTECT(
        core = paradox_core_refresh(self, private_environment),
        core_index
      );
    }
    *has_graph = FALSE;
  }
  const paradox_core_kind_t kind = paradox_core_kind(core);
  if (kind != PARADOX_CORE_BASE && kind != PARADOX_CORE_COLLECTION &&
      kind != PARADOX_CORE_SHADOW) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet state: unknown capsule node kind");
  }
  SEXP state = paradox_core_payload(core);
  if (state == R_UnboundValue) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet state capsule");
  }

  if (kind == PARADOX_CORE_COLLECTION) {
    if (!*has_graph || graph->nodes[0].state != state) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSetCollection subset snapshot");
    }
    /* Values, dependencies, static schema, callbacks, and detachment all
     * consume this one admitted graph. */
    R_xlen_t work_since_interrupt = 0;
    SEXP values = PROTECT(paradox_collection_values_from_graph(
      graph,
      &work_since_interrupt
    ));
    SEXP dependencies = PROTECT(paradox_collection_dependencies_from_graph(
      graph,
      &work_since_interrupt
    ));
    install_source_fields(
      core,
      state,
      roots,
      values,
      dependencies,
      source
    );
    UNPROTECT(2);
  } else {
    install_source_fields(
      core,
      state,
      roots,
      VECTOR_ELT(state, PARADOX_CORE_VALUES),
      VECTOR_ELT(state, PARADOX_CORE_DEPS),
      source
    );
  }
  UNPROTECT(1);
}

static int source_snapshot_is_intact(
    SEXP roots, const paradox_collection_graph_t *graph, int has_graph,
    R_xlen_t *work_since_interrupt) {
  if (has_graph &&
      !paradox_collection_graph_snapshot_is_intact(
        graph,
        work_since_interrupt
      )) {
    return FALSE;
  }
  SEXP core = VECTOR_ELT(roots, SOURCE_CORE);
  SEXP state = VECTOR_ELT(roots, SOURCE_STATE);
  const paradox_core_kind_t kind = paradox_core_kind(core);
  return paradox_core_payload(core) == state &&
    VECTOR_ELT(state, PARADOX_CORE_PARAMS) ==
      VECTOR_ELT(roots, SOURCE_PARAMS) &&
    VECTOR_ELT(state, PARADOX_CORE_TAGS) ==
      VECTOR_ELT(roots, SOURCE_TAGS) &&
    VECTOR_ELT(state, PARADOX_CORE_TRAFOS) ==
      VECTOR_ELT(roots, SOURCE_TRAFOS) &&
    VECTOR_ELT(state, PARADOX_CORE_CONSTRAINT) ==
      VECTOR_ELT(roots, SOURCE_CONSTRAINT) &&
    VECTOR_ELT(state, PARADOX_CORE_EXTRA_TRAFO) ==
      VECTOR_ELT(roots, SOURCE_EXTRA_TRAFO) &&
    (kind == PARADOX_CORE_COLLECTION ||
      (VECTOR_ELT(state, PARADOX_CORE_DEPS) ==
          VECTOR_ELT(roots, SOURCE_DEPS) &&
        VECTOR_ELT(state, PARADOX_CORE_VALUES) ==
          VECTOR_ELT(roots, SOURCE_VALUES))) &&
    (kind != PARADOX_CORE_SHADOW ||
      paradox_shadow_signature_receipt_is_current(
        core,
        VECTOR_ELT(roots, SOURCE_SHADOW_SIGNATURE),
        VECTOR_ELT(roots, SOURCE_SHADOW_SIGNATURE_CONTENT)
      ));
}

static int graph_has_callback(const paradox_collection_graph_t *graph,
    int field) {
  for (R_xlen_t index = 0; index < graph->count; ++index) {
    if (VECTOR_ELT(graph->nodes[index].state, field) != R_NilValue) {
      return TRUE;
    }
  }
  return FALSE;
}

static SEXP new_subset_bundle(SEXP token, SEXP detach,
    int keep_constraint, int keep_trafo) {
  SEXP result = PROTECT(Rf_allocVector(VECSXP, SUBSET_BUNDLE_FIELD_COUNT));
  SEXP names = PROTECT(Rf_allocVector(
    STRSXP,
    SUBSET_BUNDLE_FIELD_COUNT
  ));
  static const char *const field_names[] = {
    "token", "detach", "keep_constraint", "keep_trafo"
  };
  SET_VECTOR_ELT(result, SUBSET_BUNDLE_TOKEN, token);
  SET_VECTOR_ELT(result, SUBSET_BUNDLE_DETACH, detach);
  SET_VECTOR_ELT(
    result,
    SUBSET_BUNDLE_KEEP_CONSTRAINT,
    Rf_ScalarLogical(keep_constraint)
  );
  SET_VECTOR_ELT(
    result,
    SUBSET_BUNDLE_KEEP_TRAFO,
    Rf_ScalarLogical(keep_trafo)
  );
  for (int field = 0; field < SUBSET_BUNDLE_FIELD_COUNT; ++field) {
    SET_STRING_ELT(names, field, Rf_mkChar(field_names[field]));
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
}

static SEXP subset_column(SEXP source, const R_xlen_t *rows,
    R_xlen_t row_count) {
  SEXP result = PROTECT(Rf_allocVector((SEXPTYPE) TYPEOF(source), row_count));
  for (R_xlen_t output = 0; output < row_count; ++output) {
    const R_xlen_t input = rows[output];
    switch (TYPEOF(source)) {
    case STRSXP:
      SET_STRING_ELT(result, output, STRING_ELT(source, input));
      break;
    case VECSXP:
      SET_VECTOR_ELT(result, output, VECTOR_ELT(source, input));
      break;
    case REALSXP:
      SET_REAL_ELT(result, output, REAL_ELT(source, input));
      break;
    case INTSXP:
      SET_INTEGER_ELT(result, output, INTEGER_ELT(source, input));
      break;
    case LGLSXP:
      SET_LOGICAL_ELT(result, output, LOGICAL_ELT(source, input));
      break;
    case CPLXSXP:
      paradox_api_set_complex_elt(
        result,
        output,
        COMPLEX_ELT(source, input)
      );
      break;
    case RAWSXP:
      paradox_api_set_raw_elt(result, output, RAW_ELT(source, input));
      break;
    default:
      UNPROTECT(1);
      Rf_error("Internal error: unsupported ParamSet table column type");
    }
  }
  UNPROTECT(1);
  return result;
}

static SEXP subset_table(SEXP source, const R_xlen_t *rows,
    R_xlen_t row_count, const char *const *column_names,
    R_xlen_t column_count) {
  SEXP result = PROTECT(Rf_allocVector(VECSXP, column_count));
  for (R_xlen_t column = 0; column < column_count; ++column) {
    SEXP selected = PROTECT(subset_column(
      VECTOR_ELT(source, column),
      rows,
      row_count
    ));
    SET_VECTOR_ELT(result, column, selected);
    UNPROTECT(1);
  }
  (void) paradox_domain_finish_plain_table(
    result,
    column_names,
    column_count,
    row_count
  );
  UNPROTECT(1);
  return result;
}

static R_xlen_t *selected_group_rows(const row_groups_t *groups,
    const R_xlen_t *parameters, R_xlen_t parameter_count,
    R_xlen_t *selected_count) {
  R_xlen_t count = 0;
  for (R_xlen_t request = 0; request < parameter_count; ++request) {
    const R_xlen_t parameter = parameters[request];
    const R_xlen_t increment =
      groups->offsets[parameter + 1] - groups->offsets[parameter];
    if (count > R_XLEN_T_MAX - increment) {
      Rf_error("ParamSet subset is too large");
    }
    count += increment;
  }
  R_xlen_t *rows = paradox_temporary_alloc(count, sizeof(*rows));
  R_xlen_t output = 0;
  for (R_xlen_t request = 0; request < parameter_count; ++request) {
    const R_xlen_t parameter = parameters[request];
    for (R_xlen_t position = groups->offsets[parameter];
        position < groups->offsets[parameter + 1]; ++position) {
      rows[output++] = groups->rows[position];
    }
  }
  *selected_count = count;
  return rows;
}

static SEXP subset_values(const subset_source_t *source,
    const R_xlen_t *parameters, R_xlen_t parameter_count) {
  R_xlen_t count = 0;
  for (R_xlen_t request = 0; request < parameter_count; ++request) {
    count += source->values_by_parameter[parameters[request]] != R_XLEN_T_MAX;
  }
  SEXP result = PROTECT(Rf_allocVector(VECSXP, count));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, count));
  R_xlen_t output = 0;
  for (R_xlen_t request = 0; request < parameter_count; ++request) {
    const R_xlen_t value = source->values_by_parameter[parameters[request]];
    if (value == R_XLEN_T_MAX) {
      continue;
    }
    SET_VECTOR_ELT(result, output, VECTOR_ELT(source->values.values, value));
    SET_STRING_ELT(names, output, STRING_ELT(source->values.names, value));
    ++output;
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
}

static SEXP new_subset_token(SEXP params, SEXP values, SEXP tags, SEXP deps,
    SEXP trafos, SEXP extra_trafo, SEXP constraint) {
  SEXP postfix = PROTECT(Rf_ScalarLogical(FALSE));
  const SEXP fields[PARADOX_CORE_FIELD_COUNT] = {
    [PARADOX_CORE_PARAMS] = params,
    [PARADOX_CORE_VALUES] = values,
    [PARADOX_CORE_TAGS] = tags,
    [PARADOX_CORE_DEPS] = deps,
    [PARADOX_CORE_TRAFOS] = trafos,
    [PARADOX_CORE_EXTRA_TRAFO] = extra_trafo,
    [PARADOX_CORE_CONSTRAINT] = constraint,
    [PARADOX_CORE_SETS] = R_NilValue,
    [PARADOX_CORE_TRANSLATION] = R_NilValue,
    [PARADOX_CORE_POSTFIX] = postfix,
    [PARADOX_CORE_EDGES] = R_NilValue
  };
  SEXP core = PROTECT(paradox_core_new_from_fields(PARADOX_CORE_BASE, fields));
  SEXP token = PROTECT(R_MakeExternalPtr(
    (void *) &subset_token_identity,
    Rf_install("paradox.subset.token.v2"),
    core
  ));
  UNPROTECT(3);
  return token;
}

static SEXP build_token(const subset_source_t *source,
    const R_xlen_t *parameters, R_xlen_t parameter_count,
    int include_dependencies, int allow_dangling, int include_trafos,
    SEXP constraint, SEXP extra_trafo) {
  R_xlen_t tag_count = 0;
  R_xlen_t trafo_count = 0;
  R_xlen_t dependency_count = 0;
  R_xlen_t *tag_rows = selected_group_rows(
    &source->tag_groups,
    parameters,
    parameter_count,
    &tag_count
  );
  R_xlen_t *trafo_rows = include_trafos
    ? selected_group_rows(
      &source->trafo_groups,
      parameters,
      parameter_count,
      &trafo_count
    )
    : paradox_temporary_alloc(0, sizeof(*trafo_rows));
  R_xlen_t *dependency_rows = include_dependencies
    ? selected_group_rows(
      &source->dependency_groups,
      parameters,
      parameter_count,
      &dependency_count
    )
    : paradox_temporary_alloc(0, sizeof(*dependency_rows));

  if (include_dependencies && !allow_dangling && dependency_count != 0) {
    int *selected = paradox_temporary_alloc(
      source->params.row_count,
      sizeof(*selected)
    );
    for (R_xlen_t parameter = 0;
        parameter < source->params.row_count; ++parameter) {
      selected[parameter] = FALSE;
    }
    for (R_xlen_t request = 0; request < parameter_count; ++request) {
      selected[parameters[request]] = TRUE;
    }
    for (R_xlen_t output = 0; output < dependency_count; ++output) {
      const R_xlen_t dependency = dependency_rows[output];
      const R_xlen_t parent = source->dependency_parents[dependency];
      if (parent == R_XLEN_T_MAX || !selected[parent]) {
        Rf_error(
          "Subsetting so that dependencies on params exist which would be "
          "gone: %s. Set `allow_dangling_dependencies = TRUE` to retain it",
          CHAR(STRING_ELT(source->deps.on, dependency))
        );
      }
    }
  }

  SEXP params = PROTECT(subset_table(
    source->params_table,
    parameters,
    parameter_count,
    paradox_domain_column_names,
    PARADOX_DOMAIN_PERMANENT_COLUMNS
  ));
  SEXP tags = PROTECT(subset_table(
    source->tags_table,
    tag_rows,
    tag_count,
    tags_columns,
    2
  ));
  SEXP trafos = PROTECT(subset_table(
    source->trafos_table,
    trafo_rows,
    trafo_count,
    trafos_columns,
    2
  ));
  SEXP deps = PROTECT(subset_table(
    source->deps_table,
    dependency_rows,
    dependency_count,
    deps_columns,
    3
  ));
  SEXP values = PROTECT(subset_values(source, parameters, parameter_count));
  SEXP token = PROTECT(new_subset_token(
    params,
    values,
    tags,
    deps,
    trafos,
    extra_trafo,
    constraint
  ));
  UNPROTECT(6);
  return token;
}

static R_xlen_t *requested_parameters(const subset_source_t *source,
    SEXP ids) {
  const R_xlen_t size = XLENGTH(ids);
  R_xlen_t *parameters = paradox_temporary_alloc(size, sizeof(*parameters));
  SEXP matches = PROTECT(Rf_match(source->params.ids, ids, 0));
  for (R_xlen_t request = 0; request < size; ++request) {
    const R_xlen_t parameter = match_position(matches, request);
    if (parameter == R_XLEN_T_MAX || parameter >= source->params.row_count) {
      SEXP id = STRING_ELT(ids, request);
      UNPROTECT(1);
      unknown_requested_parameter(id);
    }
    parameters[request] = parameter;
  }
  UNPROTECT(1);
  return parameters;
}

static SEXP param_set_subset_state_impl(SEXP private_environment, SEXP self,
    SEXP requested_ids, SEXP allow_dangling_dependencies,
    SEXP keep_constraint, SEXP keep_trafo, SEXP test_hook,
    int select_all) {
  const int allow_dangling = scalar_flag(
    allow_dangling_dependencies,
    "allow_dangling_dependencies"
  );
  const int keep = scalar_flag(keep_constraint, "keep_constraint");
  const int keep_transformations = scalar_flag(keep_trafo, "keep_trafo");

  PROTECT_INDEX ids_index;
  SEXP ids;
  PROTECT_WITH_INDEX(
    ids = select_all ? R_NilValue : snapshot_ids(requested_ids),
    &ids_index
  );
  if (!select_all && Rf_any_duplicated(ids, FALSE) != 0) {
    UNPROTECT(1);
    Rf_error("`ids` must not contain duplicates when creating a subset");
  }
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, SOURCE_ROOT_COUNT));
  PROTECT_INDEX graph_roots_index;
  SEXP graph_roots;
  PROTECT_WITH_INDEX(graph_roots = R_NilValue, &graph_roots_index);
  paradox_collection_graph_t graph;
  int has_graph = FALSE;
  subset_source_t source;
  load_source(
    private_environment,
    self,
    roots,
    &source,
    &graph,
    &graph_roots,
    graph_roots_index,
    &has_graph
  );
  if (select_all) {
    REPROTECT(ids = snapshot_ids(source.params.ids), ids_index);
  }
  R_xlen_t *parameters = requested_parameters(&source, ids);
  SEXP token = PROTECT(build_token(
    &source,
    parameters,
    XLENGTH(ids),
    TRUE,
    allow_dangling,
    keep_transformations,
    keep && !has_graph ? source.constraint : R_NilValue,
    keep_transformations && !has_graph
      ? source.extra_trafo
      : R_NilValue
  ));
  const int detach_constraint = has_graph && keep &&
    graph_has_callback(&graph, PARADOX_CORE_CONSTRAINT);
  const int detach_trafo = has_graph && keep_transformations &&
    graph_has_callback(&graph, PARADOX_CORE_EXTRA_TRAFO);
  SEXP detach = PROTECT(
    detach_constraint || detach_trafo
      ? paradox_param_set_collection_detach_plan_from_graph(&graph, ids)
      : R_NilValue
  );
  SEXP result = PROTECT(new_subset_bundle(
    token,
    detach,
    keep,
    keep_transformations
  ));
  if (test_hook != R_NilValue) {
    SEXP call = PROTECT(Rf_lang1(test_hook));
    SEXP ignored = PROTECT(Rf_eval(call, R_BaseEnv));
    (void) ignored;
    UNPROTECT(2);
  }
  R_xlen_t work_since_interrupt = 0;
  if (!source_snapshot_is_intact(
      roots,
      &graph,
      has_graph,
      &work_since_interrupt
    )) {
    UNPROTECT(6);
    Rf_error("ParamSet changed while constructing a subset");
  }
  UNPROTECT(6);
  return result;
}

SEXP paradox_param_set_subset_state(SEXP private_environment, SEXP self,
    SEXP requested_ids, SEXP allow_dangling_dependencies,
    SEXP keep_constraint, SEXP keep_trafo) {
  return param_set_subset_state_impl(
    private_environment,
    self,
    requested_ids,
    allow_dangling_dependencies,
    keep_constraint,
    keep_trafo,
    R_NilValue,
    FALSE
  );
}

SEXP paradox_param_set_flatten_state(
    SEXP private_environment, SEXP self) {
  /*
   * Select every root ID only after load_source() has admitted one complete
   * BASE/COLLECTION/SHADOW generation. This is the callback-free flatten path:
   * an R-side ID read followed by an ordinary subset could otherwise omit a
   * parameter exposed by a derived-schema refresh between those operations.
   */
  SEXP flag = PROTECT(Rf_ScalarLogical(TRUE));
  SEXP result = PROTECT(param_set_subset_state_impl(
    private_environment,
    self,
    R_NilValue,
    flag,
    flag,
    flag,
    R_NilValue,
    TRUE
  ));
  UNPROTECT(2);
  return result;
}

SEXP paradox_test_param_set_subset_reentry(SEXP private_environment,
    SEXP self, SEXP requested_ids, SEXP allow_dangling_dependencies,
    SEXP keep_constraint, SEXP keep_trafo, SEXP hook) {
  if (!Rf_isFunction(hook)) {
    Rf_error("Subset reentry test hook must be a function");
  }
  return param_set_subset_state_impl(
    private_environment,
    self,
    requested_ids,
    allow_dangling_dependencies,
    keep_constraint,
    keep_trafo,
    hook,
    FALSE
  );
}

static SEXP param_set_subspace_states_impl(SEXP private_environment,
    SEXP self, SEXP requested_ids, int select_all) {
  PROTECT_INDEX ids_index;
  SEXP ids;
  PROTECT_WITH_INDEX(
    ids = select_all ? R_NilValue : snapshot_ids(requested_ids),
    &ids_index
  );
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, SOURCE_ROOT_COUNT));
  PROTECT_INDEX graph_roots_index;
  SEXP graph_roots;
  PROTECT_WITH_INDEX(graph_roots = R_NilValue, &graph_roots_index);
  paradox_collection_graph_t graph;
  int has_graph = FALSE;
  subset_source_t source;
  load_source(
    private_environment,
    self,
    roots,
    &source,
    &graph,
    &graph_roots,
    graph_roots_index,
    &has_graph
  );
  if (select_all) {
    REPROTECT(ids = snapshot_ids(source.params.ids), ids_index);
  }
  R_xlen_t *parameters = requested_parameters(&source, ids);
  const R_xlen_t size = XLENGTH(ids);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  SEXP names = PROTECT(Rf_duplicate(ids));
  Rf_setAttrib(result, R_NamesSymbol, names);
  const int detach_trafo = has_graph &&
    graph_has_callback(&graph, PARADOX_CORE_EXTRA_TRAFO);
  for (R_xlen_t request = 0; request < size; ++request) {
    SEXP token = PROTECT(build_token(
      &source,
      &parameters[request],
      1,
      FALSE,
      TRUE,
      TRUE,
      R_NilValue,
      has_graph ? R_NilValue : source.extra_trafo
    ));
    SEXP selected = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(selected, 0, STRING_ELT(ids, request));
    SEXP detach = PROTECT(
      detach_trafo
        ? paradox_param_set_collection_detach_plan_from_graph(
            &graph,
            selected
          )
        : R_NilValue
    );
    SEXP bundle = PROTECT(new_subset_bundle(
      token,
      detach,
      FALSE,
      TRUE
    ));
    SET_VECTOR_ELT(result, request, bundle);
    UNPROTECT(4);
  }
  R_xlen_t work_since_interrupt = 0;
  if (!source_snapshot_is_intact(
      roots,
      &graph,
      has_graph,
      &work_since_interrupt
    )) {
    UNPROTECT(5);
    Rf_error("ParamSet changed while constructing subspaces");
  }
  UNPROTECT(5);
  return result;
}

SEXP paradox_param_set_subspace_states(SEXP private_environment, SEXP self,
    SEXP requested_ids) {
  return param_set_subspace_states_impl(
    private_environment,
    self,
    requested_ids,
    FALSE
  );
}

SEXP paradox_param_set_all_subspace_states(SEXP private_environment,
    SEXP self) {
  return param_set_subspace_states_impl(
    private_environment,
    self,
    R_NilValue,
    TRUE
  );
}

SEXP paradox_param_set_callback_owner_subset_state(SEXP core, SEXP ids) {
  if (!paradox_core_is_canonical(core) ||
      (paradox_core_kind(core) != PARADOX_CORE_BASE &&
       paradox_core_kind(core) != PARADOX_CORE_SHADOW)) {
    Rf_error("Corrupt ParamSetCollection callback owner capsule");
  }
  SEXP stable_ids = PROTECT(snapshot_ids(ids));
  if (Rf_any_duplicated(stable_ids, FALSE) != 0) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSetCollection callback owner IDs");
  }
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, SOURCE_ROOT_COUNT));
  SEXP state = paradox_core_payload(core);
  if (state == R_UnboundValue) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSetCollection callback owner state");
  }
  subset_source_t source;
  install_source_fields(
    core,
    state,
    roots,
    VECTOR_ELT(state, PARADOX_CORE_VALUES),
    VECTOR_ELT(state, PARADOX_CORE_DEPS),
    &source
  );
  R_xlen_t *parameters = requested_parameters(&source, stable_ids);
  SEXP token = PROTECT(build_token(
    &source,
    parameters,
    XLENGTH(stable_ids),
    TRUE,
    TRUE,
    TRUE,
    R_NilValue,
    R_NilValue
  ));
  if (paradox_core_payload(core) != state ||
      VECTOR_ELT(state, PARADOX_CORE_PARAMS) !=
        VECTOR_ELT(roots, SOURCE_PARAMS) ||
      VECTOR_ELT(state, PARADOX_CORE_TAGS) !=
        VECTOR_ELT(roots, SOURCE_TAGS) ||
      VECTOR_ELT(state, PARADOX_CORE_TRAFOS) !=
        VECTOR_ELT(roots, SOURCE_TRAFOS) ||
      VECTOR_ELT(state, PARADOX_CORE_DEPS) !=
        VECTOR_ELT(roots, SOURCE_DEPS) ||
      VECTOR_ELT(state, PARADOX_CORE_VALUES) !=
        VECTOR_ELT(roots, SOURCE_VALUES) ||
      (source.kind == PARADOX_CORE_SHADOW &&
        !paradox_shadow_signature_receipt_is_current(
          core,
          VECTOR_ELT(roots, SOURCE_SHADOW_SIGNATURE),
          VECTOR_ELT(roots, SOURCE_SHADOW_SIGNATURE_CONTENT)
        ))) {
    UNPROTECT(3);
    Rf_error("ParamSet callback owner changed while constructing a subset");
  }
  UNPROTECT(3);
  return token;
}

SEXP paradox_param_set_base_snapshot_state(SEXP private_environment,
    SEXP self) {
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, SOURCE_ROOT_COUNT));
  PROTECT_INDEX graph_roots_index;
  SEXP graph_roots;
  PROTECT_WITH_INDEX(graph_roots = R_NilValue, &graph_roots_index);
  paradox_collection_graph_t graph;
  int has_graph = FALSE;
  subset_source_t source;
  load_source(
    private_environment,
    self,
    roots,
    &source,
    &graph,
    &graph_roots,
    graph_roots_index,
    &has_graph
  );
  if (paradox_core_kind(VECTOR_ELT(roots, SOURCE_CORE)) !=
      PARADOX_CORE_BASE) {
    UNPROTECT(2);
    Rf_error("ObjectTuneToken ParamSet content must be an exact BASE ParamSet");
  }
  if (source.params.row_count == 0) {
    UNPROTECT(2);
    Rf_error("ObjectTuneToken ParamSet content must be nonempty and bounded");
  }
  SEXP storage = VECTOR_ELT(
    source.params.table,
    PARADOX_DOMAIN_STORAGE_TYPE
  );
  SEXP lower = VECTOR_ELT(source.params.table, PARADOX_DOMAIN_LOWER);
  SEXP upper = VECTOR_ELT(source.params.table, PARADOX_DOMAIN_UPPER);
  for (R_xlen_t row = 0; row < source.params.row_count; ++row) {
    SEXP cls = STRING_ELT(source.params.classes, row);
    SEXP storage_type = STRING_ELT(storage, row);
    const int discrete =
      (paradox_domain_string_is(cls, "ParamFct") &&
        paradox_domain_string_is(storage_type, "character")) ||
      (paradox_domain_string_is(cls, "ParamLgl") &&
        paradox_domain_string_is(storage_type, "logical"));
    const int numeric =
      (paradox_domain_string_is(cls, "ParamDbl") &&
        paradox_domain_string_is(storage_type, "numeric")) ||
      (paradox_domain_string_is(cls, "ParamInt") &&
        paradox_domain_string_is(storage_type, "integer"));
    double lower_value;
    double upper_value;
    if (TYPEOF(lower) == REALSXP) {
      lower_value = REAL_ELT(lower, row);
    } else {
      const int value = INTEGER_ELT(lower, row);
      lower_value = (double) value;
    }
    if (TYPEOF(upper) == REALSXP) {
      upper_value = REAL_ELT(upper, row);
    } else {
      const int value = INTEGER_ELT(upper, row);
      upper_value = (double) value;
    }
    if (!discrete && (!numeric || !R_FINITE(lower_value) ||
        !R_FINITE(upper_value))) {
      UNPROTECT(2);
      Rf_error("ObjectTuneToken ParamSet content must be nonempty and bounded");
    }
  }
  R_xlen_t *parameters = paradox_temporary_alloc(
    source.params.row_count,
    sizeof(*parameters)
  );
  for (R_xlen_t row = 0; row < source.params.row_count; ++row) {
    parameters[row] = row;
  }
  SEXP token = PROTECT(build_token(
    &source,
    parameters,
    source.params.row_count,
    TRUE,
    TRUE,
    TRUE,
    source.constraint,
    source.extra_trafo
  ));
  R_xlen_t work_since_interrupt = 0;
  if (!source_snapshot_is_intact(
      roots,
      &graph,
      has_graph,
      &work_since_interrupt
    )) {
    UNPROTECT(3);
    Rf_error("ParamSet changed while constructing a snapshot");
  }
  UNPROTECT(3);
  return token;
}

static SEXP token_core(SEXP token) {
  if (TYPEOF(token) != EXTPTRSXP ||
      R_ExternalPtrAddr(token) != (void *) &subset_token_identity ||
      R_ExternalPtrTag(token) != Rf_install("paradox.subset.token.v2")) {
    return R_UnboundValue;
  }
  SEXP core = R_ExternalPtrProtected(token);
  return paradox_core_is_valid(core) &&
      paradox_core_kind(core) == PARADOX_CORE_BASE
    ? core
    : R_UnboundValue;
}

static void validate_token_core(SEXP core) {
  SEXP state = paradox_core_payload(core);
  if (state == R_UnboundValue) {
    Rf_error("Corrupt ParamSet subset token");
  }
  require_callback(VECTOR_ELT(state, PARADOX_CORE_EXTRA_TRAFO), "extra_trafo");
  require_callback(VECTOR_ELT(state, PARADOX_CORE_CONSTRAINT), "constraint");
  SEXP postfix = VECTOR_ELT(state, PARADOX_CORE_POSTFIX);
  if (VECTOR_ELT(state, PARADOX_CORE_SETS) != R_NilValue ||
      VECTOR_ELT(state, PARADOX_CORE_TRANSLATION) != R_NilValue ||
      TYPEOF(postfix) != LGLSXP || ALTREP(postfix) ||
      XLENGTH(postfix) != 1 || LOGICAL_ELT(postfix, 0) != FALSE) {
    Rf_error("Corrupt ParamSet subset token payload");
  }
  subset_source_t validated;
  validate_source_fields(
    VECTOR_ELT(state, PARADOX_CORE_PARAMS),
    VECTOR_ELT(state, PARADOX_CORE_TAGS),
    VECTOR_ELT(state, PARADOX_CORE_TRAFOS),
    VECTOR_ELT(state, PARADOX_CORE_DEPS),
    VECTOR_ELT(state, PARADOX_CORE_VALUES),
    &validated
  );
}

SEXP paradox_param_set_adopt_subset_state(SEXP private_environment,
    SEXP token) {
  SEXP core = token_core(token);
  if (core == R_UnboundValue) {
    return Rf_ScalarLogical(FALSE);
  }
  validate_token_core(core);
  if (private_environment == R_NilValue) {
    return Rf_ScalarLogical(TRUE);
  }
  if (TYPEOF(private_environment) != ENVSXP) {
    return Rf_ScalarLogical(FALSE);
  }

  /* The only supported destination is the package-owned R6 constructor while
  * its ordinary `.core` field is still NULL. Never overwrite an initialized
  * capsule: besides corrupting the destination, doing so would consume a
  * capability that its intended fresh constructor could otherwise adopt. */
  SEXP core_symbol = Rf_install(".core");
  PROTECT(core);
  SEXP destination_core = PROTECT(
    paradox_api_optional_plain_binding_snapshot(
      private_environment,
      core_symbol
    )
  );
  const int destination_is_fresh = destination_core == R_NilValue &&
    token_core(token) == core;
  UNPROTECT(1);
  if (!destination_is_fresh) {
    UNPROTECT(1);
    return Rf_ScalarLogical(FALSE);
  }

  Rf_defineVar(core_symbol, core, private_environment);
  R_SetExternalPtrProtected(token, R_NilValue);
  R_SetExternalPtrTag(token, R_NilValue);
  R_ClearExternalPtr(token);
  UNPROTECT(1);
  return Rf_ScalarLogical(TRUE);
}
