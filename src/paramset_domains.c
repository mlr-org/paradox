#include <limits.h>
#include <stddef.h>

#include "paradox.h"

#include "core_state.h"
#include "paramset_collection_readers.h"
#include "paramset_domain_common.h"
#include "r_utils.h"

typedef struct {
  SEXPTYPE type;
  SEXP values;
} domain_match_t;

typedef struct {
  paradox_domain_params_t params;
  paradox_domain_tags_t tags;
  paradox_domain_trafos_t trafos;
  paradox_domain_dependencies_t dependencies;
  paradox_domain_values_t values;
} domain_snapshot_t;

enum domain_root_slot {
  DOMAIN_ROOT_CORE = 0,
  DOMAIN_ROOT_PAYLOAD,
  DOMAIN_ROOT_PARAMS,
  DOMAIN_ROOT_TAGS,
  DOMAIN_ROOT_TRAFOS,
  DOMAIN_ROOT_DEPENDENCIES,
  DOMAIN_ROOT_VALUES,
  DOMAIN_ROOT_COUNT
};

static void unknown_domain_id(SEXP id) {
  PROTECT(id);
  if (Rf_getCharCE(id) == CE_BYTES) {
    UNPROTECT(1);
    Rf_error("Unknown bytes-encoded parameter ID");
  }
  /* An id whose bytes are not valid UTF-8 in this locale is escaped rather
   * than failing the message builder with an internal error. */
  SEXP safe_id = PROTECT(paradox_diagnostic_charsxp(id));
  const paradox_utf8_piece_t pieces[] = {
    paradox_utf8_ascii_piece("No param with id '"),
    paradox_utf8_charsxp_piece(safe_id),
    paradox_utf8_ascii_piece("'")
  };
  SEXP message = PROTECT(paradox_utf8_message(pieces, 3));
  paradox_error_from_scalar_string(message);
}

static domain_match_t domain_match(SEXP value, R_xlen_t expected_size) {
  if (XLENGTH(value) != expected_size) {
    Rf_error("Internal error: invalid ParamSet Domain match result");
  }
  if (TYPEOF(value) == INTSXP) {
    return (domain_match_t) {INTSXP, value};
  }
  if (TYPEOF(value) == REALSXP) {
    return (domain_match_t) {REALSXP, value};
  }
  Rf_error("Internal error: unsupported ParamSet Domain match type");
}

static R_xlen_t domain_match_at(const domain_match_t *matches,
    R_xlen_t index) {
  if (matches->type == INTSXP) {
    const int value = INTEGER_ELT(matches->values, index);
    return value == NA_INTEGER || value <= 0 ? 0 : (R_xlen_t) value;
  }
  const double value = REAL_ELT(matches->values, index);
  return !R_FINITE(value) || value <= 0.0 || value > (double) R_XLEN_T_MAX
    ? 0
    : (R_xlen_t) value;
}

static void require_owned_id_matches(const domain_match_t *matches,
    R_xlen_t size, R_xlen_t owner_count, const char *field,
    R_xlen_t *work) {
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_account_work(work);
    const R_xlen_t owner = domain_match_at(matches, index);
    if (owner == 0 || owner > owner_count) {
      Rf_error("Corrupt ParamSet capsule: unknown parameter id in %s", field);
    }
  }
}

static void group_matches(const domain_match_t *matches, R_xlen_t input_size,
    R_xlen_t output_size, R_xlen_t *offsets, R_xlen_t *order,
    R_xlen_t *work) {
  for (R_xlen_t output = 0; output <= output_size; ++output) {
    offsets[output] = 0;
  }
  for (R_xlen_t input = 0; input < input_size; ++input) {
    paradox_account_work(work);
    const R_xlen_t owner = domain_match_at(matches, input);
    if (owner == 0 || owner > output_size) {
      Rf_error("Corrupt ParamSet capsule: invalid Domain owner");
    }
    ++offsets[owner];
  }
  R_xlen_t cumulative = 0;
  for (R_xlen_t output = 0; output < output_size; ++output) {
    const R_xlen_t count = offsets[output + 1];
    offsets[output] = cumulative;
    if (count > R_XLEN_T_MAX - cumulative) {
      Rf_error("ParamSet Domain grouping is too large");
    }
    cumulative += count;
  }
  offsets[output_size] = cumulative;

  R_xlen_t *cursor = paradox_temporary_alloc(
    output_size,
    sizeof(*cursor)
  );
  for (R_xlen_t output = 0; output < output_size; ++output) {
    cursor[output] = offsets[output];
  }
  for (R_xlen_t input = 0; input < input_size; ++input) {
    paradox_account_work(work);
    const R_xlen_t owner = domain_match_at(matches, input) - 1;
    order[cursor[owner]] = input;
    ++cursor[owner];
  }
}

static void index_unique_matches(const domain_match_t *matches,
    R_xlen_t input_size, R_xlen_t output_size, R_xlen_t *indices,
    const char *field, R_xlen_t *work) {
  for (R_xlen_t output = 0; output < output_size; ++output) {
    indices[output] = 0;
  }
  for (R_xlen_t input = 0; input < input_size; ++input) {
    paradox_account_work(work);
    const R_xlen_t owner = domain_match_at(matches, input);
    if (owner == 0 || owner > output_size) {
      Rf_error("Corrupt ParamSet capsule: invalid %s owner", field);
    }
    if (indices[owner - 1] != 0) {
      Rf_error("Corrupt ParamSet capsule: duplicate %s", field);
    }
    indices[owner - 1] = input + 1;
  }
}

static SEXP copy_names(SEXP source) {
  const R_xlen_t size = XLENGTH(source);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    SET_STRING_ELT(result, index, STRING_ELT(source, index));
  }
  UNPROTECT(1);
  return result;
}

static void validate_snapshot(SEXP params, SEXP tags, SEXP trafos,
    SEXP dependencies, SEXP values, domain_snapshot_t *snapshot,
    R_xlen_t *work) {
  R_xlen_t unused_row = 0;
  if (!paradox_domain_validate_params(
        params,
        R_NilValue,
        TRUE,
        &snapshot->params,
        &unused_row,
        work
      ) || !paradox_domain_validate_tags(
        tags,
        &snapshot->tags,
        work
      ) || !paradox_domain_validate_trafos(
        trafos,
        &snapshot->trafos,
        work
      ) || !paradox_domain_validate_dependencies(
        dependencies,
        &snapshot->dependencies,
        work
      ) || !paradox_domain_validate_values(
        values,
        &snapshot->values,
        work
      )) {
    Rf_error("Corrupt ParamSet Domain capsule state");
  }

  SEXP tag_owners_sexp = PROTECT(Rf_match(
    snapshot->params.ids,
    snapshot->tags.ids,
    0
  ));
  SEXP trafo_owners_sexp = PROTECT(Rf_match(
    snapshot->params.ids,
    snapshot->trafos.ids,
    0
  ));
  SEXP dependency_owners_sexp = PROTECT(Rf_match(
    snapshot->params.ids,
    snapshot->dependencies.ids,
    0
  ));
  SEXP value_owners_sexp = PROTECT(Rf_match(
    snapshot->params.ids,
    snapshot->values.names,
    0
  ));
  const domain_match_t tag_owners = domain_match(
    tag_owners_sexp,
    snapshot->tags.row_count
  );
  const domain_match_t trafo_owners = domain_match(
    trafo_owners_sexp,
    snapshot->trafos.row_count
  );
  const domain_match_t dependency_owners = domain_match(
    dependency_owners_sexp,
    snapshot->dependencies.row_count
  );
  const domain_match_t value_owners = domain_match(
    value_owners_sexp,
    snapshot->values.size
  );
  require_owned_id_matches(
    &tag_owners,
    snapshot->tags.row_count,
    snapshot->params.row_count,
    "tags",
    work
  );
  require_owned_id_matches(
    &trafo_owners,
    snapshot->trafos.row_count,
    snapshot->params.row_count,
    "transformations",
    work
  );
  require_owned_id_matches(
    &dependency_owners,
    snapshot->dependencies.row_count,
    snapshot->params.row_count,
    "dependencies",
    work
  );
  require_owned_id_matches(
    &value_owners,
    snapshot->values.size,
    snapshot->params.row_count,
    "values",
    work
  );
  UNPROTECT(4);
}

static void load_snapshot(SEXP private_environment, SEXP self, SEXP roots,
    domain_snapshot_t *snapshot, R_xlen_t *work) {
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("Corrupt ParamSet shell ownership");
  }
  SEXP core = paradox_core_from_private(private_environment);
  if (core == R_UnboundValue) {
    Rf_error("Corrupt ParamSet state: missing versioned core capsule");
  }
  if (!paradox_core_is_verified(core)) {
    core = paradox_core_refresh(self, private_environment);
  }
  const paradox_core_kind_t kind = paradox_core_kind(core);
  if (kind != PARADOX_CORE_BASE && kind != PARADOX_CORE_COLLECTION &&
      kind != PARADOX_CORE_SHADOW) {
    Rf_error("Corrupt ParamSet state: unknown capsule kind");
  }
  SET_VECTOR_ELT(roots, DOMAIN_ROOT_CORE, core);
  SEXP payload = paradox_core_payload(core);
  if (payload == R_UnboundValue) {
    Rf_error("Corrupt ParamSet state capsule");
  }
  SET_VECTOR_ELT(roots, DOMAIN_ROOT_PAYLOAD, payload);

  SEXP params = VECTOR_ELT(payload, PARADOX_CORE_PARAMS);
  SEXP tags = VECTOR_ELT(payload, PARADOX_CORE_TAGS);
  SEXP trafos = VECTOR_ELT(payload, PARADOX_CORE_TRAFOS);
  SEXP dependencies = VECTOR_ELT(payload, PARADOX_CORE_DEPS);
  SEXP values = VECTOR_ELT(payload, PARADOX_CORE_VALUES);
  SET_VECTOR_ELT(roots, DOMAIN_ROOT_PARAMS, params);
  SET_VECTOR_ELT(roots, DOMAIN_ROOT_TAGS, tags);
  SET_VECTOR_ELT(roots, DOMAIN_ROOT_TRAFOS, trafos);

  if (kind == PARADOX_CORE_COLLECTION) {
    /* Dynamic Domain fields must come from one graph generation.  Build the
     * collection graph once, then consume its two canonical internal
     * projections directly; constructing a public dependency data.table only
     * to copy it back to a plain data.frame adds no semantics. */
    PROTECT_INDEX graph_roots_index;
    SEXP graph_roots;
    PROTECT_WITH_INDEX(graph_roots = R_NilValue, &graph_roots_index);
    paradox_collection_graph_t graph;
    paradox_collection_graph_build(
      private_environment,
      self,
      &graph,
      &graph_roots,
      graph_roots_index,
      work
    );

    values = PROTECT(paradox_collection_values_from_graph(&graph, work));
    SET_VECTOR_ELT(roots, DOMAIN_ROOT_VALUES, values);
    UNPROTECT(1);

    dependencies = PROTECT(paradox_collection_dependencies_from_graph(
      &graph,
      work
    ));
    SET_VECTOR_ELT(roots, DOMAIN_ROOT_DEPENDENCIES, dependencies);
    /* The fixed operation roots now retain both results. */
    UNPROTECT(2);
  } else {
    SET_VECTOR_ELT(roots, DOMAIN_ROOT_VALUES, values);
    SET_VECTOR_ELT(roots, DOMAIN_ROOT_DEPENDENCIES, dependencies);
  }

  validate_snapshot(
    VECTOR_ELT(roots, DOMAIN_ROOT_PARAMS),
    VECTOR_ELT(roots, DOMAIN_ROOT_TAGS),
    VECTOR_ELT(roots, DOMAIN_ROOT_TRAFOS),
    VECTOR_ELT(roots, DOMAIN_ROOT_DEPENDENCIES),
    VECTOR_ELT(roots, DOMAIN_ROOT_VALUES),
    snapshot,
    work
  );
}

static SEXP build_all_domains(const domain_snapshot_t *snapshot,
    R_xlen_t *work) {
  const R_xlen_t parameter_count = snapshot->params.row_count;
  SEXP tag_owners_sexp = PROTECT(Rf_match(
    snapshot->params.ids,
    snapshot->tags.ids,
    0
  ));
  SEXP dependency_owners_sexp = PROTECT(Rf_match(
    snapshot->params.ids,
    snapshot->dependencies.ids,
    0
  ));
  SEXP trafo_owners_sexp = PROTECT(Rf_match(
    snapshot->params.ids,
    snapshot->trafos.ids,
    0
  ));
  SEXP value_owners_sexp = PROTECT(Rf_match(
    snapshot->params.ids,
    snapshot->values.names,
    0
  ));
  const domain_match_t tag_owners = domain_match(
    tag_owners_sexp,
    snapshot->tags.row_count
  );
  const domain_match_t dependency_owners = domain_match(
    dependency_owners_sexp,
    snapshot->dependencies.row_count
  );
  const domain_match_t trafo_owners = domain_match(
    trafo_owners_sexp,
    snapshot->trafos.row_count
  );
  const domain_match_t value_owners = domain_match(
    value_owners_sexp,
    snapshot->values.size
  );

  if (parameter_count == R_XLEN_T_MAX) {
    UNPROTECT(4);
    Rf_error("ParamSet Domain result is too large");
  }
  R_xlen_t *tag_offsets = paradox_temporary_alloc(
    parameter_count + 1,
    sizeof(*tag_offsets)
  );
  R_xlen_t *tag_order = paradox_temporary_alloc(
    snapshot->tags.row_count,
    sizeof(*tag_order)
  );
  R_xlen_t *dependency_offsets = paradox_temporary_alloc(
    parameter_count + 1,
    sizeof(*dependency_offsets)
  );
  R_xlen_t *dependency_order = paradox_temporary_alloc(
    snapshot->dependencies.row_count,
    sizeof(*dependency_order)
  );
  R_xlen_t *trafo_indices = paradox_temporary_alloc(
    parameter_count,
    sizeof(*trafo_indices)
  );
  R_xlen_t *value_indices = paradox_temporary_alloc(
    parameter_count,
    sizeof(*value_indices)
  );
  group_matches(
    &tag_owners,
    snapshot->tags.row_count,
    parameter_count,
    tag_offsets,
    tag_order,
    work
  );
  group_matches(
    &dependency_owners,
    snapshot->dependencies.row_count,
    parameter_count,
    dependency_offsets,
    dependency_order,
    work
  );
  index_unique_matches(
    &trafo_owners,
    snapshot->trafos.row_count,
    parameter_count,
    trafo_indices,
    "transformation",
    work
  );
  index_unique_matches(
    &value_owners,
    snapshot->values.size,
    parameter_count,
    value_indices,
    "value",
    work
  );

  SEXP result = PROTECT(Rf_allocVector(VECSXP, parameter_count));
  SEXP names = PROTECT(copy_names(snapshot->params.ids));
  Rf_setAttrib(result, R_NamesSymbol, names);
  for (R_xlen_t row = 0; row < parameter_count; ++row) {
    paradox_account_work(work);
    const R_xlen_t first_tag = tag_offsets[row];
    const R_xlen_t tag_count = tag_offsets[row + 1] - first_tag;
    const R_xlen_t first_dependency = dependency_offsets[row];
    const R_xlen_t dependency_count =
      dependency_offsets[row + 1] - first_dependency;
    const R_xlen_t trafo_index = trafo_indices[row];
    const R_xlen_t value_index = value_indices[row];
    const paradox_domain_row_t domain_row = {
      &snapshot->params,
      row,
      &snapshot->tags,
      tag_count == 0 ? NULL : tag_order + first_tag,
      tag_count,
      trafo_index == 0
        ? R_NilValue
        : VECTOR_ELT(snapshot->trafos.values, trafo_index - 1),
      &snapshot->dependencies,
      dependency_count == 0 ? NULL : dependency_order + first_dependency,
      dependency_count,
      value_index != 0,
      value_index == 0
        ? R_NilValue
        : VECTOR_ELT(snapshot->values.values, value_index - 1)
    };
    SEXP domain = PROTECT(Rf_allocVector(
      VECSXP,
      PARADOX_DOMAIN_COLUMN_COUNT
    ));
    SEXP prepared = PROTECT(paradox_domain_fill(domain, &domain_row, work));
    SET_VECTOR_ELT(result, row, prepared);
    UNPROTECT(2);
  }
  UNPROTECT(6);
  return result;
}

static SEXP build_one_domain(const domain_snapshot_t *snapshot, SEXP id,
    R_xlen_t *work) {
  R_xlen_t parameter_row = R_XLEN_T_MAX;
  for (R_xlen_t row = 0; row < snapshot->params.row_count; ++row) {
    paradox_account_work(work);
    if (paradox_domain_strings_equal(
        STRING_ELT(snapshot->params.ids, row),
        STRING_ELT(id, 0)
      )) {
      parameter_row = row;
      break;
    }
  }
  if (parameter_row == R_XLEN_T_MAX) {
    unknown_domain_id(STRING_ELT(id, 0));
  }

  R_xlen_t *tag_rows = paradox_temporary_alloc(
    snapshot->tags.row_count,
    sizeof(*tag_rows)
  );
  R_xlen_t tag_count = 0;
  for (R_xlen_t row = 0; row < snapshot->tags.row_count; ++row) {
    if (paradox_domain_strings_equal(
        STRING_ELT(snapshot->tags.ids, row),
        STRING_ELT(id, 0)
      )) {
      tag_rows[tag_count++] = row;
    }
  }
  R_xlen_t *dependency_rows = paradox_temporary_alloc(
    snapshot->dependencies.row_count,
    sizeof(*dependency_rows)
  );
  R_xlen_t dependency_count = 0;
  for (R_xlen_t row = 0; row < snapshot->dependencies.row_count; ++row) {
    if (paradox_domain_strings_equal(
        STRING_ELT(snapshot->dependencies.ids, row),
        STRING_ELT(id, 0)
      )) {
      dependency_rows[dependency_count++] = row;
    }
  }

  SEXP trafo = R_NilValue;
  for (R_xlen_t row = 0; row < snapshot->trafos.row_count; ++row) {
    if (paradox_domain_strings_equal(
        STRING_ELT(snapshot->trafos.ids, row),
        STRING_ELT(id, 0)
      )) {
      if (trafo != R_NilValue) {
        Rf_error("Corrupt ParamSet capsule: duplicate transformation");
      }
      trafo = VECTOR_ELT(snapshot->trafos.values, row);
    }
  }
  int init_given = FALSE;
  SEXP init_value = R_NilValue;
  for (R_xlen_t row = 0; row < snapshot->values.size; ++row) {
    if (paradox_domain_strings_equal(
        STRING_ELT(snapshot->values.names, row),
        STRING_ELT(id, 0)
      )) {
      if (init_given) {
        Rf_error("Corrupt ParamSet capsule: duplicate value");
      }
      init_given = TRUE;
      init_value = VECTOR_ELT(snapshot->values.values, row);
    }
  }
  const paradox_domain_row_t domain_row = {
    &snapshot->params,
    parameter_row,
    &snapshot->tags,
    tag_count == 0 ? NULL : tag_rows,
    tag_count,
    trafo,
    &snapshot->dependencies,
    dependency_count == 0 ? NULL : dependency_rows,
    dependency_count,
    init_given,
    init_value
  };
  SEXP domain = PROTECT(Rf_allocVector(
    VECSXP,
    PARADOX_DOMAIN_COLUMN_COUNT
  ));
  SEXP result = PROTECT(paradox_domain_fill(domain, &domain_row, work));
  UNPROTECT(2);
  return result;
}

SEXP paradox_param_set_domains_select(SEXP private_environment, SEXP self,
    SEXP id) {
  if (id != R_NilValue && (TYPEOF(id) != STRSXP || XLENGTH(id) != 1)) {
    Rf_error("`id` must be one non-missing string");
  }
  int protected_count = 0;
  SEXP requested = R_NilValue;
  if (id != R_NilValue) {
    requested = PROTECT(Rf_allocVector(STRSXP, 1));
    ++protected_count;
    SEXP value = STRING_ELT(id, 0);
    if (value == NA_STRING) {
      UNPROTECT(protected_count);
      Rf_error("`id` must be one non-missing string");
    }
    SET_STRING_ELT(requested, 0, value);
  }
  R_xlen_t work = 0;
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, DOMAIN_ROOT_COUNT));
  ++protected_count;
  domain_snapshot_t snapshot;
  load_snapshot(private_environment, self, roots, &snapshot, &work);
  SEXP result = requested == R_NilValue
    ? build_all_domains(&snapshot, &work)
    : build_one_domain(&snapshot, requested, &work);
  UNPROTECT(protected_count);
  return result;
}

SEXP paradox_param_set_domains(SEXP private_environment, SEXP self) {
  return paradox_param_set_domains_select(
    private_environment,
    self,
    R_NilValue
  );
}

/* `$domains` and `$get_domain()` deliberately share one capsule snapshot
 * engine.  Keeping this registered entry point as a tiny forwarder beside the
 * shared selector preserves the public/native symbol while avoiding a second
 * implementation. */
SEXP paradox_param_set_get_domain(SEXP private_environment, SEXP self,
    SEXP id) {
  return paradox_param_set_domains_select(private_environment, self, id);
}
