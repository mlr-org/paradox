#include "paradox.h"

#include "paramset_domain_common.h"
#include "paramset_params_internal.h"
#include "r_utils.h"

typedef enum {
  SELF_KIND_UNKNOWN = 0,
  SELF_KIND_PARAM_SET,
  SELF_KIND_COLLECTION
} self_kind_t;

enum get_domain_root_slot {
  GET_DOMAIN_ROOT_PARAMS_SOURCE = 0,
  GET_DOMAIN_ROOT_PARAM_COLUMNS,
  GET_DOMAIN_ROOT_PARAMS_SNAPSHOT,
  GET_DOMAIN_ROOT_PARAM_IDS,
  GET_DOMAIN_ROOT_PARAM_CLASSES,
  GET_DOMAIN_ROOT_TAGS_SOURCE,
  GET_DOMAIN_ROOT_TAG_IDS,
  GET_DOMAIN_ROOT_TAG_VALUES,
  GET_DOMAIN_ROOT_TRAFOS_SOURCE,
  GET_DOMAIN_ROOT_TRAFO_IDS,
  GET_DOMAIN_ROOT_TRAFO_VALUES,
  GET_DOMAIN_ROOT_VALUES_SOURCE,
  GET_DOMAIN_ROOT_VALUE_NAMES,
  GET_DOMAIN_ROOT_DEPENDENCIES_SOURCE,
  GET_DOMAIN_ROOT_DEPENDENCY_IDS,
  GET_DOMAIN_ROOT_DEPENDENCY_ON,
  GET_DOMAIN_ROOT_DEPENDENCY_CONDITIONS,
  GET_DOMAIN_ROOT_REQUESTED_ID,
  GET_DOMAIN_ROOT_SELECTED_INIT,
  GET_DOMAIN_ROOT_SELECTED_TRAFO,
  GET_DOMAIN_ROOT_COUNT
};

static self_kind_t self_kind(SEXP self,
    R_xlen_t *work_since_interrupt) {
  static const char *const param_set_classes[] = {"ParamSet", "R6"};
  static const char *const collection_classes[] = {
    "ParamSetCollection", "ParamSet", "R6"
  };
  SEXP classes = Rf_getAttrib(self, R_ClassSymbol);
  if (TYPEOF(self) == ENVSXP && paradox_domain_exact_string_vector(
      classes,
      param_set_classes,
      2,
      work_since_interrupt
    )) {
    return SELF_KIND_PARAM_SET;
  }
  if (TYPEOF(self) == ENVSXP && paradox_domain_exact_string_vector(
      classes,
      collection_classes,
      3,
      work_since_interrupt
    )) {
    return SELF_KIND_COLLECTION;
  }
  return SELF_KIND_UNKNOWN;
}

static void evaluate_public_member_into(SEXP state, R_xlen_t index,
    SEXP self, const char *name) {
  SEXP call = PROTECT(Rf_lang3(
    R_DollarSymbol,
    self,
    Rf_install(name)
  ));
  SEXP result = PROTECT(Rf_eval(call, R_BaseEnv));
  SET_VECTOR_ELT(state, index, result);
  UNPROTECT(2);
}

static SEXP decline_after_collection_callbacks(self_kind_t kind) {
  if (kind == SELF_KIND_COLLECTION) {
    Rf_error(
      "ParamSetCollection get_domain callbacks produced unsupported state"
    );
  }
  return R_NilValue;
}

static SEXP snapshot_parameter_row(SEXP columns, R_xlen_t row) {
  SEXP result = PROTECT(Rf_allocVector(VECSXP, PARADOX_DOMAIN_TAGS));
  for (enum paradox_domain_column column = PARADOX_DOMAIN_ID;
      column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    SEXP source = PROTECT(VECTOR_ELT(columns, column));
    SEXP output = PROTECT(Rf_allocVector((SEXPTYPE) TYPEOF(source), 1));
    switch (TYPEOF(source)) {
    case STRSXP:
      SET_STRING_ELT(output, 0, STRING_ELT(source, row));
      break;
    case VECSXP:
      SET_VECTOR_ELT(output, 0, VECTOR_ELT(source, row));
      break;
    case REALSXP:
      SET_REAL_ELT(output, 0, REAL_ELT(source, row));
      break;
    case INTSXP:
      SET_INTEGER_ELT(output, 0, INTEGER_ELT(source, row));
      break;
    default:
      UNPROTECT(3);
      Rf_error("Internal error: unsupported ParamSet Domain column type");
    }
    SET_VECTOR_ELT(result, column, output);
    UNPROTECT(2);
  }
  UNPROTECT(1);
  return result;
}

SEXP paradox_param_set_get_domain(
    SEXP private_environment,
    SEXP self,
    SEXP id) {
  R_xlen_t work_since_interrupt = 0;
  const self_kind_t kind = self_kind(self, &work_since_interrupt);
  if (kind == SELF_KIND_UNKNOWN ||
      !paradox_domain_owns_private_environment(self, private_environment) ||
      TYPEOF(id) != STRSXP || ALTREP(id) || Rf_isObject(id) ||
      XLENGTH(id) != 1) {
    return R_NilValue;
  }

  SEXP roots = PROTECT(Rf_allocVector(VECSXP, GET_DOMAIN_ROOT_COUNT));
  SEXP requested_id = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(requested_id, 0, STRING_ELT(id, 0));
  if (STRING_ELT(requested_id, 0) == NA_STRING) {
    UNPROTECT(2);
    return R_NilValue;
  }
  SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_REQUESTED_ID, requested_id);
  UNPROTECT(1);

  if (kind == SELF_KIND_PARAM_SET &&
      (!paradox_params_canonical_active_member(
          self,
          private_environment,
          "values",
          ".__ParamSet__values",
          "xs",
          NULL,
          &work_since_interrupt
        ) || !paradox_params_canonical_active_member(
          self,
          private_environment,
          "deps",
          ".__ParamSet__deps",
          "v",
          NULL,
          &work_since_interrupt
        ) || !paradox_params_canonical_private_getter(
          self,
          private_environment
        ))) {
    UNPROTECT(1);
    return R_NilValue;
  }

  SEXP params_sexp = paradox_domain_local_value(
    private_environment,
    ".params"
  );
  if (params_sexp != R_UnboundValue) {
    SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_PARAMS_SOURCE, params_sexp);
  }
  SEXP tags_sexp = paradox_domain_local_value(private_environment, ".tags");
  if (tags_sexp != R_UnboundValue) {
    SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_TAGS_SOURCE, tags_sexp);
  }
  SEXP trafos_sexp = paradox_domain_local_value(
    private_environment,
    ".trafos"
  );
  if (trafos_sexp != R_UnboundValue) {
    SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_TRAFOS_SOURCE, trafos_sexp);
  }
  if (params_sexp == R_UnboundValue || tags_sexp == R_UnboundValue ||
      trafos_sexp == R_UnboundValue) {
    UNPROTECT(1);
    return R_NilValue;
  }
  paradox_domain_params_t params;
  paradox_domain_tags_t tags;
  paradox_domain_trafos_t trafos;
  R_xlen_t parameter_row = 0;
  /* Allocate the long-lived child root before validation. The validator may
   * allocate while authenticating nested payloads; once it returns, capture
   * every permanent column without another allocation. Later snapshot
   * allocation must never reread a table shell that a GC finalizer can update
   * by reference. */
  SEXP param_columns = PROTECT(Rf_allocVector(
    VECSXP,
    PARADOX_DOMAIN_TAGS
  ));
  SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_PARAM_COLUMNS, param_columns);
  UNPROTECT(1);
  if (!paradox_domain_validate_params(
      params_sexp,
      requested_id,
      FALSE,
      &params,
      &parameter_row,
      &work_since_interrupt
    )) {
    UNPROTECT(1);
    return R_NilValue;
  }
  for (enum paradox_domain_column column = PARADOX_DOMAIN_ID;
      column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    SET_VECTOR_ELT(
      param_columns,
      column,
      VECTOR_ELT(params_sexp, column)
    );
  }
  params.ids = VECTOR_ELT(param_columns, PARADOX_DOMAIN_ID);
  params.classes = VECTOR_ELT(param_columns, PARADOX_DOMAIN_CLS);
  SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_PARAM_IDS, params.ids);
  SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_PARAM_CLASSES, params.classes);

  SEXP params_snapshot = PROTECT(snapshot_parameter_row(
    param_columns,
    parameter_row
  ));
  SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_PARAMS_SNAPSHOT, params_snapshot);
  UNPROTECT(1);
  params.table = params_snapshot;
  params.ids = VECTOR_ELT(params_snapshot, PARADOX_DOMAIN_ID);
  params.classes = VECTOR_ELT(params_snapshot, PARADOX_DOMAIN_CLS);
  params.row_count = 1;
  parameter_row = 0;
  SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_PARAM_IDS, params.ids);
  SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_PARAM_CLASSES, params.classes);

  if (!paradox_domain_validate_tags(
      tags_sexp,
      &tags,
      &work_since_interrupt
    )) {
    UNPROTECT(1);
    return R_NilValue;
  }
  SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_TAG_IDS, tags.ids);
  SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_TAG_VALUES, tags.values);
  if (!paradox_domain_validate_trafos(
      trafos_sexp,
      &trafos,
      &work_since_interrupt
    )) {
    UNPROTECT(1);
    return R_NilValue;
  }
  SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_TRAFO_IDS, trafos.ids);
  SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_TRAFO_VALUES, trafos.values);

  SEXP values_sexp;
  SEXP dependencies_sexp;
  if (kind == SELF_KIND_PARAM_SET) {
    values_sexp = paradox_domain_local_value(
      private_environment,
      ".values"
    );
    if (values_sexp != R_UnboundValue) {
      SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_VALUES_SOURCE, values_sexp);
    }
    dependencies_sexp = paradox_domain_local_value(
      private_environment,
      ".deps"
    );
    if (dependencies_sexp != R_UnboundValue) {
      SET_VECTOR_ELT(
        roots,
        GET_DOMAIN_ROOT_DEPENDENCIES_SOURCE,
        dependencies_sexp
      );
    }
    if (values_sexp == R_UnboundValue ||
        dependencies_sexp == R_UnboundValue) {
      UNPROTECT(1);
      return R_NilValue;
    }
  } else {
    /* Collection access deliberately calls the public active bindings below:
     * their child extension points may allocate, rebind storage, or modify
     * data.table columns by reference.  Materialize the selected permanent row
     * exactly as the R fallback does before those callbacks. Tags and
     * transformations are intentionally fetched again afterwards because the
     * R fallback observes their live post-callback state. */
    /* Keep every post-callback source table in the protected state.  Merely
     * reading a binding into a C local is not a GC root: a later allocation
     * may run a finalizer which rebinds the private slot and makes the old
     * table (and an extracted closure) unreachable. */
    evaluate_public_member_into(
      roots,
      GET_DOMAIN_ROOT_VALUES_SOURCE,
      self,
      "values"
    );
    evaluate_public_member_into(
      roots,
      GET_DOMAIN_ROOT_DEPENDENCIES_SOURCE,
      self,
      "deps"
    );
    values_sexp = VECTOR_ELT(roots, GET_DOMAIN_ROOT_VALUES_SOURCE);
    dependencies_sexp = VECTOR_ELT(
      roots,
      GET_DOMAIN_ROOT_DEPENDENCIES_SOURCE
    );

    tags_sexp = paradox_domain_local_value(private_environment, ".tags");
    if (tags_sexp != R_UnboundValue) {
      SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_TAGS_SOURCE, tags_sexp);
    }
    trafos_sexp = paradox_domain_local_value(
      private_environment,
      ".trafos"
    );
    if (trafos_sexp != R_UnboundValue) {
      SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_TRAFOS_SOURCE, trafos_sexp);
    }
    if (tags_sexp == R_UnboundValue || trafos_sexp == R_UnboundValue) {
      UNPROTECT(1);
      return decline_after_collection_callbacks(kind);
    }
    if (!paradox_domain_validate_tags(
          tags_sexp,
          &tags,
          &work_since_interrupt
        )) {
      UNPROTECT(1);
      return decline_after_collection_callbacks(kind);
    }
    SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_TAG_IDS, tags.ids);
    SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_TAG_VALUES, tags.values);
    if (!paradox_domain_validate_trafos(
          trafos_sexp,
          &trafos,
          &work_since_interrupt
        )) {
      UNPROTECT(1);
      return decline_after_collection_callbacks(kind);
    }
    SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_TRAFO_IDS, trafos.ids);
    SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_TRAFO_VALUES, trafos.values);
  }

  paradox_domain_values_t values;
  paradox_domain_dependencies_t dependencies;
  if (!paradox_domain_validate_values(
      values_sexp,
      &values,
      &work_since_interrupt
    )) {
    UNPROTECT(1);
    return decline_after_collection_callbacks(kind);
  }
  SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_VALUE_NAMES, values.names);
  if (!paradox_domain_validate_dependencies(
      dependencies_sexp,
      &dependencies,
      &work_since_interrupt
    )) {
    UNPROTECT(1);
    return decline_after_collection_callbacks(kind);
  }
  SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_DEPENDENCY_IDS, dependencies.ids);
  SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_DEPENDENCY_ON, dependencies.on);
  SET_VECTOR_ELT(
    roots,
    GET_DOMAIN_ROOT_DEPENDENCY_CONDITIONS,
    dependencies.conditions
  );

  R_xlen_t init_row = R_XLEN_T_MAX;
  R_xlen_t init_count = 0;
  for (R_xlen_t index = 0; index < values.size; ++index) {
    paradox_domain_account_work(&work_since_interrupt);
    if (paradox_domain_strings_equal(
        STRING_ELT(values.names, index),
        STRING_ELT(requested_id, 0)
      )) {
      if (init_count >= values.size) {
        UNPROTECT(1);
        Rf_error("ParamSet value match count exceeded validated capacity");
      }
      init_row = index;
      ++init_count;
    }
  }
  if (init_count > 1) {
    UNPROTECT(1);
    return decline_after_collection_callbacks(kind);
  }
  const int init_given = init_count == 1;
  SEXP init_value = R_NilValue;
  if (init_given) {
    init_value = VECTOR_ELT(values.values, init_row);
    SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_SELECTED_INIT, init_value);
  }

  R_xlen_t *dependency_rows = paradox_temporary_alloc(
    dependencies.row_count,
    sizeof(*dependency_rows)
  );
  R_xlen_t dependency_count = 0;
  for (R_xlen_t row = 0; row < dependencies.row_count; ++row) {
    paradox_domain_account_work(&work_since_interrupt);
    if (paradox_domain_strings_equal(
        STRING_ELT(dependencies.ids, row),
        STRING_ELT(requested_id, 0)
      )) {
      if (dependency_count >= dependencies.row_count) {
        UNPROTECT(1);
        Rf_error("ParamSet dependency match count exceeded validated capacity");
      }
      dependency_rows[dependency_count] = row;
      ++dependency_count;
    }
  }

  /* Look up the transformation after extension callbacks as well.  The
   * snapshot keeps its list column rooted, but a by-reference callback can
   * replace a shared list cell; retaining the old cell only in a bare C local
   * across allocations would not be safe. */
  R_xlen_t selected_trafo_row = R_XLEN_T_MAX;
  R_xlen_t trafo_count = 0;
  for (R_xlen_t row = 0; row < trafos.row_count; ++row) {
    paradox_domain_account_work(&work_since_interrupt);
    if (paradox_domain_strings_equal(
        STRING_ELT(trafos.ids, row),
        STRING_ELT(requested_id, 0)
      )) {
      if (trafo_count >= trafos.row_count) {
        UNPROTECT(1);
        Rf_error("ParamSet trafo match count exceeded validated capacity");
      }
      selected_trafo_row = row;
      ++trafo_count;
    }
  }
  if (trafo_count > 1) {
    UNPROTECT(1);
    return decline_after_collection_callbacks(kind);
  }
  SEXP selected_trafo = R_NilValue;
  if (trafo_count == 1) {
    selected_trafo = VECTOR_ELT(trafos.values, selected_trafo_row);
    SET_VECTOR_ELT(roots, GET_DOMAIN_ROOT_SELECTED_TRAFO, selected_trafo);
  }

  /* Collection callbacks may legitimately mutate shared column storage even
   * though the selected parameter row above is snapshotted. Collect tag
   * positions in one uninterrupted post-callback operation so the temporary
   * row buffer can never be undersized or contain uninitialized positions. */
  R_xlen_t *tag_rows = paradox_temporary_alloc(
    tags.row_count,
    sizeof(*tag_rows)
  );
  R_xlen_t tag_count = 0;
  for (R_xlen_t row = 0; row < tags.row_count; ++row) {
    paradox_domain_account_work(&work_since_interrupt);
    if (paradox_domain_strings_equal(
        STRING_ELT(tags.ids, row),
        STRING_ELT(requested_id, 0)
      )) {
      if (tag_count >= tags.row_count) {
        UNPROTECT(1);
        Rf_error("ParamSet tag match count exceeded validated capacity");
      }
      tag_rows[tag_count] = row;
      ++tag_count;
    }
  }

  const paradox_domain_row_t domain_row = {
    &params,
    parameter_row,
    &tags,
    tag_rows,
    tag_count,
    selected_trafo,
    &dependencies,
    dependency_rows,
    dependency_count,
    init_given,
    init_value
  };
  SEXP result = PROTECT(Rf_allocVector(
    VECSXP,
    PARADOX_DOMAIN_COLUMN_COUNT
  ));
  paradox_domain_fill(result, &domain_row, &work_since_interrupt);
  UNPROTECT(2);
  return result;
}
