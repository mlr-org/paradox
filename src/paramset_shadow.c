#include <limits.h>
#include <stddef.h>
#include <stdint.h>

#include "paramset_shadow.h"

#include "core_state.h"
#include "paramset_collection_readers.h"
#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

enum shadow_factory_field {
  SHADOW_FACTORY_CONSTRAINT = 0,
  SHADOW_FACTORY_COLLECTION_CONSTRAINT,
  SHADOW_FACTORY_COLLECTION_EXTRA_TRAFO,
  SHADOW_FACTORY_FIELD_COUNT
};

enum shadow_constraint_plan_field {
  SHADOW_CONSTRAINT_CALLBACK = 0,
  SHADOW_CONSTRAINT_HIDDEN_VALUES,
  SHADOW_CONSTRAINT_PLAN_FIELD_COUNT
};

enum collection_detach_field {
  COLLECTION_DETACH_TRANSLATION = 0,
  COLLECTION_DETACH_CONSTRAINT_INDICES,
  COLLECTION_DETACH_CONSTRAINT_SETS,
  COLLECTION_DETACH_TRAFO_INDICES,
  COLLECTION_DETACH_TRAFO_SETS,
  COLLECTION_DETACH_POSTFIX,
  COLLECTION_DETACH_FIELD_COUNT
};

static SEXP shadow_metadata_symbol(void) {
  return Rf_install(".paradox.shadow.snapshot.v1");
}

static int ordinary_list(SEXP value, R_xlen_t size) {
  return TYPEOF(value) == VECSXP && !ALTREP(value) &&
    !Rf_isObject(value) && XLENGTH(value) == size &&
    paradox_api_has_no_attributes(value);
}

static int exact_false(SEXP value) {
  return TYPEOF(value) == LGLSXP && !ALTREP(value) &&
    XLENGTH(value) == 1 && paradox_api_has_no_attributes(value) &&
    LOGICAL_ELT(value, 0) == FALSE;
}

static int exact_shadow_constraint_plan(SEXP plan) {
  static const char *const field_names[SHADOW_CONSTRAINT_PLAN_FIELD_COUNT] = {
    "callback", "hidden_values"
  };
  R_xlen_t work_since_interrupt = 0;
  SEXP names = Rf_getAttrib(plan, R_NamesSymbol);
  return TYPEOF(plan) == VECSXP && !ALTREP(plan) && !Rf_isObject(plan) &&
    XLENGTH(plan) == SHADOW_CONSTRAINT_PLAN_FIELD_COUNT &&
    paradox_api_has_single_attribute(plan, "names") &&
    paradox_api_has_no_attributes(names) &&
    paradox_domain_exact_string_vector(
      names,
      field_names,
      SHADOW_CONSTRAINT_PLAN_FIELD_COUNT,
      &work_since_interrupt
    );
}

static int exact_optional_s3_class(SEXP value) {
  SEXP classes = Rf_getAttrib(value, R_ClassSymbol);
  if (classes == R_NilValue) return TRUE;
  if (TYPEOF(classes) != STRSXP || ALTREP(classes) ||
      !paradox_api_has_no_attributes(classes) || XLENGTH(classes) == 0) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(classes); ++index) {
    SEXP class_name = STRING_ELT(classes, index);
    if (class_name == NA_STRING || CHAR(class_name)[0] == '\0') {
      return FALSE;
    }
  }
  return TRUE;
}

static int shadow_constraint_values_shape(SEXP values, int allow_s3,
    SEXP *names) {
  static const char *const plain_attributes[] = {"names"};
  static const char *const s3_attributes[] = {"names", "class"};
  if (TYPEOF(values) != VECSXP || ALTREP(values) || Rf_isS4(values) ||
      (!allow_s3 && Rf_isObject(values)) ||
      !paradox_api_has_only_attributes(
        values,
        allow_s3 ? s3_attributes : plain_attributes,
        allow_s3 ? 2 : 1
      ) || (allow_s3 && !exact_optional_s3_class(values))) {
    return FALSE;
  }
  SEXP observed_names = Rf_getAttrib(values, R_NamesSymbol);
  if (observed_names == R_NilValue && XLENGTH(values) == 0) {
    if (!allow_s3) return FALSE;
    *names = observed_names;
    return TRUE;
  }
  if (TYPEOF(observed_names) != STRSXP ||
      XLENGTH(observed_names) != XLENGTH(values) ||
      !paradox_api_has_no_attributes(observed_names)) {
    return FALSE;
  }
  *names = observed_names;
  return TRUE;
}

SEXP paradox_param_set_shadow_constraint(SEXP plan, SEXP visible_values) {
  PROTECT(plan);
  PROTECT(visible_values);
  if (!exact_shadow_constraint_plan(plan)) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSetShadow constraint plan");
  }

  SEXP callback = PROTECT(VECTOR_ELT(
    plan,
    SHADOW_CONSTRAINT_CALLBACK
  ));
  SEXP hidden_values = PROTECT(VECTOR_ELT(
    plan,
    SHADOW_CONSTRAINT_HIDDEN_VALUES
  ));
  SEXP hidden_names = R_NilValue;
  SEXP visible_names = R_NilValue;
  if (!Rf_isFunction(callback) || !shadow_constraint_values_shape(
      hidden_values,
      FALSE,
      &hidden_names
    )) {
    UNPROTECT(4);
    Rf_error("Corrupt ParamSetShadow constraint plan");
  }
  if (!shadow_constraint_values_shape(
      visible_values,
      TRUE,
      &visible_names
    )) {
    UNPROTECT(4);
    Rf_error("ParamSetShadow constraint input must be a named list");
  }

  const R_xlen_t hidden_count = XLENGTH(hidden_values);
  const R_xlen_t visible_count = XLENGTH(visible_values);
  if (hidden_count > R_XLEN_T_MAX - visible_count) {
    UNPROTECT(4);
    Rf_error("ParamSetShadow constraint input is too large");
  }
  const R_xlen_t count = hidden_count + visible_count;
  SEXP combined = PROTECT(Rf_allocVector(VECSXP, count));
  SEXP combined_names = PROTECT(Rf_allocVector(STRSXP, count));
  Rf_setAttrib(combined, R_NamesSymbol, combined_names);

  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_domain_account_work(&work_since_interrupt);
    const int hidden = index < hidden_count;
    const R_xlen_t source_index = hidden ? index : index - hidden_count;
    SEXP source_values = hidden ? hidden_values : visible_values;
    SEXP source_names = hidden ? hidden_names : visible_names;
    SEXP name = STRING_ELT(source_names, source_index);
    if (name == NA_STRING || CHAR(name)[0] == '\0') {
      UNPROTECT(6);
      Rf_error(
        "ParamSetShadow constraint input names must be non-missing and non-empty"
      );
    }
    SET_VECTOR_ELT(combined, index, VECTOR_ELT(source_values, source_index));
    SET_STRING_ELT(combined_names, index, name);
  }
  if (Rf_any_duplicated(combined_names, FALSE) != 0) {
    UNPROTECT(6);
    Rf_error("ParamSetShadow constraint input names must be unique");
  }

  SEXP call = PROTECT(Rf_lang2(callback, combined));
  SEXP answer = PROTECT(Rf_eval(call, R_BaseEnv));
  if (TYPEOF(answer) != LGLSXP || XLENGTH(answer) != 1) {
    UNPROTECT(8);
    Rf_error(
      "ParamSetShadow constraint must return one non-missing logical value"
    );
  }
  const int accepted = LOGICAL_ELT(answer, 0);
  if (accepted == NA_LOGICAL) {
    UNPROTECT(8);
    Rf_error(
      "ParamSetShadow constraint must return one non-missing logical value"
    );
  }
  SEXP result = PROTECT(Rf_ScalarLogical(accepted));
  UNPROTECT(9);
  return result;
}

static int string_in(SEXP ids, SEXP sought,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t count = XLENGTH(ids);
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP candidate = STRING_ELT(ids, index);
    if (candidate == sought ||
        paradox_domain_strings_equal(candidate, sought)) {
      return TRUE;
    }
  }
  return FALSE;
}

typedef struct {
  SEXP parameter_ids;
  SEXP *slots;
  R_xlen_t capacity;
} shadow_id_index_t;

static R_xlen_t shadow_id_slot(SEXP id, R_xlen_t capacity) {
  uintptr_t value = (uintptr_t) id;
  value ^= value >> 4;
  value ^= value >> 9;
  return (R_xlen_t) (value & (uintptr_t) (capacity - 1));
}

static shadow_id_index_t build_shadow_id_index(SEXP parameter_ids,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t count = XLENGTH(parameter_ids);
  if (count == 0) {
    return (shadow_id_index_t) {parameter_ids, NULL, 0};
  }
  if (count > R_XLEN_T_MAX / 2) {
    Rf_error("ParamSetShadow parameter index is too large");
  }
  const R_xlen_t required = count * 2;
  R_xlen_t capacity = 1;
  while (capacity < required) {
    if (capacity > R_XLEN_T_MAX / 2) {
      Rf_error("ParamSetShadow parameter index is too large");
    }
    capacity *= 2;
  }
  SEXP *slots = paradox_temporary_alloc(capacity, sizeof(*slots));
  for (R_xlen_t slot = 0; slot < capacity; ++slot) {
    slots[slot] = NULL;
  }
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP id = STRING_ELT(parameter_ids, index);
    R_xlen_t slot = shadow_id_slot(id, capacity);
    while (slots[slot] != NULL) {
      slot = (slot + 1) & (capacity - 1);
    }
    slots[slot] = id;
  }
  return (shadow_id_index_t) {parameter_ids, slots, capacity};
}

static int shadow_id_is_known(const shadow_id_index_t *index, SEXP sought,
    R_xlen_t *work_since_interrupt) {
  if (index->capacity == 0) {
    return FALSE;
  }
  R_xlen_t slot = shadow_id_slot(sought, index->capacity);
  while (index->slots[slot] != NULL) {
    paradox_domain_account_work(work_since_interrupt);
    if (index->slots[slot] == sought) {
      return TRUE;
    }
    slot = (slot + 1) & (index->capacity - 1);
  }
  /* Canonical package IDs are interned CHARSXP values, so the pointer index is
   * authoritative on the ordinary path. Preserve exact cross-encoding string
   * equality for safely forged-but-structural states without paying for UTF-8
   * translation or text hashing on every maintained Shadow read. */
  return string_in(index->parameter_ids, sought, work_since_interrupt);
}

static int related_ids_are_known(const shadow_id_index_t *parameter_ids,
    SEXP related_ids,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t count = XLENGTH(related_ids);
  for (R_xlen_t index = 0; index < count; ++index) {
    if (!shadow_id_is_known(
        parameter_ids,
        STRING_ELT(related_ids, index),
        work_since_interrupt
      )) {
      return FALSE;
    }
  }
  return TRUE;
}

static int exact_signature(SEXP signature) {
  if (TYPEOF(signature) != VECSXP || ALTREP(signature) ||
      Rf_isObject(signature) || !paradox_api_has_no_attributes(signature) ||
      XLENGTH(signature) < 2 || XLENGTH(signature) % 2 != 0) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(signature); index += 2) {
    if (TYPEOF(VECTOR_ELT(signature, index)) != ENVSXP ||
        !paradox_core_is_canonical(VECTOR_ELT(signature, index + 1))) {
      return FALSE;
    }
  }
  return TRUE;
}

static SEXP exact_metadata_signature(SEXP core) {
  if (!paradox_api_has_single_attribute(
      core,
      ".paradox.shadow.snapshot.v1"
    )) {
    return R_UnboundValue;
  }
  SEXP signature = Rf_getAttrib(core, shadow_metadata_symbol());
  return exact_signature(signature) ? signature : R_UnboundValue;
}

int paradox_shadow_metadata_is_exact(SEXP core) {
  return paradox_core_kind(core) == PARADOX_CORE_SHADOW &&
    exact_metadata_signature(core) != R_UnboundValue;
}

static SEXP fixed_factories(void) {
  static const char *const names[SHADOW_FACTORY_FIELD_COUNT] = {
    "param_set_shadow_constraint_factory",
    "param_set_collection_constraint_factory",
    "param_set_collection_extra_trafo_factory"
  };
  SEXP package = PROTECT(Rf_mkString("paradox"));
  SEXP namespace_environment = PROTECT(R_FindNamespace(package));
  if (TYPEOF(namespace_environment) != ENVSXP ||
      !R_IsNamespaceEnv(namespace_environment)) {
    UNPROTECT(2);
    Rf_error("Internal error: unable to resolve the paradox namespace");
  }
  SEXP factories = PROTECT(Rf_allocVector(
    VECSXP,
    SHADOW_FACTORY_FIELD_COUNT
  ));
  for (R_xlen_t index = 0;
      index < SHADOW_FACTORY_FIELD_COUNT;
      ++index) {
    SEXP value = PROTECT(Rf_eval(
      Rf_install(names[index]),
      namespace_environment
    ));
    if (TYPEOF(value) != CLOSXP) {
      UNPROTECT(4);
      Rf_error("Internal error: invalid ParamSetShadow callback factory");
    }
    SET_VECTOR_ELT(factories, index, value);
    UNPROTECT(1);
  }
  UNPROTECT(3);
  return factories;
}

static int validate_related_state(
    const paradox_domain_params_t *params,
    const paradox_domain_tags_t *tags,
    const paradox_domain_dependencies_t *dependencies,
    const paradox_domain_trafos_t *trafos,
    const paradox_domain_values_t *values,
    R_xlen_t *work_since_interrupt) {
  const shadow_id_index_t parameter_ids = build_shadow_id_index(
    params->ids,
    work_since_interrupt
  );
  return related_ids_are_known(
      &parameter_ids,
      tags->ids,
      work_since_interrupt
    ) && related_ids_are_known(
      &parameter_ids,
      dependencies->ids,
      work_since_interrupt
    ) && related_ids_are_known(
      &parameter_ids,
      dependencies->on,
      work_since_interrupt
    ) && related_ids_are_known(
      &parameter_ids,
      trafos->ids,
      work_since_interrupt
    ) && related_ids_are_known(
      &parameter_ids,
      values->names,
      work_since_interrupt
    );
}

static SEXP validate_shadow_template(SEXP core, SEXP expected_origin,
    paradox_domain_params_t *params, R_xlen_t *work_since_interrupt) {
  if (paradox_core_kind(core) != PARADOX_CORE_SHADOW ||
      !paradox_core_has_exact_schema(core)) {
    Rf_error("Corrupt ParamSetShadow capsule schema");
  }
  SEXP state = paradox_core_payload(core);
  paradox_domain_tags_t tags;
  paradox_domain_dependencies_t dependencies;
  paradox_domain_trafos_t trafos;
  paradox_domain_values_t values;
  R_xlen_t unused_row = 0;
  if (!paradox_domain_validate_params(
      VECTOR_ELT(state, PARADOX_CORE_PARAMS),
      R_NilValue,
      TRUE,
      params,
      &unused_row,
      work_since_interrupt
    ) || !paradox_domain_validate_tags(
      VECTOR_ELT(state, PARADOX_CORE_TAGS),
      &tags,
      work_since_interrupt
    ) || !paradox_domain_validate_dependencies(
      VECTOR_ELT(state, PARADOX_CORE_DEPS),
      &dependencies,
      work_since_interrupt
    ) || !paradox_domain_validate_trafos(
      VECTOR_ELT(state, PARADOX_CORE_TRAFOS),
      &trafos,
      work_since_interrupt
    ) || !paradox_domain_validate_values(
      VECTOR_ELT(state, PARADOX_CORE_VALUES),
      &values,
      work_since_interrupt
    ) || !validate_related_state(
      params,
      &tags,
      &dependencies,
      &trafos,
      &values,
      work_since_interrupt
    )) {
    Rf_error("Corrupt ParamSetShadow dynamic capsule state");
  }

  SEXP sets = VECTOR_ELT(state, PARADOX_CORE_SETS);
  SEXP constraint = VECTOR_ELT(state, PARADOX_CORE_CONSTRAINT);
  SEXP extra_trafo = VECTOR_ELT(state, PARADOX_CORE_EXTRA_TRAFO);
  if (!ordinary_list(sets, 1) || VECTOR_ELT(sets, 0) != expected_origin ||
      TYPEOF(expected_origin) != ENVSXP ||
      VECTOR_ELT(state, PARADOX_CORE_TRANSLATION) != R_NilValue ||
      !exact_false(VECTOR_ELT(state, PARADOX_CORE_POSTFIX)) ||
      (constraint != R_NilValue && !Rf_isFunction(constraint)) ||
      (extra_trafo != R_NilValue && !Rf_isFunction(extra_trafo))) {
    Rf_error("Corrupt ParamSetShadow origin capsule");
  }
  return state;
}

static SEXP validate_base_origin(SEXP core,
    paradox_domain_params_t *params,
    paradox_domain_dependencies_t *dependencies,
    paradox_domain_trafos_t *trafos,
    paradox_domain_values_t *values,
    R_xlen_t *work_since_interrupt) {
  if (paradox_core_kind(core) != PARADOX_CORE_BASE ||
      !paradox_core_has_exact_schema(core)) {
    Rf_error("Corrupt ParamSetShadow BASE origin capsule");
  }
  SEXP state = paradox_core_payload(core);
  paradox_domain_tags_t tags;
  R_xlen_t unused_row = 0;
  if (!paradox_domain_validate_params(
      VECTOR_ELT(state, PARADOX_CORE_PARAMS),
      R_NilValue,
      TRUE,
      params,
      &unused_row,
      work_since_interrupt
    ) || !paradox_domain_validate_tags(
      VECTOR_ELT(state, PARADOX_CORE_TAGS),
      &tags,
      work_since_interrupt
    ) || !paradox_domain_validate_dependencies(
      VECTOR_ELT(state, PARADOX_CORE_DEPS),
      dependencies,
      work_since_interrupt
    ) || !paradox_domain_validate_trafos(
      VECTOR_ELT(state, PARADOX_CORE_TRAFOS),
      trafos,
      work_since_interrupt
    ) || !paradox_domain_validate_values(
      VECTOR_ELT(state, PARADOX_CORE_VALUES),
      values,
      work_since_interrupt
    ) || !validate_related_state(
      params,
      &tags,
      dependencies,
      trafos,
      values,
      work_since_interrupt
    ) || VECTOR_ELT(state, PARADOX_CORE_SETS) != R_NilValue ||
      VECTOR_ELT(state, PARADOX_CORE_TRANSLATION) != R_NilValue ||
      !exact_false(VECTOR_ELT(state, PARADOX_CORE_POSTFIX))) {
    Rf_error("Corrupt ParamSetShadow BASE origin state");
  }
  SEXP constraint = VECTOR_ELT(state, PARADOX_CORE_CONSTRAINT);
  SEXP extra_trafo = VECTOR_ELT(state, PARADOX_CORE_EXTRA_TRAFO);
  if ((constraint != R_NilValue && !Rf_isFunction(constraint)) ||
      (extra_trafo != R_NilValue && !Rf_isFunction(extra_trafo))) {
    Rf_error("Corrupt ParamSetShadow BASE origin callbacks");
  }
  return state;
}

SEXP paradox_shadow_origin_from_core(SEXP core) {
  if (paradox_core_kind(core) != PARADOX_CORE_SHADOW ||
      !paradox_core_has_exact_schema(core)) {
    return R_UnboundValue;
  }
  SEXP state = paradox_core_payload(core);
  SEXP sets = VECTOR_ELT(state, PARADOX_CORE_SETS);
  return ordinary_list(sets, 1) && TYPEOF(VECTOR_ELT(sets, 0)) == ENVSXP
    ? VECTOR_ELT(sets, 0)
    : R_UnboundValue;
}

static SEXP base_signature(SEXP origin, SEXP core) {
  SEXP signature = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(signature, 0, origin);
  SET_VECTOR_ELT(signature, 1, core);
  UNPROTECT(1);
  return signature;
}

static SEXP graph_signature(const paradox_collection_graph_t *graph) {
  if (graph->count > R_XLEN_T_MAX / 2) {
    Rf_error("ParamSetShadow origin graph is too large");
  }
  SEXP signature = PROTECT(Rf_allocVector(VECSXP, graph->count * 2));
  for (R_xlen_t index = 0; index < graph->count; ++index) {
    SET_VECTOR_ELT(signature, index * 2, graph->nodes[index].self);
    SET_VECTOR_ELT(signature, index * 2 + 1, graph->nodes[index].core);
  }
  UNPROTECT(1);
  return signature;
}

static int signature_matches_base(SEXP signature, SEXP origin, SEXP core) {
  return XLENGTH(signature) == 2 && VECTOR_ELT(signature, 0) == origin &&
    VECTOR_ELT(signature, 1) == core;
}

static int signature_matches_graph(SEXP signature,
    const paradox_collection_graph_t *graph) {
  if (graph->count > R_XLEN_T_MAX / 2 ||
      XLENGTH(signature) != graph->count * 2) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < graph->count; ++index) {
    if (VECTOR_ELT(signature, index * 2) != graph->nodes[index].self ||
        VECTOR_ELT(signature, index * 2 + 1) != graph->nodes[index].core) {
      return FALSE;
    }
  }
  return TRUE;
}

static int graph_is_current(const paradox_collection_graph_t *graph) {
  for (R_xlen_t index = 0; index < graph->count; ++index) {
    const paradox_collection_graph_node_t *node = &graph->nodes[index];
    SEXP private_environment = PROTECT(
      paradox_domain_private_environment(node->self)
    );
    const int current = private_environment == node->private_environment &&
      paradox_core_from_private(private_environment) == node->core;
    UNPROTECT(1);
    if (!current) {
      return FALSE;
    }
  }
  return TRUE;
}

static SEXP split_values(const paradox_domain_values_t *source,
    SEXP visible_ids, R_xlen_t *work_since_interrupt) {
  R_xlen_t visible_count = 0;
  for (R_xlen_t visible = 0; visible < XLENGTH(visible_ids); ++visible) {
    visible_count += string_in(
      source->names,
      STRING_ELT(visible_ids, visible),
      work_since_interrupt
    );
  }
  R_xlen_t hidden_count = 0;
  for (R_xlen_t index = 0; index < source->size; ++index) {
    hidden_count += !string_in(
      visible_ids,
      STRING_ELT(source->names, index),
      work_since_interrupt
    );
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP visible_values = PROTECT(Rf_allocVector(VECSXP, visible_count));
  SEXP visible_names = PROTECT(Rf_allocVector(STRSXP, visible_count));
  SEXP hidden_values = PROTECT(Rf_allocVector(VECSXP, hidden_count));
  SEXP hidden_names = PROTECT(Rf_allocVector(STRSXP, hidden_count));
  Rf_setAttrib(visible_values, R_NamesSymbol, visible_names);
  Rf_setAttrib(hidden_values, R_NamesSymbol, hidden_names);
  SET_VECTOR_ELT(result, 0, visible_values);
  SET_VECTOR_ELT(result, 1, hidden_values);

  R_xlen_t visible_output = 0;
  for (R_xlen_t visible = 0; visible < XLENGTH(visible_ids); ++visible) {
    SEXP id = STRING_ELT(visible_ids, visible);
    for (R_xlen_t index = 0; index < source->size; ++index) {
      paradox_domain_account_work(work_since_interrupt);
      SEXP candidate = STRING_ELT(source->names, index);
      if (candidate == id || paradox_domain_strings_equal(candidate, id)) {
        SET_VECTOR_ELT(
          visible_values,
          visible_output,
          VECTOR_ELT(source->values, index)
        );
        SET_STRING_ELT(visible_names, visible_output, id);
        ++visible_output;
        break;
      }
    }
  }
  R_xlen_t hidden_output = 0;
  for (R_xlen_t index = 0; index < source->size; ++index) {
    SEXP id = STRING_ELT(source->names, index);
    if (!string_in(visible_ids, id, work_since_interrupt)) {
      SET_VECTOR_ELT(hidden_values, hidden_output, VECTOR_ELT(
        source->values,
        index
      ));
      SET_STRING_ELT(hidden_names, hidden_output, id);
      ++hidden_output;
    }
  }
  if (visible_output != visible_count || hidden_output != hidden_count) {
    UNPROTECT(5);
    Rf_error("Internal error: incomplete ParamSetShadow value snapshot");
  }
  UNPROTECT(5);
  return result;
}

static void set_plain_table_attributes(SEXP table,
    const char *const *column_names, R_xlen_t column_count,
    R_xlen_t row_count) {
  if (row_count > INT_MAX) {
    Rf_error("ParamSetShadow table exceeds data.frame limits");
  }
  SEXP names = PROTECT(Rf_allocVector(STRSXP, column_count));
  for (R_xlen_t column = 0; column < column_count; ++column) {
    SET_STRING_ELT(names, column, Rf_mkChar(column_names[column]));
  }
  Rf_setAttrib(table, R_NamesSymbol, names);
  SEXP classes = PROTECT(Rf_mkString("data.frame"));
  Rf_setAttrib(table, R_ClassSymbol, classes);
  SEXP row_names = PROTECT(Rf_allocVector(
    INTSXP,
    row_count == 0 ? 0 : 2
  ));
  if (row_count != 0) {
    SET_INTEGER_ELT(row_names, 0, NA_INTEGER);
    SET_INTEGER_ELT(row_names, 1, -(int) row_count);
  }
  Rf_setAttrib(table, R_RowNamesSymbol, row_names);
  UNPROTECT(3);
}

static SEXP filter_dependencies(
    const paradox_domain_dependencies_t *source,
    SEXP visible_ids, R_xlen_t *work_since_interrupt) {
  R_xlen_t count = 0;
  for (R_xlen_t row = 0; row < source->row_count; ++row) {
    const int id_visible = string_in(
      visible_ids,
      STRING_ELT(source->ids, row),
      work_since_interrupt
    );
    const int on_visible = string_in(
      visible_ids,
      STRING_ELT(source->on, row),
      work_since_interrupt
    );
    if (id_visible != on_visible) {
      Rf_error(
        "Params %s have dependencies that reach across shadow bounds",
        CHAR(STRING_ELT(source->ids, row))
      );
    }
    count += id_visible && on_visible;
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP ids = PROTECT(Rf_allocVector(STRSXP, count));
  SEXP on = PROTECT(Rf_allocVector(STRSXP, count));
  SEXP conditions = PROTECT(Rf_allocVector(VECSXP, count));
  SET_VECTOR_ELT(result, 0, ids);
  SET_VECTOR_ELT(result, 1, on);
  SET_VECTOR_ELT(result, 2, conditions);
  R_xlen_t output = 0;
  for (R_xlen_t row = 0; row < source->row_count; ++row) {
    if (!string_in(
        visible_ids,
        STRING_ELT(source->ids, row),
        work_since_interrupt
      )) {
      continue;
    }
    SET_STRING_ELT(ids, output, STRING_ELT(source->ids, row));
    SET_STRING_ELT(on, output, STRING_ELT(source->on, row));
    SEXP condition = PROTECT(Rf_duplicate(
      VECTOR_ELT(source->conditions, row)
    ));
    SET_VECTOR_ELT(conditions, output, condition);
    UNPROTECT(1);
    ++output;
  }
  if (output != count) {
    UNPROTECT(4);
    Rf_error("Internal error: incomplete ParamSetShadow dependencies");
  }
  static const char *const names[] = {"id", "on", "cond"};
  set_plain_table_attributes(result, names, 3, count);
  UNPROTECT(4);
  return result;
}

static SEXP filter_trafos(const paradox_domain_trafos_t *source,
    SEXP visible_ids, R_xlen_t *work_since_interrupt) {
  R_xlen_t count = 0;
  for (R_xlen_t row = 0; row < source->row_count; ++row) {
    count += string_in(
      visible_ids,
      STRING_ELT(source->ids, row),
      work_since_interrupt
    );
  }
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP ids = PROTECT(Rf_allocVector(STRSXP, count));
  SEXP callbacks = PROTECT(Rf_allocVector(VECSXP, count));
  SET_VECTOR_ELT(result, 0, ids);
  SET_VECTOR_ELT(result, 1, callbacks);
  R_xlen_t output = 0;
  for (R_xlen_t row = 0; row < source->row_count; ++row) {
    if (!string_in(
        visible_ids,
        STRING_ELT(source->ids, row),
        work_since_interrupt
      )) {
      continue;
    }
    SET_STRING_ELT(ids, output, STRING_ELT(source->ids, row));
    SET_VECTOR_ELT(callbacks, output, VECTOR_ELT(source->values, row));
    ++output;
  }
  if (output != count) {
    UNPROTECT(3);
    Rf_error("Internal error: incomplete ParamSetShadow transformations");
  }
  static const char *const names[] = {"id", "trafo"};
  set_plain_table_attributes(result, names, 2, count);
  UNPROTECT(3);
  return result;
}

static SEXP filter_tags(const paradox_domain_tags_t *source,
    SEXP visible_ids, R_xlen_t *work_since_interrupt) {
  R_xlen_t count = 0;
  for (R_xlen_t row = 0; row < source->row_count; ++row) {
    count += string_in(
      visible_ids,
      STRING_ELT(source->ids, row),
      work_since_interrupt
    );
  }
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP ids = PROTECT(Rf_allocVector(STRSXP, count));
  SEXP tags = PROTECT(Rf_allocVector(STRSXP, count));
  SET_VECTOR_ELT(result, 0, ids);
  SET_VECTOR_ELT(result, 1, tags);
  R_xlen_t output = 0;
  for (R_xlen_t row = 0; row < source->row_count; ++row) {
    if (!string_in(
        visible_ids,
        STRING_ELT(source->ids, row),
        work_since_interrupt
      )) {
      continue;
    }
    SET_STRING_ELT(ids, output, STRING_ELT(source->ids, row));
    SET_STRING_ELT(tags, output, STRING_ELT(source->values, row));
    ++output;
  }
  if (output != count) {
    UNPROTECT(3);
    Rf_error("Internal error: incomplete ParamSetShadow tags");
  }
  static const char *const names[] = {"id", "tag"};
  set_plain_table_attributes(result, names, 2, count);
  UNPROTECT(3);
  return result;
}

static void copy_param_element(SEXP target, R_xlen_t target_row,
    SEXP source, R_xlen_t source_row) {
  switch (TYPEOF(source)) {
  case LGLSXP:
    SET_LOGICAL_ELT(target, target_row, LOGICAL_ELT(source, source_row));
    break;
  case INTSXP:
    SET_INTEGER_ELT(target, target_row, INTEGER_ELT(source, source_row));
    break;
  case REALSXP:
    SET_REAL_ELT(target, target_row, REAL_ELT(source, source_row));
    break;
  case STRSXP:
    SET_STRING_ELT(target, target_row, STRING_ELT(source, source_row));
    break;
  case VECSXP:
    SET_VECTOR_ELT(target, target_row, VECTOR_ELT(source, source_row));
    break;
  default:
    Rf_error("Corrupt ParamSetShadow parameter column type");
  }
}

static SEXP visible_parameter_table(const paradox_domain_params_t *source,
    SEXP shadowed, R_xlen_t *work_since_interrupt) {
  if (TYPEOF(shadowed) != STRSXP || Rf_any_duplicated(shadowed, FALSE) != 0) {
    Rf_error("`shadowed` must be a unique character vector");
  }
  for (R_xlen_t index = 0; index < XLENGTH(shadowed); ++index) {
    SEXP id = STRING_ELT(shadowed, index);
    if (id == NA_STRING || Rf_getCharCE(id) == CE_BYTES ||
        !string_in(source->ids, id, work_since_interrupt)) {
      Rf_error("`shadowed` contains an unknown or unsupported parameter ID");
    }
  }
  R_xlen_t visible_count = 0;
  for (R_xlen_t row = 0; row < source->row_count; ++row) {
    visible_count += !string_in(
      shadowed,
      STRING_ELT(source->ids, row),
      work_since_interrupt
    );
  }
  SEXP result = PROTECT(Rf_allocVector(VECSXP, PARADOX_DOMAIN_TAGS));
  for (R_xlen_t column = 0; column < PARADOX_DOMAIN_TAGS; ++column) {
    SEXP source_column = VECTOR_ELT(source->table, column);
    SEXP target_column = PROTECT(Rf_allocVector(
      (SEXPTYPE) TYPEOF(source_column),
      visible_count
    ));
    SET_VECTOR_ELT(result, column, target_column);
    UNPROTECT(1);
  }
  R_xlen_t output = 0;
  for (R_xlen_t row = 0; row < source->row_count; ++row) {
    if (string_in(
        shadowed,
        STRING_ELT(source->ids, row),
        work_since_interrupt
      )) {
      continue;
    }
    for (R_xlen_t column = 0; column < PARADOX_DOMAIN_TAGS; ++column) {
      copy_param_element(
        VECTOR_ELT(result, column),
        output,
        VECTOR_ELT(source->table, column),
        row
      );
    }
    ++output;
  }
  if (output != visible_count) {
    UNPROTECT(1);
    Rf_error("Internal error: incomplete ParamSetShadow schema");
  }
  static const char *const names[PARADOX_DOMAIN_TAGS] = {
    "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
    "levels", "special_vals", "default", "storage_type"
  };
  set_plain_table_attributes(
    result,
    names,
    PARADOX_DOMAIN_TAGS,
    visible_count
  );
  UNPROTECT(1);
  return result;
}

static SEXP build_shadow_template_state(SEXP origin, SEXP shadowed,
    const paradox_domain_params_t *params,
    const paradox_domain_tags_t *tags,
    R_xlen_t *work_since_interrupt) {
  SEXP visible_params = PROTECT(visible_parameter_table(
    params,
    shadowed,
    work_since_interrupt
  ));
  SEXP visible_ids = VECTOR_ELT(visible_params, PARADOX_DOMAIN_ID);
  SEXP visible_tags = PROTECT(filter_tags(
    tags,
    visible_ids,
    work_since_interrupt
  ));
  SEXP sets = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(sets, 0, origin);
  SEXP postfix = PROTECT(Rf_ScalarLogical(FALSE));
  /* This short-lived construction plan is not a capsule and therefore needs
   * neither canonical field names nor placeholder tables. The final assembly
   * below creates the one exact payload and capsule exposed by the object. */
  SEXP result = PROTECT(Rf_allocVector(VECSXP, PARADOX_CORE_FIELD_COUNT));
  SET_VECTOR_ELT(result, PARADOX_CORE_PARAMS, visible_params);
  SET_VECTOR_ELT(result, PARADOX_CORE_TAGS, visible_tags);
  SET_VECTOR_ELT(result, PARADOX_CORE_SETS, sets);
  SET_VECTOR_ELT(result, PARADOX_CORE_POSTFIX, postfix);
  UNPROTECT(5);
  return result;
}

static SEXP evaluate_factory2(SEXP factory, SEXP first, SEXP second) {
  SEXP call = PROTECT(Rf_lang3(factory, first, second));
  SEXP result = PROTECT(Rf_eval(call, R_BaseEnv));
  UNPROTECT(2);
  return result;
}

static SEXP evaluate_factory3(SEXP factory, SEXP first, SEXP second,
    SEXP third) {
  SEXP call = PROTECT(Rf_lang4(factory, first, second, third));
  SEXP result = PROTECT(Rf_eval(call, R_BaseEnv));
  UNPROTECT(2);
  return result;
}

static SEXP assemble_shadow_core(SEXP template_state, SEXP factories,
    SEXP signature,
    const paradox_domain_values_t *source_values,
    const paradox_domain_dependencies_t *source_dependencies,
    const paradox_domain_trafos_t *source_trafos,
    SEXP source_constraint, SEXP source_extra_trafo,
    R_xlen_t *work_since_interrupt) {
  SEXP visible_ids = VECTOR_ELT(
    VECTOR_ELT(template_state, PARADOX_CORE_PARAMS),
    PARADOX_DOMAIN_ID
  );
  SEXP split = PROTECT(split_values(
    source_values,
    visible_ids,
    work_since_interrupt
  ));
  SEXP visible_values = VECTOR_ELT(split, 0);
  SEXP hidden_values = VECTOR_ELT(split, 1);
  SEXP dependencies = PROTECT(filter_dependencies(
    source_dependencies,
    visible_ids,
    work_since_interrupt
  ));
  SEXP trafos = PROTECT(filter_trafos(
    source_trafos,
    visible_ids,
    work_since_interrupt
  ));
  SEXP constraint = R_NilValue;
  if (source_constraint != R_NilValue) {
    constraint = evaluate_factory2(
      VECTOR_ELT(factories, SHADOW_FACTORY_CONSTRAINT),
      source_constraint,
      hidden_values
    );
    if (!Rf_isFunction(constraint)) {
      UNPROTECT(3);
      Rf_error("ParamSetShadow constraint factory returned no function");
    }
  }
  PROTECT(constraint);
  if (source_extra_trafo != R_NilValue &&
      !Rf_isFunction(source_extra_trafo)) {
    UNPROTECT(4);
    Rf_error("ParamSetShadow origin extra_trafo is not a function");
  }
  PROTECT(source_extra_trafo);

  const SEXP fields[PARADOX_CORE_FIELD_COUNT] = {
    VECTOR_ELT(template_state, PARADOX_CORE_PARAMS),
    visible_values,
    VECTOR_ELT(template_state, PARADOX_CORE_TAGS),
    dependencies,
    trafos,
    source_extra_trafo,
    constraint,
    VECTOR_ELT(template_state, PARADOX_CORE_SETS),
    R_NilValue,
    VECTOR_ELT(template_state, PARADOX_CORE_POSTFIX)
  };
  SEXP result = PROTECT(paradox_core_new_from_fields(
    PARADOX_CORE_SHADOW,
    fields
  ));
  Rf_setAttrib(result, shadow_metadata_symbol(), signature);
  UNPROTECT(6);
  return result;
}

static SEXP build_from_validated_base(SEXP template_state, SEXP state,
    const paradox_domain_dependencies_t *dependencies,
    const paradox_domain_trafos_t *trafos,
    const paradox_domain_values_t *values,
    SEXP factories, SEXP signature, R_xlen_t *work_since_interrupt) {
  return assemble_shadow_core(
    template_state,
    factories,
    signature,
    values,
    dependencies,
    trafos,
    VECTOR_ELT(state, PARADOX_CORE_CONSTRAINT),
    VECTOR_ELT(state, PARADOX_CORE_EXTRA_TRAFO),
    work_since_interrupt
  );
}

static SEXP build_from_base(SEXP template_state, SEXP origin_core,
    SEXP factories, SEXP signature,
    R_xlen_t *work_since_interrupt) {
  paradox_domain_params_t params;
  paradox_domain_dependencies_t dependencies;
  paradox_domain_trafos_t trafos;
  paradox_domain_values_t values;
  SEXP state = validate_base_origin(
    origin_core,
    &params,
    &dependencies,
    &trafos,
    &values,
    work_since_interrupt
  );
  (void) params;
  return build_from_validated_base(
    template_state,
    state,
    &dependencies,
    &trafos,
    &values,
    factories,
    signature,
    work_since_interrupt
  );
}

static SEXP collection_constraint_from_plan(SEXP plan, SEXP factories) {
  if (TYPEOF(plan) != VECSXP || ALTREP(plan) ||
      XLENGTH(plan) != COLLECTION_DETACH_FIELD_COUNT) {
    Rf_error("Corrupt ParamSetCollection callback detachment plan");
  }
  SEXP result = evaluate_factory3(
    VECTOR_ELT(factories, SHADOW_FACTORY_COLLECTION_CONSTRAINT),
    VECTOR_ELT(plan, COLLECTION_DETACH_TRANSLATION),
    VECTOR_ELT(plan, COLLECTION_DETACH_CONSTRAINT_INDICES),
    VECTOR_ELT(plan, COLLECTION_DETACH_CONSTRAINT_SETS)
  );
  if (result != R_NilValue && !Rf_isFunction(result)) {
    Rf_error("ParamSetCollection constraint factory returned invalid state");
  }
  return result;
}

static SEXP collection_extra_trafo_from_plan(SEXP plan, SEXP factories) {
  SEXP result = evaluate_factory3(
    VECTOR_ELT(factories, SHADOW_FACTORY_COLLECTION_EXTRA_TRAFO),
    VECTOR_ELT(plan, COLLECTION_DETACH_TRANSLATION),
    VECTOR_ELT(plan, COLLECTION_DETACH_TRAFO_INDICES),
    VECTOR_ELT(plan, COLLECTION_DETACH_TRAFO_SETS)
  );
  if (result != R_NilValue && !Rf_isFunction(result)) {
    Rf_error("ParamSetCollection extra_trafo factory returned invalid state");
  }
  return result;
}

static SEXP build_from_collection(SEXP template_state, SEXP origin,
    SEXP origin_private, SEXP factories, SEXP signature,
    const paradox_collection_graph_t *graph,
    R_xlen_t *work_since_interrupt) {
  const paradox_collection_graph_node_t *root = &graph->nodes[0];
  if (root->kind != PARADOX_CORE_COLLECTION || root->self != origin ||
      root->private_environment != origin_private ||
      VECTOR_ELT(root->state, PARADOX_CORE_CONSTRAINT) != R_NilValue ||
      VECTOR_ELT(root->state, PARADOX_CORE_EXTRA_TRAFO) != R_NilValue) {
    Rf_error("Corrupt ParamSetShadow COLLECTION origin snapshot");
  }
  SEXP values_object = PROTECT(paradox_collection_values_from_graph(
    graph,
    work_since_interrupt
  ));
  SEXP dependencies_object = PROTECT(
    paradox_collection_dependencies_from_graph(
      graph,
      work_since_interrupt
    )
  );
  paradox_domain_values_t values;
  paradox_domain_dependencies_t dependencies;
  paradox_domain_trafos_t trafos;
  if (!paradox_domain_validate_values(
      values_object,
      &values,
      work_since_interrupt
    ) || !paradox_domain_validate_dependencies(
      dependencies_object,
      &dependencies,
      work_since_interrupt
    ) || !paradox_domain_validate_trafos(
      VECTOR_ELT(root->state, PARADOX_CORE_TRAFOS),
      &trafos,
      work_since_interrupt
    )) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSetShadow COLLECTION dynamic snapshot");
  }

  SEXP plan = PROTECT(paradox_param_set_collection_detach_plan(
    origin_private,
    origin,
    R_NilValue
  ));
  if (!graph_is_current(graph)) {
    UNPROTECT(3);
    Rf_error("ParamSetShadow origin graph changed during refresh");
  }
  SEXP constraint = PROTECT(collection_constraint_from_plan(plan, factories));
  SEXP extra_trafo = PROTECT(collection_extra_trafo_from_plan(
    plan,
    factories
  ));
  SEXP result = PROTECT(assemble_shadow_core(
    template_state,
    factories,
    signature,
    &values,
    &dependencies,
    &trafos,
    constraint,
    extra_trafo,
    work_since_interrupt
  ));
  UNPROTECT(6);
  return result;
}

static SEXP origin_private_and_core(SEXP origin, SEXP *private_environment) {
  SEXP private_result = PROTECT(paradox_domain_private_environment(origin));
  if (private_result == R_UnboundValue) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSetShadow origin shell");
  }
  SEXP core = PROTECT(paradox_core_from_private(private_result));
  if (!paradox_core_has_exact_schema(core)) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSetShadow origin capsule");
  }
  *private_environment = private_result;
  UNPROTECT(2);
  return core;
}

SEXP paradox_param_set_shadow_construct(SEXP origin, SEXP shadowed) {
  PROTECT(origin);
  PROTECT(shadowed);
  SEXP shadowed_snapshot = PROTECT(paradox_snapshot_semantic_vector(
    shadowed
  ));
  if (TYPEOF(shadowed_snapshot) != STRSXP) {
    UNPROTECT(3);
    Rf_error("`shadowed` must be a character vector");
  }
  paradox_core_validate_graph_path(origin);

  SEXP origin_private = R_NilValue;
  SEXP origin_core = PROTECT(origin_private_and_core(
    origin,
    &origin_private
  ));
  PROTECT(origin_private);
  SEXP factories = PROTECT(fixed_factories());
  R_xlen_t work_since_interrupt = 0;
  const paradox_core_kind_t kind = paradox_core_kind(origin_core);
  SEXP result;
  if (kind == PARADOX_CORE_BASE) {
    paradox_domain_params_t params;
    paradox_domain_dependencies_t dependencies;
    paradox_domain_trafos_t trafos;
    paradox_domain_values_t values;
    SEXP state = validate_base_origin(
      origin_core,
      &params,
      &dependencies,
      &trafos,
      &values,
      &work_since_interrupt
    );
    (void) dependencies;
    (void) values;
    paradox_domain_tags_t tags;
    if (!paradox_domain_validate_tags(
        VECTOR_ELT(state, PARADOX_CORE_TAGS),
        &tags,
        &work_since_interrupt
      )) {
      UNPROTECT(6);
      Rf_error("Corrupt ParamSetShadow BASE origin tags");
    }
    SEXP template_state = PROTECT(build_shadow_template_state(
      origin,
      shadowed_snapshot,
      &params,
      &tags,
      &work_since_interrupt
    ));
    SEXP signature = PROTECT(base_signature(origin, origin_core));
    result = PROTECT(build_from_validated_base(
      template_state,
      state,
      &dependencies,
      &trafos,
      &values,
      factories,
      signature,
      &work_since_interrupt
    ));
    UNPROTECT(3);
  } else if (kind == PARADOX_CORE_COLLECTION) {
    PROTECT_INDEX roots_index;
    SEXP roots;
    PROTECT_WITH_INDEX(roots = R_NilValue, &roots_index);
    paradox_collection_graph_t graph;
    paradox_collection_graph_build(
      origin_private,
      origin,
      &graph,
      &roots,
      roots_index,
      &work_since_interrupt
    );
    const paradox_collection_graph_node_t *root = &graph.nodes[0];
    paradox_domain_tags_t tags;
    if (!paradox_domain_validate_tags(
        VECTOR_ELT(root->state, PARADOX_CORE_TAGS),
        &tags,
        &work_since_interrupt
      )) {
      UNPROTECT(7);
      Rf_error("Corrupt ParamSetShadow COLLECTION origin schema");
    }
    SEXP template_state = PROTECT(build_shadow_template_state(
      origin,
      shadowed_snapshot,
      &root->params,
      &tags,
      &work_since_interrupt
    ));
    SEXP signature = PROTECT(graph_signature(&graph));
    result = PROTECT(build_from_collection(
      template_state,
      origin,
      origin_private,
      factories,
      signature,
      &graph,
      &work_since_interrupt
    ));
    UNPROTECT(4);
  } else if (kind == PARADOX_CORE_SHADOW) {
    UNPROTECT(6);
    Rf_error("A ParamSetShadow cannot directly wrap another ParamSetShadow");
  } else {
    UNPROTECT(6);
    Rf_error("ParamSetShadow origin must be a BASE or COLLECTION node");
  }
  UNPROTECT(6);
  return result;
}

SEXP paradox_param_set_shadow_core_new(SEXP template_core, SEXP origin) {
  PROTECT(template_core);
  PROTECT(origin);
  R_xlen_t work_since_interrupt = 0;
  paradox_domain_params_t template_params;
  SEXP template_state = validate_shadow_template(
    template_core,
    origin,
    &template_params,
    &work_since_interrupt
  );
  (void) template_params;
  paradox_core_validate_graph_path(origin);

  SEXP origin_private = R_NilValue;
  SEXP origin_core = PROTECT(origin_private_and_core(
    origin,
    &origin_private
  ));
  PROTECT(origin_private);
  SEXP factories = PROTECT(fixed_factories());
  const paradox_core_kind_t kind = paradox_core_kind(origin_core);
  SEXP result;
  if (kind == PARADOX_CORE_BASE) {
    SEXP signature = PROTECT(base_signature(origin, origin_core));
    result = PROTECT(build_from_base(
      template_state,
      origin_core,
      factories,
      signature,
      &work_since_interrupt
    ));
    UNPROTECT(2);
  } else if (kind == PARADOX_CORE_COLLECTION) {
    PROTECT_INDEX roots_index;
    SEXP roots;
    PROTECT_WITH_INDEX(roots = R_NilValue, &roots_index);
    paradox_collection_graph_t graph;
    paradox_collection_graph_build(
      origin_private,
      origin,
      &graph,
      &roots,
      roots_index,
      &work_since_interrupt
    );
    SEXP signature = PROTECT(graph_signature(&graph));
    result = PROTECT(build_from_collection(
      template_state,
      origin,
      origin_private,
      factories,
      signature,
      &graph,
      &work_since_interrupt
    ));
    UNPROTECT(3);
  } else if (kind == PARADOX_CORE_SHADOW) {
    UNPROTECT(5);
    Rf_error("A ParamSetShadow cannot directly wrap another ParamSetShadow");
  } else {
    UNPROTECT(5);
    Rf_error("ParamSetShadow origin must be a BASE or COLLECTION node");
  }
  UNPROTECT(5);
  return result;
}

SEXP paradox_shadow_refresh_authoritative(SEXP self,
    SEXP private_environment) {
  PROTECT(self);
  PROTECT(private_environment);
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSetShadow shell ownership");
  }
  SEXP current_core = PROTECT(paradox_core_from_private(private_environment));
  if (current_core == R_UnboundValue) {
    UNPROTECT(3);
    Rf_error("Corrupt ParamSetShadow state: missing core capsule");
  }
  if (paradox_core_kind(current_core) != PARADOX_CORE_SHADOW) {
    UNPROTECT(3);
    return current_core;
  }

  SEXP origin = PROTECT(paradox_shadow_origin_from_core(current_core));
  if (origin == R_UnboundValue) {
    UNPROTECT(4);
    Rf_error("Corrupt ParamSetShadow origin edge");
  }
  R_xlen_t work_since_interrupt = 0;
  paradox_domain_params_t template_params;
  SEXP template_state = validate_shadow_template(
    current_core,
    origin,
    &template_params,
    &work_since_interrupt
  );
  (void) template_params;
  SEXP signature = PROTECT(exact_metadata_signature(current_core));
  if (signature == R_UnboundValue) {
    UNPROTECT(5);
    Rf_error("Corrupt ParamSetShadow native snapshot metadata");
  }

  SEXP origin_private = R_NilValue;
  SEXP origin_core = PROTECT(origin_private_and_core(
    origin,
    &origin_private
  ));
  PROTECT(origin_private);
  const paradox_core_kind_t kind = paradox_core_kind(origin_core);
  SEXP replacement;
  if (kind == PARADOX_CORE_BASE) {
    if (signature_matches_base(signature, origin, origin_core)) {
      UNPROTECT(7);
      return current_core;
    }
    SEXP factories = PROTECT(fixed_factories());
    SEXP replacement_signature = PROTECT(base_signature(origin, origin_core));
    replacement = PROTECT(build_from_base(
      template_state,
      origin_core,
      factories,
      replacement_signature,
      &work_since_interrupt
    ));
  } else if (kind == PARADOX_CORE_COLLECTION) {
    paradox_core_validate_graph_path(self);
    PROTECT_INDEX roots_index;
    SEXP roots;
    PROTECT_WITH_INDEX(roots = R_NilValue, &roots_index);
    paradox_collection_graph_t graph;
    paradox_collection_graph_build(
      origin_private,
      origin,
      &graph,
      &roots,
      roots_index,
      &work_since_interrupt
    );
    if (signature_matches_graph(signature, &graph)) {
      UNPROTECT(8);
      return current_core;
    }
    SEXP factories = PROTECT(fixed_factories());
    SEXP replacement_signature = PROTECT(graph_signature(&graph));
    replacement = PROTECT(build_from_collection(
      template_state,
      origin,
      origin_private,
      factories,
      replacement_signature,
      &graph,
      &work_since_interrupt
    ));
  } else if (kind == PARADOX_CORE_SHADOW) {
    UNPROTECT(7);
    Rf_error("A ParamSetShadow cannot directly wrap another ParamSetShadow");
  } else {
    UNPROTECT(7);
    Rf_error("Corrupt ParamSetShadow origin node kind");
  }

  if (paradox_core_from_private(private_environment) != current_core) {
    UNPROTECT(kind == PARADOX_CORE_BASE ? 10 : 11);
    Rf_error("ParamSetShadow capsule changed during native refresh");
  }
  Rf_defineVar(Rf_install(".core"), replacement, private_environment);
  UNPROTECT(kind == PARADOX_CORE_BASE ? 10 : 11);
  return replacement;
}
