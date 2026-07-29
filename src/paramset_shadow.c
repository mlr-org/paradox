#include <limits.h>
#include <stddef.h>
#include <stdint.h>

#include "paramset_shadow.h"

#include "builtin_condition.h"
#include "core_state.h"
#include "paramset_activity.h"
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
  /* Same ordinary-names admission as every sibling gate: the merge loop below
   * indexes this vector element by element, so an ALTREP or S4 names object
   * could answer differently per observation and pair a value with another
   * parameter's name. */
  if (TYPEOF(observed_names) != STRSXP || ALTREP(observed_names) ||
      Rf_isS4(observed_names) || Rf_isObject(observed_names) ||
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
    paradox_account_work(&work_since_interrupt);
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
    paradox_account_work(work_since_interrupt);
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
    paradox_account_work(work_since_interrupt);
    if (index->slots[slot] == sought) {
      return TRUE;
    }
    slot = (slot + 1) & (index->capacity - 1);
  }
  /* Canonical package IDs are interned CHARSXP values, so the pointer index is
   * authoritative on the ordinary path. Preserve exact cross-encoding string
   * equality for safely forged-but-structural states without paying for UTF-8
   * translation or text hashing on every maintained Shadow read. */
  return paradox_domain_string_in(index->parameter_ids, sought, work_since_interrupt);
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

void paradox_shadow_copy_metadata(SEXP source, SEXP target) {
  SEXP signature = PROTECT(Rf_getAttrib(source, shadow_metadata_symbol()));
  if (signature != R_NilValue) {
    Rf_setAttrib(target, shadow_metadata_symbol(), signature);
  }
  UNPROTECT(1);
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
  /* Deliberately absent: a `dependencies->on` conjunct. A dangling dependency
   * is a first-class state of an ordinary ParamSet, so an origin carrying one
   * is not corrupt, and a Shadow that shows the row is not corrupt either.
   * Every reader resolves `on` with a found-check and treats a miss as
   * never-satisfiable. */
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
      paradox_core_from_private(private_environment) == node->source_core;
    UNPROTECT(1);
    if (!current) {
      return FALSE;
    }
  }
  return TRUE;
}

static int base_is_current(SEXP origin, SEXP origin_private,
    SEXP origin_core) {
  SEXP selected_private = PROTECT(
    paradox_domain_private_environment(origin)
  );
  const int current = selected_private == origin_private &&
    paradox_core_from_private(selected_private) == origin_core;
  UNPROTECT(1);
  return current;
}

static SEXP split_values(const paradox_domain_values_t *source,
    SEXP visible_ids, R_xlen_t *work_since_interrupt) {
  R_xlen_t visible_count = 0;
  for (R_xlen_t visible = 0; visible < XLENGTH(visible_ids); ++visible) {
    visible_count += paradox_domain_string_in(
      source->names,
      STRING_ELT(visible_ids, visible),
      work_since_interrupt
    );
  }
  R_xlen_t hidden_count = 0;
  for (R_xlen_t index = 0; index < source->size; ++index) {
    hidden_count += !paradox_domain_string_in(
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
      paradox_account_work(work_since_interrupt);
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
    if (!paradox_domain_string_in(visible_ids, id, work_since_interrupt)) {
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

/*
 * Which origin dependency rows a view shows, given the visible IDs and the
 * origin's complete current ID universe:
 *
 *   id visible, `on` visible          keep
 *   id visible, `on` hidden           error: the edge crosses the boundary
 *   id visible, `on` absent           keep -- a dangling row, shown verbatim
 *   id hidden,  `on` visible          error: the edge crosses the boundary
 *   id hidden,  `on` hidden           drop: the origin's own business
 *   id hidden,  `on` absent           drop: likewise
 *
 * Absent is tested as "not an origin ID", never as "not hidden": the origin
 * decides what exists, and a name that exists nowhere cannot cross anything.
 * The extra membership test therefore only runs for a row that already leaves
 * the visible schema. Keeping the dangling row is what makes the view "origin
 * minus hidden" even before the parent arrives; because hidden IDs are fixed
 * at construction and IDs are unique, a parent the origin gains later is
 * always visible, so such a row can only ever resolve inside the view.
 */
static SEXP filter_dependencies(
    const paradox_domain_dependencies_t *source,
    SEXP visible_ids, SEXP origin_ids, R_xlen_t *work_since_interrupt) {
  R_xlen_t count = 0;
  for (R_xlen_t row = 0; row < source->row_count; ++row) {
    const int id_visible = paradox_domain_string_in(
      visible_ids,
      STRING_ELT(source->ids, row),
      work_since_interrupt
    );
    const int on_visible = paradox_domain_string_in(
      visible_ids,
      STRING_ELT(source->on, row),
      work_since_interrupt
    );
    if (id_visible != on_visible && paradox_domain_string_in(
        origin_ids,
        STRING_ELT(source->on, row),
        work_since_interrupt
      )) {
      Rf_error(
        PARADOX_SHADOW_CROSSING_MESSAGE,
        id_visible ? "visible" : "hidden",
        CHAR(STRING_ELT(source->ids, row)),
        id_visible ? "hidden" : "visible",
        CHAR(STRING_ELT(source->on, row))
      );
    }
    count += id_visible;
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
    if (!paradox_domain_string_in(
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
  (void) paradox_domain_finish_plain_table(result, names, 3, count);
  UNPROTECT(4);
  return result;
}

static SEXP filter_trafos(const paradox_domain_trafos_t *source,
    SEXP visible_ids, R_xlen_t *work_since_interrupt) {
  R_xlen_t count = 0;
  for (R_xlen_t row = 0; row < source->row_count; ++row) {
    count += paradox_domain_string_in(
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
    if (!paradox_domain_string_in(
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
  (void) paradox_domain_finish_plain_table(result, names, 2, count);
  UNPROTECT(3);
  return result;
}

static SEXP filter_tags(const paradox_domain_tags_t *source,
    SEXP visible_ids, R_xlen_t *work_since_interrupt) {
  R_xlen_t count = 0;
  for (R_xlen_t row = 0; row < source->row_count; ++row) {
    count += paradox_domain_string_in(
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
    if (!paradox_domain_string_in(
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
  (void) paradox_domain_finish_plain_table(result, names, 2, count);
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
    SEXP shadowed, int strict, R_xlen_t *work_since_interrupt) {
  if (TYPEOF(shadowed) != STRSXP || Rf_any_duplicated(shadowed, FALSE) != 0) {
    Rf_error("`shadowed` must be a unique character vector");
  }
  for (R_xlen_t index = 0; index < XLENGTH(shadowed); ++index) {
    SEXP id = STRING_ELT(shadowed, index);
    if (id == NA_STRING || Rf_getCharCE(id) == CE_BYTES) {
      Rf_error("`shadowed` contains an unknown or unsupported parameter ID");
    }
    /* Construction requires every hidden ID to exist; a later refresh does
     * not, because an origin that lost a parameter must still yield a usable
     * view rather than an unreadable object. */
    if (strict &&
        !paradox_domain_string_in(source->ids, id, work_since_interrupt)) {
      Rf_error("`shadowed` contains an unknown or unsupported parameter ID");
    }
  }
  R_xlen_t visible_count = 0;
  for (R_xlen_t row = 0; row < source->row_count; ++row) {
    visible_count += !paradox_domain_string_in(
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
    if (paradox_domain_string_in(
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
  (void) paradox_domain_finish_plain_table(
    result,
    paradox_domain_column_names,
    PARADOX_DOMAIN_PERMANENT_COLUMNS,
    visible_count
  );
  UNPROTECT(1);
  return result;
}

/* The origin fields a Shadow's own visible schema is derived from. Recording
 * them is what turns "origin minus hidden" into a live view: the hidden set
 * cannot be recovered from the visible schema alone, because a parameter the
 * origin gained after construction is neither visible nor hidden by it. */
static SEXP shadow_edge_record(SEXP origin_params, SEXP origin_tags,
    SEXP origin_trafos, SEXP shadowed, SEXP tag_override) {
  SEXP edges = PROTECT(Rf_allocVector(
    VECSXP,
    PARADOX_SHADOW_EDGE_FIELD_COUNT
  ));
  SET_VECTOR_ELT(edges, PARADOX_SHADOW_EDGE_PARAMS, origin_params);
  SET_VECTOR_ELT(edges, PARADOX_SHADOW_EDGE_TAGS, origin_tags);
  SET_VECTOR_ELT(edges, PARADOX_SHADOW_EDGE_TRAFOS, origin_trafos);
  SET_VECTOR_ELT(edges, PARADOX_SHADOW_EDGE_SHADOWED, shadowed);
  SET_VECTOR_ELT(edges, PARADOX_SHADOW_EDGE_TAG_OVERRIDE, tag_override);
  SEXP names = PROTECT(paradox_domain_character_vector(
    (const char *const[]) {
      "params", "tags", "trafos", "shadowed", "tag_override"
    },
    PARADOX_SHADOW_EDGE_FIELD_COUNT
  ));
  Rf_setAttrib(edges, R_NamesSymbol, names);
  UNPROTECT(2);
  return edges;
}

static SEXP build_shadow_template_state(SEXP origin, SEXP shadowed,
    const paradox_domain_params_t *params,
    const paradox_domain_tags_t *tags,
    SEXP origin_params, SEXP origin_tags, SEXP origin_trafos,
    SEXP tag_override, int strict_shadowed,
    R_xlen_t *work_since_interrupt) {
  SEXP visible_params = PROTECT(visible_parameter_table(
    params,
    shadowed,
    strict_shadowed,
    work_since_interrupt
  ));
  SEXP visible_ids = VECTOR_ELT(visible_params, PARADOX_DOMAIN_ID);
  SEXP visible_tags = PROTECT(paradox_domain_apply_tag_override(
    PROTECT(filter_tags(tags, visible_ids, work_since_interrupt)),
    tag_override,
    work_since_interrupt
  ));
  SEXP sets = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(sets, 0, origin);
  SEXP postfix = PROTECT(Rf_ScalarLogical(FALSE));
  SEXP edges = PROTECT(shadow_edge_record(
    origin_params,
    origin_tags,
    origin_trafos,
    shadowed,
    tag_override
  ));
  /* This short-lived construction plan is not a capsule and therefore needs
   * neither canonical field names nor placeholder tables. The final assembly
   * below creates the one exact payload and capsule exposed by the object. */
  SEXP result = PROTECT(Rf_allocVector(VECSXP, PARADOX_CORE_FIELD_COUNT));
  SET_VECTOR_ELT(result, PARADOX_CORE_PARAMS, visible_params);
  SET_VECTOR_ELT(result, PARADOX_CORE_TAGS, visible_tags);
  SET_VECTOR_ELT(result, PARADOX_CORE_SETS, sets);
  SET_VECTOR_ELT(result, PARADOX_CORE_POSTFIX, postfix);
  SET_VECTOR_ELT(result, PARADOX_CORE_EDGES, edges);
  UNPROTECT(7);
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

/*
 * A Shadow constraint carrier intentionally remains the exact two-field
 * {callback, hidden_values} ABI. Dependencies may not cross the visible /
 * hidden partition, so filtering the admitted hidden origin snapshot here and
 * the candidate visible slice in the outer check kernel is exactly equivalent
 * to filtering their merged configuration. Refresh rebuilds this snapshot
 * from the live origin generation before every authoritative operation.
 */
static SEXP active_hidden_values(
    const paradox_domain_params_t *params,
    const paradox_domain_values_t *values,
    const paradox_domain_dependencies_t *dependencies,
    SEXP hidden_values,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t parameter_count = params->row_count;
  const R_xlen_t dependency_count = dependencies->row_count;
  if (dependency_count == 0) {
    return hidden_values;
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
  for (R_xlen_t value = 0; value < values->size; ++value) {
    const R_xlen_t parameter = paradox_domain_find_string(
      params->ids,
      STRING_ELT(values->names, value),
      work_since_interrupt
    );
    if (parameter == R_XLEN_T_MAX ||
        value_by_parameter[parameter] != R_XLEN_T_MAX) {
      Rf_error("Corrupt ParamSetShadow origin values");
    }
    value_by_parameter[parameter] = value;
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
  for (R_xlen_t dependency = 0;
      dependency < dependency_count;
      ++dependency) {
    dependency_child[dependency] = paradox_domain_find_string(
      params->ids,
      STRING_ELT(dependencies->ids, dependency),
      work_since_interrupt
    );
    dependency_parent[dependency] = paradox_domain_find_string(
      params->ids,
      STRING_ELT(dependencies->on, dependency),
      work_since_interrupt
    );
    paradox_builtin_condition_kind_t kind;
    SEXP rhs = R_NilValue;
    if (dependency_child[dependency] == R_XLEN_T_MAX ||
        !paradox_builtin_condition_exact(
          VECTOR_ELT(dependencies->conditions, dependency),
          &kind,
          &rhs,
          work_since_interrupt
        )) {
      Rf_error("Corrupt ParamSetShadow origin dependencies");
    }
    dependency_rhs[dependency] = rhs;
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
    VECTOR_ELT(params->table, PARADOX_DOMAIN_DEFAULT),
    values->values,
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

  SEXP hidden_names = Rf_getAttrib(hidden_values, R_NamesSymbol);
  R_xlen_t kept = 0;
  for (R_xlen_t index = 0; index < XLENGTH(hidden_values); ++index) {
    const R_xlen_t parameter = paradox_domain_find_string(
      params->ids,
      STRING_ELT(hidden_names, index),
      work_since_interrupt
    );
    if (parameter == R_XLEN_T_MAX) {
      Rf_error("Corrupt ParamSetShadow hidden value");
    }
    kept += activity.active[parameter] != FALSE;
  }
  SEXP result = PROTECT(Rf_allocVector(VECSXP, kept));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, kept));
  R_xlen_t output = 0;
  for (R_xlen_t index = 0; index < XLENGTH(hidden_values); ++index) {
    const R_xlen_t parameter = paradox_domain_find_string(
      params->ids,
      STRING_ELT(hidden_names, index),
      work_since_interrupt
    );
    if (!activity.active[parameter]) continue;
    SET_VECTOR_ELT(result, output, VECTOR_ELT(hidden_values, index));
    SET_STRING_ELT(names, output, STRING_ELT(hidden_names, index));
    ++output;
  }
  if (output != kept) {
    UNPROTECT(2);
    Rf_error("Internal error: incomplete ParamSetShadow hidden values");
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
}

static SEXP assemble_shadow_core(SEXP template_state, SEXP factories,
    SEXP signature,
    const paradox_domain_params_t *source_params,
    const paradox_domain_values_t *source_values,
    const paradox_domain_dependencies_t *source_dependencies,
    const paradox_domain_trafos_t *source_trafos,
    SEXP source_constraint, SEXP source_extra_trafo, SEXP reused_trafos,
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
    source_params->ids,
    work_since_interrupt
  ));
  /* Reusing the previous projection when the origin's schema slice is
   * unchanged is not just an allocation saving: an enclosing collection
   * decides whether its flatten is stale by comparing these very objects, so
   * a fresh table on every value commit would re-flatten the whole graph
   * around a Shadow on every iteration of a tuning loop. */
  SEXP trafos = PROTECT(
    reused_trafos != R_NilValue
      ? reused_trafos
      : filter_trafos(source_trafos, visible_ids, work_since_interrupt)
  );
  SEXP constraint_hidden_values = PROTECT(
    source_constraint == R_NilValue
      ? R_NilValue
      : active_hidden_values(
          source_params,
          source_values,
          source_dependencies,
          hidden_values,
          work_since_interrupt
        )
  );
  SEXP constraint = R_NilValue;
  if (source_constraint != R_NilValue) {
    constraint = evaluate_factory2(
      VECTOR_ELT(factories, SHADOW_FACTORY_CONSTRAINT),
      source_constraint,
      constraint_hidden_values
    );
    if (!Rf_isFunction(constraint)) {
      UNPROTECT(4);
      Rf_error("ParamSetShadow constraint factory returned no function");
    }
  }
  PROTECT(constraint);
  if (source_extra_trafo != R_NilValue &&
      !Rf_isFunction(source_extra_trafo)) {
    UNPROTECT(5);
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
    VECTOR_ELT(template_state, PARADOX_CORE_POSTFIX),
    VECTOR_ELT(template_state, PARADOX_CORE_EDGES)
  };
  SEXP result = PROTECT(paradox_core_new_from_fields(
    PARADOX_CORE_SHADOW,
    fields
  ));
  Rf_setAttrib(result, shadow_metadata_symbol(), signature);
  UNPROTECT(7);
  return result;
}

static SEXP build_from_validated_base(SEXP template_state, SEXP state,
    const paradox_domain_params_t *params,
    const paradox_domain_dependencies_t *dependencies,
    const paradox_domain_trafos_t *trafos,
    const paradox_domain_values_t *values,
    SEXP factories, SEXP signature, SEXP reused_trafos,
    R_xlen_t *work_since_interrupt) {
  return assemble_shadow_core(
    template_state,
    factories,
    signature,
    params,
    values,
    dependencies,
    trafos,
    VECTOR_ELT(state, PARADOX_CORE_CONSTRAINT),
    VECTOR_ELT(state, PARADOX_CORE_EXTRA_TRAFO),
    reused_trafos,
    work_since_interrupt
  );
}

static SEXP collection_constraint_from_plan(SEXP plan, SEXP factories) {
  if (TYPEOF(plan) != VECSXP || ALTREP(plan) ||
      XLENGTH(plan) != PARADOX_COLLECTION_DETACH_FIELD_COUNT) {
    Rf_error("Corrupt ParamSetCollection callback detachment plan");
  }
  SEXP result = evaluate_factory3(
    VECTOR_ELT(factories, SHADOW_FACTORY_COLLECTION_CONSTRAINT),
    VECTOR_ELT(plan, PARADOX_COLLECTION_DETACH_TRANSLATION),
    VECTOR_ELT(plan, PARADOX_COLLECTION_DETACH_CONSTRAINT_INDICES),
    VECTOR_ELT(plan, PARADOX_COLLECTION_DETACH_CONSTRAINT_SETS)
  );
  if (result != R_NilValue && !Rf_isFunction(result)) {
    Rf_error("ParamSetCollection constraint factory returned invalid state");
  }
  return result;
}

static SEXP collection_extra_trafo_from_plan(SEXP plan, SEXP factories) {
  SEXP result = evaluate_factory3(
    VECTOR_ELT(factories, SHADOW_FACTORY_COLLECTION_EXTRA_TRAFO),
    VECTOR_ELT(plan, PARADOX_COLLECTION_DETACH_TRANSLATION),
    VECTOR_ELT(plan, PARADOX_COLLECTION_DETACH_TRAFO_INDICES),
    VECTOR_ELT(plan, PARADOX_COLLECTION_DETACH_TRAFO_SETS)
  );
  if (result != R_NilValue && !Rf_isFunction(result)) {
    Rf_error("ParamSetCollection extra_trafo factory returned invalid state");
  }
  return result;
}

static SEXP build_from_collection(SEXP template_state, SEXP origin,
    SEXP origin_private, SEXP factories, SEXP signature, SEXP reused_trafos,
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

  SEXP plan = PROTECT(paradox_param_set_collection_detach_plan_from_graph(
    graph,
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
    &root->params,
    &values,
    &dependencies,
    &trafos,
    constraint,
    extra_trafo,
    reused_trafos,
    work_since_interrupt
  ));
  UNPROTECT(6);
  return result;
}

static SEXP origin_private_and_core(SEXP origin, SEXP *private_environment,
    int commit) {
  SEXP private_result = PROTECT(paradox_domain_private_environment(origin));
  if (private_result == R_UnboundValue) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSetShadow origin shell");
  }
  SEXP selected = paradox_core_from_private(private_result);
  /* A Shadow projects its origin's current semantics, so an origin whose own
   * derived state is stale is brought current before it is read -- except on
   * the read-only preview path, which may not install a capsule into any
   * current shell and therefore reads the origin exactly as it stands. */
  if (commit && selected != R_UnboundValue &&
      !paradox_core_is_verified(selected)) {
    selected = paradox_core_refresh(origin, private_result);
  }
  SEXP core = PROTECT(selected);
  if (!paradox_core_has_exact_schema(core)) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSetShadow origin capsule");
  }
  *private_environment = private_result;
  UNPROTECT(2);
  return core;
}

/* TRUE when the origin still exposes the exact schema objects this Shadow's
 * visible tables were derived from, so the projection can be carried forward
 * with its identity intact. */
static int shadow_slice_unchanged(SEXP core, SEXP origin_params,
    SEXP origin_tags, SEXP origin_trafos) {
  SEXP edges = VECTOR_ELT(
    R_ExternalPtrProtected(core),
    PARADOX_CORE_EDGES
  );
  return VECTOR_ELT(edges, PARADOX_SHADOW_EDGE_PARAMS) == origin_params &&
    VECTOR_ELT(edges, PARADOX_SHADOW_EDGE_TAGS) == origin_tags &&
    VECTOR_ELT(edges, PARADOX_SHADOW_EDGE_TRAFOS) == origin_trafos;
}

static SEXP shadow_hidden_ids(SEXP core) {
  return VECTOR_ELT(
    VECTOR_ELT(R_ExternalPtrProtected(core), PARADOX_CORE_EDGES),
    PARADOX_SHADOW_EDGE_SHADOWED
  );
}

static SEXP shadow_tag_override(SEXP core) {
  return VECTOR_ELT(
    VECTOR_ELT(R_ExternalPtrProtected(core), PARADOX_CORE_EDGES),
    PARADOX_SHADOW_EDGE_TAG_OVERRIDE
  );
}

/* The template a rebuild projects through: the visible schema recomputed from
 * the origin's current parameters, or -- when the origin's schema slice has
 * not moved -- the previous one carried forward with its object identity
 * intact, so a value commit does not look like a schema change to an
 * enclosing collection. */
static SEXP shadow_refresh_template(SEXP current_core, SEXP template_state,
    SEXP origin, const paradox_domain_params_t *origin_params_checked,
    SEXP origin_state, SEXP *reused_trafos,
    R_xlen_t *work_since_interrupt) {
  SEXP origin_params = VECTOR_ELT(origin_state, PARADOX_CORE_PARAMS);
  SEXP origin_tags = VECTOR_ELT(origin_state, PARADOX_CORE_TAGS);
  SEXP origin_trafos = VECTOR_ELT(origin_state, PARADOX_CORE_TRAFOS);
  if (shadow_slice_unchanged(
      current_core, origin_params, origin_tags, origin_trafos
    )) {
    *reused_trafos = VECTOR_ELT(template_state, PARADOX_CORE_TRAFOS);
    return template_state;
  }
  paradox_domain_tags_t checked_tags;
  if (!paradox_domain_validate_tags(
      origin_tags,
      &checked_tags,
      work_since_interrupt
    )) {
    Rf_error("Corrupt ParamSetShadow origin tags");
  }
  *reused_trafos = R_NilValue;
  return build_shadow_template_state(
    origin,
    shadow_hidden_ids(current_core),
    origin_params_checked,
    &checked_tags,
    origin_params,
    origin_tags,
    origin_trafos,
    shadow_tag_override(current_core),
    FALSE,
    work_since_interrupt
  );
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

  /* Captured before the origin generation is selected: everything from that
   * selection to the stamp below allocates -- the factories, the projection
   * build -- and a finalizer installing a capsule anywhere in that span may
   * have mutated the very origin this Shadow is being built from. A refresh
   * inside the selection advances no epoch, so the ordinary path still
   * stamps. */
  const uintptr_t entry_epoch = paradox_core_state_epoch_value();
  SEXP origin_private = R_NilValue;
  SEXP origin_core = PROTECT(origin_private_and_core(
    origin,
    &origin_private,
    TRUE
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
      VECTOR_ELT(state, PARADOX_CORE_PARAMS),
      VECTOR_ELT(state, PARADOX_CORE_TAGS),
      VECTOR_ELT(state, PARADOX_CORE_TRAFOS),
      R_NilValue,
      TRUE,
      &work_since_interrupt
    ));
    SEXP signature = PROTECT(base_signature(origin, origin_core));
    result = PROTECT(build_from_validated_base(
      template_state,
      state,
      &params,
      &dependencies,
      &trafos,
      &values,
      factories,
      signature,
      R_NilValue,
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
      VECTOR_ELT(root->state, PARADOX_CORE_PARAMS),
      VECTOR_ELT(root->state, PARADOX_CORE_TAGS),
      VECTOR_ELT(root->state, PARADOX_CORE_TRAFOS),
      R_NilValue,
      TRUE,
      &work_since_interrupt
    ));
    SEXP signature = PROTECT(graph_signature(&graph));
    result = PROTECT(build_from_collection(
      template_state,
      origin,
      origin_private,
      factories,
      signature,
      R_NilValue,
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
  PROTECT(result);
  if (paradox_core_state_epoch_value() == entry_epoch) {
    paradox_core_stamp_verified(result);
  }
  UNPROTECT(7);
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

  /* Captured before the origin generation is selected, for the same reason as
   * in the constructor above: an install between that selection and the stamp
   * below must forfeit the stamp. */
  const uintptr_t entry_epoch = paradox_core_state_epoch_value();
  SEXP origin_private = R_NilValue;
  SEXP origin_core = PROTECT(origin_private_and_core(
    origin,
    &origin_private,
    TRUE
  ));
  PROTECT(origin_private);
  SEXP factories = PROTECT(fixed_factories());
  const paradox_core_kind_t kind = paradox_core_kind(origin_core);
  SEXP result;
  if (kind == PARADOX_CORE_BASE) {
    paradox_domain_params_t origin_params;
    paradox_domain_dependencies_t origin_dependencies;
    paradox_domain_trafos_t origin_trafos;
    paradox_domain_values_t origin_values;
    SEXP origin_state = validate_base_origin(
      origin_core,
      &origin_params,
      &origin_dependencies,
      &origin_trafos,
      &origin_values,
      &work_since_interrupt
    );
    SEXP reused_trafos = R_NilValue;
    SEXP refreshed_template = PROTECT(shadow_refresh_template(
      template_core,
      template_state,
      origin,
      &origin_params,
      origin_state,
      &reused_trafos,
      &work_since_interrupt
    ));
    SEXP signature = PROTECT(base_signature(origin, origin_core));
    result = PROTECT(build_from_validated_base(
      refreshed_template,
      origin_state,
      &origin_params,
      &origin_dependencies,
      &origin_trafos,
      &origin_values,
      factories,
      signature,
      reused_trafos,
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
    SEXP reused_trafos = R_NilValue;
    SEXP refreshed_template = PROTECT(shadow_refresh_template(
      template_core,
      template_state,
      origin,
      &graph.nodes[0].params,
      graph.nodes[0].state,
      &reused_trafos,
      &work_since_interrupt
    ));
    SEXP signature = PROTECT(graph_signature(&graph));
    result = PROTECT(build_from_collection(
      refreshed_template,
      origin,
      origin_private,
      factories,
      signature,
      reused_trafos,
      &graph,
      &work_since_interrupt
    ));
    UNPROTECT(4);
  } else if (kind == PARADOX_CORE_SHADOW) {
    UNPROTECT(5);
    Rf_error("A ParamSetShadow cannot directly wrap another ParamSetShadow");
  } else {
    UNPROTECT(5);
    Rf_error("ParamSetShadow origin must be a BASE or COLLECTION node");
  }
  PROTECT(result);
  if (paradox_core_state_epoch_value() == entry_epoch) {
    paradox_core_stamp_verified(result);
  }
  UNPROTECT(6);
  return result;
}

static SEXP shadow_refresh_authoritative(SEXP self,
    SEXP private_environment, int commit) {
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
    &origin_private,
    commit
  ));
  PROTECT(origin_private);
  const paradox_core_kind_t kind = paradox_core_kind(origin_core);
  const uintptr_t entry_epoch = paradox_core_state_epoch_value();
  SEXP replacement;
  if (kind == PARADOX_CORE_BASE) {
    if (signature_matches_base(signature, origin, origin_core)) {
      if (!base_is_current(origin, origin_private, origin_core) ||
          paradox_core_from_private(private_environment) != current_core) {
        UNPROTECT(7);
        Rf_error("ParamSetShadow origin changed during native refresh");
      }
      if (commit) {
        paradox_core_stamp_verified(current_core);
      }
      UNPROTECT(7);
      return current_core;
    }
    paradox_domain_params_t origin_params;
    paradox_domain_dependencies_t origin_dependencies;
    paradox_domain_trafos_t origin_trafos;
    paradox_domain_values_t origin_values;
    SEXP origin_state = validate_base_origin(
      origin_core,
      &origin_params,
      &origin_dependencies,
      &origin_trafos,
      &origin_values,
      &work_since_interrupt
    );
    SEXP reused_trafos = R_NilValue;
    SEXP refreshed_template = PROTECT(shadow_refresh_template(
      current_core,
      template_state,
      origin,
      &origin_params,
      origin_state,
      &reused_trafos,
      &work_since_interrupt
    ));
    SEXP factories = PROTECT(fixed_factories());
    SEXP replacement_signature = PROTECT(base_signature(origin, origin_core));
    replacement = PROTECT(build_from_validated_base(
      refreshed_template,
      origin_state,
      &origin_params,
      &origin_dependencies,
      &origin_trafos,
      &origin_values,
      factories,
      replacement_signature,
      reused_trafos,
      &work_since_interrupt
    ));
    if (!base_is_current(origin, origin_private, origin_core)) {
      UNPROTECT(11);
      Rf_error("ParamSetShadow origin changed during native refresh");
    }
  } else if (kind == PARADOX_CORE_COLLECTION) {
    paradox_core_validate_graph_path(self);
    PROTECT_INDEX roots_index;
    SEXP roots;
    PROTECT_WITH_INDEX(roots = R_NilValue, &roots_index);
    paradox_collection_graph_t graph;
    if (commit) {
      paradox_collection_graph_build(
        origin_private,
        origin,
        &graph,
        &roots,
        roots_index,
        &work_since_interrupt
      );
    } else {
      paradox_collection_graph_build_readonly(
        origin_private,
        origin,
        &graph,
        &roots,
        roots_index,
        &work_since_interrupt
      );
    }
    if (signature_matches_graph(signature, &graph)) {
      if (!graph_is_current(&graph) ||
          paradox_core_from_private(private_environment) != current_core) {
        UNPROTECT(8);
        Rf_error("ParamSetShadow origin graph changed during refresh");
      }
      if (commit) {
        paradox_core_stamp_verified(current_core);
      }
      UNPROTECT(8);
      return current_core;
    }
    SEXP reused_trafos = R_NilValue;
    SEXP refreshed_template = PROTECT(shadow_refresh_template(
      current_core,
      template_state,
      origin,
      &graph.nodes[0].params,
      graph.nodes[0].state,
      &reused_trafos,
      &work_since_interrupt
    ));
    SEXP factories = PROTECT(fixed_factories());
    SEXP replacement_signature = PROTECT(graph_signature(&graph));
    replacement = PROTECT(build_from_collection(
      refreshed_template,
      origin,
      origin_private,
      factories,
      replacement_signature,
      reused_trafos,
      &graph,
      &work_since_interrupt
    ));
    if (!graph_is_current(&graph)) {
      UNPROTECT(12);
      Rf_error("ParamSetShadow origin graph changed during refresh");
    }
  } else if (kind == PARADOX_CORE_SHADOW) {
    UNPROTECT(7);
    Rf_error("A ParamSetShadow cannot directly wrap another ParamSetShadow");
  } else {
    UNPROTECT(7);
    Rf_error("Corrupt ParamSetShadow origin node kind");
  }

  if (paradox_core_from_private(private_environment) != current_core) {
    UNPROTECT(kind == PARADOX_CORE_BASE ? 11 : 12);
    Rf_error("ParamSetShadow capsule changed during native refresh");
  }
  if (commit) {
    /* A refresh installs the projection this Shadow already denoted, so it is
     * not a semantic change; the generation is current unless one of the
     * callback factories above installed a capsule of its own. */
    if (paradox_core_state_epoch_value() == entry_epoch) {
      paradox_core_stamp_verified(replacement);
    }
    Rf_defineVar(Rf_install(".core"), replacement, private_environment);
  }
  UNPROTECT(kind == PARADOX_CORE_BASE ? 11 : 12);
  return replacement;
}

SEXP paradox_shadow_refresh_authoritative(SEXP self,
    SEXP private_environment) {
  return shadow_refresh_authoritative(self, private_environment, TRUE);
}

SEXP paradox_shadow_preview_authoritative(SEXP self,
    SEXP private_environment) {
  return shadow_refresh_authoritative(self, private_environment, FALSE);
}
