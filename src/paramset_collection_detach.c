#include <limits.h>
#include <stddef.h>

#include "paradox.h"

#include "paramset_domain_common.h"
#include "paramset_params_internal.h"
#include "r_api_compat.h"

typedef struct {
  SEXP namespace_environment;
  SEXP enclosure;
  SEXP sets;
  SEXP set_names;
  SEXP translation;
  SEXP translation_names;
  SEXP translation_classes;
  SEXP translation_sorted;
  SEXP translation_index;
  SEXP translation_index_cache;
  SEXP ids;
  SEXP original_ids;
  SEXP owner_indices;
  SEXP owner_names;
  SEXP postfix;
  R_xlen_t child_count;
  R_xlen_t row_count;
  R_xlen_t selected_count;
  R_xlen_t constraint_count;
  R_xlen_t trafo_count;
} detach_state_t;

typedef struct {
  SEXP enclosure;
  SEXP private_environment;
  SEXP sets;
  SEXP translation;
  SEXP postfix;
  SEXP constraint;
  SEXP extra_trafo;
  SEXP values;
  SEXP public_constraint;
  SEXP public_extra_trafo;
  SEXP children_with_trafos;
  SEXP children_with_constraints;
  SEXP get_extra_trafo_detached;
  SEXP get_constraint_detached;
  SEXP sorted;
  SEXP index;
  SEXP x;
} detach_symbols_t;

enum detach_root_slot {
  DETACH_ROOT_SELF = 0,
  DETACH_ROOT_PRIVATE,
  DETACH_ROOT_REQUESTED,
  DETACH_ROOT_NAMESPACE,
  DETACH_ROOT_ENCLOSURE,
  DETACH_ROOT_SETS,
  DETACH_ROOT_SET_NAMES,
  DETACH_ROOT_TRANSLATION,
  DETACH_ROOT_TRANSLATION_NAMES,
  DETACH_ROOT_TRANSLATION_CLASSES,
  DETACH_ROOT_TRANSLATION_SORTED,
  DETACH_ROOT_TRANSLATION_INDEX,
  DETACH_ROOT_TRANSLATION_INDEX_CACHE,
  DETACH_ROOT_IDS,
  DETACH_ROOT_ORIGINAL_IDS,
  DETACH_ROOT_OWNER_INDICES,
  DETACH_ROOT_OWNER_NAMES,
  DETACH_ROOT_POSTFIX,
  DETACH_ROOT_PUBLIC_CONSTRAINT,
  DETACH_ROOT_PUBLIC_TRAFO,
  DETACH_ROOT_PRIVATE_CHILDREN_TRAFO,
  DETACH_ROOT_PRIVATE_CHILDREN_CONSTRAINT,
  DETACH_ROOT_PRIVATE_GET_TRAFO,
  DETACH_ROOT_PRIVATE_GET_CONSTRAINT,
  DETACH_ROOT_COUNT
};

enum detach_plan_slot {
  DETACH_PLAN_TRANSLATION = 0,
  DETACH_PLAN_CONSTRAINT_INDICES,
  DETACH_PLAN_CONSTRAINT_SETS,
  DETACH_PLAN_TRAFO_INDICES,
  DETACH_PLAN_TRAFO_SETS,
  DETACH_PLAN_POSTFIX,
  DETACH_PLAN_COUNT
};

enum detach_snapshot_slot {
  DETACH_SNAPSHOT_REQUESTED = 0,
  DETACH_SNAPSHOT_SET_NAMES,
  DETACH_SNAPSHOT_IDS,
  DETACH_SNAPSHOT_ORIGINAL_IDS,
  DETACH_SNAPSHOT_OWNER_INDICES,
  DETACH_SNAPSHOT_OWNER_NAMES,
  DETACH_SNAPSHOT_CHILDREN,
  DETACH_SNAPSHOT_CHILD_ENCLOSURES,
  DETACH_SNAPSHOT_CHILD_PRIVATES,
  DETACH_SNAPSHOT_CHILD_VALUES,
  DETACH_SNAPSHOT_CONSTRAINTS,
  DETACH_SNAPSHOT_TRAFOS,
  DETACH_SNAPSHOT_CONSTRAINT_BINDINGS,
  DETACH_SNAPSHOT_TRAFO_BINDINGS,
  DETACH_SNAPSHOT_ROOTS,
  DETACH_SNAPSHOT_POSTFIX,
  DETACH_SNAPSHOT_COUNT
};

static int exact_class(SEXP self, const char *const *expected,
    R_xlen_t count, R_xlen_t *work) {
  return TYPEOF(self) == ENVSXP && paradox_domain_exact_string_vector(
    Rf_getAttrib(self, R_ClassSymbol),
    expected,
    count,
    work
  );
}

static int exact_flag(SEXP value) {
  return TYPEOF(value) == LGLSXP && !ALTREP(value) &&
    XLENGTH(value) == 1 && paradox_api_has_no_attributes(value) &&
    LOGICAL_ELT(value, 0) != NA_LOGICAL;
}

static int exact_index_marker(SEXP index, SEXP roots) {
  if (TYPEOF(index) != INTSXP || ALTREP(index) || XLENGTH(index) != 0 ||
      !paradox_api_has_single_attribute(index, "__original_id")) {
    return FALSE;
  }
  SEXP cache = Rf_getAttrib(index, Rf_install("__original_id"));
  if (TYPEOF(cache) != INTSXP || ALTREP(cache)) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, DETACH_ROOT_TRANSLATION_INDEX_CACHE, cache);
  return TRUE;
}

static int exact_sets(SEXP sets, SEXP *names, R_xlen_t *count,
    R_xlen_t *work) {
  if (TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isObject(sets) ||
      !paradox_params_names_are_only_attribute(sets)) {
    return FALSE;
  }
  *names = Rf_getAttrib(sets, R_NamesSymbol);
  *count = XLENGTH(sets);
  if (TYPEOF(*names) != STRSXP || ALTREP(*names) ||
      !paradox_api_has_no_attributes(*names) || XLENGTH(*names) != *count) {
    return FALSE;
  }
  for (R_xlen_t right = 0; right < *count; ++right) {
    paradox_domain_account_work(work);
    SEXP name = STRING_ELT(*names, right);
    if (name == NA_STRING || Rf_getCharCE(name) == CE_BYTES) {
      return FALSE;
    }
    if (paradox_domain_string_is(name, "")) {
      continue;
    }
    for (R_xlen_t left = 0; left < right; ++left) {
      SEXP earlier = STRING_ELT(*names, left);
      if (!paradox_domain_string_is(earlier, "") &&
          paradox_domain_strings_equal(earlier, name)) {
        return FALSE;
      }
    }
  }
  return TRUE;
}

static int exact_translation(SEXP table, detach_state_t *state,
    SEXP roots, R_xlen_t *work) {
  static const char *const names_expected[] = {
    "id", "original_id", "owner_ps_index", "owner_name"
  };
  static const char *const classes_expected[] = {
    "data.table", "data.frame"
  };
  static const char *const sorted_expected[] = {"id"};
  if (TYPEOF(table) != VECSXP || ALTREP(table) || XLENGTH(table) != 4 ||
      !paradox_params_supported_table_attributes(table, TRUE) ||
      Rf_getAttrib(table, R_RowNamesSymbol) != R_NilValue ||
      Rf_getAttrib(table, Rf_install(".internal.selfref")) != R_NilValue) {
    return FALSE;
  }

  state->translation_names = Rf_getAttrib(table, R_NamesSymbol);
  state->translation_classes = Rf_getAttrib(table, R_ClassSymbol);
  state->translation_sorted = Rf_getAttrib(table, Rf_install("sorted"));
  state->translation_index = Rf_getAttrib(table, Rf_install("index"));
  SET_VECTOR_ELT(
    roots,
    DETACH_ROOT_TRANSLATION_NAMES,
    state->translation_names
  );
  SET_VECTOR_ELT(
    roots,
    DETACH_ROOT_TRANSLATION_CLASSES,
    state->translation_classes
  );
  SET_VECTOR_ELT(
    roots,
    DETACH_ROOT_TRANSLATION_SORTED,
    state->translation_sorted
  );
  SET_VECTOR_ELT(
    roots,
    DETACH_ROOT_TRANSLATION_INDEX,
    state->translation_index
  );
  if (TYPEOF(state->translation_names) != STRSXP ||
      ALTREP(state->translation_names) ||
      !paradox_api_has_no_attributes(state->translation_names) ||
      TYPEOF(state->translation_classes) != STRSXP ||
      ALTREP(state->translation_classes) ||
      !paradox_api_has_no_attributes(state->translation_classes) ||
      TYPEOF(state->translation_sorted) != STRSXP ||
      ALTREP(state->translation_sorted) ||
      !paradox_api_has_no_attributes(state->translation_sorted) ||
      !paradox_domain_exact_string_vector(
        state->translation_names,
        names_expected,
        4,
        work
      ) || !paradox_domain_exact_string_vector(
        state->translation_classes,
        classes_expected,
        2,
        work
      ) || !paradox_domain_exact_string_vector(
        state->translation_sorted,
        sorted_expected,
        1,
        work
      ) || !exact_index_marker(state->translation_index, roots)) {
    return FALSE;
  }

  state->ids = VECTOR_ELT(table, 0);
  state->original_ids = VECTOR_ELT(table, 1);
  state->owner_indices = VECTOR_ELT(table, 2);
  state->owner_names = VECTOR_ELT(table, 3);
  SET_VECTOR_ELT(roots, DETACH_ROOT_IDS, state->ids);
  SET_VECTOR_ELT(roots, DETACH_ROOT_ORIGINAL_IDS, state->original_ids);
  SET_VECTOR_ELT(roots, DETACH_ROOT_OWNER_INDICES, state->owner_indices);
  SET_VECTOR_ELT(roots, DETACH_ROOT_OWNER_NAMES, state->owner_names);
  if (TYPEOF(state->ids) != STRSXP || ALTREP(state->ids) ||
      !paradox_api_has_no_attributes(state->ids) ||
      TYPEOF(state->original_ids) != STRSXP || ALTREP(state->original_ids) ||
      !paradox_api_has_no_attributes(state->original_ids) ||
      TYPEOF(state->owner_indices) != INTSXP || ALTREP(state->owner_indices) ||
      !paradox_api_has_no_attributes(state->owner_indices) ||
      TYPEOF(state->owner_names) != STRSXP || ALTREP(state->owner_names) ||
      !paradox_api_has_no_attributes(state->owner_names)) {
    return FALSE;
  }
  state->row_count = XLENGTH(state->ids);
  if (XLENGTH(state->original_ids) != state->row_count ||
      XLENGTH(state->owner_indices) != state->row_count ||
      XLENGTH(state->owner_names) != state->row_count) {
    return FALSE;
  }
  for (R_xlen_t row = 0; row < state->row_count; ++row) {
    paradox_domain_account_work(work);
    SEXP id = STRING_ELT(state->ids, row);
    SEXP original_id = STRING_ELT(state->original_ids, row);
    SEXP owner_name = STRING_ELT(state->owner_names, row);
    const int owner = INTEGER_ELT(state->owner_indices, row);
    if (id == NA_STRING || original_id == NA_STRING ||
        owner_name == NA_STRING || owner <= 0 ||
        (R_xlen_t) owner > state->child_count ||
        !paradox_domain_strings_equal(
          owner_name,
          STRING_ELT(state->set_names, (R_xlen_t) owner - 1)
        )) {
      return FALSE;
    }
    for (R_xlen_t earlier = 0; earlier < row; ++earlier) {
      if (paradox_domain_strings_equal(
          id,
          STRING_ELT(state->ids, earlier)
        )) {
        return FALSE;
      }
    }
  }
  return TRUE;
}

static int exact_callback(SEXP callback) {
  if (TYPEOF(callback) != CLOSXP) {
    return FALSE;
  }
  SEXP formals = paradox_api_closure_formals(callback);
  return TYPEOF(formals) == LISTSXP && TAG(formals) == Rf_install("x") &&
    CDR(formals) == R_NilValue;
}

static int callback_detaches_without_child(SEXP callback, SEXP child,
    SEXP enclosure, SEXP private_environment) {
  if (callback == R_NilValue) {
    return TRUE;
  }
  SEXP environment = paradox_api_closure_environment(callback);
  return TYPEOF(environment) == ENVSXP && environment != child &&
    environment != enclosure && environment != private_environment;
}

static int exact_values_without_r6(SEXP values, int inspect_values) {
  if (TYPEOF(values) != VECSXP || ALTREP(values) ||
      !paradox_params_names_are_only_attribute(values)) {
    return FALSE;
  }
  if (!inspect_values) {
    return TRUE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(values); ++index) {
    /* ParamSet$deep_clone() probes every top-level environment for an R6
     * enclosure and can then run clone(deep = TRUE).  The callback carrier
     * deliberately has no child to clone, so environments retain that
     * callback/error boundary on the historical path. */
    if (TYPEOF(VECTOR_ELT(values, index)) == ENVSXP) {
      return FALSE;
    }
  }
  return TRUE;
}

static int exact_forwarder(SEXP function, const char *target,
    int has_ids_argument) {
  if (TYPEOF(function) != CLOSXP) {
    return FALSE;
  }
  SEXP body = paradox_api_closure_expression(function);
  SEXP formals = paradox_api_closure_formals(function);
  if (TYPEOF(body) != LANGSXP || CAR(body) != Rf_install(target)) {
    return FALSE;
  }
  static const char *const fixed[] = {"self", "private", "super"};
  SEXP argument = CDR(body);
  for (R_xlen_t index = 0; index < 3; ++index) {
    SEXP symbol = Rf_install(fixed[index]);
    if (TYPEOF(argument) != LISTSXP || TAG(argument) != symbol ||
        CAR(argument) != symbol) {
      return FALSE;
    }
    argument = CDR(argument);
  }
  if (has_ids_argument) {
    SEXP ids_symbol = Rf_install("ids");
    if (TYPEOF(argument) != LISTSXP || TAG(argument) != ids_symbol ||
        CAR(argument) != ids_symbol || CDR(argument) != R_NilValue ||
        TYPEOF(formals) != LISTSXP || TAG(formals) != ids_symbol ||
        CAR(formals) != R_NilValue || CDR(formals) != R_NilValue) {
      return FALSE;
    }
  } else if (argument != R_NilValue || formals != R_NilValue) {
    return FALSE;
  }
  return TRUE;
}

static int canonical_private_method(SEXP private_environment,
    SEXP enclosure, SEXP namespace_environment, const char *name,
    const char *target, int has_ids_argument, SEXP *result) {
  SEXP symbol = Rf_install(name);
  SEXP target_symbol = Rf_install(target);
  if (!R_existsVarInFrame(private_environment, symbol) ||
      R_BindingIsActive(symbol, private_environment) ||
      !R_BindingIsLocked(symbol, private_environment) ||
      !R_existsVarInFrame(namespace_environment, target_symbol) ||
      R_BindingIsActive(target_symbol, namespace_environment) ||
      !R_BindingIsLocked(target_symbol, namespace_environment)) {
    return FALSE;
  }
  SEXP function = paradox_domain_local_value(private_environment, name);
  if (!exact_forwarder(function, target, has_ids_argument) ||
      paradox_api_closure_environment(function) != enclosure ||
      R_existsVarInFrame(enclosure, target_symbol)) {
    return FALSE;
  }
  *result = function;
  return TRUE;
}

static int requested_contains(SEXP requested, SEXP id) {
  if (requested == R_NilValue) {
    return TRUE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(requested); ++index) {
    if (paradox_domain_strings_equal(STRING_ELT(requested, index), id)) {
      return TRUE;
    }
  }
  return FALSE;
}

static int owner_selected(const detach_state_t *state, SEXP requested,
    R_xlen_t owner) {
  for (R_xlen_t row = 0; row < state->row_count; ++row) {
    if ((R_xlen_t) INTEGER_ELT(state->owner_indices, row) == owner &&
        requested_contains(requested, STRING_ELT(state->ids, row))) {
      return TRUE;
    }
  }
  return FALSE;
}

static SEXP child_private(SEXP child, SEXP *enclosure) {
  *enclosure = paradox_domain_local_value(child, ".__enclos_env__");
  if (TYPEOF(*enclosure) != ENVSXP) {
    return R_UnboundValue;
  }
  SEXP private_environment = paradox_domain_local_value(*enclosure, "private");
  return TYPEOF(private_environment) == ENVSXP
    ? private_environment
    : R_UnboundValue;
}

static SEXP active_function(SEXP self, const char *name) {
  SEXP symbol = Rf_install(name);
  return R_existsVarInFrame(self, symbol) &&
      R_BindingIsActive(symbol, self)
    ? R_ActiveBindingFunction(symbol, self)
    : R_UnboundValue;
}

static SEXP active_function_symbol(SEXP self, SEXP symbol) {
  return TYPEOF(self) == ENVSXP && R_existsVarInFrame(self, symbol) &&
      R_BindingIsActive(symbol, self)
    ? R_ActiveBindingFunction(symbol, self)
    : R_UnboundValue;
}

static detach_symbols_t install_detach_symbols(void) {
  const detach_symbols_t symbols = {
    Rf_install(".__enclos_env__"),
    Rf_install("private"),
    Rf_install(".sets"),
    Rf_install(".translation"),
    Rf_install(".postfix"),
    Rf_install(".constraint"),
    Rf_install(".extra_trafo"),
    Rf_install(".values"),
    Rf_install("constraint"),
    Rf_install("extra_trafo"),
    Rf_install(".children_with_trafos"),
    Rf_install(".children_with_constraints"),
    Rf_install(".get_extra_trafo_detached"),
    Rf_install(".get_constraint_detached"),
    Rf_install("sorted"),
    Rf_install("index"),
    Rf_install("x")
  };
  return symbols;
}

static int load_state(SEXP private_environment, SEXP self, SEXP requested,
    SEXP roots, SEXP child_enclosures, SEXP child_privates, SEXP child_values,
    SEXP constraints, SEXP trafos, SEXP constraint_bindings,
    SEXP trafo_bindings, detach_state_t *state) {
  static const char *const collection_class[] = {
    "ParamSetCollection", "ParamSet", "R6"
  };
  R_xlen_t work = 0;
  if (!exact_class(self, collection_class, 3, &work) ||
      !paradox_domain_owns_private_environment(self, private_environment)) {
    return FALSE;
  }
  state->namespace_environment = paradox_api_registered_namespace("paradox");
  state->enclosure = paradox_domain_local_value(self, ".__enclos_env__");
  SET_VECTOR_ELT(roots, DETACH_ROOT_NAMESPACE, state->namespace_environment);
  SET_VECTOR_ELT(roots, DETACH_ROOT_ENCLOSURE, state->enclosure);
  if (TYPEOF(state->namespace_environment) != ENVSXP ||
      TYPEOF(state->enclosure) != ENVSXP ||
      paradox_api_parent_environment(state->enclosure) !=
        state->namespace_environment ||
      paradox_domain_local_value(state->enclosure, "self") != self ||
      paradox_domain_local_value(state->enclosure, "private") !=
        private_environment ||
      !paradox_params_canonical_active_member(
        self,
        private_environment,
        "constraint",
        ".__ParamSetCollection__constraint",
        "f",
        ".__ParamSet__constraint",
        &work
      ) || !paradox_params_canonical_active_member(
        self,
        private_environment,
        "extra_trafo",
        ".__ParamSetCollection__extra_trafo",
        "f",
        ".__ParamSet__extra_trafo",
        &work
      )) {
    return FALSE;
  }
  SEXP public_constraint = active_function(self, "constraint");
  SEXP public_trafo = active_function(self, "extra_trafo");
  SET_VECTOR_ELT(roots, DETACH_ROOT_PUBLIC_CONSTRAINT, public_constraint);
  SET_VECTOR_ELT(roots, DETACH_ROOT_PUBLIC_TRAFO, public_trafo);

  SEXP private_methods[4];
  if (!canonical_private_method(
        private_environment,
        state->enclosure,
        state->namespace_environment,
        ".children_with_trafos",
        ".__ParamSetCollection__.children_with_trafos",
        FALSE,
        &private_methods[0]
      ) || !canonical_private_method(
        private_environment,
        state->enclosure,
        state->namespace_environment,
        ".children_with_constraints",
        ".__ParamSetCollection__.children_with_constraints",
        FALSE,
        &private_methods[1]
      ) || !canonical_private_method(
        private_environment,
        state->enclosure,
        state->namespace_environment,
        ".get_extra_trafo_detached",
        ".__ParamSetCollection__.get_extra_trafo_detached",
        TRUE,
        &private_methods[2]
      ) || !canonical_private_method(
        private_environment,
        state->enclosure,
        state->namespace_environment,
        ".get_constraint_detached",
        ".__ParamSetCollection__.get_constraint_detached",
        TRUE,
        &private_methods[3]
      )) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, DETACH_ROOT_PRIVATE_CHILDREN_TRAFO, private_methods[0]);
  SET_VECTOR_ELT(
    roots,
    DETACH_ROOT_PRIVATE_CHILDREN_CONSTRAINT,
    private_methods[1]
  );
  SET_VECTOR_ELT(roots, DETACH_ROOT_PRIVATE_GET_TRAFO, private_methods[2]);
  SET_VECTOR_ELT(
    roots,
    DETACH_ROOT_PRIVATE_GET_CONSTRAINT,
    private_methods[3]
  );

  state->sets = paradox_domain_local_value(private_environment, ".sets");
  state->translation = paradox_domain_local_value(
    private_environment,
    ".translation"
  );
  state->postfix = paradox_domain_local_value(private_environment, ".postfix");
  SET_VECTOR_ELT(roots, DETACH_ROOT_SETS, state->sets);
  SET_VECTOR_ELT(roots, DETACH_ROOT_TRANSLATION, state->translation);
  SET_VECTOR_ELT(roots, DETACH_ROOT_POSTFIX, state->postfix);
  if (!exact_sets(
        state->sets,
        &state->set_names,
        &state->child_count,
        &work
      ) || XLENGTH(child_enclosures) != state->child_count ||
      !exact_flag(state->postfix)) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, DETACH_ROOT_SET_NAMES, state->set_names);
  if (!exact_translation(state->translation, state, roots, &work)) {
    return FALSE;
  }
  if (requested != R_NilValue &&
      (TYPEOF(requested) != STRSXP || ALTREP(requested) ||
       !paradox_api_has_no_attributes(requested))) {
    return FALSE;
  }
  for (R_xlen_t index = 0; requested != R_NilValue &&
      index < XLENGTH(requested); ++index) {
    SEXP id = STRING_ELT(requested, index);
    int found = id != NA_STRING;
    if (found) {
      found = FALSE;
      for (R_xlen_t row = 0; row < state->row_count; ++row) {
        if (paradox_domain_strings_equal(id, STRING_ELT(state->ids, row))) {
          found = TRUE;
          break;
        }
      }
    }
    if (!found) {
      return FALSE;
    }
  }

  state->selected_count = 0;
  for (R_xlen_t row = 0; row < state->row_count; ++row) {
    if (requested_contains(requested, STRING_ELT(state->ids, row))) {
      ++state->selected_count;
    }
  }
  state->constraint_count = 0;
  state->trafo_count = 0;
  static const char *const set_class[] = {"ParamSet", "R6"};
  for (R_xlen_t index = 0; index < state->child_count; ++index) {
    paradox_domain_account_work(&work);
    SEXP child = VECTOR_ELT(state->sets, index);
    SEXP enclosure = R_UnboundValue;
    SEXP child_private_environment = child_private(child, &enclosure);
    SET_VECTOR_ELT(child_enclosures, index, enclosure);
    SET_VECTOR_ELT(child_privates, index, child_private_environment);
    if (!exact_class(child, set_class, 2, &work) ||
        TYPEOF(child_private_environment) != ENVSXP ||
        paradox_api_parent_environment(enclosure) !=
          state->namespace_environment ||
        paradox_domain_local_value(enclosure, "self") != child ||
        paradox_domain_local_value(enclosure, "private") !=
          child_private_environment ||
        !paradox_params_canonical_active_member(
          child,
          child_private_environment,
          "constraint",
          ".__ParamSet__constraint",
          "f",
          NULL,
          &work
        ) || !paradox_params_canonical_active_member(
          child,
          child_private_environment,
          "extra_trafo",
          ".__ParamSet__extra_trafo",
          "f",
          NULL,
          &work
        )) {
      return FALSE;
    }
    SEXP constraint_binding = active_function(child, "constraint");
    SET_VECTOR_ELT(constraint_bindings, index, constraint_binding);
    SEXP trafo_binding = active_function(child, "extra_trafo");
    SET_VECTOR_ELT(trafo_bindings, index, trafo_binding);
    SEXP constraint = paradox_domain_local_value(
      child_private_environment,
      ".constraint"
    );
    SET_VECTOR_ELT(constraints, index, constraint);
    SEXP trafo = paradox_domain_local_value(
      child_private_environment,
      ".extra_trafo"
    );
    SET_VECTOR_ELT(trafos, index, trafo);
    SEXP values = paradox_domain_local_value(
      child_private_environment,
      ".values"
    );
    SET_VECTOR_ELT(child_values, index, values);
    const int selected_child = owner_selected(
      state,
      requested,
      index + 1
    );
    if ((constraint != R_NilValue && !exact_callback(constraint)) ||
        (trafo != R_NilValue && !exact_callback(trafo)) ||
        !callback_detaches_without_child(
          constraint,
          child,
          enclosure,
          child_private_environment
        ) || !callback_detaches_without_child(
          trafo,
          child,
          enclosure,
          child_private_environment
        ) || !exact_values_without_r6(values, selected_child)) {
      return FALSE;
    }
    if (selected_child) {
      state->constraint_count += constraint != R_NilValue;
      state->trafo_count += trafo != R_NilValue;
    }
  }
  return TRUE;
}

static int state_stable(SEXP private_environment, SEXP self, SEXP requested,
    SEXP roots,
    SEXP child_enclosures, SEXP child_privates, SEXP child_values,
    SEXP constraints, SEXP trafos, SEXP constraint_bindings, SEXP trafo_bindings,
    const detach_state_t *state) {
  if (paradox_domain_local_value(self, ".__enclos_env__") !=
        state->enclosure ||
      paradox_domain_local_value(state->enclosure, "private") !=
        private_environment ||
      paradox_domain_local_value(private_environment, ".sets") !=
        state->sets ||
      paradox_domain_local_value(private_environment, ".translation") !=
        state->translation ||
      paradox_domain_local_value(private_environment, ".postfix") !=
        state->postfix ||
      Rf_getAttrib(state->sets, R_NamesSymbol) != state->set_names ||
      Rf_getAttrib(state->translation, R_NamesSymbol) !=
        state->translation_names ||
      Rf_getAttrib(state->translation, R_ClassSymbol) !=
        state->translation_classes ||
      Rf_getAttrib(state->translation, Rf_install("sorted")) !=
        state->translation_sorted ||
      Rf_getAttrib(state->translation, Rf_install("index")) !=
        state->translation_index ||
      VECTOR_ELT(state->translation, 0) != state->ids ||
      VECTOR_ELT(state->translation, 1) != state->original_ids ||
      VECTOR_ELT(state->translation, 2) != state->owner_indices ||
      VECTOR_ELT(state->translation, 3) != state->owner_names ||
      active_function(self, "constraint") !=
        VECTOR_ELT(roots, DETACH_ROOT_PUBLIC_CONSTRAINT) ||
      active_function(self, "extra_trafo") !=
        VECTOR_ELT(roots, DETACH_ROOT_PUBLIC_TRAFO) ||
      paradox_domain_local_value(
        private_environment,
        ".children_with_trafos"
      ) != VECTOR_ELT(roots, DETACH_ROOT_PRIVATE_CHILDREN_TRAFO) ||
      paradox_domain_local_value(
        private_environment,
        ".children_with_constraints"
      ) != VECTOR_ELT(roots, DETACH_ROOT_PRIVATE_CHILDREN_CONSTRAINT) ||
      paradox_domain_local_value(
        private_environment,
        ".get_extra_trafo_detached"
      ) != VECTOR_ELT(roots, DETACH_ROOT_PRIVATE_GET_TRAFO) ||
      paradox_domain_local_value(
        private_environment,
        ".get_constraint_detached"
      ) != VECTOR_ELT(roots, DETACH_ROOT_PRIVATE_GET_CONSTRAINT)) {
    return FALSE;
  }
  R_xlen_t selected_count = 0;
  for (R_xlen_t row = 0; row < state->row_count; ++row) {
    const int owner = INTEGER_ELT(state->owner_indices, row);
    if (owner <= 0 || (R_xlen_t) owner > state->child_count ||
        STRING_ELT(state->ids, row) == NA_STRING ||
        STRING_ELT(state->original_ids, row) == NA_STRING ||
        STRING_ELT(state->owner_names, row) == NA_STRING ||
        !paradox_domain_strings_equal(
          STRING_ELT(state->owner_names, row),
          STRING_ELT(state->set_names, (R_xlen_t) owner - 1)
        )) {
      return FALSE;
    }
    selected_count += requested_contains(
      requested,
      STRING_ELT(state->ids, row)
    );
  }
  if (selected_count != state->selected_count) {
    return FALSE;
  }
  R_xlen_t constraint_count = 0;
  R_xlen_t trafo_count = 0;
  for (R_xlen_t index = 0; index < state->child_count; ++index) {
    SEXP child = VECTOR_ELT(state->sets, index);
    SEXP enclosure = VECTOR_ELT(child_enclosures, index);
    SEXP child_private_environment = VECTOR_ELT(child_privates, index);
    if (paradox_domain_local_value(child, ".__enclos_env__") != enclosure ||
        paradox_domain_local_value(enclosure, "private") !=
          child_private_environment ||
        active_function(child, "constraint") !=
          VECTOR_ELT(constraint_bindings, index) ||
        active_function(child, "extra_trafo") !=
          VECTOR_ELT(trafo_bindings, index) ||
        paradox_domain_local_value(child_private_environment, ".constraint") !=
          VECTOR_ELT(constraints, index) ||
        paradox_domain_local_value(
          child_private_environment,
          ".extra_trafo"
        ) != VECTOR_ELT(trafos, index) || paradox_domain_local_value(
          child_private_environment,
          ".values"
        ) != VECTOR_ELT(child_values, index) ||
        (VECTOR_ELT(constraints, index) != R_NilValue &&
          !exact_callback(VECTOR_ELT(constraints, index))) ||
        (VECTOR_ELT(trafos, index) != R_NilValue &&
          !exact_callback(VECTOR_ELT(trafos, index))) ||
        !callback_detaches_without_child(
          VECTOR_ELT(constraints, index),
          child,
          enclosure,
          child_private_environment
        ) || !callback_detaches_without_child(
          VECTOR_ELT(trafos, index),
          child,
          enclosure,
          child_private_environment
        ) || !exact_values_without_r6(
          VECTOR_ELT(child_values, index),
          owner_selected(state, requested, index + 1)
        )) {
      return FALSE;
    }
    if (owner_selected(state, requested, index + 1)) {
      constraint_count += VECTOR_ELT(constraints, index) != R_NilValue;
      trafo_count += VECTOR_ELT(trafos, index) != R_NilValue;
    }
  }
  return constraint_count == state->constraint_count &&
    trafo_count == state->trafo_count;
}

static int live_callback_safe(SEXP callback, SEXP child, SEXP enclosure,
    SEXP private_environment, SEXP x_symbol) {
  if (callback == R_NilValue) {
    return TRUE;
  }
  if (TYPEOF(callback) != CLOSXP) {
    return FALSE;
  }
  SEXP formals = paradox_api_closure_formals(callback);
  SEXP environment = paradox_api_closure_environment(callback);
  return TYPEOF(formals) == LISTSXP && TAG(formals) == x_symbol &&
    CDR(formals) == R_NilValue && TYPEOF(environment) == ENVSXP &&
    environment != child && environment != enclosure &&
    environment != private_environment;
}

/* Commit-boundary reread.  Every symbol was interned before state inspection,
 * every environment requires the R 4.6 value-binding API, and no operation in
 * this function translates strings, checks interrupts, or allocates. */
static int live_bindings_match(SEXP private_environment, SEXP self,
    SEXP roots, SEXP child_enclosures, SEXP child_privates, SEXP child_values,
    SEXP constraints, SEXP trafos, SEXP constraint_bindings,
    SEXP trafo_bindings, SEXP selected_owners,
    const detach_symbols_t *symbols,
    const detach_state_t *state) {
  if (paradox_api_local_value(self, symbols->enclosure) != state->enclosure ||
      paradox_api_local_value(
        state->enclosure,
        symbols->private_environment
      ) != private_environment || paradox_api_local_value(
        private_environment,
        symbols->sets
      ) != state->sets || paradox_api_local_value(
        private_environment,
        symbols->translation
      ) != state->translation || paradox_api_local_value(
        private_environment,
        symbols->postfix
      ) != state->postfix || paradox_api_local_value(
        private_environment,
        symbols->children_with_trafos
      ) != VECTOR_ELT(roots, DETACH_ROOT_PRIVATE_CHILDREN_TRAFO) ||
      paradox_api_local_value(
        private_environment,
        symbols->children_with_constraints
      ) != VECTOR_ELT(roots, DETACH_ROOT_PRIVATE_CHILDREN_CONSTRAINT) ||
      paradox_api_local_value(
        private_environment,
        symbols->get_extra_trafo_detached
      ) != VECTOR_ELT(roots, DETACH_ROOT_PRIVATE_GET_TRAFO) ||
      paradox_api_local_value(
        private_environment,
        symbols->get_constraint_detached
      ) != VECTOR_ELT(roots, DETACH_ROOT_PRIVATE_GET_CONSTRAINT) ||
      active_function_symbol(self, symbols->public_constraint) !=
        VECTOR_ELT(roots, DETACH_ROOT_PUBLIC_CONSTRAINT) ||
      active_function_symbol(self, symbols->public_extra_trafo) !=
        VECTOR_ELT(roots, DETACH_ROOT_PUBLIC_TRAFO) ||
      Rf_getAttrib(state->sets, R_NamesSymbol) != state->set_names ||
      Rf_getAttrib(state->translation, R_NamesSymbol) !=
        state->translation_names ||
      Rf_getAttrib(state->translation, R_ClassSymbol) !=
        state->translation_classes ||
      Rf_getAttrib(state->translation, symbols->sorted) !=
        state->translation_sorted ||
      Rf_getAttrib(state->translation, symbols->index) !=
        state->translation_index ||
      VECTOR_ELT(state->translation, 0) != state->ids ||
      VECTOR_ELT(state->translation, 1) != state->original_ids ||
      VECTOR_ELT(state->translation, 2) != state->owner_indices ||
      VECTOR_ELT(state->translation, 3) != state->owner_names) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < state->child_count; ++index) {
    SEXP child = VECTOR_ELT(state->sets, index);
    SEXP enclosure = VECTOR_ELT(child_enclosures, index);
    SEXP child_private_environment = VECTOR_ELT(child_privates, index);
    SEXP constraint = VECTOR_ELT(constraints, index);
    SEXP trafo = VECTOR_ELT(trafos, index);
    SEXP values = VECTOR_ELT(child_values, index);
    if (paradox_api_local_value(child, symbols->enclosure) != enclosure ||
        paradox_api_local_value(
          enclosure,
          symbols->private_environment
        ) != child_private_environment || paradox_api_local_value(
          child_private_environment,
          symbols->constraint
        ) != constraint || paradox_api_local_value(
          child_private_environment,
          symbols->extra_trafo
        ) != trafo || paradox_api_local_value(
          child_private_environment,
          symbols->values
        ) != values || active_function_symbol(
          child,
          symbols->public_constraint
        ) != VECTOR_ELT(constraint_bindings, index) || active_function_symbol(
          child,
          symbols->public_extra_trafo
        ) != VECTOR_ELT(trafo_bindings, index) ||
        !live_callback_safe(
          constraint,
          child,
          enclosure,
          child_private_environment,
          symbols->x
        ) || !live_callback_safe(
          trafo,
          child,
          enclosure,
          child_private_environment,
          symbols->x
        ) || TYPEOF(values) != VECSXP || ALTREP(values)) {
      return FALSE;
    }
    if (LOGICAL_ELT(selected_owners, index)) {
      for (R_xlen_t value = 0; value < XLENGTH(values); ++value) {
        if (TYPEOF(VECTOR_ELT(values, value)) == ENVSXP) {
          return FALSE;
        }
      }
    }
  }
  return TRUE;
}

static SEXP shallow_vector_copy(SEXP source) {
  if (source == R_NilValue) {
    return R_NilValue;
  }
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
  const R_xlen_t size = XLENGTH(source);
  SEXP result = PROTECT(Rf_allocVector(type, size));
  switch (type) {
    case STRSXP:
      for (R_xlen_t index = 0; index < size; ++index) {
        SET_STRING_ELT(result, index, STRING_ELT(source, index));
      }
      break;
    case INTSXP:
      for (R_xlen_t index = 0; index < size; ++index) {
        INTEGER(result)[index] = INTEGER_ELT(source, index);
      }
      break;
    case VECSXP:
      for (R_xlen_t index = 0; index < size; ++index) {
        SET_VECTOR_ELT(result, index, VECTOR_ELT(source, index));
      }
      break;
    default:
      UNPROTECT(1);
      Rf_error("Internal error: unsupported collection snapshot vector");
  }
  UNPROTECT(1);
  return result;
}

static int same_strings(SEXP left, SEXP right) {
  if (left == R_NilValue || right == R_NilValue) {
    return left == right;
  }
  if (TYPEOF(left) != STRSXP || TYPEOF(right) != STRSXP || ALTREP(left) ||
      ALTREP(right) || XLENGTH(left) != XLENGTH(right)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(left); ++index) {
    /* Snapshot admission is deliberately pointer-exact.  Content equality
     * may call Rf_translateCharUTF8() for differently encoded CHARSXPs and
     * can therefore allocate.  A replaced string conservatively abandons the
     * native plan even when its translated text happens to match. */
    if (STRING_ELT(left, index) != STRING_ELT(right, index)) {
      return FALSE;
    }
  }
  return TRUE;
}

static int same_integers(SEXP left, SEXP right) {
  if (TYPEOF(left) != INTSXP || TYPEOF(right) != INTSXP || ALTREP(left) ||
      ALTREP(right) || XLENGTH(left) != XLENGTH(right)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(left); ++index) {
    if (INTEGER_ELT(left, index) != INTEGER_ELT(right, index)) {
      return FALSE;
    }
  }
  return TRUE;
}

static int same_list_pointers(SEXP left, SEXP right) {
  if (TYPEOF(left) != VECSXP || TYPEOF(right) != VECSXP || ALTREP(left) ||
      ALTREP(right) || XLENGTH(left) != XLENGTH(right)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(left); ++index) {
    if (VECTOR_ELT(left, index) != VECTOR_ELT(right, index)) {
      return FALSE;
    }
  }
  return TRUE;
}

static int snapshot_matches(SEXP snapshot, SEXP roots, SEXP requested,
    SEXP child_enclosures, SEXP child_privates, SEXP child_values,
    SEXP constraints, SEXP trafos, SEXP constraint_bindings, SEXP trafo_bindings,
    const detach_state_t *state) {
  if (!same_strings(
        requested,
        VECTOR_ELT(snapshot, DETACH_SNAPSHOT_REQUESTED)
      ) || !same_strings(
        state->set_names,
        VECTOR_ELT(snapshot, DETACH_SNAPSHOT_SET_NAMES)
      ) || !same_strings(
        state->ids,
        VECTOR_ELT(snapshot, DETACH_SNAPSHOT_IDS)
      ) || !same_strings(
        state->original_ids,
        VECTOR_ELT(snapshot, DETACH_SNAPSHOT_ORIGINAL_IDS)
      ) || !same_integers(
        state->owner_indices,
        VECTOR_ELT(snapshot, DETACH_SNAPSHOT_OWNER_INDICES)
      ) || !same_strings(
        state->owner_names,
        VECTOR_ELT(snapshot, DETACH_SNAPSHOT_OWNER_NAMES)
      ) || !same_list_pointers(
        state->sets,
        VECTOR_ELT(snapshot, DETACH_SNAPSHOT_CHILDREN)
      ) || !same_list_pointers(
        child_enclosures,
        VECTOR_ELT(snapshot, DETACH_SNAPSHOT_CHILD_ENCLOSURES)
      ) || !same_list_pointers(
        child_privates,
        VECTOR_ELT(snapshot, DETACH_SNAPSHOT_CHILD_PRIVATES)
      ) || !same_list_pointers(
        child_values,
        VECTOR_ELT(snapshot, DETACH_SNAPSHOT_CHILD_VALUES)
      ) || !same_list_pointers(
        constraints,
        VECTOR_ELT(snapshot, DETACH_SNAPSHOT_CONSTRAINTS)
      ) || !same_list_pointers(
        trafos,
        VECTOR_ELT(snapshot, DETACH_SNAPSHOT_TRAFOS)
      ) || !same_list_pointers(
        constraint_bindings,
        VECTOR_ELT(snapshot, DETACH_SNAPSHOT_CONSTRAINT_BINDINGS)
      ) || !same_list_pointers(
        trafo_bindings,
        VECTOR_ELT(snapshot, DETACH_SNAPSHOT_TRAFO_BINDINGS)
      ) || !same_list_pointers(
        roots,
        VECTOR_ELT(snapshot, DETACH_SNAPSHOT_ROOTS)
      )) {
    return FALSE;
  }
  SEXP postfix = VECTOR_ELT(snapshot, DETACH_SNAPSHOT_POSTFIX);
  if (TYPEOF(state->postfix) != LGLSXP || ALTREP(state->postfix) ||
      XLENGTH(state->postfix) != 1 || TYPEOF(postfix) != LGLSXP ||
      ALTREP(postfix) || XLENGTH(postfix) != 1 ||
      LOGICAL_ELT(state->postfix, 0) != LOGICAL_ELT(postfix, 0)) {
    return FALSE;
  }
  /* The allocation-capable reload immediately before this commit audit has
   * reauthenticated every generated wrapper and callback formal.  The rooted
   * pointer snapshots above now prove that none of those surfaces was
   * replaced during a later authentication.  Do not repeat structural
   * inspection here: pre-R-4.6 accessors and symbol installation can allocate. */
  return TRUE;
}

static SEXP allocate_translation(R_xlen_t count) {
  SEXP table = PROTECT(Rf_allocVector(VECSXP, 4));
  SET_VECTOR_ELT(table, 0, Rf_allocVector(STRSXP, count));
  SET_VECTOR_ELT(table, 1, Rf_allocVector(STRSXP, count));
  SET_VECTOR_ELT(table, 2, Rf_allocVector(INTSXP, count));
  SET_VECTOR_ELT(table, 3, Rf_allocVector(STRSXP, count));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 4));
  SET_STRING_ELT(names, 0, Rf_mkChar("id"));
  SET_STRING_ELT(names, 1, Rf_mkChar("original_id"));
  SET_STRING_ELT(names, 2, Rf_mkChar("owner_ps_index"));
  SET_STRING_ELT(names, 3, Rf_mkChar("owner_name"));
  Rf_setAttrib(table, R_NamesSymbol, names);
  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(classes, 0, Rf_mkChar("data.table"));
  SET_STRING_ELT(classes, 1, Rf_mkChar("data.frame"));
  Rf_setAttrib(table, R_ClassSymbol, classes);
  SEXP sorted = PROTECT(Rf_mkString("id"));
  Rf_setAttrib(table, Rf_install("sorted"), sorted);
  SEXP row_names = PROTECT(Rf_allocVector(INTSXP, count));
  for (R_xlen_t row = 0; row < count; ++row) {
    INTEGER(row_names)[row] = (int) row + 1;
  }
  Rf_setAttrib(table, R_RowNamesSymbol, row_names);
  UNPROTECT(5);
  return table;
}

static SEXP allocate_callback_sets(R_xlen_t count, const char *field) {
  SEXP sets = PROTECT(Rf_allocVector(VECSXP, count));
  SEXP set_names = PROTECT(Rf_allocVector(STRSXP, count));
  Rf_setAttrib(sets, R_NamesSymbol, set_names);
  SEXP field_name = PROTECT(Rf_mkString(field));
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP carrier = PROTECT(Rf_allocVector(VECSXP, 1));
    Rf_setAttrib(carrier, R_NamesSymbol, field_name);
    SET_VECTOR_ELT(sets, index, carrier);
    UNPROTECT(1);
  }
  UNPROTECT(3);
  return sets;
}

SEXP paradox_param_set_collection_detach_plan(SEXP private_environment,
    SEXP self, SEXP requested) {
  if (R_VERSION < R_Version(4, 6, 0)) {
    (void) private_environment;
    (void) self;
    (void) requested;
    return R_NilValue;
  }
  /* Intern every symbol before the first source observation.  The final live
   * reread receives these symbols directly and never calls Rf_install(). */
  const detach_symbols_t symbols = install_detach_symbols();
  PROTECT(private_environment);
  PROTECT(self);
  PROTECT(requested);
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, DETACH_ROOT_COUNT));
  SET_VECTOR_ELT(roots, DETACH_ROOT_SELF, self);
  SET_VECTOR_ELT(roots, DETACH_ROOT_PRIVATE, private_environment);
  SET_VECTOR_ELT(roots, DETACH_ROOT_REQUESTED, requested);

  SEXP sets = paradox_domain_local_value(private_environment, ".sets");
  const R_xlen_t child_count = TYPEOF(sets) == VECSXP && !ALTREP(sets)
    ? XLENGTH(sets)
    : 0;
  SEXP child_enclosures = PROTECT(Rf_allocVector(VECSXP, child_count));
  SEXP child_privates = PROTECT(Rf_allocVector(VECSXP, child_count));
  SEXP child_values = PROTECT(Rf_allocVector(VECSXP, child_count));
  SEXP constraints = PROTECT(Rf_allocVector(VECSXP, child_count));
  SEXP trafos = PROTECT(Rf_allocVector(VECSXP, child_count));
  SEXP constraint_bindings = PROTECT(Rf_allocVector(VECSXP, child_count));
  SEXP trafo_bindings = PROTECT(Rf_allocVector(VECSXP, child_count));

  detach_state_t initial;
  if (!load_state(
      private_environment,
      self,
      requested,
      roots,
      child_enclosures,
      child_privates,
      child_values,
      constraints,
      trafos,
      constraint_bindings,
      trafo_bindings,
      &initial
    ) || initial.selected_count > INT_MAX ||
      initial.constraint_count > INT_MAX || initial.trafo_count > INT_MAX ||
      !state_stable(
        private_environment,
        self,
        requested,
        roots,
        child_enclosures,
        child_privates,
        child_values,
        constraints,
        trafos,
        constraint_bindings,
        trafo_bindings,
        &initial
      )) {
    UNPROTECT(11);
    return R_NilValue;
  }

  SEXP snapshot = PROTECT(Rf_allocVector(
    VECSXP,
    DETACH_SNAPSHOT_COUNT
  ));
  SET_VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_REQUESTED,
    shallow_vector_copy(requested)
  );
  SET_VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_SET_NAMES,
    shallow_vector_copy(initial.set_names)
  );
  SET_VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_IDS,
    shallow_vector_copy(initial.ids)
  );
  SET_VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_ORIGINAL_IDS,
    shallow_vector_copy(initial.original_ids)
  );
  SET_VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_OWNER_INDICES,
    shallow_vector_copy(initial.owner_indices)
  );
  SET_VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_OWNER_NAMES,
    shallow_vector_copy(initial.owner_names)
  );
  SET_VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_CHILDREN,
    shallow_vector_copy(initial.sets)
  );
  SET_VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_CHILD_ENCLOSURES,
    shallow_vector_copy(child_enclosures)
  );
  SET_VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_CHILD_PRIVATES,
    shallow_vector_copy(child_privates)
  );
  SET_VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_CHILD_VALUES,
    shallow_vector_copy(child_values)
  );
  SET_VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_CONSTRAINTS,
    shallow_vector_copy(constraints)
  );
  SET_VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_TRAFOS,
    shallow_vector_copy(trafos)
  );
  SET_VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_CONSTRAINT_BINDINGS,
    shallow_vector_copy(constraint_bindings)
  );
  SET_VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_TRAFO_BINDINGS,
    shallow_vector_copy(trafo_bindings)
  );
  SET_VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_ROOTS,
    shallow_vector_copy(roots)
  );
  SET_VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_POSTFIX,
    Rf_ScalarLogical(LOGICAL_ELT(initial.postfix, 0))
  );
  if (!state_stable(
        private_environment,
        self,
        requested,
        roots,
        child_enclosures,
        child_privates,
        child_values,
        constraints,
        trafos,
        constraint_bindings,
        trafo_bindings,
        &initial
      ) || !snapshot_matches(
        snapshot,
        roots,
        requested,
        child_enclosures,
        child_privates,
        child_values,
        constraints,
        trafos,
        constraint_bindings,
        trafo_bindings,
        &initial
      )) {
    UNPROTECT(12);
    return R_NilValue;
  }

  SEXP selected_rows = PROTECT(Rf_allocVector(
    LGLSXP,
    initial.row_count
  ));
  SEXP selected_owners = PROTECT(Rf_allocVector(
    LGLSXP,
    initial.child_count
  ));
  SEXP snapshot_requested = VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_REQUESTED
  );
  SEXP snapshot_ids = VECTOR_ELT(snapshot, DETACH_SNAPSHOT_IDS);
  SEXP snapshot_owner_indices = VECTOR_ELT(
    snapshot,
    DETACH_SNAPSHOT_OWNER_INDICES
  );
  R_xlen_t selected_count = 0;
  for (R_xlen_t owner = 0; owner < initial.child_count; ++owner) {
    LOGICAL(selected_owners)[owner] = FALSE;
  }
  for (R_xlen_t row = 0; row < initial.row_count; ++row) {
    const int selected = requested_contains(
      snapshot_requested,
      STRING_ELT(snapshot_ids, row)
    );
    LOGICAL(selected_rows)[row] = selected;
    if (selected) {
      ++selected_count;
      const int owner = INTEGER_ELT(snapshot_owner_indices, row);
      if (owner <= 0 || (R_xlen_t) owner > initial.child_count) {
        UNPROTECT(14);
        return R_NilValue;
      }
      LOGICAL(selected_owners)[(R_xlen_t) owner - 1] = TRUE;
    }
  }
  if (selected_count != initial.selected_count) {
    UNPROTECT(14);
    return R_NilValue;
  }

  SEXP plan = PROTECT(Rf_allocVector(VECSXP, DETACH_PLAN_COUNT));
  SEXP plan_names = PROTECT(Rf_allocVector(STRSXP, DETACH_PLAN_COUNT));
  static const char *const names[] = {
    "translation", "constraint_indices", "constraint_sets",
    "trafo_indices", "trafo_sets", "postfix"
  };
  for (R_xlen_t index = 0; index < DETACH_PLAN_COUNT; ++index) {
    SET_STRING_ELT(plan_names, index, Rf_mkChar(names[index]));
  }
  Rf_setAttrib(plan, R_NamesSymbol, plan_names);
  SEXP translation = PROTECT(allocate_translation(initial.selected_count));
  SEXP constraint_indices = PROTECT(Rf_allocVector(
    INTSXP,
    initial.constraint_count
  ));
  SEXP constraint_sets = PROTECT(allocate_callback_sets(
    initial.constraint_count,
    "constraint"
  ));
  SEXP trafo_indices = PROTECT(Rf_allocVector(INTSXP, initial.trafo_count));
  SEXP trafo_sets = PROTECT(allocate_callback_sets(
    initial.trafo_count,
    "extra_trafo"
  ));
  SEXP postfix = PROTECT(Rf_allocVector(LGLSXP, 1));
  SET_VECTOR_ELT(plan, DETACH_PLAN_TRANSLATION, translation);
  SET_VECTOR_ELT(plan, DETACH_PLAN_CONSTRAINT_INDICES, constraint_indices);
  SET_VECTOR_ELT(plan, DETACH_PLAN_CONSTRAINT_SETS, constraint_sets);
  SET_VECTOR_ELT(plan, DETACH_PLAN_TRAFO_INDICES, trafo_indices);
  SET_VECTOR_ELT(plan, DETACH_PLAN_TRAFO_SETS, trafo_sets);
  SET_VECTOR_ELT(plan, DETACH_PLAN_POSTFIX, postfix);

  detach_state_t final;
  if (!load_state(
      private_environment,
      self,
      requested,
      roots,
      child_enclosures,
      child_privates,
      child_values,
      constraints,
      trafos,
      constraint_bindings,
      trafo_bindings,
      &final
    ) || final.child_count != initial.child_count ||
      final.selected_count != initial.selected_count ||
      final.constraint_count != initial.constraint_count ||
      final.trafo_count != initial.trafo_count || !state_stable(
        private_environment,
        self,
        requested,
        roots,
        child_enclosures,
        child_privates,
        child_values,
        constraints,
        trafos,
        constraint_bindings,
        trafo_bindings,
        &final
      ) || !live_bindings_match(
        private_environment,
        self,
        roots,
        child_enclosures,
        child_privates,
        child_values,
        constraints,
        trafos,
        constraint_bindings,
        trafo_bindings,
        selected_owners,
        &symbols,
        &final
      ) || !snapshot_matches(
        snapshot,
        roots,
        requested,
        child_enclosures,
        child_privates,
        child_values,
        constraints,
        trafos,
        constraint_bindings,
        trafo_bindings,
        &final
      )) {
    UNPROTECT(22);
    return R_NilValue;
  }

  SEXP output_ids = VECTOR_ELT(translation, 0);
  SEXP output_original_ids = VECTOR_ELT(translation, 1);
  SEXP output_owners = VECTOR_ELT(translation, 2);
  SEXP output_owner_names = VECTOR_ELT(translation, 3);
  R_xlen_t output = 0;
  for (R_xlen_t row = 0; row < final.row_count; ++row) {
    if (LOGICAL_ELT(selected_rows, row)) {
      SET_STRING_ELT(output_ids, output, STRING_ELT(final.ids, row));
      SET_STRING_ELT(
        output_original_ids,
        output,
        STRING_ELT(final.original_ids, row)
      );
      INTEGER(output_owners)[output] = INTEGER_ELT(final.owner_indices, row);
      SET_STRING_ELT(
        output_owner_names,
        output,
        STRING_ELT(final.owner_names, row)
      );
      ++output;
    }
  }

  R_xlen_t constraint_output = 0;
  R_xlen_t trafo_output = 0;
  SEXP constraint_set_names = PROTECT(Rf_getAttrib(
    constraint_sets,
    R_NamesSymbol
  ));
  SEXP trafo_set_names = PROTECT(Rf_getAttrib(
    trafo_sets,
    R_NamesSymbol
  ));
  for (R_xlen_t index = 0; index < final.child_count; ++index) {
    if (!LOGICAL_ELT(selected_owners, index)) {
      continue;
    }
    SEXP constraint = VECTOR_ELT(constraints, index);
    if (constraint != R_NilValue) {
      INTEGER(constraint_indices)[constraint_output] = (int) index + 1;
      SET_VECTOR_ELT(
        VECTOR_ELT(constraint_sets, constraint_output),
        0,
        constraint
      );
      SET_STRING_ELT(
        constraint_set_names,
        constraint_output,
        STRING_ELT(final.set_names, index)
      );
      ++constraint_output;
    }
    SEXP trafo = VECTOR_ELT(trafos, index);
    if (trafo != R_NilValue) {
      INTEGER(trafo_indices)[trafo_output] = (int) index + 1;
      SET_VECTOR_ELT(VECTOR_ELT(trafo_sets, trafo_output), 0, trafo);
      SET_STRING_ELT(
        trafo_set_names,
        trafo_output,
        STRING_ELT(final.set_names, index)
      );
      ++trafo_output;
    }
  }
  LOGICAL(postfix)[0] = LOGICAL_ELT(final.postfix, 0);
  if (output != final.selected_count ||
      constraint_output != final.constraint_count ||
      trafo_output != final.trafo_count) {
    UNPROTECT(24);
    Rf_error("Internal error: collection callback snapshot changed size");
  }

  if (!snapshot_matches(
        snapshot,
        roots,
        requested,
        child_enclosures,
        child_privates,
        child_values,
        constraints,
        trafos,
        constraint_bindings,
        trafo_bindings,
        &final
      ) || !live_bindings_match(
        private_environment,
        self,
        roots,
        child_enclosures,
        child_privates,
        child_values,
        constraints,
        trafos,
        constraint_bindings,
        trafo_bindings,
        selected_owners,
        &symbols,
        &final
      )) {
    UNPROTECT(24);
    return R_NilValue;
  }

  UNPROTECT(24);
  return plan;
}
