#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Utils.h>

#include "paramset_domain_common.h"
#include "paramset_params_internal.h"
#include "r_api_compat.h"
#include "r_utils.h"

enum subset_state_field {
  SUBSET_PARAMS = 0,
  SUBSET_TAGS,
  SUBSET_TRAFOS,
  SUBSET_DEPENDENCIES,
  SUBSET_VALUES,
  SUBSET_EXTRA_TRAFO,
  SUBSET_STATE_FIELD_COUNT
};

enum subset_plan_field {
  SUBSET_MISSING_PARENTS = 0,
  SUBSET_STATE_TOKEN,
  SUBSET_PLAN_FIELD_COUNT
};

enum subset_child_root_slot {
  SUBSET_CHILD_PARAMS_FIRST = 0,
  SUBSET_CHILD_PARAMS_NAMES = PARADOX_DOMAIN_TAGS,
  SUBSET_CHILD_PARAMS_CLASSES,
  SUBSET_CHILD_TAG_IDS,
  SUBSET_CHILD_TAG_VALUES,
  SUBSET_CHILD_TAG_NAMES,
  SUBSET_CHILD_TAG_CLASSES,
  SUBSET_CHILD_TAG_SORTED,
  SUBSET_CHILD_TRAFO_IDS,
  SUBSET_CHILD_TRAFO_VALUES,
  SUBSET_CHILD_TRAFO_NAMES,
  SUBSET_CHILD_TRAFO_CLASSES,
  SUBSET_CHILD_TRAFO_SORTED,
  SUBSET_CHILD_DEPENDENCY_IDS,
  SUBSET_CHILD_DEPENDENCY_ON,
  SUBSET_CHILD_DEPENDENCY_CONDITIONS,
  SUBSET_CHILD_DEPENDENCY_NAMES,
  SUBSET_CHILD_DEPENDENCY_CLASSES,
  SUBSET_CHILD_VALUES,
  SUBSET_CHILD_VALUE_NAMES,
  SUBSET_CHILD_ROOT_COUNT
};

typedef struct {
  SEXPTYPE type;
  const int *integer_values;
  const double *real_values;
} subset_match_vector_t;

/* Its address is a process-local capability. A token is additionally tagged,
 * protects only state built by this translation unit, and is consumed by the
 * first successful adoption. */
static unsigned char subset_state_identity = 0;

static int subset_has_no_attributes(SEXP value) {
  return paradox_api_has_no_attributes(value);
}

static int root_table_children(SEXP roots, R_xlen_t first_column,
    R_xlen_t names_slot, R_xlen_t classes_slot, R_xlen_t sorted_slot,
    SEXP table, R_xlen_t column_count) {
  if (TYPEOF(table) != VECSXP || ALTREP(table) ||
      XLENGTH(table) != column_count) {
    return FALSE;
  }
  for (R_xlen_t column = 0; column < column_count; ++column) {
    SET_VECTOR_ELT(
      roots,
      first_column + column,
      VECTOR_ELT(table, column)
    );
  }
  SET_VECTOR_ELT(roots, names_slot, Rf_getAttrib(table, R_NamesSymbol));
  SET_VECTOR_ELT(roots, classes_slot, Rf_getAttrib(table, R_ClassSymbol));
  if (sorted_slot != R_XLEN_T_MAX) {
    SET_VECTOR_ELT(
      roots,
      sorted_slot,
      Rf_getAttrib(table, Rf_install("sorted"))
    );
  }
  return TRUE;
}

static SEXP rooted_table_shell(SEXP roots, R_xlen_t first_column,
    R_xlen_t names_slot, R_xlen_t classes_slot, R_xlen_t sorted_slot,
    R_xlen_t column_count) {
  SEXP result = PROTECT(Rf_allocVector(VECSXP, column_count));
  for (R_xlen_t column = 0; column < column_count; ++column) {
    SET_VECTOR_ELT(
      result,
      column,
      VECTOR_ELT(roots, first_column + column)
    );
  }
  Rf_setAttrib(result, R_NamesSymbol, VECTOR_ELT(roots, names_slot));
  Rf_setAttrib(result, R_ClassSymbol, VECTOR_ELT(roots, classes_slot));
  if (sorted_slot != R_XLEN_T_MAX) {
    Rf_setAttrib(
      result,
      Rf_install("sorted"),
      VECTOR_ELT(roots, sorted_slot)
    );
  }
  UNPROTECT(1);
  return result;
}

typedef enum {
  SUBSET_SELF_UNKNOWN = 0,
  SUBSET_SELF_PARAM_SET,
  SUBSET_SELF_COLLECTION
} subset_self_kind_t;

static subset_self_kind_t exact_subset_self(SEXP self,
    R_xlen_t *work_since_interrupt) {
  static const char *const param_set_classes[] = {"ParamSet", "R6"};
  static const char *const collection_classes[] = {
    "ParamSetCollection", "ParamSet", "R6"
  };
  if (TYPEOF(self) != ENVSXP) {
    return SUBSET_SELF_UNKNOWN;
  }
  SEXP classes = Rf_getAttrib(self, R_ClassSymbol);
  if (paradox_domain_exact_string_vector(
      classes,
      param_set_classes,
      2,
      work_since_interrupt
    )) {
    return SUBSET_SELF_PARAM_SET;
  }
  if (paradox_domain_exact_string_vector(
      classes,
      collection_classes,
      3,
      work_since_interrupt
    )) {
    return SUBSET_SELF_COLLECTION;
  }
  return SUBSET_SELF_UNKNOWN;
}

static int canonical_subspace_extra_trafo(SEXP self,
    SEXP private_environment, SEXP *value,
    R_xlen_t *work_since_interrupt) {
  if (!paradox_params_canonical_active_member(
      self,
      private_environment,
      "extra_trafo",
      ".__ParamSet__extra_trafo",
      "f",
      NULL,
      work_since_interrupt
    )) {
    return FALSE;
  }
  SEXP observed = paradox_domain_local_value(
    private_environment,
    ".extra_trafo"
  );
  if (observed == R_UnboundValue ||
      (observed != R_NilValue && !Rf_isFunction(observed))) {
    return FALSE;
  }
  *value = observed;
  return TRUE;
}

static subset_match_vector_t match_vector(SEXP value,
    R_xlen_t expected_size) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  if ((type != INTSXP && type != REALSXP) ||
      XLENGTH(value) != expected_size) {
    Rf_error("Internal error: unexpected result from R's matching primitive");
  }
  const subset_match_vector_t result = {
    type,
    type == INTSXP ? INTEGER_RO(value) : NULL,
    type == REALSXP ? REAL_RO(value) : NULL
  };
  return result;
}

static R_xlen_t match_at(const subset_match_vector_t *matches,
    R_xlen_t index) {
  if (matches->type == INTSXP) {
    const int value = matches->integer_values[index];
    return value == NA_INTEGER || value <= 0 ? 0 : (R_xlen_t) value;
  }
  const double value = matches->real_values[index];
  return ISNAN(value) || value <= 0.0 ? 0 : (R_xlen_t) value;
}

static int checked_add(R_xlen_t *total, R_xlen_t increment) {
  if (increment < 0 || *total > R_XLEN_T_MAX - increment) {
    return FALSE;
  }
  *total += increment;
  return TRUE;
}

static int checked_multiply(R_xlen_t left, R_xlen_t right,
    R_xlen_t *result) {
  if (left < 0 || right < 0 ||
      (left != 0 && right > R_XLEN_T_MAX / left)) {
    return FALSE;
  }
  *result = left * right;
  return TRUE;
}

static int group_rows(const subset_match_vector_t *owners,
    R_xlen_t input_size, R_xlen_t output_size, R_xlen_t *offsets,
    R_xlen_t *order, R_xlen_t *work_since_interrupt) {
  for (R_xlen_t row = 0; row <= output_size; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    offsets[row] = 0;
  }
  for (R_xlen_t input = 0; input < input_size; ++input) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t owner = match_at(owners, input);
    if (owner == 0 || owner > output_size) {
      return FALSE;
    }
    ++offsets[owner];
  }
  for (R_xlen_t row = 1; row <= output_size; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    offsets[row] += offsets[row - 1];
  }

  R_xlen_t *cursor = paradox_temporary_alloc(
    output_size,
    sizeof(*cursor)
  );
  for (R_xlen_t row = 0; row < output_size; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    cursor[row] = offsets[row];
  }
  for (R_xlen_t input = 0; input < input_size; ++input) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t owner = match_at(owners, input);
    order[cursor[owner - 1]] = input;
    ++cursor[owner - 1];
  }
  return TRUE;
}

static int index_unique_rows(const subset_match_vector_t *owners,
    R_xlen_t input_size, R_xlen_t output_size, R_xlen_t *index,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t row = 0; row < output_size; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    index[row] = 0;
  }
  for (R_xlen_t input = 0; input < input_size; ++input) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t owner = match_at(owners, input);
    if (owner == 0 || owner > output_size || index[owner - 1] != 0) {
      return FALSE;
    }
    index[owner - 1] = input + 1;
  }
  return TRUE;
}

static int owners_are_grouped(const subset_match_vector_t *owners,
    R_xlen_t input_size, R_xlen_t output_size,
    R_xlen_t *work_since_interrupt) {
  int *seen = paradox_temporary_alloc(output_size, sizeof(*seen));
  for (R_xlen_t owner = 0; owner < output_size; ++owner) {
    paradox_domain_account_work(work_since_interrupt);
    seen[owner] = FALSE;
  }

  R_xlen_t previous = 0;
  for (R_xlen_t input = 0; input < input_size; ++input) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t owner = match_at(owners, input);
    if (owner == 0 || owner > output_size) {
      return FALSE;
    }
    if (owner != previous) {
      if (seen[owner - 1]) {
        return FALSE;
      }
      seen[owner - 1] = TRUE;
      previous = owner;
    }
  }
  return TRUE;
}

static void copy_element(SEXP destination, R_xlen_t destination_row,
    SEXP source, R_xlen_t source_row) {
  switch (TYPEOF(source)) {
  case STRSXP:
    SET_STRING_ELT(
      destination,
      destination_row,
      STRING_ELT(source, source_row)
    );
    return;
  case VECSXP:
    SET_VECTOR_ELT(
      destination,
      destination_row,
      VECTOR_ELT(source, source_row)
    );
    return;
  case REALSXP:
    SET_REAL_ELT(destination, destination_row, REAL_ELT(source, source_row));
    return;
  case INTSXP:
    SET_INTEGER_ELT(
      destination,
      destination_row,
      INTEGER_ELT(source, source_row)
    );
    return;
  case LGLSXP:
    SET_LOGICAL_ELT(
      destination,
      destination_row,
      LOGICAL_ELT(source, source_row)
    );
    return;
  case CPLXSXP:
    SET_COMPLEX_ELT(
      destination,
      destination_row,
      COMPLEX_ELT(source, source_row)
    );
    return;
  case RAWSXP:
    SET_RAW_ELT(destination, destination_row, RAW_ELT(source, source_row));
    return;
  default:
    Rf_error("Internal error: unsupported ParamSet subset column type");
  }
}

static SEXP fresh_character_vector(SEXP source) {
  const R_xlen_t size = XLENGTH(source);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    SET_STRING_ELT(result, index, STRING_ELT(source, index));
  }
  UNPROTECT(1);
  return result;
}

static SEXP subset_table(SEXP source, const R_xlen_t *positions,
    R_xlen_t row_count, int sorted_by_id,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t column_count = XLENGTH(source);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, column_count));
  for (R_xlen_t column = 0; column < column_count; ++column) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP input = VECTOR_ELT(source, column);
    SEXP output = PROTECT(Rf_allocVector(
      (SEXPTYPE) TYPEOF(input),
      row_count
    ));
    for (R_xlen_t row = 0; row < row_count; ++row) {
      paradox_domain_account_work(work_since_interrupt);
      copy_element(output, row, input, positions[row]);
    }
    SET_VECTOR_ELT(result, column, output);
    UNPROTECT(1);
  }

  SEXP names = PROTECT(fresh_character_vector(
    Rf_getAttrib(source, R_NamesSymbol)
  ));
  Rf_setAttrib(result, R_NamesSymbol, names);

  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(classes, 0, Rf_mkChar("data.table"));
  SET_STRING_ELT(classes, 1, Rf_mkChar("data.frame"));
  Rf_setAttrib(result, R_ClassSymbol, classes);

  SEXP row_names = PROTECT(Rf_allocVector(INTSXP, row_count == 0 ? 0 : 2));
  if (row_count != 0) {
    SET_INTEGER_ELT(row_names, 0, NA_INTEGER);
    SET_INTEGER_ELT(row_names, 1, -(int) row_count);
  }
  Rf_setAttrib(result, R_RowNamesSymbol, row_names);

  int protected_count = 4;
  if (sorted_by_id) {
    SEXP sorted = PROTECT(Rf_mkString("id"));
    ++protected_count;
    Rf_setAttrib(result, Rf_install("sorted"), sorted);
  }
  SEXP prepared = PROTECT(paradox_prepare_data_table(result, FALSE));
  ++protected_count;

  UNPROTECT(protected_count);
  return prepared;
}

static SEXP selected_key_table(SEXP source,
    const subset_match_vector_t *owners, R_xlen_t input_size,
    const R_xlen_t *selection_counts,
    R_xlen_t *work_since_interrupt) {
  R_xlen_t selected_size = 0;
  for (R_xlen_t input = 0; input < input_size;) {
    const R_xlen_t owner = match_at(owners, input);
    R_xlen_t end = input + 1;
    while (end < input_size && match_at(owners, end) == owner) {
      paradox_domain_account_work(work_since_interrupt);
      ++end;
    }
    R_xlen_t increment = 0;
    if (!checked_multiply(
        selection_counts[owner - 1],
        end - input,
        &increment
      ) || !checked_add(&selected_size, increment) ||
        selected_size > INT_MAX) {
      return R_NilValue;
    }
    input = end;
  }

  R_xlen_t *positions = paradox_temporary_alloc(
    selected_size,
    sizeof(*positions)
  );
  R_xlen_t output = 0;
  for (R_xlen_t input = 0; input < input_size;) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t owner = match_at(owners, input);
    R_xlen_t end = input + 1;
    while (end < input_size && match_at(owners, end) == owner) {
      paradox_domain_account_work(work_since_interrupt);
      ++end;
    }
    for (R_xlen_t repetition = 0;
        repetition < selection_counts[owner - 1]; ++repetition) {
      for (R_xlen_t member = input; member < end; ++member) {
        paradox_domain_account_work(work_since_interrupt);
        positions[output++] = member;
      }
    }
    input = end;
  }
  if (output != selected_size) {
    Rf_error("Internal error: inconsistent keyed subset size");
  }
  return subset_table(
    source,
    positions,
    selected_size,
    TRUE,
    work_since_interrupt
  );
}

static SEXP missing_parents(SEXP requested_ids,
    const R_xlen_t *requested_positions, R_xlen_t requested_size,
    const paradox_domain_dependencies_t *dependencies,
    const R_xlen_t *dependency_offsets, const R_xlen_t *dependency_order,
    R_xlen_t *work_since_interrupt) {
  SEXP selected_parent_sexp = PROTECT(Rf_match(
    requested_ids,
    dependencies->on,
    0
  ));
  const subset_match_vector_t selected_parents = match_vector(
    selected_parent_sexp,
    dependencies->row_count
  );

  R_xlen_t candidate_count = 0;
  for (R_xlen_t request = 0; request < requested_size; ++request) {
    const R_xlen_t owner = requested_positions[request];
    for (R_xlen_t position = dependency_offsets[owner];
        position < dependency_offsets[owner + 1]; ++position) {
      paradox_domain_account_work(work_since_interrupt);
      if (match_at(&selected_parents, dependency_order[position]) == 0 &&
          !checked_add(&candidate_count, 1)) {
        UNPROTECT(1);
        Rf_error("Unable to represent dependency subset");
      }
    }
  }

  SEXP candidates = PROTECT(Rf_allocVector(STRSXP, candidate_count));
  R_xlen_t candidate = 0;
  for (R_xlen_t request = 0; request < requested_size; ++request) {
    const R_xlen_t owner = requested_positions[request];
    for (R_xlen_t position = dependency_offsets[owner];
        position < dependency_offsets[owner + 1]; ++position) {
      paradox_domain_account_work(work_since_interrupt);
      const R_xlen_t dependency_row = dependency_order[position];
      if (match_at(&selected_parents, dependency_row) == 0) {
        SET_STRING_ELT(
          candidates,
          candidate++,
          STRING_ELT(dependencies->on, dependency_row)
        );
      }
    }
  }
  if (candidate != candidate_count) {
    UNPROTECT(2);
    Rf_error("Internal error: inconsistent dependency subset size");
  }

  SEXP first_match_sexp = PROTECT(Rf_match(candidates, candidates, 0));
  const subset_match_vector_t first_matches = match_vector(
    first_match_sexp,
    candidate_count
  );
  R_xlen_t unique_count = 0;
  for (R_xlen_t index = 0; index < candidate_count; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if (match_at(&first_matches, index) == index + 1) {
      ++unique_count;
    }
  }

  SEXP result = PROTECT(Rf_allocVector(STRSXP, unique_count));
  R_xlen_t output = 0;
  for (R_xlen_t index = 0; index < candidate_count; ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if (match_at(&first_matches, index) == index + 1) {
      SET_STRING_ELT(result, output++, STRING_ELT(candidates, index));
    }
  }

  UNPROTECT(4);
  return result;
}

static SEXP subset_values(const paradox_domain_values_t *values,
    const R_xlen_t *value_index, const R_xlen_t *requested_positions,
    R_xlen_t requested_size, R_xlen_t *work_since_interrupt) {
  R_xlen_t selected_size = 0;
  for (R_xlen_t request = 0; request < requested_size; ++request) {
    paradox_domain_account_work(work_since_interrupt);
    selected_size += value_index[requested_positions[request]] != 0;
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, selected_size));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, selected_size));
  R_xlen_t output = 0;
  for (R_xlen_t request = 0; request < requested_size; ++request) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t selected = value_index[requested_positions[request]];
    if (selected == 0) {
      continue;
    }
    if (output >= selected_size) {
      UNPROTECT(2);
      Rf_error("Internal error: ParamSet value subset exceeded capacity");
    }
    SET_VECTOR_ELT(result, output, VECTOR_ELT(values->values, selected - 1));
    SET_STRING_ELT(names, output, STRING_ELT(values->names, selected - 1));
    ++output;
  }
  if (output != selected_size) {
    UNPROTECT(2);
    Rf_error("Internal error: inconsistent ParamSet value subset size");
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
}

static SEXP new_plan(SEXP missing, SEXP token) {
  static const char *const plan_names[] = {
    "missing_parents", "state"
  };
  SEXP result = PROTECT(Rf_allocVector(VECSXP, SUBSET_PLAN_FIELD_COUNT));
  SET_VECTOR_ELT(result, SUBSET_MISSING_PARENTS, missing);
  SET_VECTOR_ELT(result, SUBSET_STATE_TOKEN, token);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, SUBSET_PLAN_FIELD_COUNT));
  for (R_xlen_t index = 0; index < SUBSET_PLAN_FIELD_COUNT; ++index) {
    SET_STRING_ELT(names, index, Rf_mkChar(plan_names[index]));
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
}

static SEXP param_set_subset_state_impl(SEXP private_environment, SEXP self,
    SEXP requested_ids, SEXP check_dependencies, SEXP expected_values) {
  R_xlen_t work_since_interrupt = 0;
  const subset_self_kind_t self_kind = exact_subset_self(
    self,
    &work_since_interrupt
  );
  /* data.table's join propagates arbitrary attributes and the request-side
   * CHARSXP encoding into each joined id column. These unusual inputs stay on
   * the R path instead of silently normalizing their externally visible form. */
  if (self_kind == SUBSET_SELF_UNKNOWN ||
      !paradox_domain_owns_private_environment(self, private_environment) ||
      TYPEOF(requested_ids) != STRSXP ||
      ALTREP(requested_ids) ||
      !subset_has_no_attributes(requested_ids) ||
      TYPEOF(check_dependencies) != LGLSXP ||
      ALTREP(check_dependencies) ||
      XLENGTH(check_dependencies) != 1) {
    return R_NilValue;
  }
  /* NA is an internal subspace mode. It emits an empty dependency table for
   * every one-row hand-off because public subspaces deliberately do not copy
   * dependency rows from their source into the detached children. */
  const int dependency_mode = LOGICAL_ELT(check_dependencies, 0);
  const int values_bound = expected_values != R_UnboundValue;
  if (values_bound && (dependency_mode != NA_LOGICAL ||
      self_kind != SUBSET_SELF_PARAM_SET || TYPEOF(expected_values) != VECSXP ||
      ALTREP(expected_values) || paradox_domain_local_value(
        private_environment,
        ".values"
      ) != expected_values)) {
    return R_NilValue;
  }
  if (self_kind == SUBSET_SELF_PARAM_SET &&
      (!paradox_params_canonical_active_member(
          self,
          private_environment,
          "deps",
          ".__ParamSet__deps",
          "v",
          NULL,
          &work_since_interrupt
        ) || !paradox_params_canonical_active_member(
          self,
          private_environment,
          "values",
          ".__ParamSet__values",
          "xs",
          NULL,
          &work_since_interrupt
        ) || !paradox_params_canonical_private_getter(
          self,
          private_environment
        ))) {
    return R_NilValue;
  }
  const R_xlen_t requested_size = XLENGTH(requested_ids);
  if (requested_size > INT_MAX) {
    return R_NilValue;
  }

  enum {
    SUBSET_SOURCE_PARAMS = 0,
    SUBSET_SOURCE_TAGS,
    SUBSET_SOURCE_TRAFOS,
    SUBSET_SOURCE_DEPENDENCIES,
    SUBSET_SOURCE_VALUES,
    SUBSET_SOURCE_EXTRA_TRAFO,
    SUBSET_SOURCE_COUNT
  };
  int protected_count = 0;
  SEXP source_state = PROTECT(Rf_allocVector(VECSXP, SUBSET_SOURCE_COUNT));
  ++protected_count;
  if (dependency_mode == NA_LOGICAL) {
    SEXP extra_trafo = R_UnboundValue;
    if (!canonical_subspace_extra_trafo(
        self,
        private_environment,
        &extra_trafo,
        &work_since_interrupt
      )) {
      UNPROTECT(protected_count);
      return R_NilValue;
    }
    SET_VECTOR_ELT(
      source_state,
      SUBSET_SOURCE_EXTRA_TRAFO,
      extra_trafo
    );
  }
  SEXP params_sexp = paradox_domain_local_value(private_environment, ".params");
  SET_VECTOR_ELT(source_state, SUBSET_SOURCE_PARAMS, params_sexp);
  SEXP tags_sexp = paradox_domain_local_value(private_environment, ".tags");
  SET_VECTOR_ELT(source_state, SUBSET_SOURCE_TAGS, tags_sexp);
  SEXP trafos_sexp = paradox_domain_local_value(private_environment, ".trafos");
  SET_VECTOR_ELT(source_state, SUBSET_SOURCE_TRAFOS, trafos_sexp);
  SEXP dependencies_sexp;
  if (self_kind == SUBSET_SELF_PARAM_SET) {
    dependencies_sexp = paradox_domain_local_value(
      private_environment,
      ".deps"
    );
  } else {
    /* Recompute through the authenticated callback-free native graph instead
     * of trusting a caller-provided table to become constructor state. The R
     * method has already observed its public `$deps` binding at the historical
     * point; this second native snapshot has no extension callbacks. */
    dependencies_sexp = PROTECT(paradox_param_set_collection_deps(
      private_environment,
      self
    ));
    ++protected_count;
    if (dependencies_sexp == R_NilValue) {
      UNPROTECT(protected_count);
      return R_NilValue;
    }
  }
  SET_VECTOR_ELT(
    source_state,
    SUBSET_SOURCE_DEPENDENCIES,
    dependencies_sexp
  );
  SEXP values_sexp;
  if (self_kind == SUBSET_SELF_PARAM_SET) {
    values_sexp = paradox_domain_local_value(private_environment, ".values");
  } else {
    /* The native collection values routine admits only an exact,
     * callback-free collection graph. Calling it directly avoids evaluating
     * a public extension binding merely to discover that the complete subset
     * must fall back. */
    values_sexp = PROTECT(paradox_param_set_collection_values(
      private_environment,
      self
    ));
    ++protected_count;
    if (values_sexp == R_NilValue) {
      UNPROTECT(protected_count);
      return R_NilValue;
    }
  }
  SET_VECTOR_ELT(source_state, SUBSET_SOURCE_VALUES, values_sexp);
  if (params_sexp == R_UnboundValue || tags_sexp == R_UnboundValue ||
      trafos_sexp == R_UnboundValue ||
      dependencies_sexp == R_UnboundValue || values_sexp == R_UnboundValue ||
      (values_bound && values_sexp != expected_values)) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }

  /* Install every exact child in a root before validators or matching can
   * allocate. A rooted table shell alone is insufficient if a finalizer
   * replaces one of its cells while a validator retains the old child. */
  SEXP child_roots = PROTECT(Rf_allocVector(
    VECSXP,
    SUBSET_CHILD_ROOT_COUNT
  ));
  ++protected_count;
  if (!root_table_children(
        child_roots,
        SUBSET_CHILD_PARAMS_FIRST,
        SUBSET_CHILD_PARAMS_NAMES,
        SUBSET_CHILD_PARAMS_CLASSES,
        R_XLEN_T_MAX,
        params_sexp,
        PARADOX_DOMAIN_TAGS
      ) || !root_table_children(
        child_roots,
        SUBSET_CHILD_TAG_IDS,
        SUBSET_CHILD_TAG_NAMES,
        SUBSET_CHILD_TAG_CLASSES,
        SUBSET_CHILD_TAG_SORTED,
        tags_sexp,
        2
      ) || !root_table_children(
        child_roots,
        SUBSET_CHILD_TRAFO_IDS,
        SUBSET_CHILD_TRAFO_NAMES,
        SUBSET_CHILD_TRAFO_CLASSES,
        SUBSET_CHILD_TRAFO_SORTED,
        trafos_sexp,
        2
      ) || !root_table_children(
        child_roots,
        SUBSET_CHILD_DEPENDENCY_IDS,
        SUBSET_CHILD_DEPENDENCY_NAMES,
        SUBSET_CHILD_DEPENDENCY_CLASSES,
        R_XLEN_T_MAX,
        dependencies_sexp,
        3
      ) || TYPEOF(values_sexp) != VECSXP || ALTREP(values_sexp)) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  SET_VECTOR_ELT(child_roots, SUBSET_CHILD_VALUES, values_sexp);
  SET_VECTOR_ELT(
    child_roots,
    SUBSET_CHILD_VALUE_NAMES,
    Rf_getAttrib(values_sexp, R_NamesSymbol)
  );

  /* Validate and assemble from private shells whose cells are the rooted
   * candidates above. Replacing a source table cell can no longer make later
   * matching observe a different column from the one validation inspected. */
  SEXP params_shell = PROTECT(rooted_table_shell(
    child_roots,
    SUBSET_CHILD_PARAMS_FIRST,
    SUBSET_CHILD_PARAMS_NAMES,
    SUBSET_CHILD_PARAMS_CLASSES,
    R_XLEN_T_MAX,
    PARADOX_DOMAIN_TAGS
  ));
  ++protected_count;
  SET_VECTOR_ELT(source_state, SUBSET_SOURCE_PARAMS, params_shell);
  params_sexp = params_shell;
  SEXP tags_shell = PROTECT(rooted_table_shell(
    child_roots,
    SUBSET_CHILD_TAG_IDS,
    SUBSET_CHILD_TAG_NAMES,
    SUBSET_CHILD_TAG_CLASSES,
    SUBSET_CHILD_TAG_SORTED,
    2
  ));
  ++protected_count;
  SET_VECTOR_ELT(source_state, SUBSET_SOURCE_TAGS, tags_shell);
  tags_sexp = tags_shell;
  SEXP trafos_shell = PROTECT(rooted_table_shell(
    child_roots,
    SUBSET_CHILD_TRAFO_IDS,
    SUBSET_CHILD_TRAFO_NAMES,
    SUBSET_CHILD_TRAFO_CLASSES,
    SUBSET_CHILD_TRAFO_SORTED,
    2
  ));
  ++protected_count;
  SET_VECTOR_ELT(source_state, SUBSET_SOURCE_TRAFOS, trafos_shell);
  trafos_sexp = trafos_shell;
  SEXP dependencies_shell = PROTECT(rooted_table_shell(
    child_roots,
    SUBSET_CHILD_DEPENDENCY_IDS,
    SUBSET_CHILD_DEPENDENCY_NAMES,
    SUBSET_CHILD_DEPENDENCY_CLASSES,
    R_XLEN_T_MAX,
    3
  ));
  ++protected_count;
  SET_VECTOR_ELT(
    source_state,
    SUBSET_SOURCE_DEPENDENCIES,
    dependencies_shell
  );
  dependencies_sexp = dependencies_shell;

  paradox_domain_params_t params;
  paradox_domain_tags_t tags;
  paradox_domain_trafos_t trafos;
  paradox_domain_dependencies_t dependencies;
  paradox_domain_values_t values;
  R_xlen_t unused_row = 0;
  if (!paradox_domain_validate_params(
        params_sexp,
        R_NilValue,
        TRUE,
        &params,
        &unused_row,
        &work_since_interrupt
      ) || !paradox_domain_validate_tags(
        tags_sexp,
        &tags,
        &work_since_interrupt
      ) || !paradox_domain_validate_trafos(
        trafos_sexp,
        &trafos,
        &work_since_interrupt
      ) || !paradox_domain_validate_dependencies(
        dependencies_sexp,
        &dependencies,
        &work_since_interrupt
      ) || !paradox_domain_validate_values(
        values_sexp,
        &values,
        &work_since_interrupt
      ) || params.row_count == R_XLEN_T_MAX ||
      params.row_count > INT_MAX) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  SEXP requested_match_sexp = PROTECT(Rf_match(
    params.ids,
    requested_ids,
    0
  ));
  ++protected_count;
  SEXP tag_match_sexp = PROTECT(Rf_match(params.ids, tags.ids, 0));
  ++protected_count;
  SEXP trafo_match_sexp = PROTECT(Rf_match(params.ids, trafos.ids, 0));
  ++protected_count;
  SEXP dependency_match_sexp = PROTECT(Rf_match(
    params.ids,
    dependencies.ids,
    0
  ));
  ++protected_count;
  SEXP value_match_sexp = PROTECT(Rf_match(params.ids, values.names, 0));
  ++protected_count;
  const subset_match_vector_t requested_matches = match_vector(
    requested_match_sexp,
    requested_size
  );
  const subset_match_vector_t tag_matches = match_vector(
    tag_match_sexp,
    tags.row_count
  );
  const subset_match_vector_t trafo_matches = match_vector(
    trafo_match_sexp,
    trafos.row_count
  );
  const subset_match_vector_t dependency_matches = match_vector(
    dependency_match_sexp,
    dependencies.row_count
  );
  const subset_match_vector_t value_matches = match_vector(
    value_match_sexp,
    values.size
  );

  R_xlen_t *requested_positions = paradox_temporary_alloc(
    requested_size,
    sizeof(*requested_positions)
  );
  R_xlen_t *selection_counts = paradox_temporary_alloc(
    params.row_count,
    sizeof(*selection_counts)
  );
  for (R_xlen_t row = 0; row < params.row_count; ++row) {
    paradox_domain_account_work(&work_since_interrupt);
    selection_counts[row] = 0;
  }
  for (R_xlen_t request = 0; request < requested_size; ++request) {
    paradox_domain_account_work(&work_since_interrupt);
    const R_xlen_t selected = match_at(&requested_matches, request);
    if (selected == 0 || selected > params.row_count ||
        STRING_ELT(requested_ids, request) !=
          STRING_ELT(params.ids, selected - 1)) {
      UNPROTECT(protected_count);
      return R_NilValue;
    }
    requested_positions[request] = selected - 1;
    ++selection_counts[selected - 1];
  }

  if (!owners_are_grouped(
      &tag_matches,
      tags.row_count,
      params.row_count,
      &work_since_interrupt
    ) || !owners_are_grouped(
      &trafo_matches,
      trafos.row_count,
      params.row_count,
      &work_since_interrupt
    )) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }

  R_xlen_t *dependency_offsets = paradox_temporary_alloc(
    params.row_count + 1,
    sizeof(*dependency_offsets)
  );
  R_xlen_t *dependency_order = paradox_temporary_alloc(
    dependencies.row_count,
    sizeof(*dependency_order)
  );
  R_xlen_t *value_index = paradox_temporary_alloc(
    params.row_count,
    sizeof(*value_index)
  );
  if (!group_rows(
      &dependency_matches,
      dependencies.row_count,
      params.row_count,
      dependency_offsets,
      dependency_order,
      &work_since_interrupt
    ) || !index_unique_rows(
      &trafo_matches,
      trafos.row_count,
      params.row_count,
      paradox_temporary_alloc(params.row_count, sizeof(R_xlen_t)),
      &work_since_interrupt
    ) || !index_unique_rows(
      &value_matches,
      values.size,
      params.row_count,
      value_index,
      &work_since_interrupt
    )) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }

  R_xlen_t dependency_output_size = 0;
  if (dependency_mode != NA_LOGICAL) {
    for (R_xlen_t request = 0; request < requested_size; ++request) {
      const R_xlen_t owner = requested_positions[request];
      if (!checked_add(
          &dependency_output_size,
          dependency_offsets[owner + 1] - dependency_offsets[owner]
        ) || dependency_output_size > INT_MAX) {
        UNPROTECT(protected_count);
        return R_NilValue;
      }
    }
  }

  SEXP missing = PROTECT(dependency_mode == TRUE
    ? missing_parents(
      requested_ids,
      requested_positions,
      requested_size,
      &dependencies,
      dependency_offsets,
      dependency_order,
      &work_since_interrupt
    )
    : Rf_allocVector(STRSXP, 0));
  ++protected_count;
  if (XLENGTH(missing) != 0) {
    SEXP plan = PROTECT(new_plan(missing, R_NilValue));
    ++protected_count;
    UNPROTECT(protected_count);
    return plan;
  }

  R_xlen_t *dependency_positions = paradox_temporary_alloc(
    dependency_output_size,
    sizeof(*dependency_positions)
  );
  R_xlen_t dependency_output = 0;
  if (dependency_mode != NA_LOGICAL) {
    for (R_xlen_t request = 0; request < requested_size; ++request) {
      const R_xlen_t owner = requested_positions[request];
      for (R_xlen_t position = dependency_offsets[owner];
          position < dependency_offsets[owner + 1]; ++position) {
        paradox_domain_account_work(&work_since_interrupt);
        dependency_positions[dependency_output++] =
          dependency_order[position];
      }
    }
  }
  if (dependency_output != dependency_output_size) {
    UNPROTECT(protected_count);
    Rf_error("Internal error: inconsistent dependency subset size");
  }

  SEXP selected_params = PROTECT(subset_table(
    params.table,
    requested_positions,
    requested_size,
    FALSE,
    &work_since_interrupt
  ));
  ++protected_count;
  /* A subspace token always carries exactly one detached row. Reuse the
   * process-authenticated data.table layout so its eventual R6 adopter need
   * not sort that singleton again. The discarded whole-request preflight and
   * ordinary subset tokens deliberately remain untouched: duplicate or
   * reordered IDs need a real composite order. */
  if (dependency_mode == NA_LOGICAL && requested_size == 1) {
    (void) paradox_param_set_attach_singleton_index(selected_params);
  }
  SEXP selected_tags = PROTECT(selected_key_table(
    tags_sexp,
    &tag_matches,
    tags.row_count,
    selection_counts,
    &work_since_interrupt
  ));
  ++protected_count;
  SEXP selected_trafos = PROTECT(selected_key_table(
    trafos_sexp,
    &trafo_matches,
    trafos.row_count,
    selection_counts,
    &work_since_interrupt
  ));
  ++protected_count;
  if (selected_tags == R_NilValue || selected_trafos == R_NilValue) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  SEXP selected_dependencies = PROTECT(subset_table(
    dependencies_sexp,
    dependency_positions,
    dependency_output_size,
    FALSE,
    &work_since_interrupt
  ));
  ++protected_count;
  SEXP selected_values = PROTECT(subset_values(
    &values,
    value_index,
    requested_positions,
    requested_size,
    &work_since_interrupt
  ));
  ++protected_count;

  SEXP payload = PROTECT(Rf_allocVector(VECSXP, SUBSET_STATE_FIELD_COUNT));
  ++protected_count;
  SET_VECTOR_ELT(payload, SUBSET_PARAMS, selected_params);
  SET_VECTOR_ELT(payload, SUBSET_TAGS, selected_tags);
  SET_VECTOR_ELT(payload, SUBSET_TRAFOS, selected_trafos);
  SET_VECTOR_ELT(payload, SUBSET_DEPENDENCIES, selected_dependencies);
  SET_VECTOR_ELT(payload, SUBSET_VALUES, selected_values);
  SET_VECTOR_ELT(
    payload,
    SUBSET_EXTRA_TRAFO,
    dependency_mode == NA_LOGICAL
      ? VECTOR_ELT(source_state, SUBSET_SOURCE_EXTRA_TRAFO)
      : R_NilValue
  );
  SEXP token = PROTECT(R_MakeExternalPtr(
    (void *) &subset_state_identity,
    Rf_install("paradox_subset_state_v1"),
    payload
  ));
  ++protected_count;
  SEXP plan = PROTECT(new_plan(missing, token));
  ++protected_count;

  if (dependency_mode == NA_LOGICAL) {
    SEXP extra_trafo = R_UnboundValue;
    if (!canonical_subspace_extra_trafo(
        self,
        private_environment,
        &extra_trafo,
        &work_since_interrupt
      ) || extra_trafo != VECTOR_ELT(
        source_state,
        SUBSET_SOURCE_EXTRA_TRAFO
      )) {
      UNPROTECT(protected_count);
      return R_NilValue;
    }
  }
  if (values_bound && paradox_domain_local_value(
      private_environment,
      ".values"
    ) != expected_values) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }

  UNPROTECT(protected_count);
  return plan;
}

SEXP paradox_param_set_subset_state(SEXP private_environment, SEXP self,
    SEXP requested_ids, SEXP check_dependencies) {
  return param_set_subset_state_impl(
    private_environment,
    self,
    requested_ids,
    check_dependencies,
    R_UnboundValue
  );
}

SEXP paradox_param_set_subspace_state(SEXP private_environment, SEXP self,
    SEXP requested_ids, SEXP check_dependencies, SEXP expected_values) {
  return param_set_subset_state_impl(
    private_environment,
    self,
    requested_ids,
    check_dependencies,
    expected_values
  );
}

static SEXP subset_token_payload(SEXP token) {
  if (TYPEOF(token) != EXTPTRSXP ||
      R_ExternalPtrAddr(token) != (void *) &subset_state_identity ||
      R_ExternalPtrTag(token) != Rf_install("paradox_subset_state_v1")) {
    return R_NilValue;
  }
  SEXP payload = R_ExternalPtrProtected(token);
  if (TYPEOF(payload) != VECSXP ||
      XLENGTH(payload) != SUBSET_STATE_FIELD_COUNT) {
    return R_NilValue;
  }
  return payload;
}

enum bulk_subspace_source_slot {
  BULK_SOURCE_PARAMS = 0,
  BULK_SOURCE_TAGS,
  BULK_SOURCE_TRAFOS,
  BULK_SOURCE_DEPENDENCIES,
  BULK_SOURCE_VALUES,
  BULK_SOURCE_EXTRA_TRAFO,
  BULK_SOURCE_COUNT
};

static int bulk_occurrence_layout(const subset_match_vector_t *owners,
    R_xlen_t request_count, R_xlen_t *occurrence_counts,
    R_xlen_t *occurrence_ranks, R_xlen_t *work_since_interrupt) {
  for (R_xlen_t request = 0; request < request_count; ++request) {
    paradox_domain_account_work(work_since_interrupt);
    occurrence_counts[request] = 0;
  }
  for (R_xlen_t request = 0; request < request_count; ++request) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t owner = match_at(owners, request);
    if (owner == 0 || owner > request_count) {
      return FALSE;
    }
    occurrence_ranks[request] = occurrence_counts[owner - 1];
    ++occurrence_counts[owner - 1];
  }
  for (R_xlen_t request = 0; request < request_count; ++request) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t owner = match_at(owners, request);
    occurrence_counts[request] = occurrence_counts[owner - 1];
  }
  return TRUE;
}

static int bulk_key_table_layout(const subset_match_vector_t *owners,
    R_xlen_t row_count, R_xlen_t request_count,
    const R_xlen_t *occurrence_counts, R_xlen_t *starts,
    R_xlen_t *rows_per_occurrence, R_xlen_t *work_since_interrupt) {
  for (R_xlen_t request = 0; request < request_count; ++request) {
    starts[request] = R_XLEN_T_MAX;
    rows_per_occurrence[request] = 0;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t owner = match_at(owners, row);
    if (owner == 0 || owner > request_count) {
      return FALSE;
    }
    const R_xlen_t index = owner - 1;
    if (starts[index] == R_XLEN_T_MAX) {
      starts[index] = row;
    } else if (row != starts[index] + rows_per_occurrence[index]) {
      return FALSE;
    }
    ++rows_per_occurrence[index];
  }
  for (R_xlen_t request = 0; request < request_count; ++request) {
    if (occurrence_counts[request] == 0) {
      continue;
    }
    if (rows_per_occurrence[request] % occurrence_counts[request] != 0) {
      return FALSE;
    }
    rows_per_occurrence[request] /= occurrence_counts[request];
  }
  return TRUE;
}

static SEXP singleton_composite_value(SEXP values,
    R_xlen_t selected) {
  if (TYPEOF(values) != VECSXP || ALTREP(values)) {
    return R_NilValue;
  }
  SEXP names = PROTECT(Rf_getAttrib(values, R_NamesSymbol));
  if (TYPEOF(names) != STRSXP || ALTREP(names) ||
      XLENGTH(names) != XLENGTH(values) ||
      (selected != R_XLEN_T_MAX && selected >= XLENGTH(values))) {
    UNPROTECT(1);
    return R_NilValue;
  }
  const R_xlen_t size = selected == R_XLEN_T_MAX ? 0 : 1;
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  SEXP result_names = PROTECT(Rf_allocVector(STRSXP, size));
  if (size != 0) {
    SET_VECTOR_ELT(result, 0, VECTOR_ELT(values, selected));
    SET_STRING_ELT(result_names, 0, STRING_ELT(names, selected));
  }
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  UNPROTECT(3);
  return result;
}

static int bulk_singleton_state_admitted(SEXP params_sexp, SEXP tags_sexp,
    SEXP trafos_sexp, SEXP dependencies_sexp, SEXP values_sexp, SEXP id,
    R_xlen_t *work_since_interrupt) {
  paradox_domain_params_t params;
  paradox_domain_tags_t tags;
  paradox_domain_trafos_t trafos;
  paradox_domain_dependencies_t dependencies;
  paradox_domain_values_t values;
  R_xlen_t unused_row = 0;
  if (!paradox_domain_validate_params(
        params_sexp,
        R_NilValue,
        TRUE,
        &params,
        &unused_row,
        work_since_interrupt
      ) || !paradox_domain_validate_tags(
        tags_sexp,
        &tags,
        work_since_interrupt
      ) || !paradox_domain_validate_trafos(
        trafos_sexp,
        &trafos,
        work_since_interrupt
      ) || !paradox_domain_validate_dependencies(
        dependencies_sexp,
        &dependencies,
        work_since_interrupt
      ) || !paradox_domain_validate_values(
        values_sexp,
        &values,
        work_since_interrupt
      ) || params.row_count != 1 || STRING_ELT(params.ids, 0) != id ||
      trafos.row_count > 1 || dependencies.row_count != 0 ||
      values.size > 1) {
    return FALSE;
  }
  for (R_xlen_t row = 0; row < tags.row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    if (!paradox_domain_strings_equal(STRING_ELT(tags.ids, row), id)) {
      return FALSE;
    }
  }
  for (R_xlen_t row = 0; row < trafos.row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    if (!paradox_domain_strings_equal(STRING_ELT(trafos.ids, row), id)) {
      return FALSE;
    }
  }
  return values.size == 0 || paradox_domain_strings_equal(
    STRING_ELT(values.names, 0),
    id
  );
}

enum bulk_source_attribute_slot {
  BULK_PARAMS_ROW_NAMES = 0,
  BULK_PARAMS_SELF_REFERENCE,
  BULK_PARAMS_INDEX,
  BULK_TAGS_ROW_NAMES,
  BULK_TAGS_SELF_REFERENCE,
  BULK_TAGS_INDEX,
  BULK_TRAFOS_ROW_NAMES,
  BULK_TRAFOS_SELF_REFERENCE,
  BULK_TRAFOS_INDEX,
  BULK_DEPENDENCIES_ROW_NAMES,
  BULK_DEPENDENCIES_SELF_REFERENCE,
  BULK_DEPENDENCIES_INDEX,
  BULK_VALUES_CLASS,
  BULK_SOURCE_ATTRIBUTE_COUNT
};

static void snapshot_bulk_table_attributes(SEXP snapshot,
    R_xlen_t first, SEXP table) {
  SET_VECTOR_ELT(snapshot, first, Rf_getAttrib(table, R_RowNamesSymbol));
  SET_VECTOR_ELT(
    snapshot,
    first + 1,
    Rf_getAttrib(table, Rf_install(".internal.selfref"))
  );
  SET_VECTOR_ELT(
    snapshot,
    first + 2,
    Rf_getAttrib(table, Rf_install("index"))
  );
}

static int bulk_table_children_match(SEXP roots, R_xlen_t first_column,
    R_xlen_t names_slot, R_xlen_t classes_slot, R_xlen_t sorted_slot,
    SEXP table, R_xlen_t column_count) {
  if (TYPEOF(table) != VECSXP || ALTREP(table) ||
      XLENGTH(table) != column_count) {
    return FALSE;
  }
  for (R_xlen_t column = 0; column < column_count; ++column) {
    if (VECTOR_ELT(table, column) !=
        VECTOR_ELT(roots, first_column + column)) {
      return FALSE;
    }
  }
  return Rf_getAttrib(table, R_NamesSymbol) ==
      VECTOR_ELT(roots, names_slot) &&
    Rf_getAttrib(table, R_ClassSymbol) ==
      VECTOR_ELT(roots, classes_slot) &&
    (sorted_slot == R_XLEN_T_MAX || Rf_getAttrib(
      table,
      Rf_install("sorted")
    ) == VECTOR_ELT(roots, sorted_slot));
}

static int bulk_table_attributes_match(SEXP snapshot, R_xlen_t first,
    SEXP table) {
  return Rf_getAttrib(table, Rf_install(".internal.selfref")) ==
      VECTOR_ELT(snapshot, first + 1) &&
    Rf_getAttrib(table, Rf_install("index")) ==
      VECTOR_ELT(snapshot, first + 2);
}

static int bulk_table_row_names_match(SEXP snapshot, R_xlen_t first,
    SEXP table) {
  SEXP expected = VECTOR_ELT(snapshot, first);
  SEXP observed = Rf_getAttrib(table, R_RowNamesSymbol);
  if (observed == R_NilValue || expected == R_NilValue) {
    return observed == expected;
  }
  if (TYPEOF(observed) != TYPEOF(expected) ||
      XLENGTH(observed) != XLENGTH(expected)) {
    return FALSE;
  }
  if (TYPEOF(observed) == INTSXP) {
    for (R_xlen_t index = 0; index < XLENGTH(observed); ++index) {
      if (INTEGER_ELT(observed, index) != INTEGER_ELT(expected, index)) {
        return FALSE;
      }
    }
    return TRUE;
  }
  if (TYPEOF(observed) == STRSXP) {
    for (R_xlen_t index = 0; index < XLENGTH(observed); ++index) {
      if (STRING_ELT(observed, index) != STRING_ELT(expected, index)) {
        return FALSE;
      }
    }
    return TRUE;
  }
  return observed == expected;
}

static int bulk_source_children_match(SEXP source, SEXP roots,
    SEXP value_cells, SEXP attributes,
    R_xlen_t *work_since_interrupt) {
  SEXP params = VECTOR_ELT(source, BULK_SOURCE_PARAMS);
  SEXP tags = VECTOR_ELT(source, BULK_SOURCE_TAGS);
  SEXP trafos = VECTOR_ELT(source, BULK_SOURCE_TRAFOS);
  SEXP dependencies = VECTOR_ELT(source, BULK_SOURCE_DEPENDENCIES);
  SEXP values = VECTOR_ELT(source, BULK_SOURCE_VALUES);
  if (!bulk_table_children_match(
        roots,
        SUBSET_CHILD_PARAMS_FIRST,
        SUBSET_CHILD_PARAMS_NAMES,
        SUBSET_CHILD_PARAMS_CLASSES,
        R_XLEN_T_MAX,
        params,
        PARADOX_DOMAIN_TAGS
      ) || !bulk_table_children_match(
        roots,
        SUBSET_CHILD_TAG_IDS,
        SUBSET_CHILD_TAG_NAMES,
        SUBSET_CHILD_TAG_CLASSES,
        SUBSET_CHILD_TAG_SORTED,
        tags,
        2
      ) || !bulk_table_children_match(
        roots,
        SUBSET_CHILD_TRAFO_IDS,
        SUBSET_CHILD_TRAFO_NAMES,
        SUBSET_CHILD_TRAFO_CLASSES,
        SUBSET_CHILD_TRAFO_SORTED,
        trafos,
        2
      ) || !bulk_table_children_match(
        roots,
        SUBSET_CHILD_DEPENDENCY_IDS,
        SUBSET_CHILD_DEPENDENCY_NAMES,
        SUBSET_CHILD_DEPENDENCY_CLASSES,
        R_XLEN_T_MAX,
        dependencies,
        3
      ) || !bulk_table_attributes_match(
        attributes,
        BULK_PARAMS_ROW_NAMES,
        params
      ) || !bulk_table_attributes_match(
        attributes,
        BULK_TAGS_ROW_NAMES,
        tags
      ) || !bulk_table_attributes_match(
        attributes,
        BULK_TRAFOS_ROW_NAMES,
        trafos
      ) || !bulk_table_attributes_match(
        attributes,
        BULK_DEPENDENCIES_ROW_NAMES,
        dependencies
      ) || TYPEOF(values) != VECSXP || ALTREP(values) ||
      XLENGTH(values) != XLENGTH(value_cells) ||
      Rf_getAttrib(values, R_NamesSymbol) !=
        VECTOR_ELT(roots, SUBSET_CHILD_VALUE_NAMES) ||
      Rf_getAttrib(values, R_ClassSymbol) !=
        VECTOR_ELT(attributes, BULK_VALUES_CLASS)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(values); ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if (VECTOR_ELT(values, index) != VECTOR_ELT(value_cells, index)) {
      return FALSE;
    }
  }
  return TRUE;
}

static int bulk_subspace_source_matches(SEXP private_environment, SEXP self,
    SEXP source, SEXP source_children, SEXP value_cells,
    SEXP source_attributes, SEXP requested_ids, SEXP requested_snapshot,
    SEXP expected_values, R_xlen_t *work_since_interrupt) {
  if (TYPEOF(source) != VECSXP || XLENGTH(source) != BULK_SOURCE_COUNT ||
      TYPEOF(requested_ids) != STRSXP || ALTREP(requested_ids) ||
      !subset_has_no_attributes(requested_ids) ||
      TYPEOF(requested_snapshot) != STRSXP || ALTREP(requested_snapshot) ||
      !subset_has_no_attributes(requested_snapshot) ||
      XLENGTH(requested_ids) != XLENGTH(requested_snapshot) ||
      exact_subset_self(self, work_since_interrupt) != SUBSET_SELF_PARAM_SET ||
      !paradox_domain_owns_private_environment(self, private_environment) ||
      !paradox_params_canonical_active_member(
        self,
        private_environment,
        "deps",
        ".__ParamSet__deps",
        "v",
        NULL,
        work_since_interrupt
      ) || !paradox_params_canonical_active_member(
        self,
        private_environment,
        "values",
        ".__ParamSet__values",
        "xs",
        NULL,
        work_since_interrupt
      ) || !paradox_params_canonical_private_getter(
        self,
        private_environment
      )) {
    return FALSE;
  }
  SEXP extra_trafo = R_UnboundValue;
  if (!canonical_subspace_extra_trafo(
      self,
      private_environment,
      &extra_trafo,
      work_since_interrupt
    ) || extra_trafo != VECTOR_ELT(source, BULK_SOURCE_EXTRA_TRAFO)) {
    return FALSE;
  }
  if (!bulk_table_row_names_match(
        source_attributes,
        BULK_PARAMS_ROW_NAMES,
        VECTOR_ELT(source, BULK_SOURCE_PARAMS)
      ) || !bulk_table_row_names_match(
        source_attributes,
        BULK_TAGS_ROW_NAMES,
        VECTOR_ELT(source, BULK_SOURCE_TAGS)
      ) || !bulk_table_row_names_match(
        source_attributes,
        BULK_TRAFOS_ROW_NAMES,
        VECTOR_ELT(source, BULK_SOURCE_TRAFOS)
      ) || !bulk_table_row_names_match(
        source_attributes,
        BULK_DEPENDENCIES_ROW_NAMES,
        VECTOR_ELT(source, BULK_SOURCE_DEPENDENCIES)
      )) {
    return FALSE;
  }
  /* All potentially allocation-capable surface authentication is complete.
   * The remaining identity and request audit is inert and is the last work
   * before the composite capability is cleared and the batch is returned. */
  if (paradox_domain_local_value(private_environment, ".params") !=
        VECTOR_ELT(source, BULK_SOURCE_PARAMS) ||
      paradox_domain_local_value(private_environment, ".tags") !=
        VECTOR_ELT(source, BULK_SOURCE_TAGS) ||
      paradox_domain_local_value(private_environment, ".trafos") !=
        VECTOR_ELT(source, BULK_SOURCE_TRAFOS) ||
      paradox_domain_local_value(private_environment, ".deps") !=
        VECTOR_ELT(source, BULK_SOURCE_DEPENDENCIES) ||
      paradox_domain_local_value(private_environment, ".values") !=
        expected_values || expected_values !=
        VECTOR_ELT(source, BULK_SOURCE_VALUES) ||
      !bulk_source_children_match(
        source,
        source_children,
        value_cells,
        source_attributes,
        work_since_interrupt
      )) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(requested_ids); ++index) {
    paradox_domain_account_work(work_since_interrupt);
    if (STRING_ELT(requested_ids, index) !=
        STRING_ELT(requested_snapshot, index)) {
      return FALSE;
    }
  }
  return TRUE;
}

SEXP paradox_param_set_subspace_states(SEXP private_environment, SEXP self,
    SEXP requested_ids, SEXP expected_values) {
  if (TYPEOF(requested_ids) != STRSXP || ALTREP(requested_ids) ||
      !subset_has_no_attributes(requested_ids) ||
      TYPEOF(expected_values) != VECSXP || ALTREP(expected_values)) {
    return R_NilValue;
  }
  R_xlen_t work_since_interrupt = 0;
  int protected_count = 0;
  SEXP source = PROTECT(Rf_allocVector(VECSXP, BULK_SOURCE_COUNT));
  ++protected_count;
  SET_VECTOR_ELT(source, BULK_SOURCE_PARAMS, paradox_domain_local_value(
    private_environment,
    ".params"
  ));
  SET_VECTOR_ELT(source, BULK_SOURCE_TAGS, paradox_domain_local_value(
    private_environment,
    ".tags"
  ));
  SET_VECTOR_ELT(source, BULK_SOURCE_TRAFOS, paradox_domain_local_value(
    private_environment,
    ".trafos"
  ));
  SET_VECTOR_ELT(source, BULK_SOURCE_DEPENDENCIES, paradox_domain_local_value(
    private_environment,
    ".deps"
  ));
  SET_VECTOR_ELT(source, BULK_SOURCE_VALUES, paradox_domain_local_value(
    private_environment,
    ".values"
  ));
  SET_VECTOR_ELT(source, BULK_SOURCE_EXTRA_TRAFO, paradox_domain_local_value(
    private_environment,
    ".extra_trafo"
  ));
  SEXP requested_snapshot = PROTECT(Rf_duplicate(requested_ids));
  ++protected_count;
  SEXP source_children = PROTECT(Rf_allocVector(
    VECSXP,
    SUBSET_CHILD_ROOT_COUNT
  ));
  ++protected_count;
  SEXP source_params = VECTOR_ELT(source, BULK_SOURCE_PARAMS);
  SEXP source_tags = VECTOR_ELT(source, BULK_SOURCE_TAGS);
  SEXP source_trafos = VECTOR_ELT(source, BULK_SOURCE_TRAFOS);
  SEXP source_dependencies = VECTOR_ELT(
    source,
    BULK_SOURCE_DEPENDENCIES
  );
  SEXP source_values = VECTOR_ELT(source, BULK_SOURCE_VALUES);
  if (!root_table_children(
        source_children,
        SUBSET_CHILD_PARAMS_FIRST,
        SUBSET_CHILD_PARAMS_NAMES,
        SUBSET_CHILD_PARAMS_CLASSES,
        R_XLEN_T_MAX,
        source_params,
        PARADOX_DOMAIN_TAGS
      ) || !root_table_children(
        source_children,
        SUBSET_CHILD_TAG_IDS,
        SUBSET_CHILD_TAG_NAMES,
        SUBSET_CHILD_TAG_CLASSES,
        SUBSET_CHILD_TAG_SORTED,
        source_tags,
        2
      ) || !root_table_children(
        source_children,
        SUBSET_CHILD_TRAFO_IDS,
        SUBSET_CHILD_TRAFO_NAMES,
        SUBSET_CHILD_TRAFO_CLASSES,
        SUBSET_CHILD_TRAFO_SORTED,
        source_trafos,
        2
      ) || !root_table_children(
        source_children,
        SUBSET_CHILD_DEPENDENCY_IDS,
        SUBSET_CHILD_DEPENDENCY_NAMES,
        SUBSET_CHILD_DEPENDENCY_CLASSES,
        R_XLEN_T_MAX,
        source_dependencies,
        3
      ) || TYPEOF(source_values) != VECSXP || ALTREP(source_values)) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  SET_VECTOR_ELT(source_children, SUBSET_CHILD_VALUES, source_values);
  SET_VECTOR_ELT(
    source_children,
    SUBSET_CHILD_VALUE_NAMES,
    Rf_getAttrib(source_values, R_NamesSymbol)
  );
  SEXP value_cells = PROTECT(Rf_allocVector(
    VECSXP,
    XLENGTH(source_values)
  ));
  ++protected_count;
  for (R_xlen_t index = 0; index < XLENGTH(source_values); ++index) {
    paradox_domain_account_work(&work_since_interrupt);
    SET_VECTOR_ELT(value_cells, index, VECTOR_ELT(source_values, index));
  }
  SEXP source_attributes = PROTECT(Rf_allocVector(
    VECSXP,
    BULK_SOURCE_ATTRIBUTE_COUNT
  ));
  ++protected_count;
  snapshot_bulk_table_attributes(
    source_attributes,
    BULK_PARAMS_ROW_NAMES,
    source_params
  );
  snapshot_bulk_table_attributes(
    source_attributes,
    BULK_TAGS_ROW_NAMES,
    source_tags
  );
  snapshot_bulk_table_attributes(
    source_attributes,
    BULK_TRAFOS_ROW_NAMES,
    source_trafos
  );
  snapshot_bulk_table_attributes(
    source_attributes,
    BULK_DEPENDENCIES_ROW_NAMES,
    source_dependencies
  );
  SET_VECTOR_ELT(
    source_attributes,
    BULK_VALUES_CLASS,
    Rf_getAttrib(source_values, R_ClassSymbol)
  );
  SEXP dependency_mode = PROTECT(Rf_ScalarLogical(NA_LOGICAL));
  ++protected_count;
  SEXP composite_plan = PROTECT(param_set_subset_state_impl(
    private_environment,
    self,
    requested_snapshot,
    dependency_mode,
    expected_values
  ));
  ++protected_count;
  if (composite_plan == R_NilValue || TYPEOF(composite_plan) != VECSXP ||
      XLENGTH(composite_plan) != SUBSET_PLAN_FIELD_COUNT) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  SEXP composite_token = VECTOR_ELT(composite_plan, SUBSET_STATE_TOKEN);
  SEXP composite = subset_token_payload(composite_token);
  if (composite == R_NilValue) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  SEXP params = VECTOR_ELT(composite, SUBSET_PARAMS);
  SEXP tags = VECTOR_ELT(composite, SUBSET_TAGS);
  SEXP trafos = VECTOR_ELT(composite, SUBSET_TRAFOS);
  SEXP dependencies = VECTOR_ELT(composite, SUBSET_DEPENDENCIES);
  SEXP values = VECTOR_ELT(composite, SUBSET_VALUES);
  const R_xlen_t count = XLENGTH(requested_snapshot);
  if (TYPEOF(params) != VECSXP || XLENGTH(params) == 0 ||
      TYPEOF(VECTOR_ELT(params, 0)) != STRSXP ||
      XLENGTH(VECTOR_ELT(params, 0)) != count ||
      TYPEOF(tags) != VECSXP || XLENGTH(tags) != 2 ||
      TYPEOF(trafos) != VECSXP || XLENGTH(trafos) != 2 ||
      TYPEOF(dependencies) != VECSXP || XLENGTH(dependencies) != 3 ||
      XLENGTH(VECTOR_ELT(dependencies, 0)) != 0) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }

  SEXP request_owner_sexp = PROTECT(Rf_match(
    requested_snapshot,
    requested_snapshot,
    0
  ));
  ++protected_count;
  SEXP tag_owner_sexp = PROTECT(Rf_match(
    requested_snapshot,
    VECTOR_ELT(tags, 0),
    0
  ));
  ++protected_count;
  SEXP trafo_owner_sexp = PROTECT(Rf_match(
    requested_snapshot,
    VECTOR_ELT(trafos, 0),
    0
  ));
  ++protected_count;
  const subset_match_vector_t request_owners = match_vector(
    request_owner_sexp,
    count
  );
  const subset_match_vector_t tag_owners = match_vector(
    tag_owner_sexp,
    XLENGTH(VECTOR_ELT(tags, 0))
  );
  const subset_match_vector_t trafo_owners = match_vector(
    trafo_owner_sexp,
    XLENGTH(VECTOR_ELT(trafos, 0))
  );
  R_xlen_t *occurrence_counts = paradox_temporary_alloc(
    count,
    sizeof(*occurrence_counts)
  );
  R_xlen_t *occurrence_ranks = paradox_temporary_alloc(
    count,
    sizeof(*occurrence_ranks)
  );
  R_xlen_t *tag_starts = paradox_temporary_alloc(count, sizeof(*tag_starts));
  R_xlen_t *tag_counts = paradox_temporary_alloc(count, sizeof(*tag_counts));
  R_xlen_t *trafo_starts = paradox_temporary_alloc(
    count,
    sizeof(*trafo_starts)
  );
  R_xlen_t *trafo_counts = paradox_temporary_alloc(
    count,
    sizeof(*trafo_counts)
  );
  R_xlen_t *value_indices = paradox_temporary_alloc(
    count,
    sizeof(*value_indices)
  );
  if (!bulk_occurrence_layout(
      &request_owners,
      count,
      occurrence_counts,
      occurrence_ranks,
      &work_since_interrupt
    ) || !bulk_key_table_layout(
      &tag_owners,
      XLENGTH(VECTOR_ELT(tags, 0)),
      count,
      occurrence_counts,
      tag_starts,
      tag_counts,
      &work_since_interrupt
    ) || !bulk_key_table_layout(
      &trafo_owners,
      XLENGTH(VECTOR_ELT(trafos, 0)),
      count,
      occurrence_counts,
      trafo_starts,
      trafo_counts,
      &work_since_interrupt
    )) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  SEXP value_names = Rf_getAttrib(values, R_NamesSymbol);
  if (TYPEOF(values) != VECSXP || ALTREP(values) ||
      TYPEOF(value_names) != STRSXP || ALTREP(value_names) ||
      XLENGTH(value_names) != XLENGTH(values)) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  R_xlen_t value_cursor = 0;
  for (R_xlen_t request = 0; request < count; ++request) {
    paradox_domain_account_work(&work_since_interrupt);
    if (STRING_ELT(VECTOR_ELT(params, 0), request) !=
        STRING_ELT(requested_snapshot, request)) {
      UNPROTECT(protected_count);
      return R_NilValue;
    }
    value_indices[request] = R_XLEN_T_MAX;
    if (value_cursor < XLENGTH(values) && paradox_domain_strings_equal(
        STRING_ELT(value_names, value_cursor),
        STRING_ELT(requested_snapshot, request)
      )) {
      value_indices[request] = value_cursor++;
    }
  }
  if (value_cursor != XLENGTH(values)) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }

  SEXP plans = PROTECT(Rf_allocVector(VECSXP, count));
  ++protected_count;
  SEXP plan_names = PROTECT(Rf_duplicate(requested_snapshot));
  ++protected_count;
  Rf_setAttrib(plans, R_NamesSymbol, plan_names);
  R_xlen_t *tag_positions = paradox_temporary_alloc(
    XLENGTH(VECTOR_ELT(tags, 0)),
    sizeof(*tag_positions)
  );
  R_xlen_t *trafo_positions = paradox_temporary_alloc(
    XLENGTH(VECTOR_ELT(trafos, 0)),
    sizeof(*trafo_positions)
  );
  R_xlen_t dependency_position = 0;
  for (R_xlen_t request = 0; request < count; ++request) {
    paradox_domain_account_work(&work_since_interrupt);
    SEXP id = STRING_ELT(requested_snapshot, request);
    const R_xlen_t owner = match_at(&request_owners, request) - 1;
    const R_xlen_t tag_count = tag_counts[owner];
    const R_xlen_t trafo_count = trafo_counts[owner];
    const R_xlen_t tag_first = tag_starts[owner] == R_XLEN_T_MAX
      ? 0
      : tag_starts[owner] + occurrence_ranks[request] * tag_count;
    const R_xlen_t trafo_first = trafo_starts[owner] == R_XLEN_T_MAX
      ? 0
      : trafo_starts[owner] + occurrence_ranks[request] * trafo_count;
    for (R_xlen_t index = 0; index < tag_count; ++index) {
      tag_positions[index] = tag_first + index;
    }
    for (R_xlen_t index = 0; index < trafo_count; ++index) {
      trafo_positions[index] = trafo_first + index;
    }
    SEXP selected_params = PROTECT(subset_table(
      params,
      &request,
      1,
      FALSE,
      &work_since_interrupt
    ));
    ++protected_count;
    if (!paradox_param_set_attach_singleton_index(selected_params)) {
      UNPROTECT(protected_count);
      return R_NilValue;
    }
    SEXP selected_tags = PROTECT(subset_table(
      tags,
      tag_positions,
      tag_count,
      TRUE,
      &work_since_interrupt
    ));
    ++protected_count;
    SEXP selected_trafos = PROTECT(subset_table(
      trafos,
      trafo_positions,
      trafo_count,
      TRUE,
      &work_since_interrupt
    ));
    ++protected_count;
    SEXP selected_dependencies = PROTECT(subset_table(
      dependencies,
      &dependency_position,
      0,
      FALSE,
      &work_since_interrupt
    ));
    ++protected_count;
    SEXP selected_values = PROTECT(singleton_composite_value(
      values,
      value_indices[request]
    ));
    ++protected_count;
    if (selected_params == R_NilValue || selected_tags == R_NilValue ||
        selected_trafos == R_NilValue ||
        selected_dependencies == R_NilValue ||
        selected_values == R_NilValue ||
        !bulk_singleton_state_admitted(
          selected_params,
          selected_tags,
          selected_trafos,
          selected_dependencies,
          selected_values,
          id,
          &work_since_interrupt
        )) {
      UNPROTECT(protected_count);
      return R_NilValue;
    }
    SEXP payload = PROTECT(Rf_allocVector(VECSXP, SUBSET_STATE_FIELD_COUNT));
    ++protected_count;
    SET_VECTOR_ELT(payload, SUBSET_PARAMS, selected_params);
    SET_VECTOR_ELT(payload, SUBSET_TAGS, selected_tags);
    SET_VECTOR_ELT(payload, SUBSET_TRAFOS, selected_trafos);
    SET_VECTOR_ELT(payload, SUBSET_DEPENDENCIES, selected_dependencies);
    SET_VECTOR_ELT(payload, SUBSET_VALUES, selected_values);
    SET_VECTOR_ELT(
      payload,
      SUBSET_EXTRA_TRAFO,
      VECTOR_ELT(composite, SUBSET_EXTRA_TRAFO)
    );
    SEXP token = PROTECT(R_MakeExternalPtr(
      (void *) &subset_state_identity,
      Rf_install("paradox_subset_state_v1"),
      payload
    ));
    ++protected_count;
    SEXP missing = PROTECT(Rf_allocVector(STRSXP, 0));
    ++protected_count;
    SEXP plan = PROTECT(new_plan(missing, token));
    ++protected_count;
    SET_VECTOR_ELT(plans, request, plan);
    UNPROTECT(9);
    protected_count -= 9;
  }

  if (!bulk_subspace_source_matches(
      private_environment,
      self,
      source,
      source_children,
      value_cells,
      source_attributes,
      requested_ids,
      requested_snapshot,
      expected_values,
      &work_since_interrupt
    )) {
    UNPROTECT(protected_count);
    return R_NilValue;
  }
  R_SetExternalPtrProtected(composite_token, R_NilValue);
  R_ClearExternalPtr(composite_token);
  UNPROTECT(protected_count);
  return plans;
}

int paradox_param_set_subset_state_is_singleton(SEXP token) {
  SEXP payload = subset_token_payload(token);
  if (payload == R_NilValue) {
    return FALSE;
  }
  SEXP params = VECTOR_ELT(payload, SUBSET_PARAMS);
  SEXP dependencies = VECTOR_ELT(payload, SUBSET_DEPENDENCIES);
  return TYPEOF(params) == VECSXP && XLENGTH(params) > 0 &&
    XLENGTH(VECTOR_ELT(params, 0)) == 1 &&
    TYPEOF(dependencies) == VECSXP && XLENGTH(dependencies) > 0 &&
    XLENGTH(VECTOR_ELT(dependencies, 0)) == 0;
}

static int subset_state_contains_environment(SEXP value, unsigned int depth,
    R_xlen_t *remaining) {
  if (TYPEOF(value) == ENVSXP) {
    return TRUE;
  }
  if (TYPEOF(value) != VECSXP) {
    return FALSE;
  }
  const R_xlen_t size = XLENGTH(value);
  if (ALTREP(value) || depth >= 64U || size < 0 || size > *remaining) {
    return TRUE;
  }
  *remaining -= size;
  for (R_xlen_t index = 0; index < size; ++index) {
    if (subset_state_contains_environment(
        VECTOR_ELT(value, index),
        depth + 1U,
        remaining
      )) {
      return TRUE;
    }
  }
  return FALSE;
}

static int subset_sampler_builtin_bounded(SEXP payload) {
  SEXP params = VECTOR_ELT(payload, SUBSET_PARAMS);
  if (TYPEOF(params) != VECSXP || ALTREP(params) ||
      XLENGTH(params) != PARADOX_DOMAIN_STORAGE_TYPE + 1) {
    return FALSE;
  }
  SEXP classes = VECTOR_ELT(params, PARADOX_DOMAIN_CLS);
  SEXP storage = VECTOR_ELT(params, PARADOX_DOMAIN_STORAGE_TYPE);
  if (TYPEOF(classes) != STRSXP || ALTREP(classes) ||
      XLENGTH(classes) != 1 || TYPEOF(storage) != STRSXP ||
      ALTREP(storage) || XLENGTH(storage) != 1) {
    return FALSE;
  }
  SEXP class_name = STRING_ELT(classes, 0);
  SEXP storage_name = STRING_ELT(storage, 0);
  const int numeric =
    (paradox_domain_string_is(class_name, "ParamDbl") &&
      paradox_domain_string_is(storage_name, "numeric")) ||
    (paradox_domain_string_is(class_name, "ParamInt") &&
      paradox_domain_string_is(storage_name, "integer"));
  if (numeric) {
    SEXP lower = VECTOR_ELT(params, PARADOX_DOMAIN_LOWER);
    SEXP upper = VECTOR_ELT(params, PARADOX_DOMAIN_UPPER);
    return TYPEOF(lower) == REALSXP && !ALTREP(lower) &&
      XLENGTH(lower) == 1 && TYPEOF(upper) == REALSXP &&
      !ALTREP(upper) && XLENGTH(upper) == 1 &&
      R_FINITE(REAL_ELT(lower, 0)) && R_FINITE(REAL_ELT(upper, 0));
  }
  return (paradox_domain_string_is(class_name, "ParamFct") &&
      paradox_domain_string_is(storage_name, "character")) ||
    (paradox_domain_string_is(class_name, "ParamLgl") &&
      paradox_domain_string_is(storage_name, "logical"));
}

int paradox_param_set_subset_state_is_sampler_safe(SEXP token) {
  SEXP payload = subset_token_payload(token);
  if (payload == R_NilValue ||
      !paradox_param_set_subset_state_is_singleton(token) ||
      VECTOR_ELT(payload, SUBSET_EXTRA_TRAFO) != R_NilValue ||
      !subset_sampler_builtin_bounded(payload)) {
    return FALSE;
  }
  /* Sampler's historical deep clone recursively owns R6/environment values.
   * Decline before adoption if any nested child state can carry that ownership
   * boundary.  Closures are deliberately shared by R6 and are not ENVSXP. */
  R_xlen_t remaining = 100000;
  for (R_xlen_t field = 0; field < SUBSET_STATE_FIELD_COUNT; ++field) {
    if (subset_state_contains_environment(
        VECTOR_ELT(payload, field),
        0U,
        &remaining
      )) {
      return FALSE;
    }
  }
  return TRUE;
}

int paradox_param_set_subset_destination_ready(SEXP private_environment) {
  if (TYPEOF(private_environment) != ENVSXP) {
    return FALSE;
  }

  static const char *const field_names[SUBSET_STATE_FIELD_COUNT] = {
    ".params", ".tags", ".trafos", ".deps", ".values", ".extra_trafo"
  };
  SEXP symbols[SUBSET_STATE_FIELD_COUNT];
  for (R_xlen_t field = 0; field < SUBSET_STATE_FIELD_COUNT; ++field) {
    symbols[field] = Rf_install(field_names[field]);
    if (!R_existsVarInFrame(private_environment, symbols[field]) ||
        R_BindingIsActive(symbols[field], private_environment) ||
        R_BindingIsLocked(symbols[field], private_environment)) {
      return FALSE;
    }
  }
  /* A constructor hand-off may only initialize a fresh R6 private state.
   * This also prevents an accidentally retained token from overwriting an
   * already initialized ParamSet before the token is consumed. */
  if (paradox_domain_local_value(private_environment, ".params") !=
      R_NilValue) {
    return FALSE;
  }
  return TRUE;
}

int paradox_param_set_adopt_subset_state_internal(SEXP private_environment,
    SEXP token) {
  SEXP payload = subset_token_payload(token);
  if (payload == R_NilValue ||
      !paradox_param_set_subset_destination_ready(private_environment)) {
    return FALSE;
  }

  static const char *const field_names[SUBSET_STATE_FIELD_COUNT] = {
    ".params", ".tags", ".trafos", ".deps", ".values", ".extra_trafo"
  };
  SEXP symbols[SUBSET_STATE_FIELD_COUNT];
  for (R_xlen_t field = 0; field < SUBSET_STATE_FIELD_COUNT; ++field) {
    symbols[field] = Rf_install(field_names[field]);
    Rf_defineVar(
      symbols[field],
      VECTOR_ELT(payload, field),
      private_environment
    );
  }

  R_SetExternalPtrProtected(token, R_NilValue);
  R_ClearExternalPtr(token);
  return TRUE;
}

SEXP paradox_param_set_adopt_subset_state(SEXP private_environment,
    SEXP token) {
  /* A NULL owner is a read-only capability probe used by internal sampler
   * construction. It neither exposes the payload nor consumes the token. */
  if (private_environment == R_NilValue) {
    return Rf_ScalarLogical(subset_token_payload(token) != R_NilValue);
  }
  return Rf_ScalarLogical(paradox_param_set_adopt_subset_state_internal(
    private_environment,
    token
  ));
}
