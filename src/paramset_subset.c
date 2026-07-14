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
  SEXP prepared = PROTECT(paradox_prepare_data_table(result));
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

SEXP paradox_param_set_subset_state(SEXP private_environment, SEXP self,
    SEXP requested_ids, SEXP check_dependencies) {
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
      XLENGTH(check_dependencies) != 1 ||
      LOGICAL_ELT(check_dependencies, 0) == NA_LOGICAL) {
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
    SUBSET_SOURCE_COUNT
  };
  int protected_count = 0;
  SEXP source_state = PROTECT(Rf_allocVector(VECSXP, SUBSET_SOURCE_COUNT));
  ++protected_count;
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
      dependencies_sexp == R_UnboundValue || values_sexp == R_UnboundValue) {
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

  SEXP missing = PROTECT(LOGICAL_ELT(check_dependencies, 0)
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
  for (R_xlen_t request = 0; request < requested_size; ++request) {
    const R_xlen_t owner = requested_positions[request];
    for (R_xlen_t position = dependency_offsets[owner];
        position < dependency_offsets[owner + 1]; ++position) {
      paradox_domain_account_work(&work_since_interrupt);
      dependency_positions[dependency_output++] = dependency_order[position];
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
  SEXP token = PROTECT(R_MakeExternalPtr(
    (void *) &subset_state_identity,
    Rf_install("paradox_subset_state_v1"),
    payload
  ));
  ++protected_count;
  SEXP plan = PROTECT(new_plan(missing, token));
  ++protected_count;

  UNPROTECT(protected_count);
  return plan;
}

SEXP paradox_param_set_adopt_subset_state(SEXP private_environment,
    SEXP token) {
  if (TYPEOF(private_environment) != ENVSXP || TYPEOF(token) != EXTPTRSXP ||
      R_ExternalPtrAddr(token) != (void *) &subset_state_identity ||
      R_ExternalPtrTag(token) != Rf_install("paradox_subset_state_v1")) {
    return Rf_ScalarLogical(FALSE);
  }
  SEXP payload = R_ExternalPtrProtected(token);
  if (TYPEOF(payload) != VECSXP ||
      XLENGTH(payload) != SUBSET_STATE_FIELD_COUNT) {
    return Rf_ScalarLogical(FALSE);
  }

  static const char *const field_names[SUBSET_STATE_FIELD_COUNT] = {
    ".params", ".tags", ".trafos", ".deps", ".values"
  };
  SEXP symbols[SUBSET_STATE_FIELD_COUNT];
  for (R_xlen_t field = 0; field < SUBSET_STATE_FIELD_COUNT; ++field) {
    symbols[field] = Rf_install(field_names[field]);
    if (!R_existsVarInFrame(private_environment, symbols[field]) ||
        R_BindingIsActive(symbols[field], private_environment) ||
        R_BindingIsLocked(symbols[field], private_environment)) {
      return Rf_ScalarLogical(FALSE);
    }
  }
  /* A constructor hand-off may only initialize a fresh R6 private state.
   * This also prevents an accidentally retained token from overwriting an
   * already initialized ParamSet before the token is consumed. */
  if (paradox_domain_local_value(private_environment, ".params") !=
      R_NilValue) {
    return Rf_ScalarLogical(FALSE);
  }
  for (R_xlen_t field = 0; field < SUBSET_STATE_FIELD_COUNT; ++field) {
    Rf_defineVar(
      symbols[field],
      VECTOR_ELT(payload, field),
      private_environment
    );
  }

  R_SetExternalPtrProtected(token, R_NilValue);
  R_ClearExternalPtr(token);
  return Rf_ScalarLogical(TRUE);
}
