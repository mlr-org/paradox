#include <string.h>

#include "paradox.h"
#include <R_ext/Utils.h>

#include "r_api_compat.h"

/* This routine deliberately plans calls without evaluating them.  Keeping
 * callback evaluation in R preserves promises, non-standard evaluation, and
 * the historical `trafo(value)` call recorded by conditions. */

static inline void interrupt_after_work(R_xlen_t *work) {
  ++*work;
  if (*work >= PARADOX_INTERRUPT_CHECK_INTERVAL) {
    R_CheckUserInterrupt();
    *work = 0;
  }
}

static int string_equals(SEXP value, const char *expected) {
  return value != NA_STRING && strcmp(CHAR(value), expected) == 0;
}

static int exact_character_pair(SEXP value, const char *first,
    const char *second) {
  return TYPEOF(value) == STRSXP && !ALTREP(value) && XLENGTH(value) == 2 &&
    paradox_api_has_no_attributes(value) &&
    string_equals(STRING_ELT(value, 0), first) &&
    string_equals(STRING_ELT(value, 1), second);
}

static int exact_character_scalar(SEXP value, const char *expected) {
  return TYPEOF(value) == STRSXP && !ALTREP(value) && XLENGTH(value) == 1 &&
    paradox_api_has_no_attributes(value) &&
    string_equals(STRING_ELT(value, 0), expected);
}

static int names_are_only_attribute(SEXP value) {
  return paradox_api_has_single_attribute(value, "names");
}

/* The R caller has already established that names are the sole attribute, so
 * the fast path cannot bypass S3 `[` or `insert_named()` methods. Repeat the
 * public-API checks available here, including the historically accepted empty
 * unnamed list, before reading any vector element. */
enum trafo_root_slot {
  TRAFO_ROOT_INPUT_NAMES = 0,
  TRAFO_ROOT_TABLE_NAMES,
  TRAFO_ROOT_TABLE_CLASSES,
  TRAFO_ROOT_TABLE_SORTED,
  TRAFO_ROOT_IDS,
  TRAFO_ROOT_CALLBACKS,
  TRAFO_ROOT_COUNT
};

static int ordinary_input_names(SEXP x, SEXP *names, R_xlen_t *size,
    SEXP roots, R_xlen_t *work) {
  if (TYPEOF(x) != VECSXP || ALTREP(x)) {
    return FALSE;
  }

  *size = XLENGTH(x);
  if (paradox_api_has_no_attributes(x)) {
    if (*size == 0) {
      *names = R_NilValue;
      return TRUE;
    }
    return FALSE;
  }

  if (Rf_isObject(x) || !names_are_only_attribute(x)) {
    return FALSE;
  }

  SEXP candidate = Rf_getAttrib(x, R_NamesSymbol);
  SET_VECTOR_ELT(roots, TRAFO_ROOT_INPUT_NAMES, candidate);
  if (TYPEOF(candidate) != STRSXP || ALTREP(candidate) ||
      XLENGTH(candidate) != *size ||
      !paradox_api_has_no_attributes(candidate)) {
    return FALSE;
  }

  for (R_xlen_t index = 0; index < *size; ++index) {
    interrupt_after_work(work);
    SEXP name = STRING_ELT(candidate, index);
    if (name == NA_STRING || CHAR(name)[0] == '\0') {
      return FALSE;
    }
  }
  if (Rf_any_duplicated(candidate, FALSE) != 0) {
    return FALSE;
  }

  *names = candidate;
  return TRUE;
}

static int canonical_trafo_table(SEXP table, SEXP *ids, SEXP *callbacks,
    R_xlen_t *size, SEXP roots, R_xlen_t *work) {
  if (TYPEOF(table) != VECSXP || ALTREP(table) || XLENGTH(table) != 2) {
    return FALSE;
  }

  SEXP table_names = Rf_getAttrib(table, R_NamesSymbol);
  SET_VECTOR_ELT(roots, TRAFO_ROOT_TABLE_NAMES, table_names);
  SEXP classes = Rf_getAttrib(table, R_ClassSymbol);
  SET_VECTOR_ELT(roots, TRAFO_ROOT_TABLE_CLASSES, classes);
  SEXP sorted = Rf_getAttrib(table, Rf_install("sorted"));
  SET_VECTOR_ELT(roots, TRAFO_ROOT_TABLE_SORTED, sorted);
  if (!exact_character_pair(table_names, "id", "trafo") ||
      !exact_character_pair(classes, "data.table", "data.frame") ||
      !exact_character_scalar(sorted, "id")) {
    return FALSE;
  }

  SEXP candidate_ids = VECTOR_ELT(table, 0);
  SET_VECTOR_ELT(roots, TRAFO_ROOT_IDS, candidate_ids);
  SEXP candidate_callbacks = VECTOR_ELT(table, 1);
  SET_VECTOR_ELT(roots, TRAFO_ROOT_CALLBACKS, candidate_callbacks);
  if (TYPEOF(candidate_ids) != STRSXP ||
      TYPEOF(candidate_callbacks) != VECSXP ||
      ALTREP(candidate_ids) || ALTREP(candidate_callbacks)) {
    return FALSE;
  }
  *size = XLENGTH(candidate_ids);
  if (XLENGTH(candidate_callbacks) != *size ||
      !paradox_api_has_no_attributes(candidate_ids) ||
      !paradox_api_has_no_attributes(candidate_callbacks)) {
    return FALSE;
  }

  for (R_xlen_t index = 0; index < *size; ++index) {
    interrupt_after_work(work);
    SEXP id = STRING_ELT(candidate_ids, index);
    if (id == NA_STRING || CHAR(id)[0] == '\0' ||
        !Rf_isFunction(VECTOR_ELT(candidate_callbacks, index))) {
      return FALSE;
    }
  }
  if (Rf_any_duplicated(candidate_ids, FALSE) != 0) {
    return FALSE;
  }

  *ids = candidate_ids;
  *callbacks = candidate_callbacks;
  return TRUE;
}

typedef struct {
  SEXPTYPE type;
  const int *integer_values;
  const double *real_values;
} match_vector_t;

static match_vector_t match_vector(SEXP value, R_xlen_t expected_size) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  if ((type != INTSXP && type != REALSXP) ||
      XLENGTH(value) != expected_size) {
    Rf_error("Internal error: unexpected result from R's matching primitive");
  }

  const match_vector_t result = {
    type,
    type == INTSXP ? INTEGER_RO(value) : NULL,
    type == REALSXP ? REAL_RO(value) : NULL
  };
  return result;
}

static R_xlen_t match_at(const match_vector_t *matches, R_xlen_t index) {
  if (matches->type == INTSXP) {
    const int value = matches->integer_values[index];
    return value == NA_INTEGER || value <= 0 ? 0 : (R_xlen_t) value;
  }

  const double value = matches->real_values[index];
  return ISNAN(value) || value <= 0.0 ? 0 : (R_xlen_t) value;
}

SEXP paradox_param_set_trafo_plan(SEXP x, SEXP table) {
  R_xlen_t work = 0;
  R_xlen_t input_size = 0;
  R_xlen_t table_size = 0;
  SEXP input_names = R_NilValue;
  SEXP ids = R_NilValue;
  SEXP callbacks = R_NilValue;
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, TRAFO_ROOT_COUNT));

  if (!ordinary_input_names(
      x,
      &input_names,
      &input_size,
      roots,
      &work
    ) || !canonical_trafo_table(
      table,
      &ids,
      &callbacks,
      &table_size,
      roots,
      &work
    )) {
    UNPROTECT(1);
    return R_NilValue;
  }

  if (input_names == R_NilValue) {
    /* Rf_match() does not need to participate in the empty unnamed case. */
    SEXP matched_ids = PROTECT(Rf_allocVector(STRSXP, 0));
    SEXP matched_callbacks = PROTECT(Rf_allocVector(VECSXP, 0));
    SEXP matched_values = PROTECT(Rf_allocVector(VECSXP, 0));
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 3));
    SET_VECTOR_ELT(result, 0, matched_ids);
    SET_VECTOR_ELT(result, 1, matched_callbacks);
    SET_VECTOR_ELT(result, 2, matched_values);
    UNPROTECT(5);
    return result;
  }

  /* Rf_match(table, x, nomatch) uses R's encoding-aware character matching
   * and returns, for each input name, its one-based table position. */
  SEXP matches_sexp = PROTECT(Rf_match(ids, input_names, 0));
  const match_vector_t matches = match_vector(matches_sexp, input_size);

  R_xlen_t matched_size = 0;
  for (R_xlen_t input = 0; input < input_size; ++input) {
    interrupt_after_work(&work);
    const R_xlen_t matched = match_at(&matches, input);
    if (matched > table_size) {
      UNPROTECT(2);
      Rf_error("Internal error: invalid result from R's matching primitive");
    }
    matched_size += matched != 0;
  }

  SEXP matched_ids = PROTECT(Rf_allocVector(STRSXP, matched_size));
  SEXP matched_callbacks = PROTECT(Rf_allocVector(VECSXP, matched_size));
  SEXP matched_values = PROTECT(Rf_allocVector(VECSXP, matched_size));
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 3));

  R_xlen_t output = 0;
  for (R_xlen_t input = 0; input < input_size; ++input) {
    interrupt_after_work(&work);
    const R_xlen_t matched = match_at(&matches, input);
    if (matched == 0) {
      continue;
    }
    const R_xlen_t table_index = matched - 1;
    SET_STRING_ELT(matched_ids, output, STRING_ELT(ids, table_index));
    SET_VECTOR_ELT(
      matched_callbacks,
      output,
      VECTOR_ELT(callbacks, table_index)
    );
    SET_VECTOR_ELT(matched_values, output, VECTOR_ELT(x, input));
    ++output;
  }

  SET_VECTOR_ELT(result, 0, matched_ids);
  SET_VECTOR_ELT(result, 1, matched_callbacks);
  SET_VECTOR_ELT(result, 2, matched_values);
  UNPROTECT(6);
  return result;
}
