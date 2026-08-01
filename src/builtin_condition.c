#include <string.h>

#include "builtin_condition.h"
#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

static int exact_scalar_string(SEXP value, const char *expected) {
  return TYPEOF(value) == STRSXP && !ALTREP(value) && !Rf_isS4(value) &&
    XLENGTH(value) == 1 &&
    paradox_domain_string_is(STRING_ELT(value, 0), expected);
}

static int condition_rhs_is_plain(SEXP rhs,
    paradox_builtin_condition_kind_t kind,
    R_xlen_t *work_since_interrupt) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(rhs);
  if ((type != LGLSXP && type != INTSXP && type != REALSXP &&
       type != STRSXP) || ALTREP(rhs) || Rf_isObject(rhs) || Rf_isS4(rhs) ||
      !paradox_api_has_no_attributes(rhs)) {
    return FALSE;
  }
  const R_xlen_t size = XLENGTH(rhs);
  if ((kind == PARADOX_BUILTIN_CONDITION_EQUAL && size != 1) ||
      (kind == PARADOX_BUILTIN_CONDITION_ANY_OF && size == 0)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_account_work(work_since_interrupt);
    if ((type == LGLSXP && LOGICAL_ELT(rhs, index) == NA_LOGICAL) ||
        (type == INTSXP && INTEGER_ELT(rhs, index) == NA_INTEGER) ||
        (type == REALSXP && ISNAN(REAL_ELT(rhs, index))) ||
        (type == STRSXP &&
         (STRING_ELT(rhs, index) == NA_STRING ||
          Rf_getCharCE(STRING_ELT(rhs, index)) == CE_BYTES))) {
      return FALSE;
    }
  }
  return kind != PARADOX_BUILTIN_CONDITION_ANY_OF ||
    Rf_any_duplicated(rhs, FALSE) == 0;
}

typedef struct {
  SEXP names;
  SEXP classes;
  R_xlen_t count;
  int saw_names;
  int saw_classes;
  int exact;
} condition_attribute_snapshot_t;

static void snapshot_condition_attribute(SEXP tag, SEXP value, void *data) {
  condition_attribute_snapshot_t *snapshot = data;
  if (!snapshot->exact || snapshot->count == R_XLEN_T_MAX) {
    snapshot->exact = FALSE;
    return;
  }
  ++snapshot->count;
  if (tag == R_NamesSymbol && !snapshot->saw_names) {
    snapshot->names = value;
    snapshot->saw_names = TRUE;
  } else if (tag == R_ClassSymbol && !snapshot->saw_classes) {
    snapshot->classes = value;
    snapshot->saw_classes = TRUE;
  } else {
    snapshot->exact = FALSE;
  }
}

static int condition_outer_exact(SEXP condition,
    paradox_builtin_condition_kind_t *kind, SEXP *rhs,
    R_xlen_t *work_since_interrupt) {
  static const char *const names[] = {"rhs", "condition_format_string"};
  static const char *const equal_classes[] = {"CondEqual", "Condition"};
  static const char *const any_of_classes[] = {"CondAnyOf", "Condition"};
  /* Both callers root `condition`. Capture its two raw attributes in one
   * public-API traversal instead of repeatedly counting, selecting, and
   * installing their names for every dependency row. */
  condition_attribute_snapshot_t attributes = {
    R_NilValue, R_NilValue, 0, FALSE, FALSE, TRUE
  };
  R_xlen_t observed_attribute_count = 0;
  const int bounded_attributes =
    paradox_api_map_bounded_stored_attributes(
    condition,
    2,
    snapshot_condition_attribute,
    &attributes,
    &observed_attribute_count
  );
  if (!bounded_attributes || !attributes.exact ||
      attributes.count != (R_xlen_t) observed_attribute_count) {
    return FALSE;
  }
  /*
   * Class selection owns the public closed-dispatch diagnostic even when the
   * remaining Condition shell is malformed. This preserves the useful
   * distinction between an unsupported/missing Condition kind and corrupt
   * payload fields without reading an unbounded attribute spine.
   */
  if (!attributes.saw_classes) {
    Rf_error(
      "Unsupported Condition class; supported classes are 'CondEqual' and 'CondAnyOf'."
    );
  }

  SEXP classes = PROTECT(attributes.classes);
  const int plain_classes = !Rf_isS4(classes) &&
    paradox_api_has_no_attributes(classes);
  const int is_equal = plain_classes && paradox_domain_exact_string_vector(
    classes,
    equal_classes,
    2,
    work_since_interrupt
  );
  const int is_any_of = plain_classes && !is_equal &&
    paradox_domain_exact_string_vector(
    classes,
    any_of_classes,
    2,
    work_since_interrupt
  );
  if (!is_equal && !is_any_of) {
    UNPROTECT(1);
    Rf_error(
      "Unsupported Condition class; supported classes are 'CondEqual' and 'CondAnyOf'."
    );
  }
  if (TYPEOF(condition) != VECSXP || ALTREP(condition) ||
      Rf_isS4(condition) || XLENGTH(condition) != 2 ||
      attributes.count != 2 || observed_attribute_count != 2 ||
      !attributes.saw_names) {
    UNPROTECT(1);
    return FALSE;
  }
  *kind = is_equal
    ? PARADOX_BUILTIN_CONDITION_EQUAL
    : PARADOX_BUILTIN_CONDITION_ANY_OF;

  SEXP condition_names = PROTECT(attributes.names);
  if (Rf_isS4(condition_names) ||
      !paradox_api_has_no_attributes(condition_names) ||
      !paradox_domain_exact_string_vector(
        condition_names,
        names,
        2,
        work_since_interrupt
      )) {
    UNPROTECT(2);
    return FALSE;
  }

  const char *format_text;
  if (is_equal) {
    format_text = "%s == %s";
  } else {
    format_text = "%s %%in%% {%s}";
  }

  SEXP format = PROTECT(VECTOR_ELT(condition, 1));
  SEXP candidate_rhs = PROTECT(VECTOR_ELT(condition, 0));
  const int exact = exact_scalar_string(format, format_text) &&
    paradox_api_has_no_attributes(format);
  if (exact) {
    *rhs = candidate_rhs;
  }
  UNPROTECT(4);
  return exact;
}

int paradox_builtin_condition_exact(SEXP condition,
    paradox_builtin_condition_kind_t *kind, SEXP *rhs,
    R_xlen_t *work_since_interrupt) {
  PROTECT(condition);
  if (!condition_outer_exact(
      condition, kind, rhs, work_since_interrupt
    )) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP candidate_rhs = PROTECT(*rhs);
  const int exact = condition_rhs_is_plain(
    candidate_rhs,
    *kind,
    work_since_interrupt
  );
  UNPROTECT(2);
  return exact;
}

static int compatible_operand_types(SEXPTYPE value_type,
    SEXPTYPE rhs_type) {
  const int value_numeric = value_type == LGLSXP ||
    value_type == INTSXP || value_type == REALSXP;
  const int rhs_numeric = rhs_type == LGLSXP ||
    rhs_type == INTSXP || rhs_type == REALSXP;
  return (value_numeric && rhs_numeric) ||
    (value_type == STRSXP && rhs_type == STRSXP);
}

static int scalar_leaf_is_inspectable(SEXP value, SEXP rhs) {
  const SEXPTYPE value_type = (SEXPTYPE) TYPEOF(value);
  if ((value_type != LGLSXP && value_type != INTSXP &&
      value_type != REALSXP && value_type != STRSXP) || ALTREP(value) ||
      XLENGTH(value) != 1 || Rf_isObject(value) || Rf_isS4(value) ||
      Rf_isS4(rhs) ||
      !paradox_api_has_no_attributes(value)) {
    return FALSE;
  }
  return value_type != STRSXP || STRING_ELT(value, 0) == NA_STRING ||
    Rf_getCharCE(STRING_ELT(value, 0)) != CE_BYTES;
}

static int condition_operand_is_tune_token(SEXP value) {
  int token = FALSE;
  if (!paradox_api_opaque_leaf_class_matches(
      value,
      "TuneToken",
      &token
    )) {
    Rf_error(
      "Dependency value class metadata must be ordinary and bounded"
    );
  }
  return token;
}

int paradox_builtin_condition_scalar_supported(SEXP value, SEXP rhs) {
  if (value == R_NilValue || condition_operand_is_tune_token(value)) {
    return TRUE;
  }
  return scalar_leaf_is_inspectable(value, rhs) &&
    compatible_operand_types(
      (SEXPTYPE) TYPEOF(value),
      (SEXPTYPE) TYPEOF(rhs)
    );
}

/* Distinguishes the two reasons the predicate above can refuse a leaf. A
 * plain scalar whose type the right-hand side can never equal is an ordinary
 * unsatisfied comparison, exactly as `condition_test()` reports it; only a
 * leaf the comparator cannot inspect at all is a shape the caller must
 * diagnose separately. */
int paradox_builtin_condition_scalar_type_mismatch(SEXP value, SEXP rhs) {
  return value != R_NilValue && !condition_operand_is_tune_token(value) &&
    scalar_leaf_is_inspectable(value, rhs);
}

int paradox_builtin_condition_element_matches(SEXP values,
    R_xlen_t index, SEXP rhs, R_xlen_t *work_since_interrupt) {
  const SEXPTYPE value_type = (SEXPTYPE) TYPEOF(values);
  const SEXPTYPE rhs_type = (SEXPTYPE) TYPEOF(rhs);
  if ((value_type == LGLSXP && LOGICAL_ELT(values, index) == NA_LOGICAL) ||
      (value_type == INTSXP && INTEGER_ELT(values, index) == NA_INTEGER) ||
      (value_type == REALSXP && ISNAN(REAL_ELT(values, index))) ||
      (value_type == STRSXP && STRING_ELT(values, index) == NA_STRING)) {
    return FALSE;
  }

  double real_value = 0.0;
  int integer_value = 0;
  if (value_type == REALSXP) {
    real_value = REAL_ELT(values, index);
  } else if (value_type == INTSXP) {
    integer_value = INTEGER_ELT(values, index);
    real_value = (double) integer_value;
  } else if (value_type == LGLSXP) {
    integer_value = LOGICAL_ELT(values, index);
    real_value = (double) integer_value;
  }

  const R_xlen_t size = XLENGTH(rhs);
  for (R_xlen_t rhs_index = 0; rhs_index < size; ++rhs_index) {
    paradox_account_work(work_since_interrupt);
    if ((rhs_type == LGLSXP &&
         LOGICAL_ELT(rhs, rhs_index) == NA_LOGICAL) ||
        (rhs_type == INTSXP &&
         INTEGER_ELT(rhs, rhs_index) == NA_INTEGER) ||
        (rhs_type == REALSXP && ISNAN(REAL_ELT(rhs, rhs_index))) ||
        (rhs_type == STRSXP &&
         STRING_ELT(rhs, rhs_index) == NA_STRING)) {
      continue;
    }
    int equal = FALSE;
    if (value_type == STRSXP) {
      SEXP left = STRING_ELT(values, index);
      SEXP right = STRING_ELT(rhs, rhs_index);
      if (left == right) {
        equal = TRUE;
      } else if (Rf_getCharCE(left) == Rf_getCharCE(right)) {
        equal = strcmp(CHAR(left), CHAR(right)) == 0;
      } else {
        equal = paradox_domain_strings_equal(left, right);
      }
    } else if (value_type == REALSXP || rhs_type == REALSXP) {
      double rhs_value;
      if (rhs_type == REALSXP) {
        rhs_value = REAL_ELT(rhs, rhs_index);
      } else if (rhs_type == INTSXP) {
        const int integer_rhs = INTEGER_ELT(rhs, rhs_index);
        rhs_value = (double) integer_rhs;
      } else {
        const int logical_rhs = LOGICAL_ELT(rhs, rhs_index);
        rhs_value = (double) logical_rhs;
      }
      equal = real_value == rhs_value;
    } else {
      const int rhs_value = rhs_type == INTSXP
        ? INTEGER_ELT(rhs, rhs_index)
        : LOGICAL_ELT(rhs, rhs_index);
      equal = integer_value == rhs_value;
    }
    if (equal) {
      return TRUE;
    }
  }
  return FALSE;
}

static void condition_equal_vector(SEXP values, SEXP rhs, SEXP result,
    R_xlen_t *work_since_interrupt) {
  const SEXPTYPE value_type = (SEXPTYPE) TYPEOF(values);
  const SEXPTYPE rhs_type = (SEXPTYPE) TYPEOF(rhs);
  const R_xlen_t size = XLENGTH(values);

  if (value_type == STRSXP) {
    SEXP target = STRING_ELT(rhs, 0);
    for (R_xlen_t index = 0; index < size; ++index) {
      paradox_account_work(work_since_interrupt);
      SEXP value = STRING_ELT(values, index);
      const int equal = value != NA_STRING &&
        paradox_domain_strings_equal(value, target);
      SET_LOGICAL_ELT(result, index, equal);
    }
    return;
  }

  double target;
  if (rhs_type == REALSXP) {
    target = REAL_ELT(rhs, 0);
  } else if (rhs_type == INTSXP) {
    const int integer_target = INTEGER_ELT(rhs, 0);
    target = (double) integer_target;
  } else {
    const int logical_target = LOGICAL_ELT(rhs, 0);
    target = (double) logical_target;
  }
  if (value_type == REALSXP) {
    for (R_xlen_t index = 0; index < size; ++index) {
      paradox_account_work(work_since_interrupt);
      const double value = REAL_ELT(values, index);
      SET_LOGICAL_ELT(
        result,
        index,
        !ISNAN(value) && value == target
      );
    }
  } else {
    for (R_xlen_t index = 0; index < size; ++index) {
      paradox_account_work(work_since_interrupt);
      const int value = value_type == INTSXP
        ? INTEGER_ELT(values, index)
        : LOGICAL_ELT(values, index);
      SET_LOGICAL_ELT(
        result,
        index,
        value != NA_INTEGER && (double) value == target
      );
    }
  }
}

/* Thin R constructors may retain a compact base sequence.  Every native
 * admission boundary uses this one helper to validate the fixed outer shape,
 * root the selected RHS, and copy each semantic element once.  The strict
 * exact validator above remains the capsule validator and rejects ALTREP. */
SEXP paradox_builtin_condition_admit(SEXP condition,
    paradox_builtin_condition_kind_t *kind,
    R_xlen_t *work_since_interrupt) {
  SEXP raw_rhs = R_NilValue;
  PROTECT(condition);
  if (!condition_outer_exact(
      condition, kind, &raw_rhs, work_since_interrupt
    )) {
    UNPROTECT(1);
    return R_NilValue;
  }
  SEXP candidate_rhs = PROTECT(raw_rhs);
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(candidate_rhs);
  if ((type != LGLSXP && type != INTSXP && type != REALSXP &&
       type != STRSXP) || Rf_isObject(candidate_rhs) ||
      Rf_isS4(candidate_rhs) ||
      !paradox_api_has_no_attributes(candidate_rhs)) {
    UNPROTECT(2);
    return R_NilValue;
  }
  SEXP stable_rhs = PROTECT(paradox_snapshot_semantic_vector(candidate_rhs));
  const int exact = condition_rhs_is_plain(
    stable_rhs,
    *kind,
    work_since_interrupt
  );
  UNPROTECT(3);
  return exact ? stable_rhs : R_NilValue;
}

SEXP paradox_builtin_condition_snapshot(SEXP condition,
    R_xlen_t *work_since_interrupt) {
  paradox_builtin_condition_kind_t kind;
  PROTECT(condition);
  SEXP stable_rhs = paradox_builtin_condition_admit(
    condition,
    &kind,
    work_since_interrupt
  );
  if (stable_rhs == R_NilValue) {
    UNPROTECT(1);
    return R_UnboundValue;
  }
  PROTECT(stable_rhs);

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, stable_rhs);
  SET_VECTOR_ELT(
    result,
    1,
    Rf_mkString(kind == PARADOX_BUILTIN_CONDITION_EQUAL
      ? "%s == %s"
      : "%s %%in%% {%s}")
  );
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, Rf_mkChar("rhs"));
  SET_STRING_ELT(names, 1, Rf_mkChar("condition_format_string"));
  Rf_setAttrib(result, R_NamesSymbol, names);
  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(
    classes,
    0,
    Rf_mkChar(kind == PARADOX_BUILTIN_CONDITION_EQUAL
      ? "CondEqual"
      : "CondAnyOf")
  );
  SET_STRING_ELT(classes, 1, Rf_mkChar("Condition"));
  Rf_setAttrib(result, R_ClassSymbol, classes);
  UNPROTECT(5);
  return result;
}

static int condition_snapshot_parts(SEXP condition,
    paradox_builtin_condition_kind_t *kind, SEXP *rhs) {
  R_xlen_t work_since_interrupt = 0;
  return condition_outer_exact(
    condition,
    kind,
    rhs,
    &work_since_interrupt
  );
}

int paradox_builtin_condition_snapshot_lengths_current(
    SEXP source, SEXP snapshot) {
  paradox_builtin_condition_kind_t source_kind;
  paradox_builtin_condition_kind_t snapshot_kind;
  SEXP source_rhs = R_NilValue;
  SEXP snapshot_rhs = R_NilValue;
  if (!condition_snapshot_parts(source, &source_kind, &source_rhs) ||
      !condition_snapshot_parts(snapshot, &snapshot_kind, &snapshot_rhs) ||
      source_kind != snapshot_kind || TYPEOF(source_rhs) != TYPEOF(snapshot_rhs) ||
      Rf_isS4(source_rhs) || Rf_isObject(source_rhs) ||
      !paradox_api_has_no_attributes(source_rhs) || ALTREP(snapshot_rhs) ||
      Rf_isS4(snapshot_rhs) || Rf_isObject(snapshot_rhs) ||
      !paradox_api_has_no_attributes(snapshot_rhs)) {
    return FALSE;
  }
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(source_rhs);
  if (type != LGLSXP && type != INTSXP && type != REALSXP &&
      type != STRSXP) {
    return FALSE;
  }
  if (!ALTREP(source_rhs)) return TRUE;
  /* Length may dispatch into R.  The callback can rewrite the owning
   * Condition cell before allocating, so the owner graph alone is not a root
   * for the exact RHS generation whose method is currently executing. */
  PROTECT(source_rhs);
  PROTECT(snapshot_rhs);
  const int current = XLENGTH(source_rhs) == XLENGTH(snapshot_rhs);
  UNPROTECT(2);
  return current;
}

int paradox_builtin_condition_snapshot_is_current(
    SEXP source, SEXP snapshot) {
  paradox_builtin_condition_kind_t source_kind;
  paradox_builtin_condition_kind_t snapshot_kind;
  SEXP source_rhs = R_NilValue;
  SEXP snapshot_rhs = R_NilValue;
  if (!condition_snapshot_parts(source, &source_kind, &source_rhs) ||
      !condition_snapshot_parts(snapshot, &snapshot_kind, &snapshot_rhs) ||
      source_kind != snapshot_kind || TYPEOF(source_rhs) != TYPEOF(snapshot_rhs) ||
      Rf_isS4(source_rhs) || Rf_isObject(source_rhs) ||
      !paradox_api_has_no_attributes(source_rhs) || ALTREP(snapshot_rhs) ||
      Rf_isS4(snapshot_rhs) || Rf_isObject(snapshot_rhs) ||
      !paradox_api_has_no_attributes(snapshot_rhs)) {
    return FALSE;
  }
  return ALTREP(source_rhs)
    ? paradox_altrep_builtin_value_leaf_metadata_is_current(
        source_rhs,
        snapshot_rhs
      )
    : paradox_builtin_value_leaf_receipt_current(
        source_rhs,
        snapshot_rhs
      );
}

SEXP paradox_condition_test_builtin(SEXP condition, SEXP x) {
  static const char *const allowed_attributes[] = {"names"};
  paradox_builtin_condition_kind_t kind;
  R_xlen_t work_since_interrupt = 0;
  PROTECT(condition);
  SEXP stable_rhs = paradox_builtin_condition_admit(
    condition, &kind, &work_since_interrupt
  );
  if (stable_rhs == R_NilValue) {
    UNPROTECT(1);
    Rf_error("Malformed built-in Condition object");
  }
  PROTECT(stable_rhs);

  if (x == R_NilValue) {
    SEXP result = PROTECT(Rf_allocVector(LGLSXP, 0));
    UNPROTECT(3);
    return result;
  }
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(x);
  const int source_altrep = ALTREP(x);
  if (type != LGLSXP && type != INTSXP && type != REALSXP &&
      type != STRSXP) {
    UNPROTECT(2);
    Rf_error("Condition comparison requires a plain atomic vector");
  }
  if (Rf_isObject(x) || Rf_isS4(x)) {
    UNPROTECT(2);
    Rf_error("Condition comparison requires a plain atomic vector");
  }
  /*
   * Length may dispatch for an admitted ALTREP. Observe it before the
   * terminal bounded attribute proof so no callback can splice a malformed
   * attribute spine between that proof and the raw names selector.
   */
  const R_xlen_t initial_size = XLENGTH(x);
  if ((source_altrep &&
        ((SEXPTYPE) TYPEOF(x) != type || !ALTREP(x) ||
          Rf_isObject(x) || Rf_isS4(x))) ||
      !paradox_api_has_only_attributes(x, allowed_attributes, 1)) {
    UNPROTECT(2);
    Rf_error("Condition comparison requires a plain atomic vector");
  }
  /* Deferred-string and wrapper ALTREP names are ordinary base-R output
   * (`names(x) <- as.character(...)`), so the names shell admits ALTREP.
   * This boundary never reads a name element: the ordinary path attaches the
   * caller's own names object to the fresh result by identity, and the
   * snapshot path owns its names before observing any semantic element. */
  SEXP initial_names = paradox_api_raw_attribute(x, R_NamesSymbol);
  if (initial_names != R_NilValue &&
      (TYPEOF(initial_names) != STRSXP ||
        Rf_isS4(initial_names) || Rf_isObject(initial_names) ||
        XLENGTH(initial_names) != initial_size ||
        !paradox_api_has_no_attributes(initial_names))) {
    UNPROTECT(2);
    Rf_error("Condition comparison names are malformed");
  }
  /* A value whose type the right-hand side cannot equal simply does not
   * satisfy the Condition: this is an ordinary negative comparison result, not
   * a failure of the operation. Paradox 1 also never raised here, but it
   * compared through R's `==`, so `1 == "1"` coerced to TRUE; the closed
   * comparator deliberately does not reinterpret a value as another type. */
  /*
   * Character comparison may allocate while translating mixed encodings.
   * Own its complete vector before the first comparison so a pending finalizer
   * cannot replace later elements or names halfway through the result. ALTREP
   * operands of every admitted type require the same one-observation snapshot.
   * Ordinary numeric/logical/integer input remains zero-copy: output allocation
   * happens before its terminal structural receipt, and its comparison kernel
   * is allocation-free.
   */
  const int owns_stable = ALTREP(x) || type == STRSXP;
  SEXP stable = PROTECT(
    owns_stable ? paradox_snapshot_semantic_vector(x) : x
  );
  const R_xlen_t size = XLENGTH(stable);
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, size));
  if (!owns_stable &&
      ((SEXPTYPE) TYPEOF(x) != type || ALTREP(x) ||
        Rf_isObject(x) || Rf_isS4(x) ||
        XLENGTH(x) != initial_size || size != initial_size ||
        !paradox_api_has_only_attributes(x, allowed_attributes, 1))) {
    UNPROTECT(4);
    Rf_error("Condition comparison input changed during native evaluation");
  }
  const int comparable = compatible_operand_types(
    (SEXPTYPE) TYPEOF(stable),
    (SEXPTYPE) TYPEOF(stable_rhs)
  );
  if (!comparable) {
    for (R_xlen_t index = 0; index < size; ++index) {
      SET_LOGICAL_ELT(result, index, FALSE);
    }
  } else if (kind == PARADOX_BUILTIN_CONDITION_EQUAL) {
    condition_equal_vector(
      stable, stable_rhs, result, &work_since_interrupt
    );
  } else {
    for (R_xlen_t index = 0; index < size; ++index) {
      paradox_account_work(&work_since_interrupt);
      const int matches = paradox_builtin_condition_element_matches(
        stable, index, stable_rhs, &work_since_interrupt
      );
      SET_LOGICAL_ELT(result, index, matches);
    }
  }
  SEXP names = PROTECT(paradox_api_raw_attribute(
    stable,
    R_NamesSymbol
  ));
  if (names != R_NilValue) {
    if (TYPEOF(names) != STRSXP || Rf_isS4(names) ||
        Rf_isObject(names) || XLENGTH(names) != size ||
        !paradox_api_has_no_attributes(names)) {
      UNPROTECT(5);
      Rf_error("Condition comparison names are malformed");
    }
    Rf_setAttrib(result, R_NamesSymbol, names);
  }
  UNPROTECT(5);
  return result;
}
