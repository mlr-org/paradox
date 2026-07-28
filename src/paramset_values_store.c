#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "paramset_domain_common.h"
#include "paramset_params_internal.h"
#include "parameter_suggestion.h"
#include "r_api_compat.h"
#include "r_utils.h"
#include "core_state.h"

typedef struct {
  SEXP params;
  SEXP ids;
  R_xlen_t size;
} value_param_state_t;

static SEXP value_core_symbol = NULL;

static SEXP initialize_value_core_symbol(void) {
  if (value_core_symbol == NULL) value_core_symbol = Rf_install(".core");
  return value_core_symbol;
}

static SEXP run_value_transaction(SEXP private_environment, SEXP self,
  SEXP values, int validate);

static int exact_flag(SEXP value, int *result) {
  if (TYPEOF(value) != LGLSXP || ALTREP(value) || XLENGTH(value) != 1 ||
      !paradox_api_has_no_attributes(value)) {
    return FALSE;
  }
  const int flag = LOGICAL_ELT(value, 0);
  if (flag == NA_LOGICAL) {
    return FALSE;
  }
  *result = flag;
  return TRUE;
}

static void shadow_parameter_unavailable(SEXP id, SEXP candidate_ids) {
  PROTECT(id);
  if (Rf_getCharCE(id) == CE_BYTES) {
    UNPROTECT(1);
    Rf_error("Unknown bytes-encoded parameter ID");
  }
  SEXP message = PROTECT(paradox_parameter_unavailable_diagnostic(
    id,
    candidate_ids,
    " in ParamSetShadow",
    FALSE
  ));
  paradox_error_from_scalar_string(message);
}

static int plain_list(SEXP value, SEXP *names, R_xlen_t *size) {
  if (TYPEOF(value) != VECSXP || Rf_isS4(value)) {
    return FALSE;
  }
  *size = XLENGTH(value);
  SEXP observed_names = PROTECT(ALTREP(value)
    ? paradox_stored_attribute(value, R_NamesSymbol)
    : Rf_getAttrib(value, R_NamesSymbol));
  SEXP observed_class = PROTECT(ALTREP(value)
    ? paradox_stored_attribute(value, R_ClassSymbol)
    : Rf_getAttrib(value, R_ClassSymbol));
  *names = observed_names;
  int plain = observed_class == R_NilValue ||
    (TYPEOF(observed_class) == STRSXP && !ALTREP(observed_class) &&
      paradox_api_has_no_attributes(observed_class));
  if (plain && observed_names == R_NilValue) {
    static const char *const allowed[] = {"class"};
    plain = *size == 0 &&
      paradox_api_has_only_attributes(value, allowed, 1);
  } else if (plain) {
    static const char *const allowed[] = {"names", "class"};
    plain = TYPEOF(observed_names) == STRSXP && !ALTREP(observed_names) &&
      !Rf_isS4(observed_names) && !Rf_isObject(observed_names) &&
      paradox_api_has_no_attributes(observed_names) &&
      XLENGTH(observed_names) == *size &&
      paradox_api_has_only_attributes(value, allowed, 2);
  }
  UNPROTECT(2);
  return plain;
}

static int ordinary_plain_list(SEXP value, SEXP *names, R_xlen_t *size) {
  if (TYPEOF(value) != VECSXP || ALTREP(value) || Rf_isObject(value) ||
      !plain_list(value, names, size)) {
    return FALSE;
  }
  return *names == R_NilValue || !ALTREP(*names);
}

static SEXP materialize_altrep_value(SEXP value,
    R_xlen_t *work_since_interrupt) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  const R_xlen_t size = XLENGTH(value);
  SEXP result = PROTECT(Rf_allocVector(type, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_account_work(work_since_interrupt);
    switch (type) {
    case LGLSXP:
      SET_LOGICAL_ELT(result, index, LOGICAL_ELT(value, index));
      break;
    case INTSXP:
      SET_INTEGER_ELT(result, index, INTEGER_ELT(value, index));
      break;
    case REALSXP:
      SET_REAL_ELT(result, index, REAL_ELT(value, index));
      break;
    case CPLXSXP:
      paradox_api_set_complex_elt(
        result,
        index,
        COMPLEX_ELT(value, index)
      );
      break;
    case STRSXP:
      SET_STRING_ELT(result, index, STRING_ELT(value, index));
      break;
    case RAWSXP:
      paradox_api_set_raw_elt(result, index, RAW_ELT(value, index));
      break;
    case VECSXP:
      SET_VECTOR_ELT(result, index, VECTOR_ELT(value, index));
      break;
    default:
      UNPROTECT(1);
      Rf_error("Unsupported ALTREP parameter value type");
    }
  }
  SHALLOW_DUPLICATE_ATTRIB(result, value);
  UNPROTECT(1);
  return result;
}

/* Materialize an ordinary list shell and names vector. The copied shell is a
 * stable decision source for multi-pass merge/store planning and independently
 * roots every element returned by an ALTREP list method. */
static SEXP snapshot_plain_list(SEXP value, SEXP names, R_xlen_t size,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(value) != VECSXP ||
      (names != R_NilValue && TYPEOF(names) != STRSXP)) {
    return R_NilValue;
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  SEXP stable_names = R_NilValue;
  int protected_count = 1;
  if (names != R_NilValue) {
    stable_names = PROTECT(Rf_allocVector(STRSXP, size));
    ++protected_count;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP element = PROTECT(VECTOR_ELT(value, index));
    SEXP stable_element = PROTECT(ALTREP(element)
      ? materialize_altrep_value(element, work_since_interrupt)
      : element);
    SET_VECTOR_ELT(result, index, stable_element);
    UNPROTECT(2);
    if (stable_names != R_NilValue) {
      SET_STRING_ELT(stable_names, index, STRING_ELT(names, index));
    }
  }
  if (stable_names != R_NilValue) {
    Rf_setAttrib(result, R_NamesSymbol, stable_names);
  }
  UNPROTECT(protected_count);
  return result;
}

static int names_are_unique(SEXP names, R_xlen_t size,
    R_xlen_t *work_since_interrupt) {
  if (size == 0) {
    return TRUE;
  }
  if (TYPEOF(names) != STRSXP || XLENGTH(names) != size) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_account_work(work_since_interrupt);
    if (STRING_ELT(names, index) == NA_STRING) {
      return FALSE;
    }
  }
  return Rf_any_duplicated(names, FALSE) == 0;
}

static R_xlen_t find_name(SEXP names, SEXP sought,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t size = names == R_NilValue ? 0 : XLENGTH(names);
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_account_work(work_since_interrupt);
    if (paradox_domain_strings_equal(STRING_ELT(names, index), sought)) {
      return index;
    }
  }
  return -1;
}

static SEXP update_name(SEXP dot_names, SEXP value_names,
    R_xlen_t dot_size, R_xlen_t index) {
  return index < dot_size
    ? STRING_ELT(dot_names, index)
    : STRING_ELT(value_names, index - dot_size);
}

static SEXP update_value(SEXP dots, SEXP values,
    R_xlen_t dot_size, R_xlen_t index) {
  return index < dot_size
    ? VECTOR_ELT(dots, index)
    : VECTOR_ELT(values, index - dot_size);
}

static int disjoint_names(SEXP left, SEXP right,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t left_size = left == R_NilValue ? 0 : XLENGTH(left);
  for (R_xlen_t left_index = 0;
      left_index < left_size;
      ++left_index) {
    if (find_name(
        right,
        STRING_ELT(left, left_index),
        work_since_interrupt
      ) >= 0) {
      return FALSE;
    }
  }
  return TRUE;
}

static void load_base_param_state(SEXP private_environment, SEXP self,
    value_param_state_t *state, R_xlen_t *work_since_interrupt) {
  if (paradox_core_kind(paradox_core_from_private(private_environment)) !=
        PARADOX_CORE_BASE ||
      TYPEOF(private_environment) != ENVSXP ||
      !paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("Corrupt or unsupported ParamSet value-store capsule");
  }
  state->params = PROTECT(paradox_domain_local_value(
    private_environment,
    ".params"
  ));
  SEXP current_values = PROTECT(paradox_domain_local_value(
    private_environment,
    ".values"
  ));
  paradox_domain_params_t checked;
  R_xlen_t unused_row = 0;
  const int valid = state->params != R_UnboundValue &&
    current_values != R_UnboundValue &&
    paradox_params_supported_table_attributes(state->params) &&
    paradox_domain_validate_params(
      state->params,
      R_NilValue,
      TRUE,
      &checked,
      &unused_row,
      work_since_interrupt
    ) && checked.row_count <= INT_MAX;
  if (!valid) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSet value-store state");
  }
  state->ids = checked.ids;
  state->size = checked.row_count;
  UNPROTECT(2);
}

static SEXP ordered_values(SEXP ids, SEXP values,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(values) != VECSXP) {
    Rf_error("Internal error: unstable ParamSet value write plan");
  }
  const R_xlen_t original_size = XLENGTH(values);
  SEXP value_names = Rf_getAttrib(values, R_NamesSymbol);
  PROTECT(value_names);
  if (ALTREP(values) || Rf_isObject(values) ||
      TYPEOF(value_names) != STRSXP || ALTREP(value_names) ||
      XLENGTH(value_names) != original_size || original_size > INT_MAX) {
    UNPROTECT(1);
    Rf_error("Internal error: unstable ParamSet value write plan");
  }
  const R_xlen_t id_size = XLENGTH(ids);

  if (!names_are_unique(value_names, original_size, work_since_interrupt)) {
    UNPROTECT(1);
    Rf_error("ParamSet value names must be unique and non-missing");
  }

  /* Rf_match(table, x, nomatch): map every parameter id (`x`) to its
   * position in the supplied value names (`table`). */
  SEXP matches = PROTECT(Rf_match(value_names, ids, 0));
  if (TYPEOF(matches) != INTSXP || XLENGTH(matches) != id_size) {
    UNPROTECT(2);
    Rf_error("Internal error: invalid ParamSet value match");
  }
  R_xlen_t output_size = 0;
  for (R_xlen_t row = 0; row < id_size; ++row) {
    paradox_account_work(work_since_interrupt);
    if (INTEGER_ELT(matches, row) > 0) {
      ++output_size;
    }
  }
  SEXP result = PROTECT(Rf_allocVector(VECSXP, output_size));
  SEXP result_names = PROTECT(Rf_allocVector(STRSXP, output_size));
  R_xlen_t output = 0;
  for (R_xlen_t row = 0; row < id_size; ++row) {
    paradox_account_work(work_since_interrupt);
    const int matched = INTEGER_ELT(matches, row);
    if (matched > 0) {
      const R_xlen_t source = (R_xlen_t) matched - 1;
      if (source >= original_size || output >= output_size) {
        UNPROTECT(4);
        Rf_error("Internal error: ordered values exceeded its capacity");
      }
      SET_VECTOR_ELT(result, output, VECTOR_ELT(values, source));
      SET_STRING_ELT(
        result_names,
        output,
        STRING_ELT(value_names, source)
      );
      ++output;
    }
  }
  if (output != output_size) {
    UNPROTECT(4);
    Rf_error("Internal error: incomplete ordered values output");
  }
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  UNPROTECT(4);
  return result;
}

SEXP paradox_param_set_values_merge(SEXP dots, SEXP values,
    SEXP current, SEXP insert_sexp) {
  R_xlen_t work_since_interrupt = 0;
  int insert = FALSE;
  int protected_count = 0;
  SEXP dot_names = R_NilValue;
  SEXP value_names = R_NilValue;
  SEXP current_names = R_NilValue;
  R_xlen_t original_dot_size = 0;
  R_xlen_t original_value_size = 0;
  R_xlen_t current_size = 0;
  if (!exact_flag(insert_sexp, &insert)) {
    Rf_error("`.insert` must be TRUE or FALSE");
  }
  if (!plain_list(dots, &dot_names, &original_dot_size)) {
    Rf_error("`...` values must be a plain named list");
  }
  PROTECT(dot_names);
  ++protected_count;
  if (!plain_list(values, &value_names, &original_value_size)) {
    UNPROTECT(protected_count);
    Rf_error("`.values` must be a plain named list");
  }
  PROTECT(value_names);
  ++protected_count;
  if (insert) {
    if (!plain_list(current, &current_names, &current_size)) {
      UNPROTECT(protected_count);
      Rf_error("Current ParamSet values are corrupt");
    }
    PROTECT(current_names);
    ++protected_count;
  }

  SEXP stable_dots = PROTECT(snapshot_plain_list(
    dots,
    dot_names,
    original_dot_size,
    &work_since_interrupt
  ));
  ++protected_count;
  SEXP stable_values = PROTECT(snapshot_plain_list(
    values,
    value_names,
    original_value_size,
    &work_since_interrupt
  ));
  ++protected_count;
  if (stable_dots == R_NilValue || stable_values == R_NilValue ||
      XLENGTH(stable_dots) != original_dot_size ||
      XLENGTH(stable_values) != original_value_size) {
    UNPROTECT(protected_count);
    Rf_error("ParamSet value inputs changed while being admitted");
  }
  dots = stable_dots;
  values = stable_values;
  dot_names = Rf_getAttrib(dots, R_NamesSymbol);
  value_names = Rf_getAttrib(values, R_NamesSymbol);
  if (!names_are_unique(
      dot_names,
      XLENGTH(dots),
      &work_since_interrupt
    ) || !names_are_unique(
      value_names,
      XLENGTH(values),
      &work_since_interrupt
    ) || !disjoint_names(
      dot_names,
      value_names,
      &work_since_interrupt
    )) {
    UNPROTECT(protected_count);
    Rf_error("ParamSet value inputs must have unique, disjoint names");
  }

  if (insert) {
    SEXP stable_current = PROTECT(snapshot_plain_list(
      current,
      current_names,
      current_size,
      &work_since_interrupt
    ));
    ++protected_count;
    if (stable_current == R_NilValue) {
      UNPROTECT(protected_count);
      Rf_error("Current ParamSet values changed while being admitted");
    }
    current = stable_current;
    current_names = Rf_getAttrib(current, R_NamesSymbol);
    if (!names_are_unique(
        current_names,
        XLENGTH(current),
        &work_since_interrupt
      )) {
      UNPROTECT(protected_count);
      Rf_error("Current ParamSet values have invalid names");
    }
  }
  const R_xlen_t dot_size = original_dot_size;
  const R_xlen_t value_size = original_value_size;
  if (dot_size > R_XLEN_T_MAX - value_size) {
    UNPROTECT(protected_count);
    Rf_error("ParamSet value update is too large");
  }
  const R_xlen_t update_size = dot_size + value_size;

  if (!insert) {
    SEXP result = PROTECT(Rf_allocVector(VECSXP, update_size));
    if (update_size == 0) {
      UNPROTECT(protected_count + 1);
      return result;
    }
    SEXP result_names = PROTECT(Rf_allocVector(STRSXP, update_size));
    for (R_xlen_t index = 0; index < update_size; ++index) {
      paradox_account_work(&work_since_interrupt);
      SET_VECTOR_ELT(
        result,
        index,
        update_value(dots, values, dot_size, index)
      );
      SET_STRING_ELT(
        result_names,
        index,
        update_name(dot_names, value_names, dot_size, index)
      );
    }
    Rf_setAttrib(result, R_NamesSymbol, result_names);
    UNPROTECT(protected_count + 2);
    return result;
  }

  if (current_size > R_XLEN_T_MAX - update_size) {
    UNPROTECT(protected_count);
    Rf_error("Merged ParamSet values are too large");
  }
  R_xlen_t output_size = 0;
  for (R_xlen_t index = 0; index < current_size; ++index) {
    paradox_account_work(&work_since_interrupt);
    const R_xlen_t update = find_name(
      dot_names,
      STRING_ELT(current_names, index),
      &work_since_interrupt
    );
    const R_xlen_t second_update = update >= 0 ? -1 : find_name(
      value_names,
      STRING_ELT(current_names, index),
      &work_since_interrupt
    );
    const R_xlen_t combined = update >= 0
      ? update
      : second_update >= 0 ? dot_size + second_update : -1;
    if (combined < 0 ||
        update_value(dots, values, dot_size, combined) != R_NilValue) {
      ++output_size;
    }
  }
  for (R_xlen_t update = 0; update < update_size; ++update) {
    paradox_account_work(&work_since_interrupt);
    if (update_value(dots, values, dot_size, update) != R_NilValue &&
        find_name(
          current_names,
          update_name(dot_names, value_names, dot_size, update),
          &work_since_interrupt
        ) < 0) {
      ++output_size;
    }
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, output_size));
  SEXP result_names = PROTECT(Rf_allocVector(STRSXP, output_size));
  R_xlen_t output = 0;
  for (R_xlen_t index = 0; index < current_size; ++index) {
    paradox_account_work(&work_since_interrupt);
    R_xlen_t update = find_name(
      dot_names,
      STRING_ELT(current_names, index),
      &work_since_interrupt
    );
    if (update < 0) {
      const R_xlen_t value_update = find_name(
        value_names,
        STRING_ELT(current_names, index),
        &work_since_interrupt
      );
      if (value_update >= 0) {
        update = dot_size + value_update;
      }
    }
    SEXP replacement = update >= 0
      ? update_value(dots, values, dot_size, update)
      : VECTOR_ELT(current, index);
    if (update < 0 || replacement != R_NilValue) {
      if (output >= output_size) {
        UNPROTECT(protected_count + 2);
        Rf_error("Internal error: values merge exceeded its capacity");
      }
      SET_VECTOR_ELT(result, output, replacement);
      SET_STRING_ELT(
        result_names,
        output,
        STRING_ELT(current_names, index)
      );
      ++output;
    }
  }
  for (R_xlen_t update = 0; update < update_size; ++update) {
    paradox_account_work(&work_since_interrupt);
    SEXP name = update_name(dot_names, value_names, dot_size, update);
    SEXP replacement = update_value(dots, values, dot_size, update);
    if (replacement != R_NilValue && find_name(
        current_names,
        name,
        &work_since_interrupt
      ) < 0) {
      if (output >= output_size) {
        UNPROTECT(protected_count + 2);
        Rf_error("Internal error: values merge exceeded its capacity");
      }
      SET_VECTOR_ELT(result, output, replacement);
      SET_STRING_ELT(result_names, output, name);
      ++output;
    }
  }
  if (output != output_size) {
    UNPROTECT(protected_count + 2);
    Rf_error("Internal error: incomplete values merge output");
  }
  Rf_setAttrib(result, R_NamesSymbol, result_names);
  UNPROTECT(protected_count + 2);
  return result;
}

SEXP paradox_param_set_store_values(SEXP private_environment, SEXP self,
    SEXP values) {
  return run_value_transaction(private_environment, self, values, FALSE);
}

SEXP paradox_param_set_assign_values_checked(SEXP private_environment,
    SEXP self, SEXP values) {
  return run_value_transaction(private_environment, self, values, TRUE);
}

static int supported_ascii(SEXP value) {
  if (value == NA_STRING || Rf_getCharCE(value) == CE_BYTES) {
    return FALSE;
  }
  const unsigned char *text = (const unsigned char *) CHAR(value);
  for (; *text != '\0'; ++text) {
    if (*text >= 0x80U) {
      return FALSE;
    }
  }
  return TRUE;
}

static int translated_id_matches(SEXP translated, SEXP original,
    SEXP owner, int postfix) {
  if (!supported_ascii(translated) || !supported_ascii(original) ||
      !supported_ascii(owner)) {
    return FALSE;
  }
  const char *translated_text = CHAR(translated);
  const char *original_text = CHAR(original);
  const char *owner_text = CHAR(owner);
  const size_t original_size = strlen(original_text);
  const size_t owner_size = strlen(owner_text);
  if (owner_size == 0) {
    return strcmp(translated_text, original_text) == 0;
  }
  const size_t translated_size = strlen(translated_text);
  if (original_size > SIZE_MAX - owner_size - 1U ||
      translated_size != original_size + owner_size + 1U) {
    return FALSE;
  }
  if (postfix) {
    return memcmp(translated_text, original_text, original_size) == 0 &&
      translated_text[original_size] == '.' &&
      memcmp(
        translated_text + original_size + 1U,
        owner_text,
        owner_size
      ) == 0;
  }
  return memcmp(translated_text, owner_text, owner_size) == 0 &&
    translated_text[owner_size] == '.' &&
    memcmp(
      translated_text + owner_size + 1U,
      original_text,
      original_size
    ) == 0;
}

static int exact_translation(SEXP translation, SEXP sets, int postfix,
    R_xlen_t *work_since_interrupt) {
  static const char *const column_names[] = {
    "id", "original_id", "owner_ps_index", "owner_name"
  };
  R_xlen_t table_row_count = 0;
  if (!paradox_domain_exact_plain_table(
      translation,
      column_names,
      4,
      &table_row_count,
      work_since_interrupt
    )) {
    return FALSE;
  }
  SEXP ids = PROTECT(VECTOR_ELT(translation, 0));
  SEXP originals = PROTECT(VECTOR_ELT(translation, 1));
  SEXP owners = PROTECT(VECTOR_ELT(translation, 2));
  SEXP owner_names = PROTECT(VECTOR_ELT(translation, 3));
  const R_xlen_t row_count = XLENGTH(ids);
  if (TYPEOF(ids) != STRSXP || TYPEOF(originals) != STRSXP ||
      TYPEOF(owners) != INTSXP || TYPEOF(owner_names) != STRSXP ||
      ALTREP(ids) || ALTREP(originals) || ALTREP(owners) ||
      ALTREP(owner_names) ||
      !paradox_api_has_no_attributes(ids) ||
      !paradox_api_has_no_attributes(originals) ||
      !paradox_api_has_no_attributes(owners) ||
      !paradox_api_has_no_attributes(owner_names) ||
      XLENGTH(originals) != row_count || XLENGTH(owners) != row_count ||
      XLENGTH(owner_names) != row_count || table_row_count != row_count ||
      row_count > INT_MAX ||
      Rf_any_duplicated(ids, FALSE) != 0) {
    UNPROTECT(4);
    return FALSE;
  }
  const R_xlen_t set_count = XLENGTH(sets);
  SEXP set_names = PROTECT(Rf_getAttrib(sets, R_NamesSymbol));
  if (TYPEOF(set_names) != STRSXP || ALTREP(set_names) ||
      XLENGTH(set_names) != set_count) {
    UNPROTECT(5);
    return FALSE;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    const int owner = INTEGER_ELT(owners, row);
    if (owner == NA_INTEGER || owner < 1 ||
        (R_xlen_t) owner > set_count ||
        STRING_ELT(owner_names, row) == NA_STRING ||
        !paradox_domain_strings_equal(
          STRING_ELT(owner_names, row),
          STRING_ELT(set_names, (R_xlen_t) owner - 1)
        ) || !translated_id_matches(
          STRING_ELT(ids, row),
          STRING_ELT(originals, row),
          STRING_ELT(owner_names, row),
          postfix
        )) {
      UNPROTECT(5);
      return FALSE;
    }
  }
  UNPROTECT(5);
  return TRUE;
}

/* Revalidate and materialize only the translation columns used by store
 * planning. Every later count/fill operation reads these ordinary snapshots,
 * never the callback-capable table that was admitted above. */
static SEXP snapshot_store_translation(SEXP translation, SEXP set_names,
    R_xlen_t child_count, int postfix,
    R_xlen_t *work_since_interrupt) {
  SEXP ids = PROTECT(VECTOR_ELT(translation, 0));
  SEXP originals = PROTECT(VECTOR_ELT(translation, 1));
  SEXP owners = PROTECT(VECTOR_ELT(translation, 2));
  SEXP owner_names = PROTECT(VECTOR_ELT(translation, 3));
  const R_xlen_t row_count = XLENGTH(ids);
  if (TYPEOF(ids) != STRSXP || TYPEOF(originals) != STRSXP ||
      TYPEOF(owners) != INTSXP || TYPEOF(owner_names) != STRSXP ||
      XLENGTH(originals) != row_count || XLENGTH(owners) != row_count ||
      XLENGTH(owner_names) != row_count || row_count > INT_MAX) {
    UNPROTECT(4);
    return R_NilValue;
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP stable_ids = PROTECT(Rf_allocVector(STRSXP, row_count));
  SET_VECTOR_ELT(result, 0, stable_ids);
  UNPROTECT(1);
  SEXP stable_originals = PROTECT(Rf_allocVector(STRSXP, row_count));
  SET_VECTOR_ELT(result, 1, stable_originals);
  UNPROTECT(1);
  SEXP stable_owners = PROTECT(Rf_allocVector(INTSXP, row_count));
  SET_VECTOR_ELT(result, 2, stable_owners);
  UNPROTECT(1);

  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    SEXP id = STRING_ELT(ids, row);
    SEXP original = STRING_ELT(originals, row);
    SEXP owner_name = STRING_ELT(owner_names, row);
    const int owner = INTEGER_ELT(owners, row);
    if (id == NA_STRING || original == NA_STRING || owner_name == NA_STRING ||
        owner == NA_INTEGER || owner < 1 ||
        (R_xlen_t) owner > child_count ||
        !paradox_domain_strings_equal(
          owner_name,
          STRING_ELT(set_names, (R_xlen_t) owner - 1)
        ) || !translated_id_matches(
          id,
          original,
          owner_name,
          postfix
        )) {
      UNPROTECT(5);
      return R_NilValue;
    }
    SET_STRING_ELT(stable_ids, row, id);
    SET_STRING_ELT(stable_originals, row, original);
    SET_INTEGER_ELT(stable_owners, row, owner);
  }

  UNPROTECT(5);
  return result;
}

static SEXP param_set_collection_store_plan(SEXP private_environment,
    SEXP self, SEXP sets, SEXP values) {
  R_xlen_t work_since_interrupt = 0;
  int protected_count = 0;
  SEXP set_names = R_NilValue;
  SEXP value_names = R_NilValue;
  R_xlen_t original_child_count = 0;
  R_xlen_t original_value_count = 0;
  int postfix = FALSE;
  SEXP core = PROTECT(paradox_core_from_private(private_environment));
  ++protected_count;
  if (paradox_core_kind(core) != PARADOX_CORE_COLLECTION ||
      !paradox_domain_owns_private_environment(self, private_environment)) {
    Rf_error("Corrupt or unsupported ParamSetCollection value-store capsule");
  }
  SEXP state = PROTECT(paradox_core_payload(core));
  ++protected_count;
  if (!ordinary_plain_list(sets, &set_names, &original_child_count) ||
      set_names == R_NilValue) {
    Rf_error("Corrupt ParamSetCollection child state");
  }
  PROTECT(set_names);
  ++protected_count;
  if (original_child_count > INT_MAX ||
      !ordinary_plain_list(values, &value_names, &original_value_count)) {
    UNPROTECT(protected_count);
    Rf_error("ParamSetCollection values must be supplied as a plain list");
  }
  PROTECT(value_names);
  ++protected_count;
  if (original_value_count > INT_MAX) {
    UNPROTECT(protected_count);
    Rf_error("ParamSetCollection values exceed supported size");
  }

  SEXP current_sets = PROTECT(VECTOR_ELT(state, PARADOX_CORE_SETS));
  ++protected_count;
  SEXP translation = PROTECT(VECTOR_ELT(
    state,
    PARADOX_CORE_TRANSLATION
  ));
  ++protected_count;
  SEXP postfix_sexp = PROTECT(VECTOR_ELT(state, PARADOX_CORE_POSTFIX));
  ++protected_count;
  if (current_sets != sets || translation == R_NilValue ||
      !exact_flag(postfix_sexp, &postfix)) {
    UNPROTECT(protected_count);
    Rf_error("Corrupt ParamSetCollection translation state");
  }
  if (
      !exact_translation(
        translation,
        sets,
        postfix,
        &work_since_interrupt
      )) {
    UNPROTECT(protected_count);
    Rf_error("Corrupt ParamSetCollection translation table");
  }

  const R_xlen_t child_count = original_child_count;

  for (R_xlen_t child = 0; child < child_count; ++child) {
    paradox_account_work(&work_since_interrupt);
    SEXP child_set = VECTOR_ELT(sets, child);
    if (TYPEOF(child_set) != ENVSXP ||
        !Rf_inherits(child_set, "ParamSet")) {
      UNPROTECT(protected_count);
      Rf_error("Corrupt ParamSetCollection child reference");
    }
  }

  SEXP translation_snapshot = PROTECT(snapshot_store_translation(
    translation,
    set_names,
    child_count,
    postfix,
    &work_since_interrupt
  ));
  ++protected_count;
  if (translation_snapshot == R_NilValue) {
    UNPROTECT(protected_count);
    Rf_error("Corrupt ParamSetCollection translation snapshot");
  }
  SEXP translation_ids = VECTOR_ELT(translation_snapshot, 0);
  SEXP originals = VECTOR_ELT(translation_snapshot, 1);
  SEXP owners = VECTOR_ELT(translation_snapshot, 2);
  const R_xlen_t translation_rows = XLENGTH(translation_ids);
  const R_xlen_t value_count = XLENGTH(values);

  SEXP matches;
  if (value_names == R_NilValue) {
    matches = PROTECT(Rf_allocVector(INTSXP, value_count));
    for (R_xlen_t index = 0; index < value_count; ++index) {
      SET_INTEGER_ELT(matches, index, 0);
    }
  } else {
    /* Map each supplied value name (`x`) into the translated-id table. */
    matches = PROTECT(Rf_match(translation_ids, value_names, 0));
  }
  ++protected_count;
  if (TYPEOF(matches) != INTSXP ||
      XLENGTH(matches) != value_count) {
    UNPROTECT(protected_count);
    Rf_error("Internal error: invalid ParamSetCollection value match");
  }

  R_xlen_t *counts = paradox_temporary_alloc(
    child_count == 0 ? 1 : child_count,
    sizeof(*counts)
  );
  R_xlen_t *positions = paradox_temporary_alloc(
    child_count == 0 ? 1 : child_count,
    sizeof(*positions)
  );
  R_xlen_t *filled = paradox_temporary_alloc(
    child_count == 0 ? 1 : child_count,
    sizeof(*filled)
  );
  for (R_xlen_t child = 0; child < child_count; ++child) {
    counts[child] = 0;
    positions[child] = 0;
    filled[child] = 0;
  }
  for (R_xlen_t index = 0; index < value_count; ++index) {
    paradox_account_work(&work_since_interrupt);
    const int matched = INTEGER_ELT(matches, index);
    if (matched > 0) {
      const R_xlen_t row = (R_xlen_t) matched - 1;
      if (row >= translation_rows) {
        UNPROTECT(protected_count);
        Rf_error("Internal error: invalid collection store match");
      }
      const int owner = INTEGER_ELT(owners, row);
      if (owner < 1 || (R_xlen_t) owner > child_count) {
        UNPROTECT(protected_count);
        Rf_error("Internal error: invalid collection store owner");
      }
      ++counts[(R_xlen_t) owner - 1];
    }
  }

  SEXP order = PROTECT(Rf_allocVector(INTSXP, child_count));
  ++protected_count;
  SEXP assignments = PROTECT(Rf_allocVector(VECSXP, child_count));
  ++protected_count;
  R_xlen_t output = 0;
  for (int touched = TRUE; touched >= FALSE; --touched) {
    for (R_xlen_t child = 0; child < child_count; ++child) {
      paradox_account_work(&work_since_interrupt);
      if ((counts[child] != 0) == touched) {
        if (output >= child_count) {
          UNPROTECT(protected_count);
          Rf_error("Internal error: collection store plan exceeded capacity");
        }
        positions[child] = output;
        SET_INTEGER_ELT(order, output, (int) child + 1);
        SEXP assignment = PROTECT(Rf_allocVector(
          VECSXP,
          counts[child]
        ));
        SEXP assignment_names = PROTECT(Rf_allocVector(
          STRSXP,
          counts[child]
        ));
        Rf_setAttrib(assignment, R_NamesSymbol, assignment_names);
        SET_VECTOR_ELT(assignments, output, assignment);
        UNPROTECT(2);
        ++output;
      }
    }
  }
  if (output != child_count) {
    UNPROTECT(protected_count);
    Rf_error("Internal error: incomplete collection store child plan");
  }

  for (R_xlen_t index = 0; index < value_count; ++index) {
    paradox_account_work(&work_since_interrupt);
    const int matched = INTEGER_ELT(matches, index);
    if (matched > 0) {
      const R_xlen_t row = (R_xlen_t) matched - 1;
      if (row >= translation_rows) {
        UNPROTECT(protected_count);
        Rf_error("Internal error: invalid collection store match");
      }
      const int owner = INTEGER_ELT(owners, row);
      if (owner < 1 || (R_xlen_t) owner > child_count) {
        UNPROTECT(protected_count);
        Rf_error("Internal error: invalid collection store owner");
      }
      const R_xlen_t child = (R_xlen_t) owner - 1;
      SEXP assignment = VECTOR_ELT(assignments, positions[child]);
      SEXP assignment_names = Rf_getAttrib(assignment, R_NamesSymbol);
      const R_xlen_t destination = filled[child];
      if (destination >= counts[child]) {
        UNPROTECT(protected_count);
        Rf_error("Internal error: collection child assignment overflow");
      }
      SET_VECTOR_ELT(
        assignment,
        destination,
        VECTOR_ELT(values, index)
      );
      SET_STRING_ELT(
        assignment_names,
        destination,
        STRING_ELT(originals, row)
      );
      ++filled[child];
    }
  }
  for (R_xlen_t child = 0; child < child_count; ++child) {
    if (filled[child] != counts[child]) {
      UNPROTECT(protected_count);
      Rf_error("Internal error: incomplete collection child assignment");
    }
  }

  SEXP plan = PROTECT(Rf_allocVector(VECSXP, 2));
  ++protected_count;
  SET_VECTOR_ELT(plan, 0, order);
  SET_VECTOR_ELT(plan, 1, assignments);
  UNPROTECT(protected_count);
  return plan;
}

/*
 * Value assignment is one graph transaction. Planning recursively resolves
 * COLLECTION routes and SHADOW origins before any validation callback runs.
 * Every final BASE target retains its selected capsule generation. The engine
 * then validates once, prebuilds every replacement capsule, checks all target
 * generations, and performs a callback/allocation-free binding swap wave.
 *
 * A shared target may be reached along several DAG paths. Processing follows
 * the former depth-first child-store order; a later path replaces the earlier
 * complete assignment in the target table. This preserves deterministic
 * last-owner semantics without transiently committing either version.
 */

typedef struct value_write_path value_write_path_t;

struct value_write_path {
  SEXP self;
  const value_write_path_t *parent;
};

typedef struct {
  SEXP self;
  SEXP expected_private;
  SEXP expected_core;
  SEXP values;
  SEXP sources;
  const value_write_path_t *path;
} value_write_task_t;

typedef struct {
  SEXP self;
  SEXP private_environment;
  SEXP expected_core;
  SEXP values;
  SEXP sources;
  SEXP replacement_core;
} value_write_target_t;

typedef struct {
  SEXP *roots;
  PROTECT_INDEX roots_index;
  value_write_task_t *tasks;
  R_xlen_t task_count;
  R_xlen_t task_capacity;
  value_write_target_t *targets;
  R_xlen_t target_count;
  R_xlen_t target_capacity;
  SEXP root_private;
  paradox_core_kind_t root_kind;
  R_xlen_t *work_since_interrupt;
} value_write_transaction_t;

static void value_transaction_retain(value_write_transaction_t *transaction,
    SEXP value) {
  PROTECT(value);
  SEXP expanded = PROTECT(Rf_cons(value, *transaction->roots));
  REPROTECT(expanded, transaction->roots_index);
  *transaction->roots = expanded;
  UNPROTECT(2);
}

static void reserve_write_tasks(value_write_transaction_t *transaction) {
  if (transaction->task_count < transaction->task_capacity) {
    return;
  }
  if (transaction->task_capacity > R_XLEN_T_MAX / 2) {
    Rf_error("ParamSet value transaction graph is too large");
  }
  const R_xlen_t capacity = transaction->task_capacity * 2;
  value_write_task_t *tasks = paradox_temporary_alloc(
    capacity,
    sizeof(*tasks)
  );
  memcpy(
    tasks,
    transaction->tasks,
    (size_t) transaction->task_count * sizeof(*tasks)
  );
  transaction->tasks = tasks;
  transaction->task_capacity = capacity;
}

static void reserve_write_targets(value_write_transaction_t *transaction) {
  if (transaction->target_count < transaction->target_capacity) {
    return;
  }
  if (transaction->target_capacity > R_XLEN_T_MAX / 2) {
    Rf_error("ParamSet value transaction has too many targets");
  }
  const R_xlen_t capacity = transaction->target_capacity * 2;
  value_write_target_t *targets = paradox_temporary_alloc(
    capacity,
    sizeof(*targets)
  );
  memcpy(
    targets,
    transaction->targets,
    (size_t) transaction->target_count * sizeof(*targets)
  );
  transaction->targets = targets;
  transaction->target_capacity = capacity;
}

static int write_path_contains(const value_write_path_t *path, SEXP self) {
  for (const value_write_path_t *cursor = path;
      cursor != NULL;
      cursor = cursor->parent) {
    if (cursor->self == self) {
      return TRUE;
    }
  }
  return FALSE;
}

static const value_write_path_t *extend_write_path(
    const value_write_path_t *parent, SEXP self) {
  if (write_path_contains(parent, self)) {
    Rf_error("ParamSet value transaction graph contains a cycle");
  }
  value_write_path_t *path = paradox_temporary_alloc(1, sizeof(*path));
  path->self = self;
  path->parent = parent;
  return path;
}

static void push_write_task(value_write_transaction_t *transaction,
    SEXP self, SEXP expected_private, SEXP expected_core, SEXP values,
    SEXP sources, const value_write_path_t *parent_path) {
  if (TYPEOF(self) != ENVSXP || TYPEOF(values) != VECSXP ||
      TYPEOF(sources) != STRSXP || XLENGTH(values) != XLENGTH(sources)) {
    Rf_error("Internal error: malformed ParamSet value write task");
  }
  value_transaction_retain(transaction, self);
  value_transaction_retain(transaction, values);
  value_transaction_retain(transaction, sources);
  if (expected_private != R_NilValue) {
    value_transaction_retain(transaction, expected_private);
  }
  if (expected_core != R_NilValue) {
    value_transaction_retain(transaction, expected_core);
  }
  reserve_write_tasks(transaction);
  const value_write_path_t *path = extend_write_path(parent_path, self);
  transaction->tasks[transaction->task_count++] = (value_write_task_t) {
    self,
    expected_private,
    expected_core,
    values,
    sources,
    path
  };
}

static SEXP snapshot_transaction_values(SEXP values,
    R_xlen_t *work_since_interrupt) {
  const SEXPTYPE input_type = (SEXPTYPE) TYPEOF(values);
  const int empty_vector_type = input_type == LGLSXP ||
    input_type == INTSXP || input_type == REALSXP ||
    input_type == CPLXSXP || input_type == STRSXP ||
    input_type == RAWSXP || input_type == EXPRSXP;
  const int ordinary_empty_vector = values == R_NilValue ||
    (empty_vector_type && !ALTREP(values) && !Rf_isS4(values) &&
      !Rf_isObject(values) && paradox_api_has_no_attributes(values) &&
      XLENGTH(values) == 0);
  if (ordinary_empty_vector) {
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 0));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 0));
    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);
    return result;
  }
  SEXP names = R_NilValue;
  R_xlen_t size = 0;
  if (ALTREP(values) || !plain_list(values, &names, &size)) {
    Rf_error("ParamSet values must be supplied as a plain named list");
  }
  PROTECT(names);
  if (size != 0 && names == R_NilValue) {
    UNPROTECT(1);
    Rf_error("ParamSet values must be a named list");
  }
  SEXP result = PROTECT(snapshot_plain_list(
    values,
    names,
    size,
    work_since_interrupt
  ));
  if (result == R_NilValue || XLENGTH(result) != size) {
    UNPROTECT(2);
    Rf_error("Unable to materialize ParamSet values");
  }
  if (size == 0 && Rf_getAttrib(result, R_NamesSymbol) == R_NilValue) {
    SEXP empty_names = PROTECT(Rf_allocVector(STRSXP, 0));
    Rf_setAttrib(result, R_NamesSymbol, empty_names);
    UNPROTECT(1);
  }
  SEXP stable_names = Rf_getAttrib(result, R_NamesSymbol);
  if (!names_are_unique(stable_names, size, work_since_interrupt)) {
    UNPROTECT(2);
    Rf_error("ParamSet value names must be unique and non-missing");
  }
  UNPROTECT(2);
  return result;
}

static SEXP root_value_sources(SEXP values) {
  const R_xlen_t size = XLENGTH(values);
  SEXP names = Rf_getAttrib(values, R_NamesSymbol);
  if (size != 0 && (TYPEOF(names) != STRSXP || XLENGTH(names) != size)) {
    Rf_error("Internal error: ParamSet transaction values lost their names");
  }
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    SET_STRING_ELT(result, index, STRING_ELT(names, index));
  }
  UNPROTECT(1);
  return result;
}

static R_xlen_t target_index(const value_write_transaction_t *transaction,
    SEXP self, SEXP private_environment) {
  for (R_xlen_t index = 0; index < transaction->target_count; ++index) {
    const value_write_target_t *target = &transaction->targets[index];
    if (target->self == self ||
        target->private_environment == private_environment) {
      if (target->self != self ||
          target->private_environment != private_environment) {
        Rf_error("Corrupt ParamSet value target ownership");
      }
      return index;
    }
  }
  return R_XLEN_T_MAX;
}

static void retain_write_target(value_write_transaction_t *transaction,
    SEXP self, SEXP private_environment, SEXP expected_core, SEXP values,
    SEXP sources) {
  R_xlen_t index = target_index(transaction, self, private_environment);
  if (index != R_XLEN_T_MAX) {
    value_write_target_t *target = &transaction->targets[index];
    if (target->expected_core != expected_core) {
      Rf_error("ParamSet value target changed while planning the transaction");
    }
    target->values = values;
    target->sources = sources;
    value_transaction_retain(transaction, values);
    value_transaction_retain(transaction, sources);
    return;
  }

  value_transaction_retain(transaction, self);
  value_transaction_retain(transaction, private_environment);
  value_transaction_retain(transaction, expected_core);
  value_transaction_retain(transaction, values);
  value_transaction_retain(transaction, sources);
  reserve_write_targets(transaction);
  transaction->targets[transaction->target_count++] = (value_write_target_t) {
    self,
    private_environment,
    expected_core,
    values,
    sources,
    R_NilValue
  };
}

static SEXP ordered_value_sources(SEXP stored, SEXP input, SEXP sources,
    R_xlen_t *work_since_interrupt) {
  SEXP stored_names = Rf_getAttrib(stored, R_NamesSymbol);
  SEXP input_names = Rf_getAttrib(input, R_NamesSymbol);
  const R_xlen_t size = XLENGTH(stored);
  if (TYPEOF(stored_names) != STRSXP || XLENGTH(stored_names) != size ||
      TYPEOF(input_names) != STRSXP ||
      XLENGTH(input_names) != XLENGTH(input) ||
      TYPEOF(sources) != STRSXP || XLENGTH(sources) != XLENGTH(input)) {
    Rf_error("Internal error: malformed ParamSet value source map");
  }
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_account_work(work_since_interrupt);
    const R_xlen_t input_index = find_name(
      input_names,
      STRING_ELT(stored_names, index),
      work_since_interrupt
    );
    if (input_index < 0 || input_index >= XLENGTH(input)) {
      UNPROTECT(1);
      Rf_error("Internal error: stored ParamSet value has no source");
    }
    SET_STRING_ELT(result, index, STRING_ELT(sources, input_index));
  }
  UNPROTECT(1);
  return result;
}

static void process_base_write(value_write_transaction_t *transaction,
    const value_write_task_t *task, SEXP private_environment, SEXP core) {
  if (paradox_core_from_private(private_environment) != core) {
    Rf_error("ParamSet value target changed while being planned");
  }
  value_param_state_t state;
  load_base_param_state(
    private_environment,
    task->self,
    &state,
    transaction->work_since_interrupt
  );
  SEXP stored = PROTECT(ordered_values(
    state.ids,
    task->values,
    transaction->work_since_interrupt
  ));
  SEXP sources = PROTECT(ordered_value_sources(
    stored,
    task->values,
    task->sources,
    transaction->work_since_interrupt
  ));
  if (paradox_core_from_private(private_environment) != core) {
    UNPROTECT(2);
    Rf_error("ParamSet value target changed while being planned");
  }
  retain_write_target(
    transaction,
    task->self,
    private_environment,
    core,
    stored,
    sources
  );
  UNPROTECT(2);
}

static R_xlen_t collection_source_row(SEXP translation_ids,
    SEXP original_ids, SEXP owners, R_xlen_t child, SEXP local_name,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t rows = XLENGTH(translation_ids);
  for (R_xlen_t row = 0; row < rows; ++row) {
    paradox_account_work(work_since_interrupt);
    if (INTEGER_ELT(owners, row) == (int) (child + 1) &&
        paradox_domain_strings_equal(
          STRING_ELT(original_ids, row),
          local_name
        )) {
      return row;
    }
  }
  return R_XLEN_T_MAX;
}

static SEXP collection_child_sources(SEXP assignment, R_xlen_t child,
    SEXP translation, SEXP parent_values, SEXP parent_sources,
    R_xlen_t *work_since_interrupt) {
  SEXP assignment_names = Rf_getAttrib(assignment, R_NamesSymbol);
  SEXP parent_names = Rf_getAttrib(parent_values, R_NamesSymbol);
  SEXP translation_ids = VECTOR_ELT(translation, 0);
  SEXP original_ids = VECTOR_ELT(translation, 1);
  SEXP owners = VECTOR_ELT(translation, 2);
  const R_xlen_t size = XLENGTH(assignment);
  if (TYPEOF(assignment_names) != STRSXP ||
      XLENGTH(assignment_names) != size ||
      TYPEOF(parent_sources) != STRSXP ||
      XLENGTH(parent_sources) != XLENGTH(parent_values) ||
      TYPEOF(translation_ids) != STRSXP ||
      TYPEOF(original_ids) != STRSXP || TYPEOF(owners) != INTSXP ||
      XLENGTH(original_ids) != XLENGTH(translation_ids) ||
      XLENGTH(owners) != XLENGTH(translation_ids)) {
    Rf_error("Internal error: malformed collection value source map");
  }

  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    const R_xlen_t translation_row = collection_source_row(
      translation_ids,
      original_ids,
      owners,
      child,
      STRING_ELT(assignment_names, index),
      work_since_interrupt
    );
    if (translation_row == R_XLEN_T_MAX) {
      UNPROTECT(1);
      Rf_error("Internal error: collection child value is not translated");
    }
    const R_xlen_t parent_index = find_name(
      parent_names,
      STRING_ELT(translation_ids, translation_row),
      work_since_interrupt
    );
    if (parent_index < 0 || parent_index >= XLENGTH(parent_values)) {
      UNPROTECT(1);
      Rf_error("Internal error: collection child value has no source");
    }
    SET_STRING_ELT(
      result,
      index,
      STRING_ELT(parent_sources, parent_index)
    );
  }
  UNPROTECT(1);
  return result;
}

static void process_collection_write(value_write_transaction_t *transaction,
    const value_write_task_t *task, SEXP private_environment, SEXP core) {
  SEXP state = paradox_core_payload(core);
  if (state == R_UnboundValue) {
    Rf_error("Corrupt ParamSetCollection value transaction capsule");
  }
  SEXP sets = VECTOR_ELT(state, PARADOX_CORE_SETS);
  SEXP translation = VECTOR_ELT(state, PARADOX_CORE_TRANSLATION);
  SEXP plan = PROTECT(param_set_collection_store_plan(
    private_environment,
    task->self,
    sets,
    task->values
  ));
  if (paradox_core_from_private(private_environment) != core) {
    UNPROTECT(1);
    Rf_error("ParamSetCollection changed while planning value assignment");
  }
  if (TYPEOF(plan) != VECSXP || XLENGTH(plan) != 2) {
    UNPROTECT(1);
    Rf_error("Internal error: malformed collection value plan");
  }
  SEXP order = VECTOR_ELT(plan, 0);
  SEXP assignments = VECTOR_ELT(plan, 1);
  if (TYPEOF(order) != INTSXP || TYPEOF(assignments) != VECSXP ||
      XLENGTH(order) != XLENGTH(assignments)) {
    UNPROTECT(1);
    Rf_error("Internal error: malformed collection value plan entries");
  }
  value_transaction_retain(transaction, plan);

  for (R_xlen_t position = XLENGTH(order); position > 0; --position) {
    const R_xlen_t plan_index = position - 1;
    const int one_based_child = INTEGER_ELT(order, plan_index);
    if (one_based_child <= 0 ||
        (R_xlen_t) one_based_child > XLENGTH(sets)) {
      UNPROTECT(1);
      Rf_error("Internal error: invalid collection value plan owner");
    }
    const R_xlen_t child = (R_xlen_t) one_based_child - 1;
    SEXP child_self = VECTOR_ELT(sets, child);
    SEXP assignment = VECTOR_ELT(assignments, plan_index);
    if (TYPEOF(child_self) != ENVSXP || TYPEOF(assignment) != VECSXP) {
      UNPROTECT(1);
      Rf_error("Corrupt ParamSetCollection value plan child");
    }
    SEXP sources = PROTECT(collection_child_sources(
      assignment,
      child,
      translation,
      task->values,
      task->sources,
      transaction->work_since_interrupt
    ));
    push_write_task(
      transaction,
      child_self,
      R_NilValue,
      R_NilValue,
      assignment,
      sources,
      task->path
    );
    UNPROTECT(1);
  }
  UNPROTECT(1);
}

static SEXP current_node_values(SEXP self, SEXP private_environment,
    SEXP core, R_xlen_t *work_since_interrupt) {
  const paradox_core_kind_t kind = paradox_core_kind(core);
  SEXP values;
  if (kind == PARADOX_CORE_COLLECTION) {
    values = PROTECT(paradox_param_set_collection_values(
      private_environment,
      self
    ));
  } else {
    SEXP state = paradox_core_payload(core);
    if (state == R_UnboundValue) {
      Rf_error("Corrupt ParamSet value transaction origin capsule");
    }
    values = PROTECT(VECTOR_ELT(state, PARADOX_CORE_VALUES));
  }
  SEXP result = PROTECT(snapshot_transaction_values(
    values,
    work_since_interrupt
  ));
  if (paradox_core_from_private(private_environment) != core) {
    UNPROTECT(2);
    Rf_error("ParamSetShadow origin changed while planning value assignment");
  }
  UNPROTECT(2);
  return result;
}

static int contains_parameter_id(SEXP ids, SEXP sought,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t index = 0; index < XLENGTH(ids); ++index) {
    paradox_account_work(work_since_interrupt);
    if (paradox_domain_strings_equal(STRING_ELT(ids, index), sought)) {
      return TRUE;
    }
  }
  return FALSE;
}

static void process_shadow_write(value_write_transaction_t *transaction,
    const value_write_task_t *task, SEXP private_environment, SEXP core) {
  if (paradox_core_from_private(private_environment) != core) {
    Rf_error("ParamSetShadow changed while planning value assignment");
  }
  SEXP state = paradox_core_payload(core);
  if (state == R_UnboundValue) {
    Rf_error("Corrupt ParamSetShadow value transaction capsule");
  }
  SEXP params = VECTOR_ELT(state, PARADOX_CORE_PARAMS);
  SEXP sets = VECTOR_ELT(state, PARADOX_CORE_SETS);
  paradox_domain_params_t checked_params;
  R_xlen_t unused_row = 0;
  if (!paradox_domain_validate_params(
        params,
        R_NilValue,
        TRUE,
        &checked_params,
        &unused_row,
        transaction->work_since_interrupt
      ) || TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isObject(sets) ||
      XLENGTH(sets) != 1 || TYPEOF(VECTOR_ELT(sets, 0)) != ENVSXP) {
    Rf_error("Corrupt ParamSetShadow value transaction state");
  }

  SEXP input_names = Rf_getAttrib(task->values, R_NamesSymbol);
  if (TYPEOF(input_names) != STRSXP ||
      XLENGTH(input_names) != XLENGTH(task->values)) {
    Rf_error("Internal error: malformed ParamSetShadow value names");
  }
  for (R_xlen_t index = 0; index < XLENGTH(task->values); ++index) {
    if (!contains_parameter_id(
        checked_params.ids,
        STRING_ELT(input_names, index),
        transaction->work_since_interrupt
      )) {
      shadow_parameter_unavailable(
        STRING_ELT(input_names, index),
        checked_params.ids
      );
    }
  }

  SEXP origin = PROTECT(VECTOR_ELT(sets, 0));
  SEXP origin_private = PROTECT(paradox_domain_private_environment(origin));
  if (origin_private == R_UnboundValue) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSetShadow origin shell");
  }
  SEXP origin_core = PROTECT(paradox_core_refresh_shadow(
    origin,
    origin_private
  ));
  SEXP origin_values = PROTECT(current_node_values(
    origin,
    origin_private,
    origin_core,
    transaction->work_since_interrupt
  ));
  SEXP origin_names = Rf_getAttrib(origin_values, R_NamesSymbol);
  if (XLENGTH(origin_values) != 0 &&
      (TYPEOF(origin_names) != STRSXP ||
       XLENGTH(origin_names) != XLENGTH(origin_values))) {
    UNPROTECT(4);
    Rf_error("Corrupt ParamSetShadow origin values");
  }
  R_xlen_t hidden_count = 0;
  for (R_xlen_t index = 0; index < XLENGTH(origin_values); ++index) {
    if (!contains_parameter_id(
        checked_params.ids,
        STRING_ELT(origin_names, index),
        transaction->work_since_interrupt
      )) {
      ++hidden_count;
    }
  }
  if (hidden_count > R_XLEN_T_MAX - XLENGTH(task->values)) {
    UNPROTECT(4);
    Rf_error("ParamSetShadow value transaction is too large");
  }
  const R_xlen_t merged_size = hidden_count + XLENGTH(task->values);
  SEXP merged = PROTECT(Rf_allocVector(VECSXP, merged_size));
  SEXP merged_names = PROTECT(Rf_allocVector(STRSXP, merged_size));
  SEXP merged_sources = PROTECT(Rf_allocVector(STRSXP, merged_size));
  R_xlen_t output = 0;
  for (R_xlen_t index = 0; index < XLENGTH(origin_values); ++index) {
    SEXP name = STRING_ELT(origin_names, index);
    if (contains_parameter_id(
        checked_params.ids,
        name,
        transaction->work_since_interrupt
      )) {
      continue;
    }
    SET_VECTOR_ELT(merged, output, VECTOR_ELT(origin_values, index));
    SET_STRING_ELT(merged_names, output, name);
    SET_STRING_ELT(merged_sources, output, NA_STRING);
    ++output;
  }
  for (R_xlen_t index = 0; index < XLENGTH(task->values); ++index) {
    SET_VECTOR_ELT(merged, output, VECTOR_ELT(task->values, index));
    SET_STRING_ELT(merged_names, output, STRING_ELT(input_names, index));
    SET_STRING_ELT(
      merged_sources,
      output,
      STRING_ELT(task->sources, index)
    );
    ++output;
  }
  if (output != merged_size) {
    UNPROTECT(7);
    Rf_error("Internal error: incomplete ParamSetShadow value plan");
  }
  Rf_setAttrib(merged, R_NamesSymbol, merged_names);
  push_write_task(
    transaction,
    origin,
    origin_private,
    origin_core,
    merged,
    merged_sources,
    task->path
  );
  UNPROTECT(7);
}

static void process_write_task(value_write_transaction_t *transaction,
    const value_write_task_t *task, int is_root) {
  SEXP private_environment = PROTECT(paradox_domain_private_environment(
    task->self
  ));
  if (private_environment == R_UnboundValue ||
      (task->expected_private != R_NilValue &&
       private_environment != task->expected_private)) {
    UNPROTECT(1);
    Rf_error("Corrupt ParamSet value transaction shell ownership");
  }
  SEXP core = PROTECT(paradox_core_refresh_shadow(
    task->self,
    private_environment
  ));
  if (task->expected_core != R_NilValue && core != task->expected_core) {
    UNPROTECT(2);
    Rf_error("ParamSet value transaction graph changed while being planned");
  }
  value_transaction_retain(transaction, private_environment);
  value_transaction_retain(transaction, core);
  const paradox_core_kind_t kind = paradox_core_kind(core);
  if (is_root) {
    transaction->root_kind = kind;
  }
  switch (kind) {
  case PARADOX_CORE_BASE:
    process_base_write(transaction, task, private_environment, core);
    break;
  case PARADOX_CORE_COLLECTION:
    process_collection_write(transaction, task, private_environment, core);
    break;
  case PARADOX_CORE_SHADOW:
    process_shadow_write(transaction, task, private_environment, core);
    break;
  default:
    UNPROTECT(2);
    Rf_error("Corrupt ParamSet value transaction node kind");
  }
  UNPROTECT(2);
}

static void build_value_write_plan(value_write_transaction_t *transaction,
    SEXP private_environment, SEXP self, SEXP values) {
  SEXP sources = PROTECT(root_value_sources(values));
  value_transaction_retain(transaction, values);
  value_transaction_retain(transaction, sources);
  transaction->root_private = private_environment;
  value_transaction_retain(transaction, private_environment);
  push_write_task(
    transaction,
    self,
    private_environment,
    R_NilValue,
    values,
    sources,
    NULL
  );
  UNPROTECT(1);

  int is_root = TRUE;
  while (transaction->task_count != 0) {
    const value_write_task_t task =
      transaction->tasks[--transaction->task_count];
    process_write_task(transaction, &task, is_root);
    is_root = FALSE;
  }
}

static void validate_target_generations(
    const value_write_transaction_t *transaction) {
  for (R_xlen_t index = 0; index < transaction->target_count; ++index) {
    const value_write_target_t *target = &transaction->targets[index];
    if (paradox_api_plain_binding_snapshot(
          target->private_environment,
          value_core_symbol
        ) !=
        target->expected_core) {
      Rf_error(
        "ParamSet values changed during validation; nested mutation was preserved"
      );
    }
  }
}

static void scan_target_generations(
    const value_write_transaction_t *transaction) {
  for (R_xlen_t index = 0; index < transaction->target_count; ++index) {
    const value_write_target_t *target = &transaction->targets[index];
    if (paradox_api_plain_binding_scan(
          target->private_environment,
          value_core_symbol
        ) !=
        target->expected_core) {
      Rf_error(
        "ParamSet values changed during validation; nested mutation was preserved"
      );
    }
  }
}

static SEXP validate_transaction_values(SEXP private_environment, SEXP self,
    SEXP values, SEXP *receipts_result) {
  *receipts_result = R_NilValue;
  SEXP check_strict = PROTECT(Rf_ScalarLogical(TRUE));
  SEXP sanitize = PROTECT(Rf_ScalarLogical(TRUE));
  SEXP presence = PROTECT(Rf_mkString("none"));
  SEXP allow_token = PROTECT(Rf_ScalarLogical(TRUE));
  SEXP checked = PROTECT(paradox_param_set_check_builtin_with_receipts(
    private_environment,
    self,
    values,
    check_strict,
    sanitize,
    presence,
    allow_token,
    FALSE,
    receipts_result
  ));
  PROTECT(*receipts_result);
  if (TYPEOF(checked) == STRSXP && XLENGTH(checked) == 1 &&
      STRING_ELT(checked, 0) != NA_STRING) {
    paradox_assertion_error("xs", checked);
  }
  if (TYPEOF(checked) != LGLSXP || XLENGTH(checked) != 1 ||
      LOGICAL_ELT(checked, 0) != TRUE) {
    UNPROTECT(6);
    Rf_error("Internal error: invalid ParamSet check result");
  }
  SEXP sanitized = PROTECT(Rf_getAttrib(checked, Rf_install("sanitized")));
  if (TYPEOF(sanitized) != VECSXP) {
    UNPROTECT(7);
    Rf_error("Internal error: sanitized ParamSet values are missing");
  }
  UNPROTECT(7);
  return sanitized;
}

static SEXP apply_sanitized_values(value_write_transaction_t *transaction,
    SEXP sanitized) {
  SEXP stable = PROTECT(snapshot_transaction_values(
    sanitized,
    transaction->work_since_interrupt
  ));
  SEXP stable_names = Rf_getAttrib(stable, R_NamesSymbol);
  value_transaction_retain(transaction, stable);
  for (R_xlen_t target_index_value = 0;
      target_index_value < transaction->target_count;
      ++target_index_value) {
    value_write_target_t *target =
      &transaction->targets[target_index_value];
    const R_xlen_t input_size = XLENGTH(target->values);
    SEXP input_names = Rf_getAttrib(target->values, R_NamesSymbol);
    if (TYPEOF(input_names) != STRSXP ||
        XLENGTH(input_names) != input_size ||
        TYPEOF(target->sources) != STRSXP ||
        XLENGTH(target->sources) != input_size) {
      UNPROTECT(1);
      Rf_error("Internal error: malformed sanitized ParamSet target map");
    }
    R_xlen_t output_size = 0;
    for (R_xlen_t index = 0; index < input_size; ++index) {
      SEXP source = STRING_ELT(target->sources, index);
      if (source == NA_STRING || find_name(
          stable_names,
          source,
          transaction->work_since_interrupt
        ) >= 0) {
        ++output_size;
      }
    }
    SEXP values = PROTECT(Rf_allocVector(VECSXP, output_size));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, output_size));
    R_xlen_t output = 0;
    for (R_xlen_t index = 0; index < input_size; ++index) {
      SEXP source = STRING_ELT(target->sources, index);
      SEXP value = VECTOR_ELT(target->values, index);
      if (source != NA_STRING) {
        const R_xlen_t source_index = find_name(
          stable_names,
          source,
          transaction->work_since_interrupt
        );
        if (source_index < 0) {
          continue;
        }
        if (source_index >= XLENGTH(stable)) {
          UNPROTECT(3);
          Rf_error("Internal error: invalid sanitized ParamSet value source");
        }
        value = VECTOR_ELT(stable, source_index);
      }
      if (output >= output_size) {
        UNPROTECT(3);
        Rf_error("Internal error: sanitized ParamSet target overflow");
      }
      SET_VECTOR_ELT(values, output, value);
      SET_STRING_ELT(names, output, STRING_ELT(input_names, index));
      ++output;
    }
    if (output != output_size) {
      UNPROTECT(3);
      Rf_error("Internal error: incomplete sanitized ParamSet target");
    }
    Rf_setAttrib(values, R_NamesSymbol, names);
    target->values = values;
    value_transaction_retain(transaction, values);
    UNPROTECT(2);
  }
  UNPROTECT(1);
  return stable;
}

static void build_replacement_cores(value_write_transaction_t *transaction) {
  for (R_xlen_t index = 0; index < transaction->target_count; ++index) {
    value_write_target_t *target = &transaction->targets[index];
    SEXP payload = paradox_core_payload(target->expected_core);
    if (paradox_core_kind(target->expected_core) != PARADOX_CORE_BASE ||
        payload == R_UnboundValue) {
      Rf_error("Corrupt ParamSet value transaction target capsule");
    }
    SEXP fields[PARADOX_CORE_FIELD_COUNT];
    for (int field = 0; field < PARADOX_CORE_FIELD_COUNT; ++field) {
      fields[field] = VECTOR_ELT(payload, field);
    }
    fields[PARADOX_CORE_VALUES] = target->values;
    SEXP replacement = PROTECT(paradox_core_new_from_fields(
      PARADOX_CORE_BASE,
      fields
    ));
    target->replacement_core = replacement;
    value_transaction_retain(transaction, replacement);
    UNPROTECT(1);
  }
}

static void commit_replacement_cores(
    const value_write_transaction_t *transaction, SEXP token_receipts) {
  validate_target_generations(transaction);
  scan_target_generations(transaction);
  paradox_param_set_scan_token_receipts(token_receipts);
  /* Captured private environments are the package-authoritative mutation
   * targets. After the generation scan, only their existing `.core` bindings
   * are replaced; generated R6 surface topology is deliberately irrelevant. */
  for (R_xlen_t index = 0; index < transaction->target_count; ++index) {
    const value_write_target_t *target = &transaction->targets[index];
    Rf_defineVar(
      value_core_symbol,
      target->replacement_core,
      target->private_environment
    );
  }
}

static SEXP unvalidated_transaction_result(
    const value_write_transaction_t *transaction, SEXP input) {
  if (transaction->root_kind == PARADOX_CORE_BASE) {
    for (R_xlen_t index = 0; index < transaction->target_count; ++index) {
      if (transaction->targets[index].private_environment ==
          transaction->root_private) {
        return transaction->targets[index].values;
      }
    }
    Rf_error("Internal error: BASE value transaction lost its target");
  }
  return input;
}

static SEXP run_value_transaction(SEXP private_environment, SEXP self,
    SEXP values, int validate) {
  (void) initialize_value_core_symbol();
  R_xlen_t work_since_interrupt = 0;
  PROTECT_INDEX roots_index;
  SEXP roots;
  PROTECT_WITH_INDEX(roots = R_NilValue, &roots_index);
  value_write_transaction_t transaction = {
    .roots = &roots,
    .roots_index = roots_index,
    .tasks = paradox_temporary_alloc(8, sizeof(*transaction.tasks)),
    .task_count = 0,
    .task_capacity = 8,
    .targets = paradox_temporary_alloc(8, sizeof(*transaction.targets)),
    .target_count = 0,
    .target_capacity = 8,
    .root_private = R_NilValue,
    .root_kind = 0,
    .work_since_interrupt = &work_since_interrupt
  };

  SEXP stable_values = PROTECT(snapshot_transaction_values(
    values,
    &work_since_interrupt
  ));
  build_value_write_plan(
    &transaction,
    private_environment,
    self,
    stable_values
  );
  validate_target_generations(&transaction);

  SEXP result = stable_values;
  SEXP token_receipts = R_NilValue;
  if (validate) {
    SEXP sanitized = PROTECT(validate_transaction_values(
      private_environment,
      self,
      stable_values,
      &token_receipts
    ));
    PROTECT(token_receipts);
    value_transaction_retain(&transaction, token_receipts);
    result = apply_sanitized_values(&transaction, sanitized);
    UNPROTECT(2);
  }
  build_replacement_cores(&transaction);
  commit_replacement_cores(&transaction, token_receipts);
  if (!validate) {
    result = unvalidated_transaction_result(&transaction, stable_values);
  }
  UNPROTECT(2);
  return result;
}
