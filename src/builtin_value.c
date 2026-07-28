#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

#include "paradox.h"

#include <R_ext/Arith.h>

#include "builtin_value.h"
#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

static paradox_builtin_value_result_t valid_result(void) {
  const paradox_builtin_value_result_t result = {
    PARADOX_BUILTIN_VALUE_OK,
    NA_REAL,
    NA_REAL,
    NA_REAL,
    FALSE
  };
  return result;
}

static paradox_builtin_value_result_t failure_result(
    paradox_builtin_value_failure_t failure) {
  paradox_builtin_value_result_t result = valid_result();
  result.failure = failure;
  return result;
}

int paradox_builtin_special_values_contain(
    paradox_builtin_domain_kind_t kind, SEXP special_values, SEXP value,
    R_xlen_t *work_since_interrupt) {
  if (kind == PARADOX_BUILTIN_DOMAIN_UNKNOWN) {
    Rf_error("Internal error: unknown built-in Domain kind");
  }
  if (work_since_interrupt == NULL) {
    Rf_error("Internal error: missing special-value work counter");
  }
  if (XLENGTH(special_values) == 0) return FALSE;

  PROTECT(special_values);
  PROTECT(value);
  for (R_xlen_t index = 0; index < XLENGTH(special_values); ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP special = VECTOR_ELT(special_values, index);
    const int exact = special == value;
    const int compare_structurally =
      kind == PARADOX_BUILTIN_DOMAIN_UTY ||
      (!Rf_isS4(special) && !Rf_isS4(value));
    if (exact || (compare_structurally && R_compute_identical(
        special, value, paradox_api_identical_default_flags()
      ))) {
      UNPROTECT(2);
      return TRUE;
    }
  }
  UNPROTECT(2);
  return FALSE;
}

static int numeric_type(SEXP value) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  return type == REALSXP || type == INTSXP;
}

static int atomic_vector_type(SEXPTYPE type) {
  switch (type) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
    return TRUE;
  default:
    return FALSE;
  }
}

static int scalar_is_missing(SEXP value, int include_complex) {
  if (ALTREP(value)) return FALSE;

  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  switch (type) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case STRSXP:
    break;
  case CPLXSXP:
    if (!include_complex) return FALSE;
    break;
  default:
    return FALSE;
  }
  if (XLENGTH(value) != 1) return FALSE;

  switch (type) {
  case LGLSXP:
    return LOGICAL_ELT(value, 0) == NA_LOGICAL;
  case INTSXP:
    return INTEGER_ELT(value, 0) == NA_INTEGER;
  case REALSXP:
    return ISNAN(REAL_ELT(value, 0));
  case CPLXSXP: {
    const Rcomplex number = COMPLEX_ELT(value, 0);
    return ISNAN(number.r) || ISNAN(number.i);
  }
  case STRSXP:
    return STRING_ELT(value, 0) == NA_STRING;
  default:
    break;
  }
  return FALSE;
}

static paradox_builtin_value_result_t check_numeric(
    const paradox_builtin_value_spec_t *spec, SEXP value) {
  if (Rf_isS4(value) || Rf_isObject(value) || ALTREP(value) ||
      !numeric_type(value)) {
    if (scalar_is_missing(value, FALSE)) {
      return failure_result(PARADOX_BUILTIN_VALUE_MISSING);
    }
    return failure_result(PARADOX_BUILTIN_VALUE_WRONG_TYPE);
  }
  if (XLENGTH(value) != 1) {
    return failure_result(PARADOX_BUILTIN_VALUE_WRONG_LENGTH);
  }

  double number;
  if (TYPEOF(value) == REALSXP) {
    number = REAL_ELT(value, 0);
    if (ISNAN(number)) {
      return failure_result(PARADOX_BUILTIN_VALUE_MISSING);
    }
  } else {
    const int integer = INTEGER_ELT(value, 0);
    if (integer == NA_INTEGER) {
      return failure_result(PARADOX_BUILTIN_VALUE_MISSING);
    }
    number = (double) integer;
  }

  paradox_builtin_value_result_t result = valid_result();
  result.number = number;
  result.canonical_number = number;
  if (spec->kind == PARADOX_BUILTIN_DOMAIN_DBL) {
    const double accepted_lower = paradox_accepted_lower(
      spec->lower,
      spec->tolerance
    );
    const double accepted_upper = paradox_accepted_upper(
      spec->upper,
      spec->tolerance
    );
    if (number < accepted_lower) {
      result.failure = PARADOX_BUILTIN_VALUE_BELOW_LOWER;
      result.diagnostic_bound = accepted_lower;
    } else if (number > accepted_upper) {
      result.failure = PARADOX_BUILTIN_VALUE_ABOVE_UPPER;
      result.diagnostic_bound = accepted_upper;
    }
    return result;
  }

  if (!R_FINITE(number)) {
    result.failure = PARADOX_BUILTIN_VALUE_NOT_INTEGERISH;
    return result;
  }
  const double rounded = nearbyint(number);
  result.canonical_number = rounded;
  if (!paradox_within_integer_tolerance(
      number,
      rounded,
      spec->tolerance
    ) ||
      rounded <= (double) INT_MIN || rounded > (double) INT_MAX) {
    result.failure = PARADOX_BUILTIN_VALUE_NOT_INTEGERISH;
    return result;
  }
  if (rounded < spec->lower) {
    result.failure = PARADOX_BUILTIN_VALUE_BELOW_LOWER;
    result.diagnostic_bound = spec->lower - 0.5;
  } else if (rounded > spec->upper) {
    result.failure = PARADOX_BUILTIN_VALUE_ABOVE_UPPER;
    result.diagnostic_bound = spec->upper + 0.5;
  }
  return result;
}

static paradox_builtin_value_result_t check_factor(
    const paradox_builtin_value_spec_t *spec, SEXP value,
    R_xlen_t *work_since_interrupt) {
  if (value == R_NilValue) {
    return failure_result(PARADOX_BUILTIN_VALUE_NULL_FACTOR);
  }
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  if (Rf_isS4(value) || ALTREP(value) || !atomic_vector_type(type)) {
    return failure_result(PARADOX_BUILTIN_VALUE_NOT_ATOMIC_SCALAR);
  }
  if (XLENGTH(value) != 1) {
    return failure_result(PARADOX_BUILTIN_VALUE_NOT_ATOMIC_SCALAR);
  }
  if (Rf_isObject(value) || type != STRSXP) {
    if (scalar_is_missing(value, TRUE)) {
      return failure_result(PARADOX_BUILTIN_VALUE_MISSING);
    }
    return failure_result(PARADOX_BUILTIN_VALUE_WRONG_TYPE);
  }
  SEXP selected = STRING_ELT(value, 0);
  if (selected == NA_STRING) {
    return failure_result(PARADOX_BUILTIN_VALUE_MISSING);
  }
  for (R_xlen_t level = 0; level < XLENGTH(spec->levels); ++level) {
    paradox_account_work(work_since_interrupt);
    if (paradox_domain_strings_equal(
        selected,
        STRING_ELT(spec->levels, level)
      )) {
      return valid_result();
    }
  }
  return failure_result(PARADOX_BUILTIN_VALUE_NOT_IN_LEVELS);
}

static paradox_builtin_value_result_t check_logical(SEXP value) {
  if (Rf_isS4(value) || Rf_isObject(value) || ALTREP(value) ||
      TYPEOF(value) != LGLSXP) {
    if (scalar_is_missing(value, FALSE)) {
      return failure_result(PARADOX_BUILTIN_VALUE_MISSING);
    }
    return failure_result(PARADOX_BUILTIN_VALUE_WRONG_TYPE);
  }
  if (XLENGTH(value) != 1) {
    return failure_result(PARADOX_BUILTIN_VALUE_WRONG_LENGTH);
  }
  if (LOGICAL_ELT(value, 0) == NA_LOGICAL) {
    return failure_result(PARADOX_BUILTIN_VALUE_MISSING);
  }
  return valid_result();
}

paradox_builtin_value_result_t paradox_builtin_value_check(
    const paradox_builtin_value_spec_t *spec, SEXP value,
    int respect_special_values, R_xlen_t *work_since_interrupt) {
  if (spec == NULL || work_since_interrupt == NULL ||
      spec->kind == PARADOX_BUILTIN_DOMAIN_UNKNOWN) {
    Rf_error("Internal error: invalid built-in value specification");
  }
  if (respect_special_values && paradox_builtin_special_values_contain(
      spec->kind,
      spec->special_values,
      value,
      work_since_interrupt
    )) {
    paradox_builtin_value_result_t result = valid_result();
    result.special = TRUE;
    return result;
  }

  switch (spec->kind) {
  case PARADOX_BUILTIN_DOMAIN_DBL:
  case PARADOX_BUILTIN_DOMAIN_INT:
    return check_numeric(spec, value);
  case PARADOX_BUILTIN_DOMAIN_FCT:
    return check_factor(spec, value, work_since_interrupt);
  case PARADOX_BUILTIN_DOMAIN_LGL:
    return check_logical(value);
  case PARADOX_BUILTIN_DOMAIN_UTY:
    return valid_result();
  case PARADOX_BUILTIN_DOMAIN_UNKNOWN:
    break;
  }
  Rf_error("Internal error: unknown built-in Domain kind");
  return failure_result(PARADOX_BUILTIN_VALUE_WRONG_TYPE);
}

static SEXP observed_type(SEXP value, int factor_style) {
  if (Rf_isObject(value) || Rf_isS4(value)) {
    SEXP classes = PROTECT(Rf_getAttrib(value, R_ClassSymbol));
    if (TYPEOF(classes) == STRSXP && !ALTREP(classes) &&
        XLENGTH(classes) != 0) {
      SEXP first = STRING_ELT(classes, 0);
      if (first != NA_STRING) {
        UNPROTECT(1);
        return first;
      }
    }
    UNPROTECT(1);
  }
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  return Rf_mkChar(
    factor_style && type == REALSXP ? "numeric" : Rf_type2char(type)
  );
}

static SEXP prefixed_literal(SEXP id, const char *reason) {
  SEXP safe_id = PROTECT(paradox_diagnostic_charsxp(id));
  paradox_utf8_piece_t pieces[] = {
    paradox_utf8_charsxp_piece(safe_id),
    paradox_utf8_ascii_piece(": "),
    paradox_utf8_ascii_piece(reason)
  };
  SEXP result = PROTECT(paradox_utf8_message(pieces, 3));
  UNPROTECT(2);
  return result;
}

static SEXP wrong_type_diagnostic(SEXP id, const char *expected,
    SEXP value, int factor_style) {
  SEXP safe_id = PROTECT(paradox_diagnostic_charsxp(id));
  SEXP type = PROTECT(observed_type(value, factor_style));
  SEXP safe_type = PROTECT(paradox_diagnostic_charsxp(type));
  paradox_utf8_piece_t pieces[] = {
    paradox_utf8_charsxp_piece(safe_id),
    paradox_utf8_ascii_piece(": Must be of type '"),
    paradox_utf8_ascii_piece(expected),
    paradox_utf8_ascii_piece("', not '"),
    paradox_utf8_charsxp_piece(safe_type),
    paradox_utf8_ascii_piece("'")
  };
  SEXP result = PROTECT(paradox_utf8_message(pieces, 6));
  UNPROTECT(4);
  return result;
}

static SEXP bound_diagnostic(SEXP id, int lower, double bound) {
  char number[64];
  if (R_FINITE(bound)) {
    const int written = snprintf(number, sizeof(number), "%g", bound);
    if (written < 0 || (size_t) written >= sizeof(number)) {
      Rf_error("Internal error while formatting a Domain bound");
    }
  } else if (bound > 0.0) {
    memcpy(number, "Inf", sizeof("Inf"));
  } else {
    memcpy(number, "-Inf", sizeof("-Inf"));
  }
  SEXP safe_id = PROTECT(paradox_diagnostic_charsxp(id));
  paradox_utf8_piece_t pieces[] = {
    paradox_utf8_charsxp_piece(safe_id),
    paradox_utf8_ascii_piece(
      lower ? ": Element 1 is not >= " : ": Element 1 is not <= "
    ),
    paradox_utf8_ascii_piece(number)
  };
  SEXP result = PROTECT(paradox_utf8_message(pieces, 3));
  UNPROTECT(2);
  return result;
}

static SEXP factor_text_charsxp(SEXP string) {
  if (TYPEOF(string) != CHARSXP || string == NA_STRING) {
    Rf_error("Internal error: invalid factor diagnostic text");
  }
  static const char hex[] = "0123456789abcdef";
  const int bytes_encoding = Rf_getCharCE(string) == CE_BYTES;
  const unsigned char *source = (const unsigned char *) (
    bytes_encoding ? CHAR(string) : Rf_translateCharUTF8(string)
  );
  const size_t source_size = strlen((const char *) source);
  if (source_size > (size_t) INT_MAX / 4U) {
    Rf_error("Factor diagnostic text exceeds R's string limit");
  }
  char *output = paradox_temporary_alloc(
    (R_xlen_t) (source_size * 4U) + 1,
    sizeof(*output)
  );
  source = (const unsigned char *) (
    bytes_encoding ? CHAR(string) : Rf_translateCharUTF8(string)
  );
  size_t output_size = 0;
  for (size_t index = 0; index < source_size; ++index) {
    const unsigned char byte = source[index];
    if (byte == (unsigned char) '\'' || byte == (unsigned char) '\\') {
      output[output_size++] = '\\';
      output[output_size++] = (char) byte;
    } else if (byte == (unsigned char) '\n') {
      output[output_size++] = '\\';
      output[output_size++] = 'n';
    } else if (byte == (unsigned char) '\r') {
      output[output_size++] = '\\';
      output[output_size++] = 'r';
    } else if (byte == (unsigned char) '\t') {
      output[output_size++] = '\\';
      output[output_size++] = 't';
    } else if ((bytes_encoding &&
        (byte < 0x20U || byte > 0x7eU)) ||
        (!bytes_encoding && (byte < 0x20U || byte == 0x7fU))) {
      output[output_size++] = '\\';
      output[output_size++] = 'x';
      output[output_size++] = hex[byte >> 4U];
      output[output_size++] = hex[byte & 0x0fU];
    } else {
      output[output_size++] = (char) byte;
    }
  }
  output[output_size] = '\0';
  return Rf_mkCharLenCE(output, (int) output_size, CE_UTF8);
}

static SEXP factor_level_set(SEXP levels) {
  const R_xlen_t count = XLENGTH(levels);
  if (count > (R_XLEN_T_MAX - 1) / 2) {
    Rf_error("Factor Domain has too many levels to format");
  }
  SEXP safe_levels = PROTECT(Rf_allocVector(STRSXP, count));
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP safe = PROTECT(factor_text_charsxp(
      STRING_ELT(levels, index)
    ));
    SET_STRING_ELT(safe_levels, index, safe);
    UNPROTECT(1);
  }
  const R_xlen_t piece_count = count == 0 ? 1 : 2 * count + 1;
  paradox_utf8_piece_t *pieces = paradox_temporary_alloc(
    piece_count,
    sizeof(*pieces)
  );
  if (count == 0) {
    pieces[0] = paradox_utf8_ascii_piece("{}");
  } else {
    pieces[0] = paradox_utf8_ascii_piece("{'");
    for (R_xlen_t index = 0; index < count; ++index) {
      pieces[2 * index + 1] = paradox_utf8_charsxp_piece(
        STRING_ELT(safe_levels, index)
      );
      pieces[2 * index + 2] = paradox_utf8_ascii_piece(
        index + 1 == count ? "'}" : "','"
      );
    }
  }
  SEXP result = PROTECT(paradox_utf8_message(pieces, piece_count));
  UNPROTECT(2);
  return result;
}

static SEXP factor_diagnostic(SEXP id,
    const paradox_builtin_value_spec_t *spec, SEXP value,
    paradox_builtin_value_failure_t failure) {
  SEXP safe_id = PROTECT(paradox_diagnostic_charsxp(id));
  SEXP set = PROTECT(factor_level_set(spec->levels));
  SEXP set_text = STRING_ELT(set, 0);
  if (failure == PARADOX_BUILTIN_VALUE_NOT_ATOMIC_SCALAR) {
    paradox_utf8_piece_t pieces[] = {
      paradox_utf8_charsxp_piece(safe_id),
      paradox_utf8_ascii_piece(": Must be element of set "),
      paradox_utf8_charsxp_piece(set_text),
      paradox_utf8_ascii_piece(", but is not atomic scalar")
    };
    SEXP result = PROTECT(paradox_utf8_message(pieces, 4));
    UNPROTECT(3);
    return result;
  }
  if (failure == PARADOX_BUILTIN_VALUE_NULL_FACTOR) {
    paradox_utf8_piece_t pieces[] = {
      paradox_utf8_charsxp_piece(safe_id),
      paradox_utf8_ascii_piece(": Must be element of set "),
      paradox_utf8_charsxp_piece(set_text),
      paradox_utf8_ascii_piece(", but is NULL")
    };
    SEXP result = PROTECT(paradox_utf8_message(pieces, 4));
    UNPROTECT(3);
    return result;
  }
  if (failure == PARADOX_BUILTIN_VALUE_WRONG_TYPE) {
    SEXP type = PROTECT(observed_type(value, TRUE));
    SEXP safe_type = PROTECT(paradox_diagnostic_charsxp(type));
    paradox_utf8_piece_t pieces[] = {
      paradox_utf8_charsxp_piece(safe_id),
      paradox_utf8_ascii_piece(": Must be element of set "),
      paradox_utf8_charsxp_piece(set_text),
      paradox_utf8_ascii_piece(", but types do not match ("),
      paradox_utf8_charsxp_piece(safe_type),
      paradox_utf8_ascii_piece(" != character)")
    };
    SEXP result = PROTECT(paradox_utf8_message(pieces, 6));
    UNPROTECT(5);
    return result;
  }

  SEXP observed;
  if (failure == PARADOX_BUILTIN_VALUE_MISSING) {
    observed = PROTECT(Rf_mkChar("NA"));
  } else if (failure == PARADOX_BUILTIN_VALUE_NOT_IN_LEVELS &&
      TYPEOF(value) == STRSXP && XLENGTH(value) == 1) {
    observed = PROTECT(factor_text_charsxp(STRING_ELT(value, 0)));
  } else {
    UNPROTECT(2);
    Rf_error("Internal error: invalid factor Domain failure");
  }
  paradox_utf8_piece_t pieces[] = {
    paradox_utf8_charsxp_piece(safe_id),
    paradox_utf8_ascii_piece(": Must be element of set "),
    paradox_utf8_charsxp_piece(set_text),
    paradox_utf8_ascii_piece(", but is '"),
    paradox_utf8_charsxp_piece(observed),
    paradox_utf8_ascii_piece("'")
  };
  SEXP result = PROTECT(paradox_utf8_message(pieces, 6));
  UNPROTECT(4);
  return result;
}

SEXP paradox_builtin_value_diagnostic(SEXP id,
    const paradox_builtin_value_spec_t *spec, SEXP value,
    const paradox_builtin_value_result_t *result) {
  if (spec == NULL || result == NULL) {
    Rf_error("Internal error: invalid built-in value diagnostic");
    return R_NilValue;
  }
  if (TYPEOF(id) != CHARSXP || id == NA_STRING ||
      result->failure == PARADOX_BUILTIN_VALUE_OK) {
    Rf_error("Internal error: invalid built-in value diagnostic");
    return R_NilValue;
  }

  if (spec->kind == PARADOX_BUILTIN_DOMAIN_FCT) {
    return factor_diagnostic(id, spec, value, result->failure);
  }
  switch (result->failure) {
  case PARADOX_BUILTIN_VALUE_WRONG_TYPE:
    if (spec->kind == PARADOX_BUILTIN_DOMAIN_DBL) {
      return wrong_type_diagnostic(id, "number", value, FALSE);
    }
    if (spec->kind == PARADOX_BUILTIN_DOMAIN_INT) {
      return wrong_type_diagnostic(
        id, "single integerish value", value, FALSE
      );
    }
    if (spec->kind == PARADOX_BUILTIN_DOMAIN_LGL) {
      return wrong_type_diagnostic(id, "logical flag", value, FALSE);
    }
    break;
  case PARADOX_BUILTIN_VALUE_WRONG_LENGTH:
    return prefixed_literal(id, "Must have length 1");
  case PARADOX_BUILTIN_VALUE_MISSING:
    return prefixed_literal(id, "May not be NA");
  case PARADOX_BUILTIN_VALUE_NOT_INTEGERISH:
    return wrong_type_diagnostic(
      id, "single integerish value", value, FALSE
    );
  case PARADOX_BUILTIN_VALUE_BELOW_LOWER:
    return bound_diagnostic(id, TRUE, result->diagnostic_bound);
  case PARADOX_BUILTIN_VALUE_ABOVE_UPPER:
    return bound_diagnostic(id, FALSE, result->diagnostic_bound);
  case PARADOX_BUILTIN_VALUE_NOT_IN_LEVELS:
  case PARADOX_BUILTIN_VALUE_NOT_ATOMIC_SCALAR:
  case PARADOX_BUILTIN_VALUE_NULL_FACTOR:
  case PARADOX_BUILTIN_VALUE_OK:
    break;
  }
  Rf_error("Internal error: inconsistent built-in value failure");
  return R_NilValue;
}
