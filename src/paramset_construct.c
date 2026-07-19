#include <limits.h>
#include <stddef.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Utils.h>

#include "domain_admission.h"
#include "r_api_compat.h"
#include "r_utils.h"

enum domain_column {
  DOMAIN_ID = 0,
  DOMAIN_CLS,
  DOMAIN_GROUPING,
  DOMAIN_CARGO,
  DOMAIN_LOWER,
  DOMAIN_UPPER,
  DOMAIN_TOLERANCE,
  DOMAIN_LEVELS,
  DOMAIN_SPECIAL_VALS,
  DOMAIN_DEFAULT,
  DOMAIN_STORAGE_TYPE,
  DOMAIN_TAGS,
  DOMAIN_TRAFO,
  DOMAIN_REQUIREMENTS,
  DOMAIN_INIT_GIVEN,
  DOMAIN_INIT,
  DOMAIN_COLUMN_COUNT
};

enum construction_result {
  RESULT_PARAMS = 0,
  RESULT_TAGS,
  RESULT_TRAFOS,
  RESULT_REQUIREMENTS,
  RESULT_INIT_VALUES,
  RESULT_COUNT
};

static paradox_domain_field_t domain_column_field(
    enum domain_column column) {
  switch (column) {
  case DOMAIN_ID: return PARADOX_DOMAIN_FIELD_ID;
  case DOMAIN_CLS:
  case DOMAIN_STORAGE_TYPE:
    return PARADOX_DOMAIN_FIELD_CLASS_STORAGE;
  case DOMAIN_GROUPING: return PARADOX_DOMAIN_FIELD_GROUPING;
  case DOMAIN_CARGO: return PARADOX_DOMAIN_FIELD_CARGO;
  case DOMAIN_LOWER:
  case DOMAIN_UPPER:
  case DOMAIN_TOLERANCE:
    return PARADOX_DOMAIN_FIELD_BOUNDS;
  case DOMAIN_LEVELS: return PARADOX_DOMAIN_FIELD_LEVELS;
  case DOMAIN_SPECIAL_VALS: return PARADOX_DOMAIN_FIELD_SPECIAL_VALUES;
  case DOMAIN_DEFAULT: return PARADOX_DOMAIN_FIELD_DEFAULT;
  case DOMAIN_TAGS: return PARADOX_DOMAIN_FIELD_TAGS;
  case DOMAIN_TRAFO: return PARADOX_DOMAIN_FIELD_TRAFO;
  case DOMAIN_REQUIREMENTS: return PARADOX_DOMAIN_FIELD_REQUIREMENTS;
  case DOMAIN_INIT_GIVEN:
  case DOMAIN_INIT:
    return PARADOX_DOMAIN_FIELD_INIT;
  case DOMAIN_COLUMN_COUNT:
    break;
  }
  return PARADOX_DOMAIN_FIELD_NONE;
}

static const char *const domain_column_names[DOMAIN_COLUMN_COUNT] = {
  "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
  "levels", "special_vals", "default", "storage_type", ".tags",
  ".trafo", ".requirements", ".init_given", ".init"
};

static const SEXPTYPE domain_column_types[DOMAIN_COLUMN_COUNT] = {
  STRSXP, STRSXP, STRSXP, VECSXP, REALSXP, REALSXP, REALSXP, VECSXP,
  VECSXP, VECSXP, STRSXP, VECSXP, VECSXP, VECSXP, LGLSXP, VECSXP
};

static const char *const permanent_column_names[DOMAIN_TAGS] = {
  "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
  "levels", "special_vals", "default", "storage_type"
};

static const char *const result_names[RESULT_COUNT] = {
  "params", "tags", "trafos", "requirements", "init_values"
};

static int string_is(SEXP value, const char *expected) {
  return value != NA_STRING && strcmp(CHAR(value), expected) == 0;
}

/* This is the documented checkmate `type = "strict"` grammar used by the R
 * constructor: ^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$. Enforcing it here makes the
 * common native path independent of a preceding checkmate call and also makes
 * bytewise key ordering unambiguous. */
static int string_is_strict_id(SEXP value) {
  if (value == NA_STRING || Rf_getCharCE(value) == CE_BYTES ||
      LENGTH(value) == 0) {
    return FALSE;
  }

  const unsigned char *bytes = (const unsigned char *) CHAR(value);
  const int size = LENGTH(value);
  int index = 0;
  while (index < size && bytes[index] == '.') {
    ++index;
  }
  if (index == size ||
      !((bytes[index] >= 'a' && bytes[index] <= 'z') ||
        (bytes[index] >= 'A' && bytes[index] <= 'Z'))) {
    return FALSE;
  }

  for (++index; index < size; ++index) {
    const unsigned char byte = bytes[index];
    if (!((byte >= 'a' && byte <= 'z') ||
          (byte >= 'A' && byte <= 'Z') ||
          (byte >= '0' && byte <= '9') || byte == '.' || byte == '_')) {
      return FALSE;
    }
  }
  return TRUE;
}

static int class_is_builtin_domain(SEXP domain, SEXP cls) {
  if (TYPEOF(cls) != STRSXP || ALTREP(cls) || Rf_isS4(cls) ||
      XLENGTH(cls) != 1) {
    return FALSE;
  }

  SEXP classes = PROTECT(Rf_getAttrib(domain, R_ClassSymbol));
  if (TYPEOF(classes) != STRSXP || ALTREP(classes) || Rf_isS4(classes) ||
      Rf_isObject(classes) || !paradox_api_has_no_attributes(classes) ||
      XLENGTH(classes) != 4) {
    UNPROTECT(1);
    return FALSE;
  }

  SEXP class_name = STRING_ELT(cls, 0);
  if (!string_is(class_name, "ParamDbl") &&
      !string_is(class_name, "ParamInt") &&
      !string_is(class_name, "ParamFct") &&
      !string_is(class_name, "ParamLgl") &&
      !string_is(class_name, "ParamUty")) {
    UNPROTECT(1);
    return FALSE;
  }

  SEXP first = STRING_ELT(classes, 0);
  SEXP second = STRING_ELT(classes, 1);
  SEXP third = STRING_ELT(classes, 2);
  SEXP fourth = STRING_ELT(classes, 3);
  const int supported = first != NA_STRING &&
    strcmp(CHAR(first), CHAR(class_name)) == 0 &&
    string_is(second, "Domain") && string_is(third, "data.table") &&
    string_is(fourth, "data.frame");
  UNPROTECT(1);
  return supported;
}

static int exact_domain_outer_attributes(SEXP domain) {
  static const char *const allowed[] = {
    "names", "class", "row.names", ".internal.selfref", "repr"
  };
  if (Rf_isS4(domain) ||
      !paradox_api_has_only_attributes(domain, allowed, 5)) return FALSE;
  SEXP row_names = PROTECT(Rf_getAttrib(domain, R_RowNamesSymbol));
  SEXP selfref = PROTECT(Rf_getAttrib(
    domain,
    Rf_install(".internal.selfref")
  ));
  SEXP repr = PROTECT(Rf_getAttrib(domain, Rf_install("repr")));
  const int valid = TYPEOF(row_names) == INTSXP && !ALTREP(row_names) &&
    !Rf_isS4(row_names) && !Rf_isObject(row_names) &&
    paradox_api_has_no_attributes(row_names) &&
    XLENGTH(row_names) == 1 && INTEGER_ELT(row_names, 0) == 1 &&
    (selfref == R_NilValue ||
      (TYPEOF(selfref) == EXTPTRSXP && !Rf_isS4(selfref))) &&
    (repr == R_NilValue || !Rf_isS4(repr));
  UNPROTECT(3);
  return valid;
}

static int exact_domain_column_names(SEXP domain) {
  SEXP names = PROTECT(Rf_getAttrib(domain, R_NamesSymbol));
  if (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
      Rf_isObject(names) || !paradox_api_has_no_attributes(names) ||
      XLENGTH(names) != DOMAIN_COLUMN_COUNT) {
    UNPROTECT(1);
    return FALSE;
  }
  for (R_xlen_t column = 0; column < DOMAIN_COLUMN_COUNT; ++column) {
    if (!string_is(STRING_ELT(names, column), domain_column_names[column])) {
      UNPROTECT(1);
      return FALSE;
    }
  }
  UNPROTECT(1);
  return TRUE;
}

static SEXP snapshot_scalar_vector(SEXP source) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
  SEXP result = PROTECT(Rf_allocVector(type, 1));
  switch (type) {
  case STRSXP:
    SET_STRING_ELT(result, 0, STRING_ELT(source, 0));
    break;
  case VECSXP:
    SET_VECTOR_ELT(result, 0, VECTOR_ELT(source, 0));
    break;
  case REALSXP:
    SET_REAL_ELT(result, 0, REAL_ELT(source, 0));
    break;
  case INTSXP:
    SET_INTEGER_ELT(result, 0, INTEGER_ELT(source, 0));
    break;
  case LGLSXP:
    SET_LOGICAL_ELT(result, 0, LOGICAL_ELT(source, 0));
    break;
  default:
    UNPROTECT(1);
    Rf_error("Internal error: unsupported Domain snapshot type");
  }
  UNPROTECT(1);
  return result;
}

static SEXP snapshot_builtin_value_leaf(SEXP source) {
  /* Preserve the S4 bit until the shared Domain-row owner can admit the exact
   * leaf as a special value or reject it for a typed Domain. */
  if (Rf_isS4(source)) {
    return source;
  }
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
  if (type != LGLSXP && type != INTSXP && type != REALSXP &&
      type != CPLXSXP && type != STRSXP && type != RAWSXP) {
    return source;
  }
  SEXP result = PROTECT(paradox_snapshot_semantic_vector(source));
  SHALLOW_DUPLICATE_ATTRIB(result, source);
  UNPROTECT(1);
  return result;
}

static int cargo_nested_container_name(SEXP name) {
  return name != NA_STRING &&
    (string_is(name, "disable_in_tune") || string_is(name, "logscale") ||
      string_is(name, "repr"));
}

static int ordinary_names(SEXP names, R_xlen_t expected,
    int allow_absent) {
  if (names == R_NilValue) return allow_absent;
  return TYPEOF(names) == STRSXP && !ALTREP(names) &&
    !Rf_isS4(names) && !Rf_isObject(names) &&
    paradox_api_has_no_attributes(names) && XLENGTH(names) == expected;
}

static SEXP snapshot_domain_cargo(SEXP source) {
  if (source == R_NilValue) {
    return source;
  }
  static const char *const names_only[] = {"names"};
  if (TYPEOF(source) != VECSXP || ALTREP(source) || Rf_isS4(source) ||
      Rf_isObject(source) ||
      !paradox_api_has_only_attributes(source, names_only, 1)) {
    return R_UnboundValue;
  }

  const R_xlen_t size = XLENGTH(source);
  SEXP names = PROTECT(Rf_getAttrib(source, R_NamesSymbol));
  if (!ordinary_names(names, size, size == 0)) {
    UNPROTECT(1);
    return R_UnboundValue;
  }
  SEXP result = PROTECT(paradox_snapshot_semantic_vector(source));
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP name = STRING_ELT(names, index);
    if (!cargo_nested_container_name(name)) {
      continue;
    }
    SEXP value = VECTOR_ELT(result, index);
    if (value == R_NilValue) {
      continue;
    }
    if (ALTREP(value) || Rf_isS4(value) || Rf_isObject(value)) {
      UNPROTECT(2);
      return R_UnboundValue;
    }
    const int attributes_ok = string_is(name, "disable_in_tune")
      ? paradox_api_has_only_attributes(value, names_only, 1)
      : paradox_api_has_no_attributes(value);
    if (!attributes_ok) {
      UNPROTECT(2);
      return R_UnboundValue;
    }
    const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
    const int expected = string_is(name, "disable_in_tune")
      ? type == VECSXP
      : string_is(name, "logscale")
        ? type == LGLSXP
        : type == STRSXP;
    if (!expected) {
      UNPROTECT(2);
      return R_UnboundValue;
    }
    if (type == VECSXP) {
      SEXP nested_names = PROTECT(Rf_getAttrib(value, R_NamesSymbol));
      const int valid_names = ordinary_names(
        nested_names,
        XLENGTH(value),
        TRUE
      );
      UNPROTECT(1);
      if (!valid_names) {
        UNPROTECT(2);
        return R_UnboundValue;
      }
    }
    SEXP snapshot = PROTECT(paradox_snapshot_semantic_vector(value));
    SET_VECTOR_ELT(result, index, snapshot);
    UNPROTECT(1);
  }
  UNPROTECT(2);
  return result;
}

static SEXP snapshot_domain_nested(SEXP source, enum domain_column column) {
  if (column == DOMAIN_CARGO) {
    return snapshot_domain_cargo(source);
  }
  if (column == DOMAIN_LEVELS) {
    if (source == R_NilValue) {
      return source;
    }
    const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
    if ((type != STRSXP && type != LGLSXP) || Rf_isS4(source) ||
        Rf_isObject(source) ||
        !paradox_api_has_no_attributes(source)) {
      return R_UnboundValue;
    }
    SEXP result = PROTECT(paradox_snapshot_semantic_vector(source));
    Rf_setAttrib(result, R_NamesSymbol, R_NilValue);
    UNPROTECT(1);
    return result;
  }
  if (column == DOMAIN_SPECIAL_VALS) {
    static const char *const names_only[] = {"names"};
    if (TYPEOF(source) != VECSXP || ALTREP(source) || Rf_isS4(source) ||
        Rf_isObject(source) ||
        !paradox_api_has_only_attributes(source, names_only, 1)) {
      return R_UnboundValue;
    }
    SEXP names = PROTECT(Rf_getAttrib(source, R_NamesSymbol));
    const int valid_names = ordinary_names(
      names,
      XLENGTH(source),
      TRUE
    );
    UNPROTECT(1);
    if (!valid_names) return R_UnboundValue;
    return paradox_snapshot_semantic_vector(source);
  }
  if (column == DOMAIN_REQUIREMENTS) {
    R_xlen_t work_since_interrupt = 0;
    return paradox_snapshot_builtin_requirements(
      source,
      &work_since_interrupt
    );
  }
  return source;
}

/* Capture each column exactly once into ordinary length-one vectors. This is
 * both an ALTREP materialization boundary and an independent GC root for every
 * child used after the constructor starts allocating its result tables. */
static SEXP snapshot_domain(SEXP domain,
    paradox_domain_field_t *failed_field) {
  *failed_field = PARADOX_DOMAIN_FIELD_NONE;
  if (TYPEOF(domain) != VECSXP || ALTREP(domain) || Rf_isS4(domain) ||
      !Rf_isObject(domain) || XLENGTH(domain) != DOMAIN_COLUMN_COUNT ||
      !exact_domain_outer_attributes(domain)) {
    return R_NilValue;
  }

  if (!exact_domain_column_names(domain)) {
    return R_NilValue;
  }
  SEXP names = PROTECT(Rf_getAttrib(domain, R_NamesSymbol));

  SEXP snapshot = PROTECT(Rf_allocVector(VECSXP, DOMAIN_COLUMN_COUNT));
  for (R_xlen_t column = 0; column < DOMAIN_COLUMN_COUNT; ++column) {
    if (!string_is(STRING_ELT(names, column), domain_column_names[column])) {
      UNPROTECT(2);
      return R_NilValue;
    }
    SEXP value = PROTECT(VECTOR_ELT(domain, column));
    const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
    const int numeric_column = column == DOMAIN_LOWER ||
      column == DOMAIN_UPPER || column == DOMAIN_TOLERANCE;
    if (ALTREP(value) || Rf_isS4(value) || Rf_isObject(value) ||
        !paradox_api_has_no_attributes(value) || (numeric_column
        ? type != INTSXP && type != REALSXP
          : type != domain_column_types[column]) ||
        XLENGTH(value) != 1) {
      *failed_field = domain_column_field((enum domain_column) column);
      UNPROTECT(3);
      return R_NilValue;
    }
    SEXP copy = PROTECT(snapshot_scalar_vector(value));
    if (column == DOMAIN_CARGO || column == DOMAIN_LEVELS ||
        column == DOMAIN_SPECIAL_VALS || column == DOMAIN_REQUIREMENTS) {
      SEXP nested = PROTECT(snapshot_domain_nested(
        VECTOR_ELT(copy, 0),
        (enum domain_column) column
      ));
      if (nested == R_UnboundValue) {
        *failed_field = domain_column_field((enum domain_column) column);
        UNPROTECT(5);
        return R_NilValue;
      }
      SET_VECTOR_ELT(copy, 0, nested);
      UNPROTECT(1);
    }
    SET_VECTOR_ELT(snapshot, column, copy);
    UNPROTECT(2);
  }

  SEXP cls = VECTOR_ELT(snapshot, DOMAIN_CLS);
  if (!class_is_builtin_domain(domain, cls)) {
    *failed_field = PARADOX_DOMAIN_FIELD_CLASS_STORAGE;
    UNPROTECT(2);
    return R_NilValue;
  }

  if (!string_is(STRING_ELT(cls, 0), "ParamUty")) {
    SEXP stable_default = PROTECT(snapshot_builtin_value_leaf(
      VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_DEFAULT), 0)
    ));
    SET_VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_DEFAULT), 0, stable_default);
    UNPROTECT(1);
    if (LOGICAL_ELT(VECTOR_ELT(snapshot, DOMAIN_INIT_GIVEN), 0) == TRUE) {
      SEXP stable_init = PROTECT(snapshot_builtin_value_leaf(
        VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_INIT), 0)
      ));
      SET_VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_INIT), 0, stable_init);
      UNPROTECT(1);
    }
  }

  SEXP tags = VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_TAGS), 0);
  if (TYPEOF(tags) != STRSXP || ALTREP(tags) || Rf_isS4(tags) ||
      !paradox_api_has_no_attributes(tags)) {
    *failed_field = PARADOX_DOMAIN_FIELD_TAGS;
    UNPROTECT(2);
    return R_NilValue;
  }
  SEXP stable_tags = PROTECT(paradox_snapshot_semantic_vector(tags));
  SET_VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_TAGS), 0, stable_tags);
  UNPROTECT(1);

  paradox_builtin_domain_kind_t admitted_kind;
  R_xlen_t work_since_interrupt = 0;
  if (!paradox_admit_builtin_domain_row(
      VECTOR_ELT(snapshot, DOMAIN_ID),
      VECTOR_ELT(snapshot, DOMAIN_CLS),
      VECTOR_ELT(snapshot, DOMAIN_GROUPING),
      VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_CARGO), 0),
      VECTOR_ELT(snapshot, DOMAIN_LOWER),
      VECTOR_ELT(snapshot, DOMAIN_UPPER),
      VECTOR_ELT(snapshot, DOMAIN_TOLERANCE),
      VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_LEVELS), 0),
      VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_SPECIAL_VALS), 0),
      VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_DEFAULT), 0),
      VECTOR_ELT(snapshot, DOMAIN_STORAGE_TYPE),
      VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_TAGS), 0),
      VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_TRAFO), 0),
      VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_REQUIREMENTS), 0),
      VECTOR_ELT(snapshot, DOMAIN_INIT_GIVEN),
      VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_INIT), 0),
      &admitted_kind,
      failed_field,
      &work_since_interrupt
    )) {
    UNPROTECT(2);
    return R_NilValue;
  }
  (void) admitted_kind;
  UNPROTECT(2);
  return snapshot;
}

SEXP paradox_snapshot_builtin_domain(SEXP domain,
    paradox_domain_field_t *failed_field) {
  SEXP snapshot = PROTECT(snapshot_domain(domain, failed_field));
  if (snapshot == R_NilValue) {
    UNPROTECT(1);
    return R_NilValue;
  }
  SEXP cls = VECTOR_ELT(snapshot, DOMAIN_CLS);
  const int utility = string_is(STRING_ELT(cls, 0), "ParamUty");
  const int numeric = string_is(STRING_ELT(cls, 0), "ParamDbl") ||
    string_is(STRING_ELT(cls, 0), "ParamInt");
  double lower;
  double upper;
  if (TYPEOF(VECTOR_ELT(snapshot, DOMAIN_LOWER)) == REALSXP) {
    lower = REAL_ELT(VECTOR_ELT(snapshot, DOMAIN_LOWER), 0);
  } else {
    const int value = INTEGER_ELT(VECTOR_ELT(snapshot, DOMAIN_LOWER), 0);
    lower = (double) value;
  }
  if (TYPEOF(VECTOR_ELT(snapshot, DOMAIN_UPPER)) == REALSXP) {
    upper = REAL_ELT(VECTOR_ELT(snapshot, DOMAIN_UPPER), 0);
  } else {
    const int value = INTEGER_ELT(VECTOR_ELT(snapshot, DOMAIN_UPPER), 0);
    upper = (double) value;
  }
  if (utility || (numeric && (!R_FINITE(lower) || !R_FINITE(upper)))) {
    *failed_field = PARADOX_DOMAIN_FIELD_BOUNDS;
    UNPROTECT(1);
    return R_NilValue;
  }
  /* Reuse the admitted outward metadata, but never its self-reference.  The
   * finalizer owns the result shell and names vector and installs a public
   * data.table self-reference for that detached shell. */
  SHALLOW_DUPLICATE_ATTRIB(snapshot, domain);
  if (Rf_isS4(domain) || !exact_domain_outer_attributes(domain) ||
      !exact_domain_column_names(domain) ||
      !class_is_builtin_domain(domain, cls) ||
      !exact_domain_outer_attributes(snapshot) ||
      !exact_domain_column_names(snapshot) ||
      !class_is_builtin_domain(snapshot, cls)) {
    UNPROTECT(1);
    return R_NilValue;
  }
  Rf_setAttrib(snapshot, Rf_install(".internal.selfref"), R_NilValue);
  SEXP result = PROTECT(paradox_finalize_data_table(snapshot));
  UNPROTECT(2);
  return result;
}

static int checked_add(R_xlen_t *total, R_xlen_t increment) {
  if (increment < 0 || *total > R_XLEN_T_MAX - increment) {
    return FALSE;
  }
  *total += increment;
  return TRUE;
}

static inline void account_work(R_xlen_t *work_since_interrupt) {
  ++*work_since_interrupt;
  if (*work_since_interrupt >= PARADOX_INTERRUPT_CHECK_INTERVAL) {
    R_CheckUserInterrupt();
    *work_since_interrupt = 0;
  }
}

static int character_precedes(SEXP values, R_xlen_t left, R_xlen_t right) {
  return strcmp(
    CHAR(STRING_ELT(values, left)),
    CHAR(STRING_ELT(values, right))
  ) <= 0;
}

static void stable_character_order(SEXP values, R_xlen_t *order,
    R_xlen_t *workspace,
    R_xlen_t size, R_xlen_t *work_since_interrupt) {
  for (R_xlen_t index = 0; index < size; ++index) {
    account_work(work_since_interrupt);
    order[index] = index;
  }

  for (R_xlen_t width = 1; width < size;) {
    for (R_xlen_t begin = 0; begin < size;) {
      const R_xlen_t middle = begin > size - width ? size : begin + width;
      const R_xlen_t remaining = size - middle;
      const R_xlen_t end = remaining < width ? size : middle + width;
      R_xlen_t left = begin;
      R_xlen_t right = middle;
      R_xlen_t output = begin;

      while (left < middle && right < end) {
        account_work(work_since_interrupt);
        workspace[output++] = character_precedes(
          values,
          order[left],
          order[right]
        )
          ? order[left++]
          : order[right++];
      }
      while (left < middle) {
        account_work(work_since_interrupt);
        workspace[output++] = order[left++];
      }
      while (right < end) {
        account_work(work_since_interrupt);
        workspace[output++] = order[right++];
      }

      if (end == size) {
        break;
      }
      begin = end;
    }

    for (R_xlen_t index = 0; index < size; ++index) {
      account_work(work_since_interrupt);
      order[index] = workspace[index];
    }

    if (width > size / 2) {
      break;
    }
    width *= 2;
  }
}

static SEXP character_vector(const char *const *values, R_xlen_t size) {
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    SET_STRING_ELT(result, index, Rf_mkChar(values[index]));
  }
  UNPROTECT(1);
  return result;
}

static SEXP set_plain_table_attributes(SEXP table,
    const char *const *column_names, R_xlen_t column_count,
    R_xlen_t row_count) {
  SEXP names = PROTECT(character_vector(column_names, column_count));
  const char *const class_names[] = {"data.frame"};
  SEXP classes = PROTECT(character_vector(class_names, 1));
  Rf_setAttrib(table, R_NamesSymbol, names);
  Rf_setAttrib(table, R_ClassSymbol, classes);

  SEXP row_names;
  if (row_count == 0) {
    row_names = PROTECT(Rf_allocVector(INTSXP, 0));
  } else {
    row_names = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(row_names)[0] = NA_INTEGER;
    INTEGER(row_names)[1] = -(int) row_count;
  }
  Rf_setAttrib(table, R_RowNamesSymbol, row_names);
  UNPROTECT(3);
  return table;
}

static SEXP new_table(const char *const *column_names,
    const SEXPTYPE *column_types, R_xlen_t column_count,
    R_xlen_t row_count) {
  SEXP table = PROTECT(Rf_allocVector(VECSXP, column_count));
  for (R_xlen_t column = 0; column < column_count; ++column) {
    SEXP value = PROTECT(Rf_allocVector(column_types[column], row_count));
    SET_VECTOR_ELT(table, column, value);
    UNPROTECT(1);
  }
  SEXP result = PROTECT(set_plain_table_attributes(
    table,
    column_names,
    column_count,
    row_count
  ));
  UNPROTECT(2);
  return result;
}

static void copy_permanent_value(SEXP destination, R_xlen_t row,
    SEXP source) {
  switch (TYPEOF(destination)) {
  case STRSXP:
    SET_STRING_ELT(destination, row, STRING_ELT(source, 0));
    return;
  case INTSXP:
    INTEGER(destination)[row] = INTEGER_ELT(source, 0);
    return;
  case REALSXP:
    if (TYPEOF(source) == REALSXP) {
      REAL(destination)[row] = REAL_ELT(source, 0);
    } else {
      const int value = INTEGER_ELT(source, 0);
      REAL(destination)[row] = value == NA_INTEGER
        ? NA_REAL
        : (double) value;
    }
    return;
  case VECSXP:
    SET_VECTOR_ELT(destination, row, VECTOR_ELT(source, 0));
    return;
  default:
    Rf_error("Internal error: unsupported permanent Domain column type");
  }
}

SEXP paradox_param_set_construct(SEXP domains) {
  static const char *const names_only[] = {"names"};
  if (TYPEOF(domains) != VECSXP || ALTREP(domains) || Rf_isS4(domains) ||
      Rf_isObject(domains) ||
      !paradox_api_has_only_attributes(domains, names_only, 1)) {
    Rf_error("ParamSet parameters must be supplied as an ordinary named list");
  }

  const R_xlen_t size = XLENGTH(domains);
  if (size > INT_MAX) {
    Rf_error("ParamSet contains too many parameters");
  }

  SEXP ids = PROTECT(paradox_stored_attribute(domains, R_NamesSymbol));
  if (size == 0) {
    if (!ordinary_names(ids, 0, TRUE)) {
      UNPROTECT(1);
      Rf_error("ParamSet parameter names must be an ordinary character vector");
    }
  } else if (!ordinary_names(ids, size, FALSE)) {
    UNPROTECT(1);
    Rf_error("ParamSet parameters must have ordinary character names");
  }

  SEXP stable_ids = PROTECT(Rf_allocVector(STRSXP, size));
  SEXP domain_snapshots = PROTECT(Rf_allocVector(VECSXP, size));

  R_xlen_t tag_count = 0;
  R_xlen_t trafo_count = 0;
  R_xlen_t init_count = 0;
  SEXPTYPE permanent_column_types[DOMAIN_TAGS];
  for (R_xlen_t column = 0; column < DOMAIN_TAGS; ++column) {
    permanent_column_types[column] = domain_column_types[column];
  }
  if (size > 0) {
    permanent_column_types[DOMAIN_LOWER] = INTSXP;
    permanent_column_types[DOMAIN_UPPER] = INTSXP;
    permanent_column_types[DOMAIN_TOLERANCE] = INTSXP;
  }

  for (R_xlen_t row = 0; row < size; ++row) {
    if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP id = STRING_ELT(ids, row);
    if (!string_is_strict_id(id)) {
      UNPROTECT(3);
      Rf_error(
        "ParamSet parameter names must be nonempty, non-missing strict ASCII IDs"
      );
    }
    SET_STRING_ELT(stable_ids, row, id);

    SEXP domain = PROTECT(VECTOR_ELT(domains, row));
    paradox_domain_field_t failed_field = PARADOX_DOMAIN_FIELD_NONE;
    SEXP snapshot = PROTECT(snapshot_domain(domain, &failed_field));
    if (snapshot == R_NilValue) {
      UNPROTECT(5);
      if (failed_field != PARADOX_DOMAIN_FIELD_NONE) {
        Rf_error(
          "ParamSet parameter Domain has noncanonical field `%s`",
          paradox_domain_field_name(failed_field)
        );
      }
      Rf_error("ParamSet parameters must be canonical built-in Domain objects");
    }
    SET_VECTOR_ELT(domain_snapshots, row, snapshot);

    for (enum domain_column column = DOMAIN_LOWER;
        column <= DOMAIN_TOLERANCE;
        column = (enum domain_column) (column + 1)) {
      if (TYPEOF(VECTOR_ELT(snapshot, column)) == REALSXP) {
        permanent_column_types[column] = REALSXP;
      }
    }

    SEXP tags = VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_TAGS), 0);
    if (!checked_add(&tag_count, XLENGTH(tags))) {
      UNPROTECT(5);
      Rf_error("ParamSet tag state exceeds the supported size");
    }
    if (VECTOR_ELT(VECTOR_ELT(snapshot, DOMAIN_TRAFO), 0) != R_NilValue) {
      ++trafo_count;
    }
    if (LOGICAL_ELT(VECTOR_ELT(snapshot, DOMAIN_INIT_GIVEN), 0)) {
      ++init_count;
    }
    UNPROTECT(2);
  }
  ids = stable_ids;

  R_xlen_t *order = NULL;
  R_xlen_t *workspace = NULL;
  R_xlen_t work_since_interrupt = 0;
  if (size > 0) {
    order = paradox_temporary_alloc(size, sizeof(*order));
    workspace = paradox_temporary_alloc(size, sizeof(*workspace));
    stable_character_order(
      ids,
      order,
      workspace,
      size,
      &work_since_interrupt
    );
    for (R_xlen_t index = 1; index < size; ++index) {
      account_work(&work_since_interrupt);
      if (strcmp(
          CHAR(STRING_ELT(ids, order[index - 1])),
          CHAR(STRING_ELT(ids, order[index]))
        ) == 0) {
        UNPROTECT(3);
        Rf_error("ParamSet parameter names must be unique");
      }
    }
  }

  SEXP params = PROTECT(new_table(
    permanent_column_names,
    permanent_column_types,
    DOMAIN_TAGS,
    size
  ));
  for (R_xlen_t row = 0; row < size; ++row) {
    if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP domain = VECTOR_ELT(domain_snapshots, row);
    SET_STRING_ELT(VECTOR_ELT(params, DOMAIN_ID), row, STRING_ELT(ids, row));
    for (enum domain_column column = DOMAIN_CLS;
        column < DOMAIN_TAGS;
        column = (enum domain_column) (column + 1)) {
      copy_permanent_value(
        VECTOR_ELT(params, column),
        row,
        VECTOR_ELT(domain, column)
      );
    }
  }
  const char *const tag_column_names[] = {"id", "tag"};
  const SEXPTYPE tag_column_types[] = {STRSXP, STRSXP};
  SEXP tags = PROTECT(new_table(
    tag_column_names,
    tag_column_types,
    2,
    tag_count
  ));
  R_xlen_t tag_row = 0;
  for (R_xlen_t position = 0; position < size; ++position) {
    if (position != 0 &&
        position % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    R_xlen_t row = order[position];
    SEXP row_tags = VECTOR_ELT(
      VECTOR_ELT(VECTOR_ELT(domain_snapshots, row), DOMAIN_TAGS),
      0
    );
    for (R_xlen_t tag = 0; tag < XLENGTH(row_tags); ++tag) {
      if (tag_row != 0 &&
          tag_row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
        R_CheckUserInterrupt();
      }
      if (tag_row >= tag_count) {
        Rf_error("Internal error: ParamSet tag output exceeded its capacity");
      }
      SET_STRING_ELT(VECTOR_ELT(tags, 0), tag_row, STRING_ELT(ids, row));
      SET_STRING_ELT(VECTOR_ELT(tags, 1), tag_row, STRING_ELT(row_tags, tag));
      ++tag_row;
    }
  }
  if (tag_row != tag_count) {
    Rf_error("Internal error: incomplete ParamSet tag output");
  }
  const char *const trafo_column_names[] = {"id", "trafo"};
  const SEXPTYPE trafo_column_types[] = {STRSXP, VECSXP};
  SEXP trafos = PROTECT(new_table(
    trafo_column_names,
    trafo_column_types,
    2,
    trafo_count
  ));
  R_xlen_t trafo_row = 0;
  for (R_xlen_t position = 0; position < size; ++position) {
    if (position != 0 &&
        position % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    const R_xlen_t row = order[position];
    SEXP trafo = VECTOR_ELT(
      VECTOR_ELT(VECTOR_ELT(domain_snapshots, row), DOMAIN_TRAFO),
      0
    );
    if (trafo != R_NilValue) {
      if (trafo_row >= trafo_count) {
        Rf_error("Internal error: ParamSet trafo output exceeded its capacity");
      }
      SET_STRING_ELT(
        VECTOR_ELT(trafos, 0),
        trafo_row,
        STRING_ELT(ids, row)
      );
      SET_VECTOR_ELT(VECTOR_ELT(trafos, 1), trafo_row, trafo);
      ++trafo_row;
    }
  }
  if (trafo_row != trafo_count) {
    Rf_error("Internal error: incomplete ParamSet trafo output");
  }

  SEXP requirements = PROTECT(Rf_allocVector(VECSXP, size));
  for (R_xlen_t row = 0; row < size; ++row) {
    if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SET_VECTOR_ELT(
      requirements,
      row,
      VECTOR_ELT(
        VECTOR_ELT(VECTOR_ELT(domain_snapshots, row), DOMAIN_REQUIREMENTS),
        0
      )
    );
  }

  SEXP init_values = PROTECT(Rf_allocVector(VECSXP, init_count));
  SEXP init_names = PROTECT(Rf_allocVector(STRSXP, init_count));
  R_xlen_t init_row = 0;
  for (R_xlen_t row = 0; row < size; ++row) {
    if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP domain = VECTOR_ELT(domain_snapshots, row);
    if (LOGICAL_ELT(VECTOR_ELT(domain, DOMAIN_INIT_GIVEN), 0)) {
      if (init_row >= init_count) {
        Rf_error("Internal error: ParamSet init output exceeded its capacity");
      }
      SET_VECTOR_ELT(
        init_values,
        init_row,
        VECTOR_ELT(VECTOR_ELT(domain, DOMAIN_INIT), 0)
      );
      SET_STRING_ELT(init_names, init_row, STRING_ELT(ids, row));
      ++init_row;
    }
  }
  if (init_row != init_count) {
    Rf_error("Internal error: incomplete ParamSet init output");
  }
  Rf_setAttrib(init_values, R_NamesSymbol, init_names);

  SEXP result = PROTECT(Rf_allocVector(VECSXP, RESULT_COUNT));
  SET_VECTOR_ELT(result, RESULT_PARAMS, params);
  SET_VECTOR_ELT(result, RESULT_TAGS, tags);
  SET_VECTOR_ELT(result, RESULT_TRAFOS, trafos);
  SET_VECTOR_ELT(result, RESULT_REQUIREMENTS, requirements);
  SET_VECTOR_ELT(result, RESULT_INIT_VALUES, init_values);
  SEXP names = PROTECT(character_vector(result_names, RESULT_COUNT));
  Rf_setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(11);
  return result;
}
