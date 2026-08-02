#include <limits.h>
#include <stddef.h>
#include <string.h>

#include "paradox.h"
#include "paramset_domain_common.h"
#include <R_ext/Utils.h>

#include "domain_admission.h"
#include "r_api_compat.h"
#include "r_utils.h"
#include "shell_auth.h"

enum construction_result {
  RESULT_PARAMS = 0,
  RESULT_TAGS,
  RESULT_TRAFOS,
  RESULT_REQUIREMENTS,
  RESULT_INIT_VALUES,
  RESULT_COUNT
};

static paradox_domain_field_t domain_column_field(
    enum paradox_domain_column column) {
  switch (column) {
  case PARADOX_DOMAIN_ID: return PARADOX_DOMAIN_FIELD_ID;
  case PARADOX_DOMAIN_CLS:
  case PARADOX_DOMAIN_STORAGE_TYPE:
    return PARADOX_DOMAIN_FIELD_CLASS_STORAGE;
  case PARADOX_DOMAIN_GROUPING: return PARADOX_DOMAIN_FIELD_GROUPING;
  case PARADOX_DOMAIN_CARGO: return PARADOX_DOMAIN_FIELD_CARGO;
  case PARADOX_DOMAIN_LOWER:
  case PARADOX_DOMAIN_UPPER:
  case PARADOX_DOMAIN_TOLERANCE:
    return PARADOX_DOMAIN_FIELD_BOUNDS;
  case PARADOX_DOMAIN_LEVELS: return PARADOX_DOMAIN_FIELD_LEVELS;
  case PARADOX_DOMAIN_SPECIAL_VALS: return PARADOX_DOMAIN_FIELD_SPECIAL_VALUES;
  case PARADOX_DOMAIN_DEFAULT: return PARADOX_DOMAIN_FIELD_DEFAULT;
  case PARADOX_DOMAIN_TAGS: return PARADOX_DOMAIN_FIELD_TAGS;
  case PARADOX_DOMAIN_TRAFO: return PARADOX_DOMAIN_FIELD_TRAFO;
  case PARADOX_DOMAIN_REQUIREMENTS: return PARADOX_DOMAIN_FIELD_REQUIREMENTS;
  case PARADOX_DOMAIN_INIT_GIVEN:
  case PARADOX_DOMAIN_INIT:
    return PARADOX_DOMAIN_FIELD_INIT;
  case PARADOX_DOMAIN_COLUMN_COUNT:
    break;
  }
  return PARADOX_DOMAIN_FIELD_NONE;
}

static const SEXPTYPE domain_column_types[PARADOX_DOMAIN_COLUMN_COUNT] = {
  STRSXP, STRSXP, STRSXP, VECSXP, REALSXP, REALSXP, REALSXP, VECSXP,
  VECSXP, VECSXP, STRSXP, VECSXP, VECSXP, VECSXP, LGLSXP, VECSXP
};

static const char *const result_names[RESULT_COUNT] = {
  "params", "tags", "trafos", "requirements", "init_values"
};

typedef struct {
  SEXP selfref_symbol;
  SEXP repr_symbol;
  SEXP names;
  SEXP classes;
  SEXP row_names;
  SEXP selfref;
  SEXP repr;
  R_xlen_t count;
  int valid;
} domain_outer_metadata_t;

static void capture_domain_outer_attribute(
    SEXP tag, SEXP value, void *data) {
  domain_outer_metadata_t *metadata = data;
  if (!metadata->valid || TYPEOF(tag) != SYMSXP ||
      value == R_NilValue) {
    metadata->valid = FALSE;
    return;
  }
  SEXP *destination = NULL;
  if (tag == R_NamesSymbol) {
    destination = &metadata->names;
  } else if (tag == R_ClassSymbol) {
    destination = &metadata->classes;
  } else if (tag == R_RowNamesSymbol) {
    destination = &metadata->row_names;
  } else if (tag == metadata->selfref_symbol) {
    destination = &metadata->selfref;
  } else if (tag == metadata->repr_symbol) {
    destination = &metadata->repr;
  } else {
    metadata->valid = FALSE;
    return;
  }
  if (*destination != R_NilValue) {
    metadata->valid = FALSE;
    return;
  }
  *destination = value;
  ++metadata->count;
}

/*
 * Capture one complete public Domain metadata generation. Both non-global
 * symbols are interned before selection; the bounded mapper and every
 * subsequent field check are allocation-free, so no compact-row-name
 * expansion or pending finalizer can splice later selectors.
 */
static int capture_domain_outer_metadata(
    SEXP domain, domain_outer_metadata_t *metadata) {
  SEXP selfref_symbol = Rf_install(".internal.selfref");
  SEXP repr_symbol = Rf_install("repr");
  *metadata = (domain_outer_metadata_t) {
    selfref_symbol,
    repr_symbol,
    R_NilValue,
    R_NilValue,
    R_NilValue,
    R_NilValue,
    R_NilValue,
    0,
    TRUE
  };
  R_xlen_t count = 0;
  return !Rf_isS4(domain) &&
    paradox_api_map_bounded_stored_attributes(
      domain,
      5,
      capture_domain_outer_attribute,
      metadata,
      &count
    ) && metadata->valid && metadata->count == count;
}

static int class_is_builtin_domain(SEXP domain, SEXP cls) {
  if (TYPEOF(cls) != STRSXP || ALTREP(cls) || Rf_isS4(cls) ||
      XLENGTH(cls) != 1) {
    return FALSE;
  }

  domain_outer_metadata_t metadata;
  if (!capture_domain_outer_metadata(domain, &metadata)) return FALSE;
  SEXP classes = metadata.classes;
  if (TYPEOF(classes) != STRSXP || ALTREP(classes) || Rf_isS4(classes) ||
      Rf_isObject(classes) || !paradox_api_has_no_attributes(classes) ||
      XLENGTH(classes) != 4) {
    return FALSE;
  }

  SEXP class_name = STRING_ELT(cls, 0);
  if (!paradox_domain_string_is(class_name, "ParamDbl") &&
      !paradox_domain_string_is(class_name, "ParamInt") &&
      !paradox_domain_string_is(class_name, "ParamFct") &&
      !paradox_domain_string_is(class_name, "ParamLgl") &&
      !paradox_domain_string_is(class_name, "ParamUty")) {
    return FALSE;
  }

  SEXP first = STRING_ELT(classes, 0);
  SEXP second = STRING_ELT(classes, 1);
  SEXP third = STRING_ELT(classes, 2);
  SEXP fourth = STRING_ELT(classes, 3);
  const int supported = first != NA_STRING &&
    strcmp(CHAR(first), CHAR(class_name)) == 0 &&
    paradox_domain_string_is(second, "Domain") && paradox_domain_string_is(third, "data.table") &&
    paradox_domain_string_is(fourth, "data.frame");
  return supported;
}

static int exact_domain_outer_attributes(SEXP domain) {
  domain_outer_metadata_t metadata;
  if (!capture_domain_outer_metadata(domain, &metadata)) return FALSE;
  if (TYPEOF(metadata.row_names) != INTSXP ||
      ALTREP(metadata.row_names) ||
      Rf_isS4(metadata.row_names) || Rf_isObject(metadata.row_names) ||
      !paradox_api_has_no_attributes(metadata.row_names)) {
    return FALSE;
  }
  const R_xlen_t row_name_count = XLENGTH(metadata.row_names);
  const int exact_one_row =
    (row_name_count == 1 &&
      INTEGER_ELT(metadata.row_names, 0) == 1) ||
    (row_name_count == 2 &&
      INTEGER_ELT(metadata.row_names, 0) == NA_INTEGER &&
      (INTEGER_ELT(metadata.row_names, 1) == 1 ||
        INTEGER_ELT(metadata.row_names, 1) == -1));
  /* The `repr` carrier's shape rule has one owner in the shared Domain
   * metadata layer; construction states no second spelling of it. */
  return exact_one_row &&
    (metadata.selfref == R_NilValue ||
      (TYPEOF(metadata.selfref) == EXTPTRSXP &&
        !Rf_isS4(metadata.selfref))) &&
    paradox_domain_repr_carrier_is_ordinary(metadata.repr);
}

static int exact_domain_column_names(SEXP domain) {
  domain_outer_metadata_t metadata;
  if (!capture_domain_outer_metadata(domain, &metadata)) return FALSE;
  SEXP names = metadata.names;
  if (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
      Rf_isObject(names) || !paradox_api_has_no_attributes(names) ||
      XLENGTH(names) != PARADOX_DOMAIN_COLUMN_COUNT) {
    return FALSE;
  }
  for (R_xlen_t column = 0; column < PARADOX_DOMAIN_COLUMN_COUNT; ++column) {
    if (!paradox_domain_string_is(STRING_ELT(names, column), paradox_domain_column_names[column])) {
      return FALSE;
    }
  }
  return TRUE;
}

static int cargo_nested_container_name(SEXP name) {
  return name != NA_STRING &&
    (paradox_domain_string_is(name, "disable_in_tune") || paradox_domain_string_is(name, "logscale") ||
      paradox_domain_string_is(name, "repr"));
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
  /*
   * The semantic snapshot owns a coherent name/value generation.  Continue
   * interpreting nested cargo through that generation, rather than pairing
   * its captured values with the pre-allocation names object selected above.
   */
  names = Rf_getAttrib(result, R_NamesSymbol);
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
    const int attributes_ok = paradox_domain_string_is(name, "disable_in_tune")
      ? paradox_api_has_only_attributes(value, names_only, 1)
      : paradox_api_has_no_attributes(value);
    if (!attributes_ok) {
      UNPROTECT(2);
      return R_UnboundValue;
    }
    const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
    const int expected = paradox_domain_string_is(name, "disable_in_tune")
      ? type == VECSXP
      : paradox_domain_string_is(name, "logscale")
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

SEXP paradox_snapshot_domain_nested(SEXP source,
    enum paradox_domain_column column,
    R_xlen_t *work_since_interrupt) {
  if (column == PARADOX_DOMAIN_CARGO) {
    return snapshot_domain_cargo(source);
  }
  if (column == PARADOX_DOMAIN_LEVELS) {
    if (source == R_NilValue) {
      return source;
    }
    const SEXPTYPE type = (SEXPTYPE) TYPEOF(source);
    if ((type != STRSXP && type != LGLSXP) || ALTREP(source) ||
        Rf_isS4(source) ||
        Rf_isObject(source) ||
        !paradox_api_has_no_attributes(source)) {
      return R_UnboundValue;
    }
    SEXP result = PROTECT(paradox_snapshot_semantic_vector(source));
    Rf_setAttrib(result, R_NamesSymbol, R_NilValue);
    UNPROTECT(1);
    return result;
  }
  if (column == PARADOX_DOMAIN_SPECIAL_VALS) {
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
  if (column == PARADOX_DOMAIN_REQUIREMENTS) {
    return paradox_snapshot_builtin_requirements(
      source,
      work_since_interrupt
    );
  }
  return source;
}

static int snapshot_domain_column(SEXP snapshot,
    enum paradox_domain_column column,
    paradox_domain_field_t *failed_field) {
  SEXP value = PROTECT(VECTOR_ELT(snapshot, column));
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(value);
  const int numeric_column = column == PARADOX_DOMAIN_LOWER ||
    column == PARADOX_DOMAIN_UPPER || column == PARADOX_DOMAIN_TOLERANCE;
  if (ALTREP(value) || Rf_isS4(value) || Rf_isObject(value) ||
      !paradox_api_has_no_attributes(value) || (numeric_column
      ? type != INTSXP && type != REALSXP
        : type != domain_column_types[column]) ||
      XLENGTH(value) != 1) {
    *failed_field = domain_column_field(column);
    UNPROTECT(1);
    return FALSE;
  }

  SEXP copy = PROTECT(paradox_snapshot_semantic_vector(value));
  if (column == PARADOX_DOMAIN_CARGO || column == PARADOX_DOMAIN_LEVELS ||
      column == PARADOX_DOMAIN_SPECIAL_VALS ||
      column == PARADOX_DOMAIN_REQUIREMENTS) {
    R_xlen_t work_since_interrupt = 0;
    SEXP nested = PROTECT(paradox_snapshot_domain_nested(
      VECTOR_ELT(copy, 0),
      column,
      &work_since_interrupt
    ));
    if (nested == R_UnboundValue) {
      *failed_field = domain_column_field(column);
      UNPROTECT(3);
      return FALSE;
    }
    SET_VECTOR_ELT(copy, 0, nested);
    UNPROTECT(1);
  }
  SET_VECTOR_ELT(snapshot, column, copy);
  UNPROTECT(2);
  return TRUE;
}

/* Capture each column exactly once into ordinary length-one vectors. This is
 * both an ALTREP materialization boundary and an independent GC root for every
 * child used after the constructor starts allocating its result tables. */
static SEXP snapshot_domain(SEXP domain,
    paradox_domain_field_t *failed_field) {
  *failed_field = PARADOX_DOMAIN_FIELD_NONE;
  if (TYPEOF(domain) != VECSXP || ALTREP(domain) || Rf_isS4(domain) ||
      XLENGTH(domain) != PARADOX_DOMAIN_COLUMN_COUNT) {
    return R_NilValue;
  }

  SEXP names = PROTECT(Rf_allocVector(
    STRSXP,
    PARADOX_DOMAIN_COLUMN_COUNT
  ));
  SEXP snapshot = PROTECT(Rf_allocVector(
    VECSXP,
    PARADOX_DOMAIN_COLUMN_COUNT
  ));
  if (!Rf_isObject(domain) || !exact_domain_outer_attributes(domain) ||
      !paradox_capture_list_identities(domain, names, snapshot)) {
    UNPROTECT(2);
    return R_NilValue;
  }
  for (R_xlen_t column = 0; column < PARADOX_DOMAIN_COLUMN_COUNT; ++column) {
    if (!paradox_domain_string_is(STRING_ELT(names, column), paradox_domain_column_names[column])) {
      UNPROTECT(2);
      return R_NilValue;
    }
  }

  /*
   * Snapshot the kind selectors and special-values carrier first.  Every
   * remaining column may allocate, and requirements/default/init can expose a
   * leaf through a semantic operation.  The special leaf policy therefore
   * needs an operation-local receipt before those columns are touched.
   */
  static const enum paradox_domain_column early_columns[] = {
    PARADOX_DOMAIN_CLS,
    PARADOX_DOMAIN_STORAGE_TYPE,
    PARADOX_DOMAIN_SPECIAL_VALS
  };
  for (size_t index = 0;
      index < sizeof(early_columns) / sizeof(early_columns[0]);
      ++index) {
    if (!snapshot_domain_column(
        snapshot,
        early_columns[index],
        failed_field
      )) {
      UNPROTECT(2);
      return R_NilValue;
    }
  }

  R_xlen_t work_since_interrupt = 0;
  paradox_special_values_receipt_t special_receipt;
  if (!paradox_prepare_builtin_special_values(
      VECTOR_ELT(snapshot, PARADOX_DOMAIN_CLS),
      VECTOR_ELT(snapshot, PARADOX_DOMAIN_STORAGE_TYPE),
      VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_SPECIAL_VALS), 0),
      &special_receipt,
      &work_since_interrupt
    )) {
    *failed_field = class_is_builtin_domain(
      domain,
      VECTOR_ELT(snapshot, PARADOX_DOMAIN_CLS)
    )
      ? PARADOX_DOMAIN_FIELD_SPECIAL_VALUES
      : PARADOX_DOMAIN_FIELD_CLASS_STORAGE;
    UNPROTECT(2);
    return R_NilValue;
  }
  paradox_own_builtin_special_value_leaves(
    VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_SPECIAL_VALS), 0),
    special_receipt.typed
  );

  for (enum paradox_domain_column column = PARADOX_DOMAIN_ID;
      column < PARADOX_DOMAIN_COLUMN_COUNT;
      column = (enum paradox_domain_column) (column + 1)) {
    if (column == PARADOX_DOMAIN_CLS ||
        column == PARADOX_DOMAIN_STORAGE_TYPE ||
        column == PARADOX_DOMAIN_SPECIAL_VALS) {
      continue;
    }
    if (!snapshot_domain_column(snapshot, column, failed_field)) {
      UNPROTECT(2);
      return R_NilValue;
    }
  }

  SEXP cls = VECTOR_ELT(snapshot, PARADOX_DOMAIN_CLS);
  if (!class_is_builtin_domain(domain, cls)) {
    *failed_field = PARADOX_DOMAIN_FIELD_CLASS_STORAGE;
    UNPROTECT(2);
    return R_NilValue;
  }

  if (!paradox_domain_string_is(STRING_ELT(cls, 0), "ParamUty")) {
    SEXP stable_default = PROTECT(paradox_snapshot_builtin_value_leaf(
      VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_DEFAULT), 0)
    ));
    SET_VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_DEFAULT), 0, stable_default);
    UNPROTECT(1);
    if (LOGICAL_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_INIT_GIVEN), 0) == TRUE) {
      SEXP stable_init = PROTECT(paradox_snapshot_builtin_value_leaf(
        VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_INIT), 0)
      ));
      SET_VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_INIT), 0, stable_init);
      UNPROTECT(1);
    }
  }

  SEXP tags = VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_TAGS), 0);
  if (TYPEOF(tags) != STRSXP || ALTREP(tags) || Rf_isS4(tags) ||
      !paradox_api_has_no_attributes(tags)) {
    *failed_field = PARADOX_DOMAIN_FIELD_TAGS;
    UNPROTECT(2);
    return R_NilValue;
  }
  SEXP stable_tags = PROTECT(paradox_snapshot_semantic_vector(tags));
  SET_VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_TAGS), 0, stable_tags);
  UNPROTECT(1);

  paradox_builtin_domain_kind_t admitted_kind;
  if (!paradox_admit_builtin_domain_row(
      VECTOR_ELT(snapshot, PARADOX_DOMAIN_ID),
      VECTOR_ELT(snapshot, PARADOX_DOMAIN_CLS),
      VECTOR_ELT(snapshot, PARADOX_DOMAIN_GROUPING),
      VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_CARGO), 0),
      VECTOR_ELT(snapshot, PARADOX_DOMAIN_LOWER),
      VECTOR_ELT(snapshot, PARADOX_DOMAIN_UPPER),
      VECTOR_ELT(snapshot, PARADOX_DOMAIN_TOLERANCE),
      VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_LEVELS), 0),
      VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_SPECIAL_VALS), 0),
      VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_DEFAULT), 0),
      VECTOR_ELT(snapshot, PARADOX_DOMAIN_STORAGE_TYPE),
      VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_TAGS), 0),
      VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_TRAFO), 0),
      VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_REQUIREMENTS), 0),
      VECTOR_ELT(snapshot, PARADOX_DOMAIN_INIT_GIVEN),
      VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_INIT), 0),
      &special_receipt,
      &admitted_kind,
      failed_field,
      NULL,
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
  SEXP cls = VECTOR_ELT(snapshot, PARADOX_DOMAIN_CLS);
  const int utility = paradox_domain_string_is(STRING_ELT(cls, 0), "ParamUty");
  const int numeric = paradox_domain_string_is(STRING_ELT(cls, 0), "ParamDbl") ||
    paradox_domain_string_is(STRING_ELT(cls, 0), "ParamInt");
  double lower;
  double upper;
  if (TYPEOF(VECTOR_ELT(snapshot, PARADOX_DOMAIN_LOWER)) == REALSXP) {
    lower = REAL_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_LOWER), 0);
  } else {
    const int value = INTEGER_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_LOWER), 0);
    lower = (double) value;
  }
  if (TYPEOF(VECTOR_ELT(snapshot, PARADOX_DOMAIN_UPPER)) == REALSXP) {
    upper = REAL_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_UPPER), 0);
  } else {
    const int value = INTEGER_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_UPPER), 0);
    upper = (double) value;
  }
  if (utility || (numeric && (!R_FINITE(lower) || !R_FINITE(upper)))) {
    *failed_field = PARADOX_DOMAIN_FIELD_BOUNDS;
    UNPROTECT(1);
    return R_NilValue;
  }
  /*
   * Reuse the admitted outward metadata, but never expose its caller-owned
   * raw pairlist to R's shallow duplicator. `snapshot` is a fresh
   * attribute-free shell; the bounded copier retains the exact top-level
   * metadata identities and rejects a finalizer splice. `repr` is the one
   * package-defined opaque presentation attribute: it may legitimately contain
   * closures (for example a function-valued factor token), so retain its exact
   * selected identity outside the general metadata owner. Every structural
   * attribute and column still passes through that strict owner.
   */
  paradox_copy_bounded_shallow_attributes(
    snapshot,
    domain,
    PARADOX_SHALLOW_ATTRIBUTES_ALL,
    "Domain metadata changed while being snapshotted"
  );
  if (Rf_isS4(domain) || !exact_domain_outer_attributes(domain) ||
      !exact_domain_column_names(domain) ||
      !class_is_builtin_domain(domain, cls) ||
      !exact_domain_outer_attributes(snapshot) ||
      !exact_domain_column_names(snapshot) ||
      !class_is_builtin_domain(snapshot, cls)) {
    UNPROTECT(1);
    return R_NilValue;
  }
  domain_outer_metadata_t metadata;
  if (!capture_domain_outer_metadata(snapshot, &metadata)) {
    UNPROTECT(1);
    return R_NilValue;
  }
  SEXP repr = PROTECT(metadata.repr);
  Rf_setAttrib(snapshot, metadata.selfref_symbol, R_NilValue);
  Rf_setAttrib(snapshot, metadata.repr_symbol, R_NilValue);
  SEXP result = PROTECT(paradox_finalize_data_table(snapshot));
  if (repr != R_NilValue) {
    Rf_setAttrib(result, metadata.repr_symbol, repr);
  }
  UNPROTECT(3);
  return result;
}

static int checked_add(R_xlen_t *total, R_xlen_t increment) {
  if (increment < 0 || *total > R_XLEN_T_MAX - increment) {
    return FALSE;
  }
  *total += increment;
  return TRUE;
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
    paradox_account_work(work_since_interrupt);
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
        paradox_account_work(work_since_interrupt);
        workspace[output++] = character_precedes(
          values,
          order[left],
          order[right]
        )
          ? order[left++]
          : order[right++];
      }
      while (left < middle) {
        paradox_account_work(work_since_interrupt);
        workspace[output++] = order[left++];
      }
      while (right < end) {
        paradox_account_work(work_since_interrupt);
        workspace[output++] = order[right++];
      }

      if (end == size) {
        break;
      }
      begin = end;
    }

    for (R_xlen_t index = 0; index < size; ++index) {
      paradox_account_work(work_since_interrupt);
      order[index] = workspace[index];
    }

    if (width > size / 2) {
      break;
    }
    width *= 2;
  }
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

SEXP paradox_param_set_construct(SEXP domains,
    SEXP allow_dangling_dependencies) {
  if (!paradox_param_set_assert_values_is_exact(
      allow_dangling_dependencies
    )) {
    Rf_error(
      "`allow_dangling_dependencies` must be TRUE or FALSE"
    );
  }
  static const char *const names_only[] = {"names"};
  if (TYPEOF(domains) != VECSXP || ALTREP(domains) || Rf_isS4(domains) ||
      Rf_isObject(domains)) {
    Rf_error("ParamSet parameters must be supplied as an ordinary named list");
  }

  const R_xlen_t size = XLENGTH(domains);
  if (size > INT_MAX) {
    Rf_error("ParamSet contains too many parameters");
  }

  /* The caller's own name attribute is only ever read through the shared
   * capture below; the captured `stable_ids` is the one name carrier this
   * constructor consumes. */
  SEXP stable_ids = PROTECT(Rf_allocVector(STRSXP, size));
  SEXP domain_snapshots = PROTECT(Rf_allocVector(VECSXP, size));
  if (Rf_isObject(domains) ||
      !paradox_api_has_only_attributes(domains, names_only, 1)) {
    UNPROTECT(2);
    Rf_error("ParamSet parameters must be supplied as an ordinary named list");
  }
  if (!paradox_capture_list_identities(
        domains,
        stable_ids,
        domain_snapshots
      )) {
    UNPROTECT(2);
    Rf_error("ParamSet parameters must have ordinary character names");
  }

  R_xlen_t tag_count = 0;
  R_xlen_t trafo_count = 0;
  R_xlen_t init_count = 0;
  SEXPTYPE permanent_column_types[PARADOX_DOMAIN_TAGS];
  for (R_xlen_t column = 0; column < PARADOX_DOMAIN_TAGS; ++column) {
    permanent_column_types[column] = domain_column_types[column];
  }
  if (size > 0) {
    permanent_column_types[PARADOX_DOMAIN_LOWER] = INTSXP;
    permanent_column_types[PARADOX_DOMAIN_UPPER] = INTSXP;
    permanent_column_types[PARADOX_DOMAIN_TOLERANCE] = INTSXP;
  }

  for (R_xlen_t row = 0; row < size; ++row) {
    if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP id = STRING_ELT(stable_ids, row);
    if (!paradox_string_is_strict_id(id)) {
      UNPROTECT(2);
      Rf_error(
        "ParamSet parameter names must be nonempty, non-missing strict ASCII IDs"
      );
    }
    SEXP domain = PROTECT(VECTOR_ELT(domain_snapshots, row));
    paradox_domain_field_t failed_field = PARADOX_DOMAIN_FIELD_NONE;
    SEXP snapshot = PROTECT(snapshot_domain(domain, &failed_field));
    if (snapshot == R_NilValue) {
      UNPROTECT(4);
      if (failed_field != PARADOX_DOMAIN_FIELD_NONE) {
        Rf_error(
          "ParamSet parameter Domain has noncanonical field `%s`",
          paradox_domain_field_name(failed_field)
        );
      }
      Rf_error("ParamSet parameters must be canonical built-in Domain objects");
    }
    SET_VECTOR_ELT(domain_snapshots, row, snapshot);

    for (enum paradox_domain_column column = PARADOX_DOMAIN_LOWER;
        column <= PARADOX_DOMAIN_TOLERANCE;
        column = (enum paradox_domain_column) (column + 1)) {
      if (TYPEOF(VECTOR_ELT(snapshot, column)) == REALSXP) {
        permanent_column_types[column] = REALSXP;
      }
    }

    SEXP tags = VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_TAGS), 0);
    if (!checked_add(&tag_count, XLENGTH(tags))) {
      UNPROTECT(4);
      Rf_error("ParamSet tag state exceeds the supported size");
    }
    if (VECTOR_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_TRAFO), 0) != R_NilValue) {
      ++trafo_count;
    }
    if (LOGICAL_ELT(VECTOR_ELT(snapshot, PARADOX_DOMAIN_INIT_GIVEN), 0)) {
      ++init_count;
    }
    UNPROTECT(2);
  }
  SEXP ids = stable_ids;

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
      paradox_account_work(&work_since_interrupt);
      if (strcmp(
          CHAR(STRING_ELT(ids, order[index - 1])),
          CHAR(STRING_ELT(ids, order[index]))
        ) == 0) {
        UNPROTECT(2);
        Rf_error("ParamSet parameter names must be unique");
      }
    }
  }

  SEXP params = PROTECT(paradox_domain_new_plain_table(
    paradox_domain_column_names,
    permanent_column_types,
    PARADOX_DOMAIN_TAGS,
    size
  ));
  for (R_xlen_t row = 0; row < size; ++row) {
    if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP domain = VECTOR_ELT(domain_snapshots, row);
    SET_STRING_ELT(VECTOR_ELT(params, PARADOX_DOMAIN_ID), row, STRING_ELT(ids, row));
    for (enum paradox_domain_column column = PARADOX_DOMAIN_CLS;
        column < PARADOX_DOMAIN_TAGS;
        column = (enum paradox_domain_column) (column + 1)) {
      copy_permanent_value(
        VECTOR_ELT(params, column),
        row,
        VECTOR_ELT(domain, column)
      );
    }
  }
  const char *const tag_column_names[] = {"id", "tag"};
  const SEXPTYPE tag_column_types[] = {STRSXP, STRSXP};
  SEXP tags = PROTECT(paradox_domain_new_plain_table(
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
      VECTOR_ELT(VECTOR_ELT(domain_snapshots, row), PARADOX_DOMAIN_TAGS),
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
  SEXP trafos = PROTECT(paradox_domain_new_plain_table(
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
      VECTOR_ELT(VECTOR_ELT(domain_snapshots, row), PARADOX_DOMAIN_TRAFO),
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
        VECTOR_ELT(VECTOR_ELT(domain_snapshots, row), PARADOX_DOMAIN_REQUIREMENTS),
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
    if (LOGICAL_ELT(VECTOR_ELT(domain, PARADOX_DOMAIN_INIT_GIVEN), 0)) {
      if (init_row >= init_count) {
        Rf_error("Internal error: ParamSet init output exceeded its capacity");
      }
      SET_VECTOR_ELT(
        init_values,
        init_row,
        VECTOR_ELT(VECTOR_ELT(domain, PARADOX_DOMAIN_INIT), 0)
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
  SEXP names = PROTECT(paradox_domain_character_vector(result_names, RESULT_COUNT));
  Rf_setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(10);
  return result;
}
