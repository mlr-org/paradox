#include <string.h>

#include "paradox.h"
#include <R_ext/Arith.h>
#include <R_ext/Utils.h>

#include "paramset_domain_common.h"
#include "paramset_params_internal.h"
#include "r_utils.h"

typedef enum {
  PARAM_CLASS_UNKNOWN = 0,
  PARAM_CLASS_DBL,
  PARAM_CLASS_INT,
  PARAM_CLASS_FCT,
  PARAM_CLASS_LGL,
  PARAM_CLASS_UTY
} param_class_t;

enum public_property_selector {
  PUBLIC_PROPERTY_CLASS = PARADOX_PROPERTY_COUNT,
  PUBLIC_PROPERTY_LOWER,
  PUBLIC_PROPERTY_UPPER,
  PUBLIC_PROPERTY_LEVELS,
  PUBLIC_PROPERTY_STORAGE_TYPE,
  PUBLIC_PROPERTY_SPECIAL_VALS,
  PUBLIC_PROPERTY_DEFAULT,
  PUBLIC_PROPERTY_COUNT
};

enum property_root_slot {
  PROPERTY_ROOT_IDS = 0,
  PROPERTY_ROOT_CLASSES,
  PROPERTY_ROOT_LOWER,
  PROPERTY_ROOT_UPPER,
  PROPERTY_ROOT_LEVELS,
  PROPERTY_ROOT_COUNT
};

static param_class_t classify_param(SEXP cls) {
  if (cls == NA_STRING) {
    return PARAM_CLASS_UNKNOWN;
  }

  const char *name = CHAR(cls);
  if (strcmp(name, "ParamDbl") == 0) {
    return PARAM_CLASS_DBL;
  }
  if (strcmp(name, "ParamInt") == 0) {
    return PARAM_CLASS_INT;
  }
  if (strcmp(name, "ParamFct") == 0) {
    return PARAM_CLASS_FCT;
  }
  if (strcmp(name, "ParamLgl") == 0) {
    return PARAM_CLASS_LGL;
  }
  if (strcmp(name, "ParamUty") == 0) {
    return PARAM_CLASS_UTY;
  }
  return PARAM_CLASS_UNKNOWN;
}

static double param_nlevels(param_class_t cls, double lower, double upper,
    SEXP levels) {
  switch (cls) {
  case PARAM_CLASS_DBL:
    if (ISNAN(lower) || ISNAN(upper)) {
      return NA_REAL;
    }
    return lower == upper ? 1.0 : R_PosInf;
  case PARAM_CLASS_INT:
    return paradox_integer_domain_nlevels(lower, upper);
  case PARAM_CLASS_FCT: {
    /* Keep the function result in its native type before conversion.  This is
     * equivalent but also satisfies GCC's useful -Wbad-function-cast audit. */
    const R_xlen_t size = XLENGTH(levels);
    return (double) size;
  }
  case PARAM_CLASS_LGL:
    return 2.0;
  case PARAM_CLASS_UTY:
    return R_PosInf;
  case PARAM_CLASS_UNKNOWN:
    return NA_REAL;
  }
  return NA_REAL;
}

static int param_is_number(param_class_t cls) {
  return cls == PARAM_CLASS_DBL || cls == PARAM_CLASS_INT;
}

static int param_is_categ(param_class_t cls) {
  return cls == PARAM_CLASS_FCT || cls == PARAM_CLASS_LGL;
}

static int param_is_bounded(param_class_t cls, double lower, double upper) {
  switch (cls) {
  case PARAM_CLASS_DBL:
  case PARAM_CLASS_INT:
    return R_FINITE(lower) && R_FINITE(upper);
  case PARAM_CLASS_FCT:
  case PARAM_CLASS_LGL:
    return TRUE;
  case PARAM_CLASS_UTY:
  case PARAM_CLASS_UNKNOWN:
    return FALSE;
  }
  return FALSE;
}

static enum paradox_domain_column public_property_column(int selector) {
  switch (selector) {
  case PUBLIC_PROPERTY_CLASS: return PARADOX_DOMAIN_CLS;
  case PUBLIC_PROPERTY_LOWER: return PARADOX_DOMAIN_LOWER;
  case PUBLIC_PROPERTY_UPPER: return PARADOX_DOMAIN_UPPER;
  case PUBLIC_PROPERTY_LEVELS: return PARADOX_DOMAIN_LEVELS;
  case PUBLIC_PROPERTY_STORAGE_TYPE: return PARADOX_DOMAIN_STORAGE_TYPE;
  case PUBLIC_PROPERTY_SPECIAL_VALS: return PARADOX_DOMAIN_SPECIAL_VALS;
  case PUBLIC_PROPERTY_DEFAULT: return PARADOX_DOMAIN_DEFAULT;
  default:
    Rf_error("Internal error: invalid detached property selector");
  }
  return PARADOX_DOMAIN_ID;
}

static SEXP detached_public_property(SEXP params, int selector) {
  R_xlen_t work_since_interrupt = 0;
  paradox_domain_params_t parsed;
  R_xlen_t unused_row = 0;
  if (!paradox_domain_validate_params(
      params,
      R_NilValue,
      TRUE,
      &parsed,
      &unused_row,
      &work_since_interrupt
    )) {
    Rf_error("Corrupt ParamSet storage: invalid canonical `.params` table");
  }
  const enum paradox_domain_column column =
    public_property_column(selector);
  SEXP source = VECTOR_ELT(params, column);
  SEXP result;
  if (column == PARADOX_DOMAIN_LEVELS ||
      column == PARADOX_DOMAIN_SPECIAL_VALS ||
      column == PARADOX_DOMAIN_DEFAULT) {
    result = PROTECT(Rf_allocVector(VECSXP, parsed.row_count));
    for (R_xlen_t row = 0; row < parsed.row_count; ++row) {
      paradox_account_work(&work_since_interrupt);
      const int typed = !paradox_domain_string_is(
        STRING_ELT(parsed.classes, row),
        "ParamUty"
      );
      SEXP detached = PROTECT(paradox_detach_domain_row_field(
        VECTOR_ELT(source, row),
        column,
        typed,
        &work_since_interrupt
      ));
      if (detached == R_UnboundValue) {
        UNPROTECT(2);
        Rf_error("Corrupt ParamSet storage: cannot detach public property");
      }
      SET_VECTOR_ELT(result, row, detached);
      UNPROTECT(1);
    }
  } else {
    result = PROTECT(paradox_snapshot_semantic_vector(source));
  }
  SEXP names = PROTECT(paradox_snapshot_semantic_vector(parsed.ids));
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
}

SEXP paradox_param_set_property(SEXP params, SEXP property) {
  if (TYPEOF(params) != VECSXP) {
    Rf_error("Corrupt ParamSet storage: `.params` must be a list");
  }
  if (TYPEOF(property) != INTSXP || ALTREP(property) ||
      XLENGTH(property) != 1 ||
      INTEGER_ELT(property, 0) == NA_INTEGER ||
      INTEGER_ELT(property, 0) < 0 ||
      INTEGER_ELT(property, 0) >= PUBLIC_PROPERTY_COUNT) {
    Rf_error("Internal error: invalid ParamSet property selector");
  }

  const int selector = INTEGER_ELT(property, 0);
  if (selector >= PARADOX_PROPERTY_COUNT) {
    return detached_public_property(params, selector);
  }
  const paradox_property_t selected = (paradox_property_t) selector;
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, PROPERTY_ROOT_COUNT));
  SEXP ids = paradox_get_named_column(params, ".params", "id");
  SET_VECTOR_ELT(roots, PROPERTY_ROOT_IDS, ids);
  SEXP classes = paradox_get_named_column(params, ".params", "cls");
  SET_VECTOR_ELT(roots, PROPERTY_ROOT_CLASSES, classes);
  SEXP lower = paradox_get_named_column(params, ".params", "lower");
  SET_VECTOR_ELT(roots, PROPERTY_ROOT_LOWER, lower);
  SEXP upper = paradox_get_named_column(params, ".params", "upper");
  SET_VECTOR_ELT(roots, PROPERTY_ROOT_UPPER, upper);
  SEXP levels = paradox_get_named_column(params, ".params", "levels");
  SET_VECTOR_ELT(roots, PROPERTY_ROOT_LEVELS, levels);

  /* Check storage types without observing ALTREP length methods.  This keeps
   * useful corruption diagnostics for vectors of the wrong type while making
   * the ordinary-representation guard the first operation which can dispatch
   * on a correctly typed ALTREP column. */
  if (TYPEOF(ids) != STRSXP) {
    Rf_error("Corrupt ParamSet storage: `id` must have type `character`");
  }
  if (ALTREP(ids) ||
      (TYPEOF(classes) == STRSXP && ALTREP(classes)) ||
      (TYPEOF(levels) == VECSXP && ALTREP(levels))) {
    Rf_error(
      "Corrupt ParamSet storage: property columns must use ordinary representations"
    );
  }

  const R_xlen_t size = XLENGTH(ids);
  paradox_require_column(ids, STRSXP, size, "id");
  paradox_require_column(classes, STRSXP, size, "cls");
  paradox_require_column(levels, VECSXP, size, "levels");

  const SEXPTYPE value_type = selected == PARADOX_PROPERTY_NLEVELS
    ? (size == 0 ? INTSXP : REALSXP)
    : LGLSXP;
  SEXP value = PROTECT(Rf_allocVector(value_type, size));
  /* Reuse the shared canonical-column diagnostics, but deliberately discard
   * their temporary raw views. Element APIs below keep no vector pointer live
   * across an interrupt poll. */
  paradox_require_numeric_column(
    lower,
    size,
    "ParamSet storage",
    "lower"
  );
  paradox_require_numeric_column(
    upper,
    size,
    "ParamSet storage",
    "upper"
  );

  for (R_xlen_t row = 0; row < size; ++row) {
    if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }

    const param_class_t cls = classify_param(STRING_ELT(classes, row));
    if (cls == PARAM_CLASS_UNKNOWN) {
      Rf_error(
        "Corrupt ParamSet storage: unsupported parameter class at row %.0f",
        (double) (row + 1)
      );
    }
    const double row_lower = paradox_numeric_elt(lower, row);
    const double row_upper = paradox_numeric_elt(upper, row);
    SEXP row_levels = PROTECT(VECTOR_ELT(levels, row));
    if (selected == PARADOX_PROPERTY_NLEVELS && cls == PARAM_CLASS_FCT) {
      if (TYPEOF(row_levels) != STRSXP) {
        UNPROTECT(1);
        Rf_error(
          "Corrupt ParamSet storage: each `levels` element for `ParamFct` must be character"
        );
      }
      if (ALTREP(row_levels)) {
        Rf_error(
          "Corrupt ParamSet storage: factor levels must use ordinary representations"
        );
      }
    }

    if (selected == PARADOX_PROPERTY_NLEVELS) {
      SET_REAL_ELT(
        value,
        row,
        param_nlevels(cls, row_lower, row_upper, row_levels)
      );
    } else if (selected == PARADOX_PROPERTY_IS_NUMBER) {
      SET_LOGICAL_ELT(value, row, param_is_number(cls));
    } else if (selected == PARADOX_PROPERTY_IS_CATEG) {
      SET_LOGICAL_ELT(value, row, param_is_categ(cls));
    } else {
      SET_LOGICAL_ELT(
        value,
        row,
        param_is_bounded(cls, row_lower, row_upper)
      );
    }
    UNPROTECT(1);
  }

  /* Attribute vectors are mutable under data.table::setattr(), so even the
   * outward names carrier must not alias the capsule's `id` column. */
  SEXP names = PROTECT(paradox_snapshot_semantic_vector(ids));
  Rf_setAttrib(value, R_NamesSymbol, names);
  UNPROTECT(3);
  return value;
}
