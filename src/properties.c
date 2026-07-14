#include <string.h>

#include "paradox.h"
#include <R_ext/Arith.h>
#include <R_ext/Utils.h>

#include "r_utils.h"

typedef enum {
  PARAM_CLASS_UNKNOWN = 0,
  PARAM_CLASS_DBL,
  PARAM_CLASS_INT,
  PARAM_CLASS_FCT,
  PARAM_CLASS_LGL,
  PARAM_CLASS_UTY
} param_class_t;

typedef enum {
  PROPERTY_NLEVELS = 0,
  PROPERTY_IS_NUMBER,
  PROPERTY_IS_CATEG,
  PROPERTY_IS_BOUNDED,
  PROPERTY_COUNT
} property_t;

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
    return upper - lower + 1.0;
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

SEXP paradox_param_set_property(SEXP params, SEXP property) {
  if (TYPEOF(params) != VECSXP) {
    Rf_error("Corrupt ParamSet storage: `.params` must be a list");
  }
  if (TYPEOF(property) != INTSXP || ALTREP(property) ||
      XLENGTH(property) != 1 ||
      INTEGER(property)[0] == NA_INTEGER || INTEGER(property)[0] < 0 ||
      INTEGER(property)[0] >= PROPERTY_COUNT) {
    Rf_error("Internal error: invalid ParamSet property selector");
  }

  const property_t selected = (property_t) INTEGER(property)[0];
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

  const SEXPTYPE value_type = selected == PROPERTY_NLEVELS
    ? (size == 0 ? INTSXP : REALSXP)
    : LGLSXP;
  SEXP value = PROTECT(Rf_allocVector(value_type, size));
  SEXP known = PROTECT(Rf_allocVector(LGLSXP, size));
  /* Acquire read-only vector pointers only after the last allocation which
   * precedes their use. Their exact ordinary owners remain in `roots`. */
  const paradox_numeric_column_t lower_data = paradox_get_numeric_column(
    lower,
    size,
    "ParamSet storage",
    "lower"
  );
  const paradox_numeric_column_t upper_data = paradox_get_numeric_column(
    upper,
    size,
    "ParamSet storage",
    "upper"
  );
  double *real_value = value_type == REALSXP ? REAL(value) : NULL;
  int *logical_value = selected == PROPERTY_NLEVELS ? NULL : LOGICAL(value);
  int *known_data = LOGICAL(known);

  for (R_xlen_t row = 0; row < size; ++row) {
    if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }

    const param_class_t cls = classify_param(STRING_ELT(classes, row));
    const double row_lower = paradox_numeric_at(&lower_data, row);
    const double row_upper = paradox_numeric_at(&upper_data, row);
    SEXP row_levels = PROTECT(VECTOR_ELT(levels, row));
    if (selected == PROPERTY_NLEVELS && cls == PARAM_CLASS_FCT) {
      if (TYPEOF(row_levels) != STRSXP) {
        UNPROTECT(1);
        Rf_error(
          "Corrupt ParamSet storage: each `levels` element for `ParamFct` must be character"
        );
      }
      if (ALTREP(row_levels)) {
        /* Numeric and other non-character factor levels are represented by a
         * deferred-string ALTREP after p_fct() installs its automatic
         * transformation.  Asking that provider for its length can execute
         * arbitrary code, so decline the complete native property operation
         * and let the established grouped R/S3 path observe it once. */
        UNPROTECT(4);
        return R_NilValue;
      }
    }
    known_data[row] = cls != PARAM_CLASS_UNKNOWN;

    if (selected == PROPERTY_NLEVELS) {
      real_value[row] = param_nlevels(
        cls,
        row_lower,
        row_upper,
        row_levels
      );
    } else if (cls == PARAM_CLASS_UNKNOWN) {
      logical_value[row] = NA_LOGICAL;
    } else if (selected == PROPERTY_IS_NUMBER) {
      logical_value[row] = param_is_number(cls);
    } else if (selected == PROPERTY_IS_CATEG) {
      logical_value[row] = param_is_categ(cls);
    } else {
      logical_value[row] = param_is_bounded(
        cls,
        row_lower,
        row_upper
      );
    }
    UNPROTECT(1);
  }

  Rf_setAttrib(value, R_NamesSymbol, ids);

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, value);
  SET_VECTOR_ELT(result, 1, known);
  UNPROTECT(4);
  return result;
}
