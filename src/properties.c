#include "paradox.h"
#include <R_ext/Arith.h>
#include <R_ext/Utils.h>

#include "paramset_domain_common.h"
#include "paramset_params_internal.h"
#include "core_state.h"
#include "domain_admission.h"
#include "r_utils.h"

enum public_property_selector {
  PUBLIC_PROPERTY_CLASS = PARADOX_PROPERTY_COUNT,
  PUBLIC_PROPERTY_LOWER,
  PUBLIC_PROPERTY_UPPER,
  PUBLIC_PROPERTY_LEVELS,
  PUBLIC_PROPERTY_STORAGE_TYPE,
  PUBLIC_PROPERTY_SPECIAL_VALS,
  PUBLIC_PROPERTY_DEFAULT,
  PUBLIC_PROPERTY_ALL_NUMERIC,
  PUBLIC_PROPERTY_ALL_CATEGORICAL,
  PUBLIC_PROPERTY_ALL_BOUNDED,
  PUBLIC_PROPERTY_LENGTH,
  PUBLIC_PROPERTY_IS_EMPTY,
  PUBLIC_PROPERTY_COUNT
};

static double param_nlevels(paradox_builtin_domain_kind_t cls,
    double lower, double upper,
    SEXP levels) {
  switch (cls) {
  case PARADOX_BUILTIN_DOMAIN_DBL:
    if (ISNAN(lower) || ISNAN(upper)) {
      return NA_REAL;
    }
    return lower == upper ? 1.0 : R_PosInf;
  case PARADOX_BUILTIN_DOMAIN_INT:
    return paradox_integer_domain_nlevels(lower, upper);
  case PARADOX_BUILTIN_DOMAIN_FCT: {
    /* Keep the function result in its native type before conversion.  This is
     * equivalent but also satisfies GCC's useful -Wbad-function-cast audit. */
    const R_xlen_t size = XLENGTH(levels);
    return (double) size;
  }
  case PARADOX_BUILTIN_DOMAIN_LGL:
    return 2.0;
  case PARADOX_BUILTIN_DOMAIN_UTY:
    return R_PosInf;
  case PARADOX_BUILTIN_DOMAIN_UNKNOWN:
    return NA_REAL;
  }
  return NA_REAL;
}

static int param_is_number(paradox_builtin_domain_kind_t cls) {
  return cls == PARADOX_BUILTIN_DOMAIN_DBL || cls == PARADOX_BUILTIN_DOMAIN_INT;
}

static int param_is_categ(paradox_builtin_domain_kind_t cls) {
  return cls == PARADOX_BUILTIN_DOMAIN_FCT || cls == PARADOX_BUILTIN_DOMAIN_LGL;
}

static int param_is_bounded(paradox_builtin_domain_kind_t cls,
    double lower, double upper) {
  switch (cls) {
  case PARADOX_BUILTIN_DOMAIN_DBL:
  case PARADOX_BUILTIN_DOMAIN_INT:
    return R_FINITE(lower) && R_FINITE(upper);
  case PARADOX_BUILTIN_DOMAIN_FCT:
  case PARADOX_BUILTIN_DOMAIN_LGL:
    return TRUE;
  case PARADOX_BUILTIN_DOMAIN_UTY:
  case PARADOX_BUILTIN_DOMAIN_UNKNOWN:
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
  const enum paradox_domain_column column =
    public_property_column(selector);
  const int typed_leaves = column == PARADOX_DOMAIN_SPECIAL_VALS ||
    column == PARADOX_DOMAIN_DEFAULT;
  SEXP columns[PARADOX_DOMAIN_COLUMN_COUNT];
  paradox_domain_select_columns(params, "ParamSet storage", ".params",
    (1U << PARADOX_DOMAIN_ID) | (1U << column) |
      (typed_leaves ? (1U << PARADOX_DOMAIN_CLS) : 0U), columns);

  /* Retain the admitted children themselves, not only their table: a pending
   * finalizer can replace a table field during outward allocation. Only the
   * selected columns matter: unrelated private semantics are not a getter's
   * input contract. The row detacher owns nested representation admission. */
  SEXP source = PROTECT(columns[column]);
  SEXP ids = PROTECT(columns[PARADOX_DOMAIN_ID]);
  SEXP classes = PROTECT(columns[PARADOX_DOMAIN_CLS]);
  if (TYPEOF(ids) != STRSXP || ALTREP(ids)) {
    Rf_error("Corrupt ParamSet storage: `id` must be ordinary character");
  }
  const R_xlen_t size = XLENGTH(ids);
  if (column == PARADOX_DOMAIN_LOWER || column == PARADOX_DOMAIN_UPPER) {
    paradox_require_numeric_column(source, size, "ParamSet storage",
      paradox_domain_column_names[column]);
  } else {
    paradox_require_column(source, paradox_domain_column_types[column], size,
      paradox_domain_column_names[column]);
  }
  if (typed_leaves) paradox_require_column(classes, STRSXP, size, "cls");
  SEXP result;
  if (column == PARADOX_DOMAIN_LEVELS ||
      column == PARADOX_DOMAIN_SPECIAL_VALS ||
      column == PARADOX_DOMAIN_DEFAULT) {
    result = PROTECT(Rf_allocVector(VECSXP, size));
    for (R_xlen_t row = 0; row < size; ++row) {
      paradox_account_work(&work_since_interrupt);
      const int typed = column != PARADOX_DOMAIN_LEVELS &&
        !paradox_domain_string_is(STRING_ELT(classes, row), "ParamUty");
      SEXP detached = PROTECT(paradox_detach_domain_row_field(
        VECTOR_ELT(source, row),
        column,
        typed,
        &work_since_interrupt
      ));
      if (detached == R_UnboundValue) {
        UNPROTECT(5);
        Rf_error("Corrupt ParamSet storage: cannot detach public property");
      }
      SET_VECTOR_ELT(result, row, detached);
      UNPROTECT(1);
    }
  } else {
    result = PROTECT(paradox_snapshot_semantic_vector(source));
  }
  SEXP names = PROTECT(paradox_snapshot_semantic_vector(ids));
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(5);
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
  if (selector == PUBLIC_PROPERTY_LENGTH || selector == PUBLIC_PROPERTY_IS_EMPTY) {
    SEXP columns[PARADOX_DOMAIN_COLUMN_COUNT];
    paradox_domain_select_columns(params, "ParamSet storage", ".params",
      1U << PARADOX_DOMAIN_ID, columns);
    SEXP ids = columns[PARADOX_DOMAIN_ID];
    if (TYPEOF(ids) != STRSXP || ALTREP(ids) || XLENGTH(ids) > INT_MAX) {
      Rf_error("Corrupt ParamSet storage: invalid `id` column");
    }
    const R_xlen_t size = XLENGTH(ids);
    return selector == PUBLIC_PROPERTY_LENGTH
      ? Rf_ScalarInteger((int) size)
      : Rf_ScalarLogical(size == 0);
  }
  const int aggregate = selector >= PUBLIC_PROPERTY_ALL_NUMERIC;
  if (selector >= PARADOX_PROPERTY_COUNT && !aggregate) {
    return detached_public_property(params, selector);
  }
  const paradox_property_t selected = (paradox_property_t) (aggregate
    ? selector - PUBLIC_PROPERTY_ALL_NUMERIC + PARADOX_PROPERTY_IS_NUMBER
    : selector);
  const int uses_bounds = selected == PARADOX_PROPERTY_NLEVELS ||
    selected == PARADOX_PROPERTY_IS_BOUNDED;
  const int uses_levels = selected == PARADOX_PROPERTY_NLEVELS;
  SEXP columns[PARADOX_DOMAIN_COLUMN_COUNT];
  paradox_domain_select_columns(
    params, "ParamSet storage", ".params",
    (1U << PARADOX_DOMAIN_ID) | (1U << PARADOX_DOMAIN_CLS) |
      (uses_bounds ? (1U << PARADOX_DOMAIN_LOWER) |
        (1U << PARADOX_DOMAIN_UPPER) : 0U) |
      (uses_levels ? (1U << PARADOX_DOMAIN_LEVELS) : 0U),
    columns
  );
  SEXP ids = PROTECT(columns[PARADOX_DOMAIN_ID]);
  SEXP classes = PROTECT(columns[PARADOX_DOMAIN_CLS]);
  SEXP lower = PROTECT(columns[PARADOX_DOMAIN_LOWER]);
  SEXP upper = PROTECT(columns[PARADOX_DOMAIN_UPPER]);
  SEXP levels = PROTECT(columns[PARADOX_DOMAIN_LEVELS]);

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
  if (uses_levels) paradox_require_column(levels, VECSXP, size, "levels");

  const SEXPTYPE value_type = selected == PARADOX_PROPERTY_NLEVELS
    ? (size == 0 ? INTSXP : REALSXP)
    : LGLSXP;
  SEXP value = PROTECT(Rf_allocVector(value_type, aggregate ? 1 : size));
  /* Reuse the shared canonical-column diagnostics, but deliberately discard
   * their temporary raw views. Element APIs below keep no vector pointer live
   * across an interrupt poll. */
  if (uses_bounds) {
    paradox_require_numeric_column(lower, size, "ParamSet storage", "lower");
    paradox_require_numeric_column(upper, size, "ParamSet storage", "upper");
  }

  int all = TRUE;
  for (R_xlen_t row = 0; row < size; ++row) {
    if (row != 0 && row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }

    const paradox_builtin_domain_kind_t cls =
      paradox_resolve_builtin_domain_class_char(STRING_ELT(classes, row));
    if (cls == PARADOX_BUILTIN_DOMAIN_UNKNOWN) {
      Rf_error(
        "Corrupt ParamSet storage: unsupported parameter class at row %.0f",
        (double) (row + 1)
      );
    }
    if (selected == PARADOX_PROPERTY_NLEVELS) {
      SEXP row_levels = VECTOR_ELT(levels, row);
      if (cls == PARADOX_BUILTIN_DOMAIN_FCT) {
        if (TYPEOF(row_levels) != STRSXP) {
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
      SET_REAL_ELT(
        value,
        row,
        param_nlevels(cls, paradox_numeric_elt(lower, row),
          paradox_numeric_elt(upper, row), row_levels)
      );
    } else {
      const int flag = selected == PARADOX_PROPERTY_IS_NUMBER
        ? param_is_number(cls)
        : selected == PARADOX_PROPERTY_IS_CATEG
          ? param_is_categ(cls)
          : param_is_bounded(cls, paradox_numeric_elt(lower, row),
              paradox_numeric_elt(upper, row));
      if (aggregate) {
        all = all && flag;
        if (!all) break;
      } else {
        SET_LOGICAL_ELT(value, row, flag);
      }
    }
  }

  if (aggregate) {
    /* An unread private row cannot affect memory safety. Scalar reductions
     * short-circuit instead of diagnosing unrelated later corruption. */
    SET_LOGICAL_ELT(value, 0, all);
    UNPROTECT(6);
    return value;
  }

  /* Attribute vectors are mutable under data.table::setattr(), so even the
   * outward names carrier must not alias the capsule's `id` column. */
  SEXP names = PROTECT(paradox_snapshot_semantic_vector(ids));
  Rf_setAttrib(value, R_NamesSymbol, names);
  UNPROTECT(7);
  return value;
}

SEXP paradox_param_set_get_property(SEXP private_environment, SEXP self,
    SEXP property) {
  /* Refresh once, then retain the kernel's complete input itself: retaining
   * only its parent would not root a table detached by a pending finalizer.
   * No allocation intervenes between the ordinary payload selection and its
   * protection. Neither the payload nor its table makes a round trip to R. */
  SEXP state = paradox_param_set_core_state(private_environment, self);
  SEXP params = PROTECT(VECTOR_ELT(state, PARADOX_CORE_PARAMS));
  SEXP result = paradox_param_set_property(params, property);
  UNPROTECT(1);
  return result;
}
