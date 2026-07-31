#include <string.h>

#include "paradox.h"

#include "domain_admission.h"
#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

/*
 * Public Domain operations are the fourth consumer of the canonical
 * Domain-row admission owner, beside constructor final-state validation,
 * ParamSet construction, and ObjectTuneToken Domain admission. This adapter
 * owns only the outward table/column container -- the boundary work the
 * contract explicitly assigns to an operation -- and hands every semantic rule
 * to `paradox_admit_builtin_domain_row()`. It deliberately does not build the
 * detached single-row snapshot that ObjectTuneToken admission builds: these
 * kernels also serve multi-row tables, zero-row tables, `ParamUty`, and
 * unbounded numeric Domains, none of which that snapshot accepts, and a
 * read-only operation must not change the identity of what it validates.
 */

enum admitted_bundle_slot {
  ADMITTED_BUNDLE_COLUMNS = 0,
  ADMITTED_BUNDLE_ROWS,
  ADMITTED_BUNDLE_SCALARS,
  ADMITTED_BUNDLE_SLOT_COUNT
};

enum admitted_scalar_slot {
  ADMITTED_SCALAR_LOWER = 0,
  ADMITTED_SCALAR_UPPER,
  ADMITTED_SCALAR_TOLERANCE,
  ADMITTED_SCALAR_ID,
  ADMITTED_SCALAR_CLS,
  ADMITTED_SCALAR_GROUPING,
  ADMITTED_SCALAR_STORAGE,
  ADMITTED_SCALAR_SLOT_COUNT
};

static const SEXPTYPE domain_column_types[PARADOX_DOMAIN_COLUMN_COUNT] = {
  STRSXP,  /* id */
  STRSXP,  /* cls */
  STRSXP,  /* grouping */
  VECSXP,  /* cargo */
  REALSXP, /* lower -- numeric column, INTSXP also admitted */
  REALSXP, /* upper */
  REALSXP, /* tolerance */
  VECSXP,  /* levels */
  VECSXP,  /* special_vals */
  VECSXP,  /* default */
  STRSXP,  /* storage_type */
  VECSXP,  /* .tags */
  VECSXP,  /* .trafo */
  VECSXP,  /* .requirements */
  LGLSXP,  /* .init_given */
  VECSXP   /* .init */
};

static int ordinary_string_vector(SEXP value) {
  return TYPEOF(value) == STRSXP && !ALTREP(value) && !Rf_isS4(value) &&
    !Rf_isObject(value) && paradox_api_has_no_attributes(value);
}

/*
 * The owner reports one field; several of its rejections already had a more
 * specific public diagnostic. Refining the message on the failure path is the
 * pattern the owner itself uses for duplicate levels: the owner decides
 * accept or reject, the operation decides wording. No rule is restated.
 */
static void report_levels_failure(paradox_builtin_domain_kind_t kind,
    SEXP levels, paradox_domain_field_t failure) {
  if (kind == PARADOX_BUILTIN_DOMAIN_LGL) {
    Rf_error("Corrupt Domain storage: ParamLgl levels must be c(TRUE, FALSE)");
  }
  if (kind == PARADOX_BUILTIN_DOMAIN_FCT) {
    if (TYPEOF(levels) != STRSXP) {
      Rf_error(
        "Corrupt Domain storage: each `levels` element must be character"
      );
    }
    if (ALTREP(levels) || Rf_isS4(levels) || Rf_isObject(levels) ||
        !paradox_api_has_no_attributes(levels)) {
      Rf_error("Corrupt Domain storage: factor levels must be ordinary");
    }
    if (failure != PARADOX_DOMAIN_FIELD_LEVELS_DUPLICATE) {
      for (R_xlen_t index = 0; index < XLENGTH(levels); ++index) {
        if (STRING_ELT(levels, index) == NA_STRING) {
          Rf_error(
            "Corrupt Domain storage: `levels` may not contain missing values"
          );
        }
      }
    }
    Rf_error(
      "Corrupt Domain storage: `levels` must contain unique, non-missing values"
    );
  }
  Rf_error(
    "Corrupt Domain storage: `levels` is not canonical for this Domain kind"
  );
}

static void report_row_failure(paradox_builtin_domain_kind_t kind,
    paradox_domain_field_t failure, SEXP levels, SEXP special_values) {
  switch (failure) {
  case PARADOX_DOMAIN_FIELD_BOUNDS:
    Rf_error("Corrupt Domain storage: invalid numeric bounds or tolerance");
    break;
  case PARADOX_DOMAIN_FIELD_LEVELS:
  case PARADOX_DOMAIN_FIELD_LEVELS_DUPLICATE:
    report_levels_failure(kind, levels, failure);
    break;
  case PARADOX_DOMAIN_FIELD_SPECIAL_VALUES:
    if (TYPEOF(special_values) != VECSXP) {
      Rf_error(
        "Corrupt Domain storage: each `special_vals` element must be a list"
      );
    }
    Rf_error("Corrupt Domain storage: `special_vals` is not canonical");
    break;
  case PARADOX_DOMAIN_FIELD_NONE:
    Rf_error("Corrupt Domain storage");
    break;
  default:
    Rf_error(
      "Corrupt Domain storage: `%s` is not canonical",
      paradox_domain_field_name(failure)
    );
    break;
  }
}

SEXP paradox_admit_public_domain_table(SEXP domain,
    paradox_builtin_domain_kind_t kind, R_xlen_t row_count,
    paradox_admitted_domain_table_t *table,
    R_xlen_t *work_since_interrupt) {
  if (row_count < 0 ||
      row_count > R_XLEN_T_MAX / PARADOX_ADMITTED_ROW_STRIDE) {
    Rf_error("Corrupt Domain storage: unsupported Domain row count");
  }
  /*
   * Allocate every destination first. The capture pass below then pairs each
   * canonical column name with the exact column it selected without another
   * allocation, so a pending finalizer cannot leave the admitted table
   * describing one generation while the operation reads another.
   */
  SEXP bundle = PROTECT(Rf_allocVector(VECSXP, ADMITTED_BUNDLE_SLOT_COUNT));
  SEXP columns = PROTECT(Rf_allocVector(
    VECSXP,
    PARADOX_DOMAIN_COLUMN_COUNT
  ));
  SET_VECTOR_ELT(bundle, ADMITTED_BUNDLE_COLUMNS, columns);
  UNPROTECT(1);
  SEXP rows = PROTECT(Rf_allocVector(
    VECSXP,
    row_count * PARADOX_ADMITTED_ROW_STRIDE
  ));
  SET_VECTOR_ELT(bundle, ADMITTED_BUNDLE_ROWS, rows);
  UNPROTECT(1);
  SEXP scalars = PROTECT(Rf_allocVector(VECSXP, ADMITTED_SCALAR_SLOT_COUNT));
  SET_VECTOR_ELT(bundle, ADMITTED_BUNDLE_SCALARS, scalars);
  UNPROTECT(1);
  for (int slot = ADMITTED_SCALAR_LOWER;
      slot <= ADMITTED_SCALAR_TOLERANCE;
      ++slot) {
    SEXP carrier = PROTECT(Rf_allocVector(REALSXP, 1));
    SET_VECTOR_ELT(scalars, slot, carrier);
    UNPROTECT(1);
  }
  for (int slot = ADMITTED_SCALAR_ID;
      slot <= ADMITTED_SCALAR_STORAGE;
      ++slot) {
    SEXP carrier = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_VECTOR_ELT(scalars, slot, carrier);
    UNPROTECT(1);
  }

  const R_xlen_t buffer_size = row_count == 0 ? 1 : row_count;
  double *numeric_storage = paradox_temporary_alloc(
    buffer_size,
    3U * sizeof(*numeric_storage)
  );

  /*
   * The interpreted column selection is one allocation-free pass. Column shape
   * diagnostics stay here because the outward table container is the
   * operation's own boundary; every semantic rule below belongs to the owner.
   * The four columns only a constructor interprets -- `default`,
   * `.requirements`, `.init_given`, and `.init` -- are deliberately not
   * selected: a public projection legitimately carries a stored TuneToken in
   * `.init`, and no Domain operation reads any of them.
   */
  unsigned int selected_mask = 0U;
  for (int column = 0; column < PARADOX_DOMAIN_COLUMN_COUNT; ++column) {
    if (column == PARADOX_DOMAIN_DEFAULT ||
        column == PARADOX_DOMAIN_REQUIREMENTS ||
        column == PARADOX_DOMAIN_INIT_GIVEN ||
        column == PARADOX_DOMAIN_INIT) {
      continue;
    }
    selected_mask |= 1U << column;
  }
  SEXP selected_columns[PARADOX_DOMAIN_COLUMN_COUNT];
  paradox_domain_select_columns(
    domain,
    "Domain storage",
    "Domain",
    selected_mask,
    selected_columns
  );
  for (int column = 0; column < PARADOX_DOMAIN_COLUMN_COUNT; ++column) {
    if (!((selected_mask >> column) & 1U)) {
      continue;
    }
    SEXP value = selected_columns[column];
    if (domain_column_types[column] == REALSXP) {
      paradox_require_numeric_column(
        value,
        row_count,
        "Domain storage",
        paradox_domain_column_names[column]
      );
    } else {
      paradox_require_column_checked(
        value,
        domain_column_types[column],
        row_count,
        "Domain storage",
        paradox_domain_column_names[column]
      );
    }
    SET_VECTOR_ELT(columns, column, value);
  }

  double *lower_values = numeric_storage;
  double *upper_values = numeric_storage + buffer_size;
  double *tolerance_values = numeric_storage + 2 * buffer_size;
  SEXP lower_column = VECTOR_ELT(columns, PARADOX_DOMAIN_LOWER);
  SEXP upper_column = VECTOR_ELT(columns, PARADOX_DOMAIN_UPPER);
  SEXP tolerance_column = VECTOR_ELT(columns, PARADOX_DOMAIN_TOLERANCE);
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    lower_values[row] = paradox_numeric_elt(lower_column, row);
    upper_values[row] = paradox_numeric_elt(upper_column, row);
    tolerance_values[row] = paradox_numeric_elt(tolerance_column, row);
  }

  SEXP lower_carrier = VECTOR_ELT(scalars, ADMITTED_SCALAR_LOWER);
  SEXP upper_carrier = VECTOR_ELT(scalars, ADMITTED_SCALAR_UPPER);
  SEXP tolerance_carrier = VECTOR_ELT(scalars, ADMITTED_SCALAR_TOLERANCE);
  SEXP id_carrier = VECTOR_ELT(scalars, ADMITTED_SCALAR_ID);
  SEXP cls_carrier = VECTOR_ELT(scalars, ADMITTED_SCALAR_CLS);
  SEXP grouping_carrier = VECTOR_ELT(scalars, ADMITTED_SCALAR_GROUPING);
  SEXP storage_carrier = VECTOR_ELT(scalars, ADMITTED_SCALAR_STORAGE);

  SEXP ids = VECTOR_ELT(columns, PARADOX_DOMAIN_ID);
  SEXP classes = VECTOR_ELT(columns, PARADOX_DOMAIN_CLS);
  SEXP groupings = VECTOR_ELT(columns, PARADOX_DOMAIN_GROUPING);
  SEXP storages = VECTOR_ELT(columns, PARADOX_DOMAIN_STORAGE_TYPE);
  if (!ordinary_string_vector(ids) || !ordinary_string_vector(classes) ||
      !ordinary_string_vector(groupings) ||
      !ordinary_string_vector(storages)) {
    UNPROTECT(1);
    Rf_error("Corrupt Domain storage: schema columns must be ordinary vectors");
  }

  SEXP levels_column = VECTOR_ELT(columns, PARADOX_DOMAIN_LEVELS);
  SEXP special_column = VECTOR_ELT(columns, PARADOX_DOMAIN_SPECIAL_VALS);
  SEXP cargo_column = VECTOR_ELT(columns, PARADOX_DOMAIN_CARGO);
  SEXP tags_column = VECTOR_ELT(columns, PARADOX_DOMAIN_TAGS);
  SEXP trafo_column = VECTOR_ELT(columns, PARADOX_DOMAIN_TRAFO);
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    /*
     * Capture the interpreted row before the owner's own admission can
     * allocate, and keep using exactly those objects afterwards. The kernels
     * read the same captured row, so a value they operate on is always one the
     * owner admitted.
     */
    const R_xlen_t offset = row * PARADOX_ADMITTED_ROW_STRIDE;
    SET_VECTOR_ELT(
      rows,
      offset + PARADOX_ADMITTED_LEVELS,
      VECTOR_ELT(levels_column, row)
    );
    SET_VECTOR_ELT(
      rows,
      offset + PARADOX_ADMITTED_SPECIAL_VALS,
      VECTOR_ELT(special_column, row)
    );
    SET_VECTOR_ELT(
      rows,
      offset + PARADOX_ADMITTED_CARGO,
      VECTOR_ELT(cargo_column, row)
    );
    SET_VECTOR_ELT(
      rows,
      offset + PARADOX_ADMITTED_TAGS,
      VECTOR_ELT(tags_column, row)
    );
    SET_VECTOR_ELT(
      rows,
      offset + PARADOX_ADMITTED_TRAFO,
      VECTOR_ELT(trafo_column, row)
    );

    SET_REAL_ELT(lower_carrier, 0, lower_values[row]);
    SET_REAL_ELT(upper_carrier, 0, upper_values[row]);
    SET_REAL_ELT(tolerance_carrier, 0, tolerance_values[row]);
    SET_STRING_ELT(id_carrier, 0, STRING_ELT(ids, row));
    /* The three uniform schema strings repeat the same interned CHARSXP on
     * every row of a canonical table; rewriting an unchanged element would
     * only pay the write barrier again. */
    if (STRING_ELT(cls_carrier, 0) != STRING_ELT(classes, row)) {
      SET_STRING_ELT(cls_carrier, 0, STRING_ELT(classes, row));
    }
    if (STRING_ELT(grouping_carrier, 0) != STRING_ELT(groupings, row)) {
      SET_STRING_ELT(grouping_carrier, 0, STRING_ELT(groupings, row));
    }
    if (STRING_ELT(storage_carrier, 0) != STRING_ELT(storages, row)) {
      SET_STRING_ELT(storage_carrier, 0, STRING_ELT(storages, row));
    }

    SEXP levels = VECTOR_ELT(rows, offset + PARADOX_ADMITTED_LEVELS);
    SEXP special_values = VECTOR_ELT(
      rows,
      offset + PARADOX_ADMITTED_SPECIAL_VALS
    );
    paradox_special_values_receipt_t receipt;
    if (!paradox_prepare_builtin_special_values(
        cls_carrier,
        storage_carrier,
        special_values,
        &receipt,
        work_since_interrupt
      )) {
      UNPROTECT(1);
      report_row_failure(
        kind,
        PARADOX_DOMAIN_FIELD_SPECIAL_VALUES,
        levels,
        special_values
      );
    }
    paradox_builtin_domain_kind_t admitted_kind =
      PARADOX_BUILTIN_DOMAIN_UNKNOWN;
    paradox_domain_field_t failure = PARADOX_DOMAIN_FIELD_NONE;
    /*
     * The schema half is exactly the rule set these operations interpret.
     * Default, requirement, and initialization admission belongs to the
     * constructor: a public `$domains` projection deliberately carries the
     * stored TuneToken in `.init`, and that is detached by the cold
     * search-space converter rather than by a Domain operation.
     */
    if (!paradox_admit_builtin_domain_schema_row(
        id_carrier,
        cls_carrier,
        grouping_carrier,
        VECTOR_ELT(rows, offset + PARADOX_ADMITTED_CARGO),
        lower_carrier,
        upper_carrier,
        tolerance_carrier,
        levels,
        special_values,
        storage_carrier,
        VECTOR_ELT(rows, offset + PARADOX_ADMITTED_TAGS),
        VECTOR_ELT(rows, offset + PARADOX_ADMITTED_TRAFO),
        &receipt,
        &admitted_kind,
        NULL,
        &failure,
        work_since_interrupt
      )) {
      UNPROTECT(1);
      report_row_failure(kind, failure, levels, special_values);
    }
  }

  table->bundle = bundle;
  table->columns = columns;
  table->rows = rows;
  table->lower = lower_values;
  table->upper = upper_values;
  table->tolerance = tolerance_values;
  table->row_count = row_count;
  UNPROTECT(1);
  return bundle;
}
