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
 * owns only the operation-local outward table/row snapshot -- the boundary
 * work the contract explicitly assigns to an operation -- and hands every
 * semantic rule to `paradox_admit_builtin_domain_row()`. It does not use the
 * bounded single-row ObjectTuneToken snapshot: these kernels also serve
 * multi-row tables, zero-row tables, `ParamUty`, and unbounded numeric
 * Domains. Instead it detaches exactly the mutable interpreted fields named
 * by the operation and terminally reauthenticates their common source
 * generation.
 */

enum domain_admission_root {
  DOMAIN_ADMISSION_ROOT_CLASS = PARADOX_DOMAIN_COLUMN_COUNT,
  DOMAIN_ADMISSION_ROOT_ROW_NAMES,
  DOMAIN_ADMISSION_ROOT_SELFREF,
  DOMAIN_ADMISSION_ROOT_REPR,
  DOMAIN_ADMISSION_ROOT_RARE_GROUPING,
  DOMAIN_ADMISSION_ROOT_ACCEPTED_CLASS,
  DOMAIN_ADMISSION_ROOT_ACCEPTED_GROUPING,
  DOMAIN_ADMISSION_ROOT_ACCEPTED_STORAGE,
  DOMAIN_ADMISSION_ROOT_ROWS,
  DOMAIN_ADMISSION_ROOT_COUNT
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

typedef struct {
  SEXP names;
  int valid;
} exact_optional_names_capture_t;

static void capture_exact_optional_names(SEXP tag, SEXP value, void *data) {
  exact_optional_names_capture_t *capture = data;
  if (!capture->valid || tag != R_NamesSymbol ||
      value == R_NilValue || capture->names != R_NilValue) {
    capture->valid = FALSE;
    return;
  }
  capture->names = value;
}

static SEXP exact_optional_names(SEXP value) {
  /* The overwhelmingly common attr-free carrier remains one constant-time
   * predicate. Otherwise capture the sole names cell through the hard-bounded
   * raw mapper; no untrusted pairlist reaches an unbounded count or selector,
   * including from the allocation-free terminal receipt. */
  if (paradox_api_has_no_attributes(value)) return R_NilValue;
  exact_optional_names_capture_t capture = {R_NilValue, TRUE};
  R_xlen_t attribute_count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
      value,
      1,
      capture_exact_optional_names,
      &capture,
      &attribute_count
    ) || !capture.valid || attribute_count != 1 ||
      capture.names == R_NilValue) {
    return R_UnboundValue;
  }
  SEXP names = capture.names;
  return TYPEOF(names) == STRSXP && !ALTREP(names) &&
    !Rf_isS4(names) && !Rf_isObject(names) &&
    paradox_api_has_no_attributes(names) &&
    XLENGTH(names) == XLENGTH(value)
      ? names
      : R_UnboundValue;
}

static int exact_names_equal(SEXP source, SEXP snapshot) {
  SEXP source_names = exact_optional_names(source);
  SEXP snapshot_names = exact_optional_names(snapshot);
  if (source_names == R_UnboundValue ||
      snapshot_names == R_UnboundValue ||
      (source_names == R_NilValue) != (snapshot_names == R_NilValue)) {
    return FALSE;
  }
  return source_names == R_NilValue ||
    paradox_ordinary_vector_payload_equal(source_names, snapshot_names);
}

static int plain_vector_receipt_current(SEXP source, SEXP snapshot,
    SEXPTYPE type, int names_allowed) {
  if ((SEXPTYPE) TYPEOF(source) != type ||
      (SEXPTYPE) TYPEOF(snapshot) != type ||
      ALTREP(source) || ALTREP(snapshot) ||
      Rf_isS4(source) || Rf_isS4(snapshot) ||
      Rf_isObject(source) || Rf_isObject(snapshot)) {
    return FALSE;
  }
  if (names_allowed) {
    if (!exact_names_equal(source, snapshot)) return FALSE;
  } else if (!paradox_api_has_no_attributes(source) ||
      !paradox_api_has_no_attributes(snapshot)) {
    return FALSE;
  }
  return paradox_ordinary_vector_payload_equal(source, snapshot);
}

static int cargo_nested_name(SEXP name) {
  return name != NA_STRING &&
    (paradox_domain_string_is(name, "disable_in_tune") ||
      paradox_domain_string_is(name, "logscale") ||
      paradox_domain_string_is(name, "repr"));
}

int paradox_domain_cargo_snapshot_is_current(
    SEXP source, SEXP snapshot) {
  if (source == R_NilValue || snapshot == R_NilValue) {
    return source == snapshot;
  }
  if (TYPEOF(source) != VECSXP || TYPEOF(snapshot) != VECSXP ||
      ALTREP(source) || ALTREP(snapshot) ||
      Rf_isS4(source) || Rf_isS4(snapshot) ||
      Rf_isObject(source) || Rf_isObject(snapshot) ||
      XLENGTH(source) != XLENGTH(snapshot) ||
      !exact_names_equal(source, snapshot)) {
    return FALSE;
  }
  SEXP names = exact_optional_names(source);
  if (names == R_UnboundValue ||
      (XLENGTH(source) != 0 && names == R_NilValue)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(source); ++index) {
    SEXP left = VECTOR_ELT(source, index);
    SEXP right = VECTOR_ELT(snapshot, index);
    SEXP name = STRING_ELT(names, index);
    if (!cargo_nested_name(name)) {
      if (left != right) return FALSE;
    } else if (left == R_NilValue || right == R_NilValue) {
      if (left != right) return FALSE;
    } else if (paradox_domain_string_is(name, "disable_in_tune")) {
      if (!plain_vector_receipt_current(left, right, VECSXP, TRUE)) {
        return FALSE;
      }
    } else if (paradox_domain_string_is(name, "logscale")) {
      if (!plain_vector_receipt_current(left, right, LGLSXP, FALSE)) {
        return FALSE;
      }
    } else if (!plain_vector_receipt_current(
        left,
        right,
        STRSXP,
        FALSE
      )) {
      return FALSE;
    }
  }
  return TRUE;
}

static int special_values_receipt_current(SEXP source, SEXP snapshot,
    int typed, unsigned char empty_names_present) {
  if (TYPEOF(source) != VECSXP || TYPEOF(snapshot) != VECSXP ||
      ALTREP(source) || ALTREP(snapshot) ||
      Rf_isS4(source) || Rf_isS4(snapshot) ||
      Rf_isObject(source) || Rf_isObject(snapshot) ||
      XLENGTH(source) != XLENGTH(snapshot) ||
      !exact_names_equal(source, snapshot)) {
    return FALSE;
  }
  /*
   * Empty ordinary special-value shells are deliberately not copied. Their
   * optional names attribute is nevertheless generation state: source and
   * snapshot alias, so comparing the two live views cannot prove whether
   * names were added or removed after admission. The byte receipt was
   * captured immediately after the canonical special-value owner admitted
   * this exact shell.
   */
  if (XLENGTH(source) == 0) {
    SEXP names = exact_optional_names(source);
    if (names == R_UnboundValue ||
        (unsigned char) (names != R_NilValue) != empty_names_present) {
      return FALSE;
    }
  }
  for (R_xlen_t index = 0; index < XLENGTH(source); ++index) {
    SEXP left = VECTOR_ELT(source, index);
    SEXP right = VECTOR_ELT(snapshot, index);
    if (typed
        ? !paradox_builtin_value_leaf_receipt_current(left, right)
        : left != right) {
      return FALSE;
    }
  }
  return TRUE;
}

int paradox_domain_special_values_snapshot_is_current(
    SEXP source, SEXP snapshot, int typed) {
  SEXP snapshot_names = exact_optional_names(snapshot);
  if (snapshot_names == R_UnboundValue || TYPEOF(source) != VECSXP ||
      TYPEOF(snapshot) != VECSXP || ALTREP(source) || ALTREP(snapshot) ||
      Rf_isS4(source) || Rf_isS4(snapshot) || Rf_isObject(source) ||
      Rf_isObject(snapshot) || XLENGTH(source) != XLENGTH(snapshot) ||
      !exact_names_equal(source, snapshot)) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < XLENGTH(source); ++index) {
    SEXP left = VECTOR_ELT(source, index);
    SEXP right = VECTOR_ELT(snapshot, index);
    if (typed
        ? (ALTREP(left)
          ? !paradox_altrep_builtin_value_leaf_metadata_is_current(
              left,
              right
            )
          : !paradox_builtin_value_leaf_receipt_current(left, right))
        : left != right) {
      return FALSE;
    }
  }
  return TRUE;
}

static int same_double_bits(double left, double right) {
  return memcmp(&left, &right, sizeof(left)) == 0;
}

/*
 * Terminal-only read of an already structurally authenticated ordinary
 * numeric column. No allocation, interrupt poll, or callback follows pointer
 * selection until the complete comparison has finished.
 */
static int numeric_column_receipt_current(SEXP source,
    const double *snapshot, R_xlen_t size) {
  if (size == 0) return TRUE;
  if (TYPEOF(source) == REALSXP) {
    return memcmp(
      REAL_RO(source),
      snapshot,
      (size_t) size * sizeof(double)
    ) == 0;
  }
  const int *values = INTEGER_RO(source);
  for (R_xlen_t row = 0; row < size; ++row) {
    const double value =
      values[row] == NA_INTEGER ? NA_REAL : (double) values[row];
    if (!same_double_bits(value, snapshot[row])) return FALSE;
  }
  return TRUE;
}

enum domain_admission_test_phase {
  DOMAIN_ADMISSION_AFTER_CAPTURE = 0,
  DOMAIN_ADMISSION_AFTER_OWNERSHIP = 1
};

static int valid_domain_admission_test_hooks(SEXP hooks) {
  if (Rf_isFunction(hooks)) return TRUE;
  if (TYPEOF(hooks) != VECSXP || ALTREP(hooks) || Rf_isS4(hooks) ||
      Rf_isObject(hooks) || !paradox_api_has_no_attributes(hooks) ||
      XLENGTH(hooks) != 2) {
    return FALSE;
  }
  for (R_xlen_t phase = 0; phase < 2; ++phase) {
    SEXP hook = VECTOR_ELT(hooks, phase);
    if (hook != R_NilValue && !Rf_isFunction(hook)) return FALSE;
  }
  return TRUE;
}

static void run_domain_admission_test_hook(SEXP hooks,
    enum domain_admission_test_phase phase) {
  SEXP hook = R_NilValue;
  if (Rf_isFunction(hooks)) {
    if (phase == DOMAIN_ADMISSION_AFTER_CAPTURE) hook = hooks;
  } else {
    hook = VECTOR_ELT(hooks, (R_xlen_t) phase);
  }
  if (hook == R_NilValue) return;
  SEXP call = PROTECT(Rf_lang1(hook));
  SEXP hook_result = PROTECT(Rf_eval(call, R_BaseEnv));
  (void) hook_result;
  UNPROTECT(2);
}

static SEXP admit_public_domain_table_impl(SEXP domain,
    paradox_builtin_domain_kind_t kind, R_xlen_t row_count,
    unsigned int interpreted,
    paradox_admitted_domain_table_t *table,
    R_xlen_t *work_since_interrupt,
    SEXP capture_hook) {
  if (row_count < 0 ||
      row_count > R_XLEN_T_MAX / PARADOX_ADMITTED_ROW_STRIDE) {
    Rf_error("Corrupt Domain storage: unsupported Domain row count");
  }
  /* The owner's closure decides what the declaration implies; expanding it
   * here as well lets the capture below skip fields no rule will read. */
  interpreted = paradox_domain_interpretation_closure(interpreted);
  /*
   * Reserve the complete permanent protection block before selecting a
   * Domain value. The indexed slots are the scanned roots behind the native
   * column arrays and scalar representatives below; every temporary
   * protection is balanced above this block.
   */
  PROTECT_INDEX root_indices[DOMAIN_ADMISSION_ROOT_COUNT];
  for (int root = 0; root < DOMAIN_ADMISSION_ROOT_COUNT; ++root) {
    PROTECT_WITH_INDEX(R_NilValue, &root_indices[root]);
  }
  SEXP rows = PROTECT(Rf_allocVector(
    VECSXP,
    row_count * PARADOX_ADMITTED_ROW_STRIDE
  ));
  REPROTECT(rows, root_indices[DOMAIN_ADMISSION_ROOT_ROWS]);
  UNPROTECT(1);

  const R_xlen_t buffer_size = row_count == 0 ? 1 : row_count;
  const int captures_bounds =
    (interpreted & PARADOX_DOMAIN_INTERPRET_BOUNDS) != 0;
  const int captures_special_values =
    (interpreted & PARADOX_DOMAIN_INTERPRET_SPECIAL_VALUES) != 0;
  const size_t workspace_element_size =
    (captures_bounds ? 3U * sizeof(double) : 0U) +
    (captures_special_values ? sizeof(unsigned char) : 0U);
  double *numeric_storage = NULL;
  unsigned char *empty_special_names_present = NULL;
  if (workspace_element_size != 0U) {
    void *workspace = paradox_temporary_alloc(
      buffer_size,
      workspace_element_size
    );
    if (captures_bounds) {
      numeric_storage = (double *) workspace;
    }
    if (captures_special_values) {
      empty_special_names_present = (unsigned char *) workspace +
        (captures_bounds
          ? (size_t) buffer_size * 3U * sizeof(double)
          : 0U);
    }
  }

  /*
   * Select the complete canonical outward schema in one allocation-free pass.
   * Presence, uniqueness, storage type, and rectangularity are structural
   * properties of the public Domain table even when this operation does not
   * interpret a column's contents. In particular, `default`,
   * `.requirements`, `.init_given`, and `.init` remain opaque here, but their
   * columns may not be absent, duplicated, or malformed.
   */
  unsigned int selected_mask = 0U;
  for (int column = 0; column < PARADOX_DOMAIN_COLUMN_COUNT; ++column) {
    selected_mask |= 1U << column;
  }
  SEXP selected_columns[PARADOX_DOMAIN_COLUMN_COUNT];
  R_xlen_t selected_positions[PARADOX_DOMAIN_COLUMN_COUNT];
  paradox_domain_outer_metadata_t outward_metadata;
  if (!paradox_domain_capture_outer_metadata(domain, &outward_metadata)) {
    Rf_error(
      "Corrupt Domain storage: outer metadata must be ordinary and bounded"
    );
  }
  paradox_domain_select_captured_columns_with_positions(
    domain,
    outward_metadata.names,
    "Domain storage",
    "Domain",
    selected_mask,
    selected_columns,
    selected_positions
  );
  if (XLENGTH(domain) != PARADOX_DOMAIN_COLUMN_COUNT) {
    Rf_error(
      "Corrupt Domain storage: `Domain` must have exactly %d columns",
      PARADOX_DOMAIN_COLUMN_COUNT
    );
  }
  for (int column = 0; column < PARADOX_DOMAIN_COLUMN_COUNT; ++column) {
    REPROTECT(selected_columns[column], root_indices[column]);
  }
  for (int column = 0; column < PARADOX_DOMAIN_COLUMN_COUNT; ++column) {
    SEXP value = selected_columns[column];
    if (column == PARADOX_DOMAIN_ID) {
      /* The entry probe already authenticated this exact shell and length.
       * Any difference after receipt allocation is a shape replacement, not
       * an independently malformed generation selected by this operation. */
      if (!paradox_domain_column_shell_is_exact(
          value,
          PARADOX_DOMAIN_ID,
          row_count
        )) {
        Rf_error("Domain shape changed during admission");
      }
    } else if (paradox_domain_column_types[column] == REALSXP) {
      paradox_require_numeric_column(
        value,
        row_count,
        "Domain storage",
        paradox_domain_column_names[column]
      );
    } else {
      paradox_require_column_checked(
        value,
        paradox_domain_column_types[column],
        row_count,
        "Domain storage",
        paradox_domain_column_names[column]
      );
    }
  }
  /*
   * Capture and validate the complete supported outer metadata generation.
   * Row labels are deliberately count-only; selfref and repr are opaque
   * representation carriers whose exact identities are retained.
   */
  SEXP outward_row_names = outward_metadata.row_names;
  const int callback_capable_row_names = ALTREP(outward_row_names);
  /*
   * Length may dispatch. Root the exact selected carrier before that sole
   * observation, then require that it is still the table's carrier
   * afterwards. A callback may not replace A with an unobserved B and smuggle
   * B's different row count into the admitted generation.
   */
  REPROTECT(
    outward_row_names,
    root_indices[DOMAIN_ADMISSION_ROOT_ROW_NAMES]
  );
  R_xlen_t outward_row_count = 0;
  if (!paradox_public_row_names_count(
      outward_row_names,
      &outward_row_count
    )) {
    Rf_error("Domain changed during admission");
  }
  double *lower_values = NULL;
  double *upper_values = NULL;
  double *tolerance_values = NULL;
  if (interpreted & PARADOX_DOMAIN_INTERPRET_BOUNDS) {
    lower_values = numeric_storage;
    upper_values = numeric_storage + buffer_size;
    tolerance_values = numeric_storage + 2 * buffer_size;
  }

  /*
   * A stable row-name ALTREP may have dispatched its one allowed Length above.
   * Select and own the complete post-callback generation. Canonical Domains
   * share one grouping pointer; a foreign but semantically grouped table may
   * need one rare exact per-row receipt. Allocating that receipt can run a
   * finalizer, so the loop restarts and overwrites every prior capture.
  */
  R_xlen_t expected_attribute_count = 0;
  SEXP outward_class = R_NilValue;
  SEXP outward_selfref = R_NilValue;
  SEXP outward_repr = R_NilValue;
  SEXP rare_grouping = R_NilValue;
  SEXP accepted_class = R_NilValue;
  SEXP accepted_grouping = R_NilValue;
  SEXP accepted_storage = R_NilValue;
  paradox_builtin_domain_kind_t resolved_kind = kind;
  int selection_is_current = !callback_capable_row_names;
  for (;;) {
    if (!selection_is_current) {
      if (!paradox_domain_capture_outer_metadata(
          domain,
          &outward_metadata
        ) || outward_metadata.row_names != outward_row_names) {
        Rf_error("Domain changed during admission");
      }
      paradox_domain_select_captured_columns_with_positions(
        domain,
        outward_metadata.names,
        "Domain storage",
        "Domain",
        selected_mask,
        selected_columns,
        selected_positions
      );
    }
    if (XLENGTH(domain) != PARADOX_DOMAIN_COLUMN_COUNT ||
        outward_metadata.row_names != outward_row_names) {
      Rf_error("Domain changed during admission");
    }
    if (!selection_is_current) {
      for (int column = 0; column < PARADOX_DOMAIN_COLUMN_COUNT; ++column) {
        REPROTECT(selected_columns[column], root_indices[column]);
      }
      for (int column = 0; column < PARADOX_DOMAIN_COLUMN_COUNT; ++column) {
        SEXP value = selected_columns[column];
        if (column == PARADOX_DOMAIN_ID) {
          if (!paradox_domain_column_shell_is_exact(
              value,
              PARADOX_DOMAIN_ID,
              row_count
            )) {
            Rf_error("Domain shape changed during admission");
          }
        } else if (paradox_domain_column_types[column] == REALSXP) {
          paradox_require_numeric_column(
            value,
            row_count,
            "Domain storage",
            paradox_domain_column_names[column]
          );
        } else {
          paradox_require_column_checked(
            value,
            paradox_domain_column_types[column],
            row_count,
            "Domain storage",
            paradox_domain_column_names[column]
          );
        }
      }
    }
    /* Any allocation followed by `continue` must select a fresh generation. */
    selection_is_current = FALSE;

    outward_class = outward_metadata.classes;
    outward_selfref = outward_metadata.selfref;
    outward_repr = outward_metadata.repr;
    const R_xlen_t outward_attribute_count = outward_metadata.count;
    expected_attribute_count = 3 +
      (outward_selfref != R_NilValue) + (outward_repr != R_NilValue);
    if (paradox_resolve_builtin_domain_table_class(outward_class) != kind) {
      Rf_error("Domain shape changed during admission");
    }
    if (outward_attribute_count != expected_attribute_count ||
        outward_row_count != row_count ||
        (outward_selfref != R_NilValue &&
          (TYPEOF(outward_selfref) != EXTPTRSXP ||
            Rf_isS4(outward_selfref))) ||
        (outward_repr != R_NilValue && Rf_isS4(outward_repr))) {
      Rf_error("Domain changed during admission");
    }
    REPROTECT(
      outward_class,
      root_indices[DOMAIN_ADMISSION_ROOT_CLASS]
    );
    REPROTECT(
      outward_selfref,
      root_indices[DOMAIN_ADMISSION_ROOT_SELFREF]
    );
    REPROTECT(outward_repr, root_indices[DOMAIN_ADMISSION_ROOT_REPR]);

    if (interpreted & PARADOX_DOMAIN_INTERPRET_BOUNDS) {
      SEXP lower_column = selected_columns[PARADOX_DOMAIN_LOWER];
      SEXP upper_column = selected_columns[PARADOX_DOMAIN_UPPER];
      SEXP tolerance_column = selected_columns[PARADOX_DOMAIN_TOLERANCE];
      for (R_xlen_t row = 0; row < row_count; ++row) {
        paradox_account_work(work_since_interrupt);
        lower_values[row] = paradox_numeric_elt(lower_column, row);
        upper_values[row] = paradox_numeric_elt(upper_column, row);
        tolerance_values[row] = paradox_numeric_elt(tolerance_column, row);
      }
    }

    SEXP ids = selected_columns[PARADOX_DOMAIN_ID];
    SEXP classes = selected_columns[PARADOX_DOMAIN_CLS];
    SEXP groupings = selected_columns[PARADOX_DOMAIN_GROUPING];
    SEXP storages = selected_columns[PARADOX_DOMAIN_STORAGE_TYPE];
    if (!ordinary_string_vector(ids) || !ordinary_string_vector(classes) ||
        !ordinary_string_vector(groupings) ||
        !ordinary_string_vector(storages)) {
      Rf_error(
        "Corrupt Domain storage: schema columns must be ordinary vectors"
      );
    }
    SEXP levels_column = selected_columns[PARADOX_DOMAIN_LEVELS];
    SEXP special_column = selected_columns[PARADOX_DOMAIN_SPECIAL_VALS];
    SEXP cargo_column = selected_columns[PARADOX_DOMAIN_CARGO];
    SEXP tags_column = selected_columns[PARADOX_DOMAIN_TAGS];
    SEXP trafo_column = selected_columns[PARADOX_DOMAIN_TRAFO];

    if (rare_grouping == R_NilValue && row_count > 1) {
      SEXP first_grouping = STRING_ELT(groupings, 0);
      int different_pointer = FALSE;
      for (R_xlen_t row = 1; row < row_count; ++row) {
        if (STRING_ELT(groupings, row) != first_grouping) {
          different_pointer = TRUE;
          break;
        }
      }
      if (different_pointer) {
        SEXP receipt = PROTECT(Rf_allocVector(STRSXP, row_count));
        rare_grouping = receipt;
        REPROTECT(
          rare_grouping,
          root_indices[DOMAIN_ADMISSION_ROOT_RARE_GROUPING]
        );
        UNPROTECT(1);
        continue;
      }
    }

    /*
     * One allocation-free pointer capture of every interpreted row precedes
     * every nested snapshot and duplicate check. A selected source slot is
     * overwritten by its owned copy later; the terminal barrier compares the
     * complete live generation to that copy. Unselected slots remain NULL.
     */
    if (row_count != 0) {
      accepted_class = STRING_ELT(classes, 0);
      accepted_grouping = STRING_ELT(groupings, 0);
      accepted_storage = STRING_ELT(storages, 0);
      /*
       * Install each exact representative in its scanned indexed root before
       * the sole canonical resolver sees it. No nested snapshot has started.
       */
      REPROTECT(
        accepted_class,
        root_indices[DOMAIN_ADMISSION_ROOT_ACCEPTED_CLASS]
      );
      REPROTECT(
        accepted_grouping,
        root_indices[DOMAIN_ADMISSION_ROOT_ACCEPTED_GROUPING]
      );
      REPROTECT(
        accepted_storage,
        root_indices[DOMAIN_ADMISSION_ROOT_ACCEPTED_STORAGE]
      );
      if (paradox_resolve_builtin_domain_class_char(
          accepted_class
        ) != kind) {
        Rf_error(
          "Corrupt Domain storage: `cls` is inconsistent with its class"
        );
      }
      resolved_kind = paradox_resolve_builtin_domain_kind_chars(
        accepted_class,
        accepted_storage
      );
      if (resolved_kind != kind) {
        Rf_error(
          "Corrupt Domain storage: `storage_type` is inconsistent with its "
          "class"
        );
      }
    }
    for (R_xlen_t row = 0; row < row_count; ++row) {
      const R_xlen_t offset = row * PARADOX_ADMITTED_ROW_STRIDE;
      SET_VECTOR_ELT(
        rows,
        offset + PARADOX_ADMITTED_ID,
        STRING_ELT(ids, row)
      );
      if (rare_grouping != R_NilValue) {
        SET_STRING_ELT(
          rare_grouping,
          row,
          STRING_ELT(groupings, row)
        );
      }
      if (interpreted & PARADOX_DOMAIN_INTERPRET_LEVELS) {
        SET_VECTOR_ELT(
          rows,
          offset + PARADOX_ADMITTED_LEVELS,
          VECTOR_ELT(levels_column, row)
        );
      }
      if (interpreted & PARADOX_DOMAIN_INTERPRET_SPECIAL_VALUES) {
        SET_VECTOR_ELT(
          rows,
          offset + PARADOX_ADMITTED_SPECIAL_VALS,
          VECTOR_ELT(special_column, row)
        );
      }
      if (interpreted & PARADOX_DOMAIN_INTERPRET_CARGO) {
        SET_VECTOR_ELT(
          rows,
          offset + PARADOX_ADMITTED_CARGO,
          VECTOR_ELT(cargo_column, row)
        );
      }
      if (interpreted & PARADOX_DOMAIN_INTERPRET_TAGS) {
        SET_VECTOR_ELT(
          rows,
          offset + PARADOX_ADMITTED_TAGS,
          VECTOR_ELT(tags_column, row)
        );
      }
      if (interpreted & PARADOX_DOMAIN_INTERPRET_TRAFO) {
        SET_VECTOR_ELT(
          rows,
          offset + PARADOX_ADMITTED_TRAFO,
          VECTOR_ELT(trafo_column, row)
        );
      }
      SEXP row_class = STRING_ELT(classes, row);
      SEXP row_storage = STRING_ELT(storages, row);
      if (row_class != accepted_class &&
          !paradox_domain_string_is(row_class, CHAR(accepted_class))) {
        Rf_error(
          "Corrupt Domain storage: `cls` is inconsistent with its class"
        );
      }
      if (row_storage != accepted_storage &&
          !paradox_domain_string_is(
            row_storage,
            CHAR(accepted_storage)
          )) {
        Rf_error(
          "Corrupt Domain storage: `storage_type` is inconsistent with its "
          "class"
        );
      }
    }
    if (rare_grouping != R_NilValue && row_count > 1) {
      SEXP first_grouping = STRING_ELT(rare_grouping, 0);
      for (R_xlen_t row = 1; row < row_count; ++row) {
        if (!paradox_domain_strings_equal(
            STRING_ELT(rare_grouping, row),
            first_grouping
          )) {
          Rf_error(
            "Corrupt Domain storage: rows must share one grouping"
          );
        }
      }
    }
    break;
  }

  if (capture_hook != R_NilValue) {
    run_domain_admission_test_hook(
      capture_hook,
      DOMAIN_ADMISSION_AFTER_CAPTURE
    );
  }

  /*
   * Reuse is carried forward as a Boolean proven while both adjacent source
   * slots are still rooted in `rows`. Never retain a source SEXP only in C
   * across the snapshot/owner allocations: a finalizer may replace the live
   * outward column after the current source slot is overwritten. The previous
   * owned value is safe to retain because the preceding row slot roots it.
   */
  SEXP previous_levels_owned = R_UnboundValue;
  SEXP previous_special_values_owned = R_UnboundValue;
  SEXP previous_cargo_owned = R_UnboundValue;
  SEXP previous_tags_owned = R_UnboundValue;
  int levels_reuses_previous = FALSE;
  int special_values_reuses_previous = FALSE;
  int cargo_reuses_previous = FALSE;
  int tags_reuses_previous = FALSE;
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    const R_xlen_t offset = row * PARADOX_ADMITTED_ROW_STRIDE;
    if (interpreted & PARADOX_DOMAIN_INTERPRET_LEVELS) {
      SEXP source = PROTECT(VECTOR_ELT(
        rows,
        offset + PARADOX_ADMITTED_LEVELS
      ));
      /* XLENGTH() errors for environments, closures, and several other
       * malformed row leaves. Canonical levels can only be character,
       * logical, or NULL; reject every other non-NULL type before observing
       * length so the semantic owner keeps its stable field diagnostic. The
       * common numeric NULL path performs no additional type read. */
      if (source != R_NilValue &&
          TYPEOF(source) != STRSXP && TYPEOF(source) != LGLSXP) {
        report_row_failure(
          kind,
          PARADOX_DOMAIN_FIELD_LEVELS,
          source,
          R_NilValue
        );
      }
      if (source != R_NilValue &&
          (ALTREP(source) || XLENGTH(source) != 0)) {
        const int next_reuses = row + 1 < row_count &&
          VECTOR_ELT(
            rows,
            (row + 1) * PARADOX_ADMITTED_ROW_STRIDE +
              PARADOX_ADMITTED_LEVELS
          ) == source;
        SEXP owned;
        if (levels_reuses_previous) {
          owned = previous_levels_owned;
        } else {
          owned = PROTECT(paradox_snapshot_domain_nested(
            source,
            PARADOX_DOMAIN_LEVELS,
            work_since_interrupt
          ));
          if (owned == R_UnboundValue) {
            report_row_failure(
              kind,
              PARADOX_DOMAIN_FIELD_LEVELS,
              source,
              R_NilValue
            );
          }
          UNPROTECT(1);
        }
        SET_VECTOR_ELT(rows, offset + PARADOX_ADMITTED_LEVELS, owned);
        previous_levels_owned = owned;
        levels_reuses_previous = next_reuses;
      } else {
        levels_reuses_previous = FALSE;
      }
      UNPROTECT(1);
    }
    int special_values_reused = FALSE;
    if (interpreted & PARADOX_DOMAIN_INTERPRET_SPECIAL_VALUES) {
      SEXP source = PROTECT(VECTOR_ELT(
        rows,
        offset + PARADOX_ADMITTED_SPECIAL_VALS
      ));
      if (TYPEOF(source) == VECSXP &&
          (ALTREP(source) || XLENGTH(source) != 0)) {
        const int next_reuses = row + 1 < row_count &&
          VECTOR_ELT(
            rows,
            (row + 1) * PARADOX_ADMITTED_ROW_STRIDE +
              PARADOX_ADMITTED_SPECIAL_VALS
          ) == source;
        SEXP owned;
        if (special_values_reuses_previous) {
          owned = previous_special_values_owned;
          special_values_reused = TRUE;
        } else {
          owned = PROTECT(paradox_snapshot_domain_nested(
            source,
            PARADOX_DOMAIN_SPECIAL_VALS,
            work_since_interrupt
          ));
          if (owned == R_UnboundValue) {
            report_row_failure(
              kind,
              PARADOX_DOMAIN_FIELD_SPECIAL_VALUES,
              R_NilValue,
              source
            );
          }
          UNPROTECT(1);
        }
        SET_VECTOR_ELT(rows, offset + PARADOX_ADMITTED_SPECIAL_VALS, owned);
        previous_special_values_owned = owned;
        special_values_reuses_previous = next_reuses;
      } else {
        special_values_reuses_previous = FALSE;
      }
      UNPROTECT(1);
    }
    if (interpreted & PARADOX_DOMAIN_INTERPRET_CARGO) {
      SEXP source = PROTECT(VECTOR_ELT(
        rows,
        offset + PARADOX_ADMITTED_CARGO
      ));
      if (TYPEOF(source) == VECSXP &&
          (ALTREP(source) || XLENGTH(source) != 0)) {
        const int next_reuses = row + 1 < row_count &&
          VECTOR_ELT(
            rows,
            (row + 1) * PARADOX_ADMITTED_ROW_STRIDE +
              PARADOX_ADMITTED_CARGO
          ) == source;
        SEXP owned;
        if (cargo_reuses_previous) {
          owned = previous_cargo_owned;
        } else {
          owned = PROTECT(paradox_snapshot_domain_nested(
            source,
            PARADOX_DOMAIN_CARGO,
            work_since_interrupt
          ));
          if (owned == R_UnboundValue) {
            report_row_failure(
              kind,
              PARADOX_DOMAIN_FIELD_CARGO,
              R_NilValue,
              R_NilValue
            );
          }
          UNPROTECT(1);
        }
        SET_VECTOR_ELT(rows, offset + PARADOX_ADMITTED_CARGO, owned);
        previous_cargo_owned = owned;
        cargo_reuses_previous = next_reuses;
      } else {
        cargo_reuses_previous = FALSE;
      }
      UNPROTECT(1);
    }
    if (interpreted & PARADOX_DOMAIN_INTERPRET_TAGS) {
      SEXP source = PROTECT(VECTOR_ELT(
        rows,
        offset + PARADOX_ADMITTED_TAGS
      ));
      if (TYPEOF(source) == STRSXP && !ALTREP(source) &&
          !Rf_isS4(source) && !Rf_isObject(source) &&
          paradox_api_has_no_attributes(source) &&
          XLENGTH(source) != 0) {
        const int next_reuses = row + 1 < row_count &&
          VECTOR_ELT(
            rows,
            (row + 1) * PARADOX_ADMITTED_ROW_STRIDE +
              PARADOX_ADMITTED_TAGS
          ) == source;
        SEXP owned;
        if (tags_reuses_previous) {
          owned = previous_tags_owned;
        } else {
          owned = PROTECT(paradox_snapshot_semantic_vector(source));
          UNPROTECT(1);
        }
        SET_VECTOR_ELT(rows, offset + PARADOX_ADMITTED_TAGS, owned);
        previous_tags_owned = owned;
        tags_reuses_previous = next_reuses;
      } else {
        tags_reuses_previous = FALSE;
      }
      UNPROTECT(1);
    }

    SEXP levels = VECTOR_ELT(rows, offset + PARADOX_ADMITTED_LEVELS);
    SEXP special_values = VECTOR_ELT(
      rows,
      offset + PARADOX_ADMITTED_SPECIAL_VALS
    );
    paradox_special_values_receipt_t receipt;
    paradox_special_values_receipt_t *selected_receipt = NULL;
    if (interpreted & PARADOX_DOMAIN_INTERPRET_SPECIAL_VALUES) {
      empty_special_names_present[row] = 0U;
      if (!paradox_prepare_builtin_special_values_kind(
          resolved_kind,
          special_values,
          &receipt,
          work_since_interrupt
        )) {
        report_row_failure(
          kind,
          PARADOX_DOMAIN_FIELD_SPECIAL_VALUES,
          levels,
          special_values
        );
      }
      if (XLENGTH(special_values) == 0) {
        SEXP special_names = exact_optional_names(special_values);
        if (special_names == R_UnboundValue) {
          report_row_failure(
            kind,
            PARADOX_DOMAIN_FIELD_SPECIAL_VALUES,
            levels,
            special_values
          );
        }
        empty_special_names_present[row] =
          (unsigned char) (special_names != R_NilValue);
      }
      if (receipt.typed && XLENGTH(special_values) != 0 &&
          !special_values_reused) {
        paradox_own_builtin_special_value_leaves(
          special_values,
          TRUE
        );
      }
      selected_receipt = &receipt;
    }
    paradox_builtin_domain_kind_t admitted_kind =
      PARADOX_BUILTIN_DOMAIN_UNKNOWN;
    paradox_domain_field_t failure = PARADOX_DOMAIN_FIELD_NONE;
    const int admits_bounds =
      (interpreted & PARADOX_DOMAIN_INTERPRET_BOUNDS) != 0;
    const paradox_captured_domain_schema_t schema = {
      VECTOR_ELT(rows, offset + PARADOX_ADMITTED_ID),
      accepted_class,
      accepted_grouping,
      accepted_storage,
      resolved_kind,
      admits_bounds ? lower_values[row] : NA_REAL,
      admits_bounds ? upper_values[row] : NA_REAL,
      admits_bounds ? tolerance_values[row] : NA_REAL
    };
    /*
     * The schema half is exactly the rule set these operations interpret.
     * Default, requirement, and initialization admission belongs to the
     * constructor: a public `$domains` projection deliberately carries the
     * stored TuneToken in `.init`, and that is detached by the cold
     * search-space converter rather than by a Domain operation.
     */
    if (!paradox_admit_builtin_domain_schema_captured(
        &schema,
        VECTOR_ELT(rows, offset + PARADOX_ADMITTED_CARGO),
        levels,
        special_values,
        VECTOR_ELT(rows, offset + PARADOX_ADMITTED_TAGS),
        VECTOR_ELT(rows, offset + PARADOX_ADMITTED_TRAFO),
        selected_receipt,
        interpreted,
        &admitted_kind,
        &failure,
        work_since_interrupt
      )) {
      report_row_failure(kind, failure, levels, special_values);
    }
    if (admitted_kind != resolved_kind) {
      Rf_error("Domain changed during admission");
    }
  }

  if (capture_hook != R_NilValue) {
    run_domain_admission_test_hook(
      capture_hook,
      DOMAIN_ADMISSION_AFTER_OWNERSHIP
    );
  }

  /*
   * Terminal, callback-free simultaneous-generation receipt. The selector-
   * owned indexed companion proves the complete canonical pairing directly;
   * the row receipts below catch in-place mutation of a retained column or
   * nested field.
   */
  paradox_domain_outer_metadata_t current_metadata;
  int current = paradox_domain_capture_outer_metadata(
    domain,
    &current_metadata
  );
  SEXP current_class = current_metadata.classes;
  SEXP current_row_names = current_metadata.row_names;
  SEXP current_selfref = current_metadata.selfref;
  SEXP current_repr = current_metadata.repr;
  const R_xlen_t current_attribute_count = current_metadata.count;
  R_xlen_t current_row_count = row_count;
  current = current && paradox_domain_captured_columns_current(
      domain,
      current_metadata.names,
      selected_columns,
      selected_positions,
      row_count
    ) &&
    current_attribute_count == expected_attribute_count &&
    current_class == outward_class &&
    paradox_resolve_builtin_domain_table_class(current_class) == kind &&
    current_row_names == outward_row_names &&
    current_selfref == outward_selfref &&
    current_repr == outward_repr &&
    (current_selfref == R_NilValue ||
      (TYPEOF(current_selfref) == EXTPTRSXP &&
        !Rf_isS4(current_selfref))) &&
    (current_repr == R_NilValue || !Rf_isS4(current_repr)) &&
    (TYPEOF(current_row_names) == INTSXP ||
      TYPEOF(current_row_names) == STRSXP) &&
    !Rf_isS4(current_row_names) && !Rf_isObject(current_row_names) &&
    paradox_api_has_no_attributes(current_row_names);
  if (current && !ALTREP(current_row_names)) {
    current = paradox_public_row_names_count(
        current_row_names,
        &current_row_count
      ) &&
      current_row_count == row_count;
  }
  if (current && (interpreted & PARADOX_DOMAIN_INTERPRET_BOUNDS)) {
    current = numeric_column_receipt_current(
        selected_columns[PARADOX_DOMAIN_LOWER],
        lower_values,
        row_count
      ) &&
      numeric_column_receipt_current(
        selected_columns[PARADOX_DOMAIN_UPPER],
        upper_values,
        row_count
      ) &&
      numeric_column_receipt_current(
        selected_columns[PARADOX_DOMAIN_TOLERANCE],
        tolerance_values,
        row_count
      );
  }
  const int typed_special = kind != PARADOX_BUILTIN_DOMAIN_UTY;
  const SEXP *live_ids = NULL;
  const SEXP *live_classes = NULL;
  const SEXP *live_groupings = NULL;
  const SEXP *live_storages = NULL;
  const SEXP *rare_groupings = NULL;
  if (current && row_count != 0) {
    /*
     * Every selected string column and the optional rare receipt is ordinary
     * and rooted above. The remaining terminal scan has no allocation,
     * interrupt poll, or callback, so these read-only pointers cannot be
     * invalidated before its final decision.
     */
    live_ids = STRING_PTR_RO(selected_columns[PARADOX_DOMAIN_ID]);
    live_classes = STRING_PTR_RO(selected_columns[PARADOX_DOMAIN_CLS]);
    live_groupings = STRING_PTR_RO(
      selected_columns[PARADOX_DOMAIN_GROUPING]
    );
    live_storages = STRING_PTR_RO(
      selected_columns[PARADOX_DOMAIN_STORAGE_TYPE]
    );
    if (rare_grouping != R_NilValue) {
      rare_groupings = STRING_PTR_RO(rare_grouping);
    }
  }
  int grouping_current = TRUE;
  SEXP prior_live_levels = R_UnboundValue;
  SEXP prior_owned_levels = R_UnboundValue;
  SEXP prior_live_special_values = R_UnboundValue;
  SEXP prior_owned_special_values = R_UnboundValue;
  unsigned char prior_empty_special_names_present = 0U;
  SEXP prior_live_cargo = R_UnboundValue;
  SEXP prior_owned_cargo = R_UnboundValue;
  SEXP prior_live_tags = R_UnboundValue;
  SEXP prior_owned_tags = R_UnboundValue;
  int have_prior_levels = FALSE;
  int have_prior_special_values = FALSE;
  int have_prior_cargo = FALSE;
  int have_prior_tags = FALSE;
  for (R_xlen_t row = 0; current && row < row_count; ++row) {
    const R_xlen_t offset = row * PARADOX_ADMITTED_ROW_STRIDE;
    SEXP expected_grouping = rare_grouping == R_NilValue
      ? accepted_grouping
      : rare_groupings[row];
    SEXP live_grouping = live_groupings[row];
    SEXP live_class = live_classes[row];
    SEXP live_storage = live_storages[row];
    current =
      live_ids[row] ==
        VECTOR_ELT(rows, offset + PARADOX_ADMITTED_ID) &&
      (live_class == accepted_class ||
        paradox_domain_string_is(live_class, CHAR(accepted_class))) &&
      (live_storage == accepted_storage ||
        paradox_domain_string_is(
          live_storage,
          CHAR(accepted_storage)
        ));
    if (current && live_grouping != expected_grouping) {
      current = FALSE;
      grouping_current = FALSE;
    }
    if (current && (interpreted & PARADOX_DOMAIN_INTERPRET_LEVELS)) {
      SEXP source = VECTOR_ELT(
        selected_columns[PARADOX_DOMAIN_LEVELS],
        row
      );
      SEXP snapshot = VECTOR_ELT(rows, offset + PARADOX_ADMITTED_LEVELS);
      if (!(have_prior_levels && source == prior_live_levels &&
          snapshot == prior_owned_levels)) {
        const SEXPTYPE type = (SEXPTYPE) TYPEOF(snapshot);
        current = (snapshot == R_NilValue && source == R_NilValue) ||
          ((type == STRSXP || type == LGLSXP) &&
            plain_vector_receipt_current(source, snapshot, type, FALSE));
      }
      prior_live_levels = source;
      prior_owned_levels = snapshot;
      have_prior_levels = TRUE;
    }
    if (current &&
        (interpreted & PARADOX_DOMAIN_INTERPRET_SPECIAL_VALUES)) {
      SEXP source = VECTOR_ELT(
        selected_columns[PARADOX_DOMAIN_SPECIAL_VALS],
        row
      );
      SEXP snapshot = VECTOR_ELT(
        rows,
        offset + PARADOX_ADMITTED_SPECIAL_VALS
      );
      /*
       * The optional-names receipt of an undetached empty shell is per row.
       * An identical live/admitted pair can reuse the preceding comparison
       * only when both rows also captured the same names-presence byte.
       */
      if (!(have_prior_special_values &&
          source == prior_live_special_values &&
          snapshot == prior_owned_special_values &&
          (XLENGTH(source) != 0 ||
            empty_special_names_present[row] ==
              prior_empty_special_names_present))) {
        current = special_values_receipt_current(
          source,
          snapshot,
          typed_special,
          empty_special_names_present[row]
        );
      }
      prior_live_special_values = source;
      prior_owned_special_values = snapshot;
      prior_empty_special_names_present =
        empty_special_names_present[row];
      have_prior_special_values = TRUE;
    }
    if (current && (interpreted & PARADOX_DOMAIN_INTERPRET_CARGO)) {
      SEXP source = VECTOR_ELT(
        selected_columns[PARADOX_DOMAIN_CARGO],
        row
      );
      SEXP snapshot = VECTOR_ELT(rows, offset + PARADOX_ADMITTED_CARGO);
      if (!(have_prior_cargo && source == prior_live_cargo &&
          snapshot == prior_owned_cargo)) {
        current = paradox_domain_cargo_snapshot_is_current(
          source,
          snapshot
        );
      }
      prior_live_cargo = source;
      prior_owned_cargo = snapshot;
      have_prior_cargo = TRUE;
    }
    if (current && (interpreted & PARADOX_DOMAIN_INTERPRET_TAGS)) {
      SEXP source = VECTOR_ELT(
        selected_columns[PARADOX_DOMAIN_TAGS],
        row
      );
      SEXP snapshot = VECTOR_ELT(rows, offset + PARADOX_ADMITTED_TAGS);
      if (!(have_prior_tags && source == prior_live_tags &&
          snapshot == prior_owned_tags)) {
        current = plain_vector_receipt_current(
          source,
          snapshot,
          STRSXP,
          FALSE
        );
      }
      prior_live_tags = source;
      prior_owned_tags = snapshot;
      have_prior_tags = TRUE;
    }
    if (current && (interpreted & PARADOX_DOMAIN_INTERPRET_TRAFO)) {
      current = VECTOR_ELT(
        selected_columns[PARADOX_DOMAIN_TRAFO],
        row
      ) == VECTOR_ELT(rows, offset + PARADOX_ADMITTED_TRAFO);
    }
  }
  if (!current) {
    if (!grouping_current) {
      Rf_error("Corrupt Domain storage: rows must share one grouping");
    }
    Rf_error("Domain changed during admission");
  }

  table->rows = rows;
  table->lower = lower_values;
  table->upper = upper_values;
  table->tolerance = tolerance_values;
  table->row_count = row_count;
  UNPROTECT(DOMAIN_ADMISSION_ROOT_COUNT);
  return rows;
}

SEXP paradox_admit_public_domain_table(SEXP domain,
    paradox_builtin_domain_kind_t kind, R_xlen_t row_count,
    unsigned int interpreted,
    paradox_admitted_domain_table_t *table,
    R_xlen_t *work_since_interrupt) {
  return admit_public_domain_table_impl(
    domain,
    kind,
    row_count,
    interpreted,
    table,
    work_since_interrupt,
    R_NilValue
  );
}

SEXP paradox_test_domain_admission_reentry(SEXP domain, SEXP kind,
    SEXP interpreted, SEXP capture_hook) {
  int kind_value = TYPEOF(kind) == INTSXP && !ALTREP(kind) &&
    XLENGTH(kind) == 1
      ? INTEGER_ELT(kind, 0)
      : 0;
  int interpreted_value = TYPEOF(interpreted) == INTSXP &&
    !ALTREP(interpreted) && XLENGTH(interpreted) == 1
      ? INTEGER_ELT(interpreted, 0)
      : -1;
  if (TYPEOF(kind) != INTSXP || ALTREP(kind) || XLENGTH(kind) != 1 ||
      kind_value < (int) PARADOX_BUILTIN_DOMAIN_DBL ||
      kind_value > (int) PARADOX_BUILTIN_DOMAIN_UTY ||
      TYPEOF(interpreted) != INTSXP || ALTREP(interpreted) ||
      XLENGTH(interpreted) != 1 || interpreted_value < 0 ||
      interpreted_value > (int) PARADOX_DOMAIN_INTERPRET_ALL ||
      !valid_domain_admission_test_hooks(capture_hook)) {
    Rf_error("Invalid test Domain-admission arguments");
  }
  R_xlen_t row_count;
  if (!paradox_public_table_row_count(domain, &row_count)) {
    Rf_error("Invalid test Domain row names");
  }
  R_xlen_t work_since_interrupt = 0;
  paradox_admitted_domain_table_t table;
  PROTECT(admit_public_domain_table_impl(
    domain,
    (paradox_builtin_domain_kind_t) kind_value,
    row_count,
    (unsigned int) interpreted_value,
    &table,
    &work_since_interrupt,
    capture_hook
  ));
  UNPROTECT(1);
  return R_NilValue;
}
