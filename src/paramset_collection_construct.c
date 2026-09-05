#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "paramset_collection_readers.h"
#include "paramset_domain_common.h"
#include "paramset_params_internal.h"
#include "paramset_shadow.h"
#include "r_api_compat.h"
#include "r_utils.h"
#include "core_state.h"

enum constructor_root_slot {
  CONSTRUCTOR_ROOT_SELF = 0,
  CONSTRUCTOR_ROOT_OWNER,
  CONSTRUCTOR_ROOT_ENCLOSURE,
  CONSTRUCTOR_ROOT_PRIVATE,
  CONSTRUCTOR_ROOT_PARAMS,
  CONSTRUCTOR_ROOT_PARAM_COLUMNS,
  CONSTRUCTOR_ROOT_TAGS = CONSTRUCTOR_ROOT_PARAM_COLUMNS +
    PARADOX_DOMAIN_TAGS,
  CONSTRUCTOR_ROOT_TAG_IDS,
  CONSTRUCTOR_ROOT_TAG_VALUES,
  CONSTRUCTOR_ROOT_TRAFOS,
  CONSTRUCTOR_ROOT_TRAFO_IDS,
  CONSTRUCTOR_ROOT_TRAFO_VALUES,
  CONSTRUCTOR_ROOT_CORE,
  CONSTRUCTOR_ROOT_STRIDE
};

typedef struct {
  SEXP self;
  SEXP private_environment;
  SEXP core;
  SEXP owner;
  SEXP params_columns[PARADOX_DOMAIN_TAGS];
  SEXP tag_ids;
  SEXP tag_values;
  SEXP trafo_ids;
  SEXP trafo_values;
  R_xlen_t param_count;
  R_xlen_t tag_count;
  R_xlen_t trafo_count;
} constructor_child_t;

static const SEXPTYPE params_types[PARADOX_DOMAIN_TAGS] = {
  STRSXP, STRSXP, STRSXP, VECSXP, REALSXP, REALSXP, REALSXP, VECSXP,
  VECSXP, VECSXP, STRSXP
};

static const char *const tag_names[] = {"id", "tag"};
static const SEXPTYPE tag_types[] = {STRSXP, STRSXP};
static const char *const trafo_names[] = {"id", "trafo"};
static const SEXPTYPE trafo_types[] = {STRSXP, VECSXP};
static const char *const translation_names[] = {
  "id", "original_id", "owner_ps_index", "owner_name"
};
static const SEXPTYPE translation_types[] = {
  STRSXP, STRSXP, INTSXP, STRSXP
};

static int has_no_attributes(SEXP value) {
  return paradox_api_has_no_attributes(value);
}

static int exact_flag(SEXP value, int *result) {
  if (TYPEOF(value) != LGLSXP || ALTREP(value) || Rf_isS4(value) ||
      Rf_isObject(value) || XLENGTH(value) != 1 ||
      !has_no_attributes(value)) {
    return FALSE;
  }
  const int flag = LOGICAL_ELT(value, 0);
  if (flag == NA_LOGICAL) {
    return FALSE;
  }
  *result = flag;
  return TRUE;
}

static int checked_flag(SEXP value, const char *name) {
  int result = FALSE;
  if (exact_flag(value, &result)) {
    return result;
  }
  if (TYPEOF(value) == LGLSXP && !ALTREP(value) && !Rf_isS4(value) &&
      !Rf_isObject(value) &&
      XLENGTH(value) == 1 && has_no_attributes(value) &&
      LOGICAL_ELT(value, 0) == NA_LOGICAL) {
    Rf_error("`%s`: May not be NA", name);
  }
  Rf_error("`%s` must be an unclassed logical flag", name);
  return FALSE;
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

static SEXP checked_set_names(SEXP sets, int postfix,
    const char *names_argument, R_xlen_t *work_since_interrupt) {
  if (TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isS4(sets) ||
      Rf_isObject(sets) ||
      !paradox_params_names_are_only_attribute(sets)) {
    Rf_error("`sets` must be an ordinary named list");
  }
  const R_xlen_t set_count = XLENGTH(sets);
  SEXP observed_names = PROTECT(Rf_getAttrib(sets, R_NamesSymbol));
  if (TYPEOF(observed_names) != STRSXP || ALTREP(observed_names) ||
      Rf_isS4(observed_names) || Rf_isObject(observed_names) ||
      !has_no_attributes(observed_names)) {
    UNPROTECT(1);
    Rf_error("`sets` must have ordinary character names");
  }
  const R_xlen_t name_count = XLENGTH(observed_names);
  if (name_count != set_count) {
    UNPROTECT(1);
    Rf_error("`sets` names must have the same length as `sets`");
  }
  for (R_xlen_t right = 0; right < name_count; ++right) {
    paradox_account_work(work_since_interrupt);
    SEXP right_name = STRING_ELT(observed_names, right);
    if (right_name == NA_STRING) {
      UNPROTECT(1);
      Rf_error("`sets` name is NA at position %.0f", (double) right + 1.0);
    }
    if (CHAR(right_name)[0] == '\0') {
      continue;
    }
    /* A named set affixes every ID it contributes, so its name must keep that
     * ID inside the parameter-name grammar.  Admitting a looser name would
     * build a collection whose own `$search_space()` and
     * `ParamSet$new(<collection>$domains)` reject its generated IDs.  A
     * prepended name owns the leading position and must itself be a strict
     * ID; an appended one only has to be a legal continuation. */
    if (postfix ? !paradox_string_is_strict_id_tail(right_name)
        : !paradox_string_is_strict_id(right_name)) {
      UNPROTECT(1);
      Rf_error(
        postfix
          ? "%s must be \"\" or use only ASCII letters, digits, `.`, and `_`"
          : "%s must be \"\" or an ASCII parameter ID matching "
            "^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$",
        names_argument
      );
    }
    for (R_xlen_t left = 0; left < right; ++left) {
      paradox_account_work(work_since_interrupt);
      SEXP left_name = STRING_ELT(observed_names, left);
      if (CHAR(left_name)[0] != '\0' &&
          strcmp(CHAR(left_name), CHAR(right_name)) == 0) {
        UNPROTECT(1);
        Rf_error("`sets` must have unique names except for empty names");
      }
    }
  }
  UNPROTECT(1);
  return observed_names;
}

static int checked_add(R_xlen_t *total, R_xlen_t increment) {
  if (increment < 0 || *total > R_XLEN_T_MAX - increment) {
    return FALSE;
  }
  *total += increment;
  return TRUE;
}

static int contains_id(SEXP ids, SEXP sought) {
  for (R_xlen_t index = 0; index < XLENGTH(ids); ++index) {
    if (strcmp(CHAR(STRING_ELT(ids, index)), CHAR(sought)) == 0) {
      return TRUE;
    }
  }
  return FALSE;
}

static int validate_related_ids(SEXP params_ids, SEXP related_ids,
    int require_unique, R_xlen_t *work_since_interrupt) {
  for (R_xlen_t index = 0; index < XLENGTH(related_ids); ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP id = STRING_ELT(related_ids, index);
    if (!supported_ascii(id) || !contains_id(params_ids, id)) {
      return FALSE;
    }
  }
  return !require_unique || Rf_any_duplicated(related_ids, FALSE) == 0;
}

static int checked_affixed_lengths(size_t owner_size, size_t id_size,
    size_t *result) {
  const size_t maximum = (size_t) INT_MAX;
  if (owner_size > SIZE_MAX - id_size || owner_size > maximum ||
      id_size > maximum) {
    return FALSE;
  }
  const size_t combined_size = owner_size + id_size;
  if (combined_size >= maximum) {
    return FALSE;
  }
  *result = combined_size + 1U;
  return TRUE;
}

static int checked_affixed_size(SEXP owner, SEXP id, size_t *result) {
  return checked_affixed_lengths(
    strlen(CHAR(owner)),
    strlen(CHAR(id)),
    result
  );
}

static int combined_sizes_supported(SEXP owner, SEXP id) {
  size_t unused_size = 0;
  return checked_affixed_size(owner, id, &unused_size);
}

/* Internal boundary probe. R cannot construct a CHARSXP near SIZE_MAX, so
 * focused native tests select exact C boundary values without allocating the
 * corresponding strings. Invalid combinations return NA; valid combinations
 * return their separator-inclusive size, which is guaranteed to fit in int. */
SEXP paradox_test_checked_affixed_size(SEXP owner_boundary,
    SEXP id_boundary) {
  if (TYPEOF(owner_boundary) != INTSXP || ALTREP(owner_boundary) ||
      Rf_isS4(owner_boundary) || Rf_isObject(owner_boundary) ||
      !has_no_attributes(owner_boundary) || XLENGTH(owner_boundary) != 1 ||
      TYPEOF(id_boundary) != INTSXP || ALTREP(id_boundary) ||
      Rf_isS4(id_boundary) || Rf_isObject(id_boundary) ||
      !has_no_attributes(id_boundary) || XLENGTH(id_boundary) != 1) {
    Rf_error("Affix boundary selectors must be ordinary integer scalars");
  }

  const int owner_selector = INTEGER_ELT(owner_boundary, 0);
  const int id_selector = INTEGER_ELT(id_boundary, 0);
  const size_t sizes[6] = {
    0U,
    1U,
    (size_t) INT_MAX - 1U,
    (size_t) INT_MAX,
    SIZE_MAX - 1U,
    SIZE_MAX
  };
  if (owner_selector < 0 || owner_selector >= 6 ||
      id_selector < 0 || id_selector >= 6) {
    Rf_error("Invalid affix boundary selector");
    return R_NilValue;
  }

  size_t output_size = 0;
  if (!checked_affixed_lengths(
      sizes[owner_selector],
      sizes[id_selector],
      &output_size
    )) {
    return Rf_ScalarInteger(NA_INTEGER);
  }
  if (output_size > (size_t) INT_MAX) {
    Rf_error("Internal error: checked affix size exceeded INT_MAX");
  }
  return Rf_ScalarInteger((int) output_size);
}

static int generated_tag_size_supported(SEXP component,
    size_t prefix_size) {
  const size_t component_size = strlen(CHAR(component));
  return component_size <= (size_t) INT_MAX &&
    prefix_size <= (size_t) INT_MAX - component_size;
}

static int initialize_child(SEXP self, SEXP owner, SEXP roots,
    R_xlen_t roots_offset, constructor_child_t *child,
    R_xlen_t *work_since_interrupt) {
  child->self = self;
  child->owner = owner;
  SET_VECTOR_ELT(roots, roots_offset + CONSTRUCTOR_ROOT_SELF, self);
  SET_VECTOR_ELT(roots, roots_offset + CONSTRUCTOR_ROOT_OWNER, owner);
  if (TYPEOF(self) != ENVSXP || Rf_isS4(self)) {
    return FALSE;
  }

  SEXP private_environment = paradox_domain_private_environment(self);
  if (private_environment == R_UnboundValue) {
    return FALSE;
  }
  child->private_environment = private_environment;
  SET_VECTOR_ELT(
    roots,
    roots_offset + CONSTRUCTOR_ROOT_PRIVATE,
    private_environment
  );
  if (!paradox_domain_owns_private_environment(self, private_environment)) {
    return FALSE;
  }
  SEXP core = paradox_core_from_private(private_environment);
  if (core == R_UnboundValue) {
    return FALSE;
  }
  /* A parent caches its children's schema, so a child whose own derived state
   * is stale must be brought current before it is flattened -- otherwise a
   * nested collection copies an outdated slice into the new parent. */
  if (!paradox_core_is_verified(core)) {
    core = paradox_core_refresh(self, private_environment);
  }
  const paradox_core_kind_t kind = paradox_core_kind(core);
  if (kind != PARADOX_CORE_BASE && kind != PARADOX_CORE_COLLECTION &&
      kind != PARADOX_CORE_SHADOW) {
    return FALSE;
  }
  child->core = core;
  SET_VECTOR_ELT(roots, roots_offset + CONSTRUCTOR_ROOT_CORE, core);

  /*
   * The selected generation is the constructor's one child snapshot and is
   * already retained by the root carrier.  Read every flattened table from
   * that payload: validation allocates, so rereading `.core` between tables
   * could otherwise combine an old `.params` with newer `.tags`/`.trafos`
   * installed by a pending finalizer while recording the old generation in
   * `.edges$cores`.
   */
  SEXP state = paradox_core_payload(core);
  if (state == R_UnboundValue) {
    return FALSE;
  }
  SEXP params = VECTOR_ELT(state, PARADOX_CORE_PARAMS);
  SET_VECTOR_ELT(roots, roots_offset + CONSTRUCTOR_ROOT_PARAMS, params);
  paradox_domain_params_t checked_params;
  if (!paradox_domain_read_params(
        params,
      PARADOX_PARAMS_COLUMNS_ALL, &checked_params)) {
    return FALSE;
  }
  child->param_count = checked_params.row_count;
  for (enum paradox_domain_column column = PARADOX_DOMAIN_ID;
      column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    SEXP value = VECTOR_ELT(params, column);
    child->params_columns[column] = value;
    SET_VECTOR_ELT(
      roots,
      roots_offset + CONSTRUCTOR_ROOT_PARAM_COLUMNS + column,
      value
    );
  }
  SEXP params_ids = child->params_columns[PARADOX_DOMAIN_ID];
  for (R_xlen_t row = 0; row < child->param_count; ++row) {
    paradox_account_work(work_since_interrupt);
    SEXP id = STRING_ELT(params_ids, row);
    if (!supported_ascii(id) ||
        (CHAR(owner)[0] != '\0' && !combined_sizes_supported(owner, id)) ||
        !generated_tag_size_supported(id, 6U)) {
      return FALSE;
    }
  }
  if (!generated_tag_size_supported(owner, 4U)) {
    return FALSE;
  }

  SEXP tags = VECTOR_ELT(state, PARADOX_CORE_TAGS);
  SET_VECTOR_ELT(roots, roots_offset + CONSTRUCTOR_ROOT_TAGS, tags);
  paradox_domain_tags_t checked_tags;
  if (!paradox_params_supported_table_attributes(tags) ||
      !paradox_domain_validate_tags(
        tags,
        &checked_tags,
        work_since_interrupt
      )) {
    return FALSE;
  }
  child->tag_ids = checked_tags.ids;
  child->tag_values = checked_tags.values;
  child->tag_count = checked_tags.row_count;
  SET_VECTOR_ELT(
    roots,
    roots_offset + CONSTRUCTOR_ROOT_TAG_IDS,
    child->tag_ids
  );
  SET_VECTOR_ELT(
    roots,
    roots_offset + CONSTRUCTOR_ROOT_TAG_VALUES,
    child->tag_values
  );
  if (!validate_related_ids(
      params_ids,
      child->tag_ids,
      FALSE,
      work_since_interrupt
    )) {
    return FALSE;
  }

  SEXP trafos = VECTOR_ELT(state, PARADOX_CORE_TRAFOS);
  SET_VECTOR_ELT(roots, roots_offset + CONSTRUCTOR_ROOT_TRAFOS, trafos);
  paradox_domain_trafos_t checked_trafos;
  if (!paradox_params_supported_table_attributes(trafos) ||
      !paradox_domain_validate_trafos(
        trafos,
        &checked_trafos,
        work_since_interrupt
      )) {
    return FALSE;
  }
  child->trafo_ids = checked_trafos.ids;
  child->trafo_values = checked_trafos.values;
  child->trafo_count = checked_trafos.row_count;
  SET_VECTOR_ELT(
    roots,
    roots_offset + CONSTRUCTOR_ROOT_TRAFO_IDS,
    child->trafo_ids
  );
  SET_VECTOR_ELT(
    roots,
    roots_offset + CONSTRUCTOR_ROOT_TRAFO_VALUES,
    child->trafo_values
  );
  if (!validate_related_ids(
      params_ids,
      child->trafo_ids,
      TRUE,
      work_since_interrupt
    )) {
    return FALSE;
  }
  return TRUE;
}

static void copy_at(SEXP destination, R_xlen_t destination_row,
    SEXP source, R_xlen_t source_row) {
  switch (TYPEOF(destination)) {
  case STRSXP:
    SET_STRING_ELT(
      destination,
      destination_row,
      STRING_ELT(source, source_row)
    );
    return;
  case VECSXP:
    SET_VECTOR_ELT(
      destination,
      destination_row,
      VECTOR_ELT(source, source_row)
    );
    return;
  case INTSXP:
    SET_INTEGER_ELT(
      destination,
      destination_row,
      INTEGER_ELT(source, source_row)
    );
    return;
  case REALSXP:
    if (TYPEOF(source) == REALSXP) {
      SET_REAL_ELT(destination, destination_row, REAL_ELT(source, source_row));
    } else {
      const int value = INTEGER_ELT(source, source_row);
      SET_REAL_ELT(
        destination,
        destination_row,
        value == NA_INTEGER ? NA_REAL : (double) value
      );
    }
    return;
  default:
    Rf_error("Internal error: unsupported collection constructor column");
  }
}

static SEXP make_affixed_id(SEXP owner, SEXP id, int postfix) {
  PROTECT(owner);
  PROTECT(id);
  if (CHAR(owner)[0] == '\0') {
    UNPROTECT(2);
    return id;
  }
  const size_t owner_size = strlen(CHAR(owner));
  const size_t id_size = strlen(CHAR(id));
  size_t output_size = 0;
  if (!checked_affixed_lengths(owner_size, id_size, &output_size)) {
    Rf_error("Internal error: affixed collection id exceeds R's string limit");
  }
  char *buffer = R_alloc(output_size + 1U, 1);
  /* R_alloc() may collect.  Retain only sizes across it and reacquire the
   * rooted CHARSXP byte views before copying. */
  const char *owner_text = CHAR(owner);
  const char *id_text = CHAR(id);
  if (postfix) {
    memcpy(buffer, id_text, id_size);
    buffer[id_size] = '.';
    memcpy(buffer + id_size + 1U, owner_text, owner_size);
  } else {
    memcpy(buffer, owner_text, owner_size);
    buffer[owner_size] = '.';
    memcpy(buffer + owner_size + 1U, id_text, id_size);
  }
  buffer[output_size] = '\0';
  SEXP result = Rf_mkCharLenCE(buffer, (int) output_size, CE_NATIVE);
  UNPROTECT(2);
  return result;
}

static SEXP make_generated_tag(const char *prefix, size_t prefix_size,
    SEXP component) {
  PROTECT(component);
  const size_t component_size = strlen(CHAR(component));
  if (prefix_size > (size_t) INT_MAX ||
      component_size > (size_t) INT_MAX - prefix_size) {
    UNPROTECT(1);
    Rf_error("Generated ParamSetCollection tag exceeds R's string limit");
  }
  const size_t output_size = prefix_size + component_size;
  char *buffer = R_alloc(output_size + 1U, 1);
  const char *component_text = CHAR(component);
  memcpy(buffer, prefix, prefix_size);
  memcpy(buffer + prefix_size, component_text, component_size);
  buffer[output_size] = '\0';
  SEXP result = Rf_mkCharLenCE(buffer, (int) output_size, CE_NATIVE);
  UNPROTECT(1);
  return result;
}

static int id_precedes(SEXP ids, R_xlen_t left, R_xlen_t right) {
  return strcmp(
    CHAR(STRING_ELT(ids, left)),
    CHAR(STRING_ELT(ids, right))
  ) <= 0;
}

static void stable_id_order(SEXP ids, R_xlen_t *order,
    R_xlen_t *workspace, R_xlen_t size,
    R_xlen_t *work_since_interrupt) {
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
        workspace[output++] = id_precedes(ids, order[left], order[right])
          ? order[left++]
          : order[right++];
      }
      while (left < middle) {
        workspace[output++] = order[left++];
      }
      while (right < end) {
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

static void reorder_table(SEXP destination, SEXP source,
    const R_xlen_t *order, R_xlen_t row_count,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t column = 0; column < XLENGTH(destination); ++column) {
    SEXP output = VECTOR_ELT(destination, column);
    SEXP input = VECTOR_ELT(source, column);
    for (R_xlen_t row = 0; row < row_count; ++row) {
      paradox_account_work(work_since_interrupt);
      copy_at(output, row, input, order[row]);
    }
  }
}

static SEXP sorted_table(SEXP source, const char *const *column_names,
    const SEXPTYPE *types, R_xlen_t column_count, R_xlen_t row_count,
    R_xlen_t *work_since_interrupt) {
  SEXP result = PROTECT(paradox_domain_new_plain_table(
    column_names,
    types,
    column_count,
    row_count
  ));
  if (row_count != 0) {
    R_xlen_t *order = paradox_temporary_alloc(row_count, sizeof(*order));
    R_xlen_t *workspace = paradox_temporary_alloc(
      row_count,
      sizeof(*workspace)
    );
    stable_id_order(
      VECTOR_ELT(source, 0),
      order,
      workspace,
      row_count,
      work_since_interrupt
    );
    reorder_table(
      result,
      source,
      order,
      row_count,
      work_since_interrupt
    );
  }
  UNPROTECT(1);
  return result;
}


/* Bare typed columns for a table whose metadata is installed separately. */
static SEXP allocate_columns(const SEXPTYPE *types, R_xlen_t column_count,
    R_xlen_t row_count) {
  SEXP table = PROTECT(Rf_allocVector(VECSXP, column_count));
  for (R_xlen_t column = 0; column < column_count; ++column) {
    SEXP values = PROTECT(Rf_allocVector(types[column], row_count));
    SET_VECTOR_ELT(table, column, values);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return table;
}

/* `tag_sets` and `tag_params` are per-edge logical vectors, not scalars: a
 * re-flatten has to reproduce the flags each edge was originally added with,
 * and an edge whose child is still empty carries no row that could record
 * them. */
/* A re-flatten discovers a collision long after the statement that caused it,
 * so its diagnostic has to name the edge that moved as well as the ID. */
static void report_duplicate_id(SEXP id, SEXP changed_owner) {
  if (changed_owner == R_NilValue) {
    Rf_error(
      "Cannot construct ParamSetCollection: translated parameter IDs must "
      "be unique"
    );
  }
  if (CHAR(changed_owner)[0] == '\0') {
    Rf_error(
      "Cannot refresh ParamSetCollection after a contained set changed: "
      "translated parameter ID '%s' is not unique",
      CHAR(id)
    );
  }
  Rf_error(
    "Cannot refresh ParamSetCollection after contained set '%s' changed: "
    "translated parameter ID '%s' is not unique",
    CHAR(changed_owner),
    CHAR(id)
  );
}

static void run_add_test_hook(SEXP hook);

static SEXP build_collection_static_state(SEXP sets, SEXP tag_sets,
    SEXP tag_params, SEXP tag_override, int postfix,
    const char *names_argument, SEXP changed_owner, SEXP capture_hook) {
  R_xlen_t work_since_interrupt = 0;
  /*
   * Allocate both carriers before capturing the caller-owned list.  A pending
   * finalizer may rewrite its names or children during either allocation; the
   * allocation-free shared capture then pairs one post-allocation generation
   * exactly.  Keep that stable carrier through the returned constructor
   * bundle so the R shell installs the same child graph that was flattened.
   */
  if (TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isS4(sets) ||
      Rf_isObject(sets) ||
      !paradox_params_names_are_only_attribute(sets)) {
    Rf_error("`sets` must be an ordinary named list");
  }
  const R_xlen_t source_child_count = XLENGTH(sets);
  SEXP stable_sets = PROTECT(Rf_allocVector(VECSXP, source_child_count));
  SEXP stable_names = PROTECT(Rf_allocVector(STRSXP, source_child_count));
  if (!paradox_capture_list_identities(sets, stable_names, stable_sets)) {
    /*
     * The capture can fail because a pending finalizer changed or malformed
     * the complete attribute spine. Re-establish the bound immediately before
     * selecting names for the more specific diagnostic; never replay an
     * unbounded selector merely to explain a bounded-capture failure.
     */
    if (!paradox_params_names_are_only_attribute(sets)) {
      UNPROTECT(2);
      Rf_error(
        "`sets` changed or became malformed while being snapshotted"
      );
    }
    SEXP observed_names = paradox_api_raw_attribute(
      sets,
      R_NamesSymbol
    );
    UNPROTECT(2);
    if (TYPEOF(observed_names) != STRSXP || ALTREP(observed_names) ||
        Rf_isS4(observed_names) || Rf_isObject(observed_names) ||
        !has_no_attributes(observed_names)) {
      Rf_error("`sets` must have ordinary character names");
    }
    if (XLENGTH(observed_names) != source_child_count) {
      Rf_error("`sets` names must have the same length as `sets`");
    }
    Rf_error("`sets` changed while being snapshotted");
  }
  Rf_setAttrib(stable_sets, R_NamesSymbol, stable_names);
  UNPROTECT(1);
  SEXP set_names = checked_set_names(
    stable_sets,
    postfix,
    names_argument,
    &work_since_interrupt
  );
  /* Test-only boundary: the one supported mutation window between the stable
   * name/child capture above and child admission below. Production callers
   * pass R_NilValue. */
  run_add_test_hook(capture_hook);
  const R_xlen_t child_count = XLENGTH(stable_sets);
  if (child_count > INT_MAX ||
      child_count > R_XLEN_T_MAX / CONSTRUCTOR_ROOT_STRIDE) {
    UNPROTECT(1);
    Rf_error("ParamSetCollection contains too many child sets");
  }
  if (TYPEOF(tag_sets) != LGLSXP || ALTREP(tag_sets) ||
      Rf_isS4(tag_sets) || Rf_isObject(tag_sets) ||
      !has_no_attributes(tag_sets) || XLENGTH(tag_sets) != child_count ||
      TYPEOF(tag_params) != LGLSXP || ALTREP(tag_params) ||
      Rf_isS4(tag_params) || Rf_isObject(tag_params) ||
      !has_no_attributes(tag_params) || XLENGTH(tag_params) != child_count) {
    UNPROTECT(1);
    Rf_error(
      "Internal error: ParamSetCollection edge flags disagree with sets"
    );
  }
  SEXP roots = PROTECT(Rf_allocVector(
    VECSXP,
    child_count * CONSTRUCTOR_ROOT_STRIDE
  ));
  for (R_xlen_t child_index = 0;
      child_index < child_count;
      ++child_index) {
    const R_xlen_t roots_offset =
      child_index * CONSTRUCTOR_ROOT_STRIDE;
    SET_VECTOR_ELT(
      roots,
      roots_offset + CONSTRUCTOR_ROOT_SELF,
      VECTOR_ELT(stable_sets, child_index)
    );
    SET_VECTOR_ELT(
      roots,
      roots_offset + CONSTRUCTOR_ROOT_OWNER,
      STRING_ELT(set_names, child_index)
    );
  }
  constructor_child_t *children = paradox_temporary_alloc(
    child_count,
    sizeof(*children)
  );
  /*
   * Child admission allocates and may run a pending finalizer. Immutable
   * selected capsules remain safe, but without one generation barrier the
   * new flatten could combine child states from supported mutations that
   * never coexisted. Cache refreshes preserve denotation and do not advance
   * this epoch.
   */
  const uintptr_t child_selection_epoch =
    paradox_core_state_epoch_value();
  R_xlen_t total_params = 0;
  R_xlen_t total_tags = 0;
  R_xlen_t total_trafos = 0;
  for (R_xlen_t child_index = 0;
      child_index < child_count;
      ++child_index) {
    constructor_child_t *child = &children[child_index];
    const R_xlen_t roots_offset =
      child_index * CONSTRUCTOR_ROOT_STRIDE;
    if (!initialize_child(
        VECTOR_ELT(roots, roots_offset + CONSTRUCTOR_ROOT_SELF),
        VECTOR_ELT(roots, roots_offset + CONSTRUCTOR_ROOT_OWNER),
        roots,
        roots_offset,
        child,
        &work_since_interrupt
      )) {
      UNPROTECT(2);
      if (changed_owner != R_NilValue) {
        Rf_error(
          "Corrupt or unsupported ParamSet state in a contained set while "
          "refreshing a ParamSetCollection"
        );
      }
      Rf_error(
        "Cannot construct ParamSetCollection from unsupported or corrupt "
        "ParamSet child state"
      );
    }
    /* initialize_child admitted the fields this flatten actually consumes.
     * Unrelated private values/dependencies are checked by their readers, not
     * by a second traversal of the child's complete graph here. */
    const int edge_tag_sets = LOGICAL_ELT(tag_sets, child_index);
    const int edge_tag_params = LOGICAL_ELT(tag_params, child_index);
    if (!checked_add(&total_params, child->param_count) ||
        !checked_add(&total_tags, child->tag_count) ||
        !checked_add(&total_trafos, child->trafo_count) ||
        (edge_tag_sets && CHAR(child->owner)[0] != '\0' &&
          !checked_add(&total_tags, child->param_count)) ||
        (edge_tag_params && !checked_add(&total_tags, child->param_count))) {
      UNPROTECT(2);
      Rf_error("ParamSetCollection metadata exceeds the supported size");
    }
  }
  if (paradox_core_state_epoch_value() != child_selection_epoch) {
    UNPROTECT(2);
    Rf_error(
      "ParamSet children changed while a collection was being constructed"
    );
  }
  if (total_params > INT_MAX || total_tags > INT_MAX ||
      total_trafos > INT_MAX) {
    UNPROTECT(2);
    Rf_error("ParamSetCollection metadata exceeds the supported row count");
  }

  SEXP params = PROTECT(paradox_domain_new_plain_table(
    paradox_domain_column_names,
    params_types,
    PARADOX_DOMAIN_TAGS,
    total_params
  ));
  SEXP translation_raw = PROTECT(allocate_columns(
    translation_types,
    4,
    total_params
  ));
  R_xlen_t param_output = 0;
  for (R_xlen_t child_index = 0;
      child_index < child_count;
      ++child_index) {
    const constructor_child_t *child = &children[child_index];
    for (R_xlen_t row = 0; row < child->param_count; ++row) {
      paradox_account_work(&work_since_interrupt);
      if (param_output >= total_params) {
        UNPROTECT(4);
        Rf_error("Internal error: collection params exceeded capacity");
      }
      SEXP original_id = PROTECT(STRING_ELT(
        child->params_columns[PARADOX_DOMAIN_ID],
        row
      ));
      SEXP output_id = PROTECT(make_affixed_id(
        child->owner,
        original_id,
        postfix
      ));
      SET_STRING_ELT(
        VECTOR_ELT(params, PARADOX_DOMAIN_ID),
        param_output,
        output_id
      );
      SET_STRING_ELT(
        VECTOR_ELT(translation_raw, 0),
        param_output,
        output_id
      );
      for (enum paradox_domain_column column = PARADOX_DOMAIN_CLS;
          column < PARADOX_DOMAIN_TAGS;
          column = (enum paradox_domain_column) (column + 1)) {
        copy_at(
          VECTOR_ELT(params, column),
          param_output,
          child->params_columns[column],
          row
        );
      }
      SET_STRING_ELT(
        VECTOR_ELT(translation_raw, 1),
        param_output,
        original_id
      );
      SET_INTEGER_ELT(
        VECTOR_ELT(translation_raw, 2),
        param_output,
        (int) child_index + 1
      );
      SET_STRING_ELT(
        VECTOR_ELT(translation_raw, 3),
        param_output,
        child->owner
      );
      UNPROTECT(2);
      ++param_output;
    }
  }
  if (param_output != total_params) {
    UNPROTECT(4);
    Rf_error("Internal error: incomplete collection params output");
  }
  const R_xlen_t duplicate = Rf_any_duplicated(
    VECTOR_ELT(params, PARADOX_DOMAIN_ID),
    FALSE
  );
  if (duplicate != 0) {
    SEXP duplicate_id = STRING_ELT(
      VECTOR_ELT(params, PARADOX_DOMAIN_ID),
      duplicate - 1
    );
    UNPROTECT(4);
    report_duplicate_id(duplicate_id, changed_owner);
  }

  SEXP tags_raw = PROTECT(allocate_columns(tag_types, 2, total_tags));
  SEXP trafos_raw = PROTECT(allocate_columns(
    trafo_types,
    2,
    total_trafos
  ));
  R_xlen_t tag_output = 0;
  R_xlen_t trafo_output = 0;
  param_output = 0;
  for (R_xlen_t child_index = 0;
      child_index < child_count;
      ++child_index) {
    const constructor_child_t *child = &children[child_index];
    for (R_xlen_t row = 0; row < child->tag_count; ++row) {
      if (tag_output >= total_tags) {
        UNPROTECT(6);
        Rf_error("Internal error: collection tags exceeded capacity");
      }
      SEXP output_id = PROTECT(make_affixed_id(
        child->owner,
        STRING_ELT(child->tag_ids, row),
        postfix
      ));
      SET_STRING_ELT(VECTOR_ELT(tags_raw, 0), tag_output, output_id);
      UNPROTECT(1);
      SET_STRING_ELT(
        VECTOR_ELT(tags_raw, 1),
        tag_output,
        STRING_ELT(child->tag_values, row)
      );
      ++tag_output;
    }
    if (LOGICAL_ELT(tag_sets, child_index) && CHAR(child->owner)[0] != '\0') {
      SEXP generated = PROTECT(make_generated_tag("set_", 4U, child->owner));
      for (R_xlen_t row = 0; row < child->param_count; ++row) {
        if (tag_output >= total_tags) {
          UNPROTECT(7);
          Rf_error("Internal error: collection tags exceeded capacity");
        }
        SET_STRING_ELT(
          VECTOR_ELT(tags_raw, 0),
          tag_output,
          STRING_ELT(VECTOR_ELT(params, PARADOX_DOMAIN_ID), param_output + row)
        );
        SET_STRING_ELT(VECTOR_ELT(tags_raw, 1), tag_output, generated);
        ++tag_output;
      }
      UNPROTECT(1);
    }
    if (LOGICAL_ELT(tag_params, child_index)) {
      for (R_xlen_t row = 0; row < child->param_count; ++row) {
        if (tag_output >= total_tags) {
          UNPROTECT(6);
          Rf_error("Internal error: collection tags exceeded capacity");
        }
        SEXP generated = PROTECT(make_generated_tag(
          "param_",
          6U,
          STRING_ELT(child->params_columns[PARADOX_DOMAIN_ID], row)
        ));
        SET_STRING_ELT(
          VECTOR_ELT(tags_raw, 0),
          tag_output,
          STRING_ELT(VECTOR_ELT(params, PARADOX_DOMAIN_ID), param_output + row)
        );
        SET_STRING_ELT(VECTOR_ELT(tags_raw, 1), tag_output, generated);
        UNPROTECT(1);
        ++tag_output;
      }
    }
    for (R_xlen_t row = 0; row < child->trafo_count; ++row) {
      if (trafo_output >= total_trafos) {
        UNPROTECT(6);
        Rf_error("Internal error: collection trafos exceeded capacity");
      }
      SEXP output_id = PROTECT(make_affixed_id(
        child->owner,
        STRING_ELT(child->trafo_ids, row),
        postfix
      ));
      SET_STRING_ELT(VECTOR_ELT(trafos_raw, 0), trafo_output, output_id);
      UNPROTECT(1);
      SET_VECTOR_ELT(
        VECTOR_ELT(trafos_raw, 1),
        trafo_output,
        VECTOR_ELT(child->trafo_values, row)
      );
      ++trafo_output;
    }
    param_output += child->param_count;
  }
  if (tag_output != total_tags || trafo_output != total_trafos ||
      param_output != total_params) {
    UNPROTECT(6);
    Rf_error("Internal error: incomplete collection auxiliary output");
  }

  SEXP translation = PROTECT(sorted_table(
    translation_raw,
    translation_names,
    translation_types,
    4,
    total_params,
    &work_since_interrupt
  ));
  SEXP tags = PROTECT(paradox_domain_apply_tag_override(
    PROTECT(sorted_table(
      tags_raw,
      tag_names,
      tag_types,
      2,
      total_tags,
      &work_since_interrupt
    )),
    tag_override,
    &work_since_interrupt
  ));
  SEXP trafos = PROTECT(sorted_table(
    trafos_raw,
    trafo_names,
    trafo_types,
    2,
    total_trafos,
    &work_since_interrupt
  ));

  /* The exact generations this flatten consumed, so a later read can decide
   * in one pointer comparison per edge whether it is still current. */
  SEXP edge_cores = PROTECT(Rf_allocVector(VECSXP, child_count));
  for (R_xlen_t child_index = 0; child_index < child_count; ++child_index) {
    SET_VECTOR_ELT(edge_cores, child_index, children[child_index].core);
  }
  SEXP edges = PROTECT(Rf_allocVector(
    VECSXP,
    PARADOX_COLLECTION_EDGE_FIELD_COUNT
  ));
  SET_VECTOR_ELT(edges, PARADOX_COLLECTION_EDGE_CORES, edge_cores);
  SET_VECTOR_ELT(edges, PARADOX_COLLECTION_EDGE_TAG_SETS, tag_sets);
  SET_VECTOR_ELT(edges, PARADOX_COLLECTION_EDGE_TAG_PARAMS, tag_params);
  SET_VECTOR_ELT(
    edges,
    PARADOX_COLLECTION_EDGE_TAG_OVERRIDE,
    tag_override
  );
  SEXP edge_names = PROTECT(paradox_domain_character_vector(
    (const char *const[]) {"cores", "tag_sets", "tag_params", "tag_override"},
    PARADOX_COLLECTION_EDGE_FIELD_COUNT
  ));
  Rf_setAttrib(edges, R_NamesSymbol, edge_names);

  SEXP state = PROTECT(Rf_allocVector(VECSXP, 6));
  SET_VECTOR_ELT(state, 0, params);
  SET_VECTOR_ELT(state, 1, tags);
  SET_VECTOR_ELT(state, 2, trafos);
  SET_VECTOR_ELT(state, 3, translation);
  SET_VECTOR_ELT(state, 4, edges);
  SET_VECTOR_ELT(state, 5, stable_sets);
  SEXP state_names = PROTECT(paradox_domain_character_vector(
    (const char *const[]) {
      "params", "tags", "trafos", "translation", "edges", "sets"
    },
    6
  ));
  Rf_setAttrib(state, R_NamesSymbol, state_names);
  UNPROTECT(15);
  return state;
}

/* The recorded flags fitted to the node's current edge count. */
static SEXP resized_edge_flags(SEXP flags, R_xlen_t count) {
  const R_xlen_t available = TYPEOF(flags) == LGLSXP && !ALTREP(flags) &&
      !Rf_isS4(flags) && !Rf_isObject(flags) &&
      has_no_attributes(flags)
    ? XLENGTH(flags)
    : 0;
  if (available == count) {
    return flags;
  }
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, count));
  for (R_xlen_t index = 0; index < count; ++index) {
    SET_LOGICAL_ELT(
      result,
      index,
      index < available ? LOGICAL_ELT(flags, index) : FALSE
    );
  }
  UNPROTECT(1);
  return result;
}

/* One flag vector per edge from a single construction-time scalar. */
static SEXP uniform_edge_flags(int flag, R_xlen_t count) {
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, count));
  for (R_xlen_t index = 0; index < count; ++index) {
    SET_LOGICAL_ELT(result, index, flag);
  }
  UNPROTECT(1);
  return result;
}

static SEXP param_set_collection_construct_impl(SEXP sets,
    SEXP tag_sets_sexp, SEXP tag_params_sexp, SEXP postfix_sexp,
    SEXP capture_hook) {
  const int tag_sets = checked_flag(tag_sets_sexp, "tag_sets");
  const int tag_params = checked_flag(tag_params_sexp, "tag_params");
  const int postfix = checked_flag(postfix_sexp, "postfix_names");
  /* Sizing only: the builder owns the `sets` shape diagnostics, and it rejects
   * anything this fallback length could not describe. */
  const R_xlen_t child_count =
    TYPEOF(sets) == VECSXP && !ALTREP(sets) && !Rf_isS4(sets)
      ? XLENGTH(sets)
      : 0;
  SEXP edge_tag_sets = PROTECT(uniform_edge_flags(tag_sets, child_count));
  SEXP edge_tag_params = PROTECT(uniform_edge_flags(tag_params, child_count));
  SEXP result = PROTECT(build_collection_static_state(
    sets, edge_tag_sets, edge_tag_params, R_NilValue, postfix,
    "`sets` names", R_NilValue, capture_hook
  ));
  UNPROTECT(3);
  return result;
}

SEXP paradox_param_set_collection_construct(SEXP sets, SEXP tag_sets_sexp,
    SEXP tag_params_sexp, SEXP postfix_sexp) {
  return param_set_collection_construct_impl(
    sets,
    tag_sets_sexp,
    tag_params_sexp,
    postfix_sexp,
    R_NilValue
  );
}

/* Test-only construction entry: `capture_hook` runs at the boundary between
 * the stable name/child capture and child admission, so a regression test can
 * mutate the caller's `sets` at the exact window a pending finalizer could
 * occupy, deterministically. */
SEXP paradox_test_param_set_collection_construct_reentry(SEXP sets,
    SEXP tag_sets_sexp, SEXP tag_params_sexp, SEXP postfix_sexp,
    SEXP capture_hook) {
  if (capture_hook != R_NilValue && !Rf_isFunction(capture_hook)) {
    Rf_error("Collection construction reentry hook must be a function");
  }
  return param_set_collection_construct_impl(
    sets,
    tag_sets_sexp,
    tag_params_sexp,
    postfix_sexp,
    capture_hook
  );
}

typedef struct {
  SEXP self;
  SEXP sets;
  R_xlen_t child_count;
  R_xlen_t next_child;
  int entered;
} add_graph_frame_t;

typedef struct {
  SEXP *shells;
  SEXP *private_environments;
  SEXP *cores;
  /* A SHADOW's ordinary metadata attribute is the one mutable carrier outside
   * its immutable capsule payload, so the exact carrier and its entries are
   * retained beside the capsule.  Both are `R_NilValue` on every other node
   * kind, which the terminal scan requires as well. */
  SEXP *shadow_signatures;
  SEXP *shadow_signature_contents;
  R_xlen_t count;
  R_xlen_t capacity;
} add_graph_snapshot_t;

static SEXP checked_add_name(SEXP value) {
  if (TYPEOF(value) != STRSXP || ALTREP(value) || Rf_isS4(value) ||
      Rf_isObject(value) || XLENGTH(value) != 1 ||
      !has_no_attributes(value)) {
    Rf_error("`n` must be an unclassed character scalar");
  }
  SEXP result = STRING_ELT(value, 0);
  if (result == NA_STRING) {
    Rf_error("`n` must be one non-missing name");
  }
  /* The empty name adds a set without affixing its IDs. Which grammar every
   * other name has to satisfy depends on whether this collection prepends or
   * appends it, so the shared admission below applies that rule to this same
   * name once the collection's own affix direction is known. */
  return result;
}

static void retain_add_graph_root(SEXP value, SEXP *roots,
    PROTECT_INDEX roots_index) {
  SEXP expanded = PROTECT(Rf_cons(value, *roots));
  REPROTECT(expanded, roots_index);
  *roots = expanded;
  UNPROTECT(1);
}

static void reserve_add_snapshot(add_graph_snapshot_t *snapshot,
    R_xlen_t required) {
  if (required <= snapshot->capacity) {
    return;
  }
  R_xlen_t capacity = snapshot->capacity;
  while (capacity < required) {
    if (capacity > R_XLEN_T_MAX / 2) {
      Rf_error("ParamSet capsule graph is too large to add");
    }
    capacity *= 2;
  }
  SEXP *shells = paradox_temporary_alloc(capacity, sizeof(*shells));
  SEXP *private_environments = paradox_temporary_alloc(
    capacity,
    sizeof(*private_environments)
  );
  SEXP *cores = paradox_temporary_alloc(capacity, sizeof(*cores));
  SEXP *shadow_signatures = paradox_temporary_alloc(
    capacity,
    sizeof(*shadow_signatures)
  );
  SEXP *shadow_signature_contents = paradox_temporary_alloc(
    capacity,
    sizeof(*shadow_signature_contents)
  );
  memcpy(
    shells,
    snapshot->shells,
    (size_t) snapshot->count * sizeof(*shells)
  );
  memcpy(
    private_environments,
    snapshot->private_environments,
    (size_t) snapshot->count * sizeof(*private_environments)
  );
  memcpy(
    cores,
    snapshot->cores,
    (size_t) snapshot->count * sizeof(*cores)
  );
  memcpy(
    shadow_signatures,
    snapshot->shadow_signatures,
    (size_t) snapshot->count * sizeof(*shadow_signatures)
  );
  memcpy(
    shadow_signature_contents,
    snapshot->shadow_signature_contents,
    (size_t) snapshot->count * sizeof(*shadow_signature_contents)
  );
  snapshot->shells = shells;
  snapshot->private_environments = private_environments;
  snapshot->cores = cores;
  snapshot->shadow_signatures = shadow_signatures;
  snapshot->shadow_signature_contents = shadow_signature_contents;
  snapshot->capacity = capacity;
}

static R_xlen_t snapshot_shell_index(const add_graph_snapshot_t *snapshot,
    SEXP shell, R_xlen_t *work_since_interrupt) {
  for (R_xlen_t index = 0; index < snapshot->count; ++index) {
    paradox_account_work(work_since_interrupt);
    if (snapshot->shells[index] == shell) {
      return index;
    }
  }
  return R_XLEN_T_MAX;
}

/* Snapshot every package graph edge, including the origin edge hidden behind
 * a SHADOW's fixed visible schema. COLLECTION readers deliberately treat a
 * SHADOW as a semantic leaf after refreshing it, whereas add() must also prove
 * that the proposed child cannot reach the collection receiving the edge. */
static void snapshot_add_graph(SEXP root, SEXP forbidden,
    add_graph_snapshot_t *snapshot, SEXP *roots,
    PROTECT_INDEX roots_index, R_xlen_t *work_since_interrupt) {
  snapshot->capacity = 8;
  snapshot->count = 0;
  snapshot->shells = paradox_temporary_alloc(
    snapshot->capacity,
    sizeof(*snapshot->shells)
  );
  snapshot->private_environments = paradox_temporary_alloc(
    snapshot->capacity,
    sizeof(*snapshot->private_environments)
  );
  snapshot->cores = paradox_temporary_alloc(
    snapshot->capacity,
    sizeof(*snapshot->cores)
  );
  snapshot->shadow_signatures = paradox_temporary_alloc(
    snapshot->capacity,
    sizeof(*snapshot->shadow_signatures)
  );
  snapshot->shadow_signature_contents = paradox_temporary_alloc(
    snapshot->capacity,
    sizeof(*snapshot->shadow_signature_contents)
  );

  R_xlen_t frame_capacity = 8;
  add_graph_frame_t *frames = paradox_temporary_alloc(
    frame_capacity,
    sizeof(*frames)
  );
  R_xlen_t depth = 1;
  frames[0] = (add_graph_frame_t) {
    root, R_NilValue, 0, 0, FALSE
  };

  while (depth != 0) {
    paradox_account_work(work_since_interrupt);
    add_graph_frame_t *frame = &frames[depth - 1];
    if (!frame->entered) {
      if (forbidden != R_NilValue && frame->self == forbidden) {
        Rf_error("Adding ParamSet would create a cycle in the capsule graph");
      }
      if (TYPEOF(frame->self) != ENVSXP || Rf_isS4(frame->self)) {
        Rf_error("Cannot add corrupt ParamSet graph child reference");
      }
      SEXP private_environment = PROTECT(
        paradox_domain_private_environment(frame->self)
      );
      if (private_environment == R_UnboundValue ||
          !paradox_domain_owns_private_environment(
            frame->self,
            private_environment
          )) {
        UNPROTECT(1);
        Rf_error("Cannot add corrupt ParamSet graph shell");
      }
      PROTECT_INDEX core_index;
      SEXP core;
      PROTECT_WITH_INDEX(
        core = paradox_core_from_private(private_environment),
        &core_index
      );
      if (!paradox_core_is_verified(core)) {
        REPROTECT(
          core = paradox_core_refresh(
            frame->self,
            private_environment
          ),
          core_index
        );
      }
      if (!paradox_core_is_canonical(core)) {
        UNPROTECT(2);
        Rf_error("Cannot add corrupt ParamSet graph capsule");
      }
      retain_add_graph_root(frame->self, roots, roots_index);
      retain_add_graph_root(private_environment, roots, roots_index);
      retain_add_graph_root(core, roots, roots_index);

      const paradox_core_kind_t kind = paradox_core_kind(core);
      /*
       * Select the exact refresh signature before the content snapshot below
       * allocates. A callback that runs during that allocation then either
       * swaps entries -- which the captured entry list records -- or replaces
       * the carrier itself, which the terminal carrier comparison rejects.
       * Selecting it after the refresh above is required: a refreshed SHADOW
       * installs a new capsule with a new signature, and the receipt must own
       * the generation this walk actually descended.
       */
      SEXP shadow_signature = R_NilValue;
      SEXP shadow_signature_content = R_NilValue;
      if (kind == PARADOX_CORE_SHADOW) {
        shadow_signature = PROTECT(paradox_shadow_metadata_signature(core));
        if (shadow_signature == R_UnboundValue) {
          UNPROTECT(3);
          Rf_error("Cannot add corrupt SHADOW refresh signature");
        }
        retain_add_graph_root(shadow_signature, roots, roots_index);
        shadow_signature_content = PROTECT(
          paradox_shadow_signature_content_snapshot(shadow_signature)
        );
        if (shadow_signature_content == R_NilValue) {
          UNPROTECT(4);
          Rf_error("Cannot add corrupt SHADOW refresh signature");
        }
        retain_add_graph_root(shadow_signature_content, roots, roots_index);
        UNPROTECT(2);
      }
      SEXP state = paradox_core_payload(core);
      SEXP sets = VECTOR_ELT(state, PARADOX_CORE_SETS);
      if (kind == PARADOX_CORE_BASE) {
        if (sets != R_NilValue) {
          UNPROTECT(2);
          Rf_error("Cannot add corrupt BASE graph edges");
        }
        frame->sets = R_NilValue;
        frame->child_count = 0;
      } else if (kind == PARADOX_CORE_COLLECTION) {
        if (TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isS4(sets) ||
            Rf_isObject(sets)) {
          UNPROTECT(2);
          Rf_error("Cannot add corrupt COLLECTION graph edges");
        }
        frame->sets = sets;
        frame->child_count = XLENGTH(sets);
      } else if (kind == PARADOX_CORE_SHADOW) {
        if (TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isS4(sets) ||
            Rf_isObject(sets) || XLENGTH(sets) != 1 ||
            !has_no_attributes(sets)) {
          UNPROTECT(2);
          Rf_error("Cannot add corrupt SHADOW origin edge");
        }
        frame->sets = sets;
        frame->child_count = 1;
      } else {
        UNPROTECT(2);
        Rf_error("Cannot add unknown ParamSet graph node kind");
      }
      frame->next_child = 0;
      frame->entered = TRUE;

      reserve_add_snapshot(snapshot, snapshot->count + 1);
      snapshot->shells[snapshot->count] = frame->self;
      snapshot->private_environments[snapshot->count] = private_environment;
      snapshot->cores[snapshot->count] = core;
      snapshot->shadow_signatures[snapshot->count] = shadow_signature;
      snapshot->shadow_signature_contents[snapshot->count] =
        shadow_signature_content;
      ++snapshot->count;
      UNPROTECT(2);
    }

    frame = &frames[depth - 1];
    if (frame->next_child == frame->child_count) {
      --depth;
      continue;
    }
    SEXP child = VECTOR_ELT(frame->sets, frame->next_child);
    ++frame->next_child;
    if (TYPEOF(child) != ENVSXP || Rf_isS4(child)) {
      Rf_error("Cannot add corrupt ParamSet graph child reference");
    }
    if (forbidden != R_NilValue && child == forbidden) {
      Rf_error("Adding ParamSet would create a cycle in the capsule graph");
    }
    for (R_xlen_t ancestor = 0; ancestor < depth; ++ancestor) {
      paradox_account_work(work_since_interrupt);
      if (frames[ancestor].self == child) {
        Rf_error("ParamSet capsule graph contains a cycle");
      }
    }
    if (snapshot_shell_index(
        snapshot,
        child,
        work_since_interrupt
      ) != R_XLEN_T_MAX) {
      continue;
    }
    if (depth == frame_capacity) {
      if (frame_capacity > R_XLEN_T_MAX / 2) {
        Rf_error("ParamSet capsule graph is too deep to add");
      }
      const R_xlen_t expanded_capacity = frame_capacity * 2;
      add_graph_frame_t *expanded = paradox_temporary_alloc(
        expanded_capacity,
        sizeof(*expanded)
      );
      memcpy(expanded, frames, (size_t) depth * sizeof(*expanded));
      frames = expanded;
      frame_capacity = expanded_capacity;
    }
    frames[depth] = (add_graph_frame_t) {
      child, R_NilValue, 0, 0, FALSE
    };
    ++depth;
  }
}

static int add_graph_snapshot_is_current(
    const add_graph_snapshot_t *snapshot,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t index = 0; index < snapshot->count; ++index) {
    paradox_account_work(work_since_interrupt);
    const int is_shadow =
      paradox_core_kind(snapshot->cores[index]) == PARADOX_CORE_SHADOW;
    if (paradox_domain_required_private_environment(
          snapshot->shells[index]
        ) != snapshot->private_environments[index] ||
        paradox_core_from_private(snapshot->private_environments[index]) !=
        snapshot->cores[index] ||
        is_shadow != (snapshot->shadow_signatures[index] != R_NilValue) ||
        is_shadow != (
          snapshot->shadow_signature_contents[index] != R_NilValue
        ) ||
        (is_shadow && !paradox_shadow_signature_receipt_is_current(
          snapshot->cores[index],
          snapshot->shadow_signatures[index],
          snapshot->shadow_signature_contents[index]
        ))) {
      return FALSE;
    }
  }
  return TRUE;
}

static int collection_graph_snapshot_is_current(
    const paradox_collection_graph_t *graph,
    R_xlen_t *work_since_interrupt) {
  for (R_xlen_t index = 0; index < graph->count; ++index) {
    paradox_account_work(work_since_interrupt);
    const paradox_collection_graph_node_t *node = &graph->nodes[index];
    if (paradox_domain_required_private_environment(node->self) !=
          node->private_environment ||
        paradox_core_from_private(node->private_environment) != node->core) {
      return FALSE;
    }
  }
  return TRUE;
}

static void copy_table_rows(SEXP destination, R_xlen_t destination_offset,
    SEXP source, R_xlen_t row_count,
    R_xlen_t *work_since_interrupt) {
  if (XLENGTH(destination) != XLENGTH(source)) {
    Rf_error("Internal error: incompatible collection add tables");
  }
  for (R_xlen_t column = 0; column < XLENGTH(destination); ++column) {
    for (R_xlen_t row = 0; row < row_count; ++row) {
      paradox_account_work(work_since_interrupt);
      copy_at(
        VECTOR_ELT(destination, column),
        destination_offset + row,
        VECTOR_ELT(source, column),
        row
      );
    }
  }
}

static SEXP append_collection_table(SEXP left, R_xlen_t left_rows,
    SEXP right, R_xlen_t right_rows, const char *const *column_names,
    const SEXPTYPE *types, R_xlen_t column_count,
    R_xlen_t *work_since_interrupt) {
  R_xlen_t total_rows = left_rows;
  if (!checked_add(&total_rows, right_rows) || total_rows > INT_MAX) {
    Rf_error("ParamSetCollection add result exceeds data.frame row limits");
  }
  SEXP result = PROTECT(paradox_domain_new_plain_table(
    column_names,
    types,
    column_count,
    total_rows
  ));
  copy_table_rows(result, 0, left, left_rows, work_since_interrupt);
  copy_table_rows(
    result,
    left_rows,
    right,
    right_rows,
    work_since_interrupt
  );
  UNPROTECT(1);
  return result;
}

/* Evaluate one test-only reentry hook at an exact snapshot boundary. The
 * production entry point supplies `R_NilValue`, so the accepted path pays one
 * pointer comparison inside a transaction that already allocates. */
static void run_add_test_hook(SEXP hook) {
  if (hook == R_NilValue) return;
  SEXP call = PROTECT(Rf_lang1(hook));
  SEXP ignored = PROTECT(Rf_eval(call, R_BaseEnv));
  (void) ignored;
  UNPROTECT(2);
}

static SEXP param_set_collection_add_impl(SEXP private_environment, SEXP self,
    SEXP child, SEXP name, SEXP tag_sets_sexp, SEXP tag_params_sexp,
    SEXP graph_hook, SEXP topology_hook) {
  PROTECT(private_environment);
  PROTECT(self);
  PROTECT(child);
  PROTECT(name);
  PROTECT(tag_sets_sexp);
  PROTECT(tag_params_sexp);
  const int tag_sets = checked_flag(tag_sets_sexp, "tag_sets");
  const int tag_params = checked_flag(tag_params_sexp, "tag_params");
  SEXP owner = checked_add_name(name);

  /* Both graphs are flattened here, so both must already agree with their own
   * children; otherwise the transaction would append to, or admit, an
   * outdated schema. */
  SEXP selected_core = paradox_core_from_private(private_environment);
  if (selected_core != R_UnboundValue &&
      !paradox_core_is_verified(selected_core)) {
    (void) paradox_core_refresh(self, private_environment);
  }

  R_xlen_t work_since_interrupt = 0;
  PROTECT_INDEX current_graph_roots_index;
  SEXP current_graph_roots;
  PROTECT_WITH_INDEX(
    current_graph_roots = R_NilValue,
    &current_graph_roots_index
  );
  paradox_collection_graph_t current_graph;
  /* Both flattened graphs are retained across the complete append, so both
   * take the receipted admission: a SHADOW leaf's exact metadata carrier is
   * the one part of its selected generation that can still move afterwards. */
  paradox_collection_graph_build_receipted(
    private_environment,
    self,
    &current_graph,
    &current_graph_roots,
    current_graph_roots_index,
    &work_since_interrupt
  );
  paradox_collection_graph_node_t *root = &current_graph.nodes[0];
  SEXP old_core = root->core;
  SEXP old_state = root->state;
  SEXP old_sets = root->sets;
  SEXP old_set_names = root->set_names;
  const R_xlen_t old_child_count = XLENGTH(old_sets);
  if (old_child_count >= INT_MAX) {
    Rf_error("ParamSetCollection contains too many child sets to add another");
  }
  if (CHAR(owner)[0] != '\0') {
    for (R_xlen_t index = 0; index < old_child_count; ++index) {
      paradox_account_work(&work_since_interrupt);
      SEXP existing = STRING_ELT(old_set_names, index);
      if (CHAR(existing)[0] != '\0' &&
          strcmp(CHAR(existing), CHAR(owner)) == 0) {
        Rf_error(
          "Set name '%s' already present in collection!",
          CHAR(owner)
        );
      }
    }
  }

  SEXP current_child_roots = PROTECT(Rf_allocVector(
    VECSXP,
    CONSTRUCTOR_ROOT_STRIDE
  ));
  constructor_child_t current_child;
  if (!initialize_child(
      self,
      R_BlankString,
      current_child_roots,
      0,
      &current_child,
      &work_since_interrupt
    ) || paradox_core_from_private(private_environment) != old_core) {
    Rf_error("Corrupt or concurrently changed ParamSetCollection state");
  }

  SEXP singleton = PROTECT(Rf_allocVector(VECSXP, 1));
  SEXP singleton_names = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_VECTOR_ELT(singleton, 0, child);
  SET_STRING_ELT(singleton_names, 0, owner);
  Rf_setAttrib(singleton, R_NamesSymbol, singleton_names);
  SEXP singleton_tag_sets = PROTECT(uniform_edge_flags(tag_sets, 1));
  SEXP singleton_tag_params = PROTECT(uniform_edge_flags(tag_params, 1));
  SEXP child_static = PROTECT(build_collection_static_state(
    singleton,
    singleton_tag_sets,
    singleton_tag_params,
    R_NilValue,
    root->postfix,
    "`n`",
    R_NilValue,
    R_NilValue
  ));

  PROTECT_INDEX child_graph_roots_index;
  SEXP child_graph_roots;
  PROTECT_WITH_INDEX(child_graph_roots = R_NilValue, &child_graph_roots_index);
  paradox_collection_graph_t child_graph;
  int child_graph_built = FALSE;
  SEXP child_private = PROTECT(paradox_domain_private_environment(child));
  SEXP child_core = PROTECT(
    child_private == R_UnboundValue
      ? R_UnboundValue
      : paradox_core_from_private(child_private)
  );
  if (child_private == R_UnboundValue || child_core == R_UnboundValue) {
    UNPROTECT(2);
    Rf_error("Cannot add unsupported or corrupt ParamSet child state");
  }
  if (paradox_core_kind(child_core) == PARADOX_CORE_COLLECTION) {
    paradox_collection_graph_build_receipted(
      child_private,
      child,
      &child_graph,
      &child_graph_roots,
      child_graph_roots_index,
      &work_since_interrupt
    );
    child_graph_built = TRUE;
  } else {
    paradox_collection_validate_single_node(
      child_private,
      child,
      &child_graph_roots,
      child_graph_roots_index,
      &work_since_interrupt
    );
  }
  UNPROTECT(2);
  run_add_test_hook(graph_hook);

  PROTECT_INDEX current_topology_roots_index;
  SEXP current_topology_roots;
  PROTECT_WITH_INDEX(
    current_topology_roots = R_NilValue,
    &current_topology_roots_index
  );
  add_graph_snapshot_t current_topology;
  snapshot_add_graph(
    self,
    R_NilValue,
    &current_topology,
    &current_topology_roots,
    current_topology_roots_index,
    &work_since_interrupt
  );
  PROTECT_INDEX child_topology_roots_index;
  SEXP child_topology_roots;
  PROTECT_WITH_INDEX(
    child_topology_roots = R_NilValue,
    &child_topology_roots_index
  );
  add_graph_snapshot_t child_topology;
  snapshot_add_graph(
    child,
    self,
    &child_topology,
    &child_topology_roots,
    child_topology_roots_index,
    &work_since_interrupt
  );
  run_add_test_hook(topology_hook);

  SEXP old_params = VECTOR_ELT(old_state, PARADOX_CORE_PARAMS);
  SEXP old_tags = VECTOR_ELT(old_state, PARADOX_CORE_TAGS);
  SEXP old_trafos = VECTOR_ELT(old_state, PARADOX_CORE_TRAFOS);
  SEXP old_translation = VECTOR_ELT(
    old_state,
    PARADOX_CORE_TRANSLATION
  );
  SEXP child_params = VECTOR_ELT(child_static, 0);
  SEXP child_tags = VECTOR_ELT(child_static, 1);
  SEXP child_trafos = VECTOR_ELT(child_static, 2);
  SEXP child_translation = VECTOR_ELT(child_static, 3);
  const R_xlen_t child_param_count = XLENGTH(VECTOR_ELT(child_params, 0));
  const R_xlen_t child_tag_count = XLENGTH(VECTOR_ELT(child_tags, 0));
  const R_xlen_t child_trafo_count = XLENGTH(VECTOR_ELT(child_trafos, 0));

  for (R_xlen_t row = 0; row < child_param_count; ++row) {
    paradox_account_work(&work_since_interrupt);
    SEXP id = STRING_ELT(VECTOR_ELT(child_params, 0), row);
    if (contains_id(VECTOR_ELT(old_params, 0), id)) {
      Rf_error(
        "Adding parameter set would lead to nameclashes: %s",
        CHAR(id)
      );
    }
  }

  SEXP params = PROTECT(append_collection_table(
    old_params,
    current_child.param_count,
    child_params,
    child_param_count,
    paradox_domain_column_names,
    params_types,
    PARADOX_DOMAIN_TAGS,
    &work_since_interrupt
  ));
  SEXP tags = PROTECT(append_collection_table(
    old_tags,
    current_child.tag_count,
    child_tags,
    child_tag_count,
    tag_names,
    tag_types,
    2,
    &work_since_interrupt
  ));
  SEXP trafos = PROTECT(append_collection_table(
    old_trafos,
    current_child.trafo_count,
    child_trafos,
    child_trafo_count,
    trafo_names,
    trafo_types,
    2,
    &work_since_interrupt
  ));
  SEXP translation = PROTECT(append_collection_table(
    old_translation,
    current_child.param_count,
    child_translation,
    child_param_count,
    translation_names,
    translation_types,
    4,
    &work_since_interrupt
  ));
  const int new_owner_index = (int) old_child_count + 1;
  for (R_xlen_t row = 0; row < child_param_count; ++row) {
    SET_INTEGER_ELT(
      VECTOR_ELT(translation, 2),
      current_child.param_count + row,
      new_owner_index
    );
  }
  if (Rf_any_duplicated(VECTOR_ELT(params, 0), FALSE) != 0 ||
      Rf_any_duplicated(VECTOR_ELT(translation, 0), FALSE) != 0) {
    Rf_error("Adding parameter set would lead to duplicate translated IDs");
  }

  SEXP sets = PROTECT(Rf_allocVector(VECSXP, old_child_count + 1));
  SEXP set_names = PROTECT(Rf_allocVector(STRSXP, old_child_count + 1));
  for (R_xlen_t index = 0; index < old_child_count; ++index) {
    SET_VECTOR_ELT(sets, index, VECTOR_ELT(old_sets, index));
    SET_STRING_ELT(set_names, index, STRING_ELT(old_set_names, index));
  }
  SET_VECTOR_ELT(sets, old_child_count, child);
  SET_STRING_ELT(set_names, old_child_count, owner);
  Rf_setAttrib(sets, R_NamesSymbol, set_names);

  /* Extend the edge record with the generation and flags this append actually
   * consumed. The appended core comes from the singleton flatten, so it is the
   * exact generation the new rows describe. */
  SEXP old_edges = VECTOR_ELT(old_state, PARADOX_CORE_EDGES);
  SEXP child_edges = VECTOR_ELT(child_static, 4);
  SEXP edge_cores = PROTECT(Rf_allocVector(VECSXP, old_child_count + 1));
  SEXP edge_tag_sets = PROTECT(Rf_allocVector(LGLSXP, old_child_count + 1));
  SEXP edge_tag_params = PROTECT(Rf_allocVector(
    LGLSXP,
    old_child_count + 1
  ));
  for (R_xlen_t index = 0; index < old_child_count; ++index) {
    SET_VECTOR_ELT(
      edge_cores,
      index,
      VECTOR_ELT(VECTOR_ELT(old_edges, PARADOX_COLLECTION_EDGE_CORES), index)
    );
    SET_LOGICAL_ELT(
      edge_tag_sets,
      index,
      LOGICAL_ELT(
        VECTOR_ELT(old_edges, PARADOX_COLLECTION_EDGE_TAG_SETS),
        index
      )
    );
    SET_LOGICAL_ELT(
      edge_tag_params,
      index,
      LOGICAL_ELT(
        VECTOR_ELT(old_edges, PARADOX_COLLECTION_EDGE_TAG_PARAMS),
        index
      )
    );
  }
  SET_VECTOR_ELT(
    edge_cores,
    old_child_count,
    VECTOR_ELT(VECTOR_ELT(child_edges, PARADOX_COLLECTION_EDGE_CORES), 0)
  );
  SET_LOGICAL_ELT(edge_tag_sets, old_child_count, tag_sets);
  SET_LOGICAL_ELT(edge_tag_params, old_child_count, tag_params);
  SEXP edges = PROTECT(Rf_allocVector(
    VECSXP,
    PARADOX_COLLECTION_EDGE_FIELD_COUNT
  ));
  SET_VECTOR_ELT(edges, PARADOX_COLLECTION_EDGE_CORES, edge_cores);
  SET_VECTOR_ELT(edges, PARADOX_COLLECTION_EDGE_TAG_SETS, edge_tag_sets);
  SET_VECTOR_ELT(edges, PARADOX_COLLECTION_EDGE_TAG_PARAMS, edge_tag_params);
  /* An assignment this node made governs the IDs it named; the appended child
   * brings IDs it never named, so the record carries over untouched and the
   * appended rows stay derived. */
  SET_VECTOR_ELT(
    edges,
    PARADOX_COLLECTION_EDGE_TAG_OVERRIDE,
    VECTOR_ELT(old_edges, PARADOX_COLLECTION_EDGE_TAG_OVERRIDE)
  );
  SEXP edge_names = PROTECT(paradox_domain_character_vector(
    (const char *const[]) {"cores", "tag_sets", "tag_params", "tag_override"},
    PARADOX_COLLECTION_EDGE_FIELD_COUNT
  ));
  Rf_setAttrib(edges, R_NamesSymbol, edge_names);

  SEXP fields[PARADOX_CORE_FIELD_COUNT];
  for (int field = 0; field < PARADOX_CORE_FIELD_COUNT; ++field) {
    fields[field] = VECTOR_ELT(old_state, field);
  }
  fields[PARADOX_CORE_PARAMS] = params;
  fields[PARADOX_CORE_TAGS] = tags;
  fields[PARADOX_CORE_TRAFOS] = trafos;
  fields[PARADOX_CORE_SETS] = sets;
  fields[PARADOX_CORE_TRANSLATION] = translation;
  fields[PARADOX_CORE_EDGES] = edges;
  SEXP replacement = PROTECT(paradox_core_new_from_fields(
    PARADOX_CORE_COLLECTION,
    fields
  ));

  /* The two scans are complementary, not redundant: the local one re-selects
   * every live shell/private/capsule binding, while the shared one proves that
   * each retained generation -- including a SHADOW leaf's exact metadata
   * carrier and entries -- is still the one this append was derived from. */
  if (paradox_core_from_private(private_environment) != old_core ||
      !collection_graph_snapshot_is_current(
        &current_graph,
        &work_since_interrupt
      ) || !paradox_collection_graph_snapshot_is_intact(
        &current_graph,
        &work_since_interrupt
      ) || (child_graph_built && (!collection_graph_snapshot_is_current(
        &child_graph,
        &work_since_interrupt
      ) || !paradox_collection_graph_snapshot_is_intact(
        &child_graph,
        &work_since_interrupt
      ))) || !add_graph_snapshot_is_current(
        &current_topology,
        &work_since_interrupt
      ) || !add_graph_snapshot_is_current(
        &child_topology,
        &work_since_interrupt
      )) {
    Rf_error("ParamSet capsule graph changed during collection add");
  }

  /* The flatten grew, so every derived schema in the session revalidates; this
   * generation was just built from its current children and is exempt. */
  paradox_core_note_change(PARADOX_CORE_CHANGE_SCHEMA);
  paradox_core_stamp_verified(replacement);
  Rf_defineVar(Rf_install(".core"), replacement, private_environment);
  UNPROTECT(28);
  return self;
}

SEXP paradox_param_set_collection_add(SEXP private_environment, SEXP self,
    SEXP child, SEXP name, SEXP tag_sets_sexp, SEXP tag_params_sexp) {
  return param_set_collection_add_impl(
    private_environment,
    self,
    child,
    name,
    tag_sets_sexp,
    tag_params_sexp,
    R_NilValue,
    R_NilValue
  );
}

SEXP paradox_test_param_set_collection_add_reentry(SEXP private_environment,
    SEXP self, SEXP child, SEXP name, SEXP tag_sets_sexp,
    SEXP tag_params_sexp, SEXP graph_hook, SEXP topology_hook) {
  if ((graph_hook != R_NilValue && !Rf_isFunction(graph_hook)) ||
      (topology_hook != R_NilValue && !Rf_isFunction(topology_hook))) {
    Rf_error("Collection add reentry test hook must be a function or NULL");
  }
  return param_set_collection_add_impl(
    private_environment,
    self,
    child,
    name,
    tag_sets_sexp,
    tag_params_sexp,
    graph_hook,
    topology_hook
  );
}

SEXP paradox_collection_reflatten(SEXP private_environment, SEXP core,
    R_xlen_t changed_edge) {
  PROTECT(private_environment);
  PROTECT(core);
  SEXP state = R_ExternalPtrProtected(core);
  SEXP sets = VECTOR_ELT(state, PARADOX_CORE_SETS);
  SEXP edges = VECTOR_ELT(state, PARADOX_CORE_EDGES);
  if (TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isS4(sets) ||
      Rf_isObject(sets) ||
      !paradox_params_names_are_only_attribute(sets)) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSetCollection child edges");
  }
  int postfix = FALSE;
  if (!exact_flag(VECTOR_ELT(state, PARADOX_CORE_POSTFIX), &postfix)) {
    UNPROTECT(2);
    Rf_error("Corrupt ParamSetCollection affix policy");
  }
  SEXP set_names = PROTECT(Rf_getAttrib(sets, R_NamesSymbol));
  SEXP changed_owner = TYPEOF(set_names) == STRSXP &&
      changed_edge >= 0 && changed_edge < XLENGTH(set_names)
    ? STRING_ELT(set_names, changed_edge)
    : R_BlankString;
  /* The record is a cache and may describe fewer or more edges than the node
   * currently has; an edge it does not describe simply carries no generated
   * tags. */
  const R_xlen_t child_count = TYPEOF(sets) == VECSXP ? XLENGTH(sets) : 0;
  SEXP tag_sets = PROTECT(resized_edge_flags(
    VECTOR_ELT(edges, PARADOX_COLLECTION_EDGE_TAG_SETS),
    child_count
  ));
  SEXP tag_params = PROTECT(resized_edge_flags(
    VECTOR_ELT(edges, PARADOX_COLLECTION_EDGE_TAG_PARAMS),
    child_count
  ));
  /* Rebuilding through the constructor's own builder is what makes the healed
   * node identical to a collection freshly constructed from these children,
   * including the per-edge tag flags recorded when each edge was added. */
  SEXP rebuilt = PROTECT(build_collection_static_state(
    sets,
    tag_sets,
    tag_params,
    VECTOR_ELT(edges, PARADOX_COLLECTION_EDGE_TAG_OVERRIDE),
    postfix,
    "`sets` names",
    changed_owner,
    R_NilValue
  ));

  SEXP fields[PARADOX_CORE_FIELD_COUNT];
  for (int field = 0; field < PARADOX_CORE_FIELD_COUNT; ++field) {
    fields[field] = VECTOR_ELT(state, field);
  }
  fields[PARADOX_CORE_PARAMS] = VECTOR_ELT(rebuilt, 0);
  fields[PARADOX_CORE_TAGS] = VECTOR_ELT(rebuilt, 1);
  fields[PARADOX_CORE_TRAFOS] = VECTOR_ELT(rebuilt, 2);
  /*
   * The builder froze names and child identities together before deriving the
   * replacement tables.  Install that exact edge snapshot as well: retaining
   * the caller-owned list here could pair the rebuilt schema with a later
   * by-reference mutation of the old capsule's `.sets` carrier.
   */
  fields[PARADOX_CORE_SETS] = VECTOR_ELT(rebuilt, 5);
  fields[PARADOX_CORE_TRANSLATION] = VECTOR_ELT(rebuilt, 3);
  fields[PARADOX_CORE_EDGES] = VECTOR_ELT(rebuilt, 4);
  SEXP replacement = PROTECT(paradox_core_new_from_fields(
    PARADOX_CORE_COLLECTION,
    fields
  ));
  if (paradox_core_from_private(private_environment) != core) {
    UNPROTECT(7);
    Rf_error("ParamSetCollection capsule changed during refresh");
  }
  /* A refresh installs the schema this node already denoted, so it is not a
   * semantic change and must not advance the epoch. */
  Rf_defineVar(Rf_install(".core"), replacement, private_environment);
  UNPROTECT(7);
  return replacement;
}
