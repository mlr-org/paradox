#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "paramset_domain_common.h"
#include "paramset_params_internal.h"
#include "r_api_compat.h"
#include "r_utils.h"

typedef enum {
  PARAMS_NODE_UNKNOWN = 0,
  PARAMS_NODE_SET,
  PARAMS_NODE_COLLECTION
} params_node_kind_t;

typedef struct {
  SEXP table;
  SEXP names;
  SEXP classes;
  SEXP sorted;
  SEXP index;
  SEXP index_cache;
  SEXP ids;
  SEXP original_ids;
  SEXP owner_indices;
  SEXP owner_names;
  R_xlen_t row_count;
} collection_translation_t;

typedef struct {
  SEXP self;
  SEXP private_environment;
  params_node_kind_t kind;
  paradox_params_state_t params_state;
  SEXP sets;
  SEXP set_names;
  R_xlen_t child_count;
  SEXP postfix;
  collection_translation_t translation;
  R_xlen_t *translation_by_param;
  R_xlen_t next_child;
  R_xlen_t consumed_rows;
} collection_frame_t;

enum collection_root_slot {
  COLLECTION_ROOT_SELF = 0,
  COLLECTION_ROOT_PRIVATE,
  COLLECTION_ROOT_PARAMS_STATE,
  COLLECTION_ROOT_SETS,
  COLLECTION_ROOT_SET_NAMES,
  COLLECTION_ROOT_TRANSLATION,
  COLLECTION_ROOT_TRANSLATION_NAMES,
  COLLECTION_ROOT_TRANSLATION_CLASSES,
  COLLECTION_ROOT_TRANSLATION_SORTED,
  COLLECTION_ROOT_TRANSLATION_INDEX,
  COLLECTION_ROOT_TRANSLATION_INDEX_CACHE,
  COLLECTION_ROOT_TRANSLATION_IDS,
  COLLECTION_ROOT_TRANSLATION_ORIGINAL_IDS,
  COLLECTION_ROOT_TRANSLATION_OWNER_INDICES,
  COLLECTION_ROOT_TRANSLATION_OWNER_NAMES,
  COLLECTION_ROOT_PARAMS_INDEX,
  COLLECTION_ROOT_PARAMS_INDEX_CACHE,
  COLLECTION_ROOT_POSTFIX,
  COLLECTION_ROOT_COUNT
};

static int has_no_attributes(SEXP value) {
  return paradox_api_has_no_attributes(value);
}

static params_node_kind_t exact_node_kind(SEXP self,
    R_xlen_t *work_since_interrupt) {
  static const char *const set_classes[] = {"ParamSet", "R6"};
  static const char *const collection_classes[] = {
    "ParamSetCollection", "ParamSet", "R6"
  };
  if (TYPEOF(self) != ENVSXP) {
    return PARAMS_NODE_UNKNOWN;
  }
  SEXP classes = Rf_getAttrib(self, R_ClassSymbol);
  if (paradox_domain_exact_string_vector(
      classes,
      set_classes,
      2,
      work_since_interrupt
    )) {
    return PARAMS_NODE_SET;
  }
  if (paradox_domain_exact_string_vector(
      classes,
      collection_classes,
      3,
      work_since_interrupt
    )) {
    return PARAMS_NODE_COLLECTION;
  }
  return PARAMS_NODE_UNKNOWN;
}

static SEXP owned_private_environment(SEXP self) {
  SEXP enclosure = PROTECT(paradox_domain_local_value(
    self,
    ".__enclos_env__"
  ));
  if (TYPEOF(enclosure) != ENVSXP) {
    UNPROTECT(1);
    return R_UnboundValue;
  }
  SEXP private_environment = paradox_domain_local_value(enclosure, "private");
  SEXP result = TYPEOF(private_environment) == ENVSXP
    ? private_environment
    : R_UnboundValue;
  UNPROTECT(1);
  return result;
}

static int exact_index_marker(SEXP index, const char *marker_name,
    SEXP roots, R_xlen_t index_slot, R_xlen_t cache_slot) {
  if (TYPEOF(index) != INTSXP || ALTREP(index)) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, index_slot, index);
  const SEXP marker = Rf_install(marker_name);
  SEXP cache = Rf_getAttrib(index, marker);
  if (TYPEOF(cache) != INTSXP || ALTREP(cache)) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, cache_slot, cache);
  if (XLENGTH(index) != 0) {
    return FALSE;
  }
  return paradox_api_has_single_attribute(index, marker_name);
}

static int validate_set_names(SEXP sets, SEXP *names,
    R_xlen_t *set_count, SEXP roots,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(sets) != VECSXP || ALTREP(sets) || Rf_isObject(sets) ||
      !paradox_params_names_are_only_attribute(sets)) {
    return FALSE;
  }
  const R_xlen_t observed_set_count = XLENGTH(sets);
  *names = Rf_getAttrib(sets, R_NamesSymbol);
  if (TYPEOF(*names) != STRSXP || ALTREP(*names) ||
      !has_no_attributes(*names)) {
    return FALSE;
  }
  SET_VECTOR_ELT(roots, COLLECTION_ROOT_SET_NAMES, *names);
  const R_xlen_t observed_name_count = XLENGTH(*names);
  if (observed_name_count != observed_set_count) {
    return FALSE;
  }
  for (R_xlen_t right = 0; right < observed_name_count; ++right) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP right_name = STRING_ELT(*names, right);
    if (right_name == NA_STRING) {
      return FALSE;
    }
    if (paradox_domain_string_is(right_name, "")) {
      continue;
    }
    for (R_xlen_t left = 0; left < right; ++left) {
      paradox_domain_account_work(work_since_interrupt);
      SEXP left_name = STRING_ELT(*names, left);
      if (!paradox_domain_string_is(left_name, "") &&
          paradox_domain_strings_equal(left_name, right_name)) {
        return FALSE;
      }
    }
  }
  *set_count = observed_set_count;
  return TRUE;
}

static int exact_translation_table(SEXP table,
    collection_translation_t *translation, SEXP roots,
    R_xlen_t *work_since_interrupt) {
  static const char *const column_names[] = {
    "id", "original_id", "owner_ps_index", "owner_name"
  };
  static const char *const classes[] = {"data.table", "data.frame"};
  static const char *const key[] = {"id"};
  if (TYPEOF(table) != VECSXP || ALTREP(table)) {
    return FALSE;
  }
  const R_xlen_t column_count = XLENGTH(table);
  SEXP names = Rf_getAttrib(table, R_NamesSymbol);
  SEXP table_classes = Rf_getAttrib(table, R_ClassSymbol);
  SEXP sorted = Rf_getAttrib(table, Rf_install("sorted"));
  SEXP index = Rf_getAttrib(table, Rf_install("index"));
  SET_VECTOR_ELT(roots, COLLECTION_ROOT_TRANSLATION_NAMES, names);
  SET_VECTOR_ELT(roots, COLLECTION_ROOT_TRANSLATION_CLASSES, table_classes);
  SET_VECTOR_ELT(roots, COLLECTION_ROOT_TRANSLATION_SORTED, sorted);
  if (column_count != 4 || TYPEOF(names) != STRSXP || ALTREP(names) ||
      TYPEOF(table_classes) != STRSXP || ALTREP(table_classes) ||
      TYPEOF(sorted) != STRSXP || ALTREP(sorted) ||
      !has_no_attributes(names) || !has_no_attributes(table_classes) ||
      !has_no_attributes(sorted) || !paradox_domain_exact_string_vector(
        names,
        column_names,
        4,
        work_since_interrupt
      ) || !paradox_domain_exact_string_vector(
        table_classes,
        classes,
        2,
        work_since_interrupt
      ) || !paradox_domain_exact_string_vector(
        sorted,
        key,
        1,
        work_since_interrupt
      ) || !paradox_params_supported_table_attributes(table, TRUE) ||
      Rf_getAttrib(table, R_RowNamesSymbol) != R_NilValue ||
      Rf_getAttrib(table, Rf_install(".internal.selfref")) != R_NilValue ||
      !exact_index_marker(
        index,
        "__original_id",
        roots,
        COLLECTION_ROOT_TRANSLATION_INDEX,
        COLLECTION_ROOT_TRANSLATION_INDEX_CACHE
      )) {
    return FALSE;
  }

  SEXP ids = VECTOR_ELT(table, 0);
  SEXP original_ids = VECTOR_ELT(table, 1);
  SEXP owner_indices = VECTOR_ELT(table, 2);
  SEXP owner_names = VECTOR_ELT(table, 3);
  SET_VECTOR_ELT(roots, COLLECTION_ROOT_TRANSLATION_IDS, ids);
  SET_VECTOR_ELT(
    roots,
    COLLECTION_ROOT_TRANSLATION_ORIGINAL_IDS,
    original_ids
  );
  SET_VECTOR_ELT(
    roots,
    COLLECTION_ROOT_TRANSLATION_OWNER_INDICES,
    owner_indices
  );
  SET_VECTOR_ELT(
    roots,
    COLLECTION_ROOT_TRANSLATION_OWNER_NAMES,
    owner_names
  );
  if (TYPEOF(ids) != STRSXP || TYPEOF(original_ids) != STRSXP ||
      TYPEOF(owner_indices) != INTSXP || TYPEOF(owner_names) != STRSXP ||
      ALTREP(ids) || ALTREP(original_ids) || ALTREP(owner_indices) ||
      ALTREP(owner_names) ||
      !has_no_attributes(ids) || !has_no_attributes(original_ids) ||
      !has_no_attributes(owner_indices) || !has_no_attributes(owner_names)) {
    return FALSE;
  }
  const R_xlen_t row_count = XLENGTH(ids);
  if (XLENGTH(original_ids) != row_count ||
      XLENGTH(owner_indices) != row_count ||
      XLENGTH(owner_names) != row_count) {
    return FALSE;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    if (STRING_ELT(ids, row) == NA_STRING ||
        STRING_ELT(original_ids, row) == NA_STRING ||
        STRING_ELT(owner_names, row) == NA_STRING ||
        INTEGER_ELT(owner_indices, row) == NA_INTEGER) {
      return FALSE;
    }
  }
  if (Rf_any_duplicated(ids, FALSE) != 0) {
    return FALSE;
  }

  translation->table = table;
  translation->names = names;
  translation->classes = table_classes;
  translation->sorted = sorted;
  translation->index = index;
  translation->index_cache = VECTOR_ELT(
    roots,
    COLLECTION_ROOT_TRANSLATION_INDEX_CACHE
  );
  translation->ids = ids;
  translation->original_ids = original_ids;
  translation->owner_indices = owner_indices;
  translation->owner_names = owner_names;
  translation->row_count = row_count;
  return TRUE;
}

static SEXP append_frame_roots(SEXP *root_plan,
    PROTECT_INDEX root_plan_index, SEXP self, SEXP private_environment) {
  SEXP roots = PROTECT(Rf_allocVector(VECSXP, COLLECTION_ROOT_COUNT));
  SET_VECTOR_ELT(roots, COLLECTION_ROOT_SELF, self);
  SET_VECTOR_ELT(roots, COLLECTION_ROOT_PRIVATE, private_environment);
  SEXP expanded_plan = PROTECT(Rf_cons(roots, *root_plan));
  REPROTECT(expanded_plan, root_plan_index);
  *root_plan = expanded_plan;
  UNPROTECT(2);
  return roots;
}

static R_xlen_t match_position(SEXP matches, R_xlen_t index) {
  if (TYPEOF(matches) == INTSXP) {
    const int value = INTEGER_ELT(matches, index);
    return value == NA_INTEGER || value <= 0 ? 0 : (R_xlen_t) value;
  }
  if (TYPEOF(matches) == REALSXP) {
    const double value = REAL_ELT(matches, index);
    return !R_FINITE(value) || value <= 0.0 ||
      value > (double) R_XLEN_T_MAX
      ? 0
      : (R_xlen_t) value;
  }
  Rf_error("Internal error: unexpected collection translation match");
}

static int initialize_collection_frame(SEXP self, SEXP private_environment,
    collection_frame_t *frame, SEXP *root_plan,
    PROTECT_INDEX root_plan_index, R_xlen_t *work_since_interrupt) {
  SEXP roots = append_frame_roots(
    root_plan,
    root_plan_index,
    self,
    private_environment
  );
  frame->self = self;
  frame->private_environment = private_environment;
  frame->kind = exact_node_kind(self, work_since_interrupt);
  frame->sets = R_NilValue;
  frame->set_names = R_NilValue;
  frame->child_count = 0;
  frame->postfix = R_NilValue;
  frame->translation_by_param = NULL;
  frame->next_child = 0;
  frame->consumed_rows = 0;
  if (frame->kind == PARAMS_NODE_UNKNOWN ||
      !paradox_domain_owns_private_environment(self, private_environment) ||
      !paradox_params_canonical_active_member(
        self,
        private_environment,
        "params",
        frame->kind == PARAMS_NODE_COLLECTION
          ? ".__ParamSetCollection__params"
          : ".__ParamSet__params",
        "rhs",
        frame->kind == PARAMS_NODE_COLLECTION
          ? ".__ParamSet__params"
          : NULL,
        work_since_interrupt
      ) ||
      !paradox_params_canonical_active_member(
        self,
        private_environment,
        "deps",
        frame->kind == PARAMS_NODE_COLLECTION
          ? ".__ParamSetCollection__deps"
          : ".__ParamSet__deps",
        "v",
        frame->kind == PARAMS_NODE_COLLECTION
          ? ".__ParamSet__deps"
          : NULL,
        work_since_interrupt
      ) || !paradox_params_canonical_active_member(
        self,
        private_environment,
        "values",
        ".__ParamSet__values",
        "xs",
        NULL,
        work_since_interrupt
      ) || !paradox_params_canonical_active_member(
        self,
        private_environment,
        "tags",
        ".__ParamSet__tags",
        "v",
        NULL,
        work_since_interrupt
      ) ||
      !paradox_params_load_private_state_rooted(
        private_environment,
        &frame->params_state,
        roots,
        COLLECTION_ROOT_PARAMS_STATE,
        work_since_interrupt
      )) {
    return FALSE;
  }
  if (frame->kind == PARAMS_NODE_SET) {
    return TRUE;
  }

  frame->sets = paradox_domain_local_value(private_environment, ".sets");
  if (frame->sets != R_UnboundValue) {
    SET_VECTOR_ELT(roots, COLLECTION_ROOT_SETS, frame->sets);
  }
  SEXP translation_sexp = paradox_domain_local_value(
    private_environment,
    ".translation"
  );
  if (translation_sexp != R_UnboundValue) {
    SET_VECTOR_ELT(roots, COLLECTION_ROOT_TRANSLATION, translation_sexp);
  }
  frame->postfix = paradox_domain_local_value(private_environment, ".postfix");
  if (frame->postfix != R_UnboundValue) {
    SET_VECTOR_ELT(roots, COLLECTION_ROOT_POSTFIX, frame->postfix);
  }
  SEXP params_index = Rf_getAttrib(
    frame->params_state.params.table,
    Rf_install("index")
  );
  if (frame->sets == R_UnboundValue ||
      translation_sexp == R_UnboundValue ||
      frame->postfix == R_UnboundValue ||
      !validate_set_names(
        frame->sets,
        &frame->set_names,
        &frame->child_count,
        roots,
        work_since_interrupt
      ) || TYPEOF(frame->postfix) != LGLSXP || ALTREP(frame->postfix) ||
      !has_no_attributes(frame->postfix) ||
      XLENGTH(frame->postfix) != 1 ||
      LOGICAL_ELT(frame->postfix, 0) == NA_LOGICAL ||
      frame->params_state.values.size != 0 ||
      !exact_index_marker(
        params_index,
        "__id__cls__grouping",
        roots,
        COLLECTION_ROOT_PARAMS_INDEX,
        COLLECTION_ROOT_PARAMS_INDEX_CACHE
      ) || !exact_translation_table(
        translation_sexp,
        &frame->translation,
        roots,
        work_since_interrupt
      ) || frame->translation.row_count !=
        frame->params_state.params.row_count) {
    return FALSE;
  }

  const R_xlen_t child_count = frame->child_count;
  if (child_count > INT_MAX) {
    return FALSE;
  }
  for (R_xlen_t row = 0; row < frame->translation.row_count; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    const int owner = INTEGER_ELT(frame->translation.owner_indices, row);
    if (owner <= 0 || (R_xlen_t) owner > child_count ||
        !paradox_domain_strings_equal(
          STRING_ELT(frame->translation.owner_names, row),
          STRING_ELT(frame->set_names, (R_xlen_t) owner - 1)
        )) {
      return FALSE;
    }
  }
  for (R_xlen_t child = 0; child < child_count; ++child) {
    paradox_domain_account_work(work_since_interrupt);
    if (exact_node_kind(
        VECTOR_ELT(frame->sets, child),
        work_since_interrupt
      ) == PARAMS_NODE_UNKNOWN) {
      return FALSE;
    }
  }

  /* Rf_match(table, x, nomatch): for each construction-order `.params` ID,
   * retain its row in the independently keyed (and therefore usually
   * permuted) translation table.  Do not swap these arguments: a three-cycle
   * permutation is not its own inverse. */
  SEXP matches = PROTECT(Rf_match(
    frame->translation.ids,
    frame->params_state.params.ids,
    0
  ));
  if ((TYPEOF(matches) != INTSXP && TYPEOF(matches) != REALSXP) ||
      ALTREP(matches) ||
      XLENGTH(matches) != frame->params_state.params.row_count) {
    UNPROTECT(1);
    Rf_error("Internal error: unexpected collection translation match");
  }
  frame->translation_by_param = paradox_temporary_alloc(
    frame->params_state.params.row_count,
    sizeof(*frame->translation_by_param)
  );
  for (R_xlen_t row = 0;
      row < frame->params_state.params.row_count;
      ++row) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t position = match_position(matches, row);
    if (position == 0 || position > frame->translation.row_count) {
      UNPROTECT(1);
      return FALSE;
    }
    frame->translation_by_param[row] = position - 1;
  }
  UNPROTECT(1);
  return TRUE;
}

static int numeric_row_equal(SEXP left, R_xlen_t left_row,
    SEXP right, R_xlen_t right_row) {
  const SEXPTYPE left_type = (SEXPTYPE) TYPEOF(left);
  const SEXPTYPE right_type = (SEXPTYPE) TYPEOF(right);
  if ((left_type != INTSXP && left_type != REALSXP) ||
      (right_type != INTSXP && right_type != REALSXP)) {
    return FALSE;
  }
  double left_value;
  if (left_type == INTSXP) {
    const int integer_value = INTEGER_ELT(left, left_row);
    left_value = integer_value == NA_INTEGER
      ? NA_REAL
      : (double) integer_value;
  } else {
    left_value = REAL_ELT(left, left_row);
  }
  double right_value;
  if (right_type == INTSXP) {
    const int integer_value = INTEGER_ELT(right, right_row);
    right_value = integer_value == NA_INTEGER
      ? NA_REAL
      : (double) integer_value;
  } else {
    right_value = REAL_ELT(right, right_row);
  }
  if (ISNA(left_value) || ISNA(right_value)) {
    return ISNA(left_value) && ISNA(right_value);
  }
  if (ISNAN(left_value) || ISNAN(right_value)) {
    return ISNAN(left_value) && ISNAN(right_value);
  }
  return left_value == right_value;
}

static int permanent_row_equal(const paradox_domain_params_t *parent,
    R_xlen_t parent_row, const paradox_domain_params_t *child,
    R_xlen_t child_row, R_xlen_t *work_since_interrupt) {
  for (enum paradox_domain_column column = PARADOX_DOMAIN_CLS;
      column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    paradox_domain_account_work(work_since_interrupt);
    SEXP left = VECTOR_ELT(parent->table, column);
    SEXP right = VECTOR_ELT(child->table, column);
    switch (TYPEOF(left)) {
    case STRSXP:
      if (TYPEOF(right) != STRSXP || !paradox_domain_strings_equal(
          STRING_ELT(left, parent_row),
          STRING_ELT(right, child_row)
        )) {
        return FALSE;
      }
      break;
    case VECSXP:
      if (TYPEOF(right) != VECSXP) {
        return FALSE;
      }
      SEXP left_value = PROTECT(VECTOR_ELT(left, parent_row));
      SEXP right_value = PROTECT(VECTOR_ELT(right, child_row));
      const Rboolean identical = R_compute_identical(
        left_value,
        right_value,
        IDENT_USE_CLOENV
      );
      UNPROTECT(2);
      if (!identical) {
        return FALSE;
      }
      break;
    case INTSXP:
    case REALSXP:
      if (!numeric_row_equal(left, parent_row, right, child_row)) {
        return FALSE;
      }
      break;
    default:
      return FALSE;
    }
  }
  return TRUE;
}

static int translated_component(SEXP value, char **copy,
    size_t *size) {
  const int input_size = Rf_length(value);
  if (input_size < 0 || (size_t) input_size > (SIZE_MAX - 1) / 4) {
    return FALSE;
  }
  const size_t capacity = (size_t) input_size * 4 + 1;
  if (capacity > (size_t) R_XLEN_T_MAX) {
    return FALSE;
  }
  *copy = paradox_temporary_alloc((R_xlen_t) capacity, sizeof(**copy));
  const char *translated = Rf_translateCharUTF8(value);
  *size = strlen(translated);
  if (*size >= capacity) {
    return FALSE;
  }
  memcpy(*copy, translated, *size + 1);
  return TRUE;
}

static int affixed_id_equal(SEXP outer, SEXP owner, SEXP inner, int postfix) {
  if (paradox_domain_string_is(owner, "")) {
    return paradox_domain_strings_equal(outer, inner);
  }
  if (outer == NA_STRING || owner == NA_STRING || inner == NA_STRING ||
      Rf_getCharCE(outer) == CE_BYTES ||
      Rf_getCharCE(owner) == CE_BYTES ||
      Rf_getCharCE(inner) == CE_BYTES) {
    return FALSE;
  }
  PROTECT(outer);
  PROTECT(owner);
  PROTECT(inner);

  /* translated_component() deliberately owns its UTF-8 copies because a later
   * translation may recycle R's transient buffer. Bracket those R_alloc
   * copies per comparison so a large collection graph does not retain every
   * temporary string until the outer .Call returns. */
  const void *vmax = vmaxget();
  char *outer_text;
  char *owner_text;
  char *inner_text;
  size_t outer_size;
  size_t owner_size;
  size_t inner_size;
  int equal = FALSE;
  if (!translated_component(outer, &outer_text, &outer_size) ||
      !translated_component(owner, &owner_text, &owner_size) ||
      !translated_component(inner, &inner_text, &inner_size) ||
      inner_size == SIZE_MAX ||
      owner_size > SIZE_MAX - inner_size - 1U ||
      outer_size != owner_size + inner_size + 1) {
    goto cleanup;
  }
  if (postfix) {
    equal = memcmp(outer_text, inner_text, inner_size) == 0 &&
      outer_text[inner_size] == '.' &&
      memcmp(outer_text + inner_size + 1, owner_text, owner_size) == 0;
  } else {
    equal = memcmp(outer_text, owner_text, owner_size) == 0 &&
      outer_text[owner_size] == '.' &&
      memcmp(outer_text + owner_size + 1, inner_text, inner_size) == 0;
  }

cleanup:
  vmaxset(vmax);
  UNPROTECT(3);
  return equal;
}

static int validate_parent_child(collection_frame_t *parent,
    const collection_frame_t *child, R_xlen_t child_index,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t child_rows = child->params_state.params.row_count;
  if (parent->consumed_rows > parent->params_state.params.row_count ||
      child_rows > parent->params_state.params.row_count -
        parent->consumed_rows) {
    return FALSE;
  }
  SEXP owner_name = STRING_ELT(parent->set_names, child_index);
  const int postfix = LOGICAL_ELT(parent->postfix, 0);
  for (R_xlen_t row = 0; row < child_rows; ++row) {
    paradox_domain_account_work(work_since_interrupt);
    const R_xlen_t parent_row = parent->consumed_rows + row;
    const R_xlen_t translation_row =
      parent->translation_by_param[parent_row];
    SEXP parent_id = STRING_ELT(
      parent->params_state.params.ids,
      parent_row
    );
    SEXP child_id = STRING_ELT(child->params_state.params.ids, row);
    if (!affixed_id_equal(parent_id, owner_name, child_id, postfix) ||
        INTEGER_ELT(parent->translation.owner_indices, translation_row) !=
          (int) (child_index + 1) ||
        !paradox_domain_strings_equal(
          STRING_ELT(parent->translation.ids, translation_row),
          parent_id
        ) || !paradox_domain_strings_equal(
          STRING_ELT(parent->translation.original_ids, translation_row),
          child_id
        ) || !paradox_domain_strings_equal(
          STRING_ELT(parent->translation.owner_names, translation_row),
          owner_name
        ) || !permanent_row_equal(
          &parent->params_state.params,
          parent_row,
          &child->params_state.params,
          row,
          work_since_interrupt
        )) {
      return FALSE;
    }
  }
  parent->consumed_rows += child_rows;
  return TRUE;
}

static int validate_collection_graph(SEXP self, SEXP private_environment,
    collection_frame_t **frames_out, SEXP *root_plan,
    PROTECT_INDEX root_plan_index, R_xlen_t *work_since_interrupt) {
  R_xlen_t capacity = 8;
  collection_frame_t *frames = paradox_temporary_alloc(
    capacity,
    sizeof(*frames)
  );
  if (!initialize_collection_frame(
      self,
      private_environment,
      &frames[0],
      root_plan,
      root_plan_index,
      work_since_interrupt
    ) || frames[0].kind != PARAMS_NODE_COLLECTION) {
    return FALSE;
  }

  R_xlen_t depth = 1;
  while (depth != 0) {
    collection_frame_t *parent = &frames[depth - 1];
    if (parent->kind == PARAMS_NODE_SET ||
        parent->next_child == parent->child_count) {
      if (parent->kind == PARAMS_NODE_COLLECTION &&
          parent->consumed_rows != parent->params_state.params.row_count) {
        return FALSE;
      }
      --depth;
      continue;
    }

    const R_xlen_t child_index = parent->next_child;
    SEXP child_self = PROTECT(VECTOR_ELT(parent->sets, child_index));
    for (R_xlen_t ancestor = 0; ancestor < depth; ++ancestor) {
      paradox_domain_account_work(work_since_interrupt);
      if (frames[ancestor].self == child_self) {
        Rf_error("Cyclic ParamSetCollection graph is unsupported");
      }
    }
    SEXP child_private = PROTECT(owned_private_environment(child_self));
    if (child_private == R_UnboundValue) {
      UNPROTECT(2);
      return FALSE;
    }

    if (depth == capacity) {
      if (capacity > R_XLEN_T_MAX / 2) {
        UNPROTECT(2);
        return FALSE;
      }
      const R_xlen_t expanded_capacity = capacity * 2;
      collection_frame_t *expanded = paradox_temporary_alloc(
        expanded_capacity,
        sizeof(*expanded)
      );
      memcpy(expanded, frames, (size_t) depth * sizeof(*expanded));
      frames = expanded;
      capacity = expanded_capacity;
      parent = &frames[depth - 1];
    }
    const int valid_child = initialize_collection_frame(
        child_self,
        child_private,
        &frames[depth],
        root_plan,
        root_plan_index,
        work_since_interrupt
      ) && validate_parent_child(
        parent,
        &frames[depth],
        child_index,
        work_since_interrupt
      );
    UNPROTECT(2);
    if (!valid_child) {
      return FALSE;
    }
    ++parent->next_child;
    ++depth;
  }
  *frames_out = frames;
  return TRUE;
}

static void evaluate_public_member_into(SEXP state, R_xlen_t index,
    SEXP self, const char *name) {
  SEXP call = PROTECT(Rf_lang3(
    R_DollarSymbol,
    self,
    Rf_install(name)
  ));
  SEXP result = PROTECT(Rf_eval(call, R_BaseEnv));
  SET_VECTOR_ELT(state, index, result);
  UNPROTECT(2);
}

SEXP paradox_param_set_collection_params(SEXP private_environment, SEXP self) {
  R_xlen_t work_since_interrupt = 0;
  collection_frame_t *frames;
  PROTECT_INDEX root_plan_index;
  SEXP root_plan;
  PROTECT_WITH_INDEX(root_plan = R_NilValue, &root_plan_index);
  if (!validate_collection_graph(
      self,
      private_environment,
      &frames,
      &root_plan,
      root_plan_index,
      &work_since_interrupt
    )) {
    UNPROTECT(1);
    return R_NilValue;
  }

  SEXP protected_state = PROTECT(Rf_allocVector(VECSXP, 4));
  SEXP result = PROTECT(paradox_params_build_static(
    &frames[0].params_state,
    &work_since_interrupt
  ));
  SET_VECTOR_ELT(protected_state, 0, result);
  /* A callback may rebind the private `.params` binding and collect its old
   * value.  Root the admitted source for the lifetime of every parsed pointer,
   * while using the already-owned result IDs for post-callback joins so an
   * in-place mutation cannot alter the pre-callback snapshot semantics. */
  SET_VECTOR_ELT(
    protected_state,
    1,
    frames[0].params_state.params_sexp
  );
  UNPROTECT(1);

  /* This is the frozen public order: aggregate every dependency first, then
   * every delegated value.  The state vector roots the static table and the
   * first callback result across the second allocating evaluation. */
  evaluate_public_member_into(protected_state, 2, self, "deps");
  evaluate_public_member_into(protected_state, 3, self, "values");
  result = VECTOR_ELT(protected_state, 0);
  paradox_domain_params_t snapshot_params = frames[0].params_state.params;
  snapshot_params.table = result;
  snapshot_params.ids = VECTOR_ELT(result, PARADOX_DOMAIN_ID);
  if (!paradox_params_finish_dynamic(
      result,
      &snapshot_params,
      VECTOR_ELT(protected_state, 2),
      VECTOR_ELT(protected_state, 3),
      &work_since_interrupt
    )) {
    Rf_error(
      "ParamSetCollection state changed while evaluating the params binding"
    );
  }
  UNPROTECT(2);
  return result;
}
