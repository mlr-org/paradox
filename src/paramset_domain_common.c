#include <limits.h>
#include <string.h>

#include "paramset_domain_common.h"
#include <R_ext/Utils.h>

#include "builtin_condition.h"
#include "domain_admission.h"
#include "paramset_params_internal.h"
#include "r_api_compat.h"
#include "r_utils.h"
#include "core_state.h"

const char *const paradox_domain_column_names[PARADOX_DOMAIN_COLUMN_COUNT] = {
  "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
  "levels", "special_vals", "default", "storage_type", ".tags",
  ".trafo", ".requirements", ".init_given", ".init"
};

const SEXPTYPE paradox_domain_column_types[PARADOX_DOMAIN_COLUMN_COUNT] = {
  STRSXP, STRSXP, STRSXP, VECSXP, REALSXP, REALSXP, REALSXP, VECSXP,
  VECSXP, VECSXP, STRSXP, VECSXP, VECSXP, VECSXP, LGLSXP, VECSXP
};

/*
 * Byte comparison against a constant ASCII label. Every caller compares
 * against a member of a closed, package-owned ASCII name set -- column names,
 * built-in class and storage names, cargo keys -- where the stored bytes
 * decide identity and no translation can change the answer. A `bytes`-encoded
 * spelling of such a name is therefore accepted here, deliberately unlike
 * `paradox_domain_strings_equal()` below, which decides semantic equality of
 * arbitrary user strings and never equates a `bytes` string with any other
 * encoding. The asymmetry is the difference between the two questions, not an
 * inconsistency.
 */
int paradox_domain_string_is(SEXP string, const char *expected) {
  return string != NA_STRING && strcmp(CHAR(string), expected) == 0;
}

/* Interned CHARSXPs for the sixteen canonical column names. R's global
 * CHARSXP cache stores one object per exact string, so a canonically built
 * table name is pointer-identical to its entry here; the byte comparison in
 * the selector below only runs for names this table does not know. */
static SEXP interned_domain_column_names[PARADOX_DOMAIN_COLUMN_COUNT];
static SEXP interned_domain_selfref_symbol;
static SEXP interned_domain_repr_symbol;

/* data.table's by-reference caches. They are not part of any Domain
 * representation, but ordinary filtering or keying installs them on a live
 * Domain, so the rejection below can name them. */
static const char *const domain_data_table_cache_tags[] = {"index", "sorted"};
#define PARADOX_DOMAIN_CACHE_TAG_COUNT 2
static SEXP interned_domain_cache_symbols[PARADOX_DOMAIN_CACHE_TAG_COUNT];

void paradox_domain_intern_column_names(void) {
  for (int column = 0; column < PARADOX_DOMAIN_COLUMN_COUNT; ++column) {
    SEXP name = Rf_mkChar(paradox_domain_column_names[column]);
    R_PreserveObject(name);
    interned_domain_column_names[column] = name;
  }
  interned_domain_selfref_symbol = Rf_install(".internal.selfref");
  interned_domain_repr_symbol = Rf_install("repr");
  for (int tag = 0; tag < PARADOX_DOMAIN_CACHE_TAG_COUNT; ++tag) {
    interned_domain_cache_symbols[tag] =
      Rf_install(domain_data_table_cache_tags[tag]);
  }
}

void paradox_domain_reject_outer_metadata(SEXP domain,
    const paradox_domain_outer_metadata_t *metadata) {
  /* The capture's five-cell bound aborts before reporting a sixth tag, so the
   * cache tags are also probed directly. This runs only once admission has
   * already decided to reject, and never widens what is admitted. */
  for (int tag = 0; tag < PARADOX_DOMAIN_CACHE_TAG_COUNT; ++tag) {
    int present = FALSE;
    if (metadata->unsupported_tag == interned_domain_cache_symbols[tag] ||
        (paradox_bounded_metadata_has_tag(
          domain,
          interned_domain_cache_symbols[tag],
          &present
        ) && present)) {
      Rf_error(
        "Corrupt Domain storage: `Domain` carries the data.table `%s` cache "
        "attribute; remove it with `data.table::setattr(x, \"%s\", NULL)` or "
        "rebuild the Domain",
        domain_data_table_cache_tags[tag],
        domain_data_table_cache_tags[tag]
      );
    }
  }
  Rf_error(
    "Corrupt Domain storage: outer metadata must be ordinary and bounded"
  );
}

SEXP paradox_domain_selfref_symbol(void) {
  return interned_domain_selfref_symbol;
}

static void capture_domain_outer_attribute(SEXP tag, SEXP value, void *data) {
  paradox_domain_outer_metadata_t *metadata = data;
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
  } else if (tag == interned_domain_selfref_symbol) {
    destination = &metadata->selfref;
  } else if (tag == interned_domain_repr_symbol) {
    destination = &metadata->repr;
  } else {
    if (metadata->unsupported_tag == R_NilValue) {
      metadata->unsupported_tag = tag;
    }
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
 * The printable `repr` carrier is metadata no Domain rule interprets: its
 * content stays opaque to every operation. Its shape is nevertheless part of
 * the public table contract, because an ALTREP or S4 carrier can dispatch
 * arbitrary R from an attribute read and this capture is allocation- and
 * callback-free by construction. One spelling of that rule serves every
 * boundary admitting a public Domain: a boundary that tolerates a carrier only
 * because its adapter never reads it still admits tables the constructor
 * rejects.
 */
int paradox_domain_repr_carrier_is_ordinary(SEXP repr) {
  return repr == R_NilValue || (!ALTREP(repr) && !Rf_isS4(repr));
}

int paradox_domain_capture_outer_metadata(SEXP domain,
    paradox_domain_outer_metadata_t *metadata) {
  *metadata = (paradox_domain_outer_metadata_t) {
    R_NilValue,
    R_NilValue,
    R_NilValue,
    R_NilValue,
    R_NilValue,
    R_NilValue,
    0,
    TRUE
  };
  R_xlen_t count = 0;
  if (!paradox_api_map_bounded_stored_attributes(
      domain,
      5,
      capture_domain_outer_attribute,
      metadata,
      &count
    ) || !metadata->valid || metadata->count != count ||
      metadata->names == R_NilValue ||
      metadata->classes == R_NilValue ||
      metadata->row_names == R_NilValue ||
      !paradox_domain_repr_carrier_is_ordinary(metadata->repr)) {
    return FALSE;
  }
  const R_xlen_t expected = 3 +
    (metadata->selfref != R_NilValue) +
    (metadata->repr != R_NilValue);
  return count == expected;
}

void paradox_domain_select_captured_columns_with_positions(SEXP table,
    SEXP names, const char *corrupt_context, const char *storage_name,
    unsigned int required_mask, SEXP *columns, R_xlen_t *positions) {
  if (TYPEOF(table) != VECSXP || Rf_isS4(table)) {
    Rf_error("Corrupt %s: `%s` must be a list", corrupt_context, storage_name);
  }
  if (ALTREP(table)) {
    Rf_error(
      "Corrupt %s: `%s` must use an ordinary list representation",
      corrupt_context,
      storage_name
    );
  }

  const R_xlen_t n_columns = XLENGTH(table);
  if (TYPEOF(names) != STRSXP || Rf_isS4(names) ||
      Rf_isObject(names) || !paradox_api_has_no_attributes(names)) {
    Rf_error(
      "Corrupt %s: `%s` must be a named list",
      corrupt_context,
      storage_name
    );
  }
  if (ALTREP(names)) {
    Rf_error(
      "Corrupt %s: `%s` names must use an ordinary character representation",
      corrupt_context,
      storage_name
    );
  }
  if (XLENGTH(names) != n_columns) {
    Rf_error(
      "Corrupt %s: `%s` must be a named list",
      corrupt_context,
      storage_name
    );
  }

  int canonical_layout =
    (n_columns == (R_xlen_t) PARADOX_DOMAIN_COLUMN_COUNT ||
     n_columns == PARADOX_DOMAIN_PERMANENT_COLUMNS) &&
    (required_mask >> (unsigned int) n_columns) == 0U;
  const SEXP *name_values = canonical_layout
    ? STRING_PTR_RO(names)
    : NULL;
  for (int target = 0; target < PARADOX_DOMAIN_COLUMN_COUNT; ++target) {
    columns[target] = R_NilValue;
    if (positions != NULL) positions[target] = R_XLEN_T_MAX;
    if (canonical_layout && (R_xlen_t) target < n_columns &&
        name_values[target] != interned_domain_column_names[target]) {
      canonical_layout = FALSE;
    }
  }
  /*
   * Canonical Domain and private parameter tables prove requested names present
   * and unique in one pointer-only pass. This is also the pre-value-callback
   * fast path for the Domain shape probe: malformed, reordered, or foreign
   * layouts retain the complete diagnostic selector below.
   */
  if (canonical_layout) {
    const SEXP *table_values = (const SEXP *) DATAPTR_RO(table);
    for (int target = 0; target < PARADOX_DOMAIN_COLUMN_COUNT; ++target) {
      if ((required_mask >> target) & 1U) {
        columns[target] = table_values[target];
        if (positions != NULL) positions[target] = (R_xlen_t) target;
      }
    }
    return;
  }

  int counts[PARADOX_DOMAIN_COLUMN_COUNT] = {0};
  for (R_xlen_t index = 0; index < n_columns; ++index) {
    SEXP name = STRING_ELT(names, index);
    if (name == NA_STRING) {
      continue;
    }
    int matched =
      index < (R_xlen_t) PARADOX_DOMAIN_COLUMN_COUNT &&
        name == interned_domain_column_names[index]
        ? (int) index
        : -1;
    if (matched < 0) {
      for (int target = 0; target < PARADOX_DOMAIN_COLUMN_COUNT; ++target) {
        if (name == interned_domain_column_names[target]) {
          matched = target;
          break;
        }
      }
    }
    if (matched < 0) {
      /* A name that is not the interned object may still spell a canonical
       * name in a foreign representation; bytes decide, exactly as the
       * single-column selector always has. */
      for (int target = 0; target < PARADOX_DOMAIN_COLUMN_COUNT; ++target) {
        if (strcmp(CHAR(name), paradox_domain_column_names[target]) == 0) {
          matched = target;
          break;
        }
      }
    }
    if (matched < 0 || !((required_mask >> matched) & 1U)) {
      continue;
    }
    columns[matched] = VECTOR_ELT(table, index);
    if (positions != NULL) positions[matched] = index;
    ++counts[matched];
  }
  for (int target = 0; target < PARADOX_DOMAIN_COLUMN_COUNT; ++target) {
    if (!((required_mask >> target) & 1U) || counts[target] == 1) {
      continue;
    }
    if (counts[target] > 1) {
      Rf_error(
        "Corrupt %s: `%s` has more than one `%s` column",
        corrupt_context,
        storage_name,
        paradox_domain_column_names[target]
      );
    }
    Rf_error(
      "Corrupt %s: `%s` has no `%s` column",
      corrupt_context,
      storage_name,
      paradox_domain_column_names[target]
    );
  }
}

void paradox_domain_select_columns_with_positions(SEXP table,
    const char *corrupt_context, const char *storage_name,
    unsigned int required_mask, SEXP *columns, R_xlen_t *positions) {
  if (TYPEOF(table) != VECSXP || Rf_isS4(table)) {
    Rf_error("Corrupt %s: `%s` must be a list", corrupt_context, storage_name);
  }
  if (ALTREP(table)) {
    Rf_error(
      "Corrupt %s: `%s` must use an ordinary list representation",
      corrupt_context,
      storage_name
    );
  }
  /* Bound the caller-owned spine before the allocation-free raw selector.
   * Captured-generation callers use the companion above and do not pay this
   * second traversal. */
  int has_names = FALSE;
  if (!paradox_bounded_metadata_has_tag(
      table,
      R_NamesSymbol,
      &has_names
    ) || !has_names) {
    Rf_error(
      "Corrupt %s: `%s` must be a named list",
      corrupt_context,
      storage_name
    );
  }
  SEXP names = PROTECT(paradox_api_raw_attribute(table, R_NamesSymbol));
  paradox_domain_select_captured_columns_with_positions(
    table,
    names,
    corrupt_context,
    storage_name,
    required_mask,
    columns,
    positions
  );
  UNPROTECT(1);
}

void paradox_domain_select_columns(SEXP table, const char *corrupt_context,
    const char *storage_name, unsigned int required_mask, SEXP *columns) {
  paradox_domain_select_columns_with_positions(
    table,
    corrupt_context,
    storage_name,
    required_mask,
    columns,
    NULL
  );
}

int paradox_domain_column_shell_is_exact(SEXP column,
    enum paradox_domain_column selected, R_xlen_t row_count) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(column);
  const int numeric = selected == PARADOX_DOMAIN_LOWER ||
    selected == PARADOX_DOMAIN_UPPER ||
    selected == PARADOX_DOMAIN_TOLERANCE;
  return (numeric ? type == INTSXP || type == REALSXP
      : type == paradox_domain_column_types[selected]) &&
    !ALTREP(column) && !Rf_isS4(column) &&
    !Rf_isObject(column) && paradox_api_has_no_attributes(column) &&
    XLENGTH(column) == row_count;
}

int paradox_domain_captured_columns_current(SEXP table, SEXP names,
    const SEXP *columns, const R_xlen_t *positions, R_xlen_t row_count) {
  if (TYPEOF(table) != VECSXP || ALTREP(table) || Rf_isS4(table) ||
      XLENGTH(table) != PARADOX_DOMAIN_COLUMN_COUNT) {
    return FALSE;
  }
  if (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
      Rf_isObject(names) || !paradox_api_has_no_attributes(names) ||
      XLENGTH(names) != PARADOX_DOMAIN_COLUMN_COUNT) {
    return FALSE;
  }
  const SEXP *table_values = (const SEXP *) DATAPTR_RO(table);
  const SEXP *name_values = STRING_PTR_RO(names);
  for (int target = 0; target < PARADOX_DOMAIN_COLUMN_COUNT; ++target) {
    const R_xlen_t position = positions[target];
    if (position < 0 || position >= PARADOX_DOMAIN_COLUMN_COUNT) {
      return FALSE;
    }
    SEXP name = name_values[position];
    SEXP column = table_values[position];
    if ((name != interned_domain_column_names[target] &&
        (name == NA_STRING ||
          strcmp(CHAR(name), paradox_domain_column_names[target]) != 0)) ||
        column != columns[target] ||
        !paradox_domain_column_shell_is_exact(
          column,
          (enum paradox_domain_column) target,
          row_count
        )) {
      return FALSE;
    }
  }
  return TRUE;
}

int paradox_domain_selected_columns_current(SEXP table, const SEXP *columns,
    const R_xlen_t *positions, R_xlen_t row_count) {
  int has_names = FALSE;
  if (!paradox_bounded_metadata_has_tag(
      table,
      R_NamesSymbol,
      &has_names
    ) || !has_names) {
    return FALSE;
  }
  SEXP names = paradox_api_raw_attribute(table, R_NamesSymbol);
  return paradox_domain_captured_columns_current(
    table,
    names,
    columns,
    positions,
    row_count
  );
}

SEXP paradox_domain_character_vector(const char *const *values,
    R_xlen_t size) {
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  for (R_xlen_t index = 0; index < size; ++index) {
    SET_STRING_ELT(result, index, Rf_mkChar(values[index]));
  }
  UNPROTECT(1);
  return result;
}

SEXP paradox_domain_finish_plain_table(SEXP table,
    const char *const *column_names, R_xlen_t column_count,
    R_xlen_t row_count) {
  if (row_count > INT_MAX) {
    Rf_error("ParamSet state table exceeds the supported row count");
  }
  PROTECT(table);
  SEXP names = PROTECT(paradox_domain_character_vector(
    column_names,
    column_count
  ));
  Rf_setAttrib(table, R_NamesSymbol, names);
  SEXP classes = PROTECT(Rf_mkString("data.frame"));
  Rf_setAttrib(table, R_ClassSymbol, classes);
  SEXP row_names = PROTECT(Rf_allocVector(
    INTSXP,
    row_count == 0 ? 0 : 2
  ));
  if (row_count != 0) {
    SET_INTEGER_ELT(row_names, 0, NA_INTEGER);
    /* Negate in R_xlen_t after the INT_MAX bound above, then narrow the
     * representable result.  This also makes the overflow proof visible to
     * conservative integer-flow analyzers. */
    SET_INTEGER_ELT(row_names, 1, (int) (-row_count));
  }
  Rf_setAttrib(table, R_RowNamesSymbol, row_names);
  UNPROTECT(4);
  return table;
}

SEXP paradox_domain_new_plain_table(const char *const *column_names,
    const SEXPTYPE *column_types, R_xlen_t column_count,
    R_xlen_t row_count) {
  SEXP table = PROTECT(Rf_allocVector(VECSXP, column_count));
  for (R_xlen_t column = 0; column < column_count; ++column) {
    SEXP values = PROTECT(Rf_allocVector(
      column_types[column],
      row_count
    ));
    SET_VECTOR_ELT(table, column, values);
    UNPROTECT(1);
  }
  SEXP result = PROTECT(paradox_domain_finish_plain_table(
    table,
    column_names,
    column_count,
    row_count
  ));
  UNPROTECT(2);
  return result;
}

static int has_no_attributes(SEXP value) {
  return paradox_api_has_no_attributes(value);
}

static char *copy_utf8_string(SEXP string) {
  return paradox_temporary_utf8_copy(string, NULL);
}

static int native_ascii_strings_equal(SEXP left, SEXP right, int *known) {
  const unsigned char *left_text = (const unsigned char *) CHAR(left);
  const unsigned char *right_text = (const unsigned char *) CHAR(right);
  for (;;) {
    const unsigned char left_byte = *left_text;
    const unsigned char right_byte = *right_text;
    if ((left_byte | right_byte) >= 0x80U) {
      *known = FALSE;
      return FALSE;
    }
    if (left_byte != right_byte) {
      *known = TRUE;
      return FALSE;
    }
    if (left_byte == '\0') {
      *known = TRUE;
      return TRUE;
    }
    ++left_text;
    ++right_text;
  }
}

/*
 * Semantic equality of two arbitrary user strings, such as parameter IDs and
 * factor groupings. A `bytes` string declares that its payload has no
 * character interpretation, so it equals only another `bytes` string with the
 * same payload. `paradox_domain_string_is()` above answers a different
 * question over a closed ASCII name set and deliberately does not share this
 * rule.
 */
int paradox_domain_strings_equal(SEXP left, SEXP right) {
  if (left == right) {
    return TRUE;
  }
  if (left == NA_STRING || right == NA_STRING) {
    return FALSE;
  }

  const cetype_t left_encoding = Rf_getCharCE(left);
  const cetype_t right_encoding = Rf_getCharCE(right);
  if (left_encoding == right_encoding) {
    if (left_encoding == CE_UTF8 || left_encoding == CE_LATIN1 ||
        left_encoding == CE_BYTES) {
      return strcmp(CHAR(left), CHAR(right)) == 0;
    }
    if (left_encoding == CE_NATIVE) {
      int known = FALSE;
      const int equal = native_ascii_strings_equal(left, right, &known);
      if (known) {
        return equal;
      }
    }
  }
  if (left_encoding == CE_BYTES || right_encoding == CE_BYTES) {
    return FALSE;
  }

  PROTECT(left);
  PROTECT(right);
  const void *vmax = vmaxget();
  const char *left_text = copy_utf8_string(left);
  const char *right_text = Rf_translateCharUTF8(right);
  const int equal = strcmp(left_text, right_text) == 0;
  vmaxset(vmax);
  UNPROTECT(2);
  return equal;
}

int paradox_domain_strings_have_duplicates(SEXP values) {
  const R_xlen_t size = XLENGTH(values);
  if (size < 2) return FALSE;
  if (size > 8) return Rf_any_duplicated(values, FALSE) != 0;

  /* No hash table is needed for the tiny ASCII level/tag/ID sets common in
   * search spaces. Compare contents as well as identity: this shortcut does
   * not depend on R's private CHARSXP-cache bits. Delegate the complete vector
   * to R whenever encoding or missingness makes its matching rules relevant.
   * This bounded branch allocates nothing and retains no pointer across the
   * delegated call; all observations here are on admitted ordinary strings. */
  SEXP strings[8];
  const char *text[8];
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP string = STRING_ELT(values, index);
    if (string == NA_STRING || Rf_getCharCE(string) != CE_NATIVE ||
        LENGTH(string) > 64) {
      return Rf_any_duplicated(values, FALSE) != 0;
    }
    strings[index] = string;
    text[index] = CHAR(string);
    const unsigned char *cursor = (const unsigned char *) text[index];
    while (*cursor != '\0') {
      if (*cursor >= 0x80U) return Rf_any_duplicated(values, FALSE) != 0;
      ++cursor;
    }
  }
  for (R_xlen_t index = 1; index < size; ++index) {
    for (R_xlen_t previous = 0; previous < index; ++previous) {
      if (strings[index] == strings[previous] ||
          strcmp(text[index], text[previous]) == 0) {
        return TRUE;
      }
    }
  }
  return FALSE;
}

R_xlen_t paradox_domain_find_string(SEXP strings, SEXP sought,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t count = XLENGTH(strings);
  for (R_xlen_t index = 0; index < count; ++index) {
    paradox_account_work(work_since_interrupt);
    SEXP candidate = STRING_ELT(strings, index);
    if (candidate == sought ||
        paradox_domain_strings_equal(candidate, sought)) {
      return index;
    }
  }
  return R_XLEN_T_MAX;
}

SEXP paradox_domain_apply_tag_override(SEXP derived, SEXP override,
    R_xlen_t *work_since_interrupt) {
  if (override == R_NilValue) {
    return derived;
  }
  PROTECT(derived);
  PROTECT(override);
  SEXP governed = VECTOR_ELT(override, PARADOX_TAG_OVERRIDE_IDS);
  SEXP asserted = VECTOR_ELT(override, PARADOX_TAG_OVERRIDE_TAGS);
  SEXP derived_ids = VECTOR_ELT(derived, 0);
  SEXP derived_tags = VECTOR_ELT(derived, 1);
  SEXP asserted_ids = VECTOR_ELT(asserted, 0);
  SEXP asserted_tags = VECTOR_ELT(asserted, 1);
  const R_xlen_t derived_rows = XLENGTH(derived_ids);
  const R_xlen_t asserted_rows = XLENGTH(asserted_ids);

  R_xlen_t kept = 0;
  for (R_xlen_t row = 0; row < derived_rows; ++row) {
    kept += !paradox_domain_string_in(
      governed,
      STRING_ELT(derived_ids, row),
      work_since_interrupt
    );
  }
  if (kept == derived_rows && asserted_rows == 0) {
    UNPROTECT(2);
    return derived;
  }
  if (kept > R_XLEN_T_MAX - asserted_rows) {
    UNPROTECT(2);
    Rf_error("ParamSet tag table exceeds the supported row count");
  }

  static const char *const columns[] = {"id", "tag"};
  static const SEXPTYPE types[] = {STRSXP, STRSXP};
  SEXP result = PROTECT(paradox_domain_new_plain_table(
    columns,
    types,
    2,
    kept + asserted_rows
  ));
  SEXP result_ids = VECTOR_ELT(result, 0);
  SEXP result_tags = VECTOR_ELT(result, 1);
  R_xlen_t output = 0;
  for (R_xlen_t row = 0; row < derived_rows; ++row) {
    paradox_account_work(work_since_interrupt);
    if (paradox_domain_string_in(
        governed,
        STRING_ELT(derived_ids, row),
        work_since_interrupt
      )) {
      continue;
    }
    SET_STRING_ELT(result_ids, output, STRING_ELT(derived_ids, row));
    SET_STRING_ELT(result_tags, output, STRING_ELT(derived_tags, row));
    ++output;
  }
  for (R_xlen_t row = 0; row < asserted_rows; ++row) {
    paradox_account_work(work_since_interrupt);
    SET_STRING_ELT(result_ids, output, STRING_ELT(asserted_ids, row));
    SET_STRING_ELT(result_tags, output, STRING_ELT(asserted_tags, row));
    ++output;
  }
  UNPROTECT(3);
  return result;
}

int paradox_domain_string_in(SEXP strings, SEXP sought,
    R_xlen_t *work_since_interrupt) {
  return paradox_domain_find_string(
    strings,
    sought,
    work_since_interrupt
  ) != R_XLEN_T_MAX;
}

int paradox_domain_exact_string_vector(SEXP value,
    const char *const *expected, R_xlen_t size,
    R_xlen_t *work_since_interrupt) {
  if (TYPEOF(value) != STRSXP || ALTREP(value) || Rf_isS4(value) ||
      Rf_isObject(value) || !has_no_attributes(value)) {
    return FALSE;
  }
  const R_xlen_t observed_size = XLENGTH(value);
  if (observed_size != size) {
    return FALSE;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_account_work(work_since_interrupt);
    if (!paradox_domain_string_is(STRING_ELT(value, index), expected[index])) {
      return FALSE;
    }
  }
  return TRUE;
}

typedef struct {
  SEXP names;
  SEXP classes;
  SEXP row_names;
  int valid;
} exact_plain_table_attributes_t;

static void capture_exact_plain_table_attribute(
    SEXP tag, SEXP value, void *data) {
  exact_plain_table_attributes_t *attributes = data;
  if (!attributes->valid || TYPEOF(tag) != SYMSXP ||
      value == R_NilValue) {
    attributes->valid = FALSE;
    return;
  }
  SEXP *destination = NULL;
  if (tag == R_NamesSymbol) {
    destination = &attributes->names;
  } else if (tag == R_ClassSymbol) {
    destination = &attributes->classes;
  } else if (tag == R_RowNamesSymbol) {
    destination = &attributes->row_names;
  } else {
    attributes->valid = FALSE;
    return;
  }
  if (*destination != R_NilValue) {
    attributes->valid = FALSE;
    return;
  }
  *destination = value;
}

static int capture_exact_plain_table_attributes(
    SEXP table, exact_plain_table_attributes_t *attributes) {
  *attributes = (exact_plain_table_attributes_t) {
    R_NilValue,
    R_NilValue,
    R_NilValue,
    TRUE
  };
  R_xlen_t count = 0;
  return paradox_api_map_bounded_stored_attributes(
      table,
      3,
      capture_exact_plain_table_attribute,
      attributes,
      &count
    ) && attributes->valid && count == 3 &&
    attributes->names != R_NilValue &&
    attributes->classes != R_NilValue &&
    attributes->row_names != R_NilValue;
}

int paradox_domain_exact_plain_table(SEXP table,
    const char *const *column_names, R_xlen_t column_count,
    R_xlen_t *row_count, R_xlen_t *work_since_interrupt) {
  static const char *const table_classes[] = {"data.frame"};
  if (TYPEOF(table) != VECSXP || ALTREP(table) || Rf_isS4(table)) {
    return FALSE;
  }
  PROTECT(table);
  const R_xlen_t observed_column_count = XLENGTH(table);
  /*
   * Prove and select the complete three-cell metadata generation in the same
   * hard-bounded pass. No cyclic or overlong pre-4.6 pairlist can reach an
   * unbounded compatibility selector, and the ordinary path avoids a second
   * attribute-spine scan.
   */
  exact_plain_table_attributes_t attributes;
  if (!capture_exact_plain_table_attributes(table, &attributes)) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP names = PROTECT(attributes.names);
  SEXP classes = PROTECT(attributes.classes);
  SEXP row_names = PROTECT(attributes.row_names);
  R_xlen_t observed_row_count = 0;
  int valid = observed_column_count == column_count &&
    !Rf_isS4(names) && !Rf_isObject(names) &&
    !Rf_isS4(classes) && !Rf_isObject(classes) &&
    !Rf_isS4(row_names) && !Rf_isObject(row_names);
  if (valid && column_count != 0) {
    /*
     * Column zero defines the row count, but it is subject to the same strict
     * representation contract as every later column.  Reject it before
     * observing XLENGTH: an ALTREP Length method may execute arbitrary R.
     */
    SEXP first = VECTOR_ELT(table, 0);
    valid = !ALTREP(first) && !Rf_isS4(first) && !Rf_isObject(first);
    if (valid) observed_row_count = XLENGTH(first);
  }
  valid = valid &&
    has_no_attributes(names) &&
    has_no_attributes(classes) &&
    paradox_domain_exact_string_vector(
      names,
      column_names,
      column_count,
      work_since_interrupt
    ) &&
    paradox_domain_exact_string_vector(
      classes,
      table_classes,
      1,
      work_since_interrupt
    ) && TYPEOF(row_names) == INTSXP && !ALTREP(row_names) &&
    paradox_api_has_no_attributes(row_names);
  if (valid) {
    /*
     * Read the stored attribute above, not Rf_getAttrib()'s expanded `1:n`
     * view.  Existing package producers use both exact ordinary `1:n` row
     * names and R's compact c(NA, +/-n) representation.  Reject a
     * callback-capable row-name ALTREP before observing either Length or Elt;
     * compact forms then remain constant-time while the explicit form pays
     * the same exact validation it did before this raw-attribute reader.
     */
    const R_xlen_t row_name_count = XLENGTH(row_names);
    if (observed_row_count == 0) {
      valid = row_name_count == 0;
    } else if (observed_row_count > INT_MAX) {
      valid = FALSE;
    } else if (row_name_count == 2 &&
        INTEGER_ELT(row_names, 0) == NA_INTEGER) {
      const int encoded = INTEGER_ELT(row_names, 1);
      const int expected = (int) observed_row_count;
      valid = encoded == expected || encoded == -expected;
    } else if (row_name_count == observed_row_count) {
      for (R_xlen_t row = 0; valid && row < observed_row_count; ++row) {
        paradox_account_work(work_since_interrupt);
        valid = INTEGER_ELT(row_names, row) == (int) row + 1;
      }
    } else {
      valid = FALSE;
    }
  }
  /* An exact plain capsule table is rectangular and ordinary. Column zero
   * defined the row count above; admitting a table whose remaining columns
   * are shorter would let every consumer index them out of bounds. */
  for (R_xlen_t column = 1; valid && column < column_count; ++column) {
    SEXP child = VECTOR_ELT(table, column);
    if (ALTREP(child) || Rf_isS4(child) || Rf_isObject(child) ||
        XLENGTH(child) != observed_row_count) {
      valid = FALSE;
    }
  }
  if (valid && row_count != NULL) {
    *row_count = observed_row_count;
  }
  UNPROTECT(4);
  return valid;
}


SEXP paradox_domain_local_value(SEXP environment, const char *name) {
  if (TYPEOF(environment) != ENVSXP || Rf_isS4(environment)) {
    return R_UnboundValue;
  }
  /* Current state has exactly one authority. Retired private-table bindings
   * are neither inspected nor used as a compatibility path. */
  return paradox_core_local_value(environment, name);
}

int paradox_domain_owns_private_environment(SEXP self,
    SEXP private_environment) {
  if (TYPEOF(self) != ENVSXP || Rf_isS4(self) ||
      TYPEOF(private_environment) != ENVSXP ||
      Rf_isS4(private_environment)) {
    return FALSE;
  }
#if R_VERSION < R_Version(4, 2, 0)
  /*
   * Old R's required ordinary-binding boundary has no allocation-free
   * existence query and deliberately assumes an admitted package shell.
   * Reject a plain malformed environment by its constant-time object bit
   * before that reader; current runtimes keep their existing hot path.
   */
  if (!Rf_isObject(self)) {
    return FALSE;
  }
#endif
  /* Current package-generated shells are authorized by their sealed capsule,
   * not by replaying R6's generated closure/private-environment topology.
   * Registered entry points receive `self` and `private` from thin package
   * wrappers; direct calls with a different environment remain unsupported,
   * while every payload field is still structurally validated before use. */
  SEXP owned_private = PROTECT(
    paradox_domain_required_private_environment(self)
  );
  const int owns = owned_private != R_UnboundValue &&
    owned_private == private_environment;
  UNPROTECT(1);
  return owns;
}

/*
 * Generate the required operation reader and the absence-tolerant candidate
 * reader from one source template.  This is deliberately compile-time rather
 * than a function-pointer or run-time-flag abstraction: on R < 4.2 the
 * optional reader must call base::exists(), while a known package shell must
 * stay on the allocation-free required-binding API.  The two readers otherwise
 * retain exactly the same structural admission.
 */
#define PARADOX_DEFINE_PRIVATE_ENVIRONMENT_READER( \
    name, binding_snapshot, core_snapshot) \
  static SEXP name(SEXP self) { \
    if (TYPEOF(self) != ENVSXP || Rf_isS4(self)) { \
      return R_UnboundValue; \
    } \
    SEXP enclosure_symbol = Rf_install(".__enclos_env__"); \
    SEXP enclosure = PROTECT(binding_snapshot(self, enclosure_symbol)); \
    if (TYPEOF(enclosure) != ENVSXP || Rf_isS4(enclosure)) { \
      UNPROTECT(1); \
      return R_UnboundValue; \
    } \
    SEXP private_symbol = Rf_install("private"); \
    SEXP private_environment = PROTECT( \
      binding_snapshot(enclosure, private_symbol) \
    ); \
    SEXP result = TYPEOF(private_environment) == ENVSXP && \
        !Rf_isS4(private_environment) && \
        core_snapshot(private_environment) != R_UnboundValue \
      ? private_environment \
      : R_UnboundValue; \
    UNPROTECT(2); \
    return result; \
  }

PARADOX_DEFINE_PRIVATE_ENVIRONMENT_READER(
  required_private_environment,
  paradox_api_plain_binding_snapshot,
  paradox_core_from_private
)

PARADOX_DEFINE_PRIVATE_ENVIRONMENT_READER(
  optional_private_environment,
  paradox_api_optional_plain_binding_snapshot,
  paradox_core_from_private_optional
)

#undef PARADOX_DEFINE_PRIVATE_ENVIRONMENT_READER

SEXP paradox_domain_required_private_environment(SEXP self) {
  return required_private_environment(self);
}

SEXP paradox_domain_private_environment(SEXP self) {
  return optional_private_environment(self);
}

int paradox_domain_plain_integer_bound(double value) {
  if (!R_FINITE(value)) {
    return TRUE;
  }
  if (value < -(double) INT_MAX || value > (double) INT_MAX) {
    return FALSE;
  }
  return value == (double) ((int) value);
}

int paradox_domain_numeric_capsule_is_canonical(int integer_kind,
    double lower, double upper, double tolerance) {
  /* `R_FINITE()` already refuses a missing or NaN tolerance; the bound
   * comparison below is false for either NaN operand, so both bounds are
   * tested for missingness explicitly. */
  if (ISNAN(lower) || ISNAN(upper) || !R_FINITE(tolerance) ||
      tolerance < 0.0 || lower > upper) {
    return FALSE;
  }
  return !integer_kind ||
    (tolerance <= 0.5 &&
      paradox_domain_plain_integer_bound(lower) &&
      paradox_domain_plain_integer_bound(upper));
}

static uint64_t hash_id_bytes(const unsigned char *text, uint64_t hash) {
  while (*text != '\0') {
    hash ^= (uint64_t) *text;
    hash *= UINT64_C(1099511628211);
    ++text;
  }
  return hash;
}

/* Identifier equality is the encoding-aware comparator below, so the hash has
 * to agree with it: every translatable spelling hashes its UTF-8 bytes, and a
 * bytes-encoded identifier -- which that comparator never equates with a
 * non-bytes one -- is separated by a distinct prefix. */
static uint64_t hash_id_string(SEXP string) {
  uint64_t hash = UINT64_C(14695981039346656037);
  if (Rf_getCharCE(string) == CE_BYTES) {
    hash ^= UINT64_C(0xff);
    hash *= UINT64_C(1099511628211);
    return hash_id_bytes((const unsigned char *) CHAR(string), hash);
  }
  PROTECT(string);
  const void *vmax = vmaxget();
  const char *text = Rf_translateCharUTF8(string);
  hash = hash_id_bytes((const unsigned char *) text, hash);
  vmaxset(vmax);
  UNPROTECT(1);
  return hash;
}

paradox_domain_id_map_status_t paradox_domain_id_map_init(SEXP ids,
    paradox_domain_id_map_t *map) {
  const R_xlen_t size = XLENGTH(ids);
  if (size > R_XLEN_T_MAX / 2) {
    return PARADOX_DOMAIN_ID_MAP_TOO_MANY;
  }
  R_xlen_t capacity = 1;
  const R_xlen_t needed = size == 0 ? 1 : size * 2;
  while (capacity < needed) {
    if (capacity > R_XLEN_T_MAX / 2) {
      return PARADOX_DOMAIN_ID_MAP_CAPACITY;
    }
    capacity *= 2;
  }
  paradox_domain_id_slot_t *slots =
    paradox_temporary_alloc(capacity, sizeof(*slots));
  memset(slots, 0, (size_t) capacity * sizeof(*slots));
  const R_xlen_t mask = capacity - 1;
  R_xlen_t work_since_interrupt = 0;
  for (R_xlen_t row = 0; row < size; ++row) {
    paradox_account_work(&work_since_interrupt);
    SEXP id = STRING_ELT(ids, row);
    const uint64_t hash = hash_id_string(id);
    R_xlen_t slot = (R_xlen_t) (hash & (uint64_t) mask);
    while (slots[slot].row_plus_one != 0) {
      paradox_account_work(&work_since_interrupt);
      const R_xlen_t present = slots[slot].row_plus_one - 1;
      if (slots[slot].hash == hash && paradox_domain_strings_equal(
          STRING_ELT(ids, present), id
        )) {
        return PARADOX_DOMAIN_ID_MAP_DUPLICATE;
      }
      slot = (slot + 1) & mask;
    }
    slots[slot].hash = hash;
    slots[slot].row_plus_one = row + 1;
  }
  map->slots = slots;
  map->capacity = capacity;
  map->ids = ids;
  return PARADOX_DOMAIN_ID_MAP_OK;
}

int paradox_domain_id_map_find(const paradox_domain_id_map_t *map, SEXP id,
    R_xlen_t *row, R_xlen_t *work_since_interrupt) {
  /* An early exit, not a rule: the comparator below refuses every pairing
   * involving a missing identifier, and admitted `id` columns carry none. */
  if (id == NA_STRING) return FALSE;
  const uint64_t hash = hash_id_string(id);
  const R_xlen_t mask = map->capacity - 1;
  R_xlen_t slot = (R_xlen_t) (hash & (uint64_t) mask);
  while (map->slots[slot].row_plus_one != 0) {
    paradox_account_work(work_since_interrupt);
    const R_xlen_t present = map->slots[slot].row_plus_one - 1;
    if (map->slots[slot].hash == hash && paradox_domain_strings_equal(
        STRING_ELT(map->ids, present), id
      )) {
      *row = present;
      return TRUE;
    }
    slot = (slot + 1) & mask;
  }
  return FALSE;
}

int paradox_domain_read_params(SEXP params, unsigned int columns,
    paradox_domain_params_t *result) {
  /* Private tables use fixed positions, not their printable metadata. This
   * allocation-free boundary proves only the accesses the consumer declares.
   * Constructors own semantic admission; readers do not repeat ID/level
   * uniqueness or class/storage consistency checks. Nested values are guarded
   * at their point of interpretation or by the outward detachment helper. */
  if (TYPEOF(params) != VECSXP || ALTREP(params) || Rf_isS4(params) ||
      XLENGTH(params) != PARADOX_DOMAIN_PERMANENT_COLUMNS) {
    return FALSE;
  }
  SEXP ids = VECTOR_ELT(params, PARADOX_DOMAIN_ID);
  if (TYPEOF(ids) != STRSXP || ALTREP(ids)) return FALSE;
  const R_xlen_t size = XLENGTH(ids);
  if (size > INT_MAX) return FALSE;
  columns |= 1U << PARADOX_DOMAIN_ID;
  for (int column = 0; column < PARADOX_DOMAIN_TAGS; ++column) {
    if (((columns >> column) & 1U) &&
        !paradox_domain_column_shell_is_exact(VECTOR_ELT(params, column),
          (enum paradox_domain_column) column, size)) {
      return FALSE;
    }
  }
  result->table = params;
  result->ids = ids;
  result->classes = (columns & (1U << PARADOX_DOMAIN_CLS))
    ? VECTOR_ELT(params, PARADOX_DOMAIN_CLS) : R_NilValue;
  result->row_count = size;
  return TRUE;
}

int paradox_domain_validate_tags(SEXP tags, paradox_domain_tags_t *result,
    R_xlen_t *work_since_interrupt) {
  static const char *const column_names[] = {"id", "tag"};
  PROTECT(tags);
  if (!paradox_domain_exact_plain_table(
      tags, column_names, 2, NULL, work_since_interrupt
    )) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP ids = PROTECT(VECTOR_ELT(tags, 0));
  SEXP values = PROTECT(VECTOR_ELT(tags, 1));
  if (TYPEOF(ids) != STRSXP || TYPEOF(values) != STRSXP || ALTREP(ids) ||
      ALTREP(values) || Rf_isS4(ids) || Rf_isObject(ids) ||
      Rf_isS4(values) || Rf_isObject(values) || !has_no_attributes(ids) ||
      !has_no_attributes(values)) {
    UNPROTECT(3);
    return FALSE;
  }
  const R_xlen_t row_count = XLENGTH(ids);
  const R_xlen_t value_count = XLENGTH(values);
  if (value_count != row_count) {
    UNPROTECT(3);
    return FALSE;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    if (STRING_ELT(ids, row) == NA_STRING ||
        STRING_ELT(values, row) == NA_STRING) {
      UNPROTECT(3);
      return FALSE;
    }
  }
  result->ids = ids;
  result->values = values;
  result->row_count = row_count;
  UNPROTECT(3);
  return TRUE;
}

int paradox_domain_validate_trafos(SEXP trafos,
    paradox_domain_trafos_t *result,
    R_xlen_t *work_since_interrupt) {
  static const char *const column_names[] = {"id", "trafo"};
  PROTECT(trafos);
  if (!paradox_domain_exact_plain_table(
      trafos, column_names, 2, NULL, work_since_interrupt
    )) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP ids = PROTECT(VECTOR_ELT(trafos, 0));
  SEXP values = PROTECT(VECTOR_ELT(trafos, 1));
  if (TYPEOF(ids) != STRSXP || TYPEOF(values) != VECSXP || ALTREP(ids) ||
      ALTREP(values) || Rf_isS4(ids) || Rf_isObject(ids) ||
      Rf_isS4(values) || Rf_isObject(values) || !has_no_attributes(ids) ||
      !has_no_attributes(values)) {
    UNPROTECT(3);
    return FALSE;
  }
  const R_xlen_t row_count = XLENGTH(ids);
  const R_xlen_t value_count = XLENGTH(values);
  if (value_count != row_count) {
    UNPROTECT(3);
    return FALSE;
  }
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    if (STRING_ELT(ids, row) == NA_STRING ||
        !Rf_isFunction(VECTOR_ELT(values, row))) {
      UNPROTECT(3);
      return FALSE;
    }
  }
  result->ids = ids;
  result->values = values;
  result->row_count = row_count;
  UNPROTECT(3);
  return TRUE;
}

int paradox_domain_read_dependencies(SEXP dependencies,
    paradox_domain_dependencies_t *result) {
  if (TYPEOF(dependencies) != VECSXP || ALTREP(dependencies) ||
      XLENGTH(dependencies) != 3) return FALSE;
  SEXP ids = VECTOR_ELT(dependencies, 0);
  SEXP on = VECTOR_ELT(dependencies, 1);
  SEXP conditions = VECTOR_ELT(dependencies, 2);
  if (TYPEOF(ids) != STRSXP || ALTREP(ids) ||
      TYPEOF(on) != STRSXP || ALTREP(on) ||
      TYPEOF(conditions) != VECSXP || ALTREP(conditions)) return FALSE;
  const R_xlen_t size = XLENGTH(ids);
  if (size > INT_MAX || XLENGTH(on) != size || XLENGTH(conditions) != size) {
    return FALSE;
  }
  *result = (paradox_domain_dependencies_t) {ids, on, conditions, size};
  return TRUE;
}

static int validate_dependencies(SEXP dependencies,
    paradox_domain_dependencies_t *result,
    SEXP **condition_rhs,
    R_xlen_t *work_since_interrupt) {
  static const char *const column_names[] = {"id", "on", "cond"};
  PROTECT(dependencies);
  if (!paradox_domain_read_dependencies(dependencies, result) ||
      !paradox_domain_exact_plain_table(
      dependencies,
      column_names,
      3,
      NULL,
      work_since_interrupt
    )) {
    UNPROTECT(1);
    return FALSE;
  }
  SEXP ids = PROTECT(VECTOR_ELT(dependencies, 0));
  SEXP on = PROTECT(VECTOR_ELT(dependencies, 1));
  SEXP conditions = PROTECT(VECTOR_ELT(dependencies, 2));
  if (Rf_isS4(ids) || Rf_isObject(ids) ||
      Rf_isS4(on) || Rf_isObject(on) ||
      Rf_isS4(conditions) || Rf_isObject(conditions)) {
    UNPROTECT(4);
    return FALSE;
  }
  const R_xlen_t row_count = XLENGTH(ids);
  if (!has_no_attributes(ids) ||
      !has_no_attributes(on) || !has_no_attributes(conditions)) {
    UNPROTECT(4);
    return FALSE;
  }
  SEXP *rhs_by_row = condition_rhs == NULL
    ? NULL
    : paradox_temporary_alloc(row_count, sizeof(*rhs_by_row));
  for (R_xlen_t row = 0; row < row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    SEXP condition = PROTECT(VECTOR_ELT(conditions, row));
    paradox_builtin_condition_kind_t kind;
    SEXP rhs = R_NilValue;
    if (STRING_ELT(ids, row) == NA_STRING ||
        Rf_getCharCE(STRING_ELT(ids, row)) == CE_BYTES ||
        STRING_ELT(on, row) == NA_STRING ||
        Rf_getCharCE(STRING_ELT(on, row)) == CE_BYTES ||
        !paradox_builtin_condition_exact(
          condition,
          &kind,
          &rhs,
          work_since_interrupt
        )) {
      UNPROTECT(5);
      return FALSE;
    }
    if (rhs_by_row != NULL) {
      rhs_by_row[row] = rhs;
    }
    (void) kind;
    UNPROTECT(1);
  }
  result->ids = ids;
  result->on = on;
  result->conditions = conditions;
  result->row_count = row_count;
  if (condition_rhs != NULL) {
    *condition_rhs = rhs_by_row;
  }
  UNPROTECT(4);
  return TRUE;
}

int paradox_domain_validate_dependencies(SEXP dependencies,
    paradox_domain_dependencies_t *result,
    R_xlen_t *work_since_interrupt) {
  return validate_dependencies(
    dependencies,
    result,
    NULL,
    work_since_interrupt
  );
}

int paradox_domain_validate_dependencies_with_rhs(SEXP dependencies,
    paradox_domain_dependencies_t *result,
    SEXP **condition_rhs,
    R_xlen_t *work_since_interrupt) {
  if (condition_rhs == NULL) {
    Rf_error("Internal error: missing dependency RHS output");
  }
  return validate_dependencies(
    dependencies,
    result,
    condition_rhs,
    work_since_interrupt
  );
}

int paradox_domain_read_values(SEXP values,
    paradox_domain_values_t *result,
    R_xlen_t *work_since_interrupt) {
  (void) work_since_interrupt;
  /* This is a private store, not a public assignment. The writer owns name
   * semantics; readers need only a safe list/name view. */
  if (TYPEOF(values) != VECSXP || ALTREP(values)) {
    return FALSE;
  }
  PROTECT(values);
  const R_xlen_t value_count = XLENGTH(values);
  SEXP names = PROTECT(Rf_getAttrib(values, R_NamesSymbol));
  /* Some consumers use R's matcher, which dispatches for classed names. */
  if (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isObject(names)) {
    UNPROTECT(2);
    return FALSE;
  }
  const R_xlen_t name_count = XLENGTH(names);
  if (name_count != value_count) {
    UNPROTECT(2);
    return FALSE;
  }
  result->values = values;
  result->names = names;
  result->size = value_count;
  UNPROTECT(2);
  return TRUE;
}

SEXP paradox_domain_value_rows(SEXP ids, SEXP names,
    R_xlen_t *work_since_interrupt) {
  const R_xlen_t size = XLENGTH(names);
  /* Full stores, and prefix subsets, commonly already follow schema order.
   * NULL denotes that identity mapping; otherwise R owns encoding-correct
   * matching. No persistent index or second string-matching engine. */
  int ordered = size <= XLENGTH(ids);
  for (R_xlen_t index = 0; ordered && index < size; ++index) {
    paradox_account_work(work_since_interrupt);
    ordered = STRING_ELT(names, index) == STRING_ELT(ids, index);
  }
  if (ordered) return R_NilValue;
  PROTECT(ids);
  PROTECT(names);
  SEXP rows = PROTECT(Rf_match(ids, names, 0));
  /* The admitted parameter count is at most INT_MAX. R returns integer
   * positions, and these newly produced rows have never escaped to R. */
  for (R_xlen_t index = 0; index < size; ++index) {
    paradox_account_work(work_since_interrupt);
    if (INTEGER_ELT(rows, index) == 0) {
      UNPROTECT(3);
      Rf_error("Corrupt ParamSet capsule: value owner is unknown");
    }
  }
  UNPROTECT(3);
  return rows;
}

static void set_scalar_column(SEXP result,
    enum paradox_domain_column output_column, SEXP source,
    R_xlen_t source_row) {
  PROTECT(source);
  SEXP column = PROTECT(Rf_allocVector((SEXPTYPE) TYPEOF(source), 1));
  switch (TYPEOF(source)) {
  case STRSXP:
    SET_STRING_ELT(column, 0, STRING_ELT(source, source_row));
    break;
  case VECSXP:
    SET_VECTOR_ELT(column, 0, VECTOR_ELT(source, source_row));
    break;
  case REALSXP:
    SET_REAL_ELT(column, 0, REAL_ELT(source, source_row));
    break;
  case INTSXP:
    SET_INTEGER_ELT(column, 0, INTEGER_ELT(source, source_row));
    break;
  default:
    Rf_error("Internal error: unsupported ParamSet Domain column type");
  }
  SET_VECTOR_ELT(result, output_column, column);
  UNPROTECT(2);
}

SEXP paradox_domain_prepare_facade(SEXP result, SEXP cls,
    R_xlen_t *work_since_interrupt) {
  PROTECT(cls);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, PARADOX_DOMAIN_COLUMN_COUNT));
  for (R_xlen_t column = 0;
      column < PARADOX_DOMAIN_COLUMN_COUNT;
      ++column) {
    paradox_account_work(work_since_interrupt);
    SET_STRING_ELT(names, column, Rf_mkChar(paradox_domain_column_names[column]));
  }
  Rf_setAttrib(result, R_NamesSymbol, names);

  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 4));
  SET_STRING_ELT(classes, 0, cls);
  SET_STRING_ELT(classes, 1, Rf_mkChar("Domain"));
  SET_STRING_ELT(classes, 2, Rf_mkChar("data.table"));
  SET_STRING_ELT(classes, 3, Rf_mkChar("data.frame"));
  Rf_setAttrib(result, R_ClassSymbol, classes);

  SEXP row_names = PROTECT(Rf_allocVector(INTSXP, 2));
  SET_INTEGER_ELT(row_names, 0, NA_INTEGER);
  SET_INTEGER_ELT(row_names, 1, -1);
  Rf_setAttrib(result, R_RowNamesSymbol, row_names);

  SEXP prepared = PROTECT(paradox_prepare_fresh_data_table(result));
  UNPROTECT(5);
  return prepared;
}

SEXP paradox_domain_fill(SEXP domain, const paradox_domain_row_t *row,
    R_xlen_t *work_since_interrupt) {
  paradox_account_work(work_since_interrupt);
  for (enum paradox_domain_column column = PARADOX_DOMAIN_ID;
      column < PARADOX_DOMAIN_TAGS;
      column = (enum paradox_domain_column) (column + 1)) {
    paradox_account_work(work_since_interrupt);
    set_scalar_column(
      domain,
      column,
      VECTOR_ELT(row->params->table, column),
      row->parameter_row
    );
  }
  const int typed = !paradox_domain_string_is(
    STRING_ELT(row->params->classes, row->parameter_row),
    "ParamUty"
  );
  static const enum paradox_domain_column detached_static[] = {
    PARADOX_DOMAIN_CARGO,
    PARADOX_DOMAIN_LEVELS,
    PARADOX_DOMAIN_SPECIAL_VALS,
    PARADOX_DOMAIN_DEFAULT
  };
  for (size_t index = 0;
      index < sizeof(detached_static) / sizeof(detached_static[0]);
      ++index) {
    const enum paradox_domain_column column = detached_static[index];
    SEXP output = VECTOR_ELT(domain, column);
    SEXP detached = PROTECT(paradox_detach_domain_row_field(
      VECTOR_ELT(output, 0),
      column,
      typed,
      work_since_interrupt
    ));
    if (detached == R_UnboundValue) {
      UNPROTECT(1);
      Rf_error(
        "Corrupt ParamSet capsule: cannot detach Domain `%s` field",
        paradox_domain_column_names[column]
      );
    }
    SET_VECTOR_ELT(output, 0, detached);
    UNPROTECT(1);
  }

  SEXP selected_tags = PROTECT(Rf_allocVector(STRSXP, row->tag_count));
  for (R_xlen_t index = 0; index < row->tag_count; ++index) {
    paradox_account_work(work_since_interrupt);
    SET_STRING_ELT(
      selected_tags,
      index,
      STRING_ELT(row->tags->values, row->tag_rows[index])
    );
  }
  SEXP tag_column = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(tag_column, 0, selected_tags);
  SET_VECTOR_ELT(domain, PARADOX_DOMAIN_TAGS, tag_column);
  UNPROTECT(2);

  SEXP trafo_column = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(trafo_column, 0, row->trafo);
  SET_VECTOR_ELT(domain, PARADOX_DOMAIN_TRAFO, trafo_column);
  UNPROTECT(1);

  SEXP requirements_column = PROTECT(Rf_allocVector(VECSXP, 1));
  if (row->dependency_count == 0) {
    SET_VECTOR_ELT(requirements_column, 0, R_NilValue);
  } else {
    SEXP requirements = PROTECT(Rf_allocVector(
      VECSXP,
      row->dependency_count
    ));
    for (R_xlen_t index = 0; index < row->dependency_count; ++index) {
      paradox_account_work(work_since_interrupt);
      const R_xlen_t dependency_row = row->dependency_rows[index];
      SEXP requirement = PROTECT(Rf_allocVector(VECSXP, 2));
      SEXP on = PROTECT(Rf_allocVector(STRSXP, 1));
      SET_STRING_ELT(
        on,
        0,
        STRING_ELT(row->dependencies->on, dependency_row)
      );
      SET_VECTOR_ELT(requirement, 0, on);
      SEXP condition = PROTECT(paradox_builtin_condition_snapshot(
        VECTOR_ELT(row->dependencies->conditions, dependency_row),
        work_since_interrupt
      ));
      if (condition == R_UnboundValue) {
        UNPROTECT(5);
        Rf_error("Corrupt ParamSet capsule: malformed dependency Condition");
      }
      SET_VECTOR_ELT(requirement, 1, condition);
      SEXP requirement_names = PROTECT(Rf_allocVector(STRSXP, 2));
      SET_STRING_ELT(requirement_names, 0, Rf_mkChar("on"));
      SET_STRING_ELT(requirement_names, 1, Rf_mkChar("cond"));
      Rf_setAttrib(requirement, R_NamesSymbol, requirement_names);
      SET_VECTOR_ELT(requirements, index, requirement);
      UNPROTECT(4);
    }
    SET_VECTOR_ELT(requirements_column, 0, requirements);
    UNPROTECT(1);
  }
  SET_VECTOR_ELT(domain, PARADOX_DOMAIN_REQUIREMENTS, requirements_column);
  UNPROTECT(1);

  SEXP init_given_column = PROTECT(Rf_allocVector(LGLSXP, 1));
  SET_LOGICAL_ELT(init_given_column, 0, row->init_given);
  SET_VECTOR_ELT(domain, PARADOX_DOMAIN_INIT_GIVEN, init_given_column);
  SEXP init_column = PROTECT(Rf_allocVector(VECSXP, 1));
  SEXP detached_init = PROTECT(paradox_detach_domain_row_field(
    row->init_value,
    PARADOX_DOMAIN_INIT,
    typed,
    work_since_interrupt
  ));
  SET_VECTOR_ELT(init_column, 0, detached_init);
  SET_VECTOR_ELT(domain, PARADOX_DOMAIN_INIT, init_column);
  UNPROTECT(3);

  return paradox_domain_prepare_facade(
    domain,
    STRING_ELT(row->params->classes, row->parameter_row),
    work_since_interrupt
  );
}
