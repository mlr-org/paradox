#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Utils.h>

#include "r_api_compat.h"
#include "r_utils.h"

SEXP paradox_stored_attribute(SEXP object, SEXP symbol) {
  SEXP holder = PROTECT(R_MakeExternalPtr(
    NULL,
    R_NilValue,
    R_NilValue
  ));
  SHALLOW_DUPLICATE_ATTRIB(holder, object);
  SEXP result = PROTECT(Rf_getAttrib(holder, symbol));
  UNPROTECT(2);
  return result;
}

static const char *argument_type(SEXP value) {
  if (Rf_isObject(value)) {
    SEXP classes = Rf_getAttrib(value, R_ClassSymbol);
    if (TYPEOF(classes) == STRSXP && !ALTREP(classes) &&
        XLENGTH(classes) > 0) {
      SEXP first_class = STRING_ELT(classes, 0);
      if (first_class != NA_STRING) {
        return CHAR(first_class);
      }
    }
  }

  return Rf_type2char((SEXPTYPE) TYPEOF(value));
}

SEXP paradox_get_named_column_checked(SEXP table, const char *corrupt_context,
    const char *storage_name, const char *column_name) {
  if (TYPEOF(table) != VECSXP) {
    Rf_error("Corrupt %s: `%s` must be a list", corrupt_context, storage_name);
  }
  if (ALTREP(table)) {
    Rf_error(
      "Corrupt %s: `%s` must use an ordinary list representation",
      corrupt_context,
      storage_name
    );
  }

  SEXP names = Rf_getAttrib(table, R_NamesSymbol);
  const R_xlen_t n_columns = XLENGTH(table);
  if (TYPEOF(names) != STRSXP) {
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

  SEXP result = R_NilValue;
  int matches = 0;
  for (R_xlen_t column = 0; column < n_columns; ++column) {
    SEXP name = STRING_ELT(names, column);
    if (name != NA_STRING && strcmp(CHAR(name), column_name) == 0) {
      result = VECTOR_ELT(table, column);
      ++matches;
    }
  }

  if (matches == 1) {
    return result;
  }
  if (matches > 1) {
    Rf_error(
      "Corrupt %s: `%s` has more than one `%s` column",
      corrupt_context,
      storage_name,
      column_name
    );
  }

  Rf_error(
    "Corrupt %s: `%s` has no `%s` column",
    corrupt_context,
    storage_name,
    column_name
  );
  return R_NilValue;
}

SEXP paradox_get_named_column(SEXP table, const char *storage_name,
    const char *column_name) {
  return paradox_get_named_column_checked(
    table,
    "ParamSet storage",
    storage_name,
    column_name
  );
}

void paradox_require_column_checked(SEXP column, SEXPTYPE type,
    R_xlen_t size, const char *corrupt_context, const char *column_name) {
  if ((SEXPTYPE) TYPEOF(column) != type) {
    Rf_error(
      "Corrupt %s: `%s` must have type `%s` and length %lld",
      corrupt_context,
      column_name,
      Rf_type2char(type),
      (long long) size
    );
  }
  /* Canonical table columns are always ordinary vectors.  Reject an ALTREP
   * shell before asking its Length method anything: a hostile or merely
   * stateful implementation may call back into R or report a different size
   * on the second observation. */
  if (ALTREP(column)) {
    Rf_error(
      "Corrupt %s: `%s` must use an ordinary %s representation",
      corrupt_context,
      column_name,
      Rf_type2char(type)
    );
  }
  if (XLENGTH(column) != size) {
    Rf_error(
      "Corrupt %s: `%s` must have type `%s` and length %lld",
      corrupt_context,
      column_name,
      Rf_type2char(type),
      (long long) size
    );
  }
}

void paradox_require_column(SEXP column, SEXPTYPE type, R_xlen_t size,
    const char *column_name) {
  paradox_require_column_checked(
    column,
    type,
    size,
    "ParamSet storage",
    column_name
  );
}

paradox_numeric_column_t paradox_get_numeric_column(SEXP column,
    R_xlen_t size, const char *corrupt_context, const char *column_name) {
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(column);
  if (type != REALSXP && type != INTSXP) {
    Rf_error(
      "Corrupt %s: `%s` must be numeric and have length %lld",
      corrupt_context,
      column_name,
      (long long) size
    );
  }
  if (ALTREP(column)) {
    Rf_error(
      "Corrupt %s: `%s` must use an ordinary numeric representation",
      corrupt_context,
      column_name
    );
  }
  if (XLENGTH(column) != size) {
    Rf_error(
      "Corrupt %s: `%s` must be numeric and have length %lld",
      corrupt_context,
      column_name,
      (long long) size
    );
  }

  paradox_numeric_column_t result = {
    type,
    type == REALSXP ? REAL_RO(column) : NULL,
    type == INTSXP ? INTEGER_RO(column) : NULL
  };
  return result;
}

double paradox_numeric_at(const paradox_numeric_column_t *column,
    R_xlen_t index) {
  if (column->type == REALSXP) {
    return column->real_values[index];
  }

  const int value = column->integer_values[index];
  return value == NA_INTEGER ? NA_REAL : (double) value;
}

double paradox_accepted_lower(double bound, double tolerance) {
  if (tolerance == 0.0 || !R_FINITE(bound)) {
    return bound;
  }
  return bound - tolerance * fmax(1.0, fabs(bound));
}

double paradox_accepted_upper(double bound, double tolerance) {
  if (tolerance == 0.0 || !R_FINITE(bound)) {
    return bound;
  }
  return bound + tolerance * fmax(1.0, fabs(bound));
}

void paradox_require_character_argument_type(SEXP value,
    const char *argument_name) {
  if (value == R_NilValue) {
    return;
  }
  if (TYPEOF(value) != STRSXP) {
    Rf_error(
      "Assertion on '%s' failed: Must be of type 'character' (or 'NULL'), not '%s'.",
      argument_name,
      argument_type(value)
    );
  }
}

void paradox_require_character_argument(SEXP value, const char *argument_name) {
  paradox_require_character_argument_type(value, argument_name);
  if (value == R_NilValue) {
    return;
  }

  const R_xlen_t size = XLENGTH(value);
  for (R_xlen_t index = 0; index < size; ++index) {
    if (index != 0 && index % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    if (STRING_ELT(value, index) == NA_STRING) {
      Rf_error(
        "Assertion on '%s' failed: Contains missing values (element %lld).",
        argument_name,
        (long long) (index + 1)
      );
    }
  }
}

void *paradox_temporary_alloc(R_xlen_t count, size_t element_size) {
  if (count < 0 || element_size == 0 || element_size > (size_t) INT_MAX ||
      (uintmax_t) count > (uintmax_t) (SIZE_MAX / element_size)) {
    Rf_error("Unable to allocate temporary native workspace");
  }

  return (void *) R_alloc((size_t) count, (int) element_size);
}

static int parse_version_component(const char **cursor, unsigned int *value) {
  const unsigned char *position = (const unsigned char *) *cursor;
  if (*position < (unsigned char) '0' || *position > (unsigned char) '9') {
    return FALSE;
  }

  unsigned int result = 0;
  do {
    const unsigned int digit = (unsigned int) (*position - '0');
    if (result > (UINT_MAX - digit) / 10U) {
      return FALSE;
    }
    result = result * 10U + digit;
    ++position;
  } while (*position >= (unsigned char) '0' &&
      *position <= (unsigned char) '9');

  *cursor = (const char *) position;
  *value = result;
  return TRUE;
}

static int data_table_requires_capacity_bridge(void) {
  /* data.table 1.17.8's shallowwrapper() passes raw TRUELENGTH to its shell
   * allocator whenever selfrefok() succeeds. Version 1.18 switched that
   * decision to R_maxLength()/R_isResizable(), so only older releases need
   * an explicitly allocated shell around paradox's exact-length VECSXP. */
  static int cached_result = -1;
  if (cached_result >= 0) {
    return cached_result;
  }

  SEXP package = PROTECT(Rf_mkString("data.table"));
  SEXP version_call = PROTECT(Rf_lang2(
    Rf_install("getNamespaceVersion"),
    package
  ));
  SEXP version = PROTECT(Rf_eval(version_call, R_BaseEnv));
  SEXP character_call = PROTECT(Rf_lang2(
    Rf_install("as.character"),
    version
  ));
  SEXP character = PROTECT(Rf_eval(character_call, R_BaseEnv));
  if (TYPEOF(character) != STRSXP || XLENGTH(character) != 1 ||
      STRING_ELT(character, 0) == NA_STRING) {
    UNPROTECT(5);
    Rf_error("Unable to determine the loaded data.table version");
  }

  const char *cursor = CHAR(STRING_ELT(character, 0));
  unsigned int major;
  unsigned int minor;
  if (!parse_version_component(&cursor, &major) || *cursor != '.') {
    UNPROTECT(5);
    Rf_error("Unable to parse the loaded data.table version");
  }
  ++cursor;
  if (!parse_version_component(&cursor, &minor)) {
    UNPROTECT(5);
    Rf_error("Unable to parse the loaded data.table version");
  }

  cached_result = major < 1U || (major == 1U && minor < 18U);
  UNPROTECT(5);
  return cached_result;
}

static void validate_capacity_bridge(SEXP source, SEXP result,
    SEXP namespace_environment) {
  if (TYPEOF(result) != VECSXP || XLENGTH(result) != XLENGTH(source)) {
    Rf_error("data.table::alloc.col() returned an invalid table shell");
  }
  for (R_xlen_t column = 0; column < XLENGTH(source); ++column) {
    if (VECTOR_ELT(result, column) != VECTOR_ELT(source, column)) {
      Rf_error("data.table::alloc.col() copied a native table column");
    }
  }

  SEXP names = Rf_getAttrib(result, R_NamesSymbol);
  SEXP selfref = Rf_getAttrib(
    result,
    Rf_install(".internal.selfref")
  );
  if (TYPEOF(names) != STRSXP || XLENGTH(names) != XLENGTH(result) ||
      TYPEOF(selfref) != EXTPTRSXP ||
      R_ExternalPtrAddr(selfref) != (void *) R_NilValue ||
      R_ExternalPtrTag(selfref) != names) {
    Rf_error("data.table::alloc.col() returned an invalid self-reference");
  }
  SEXP owner = R_ExternalPtrProtected(selfref);
  if (TYPEOF(owner) != EXTPTRSXP ||
      R_ExternalPtrAddr(owner) != (void *) result) {
    Rf_error("data.table::alloc.col() returned an invalid table owner");
  }

  SEXP truelength_function = PROTECT(Rf_findFun(
    Rf_install("truelength"),
    namespace_environment
  ));
  SEXP truelength_call = PROTECT(Rf_lang2(truelength_function, result));
  SEXP truelength = PROTECT(Rf_eval(
    truelength_call,
    namespace_environment
  ));
  if (TYPEOF(truelength) != INTSXP || XLENGTH(truelength) != 1 ||
      INTEGER_ELT(truelength, 0) < 0 ||
      (R_xlen_t) INTEGER_ELT(truelength, 0) < XLENGTH(result)) {
    UNPROTECT(3);
    Rf_error("data.table::alloc.col() did not allocate its table shell");
  }
  UNPROTECT(3);
}

SEXP paradox_prepare_data_table(SEXP table) {
  if (data_table_requires_capacity_bridge()) {
    SEXP namespace_environment = PROTECT(
      paradox_api_registered_namespace("data.table")
    );
    if (TYPEOF(namespace_environment) != ENVSXP) {
      UNPROTECT(1);
      Rf_error("The imported data.table namespace is not loaded");
    }
    SEXP allocate = PROTECT(Rf_findFun(
      Rf_install("alloc.col"),
      namespace_environment
    ));
    SEXP zero = PROTECT(Rf_ScalarInteger(0));
    SEXP call = PROTECT(Rf_lang3(allocate, table, zero));
    SET_TAG(CDDR(call), Rf_install("n"));
    SEXP result = PROTECT(Rf_eval(call, namespace_environment));
    validate_capacity_bridge(table, result, namespace_environment);
    UNPROTECT(5);
    return result;
  }

  /* data.table's public object representation uses an external pointer whose
   * protected external pointer identifies the owning table and whose tag
   * identifies its names vector. Constructing the same representation solely
   * through R's public API keeps a native-built facade safe for subsequent
   * by-reference data.table operations without calling into data.table. */
  SEXP names = PROTECT(Rf_getAttrib(table, R_NamesSymbol));
  SEXP owner = PROTECT(R_MakeExternalPtr(
    (void *) table,
    R_NilValue,
    R_NilValue
  ));
  SEXP selfref = PROTECT(R_MakeExternalPtr(
    (void *) R_NilValue,
    names,
    owner
  ));
  Rf_setAttrib(table, Rf_install(".internal.selfref"), selfref);
  UNPROTECT(3);
  return table;
}
