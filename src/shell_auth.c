#include <string.h>

#include "shell_auth.h"

#include "paramset_shadow.h"
#include "r_api_compat.h"

static int ordinary_class_label(SEXP label) {
  return label != NA_STRING && Rf_getCharCE(label) != CE_BYTES &&
    CHAR(label)[0] != '\0';
}

static int class_label_is(SEXP label, const char *expected) {
  return ordinary_class_label(label) &&
    strcmp(CHAR(label), expected) == 0;
}

int paradox_param_set_assert_values_is_exact(SEXP value) {
  return TYPEOF(value) == LGLSXP && !ALTREP(value) && !Rf_isS4(value) &&
    XLENGTH(value) == 1 && paradox_api_has_no_attributes(value) &&
    LOGICAL_ELT(value, 0) != NA_LOGICAL;
}

SEXP paradox_param_set_assert_values_exact(SEXP value) {
  return Rf_ScalarLogical(
    paradox_param_set_assert_values_is_exact(value)
  );
}

paradox_core_kind_t paradox_param_set_class_kind_raw(
    SEXP self, SEXP *classes_result) {
  if (classes_result != NULL) *classes_result = R_NilValue;
  if (TYPEOF(self) != ENVSXP || Rf_isS4(self)) return 0;

  SEXP classes = Rf_getAttrib(self, R_ClassSymbol);
  if (classes_result != NULL) *classes_result = classes;
  if (TYPEOF(classes) != STRSXP || ALTREP(classes) || Rf_isS4(classes) ||
      !paradox_api_has_no_attributes(classes)) {
    return 0;
  }
  const R_xlen_t size = XLENGTH(classes);
  if (size < 2 ||
      !class_label_is(STRING_ELT(classes, size - 2), "ParamSet") ||
      !class_label_is(STRING_ELT(classes, size - 1), "R6")) {
    return 0;
  }
  for (R_xlen_t index = 0; index < size; ++index) {
    SEXP label = STRING_ELT(classes, index);
    if (!ordinary_class_label(label)) return 0;
    for (R_xlen_t previous = 0; previous < index; ++previous) {
      if (strcmp(CHAR(label), CHAR(STRING_ELT(classes, previous))) == 0) {
        return 0;
      }
    }
  }

  paradox_core_kind_t kind = PARADOX_CORE_BASE;
  R_xlen_t additive_count = size - 2;
  if (additive_count != 0 &&
      class_label_is(STRING_ELT(classes, additive_count - 1),
        "ParamSetCollection")) {
    kind = PARADOX_CORE_COLLECTION;
    --additive_count;
  } else if (additive_count != 0 &&
      class_label_is(STRING_ELT(classes, additive_count - 1),
        "ParamSetShadow")) {
    kind = PARADOX_CORE_SHADOW;
    --additive_count;
  }

  /*
   * Earlier labels are presentation-only additive subclasses. Reserved
   * family labels may occur only in the exact terminal positions above;
   * accepting one elsewhere would make class/capsule kind ambiguous.
   */
  for (R_xlen_t index = 0; index < additive_count; ++index) {
    SEXP label = STRING_ELT(classes, index);
    if (class_label_is(label, "ParamSet") ||
        class_label_is(label, "R6") ||
        class_label_is(label, "ParamSetCollection") ||
        class_label_is(label, "ParamSetShadow")) {
      return 0;
    }
  }
  return kind;
}

SEXP paradox_param_set_class_kind(SEXP self) {
  return Rf_ScalarInteger((int) paradox_param_set_class_kind_raw(self, NULL));
}

enum gateway_context_field {
  GATEWAY_CONTEXT_OK = 0,
  GATEWAY_CONTEXT_ENCLOSURE,
  GATEWAY_CONTEXT_PRIVATE,
  GATEWAY_CONTEXT_SUPER,
  GATEWAY_CONTEXT_CORE,
  GATEWAY_CONTEXT_CLASS,
  GATEWAY_CONTEXT_ASSERT_VALUES,
  GATEWAY_CONTEXT_FIELD_COUNT
};

static void clear_gateway_context(SEXP result, SEXP false_value) {
  SET_VECTOR_ELT(result, GATEWAY_CONTEXT_OK, false_value);
  for (int field = GATEWAY_CONTEXT_ENCLOSURE;
      field < GATEWAY_CONTEXT_FIELD_COUNT;
      ++field) {
    SET_VECTOR_ELT(result, field, R_NilValue);
  }
}

typedef SEXP (*gateway_binding_reader_t)(SEXP, SEXP);

static SEXP gateway_enclosure_at(SEXP top, SEXP self,
    SEXP private_environment, R_xlen_t steps,
    SEXP enclosure_symbol, SEXP self_symbol, SEXP private_symbol,
    SEXP super_symbol, gateway_binding_reader_t read_binding) {
  SEXP enclosure = top;
  for (R_xlen_t step = 0; step <= steps; ++step) {
    if (TYPEOF(enclosure) != ENVSXP || Rf_isS4(enclosure) ||
        read_binding(enclosure, self_symbol) != self ||
        read_binding(enclosure, private_symbol) != private_environment) {
      return R_UnboundValue;
    }
    if (step == steps) return enclosure;
    SEXP super = read_binding(enclosure, super_symbol);
    if (TYPEOF(super) != ENVSXP || Rf_isS4(super)) {
      return R_UnboundValue;
    }
    enclosure = read_binding(super, enclosure_symbol);
  }
  return R_UnboundValue;
}

static SEXP gateway_target_super(SEXP target, SEXP self,
    SEXP private_environment, paradox_core_kind_t expected_kind,
    SEXP enclosure_symbol, SEXP self_symbol, SEXP private_symbol,
    SEXP super_symbol, gateway_binding_reader_t read_binding) {
  SEXP super = read_binding(target, super_symbol);
  if (expected_kind == PARADOX_CORE_BASE) {
    return super == R_UnboundValue &&
        !paradox_api_frame_has_binding(target, super_symbol)
      ? R_NilValue
      : R_UnboundValue;
  }
  if (TYPEOF(super) != ENVSXP || Rf_isS4(super)) {
    return R_UnboundValue;
  }
  SEXP base_enclosure = read_binding(super, enclosure_symbol);
  if (TYPEOF(base_enclosure) != ENVSXP || Rf_isS4(base_enclosure) ||
      read_binding(base_enclosure, self_symbol) != self ||
      read_binding(base_enclosure, private_symbol) != private_environment ||
      read_binding(base_enclosure, super_symbol) != R_UnboundValue ||
      paradox_api_frame_has_binding(base_enclosure, super_symbol)) {
    return R_UnboundValue;
  }
  return super;
}

static int gateway_expected_kind(SEXP value) {
  if (TYPEOF(value) != INTSXP || ALTREP(value) || Rf_isS4(value) ||
      !paradox_api_has_no_attributes(value) || XLENGTH(value) != 1) {
    return -1;
  }
  const int parsed = INTEGER_ELT(value, 0);
  return parsed >= 0 && parsed <= PARADOX_CORE_SHADOW
    ? parsed
    : -1;
}

SEXP paradox_gateway_context_snapshot(SEXP self, SEXP expected_kind_value) {
  static const char *const field_names[GATEWAY_CONTEXT_FIELD_COUNT] = {
    "ok", "enclosure", "private", "super", "core", "class", "assert_values"
  };

  /*
   * Allocate the complete return carrier before selecting any binding. Once a
   * selected value enters this vector it remains rooted even if an
   * allocation-triggered finalizer rewires the shell. The final receipt scan
   * itself allocates nothing.
   */
  SEXP result = PROTECT(Rf_allocVector(VECSXP, GATEWAY_CONTEXT_FIELD_COUNT));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, GATEWAY_CONTEXT_FIELD_COUNT));
  SEXP false_value = PROTECT(Rf_ScalarLogical(FALSE));
  SEXP true_value = PROTECT(Rf_ScalarLogical(TRUE));
  for (int field = 0; field < GATEWAY_CONTEXT_FIELD_COUNT; ++field) {
    SET_STRING_ELT(names, field, Rf_mkChar(field_names[field]));
  }
  Rf_setAttrib(result, R_NamesSymbol, names);
  clear_gateway_context(result, false_value);

  SEXP enclosure_symbol = Rf_install(".__enclos_env__");
  SEXP self_symbol = Rf_install("self");
  SEXP private_symbol = Rf_install("private");
  SEXP super_symbol = Rf_install("super");
  SEXP core_symbol = Rf_install(".core");
  SEXP assert_values_symbol = Rf_install("assert_values");
  const int parsed_expected_kind =
    gateway_expected_kind(expected_kind_value);
  if (parsed_expected_kind < 0) {
    UNPROTECT(4);
    return result;
  }
  paradox_core_kind_t expected_kind =
    (paradox_core_kind_t) parsed_expected_kind;

  SEXP classes = R_NilValue;
  const paradox_core_kind_t class_kind =
    paradox_param_set_class_kind_raw(self, &classes);
  if (class_kind == 0) {
    UNPROTECT(4);
    return result;
  }
  SET_VECTOR_ELT(result, GATEWAY_CONTEXT_CLASS, classes);

  SEXP top_enclosure = paradox_api_plain_binding_snapshot(
    self,
    enclosure_symbol
  );
  if (TYPEOF(top_enclosure) != ENVSXP || Rf_isS4(top_enclosure)) {
    clear_gateway_context(result, false_value);
    UNPROTECT(4);
    return result;
  }
  if (paradox_api_plain_binding_snapshot(top_enclosure, self_symbol) != self) {
    clear_gateway_context(result, false_value);
    UNPROTECT(4);
    return result;
  }
  SEXP private_environment = paradox_api_plain_binding_snapshot(
    top_enclosure,
    private_symbol
  );
  if (TYPEOF(private_environment) != ENVSXP ||
      Rf_isS4(private_environment)) {
    clear_gateway_context(result, false_value);
    UNPROTECT(4);
    return result;
  }
  SET_VECTOR_ELT(result, GATEWAY_CONTEXT_PRIVATE, private_environment);

  SEXP assert_values = paradox_api_plain_binding_snapshot(
    self,
    assert_values_symbol
  );
  if (!paradox_param_set_assert_values_is_exact(assert_values)) {
    clear_gateway_context(result, false_value);
    UNPROTECT(4);
    return result;
  }
  SET_VECTOR_ELT(
    result,
    GATEWAY_CONTEXT_ASSERT_VALUES,
    assert_values
  );

  SEXP core = paradox_api_plain_binding_snapshot(
    private_environment,
    core_symbol
  );
  if (!paradox_core_is_canonical(core)) {
    clear_gateway_context(result, false_value);
    UNPROTECT(4);
    return result;
  }
  SET_VECTOR_ELT(result, GATEWAY_CONTEXT_CORE, core);
  const paradox_core_kind_t core_kind = paradox_core_kind(core);
  if (core_kind != class_kind ||
      (expected_kind != 0 && expected_kind != PARADOX_CORE_BASE &&
        expected_kind != core_kind) ||
      (core_kind == PARADOX_CORE_SHADOW &&
        !paradox_shadow_metadata_is_exact(core))) {
    clear_gateway_context(result, false_value);
    UNPROTECT(4);
    return result;
  }
  if (expected_kind == 0) expected_kind = core_kind;

  const R_xlen_t family_marker_count =
    core_kind == PARADOX_CORE_BASE ? 0 : 1;
  const R_xlen_t additive_count =
    XLENGTH(classes) - 2 - family_marker_count;
  const R_xlen_t steps = additive_count +
    (expected_kind == PARADOX_CORE_BASE &&
      core_kind != PARADOX_CORE_BASE);
  SEXP enclosure = gateway_enclosure_at(
    top_enclosure,
    self,
    private_environment,
    steps,
    enclosure_symbol,
    self_symbol,
    private_symbol,
    super_symbol,
    paradox_api_plain_binding_snapshot
  );
  SEXP super = enclosure == R_UnboundValue
    ? R_UnboundValue
    : gateway_target_super(
        enclosure,
        self,
        private_environment,
        expected_kind,
        enclosure_symbol,
        self_symbol,
        private_symbol,
        super_symbol,
        paradox_api_plain_binding_snapshot
      );
  if (enclosure == R_UnboundValue || super == R_UnboundValue) {
    clear_gateway_context(result, false_value);
    UNPROTECT(4);
    return result;
  }
  SET_VECTOR_ELT(result, GATEWAY_CONTEXT_ENCLOSURE, enclosure);
  SET_VECTOR_ELT(result, GATEWAY_CONTEXT_SUPER, super);

  SEXP scanned_enclosure = gateway_enclosure_at(
    top_enclosure,
    self,
    private_environment,
    steps,
    enclosure_symbol,
    self_symbol,
    private_symbol,
    super_symbol,
    paradox_api_plain_binding_scan
  );
  SEXP scanned_super = scanned_enclosure == R_UnboundValue
    ? R_UnboundValue
    : gateway_target_super(
        scanned_enclosure,
        self,
        private_environment,
        expected_kind,
        enclosure_symbol,
        self_symbol,
        private_symbol,
        super_symbol,
        paradox_api_plain_binding_scan
      );
  if (Rf_getAttrib(self, R_ClassSymbol) != classes ||
      paradox_param_set_class_kind_raw(self, NULL) != class_kind ||
      paradox_api_plain_binding_scan(self, enclosure_symbol) != top_enclosure ||
      scanned_enclosure != enclosure ||
      scanned_super != super ||
      paradox_api_plain_binding_scan(self, assert_values_symbol) !=
        assert_values ||
      paradox_api_plain_binding_scan(private_environment, core_symbol) !=
        core) {
    clear_gateway_context(result, false_value);
    UNPROTECT(4);
    return result;
  }
  SET_VECTOR_ELT(result, GATEWAY_CONTEXT_OK, true_value);
  UNPROTECT(4);
  return result;
}
