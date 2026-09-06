#include "paradox.h"

#include "core_state.h"
#include "generation_receipt.h"
#include "paramset_shadow.h"
#include "r_api_compat.h"

enum generation_receipt_field {
  GENERATION_RECEIPT_PRIVATE = 0,
  GENERATION_RECEIPT_CORE,
  GENERATION_RECEIPT_SHADOW_SIGNATURE,
  GENERATION_RECEIPT_SHADOW_CONTENT,
  GENERATION_RECEIPT_STRIDE
};

static SEXP generation_receipt_core_symbol(void) {
  static SEXP symbol = NULL;
  if (symbol == NULL) {
    symbol = Rf_install(".core");
  }
  return symbol;
}

void paradox_generation_receipt_prepare(void) {
  (void) generation_receipt_core_symbol();
}

SEXP paradox_generation_receipt_new(R_xlen_t count) {
  if (count <= 0 ||
      count > R_XLEN_T_MAX / GENERATION_RECEIPT_STRIDE) {
    Rf_error("Internal error: invalid ParamSet generation receipt size");
  }
  /*
   * Intern the sole binding symbol before constructing the receipt. The
   * terminal scanner below may then remain genuinely allocation-free even
   * when this is the first deferred operation in the session.
   */
  (void) generation_receipt_core_symbol();
  return Rf_allocVector(
    VECSXP,
    count * GENERATION_RECEIPT_STRIDE
  );
}

void paradox_generation_receipt_set(SEXP receipt, R_xlen_t index,
    SEXP private_environment, SEXP core, SEXP shadow_signature,
    SEXP shadow_signature_content) {
  const R_xlen_t offset = index * GENERATION_RECEIPT_STRIDE;
  SET_VECTOR_ELT(
    receipt,
    offset + GENERATION_RECEIPT_PRIVATE,
    private_environment
  );
  SET_VECTOR_ELT(
    receipt,
    offset + GENERATION_RECEIPT_CORE,
    core
  );
  SET_VECTOR_ELT(
    receipt,
    offset + GENERATION_RECEIPT_SHADOW_SIGNATURE,
    shadow_signature
  );
  SET_VECTOR_ELT(
    receipt,
    offset + GENERATION_RECEIPT_SHADOW_CONTENT,
    shadow_signature_content
  );
}

void paradox_generation_receipt_scan(SEXP receipt) {
  if (TYPEOF(receipt) != VECSXP || ALTREP(receipt) ||
      Rf_isS4(receipt) || Rf_isObject(receipt) ||
      !paradox_api_has_no_attributes(receipt)) {
    Rf_error("Internal error: malformed ParamSet generation receipt");
  }
  const R_xlen_t receipt_size = XLENGTH(receipt);
  if (receipt_size == 0 ||
      receipt_size % GENERATION_RECEIPT_STRIDE != 0) {
    Rf_error("Internal error: malformed ParamSet generation receipt");
  }

  for (R_xlen_t offset = 0;
      offset < receipt_size;
      offset += GENERATION_RECEIPT_STRIDE) {
    SEXP private_environment = VECTOR_ELT(
      receipt,
      offset + GENERATION_RECEIPT_PRIVATE
    );
    SEXP core = VECTOR_ELT(
      receipt,
      offset + GENERATION_RECEIPT_CORE
    );
    SEXP shadow_signature = VECTOR_ELT(
      receipt,
      offset + GENERATION_RECEIPT_SHADOW_SIGNATURE
    );
    SEXP shadow_content = VECTOR_ELT(
      receipt,
      offset + GENERATION_RECEIPT_SHADOW_CONTENT
    );
    paradox_generation_receipt_scan_entry(
      private_environment,
      core,
      shadow_signature,
      shadow_content
    );
  }
}

void paradox_generation_receipt_scan_entry(SEXP private_environment,
    SEXP core, SEXP shadow_signature, SEXP shadow_signature_content) {
  /*
   * Receipt construction follows complete graph/capsule admission, and a
   * selected external-pointer capsule is immutable under every supported
   * operation. This terminal barrier therefore authenticates generation, not
   * schema, by comparing the required plain binding directly. Calling
   * paradox_core_from_private() here would replay the complete capsule
   * validator for every node. A Shadow's ordinary metadata attribute is the
   * one mutable carrier outside that immutable payload, so it retains the
   * separate exact carrier/content check below.
   */
  SEXP core_symbol = generation_receipt_core_symbol();
  if (TYPEOF(private_environment) != ENVSXP ||
      Rf_isS4(private_environment) ||
      TYPEOF(core) != EXTPTRSXP || Rf_isS4(core) ||
      paradox_api_plain_binding_snapshot(
        private_environment,
        core_symbol
      ) != core ||
      ((shadow_signature != R_NilValue ||
        shadow_signature_content != R_NilValue)
        ? (shadow_signature == R_NilValue ||
          shadow_signature_content == R_NilValue ||
          !paradox_shadow_signature_receipt_is_current(
            core,
            shadow_signature,
            shadow_signature_content
          ))
        : FALSE)) {
    Rf_error("ParamSet graph changed during deferred operation");
  }
}

SEXP paradox_generation_receipt_single(SEXP private_environment,
    SEXP core, SEXP shadow_signature, SEXP shadow_signature_content) {
  SEXP result = PROTECT(paradox_generation_receipt_new(1));
  paradox_generation_receipt_set(
    result,
    0,
    private_environment,
    core,
    shadow_signature,
    shadow_signature_content
  );
  UNPROTECT(1);
  return result;
}

SEXP paradox_generation_receipt_graph(
    const paradox_collection_graph_t *graph) {
  if (graph == NULL) {
    Rf_error("Internal error: missing ParamSet generation graph");
    return R_NilValue;
  }
  const R_xlen_t count = graph->count;
  if (count <= 0) {
    Rf_error("Internal error: empty ParamSet generation graph");
  }
  SEXP result = PROTECT(paradox_generation_receipt_new(count));
  for (R_xlen_t index = 0; index < count; ++index) {
    const paradox_collection_graph_node_t *node = &graph->nodes[index];
    paradox_generation_receipt_set(
      result,
      index,
      node->private_environment,
      node->source_core,
      node->kind == PARADOX_CORE_SHADOW
        ? node->shadow_signature
        : R_NilValue,
      node->kind == PARADOX_CORE_SHADOW
        ? node->shadow_signature_content
        : R_NilValue
    );
  }
  UNPROTECT(1);
  return result;
}

SEXP paradox_param_set_generation_receipt(SEXP receipt) {
  paradox_generation_receipt_scan(receipt);
  /*
   * This is a terminal barrier. R_NilValue is immortal and returning it cannot
   * create a finalizer window after the exact allocation-free scan.
   */
  return R_NilValue;
}
