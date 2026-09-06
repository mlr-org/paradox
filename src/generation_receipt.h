#ifndef PARADOX_GENERATION_RECEIPT_H
#define PARADOX_GENERATION_RECEIPT_H

#include "paramset_collection_readers.h"

/*
 * Compact, ordinary-R receipts used when a native operation has selected a
 * complete ParamSet generation but must return to a short R ownership or
 * mutation handoff before it can finish. The flat carrier has four fields per
 * node and is scanned allocation-free after its symbols have been interned.
 */
attribute_hidden SEXP paradox_generation_receipt_new(R_xlen_t count);
/* Intern the terminal scanner's binding symbol while allocation is still
 * permitted. C-only operations may then retain entries in rooted native
 * workspace instead of allocating a flat carrier needed only for an R
 * handoff. */
attribute_hidden void paradox_generation_receipt_prepare(void);
attribute_hidden void paradox_generation_receipt_set(
  SEXP receipt,
  R_xlen_t index,
  SEXP private_environment,
  SEXP core,
  SEXP shadow_signature,
  SEXP shadow_signature_content
);
attribute_hidden void paradox_generation_receipt_scan_entry(
  SEXP private_environment,
  SEXP core,
  SEXP shadow_signature,
  SEXP shadow_signature_content
);
attribute_hidden SEXP paradox_generation_receipt_single(
  SEXP private_environment,
  SEXP core,
  SEXP shadow_signature,
  SEXP shadow_signature_content
);
attribute_hidden SEXP paradox_generation_receipt_graph(
  const paradox_collection_graph_t *graph
);
attribute_hidden void paradox_generation_receipt_scan(SEXP receipt);

#endif
