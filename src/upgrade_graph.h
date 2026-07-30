#ifndef PARADOX_UPGRADE_GRAPH_H
#define PARADOX_UPGRADE_GRAPH_H

#include "paradox.h"

/* Read-only, identity-preserving discovery for recursive Paradox migration.
 * The returned named list contains `objects`, the unique legacy
 * ParamSet-family candidate shells, and parallel deterministic `paths`.
 * Current capsule-backed shells are traversed but are not candidates. */
attribute_hidden SEXP paradox_upgrade_graph_discover(SEXP root);

/* Own exact ordinary class metadata without inheritance dispatch or ALTREP
 * observation. NULL denotes no class; FALSE denotes malformed structure. */
attribute_hidden SEXP paradox_upgrade_class_snapshot(SEXP value);

/* Validate and shallow-snapshot the plain list shell captured by an
 * authenticated Paradox-1 detached callback wrapper. Returns NULL for an
 * unsupported structural shell without observing ALTREP length or elements. */
attribute_hidden SEXP paradox_upgrade_carrier_list_snapshot(SEXP source);

/* Capture one exact legacy data.table/data.frame shell after all destination
 * allocations. The returned exact two-field list contains an ordinary,
 * unclassed names/column snapshot and the optional `repr` attribute selected
 * from the same generation. `expected_classes` is the exact legacy class
 * vector and `allow_repr` is one non-missing logical scalar. */
attribute_hidden SEXP paradox_upgrade_table_list_snapshot(
  SEXP source,
  SEXP expected_classes,
  SEXP allow_repr
);

/* Allocation-free terminal receipt for the complete known public binding
 * surface selected by legacy shell transplantation. Each record retains one
 * exact shell/class plus symbol, value, active, lock, and environment-lock
 * vectors. */
attribute_hidden void paradox_validate_upgrade_public_binding_receipts(
  SEXP receipts
);
attribute_hidden SEXP paradox_upgrade_public_binding_receipts(SEXP receipts);

/* Representation-only admission used by the cold legacy-schema validator.
 * Attributes are owned by the caller-specific validator; this predicate only
 * distinguishes an ordinary VECSXP shell from structural ALTREP, S4, and
 * pairlist impostors without observing elements. */
attribute_hidden SEXP paradox_upgrade_structural_list_exact(SEXP source);

#endif
