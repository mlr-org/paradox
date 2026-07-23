#ifndef PARADOX_UPGRADE_GRAPH_H
#define PARADOX_UPGRADE_GRAPH_H

#include "paradox.h"

/* Read-only, identity-preserving discovery for recursive Paradox migration.
 * The returned named list contains `objects`, the unique legacy
 * ParamSet-family candidate shells, and parallel deterministic `paths`.
 * Current capsule-backed shells are traversed but are not candidates. */
attribute_hidden SEXP paradox_upgrade_graph_discover(SEXP root);

#endif
