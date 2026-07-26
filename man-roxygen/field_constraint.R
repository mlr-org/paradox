#' @field constraint (`function(x)`)\cr
#' Constraint function. Settable on a base [`ParamSet`]; read-only and derived
#' from children on a [`ParamSetCollection`].
#' This function must evaluate a named `list()` of values and determine whether it satisfies
#' constraints, returning a scalar `logical(1)` value.
#' At package-owned check, test, and assignment sites, it receives only the
#' default-aware active subset of the configuration being validated. A
#' collection computes activity over its complete translated configuration,
#' including cross-set dependencies, before each child receives an unprefixed
#' active child-scope slice.
#' The input list shell and structural names/list metadata are ordinary
#' non-ALTREP/non-S4; admitted semantic atomic leaves may be stable ALTREP.
