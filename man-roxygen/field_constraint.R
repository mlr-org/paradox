#' @field constraint (`function(x)`)\cr
#' Constraint function. Settable on a base [`ParamSet`]; read-only and derived
#' from children on a [`ParamSetCollection`].
#' This function must evaluate a named `list()` of values and determine whether it satisfies
#' constraints, returning a scalar `logical(1)` value.
#' The input list shell and structural names/list metadata are ordinary
#' non-ALTREP/non-S4; admitted semantic atomic leaves may be stable ALTREP.
