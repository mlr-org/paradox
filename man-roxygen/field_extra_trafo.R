#' @field extra_trafo (`function(x, param_set)`)\cr
#' Transformation function. Settable on a base [`ParamSet`]; read-only and
#' derived from children on a [`ParamSetCollection`].
#' User has to pass a `function(x)`, of the form\cr
#' (named `list()`, [ParamSet]) -> `list()`.\cr
#' Input and result list shells, names, and list metadata must be ordinary
#' non-ALTREP/non-S4. Admitted semantic atomic leaves (or columns of a
#' documented ordinary data-frame input) may be stable ALTREP.
#' The function is responsible to transform a feasible configuration into another encoding,
#' before potentially evaluating the configuration with the target algorithm.
#' For the output, not many things have to hold.
#' A base [`ParamSet`] retains an unnamed result for backward compatibility;
#' otherwise supplied names must be unique. A child transformation in a
#' [`ParamSetCollection`] must return unique names so they can be translated
#' into the collection namespace. The target algorithm must accept the result.
#' For convenience, the self-paramset is also passed in, if you need some info from it (e.g. tags).
#' Is NULL by default, and you can set it to NULL to switch the transformation off.
