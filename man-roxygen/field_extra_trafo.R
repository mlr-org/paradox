#' @field extra_trafo (`function(x, param_set)`)\cr
#' Transformation function. Settable.
#' User has to pass a `function(x)`, of the form\cr
#' (named `list()`, [ParamSet]) -> named `list()`.\cr
#' The function is responsible to transform a feasible configuration into another encoding,
#' before potentially evaluating the configuration with the target algorithm.
#' For the output, not many things have to hold.
#' It needs to have unique names, and the target algorithm has to accept the configuration.
#' For convenience, the self-paramset is also passed in, if you need some info from it (e.g. tags).
#' Is NULL by default, and you can set it to NULL to switch the transformation off.
