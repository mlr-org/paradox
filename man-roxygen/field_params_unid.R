#' @field params_unid (named `list()`)\cr
#' List of [Param], named with their true ID. However,
#' this field has the [Param]'s `$id` value set to a
#' potentially invalid value, and it is furthermore not
#' cloned when the `ParamSet` is cloned. This active
#' binding should be used internally, or to avoid unnecessary
#' cloning when that becomes a bottleneck.
