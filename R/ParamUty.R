#' @title Untyped Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent untyped parameters.
#'
#' @family Parameter
#' @export
ParamUty = R6Class("ParamUty", inherit = Parameter,
  public = list(
    initialize = function(id, default = NULL, tags = NULL) {
      super$initialize(id, special_vals = NULL, default = default, tags = tags)
    }
  ),

  active = list(
    lower = function() NA_real_,
    upper = function() NA_real_,
    values = function() NULL,
    nlevels = function() Inf,
    is_bounded = function() stop("undefined"),
    storage_type = function() "list"
  ),

  private = list(
    .check = function(x) TRUE,  # values are always feasible

    .map_unitint_to_values = function(x) stop("Not possible!"),

    .fix = function(x) stop("Not possible!")
  )
)
