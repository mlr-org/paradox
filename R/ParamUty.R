#' @title Param: Untyped
#' @format [R6Class] object.
#'
#' @description
#' Untyped parameters, can be used to bypass any complicated feasibilty checks, when
#' a param is of truely complex type. OTOH we cannot perform meaningful perations like
#' sampling or generaring designs with this param.
# FIXME: check that the above is checked and unit tested in code
#'
#' @section Public methods:
#' * `new(id, default, tags)` \cr
#'   `character(1)`, `numeric(1)`, `numeric(1)`, `list`, `any`, `character` -> self
#'
#' @name ParamUty
#' @family Param
#' @export
ParamUty = R6Class("ParamUty", inherit = Param,
  public = list(
    initialize = function(id, default = NULL, tags = NULL) {
      super$initialize(id, special_vals = list(), default = default, tags = tags)
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
