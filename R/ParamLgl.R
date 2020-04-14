#' @title Logical Parameter
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Param].
#'
#' @description
#' A [Param] to describe logical parameters.
#'
#' @section Construction:
#' ```
#' ParamLgl$new(id, special_vals = list(), default = NO_DEF, tags = character())
#' ```
#' See Arguments of [Param].
#'
#' @section Fields:
#' Fields of [Param], and additionally:
#'
#' * `lower` :: `numeric(1)`\cr
#'   Lower bound.
#'   Always `NA` for this parameter.
#' * `upper` :: `numeric(1)`\cr
#'   Upper bound.
#'   Always `NA` for this parameter.
#' * `levels` :: `logical(2)`\cr
#'   Allowed levels.
#'   Always `c(TRUE, FALSE)` for this parameter.
#' * `nlevels` :: `Inf` \cr
#'   Number of categorical levels.
#'   Always 2 for this parameter.
#' * `is_bounded` :: `TRUE`\cr
#'   Are the bounds finite?
#'   Always `TRUE` for this parameter.
#'
#' @section Methods:
#' See [Param].
#'
#' @family Params
#' @include Param.R
#' @export
#' @examples
#' ParamLgl$new("flag", default = TRUE)
ParamLgl = R6Class("ParamLgl", inherit = Param,
  public = list(
    initialize = function(id, special_vals = list(), default = NO_DEF, tags = character()) {
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
    }
  ),

  active = list(
    lower = function() NA_real_,
    upper = function() NA_real_,
    levels = function() c(TRUE, FALSE),
    nlevels = function() 2L,
    is_bounded = function() TRUE,
    storage_type = function() "logical"
  ),

  private = list(
    .check = function(x) check_flag(x),
    .qunif = function(x) x < 0.5
  )
)
