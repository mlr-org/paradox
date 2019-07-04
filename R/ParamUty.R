#' @title Untyped Parameter
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Param].
#'
#' @description
#' A [Param] to describe untyped parameters.
#'
#' @section Construction:
#' ```
#' ParamUty$new(id, default = NO_DEF, tags = character(), custom_check = NULL)
#' ```
#' Arguments of [Param], and additionally:
#' * `custom_check` :: `function()`\cr
#'   Custom function to check the feasibility. Defaults to `NULL`.
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
#' * `levels` :: `NULL`\cr
#'   Allowed levels.
#'   Always `NULL` for this parameter.
#' * `nlevels` :: `numeric(1)` \cr
#'   Number of categorical levels.
#'   Always `Inf` for this parameter.
#' * `is_bounded` :: `FALSE`\cr
#'   Are the bounds finite?
#'   Always `FALSE` for this parameter.
#'
#' @section Methods:
#' See [Param].
#'
#' @family Params
#' @export
ParamUty = R6Class("ParamUty", inherit = Param,
  public = list(
    custom_check = NULL,

    initialize = function(id, default = NO_DEF, tags = character(), custom_check = NULL) {
      # super class calls private$.check, so this must be set BEFORE
      # we initialize the super class
      if (is.null(custom_check)) {
        self$custom_check = function(x) TRUE
      } else {
        self$custom_check = assert_function(custom_check, "x")
      }
      super$initialize(id, special_vals = list(), default = default, tags = tags)
    }
  ),

  active = list(
    lower = function() NA_real_,
    upper = function() NA_real_,
    levels = function() NULL,
    nlevels = function() Inf,
    is_bounded = function() FALSE,
    storage_type = function() "list"
  ),

  private = list(
    .check = function(x) self$custom_check(x),
    .qunif = function(x) stop("undefined")
  )
)
