#' @title Factor Parameter
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Param].
#'
#' @description
#' A [Param] to describe categorical (factor) parameters.
#'
#' @section Construction:
#' ```
#' ParamFct$new(id, levels, special_vals = list(), default = NO_DEF, tags = character())
#' ```
#' Arguments of [Param], and additionally:
#'
#' * `levels` :: `character()`\cr
#'   Set of allowed levels.
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
#' * `levels` :: `character()`\cr
#'   Allowed levels.
#' * `nlevels` :: `Inf` \cr
#'   Number of categorical levels.
#' * `is_bounded` :: `TRUE`\cr
#'   Are the bounds finite?
#'   Always `TRUE` for this parameter.
#'
#' @section Methods:
#' See [Param].
#'
#' @family Params
#' @export
#' @examples
#' ParamFct$new("f", levels = letters[1:3])
ParamFct = R6Class("ParamFct", inherit = Param,
  public = list(
    levels = NULL,

    initialize = function(id, levels, default = NO_DEF, special_vals = list(), tags = character()) {
      assert_character(levels, any.missing = FALSE, unique = TRUE)
      self$levels = levels
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
    }
  ),

  active = list(
    lower = function() NA_real_,
    upper = function() NA_real_,
    nlevels = function() length(self$levels),
    is_bounded = function() TRUE,
    storage_type = function() "character"
  ),

  private = list(
    .check = function(x) check_choice(x, choices = self$levels),

    .qunif = function(x) {
      z = floor(x * self$nlevels * (1 - 1e-16)) + 1 # make sure we dont map to upper+1
      self$levels[z]
    }
  )
)
