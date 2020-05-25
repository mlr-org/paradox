#' @title Factor Parameter
#'
#' @description
#' A [Param] to describe categorical (factor) parameters.
#'
#' @template param_id
#' @template param_special_vals
#' @template param_default
#' @template param_tags
#'
#' @family Params
#' @include Param.R
#' @export
#' @examples
#' ParamFct$new("f", levels = letters[1:3])
ParamFct = R6Class("ParamFct", inherit = Param,
  public = list(

    #' @template field_levels
    levels = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param levels (`character()`)\cr
    #'   Set of allowed levels.
    initialize = function(id, levels, default = NO_DEF, special_vals = list(), tags = character()) {
      assert_character(levels, any.missing = FALSE, unique = TRUE)
      self$levels = levels
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
    }
  ),

  active = list(
    #' @template field_lower
    lower = function() NA_real_,
    #' @template field_upper
    upper = function() NA_real_,
    #' @template field_nlevels
    nlevels = function() length(self$levels),
    #' @template field_is_bounded
    is_bounded = function() TRUE,
    #' @template field_storage_type
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
