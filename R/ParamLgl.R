#' @title Logical Parameter
#'
#' @description
#' A [Param] to describe logical parameters.
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
#' ParamLgl$new("flag", default = TRUE)
ParamLgl = R6Class("ParamLgl", inherit = Param,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, special_vals = list(), default = NO_DEF, tags = character()) {
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
    },

    to_tune = function() {
      private$.to_tune_param = ParamLgl$new(id = self$id)
    }
  ),

  active = list(
    #' @template field_lower
    lower = function() NA_real_,
    #' @template field_upper
    upper = function() NA_real_,
    #' @template field_levels
    levels = function() c(TRUE, FALSE),
    #' @template field_nlevels
    nlevels = function() 2L,
    #' @template field_is_bounded
    is_bounded = function() TRUE,
    #' @template field_storage_type
    storage_type = function() "logical"
  ),

  private = list(
    .check = function(x) check_flag(x),
    .qunif = function(x) x < 0.5
  )
)
