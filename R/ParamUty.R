#' @title Untyped Parameter
#'
#' @description
#' A [Param] to describe untyped parameters.
#'
#' @template param_id
#' @template param_default
#' @template param_tags
#'
#' @family Params
#' @include Param.R
#' @export
#' @examples
#' ParamUty$new("untyped", default = Inf)
ParamUty = R6Class("ParamUty", inherit = Param,
  public = list(
    #' @field custom_check (`function()`)\cr
    #' Custom function to check the feasibility.
    custom_check = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param custom_check (`function()`)\cr
    #'   Custom function to check the feasibility.
    #'   Function which checks the input.
    #'   Must return 'TRUE' if the input is valid and a string with the error message otherwise.
    #'   Defaults to `NULL`, which means that no check is performed.
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
    #' @template field_lower
    lower = function() NA_real_,
    #' @template field_upper
    upper = function() NA_real_,
    #' @template field_levels
    levels = function() NULL,
    #' @template field_nlevels
    nlevels = function() Inf,
    #' @template field_is_bounded
    is_bounded = function() FALSE,
    #' @template field_storage_type
    storage_type = function() "list"
  ),

  private = list(
    .check = function(x) self$custom_check(x),
    .qunif = function(x) stop("undefined")
  )
)
