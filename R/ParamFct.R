#' @title Param: Factor
#' @format [R6Class] object. Inherits from [Param]
#'
#' @section Public methods:
#' * `new(id, values, special_vals, default, tags)` \cr
#'   `character(1)`, `character`, `list`, `any`, `character` -> self
#'
#' @name ParamFact
#' @family Param
#' @export
ParamFct = R6Class("ParamFct", inherit = Param,
  public = list(
    values = NULL,

    initialize = function(id, values, default = NO_DEF, special_vals = list(), tags = NULL) {
      assert_character(values, any.missing = FALSE, unique = TRUE)
      self$values = values
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
    }
  ),

  active = list(
    lower = function() NA_real_,
    upper = function() NA_real_,
    nlevels = function() length(self$values),
    is_bounded = function() TRUE,
    storage_type = function() "character"
  ),

  private = list(
    .check = function(x) check_choice(x, choices = self$values),

    .qunif = function(x) {
      z = floor(x * self$nlevels * (1 - 1e-16)) + 1 # make sure we dont map to upper+1
      self$values[z]
    }
  )
)

