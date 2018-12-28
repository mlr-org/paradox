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

    initialize = function(id, values, default = NULL, special_vals = list(), tags = NULL) {
      assert_character(values, any.missing = FALSE, unique = TRUE)
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
      self$values = values
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

    # maps [0, 1/k] --> a, (1/k, 2/k] --> b, ..., ((k-1)/k, 1] --> z
    # NB: this is a bit inconsistent for the first part
    .map_unitint_to_values = function(x) {
      res = cut(x, breaks = seq(0, 1, length.out = self$nlevels+1), include.lowest = TRUE)
      levels(res) = self$values
      as.character(res)
    }
  )
)
