#' @title Parameter: Logical
#' @format [R6Class] object.
#'
#' @section Public methods:
#' * `new(id, special_vals, default, tags)` \cr
#'   `character(1)`, `list`, `any`, `character` -> self
#'
#' @name ParamLgl
#' @family Parameter
#' @export
ParamLgl = R6Class("ParamLgl", inherit = Parameter,
  public = list(
    initialize = function(id, special_vals = list(), default = NULL, tags = NULL) {
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
    }
  ),

  active = list(
    # FIXME: add unit test that this has "values"
    lower = function() NA_real_,
    upper = function() NA_real_,
    values = function() c(TRUE, FALSE),
    # FIXME: what is this? has to do with fix? maybe ok now?
    nlevels = function() length(self$values),
    is_bounded = function() TRUE,
    storage_type = function() "logical"
  ),

  private = list(
    .check = function(x) {
      check_choice(x, self$values)
    },

    .map_unitint_to_values = function(x) {
      # FIXME: the code was bad here because of the "fixing" i think. we should do that in the super class!
      x < 0.5
    },

    .fix = function(x) {
      self$values = x
    }
  )
)
