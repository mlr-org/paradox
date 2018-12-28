#' @title Param: Logical
#' @format [R6Class] object.
#'
#' @section Public methods:
#' * `new(id, special_vals, default, tags)` \cr
#'   `character(1)`, `list`, `any`, `character` -> self
#'
#' @name ParamLgl
#' @family Param
#' @export
ParamLgl = R6Class("ParamLgl", inherit = Param,
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
    nlevels = function() 2L,
    is_bounded = function() TRUE,
    storage_type = function() "logical"
  ),

  private = list(
    .check = function(x) check_flag(x),
    .map_unitint_to_values = function(x) x < 0.5
  )
)
