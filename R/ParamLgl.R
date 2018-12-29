#' @title Param: Logical
#' @format [R6Class] object. Inherits from [Param]
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
    initialize = function(id, special_vals = list(), default = NO_DEF, tags = character(0L)) {
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
    }
  ),

  active = list(
    lower = function() NA_real_,
    upper = function() NA_real_,
    values = function() c(TRUE, FALSE),
    nlevels = function() 2L,
    is_bounded = function() TRUE,
    storage_type = function() "logical"
  ),

  private = list(
    .check = function(x) check_flag(x),
    .qunif = function(x) x < 0.5
  )
)

