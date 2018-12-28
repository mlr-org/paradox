#' @title Param: Untyped
#' @format [R6Class] object. Inherits from [Param]
#'
#' @description
#' Untyped parameters, can be used to bypass any complicated feasibility checks, when
#' a param is of truly complex type. OTOH we cannot perform meaningful operations like
#' sampling or generating designs with this param.
#'
#' @section Public methods:
#' * `new(id, default, tags)` \cr
#'   `character(1)`, `numeric(1)`, `numeric(1)`, `list`, `any`, `character` -> self
#'
#' @name ParamUty
#' @family Param
#' @export
ParamUty = R6Class("ParamUty", inherit = Param,
  public = list(
    initialize = function(id, default = NO_DEF, tags = NULL) {
      super$initialize(id, special_vals = list(), default = default, tags = tags)
    }
  ),

  active = list(
    lower = function() NA_real_,
    upper = function() NA_real_,
    values = function() NULL,
    nlevels = function() Inf,
    is_bounded = function() FALSE,
    storage_type = function() "list"
  ),

  private = list(
    .check = function(x) TRUE,  # values are always feasible
    .map_unitint_to_values = function(x) stop("undefined")
  )
)
