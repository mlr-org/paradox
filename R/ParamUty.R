#' @export
ParamUty = R6Class("ParamUty", inherit = Param,
  public = list(
    initialize = function(id, default = NO_DEF, tags = character(0L)) {
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
    .qunif = function(x) stop("undefined")
  )
)
