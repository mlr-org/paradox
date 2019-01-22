#' @export
ParamUty = R6Class("ParamUty", inherit = Param,
  public = list(
    custom_check = NULL,

    initialize = function(id, default = NO_DEF, tags = character(0L), custom_check = NULL) {
      super$initialize(id, special_vals = list(), default = default, tags = tags)
      if (is.null(custom_check))
        custom_check = function(x) TRUE
      else
        assert_function(custom_check, "x")
      self$custom_check = custom_check
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
    .check = function(x) self$custom_check(x),
    .qunif = function(x) stop("undefined")
  )
)
