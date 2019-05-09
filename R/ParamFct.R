#' @export
ParamFct = R6Class("ParamFct", inherit = Param,
  public = list(
    levels = NULL,

    initialize = function(id, levels, default = NO_DEF, special_vals = list(), tags = character(0L)) {
      assert_character(levels, any.missing = FALSE, unique = TRUE)
      self$levels = levels
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
    }),

  active = list(
    lower = function() NA_real_,
    upper = function() NA_real_,
    nlevels = function() length(self$levels),
    is_bounded = function() TRUE,
    storage_type = function() "character"
  ),

  private = list(
    .check = function(x) check_choice(x, choices = self$levels),

    .qunif = function(x) {
      z = floor(x * self$nlevels * (1 - 1e-16)) + 1 # make sure we dont map to upper+1
      self$levels[z]
    })
)
