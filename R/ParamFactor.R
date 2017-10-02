ParamFactor = R6Class(
  "ParamFactor",
  inherit = ParamSimple,
  public = list(
    values = NULL,
    initialize = function(id, values, default = NULL, special.vals = NULL) {
      assertList(values, names = "named", any.missing = FALSE)
      value.names = names(values)
      check = function(x, na.ok = FALSE, null.ok = FALSE) assertChoice(x, choices = value.names, null.ok = null.ok)
      super$initialize(id = id, type = "character", check = check, default = default, special.vals = special.vals)
      self$values = values
    },
    sample = function(n = 1L) {
      sample(names(self$values), n)
    }, 
    denorm = function(x) {
      res = cut(x, breaks = self$nlevels)
      as.character(factor(res, labels = names(values)))
    }
  ),
  active = list(
    nlevels = function() length(names(values))
  ),
  private = list(
  )
)
