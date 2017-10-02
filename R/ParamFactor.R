ParamFactor = R6Class(
  "ParamFactor",
  inherit = ParamSimple,
  public = list(
    values = NULL,
    initialize = function(id, values) {
      assertList(values, names = "named", any.missing = FALSE)
      value.names = names(values)
      check = function(x, na.ok = FALSE, null.ok = FALSE) assertChoice(x, choices = value.names, null.ok = null.ok)
      super$initialize(id = id, type = "character", check = check)
      self$values = values
    },
    sample = function(n = 1L) {
      sample(names(self$values), n)
    }
  ),
  private = list(
  )
)
