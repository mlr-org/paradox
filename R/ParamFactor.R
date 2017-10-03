ParamFactor = R6Class(
  "ParamFactor",
  inherit = ParamSimple,
  public = list(
    values = NULL,
    initialize = function(id, values, default = NULL, special.vals = NULL) {
      # convinience: convert character vector to named list
      if (testCharacter(values, any.missing = FALSE, unique = TRUE)) {
        values = as.list(values)
        names(values) = values
      }

      assertList(values, names = "named", any.missing = FALSE)
      value.names = names(values)
      
      check = function(x, na.ok = FALSE, null.ok = FALSE) checkChoice(x, choices = value.names, null.ok = null.ok)
      
      super$initialize(id = id, type = "character", check = check, default = default, special.vals = special.vals)
      
      self$values = values
    },

    # public methods
    sample = function(n = 1L) {
      sample(names(self$values), n, replace = TRUE)
    }, 
    denorm = function(x) {
      res = cut(x, breaks = self$nlevels)
      as.character(factor(res, labels = names(self$values)))
    }
  ),
  active = list(
    levels = function() names(self$values),
    nlevels = function() length(self$values),
    is.finite = function() TRUE
  ),
  private = list(
  )
)
