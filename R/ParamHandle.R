
ParamHandle = R6Class("ParamHandle",
  inherit = ParamBase, # FIXME: Are we sure?
  public = list(
  	initialize = function(dictionary = NULL) {
      self$priv.setDictionary(dictionary)
  	}
  ),
  active = list(
    dictionary = function(x) {
      if (missing(x)) {
        self$priv.dictionary
      } else {
        self$priv.setDictionary(x)
        invisible(self)
      }
    }
  ),
  private = list(
    priv.setDictionary = function(x) {
      if (is.list(x)) {
        x = as.environment(x)
      }
      self$priv.dictionary = x #assertEnvironment(x, null.ok = TRUE)
    },
    priv.dictionary = NA
  )
)
