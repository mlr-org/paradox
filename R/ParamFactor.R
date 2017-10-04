ParamFactor = R6Class(
  "ParamFactor",
  inherit = ParamSimple,
  public = list(
   
    # member variables
    values = NULL,
    true.values = NULL,

    # constructor
    initialize = function(id, values, default = NULL, special.vals = NULL, trafo = NULL) {
      # convinience: if values is a list, store the entries in true.values and don't allow trafo
      if (is.list(values) && !is.null(trafo))
        stop("If the values are supplied as a list, trafo is not allowed!")
      if (is.list(values)) {
        assertList(values, names = "named", any.missing = FALSE)
        true.values = values
        values = names(true.values)
      } else {
        true.values = NULL
      }
      
      check = function(x, na.ok = FALSE, null.ok = FALSE) checkChoice(x, choices = values, null.ok = null.ok)
      
      super$initialize(id = id, type = "character", check = check, default = default, special.vals = special.vals, trafo = trafo)
      
      self$values = assertCharacter(values, any.missing = FALSE, unique = TRUE)
      self$true.values = true.values
      if (!is.null(true.values)) {
        trafo = function(x) {
          self$true.values[x]
        }
      }
      self$trafo = trafo
    },

    # public methods
    sample = function(n = 1L) {
      res = sample(self$values, n, replace = TRUE)
      catf(res)
      res
    }, 
    denorm = function(x) {
      res = cut(x, breaks = self$nlevels)
      as.character(factor(res, labels = self$values))
    }
  ),
  active = list(
    nlevels = function() length(self$values),
    is.finite = function() TRUE
  ),
  private = list(
  )
)
