ParamFactor = R6Class(
  "ParamFactor",
  inherit = ParamSimple,
  public = list(

    # member variables
    values = NULL,
    true.values = NULL,

    # constructor
    initialize = function(id, values, default = NULL, special.vals = NULL, trafo = NULL, allowed = NULL, tags = character()) {

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

      # construct super class
      super$initialize(id = id, type = "character", check = check, default = default, special.vals = special.vals, trafo = trafo, allowed = allowed, tags = tags)

      # write member variables
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
    msample = function(n = 1L) { # FIXME: This will break stuff! Better not overwrite super$sample() here!
      res = sample(self$values, n, replace = TRUE)
      cat(res)
      res
    },
    sampleVectorUnrestricted = function(n = 1L) {
      sample(self$values, n, replace = TRUE)
    },
    denormVector = function(x) {
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
