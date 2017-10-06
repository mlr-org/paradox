ParamUntyped = R6Class(
  "ParamUntyped",
  inherit = ParamSimple,
  public = list(
    # member variables

    # constructor
    initialize = function(id, default = NULL, tags = NULL) {
      check = function(x, na.ok = FALSE, null.ok = FALSE) return(TRUE)

      # construct super class
      super$initialize(id = id, type = "list", check = check, default = default, tags = tags)
    },

    # public methods
    sampleVector = function(n = 1L) {
      stop("Untped Param can not be sampled")
    },
    denormVector = function(x) {
      stop("Untyped Param can not be denormed")
    }
  )
)
