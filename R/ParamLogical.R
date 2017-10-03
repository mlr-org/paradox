ParamLogical = R6Class(
  "ParamLogical",
  inherit = ParamSimple,
  public = list(
    
    # constructor
    initialize = function(id, special.vals = NULL, default = NULL) {
      check = checkFlag
      super$initialize(id = id, type = "logical", check = check, special.vals = special.vals, default = default)
    },

    # public methods
    sample = function(n = 1L) {
      sample(c(TRUE, FALSE), n = n, replicate = TRUE)
    },
    denorm = function(x) {
      x < 0.5 #FIXME: Do we have to take care of x==0.5?
    }
  ),
  active = list(
    is.finite = function() TRUE,
    levels = function() c(TRUE, FALSE),
    nlevels = function() 2L,
    is.finite = TRUE
  )
)
