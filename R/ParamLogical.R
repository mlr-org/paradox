ParamLogical = R6Class(
  "ParamLogical",
  inherit = ParamSimple,
  public = list(
    
    # constructor
    initialize = function(id, special.vals = NULL, default = NULL, tags = NULL) {
      check = checkFlag
      
      # construct super class
      super$initialize(id = id, type = "logical", check = check, special.vals = special.vals, default = default, tags = tags)
    },

    # public methods
    sampleVector = function(n = 1L) {
      sample(c(TRUE, FALSE), size = n, replace = TRUE)
    },
    denormVector = function(x) {
      x < 0.5 #FIXME: Do we have to take care of x==0.5?
    }
  ),
  active = list(
    is.finite = function() TRUE,
    values = function() c(TRUE, FALSE),
    nlevels = function() 2L
  )
)
