

Dependency = R6Class("Dependency",
  public = list(

    # member variables
    child = NULL, # the subordinate param
    parent = NULL, # the (categ) parent param, that the child depends on

    # constructor
    initialize = function(child, parent, expr) {
      assert_r6(child, "Parameter")
      assert_r6(parent, "ParamFct")
      self$child = child
      self$parent = parent
    }
  )
)

DependencyNode = R6Class("DependencyNode",
  public = list(
    # member variables
    param = NULL,
    children = NULL,
    parents = NULL,

    # constructor
    initialize = function(param) {
      self$param = param
    }
  )
)




