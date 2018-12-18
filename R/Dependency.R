

Dependency = R6Class("Dependency",
  public = list(

    # member variables
    child = NULL, # the subordinate param
    parent = NULL, # the (categ) parent param, that the child depends on
    condition = NULL,

    # constructor
    initialize = function(child, parent, condition) {
      assert_r6(child, "Parameter")
      assert_r6(parent, "ParamFct")
      assert_r6(condition, "Condition")
      self$child = child
      self$parent = parent
      self$condition = condition
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




