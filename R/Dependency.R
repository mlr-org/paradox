#' @title Parameter Dependency
#' @name Dependency
#' @export
Dependency = R6Class("Dependency",
  public = list(

    # member variables
    node_id = NULL, # thie param that is dependent
    parent_id = NULL, # the (categ) parent param, that this node depends on
    condition = NULL,

    # constructor
    initialize = function(node_id, parent_id, condition) {
      assert_character(node_id)
      assert_character(parent_id)
      assert_r6(condition, "Condition")
      self$node_id = node_id
      self$parent_id = parent_id
      self$condition = condition
    }
  )
)

