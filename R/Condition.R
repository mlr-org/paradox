Condition = R6Class("Condition",
  public = list(
    id = NULL,
    fun = NULL, # Function called on parent_val and self$data and has to return TRUE or FALSE
    data = NULL,

    # constructor
    initialize = function(id, fun, data) {
      self$id = assert_string(id)
      self$fun = assert_function(fun)
      self$data = data
    },

    eval = function(parent_val) {
      self$fun(parent_val, self$data)
    })
)

cond_equal = function(x) {
  Condition$new(
    id = "equal",
    fun = identical,
    data = x
  )
}

cond_choice = function(x) {
  Condition$new(
    id = "anyof",
    fun = test_choice,
    data = x
  )
}
