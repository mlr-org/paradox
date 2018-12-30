#' @title Dependency condition
#'
#' @description
#' Condition object, to specify the condition in a dependency.
#'
#' @section Public members / active bindings:
#' * `type`          :: `character(1)`
#'   Name / type of the condition. Read-only.
#' * `fun`           :: `function(lhs, rhs) -> logical(1)`
#'   Function to check if condition is satisfied.
#'   The `lhs` during check will be bound to the value of the parent param,
#'   the `rhs` to the defined constant in this object.
#'   Read-only.
#' * `rhs`           :: `any`
#'   The right-hand-side of the condition, that we compare to, to check if it is satisfied.
#'
#' @section Currently implemented simple conditions:
#' * `cond_equal(rhs)`
#'   Parent must be equal to `rhs`.
#' * `cond_any(rhs)`
#'   Parent must be any value of `rhs`.
#'
#' @name Dependency
#' @aliases cond_equal cond_anyof
#' @export
Condition = R6Class("Condition",
  public = list(
    rhs = NULL,

    initialize = function(type, fun, rhs) {
      private$.type = assert_string(type)
      private$.fun = assert_function(fun)
      self$rhs = rhs
    },

    eval = function(parent_val) self$fun(parent_val, self$rhs)
  ),

  active = list(
    type = function() private$.type,
    fun = function() private$.fun
  ),

  private = list(
    .type = NULL,
    .fun = NULL
  )
)

#' @export
cond_equal = function(rhs) Condition$new("equal", identical, rhs)

#' @export
cond_anyof = function(rhs) Condition$new("anyof", test_choice, rhs)
