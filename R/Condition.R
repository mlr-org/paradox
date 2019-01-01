#' @title Dependency condition
#'
#' @description
#' Condition object, to specify the condition in a dependency.
#'
#' @section Public members / active bindings:
#' * `type`          :: `character(1)`
#'   Name / type of the condition. Read-only.
#'
#' @section Public methods:
#' * `new(type, rhs)` \cr
#'   `character(1)`, `any` -> `self`
#'   Abstract constructor, called by inheriting subclasses.
#' * `test`          :: `function(x) -> logical(n)`
#'   Checks if condition is satisfied.
#'   Called on a vector of parent param values.
#'
#' @section Currently implemented simple conditions:
#' * `CondEqual$new(rhs)`
#'   Parent must be equal to `rhs`.
#' * `CondAnyOf$new(rhs)`
#'   Parent must be any value of `rhs`.
#'
#' @name Condition
#' @aliases CondEqual CondAnyOf
#' @export
Condition = R6Class("Condition",
  public = list(
    initialize = function(type, rhs) {
      private$.type = assert_string(type)
      private$.rhs = rhs
    },

    test = function(x) stop("abstract")
  ),

  active = list(
    type = function() private$.type
  ),

  private = list(
    .type = NULL,
    .rhs = NULL
  )
)

#' @export
CondEqual = R6Class("CondEqual", inherit = Condition,
  public = list(
    initialize = function(rhs) super$initialize("equal", rhs),
    test = function(x) x == private$.rhs
  )
)

#' @export
CondAnyOf = R6Class("CondAnyOf", inherit = Condition,
  public = list(
    initialize = function(rhs) super$initialize("anyof", rhs),
    test = function(x) x %in% private$.rhs
  )
)

