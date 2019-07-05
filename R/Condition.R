#' @title Dependency Condition
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Condition object, to specify the condition in a dependency.
#'
#' @section Construction:
#'
#' c = Condition$new(type, rhs)
#'
#' * `type::character(1)` \cr
#'   Name / type of the condition. Read-only.
#'
#' * `rhs::any` \cr
#'   Right-hand-side of the condition.
#'
#' @section Methods:
#'
#' * `test(function(x))`\cr
#'   `??? -> logical(n)` \cr
#'   Checks if condition is satisfied.
#'   Called on a vector of parent param values.
#'
#' @section Currently implemented simple conditions:
#' * `CondEqual$new(rhs)` \cr
#'   Parent must be equal to `rhs`.
#' * `CondAnyOf$new(rhs)` \cr
#'   Parent must be any value of `rhs`.
#'
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
    type = function() private$.type,
    rhs = function() private$.rhs
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
    test = function(x) !is.na(x) & x == private$.rhs
  )
)

#' @export
CondAnyOf = R6Class("CondAnyOf", inherit = Condition,
  public = list(
    initialize = function(rhs) super$initialize("anyof", rhs),
    test = function(x) !is.na(x) & x %in% private$.rhs
  )
)
