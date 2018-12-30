#' @title Param Dependency
#'
#' @description
#' A dependency object for a parameter, connects a subordinate parameter with
#' its superordinate parent, and stores a condition on the parents value.
#' In simpler words: we can encode stuff like "foo is only valid, if bar='a'".
#'
#' @section Public members / active bindings:
#' * `param`             :: [Param]
#'   The dependent param.
#' * `parent`            :: [Param]
#'   The (categorical) param this param depends on.
#' * `cond`              :: [Condition]
#'   Condition of the dependency.
#'
#' @name Dependency
#' @export
Dependency = R6Class("Dependency",
  public = list(
    param = NULL,
    parent = NULL,
    cond = NULL,

    initialize = function(param, parent, cond) {
      self$param = assert_param(param)
      self$parent = assert_param(parent, cl = c("ParamFct", "ParamLgl"))
      self$cond = assert_r6(cond, "Condition")
    }
  )
)

