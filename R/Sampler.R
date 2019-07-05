#' @title Sampler Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' This is the abstract base class for sampling objects like [Sampler1D], [SamplerHierarchical] and [SamplerJointIndep].
#'
#' @section Construction:
#' Note: This object is typically constructed via a derived classes, e.g. [Sampler1D], [SamplerHierarchical] or [SamplerJointIndep].
#'
#' ```
#' smpl = Sampler$new(param_set)
#' ```
#'
#' * `param_set` :: [ParamSet]\cr
#'   Domain / support of the distribution we want to sample from.
#'   ParamSet is cloned on construction.
#'
#' @section Fields:
#' * `param_set` :: [ParamSet]\cr
#'   Domain / support of the distribution we want to sample from.
#'
#' @section Methods:
#' * `sample(n)` \cr
#'   `integer(1)` -> [Design] \cr
#'   Sample n values from the distribution.
#'
#' @family Sampler
#' @export
Sampler = R6Class("Sampler",
  public = list(
    param_set = NULL,

    initialize = function(param_set) {
      assert_param_set(param_set, no_untyped = TRUE)
      self$param_set = param_set$clone(deep = TRUE)
    },

    sample = function(n) {
      assert_count(n, positive = TRUE) # we do argcheck on toplevel
      Design$new(self$param_set, private$.sample(n), remove_dupl = FALSE) # user wants n points, dont remove
    },

    print = function(...) {
      catf("Sampler: %s", class(self)[[1L]])
      catf("For params: %s", str_trunc(str_collapse(self$param_set$ids()), width = 40L))
      private$.print()
    }
  ),
  private = list(
    .sample = function(n) stop("abstract"), # inheriting classes have to implement this
    .print = function() {
    } # inheriting classes can overwrite to add lines
  )
)
