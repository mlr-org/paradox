#' @title Sampler
#' @format [R6Class] object. Abstract base class.
#'
#' @description
#' Random sampler for an arbitrary [ParamSet].
#'
#' @section Public members / active bindings:
#' * `param_set`            :: [ParamSet] \cr
#'   Domain / support of the distribution we want to sample from.
#'
#' @section Public methods:
#' * `new(param_set)` \cr
#'   [ParamSet] -> `self` \cr
#'   Abstract, only inheriting subclasses call this.
#'   Param set is cloned on construction.
#' * `sample(n)` \cr
#'   `integer(1)` -> [Design] \cr
#'   Sample n values from the distribution.
#'
#' @section Private methods / Internals:
#' * `.sample(n)` \cr
#'   `integer(1)` -> [data.table] \cr
#'   Inheriting sublcasses have to implement thus, called from `sample()`
#'
#' @name Sampler
#' @family Sampler
#' @export

Sampler = R6Class("Sampler",
  public = list(
    param_set = NULL,

    # params.cl allows asserting params of only a certain type, vector of multiple entries is OK
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
