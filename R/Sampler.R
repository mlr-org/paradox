#' @title Sampler
#' @format [R6Class] object. Abstract base class.
#'
#' @description
#' Random sampler for an arbitrary [ParamSet].
#'
#' @section Public members / active bindings:
#' * `param_set`            :: [ParamSet]
#'   Domain / support of the distribution we want to sample from.
#'
#' @section Public methods:
#' * `new(param_set)` \cr
#'   [ParamSet] -> `self`
#'   Abstract, only inheriting subclasses call this.
#' * `sample(n)` \cr
#'   `integer(1)` -> [data.table]
#'   Sample n values from the distribution.
#'
#' @section Private methods / Internals:
#' * `.sample(n)` \cr
#'   `integer(1)` -> [data.table]
#'   Inheriting sublcasses have to implement thus, called from `sample()`
#'
#' @name Sampler
#' @family Sampler
#' @export

#FIXME: deepclone on contruction?
# FIXME: rename 1Dfct to 1dcat? and doc that this also works with lgl?

Sampler = R6Class("Sampler",
  public = list(
    param_set = NULL,

    # params.cl allows asserting params of only a certain type, vector of multiple entries is OK
    initialize = function(param_set, params.cl = "Param") {
      assert_paramset(param_set, no_untyped = TRUE)
      assert_subset(param_set$pclasses, params.cl)
      self$param_set = param_set
    },

    sample = function(n) {
      assert_count(n, positive = TRUE) # we do argcheck on toplevel
      private$.sample(n)
    }
  ),
  private = list(
    .sample = function(n) stop("abstract") # inheriting classes have to implement this
  )
)

