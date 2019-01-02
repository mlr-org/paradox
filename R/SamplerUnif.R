#' @title Sampler: Multivariate uniform.
#' @format [R6Class] object. Inherits from [SamplerHierachical].
#'
#' @description
#' Uniform random sampling for arbitrary (bounded) param sets.
#' Constructs 1 uniform sampler per param, then passes them to [SamplerHierachical].
#' Hence, also works for param sets with dependencies.
#'
#' @section Public methods:
#' * `new(param_set)` \cr
#'   [ParamSet] -> `self`
#'
#' @name SamplerUnif
#' @family Sampler
#' @export
SamplerUnif = R6Class("SamplerUnif", inherit = SamplerHierachical,
  public = list(
    initialize = function(param_set) {
      assert_paramset(param_set, must_bounded = TRUE, no_deps = TRUE, no_untyped = TRUE)
      samplers = lapply(param_set$params, Sampler1DUnif$new)
      super$initialize(param_set, samplers)
    }
  )
)

