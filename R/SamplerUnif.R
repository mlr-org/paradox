#' @title Sampler: Multivariate uniform.
#' @format [R6Class] object. Inherits from [SamplerJointIndep].
#'
#' @description
#' Uniform random sampling for arbitrary (bounded) param sets.
#' Constructs 1 uniform sampler per param, then passes them to [SamplerJointIndep].
#'
#' @section Public methods:
#' * `new(param_set)` \cr
#'   [ParamSet] -> `self`
#'
#' @name SamplerUnif
#' @family Sampler
#' @export
SamplerUnif = R6Class("SamplerUnif", inherit = SamplerJointIndep,
  public = list(
    initialize = function(param_set) {
      assert_r6(param_set, "ParamSet")
      samplers = lapply(param_set$params, Sampler1DUnif$new)
      super$initialize(samplers)
    }
  )
)

