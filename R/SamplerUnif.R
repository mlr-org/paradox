#' @title SamplerUnif Class
#'
#' @usage NULL
#' @format [R6::R6Class] inheriting from [SamplerHierarchical].
#'
#' @description
#' Uniform random sampling for an arbitrary (bounded) [ParamSet].
#' Constructs 1 uniform sampler per [Param], then passes them to [SamplerHierarchical].
#' Hence, also works for [ParamSet]s sets with dependencies.
#'
#' @section Construction:
#' See [Sampler].
#'
#' @section Fields:
#' See [Sampler].
#'
#' @section Methods:
#' See [Sampler].
#'
#' @family Sampler
#' @include SamplerHierarchical.R
#' @export
SamplerUnif = R6Class("SamplerUnif", inherit = SamplerHierarchical,
  public = list(
    initialize = function(param_set) {
      assert_param_set(param_set, must_bounded = TRUE, no_deps = FALSE, no_untyped = TRUE)
      samplers = lapply(param_set$params, Sampler1DUnif$new)
      super$initialize(param_set, samplers)
    }
  )
)
