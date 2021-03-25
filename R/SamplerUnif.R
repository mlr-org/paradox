#' @title SamplerUnif Class
#'
#' @description
#' Uniform random sampling for an arbitrary (bounded) [ParamSet].
#' Constructs 1 uniform sampler per [Param], then passes them to [SamplerHierarchical].
#' Hence, also works for [ParamSet]s sets with dependencies.
#'
#' @template param_param_set
#'
#' @family Sampler
#' @include SamplerHierarchical.R
#' @export
SamplerUnif = R6Class("SamplerUnif", inherit = SamplerHierarchical,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(param_set) {
      assert_param_set(param_set, must_bounded = TRUE, no_deps = FALSE, no_untyped = TRUE)
      samplers = lapply(param_set$subspaces(), Sampler1DUnif$new)
      super$initialize(param_set, samplers)
    }
  )
)
