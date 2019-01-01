#' @title Sampler: Multivariate joint independent.
#' @format [R6Class] object. Inherits from [Sampler].
#'
#' @description
#' Create joint, independent sampler out of multiple other samplers.
#'
#' @section Public members / active bindings:
#' * `param_set`            :: [ParamSet]
#'    Combined param sets all passed samplers.
#'
#' @section Public methods:
#' * `new(samplers)` \cr
#'   list of [Sampler] -> `self`
#'
#' @name SamplerJointIndep
#' @family Sampler
#' @export
SamplerJointIndep = R6Class("SamplerJointIndep", inherit = Sampler,
  public = list(
    samplers = NULL,

    initialize = function(samplers) {
      assert_list(samplers, types = "Sampler")
      self$samplers = samplers
      pss = map(samplers, "param_set")
      self$param_set = Reduce(function(ps1, ps2) ps1$add(ps2), pss)
    }
  ),

  private = list(
      # FIXME: unname should ne needed, added an issue in ml3misc
    .sample = function(n) map_dtc(unname(self$samplers), function(s) s$sample(n)),
    .print = function() catf("Independent comps: %i", length(self$samplers))
  )
)





