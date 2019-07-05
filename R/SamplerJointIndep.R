#' @title SamplerJointIndep Class
#'
#' @usage NULL
#' @format [R6::R6Class] inheriting from [Sampler].
#'
#' @description
#' Create joint, independent sampler out of multiple other samplers.
#'
#' @section Construction:
#' ```
#' smpl = SamplerJointIndep$new(samplers)
#' ```
#'
#' * `samplers` :: `list()`\cr
#'   List of [Sampler] objects.
#'
#' @section Fields:
#' See [Sampler].
#' Additionally, the class provides:
#' * `samplers` :: `list()`\cr
#'   List of [Sampler] objects.
#'
#' @section Methods:
#' See [Sampler].
#'
#' @family Sampler
#' @export
SamplerJointIndep = R6Class("SamplerJointIndep", inherit = Sampler,
  public = list(
    samplers = NULL,

    initialize = function(samplers) {
      assert_list(samplers, types = "Sampler")
      self$samplers = samplers
      pss = map(samplers, "param_set")
      # FIXME: maybe we should use a paramset collection here?
      pss[[1L]] = pss[[1]]$clone() # we need to clone, add will clone later, too, otherwise we change the 1set in place
      self$param_set = Reduce(function(ps1, ps2) ps1$add(ps2), pss)
      assert_param_set(self$param_set, no_deps = TRUE) # must_bounded and untyped should be check by the sapler, or if the sampler still works, then ok
    }
  ),

  private = list(
    # FIXME: would be nice if we could call .sample here instead of sample, for less type conversion and peed, but .sample is private. make it public? also not great...
    .sample = function(n) map_dtc(self$samplers, function(s) s$sample(n)$data),
    .print = function() catf("Independent comps: %i", length(self$samplers))
  )
)
