#' @title Sampler: Multivariate hierarchical.
#' @format [R6Class] object. Inherits from [Sampler].
#'
#' @description
#' Hierarchical sampling for arbitrary param sets with dependencies, where the user specifies 1D samplers per param.
#' Dependencies are topologically sorted, parameters are then sampled in topological order,
#' and if dependencies do not hold, values are set to NA in the resulting data.table.
#'
#' @section Public methods:
#' * `new(param_set, samplers)` \cr
#'   [ParamSet], list of [Sampler] -> `self` \cr
#'   User has to pass one [Sampler1D] per param in set, which specifies its distribution.
#'
#' @name SamplerHierarchical
#' @family Sampler
#' @export
SamplerHierarchical = R6Class("SamplerHierarchical", inherit = Sampler,
  public = list(
    samplers = NULL,

    initialize = function(param_set, samplers) {
      assert_paramset(param_set, no_untyped = TRUE)
      assert_list(samplers, types = "Sampler1D")
      ids1 = param_set$ids()
      ids2 = map_chr(samplers, function(s) s$param$id)
      if (!setequal(ids1, ids2))
        stop("IDs of params in samplers to not correspond to IDs of params in set!")
      super$initialize(param_set)
      self$samplers = samplers
    }
  ),
  private = list(
    # samples independently from the 1d distributions
    # dependencies are actually handled when in "sample" we create the Design, then set entries to NA
    .sample = function(n) map_dtc(self$samplers, function(s) s$sample(n)$data)
  )
)




