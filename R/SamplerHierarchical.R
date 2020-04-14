#' @title SamplerHierarchical Class
#'
#' @usage NULL
#' @format [R6::R6Class] inheriting from [Sampler].
#'
#' @description
#' Hierarchical sampling for arbitrary param sets with dependencies, where the user specifies 1D samplers per param.
#' Dependencies are topologically sorted, parameters are then sampled in topological order,
#' and if dependencies do not hold, values are set to `NA` in the resulting `data.table`.
#'
#' @section Construction:
#' ```
#' smpl = SamplerHierarchical$new(param_set, samplers)
#' ```
#'
#' * `param_set` :: [ParamSet]\cr
#'   Domain / support of the distribution we want to sample from.
#' * `samplers` :: `list()`\cr
#'   List of [Sampler1D] objects that gives a Sampler for each [Param] in the `param_set`.
#'
#' @section Fields:
#' See [Sampler].
#' Additionally, the class provides:
#' * `samplers` :: `list()`\cr
#'   List of [Sampler1D] objects that gives a Sampler for each [Param] in the `param_set`.
#'
#' @section Methods:
#' See [Sampler].
#'
#' @family Sampler
#' @include Sampler.R
#' @export
SamplerHierarchical = R6Class("SamplerHierarchical", inherit = Sampler,
  public = list(
    samplers = NULL,

    initialize = function(param_set, samplers) {
      assert_param_set(param_set, no_untyped = TRUE)
      assert_list(samplers, types = "Sampler1D")
      ids1 = param_set$ids()
      ids2 = map_chr(samplers, function(s) s$param$id)
      if (!setequal(ids1, ids2)) {
        stop("IDs of params in samplers to not correspond to IDs of params in set!")
      }
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
