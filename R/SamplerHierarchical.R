#' @title SamplerHierarchical Class
#'
#' @description
#' Hierarchical sampling for arbitrary param sets with dependencies, where the user specifies 1D samplers per param.
#' Dependencies are topologically sorted, parameters are then sampled in topological order,
#' and if dependencies do not hold, values are set to `NA` in the resulting `data.table`.
#'
#' @template param_param_set
#'
#' @family Sampler
#' @include Sampler.R
#' @export
SamplerHierarchical = R6Class("SamplerHierarchical", inherit = Sampler,
  public = list(
    #' @field samplers (`list()`)\cr
    #' List of [`Sampler1D`] objects that gives a Sampler for each dimension in the `param_set`.
    samplers = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param param_set ([`ParamSet`])\cr
    #'   The [`ParamSet`] to associated with this `SamplerHierarchical`.
    #' @param samplers (`list()`)\cr
    #'   List of [`Sampler1D`] objects that gives a Sampler for each dimension in the `param_set`.
    initialize = function(param_set, samplers) {
      assert_param_set(param_set, no_untyped = TRUE)
      assert_list(samplers, types = "Sampler1D")
      ids1 = param_set$ids()
      ids2 = map_chr(samplers, function(s) s$param$ids())
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
