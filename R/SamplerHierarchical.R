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
    #'   The [`ParamSet`] to associate with this `SamplerHierarchical`.
    #' @param samplers (`list()`)\cr
    #'   List of [`Sampler1D`] objects that gives a Sampler for each dimension in the `param_set`.
    initialize = function(param_set, samplers) {
      assert_r6(param_set, "ParamSet")
      # Own the source generation before consulting arbitrary Sampler
      # subclasses. A custom `$param` binding may allocate or run user code;
      # validating IDs first and cloning afterward could otherwise install a
      # later, incompatible ParamSet generation.
      assert_list(samplers, types = "Sampler1D")
      owned = param_set$clone(deep = TRUE)
      assert_param_set(owned, no_untyped = TRUE)
      ids1 = owned$ids()
      ids2 = map_chr(samplers, function(s) s$param$ids())
      if (length(ids1) != length(ids2) ||
          anyDuplicated(ids2) != 0L ||
          !setequal(ids1, ids2)) {
        stop("IDs of params in samplers do not correspond to IDs of params in set!")
      }
      self$param_set = owned
      self$samplers = samplers
    }
  ),
  private = list(
    # samples independently from the 1d distributions
    # dependencies are actually handled when in "sample" we create the Design, then set entries to NA
    .sample = function(n) map_dtc(self$samplers, function(s) s$sample(n)$data)
  )
)
