#' @title Sampler: Multivariate hierachical.
#' @format [R6Class] object. Inherits from [Sampler].
#'
#' @description
#' Hierarchical sampling for arbitrary param sets with dependencies, where the user specifies 1D samplers per param.
#' Dependencies are topologically sorted, parameters are then sampled in topological order,
#' and if dependencies do not hold, values are set to NA in the resulting data.table.
#'
#' @section Public methods:
#' * `new(param_set, samplers)` \cr
#'   [ParamSet], list of [Sampler] -> `self`
#'   User has to pass one [Sampler1D] per param in set, which specifies its distribution.
#'
#' @name SamplerHierachical
#' @family Sampler
#' @export
SamplerHierachical = R6Class("SamplerHierachical", inherit = Sampler,
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
      private$.deps_on = param_set$deps_on # compute that once
      graph = private$.deps_on[,1:2]
      colnames(graph) = c("id", "parents")
      private$.topo = topo_sort(graph)
      self$samplers = samplers[private$.topo$id] # store samplers sorted
      private$.deps_on = param_set$deps_on # compute that once
    }
  ),

  private = list(
    .deps_on = NULL,
    .topo = NULL,

    .sample = function(n) {
      k = length(self$samplers)
      res = named_list(self$param_set$ids()) # result, list of sampled cols
      for (j in 1:k) { # walk thru all sampler, toposorted order, and sample a col of size n
        s = self$samplers[[j]]
        param_id = s$param$id
        # FIXME: could we call .sample here?
        x = s$sample(n)$data
        # walk thru all deps (they were sampled before us, topo!),
        # and set values in x to NA which where the dep is not OK
        deps = private$.deps_on[param_id, on = "id"]$deps[[1L]]
        for (d in deps) {
          pcol = res[[d$parent$id]][[1]] # NB: res is a list of dts (with ncol==1)
          is_ok = !is.na(pcol) & d$cond$test(pcol) # we are ok if parent was active and cond on parent is OK
          x[!is_ok] = as(NA, d$param$storage_type)
        }
        res[[param_id]] = x
      }
      res = do.call(cbind, unname(res))
      return(res)
    }
  )
)




