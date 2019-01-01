SamplerH = R6Class("SamplerH", inherit = Sampler,
  public = list(
    samplers = NULL,
    topo = NULL,

    initialize = function(param_set, samplers) {
      assert_paramset(param_set, no_untyped = TRUE)
      assert_list(samplers, types = "Sampler1D")
      # FIXME: check that samplers correspond to ps
      super$initialize(param_set)
      graph = self$param_set$deps_on
      colnames(graph) = c("id", "parents")
      self$topo = topo_sort(graph)
      self$samplers = samplers[self$topo$id] # store samplers sorted
    }
  ),

  private = list(
    .sample = function(n) {
      k = length(self$samplers)
      res = list()
      deps = self$param_set$deps_on
      for (j in 1:k) {
        s = self$samplers[[j]]
        id = s$param$id
        print(s)
        x = s$sample(n)
        # pars = deps[[id, "deps_parents"]]
        for (d in self$param_set$deps) {
          if (d$param$id == id) {
            pid = d$param$parent$id
            cond = d$cond
            is_ok
          }
          res[[p]]
        }
        res[[id]] = x
      }
      res = do.call(cbind, res)
      return(res)
    }
  )
)




