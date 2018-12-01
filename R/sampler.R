
# base class for all samplers. stores a list of params that it refers to.
# offers a method to sample n values from and returns a dt
Sampler = R6Class("Sampler",
  public = list(
    # member variables
    params = NULL, # params that the sampler refers to

    initialize = function(params, params.cl = "ParamBase") {
      assert_list(params, "ParamBase")
      self$params = params
    },

    sample = function(n) {
      stopf("Abstract base method. Not implemented in class '%s'", class(self))
    }
  )
)

Sampler1D = R6Class("Sampler1D", inherit = Sampler,

  public = list(
    initialize = function(param, cl)  {
      super$initialize(list(param), params.cl = cl)
    }
  ),

  active = list(
    param = function() self$params[[1L]]
  ),

  private = list(
    return_dt = function(x) {
      dt = as.data.frame(x)
      names(dt) = self$param$id
      setDT(dt)
    }
  )

)


# samples from a 1D real-values, from an arbitrary distribution. default is uniform
# note that we always sample from the truncated distribution
Sampler1DReal = R6Class("Sampler1DReal", inherit = Sampler1D,
  public = list(

    # member variables
    rfun = NULL,
    trunc = NULL,

    initialize = function(param, rfun, trunc = TRUE) {
      super$initialize(param, "ParamReal")
      assert_function(rfun, args = "n")
      self$rfun = rfun
      assert_flag(trunc)
      self$trunc = trunc
    },

    # maybe we want an option to use my truncation here, as this slows stuff down somewhat and
    # there are some real truncated rngs in R
    sample = function(n) {
      if (self$trunc)
        s = sample_truncated(n, self$rfun)
      else
        s = self$rfun(n = n)
      super$return_dt(s)
    },

    sample_truncated = function(n, rfun) {
      for (i in 1:1000) {
        s = rfun(n = 2*n)
        s = s[s >= self$param$lower & s <= self$param$upper]
        r = c(r, s)
        if (length(r) >= n)
          return(r[1:n])
      }
      stopf("Tried rejection sampling 1000x. Giving up.")
    }
  )
)

Sampler1DRealUnif = R6Class("Sampler1DRealUni", inherit = Sampler1DReal,
  public = list(
    initialize = function(param) {
      super$initialize(param, trunc = FALSE,
        rfun = function(n) runif(n, min = self$param$lower, max = self$param$upper))
    }
  )
)


# samples from a categorical distribution, default is uniform with equal weights
Sampler1DCat = R6Class("Sampler1DCat", inherit = Sampler1D,
  public = list(
    # member variables
    prob = NULL,

    initialize = function(param, prob = NULL) {
      super$initialize(param, "ParamCategorical")
      k = param$nlevels
      if (is.null(prob))
        prob = rep(1/k, k)
      assert_numeric(prob, lower = 0, upper = 1, len = k)
      assert_true(all.equal(sum(prob), 1))
      self$prob = prob
    },

    sample = function(n) {
      s = sample(self$param$values, n, replace = TRUE, prob = self$prob)
      super$return_dt(s)
    }
  )
)


# creates a joint, independent sampler out of multiple samplers
SamplerJointIndep = R6Class("SamplerJointIndep", inherit = Sampler,
  public = list(
    # member variables
    samplers = NULL,

    initialize = function(samplers) {
      assert_list(samplers, types = "Sampler")
      self$samplers = samplers
    },

    sample = function(n) {
      # FIXME: should use map_dtc here? doesnt work, reported in mlr3misc
      s = lapply(self$samplers, function(s) s$sample(n))
      do.call(cbind, s)
    }
  )
)
