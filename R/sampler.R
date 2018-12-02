
# base class for all samplers. stores a list of params that it refers to.
# offers a method to sample n values from and returns a dt
Sampler = R6Class("Sampler",
  public = list(
    # member variables
    params = NULL, # params that the sampler refers to

    initialize = function(params, params.cl = "Parameter") {
      assert_list(params, "Parameter")
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
    as_dt_col = function(x) as_dt_cols(list(x), names = self$param$id)
  )

)

# static method
Sampler1D$new_1d_unif = function(param) {
  # not so great code here with the switch-on-class, but i think we live with this
  switch(class(param)[1L],
    ParamFloat = Sampler1DFloatUnif$new(param),
    ParamInt = Sampler1DIntUnif$new(param),
    ParamCateg = Sampler1DCat$new(param),
    ParamBool = Sampler1DCat$new(param),
    stopf("Sampler not implemented for param of type: %s", class(param)[1L])
  )
}


# samples from a 1D real-values, from an arbitrary distribution. default is uniform
# note that we always sample from the truncated distribution
Sampler1DNumber = R6Class("Sampler1DFloat", inherit = Sampler1D,
  public = list(

    # member variables
    rfun = NULL,
    trunc = NULL,

    initialize = function(param, param.cl, rfun, trunc = TRUE) {
      super$initialize(param, param.cl)
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
      super$as_dt_col(s)
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

Sampler1DFloatUnif = R6Class("Sampler1DFloatUnif", inherit = Sampler1DNumber,
  public = list(
    initialize = function(param) {
      super$initialize(param, "ParamRa",  trunc = FALSE,
        rfun = function(n) runif(n, min = self$param$lower, max = self$param$upper))
      assert_true(param$has_finite_bounds)
    }
  )
)

Sampler1DFloatNorm = R6Class("Sampler1DFloatNorm", inherit = Sampler1DNumber,

  public = list(
    # member variables
    mu = NULL,
    sd = NULL,

    initialize = function(param, mu = NULL, sd = NULL) {
      super$initialize(param, trunc = FALSE,
        rfun = function(n) rnorm(n, min = self$param$lower, max = self$param$upper))
      if (is.null(mu))
        mu = self$param$center
      assert_number(mu)
      assert_number(sd, lower = 0)
      self$mu = mu
      self$sd = sd
    }
  )
)

# samples 1D ints, between lower and upper
Sampler1DIntUnif = R6Class("Sampler1DIntUnif", inherit = Sampler1DNumber,
  public = list(
    initialize = function(param) {
      super$initialize(param, "ParamInt", trunc = FALSE,
        rfun = function(n) as.integer(round(runif(n, self$param$lower - 0.5, max = param$upper + 0.5))))
      assert_true(param$has_finite_bounds)
    }
  )
)

# samples 1D ints, between lower and upper
Sampler1DIntGeom = R6Class("Sampler1DIntGeom", inherit = Sampler1DNumber,
  public = list(
    initialize = function(param) {
      super$initialize(param, "ParamInt")
    }
  )
)


# samples from a categorical distribution, default is uniform with equal weights
Sampler1DCat = R6Class("Sampler1DCat", inherit = Sampler1D,
  public = list(
    # member variables
    prob = NULL,

    initialize = function(param, prob = NULL) {
      super$initialize(param, "ParamCateg")
      k = param$nlevels
      if (is.null(prob))
        prob = rep(1/k, k)
      assert_numeric(prob, lower = 0, upper = 1, len = k)
      assert_true(all.equal(sum(prob), 1))
      self$prob = prob
    },

    sample = function(n) {
      s = sample(self$param$values, n, replace = TRUE, prob = self$prob)
      super$as_dt_col(s)
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
      names(s) = NULL
      do.call(cbind, s)
    }
  )
)

# creates a joint, independent sampler out of multiple samplers
SamplerUnif = R6Class("SamplerUnif", inherit = SamplerJointIndep,
  public = list(
    initialize = function(param_set) {
      samplers = lapply(param_set$params, Sampler1D$new_1d_unif)
      super$initialize(samplers)
    }
  )
)




