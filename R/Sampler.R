
# base class for all samplers. stores a list of params that it refers to.
# offers a method to sample n values from and returns a dt
Sampler = R6Class("Sampler",
  public = list(
    # member variables
    param_set = NULL, # param_set that the sampler refers to

    # params.cl allows asserting params of only a certain type, vector of multiple entries is OK
    initialize = function(param_set, params.cl = "Param") {
      assert_r6(param_set, "ParamSet")
      assert_subset(param_set$pclasses, params.cl)
      self$param_set = param_set
    },

    sample = function(n) {
      stopf("Abstract base method. Not implemented in class '%s'", class(self))
    }
  )
)

Sampler1D = R6Class("Sampler1D", inherit = Sampler,

  public = list(
    initialize = function(param, param.cl)  {
      super$initialize(ParamSet$new(list(param)), params.cl = param.cl)
    }
  ),

  active = list(
    # retrieve the only param in the set, return Param object
    param = function() self$param_set$params[[self$param_set$ids[1L]]]
  ),

  private = list(
    as_dt_col = function(x) set_names(data.table(x), self$param$id)
  )

)

# static method
new_1d_unif = function(param) {
  # not so great code here with the switch-on-class, but i think we live with this
  switch(class(param)[1L],
    ParamDbl = Sampler1DDblUnif$new(param),
    ParamInt = Sampler1DIntUnif$new(param),
    ParamFct = Sampler1DFct$new(param),
    ParamLgl = Sampler1DFct$new(param),
    stopf("Sampler not implemented for param of type: %s", class(param)[1L])
  )
}


# samples from a 1D real-values, from an arbitrary distribution. default is uniform
# note that we always sample from the truncated distribution
Sampler1DNumber = R6Class("Sampler1DDbl", inherit = Sampler1D,
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
      assert_count(n, positive = TRUE)
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

Sampler1DDblUnif = R6Class("Sampler1DDblUnif", inherit = Sampler1DNumber,
  public = list(
    initialize = function(param) {
      super$initialize(param, "ParamDbl", trunc = FALSE,
        rfun = function(n) runif(n, min = self$param$lower, max = self$param$upper))
      assert_true(param$is_bounded)
    }
  )
)

# samples from a (truncated) normal distribution
Sampler1DDblNorm = R6Class("Sampler1DDblNorm", inherit = Sampler1DNumber,

  public = list(
    # member variables
    mean = NULL,
    sd = NULL,

    initialize = function(param, mean = NULL, sd = NULL) {
      super$initialize(param, trunc = TRUE,
        rfun = function(n) rnorm(n, mean = mean , sd = sd))
      if (is.null(mean))
        mean = self$param$map_unitint_to_values(0.5)
      assert_number(mean)
      assert_number(sd, lower = 0)
      self$mean = mean
      self$sd = sd
      assert_true(param$is_bounded)
    }
  )
)

# samples 1D ints, between lower and upper
Sampler1DIntUnif = R6Class("Sampler1DIntUnif", inherit = Sampler1DNumber,
  public = list(
    initialize = function(param) {
      super$initialize(param, "ParamInt", trunc = FALSE,
        rfun = function(n) as.integer(round(runif(n, self$param$lower - 0.5, max = param$upper + 0.5))))
      assert_true(param$is_bounded)
    }
  )
)

# samples from a categorical distribution, default is uniform with equal weights
Sampler1DFct = R6Class("Sampler1DFct", inherit = Sampler1D,
  public = list(
    # member variables
    prob = NULL,

    initialize = function(param, prob = NULL) {
      super$initialize(param, c("ParamFct", "ParamLgl"))
      k = param$nlevels
      if (is.null(prob))
        prob = rep(1/k, k)
      assert_numeric(prob, lower = 0, upper = 1, len = k)
      assert_true(all.equal(sum(prob), 1))
      self$prob = prob
    },

    sample = function(n) {
      assert_count(n, positive = TRUE)
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
      assert_count(n, positive = TRUE)
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
      assert_r6(param_set, "ParamSet")
      samplers = lapply(param_set$params, new_1d_unif)
      super$initialize(samplers)
    }
  )
)




