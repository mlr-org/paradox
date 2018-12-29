#' @title Sampler
#' @format [R6Class] object. Inherits from [Sampler].
#'
#' @description
#' 1D sampler, abstract base class and inheriting concrete implementations.
#'
#' @section Public members / active bindings:
#' * `param`            :: [Param]
#'   Quick access to the one param in the set.
#'
#' @section Currently implenented samplers:
#' * `Sampler1DDblUnif$new(param)` \cr
#'   Uniform random for (bounded) doubles.
#' * `Sampler1DIntUnif$new(param)` \cr
#'   Uniform random for (bounded) integer.
#' * `Sampler1DFct$new(param, prob = NULL)` \cr
#'   Categorical distribution, for a fct or lgl param.
#'   `prob` is a numeric vector of `nlevels` probabilities, which is uniform by default.
#' * `Sampler1DDblNorm$new(param)` \cr
#'   Normal sampling (truncated) for doubles.
#'   Has member variables `mean` and 'sd' which you can change to influence sampling,
#'   they are initialized to `mean=mean(range)` and `sd=span/4`.
#' * `Sampler1DRfun(param, rfun, trunc = TRUE)` \cr
#'   Arbitrary sampling from 1D rng functions from R.
#'   Pass e.g. rfun=rexp to sample from exponential distribution.
#'   `trunc = TRUE` enables naive rejection sampling, so we stay inside of \[lower, upper\].
#' * `new_sampler_1d_unif(param)` \cr
#'   Creates an approriate 1D uniform sampler from above for an arbitrary param.
#'
#' @name Sampler1D
#' @aliases Sampler1DDblUnif Sampler1DIntUnif Sampler1DFct Sampler1DDblNorm Sampler1DRfun new_sampler_1d_unif
#' @family Sampler
#' NULL

#FIXME: convert to storage type and unittests


#' @export
Sampler1D = R6Class("Sampler1D", inherit = Sampler, # abstract base class
  public = list(
    initialize = function(param, param.cl)  { # we can assert that param is only from classes in param.cl
      super$initialize(ParamSet$new(list(param)), params.cl = param.cl)
    }
  ),

  active = list(
    param = function() self$param_set$params[[1]]
  ),

  private = list(
    # create a 1-col-dt, named by param-id, from a data vector (from sampling), and enforce storage type
    as_dt_col = function(x) {
      x = as(x, self$param$storage_type)
      set_names(data.table(x), self$param$id)
    }
  )
)

#' @export
Sampler1DRfun = R6Class("Sampler1DRfun", inherit = Sampler1D,
  public = list(
    rfun = NULL,
    trunc = NULL,

    initialize = function(param, param.cl, rfun, trunc = TRUE) {
      super$initialize(param, param.cl)
      assert_function(rfun, args = "n")
      assert_flag(trunc)
      self$rfun = rfun
      self$trunc = trunc
    }
  ),

  private = list(
    # maybe we want an option to use my truncation here, as this slows stuff down somewhat and there are some real truncated rngs in R
    .sample = function(n) {
      if (self$trunc)
        s = sample_truncated(n, self$rfun)
      else
        s = self$rfun(n = n)
      super$as_dt_col(s)
    },

    # extreme naive rejection sampling to enable trunc sampling from finite, restricted support
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

#' @export
Sampler1DDblUnif = R6Class("Sampler1DblUnif", inherit = Sampler1DRfun,
  public = list(
    initialize = function(param) {
      super$initialize(param, "ParamDbl", trunc = FALSE,
        rfun = function(n) runif(n, min = self$param$lower, max = self$param$upper))
      assert_true(param$is_bounded)
    }
  )
)

#' @export
Sampler1DIntUnif = R6Class("Sampler1DIntUnif", inherit = Sampler1DRfun,
  public = list(
    initialize = function(param) {
      super$initialize(param, "ParamInt", trunc = FALSE,
        rfun = function(n) as.integer(round(runif(n, self$param$lower - 0.5, max = param$upper + 0.5))))
      assert_true(param$is_bounded)
    }
  )
)

#' @export
Sampler1DFct = R6Class("Sampler1DFct", inherit = Sampler1D,
  public = list(
    prob = NULL,

    initialize = function(param, prob = NULL) {
      super$initialize(param, c("ParamFct", "ParamLgl"))
      k = param$nlevels
      if (is.null(prob))
        prob = rep(1/k, k)
      assert_numeric(prob, lower = 0, upper = 1, len = k)
      assert_true(all.equal(sum(prob), 1))
      self$prob = prob
    }
  ),

  private = list(
    .sample = function(n) {
      s = sample(self$param$values, n, replace = TRUE, prob = self$prob)
      super$as_dt_col(s)
    }
  )
)

#' @export
Sampler1DDblNorm = R6Class("Sampler1DDblNorm", inherit = Sampler1DRfun,
  public = list(
    mean = NULL,
    sd = NULL,

    initialize = function(param, mean = NULL, sd = NULL) {
      super$initialize(param, trunc = TRUE,
        rfun = function(n) rnorm(n, mean = mean , sd = sd))
      if (is.null(mean))
        mean = self$param$qunif(0.5)
      assert_number(mean)
      assert_number(sd, lower = 0)
      self$mean = mean
      self$sd = sd
      assert_true(param$is_bounded)
    }
  )
)

#' @export
new_sampler_1d_unif = function(param) {
  # not so great code here with the switch-on-class, but i think we live with this
  switch(class(param)[1L],
    ParamDbl = Sampler1DDblUnif$new(param),
    ParamInt = Sampler1DIntUnif$new(param),
    ParamFct = Sampler1DFct$new(param),
    ParamLgl = Sampler1DFct$new(param),
    stopf("Sampler not implemented for param of type: %s", class(param)[1L])
  )
}



