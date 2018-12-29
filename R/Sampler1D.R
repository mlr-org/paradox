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
#' * `Sampler1DUnif$new(param)` \cr
#'   Uniform random for arbitrary (bounded) params.
#' * `Sampler1DFct$new(param, prob = NULL)` \cr
#'   Categorical distribution, for a fct or lgl param.
#'   `prob` is a numeric vector of `nlevels` probabilities, which is uniform by default.
#' * `Sampler1DDblNorm$new(param)` \cr
#'   Normal sampling (truncated) for (bounded) doubles.
#'   Has member variables `mean` and 'sd' which you can change to influence sampling,
#'   they are initialized to `mean=mean(range)` and `sd=span/4`.
#' * `Sampler1DRfun(param, rfun, trunc = TRUE)` \cr
#'   Arbitrary sampling from 1D rng functions from R.
#'   Pass e.g. rfun=rexp to sample from exponential distribution.
#'   `trunc = TRUE` enables naive rejection sampling, so we stay inside of \[lower, upper\].
#'
#' @name Sampler1D
#' @aliases Sampler1DUnif Sampler1DFct Sampler1DDblNorm Sampler1DRfun
#' @family Sampler
#' NULL

#' @export
Sampler1D = R6Class("Sampler1D", inherit = Sampler, # abstract base class
  public = list(
    initialize = function(param)  {
      super$initialize(ParamSet$new(list(param)))
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
Sampler1DUnif = R6Class("Sampler1DUnif", inherit = Sampler1D,
  public = list(
    initialize = function(param)  {
      assert_param(param, no_untyped = TRUE, must_bounded = TRUE)
      super$initialize(param)
    }
  ),

  private = list(
    .sample = function(n) private$as_dt_col(self$param$qunif(runif(n))) # sample by doing qunif(u)
  )
)


#' @export
Sampler1DRfun = R6Class("Sampler1DRfun", inherit = Sampler1D,
  public = list(
    rfun = NULL,
    trunc = NULL,

    # FIXME: can we use this to smaple unbounded? at least doc correctly
    initialize = function(param, rfun, trunc = TRUE) {
      assert_param(param, "ParamDbl", must_bounded = TRUE)
      super$initialize(param)
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
        s = private$sample_truncated(n, self$rfun)
      else
        s = self$rfun(n = n)
      super$as_dt_col(s)
    },

    # extreme naive rejection sampling to enable trunc sampling from finite, restricted support
    sample_truncated = function(n, rfun) {
      r = numeric(0L)
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
Sampler1DFct = R6Class("Sampler1DFct", inherit = Sampler1D,
  public = list(
    prob = NULL,

    initialize = function(param, prob = NULL) {
      assert_multi_class(param, c("ParamFct", "ParamLgl"))
      super$initialize(param)
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
    initialize = function(param) {
      assert_param(param, "ParamDbl", must_bounded = TRUE)
      super$initialize(param, trunc = TRUE,
        rfun = function(n) rnorm(n, mean = self$mean, sd = self$sd))
      private$.mean = mean(self$param$range)
      private$.sd = self$param$span / 4
    }
  ),

  active = list(
    mean = function(v) if (missing(v)) private$.mean else private$.mean = assert_number(v),
    sd = function(v) if (missing(v)) private$.sd else private$.sd = assert_number(v, lower = 0)
  ),

  private = list(
    .mean = NULL,
    .sd = NULL
  )
)

