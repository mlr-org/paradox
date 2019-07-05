#' @title Sampler1D Class
#'
#' @usage NULL
#' @format [R6::R6Class] inheriting from [Sampler].
#'
#' @description
#' 1D sampler, abstract base class for Sampler like [Sampler1DUnif], [Sampler1DRfun], [Sampler1DCateg] and [Sampler1DNormal].
#'
#' @section Construction:
#' Note: This object is typically constructed via a derived classes, e.g. [Sampler1DUnif], [Sampler1DRfun], [Sampler1DCateg] or [Sampler1DNormal].
#'
#' ```
#' smpl = Sampler1D$new(param)
#' ```
#'
#' * `param` :: [Param]\cr
#'   Domain / support of the distribution we want to sample from.
#'
#' @section Fields:
#' See [Sampler].
#' Additionally, the class provides:
#' * `param` :: [Param]\cr
#'   Returns the one Parameter that is sampled from.
#'
#' @section Methods:
#' See [Sampler].
#'
#' @family Sampler
#' @export
Sampler1D = R6Class("Sampler1D", inherit = Sampler, # abstract base class
  public = list(
    initialize = function(param) {
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

#' @title Sampler1DUnif Class
#'
#' @usage NULL
#' @format [R6::R6Class] inheriting from [Sampler1D].
#'
#' @description
#' Uniform random sampler for arbitrary (bounded) parameters.
#'
#' @section Construction:
#' ```
#' smpl = Sampler1DUnif$new(param)
#' ```
#'
#' * `param` :: [Param]\cr
#'   Domain / support of the distribution we want to sample from.
#'
#' @section Fields:
#' See [Sampler1D].
#'
#' @section Methods:
#' See [Sampler1D].
#'
#' @family Sampler
#' @export
Sampler1DUnif = R6Class("Sampler1DUnif", inherit = Sampler1D,
  public = list(
    initialize = function(param) {
      assert_param(param, no_untyped = TRUE, must_bounded = TRUE)
      super$initialize(param)
    }
  ),

  private = list(
    .sample = function(n) private$as_dt_col(self$param$qunif(runif(n))) # sample by doing qunif(u)
  )
)


#' @title Sampler1DRfun Class
#'
#' @usage NULL
#' @format [R6::R6Class] inheriting from [Sampler1D].
#'
#' @description
#' Arbitrary sampling from 1D RNG functions from R.
#'
#' @section Construction:
#' ```
#' smpl = Sampler1DRfun$new(param, rfun, trunc = TRUE)
#' ```
#'
#' * `param` :: [Param]\cr
#'   Domain / support of the distribution we want to sample from.
#' * `rfun` :: `function`\cr
#'   Random number generator function, e.g. `rexp` to sample from exponential distribution.
#' * `trunc` :: `logical(1)`\cr
#'   `TRUE` enables naive rejection sampling, so we stay inside of \[lower, upper\].
#'
#' @section Fields:
#' See [Sampler1D].
#' Additionally, the class provides:
#' * `rfun` :: `function()`\cr
#'   Random number generator function, e.g. `rexp` to sample from exponential distribution.
#' * `trunc` :: `logical(1)`\cr
#'   `TRUE` enables naive rejection sampling, so we stay inside of \[lower, upper\].
#'
#' @section Methods:
#' See [Sampler1D].
#'
#' @family Sampler
#' @export
Sampler1DRfun = R6Class("Sampler1DRfun", inherit = Sampler1D,
  public = list(
    rfun = NULL,
    trunc = NULL,

    initialize = function(param, rfun, trunc = TRUE) {
      assert_param(param, "ParamDbl")
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
      if (self$trunc) {
        s = private$sample_truncated(n, self$rfun)
      } else {
        s = self$rfun(n = n)
      }
      super$as_dt_col(s)
    },

    # extreme naive rejection sampling to enable trunc sampling from finite, restricted support
    sample_truncated = function(n, rfun) {
      r = numeric(0L)
      for (i in 1:1000) {
        s = rfun(n = 2 * n)
        s = s[s >= self$param$lower & s <= self$param$upper]
        r = c(r, s)
        if (length(r) >= n) {
          return(r[1:n])
        }
      }
      stopf("Tried rejection sampling 1000x. Giving up.")
    }
  )
)

#' @title Sampler1DCateg Class
#'
#' @usage NULL
#' @format [R6::R6Class] inheriting from [Sampler1D].
#'
#' @description
#' Sampling from a discrete distribution, for a [ParamFct] or [ParamLgl].
#'
#' @section Construction:
#' ```
#' smpl = Sampler1DCateg$new(param, prob = NULL)
#' ```
#'
#' * `param` :: [Param]\cr
#'   Domain / support of the distribution we want to sample from.
#' * `prob` :: `numeric()`\cr
#'   Numeric vector of `param$nlevels` probabilities, which is uniform by default.
#'
#' @section Fields:
#' See [Sampler1D].
#' Additionally, the class provides:
#' * `prob` :: `numeric(n)`\cr
#'   Numeric vector of `param$nlevels` probabilities, which is uniform by default.
#'
#' @section Methods:
#' See [Sampler1D].
#'
#' @family Sampler
#' @export
Sampler1DCateg = R6Class("Sampler1DCateg", inherit = Sampler1D,
  public = list(
    prob = NULL,

    initialize = function(param, prob = NULL) {
      assert_multi_class(param, c("ParamFct", "ParamLgl"))
      super$initialize(param)
      k = param$nlevels
      if (is.null(prob)) {
        prob = rep(1 / k, k)
      }
      assert_numeric(prob, lower = 0, upper = 1, len = k)
      assert_true(all.equal(sum(prob), 1))
      self$prob = prob
    }
  ),

  private = list(
    .sample = function(n) {
      s = sample(self$param$levels, n, replace = TRUE, prob = self$prob)
      super$as_dt_col(s)
    }
  )
)

#' @title Sampler1DNormal Class
#'
#' @usage NULL
#' @format [R6::R6Class] inheriting from [Sampler1D].
#'
#' @description
#' Normal sampling (potentially truncated) for [ParamDbl].
#'
#' @section Construction:
#' ```
#' smpl = Sampler1DNormal$new(param, mean = NULL, sd = NULL)
#' ```
#'
#' * `mean` :: `numeric(1)`\cr
#'   Mean parameter of the normal distribution.
#'   Default is `mean(c(param$lower, param$upper)`.
#' * `sd` :: `numeric(1)`\cr
#'   SD parameter of the normal distribution.
#'   Default is `(param$upper - param$lower)/4`.
#'
#' @section Fields:
#' See [Sampler1D].
#' Additionally, the class provides:
#' * `mean` :: `numeric(1)`\cr
#'   Mean parameter of the normal distribution.
#'   Default is `mean(c(param$lower, param$upper)`.
#' * `sd` :: `numeric(1)`\cr
#'   SD parameter of the normal distribution.
#'   Default is `(param$upper - param$lower)/4`.
#'
#' @section Methods:
#' See [Sampler1D].
#'
#' @family Sampler
#' @export
Sampler1DNormal = R6Class("Sampler1DNormal", inherit = Sampler1DRfun,
  public = list(
    initialize = function(param, mean = NULL, sd = NULL) {
      assert_param(param, "ParamDbl")
      if ((is.null(mean) || is.null(sd)) && !param$is_bounded) {
        stop("If 'mean' or 'sd' are not set, param must be bounded!")
      }
      if (is.null(mean)) {
        mean = mean(c(param$lower, param$upper))
      }
      self$mean = mean
      if (is.null(sd)) {
        sd = (param$upper - param$lower) / 4
      }
      self$sd = sd
      super$initialize(param, trunc = TRUE, # we always trunc, this should not hurt for unbounded params
        rfun = function(n) rnorm(n, mean = self$mean, sd = self$sd))
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
