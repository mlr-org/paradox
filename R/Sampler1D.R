#' @title Sampler1D Class
#'
#' @description
#' 1D sampler, abstract base class for Sampler like [Sampler1DUnif], [Sampler1DRfun],
#' [Sampler1DCateg] and [Sampler1DNormal].
#'
#' @template param_param
#'
#' @family Sampler
#' @include Sampler.R
#' @export
Sampler1D = R6Class("Sampler1D", inherit = Sampler, # abstract base class
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' Note that this object is typically constructed via derived classes,
    #' e.g., [`Sampler1DUnif`].
    initialize = function(param) {
      super$initialize(param)
      dimension = self$param_set$length
      if (dimension != 1) {
        stopf(
          "param must contain exactly 1 Param, but contains %s",
          dimension
        )
      }
    }
  ),

  active = list(
    #' @field param ([`ParamSet`])\cr
    #' Returns the one-dimensional [`ParamSet`] that is sampled from.
    param = function() self$param_set
  ),

  private = list(
    # create a 1-col-dt, named by param-id, from a data vector (from sampling), and enforce storage type
    as_dt_col = function(x, storage_type = self$param$storage_type,
        id = self$param$ids()) {
      x = as_type(x, storage_type)
      set_names(data.table(x), id)
    }
  )
)

#' @title Sampler1DUnif Class
#'
#' @description
#' Uniform random sampler for arbitrary (bounded) parameters.
#'
#' @template param_param
#'
#' @family Sampler
#' @export
Sampler1DUnif = R6Class("Sampler1DUnif", inherit = Sampler1D,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(param) {
      handoff = if (typeof(param) == "externalptr") {
        .Call(C_sampler_unif_take_subspace, param)
      }
      if (is.null(handoff)) {
        super$initialize(param)
      } else {
        # Only the package-owned SamplerUnif issuer can create an accepted
        # carrier. The bundle installs its nested subset token and any callback
        # detached from the exact same source generation without a redundant
        # deep clone.
        owned = param_set_from_subset_bundle(handoff)
        if (owned$length != 1) {
          stopf(
            "param must contain exactly 1 Param, but contains %s",
            owned$length
          )
        }
        self$param_set = owned
      }
      assert_param_set(self$param, no_untyped = TRUE, must_bounded = TRUE)
    }
  ),

  private = list(
    # The one-shot engine selects schema, bounds, levels, IDs, dependency
    # state, and the RNG transaction together. It is both faster than
    # constructing a unit data.table in R and immune to split active-binding
    # reads around allocation/finalizers.
    .sample = function(n) {
      .Call(C_sampler_unif_sample_builtin, self$param_set, n)
    }
  )
)

#' @title Sampler1DRfun Class
#'
#' @description
#' Arbitrary sampling from 1D RNG functions from R.
#'
#' @template param_param
#'
#' @family Sampler
#' @export
Sampler1DRfun = R6Class("Sampler1DRfun", inherit = Sampler1D,
  public = list(
    #' @field rfun (`function()`)\cr
    #' Random number generator function.
    rfun = NULL,

    #' @field trunc (`logical(1)`)\cr
    #' `TRUE` enables naive rejection sampling, so we stay inside of \[lower, upper\].
    trunc = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param rfun (`function()`)\cr
    #'   Random number generator function, e.g. `rexp` to sample from exponential distribution.
    #' @param trunc (`logical(1)`)\cr
    #'   `TRUE` enables naive rejection sampling, so we stay inside of \[lower, upper\].
    initialize = function(param, rfun, trunc = TRUE) {
      super$initialize(param)
      assert_param_set(self$param, "ParamDbl")
      assert_function(rfun, args = "n")
      assert_flag(trunc)
      self$rfun = rfun
      self$trunc = trunc
    }
  ),

  private = list(
    # maybe we want an option to use my truncation here, as this slows stuff down somewhat
    # and there are some real truncated rngs in R
    .sample = function(n) {
      param = self$param
      domain = .Call(
        C_param_set_domains,
        get_private(param),
        param
      )[[1L]]
      lower = domain$lower[[1L]]
      upper = domain$upper[[1L]]
      storage_type = domain$storage_type[[1L]]
      id = domain$id[[1L]]
      rfun = self$rfun
      trunc = self$trunc
      if (n == 0L) {
        s = numeric() # skip truncation stuff, #338
      } else if (trunc) {
        s = private$sample_truncated(n, rfun, lower, upper)
      } else {
        s = rfun(n = n)
      }
      super$as_dt_col(s, storage_type, id)
    },

    # extreme naive rejection sampling to enable trunc sampling from finite, restricted support
    sample_truncated = function(n, rfun, lower, upper) {
      r = numeric(0L)
      for (i in 1:1000) {
        s = rfun(n = 2 * n)
        s = s[s >= lower & s <= upper]
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
#' @description
#' Sampling from a discrete distribution, for a [`ParamSet`] containing a single [`p_fct()`] or [`p_lgl()`].
#' A zero-level factor accepts `numeric()` probabilities and can be sampled
#' for zero rows; a positive-row request fails before entering the RNG.
#'
#' @template param_param
#'
#' @family Sampler
#' @export
Sampler1DCateg = R6Class("Sampler1DCateg", inherit = Sampler1D,
  public = list(
    #' @field prob (`numeric()` | NULL)\cr
    #' Numeric vector of `param$nlevels` probabilities.
    prob = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param prob (`numeric()` | NULL)\cr
    #'   Numeric vector of `param$nlevels` probabilities, which is uniform by default.
    initialize = function(param, prob = NULL) {
      super$initialize(param)
      assert_subset(self$param$class, c("ParamFct", "ParamLgl"))
      k = self$param$nlevels[[1L]]
      if (is.null(prob)) {
        prob = rep(1 / k, k)
      }
      assert_numeric(prob, lower = 0, upper = 1, len = k)
      if (k != 0L) {
        assert_true(all.equal(sum(prob), 1))
      }
      self$prob = prob
    }
  ),

  private = list(
    .sample = function(n) {
      param = self$param
      domain = .Call(
        C_param_set_domains,
        get_private(param),
        param
      )[[1L]]
      levels = domain$levels[[1L]]
      storage_type = domain$storage_type[[1L]]
      id = domain$id[[1L]]
      prob = self$prob
      if (!length(levels)) {
        if (n != 0L) {
          stop(
            "Cannot sample a factor parameter with no levels",
            call. = FALSE
          )
        }
        return(super$as_dt_col(character(), storage_type, id))
      }
      s = sample(levels, n, replace = TRUE, prob = prob)
      super$as_dt_col(s, storage_type, id)
    }
  )
)

#' @title Sampler1DNormal Class
#'
#' @description
#' Normal sampling (potentially truncated) for [`p_dbl()`].
#'
#' @template param_param
#'
#' @family Sampler
#' @export
Sampler1DNormal = R6Class("Sampler1DNormal", inherit = Sampler1DRfun,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param mean (`numeric(1)`)\cr
    #'   Mean parameter of the normal distribution.
    #'   Default is `mean(c(param$lower, param$upper)`.
    #' @param sd (`numeric(1)`)\cr
    #'   SD parameter of the normal distribution.
    #'   Default is `(param$upper - param$lower)/4`.
    initialize = function(param, mean = NULL, sd = NULL) {
      super$initialize(param, trunc = TRUE, # we always trunc, this should not hurt for unbounded params
        rfun = function(n) rnorm(n, mean = self$mean, sd = self$sd))
      param = self$param
      assert_param_set(param, "ParamDbl")
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
    }
  ),

  active = list(
    #' @field mean (`numeric(1)`)\cr
    #' Mean parameter of the normal distribution.
    mean = function(v) if (missing(v)) private$.mean else private$.mean = assert_number(v),

    #' @field sd (`numeric(1)`)\cr
    #' SD parameter of the normal distribution.
    sd = function(v) if (missing(v)) private$.sd else private$.sd = assert_number(v, lower = 0)
  ),

  private = list(
    .mean = NULL,
    .sd = NULL
  )
)
