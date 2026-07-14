#' @rdname Domain
#' @export
p_dbl = function(lower = -Inf, upper = Inf, special_vals = list(), default = NO_DEF, tags = character(), tolerance = sqrt(.Machine$double.eps), depends = NULL, trafo = NULL, logscale = FALSE, init, aggr = NULL, in_tune_fn = NULL, disable_in_tune = NULL) {
  assert_number(tolerance, lower = 0)
  assert_number(lower)
  assert_number(upper)
  assert_true(lower <= upper)
  if (assert_flag(logscale)) {
    if (!is.null(trafo)) stop("When a trafo is given then logscale must be FALSE")
    if (assert_number(lower) <= 0) stop("When logscale is TRUE then lower bound must be strictly greater than 0")
    trafo = exp
    # at this point we don't want to overwrite 'lower' and 'upper, since they get used for the representation
    real_lower = log(lower)
    real_upper = log(assert_number(upper))
  } else {
    real_lower = lower
    real_upper = upper
  }

  cargo = list()
  if (logscale) cargo$logscale = TRUE
  cargo$aggr = aggr
  cargo$in_tune_fn = in_tune_fn
  cargo$disable_in_tune = disable_in_tune
  Domain(cls = "ParamDbl", grouping = "ParamDbl", lower = real_lower, upper = real_upper, special_vals = special_vals, default = default, tags = tags, tolerance = tolerance, trafo = trafo, storage_type = "numeric",
    depends_expr = substitute(depends), init = init, cargo = if (length(cargo)) cargo)
}

#' @export
domain_check.ParamDbl = function(param, values, internal = FALSE) {
  lower_delta = param$tolerance * pmax(1, abs(param$lower))
  upper_delta = param$tolerance * pmax(1, abs(param$upper))
  lower_delta[param$tolerance == 0 | !is.finite(param$lower)] = 0
  upper_delta[param$tolerance == 0 | !is.finite(param$upper)] = 0
  lower = param$lower - lower_delta
  upper = param$upper + upper_delta
  if (qtestr(values, "N1")) {
    values_num = as.numeric(values)
    if (all(values_num >= lower) && all(values_num <= upper)) return(TRUE)
  }
  check_domain_vectorize(param$id, values, check_number, more_args = list(lower = lower, upper = upper))
}

#' @export
domain_sanitize.ParamDbl = function(param, values) {
  values = as.numeric(values)
  values = pmax(values, param$lower)
  values = pmin(values, param$upper)
  as.list(values)
}

#' @export
domain_nlevels.ParamDbl = function(param) ifelse(param$upper == param$lower, 1, Inf)
#' @export
domain_is_bounded.ParamDbl = function(param) is.finite(param$lower) & is.finite(param$upper)
#' @export
domain_qunif.ParamDbl = function(param, x) {
  lower = rep(param$lower, length.out = length(x))
  upper = rep(param$upper, length.out = length(x))
  mapped = x * upper - (x - 1) * lower

  # The affine expression contains 0 * Inf at closed interval endpoints and
  # on fixed infinite Domains. Those values are defined even though the
  # interior of a two-sided unbounded interval is not.
  fixed = lower == upper
  mapped[fixed] = lower[fixed]
  at_lower = x == 0
  mapped[at_lower] = lower[at_lower]
  at_upper = x == 1
  mapped[at_upper] = upper[at_upper]

  pmax(pmin(mapped, upper), lower)  # extra careful here w/ rounding errors
}

#' @export
domain_is_number.ParamDbl = function(param) TRUE
#' @export
domain_is_categ.ParamDbl = function(param) FALSE
