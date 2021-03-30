#' @rdname Domain
#' @export
p_dbl = function(lower = -Inf, upper = Inf, special_vals = list(), default = NO_DEF, tags = character(), tolerance = sqrt(.Machine$double.eps), depends = NULL, trafo = NULL, logscale = FALSE, init) {
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

  domain(cls = "ParamDbl", grouping = "ParamDbl", lower = real_lower, upper = real_upper, special_vals = special_vals, default = default, tags = tags, tolerance = tolerance, trafo = trafo, storage_type = "numeric",
    depends_expr = substitute(depends), init = init)
}

#' @export
domain_check.ParamDbl = function(param, values) {
  lower = param$lower - param$tolerance * pmax(1, abs(param$lower))
  upper = param$upper + param$tolerance * pmax(1, abs(param$upper))
  if (qtestr(values, "N1")) {
    values_num = as.numeric(values)
    if (all(values_num >= lower) && all(values_num <= upper)) return(TRUE)
  }
  check_domain_vectorize(param$id, values, check_number, more_args = list(lower = lower, upper = upper))
}

#' @export
domain_sanitize.ParamDbl = function(param, values) {
  as.list(min(max(as.numeric(values), param$lower), param$upper))
}

#' @export
domain_nlevels.ParamDbl = function(param) ifelse(param$upper == param$lower, 1, Inf)
#' @export
domain_is_bounded.ParamDbl = function(param) is.finite(param$lower) && is.finite(param$upper)
#' @export
domain_qunif.ParamDbl = function(param, x) {
  pmax(pmin(x * param$upper - (x-1) * param$lower, param$upper), param$lower)  # extra careful here w/ rounding errors
}

#' @export
domain_is_number.ParamDbl = function(param) TRUE
#' @export
domain_is_categ.ParamDbl = function(param) FALSE
