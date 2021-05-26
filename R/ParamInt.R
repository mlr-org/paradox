
#' @rdname Domain
#' @export
p_int = function(lower = -Inf, upper = Inf, special_vals = list(), default = NO_DEF, tags = character(), tolerance = sqrt(.Machine$double.eps), depends = NULL, trafo = NULL, logscale = FALSE, init) {
  assert_number(tolerance, lower = 0, upper = 0.5)
  # assert_int will stop for `Inf` values, which we explicitly allow as lower / upper bound
  if (!isTRUE(is.infinite(lower))) assert_int(lower, tol = 1e-300) else assert_number(lower)
  if (!isTRUE(is.infinite(upper))) assert_int(upper, tol = 1e-300) else assert_number(upper)
  assert_true(lower <= upper)
  if (assert_flag(logscale)) {
    if (!is.null(trafo)) stop("When a trafo is given then logscale must be FALSE")
    if (lower < 0) stop("When logscale is TRUE then lower bound must be greater or equal 0")
    trafo = crate(function(x) as.integer(max(min(exp(x), upper), lower)), lower, upper)
    # at this point we don't want to overwrite 'lower' and 'upper, since they get used for the representation
    real_lower = log(max(lower, 0.5))
    real_upper = log(upper + 1)
    cls = "ParamDbl"
  } else {
    cls = "ParamInt"
    real_lower = lower
    real_upper = upper
  }

  domain(cls = cls, grouping = cls, lower = real_lower, upper = real_upper, special_vals = special_vals, default = default, tags = tags, tolerance = tolerance, trafo = trafo,
    storage_type = "integer",
    depends_expr = substitute(depends), init = init)
}

#' @export
domain_check.ParamInt = function(param, values) {
  if (!qtestr(values, "N1()")) {
    return(check_domain_vectorize(param$id, values, check_int,
      more_args = list(lower = param$lower - 0.5, upper = param$upper + 0.5,  # be lenient with bounds, because they would refer to the rounded values
        tol = .51)  # we don't know the tolerances of individual values, but we do know that some values are not even (non-missing, finite) numerics
    ))
  }

  values_num = as.numeric(values)

  if (all(abs(trunc(values_num + 0.5) - 0.5) <= param$tolerance)) {
    values_num = round(values_num)
    if (all(values_num >= param$lower) && all(values_num <= param$upper)) {
      return(TRUE)
    }
  }

  check_domain_vectorize(param$id, values_num, check_int,
    more_args = list(lower = param$lower - 0.5, upper = param$upper + 0.5,  # be lenient with bounds, because they would refer to the rounded values
      tol = pmax(1e-300, param$tolerance + 2 * abs(values_num) * .Machine$double.eps))  # want to have inclusive tolerance bounds. Not sure if 2* is necessary.
  )
}

#' @export
domain_sanitize.ParamInt = function(param, values) {
  as.list(as.integer(as.numeric(values) + 0.5))
}

#' @export
domain_nlevels.ParamInt = function(param) (param$upper - param$lower) + 1
#' @export
domain_is_bounded.ParamInt = function(param) is.finite(param$lower) && is.finite(param$upper)
#' @export
domain_qunif.ParamInt = function(param, x) {
  pmax(pmin(as.integer(x * (param$upper + 1) - (x-1) * param$lower), param$upper), param$lower)  # extra careful here w/ rounding errors and the x == 1 case
}

#' @export
domain_is_number.ParamInt = function(param) TRUE
#' @export
domain_is_categ.ParamInt = function(param) FALSE
