
#' @rdname Domain
#' @export
p_uty = function(custom_check = NULL, special_vals = list(), default = NO_DEF, tags = character(), depends = NULL, trafo = NULL, repr = substitute(default), init, aggr = NULL, in_tune_fn = NULL, disable_in_tune = NULL) {
  assert_function(custom_check, null.ok = TRUE)
  assert_list(disable_in_tune, null.ok = TRUE, names = "unique")
  if ("internal_tuning" %in% tags) {
    assert_function(in_tune_fn, null.ok = FALSE, args = c("domain", "param_set"), nargs = 2L)
  } else {
    assert_true(is.null(in_tune_fn))
  }
  assert_function(aggr, null.ok = TRUE, nargs = 1L)
  if (!is.null(custom_check)) {
    custom_check_result = custom_check(1)
    assert(check_true(custom_check_result), check_string(custom_check_result), .var.name = "The result of 'custom_check()'")
  }
  repr = if (!is_nodefault(default)) {
    deparse(repr)[[1]]
  } else {
    "NoDefault"
  }
  cargo = list(custom_check = custom_check, repr = repr)
  cargo$aggr = aggr
  cargo$in_tune_fn = in_tune_fn
  cargo$disable_in_tune = disable_in_tune

  Domain(cls = "ParamUty", grouping = "ParamUty", cargo = cargo, special_vals = special_vals, default = default, tags = tags, trafo = trafo, storage_type = "list", depends_expr = substitute(depends), init = init)
}

#' @export
domain_check.ParamUty = function(param, values) {
  cargo = map(param$cargo, "custom_check")
  subset = !map_lgl(cargo, is.null)
  if (!any(subset)) return(TRUE)
  values = values[subset]
  check_domain_vectorize(param$id[subset], values, cargo[subset])
}

#' @export
domain_nlevels.ParamUty = function(param) rep(Inf, nrow(param))
#' @export
domain_is_bounded.ParamUty = function(param) rep(FALSE, nrow(param))
#' @export
domain_qunif.ParamUty = function(param, x) stop("undefined")

#' @export
domain_is_number.ParamUty = function(param) FALSE
#' @export
domain_is_categ.ParamUty = function(param) FALSE
