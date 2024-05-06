#' @rdname Domain
#' @export
p_lgl = function(special_vals = list(), default = NO_DEF, tags = character(), depends = NULL, trafo = NULL, init, aggr = NULL, in_tune_fn = NULL) {
  assert_function(aggr, null.ok = TRUE, nargs = 1L)
  if ("internal_tuning" %in% tags) {
    assert_function(in_tune_fn, null.ok = FALSE, args = c("domain", "param_set"), nargs = 2L)
  } else {
    assert_true(is.null(in_tune_fn))
  }

  cargo = c(aggr = aggr, in_tune_fn = in_tune_fn)
  cargo = if (length(cargo)) cargo
  Domain(cls = "ParamLgl", grouping = "ParamLgl", levels = c(TRUE, FALSE), special_vals = special_vals, default = default,
    tags = tags, trafo = trafo, storage_type = "logical", depends_expr = substitute(depends), init = init, cargo = cargo)
}

#' @export
domain_check.ParamLgl = function(param, values) {
  if (qtestr(values, "B1")) {
    return(TRUE)
  }
  check_domain_vectorize(param$id, values, check_flag)
}

#' @export
domain_nlevels.ParamLgl = function(param) rep(2, nrow(param))
#' @export
domain_is_bounded.ParamLgl = function(param) rep(TRUE, nrow(param))
#' @export
domain_qunif.ParamLgl = function(param, x) {
  x < 0.5
}

#' @export
domain_is_number.ParamLgl = function(param) FALSE
#' @export
domain_is_categ.ParamLgl = function(param) TRUE
