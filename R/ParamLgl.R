#' @rdname Domain
#' @export
p_lgl = function(special_vals = list(), default = NO_DEF, tags = character(), depends = NULL, trafo = NULL, init) {
  domain(cls = "ParamLgl", grouping = "ParamLgl", levels = c(TRUE, FALSE), special_vals = special_vals, default = default,
    tags = tags, trafo = trafo, storage_type = "logical", depends_expr = substitute(depends), init = init)
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
