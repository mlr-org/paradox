#' @rdname Domain
#' @export
p_lgl = function(special_vals = list(), default = NO_DEF, tags = character(), depends = NULL, trafo = NULL) {
  domain(cls = "ParamLgl", grouping = "ParamLgl", levels = c(TRUE, FALSE), special_vals = special_vals, default = default,
    tags = tags, trafo = trafo, depends_expr = substitute(depends))
}

#' @export
domain_check.ParamLgl = function(param, values, describe_error = TRUE) {
  if (qtestr(values, "B1")) {
    return(TRUE)
  }
  if (!describe_error) return(FALSE)
  check_domain_vectorized(param$id, values, check_flag)
}

#' @export
domain_storage_type.ParamLgl = function(param) rep("logical", nrow(param))
#' @export
domain_nlevels.ParamLgl = function(param) rep(2, nrow(param))
#' @export
domain_is_bounded.ParamLgl = function(param) rep(TRUE, nrow(param))
#' @export
domain_is_number.ParamLgl = function(param) rep(FALSE, nrow(param))
#' @export
domain_is_categ.ParamLgl = function(param) rep(TRUE, nrow(param))
#' @export
domain_qunif.ParamLgl = function(param, x) {
  x < 0.5
}
