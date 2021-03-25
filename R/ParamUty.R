
#' @rdname Domain
#' @export
p_uty = function(custom_check = NULL, special_vals = list(), default = NO_DEF, tags = character(), depends = NULL, trafo = NULL) {
  assert_function(custom_check, nargs = 1)
  custom_check_result = custom_check(1)
  assert(check_true(custom_check_result), check_string(custom_check_result), .var.name = "The result of 'custom_check()'")
  domain(cls = "ParamUty", grouping = "ParamUty", cargo = custom_check, special_vals = special_vals, default = default, tags = tags, trafo = trafo, depends_expr = substitute(depends))
}

#' @export
domain_check.ParamUty = function(param, values, describe_error = TRUE) {
  check_domain_vectorize(param$id, values, param$cargo, describe_error = describe_error)
}

#' @export
domain_storage_type.ParamUty = function(param) rep("list", nrow(param))
#' @export
domain_nlevels.ParamUty = function(param) rep(Inf, nrow(param))
#' @export
domain_is_bounded.ParamUty = function(param) rep(FALSE, nrow(param))
#' @export
domain_is_number.ParamUty = function(param) rep(FALSE, nrow(param))
#' @export
domain_is_categ.ParamUty = function(param) rep(FALSE, nrow(param))
#' @export
domain_qunif.ParamUty = function(param, x) stop("undefined")
