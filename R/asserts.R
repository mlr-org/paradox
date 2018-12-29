#FIXME: export

#' @title Assertion for ParamSet
#' @param param_set [ParamSet] \cr
#' @param no_untyped `logical(1)`
#'   No unytped params allowed?
#' @export
assert_paramset = function(param_set, no_untyped = FALSE) {
  assert_r6(param_set, "ParamSet")
  if (no_untyped && ("ParamUty" %in% param_set$pclasses))
    stop("ParamSet contains untyped params!")
  invisible(param_set)
}

#' @title Assertion for Param
#' @param param [Param] \cr
#' @param cl `character`
#'   Allowed subclasses.
#' @param no_untyped `logical(1)`
#'   No unytped params allowed?
#' @param must_bounded `logical(1)`
#'   Only bounded params allowed?
#' @export
assert_param = function(param, cl = "Param", no_untyped = FALSE, must_bounded = FALSE) {
  assert_r6(param, cl)
  if (no_untyped && (param$pclass == "ParamUty"))
    stop("Param is untyped!")
  if (must_bounded && !param$is_bounded)
    stop("Param is unbounded!")
  invisible(param)
}

