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

#' @title Assertion for ParamSet
#' @param param_set [ParamSet] \cr
#' @inheritParams assert_param
#' @param no_deps `logical(1)`
#'   Np dependencies allowed?
#' @export
assert_paramset = function(param_set, cl = "Param", no_untyped = FALSE, must_bounded = FALSE, no_deps = FALSE) {
  assert_r6(param_set, "ParamSet")
  assert_list(param_set$params, types = cl)
  if (no_untyped && ("ParamUty" %in% param_set$pclass))
    stop("ParamSet contains untyped params!")
  if (must_bounded && !all(param_set$is_bounded))
    stop("ParamSet contains bounded params!")
  if (no_deps && param_set$has_deps)
    stop("ParamSet contains dependencies!")
  invisible(param_set)
}

