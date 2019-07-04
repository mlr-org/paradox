#' @title Assertions for Params and ParamSets
#'
#' @param param ([Param]).
#' @param cl `character()`\cr
#'   Allowed subclasses.
#' @param no_untyped (`logical(1)`)\cr
#'   Are untyped [Param]s allowed?
#' @param must_bounded (`logical(1)`)\cr
#'   Only bounded [Param]s allowed?
#'
#' @return The checked object, invisibly.
#' @export
assert_param = function(param, cl = "Param", no_untyped = FALSE, must_bounded = FALSE) {
  assert_multi_class(param, cl)
  if (no_untyped && (param$class == "ParamUty")) {
    stop("Param is untyped!")
  }
  if (must_bounded && !param$is_bounded) {
    stop("Param is unbounded!")
  }
  invisible(param)
}

#' @param param_set [ParamSet].
#' @param no_deps (`logical(1)`)\cr
#'   Are dependencies allowed?
#' @rdname assert_param
#' @export
assert_param_set = function(param_set, cl = "Param", no_untyped = FALSE, must_bounded = FALSE, no_deps = FALSE) {
  assert_r6(param_set, "ParamSet")
  assert_list(param_set$params, types = cl)
  if (no_untyped && ("ParamUty" %in% param_set$class)) {
    stop("ParamSet contains untyped params!")
  }
  if (must_bounded && !all(param_set$is_bounded)) {
    stop("ParamSet contains unbounded params!")
  }
  if (no_deps && param_set$has_deps) {
    stop("ParamSet contains dependencies!")
  }
  invisible(param_set)
}


# assert that we can use the string in list, tables, formulas
assert_id = function(id) {
  assert_string(id, pattern = "^[[:alpha:]]+[[:alnum:]_.]*$")
}
