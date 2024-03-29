#' @title Assertions for Params and ParamSets
#'
#' @param param_set ([`ParamSet`]).
#' @param cl (`character()`)\cr
#'   Allowed subclasses.
#' @param no_untyped (`logical(1)`)\cr
#'   Are untyped [`Domain`]s allowed?
#' @param must_bounded (`logical(1)`)\cr
#'   Only bounded [`Domain`]s allowed?
#' @param no_deps (`logical(1)`)\cr
#'   Are dependencies allowed?
#' @return The checked object, invisibly.
#' @export
assert_param_set = function(param_set, cl = NULL, no_untyped = FALSE, must_bounded = FALSE, no_deps = FALSE) {
  assert_r6(param_set, "ParamSet")
  if (!is.null(cl)) {
    if (!all(param_set$class %in% cl)) stopf("Only classes %s allowed", str_collapse(cl))
  }
  if (no_untyped && !all(param_set$class %in% c("ParamLgl", "ParamInt", "ParamFct", "ParamDbl"))) {
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
