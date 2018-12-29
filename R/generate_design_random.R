#' @title Generate a random design.
#'
#' @description
#' Generates a design with randomly drawn points.
#'
#' @param param_set [ParamSet].
#' @param n `integer(1)` \cr
#'   Number of points to draw randomly. Defaults to 1.
#' @return [data.table]
#'
#' @family generate_design
#' @export
generate_design_random = function(param_set, n = 1L) {
  assert_paramset(param_set, no_untyped = TRUE, no_deps = TRUE)
  n = assert_count(n, positive = TRUE, coerce = TRUE)
  map_dtc(param_set$params, function(x) x$qunif(runif(n)))
}
