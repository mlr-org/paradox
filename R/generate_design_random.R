#' @title Generate a random design.
#'
#' @description
#' Generates a design with randomly drawn points.
#'
#' @param param_set [\code{\link{ParamSet}}].
#' @param n [\code{integer(1)}]:\cr
#'   Number of points to draw randomly. Defaults to 1.
#'
#' @return [\code{\link[data.table]{data.table}}].
#'
#' @export
#' @family generate_design
generate_design_random = function(param_set, n = 1L) {
  assert_class(param_set, "ParamSet")
  n = assert_count(n, positive = TRUE, coerce = TRUE)
  map_dtc(param_set$params, function(x) x$map_unitint_to_values(runif(n)))
}
