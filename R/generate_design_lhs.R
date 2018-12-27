#' @title Generate a space-filling LHS design
#'
#' @description
#' Generate a space-filling design using Latin hypercube sampling.
#'
#' @param param_set [\code{\link{ParamSet}}].
#' @param n [\code{integer(1)}]:\cr
#'   Number of points to sample.
#' @param lhs_function [\code{function()}]:\cr
#'   Function to use to generate a LHS sample.
#'   LHS functions are implemented in package \pkg{lhs}.
#' @export
#' @family generate_design
generate_design_lhs = function(param_set, n, lhs_function = lhs::maximinLHS) {
  n = assert_count(n, positive = TRUE, coerce = TRUE)
  assert_function(lhs_function, args = c("n", "k"))

  ids = param_set$ids
  d = lhs_function(n, k = param_set$length)
  colnames(d) = ids
  d = map_dtc(ids, function(id) param_set$params[[id]]$map_unitint_to_values(d[, id]))
  set_names(d, ids)
}

