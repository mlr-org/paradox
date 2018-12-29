#' @title Generate a space-filling LHS design
#'
#' @description
#' Generate a space-filling design using Latin hypercube sampling.
#'
#' @param param_set :: [ParamSet].
#' @param n :: `integer(1)` \cr
#'   Number of points to sample.
#' @param lhs_function :: `function` \cr
#'   Function to use to generate a LHS sample.
#'   LHS functions are implemented in package \pkg{lhs}.
#' @return [data.table]
#'
#' @family generate_design
#' @export
generate_design_lhs = function(param_set, n, lhs_function = lhs::maximinLHS) {
  assert_paramset(param_set, no_untyped = TRUE, no_deps = TRUE)
  n = assert_count(n, positive = TRUE, coerce = TRUE)
  assert_function(lhs_function, args = c("n", "k"))

  ids = param_set$ids()
  d = lhs_function(n, k = param_set$length)
  colnames(d) = ids
  d = map_dtc(ids, function(id) param_set$params[[id]]$qunif(d[, id]))
  set_names(d, ids)
}

