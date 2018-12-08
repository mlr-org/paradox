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
generate_design_lhs = function(param_set, n, lhs_function = lhs::maximinLHS) {
  n = assert_count(n, positive = TRUE, coerce = TRUE)
  assert_function(lhs_function, args = c("n", "k"))

  lhs_des = lhs_function(n, k = param_set$length)

  # converts the LHS output to values of the parameters
  sample_converter = function(lhs_des) {
    vec_cols = lapply(seq_col(lhs_des), function(z) lhs_des[, z, drop = TRUE])
    names(vec_cols) = param_set$ids
    param_set$denorm(vec_cols)
  }

  sample_converter(lhs_des)
}

