#' @title Generate a Space-Filling LHS Design
#'
#' @description
#' Generate a space-filling design using Latin hypercube sampling. Dependent
#' parameters whose constraints are unsatisfied generate `NA` entries in
#' their respective columns.
#'
#' @param param_set ([ParamSet]).
#' @param n (`integer(1)`) \cr
#'   Number of points to sample.
#' @param lhs_fun (`function(n, k)`)\cr
#'   Function to use to generate a LHS sample, with n samples and k values per param.
#'   LHS functions are implemented in package \pkg{lhs}, default is to use [lhs::maximinLHS()].
#' @return [Design].
#'
#' @family generate_design
#' @export
#' @examples
#' ps = ParamSet$new(list(
#'   ParamDbl$new("ratio", lower = 0, upper = 1),
#'   ParamFct$new("letters", levels = letters[1:3])
#' ))
#'
#' if (requireNamespace("lhs", quietly = TRUE)) {
#'   generate_design_lhs(ps, 10)
#' }
generate_design_lhs = function(param_set, n, lhs_fun = NULL) {
  if (is.null(lhs_fun)) {
    require_namespaces("lhs")
    lhs_fun = lhs::maximinLHS
  }
  assert_param_set(param_set, no_untyped = TRUE)
  n = assert_count(n, coerce = TRUE)
  assert_function(lhs_fun, args = c("n", "k"))

  ids = param_set$ids()
  if (n == 0) {
    d = matrix(nrow = 0, ncol = param_set$length)
  } else {
    d = lhs_fun(n, k = param_set$length)
  }
  colnames(d) = ids
  d = param_set$qunif(d)
  Design$new(param_set, d, remove_dupl = FALSE) # user wants n-points, dont remove
}
