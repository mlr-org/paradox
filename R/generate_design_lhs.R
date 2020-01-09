#' @title Generate a Space-Filling LHS Design
#'
#' @description
#' Generate a space-filling design using Latin hypercube sampling.
#'
#' @param param_set :: [ParamSet].
#' @param n :: `integer(1)` \cr
#'   Number of points to sample.
#' @param lhs_fun :: `function(n, k)` \cr
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
#' generate_design_lhs(ps, 10)
generate_design_lhs = function(param_set, n, lhs_fun = NULL) {

  require_namespaces("lhs") # actually we MAY do not need to load this, if user passes another lhs_fun (not from LHS)
  if (is.null(lhs_fun)) {
    lhs_fun = lhs::maximinLHS
  }
  assert_param_set(param_set, no_untyped = TRUE, no_deps = TRUE)
  n = assert_count(n, positive = TRUE, coerce = TRUE)
  assert_function(lhs_fun, args = c("n", "k"))

  ids = param_set$ids()
  d = lhs_fun(n, k = param_set$length)
  colnames(d) = ids
  d = map_dtc(ids, function(id) param_set$params[[id]]$qunif(d[, id]))
  Design$new(param_set, set_names(d, ids), remove_dupl = FALSE) # user wants n-points, dont remove
}
