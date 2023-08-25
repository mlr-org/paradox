#' @title Generate a Space-Filling Sobol Sequence Design
#'
#' @description
#' Generate a space-filling design using a Sobol sequence. Dependent
#' parameters whose constraints are unsatisfied generate `NA` entries in
#' their respective columns.
#'
#' Uses [spacefillr::generate_sobol_set].
#'
#' Note that non determinism is achieved by sampling the seed argument via
#' `sample(.Machine$integer.max, size = 1L)`.
#'
#' @param param_set ([ParamSet]).
#' @param n (`integer(1)`) \cr
#'   Number of points to sample.
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
#' if (requireNamespace("spacefillr", quietly = TRUE)) {
#'   generate_design_sobol(ps, 10)
#' }
generate_design_sobol = function(param_set, n) {
  require_namespaces("spacefillr")
  assert_param_set(param_set, no_untyped = TRUE)
  n = assert_count(n, coerce = TRUE)

  ids = param_set$ids()
  if (n == 0) {
    d = matrix(nrow = 0, ncol = param_set$length)
  } else {
    seed = sample(.Machine$integer.max, size = 1L)
    d = spacefillr::generate_sobol_set(n, dim = param_set$length, seed = seed)
  }
  colnames(d) = ids
  d = map_dtc(ids, function(id) param_set$params[[id]]$qunif(d[, id]))
  Design$new(param_set, set_names(d, ids), remove_dupl = FALSE) # user wants n-points, dont remove
}
