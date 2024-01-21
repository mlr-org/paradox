#' @title Generate a Random Design
#'
#' @description
#' Generates a design with randomly drawn points.
#' Internally uses [`SamplerUnif`], hence, also works for [ParamSet]s with dependencies.
#' If dependencies do not hold, values are set to `NA` in the resulting data.table.
#'
#' @param param_set ([`ParamSet`]).
#' @param n (`integer(1)`)\cr
#'   Number of points to draw randomly.
#' @return [`Design`].
#'
#' @family generate_design
#' @export
#' @examples
#' pset = ps(
#'   ratio = p_dbl(lower = 0, upper = 1),
#'   letters = p_fct(levels = letters[1:3])
#' ))
#' generate_design_random(pset, 10)
generate_design_random = function(param_set, n) {
  # arg checks done by SamplerUnif and sample
  SamplerUnif$new(param_set)$sample(n)
}
