#' @title Generate a Random Design
#'
#' @description
#' Generates a design with randomly drawn points.
#' Base [ParamSet]s, [ParamSetCollection]s, and [ParamSetShadow]s use the same
#' native uniform engine as [`SamplerUnif`]. Values are drawn in public
#' parameter order, so the values and random-number stream agree exactly.
#' If dependencies do not hold, values are set to `NA` in the resulting
#' data.table.
#' A zero-level categorical parameter can produce a typed zero-row design;
#' requesting one or more rows from it errors before randomness is consumed.
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
#' )
#' generate_design_random(pset, 10)
generate_design_random = function(param_set, n) {
  # Design owns a graph-aware deep clone, as it did through SamplerUnif. The
  # native operation validates bounded closed kinds and handles the unusual
  # zero-dimensional/zero-row RNG contract without a second sampler path.
  assert_param_set(param_set)
  sampled_param_set = param_set$clone(deep = TRUE)
  data = .Call(C_sampler_unif_sample_builtin, sampled_param_set, n)
  Design$new(sampled_param_set, data, remove_dupl = FALSE)
}
