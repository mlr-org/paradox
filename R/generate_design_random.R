#' @title Generate a random design.
#'
#' @description
#' Generates a design with randomly drawn points.
#' Internally uses [SamplerUnif], hence, also works for param sets with dependencies.
#' If dependencies do not hold, values are set to NA in the resulting data.table.
#'
#' @param param_set [ParamSet].
#' @param n `integer(1)` \cr
#'   Number of points to draw randomly.
#' @return [data.table]
#'
#' @family generate_design
#' @export
generate_design_random = function(param_set, n) {
  # arg checks done by SamplerUnif and sample
  SamplerUnif$new(param_set)$sample(n)
}
