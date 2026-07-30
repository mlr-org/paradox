#' @title Generate a Space-Filling Sobol Sequence Design
#'
#' @description
#' Generate a space-filling design using a Sobol sequence. Dependent
#' parameters whose constraints are unsatisfied generate `NA` entries in
#' their respective columns.
#' The returned [`Design`] retains the exact `param_set` reference supplied by
#' the caller.
#'
#' Uses [spacefillr::generate_sobol_set].
#'
#' Note that non determinism is achieved by sampling the seed argument via
#' `sample(.Machine$integer.max, size = 1L)`.
#'
#' @param param_set ([`ParamSet`]).
#' @param n (`integer(1)`) \cr
#'   Number of points to sample.
#' @return [`Design`].
#'
#' @family generate_design
#' @export
#' @examples
#' pset = ps(
#'   ratio = p_dbl(lower = 0, upper = 1),
#'   letters = p_fct(levels = letters[1:3])
#' )
#'
#' if (requireNamespace("spacefillr", quietly = TRUE)) {
#'   generate_design_sobol(pset, 10)
#' }
generate_design_sobol = function(param_set, n) {
  require_namespaces("spacefillr")
  # Sobol designs preserve the same exact caller-owned ParamSet reference as
  # ordinary Design and LHS construction. Root that source while the external
  # generator runs against an owned graph, then bind immediately before an
  # exact allocation-free generation barrier. Random designs are
  # intentionally different because their Sampler has always owned a clone.
  assert_param_set(param_set, no_untyped = TRUE)
  n = assert_count(n, coerce = TRUE)
  source_receipt = param_set_internal_tuning_plan(
    param_set,
    character()
  )$receipt
  sampled_param_set = param_set$clone(deep = TRUE)
  assert_param_set(sampled_param_set, no_untyped = TRUE)
  assert_positive_quantile_rows(sampled_param_set, n)

  ids = sampled_param_set$ids()
  dimension = sampled_param_set$length
  if (n == 0) {
    d = matrix(numeric(0), nrow = 0, ncol = dimension)
  } else {
    seed = sample(.Machine$integer.max, size = 1L)
    d = spacefillr::generate_sobol_set(
      n,
      dim = dimension,
      seed = seed
    )
  }
  colnames(d) = ids
  d = sampled_param_set$qunif(d)
  design = Design$new(
    sampled_param_set,
    d,
    remove_dupl = FALSE
  ) # Preserve the requested number of points.
  # See generate_design_lhs(): bind before the terminal receipt scan because
  # the R `$<-` replacement path allocates even for an ordinary environment.
  base::assign(
    "param_set",
    param_set,
    envir = design,
    inherits = FALSE
  )
  .Call(C_param_set_internal_tuning_receipt, source_receipt)
  design
}
