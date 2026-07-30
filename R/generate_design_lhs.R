#' @title Generate a Space-Filling LHS Design
#'
#' @description
#' Generate a space-filling design using Latin hypercube sampling. Dependent
#' parameters whose constraints are unsatisfied generate `NA` entries in
#' their respective columns.
#' The returned [`Design`] retains the exact `param_set` reference supplied by
#' the caller.
#'
#' @param param_set ([`ParamSet`]).
#' @param n (`integer(1)`) \cr
#'   Number of points to sample.
#' @param lhs_fun (`function(n, k)`)\cr
#'   Function to use to generate a LHS sample, with n samples and k values per param.
#'   LHS functions are implemented in package \pkg{lhs}, default is to use [lhs::maximinLHS()].
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
#' if (requireNamespace("lhs", quietly = TRUE)) {
#'   generate_design_lhs(pset, 10)
#' }
generate_design_lhs = function(param_set, n, lhs_fun = NULL) {
  if (is.null(lhs_fun)) {
    require_namespaces("lhs")
    lhs_fun = lhs::maximinLHS
  }
  # LHS designs have historically retained the caller's exact ParamSet
  # reference. This differs deliberately from random designs, whose Sampler
  # owns a clone. Root an exact receipt for that public source, do all
  # callback-capable work on an owned graph, bind the source reference, and
  # make the allocation-free receipt check the terminal operation.
  assert_param_set(param_set, no_untyped = TRUE)
  n = assert_count(n, coerce = TRUE)
  assert_function(lhs_fun, args = c("n", "k"))
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
    d = lhs_fun(n, k = dimension)
  }
  colnames(d) = ids
  d = sampled_param_set$qunif(d)
  design = Design$new(
    sampled_param_set,
    d,
    remove_dupl = FALSE
  ) # Preserve the requested number of points.
  # `$<-` allocates while preparing S3 dispatch even for an ordinary R6
  # environment. Bind directly before the terminal receipt barrier so a
  # finalizer during assignment cannot leave a stale source generation paired
  # with the sampled data.
  base::assign(
    "param_set",
    param_set,
    envir = design,
    inherits = FALSE
  )
  .Call(C_param_set_internal_tuning_receipt, source_receipt)
  design
}
