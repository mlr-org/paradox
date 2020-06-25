#' @title transpose
#'
#' @description
#' Converts [data.table::data.table] into a list of lists of points, possibly
#' removes `NA` entries of inactive parameter values due to unsatisfied
#' dependencies, and possibly calls the `trafo` function of the [ParamSet].
#'
#' @param data ([data.table::data.table])\cr
#' Rows are points and columns are parameters.
#'
#' @param ps ([ParamSet])\cr
#' If `trafo = TRUE`, used to call trafo function.
#'
#' @param filter_na (`logical(1)`)\cr
#' Should `NA` entries of inactive parameter values be removed due to
#' unsatisfied dependencies?
#'
#' @param trafo (`logical(1)`)\cr
#' Should the `trafo` function of the [ParamSet] be called?
transpose = function(data, ps = NULL, filter_na = TRUE, trafo = TRUE) {
  assert_data_table(data)
  assert_flag(filter_na)
  assert_flag(trafo)
  xs = transpose_list(data)
  if (filter_na) {
    xs = map(xs, function(x) Filter(Negate(is_scalar_na), x))
  }
  if (!is.null(ps) && trafo) {
    if(ps$has_trafo) xs = map(xs, function(x) ps$trafo(x, ps))
  }
  return(xs)
}
