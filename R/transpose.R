#' @title Transpose lists of lists
#'
#' @description
#' Extends [mlr3misc::transpose].
#'
#' @param .l A data.table generated from `sample()` of a [Sampler] or a `generate_design` function.
#' @param filter_na Should NAs - inactive paramater values due to unsatisfied dependencies - be filtered out?
#' @return `list()`.
#' @name transpose
#' @export
NULL

# FIXME: doc trafo and doc transpose method better
# FIXME: add more unit tests for reasonable trafo use cases

#' @S3method transpose paradox_design
transpose.paradox_design = function(.l, filter_na = TRUE, trafo = FALSE, ...) {
  assert_flag(filter_na)
  xs = NextMethod(.l, ...)
  if (filter_na)
    xs = map(xs, function(x) Filter(Negate(is_scalar_na), x))
  if (trafo) {
    ps = attr(.l, ".param_set")
    if (!ps$has_trafo)
      stopf("Design was not generated from a param set with a trafo!")
    xs = map(xs, function(x) ps$trafo(x, ps))
  }
  return(xs)
}


# adds a class to a design-datatable, so we can add an s3-transpose method, which can help the user
# to filter out NAs
make_paradox_design =  function(d, ps) {
  # FIXME: should we clone here to be safe?
  attr(d, ".param_set") = ps
  class(d) = c("paradox_design", class(d))
  return(d)
}
