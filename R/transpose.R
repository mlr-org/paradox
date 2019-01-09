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

#' @S3method transpose paradox_design
transpose.paradox_design = function(.l, filter_na = TRUE, ...) {
  assert_flag(filter_na)
  xs = NextMethod(.l, ...)
  if (filter_na)
    xs = map(xs, function(x) Filter(Negate(is_scalar_na), x))
  return(xs)
}


# adds a class to a design-datatable, so we can add an s3-transpose method, which can help the user
# to filter out NAs
make_paradox_design =  function(d) {
  class(d) = c("paradox_design", class(d))
  return(d)
}
