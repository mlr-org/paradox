#' @title A helper to create trafo functions for repeated parameters.
#'
#' @description
#' A helper to create trafo functions for repeated parameters.
#'
#' @param fun [\code{function}]\cr
#'   A trafo function with the arguments x, dict, tags
#' @param repeated.param_id [\code{character(1)}]\cr
#'   The id of the parameter the that is repeated.
#' @param additional.params [\code{character}]\cr
#'   Additional parameter ids that will be passed inside the \code{dict} object.
#' @return function
#' @export
trafoOnRepeatedParam = function(fun, repeated.param_id, additional.params = character(0L)) {
  assert_function(fun, args = c("x", "dict", "tags"))
  assert_string(repeated.param_id)
  assert_character(additional.params)
  function(x, dict, tags) {
    x = ensureDataTable(x)
    ind = names(which(BBmisc::vlapply(tags, function(z) paste0(repeated.param_id, ".repeated") %in% z)))
    ind.additional = assert_subset(additional.params, names(x))
    dict = c(dict, as.list(x)[ind.additional])
    res = fun(x = x[, ind, with = FALSE], dict = dict, tags = tags)
    res = ensureDataTable(res, nrows = nrow(x))
    x[, (ind) := NULL]
    x[, (ind.additional) := NULL]
    x[, (names(res)) := res]
    return(x)
  }
}