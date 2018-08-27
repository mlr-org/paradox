#' @title A helper to create trafo functions for repeated parameters.
#'
#' @description
#' A helper to create trafo functions for repeated parameters.
#'
#' @param fun [\code{function}]\cr
#'   A trafo function with the arguments x, dict, tags
#' @param repeated_param_id [\code{character(1)}]\cr
#'   The id of the parameter the that is repeated_
#' @param additional_params [\code{character}]\cr
#'   Additional parameter ids that will be passed inside the \code{dict} object.
#' @return function
#' @export
trafoOnRepeatedParam = function(fun, repeated_param_id, additional_params = character(0L)) {
  assert_function(fun, args = c("x", "dict", "tags"))
  assert_string(repeated_param_id)
  assert_character(additional_params)
  function(x, dict, tags) {
    x = ensure_data_table(x)
    ind = names(which(BBmisc::vlapply(tags, function(z) paste0(repeated_param_id, "_repeated") %in% z)))
    ind_additional = assert_subset(additional_params, names(x))
    dict = c(dict, as.list(x)[ind_additional])
    res = fun(x = x[, ind, with = FALSE], dict = dict, tags = tags)
    res = ensure_data_table(res, nrows = nrow(x))
    x[, (ind) := NULL]
    x[, (ind_additional) := NULL]
    x[, (names(res)) := res]
    return(x)
  }
}