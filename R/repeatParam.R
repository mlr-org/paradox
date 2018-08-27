#' @title Repeat a single Parameter
#'
#' @description
#' Creates a list of a single Parameter repeated_
#' Each parameter is tagged with \code{<param$id>.repeated}.
#' This way it can be determined that they belong together
#'
#' @param n [\code{integer(1)}]\cr
#'   How often should this parameter be copied?
#' @param param [\code{ParamSimple(1)}]\cr
#'   The parameter that should be repeated_
#' @return List of Parameters
#' @export
repeatParam = function(n = 1L, param) {
  assert_int(n)
  assert_class(param, "ParamSimple")
  joining_id = paste0(param$id, ".repeated")
  lapply(seq_len(n), function(i) {
    this_param = param$clone()
    this_param$id = paste0(joining_id, "_", i)
    this_param$tags = c(this_param$tags, joining_id)
    this_param
  })
}
