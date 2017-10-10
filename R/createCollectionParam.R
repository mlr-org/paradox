#' @title Create a collection from a single Parameter
#'
#' @description
#' Create a collection from a single Parameter
#'
#' @param n [\code{integer(1)}]\cr
#'   How often should this parameter be copied?
#' @param param [\code{ParamSimple(1)}]\cr
#'   The parameter that should be repeated.
#' @return List of Parameters
#' @export
createCollectionParamList = function(n = 1L, param) {
  assertInt(n)
  assertClass(param, "ParamSimple")
  collection.id = paste0(param$id, ".collection")
  lapply(seq_len(n), function(i) {
    this.param = param$clone()
    this.param$id = paste0(collection.id, ".", i)
    this.param$tags = c(this.param$tags, collection.id)
    this.param
  })
}
