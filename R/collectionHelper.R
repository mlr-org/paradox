#' @title A helper to create trafo functions for collections
#'
#' @description
#' A helper to create trafo functions for collections
#'
#' @param fun [\code{function}]\cr
#'   A trafo function with the arguments x, dict, tags
#' @param collection.param.id [\code{character(1)}]\cr
#'   The id of the parameter the collection was created from
#' @param additional.params [\code{character}]\cr
#'   Additional parameter ids that will be passed inside the \code{dict} object.
#' @return function
#' @export
collectionHelper = function(fun, collection.param.id, additional.params = list()) {
  assertFunction(fun, args = c("x", "dict", "tags"))
  assertString(collection.param.id)
  assertCharacter(additional.params)
  function(x, dict, tags) {
    x = as.list(x)
    ind = names(which(BBmisc::vlapply(tags, function(z) paste0(collection.param.id, ".collection") %in% z)))
    ind.additional = assertSubset(additional.params, names(x))
    dict = c(dict, x[ind.additional])
    res = fun(x = x[ind], dict = dict, tags = tags)
    res = as.list(res)
    assertList(res, names = "strict")
    x[ind] = NULL
    x[ind.additional] = NULL
    x[names(res)] = res
    return(x)
  }
}