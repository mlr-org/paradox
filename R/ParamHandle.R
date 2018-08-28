#' @title Handle Class for ParamNode
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent the tree structure of ParamSetBase.
#'
#' @return [\code{\link{ParamHandle}}].
#' @family ParamHelpers
ParamHandle = R6Class("ParamHandle",
  inherit = ParamBase, # FIXME: Are we sure? Yes!
  public = list(

    root = NULL,

    # constructor
    initialize = function() {
    }
  )
)

