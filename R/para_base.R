#' @title Base Class for ParamHelpers
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent parameters.
#' This is the abstract base class, do not use directly!
#'
#' @return [\code{\link{ParamBase}}].
#' @family ParamHelpers
#' @export
ParamBase = R6Class("ParamBase",
  public = list(
    ),
  private = list(
    )
  )

#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent parameters.
#'
#' @return [\code{\link{ParaNode}}].
#' @family ParamHelpers
#' @export
ParamNode = R6Class("ParamNode",
  inherit = ParamBase,
  public = list(
  ),
  private = list(
  )
)
