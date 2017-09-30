#' @title Base Class for ParamHelpers
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent parameters.
#' This is the abstract base class, do not use directly!
#'
#' @return [\code{\link{ParaBase}}].
#' @family ParamHelpers
#' @export
ParaBase = R6Class("ParaBase",
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
ParaNode = R6Class("ParaNode",
  inherit = ParaBase,
  public = list(
  ),
  private = list(
  )
)
