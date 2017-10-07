#' @title ParamSetTree
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of parameters in a tree form.
#'
#' @return [\code{\link{ParamSetTree}}].
#' @family ParamHelpers
ParamSetTree = R6Class("ParamSetTree",
  inherit = ParamSet,
  public = list(
   
    # member variables
    id = NULL,
    ns = NULL, # node simple
    val = NULL,  
    # val take down the value of a ParamSimple var since in ParamSimple there is no concreate value, but it is left to be discussion whether this value should be here or simple put inside handle ?
    handle = NULL,
    flatval = NULL,
    depend = NULL,
    
    # constructor
    initialize = function(id = NULL, ns, val = NULL, depend = NULL) {
      self$ns = ns
      if(is.null(id)) self$id = ns$id
      else self$id = id
      self$handle = ParamHandle$new(id = self$id, val = val, node = self, depend = depend)
      self$flatval = list()
      self$depend = depend
    },

    # public methods
    sample = function() {
      self$handle$sample()
    }
  ),
  private = list(
  )
)
