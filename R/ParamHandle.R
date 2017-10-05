#' @title Handle Class for ParamNode
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent the tree structure of ParamSet.
#'
#' @return [\code{\link{ParamHandle}}].
#' @family ParamHelpers
#' @export

ParamHandle = R6Class("ParamHandle",
  inherit = ParamBase, # FIXME: Are we sure? Yes!
  public = list(

    # member variables
    id = NULL,
    val = NULL,  # for devolepment
    node = NULL,
    root = NULL,
    parent = NULL,
    depend = NULL,  # gamma param is valid only when kernel = "RBF"
    reldepth = 0,  # this arg has to be updated when parent changed!
    flatval = NULL,
    mand.children = NULL,
    cond.children = NULL,
    require.expr = NULL,
    visitor = NULL,

    # constructor
    initialize = function(id = NULL, val = NULL, node = NULL, parent = NULL, depend = NULL, require.exp = NULL) {
      self$id = id
      self$node = node
      self$val = val
      self$parent = parent
      self$depend = depend
      self$reldepth = ifelse(is.null(parent), 0, (parent$reldepth + 1))
      self$mand.children = new.env()
      self$cond.children = new.env()
      self$require.expr = function(x) {
        if(is.null(self$depend)) return(TRUE)
          return(x$val == self$depend$val)
      }
      self$visitor = ParamVisitor$new(self)
    },

    # public methods
    isdependMet = function() {  # return wether the parent took the defined value
      if(is.null(self$depend)) return(TRUE)
      if(is.null(self$parent)) return(TRUE)
      if(is.null(self$parent$val)) return(FALSE)
      #if(is.null(self$parent$val)) stop("parent has no value!")
      if(is.null(self$depend$val)) stop("ill defined dependency")
      return(self$parent$val == self$depend$val)
    },

    addMandChild = function(cnodehandle) {
      cnodehandle$setParent(self)
      assign(cnodehandle$id, cnodehandle, self$mand.children)
      self$flatval$mand = names(self$mand.children)
      return(cnodehandle)
    },
    addCondChild = function(cnodehandle) {  # rbf kernal params
      cnodehandle$setParent(self)
      assign(cnodehandle$id, cnodehandle, self$cond.children)
      self$flatval$cond = names(self$cond.children)
      return(cnodehandle)
    },
    addChildren = function(flatnodes) {

    },
    setParent = function(pnode) {
      self$parent = pnode
      self$reldepth = self$parent$reldepth + 1
    },
    sampleCurrentNode = function() {
      if(is.null(self$node)) return(NULL)
      if(self$isdependMet()) {
        catf("sampling %s", self$node$id)
        self$val = self$node$ns$msample()
        self$node$val = self$val
      }
      # self$node$sample will cause infinite recursion
    },

    sampleMandChildChain = function() {
      if(length(self$mand.children) == 0) return(NULL)
      for(name in names(self$mand.children)) {
        handle = self$mand.children[[name]]
        handle$sample()
      }
    },
    sampleCondChildChain = function() {
      if(length(self$cond.children) == 0) return(NULL)
      for(name in names(self$cond.children)) {
        handle = self$cond.children[[name]]
        if(handle$require.expr(self)) handle$sample()
      }
    },
    sample = function() {
      self$sampleCurrentNode()
      self$sampleMandChildChain()
      self$sampleCondChildChain()
    },
    printCurrentNodeVal = function() {
      expect_int(self$reldepth)
      expect_character(self$id)
      indentsym = ifelse(is.null(self$val), "--", "++")
      indent = paste(rep(indentsym,self$reldepth), collapse = "")
      value = ifelse(is.null(self$val), "TBD", self$val)
      BBmisc::catf("%s-%s:%s", indent, self$id, value)
    },
    printMandChildChainVal = function() {
      for(name in names(self$mand.children)) {
        handle = self$mand.children[[name]]
        handle$toStringVal()
      }
    },
    printCondChildChainVal = function() {
      for (name in names(self$cond.children)) {
        handle = self$cond.children[[name]]
        if(handle$require.expr(self)) handle$toStringVal()
      }
    },
    toStringVal = function() {
      self$printCurrentNodeVal()
      if (length(self$mand.children) > 0) self$printMandChildChainVal()
      if (length(self$cond.children) > 0) self$printCondChildChainVal()
    }
  )
)

