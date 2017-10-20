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
    node = NULL, # simple ParamNode the handle(pointer) point to
    val = NULL,  # the value of the SimpleParamNode it points to. val is used for sampling
    flatval = NULL, # if the node is itself a tree, this hold the preroot traversal of the tree
    depend = NULL,  # depend is a list with field <id><[val][func]> which will decide if the current node is going to be activated in a sampling process, if depend$func() is True, then the sampling function is called
    require.expr = NULL,
    #
    parent = NULL,
    root = NULL,    # root has to be changed when parent changed!
    reldepth = 0L,  # reldepth has to be updated when parent changed!
    #
    mand.children = NULL,
    cond.children = NULL,
    visitor = NULL,

    # constructor
    initialize = function(id = NULL, node = NULL, val = NULL, depend = NULL, parent = NULL) {
      if (is.null(id) & is.null(node)) stop("either set id or node for handle!")
      self$id = ifelse(is.null(id), node$id, id)
      self$node = node
      self$val = val
      #
      self$depend = depend
      self$require.expr = function(parent) {
        if (is.null(parent$val)) return(FALSE)
        if (is.null(self$depend)) return(TRUE)
        return(parent$val == self$depend$val)
      }
      #
      self$parent = parent
      if (!is.null(parent)) {
        self$root = ifelse(is.null(parent$root), parent, parent$root)
      }
      else self$root = NULL
      self$reldepth = ifelse(is.null(parent), 0, (parent$reldepth + 1))
      self$mand.children = new.env()
      self$cond.children = new.env()
      self$visitor = ParamVisitor$new(self)
    },

    # public methods
    setRoot = function(root) {
      self$root = root
    },

    isdependMet = function() {  # return wether the parent took the defined value
      if (is.null(self$depend)) return(TRUE)
      if (is.null(self$parent)) return(TRUE)
      if (is.null(self$parent$val)) return(TRUE)  #FIXME: SHOULD HERE BE FALSE OR TRUE?
      #if (is.null(self$parent$val)) stop("parent has no value!")
      if (is.null(self$depend$val)) stop("ill defined dependency")
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
      self$root = self$parent$root
    },
    sampleCurrentNode = function() {
      if (is.null(self$node)) return(NULL)
      if (self$isdependMet()) {
        catf("sampling %s\n", self$node$id)
        self$val = self$sampleNode()
        catf("\n")
        #self$node$val = self$val
      }
      # self$node$sample will cause infinite recursion
    },
    sampleNode = function() {
      val = self$node$sampleVector()
      catf("%s", val)
      return(val)
    },
    sampleMandChildChain = function() {
      if (length(self$mand.children) == 0) return(NULL)
      for (name in names(self$mand.children)) {
        handle = self$mand.children[[name]]
        handle$sample()
      }
    },
    sampleCondChildChain = function() {
      if (length(self$cond.children) == 0) return(NULL)
      for (name in names(self$cond.children)) {
        handle = self$cond.children[[name]]
        if (handle$require.expr(self)) handle$sample()
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
      for (name in names(self$mand.children)) {
        handle = self$mand.children[[name]]
        handle$toStringVal()
      }
    },
    printCondChildChainVal = function() {
      for (name in names(self$cond.children)) {
        handle = self$cond.children[[name]]
        if (handle$require.expr(self)) handle$toStringVal()
      }
    },
    toStringVal = function() {
      self$printCurrentNodeVal()
      if (length(self$mand.children) > 0) self$printMandChildChainVal()
      if (length(self$cond.children) > 0) self$printCondChildChainVal()
    }
  ),
    active = list(
    getFirstMandChild = function() {
      mnames = names(self$mand.children)
      self$mand.children[[mnames[1L]]]
    })
)

