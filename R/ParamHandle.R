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
  inherit = ParamBase, # FIXME: Are we sure?
  public = list(
    # member variables
    id = NULL,  # by default, id should be the same with the node id which this handle points to
    node = NULL, # simple ParamNode the handle(pointer) point to
    val = NULL,  # the value of the SimpleParamNode it points to. val is used for sampling
    flatval = NULL, # if the node is itself a tree, this hold the preroot traversal of the tree
    depend = NULL,  # depend is a list with field <id><[val][func]> which will decide if the current node is going to be activated in a sampling process, if depend$func() is True, then the sampling function is called
    require.expr = NULL,  # function to take arguments as parents, but not using the self$parent
    parent = NULL,
    root = NULL,    # root has to be changed when parent changed!
    reldepth = 0L,  # reldepth has to be updated when parent changed!
    #
    mand.children = NULL,
    cond.children = NULL,
    visitor = NULL, # visitor pattern, which decouples the data structure and operation

    # constructor
    initialize = function(id = NULL, node = NULL, val = NULL, depend = NULL, parent = NULL) {
      if (is.null(id) && is.null(node)) stop("either set id or node for handle!")
      self$id = ifelse(is.null(id), node$id, id)
      self$node = node
      self$val = val
      self$depend = depend
      self$require.expr = function(parent) {
        if (is.null(parent$val)) return(FALSE)
        if (is.null(self$depend)) return(TRUE)
        return(parent$val == self$depend$val)
      }
      self$parent = parent
      if (!is.null(parent)) {
        self$root = ifelse(is.null(parent$root), parent, parent$root)
      }
      else self$root = NULL
      self$reldepth = ifelse(is.null(parent), 0, (parent$reldepth + 1))
      self$mand.children = new.env()  # environments are used because they have both unique names and reference semantic, the new.env will take global environment as parent so will not blur the search path
      self$cond.children = new.env()
      self$visitor = ParamVisitor$new(self)
    },

    # public methods
    lazyChecker = function() {
      if (is.null(self$depend$fun)) return(TRUE)
      dict = as.list(self$parent$val)
      dict = setNames(dict, self$parent$id)
      isTRUE(eval(self$depend$fun, envir = dict))
    },

    setRoot = function(root) {
      self$root = root
    },

    isDependMet = function() {
      # return wether the parent took the defined value
      if (is.null(self$depend)) return(TRUE)  # Free Hyper-Parameter, no constraint
      if (is.null(self$parent)) return(TRUE)  # No parent node
      # it is ok to have parent but no depend, which is MandChild
      if (is.null(self$parent$val)) {
        warning("ParamHandle$isDependMet: parent value has not been specified yet")
        return(TRUE)  #FIXME: SHOULD HERE BE FALSE OR TRUE? Or Should we stop here? Or just set warning messages? Currently we decide to just have a warning
      }
      if (is.null(self$depend$val) && is.null(self$depend$fun)) stop("ParamHandle$isDependMet: ill defined dependency in self$depend$val")
      # eflag = (self$parent$val == self$depend$val)
      cflag = self$lazyChecker()
      # return(eflag || cflag)
      return(cflag)
    },

    addMandChild = function(cnodehandle) {
      cnodehandle$setParent(self)
      cnodehandle$setRoot(self$getRoot)
      assign(cnodehandle$id, cnodehandle, self$mand.children)
      self$flatval$mand = names(self$mand.children)
      return(cnodehandle)
    },

    addCondChild = function(cnodehandle) {
      cnodehandle$setParent(self)
      cnodehandle$setRoot(self$getRoot)
      assign(cnodehandle$id, cnodehandle, self$cond.children)
      self$flatval$cond = names(self$cond.children)
      return(cnodehandle)
    },

    setParent = function(pnode) {
      self$parent = pnode
      self$reldepth = self$parent$reldepth + 1
      self$root = self$parent$root
    },

    sampleCurrentNode = function() {
      if (is.null(self$node)) return(NULL)  # No ParamSimple specified for the current class
      if (self$isDependMet()) {
        #catf("sampling %s\n", self$node$id)  # for future debug, please do not delete!
        self$val = self$sampleNode()  # self$node$sample will cause infinite recursion
        #catf("\n")  for future debug, please do not delete!
      }
    },

    sampleNode = function() {
      val = self$node$sampleVector()
      # catf("%s", val) # for future debug, please do not delete!
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
        # if (handle$require.expr(self)) {
        flag = handle$isDependMet()
        if (flag) {
          handle$sample()
        }
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
      indent = paste(rep(indentsym, self$reldepth), collapse = "")
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
      cns = names(self$cond.children)
      for (name in cns) {
        handle = self$cond.children[[name]]
        flag = handle$isDependMet()
        # if (handle$require.expr(self)) {
        if (flag) {
          handle$toStringVal()
        }
      }
    },

    toStringVal = function() {
      self$printCurrentNodeVal()
      num.mand = length(self$mand.children)
      num.cond = length(self$cond.children)
      if (num.mand > 0) self$printMandChildChainVal()
      if (num.cond > 0) self$printCondChildChainVal()
    }
  ),

    active = list(
    getFirstMandChild = function() {
      mnames = names(self$mand.children)
      self$mand.children[[mnames[1L]]]
    },

    getRoot = function() {
        self$root
    },

    nochild = function() {
      if (length(self$host$mand.children) > 0) return(FALSE)
      if (length(self$host$cond.children) > 0) return(FALSE)
      return(TRUE)
    }
    )
)

