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
    id = NULL,      # by default, id should be the same with the node id which this handle points to
    id.decorator = NULL,  # namespace for parameters
    node = NULL,    # simple ParamNode the handle(pointer) point to
    val = NULL,     # the value of the SimpleParamNode it points to. val is used for sampling
    flatval = NULL, # if the node is itself a tree, this hold the preroot traversal of the tree
    depend = NULL,  # depend is a list with field <id><func>
    parent = NULL,  # the ParamHandle of parent node.
    root = NULL,    # root has to be changed when parent changed!
    reldepth = 0L,  # reldepth has to be updated when parent changed!
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
      haveDep = !is.null(self$depend)
      depNc = is.null(self$depend$fun) || is.null(self$depend$id)
      if (haveDep && depNc) stop("dependency must be a list(id = id,fun =quote(expression)) form!")
      self$parent = parent
      if (!is.null(parent)) {
        self$root = ifelse(is.null(parent$root), parent, parent$root)
      }
      else self$root = NULL
      self$reldepth = ifelse(is.null(parent), 0, (parent$reldepth + 1))
      self$mand.children = new.env()
      # environments are used because they have both unique names and reference semantic, the new.env will take global environment as parent so will not blur the search path
      self$cond.children = new.env()
      self$visitor = ParamVisitor$new(self)
    },

    # public methods
    lazyChecker = function() {
      dict = as.list(self$parent$val)
      dict = setNames(dict, self$parent$id)
      isTRUE(eval(self$depend$fun, envir = dict))
    },

    setRoot = function(root) {
      self$root = root
    },


    # return wether the parent took the defined value
    isDependMet = function() {
      depend.null = is.null(self$depend)
      parent.null = is.null(self$parent)
      parent.val.null = is.null(self$parent$val)
      if (depend.null) return(TRUE)  # Free Hyper-Parameter, no constraint
      # Now there is dependency. It is ok to have parent but no depend though, which is MandChild
      if (parent.null || parent.val.null) {
        warning("ParamHandle$isDependMet: parent value has not been specified yet")
        return(TRUE)  #FIXME: SHOULD HERE BE warning or error?
      }
      cflag = self$lazyChecker()
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
      self$reldepth = self$parent$reldepth + 1L
      self$root = self$parent$root
    },

    sampleCurrentNode = function() {
      if (is.null(self$node)) return(NULL)  # No ParamSimple specified for the current class
      if (self$isDependMet()){
        # debug: self$node$id
        self$val = self$sampleNode()  # self$node$sample will cause infinite recursion
      }
    },

    sampleNode = function() {
      val = self$node$sampleVector()  # print val for debug
      return(val)
    },

    sampleMandChildChain = function() {
      if (length(self$mand.children) == 0) return(NULL)
      for (name in names(self$mand.children)) {
        handle = self$mand.children[[name]]
        handle$asample()
      }
    },

    sampleCondChildChain = function() {
      if (length(self$cond.children) == 0) return(NULL)
      for (name in names(self$cond.children)) {
        handle = self$cond.children[[name]]
        flag = handle$isDependMet()
        if (flag) {
          handle$asample()
        }
      }
    },

    asample = function() {
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
    },

    render2str = function() {
      self$asample()
      self$toStringVal()
    },

    sample = function(n = 1) {
      xs = lapply(1:n, function(i) {
        self$asample()
        self$visitor$toFlat()
      })
      colns = unique(names(unlist(xs)))
      ncol = length(colns)
      df = data.frame(xs[[1]])
      j = 2
      while (j <= n) {
        # this line is tested but in user API not used
        df = plyr::rbind.fill(df, data.frame(xs[[j]]))
        j = j + 1
      }
      as.data.table(df)
    },

    getMandChildrenName = function() {
      names(self$mand.children)
    },

    getCondChildrenName = function() {
      names(self$cond.children)
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

PHinge = R6Class("PHinge",
  inherit = ParamHandle,
  public = list(
    initialize = function(id = NULL) {
      super$initialize(id = id, node = NULL, val = NULL, depend = NULL, parent = NULL)
    },

    setNamePrefix = function(nprefix) {
      self$id.decorator = nprefix
      lapply(self$mand.children, function(x) x$id.decorator = nprefix) # this line  is not useful for the current version but severs as namespace definition for future.
    },

    asample = function() {
      self$sampleMandChildChain()
    },

    getList = function() {
      res.list = lapply(self$mand.children, function(x) x$visitor$toFlat())  # PHinge is required to only have mand child
      Reduce(c, res.list)
    },

    sample = function(n) {
      subspace.list = lapply(self$mand.children, function(x) x$sample(n))  # PHinge is required to only have mand child.
      # This function PHinge$sample(n) is calling recursion from ParamHandle$sample(n). Whilist, ParamSetTree$sample will sample another Tree
      dt.raw = Reduce(cbind, subspace.list)  # combine all hyper-parameter subspaces
      if(!is.null(self$id.decorator)) names(dt.raw) = paste(self$id.decorator, names(dt.raw), sep = ".")
      dt.raw
    },

    toStringVal = function() {
      num.mand = length(self$mand.children)
      if (num.mand > 0) self$printMandChildChainVal()
    }
)
  )
