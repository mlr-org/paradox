ParamHandle = R6Class("ParamHandle",
  inherit = ParamBase, # FIXME: Are we sure? Yes!
  public = list(
   
    # member variables
    id = NULL,
    node = NULL,
    val = NULL,
    root = NULL,
    parent = NULL,
    depend = NULL,  # gamma param is valid only when kernel = "RBF" 
    flatval = NULL,
    mand.children = NULL,
    cond.children = NULL,
    require.expr = NULL,
    
    # constructor
    initialize = function(id = NULL, node = NULL, val = NULL, parent = NULL, depend = NULL, require.exp = NULL) {
      self$id = id
      self$node = node
      self$val = val
      self$parent = parent
      self$depend = depend
      self$mand.children = new.env()
      self$cond.children = new.env()
      self$require.expr = function(x) {
        if(is.null(self$depend)) return(TRUE)
          return(x$val == self$depend)
      }
    },

    # public methods
    addMandChild = function(cnodehandle) {
      assign(cnodehandle$id, cnodehandle, self$mand.children)
      self$flatval$mand = names(self$mand.children)
      return(cnodehandle)
    },
    addCondChild = function(cnodehandle) {  # rbf kernal params
      assign(cnodehandle$id, cnodehandle, self$cond.children)
      self$flatval$cond = names(self$cond.children)
      return(cnodehandle)
    },
    addChildren = function(flatnodes) {

    },
    setParent = function(pnode) {
      self$parent = pnode
    },
    sampleCurrentNode = function() {
      self$node$sample()
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
    printCurrentNode = function() {
      catf("%s:%s", self$id, self$val)
    },
    printMandChildChain = function() {
      if(length(self$mand.children) == 0) return(NULL)
      for(name in names(self$mand.children)) {
        handle = self$mand.children[[name]]
        handle$toString()
      }
    },
    printCondChildChain = function() {
      if(length(self$cond.children) == 0) return(NULL)
      for(name in names(self$cond.children)) {
        handle = self$cond.children[[name]]
        if(handle$require.expr(self)) handle$toString()
      }
    },
    toString = function() {
      self$printCurrentNode()
      self$printMandChildChain()
      self$printCondChildChain()
    }
  )
)

