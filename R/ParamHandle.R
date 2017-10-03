ParamHandle = R6Class("ParamHandle",
  inherit = ParamBase, # FIXME: Are we sure? Yes!
  public = list(
    id = NULL,
    node = NULL,
    val = NULL,
    root = NULL,
    depend = NULL,  # by default no dependency
    flatval = NULL,
    mand.children = NULL,
    cond.children = NULL,
    initialize = function(id = NULL, node = NULL, val = NULL, depend = NULL) {
      self$id = id
      self$node = node
      self$val = val
      self$depend = depend
      self$mand.children = new.env()
      self$cond.children = new.env()
    },
    addMandChild = function(cnodehandle) {
      assign(cnodehandle$id, cnodehandle, self$mand.children)
      self$flatval$mand = names(self$mand.children)
      return(cnodehandle)
    },
    addCondChild = function(cnodehandle) {  # rbf kernal params
      assign(cnode$id, cnodehandle, self$cond.children)
      self$flatval$cond = names(self$cond.children)
      return(cnodehandle)
    },
    addChildren = function(flatnodes) {

    },
    setParent = function(pnode) {
      self$depend = pnode
    },
    sampleCurrentNode = function() {
      print(self$val)
      #self$node$sample()
    },
    sampleMandChildChain = function() {
      #for(handle in self$mand.children) handle$node$sample()
      if(length(self$mand.children) == 0) return(NULL)
      for(name in names(self$mand.children)) {
        handle = self$mand.children[[name]]
        handle$sample()
      }
    },
    sampleCondChildChain = function(expr = function(x) {return(TRUE)}) {
      if(length(self$cond.children) == 0) return(NULL)
      for(name in names(self$cond.children)) {
        handle = self$cond.children[[name]]
        #if(expr(self$node)) handle$node$sample()
        if(expr(self$node)) handle$sample()
      }
    },
    sample = function() {
      self$sampleCurrentNode()
      self$sampleMandChildChain()
      self$sampleCondChildChain()
    }
  )
)
