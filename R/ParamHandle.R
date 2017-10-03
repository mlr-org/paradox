ParamHandle = R6Class("ParamHandle",
  inherit = ParamBase, # FIXME: Are we sure? Yes!
  public = list(
    node = NULL,
    val = NULL,
    root = NULL,
    depend = NULL,  # by default no dependency
    flatval = NULL,
    mand.children = NULL,
    cond.children = NULL,
    initialize = function(node = NULL, val = NULL, depend = NULL) {
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
      self$node$sample()
    },
    sampleMandChildChain = function() {
      for(handle in self$mand.children) handle$node$sample()
    },
    sampleCondChildChain = function(expr) {
      for(handle in self$cond.children) {
        if(expr(self$node)) handle$node$sample()
      }
    },
    sample = function() {
      sampleCurrentNode()
      sampleMandChildChain()
      sampleCondChildChain()
    }
  )
)
