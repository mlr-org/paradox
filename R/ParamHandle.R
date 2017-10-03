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
    reldepth = 0,  # this arg has to be updated when parent changed!
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
      self$reldepth = ifelse(is.null(parent), 0, (parent$reldepth + 1))
      self$mand.children = new.env()
      self$cond.children = new.env()
      self$require.expr = function(x) {
        if(is.null(self$depend)) return(TRUE)
          return(x$val == self$depend)
      }
    },

    # public methods
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
      indent = paste(rep("--",self$reldepth), collapse = "")
      catf("%s-%s:%s", indent, self$id, self$val)
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
    },

    traverseMand = function(arg)
    {
      if(length(self$mand.children) == 0) return(NULL)
      for(name in names(self$mand.children)) {
        handle = self$mand.children[[name]]
        if(handle$traverse(arg)) return(TRUE)
      }
    },
    traverseCond = function(arg) {
      if(length(self$cond.children) == 0) return(NULL)
      for(name in names(self$cond.children)) {
        handle = self$cond.children[[name]]
        if(handle$traverse(arg)) return(TRUE)
      }
    },
    fun.hit = function(x, args) {
      return(TRUE)
    },
    parseFlat = function(node.list) {
      len = length(node.list)
      SAFECOUNTER = 0
      while(length(node.list) != 0) {
        for(name in names(node.list)) {
          print(name)
          if(self$traverse(node.list[[name]])) node.list[[name]] = NULL
          print(length(node.list))
        }
        SAFECOUNTER = SAFECOUNTER + 1
        if(SAFECOUNTER > 10 * len) stop("wrong flat input!")
      }
    },
    traverse = function(arg) {
      # always check arg$depend not null!!
      if(is.null(arg$depend)) {
        self$addMandChild(ParamHandle$new(id = arg$id, val = arg$val))
        return(TRUE) 
      }
      if(self$val == arg$depend)
      { 
        print("hit")
        self$addMandChild(ParamHandle$new(id = arg$id, val = arg$val))
        return(TRUE) 
      }
      if(self$traverseMand(arg)) return(TRUE)
      if(self$traverseCond(arg)) return(TRUE)
      return(FALSE)
    },
    checkValidFromFlat = function(input = list(model = list(val = "svm"), kernel = list(val = "rbf", depend = "svm"), gamma =list(val = "0.3" ,depend = "rbf"))) {
      fq = list()  # finished queue
      wq = input   # waiting queue
      hit = TRUE
      findDependNode = function(fq, node) {
        for(name in names(fq)) {
          if(node$depend == fq[[name]]$val) return(TRUE)
        }
        return(FALSE)
      }
      while(hit)
      {
        hit = FALSE
        for(name in names(wq)) {
          if(is.null(wq[[name]]$depend) | findDependNode(wq[[name]])) {
            regi(name)
            fq[[name]] =  wq[[name]]
            wq[[name]] = NULL
            hit = TRUE
          }
        }
      }
      if(length(wq) > 0) stop("invalid parameter set!")
    }
  )
)

