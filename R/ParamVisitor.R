#' @title Visitor to traverse ParamHandle
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to visit ParamHandle.
#'
#' @return [\code{\link{ParamVisitor}}].
#' @family ParamHelpers
#' @export

ParamVisitor = R6Class("ParamVisitor",
  inherit = ParamBase, # FIXME: Are we sure? Yes!
  public = list(

    # member variables
    host = NULL,
    # constructor
    initialize = function(host) {
      self$host = host
    },

    # public methods

    # Traverse current node's mandatory child and add node arg to  it
    traverseMand = function(arg)
    {
      if (length(self$host$mand.children) == 0) return(FALSE)
      for (name in names(self$host$mand.children)) {
        handle = self$host$mand.children[[name]]
        if (handle$visitor$insertNode(arg)) {
          return(TRUE)
        }
      }
      return(FALSE)
    },

    # Traverse current node's conditional child and add node arg to it
    traverseCond = function(arg) {
      if (length(self$host$cond.children) == 0) return(FALSE)
      for (name in names(self$host$cond.children)) {
        handle = self$host$cond.children[[name]]
        if (handle$visitor$insertNode(arg)) {
          return(TRUE)
        }
      }
      return(FALSE)
    },

    fun.hit = function(x, args) {
      return(TRUE)
    },

    # from a list of ParamNode, parse them to  Tree
    parseFlat = function(node.list) {
      len = length(node.list)
      SAFECOUNTER = 0
      mnames = lapply(node.list, function(x) x$node$id)
      names(node.list) = unlist(mnames)
      while(length(node.list) != 0) {
        for (name in mnames) {
          catf("parsing %s",name)
          if (self$insertNode(node.list[[name]])) node.list[[name]] = NULL
          catf("number in wait list left %d",length(node.list))
        }
        SAFECOUNTER = SAFECOUNTER + 1
        if (SAFECOUNTER > len) stop("wrong flat input!")
      }
    },

    ## traverse the tree to find out if the the arg could be inserted as leave
    insertNode = function(arg) {
      # always check arg$depend not null!!
      if (is.null(arg$depend)) {
        catf("hit no depend : %s", arg$node$id)
        self$host$addMandChild(ParamHandle$new(node = arg$node))
        return(TRUE)
      }
      # now the input arg has a field called depend
      if (is.null(arg$depend$id) && is.null(arg$func)) stop("need at least id or func in depend!")
      if ((self$host$id == arg$depend$id))
      {
        catf("hit depend:  %s", arg$node$id)
        self$host$addCondChild(ParamHandle$new(node = arg$node, depend = arg$depend))
        return(TRUE)
      }
      if (self$traverseMand(arg)) return(TRUE)  # child will be added inside the recursion
      if (self$traverseCond(arg)) return(TRUE)  # child will be added inside the recursion
      return(FALSE)
    },

    # transform the tree structure to a list and return the list
    toFlat = function(res = list()) {
      print(self$host$node$id)
      print(length(res))
      res[[self$host$node$id]] = self$host$node
      if (length(self$host$mand.children) > 0) {
      for (name in names(self$host$mand.children)) {
        handle = self$host$mand.children[[name]]
        #print(handle$node$id)
        res[[handle$node$id]] = handle$node
        res = handle$visitor$toFlat(res)
      }
      } # if
      if (length(self$host$cond.children) > 0) {
      for (name in names(self$host$cond.children)) {
        handle = self$host$cond.children[[name]]
        #print(handle$node$id)
        res[[handle$node$id]] = handle$node
        res = handle$visitor$toFlat(res)
      }
      } # if
      return(res)
    },

    # check if the flat form of paramset violates the dependency
    checkValidFromFlat = function(input = list(model = list(val = "svm"), kernel = list(val = "rbf", depend = list(val = "svm")), gamma =list(val = "0.3" ,depend = list(val = "rbf")))) {
      fq = list()  # finished queue
      wq = input   # waiting queue
      hit = TRUE
      findDependNode = function(fq, node) {
        for (name in names(fq)) {
          if (node$depend$val == fq[[name]]$val) return(TRUE)
        }
        return(FALSE)
      }
      while(hit)
      {
        hit = FALSE
        for (name in names(wq)) {
          if (is.null(wq[[name]]$depend) | findDependNode(wq[[name]])) {
            regi(name)
            fq[[name]] =  wq[[name]]
            wq[[name]] = NULL
            hit = TRUE
          }
        }
      }
      if (length(wq) > 0) stop("invalid parameter set!")
    }
  ) # public 
) # Class

