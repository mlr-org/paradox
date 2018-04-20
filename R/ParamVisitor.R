#' @title Visitor to traverse ParamHandle
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to visit attached ParamHandle object(host). Using a separate visitor to a class is a frequently used design pattern which could seperate the operations(traversal the graph structure of the ParamHandle) on the ParamHandle and the ParamHandle pointer themselves. The ParamHandle node has two kinds of children node: the mandatory node and conditional node. Mandatory nodes are unconditional children for a graph-like hyper parameter, for example, the normal SVM has a hyperparameter C which is mandatory node but the hyperparameter gamma only makes sense when the hyperparameter kernel type is rbf kernel.
#'
#' @return [\code{\link{ParamVisitor}}].
#' @family ParamHelpers
#' @export
ParamVisitor = R6Class("ParamVisitor",
  inherit = ParamBase, # FIXME: Are we sure?
  public = list(

    # member variables
    host = NULL,
    # constructor
    initialize = function(host) {
      self$host = host
    },

    # public methods

    # Depth First Traversal of current node's mandatory children and add node [arg] to it
    traverseMand = function(arg) {
      if (length(self$host$mand.children) == 0L) return(FALSE)  # if the current node has no mandatory children, recursion stop
      for (name in names(self$host$mand.children)) {
        handle = self$host$mand.children[[name]]
        if (handle$visitor$insertNode(arg)) {
          # recursion: insertNode(arg), traverseMand(arg), insertNode(arg), ...
          return(TRUE)  # insertNode returns true if the input is a direct child of the current node or recursion of insertNode is true
        }
      }
      return(FALSE)  # no place to insert the input among the current node's direct mandatory children.
    },

    # Traverse current node's conditional child and add node arg to it
    traverseCond = function(arg) {
      if (length(self$host$cond.children) == 0L) return(FALSE)  # if the current node has no conditional children, recursion stop
      for (name in names(self$host$cond.children)) {
        handle = self$host$cond.children[[name]]
        if (handle$visitor$insertNode(arg)) {
          # recursion: insertNode(arg), traverseCond(arg), insertNode(arg), ...
          return(TRUE)  # insertNode returns true if the input is a direct child of the current node or recursion of insertNode is true
        }
      }
      return(FALSE)  # no place to insert the input among the current node's direct conditional children.
    },

   parseFlat = function(node.list) {
      node.list = lapply(node.list, function(x) {
        if ("ParamSimple" %in% class(x)) return(makeCondTreeNode(x))
        return(x)
      })
      len = length(node.list)
      safecounter = 0L  # count how many nodes have been inserted
      mnames = lapply(node.list, function(x) x$node$id)
      names(node.list) = unlist(mnames)
      while (length(node.list) != 0) {
        for (name in mnames) {
          if (self$insertNode(node.list[[name]])) node.list[[name]] = NULL  # if inserted successfully, delete the node in wait list
        }
        safecounter = safecounter + 1L
        if (safecounter > len) stop("parseFlat: parsing did not finish after [length of input] steps")
      }
    },

    ## traverse the tree to find out if the the input could be inserted as leave
    insertNode = function(node.depend) {
      if (is.null(node.depend$depend)) {
        # the input is a top layer hyper-parameter
        self$host$addMandChild(ParamHandle$new(node = node.depend$node))
        return(TRUE)
      }
      # the input is **not** a top layer hyper-parameter
      if (is.null(node.depend$depend$id) && is.null(node.depend$fun)) stop("parseFlat: input need at least depend$id or func")
      # the input is direct child of the host
      if (self$host$id == node.depend$depend$id) {
        self$host$addCondChild(ParamHandle$new(node = node.depend$node, depend = node.depend$depend))
        return(TRUE)
      }
      # the input is **not** a direct child of the host
      if (self$traverseMand(node.depend)) return(TRUE)  # child will be added inside the recursion
      if (self$traverseCond(node.depend)) return(TRUE)  # child will be added inside the recursion
      return(FALSE)  # failed to insert the input
    },

    # transform the tree structure to a flat list and return the list
    toFlat = function(res = list()) {
      res[[self$host$node$id]] = self$host$node
      if (length(self$host$mand.children) > 0) {
      for (name in names(self$host$mand.children)) {
        handle = self$host$mand.children[[name]]
        res[[handle$node$id]] = handle$node
        res = handle$visitor$toFlat(res)
      }
      } # if
      if (length(self$host$cond.children) > 0) {
      for (name in names(self$host$cond.children)) {
        handle = self$host$cond.children[[name]]
        res[[handle$node$id]] = handle$node
        res = handle$visitor$toFlat(res)
      }
      } # if
      return(res)
    },

    # apply func to all node in the tree, without dependency
    treeApply = function(func) {
      if (self$host$nochild) return(func(self$host$node))  # apply func to the leave node
      if (length(self$host$mand.children) > 0) {
        for (name in names(self$host$mand.children)) {
          handle = self$host$mand.children[[name]]
          return(handle$visitor$treeApply(func))
        }
      } # if
      if (length(self$host$cond.children) > 0) {
        for (name in names(self$host$cond.children)) {
          handle = self$host$cond.children[[name]]
          return(handle$visitor$treeApply(func))
        }
      } # if
    },

    # check if the flat form of paramset violates the dependency
    # FIXME: unter development
    checkValidFromFlat = function(input = list()) {
      fq = list()  # finished queue
      wq = input   # waiting queue
      hit = TRUE
      findDependNode = function(fq, node) {
        fqns = names(fq)
        for (name in fqns) {
          flag = eval(node$depend$fun, envir = setNames(fq[[name]]$val, name))
          if (flag) return(TRUE)
        }
        return(FALSE)
      }
      while (hit) {
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

