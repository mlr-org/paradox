#' @title ParamSetTree
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of layer-wise hierachical conditional parameters in the form of a tree.
#'
#' @section Methods:
#'
#' \describe{
#'   \item{setChild(child.set)}{[\code{function}] \cr
#'     Set child.set(Another [\code{ParamSetTree}]) to be  the child of the current tree}
#'   \item{sample(n)}{[\code{function}] \cr
#'     Generate n samples of parameter set in a data table}
#' }
#' @return [\code{\link{ParamSetTree}}].
#' @family ParamSet
#' @export

ParamSetTree = R6Class("ParamSetTree",
  inherit = ParamSet,
  public = list(
    # member variables
    ns.id = NULL,  # namespace id
    rt.hinge = NULL,
    parent.set = NULL,
    child.set = NULL,
    context = NULL,

    initialize = function(ns.id,  ..., trafo = NULL, dictionary = NULL, tags = NULL, context = NULL) {
      self$ns.id = assertNames(ns.id)
      self$rt.hinge = ParamTreeFac(ns.id, ...)
      self$context = context
      # check function that checks the whole param set by simply iterating
      check = function(x, na.ok = FALSE, null.ok = FALSE) {
        if (is.data.table(x)) x = as.list(x)
        res = checkList(x, names = "named")
        x = lapply(x, function(x) ifelse(is.factor(x), as.character(x), x))
        x[is.na(x)] = NULL
        flag = self$rt.hinge$visitor$checkValidFromFlat(x)
        if (!isTRUE(flag)) {
            return(sprintf("Value Violation Found!"))
        }
        for (par.name in names(x)) {
          res = self$params[[par.name]]$check(x[[par.name]], na.ok = na.ok, null.ok = null.ok)
          if (!isTRUE(res)) return(res)
        }
        return(res)
      }
      params = self$rt.hinge$getList0()
      super$initialize(ns.id, storage.type = "list", check = check, params = params, dictionary = dictionary, tags = tags, restriction = NULL, trafo = trafo)
    },

    transform = function(x) {
      x = ensureDataTable(x)
      assertSetEqual(names(x), self$ids)
      if (is.null(self$trafo))
        return(x)
      xs = self$trafo(x = x, dict = self$dictionary, tags = self$member.tags)
      xs = ensureDataTable(xs)
      return(xs)
    },

    # public methods
    # after a tree factory is called, directly set the root
    setChild = function(child.set) {
      self$child.set = child.set
      child.set$parent.set = self
    },

    sample = function(n = 1) {
      # repeat the same process for n times
      res.list = lapply(1:n, function(i) {
        res = self$rt.hinge$sample(1L)
        # if there is another tree
        if (!is.null(self$child.set)) {
          temp = self$child.set$sample(1L)
          res = cbind(res, temp)  # combine the subspace(colums) of current tree and the child tree
        }
        return(res)
      })
      rbindlist(res.list, fill = TRUE)
    }
  )
)


# This class is to extend the functionality of ParamSetTree and should not be exported!
ParamSetTreeX = R6Class("ParamSetTreeX",
  inherit = ParamSetTree,
  public = list(
    sampleList = function(annotate = FALSE, sep = "_", recursive = FALSE) {
      if (recursive) self$getRecursiveList()
      else private$getFlatList(annotate = annotate, sep = sep)
    },


    asample = function() {
      self$rt.hinge$asample()
      if (!is.null(self$child.set)) {
        self$child.set$asample()
      }
    },

    toStringVal = function() {
      self$rt.hinge$toStringVal()
      if (!is.null(self$child.set)) {
        self$child.set$toStringVal()
      }
    },

    getRecursiveList = function(res = list()) {
      if (is.null(self$child.set)) {
        temp = self$rt.hinge$getList()
        n = length(res)
        res[[n + 1L]] = temp
        return(res)
      } else {
        res = self$child.set$getRecursiveList(res = res)
        temp = self$rt.hinge$getList()
        n = length(res)
        res[[n + 1L]] = temp
        return(res)
      }
    }
    ),
  private = list(
    # this should not be used by user, but also cannot be private since it need to call getFlatList itself
    getFlatList = function(annotate = TRUE, sep = "_") {
      res = self$rt.hinge$getList()
      if (annotate) names(res) = paste(self$ns.id, names(res), sep = sep)
      if (!is.null(self$child.set)) {
        temp = self$child.set$sampleList()
        res = c(res, temp)
      }
      res
    }
    )
  )

