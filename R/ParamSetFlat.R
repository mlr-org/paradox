#' @title ParamSetFlat
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of parameters in a flat form.
#'
#' @return [\code{\link{ParamSet}}].
#' @family ParamHelpers
#' @export
ParamSetFlat = R6Class(
  "ParamSetFlat",
  inherit = ParamSet,
  public = list(
   
    # member variables
    
    # constructor
    initialize = function(id = "parset", handle = NULL, params = list(), dictionary = NULL, tags = NULL, restriction = NULL, trafo = NULL) {
      # check function that checks the whole param set by simply iterating
      check = function(x) {
        assertSetEqual(names(x), self$ids)
        res = checkList(x, names = "named")
        if (!is.null(self$restriction)) {
          x.n.dictionary = c(as.list(self$dictionary), x)
          if (!isTRUE(eval(self$restriction, envir = x.n.dictionary))) {
            return(sprintf("Value %s not allowed by restriction: %s", BBmisc::convertToShortString(x), deparse(restriction)))
          }
        }
        for (par.name in names(x)) {
          res = self$params[[par.name]]$check(x[[par.name]])
          if(!isTRUE(res)) return(res)
        }
        return(res)
      }

      # make params a named list according to the ids
      names(params) = BBmisc::extractSubList(params, "id")

      # A Flat ParamSet can only contain ParamSimple Objects?
      assertList(params, types = "ParamSimple") # FIXME: Maybe too restricitve?
      
      # construct super class
      super$initialize(id, storage.type = "list", check = check, params = params, dictionary = dictionary, tags = tags, restriction = restriction, trafo = trafo)
    },

    # public methods
    sample = function(n = 1L) {
      sample.generator = function(n) {
        xs = lapply(self$params, function(param) param$sample(n = n))
        names(xs) = NULL
        as.data.table(xs)    
      }
      if (!is.null(self$restriction)) {
        sample.validator = function(x) {
          fn = function(...) {self$test(list(...))}
          unlist(.mapply(fn, x, list()))
        }
        oversampleForbidden2(n = n, param = param, oversample.rate = 2, max.tries = 10, sample.generator = sample.generator, sample.validator = sample.validator, sample.combine = rbind)
      } else {
        sample.generator(n)
      }
    },

    denorm = function(x) {
      assertList(x, names = 'strict')
      assertSetEqual(names(x), self$ids)
      xs = lapply(self$ids, function(id) self$params[[id]]$denorm(x = x[id]))
      names(xs) = NULL
      as.data.table(xs)
    },

    transform = function(x) {
      if (is.data.table(x)) {
        x = as.list(x)
      }
      assertList(x, names = 'strict')
      assertSetEqual(names(x), self$ids)
      if (is.null(self$trafo)) 
        return(x)
      # We require trafos to be vectorized! That's why we dont need the following
      #.mapply(function(x) {
      #  eval(self$trafo, envir = c(x, as.list(self$dictionary)))
      #}, x, list())
      xs = self$trafo(x = c(x, dict = self$dictionary))
      assertList(xs, names = "strict")
      as.data.table(xs)
    }
  ),

  #FIXME: add unit tests for empty flat set

  active = list(
    ids = function() names(self$params),
    storage.types = function() BBmisc::vcapply(self$params, function(param) param$storage.type),
    lower = function() BBmisc::vnapply(self$params, function(param) param$lower %??% NA_real_),
    upper = function() BBmisc::vnapply(self$params, function(param) param$upper %??% NA_real_),
    param.classes = function() BBmisc::vcapply(self$params, function(param) class(param)[1]),
    range = function() data.table(id = self$ids, upper = self$upper, lower = self$lower),
    is.finite = function() all(BBmisc::vlapply(self$params, function(param) param$is.finite)),
    length = function() length(self$params),
    is.empty = function() self$length == 0L
  )
)
