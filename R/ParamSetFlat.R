
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
    initialize = function(id = "parset", handle = NULL, params, dictionary = NULL, tags = character(), allowed = NULL) {
      # check function that checks the whole param set by simply iterating
      check = function(x) {
        assertSetEqual(names(x), self$ids)
        res = checkList(x, names = "named")
        if (!is.null(allowed)) {
          allowed.varpar = substitute(allowed)
          if (!isTRUE(eval(x, envir = x))) {
            sprintf("Value %s is not allowed by %s.", as.character(x), deparse(x))
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
      super$initialize(id, type = "list", check = check, params = params, dictionary = dictionary, tags = tags, allowed = allowed)
    },

    # public methods
    sampleUnrestricted = function(n = 1L) {
      xs = lapply(self$params, function(param) param$sample(n = n))
      names(xs) = NULL
      as.data.table(xs)
    },

    sample = function(n = 1L) {
      assertInt(n, lower = 1)
      if (!is.null(self$allowed)) {
        stop("why am i here?")
        oversampleForbidden(n = n, param = self)
      } else {
        self$sampleUnrestricted(n = n)
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
      assertList(x, names = 'strict')
      assertSetEqual(names(x), self$ids)
      xs = lapply(self$ids, function(id) self$params[[id]]$transform(x = x[id]))
      names(xs) = NULL
      as.data.table(xs)
    }
  ),
  active = list(
    ids = function() {
      names(self$params)
    },
    types = function() {
      BBmisc::vcapply(self$params, function(param) param$type)
    },
    lower = function() {
      BBmisc::vnapply(self$params, function(param) param$lower %??% NA_real_)
    },
    upper = function() {
      BBmisc::vnapply(self$params, function(param) param$upper %??% NA_real_)
    },
    range = function() {
      data.table(id = self$ids, upper = self$upper, lower = self$lower)
    },
    is.finite = function() {
      all(BBmisc::vlapply(self$params, function(param) param$is.finite))
    },
    length = function() {
      length(self$params)
    }
  )
)
