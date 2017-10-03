
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
    initialize = function(id = "parset", handle = NULL, params, dictionary = NULL) {
      # check function that checks the whole param set by simply iterating
      check = function(x) {
        res = checkList(x, names = "named")
        while (res == TRUE) {
          for (param in x) {
            res = param$check(x[[param$id]])
          }
        }
        return(res)
      }

      # make params a named list according to the ids
      names(params) = BBmisc::extractSubList(params, "id")


      super$initialize(id, type = "list", check = check, params = params, dictionary = dictionary)
    },

    # methods
    sample = function(n = 1L) {
      xs = lapply(self$params, function(param) param$sample(n = n))
      as.data.table(xs)
    },

    denorm = function(x) {
      xs = lapply(self$params, function(param) param$denorm(x = x))
      as.data.table(xs)
    }
  ),
  active = list(
    ids = function() {
      names(self$params)
    },
    lower = function() {
      BBmisc::vnapply(self$params, function(param) param$lower %??% NA_real)
    },
    upper = function() {
      BBmisc::vnapply(self$params, function(param) param$upper %??% NA_real)
    },
    range = function() {
      data.table(id = self$ids, upper = self$upper, lower = self$lower)
    },
    is.finite = function() {
      all(BBmisc::vlapply(self$params, function(param) param$is.finite))
    }
  )
)
