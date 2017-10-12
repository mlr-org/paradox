#' @title OptPath Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent the OptPath.
#' 
#' @field par.set [\code{ParamSet}] The [\code{ParamSet}] from which the \code{x} values will be added to this \code{OptPath}.
#' @field y.names [\code{character()}] The names for the y values. Default is \dQuote{y}.
#' @field minimize [\code{logical()}] A logical vector indicating which y components are to be minimized. Per default all are \code{TRUE}.
#' @field check.feasible [\code{logical(1)}] Should new x values be checked for feasibility according to the \code{ParamSet}.
#' @field data [\code{data.table}] This field contains all values logged into the opt.path.
#' 
#' @field add() Add a new line to the \code{OptPath}.
#' 
#' @field x.names [\code{character()}]
#' @field length [\code{integer(1)}] The number of entries in this \code{OptPath}.
#' @field x [\code{data.table}] A subset of \code{data} only containing the x values.
#' @field y [\code{data.table}] A subset of \code{data} only containing the y values.
#' @field dim [\code{integer(1)}] The dimensionality of the optimization problem.
#'
#' @family OptPath
#' @export
OptPath = R6Class(
  "OptPath",
  public = list(
   
    # member variables
    par.set = NULL,
    y.names = NULL,
    minimize = NULL,
    check.feasible = NULL,
    data = NULL,
    
    # constructor
    initialize = function(par.set, y.names = "y", minimize = TRUE, check.feasible = TRUE) {
      self$data = data.table(
        dob = integer(0L),
        message = character(0L),
        error = character(0L),
        exec.time = double(0L),
        timestamp = Sys.time()[FALSE],
        extra = list(),
        transformed.x = list()
      )
      Map(function(id, storage.type) {
        set(self$data, j = id, value = get(storage.type, mode = "function")())
        },
        id = par.set$ids,
        storage.type = par.set$storage.types
      )
      for (y.name in y.names) {
        set(self$data, j = y.name, value = numeric(0L))
      }
      if (is.null(names(minimize))) {
        names(minimize) = y.names
      }
      self$par.set = assertClass(par.set, "ParamSet")
      self$y.names = y.names
      self$minimize = minimize
      self$check.feasible = check.feasible
    },

    # public methods
    add = function(x, y, dob = NULL, message = NA_character_, error = NA_character_, exec.time = NA_real_, timestamp = Sys.time(), extra = NULL, transformed.x = NULL) {

      # convinience: handle y
      if (!testList(y)) {
        y = as.list(y)
      }
      if (!testNamed(y)) {
        names(y) = self$y.names
      }

      # convinience: handle x
      if (!testList(x)) {
        x = as.list(x)
      }

      # handle transformed.x
      if (!is.null(self$par.set$trafo) && is.null(transformed.x)) {
        transformed.x = self$par.set$transform(x)
      }

      assertList(x, names = "strict")
      assertSetEqual(names(x), self$x.names)
      assertList(y, len = self$dim)
      assertSetEqual(names(y), self$y.names)

      if (self$check.feasible) {
        self$par.set$assert(x)
      }
      self$data = rbindlist(
        list(self$data, c(list(dob = dob %??% (nrow(self$data) + 1), message = message, error = error, exec.time = exec.time, timestamp = timestamp, extra = list(extra), transformed.x = list(transformed.x)), x, y))
      )
      invisible(self)
    }
  ),

  active = list(
    x.names = function() self$par.set$ids,
    length = function() nrow(self$data),
    x = function() self$data[, self$x.names, with = FALSE],
    y = function() self$data[, self$y.names, with = FALSE],
    dim = function() length(self$y.names)
  )
)


#' @title Convert optimization path to data.frame.
#' @description
#'   Convert optimization path to data.frame.
#'
#' @param x [\code{\link{OptPath}}]\cr
#'   Optimization path.
#' @param row.names [\code{character}]\cr
#'   Row names for result.
#'   Default is none.
#' @param optional [any]\cr
#'   Currently ignored.
#' @param include.extras [\code{logical(1)}]\cr
#'   Include all extra columns?
#'   Default is \code{TRUE}.
#' @param ... [any] \cr
#'   passed to \code{as.data.frame}.
#' @return [\code{data.frame}].
#' @export
as.data.frame.OptPath = function(x, row.names = NULL, optional = FALSE, include.extras = TRUE, ...) {
  dt = data.table::copy(x$data)

  if (include.extras) {
    extra = rbindlist(dt$extra, fill = TRUE)
    dt[, "extra" := NULL]
    dt = cbind(dt, extra)
  }  
  as.data.frame(dt, ...)
}

#' @export
'[.OptPath' = function(x, ...) {
  z = x$clone()
  z$data = '['(z$data, ...)
  z
}

'[[.OptPath' = function(x, ...) {
  '[['(x$data, ...)
}
