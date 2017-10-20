#' @title OptPath Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent the OptPath.
#' 
#' @section Member Variables:
#' 
#' \describe{
#'   \item{par.set}{[\code{ParamSet}] \cr 
#'     The [\code{ParamSet}] from which the \code{x} values will be added to this \code{OptPath}.}
#'   \item{y.names}{[\code{character()}] \cr 
#'     The names for the y values. Default is \dQuote{y}.}
#'   \item{minimize}{[\code{logical()}] \cr  
#'     A logical vector indicating which y components are to be minimized. Per default all are \code{TRUE}.}
#'   \item{check.feasible}{[\code{logical(1)}] \cr  
#'     Should new x values be checked for feasibility according to the \code{ParamSet}.}
#'   \item{data}{[\code{data.table}] \cr  
#'     This field contains all values logged into the opt.path.}
#' }
#' 
#' @section Methods:
#' 
#' \describe{
#'   \item{add()}{[\code{function}] \cr  
#'     Add a new line to the \code{OptPath}.}
#' }
#' 
#' @section Active Bindings:
#' 
#' \describe{
#'   \item{x.names}{[\code{character()}] \cr
#'     Names of the x-values}
#'   \item{length}{[\code{integer(1)}] \cr
#'     The number of entries in this \code{OptPath}.}
#'   \item{x}{[\code{data.table}] \cr  
#'     A subset of \code{data} only containing the x values.}
#'   \item{y}{[\code{data.table}] \cr  
#'     A subset of \code{data} only containing the y values.}
#'   \item{dim}{[\code{integer(1)}] \cr  
#'     The dimensionality of the optimization problem.}
#' }
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
    
    # constructor
    initialize = function(par.set, y.names = "y", minimize = TRUE, check.feasible = TRUE) {
      private$p.data = data.table(
        dob = integer(0L),
        message = character(0L),
        error = character(0L),
        exec.time = double(0L),
        timestamp = Sys.time()[FALSE],
        extra = list(),
        transformed.x = list()
      )
      Map(function(id, storage.type) {
        set(private$p.data, j = id, value = get(storage.type, mode = "function")())
        },
        id = par.set$ids,
        storage.type = par.set$storage.types
      )
      for (y.name in y.names) {
        set(private$p.data, j = y.name, value = numeric(0L))
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

      # convenience: handle y
      if (!testList(y)) {
        y = as.list(y)
      }
      if (!testNamed(y)) {
        names(y) = self$y.names
      }

      # convenience: handle x
      if (!testList(x)) {
        x = as.list(x)
      }

      # handle transformed.x
      if (!is.null(self$par.set$trafo) && is.null(transformed.x)) {
        transformed.x = self$par.set$transform(x)
      }

      assertList(x, names = "strict")
      assertSetEqual(names(x), self$x.names)
      x = x[self$x.names]
      assertList(y, len = self$dim)
      assertSetEqual(names(y), self$y.names)
      y = y[self$y.names]

      if (self$check.feasible) {
        self$par.set$assert(x)
      }

      # add the data to the opt path

      if (private$cache.pos == length(private$cache)) private$flush()
      private$cache.pos = private$cache.pos + 1L
      private$cache[[private$cache.pos]] = c(list(dob = dob %??% self$length, message = message, error = error, exec.time = exec.time, timestamp = timestamp, extra = list(extra), transformed.x = list(transformed.x)), x, y)
      invisible(self)
    }
  ),

  private = list(

    # private member variables

    cache.pos = 0L, # the index of the last cached opt path row
    cache = vector("list", 512L), # list to store the cache
    p.data = NULL, # the real data.table

    # private methods

    flush = function() {
      if (private$cache.pos > 0L) {
        cached = rbindlist(head(private$cache, private$cache.pos), fill = TRUE)
        private$p.data = rbindlist(list(private$p.data, cached), fill = TRUE)
        setorderv(private$p.data, "dob")
        private$cache.pos = 0L
      }
    }
  ),

  active = list(
    data = function() {private$flush(); private$p.data},
    x.names = function() self$par.set$ids,
    length = function() nrow(private$p.data) + private$cache.pos,
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
    if (nrow(extra) > 0 && ncol(extra) > 0) {
      dt[, "extra" := NULL]
      dt = cbind(dt, extra)
    }
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
