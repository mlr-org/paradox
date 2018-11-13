#' @title OptPath Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent the OptPath_
#'
#' @section Member Variables:
#'
#' \describe{
#'   \item{param_set}{[\code{ParamSet}] \cr
#'     The [\code{ParamSet}] from which the \code{x} values will be added to this \code{OptPath}.}
#'   \item{y_names}{[\code{character()}] \cr
#'     The names for the y values. Default is \dQuote{y}.}
#'   \item{minimize}{[\code{logical()}] \cr
#'     A logical vector indicating which y components are to be minimized. Per default all are \code{TRUE}.}
#'   \item{check_feasible}{[\code{logical(1)}] \cr
#'     Should new x values be checked for feasibility according to the \code{ParamSet}.}
#'   \item{data}{[\code{data.table}] \cr
#'     This field contains all values logged into the opt_path_}
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
#'   \item{x_names}{[\code{character()}] \cr
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
    param_set = NULL,
    y_names = NULL,
    minimize = NULL,
    check_feasible = NULL,

    # constructor
    initialize = function(param_set, y_names = "y", minimize = TRUE, check_feasible = TRUE) {
      private$.data = data.table(
        dob = integer(0L),
        message = character(0L),
        error = character(0L),
        exec_time = double(0L),
        timestamp = Sys.time()[FALSE],
        extra = list(),
        transformed_x = list()
      )
      Map(function(id, storage_type) {
        set(private$.data, j = id, value = get(storage_type, mode = "function")())
        },
        id = param_set$ids,
        storage_type = param_set$storage_types
      )
      for (y_name in y_names) {
        set(private$.data, j = y_name, value = numeric(0L))
      }
      if (is.null(names(minimize))) {
        names(minimize) = y_names
      }
      self$param_set = assert_r6(param_set, "ParamSet")
      self$y_names = y_names
      self$minimize = minimize
      self$check_feasible = check_feasible
    },

    # public methods
    add = function(x, y, dob = NULL, message = NA_character_, error = NA_character_, exec_time = NA_real_, timestamp = Sys.time(), extra = NULL, transformed_x = NULL) {

      # convenience: handle y
      if (!test_list(y)) {
        y = as.list(y)
      }
      if (!test_named(y)) {
        names(y) = self$y_names
      }

      # convenience: handle x
      if (!test_list(x)) {
        x = as.list(x)
      }

      # handle transformed_x
      if (!is.null(self$param_set$trafo) && is.null(transformed_x)) {
        transformed_x = self$param_set$transform(x)
      }

      assert_list(x, names = "strict")
      assert_set_equal(names(x), self$x_names)
      x = x[self$x_names]
      assert_list(y, len = self$dim)
      assert_set_equal(names(y), self$y_names)
      y = y[self$y_names]

      if (self$check_feasible) {
        self$param_set$assert(x)
      }

      # add the data to the opt path

      if (private$cache_pos == length(private$cache)) private$flush()
      private$cache_pos = private$cache_pos + 1L
      private$cache[[private$cache_pos]] = c(list(dob = dob %??% self$length, message = message, error = error, exec_time = exec_time, timestamp = timestamp, extra = list(extra), transformed_x = list(transformed_x)), x, y)
      invisible(self)
    }
  ),

  private = list(

    # private member variables

    cache_pos = 0L, # the index of the last cached opt path row
    cache = vector("list", 512L), # list to store the cache
    .data = NULL, # the real data.table

    # private methods

    flush = function() {
      if (private$cache_pos > 0L) {
        cached = rbindlist(head(private$cache, private$cache_pos), fill = TRUE)
        private$.data = rbindlist(list(private$.data, cached), fill = TRUE)
        setorderv(private$.data, "dob")
        private$cache_pos = 0L
      }
    }
  ),

  active = list(
    data = function(x) {
      if (missing(x)) {
        private$flush()
        private$.data
      } else {
        private$flush()
        private$.data = x
      }
    },
    x_names = function() self$param_set$ids,
    length = function() nrow(private$.data) + private$cache_pos,
    x = function() self$data[, self$x_names, with = FALSE],
    y = function() self$data[, self$y_names, with = FALSE],
    dim = function() length(self$y_names)
  )
)


#' @title Convert optimization path to data.frame.
#' @description
#'   Convert optimization path to data.frame.
#'
#' @param x [\code{\link{OptPath}}]\cr
#'   Optimization path_
#' @param row.names [\code{character}]\cr
#'   Row names for result.
#'   Default is none.
#' @param optional [any]\cr
#'   Currently ignored.
#' @param include_extras [\code{logical(1)}]\cr
#'   Include all extra columns?
#'   Default is \code{TRUE}.
#' @param ... [any] \cr
#'   passed to \code{as.data.frame}.
#' @return [\code{data.frame}].
#' @export
as.data.frame.OptPath = function(x, row.names = NULL, optional = FALSE, include_extras = TRUE, ...) {
  dt = data.table::copy(x$data)

  if (include_extras) {
    extra = rbindlist(dt$extra, fill = TRUE)
    if (nrow(extra) > 0 && ncol(extra) > 0) {
      dt[, "extra" := NULL]
      dt = cbind(dt, extra)
    }
  }

  as.data.frame(dt, row.names = row.names, optional = optional, ...)
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
