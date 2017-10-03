#' @importFrom R6 R6Class
#' @import data.table
OptPath = R6Class(
  "OptPath",
  public = list(
    par.set = NULL,
    y.names = NULL,
    minimize = NULL,
    check.feasible = NULL,
    data = NULL,
    initialize = function(par.set, y.names = "y", minimize = TRUE, check.feasible = TRUE) {
      self$data = data.table(
        dob = integer(0L),
        message = character(0L),
        error = character(0L),
        exec.time = double(0L),
        timestamp = NULL, #FIXME: Initialize empty POSIXct?
        extra = list())
      Map(function(id, type) {
        set(self$data, j = id, value = get(type, mode = "function")())
        },
        id = par.set$getIds(),
        type = par.set$getTypes()
      )
      for (y.name in y.names) {
        set(self$data, j = y.name, value = numeric(0L))
      }
      if (lengths(names(minimize)) == 0) {
        names(minimize) = y.names
      }
      self$par.set = par.set
      self$y.names = y.names
      self$minimize = minimize
      self$check.feasible = check.feasible
    },

    add = function(x, y, dob = NULL, message = NA_character_, error = NA_character_, exec.time = NA_real_, timestamp = Sys.time(), extra = NULL) {
      if (!is.list(y)) {
        y = setNames(as.list(y), self$y.names)
      }
      assert_list(x, names = "strict")
      assert_list(y, names = "strict")
      if (self$check.feasible) {
        par.set$assert(x)
      }
      self$data = rbindlist(
        list(self$data, c(list(dob = dob %??% (nrow(self$data) + 1), eol = eol, msg = msg, exec.time = exec.time, extra = list(extra)), x, y))
      )
      invisible(self)
    }
  ),

  active = list(
    x.names = function() self$par.set$ids,
    length = function() nrow(self$data),
    x = function() self$data[, self$x.names, with = FALSE],
    y = function() self$data[, solf$y.names, with = FALSE]
  )
)


#' @export
as.data.frame.OptPathNg = function(x, include.extras = TRUE, ...) {
  dt = data.table::copy(x$data)

  if (include.extras) {
    extra = rbindlist(dt$extra, fill = TRUE)
    dt[, "extra" := NULL]
    dt = cbind(dt, extra)
  }  
  as.data.frame(dt, ...)
}


'[.OptPath' = function(x, ...) {
  z = x$clone()
  z$data = '['(z$data, ...)
  z
}

'[[.OptPath' = function(x, ...) {
  '[['(x$data, ...)
}
