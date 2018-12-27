#' @title Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' Abstract base class \code{\link[R6]{R6Class}} to represent a parameter.
#'
#' @section Member Variables:
#'
#' \describe{
#'   \item{default}{[\code{any}] \cr
#'     default value.}
#'   \item{special_vals}{[\code{any}] \cr
#'     Special values this parameter is allowed to take that are within the defined space.}
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{map_unitint_to_values(x)}{[\code{function}] \cr
#'     Takes a vector with values between \code{[0,1]} and maps them to values of the Parameter.}
#' }
#'
#' @section Active Bindings:
#'   \emph{none}
#'
#' @family Parameter
Parameter = R6Class("Parameter",
  public = list(
    data = NULL,

    initialize = function(id, storage_type, lower, upper, values, special_vals, default, tags) {
      # FIXME: really finish all assertions and document them
      assert_string(id)
      assert_names(id, type = "strict")
      assert_choice(storage_type, c("numeric", "integer", "character", "logical", "list"))
      # FIXME: for some args we might push the assert back up to toplevel classes. eg lower cannot be NA for numerical params
      assert_number(lower, na.ok = TRUE)
      assert_number(upper, na.ok = TRUE)
      assert_character(values, any.missing = FALSE, unique = TRUE, null.ok = TRUE)
      assert_list(special_vals, null.ok = TRUE)
      assert_character(tags, any.missing = TRUE, unique = TRUE, null.ok = TRUE)

      self$data = data.table(
        id = id,                              # string
        lower = lower,                        # double, Inf, or NA
        upper = upper,                        # double, Inf, or NA
        values = list(values),                # charvec or NULL
        special_vals = list(special_vals),    # list or NULL
        # FIXME: what if deafult is NULL?
        default = list(default),              # any or NULL
        tags = list(tags)                     # charvec
      )
    },

    check = function(x) {
      #FIXME: shuld use purrr::has_element, opened issue in mlr3misc
      if (!is.null(self$special_vals) && any(map_lgl(self$special_vals, identical, x)))
        return(TRUE)
      private$.check(x)
    },

    assert = function(x) makeAssertFunction(self$check)(x),

    test = function(x) makeTestFunction(self$check)(x),

    # repeat this param n-times, return a list of Parameter (named with <id>_rep)
    rep = function(n) {
      assert_count(n)
      pid = self$id
      join_id = paste0(pid, "_rep")
      ps = replicate(n, self$clone(), simplify = FALSE)
      for (i in 1:n) {
        p = ps[[i]]
        p$data$id = paste0(join_id, "_", i)
        p$data$tags = c(p$tags, join_id)
      }
      ParamSet$new(ps)
    },

    # FIXME: comment is bullshit, and the dt-copy, too
    deep_clone = function(name, value) {
      # deep copy the "data" dt member
      if (name == "data") copy(value) else value
    },

    print = function(..., hide.cols = c("tags")) {
      # this is bit bullshitty, but works by delegating to the printer of the PS
      d = ParamSet$new(list(self))$as_dt()
      assert_subset(hide.cols, names(d))
      print(d[, setdiff(colnames(d), hide.cols), with = FALSE])
    },

    # takes a numeric vector from [0, 1] and maps it to a vector of <storage_type> of feasible values
    # so that the values are regular distributed
    map_unitint_to_values = function(x) {
      assert_numeric(x, lower = 0, upper = 1)
      private$.map_unitint_to_values(x)
    },

    fix = function(x) {
      self$check(x)
      private$.check(x)
    }
  ),

  active = list(
    id = function() self$data$id,
    pclass = function() class(self)[[1L]],
    storage_type = function() self$data$storage_type,
    lower = function() self$data$lower,
    upper = function() self$data$upper,
    values = function() self$data$values[[1L]],
    nlevels = function() {
      v = self$values
      if (is.null(v)) NA_integer_ else length(v)
    },
    special_vals = function() self$data$special_vals[[1]],
    default = function() self$data$default[[1]],
    is_bounded = function() stop("abstract")
  ),

  private = list(
    .check = function(x) stop("abstract"),
    .map_unitint_to_values = function(x) stop("abstract"),
    .fix = function(x) stop("abstract")
  )
)






