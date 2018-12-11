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

    # constructor
    initialize = function(id, storage_type, lower, upper, values, special_vals, checker, default, tags) {
      # FIXME: really finish all assertions and document them
      assert_string(id)
      assert_names(id, type = "strict")
      assert_choice(storage_type, c("double", "integer", "character", "logical", "list"))
      # FIXME: for some args we might push the assert back up to toplevel classes. eg lower cannot be NA for numerical params
      assert_number(lower, na.ok = TRUE)
      assert_number(upper, na.ok = TRUE)
      assert_character(values, any.missing = FALSE, unique = TRUE, null.ok = TRUE)
      assert_list(special_vals, null.ok = TRUE)
      assert_character(tags, any.missing = TRUE, unique = TRUE, null.ok = TRUE)

      self$data = data.table(
        id = id,                                                   # string
        pclass = class(self)[[1]],                                 # string
        storage_type = storage_type,                               # string
        lower = lower,                                             # double, Inf, or NA
        upper = upper,                                             # double, Inf, or NA
        values = list(values),                                     # charvec or NULL
        special_vals = list(special_vals),                         # list or NULL
        # FIXME: what if deafult is NULL?
        default = list(default),                                   # any or NULL
        tags = list(tags)                                          # charvec
      )
      private$.checker = checker
    },

    check = function(x) {
      #FIXME: shuld use purrr::has_elemehnt, opened issue in mlr3misc
      if(!is.null(self$special_vals) && any(map_lgl(self$special_vals, identical, x)))
        return(TRUE)
      private$.checker(x)
    },

    assert = function(x) makeAssertFunction(self$check)(x),

    test = function(x) makeTestFunction(self$check)(x),

    # repeat this param n-times, return a ParamSet (named with <id>_rep)
    rep = function(n) {
      assert_count(n)
      pid = self$id
      # get dt, copy it n times, change id and tags, then construct param from dt
      dt = self$data
      join_id = paste0(pid, "_rep")
      pars = lapply(seq_len(n), function(i) {
        dt2 = copy(dt)
        dt2$id = paste0(join_id, "_", i)
        dt2$tags[[1L]] = c(dt2$tags[[1L]], join_id)
        new_param_from_dt(dt2)
      })
      ParamSet$new(pars, id = join_id)
    },

    deep_clone = function(name, value) {
      # deep copy the "data" dt member
      if (name == "data") copy(value) else value
    },

    print = function(...) {
      catf("%s %s %s", self$id, private$get_type_string(), private$get_range_string())
      # if (!is.null(self$special_vals)) {
        # catf("+{special_vals}") #FIXME: Better Printer for special_vals!
      # }
      # if (!is.null(self$default)) {
      #   catf(" (Default: %s)", as.character(self$default))
      # }
      if (!is.null(self$tags)) {
        catf(" (Tags: %s)", paste(self$tags, collapse = ", "))
      }
    },

    # takes a numeric vector from [0, 1] and maps it to a vector of <storage_type> of feasible values
    # so that the values are regular distributed
    map_unitint_to_values = function(x) stop("abstract")
  ),

  active = list(
    id = function() self$data$id,
    storage_type = function() self$data$storage_type,
    lower = function() self$data$lower,
    upper = function() self$data$upper,
    values = function() self$data$values[[1L]],
    nlevels = function() {
      v = self$values
      if (is.null(v)) NA_integer_ else length(v)
    },
    special_vals = function() self$data$special_vals[[1]]
  ),

  private = list(
    .checker = NULL,

    # return a short string, displaying the range of the param, called in super$print
    get_range_string = function() stop("abstract"),

    # return 1char, displaying type param, called in super$print
    get_type_string = function() stop("abstract")
  )
)


# private factory methods, creates a param from a single dt row
new_param_from_dt = function(dt) {
  # get pclass constructor from namespace then call it on all (relevent) entries from the dt row
  p = as.list(dt)
  cl = getFromNamespace(p$pclass, "paradox")
  # remove pclass and storage type, as the are not passed to contructor
  p$pclass = NULL; p$storage_type = NULL
  # FIXME: this is not that perfect code:
  # - we untangle all list-cols
  # - we remove all NULL or NA entries, as they cannot be passed to the constructor,
  # NB: we might also handle this thru a reflection table, where we "know" which elements the Param-constructors take
  p = map_if(p, is.list, function(x) x[[1L]])
  p = Filter(function(x) !is.null(x) && !is.na(x), p)
  do.call(cl$new, p)
}






