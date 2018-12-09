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
#'   \item{denorm_vector(x)}{[\code{function}] \cr
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

    denorm = function(x) as_dt_cols(self$denorm_vector(x[[self$id]]), self$id),

    denorm_vector = function(x) {
      stop("denorm function not implemented!")
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
    }
  ),

  active = list(
    id = function() self$data$id,
    storage_type = function() self$data$storage_type,
    lower = function() self$data$lower,
    upper = function() self$data$upper,
    values = function() self$data$values[[1L]],
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
