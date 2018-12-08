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

    # member variables
    id = NULL, # string to uniquely identify this param
    storage_type = NULL, # of what R data storage_type can values of this parameter be stored?
    check = NULL, # a checkmate check function to validate if a value is valid for this Param
    assert = NULL, # assert_ion generated from the above check
    test = NULL, # test generated from the above check
    tags = NULL, # additional properties like "on_train", "on_test" or "tunable" for mlr
    default = NULL,
    special_vals = NULL, # special values as list, can not be changed after initialization

    # constructor
    initialize = function(id, storage_type, check, special_vals, default, tags) {

      if (!is.null(special_vals) && is.na(special_vals)) special_vals = list(special_vals)
      assert_list(special_vals, null.ok = TRUE)


      # set member variables
      assert_string(id)
      self$id = assert_names(id, type = "strict")
      self$storage_type = assert_string(storage_type)
      self$check = assert_function(check)
      self$test = makeTestFunction(check)
      self$assert = makeAssertionFunction(check)
      self$tags = assert_character(tags, null.ok = TRUE)
      self$default = self$assert(default, null.ok = TRUE)
      self$special_vals = special_vals
    },

    # public methods
    # Overwriting ParamNode Methods
    denorm = function(x) as_dt_cols(self$denorm_vector(x[[self$id]]), self$id),

    # ParameterMethods
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

  private = list(
    # return a short string, displaying the range of the param, called in super$print
    get_range_string = function() stop("abstract"),
    # return 1char, displaying type param, called in super$print
    get_type_string = function() stop("abstract")
  )
)
