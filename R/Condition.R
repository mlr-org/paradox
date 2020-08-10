#' @title Dependency Condition
#'
#' @description
#' Condition object, to specify the condition in a dependency.
#'
#' @section Currently implemented simple conditions:
#' * `CondEqual$new(rhs)` \cr
#'   Parent must be equal to `rhs`.
#' * `CondAnyOf$new(rhs)` \cr
#'   Parent must be any value of `rhs`.
#'
#'  @details
#'  Internally we ensure that `rhs` is always a (general) vector, so either an atomic vector
#   of scalar values or a list.
#'
#' @aliases CondEqual CondAnyOf
#' @export
Condition = R6Class("Condition",
  public = list(
    #' @field type (`character(1)`)\cr
    #' Name / type of the condition.
    type = NULL,
    #' @field rhs (`any`)\cr
    #' Right-hand-side of the condition.
    rhs = NULL,
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param type (`character(1)`)\cr
    #'   Name / type of the condition.
    #' @param rhs (`any`)\cr
    #'   Right-hand-side of the condition.
    initialize = function(type, rhs) {
      self$type = assert_string(type)
      # in the code in ParamSet that uses Condition, we assume it can be indexed as a vector
      # so we upcast here, eg. CondEqual will often only consist of single value
      # that is normally an indexable vector, but ParamUty can have non-atomic objects
      if (!is.vector(rhs))
        rhs = list(rhs)
      self$rhs = rhs
    },

    #' @description
    #' Checks if condition is satisfied.
    #' Called on a vector of parent param values.
    #'
    #' @param x ().
    #' @return `logical(1)`.
    test = function(x) stop("abstract"),

    #' @description
    #' Conversion helper for print outputs.
    #' @param lhs_chr (`character(1)`)
    as_string = function(lhs_chr = "x") {
      sprintf("%s %s %s", lhs_chr, self$type, str_collapse(self$rhs))
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s:%s>", class(self)[1L], self$type)
    },

    #' @description
    #' Printer.
    #'
    #' @param ... (ignored).
    print = function(...) {
      catf("%s: %s", class(self)[1L], self$as_string())
    }
  ),
)

#' @export
CondEqual = R6Class("CondEqual", inherit = Condition,
  public = list(
    initialize = function(rhs) super$initialize("equal", rhs),
    test = function(x) !is.na(x) & identical(x, self$rhs),
    as_string = function(lhs_chr = "x") sprintf("%s = %s", lhs_chr, as.character(self$rhs))
  )
)

#' @export
CondAnyOf = R6Class("CondAnyOf", inherit = Condition,
  public = list(
    initialize = function(rhs) super$initialize("anyof", rhs),
    test = function(x) !is.na(x) & x %in% self$rhs,
    as_string = function(lhs_chr = "x") sprintf("%s \u2208 {%s}", lhs_chr, str_collapse(self$rhs))
  )
)
