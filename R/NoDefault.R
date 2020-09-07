#' @title Extra data type for "no default value"
#'
#' @description
#' Special new data type for no-default.
#' Not often needed by the end-user, mainly internal.
#'
#' * `NoDefault`: R6 factory.
#' * `NO_DEF`: R6 Singleton object for type, used in [Param].
#' * `is_nodefault()`: Is an object of type 'no default'?
#'
#' @name NO_DEF
#' @aliases NoDefault is_nodefault
NULL

#' @export
NoDefault = R6Class("NoDefault",
  public = list(
    initialize = function() {
    }
  ),
)

#' @export
NO_DEF = NoDefault$new() # nolint
is_nodefault = function(x) test_r6(x, "NoDefault")
