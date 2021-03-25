#' @title Extra data type for "no default value"
#'
#' @description
#' Special new data type for no-default.
#' Not often needed by the end-user, mainly internal.
#'
#' * `NO_DEF`: Singleton object for type, used in [Param].
#' * `is_nodefault()`: Is an object the 'no default' object?
#'
#' @name NO_DEF
#' @aliases NoDefault is_nodefault
NULL

#' @export
NO_DEF = structure(list(), class = "NoDefault") # nolint
is_nodefault = function(x) identical(x, NO_DEF)
