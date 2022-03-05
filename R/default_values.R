#' @title Extract Parameter Default Values
#'
#' @description
#' Extract parameter default values.
#'
#' @param x (`any`)\cr
#'   Object to extract default values from.
#' @param ... (any)\cr
#'   Additional arguments.
#'
#' @return `list()`.
#' @export
default_values = function(x, ...) { # nolint
  UseMethod("default_values")
}

#' @export
#' @rdname default_values
default_values.ParamSet = function(x, ...) {
  x$default
}
