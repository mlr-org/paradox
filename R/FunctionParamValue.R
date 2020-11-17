

#' @export
FunctionParamValue = function(.fn, ...) {
  assert_function(.fn, nargs = 1)
  set_class(crate(.fn, ...), "FunctionParamValue")
}

#' @export
print.FunctionParamValue = function(x, ...) {
  y = x
  cat("FunctionParamValue:\n")
  environment(y) = .GlobalEnv
  print(unclass(y), ...)
  cat("Using following environment:\n")
  print(as.list(environment(x)))
}
