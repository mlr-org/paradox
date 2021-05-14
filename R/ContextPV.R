
#' @title ParamSet Value that Depends on Context
#'
#' @description
#' Set a [`ParamSet`]`$value` slot to this. The `.fn` function will
#' be called with the respective function arguments. The function argument
#' names of `.fn` must be a subset of the [`ParamSet`]'s `$context_available` slot.
#'
#' @param .fn `function`\cr
#'   Function to be executed in the context where [`ParamSet`] values
#'   are retrieved.
#' @param ... any\cr
#'   Variable names to make available to function. Functions are run in
#'   the [`.GlobalEnv`][base::.GlobalEnv] scope and only the variables
#'   named in `...` will be available.
#' @examples
#' p = ps(x = p_dbl(), y = p_dbl())
#' p$context_available = c("a", "x")
#'
#' b = 10
#'
#' p$values$x = ContextPV(function(a) a * b, b)
#' p$values$y = ContextPV(function(x) x^2)
#' # ContextPV uses the value of b right at this moment
#' b = 20
#'
#' p$values$x(1)  # 1 * 10
#'
#' p$get_values(context = list(a = 10, x = 20))  # using 'a' from context: 10 * 10
#'
#' @export
ContextPV = function(.fn, ...) {
  assert_function(.fn)
  set_class(crate(.fn, ...), c("ContextPV", "function"))
}

#' @export
print.ContextPV = function(x, ...) {
  y = x
  cat("ContextPV ")
  environment(y) = .GlobalEnv
  print(unclass(y), ...)
  cat("Using following environment:\n")
  print(as.list(environment(x)))
}
