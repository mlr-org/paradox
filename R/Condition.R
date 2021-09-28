# -- class methods

#' @describeIn Condition
#'
#' Used internally. Tests whether a value satisfies a given condition.
#' Vectorizes when `x` is atomic.
#'
#' @param cond (`Condition`)\cr
#'   `Condition` to use
#' @param x (`any`)\cr
#'   Value to test
condition_test = function(cond, x) {
  UseMethod("conditionTest")
}

#' @describeIn Condition
#'
#' Used internally. Returns a string that represents the condition for pretty
#' printing, in the form `"<lhs> <relation> <rhs>"`, e.g. `"x == 3"` or
#' `"param %in% {1, 2, 10}"`.
#'
#' @param cond (`Condition`)\cr
#'   `Condition` to use
#' @param lhs_chr (`character(1)`)\cr
#'   Symbolic representation to use for `<lhs>` in the returned string.
condition_as_string = function(cond, lhs_chr = "x") {
  assert_string(lhs_chr)
  UseMethod("conditionTest")
}

# -- Condition

#' @title Dependency Condition
#'
#' @description
#' Condition object, to specify the condition in a dependency.
#'
#' @param rhs (`any`)\cr
#'   Right-hand-side of the condition.
#' @param condition_format_string (`character(1)`)\cr
#'   Format-string for representing the condition when pretty-printing
#'   in `condition_as_string()`.
#'   Should contain two `%s`, as it is used in an `sprintf()`-call with
#'   two further string values.
#'
#' @section Currently implemented simple conditions:
#' * `CondEqual(rhs)` \cr
#'   Value must be equal to `rhs`.
#' * `CondAnyOf(rhs)` \cr
#'   Value must be any value of `rhs`.
#'
#' @aliases CondEqual CondAnyOf
#' @export
Condition = function(rhs, condition_format_string) {
  assert_string(condition_format_string)
  structure(list(rhs = rhs, condition_format_string = condition_format_string), class = "Condition")
}

condition_as_string.Condition = function(cond, lhs_chr = "x") {
  sprintf(condition_format_string, lhs_chr, str_collapse(cond$rhs))
}

#' @export
format.Condition = function(x, ...) {
  sprintf("<Condition:%s>", class(x)[[1L]])
}

#' @export
print.Condition = function(x, ...) {
  catf("%s: %s", class(x)[[1L]], condition_as_string(x))
}

# -- CondEqual

#' @export
CondEqual = function(rhs) {
  assert_atomic(rhs, any.missing = FALSE, len = 1)
  cond = Condition(rhs, "%s == %s")
  set_class(cond, c("CondEqual", class(cond)))
}

#' @export
condition_test.CondEqual = function(cond, x) {
  !is.na(x) & x == cond$rhs
}

#' @export
CondAnyOf = function(rhw) {
  assert_atomic(rhs, any.missing = FALSE, min.len = 1, unique = TRUE)
  cond = Condition(rhs, "%s %%in%% {%s}")
  set_class(cond, c("CondEqual", class(cond)))
}

#' @export
condition_test.CondAnyOf = function(cond, x) {
  !is.na(x) & x %in% cond$rhs
}
