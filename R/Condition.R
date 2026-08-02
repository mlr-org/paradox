# -- class methods

#' @describeIn Condition
#'
#' Used internally. Tests whether a value satisfies a given condition.
#' Vectorizes over a plain logical, integer, numeric, or character `x`.
#' A character `x` never satisfies a logical, integer, or numeric right-hand
#' side, and vice versa: such an element is `FALSE` rather than an error. No
#' value is reinterpreted as another type in order to compare it, so unlike
#' Paradox 1 -- which compared through R's `==` -- `condition_test(CondEqual(1),
#' "1")` is `FALSE`. Dependency evaluation uses the same answer.
#' `NULL` produces `logical()`. A names attribute is retained; classed vectors
#' and other attributes, including S4 operands, are not part of the closed
#' Condition boundary. Stable
#' base ALTREP vectors such as compact integer sequences are materialized once
#' on native admission. The `cond` shell and its structural metadata must be
#' ordinary non-ALTREP/non-S4.
#'
#' @param cond (`Condition`)\cr
#'   `Condition` to use
#' @param x (`NULL` or plain logical, integer, numeric, or character vector)\cr
#'   Values to test. May carry only a names attribute.
#' @export
condition_test = function(cond, x) {
  .Call(C_condition_test_builtin, cond, x)
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
#' @export
condition_as_string = function(cond, lhs_chr = "x") {
  if (isS4(lhs_chr)) {
    stop("`lhs_chr` must not be an S4 object.", call. = FALSE)
  }
  assert_string(lhs_chr)
  condition_kind(cond)
  sprintf(.subset2(cond, 2L), lhs_chr, str_collapse(.subset2(cond, 1L)))
}

condition_kind = function(cond) {
  if (isS4(cond)) {
    stop(
      "Unsupported Condition class; supported classes are 'CondEqual' and 'CondAnyOf'.",
      call. = FALSE
    )
  }
  classes = class(cond)
  if (identical(classes, c("CondEqual", "Condition"))) {
    return("equal")
  }
  if (identical(classes, c("CondAnyOf", "Condition"))) {
    return("any_of")
  }
  if (identical(classes, "Condition")) {
    # The plain Condition class remains printable/constructible for
    # compatibility even though it is not testable by the closed engine.
    return("condition")
  }
  stop(
    "Unsupported Condition class; supported classes are 'CondEqual' and 'CondAnyOf'.",
    call. = FALSE
  )
}

# -- Condition

#' @title Dependency Condition
#'
#' @description
#' Condition object, to specify the condition in a dependency.
#'
#' Dependency evaluation is deliberately closed over the built-in
#' `CondEqual` and `CondAnyOf` classes. Third-party `Condition` subclasses and
#' S3 methods for `condition_test()` or `condition_as_string()` are not
#' supported. The base `Condition` class remains available for constructing
#' and printing condition-shaped objects, but it is not itself testable. A
#' Condition shell and its structural class/name metadata must be ordinary
#' non-ALTREP/non-S4 objects.
#'
#' @param rhs (`any`)\cr
#'   Right-hand side of the condition. `Condition()` itself retains this value
#'   only as a condition-shaped object for compatibility and printing. For the
#'   testable built-ins, dependency admission and [`condition_test()`] require
#'   an attribute-free logical, integer, numeric, or character vector without
#'   missing values. `CondEqual()` requires one element; `CondAnyOf()` requires
#'   a non-empty unique vector. Stable ALTREP vectors such as `1:n` are copied
#'   element-by-element once at native admission, and the ordinary snapshot is
#'   then the condition's semantic value for that operation. Atomic right-hand
#'   sides and directly tested operands may be stable ALTREP, but reject S4
#'   explicitly; they are semantic leaves rather than structural shells.
#' @param condition_format_string (`character(1)`)\cr
#'   Format-string for representing the condition when pretty-printing
#'   in [`condition_as_string()`].
#'   Should contain two `%s`, as it is used in an `sprintf()`-call with
#'   two further string values.
#'
#' @section Supported dependency conditions:
#' * `CondEqual(rhs)` \cr
#'   Value must be equal to `rhs`.
#' * `CondAnyOf(rhs)` \cr
#'   Value must be any value of `rhs`.
#'
#' @aliases CondEqual CondAnyOf
#' @export
Condition = function(rhs, condition_format_string) {
  if (isS4(condition_format_string)) {
    stop("`condition_format_string` must not be an S4 object.", call. = FALSE)
  }
  assert_string(condition_format_string)
  structure(list(rhs = rhs, condition_format_string = condition_format_string), class = "Condition")
}

#' @export
format.Condition = function(x, ...) {
  condition_kind(x)
  sprintf("<Condition:%s>", class(x)[[1L]])
}

#' @export
print.Condition = function(x, ...) {
  catf("%s: %s", class(x)[[1L]], condition_as_string(x))
}

# -- CondEqual

# The two built-in constructors stay deliberately more permissive than the
# closed engine: Paradox 1 accepted any atomic right-hand side, and a
# condition-shaped object remains constructible and printable here for that
# compatibility. The engine is the single owner of the testable shape rule, and
# every path that persists or evaluates a Condition -- `$add_dep()`,
# `$deps<-`, Domain requirement admission, ParamSet construction,
# `condition_test()`, and legacy migration -- validates through it, so no
# unusable Condition can reach stored state.
#' @export
CondEqual = function(rhs) {
  if (isS4(rhs)) {
    stop("`rhs` must not be an S4 object.", call. = FALSE)
  }
  assert_atomic(rhs, any.missing = FALSE, len = 1)
  cond = Condition(rhs, "%s == %s")
  set_class(cond, c("CondEqual", class(cond)))
}

#' @export
CondAnyOf = function(rhs) {
  if (isS4(rhs)) {
    stop("`rhs` must not be an S4 object.", call. = FALSE)
  }
  assert_atomic(rhs, any.missing = FALSE, min.len = 1, unique = TRUE)
  cond = Condition(rhs, "%s %%in%% {%s}")
  set_class(cond, c("CondAnyOf", class(cond)))
}

# Stable source-compatibility adapter for the documented `CondEqual$new()` and
# `CondAnyOf$new()` spellings. The closed Condition engine admits only these
# exact built-in classes; this adapter is not a third-party extension hook.
#' @export
`$.Constructor` = function(e1, e2) {
  if (!identical(e2, "new")) {
    stop("only 'new' element can be accessed.")
  } else {
    e1
  }
}

CondEqual = structure(CondEqual, class = c("Constructor", "function"))
CondAnyOf = structure(CondAnyOf, class = c("Constructor", "function"))
