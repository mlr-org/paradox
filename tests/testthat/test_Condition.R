context("Condition")

test_that("Condition", {
  cond = CondEqual("a")
  y = condition_test(cond, c("a", "b", "c", NA_character_))
  expect_equal(y, c(TRUE, FALSE, FALSE, FALSE))
  expect_output(print(cond), fixed = "CondEqual")

  expect_error(CondEqual(c("a","b")), "Assertion on 'rhs' failed")
  expect_error(CondEqual(NA), "Assertion on 'rhs' failed")

  cond = CondAnyOf(c("a", "b"))
  y = condition_test(cond, c("a", "b", "c", NA_character_))
  expect_equal(y, c(TRUE, TRUE, FALSE, FALSE))
  expect_output(print(cond), fixed = "CondAnyOf")

  expect_error(CondAnyOf(list("a","b")), "Assertion on 'rhs' failed")
  expect_error(CondAnyOf(c("a", "b", NA_character_)), "Assertion on 'rhs' failed")
  expect_error(CondAnyOf(character()), "Assertion on 'rhs' failed")
})

test_that("Condition keeps its public object and constructor shape", {
  equal = CondEqual(3L)
  any_of = CondAnyOf(c("a", "b"))

  expect_identical(unclass(equal), list(rhs = 3L, condition_format_string = "%s == %s"))
  expect_identical(class(equal), c("CondEqual", "Condition"))
  expect_identical(unclass(any_of), list(rhs = c("a", "b"), condition_format_string = "%s %%in%% {%s}"))
  expect_identical(class(any_of), c("CondAnyOf", "Condition"))
  expect_identical(CondEqual$new(3L), equal)
  expect_identical(CondAnyOf$new(c("a", "b")), any_of)

  expect_identical(condition_as_string(equal), "x == 3")
  expect_identical(condition_as_string(any_of, "choice"), "choice %in% {a, b}")
  expect_identical(condition_as_string(Condition(3L, "%s == %s")), "x == 3")
})

test_that("Condition evaluation is closed over exact built-in classes", {
  unsupported = "Unsupported Condition class; supported classes are 'CondEqual' and 'CondAnyOf'."
  external = structure(
    list(rhs = 1L, condition_format_string = "%s accepts %s"),
    class = c("ExternalCondition", "Condition")
  )
  condition_test.ExternalCondition = function(cond, x) rep(TRUE, length(x))
  condition_as_string.ExternalCondition = function(cond, lhs_chr = "x") "extended"

  expect_error(condition_test(external, 1L), unsupported, fixed = TRUE)
  expect_error(condition_as_string(external), unsupported, fixed = TRUE)
  expect_error(condition_test(Condition(1L, "%s == %s"), 1L), unsupported, fixed = TRUE)
  expect_error(condition_test(structure(1L, class = "CondEqual"), 1L), unsupported, fixed = TRUE)
})

test_that("Condition evaluation has no package S3 method seam", {
  namespace = asNamespace("paradox")

  expect_false(exists("condition_test.CondEqual", namespace, inherits = FALSE))
  expect_false(exists("condition_test.CondAnyOf", namespace, inherits = FALSE))
  expect_false(exists("condition_as_string.Condition", namespace, inherits = FALSE))
  expect_null(getS3method(
    "condition_test",
    "CondEqual",
    optional = TRUE,
    envir = namespace
  ))
  expect_null(getS3method(
    "condition_test",
    "CondAnyOf",
    optional = TRUE,
    envir = namespace
  ))
})

test_that("Condition access does not dispatch through dollar methods", {
  `$.CondEqual` = function(x, name) stop("unexpected dollar dispatch")
  cond = CondEqual(1L)

  expect_identical(condition_test(cond, c(1L, 2L)), c(TRUE, FALSE))
  expect_identical(condition_as_string(cond), "x == 1")
})

test_that("Condition admission accepts attribute order but rejects extras", {
  condition = CondEqual(1L)
  reordered = condition
  attributes(reordered) = attributes(condition)[c("class", "names")]

  expect_identical(condition_test(reordered, 1:2), c(TRUE, FALSE))

  malformed = condition
  attr(malformed, "unexpected") = TRUE
  expect_error(
    condition_test(malformed, 1L),
    "Malformed built-in Condition object",
    fixed = TRUE
  )
})

test_that("Condition vector comparison is one registered native operation", {
  symbol = get("C_condition_test_builtin", envir = asNamespace("paradox"))
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)

  values = 1:4
  names(values) = letters[1:4]
  expect_identical(
    condition_test(CondAnyOf(1:3), values),
    setNames(c(TRUE, TRUE, TRUE, FALSE), letters[1:4])
  )
  expect_identical(
    condition_test(CondAnyOf(1:3), 1:4),
    c(TRUE, TRUE, TRUE, FALSE)
  )
  expect_identical(
    condition_test(CondEqual("x"), c("x", NA_character_, "y")),
    c(TRUE, FALSE, FALSE)
  )
  expect_identical(condition_test(CondEqual(1L), NULL), logical())
  expect_error(
    condition_test(CondEqual(1L), factor("1")),
    "plain atomic vector"
  )
})

test_that("Condition comparison snapshots stable ALTREP inputs once", {
  calls = 0L
  condition = CondEqual(1L)
  values = native_stateful_altrep(
    c(1L, 1L),
    c(9L, 9L),
    elt_switch_after = 2L,
    callback = function() {
      calls <<- calls + 1L
      condition[[1L]] <<- 9L
      invisible(gc())
    },
    callback_after = 0L
  )

  expect_identical(condition_test(condition, values), c(TRUE, TRUE))
  expect_identical(condition[[1L]], 9L)
  expect_identical(calls, 1L)
})

test_that("native Condition admission rejects duplicate AnyOf snapshots", {
  duplicated = CondAnyOf(c(1L, 2L))
  duplicated[[1L]] = c(1L, 1L)

  expect_error(
    condition_test(duplicated, 1:2),
    "Malformed built-in Condition object"
  )
})

test_that("closed Condition boundaries reject S4-marked structure", {
  expect_error(CondEqual(asS4(1L)), "must not be an S4 object", fixed = TRUE)
  expect_error(
    CondAnyOf(asS4(c(1L, 2L))),
    "must not be an S4 object",
    fixed = TRUE
  )
  expect_error(
    Condition(1L, asS4("%s == %s")),
    "must not be an S4 object",
    fixed = TRUE
  )
  expect_error(
    condition_as_string(CondEqual(1L), asS4("x")),
    "must not be an S4 object",
    fixed = TRUE
  )

  condition = CondEqual(1L)
  expect_error(
    condition_test(asS4(condition), 1L),
    "Malformed built-in Condition object",
    fixed = TRUE
  )

  malformed = condition
  attr(malformed, "class") = asS4(class(malformed))
  expect_error(condition_test(malformed, 1L), "Unsupported Condition class")

  malformed = condition
  attr(malformed, "names") = asS4(names(malformed))
  expect_error(
    condition_test(malformed, 1L),
    "Malformed built-in Condition object",
    fixed = TRUE
  )

  malformed = condition
  malformed[[1L]] = asS4(1L)
  expect_error(
    condition_test(malformed, 1L),
    "Malformed built-in Condition object",
    fixed = TRUE
  )

  malformed = condition
  malformed[[2L]] = asS4("%s == %s")
  expect_error(
    condition_test(malformed, 1L),
    "Malformed built-in Condition object",
    fixed = TRUE
  )

  expect_error(
    condition_test(condition, asS4(1:2)),
    "plain atomic vector",
    fixed = TRUE
  )
  values = 1:2
  attr(values, "names") = asS4(c("one", "two"))
  expect_error(
    condition_test(condition, values),
    "comparison names are malformed",
    fixed = TRUE
  )
})

test_that("dependency comparison does not reinterpret an S4 ParamUty leaf", {
  parameter_set = ps(
    controller = p_uty(),
    dependent = p_int(depends = controller == 1L)
  )

  expect_match(
    parameter_set$check(list(controller = asS4(1L), dependent = 1L)),
    "dependency|supported|plain",
    ignore.case = TRUE
  )
})

test_that("cross-type Condition operands do not match instead of erroring", {
  # Paradox 1 compared through R's `==`, which coerced rather than erroring.
  # A character value simply never equals a numeric right-hand side here.
  expect_false(condition_test(CondEqual(1), "1"))
  expect_false(condition_test(CondEqual(1), "a"))
  expect_false(condition_test(CondEqual("1"), 1))
  expect_false(condition_test(CondEqual(TRUE), "TRUE"))
  expect_false(condition_test(CondAnyOf(c(1, 2)), "1"))
  expect_false(condition_test(CondAnyOf(c("a", "b")), 1L))
  expect_false(condition_test(CondEqual(1), NA_character_))
  expect_equal(condition_test(CondEqual(1), c("1", "2")), c(FALSE, FALSE))
  expect_equal(condition_test(CondEqual(1), character(0)), logical(0))
  expect_equal(
    condition_test(CondEqual("x"), c(one = 1, two = 2)),
    c(one = FALSE, two = FALSE)
  )

  # The logical/integer/double family stays mutually comparable.
  expect_true(condition_test(CondEqual(1), 1L))
  expect_true(condition_test(CondEqual(1L), TRUE))
  expect_false(condition_test(CondEqual(0), TRUE))

  # A cross-type operand is a comparison result, not an admission failure:
  # the structural gates still error.
  expect_error(
    condition_test(CondEqual(1), list(1)),
    "plain atomic vector",
    fixed = TRUE
  )
  expect_error(
    condition_test(CondEqual("a"), factor("a")),
    "plain atomic vector",
    fixed = TRUE
  )
  values = c("1", "2")
  attr(values, "names") = c("one", "two", "three")[1:2]
  expect_equal(
    condition_test(CondEqual(1), values),
    c(one = FALSE, two = FALSE)
  )
})

test_that("dependency evaluation gives the same answer as condition_test", {
  # The dependency kernel is the same closed comparator, so a cross-type parent
  # value must read as an unsatisfied condition, not as an unsupported operand.
  set = ps(u = p_uty(), c = p_dbl(0, 1, depends = u == 1))

  expect_false(condition_test(CondEqual(1), "1"))
  expect_false(set$test(list(u = "1", c = 0.5)))
  expect_match(
    set$check(list(u = "1", c = 0.5)),
    "can only be set if the following condition is met",
    fixed = TRUE
  )

  values = set$clone(deep = TRUE)
  values$values = list(u = "1", c = 0.5)
  expect_equal(names(values$get_values()), "u")

  # An operand the comparator cannot inspect at all keeps its own diagnostic.
  expect_match(
    set$check(list(u = list(1), c = 0.5)),
    "requires a plain scalar",
    fixed = TRUE
  )

  # A satisfied same-type comparison is unaffected.
  expect_true(set$check(list(u = 1, c = 0.5)))
  expect_match(
    set$check(list(u = 2, c = 0.5)),
    "can only be set if the following condition is met",
    fixed = TRUE
  )
})
