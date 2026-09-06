test_that("ParamSetCollection strict checks use live child constraints", {
  child = ps(x = p_int(0, 10))
  collection = psc(component = child)
  expect_false(collection$has_constraint)

  child$constraint = function(x) x$x <= 5L
  expect_true(collection$has_constraint)

  valid = list(component.x = 4L)
  invalid = list(component.x = 6L)

  expect_true(collection$test_constraint(valid))
  expect_false(collection$test_constraint(invalid))
  expect_true(collection$check(valid, check_strict = TRUE))
  expect_identical(
    collection$check(invalid, check_strict = TRUE),
    "Constraint not fulfilled."
  )

  child$constraint = function(x) x$x %% 2L == 0L
  expect_true(collection$test_constraint(invalid))
  expect_false(collection$test_constraint(list(component.x = 5L)))

  child$constraint = NULL
  expect_false(collection$has_constraint)
  expect_true(collection$test_constraint(invalid))
})

test_that("constraint checks do not authenticate a replaced R6 getter", {
  collection = psc(component = ps(x = p_int(0, 10)))
  reads = 0L
  # This shell is local to the test, so it need not recover the generated
  # binding through R >= 4.0's activeBindingFunction(). Keeping the hostile
  # replacement until collection lets the same non-invoking boundary run on
  # the declared-minimum R 3.6 runtime.
  makeActiveBinding("constraint", function(value) {
    if (!missing(value)) stop("constraint is read-only")
    reads <<- reads + 1L
    function(x) FALSE
  }, collection)

  expect_true(collection$test_constraint(list(component.x = 1L)))
  expect_identical(reads, 0L)
})
