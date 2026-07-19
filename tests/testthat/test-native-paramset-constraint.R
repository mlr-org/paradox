test_that("constraint-only calls use fixed registered native entries", {
  routines = getDLLRegisteredRoutines("paradox")$.Call
  expect_identical(
    routines$param_set_test_constraint_builtin$numParameters,
    4L
  )
  expect_identical(
    routines$param_set_test_constraint_dt_builtin$numParameters,
    4L
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("scalar constraint-only checks validate once and call once", {
  calls = 0L
  param_set = ps(x = p_int(0L, 2L))
  param_set$constraint = function(x) {
    calls <<- calls + 1L
    x$x < 2L
  }

  expect_true(param_set$test_constraint(list(x = 1L)))
  expect_identical(calls, 1L)
  expect_false(param_set$test_constraint(list(x = 2L)))
  expect_identical(calls, 2L)
  expect_error(
    param_set$test_constraint(list(x = 3L)),
    "Assertion on 'x' failed"
  )
  expect_identical(calls, 2L)
  expect_false(param_set$test_constraint(
    list(x = "outside"), assert_value = FALSE
  ))
  expect_identical(calls, 3L)

  param_set$constraint = function(x) NA
  expect_error(
    param_set$test_constraint(list(x = 1L)),
    "one non-missing logical"
  )
})

test_that("tabular constraint checks validate all rows before callbacks", {
  calls = integer()
  param_set = ps(x = p_int(0L, 2L))
  param_set$constraint = function(x) {
    calls <<- c(calls, x$x)
    x$x < 2L
  }

  invalid = data.table::data.table(x = c(1L, 3L))
  expect_error(
    param_set$test_constraint_dt(invalid),
    "Assertion on 'x' failed"
  )
  expect_identical(calls, integer())

  values = data.table::data.table(x = 0:2)
  expect_identical(
    param_set$test_constraint_dt(values),
    c(TRUE, TRUE, FALSE)
  )
  expect_identical(calls, 0:2)
  expect_error(
    param_set$test_constraint_dt(data.frame(x = 1L)),
    "data.table",
    fixed = TRUE
  )
})

test_that("tabular constraint callbacks retain one operation snapshot", {
  calls = 0L
  param_set = ps(x = p_int(0L, 3L))
  original = function(x) {
    calls <<- calls + 1L
    if (calls == 1L) {
      param_set$constraint = function(x) FALSE
    }
    x$x <= 2L
  }
  param_set$constraint = original

  expect_identical(
    param_set$test_constraint_dt(data.table::data.table(x = 1:3)),
    c(TRUE, TRUE, FALSE)
  )
  expect_identical(calls, 3L)
  expect_false(param_set$test_constraint(list(x = 1L)))
})

test_that("collection and Shadow constraint-only calls use capsule graphs", {
  child_calls = 0L
  child = ps(x = p_int())
  child$constraint = function(x) {
    child_calls <<- child_calls + 1L
    x$x > 0L
  }
  collection = psc(owner = child)
  expect_identical(
    collection$test_constraint_dt(
      data.table::data.table(owner.x = c(-1L, 1L))
    ),
    c(FALSE, TRUE)
  )
  expect_identical(child_calls, 2L)

  origin = ps(visible = p_int(), hidden = p_int(init = 2L))
  origin$constraint = function(x) x$visible < x$hidden
  shadow = ParamSetShadow$new(origin, "hidden")
  expect_true(shadow$test_constraint(list(visible = 1L)))
  expect_false(shadow$test_constraint(list(visible = 3L)))
})
