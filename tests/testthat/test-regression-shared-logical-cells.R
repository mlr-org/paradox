# Single-element cells cut out of a public table column must be independently
# owned. Two hazards are specific to this boundary:
#
#   * `Rf_ScalarLogical()` returns R's shared TRUE/FALSE/NA singletons, so
#     copying a column's attributes onto such a cell would attach them to every
#     `TRUE` in the session;
#   * a column's attribute set can encode the column's own length (`names`,
#     `dim`, `dimnames`), which is not a valid description of a one-element cell.

test_that("logical cells of an attributed column are independently owned", {
  first = .Call(
    paradox:::C_design_transpose,
    data.table(flag = structure(c(TRUE, TRUE), class = "first_class"), x = c(1, 2)),
    TRUE
  )
  second = .Call(
    paradox:::C_design_transpose,
    data.table(flag = structure(c(TRUE, TRUE), class = "second_class"), x = c(1, 2)),
    TRUE
  )

  expect_false(identical(first[[1L]]$flag, second[[1L]]$flag))
  expect_identical(class(first[[1L]]$flag), "first_class")
  expect_identical(class(second[[1L]]$flag), "second_class")
  expect_identical(unclass(first[[1L]]$flag), TRUE)
  expect_identical(unclass(second[[1L]]$flag), TRUE)
})

test_that("table checking does not attach column attributes to shared logicals", {
  set = ps(flag = p_lgl(), x = p_dbl(0, 1))
  set$constraint = function(x) TRUE
  table = data.table(
    flag = structure(c(TRUE, FALSE, NA), tagged = "yes"),
    x = c(0.1, 0.2, 0.3)
  )

  expect_identical(set$test_constraint_dt(table), c(TRUE, TRUE, TRUE))
  # `is.null()` returns the shared singletons; a poisoned TRUE would surface here.
  expect_null(attributes(is.null(NULL)))
  expect_null(attributes(is.null(1)))
})

test_that("length-coupled column attributes never reach a one-element cell", {
  observed = list()
  set = ps(x = p_dbl(0, 1), y = p_int(1, 5))
  set$constraint = function(x) {
    observed[[length(observed) + 1L]] <<- x
    TRUE
  }

  table = data.table(x = c(0.1, 0.2, 0.3), y = 1:3)
  setattr(table$x, "dim", c(3L, 1L))
  expect_identical(set$test_constraint_dt(table), c(TRUE, TRUE, TRUE))

  cell = observed[[1L]]$x
  expect_identical(length(cell), 1L)
  expect_null(attr(cell, "dim", exact = TRUE))
  expect_null(attr(cell, "dimnames", exact = TRUE))
  expect_null(attr(cell, "names", exact = TRUE))
  expect_identical(as.numeric(cell), 0.1)
})

test_that("ordinary column classes still reach the cell", {
  observed = list()
  set = ps(day = p_uty(), grade = p_uty())
  set$constraint = function(x) {
    observed[[length(observed) + 1L]] <<- x
    TRUE
  }

  table = data.table(
    day = as.Date(c("2020-01-01", "2020-01-02")),
    grade = factor(c("a", "b"), levels = c("a", "b"))
  )
  expect_identical(set$test_constraint_dt(table), c(TRUE, TRUE))
  expect_identical(observed[[1L]]$day, as.Date("2020-01-01"))
  expect_identical(observed[[1L]]$grade, factor("a", levels = c("a", "b")))

  transposed = .Call(paradox:::C_design_transpose, table, TRUE)
  expect_identical(transposed[[2L]]$day, as.Date("2020-01-02"))
  expect_identical(transposed[[2L]]$grade, factor("b", levels = c("a", "b")))
})
