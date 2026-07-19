test_that("native Design transpose is authoritative for ordinary columns", {
  native = get("C_design_transpose", envir = asNamespace("paradox"))
  opaque = new.env(parent = emptyenv())
  data = list(
    logical = c(TRUE, NA),
    integer = c(1L, NA_integer_),
    double = c(1, NaN),
    complex = c(1 + 2i, NA_complex_),
    character = c("x", NA_character_),
    raw = as.raw(c(1, 2)),
    factor = factor(c("a", NA), levels = c("a", "b")),
    utility = list(opaque, NULL)
  )

  unfiltered = .Call(native, data, FALSE)
  expect_identical(names(unfiltered[[1L]]), names(data))
  expect_identical(unfiltered[[1L]]$factor, factor("a", levels = c("a", "b")))
  expect_identical(unfiltered[[1L]]$utility, opaque)
  expect_identical(unfiltered[[2L]]$utility, NULL)

  filtered = .Call(native, data, TRUE)
  expect_named(filtered[[1L]], names(data))
  expect_named(filtered[[2L]], c("raw", "utility"))
  expect_identical(filtered[[2L]]$utility, NULL)
})

test_that("native transpose materializes compact inputs and owns row shells", {
  native = get("C_design_transpose", envir = asNamespace("paradox"))
  source_names = c("x", "y")
  data = setNames(list(1:3, c("a", "b", "c")), source_names)
  result = .Call(native, data, FALSE)

  expect_identical(result, list(
    list(x = 1L, y = "a"),
    list(x = 2L, y = "b"),
    list(x = 3L, y = "c")
  ))
  names(result[[1L]])[[1L]] = "changed"
  result[[1L]]$y = "changed"
  expect_identical(names(data), source_names)
  expect_identical(names(result[[2L]]), source_names)
  expect_identical(data$y, c("a", "b", "c"))
})

test_that("native transpose rejects malformed state without a sentinel", {
  native = get("C_design_transpose", envir = asNamespace("paradox"))
  expect_error(.Call(native, 1:2, FALSE), "list-like")
  expect_error(.Call(native, unname(list(1:2)), FALSE), "list-like")
  expect_error(.Call(native, setNames(list(1:2), ""), FALSE), "name")
  expect_error(.Call(native, list(x = 1:2, y = 1L), FALSE), "length")
  expect_error(.Call(native, list(x = 1L), NA), "filter_na")
  expect_error(.Call(native, list(x = 1L), logical()), "filter_na")
  expect_error(
    .Call(native, list(x = matrix(1:4, 2L)), FALSE),
    "structural attributes"
  )
})

test_that("Design observes its then-current public data exactly once", {
  parameter_set = ps(x = p_dbl(), y = p_uty())
  design = Design$new(
    parameter_set,
    data.table(x = c(1, NA_real_), y = list("a", NULL)),
    remove_dupl = FALSE
  )
  design$data$x[[1L]] = 2
  expect_identical(
    design$transpose(filter_na = TRUE, trafo = FALSE),
    list(list(x = 2, y = "a"), list(y = NULL))
  )
})
