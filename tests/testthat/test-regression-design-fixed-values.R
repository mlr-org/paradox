# Fixed `$values` are written into the generated design at one
# generator-independent boundary. The native grid generator performs the same
# normalization while pruning its search, so both paths must agree: only one
# ordinary element of the parameter's own storage type collapses into a typed
# column, and every other legal stored value keeps its identity.

test_that("a cross-storage special value keeps its identity in every generator", {
  param_set = ps(
    a = p_dbl(0, 1, special_vals = list("none")),
    b = p_dbl(0, 1)
  )
  param_set$values = list(a = "none")

  grid = generate_design_grid(param_set, resolution = 2L)
  random = generate_design_random(param_set, 2L)

  for (design in list(grid, random)) {
    expect_true(is.list(design$data$a))
    expect_identical(unique(unlist(design$data$a)), "none")
    expect_identical(design$transpose(trafo = FALSE)[[1L]]$a, "none")
  }
})

test_that("a fixed NULL keeps its column and reaches every configuration", {
  param_set = ps(a = p_dbl(0, 1), u = p_uty(special_vals = list(NULL)))
  param_set$values = list(u = NULL)

  design = Design$new(
    param_set,
    data.table(a = c(0.1, 0.2), u = list(1, 2)),
    remove_dupl = FALSE
  )

  expect_setequal(names(design$data), c("a", "u"))
  transposed = design$transpose(trafo = FALSE)
  expect_identical(length(transposed), 2L)
  for (configuration in transposed) {
    expect_true("u" %in% names(configuration))
    expect_null(configuration$u)
  }
})

test_that("container and multi-element fixed values are neither unwrapped nor recycled", {
  param_set = ps(a = p_dbl(0, 1), u = p_uty())
  param_set$values = list(u = list(x = 1))
  design = Design$new(
    param_set,
    data.table(a = c(0.1, 0.2), u = list(1, 2)),
    remove_dupl = FALSE
  )
  expect_identical(design$transpose(trafo = FALSE)[[1L]]$u, list(x = 1))

  param_set$values = list(u = c(10, 20))
  design = Design$new(
    param_set,
    data.table(a = c(0.1, 0.2, 0.3), u = list(1, 2, 3)),
    remove_dupl = FALSE
  )
  for (configuration in design$transpose(trafo = FALSE)) {
    expect_identical(configuration$u, c(10, 20))
  }
})

test_that("ordinary fixed values still produce ordinary typed columns", {
  param_set = ps(
    a = p_dbl(0, 1),
    b = p_int(1, 4),
    c = p_fct(c("x", "y")),
    d = p_lgl(),
    e = p_dbl(0, 1)
  )
  param_set$values = list(a = 0.5, b = 2L, c = "x", d = TRUE)

  design = generate_design_random(param_set, 3L)
  expect_identical(
    vapply(design$data, function(column) class(column)[[1L]], character(1)),
    c(a = "numeric", b = "integer", c = "character", d = "logical", e = "numeric")
  )
  expect_identical(design$data$a, rep(0.5, 3L))
  expect_identical(design$data$b, rep(2L, 3L))
  expect_identical(design$data$c, rep("x", 3L))
  expect_identical(design$data$d, rep(TRUE, 3L))
})

test_that("a stored TuneToken is rejected by every design generator", {
  param_set = ps(a = p_dbl(0, 1), b = p_dbl(0, 1))
  param_set$values = list(a = to_tune(0.2, 0.8))

  expect_error(generate_design_grid(param_set, 2L), "TuneToken")
  expect_error(generate_design_random(param_set, 2L), "TuneToken")
  expect_error(
    Design$new(
      param_set,
      data.table(a = c(1, 2), b = c(3, 4)),
      remove_dupl = FALSE
    ),
    "TuneToken"
  )
})
