context("characterization: data.table interoperability")

test_that("Domain copies support ordinary data.table by-reference operations", {
  domain = p_int(0, 10, tags = "numeric")
  table = data.table::copy(domain)

  expect_no_warning(data.table::set(table, j = "marker", value = TRUE))
  expect_no_warning(data.table::setkeyv(table, "id"))

  expect_identical(class(table)[1:4], c("ParamInt", "Domain", "data.table", "data.frame"))
  expect_identical(table$marker, TRUE)
  expect_false("marker" %in% names(domain))
})

test_that("as.data.table returns a mutable detached view", {
  param_set = ps(x = p_int(0, 10), y = p_dbl(-1, 1))
  table = as.data.table(param_set)

  expect_identical(class(table), c("data.table", "data.frame"))
  expect_no_warning(data.table::set(table, i = 1L, j = "lower", value = -100))
  expect_no_warning(data.table::set(table, j = "marker", value = seq_len(nrow(table))))
  expect_no_warning(data.table::setkeyv(table, "id"))

  expect_identical(table["x", lower], -100)
  expect_identical(table$marker, c(1L, 2L))
  expect_identical(table[class == "ParamInt", "id"][[1L]], "x")
  projection = table[, c("id", "class", "lower", "upper", "nlevels"), with = FALSE]
  expect_identical(names(projection), c("id", "class", "lower", "upper", "nlevels"))
  expect_identical(param_set$lower[["x"]], 0)
  expect_false("marker" %in% names(as.data.table(param_set)))
})

test_that("params returns a mutable detached table including its list columns", {
  param_set = ps(
    x = p_int(0, 10, tags = "numeric"),
    y = p_dbl(-1, 1, trafo = sqrt)
  )
  params = param_set$params

  expect_no_warning(data.table::set(params, i = 1L, j = "lower", value = -200))
  expect_no_warning(data.table::set(params, i = 1L, j = ".tags", value = list("changed")))
  expect_no_warning(data.table::set(params, j = "marker", value = c("first", "second")))

  expect_identical(params$lower[[1L]], -200)
  expect_identical(params$.tags[[1L]], "changed")
  expect_identical(params[list("y"), "cls", on = "id"][[1L]], "ParamDbl")
  expect_identical(param_set$lower[["x"]], 0)
  expect_identical(param_set$tags$x, "numeric")
  expect_false("marker" %in% names(param_set$params))
})

test_that("empty returned tables retain data.table behavior", {
  param_set = ParamSet$new()
  table = as.data.table(param_set)
  params = param_set$params

  expect_no_warning(data.table::set(table, j = "marker", value = integer()))
  expect_no_warning(data.table::set(params, j = "marker", value = integer()))
  expect_no_warning(data.table::setkeyv(table, "id"))
  expect_no_warning(data.table::setkeyv(params, "id"))

  expect_identical(nrow(table), 0L)
  expect_identical(nrow(params), 0L)
  expect_identical(typeof(table$marker), "integer")
  expect_identical(typeof(params$marker), "integer")
})
