context("characterization: serialization")

test_that("Domain, Condition, ParamSet, and collection survive raw serialization", {
  domain = p_fct(c("fast", "safe"), default = "safe", tags = "mode")
  condition = CondAnyOf(c("fast", "safe"))
  child = ps(
    count = p_int(0, 5, init = 2L),
    scale = p_dbl(0, 1, trafo = function(x) x + 10, depends = count == 2L)
  )
  collection = ParamSetCollection$new(list(component = child))
  objects = list(domain = domain, condition = condition, child = child, collection = collection)

  serialized = serialize(objects, NULL)
  restored = unserialize(serialized)

  expect_identical(typeof(serialized), "raw")
  expect_identical(class(restored$domain), c("ParamFct", "Domain", "data.table", "data.frame"))
  expect_identical(names(restored$domain), names(domain))
  expect_identical(vapply(restored$domain, typeof, character(1L)), vapply(domain, typeof, character(1L)))
  expect_true(is.call(attr(restored$domain, "repr")))
  expect_true(domain_test(restored$domain, list("fast")))

  expect_identical(class(restored$condition), c("CondAnyOf", "Condition"))
  expect_identical(unclass(restored$condition), unclass(condition))
  expect_identical(condition_test(restored$condition, c("fast", "other")), c(TRUE, FALSE))

  expect_identical(class(restored$child), c("ParamSet", "R6"))
  expect_identical(restored$child$ids(), c("count", "scale"))
  expect_identical(restored$child$values, list(count = 2L))
  expect_identical(restored$child$deps$id, "scale")
  expect_identical(restored$child$deps$on, "count")
  expect_identical(restored$child$trafo(list(count = 2L, scale = 0.5))$scale, 10.5)

  expect_identical(class(restored$collection), c("ParamSetCollection", "ParamSet", "R6"))
  expect_identical(restored$collection$ids(), c("component.count", "component.scale"))
  expect_identical(names(restored$collection$sets), "component")
  expect_identical(restored$collection$values, list(component.count = 2L))
  expect_identical(restored$collection$trafo(list(component.count = 2L, component.scale = 0.5))$component.scale, 10.5)

  restored$collection$values = list(component.count = 3L)
  expect_identical(restored$collection$sets$component$values, list(count = 3L))
  expect_identical(collection$values, list(component.count = 2L))
  expect_identical(child$values, list(count = 2L))
})

test_that("saveRDS and readRDS retain behavior and detach the restored graph", {
  child = ps(
    enabled = p_lgl(init = TRUE),
    amount = p_int(0, 10, depends = enabled == TRUE)
  )
  child$values = list(enabled = TRUE, amount = 4L)
  collection = ParamSetCollection$new(list(component = child))
  path = tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)

  saveRDS(collection, path, version = 3L)
  restored = readRDS(path)

  expect_identical(class(restored), c("ParamSetCollection", "ParamSet", "R6"))
  expect_identical(restored$ids(), c("component.enabled", "component.amount"))
  expect_identical(restored$values, list(component.enabled = TRUE, component.amount = 4L))
  expect_identical(restored$deps$id, "component.amount")
  expect_identical(restored$deps$on, "component.enabled")
  expect_true(restored$test(list(component.enabled = TRUE, component.amount = 5L)))
  expect_false(restored$test(list(component.enabled = FALSE, component.amount = 5L)))

  restored$sets$component$values = list(enabled = TRUE, amount = 6L)
  expect_identical(restored$values$component.amount, 6L)
  expect_identical(collection$values$component.amount, 4L)

  child$values = list(enabled = TRUE, amount = 7L)
  expect_identical(collection$values$component.amount, 7L)
  expect_identical(restored$values$component.amount, 6L)
})
