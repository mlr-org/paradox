context("regression: empty dependency table isolation")

private_deps = function(param_set) {
  param_set$.__enclos_env__$private$.deps
}

expect_empty_deps_schema = function(deps) {
  expect_identical(class(deps), c("data.table", "data.frame"))
  expect_identical(names(deps), c("id", "on", "cond"))
  expect_identical(nrow(deps), 0L)
  expect_identical(vapply(deps, typeof, character(1L)), c(id = "character", on = "character", cond = "list"))
  expect_identical(attr(deps, "row.names"), integer(0L))
}

test_that("empty ParamSet dependency metadata cannot leak between instances", {
  indexed = ps(x = p_dbl())
  untouched = ps(x = p_dbl())
  untouched_attributes = attributes(private_deps(untouched))

  # This reproduces the data.table side effect that made equality of two
  # independently constructed miesmuschel operators depend on test order.
  data.table::setindexv(private_deps(indexed), c("on", "id"))

  expect_identical(attributes(private_deps(untouched)), untouched_attributes)
  expect_identical(attributes(private_deps(ps(x = p_dbl()))), untouched_attributes)
  expect_empty_deps_schema(untouched$deps)
})

test_that("ParamSetCollection also owns its inherited dependency table", {
  indexed = ParamSetCollection$new(list(component = ps(x = p_dbl())))
  untouched = ParamSetCollection$new(list(component = ps(x = p_dbl())))
  untouched_attributes = attributes(private_deps(untouched))

  data.table::setindexv(private_deps(indexed), c("on", "id"))

  expect_identical(attributes(private_deps(untouched)), untouched_attributes)
  expect_identical(
    attributes(private_deps(ParamSetCollection$new(list()))),
    untouched_attributes
  )
  expect_empty_deps_schema(untouched$deps)
})

test_that("fresh dependency tables retain data.table, clone, and serialization behavior", {
  original = ps(enabled = p_lgl(), amount = p_int())
  deep = original$clone(deep = TRUE)
  restored = unserialize(serialize(original, NULL, version = 3L))

  for (object in list(original, deep, restored)) {
    expect_empty_deps_schema(object$deps)
    expect_no_warning(data.table::setindexv(object$deps, c("on", "id")))
  }

  expect_no_warning(original$add_dep("amount", "enabled", CondEqual(TRUE)))
  expect_identical(original$deps$id, "amount")
  expect_identical(original$deps$on, "enabled")
  expect_true(condition_test(original$deps$cond[[1L]], TRUE))
  expect_empty_deps_schema(deep$deps)
  expect_empty_deps_schema(restored$deps)
})
