empty_deps_state = function(param_set) {
  paradox:::param_set_core_state(param_set$.__enclos_env__$private)$.deps
}

expect_empty_deps_capsule = function(deps) {
  expect_identical(class(deps), "data.frame")
  expect_identical(names(deps), c("id", "on", "cond"))
  expect_identical(nrow(deps), 0L)
  expect_identical(
    vapply(deps, typeof, character(1L)),
    c(id = "character", on = "character", cond = "list")
  )
  expect_identical(attr(deps, "row.names", exact = TRUE), integer())
  expect_null(attr(deps, ".internal.selfref", exact = TRUE))
  expect_null(attr(deps, "index", exact = TRUE))
  expect_null(attr(deps, "sorted", exact = TRUE))
}

expect_empty_deps_facade = function(deps) {
  expect_identical(class(deps), c("data.table", "data.frame"))
  expect_identical(names(deps), c("id", "on", "cond"))
  expect_identical(nrow(deps), 0L)
  expect_identical(
    vapply(deps, typeof, character(1L)),
    c(id = "character", on = "character", cond = "list")
  )
  expect_identical(data.table::key(deps), NULL)
  expect_identical(data.table::indices(deps), NULL)
  expect_identical(data.table:::selfrefok(deps, FALSE), 1L)
}

test_that("empty dependency capsules and public facades are instance-local", {
  first = ps(x = p_dbl())
  second = ps(x = p_dbl())
  first_state = empty_deps_state(first)
  second_state = empty_deps_state(second)

  expect_empty_deps_capsule(first_state)
  expect_empty_deps_capsule(second_state)
  expect_false(identical(
    data.table::address(first_state),
    data.table::address(second_state)
  ))

  first_facade = first$deps
  second_facade = second$deps
  expect_empty_deps_facade(first_facade)
  expect_empty_deps_facade(second_facade)
  expect_false(identical(
    data.table::address(first_facade),
    data.table::address(second_facade)
  ))

  data.table::setindexv(first_facade, c("on", "id"))
  data.table::set(first_facade, j = "marker", value = logical())
  expect_empty_deps_capsule(empty_deps_state(first))
  expect_empty_deps_capsule(empty_deps_state(second))
  expect_empty_deps_facade(first$deps)
  expect_empty_deps_facade(second$deps)
})

test_that("collections also own empty dependency capsules and facades", {
  first = ParamSetCollection$new(list(component = ps(x = p_dbl())))
  second = ParamSetCollection$new(list(component = ps(x = p_dbl())))
  first_state = empty_deps_state(first)
  second_state = empty_deps_state(second)

  expect_empty_deps_capsule(first_state)
  expect_empty_deps_capsule(second_state)
  expect_false(identical(
    data.table::address(first_state),
    data.table::address(second_state)
  ))

  facade = first$deps
  expect_empty_deps_facade(facade)
  data.table::setindexv(facade, c("on", "id"))
  expect_empty_deps_capsule(empty_deps_state(first))
  expect_empty_deps_capsule(empty_deps_state(second))
  expect_empty_deps_facade(first$deps)
  expect_empty_deps_facade(second$deps)
})

test_that("clone and serialization isolate empty dependencies across mutation", {
  original = ps(enabled = p_lgl(), amount = p_int())
  shallow = original$clone(deep = FALSE)
  deep = original$clone(deep = TRUE)
  restored = unserialize(serialize(original, NULL, version = 3L))

  for (object in list(original, shallow, deep, restored)) {
    expect_empty_deps_capsule(empty_deps_state(object))
    expect_empty_deps_facade(object$deps)
  }

  expect_no_warning(original$add_dep("amount", "enabled", CondEqual(TRUE)))
  expect_identical(original$deps$id, "amount")
  expect_identical(original$deps$on, "enabled")
  expect_s3_class(original$deps$cond[[1L]], "CondEqual")
  expect_identical(original$deps$cond[[1L]]$rhs, TRUE)

  for (object in list(shallow, deep, restored)) {
    expect_empty_deps_capsule(empty_deps_state(object))
    facade = object$deps
    data.table::setindexv(facade, c("on", "id"))
    expect_empty_deps_facade(object$deps)
  }
})
