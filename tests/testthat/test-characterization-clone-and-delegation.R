context("characterization: clone and delegation")

test_that("ParamSet shallow and deep clones preserve the established alias boundaries", {
  Box = R6::R6Class(
    "ParadoxCharacterizationBox",
    public = list(
      value = NULL,
      initialize = function(value) self$value = value
    )
  )
  box = Box$new("original")
  original = ps(count = p_int(0, 10), payload = p_uty())
  original$values = list(count = 1L, payload = box)

  shallow = original$clone()
  deep = original$clone(deep = TRUE)

  shallow$values$count = 2L
  expect_identical(original$values$count, 1L)
  expect_identical(shallow$values$count, 2L)
  expect_identical(deep$values$count, 1L)

  shallow$values$payload$value = "changed through shallow clone"
  expect_identical(original$values$payload$value, "changed through shallow clone")
  expect_identical(shallow$values$payload$value, "changed through shallow clone")
  expect_identical(deep$values$payload$value, "original")

  deep$values$payload$value = "changed through deep clone"
  expect_identical(original$values$payload$value, "changed through shallow clone")
  expect_identical(deep$values$payload$value, "changed through deep clone")
})

test_that("dependency facades and Conditions are detached from every clone", {
  original = ps(enabled = p_lgl(), amount = p_int())
  original$add_dep("amount", "enabled", CondEqual(TRUE))
  shallow = original$clone()
  deep = original$clone(deep = TRUE)

  visible = shallow$deps
  visible$cond[[1L]]$rhs = FALSE

  expect_identical(visible$cond[[1L]]$rhs, FALSE)
  expect_identical(original$deps$cond[[1L]]$rhs, TRUE)
  expect_identical(shallow$deps$cond[[1L]]$rhs, TRUE)
  expect_identical(deep$deps$cond[[1L]]$rhs, TRUE)
})

test_that("ParamSetCollection clones preserve shallow child references and deep isolation", {
  child = ps(x = p_int(0, 10, init = 1L))
  collection = ParamSetCollection$new(list(component = child))
  shallow = collection$clone()
  deep = collection$clone(deep = TRUE)

  shallow$values = list(component.x = 2L)
  expect_identical(child$values$x, 2L)
  expect_identical(collection$values$component.x, 2L)
  expect_identical(shallow$values$component.x, 2L)
  expect_identical(deep$values$component.x, 1L)

  deep$values = list(component.x = 3L)
  expect_identical(child$values$x, 2L)
  expect_identical(collection$values$component.x, 2L)
  expect_identical(deep$values$component.x, 3L)
})

test_that("collection values delegate live while flatten and ps_union are detached", {
  left = ps(x = p_int(0, 10, init = 1L))
  right = ps(y = p_dbl(0, 1, init = 0.25))
  collection = ParamSetCollection$new(list(left = left, right = right))

  expect_identical(collection$values, list(left.x = 1L, right.y = 0.25))
  left$values$x = 2L
  expect_identical(collection$values$left.x, 2L)

  collection$values = list(left.x = 3L, right.y = 0.5)
  expect_identical(left$values, list(x = 3L))
  expect_identical(right$values, list(y = 0.5))

  flat = collection$flatten()
  union = ps_union(list(left = left, right = right))
  left$values$x = 4L
  right$values$y = 0.75

  expect_identical(collection$values, list(left.x = 4L, right.y = 0.75))
  expect_identical(flat$values, list(left.x = 3L, right.y = 0.5))
  expect_identical(union$values, list(left.x = 3L, right.y = 0.5))

  flat$values = list(left.x = 5L, right.y = 0.1)
  union$values = list(left.x = 6L, right.y = 0.2)
  expect_identical(left$values$x, 4L)
  expect_identical(right$values$y, 0.75)
})

test_that("collection child dependencies are live and flattened dependencies are snapshots", {
  child = ps(enabled = p_lgl(), amount = p_int())
  collection = ParamSetCollection$new(list(component = child))
  expect_identical(nrow(collection$deps), 0L)

  child$add_dep("amount", "enabled", CondEqual(TRUE))
  expect_identical(collection$deps$id, "component.amount")
  expect_identical(collection$deps$on, "component.enabled")

  flat = collection$flatten()
  union = ps_union(list(component = child))
  child$deps = child$deps[0L]

  expect_identical(nrow(collection$deps), 0L)
  expect_identical(flat$deps$id, "component.amount")
  expect_identical(flat$deps$on, "component.enabled")
  expect_identical(union$deps$id, "component.amount")
  expect_identical(union$deps$on, "component.enabled")
})

test_that("collection child trafos and constraints are live but flattened copies detach", {
  child = ps(x = p_dbl(0, 100))
  collection = ParamSetCollection$new(list(component = child))

  child$extra_trafo = function(x) {
    x$x = x$x + 1
    x
  }
  child$constraint = function(x) x$x <= 5

  flat = collection$flatten()
  union = ps_union(list(component = child))

  child$extra_trafo = function(x) {
    x$x = x$x + 10
    x
  }
  child$constraint = function(x) x$x <= 0

  expect_identical(collection$trafo(list(component.x = 1))$component.x, 11)
  expect_identical(flat$trafo(list(component.x = 1))$component.x, 2)
  expect_identical(union$trafo(list(component.x = 1))$component.x, 2)

  expect_false(collection$constraint(list(component.x = 1)))
  expect_true(flat$constraint(list(component.x = 1)))
  expect_true(union$constraint(list(component.x = 1)))
})
