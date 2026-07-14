context("characterization: value mutation")

empty_named_values = function() {
  structure(list(), names = character())
}

test_that("direct assignment sanitizes only after validation and is atomic", {
  events = new.env(parent = emptyenv())
  events$seen = list()
  holder = new.env(parent = emptyenv())

  param_set = ps(
    count = p_int(0L, 10L, tolerance = 0.2),
    ratio = p_dbl(0, 1, tolerance = 0.1),
    .constraint = function(x) {
      events$seen = c(events$seen, list(list(
        candidate = x,
        stored = holder$param_set$values
      )))
      !identical(x$count, 7L)
    }
  )
  holder$param_set = param_set
  param_set$values = list(count = 1L, ratio = 0.5)
  events$seen = list()

  assigned = list(ratio = 1.05, count = 2.1)
  returned = withVisible(param_set$values <- assigned)
  expect_false(returned$visible)
  expect_identical(returned$value, assigned)
  expect_identical(
    events$seen,
    list(list(
      candidate = assigned,
      stored = list(count = 1L, ratio = 0.5)
    ))
  )
  expect_identical(param_set$values, list(count = 2L, ratio = 1))

  events$seen = list()
  expect_error(
    { param_set$values = list(count = 3L, ratio = 2) },
    "Element 1 is not <= 1.1",
    fixed = TRUE
  )
  expect_identical(events$seen, list())
  expect_identical(param_set$values, list(count = 2L, ratio = 1))

  expect_error(
    { param_set$values = list(count = 7L, ratio = 0.25) },
    "Constraint not fulfilled",
    fixed = TRUE
  )
  expect_identical(
    events$seen,
    list(list(
      candidate = list(count = 7L, ratio = 0.25),
      stored = list(count = 2L, ratio = 1)
    ))
  )
  expect_identical(param_set$values, list(count = 2L, ratio = 1))
})

test_that("set_values keeps promise forcing and failure atomicity", {
  param_set = ps(a = p_int(), b = p_int(), c = p_int())
  param_set$values = list(a = 1L, b = 2L)
  events = character()

  expect_invisible(param_set$set_values(
    a = {
      events = c(events, "dots")
      3L
    },
    .values = {
      events = c(events, "values")
      list(c = 4L)
    },
    .insert = {
      events = c(events, "insert")
      TRUE
    }
  ))
  expect_identical(events, c("dots", "values", "insert"))
  expect_identical(param_set$values, list(a = 3L, b = 2L, c = 4L))

  events = character()
  expect_error(
    param_set$set_values(
      a = 5L,
      a = 6L,
      .values = {
        events = c(events, "values")
        list(b = 7L)
      },
      .insert = {
        events = c(events, "insert")
        FALSE
      }
    ),
    "Must have unique names",
    fixed = TRUE
  )
  expect_identical(events, character())
  expect_identical(param_set$values, list(a = 3L, b = 2L, c = 4L))

  expect_error(
    param_set$set_values(a = 5L, unknown = 1L),
    "Parameter 'unknown' not available",
    fixed = TRUE
  )
  expect_identical(param_set$values, list(a = 3L, b = 2L, c = 4L))
})

test_that("unchecked storage chooses first duplicates and owns its shell", {
  param_set = ps(a = p_int(), b = p_uty())
  param_set$assert_values = FALSE
  reference = new.env(parent = emptyenv())
  reference$value = 1L
  incoming = structure(
    list(reference, 2L, 9L, "ignored"),
    names = c("b", "a", "a", "unknown")
  )

  param_set$values = incoming
  expect_identical(param_set$values, list(a = 2L, b = reference))
  incoming[[2L]] = 8L
  incoming[[1L]] = NULL
  expect_identical(param_set$values, list(a = 2L, b = reference))
  reference$value = 3L
  expect_identical(param_set$values$b$value, 3L)

  returned = withVisible(param_set$values <- integer())
  expect_false(returned$visible)
  expect_identical(returned$value, integer())
  expect_identical(param_set$values, empty_named_values())

  private = param_set$.__enclos_env__$private
  unnamed = private$.store_values(list())
  expect_identical(unnamed, list())
  expect_null(names(unnamed))
  expect_null(names(private$.values))

  named = private$.store_values(structure(list(), names = character()))
  expect_identical(named, structure(list(), names = character()))
  expect_identical(names(private$.values), character())
})

test_that("shared collection children retain last-owner mutation semantics", {
  child = ps(a = p_int(), b = p_int())
  child$values = list(a = 0L, b = 0L)
  collection = ParamSetCollection$new(list(left = child, right = child))

  collection$values = list(
    right.b = 4L,
    left.a = 1L,
    right.a = 3L,
    left.b = 2L
  )
  expect_identical(child$values, list(a = 3L, b = 4L))
  expect_identical(
    collection$values,
    list(left.a = 3L, left.b = 4L, right.a = 3L, right.b = 4L)
  )

  # In insertion mode both aliases are present in the merged assignment. The
  # later owner therefore restores its old value over the earlier alias.
  collection$set_values(left.a = 8L)
  expect_identical(child$values, list(a = 3L, b = 4L))

  collection$set_values(right.b = NULL)
  expect_identical(child$values, list(a = 3L))
  expect_identical(collection$values, list(left.a = 3L, right.a = 3L))
})

test_that("nested postfix mutation translates and clears at each level", {
  left = ps(a = p_int(init = 1L), z = p_lgl(init = TRUE))
  right = ps(b = p_int(init = 2L))
  inner = ParamSetCollection$new(
    list(left = left, right = right),
    postfix_names = TRUE
  )
  tail = ps(q = p_dbl(init = 0.5))
  outer = ParamSetCollection$new(
    list(inner = inner, tail = tail),
    postfix_names = TRUE
  )

  outer$values = list(
    a.left.inner = 4L,
    b.right.inner = 5L,
    q.tail = 0.75
  )
  expect_identical(left$values, list(a = 4L))
  expect_identical(right$values, list(b = 5L))
  expect_identical(tail$values, list(q = 0.75))

  outer$values = list(z.left.inner = FALSE)
  expect_identical(left$values, list(z = FALSE))
  expect_identical(right$values, empty_named_values())
  expect_identical(tail$values, empty_named_values())
  expect_identical(outer$values, list(z.left.inner = FALSE))
})

test_that("collection validation failures do not touch any child", {
  left = ps(a = p_int(0L, 2L))
  right = ps(b = p_int(0L, 2L))
  collection = ParamSetCollection$new(list(left = left, right = right))
  collection$values = list(left.a = 1L, right.b = 1L)

  expect_error(
    { collection$values = list(left.a = 2L, right.b = 9L) },
    "right.b: Element 1 is not <= 2.5",
    fixed = TRUE
  )
  expect_identical(left$values, list(a = 1L))
  expect_identical(right$values, list(b = 1L))

  expect_error(
    collection$set_values(left.a = 2L, unknown = 1L),
    "Parameter 'unknown' not available",
    fixed = TRUE
  )
  expect_identical(left$values, list(a = 1L))
  expect_identical(right$values, list(b = 1L))
})

test_that("extension setter errors retain historical delegation order", {
  events = new.env(parent = emptyenv())
  events$seen = character()
  events$fail = NULL

  Child = R6::R6Class(
    "CharacterizationMutationChild",
    inherit = ParamSet,
    public = list(
      initialize = function(label) {
        private$.label = label
        super$initialize(list(value = p_int(init = 0L)))
      }
    ),
    active = list(
      values = function(value) {
        if (!missing(value)) {
          events$seen = c(events$seen, paste0(private$.label, ":set"))
          if (identical(events$fail, private$.label)) {
            stop(paste0(private$.label, " rejected values"))
          }
          super$values = value
          return(value)
        }
        super$values
      }
    ),
    private = list(.label = NULL)
  )

  one = Child$new("one")
  two = Child$new("two")
  three = Child$new("three")
  collection = ParamSetCollection$new(list(one = one, two = two, three = three))
  events$seen = character()
  events$fail = "two"

  expect_error(
    { collection$values = list(one.value = 1L, two.value = 2L) },
    "two rejected values",
    fixed = TRUE
  )
  expect_identical(events$seen, c("one:set", "two:set"))
  expect_identical(one$values, list(value = 1L))
  expect_identical(two$values, list(value = 0L))
  expect_identical(three$values, list(value = 0L))
})

test_that("mutation survives clone, serialization, and forced collection", {
  skip_on_cran()

  reference = new.env(parent = emptyenv())
  reference$value = 1L
  child = ps(value = p_uty(), count = p_int())
  child$values = list(value = reference, count = 1L)
  collection = ParamSetCollection$new(list(child = child))

  clone = collection$clone(deep = TRUE)
  clone$set_values(child.count = 2L)
  expect_identical(collection$values$child.count, 1L)
  expect_identical(clone$values$child.count, 2L)
  expect_identical(clone$values$child.value, reference)

  restored = unserialize(serialize(collection, NULL, version = 3L))
  restored$values = list(child.count = 3L)
  expect_identical(restored$values, list(child.count = 3L))
  expect_identical(collection$values$child.count, 1L)

  previous = gctorture2(1L, wait = 0L)
  on.exit(gctorture2(previous), add = TRUE)
  collection$set_values(child.count = 4L)
  observed = collection$values
  gctorture2(previous)
  expect_identical(observed$count, NULL)
  expect_identical(observed$child.count, 4L)
})
