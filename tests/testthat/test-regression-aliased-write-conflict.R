# A value assignment plans one *complete* replacement of each base ParamSet's
# store. When two paths of the graph reach the same base set -- one set
# contained twice in a ParamSetCollection, or a ParamSetShadow next to its own
# origin -- the later plan wins outright, so a value the other path asked for is
# silently dropped or overwritten. The outcome is still deterministic; it is now
# announced.

test_that("conflicting values through two aliases warn", {
  child = ps(z = p_dbl(0, 1))
  collection = ParamSetCollection$new(list(a = child, b = child))

  expect_warning(
    collection$values <- list(a.z = 0.1, b.z = 0.9),
    "conflicting values for 'z'"
  )
  expect_identical(collection$values, list(a.z = 0.9, b.z = 0.9))
})

test_that("assigning through only one alias warns instead of losing the value", {
  child = ps(z = p_dbl(0, 1))
  collection = ParamSetCollection$new(list(a = child, b = child))

  # The plan for `b` is an empty complete replacement, so it wipes the value
  # `a` asked for. This dropped the assignment with no diagnostic at all.
  expect_warning(collection$values <- list(a.z = 0.5), "more than one path")
  expect_identical(collection$values, named_list())
  expect_identical(child$values, named_list())
})

test_that("agreeing plans through two aliases do not warn", {
  child = ps(z = p_dbl(0, 1))
  collection = ParamSetCollection$new(list(a = child, b = child))

  expect_silent(collection$values <- list(a.z = 0.25, b.z = 0.25))
  expect_identical(collection$values, list(a.z = 0.25, b.z = 0.25))

  # Equal, separately allocated values must compare equal, not by identity.
  expect_silent(collection$values <- list(a.z = 1 / 3, b.z = 2 / 6))

  # Clearing both aliases together is also one agreeing plan.
  expect_silent(collection$values <- named_list())
})

test_that("three aliases report the conflict once per target", {
  child = ps(z = p_dbl(0, 1))
  collection = ParamSetCollection$new(list(a = child, b = child, c = child))

  warnings = character(0)
  withCallingHandlers(
    collection$values <- list(a.z = 0.1, b.z = 0.2, c.z = 0.3),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # One report for the one conflicting target, not one per redundant path.
  expect_length(warnings, 1L)
  expect_true(all(grepl("more than one path", warnings)))
  expect_identical(collection$values, list(a.z = 0.3, b.z = 0.3, c.z = 0.3))
})

test_that("a shadow next to its own origin is the same conflict", {
  origin = ps(z = p_dbl(0, 1))
  view = ParamSetShadow$new(origin, character(0))
  collection = ParamSetCollection$new(list(s = view, d = origin))
  expect_identical(collection$ids(), c("s.z", "d.z"))

  expect_warning(collection$values <- list(s.z = 0.4), "more than one path")
  expect_silent(collection$values <- list(s.z = 0.4, d.z = 0.4))
  expect_identical(origin$values, list(z = 0.4))
})

test_that("an ordinary graph never reports a conflict", {
  left = ps(a = p_dbl(0, 1))
  right = ps(b = p_dbl(0, 1))
  collection = ParamSetCollection$new(list(l = left, r = right))
  expect_silent(collection$values <- list(l.a = 0.5, r.b = 0.5))

  nested = ParamSetCollection$new(list(inner = collection, extra = ps(c = p_lgl())))
  expect_silent(nested$values <- list(inner.l.a = 0.25, extra.c = TRUE))
  expect_identical(left$values, list(a = 0.25))

  plain = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
  expect_silent(plain$values <- list(x = 0.5, y = 0.5))

  origin = ps(hidden = p_int(), visible = p_int())
  origin$values = list(hidden = 7L, visible = 1L)
  view = ParamSetShadow$new(origin, "hidden")
  expect_silent(view$values <- list(visible = 3L))
  expect_identical(origin$values, list(hidden = 7L, visible = 3L))
})

test_that("a conflicting write still commits atomically", {
  child = ps(z = p_dbl(0, 1), w = p_dbl(0, 1))
  collection = ParamSetCollection$new(list(a = child, b = child))
  suppressWarnings(collection$values <- list(a.z = 0.1, b.z = 0.9, b.w = 0.2))
  # The winning plan is applied whole: `w` from the same plan survives.
  expect_identical(child$values, list(z = 0.9, w = 0.2))
})
