test_that("collection Domain ordering follows the canonical capsule schema", {
  shared = ps(x = p_int(init = 1L), y = p_lgl())
  collection = ParamSetCollection$new(list(left = shared, right = shared))
  observed = collection$domains

  expect_identical(
    names(observed),
    c("left.x", "left.y", "right.x", "right.y")
  )
  expect_identical(
    unname(vapply(observed, `[[`, character(1L), "id")),
    names(observed)
  )
  expect_identical(observed$left.x$.init[[1L]], 1L)
  expect_identical(observed$right.x$.init[[1L]], 1L)
})

test_that("core method overrides do not participate in collection Domains", {
  events = character()
  Additive = R6::R6Class(
    "CollectionDomainsAdditive",
    inherit = ParamSet,
    public = list(
      ids = function(...) {
        events <<- c(events, "override")
        super$ids(...)
      },
      marker = TRUE
    )
  )
  child = Additive$new(list(x = p_dbl(0, 1)))
  collection = psc(component = child)
  events = character()
  expect_identical(collection$domains$component.x$id, "component.x")
  expect_identical(events, character())
  expect_true(child$marker)
})
