test_that("ParamSetCollection constraints receive child-specific unprefixed values", {
  received = new.env(parent = emptyenv())
  received$one = NULL
  received$two = NULL

  one = ps(
    x = p_int(),
    .constraint = function(x) {
      received$one = x
      identical(x, list(x = 1L))
    }
  )
  two = ps(
    y = p_int(),
    .constraint = function(x) {
      received$two = x
      identical(x, list(y = 2L))
    }
  )
  collection = ParamSetCollection$new(list(one = one, two = two))
  values = list(one.x = 1L, two.y = 2L)

  expect_true(collection$constraint(values))
  expect_identical(received$one, list(x = 1L))
  expect_identical(received$two, list(y = 2L))

  received$one = NULL
  received$two = NULL
  expect_true(collection$flatten()$test_constraint(values))
  expect_identical(received$one, list(x = 1L))
  expect_identical(received$two, list(y = 2L))
})
