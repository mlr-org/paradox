test_that("empty ParamSet check inputs honor presence", {
  param_set = ps(
    optional = p_int(),
    required = p_int(tags = "required")
  )

  expect_equal(
    param_set$check(list(), presence = "all"),
    "All parameters must be present. Missing parameters: optional, required"
  )
  expect_equal(
    param_set$check(list(), presence = "required"),
    "All parameters must be present. Missing parameters: required"
  )
  expect_true(param_set$check(list(), presence = "none"))

  empty_set = ps()
  expect_true(empty_set$check(list(), presence = "all"))
  expect_true(empty_set$check(list(), presence = "required"))
})

test_that("an absent dependency keeps a required parameter inactive", {
  param_set = ps(
    parent = p_lgl(),
    child = p_int(tags = "required", depends = parent == TRUE)
  )

  expect_true(param_set$check(list(), presence = "required"))
})
