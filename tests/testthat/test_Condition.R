context("Condition")

test_that("Condition", {
  cond = CondEqual$new("a")
  y = cond$test(c("a", "b", "c", NA_character_))
  expect_equal(y, c(TRUE, FALSE, FALSE, FALSE))
  expect_output(print(cond), fixed = "CondEqual")

  expect_error(CondEqual$new(c("a","b")), "Assertion on 'rhs' failed")
  expect_error(CondEqual$new(NA), "Assertion on 'rhs' failed")

  cond = CondAnyOf$new(c("a", "b"))
  y = cond$test(c("a", "b", "c", NA_character_))
  expect_equal(y, c(TRUE, TRUE, FALSE, FALSE))
  expect_output(print(cond), fixed = "CondAnyOf")

  expect_error(CondAnyOf$new(list("a","b")), "Assertion on 'rhs' failed")
  expect_error(CondAnyOf$new(c("a", "b", NA_character_)), "Assertion on 'rhs' failed")
  expect_error(CondAnyOf$new(character()), "Assertion on 'rhs' failed")
})
