context("Condition")

test_that("Condition", {
  cond = CondEqual("a")
  y = condition_test(cond, c("a", "b", "c", NA_character_))
  expect_equal(y, c(TRUE, FALSE, FALSE, FALSE))
  expect_output(print(cond), fixed = "CondEqual")

  expect_error(CondEqual(c("a","b")), "Assertion on 'rhs' failed")
  expect_error(CondEqual(NA), "Assertion on 'rhs' failed")

  cond = CondAnyOf(c("a", "b"))
  y = condition_test(cond, c("a", "b", "c", NA_character_))
  expect_equal(y, c(TRUE, TRUE, FALSE, FALSE))
  expect_output(print(cond), fixed = "CondAnyOf")

  expect_error(CondAnyOf(list("a","b")), "Assertion on 'rhs' failed")
  expect_error(CondAnyOf(c("a", "b", NA_character_)), "Assertion on 'rhs' failed")
  expect_error(CondAnyOf(character()), "Assertion on 'rhs' failed")
})
