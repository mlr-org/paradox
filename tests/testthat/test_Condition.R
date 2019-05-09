context("Condition")

test_that("Condition", {
  cond = CondEqual$new("a")
  y = cond$test(c("a", "b", "c", NA_character_))
  expect_equal(y, c(TRUE, FALSE, FALSE, FALSE))

  cond = CondAnyOf$new(c("a", "b"))
  y = cond$test(c("a", "b", "c", NA_character_))
  expect_equal(y, c(TRUE, TRUE, FALSE, FALSE))
})
