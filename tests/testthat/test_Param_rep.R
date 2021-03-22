context("Repeated params")

test_that("rep params work", {
  p = ParamDbl$new(id = "x", lower = 1, upper = 3)
  ps = p$rep(2L)
  expect_r6(ps, "ParamSet")
  expect_equal(ps$length, 2L)
  expect_subset(ps$class, "ParamDbl")
  expect_equal(ps$ids(), c("x_rep_1", "x_rep_2"))
  expect_subset(ps$lower, 1)
  expect_subset(ps$upper, 3)

  p = ParamFct$new(id = "kk", levels = c("a", "b"))
  ps = p$rep(3L)
  expect_r6(ps, "ParamSet")
  expect_equal(ps$length, 3L)
  expect_subset(ps$class, "ParamFct")
  expect_equal(ps$ids(), c("kk_rep_1", "kk_rep_2", "kk_rep_3"))
  for (id in ps$ids()) {
    expect_equal(ps$params[[id]]$levels, c("a", "b"))
  }
})

