context("Repeated params")

test_that("rep params work", {
  p = ParamDbl$new(id = "x", lower = 1, upper = 3)
  ps = ps_replicate(p, 2)
  expect_r6(ps, "ParamSet")
  expect_equal(ps$length, 2L)
  expect_subset(ps$class, "ParamDbl")
  expect_equal(ps$ids(), c("rep1.x", "rep2.x"))
  expect_subset(ps$lower, 1)
  expect_subset(ps$upper, 3)

  p = ParamFct$new(id = "kk", levels = c("a", "b"))
  ps = ps_replicate(p, 3)
  expect_r6(ps, "ParamSet")
  expect_equal(ps$length, 3L)
  expect_subset(ps$class, "ParamFct")
  expect_equal(ps$ids(), c("rep1.kk", "rep2.kk", "rep3.kk"))
  for (id in ps$ids()) {
    expect_equal(ps$levels[[id]], c("a", "b"))
  }
})

