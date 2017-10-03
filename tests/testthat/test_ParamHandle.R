context("ParamHandle")

test_that("test if ParamHandle constructor works", {
  ps = ParamHandle$new(id = "SVM", val = "SVM")
  ps$addMandChild(ParamHandle$new(id = "kernel", val = "rbf"))$addMandChild(ParamHandle$new(id = "gamma", val = 0.6))
  ps$sample()
})
