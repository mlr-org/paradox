context("ParamHandle")

test_that("test if ParamHandle constructor works", {
  print("")
  ps = ParamHandle$new(id = "Model", val = "SVM")
  ps$addMandChild(ParamHandle$new(id = "C", val = 0.3))
  kernel = ps$addMandChild(ParamHandle$new(id = "kernel", val = "rbf"))
  kernel$addCondChild(ParamHandle$new(id = "gamma", val = 0.6, depend ="rbf"))
  kernel$addCondChild(ParamHandle$new(id = "n", val = 3, depend ="poly"))
  ps$sample()
})
