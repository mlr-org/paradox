context("ParamHandle")

test_that("test if ParamHandle constructor works", {
  ps = ParamHandle$new(id = "Model", val = "SVM")
  ps$addMandChild(ParamHandle$new(id = "C", val = 0.3))
  kernel = ps$addMandChild(ParamHandle$new(id = "kernel", val = "rbf"))
  kernel$addCondChild(ParamHandle$new(id = "gamma", val = 0.6, depend = list(id = "kernel", val = "rbf")))
  kernel$addCondChild(ParamHandle$new(id = "n", val = 3L, depend = list(id = "kernel", val = "poly")))
  ps$toStringVal()
})


test_that("test if ParamHandle constructor works with SimpleParamNode", {
  ps = ParamHandle$new(id = "Model", node = ParamFactor$new(id = "Model", values = c("SVM", "RF")), val = "SVM")
  ps$addMandChild(ParamHandle$new(id = "C", node = ParamReal$new(id = "C", lower = 0, upper = 100), val = 0.3))
  kernel = ps$addMandChild(ParamHandle$new(id = "kernel", node = ParamFactor$new(id = "kernel", values = c("rbf", "poly")), val = "rbf"))
  kernel$addCondChild(ParamHandle$new(id = "gamma", ParamReal$new(id = "C", lower = 0, upper = 100), val = 0.6, depend = list(id = "kernel", val = "rbf")))
  kernel$addCondChild(ParamHandle$new(id = "n", node = ParamInt$new(id = "n", lower = 1L, upper = 10L), val = 3L, depend = list(id = "kernel", val = "poly")))
  ps$toStringVal()
})

test_that("test if ParamHandle sampling works", {
  ps = ParamHandle$new(id = "Model", node = ParamFactor$new(id = "Model", values = c("SVM", "RF")), val = "SVM")
  ps$addMandChild(ParamHandle$new(id = "C", node = ParamReal$new(id = "C", lower = 0, upper = 100), val = 0.3))
  kernel = ps$addMandChild(ParamHandle$new(id = "kernel", node = ParamFactor$new(id = "kernel", values = c("rbf", "poly")), val = "rbf"))
  kernel$addCondChild(ParamHandle$new(id = "gamma", ParamReal$new(id = "C", lower = 0, upper = 100), val = 0.6, depend = list(id = "kernel", val = "rbf")))
  kernel$addCondChild(ParamHandle$new(id = "n", node = ParamInt$new(id = "n", lower = 1L, upper = 10L), val = 3L, depend = list(id = "kernel", val = "poly")))
  ps$toStringVal()
  ps$sample()
})
