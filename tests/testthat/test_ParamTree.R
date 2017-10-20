context("ParamTree")
test_that("test if ParamTree parse from flat", {
  ps = ParamTree$fac(
      list(node = ParamCategorical$new(id = "model", values = c("SVM", "RF"))),
      list(node = ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "model", val = "SVM")),
      list(node = ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), depend = list(id = "model", val = "SVM")),
      list(node = ParamReal$new(id = "gamma", lower = 0, upper = 100), depend = list(id = "kernel", val = "rbf"))
      )  # fac
  ps$getFirstMandChild$sample()
  ps$toStringVal()
})
