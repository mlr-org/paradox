context("ParamTree")
test_that("test if ParamTree parse from flat", {
  ps = ParamTree$fac(
      ParamTree$dn(node = ParamCategorical$new(id = "model", values = c("SVM", "RF"))),
      ParamTree$dn(node = ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "model", val = "SVM")),
      ParamTree$dn(node = ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), depend = list(id = "model", val = "SVM")),
      ParamTree$dn(node = ParamReal$new(id = "gamma", lower = 0, upper = 100), depend = list(id = "kernel", val = "rbf")),
      ParamTree$dn(node = ParamInt$new(id = "n", lower = 1L, upper = 10L), depend = list(id = "kernel", val = "poly"))
      )
  ps$getFirstMandChild$sample()
  ps$toStringVal()
})

