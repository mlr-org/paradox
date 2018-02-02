context("ParamTree")
test_that("test if ParamTree parse from flat", {
  ps = ParamTreeFac(
      ParamCategorical$new(id = "model", values = c("SVM", "RF")),
      makeCondTreeNode(ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "model", val = "SVM")),
      makeCondTreeNode(ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), depend = list(id = "model", val = "SVM")),
      makeCondTreeNode(ParamReal$new(id = "gamma", lower = 0, upper = 100), depend = list(id = "kernel", val = "rbf")),
      makeCondTreeNode(ParamInt$new(id = "n", lower = 1L, upper = 10L), depend = list(id = "kernel", val = "poly"))
      )
  ps$sample()
  ps$toStringVal()
})

