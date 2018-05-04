context("ParamTree")
test_that("test if ParamFac parse from flat", {
  ps = ParamTreeFac(
      ParamCategorical$new(id = "model", values = c("SVM", "RF")),
      makeCondTreeNode(ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "model", fun = quote(model == "SVM"))),
      makeCondTreeNode(ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), depend = list(id = "model", fun = quote(model == "SVM"))),
      makeCondTreeNode(ParamReal$new(id = "gamma", lower = 0, upper = 100), depend = list(id = "kernel", fun = quote(kernel == "rbf"))),
      makeCondTreeNode(ParamInt$new(id = "n", lower = 1L, upper = 10L), depend = list(id = "kernel", fun = quote(kernel == "poly")))
      )
  ps$asample()
  ps$toStringVal()
  ps$getList()
})

test_that("test if two ParamTree works", {
  pt = ParamSetTree$new("pt1",
      ParamCategorical$new(id = "model", values = c("SVM", "RF")),
      makeCondTreeNode(ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "model", fun = quote(model == "SVM"))),
     makeCondTreeNode(ParamInt$new(id = "n_tree", lower = 1L, upper = 10L), depend = list(id = "model", fun = quote(model == "RF")))
      )
  pt$asample()
  pt$toStringVal()
  pt2 = ParamSetTree$new("pt2",
      ParamCategorical$new(id = "activation", values = c("sigmoid", "tanh")),
      ParamReal$new(id = "regu", lower = 0, upper = 100),
      ParamInt$new(id = "n", lower = 1L, upper = 100L)
      )
  pt2$asample()
  pt2$toStringVal()
  pt$setChild(pt2)
  pt$asample()
  pt$toStringVal()
})

test_that("test if two ParamTree works", {
  ps = recursiveParaFac(2,
      ParamCategorical$new(id = "model", values = c("SVM", "RF")),
      makeCondTreeNode(ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "model", fun = quote(model == "SVM"))),
     makeCondTreeNode(ParamInt$new(id = "n_tree", lower = 1L, upper = 10L), depend = list(id = "model", fun = quote(model == "RF")))
      )
  ps$asample()
  ps$toStringVal()
})

