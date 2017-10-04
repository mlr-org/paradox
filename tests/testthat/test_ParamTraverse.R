context("ParamTraverse")

test_that("test if Param Traverse works", {
  print("")
  ps = ParamHandle$new(id = "Model", val = "svm")
  input = list(model = list(id = "model", val = "svm"), kernel = list(id = "kernel", val = "rbf", depend = list(val = "svm")), gamma =list(id = "gamma", val = "0.3" ,depend = list(val = "rbf")))
  ps$parseFlat(input)
  ps$toString()

})
