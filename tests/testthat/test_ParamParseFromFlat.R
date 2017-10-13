context("ParseFromFlat")

test_that("test if Param parse from flat works", {
  # in this simple example, the node is not defined, so the sample function here does not work
  ps = ParamHandle$new(id = "Root", val = NULL)
  input = list(
    model = list(id = "model", val = "svm"),
    c = list(id = "C", val = 3, depend = list(id = "model", val = "svm")),
    kernel = list(id = "kernel", val = "rbf", depend = list(id = "model", val = "svm")),
    gamma = list(id = "gamma", val = "0.3", depend = list(id = "kernel", val = "rbf"))
    )
  ps$visitor$parseFlat(input)
  ps$toStringVal()
})


test_that("test if Param parse from flat works with sample", {
  ps = ParamHandle$new(id = "Root", val = NULL)
  input = list(
    model = list(id = "model", ParamCategorical$new(id = "model", values = c("SVM", "RF")), val = "svm"),
    c = list(id = "C", node = ParamReal$new(id = "C", lower = 0, upper = 100), val = 3, depend = list(id = "model", val = "svm")),
    kernel = list(id = "kernel", node = ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), val = "rbf", depend = list(id = "model", val = "svm")),
    gamma = list(id = "gamma", ParamReal$new(id = "gamma", lower = 0, upper = 100), val = 0.3, depend = list(id = "kernel", val = "rbf"))
 ) 
  ps$visitor$parseFlat(input)
  ps$sample()
})
