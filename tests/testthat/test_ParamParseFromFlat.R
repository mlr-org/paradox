context("ParseFromFlat")

test_that("test if Param parse from flat works", {
  ps = ParamHandle$new(id = "Root", val = NULL)
  input = list(
    model = list(id = "model", val = "svm"),
    c = list(id = "C", val = 3, depend = list(id = "model", val = "svm")),
    kernel = list(id = "kernel", val = "rbf", depend = list(id = "model", val = "svm")),
    gamma = list(id = "gamma", val = "0.3", depend = list(id = "kernel", val = "rbf"))
    )
  ps$visitor$parseFlat(input)
  ps$toString()
})


test_that("test if Param parse from flat works with sample", {
  ps = ParamHandle$new(id = "Root", val = NULL)
  input = list(
    model = list(id = "model", val = "svm"),
    c = list(id = "C", val = 3, depend = list(id = "model", val = "svm")),
    kernel = list(id = "kernel", val = "rbf", depend = list(id = "model", val = "svm")),
    gamma = list(id = "gamma", val = "0.3", depend = list(id = "kernel", val = "rbf"))
  )
  ps$visitor$parseFlat(input)
  ps$toString()
  ps$sample()
})
