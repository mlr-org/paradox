context("ParseFromFlat")

test_that("test if Param parse from flat works with sample", {
  ps = ParamHandle$new(id = "Root")
  # self$host$id == arg$depend$id is the only condition to addConchild, if depend is NULL, addMandChild
  input = list(
    model = list(node = ParamCategorical$new(id = "model", values = c("SVM", "RF"))),
    c = list(node = ParamReal$new(id = "C", lower = 0, upper = 100), val = 3, depend = list(id = "model", val = "SVM")),
    kernel = list(node = ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), depend = list(id = "model", val = "SVM")),
    gamma = list(node = ParamReal$new(id = "gamma", lower = 0, upper = 100), depend = list(id = "kernel", val = "rbf"))
 ) 
  ps$visitor$parseFlat(input)
  ps$getFirstMandChild$sample()
  ps$toStringVal()
})



test_that("test if Param parse from flat works with ParamFlat", {
  ps = ParamHandle$new(id = "Root", val = NULL)
  # list(id, ParamNode, val, depend)
  mnames = names(th.paramset.flat.full$params)
  lapply(mnames, function(x) list(id = x, th.paramset.flat.full$params[[x]]))
  #ps$visitor$parseFlat(input)
  #ps$sample()
})



