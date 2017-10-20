context("ParamVisitor-ParseFromFlat")

test_that("test if Param parse from flat works with sample", {
  ps = ParamHandle$new(id = "Root")
  # self$host$id == arg$depend$id is the only condition to addConchild, if depend is NULL, addMandChild
  input = list(
    list(node = ParamCategorical$new(id = "model", values = c("SVM", "RF"))),
    list(node = ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "model", val = "SVM")),
    list(node = ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), depend = list(id = "model", val = "SVM")),
    list(node = ParamReal$new(id = "gamma", lower = 0, upper = 100), depend = list(id = "kernel", val = "rbf")),
    list(node = ParamInt$new(id = "n", lower = 1L, upper = 10L), depend = list(id = "kernel", val = "poly")),
    list(node = ParamInt$new(id = "ntree", lower = 1L, upper = 10L), depend = list(id = "model", val = "RF"))
 )
  ps$visitor$parseFlat(input)
  ps$getFirstMandChild$sample()
  ps$toStringVal()
})

