context("ParamVisitor-ParseFromFlat")

test_that("test if Param parse from flat works with sample", {
  ps = PHinge$new(id = "Root")
  # self$host$id == arg$depend$id is the only condition to addConchild, if depend is NULL, addMandChild
  input = list(
    list(node = ParamCategorical$new(id = "model", values = c("SVM", "RF"))),
    list(node = ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "model", fun = quote(model == "SVM"))),
    list(node = ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), depend = list(id = "model", fun = quote(model == "SVM"))),
    list(node = ParamReal$new(id = "gamma", lower = 0, upper = 100), depend = list(id = "kernel", fun = quote(kernel == "rbf"))),
    list(node = ParamInt$new(id = "n", lower = 1L, upper = 10L), depend = list(id = "kernel", fun = quote(kernel == "poly"))),
    list(node = ParamInt$new(id = "ntree", lower = 1L, upper = 10L), depend = list(id = "model", fun = quote(model == "RF")))
 )
  ps$visitor$parseFlat(input)
  ps$visitor$treeApply(identity)
  ps$visitor$treeApply(function(x) x$id)
  ps$visitor$treeApply0(identity)
  ps$asample()
  ps$sample(1)
  ps1 = ps$mand.children[["model"]]
  names(ps1)
  ps1$visitor$toFlat()
  node_list = ps1$visitor$toFlat0()
  #ps$visitor$checkValidFromFlat(list(model = "RF", ntree = 2))
  ps$getFirstMandChild$asample()
  ps$toStringVal()
})

