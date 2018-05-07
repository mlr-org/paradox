context("ParamTree")
test_that("test if ParamFac parse from flat", {
  ps = ParamTreeFac(id = "test",
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

test_that("ParamSetTree constructor works", {
   pst = ParamSetTree$new("test",
       ParamCategorical$new(id = "model", values = c("SVM", "RF")),
       addDep(ParamReal$new(id = "C", lower = 0, upper = 100), 
         did = "model", expr = quote(model == "SVM")), # did here means dependant id
       addDep(ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), 
         did = "model", expr = quote(model == "SVM")),
       addDep(ParamReal$new(id = "gamma", lower = 0, upper = 101), 
         did = "kernel", expr = quote(kernel == "rbf")),
       addDep(ParamInt$new(id = "n", lower = 1L, upper = 10L), 
         did = "kernel", expr = quote(kernel == "poly")),
       addDep(ParamInt$new(id = "n_tree", lower = 1L, upper = 10L), 
         did = "model", expr = quote(model == "RF"))
       )
   pst$sample(1L)
   pst$getFlatList()
   pst$toStringVal()
   pst$sample(10L)
})

test_that("recursive para works", {
  ps = recursiveParaFac(nr = 2,
      ParamInt$new(id = "layer_dense.units", lower = 2L, upper = 1000L),
      ParamReal$new(id = "kernel_regularizer", lower = 0, upper = 3.0), 
      ParamReal$new(id = "bias_regularizer", lower = 0, upper = 3.0), 
      ParamCategorical$new(id = "reg_type", values = c("regularizer_l1", "regularizer_l2")),
      ParamCategorical$new(id = "activation_fun", values = c("sigmoid", "tanh", "linear")))
  ps$sample(3L)
})

 test_that("test conditional params works for conditional ParamTree", {
   ps = recursiveParaFac(nr = 2,
       ParamCategorical$new(id = "model", values = c("SVM", "RF")),
       makeCondTreeNode(ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "model", fun = quote(model == "SVM"))),
      makeCondTreeNode(ParamInt$new(id = "n_tree", lower = 1L, upper = 10L), depend = list(id = "model", fun = quote(model == "RF")))
       )
   ps$asample()
   ps$toStringVal()
 })

