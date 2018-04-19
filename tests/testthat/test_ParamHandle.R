context("ParamHandle")

test_that("ParamHandle constructor, addChild, toStringVal", {
  # this is an ill example,do not use in client code!
  ps = ParamHandle$new(id = "Model", val = "SVM")
  ps$addMandChild(ParamHandle$new(id = "C", val = 0.3))
  kernel = ps$addMandChild(ParamHandle$new(id = "kernel", val = "rbf"))
  kernel$addCondChild(ParamHandle$new(id = "gamma", val = 0.6, depend = list(id = "kernel", val = "rbf")))
  kernel$addCondChild(ParamHandle$new(id = "n", val = 3L, depend = list(id = "kernel", val = "poly")))
  ps$toStringVal()
})

test_that("ParamHandle works with expression", {
  # this is an ill example,do not use in client code!
  ps = ParamHandle$new(id = "Model", val = "SVM")
  ps$addMandChild(ParamHandle$new(id = "C", val = 0.3))
  kernel = ps$addMandChild(ParamHandle$new(id = "kernel", val = "rbf"))
  kernel$addCondChild(ParamHandle$new(id = "gamma", val = 0.6, depend = list(id = "kernel", fun = quote(kernel == "rbf"))))
  kernel$addCondChild(ParamHandle$new(id = "n", val = 3L, depend = list(id = "kernel", fun = quote(kernel == "poly"))))
  ps$toStringVal()
})



test_that("test if ParamHandle constructor works with SimpleParamNode", {
  ps = ParamHandle$new(node = ParamCategorical$new(id = "Model", values = c("SVM", "RF")))
  ps$setRoot(ps)
  temp = ParamHandle$new(node = ParamInt$new(id = "n_tree", lower = 1L, upper = 10L), depend = list(id = "Model", val = "RF"))
  ntree = ps$addCondChild(temp)
  temp = ParamHandle$new(node = ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "Model", val = "SVM"))
  c = ps$addCondChild(temp)
  temp = ParamHandle$new(node = ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), depend = list(id = "Model", val = "SVM"))
  kernel = ps$addCondChild(temp)
  temp = ParamHandle$new(node = ParamReal$new(id = "gamma", lower = 0, upper = 100), depend = list(id = "kernel", val = "rbf"))
  gamma = kernel$addCondChild(temp)
  temp = ParamHandle$new(node = ParamInt$new(id = "n", lower = 1L, upper = 10L), depend = list(id = "kernel", val = "poly"))
  poly = kernel$addCondChild(temp)
  list.flat = ps$visitor$toFlat()
  # ps$toStringVal()
  ps$sample()
  ps$toStringVal()  # after sampling, the string might be different
  list.flat = ps$visitor$toFlat()
  poly$getRoot$sample() # use the root to sample
  ps$toStringVal()
})



test_that("ParamHandle toStringVal behaves normal without value", {
  ps = ParamHandle$new(id = "Model", node = ParamCategorical$new(id = "Model", values = c("SVM", "RF")), val = "SVM")
  ps$addCondChild(ParamHandle$new(node = ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "Model", val = "SVM")))
  kernel = ps$addCondChild(ParamHandle$new(node = ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), depend = list(id = "Model", val = "SVM")))
  kernel$addCondChild(ParamHandle$new(node = ParamReal$new(id = "gamma", lower = 0, upper = 100), depend = list(id = "kernel", val = "rbf")))
  kernel$addCondChild(ParamHandle$new(node = ParamInt$new(id = "n", lower = 1L, upper = 10L), depend = list(id = "kernel", val = "poly")))
  ps$sample()
  ps$toStringVal()
})


