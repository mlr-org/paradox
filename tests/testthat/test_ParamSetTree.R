context("ParamTree")

test_that("test if ParamTree constructor works", {
  ps = ParamSetTree$new(ns = ParamFactor$new(id = "Model", values = c("svm", "rf")))
  pn = ParamSetTree$new(ns = ParamInt$new(id = "degree", lower = 1L, upper = 10L), depend = list(id = "kernel", val = "poly"))
  pf = ParamFactor$new(id = "Kernel", values = c("rbf", "poly", "linear"))
  pt = ParamSetTree$new(ns = pf,  depend = list(id = "model",val = "svm"))

  res = ps$handle$addMandChild(pt$handle)   
  res$addCondChild(pn$handle)
  ps$sample()
  #$addChild("rbf")
})
