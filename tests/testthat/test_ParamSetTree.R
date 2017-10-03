context("ParamTree")

test_that("test if ParamTree constructor works", {
  ps = ParamSetTree$new("SVM")
  res = ps$handle$addMandChild(ParamSetTree$new("Kernel"))   #$addChild("rbf")
})
