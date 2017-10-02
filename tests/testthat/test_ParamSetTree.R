context("ParamTree")

test_that("test if ParamTree constructor works", {
  ps = ParamSetTree$new("SVM")
  res = ps$addMandChild(ParamSetTree$new("Kernel"))   #$addChild("rbf")
})



