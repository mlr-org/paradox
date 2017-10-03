context("ParamSetFlat")

test_that("test if ParamSetFlat constructor works", {
  params = list(
    ParamInt$new('x', lower = 0, upper = 10),
    ParamReal$new('y', lower = -1, upper = 5, default = 2),
    ParamFactor$new('c', values = c('a','b'))
  )
  ps = ParamSetFlat$new(id = "flatParamSetEx", params = params)
  ps$sample()
  ps$toString()
})

