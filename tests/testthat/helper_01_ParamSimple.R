# Objects Used for Testing
# use th. to indicate a test helper object

# ParamSimple
th.param.int = ParamInt$new(id = 'th.param.int', default = 0, lower = -10, upper = 10)
th.param.nat = ParamInt$new(id = 'th.param.nat', default = 1L, lower = 1L, upper = 10L)
th.param.real = ParamReal$new(id = 'th.param.real', default = 0, lower = -10, upper = 10)
th.param.real.na = ParamReal$new(id = 'th.param.real.na', default = 0, lower = -10, upper = 10, special.vals = NA)
th.param.factor = ParamFactor$new(id = 'th.param.factor', default = 'a', values = letters[1:3])
th.param.flag = ParamFlag$new(id = 'th.param.flag', default = FALSE)