# Objects Used for Testing
# use th. to indicate a test helper object

# ParamSimple
th.param.int = ParamInt$new(id = 'th.param.int', default = 0, lower = -10, upper = 10)
th.param.real = ParamReal$new(id = 'th.param.real', default = 0, lower = -10, upper = 10)
th.param.categorical = ParamCategorical$new(id = 'th.param.categorical', default = 'a', values = letters[1:3])
th.param.flag = ParamFlag$new(id = 'th.param.flag', default = FALSE)
