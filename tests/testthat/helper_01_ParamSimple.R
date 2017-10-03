# Objects Used for Testing
# use th. to indicate a test helper object

# ParamSimple
th.param.int = ParamInt$new(id = 'th.param.int', default = 0, lower = -10, upper = 10)
th.param.real = ParamReal$new(id = 'th.param.real', default = 0, lower = -10, upper = 10)
th.param.factor = ParamFactor$new(id = 'th.param.factor', default = 'a', values = letters[1:3])
th.param.logical = ParamLogical$new(id = 'th.param.logical', default = FALSE)

# ParamSimple with trafos
th.param.int.trafo = ParamInt$new(id = 'th.param.int.trafo', default = 0, lower = 0, upper = 4, trafo = trafo.2exp)
th.param.real.trafo = ParamReal$new(id = 'th.param.real.trafo', default = 0, lower = -10, upper = 10, trafo = trafo.exp)
th.param.factor.trafo = ParamFactor$new(id = 'th.param.factor.trafo', default = 'mean', values = c('mean', 'median'), trafo = trafo.get)

# ParamSimple with expressions
th.param.int.expr = ParamInt$new(id = 'th.param.int.expr', default = expression(sqrt(p/2)), lower = expression(sqrt(p/4)), upper = expression(sqrt(p*0.75)))
th.param.real.expr = ParamInt$new(id = 'th.param.real.expr', default = expression(n/7), lower = expression(n/10), upper = expression(n/5))

# ParamSimple wiht expressions and trafos
th.param.int.trafo.expr = ParamInt$new(id = 'th.param.int.trafo.expr', lower = expression(sqrt(p/4)), upper = expression(sqrt(p*0.75)), trafo = trafo.2exp)
th.param.real.trafo.expr = ParamReal$new(id = 'th.param.real.trafo.expr', lower = expression(n/10), upper = expression(n/5), trafo = trafo.exp)