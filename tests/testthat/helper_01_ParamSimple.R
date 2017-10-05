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

# ParamSimple with call
th.param.int.varpar = ParamInt$new(id = 'th.param.int.varpar', default = call(sqrt(p/2)), lower = call(sqrt(p/4)), upper = call(sqrt(p*0.75)))
th.param.real.varpar = ParamInt$new(id = 'th.param.real.varpar', default = call(n/7), lower = call(n/10), upper = call(n/5))

# ParamSimple wiht expressions and trafos
th.param.int.trafo.varpar = ParamInt$new(id = 'th.param.int.trafo.varpar', lower = call(sqrt(p/4)), upper = call(sqrt(p*0.75)), trafo = trafo.2exp)
th.param.real.trafo.varpar = ParamReal$new(id = 'th.param.real.trafo.varpar', lower = call(n/10), upper = call(n/5), trafo = trafo.exp)