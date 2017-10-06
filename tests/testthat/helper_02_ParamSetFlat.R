# ParamSetFlat
th.paramset.flat.full = ParamSetFlat$new(
  id = 'th.paramset.flat.full',
  params = list(
    th.param.int,
    th.param.real,
    th.param.factor,
    th.param.logical
  )
)

th.paramset.flat.numeric = ParamSetFlat$new(
  id = 'th.paramset.flat.numeric',
  params = list(
    th.param.int,
    th.param.real
  )
)

th.paramset.flat.trafo = ParamSetFlat$new(
  id = 'th.paramset.flat.trafo',
  params = list(
    th.param.int,
    th.param.real
  ),
  trafo = function(x, dict) {
    x$th.param.int = x$th.param.int * 2L
    x$th.param.real = x$th.param.real * x$th.param.int
    return(x)
  )
)

th.paramset.flat.trafo.dictionary = ParamSetFlat$new(
  id = 'th.paramset.flat.trafo.dictionary',
  params = list(
    th.param.int,
    th.param.real
  ),
  dictionary = list(n = 100, p = 50),
  trafo = function(x, dict) {
    x$th.param.int = dict$n * x$th.param.int
    x$th.param.real = x$th.param.real/dict$p)
  }
)

# th.paramset.flat.numeric.trafo.varpar = ParamSetFlat$new(
#   id = 'th.paramset.flat.numeric.trafo.varpar',
#   params = list(
#     th.param.int.trafo.varpar,
#     th.param.real.trafo.varpar
#   ),
#   dictionary = list(n = 100, p = 50)
# )