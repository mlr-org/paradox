# ParamSetFlat
th.paramset.flat.full = ParamSetFlat$new(
  id = 'th.paramset.flat.full',
  params = list(
    th.param.int,
    th.param.real,
    th.param.factor,
    th.param.logical,
    th.param.int.trafo,
    th.param.real.trafo,
    th.param.factor.trafo
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
    th.param.int.trafo,
    th.param.real.trafo
  )
)
th.paramset.flat.numeric.expr = ParamSetFlat$new(
  id = 'th.paramset.flat.numeric.expr',
  params = list(
    th.param.int.expr,
    th.param.real.expr
  ),
  dictionary = list(n = 100, p = 50)
)

th.paramset.flat.numeric.trafo.expr = ParamSetFlat$new(
  id = 'th.paramset.flat.numeric.trafo.expr',
  params = list(
    th.param.int.trafo.expr,
    th.param.real.trafo.expr
  ),
  dictionary = list(n = 100, p = 50)
)