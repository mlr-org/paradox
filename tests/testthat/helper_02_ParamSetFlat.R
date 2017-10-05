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
th.paramset.flat.numeric.varpar = ParamSetFlat$new(
  id = 'th.paramset.flat.numeric.varpar',
  params = list(
    th.param.int.varpar,
    th.param.real.varpar
  ),
  dictionary = list(n = 100, p = 50)
)

th.paramset.flat.numeric.trafo.varpar = ParamSetFlat$new(
  id = 'th.paramset.flat.numeric.trafo.varpar',
  params = list(
    th.param.int.trafo.varpar,
    th.param.real.trafo.varpar
  ),
  dictionary = list(n = 100, p = 50)
)