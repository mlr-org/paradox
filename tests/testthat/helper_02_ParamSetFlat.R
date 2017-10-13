# ParamSetFlat
th.paramset.empty = ParamSetFlat$new()

th.paramset.flat.full = ParamSetFlat$new(
  id = 'th.paramset.flat.full',
  params = list(
    th.param.int,
    th.param.real,
    th.param.categorical,
    th.param.flag
  )
)

th.paramset.flat.untyped = ParamSetFlat$new(
  id = 'th.paramset.flat.untyped',
  params = list(th.param.untyped)
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
  trafo = function(x, dict, tags) {
    x$th.param.int = x$th.param.int * 2L
    x$th.param.real = x$th.param.real * x$th.param.int
    return(x)
  }
)

th.paramset.flat.trafo.dictionary = ParamSetFlat$new(
  id = 'th.paramset.flat.trafo.dictionary',
  params = list(
    th.param.int,
    th.param.real
  ),
  dictionary = list(n = 100, p = 50),
  trafo = function(x, dict, tags) {
    x$th.param.int = dict$n * x$th.param.int
    x$th.param.real = x$th.param.real/dict$p
  }
)

th.paramset.flat.restricted = ParamSetFlat$new(
  id = 'th.paramset.flat.restricted',
  params = list(
    th.param.int,
    th.param.real
  ),
  restriction = quote(th.param.real > th.param.int)
)

th.paramset.flat.collection = ParamSetFlat$new(
  id = 'th.paramset.flat.collection',
  params = c(
    list(th.param.nat, th.param.categorical),
    createCollectionParamList(10L, th.param.real.na)
  ),
  trafo = collectionHelper(fun = function(x, dict, tags) {
    xm = as.matrix(as.data.table(x))
    col.ind = seq_len(ncol(xm))
    ind.mat = sapply(dict$th.param.nat, function(z) col.ind <= z)
    ind.mat = t(ind.mat)
    xm[!ind.mat] = NA
    xm = xm / rowSums(xm, na.rm = TRUE)
    list(vector.param = lapply(seq_len(nrow(xm)), function(z) xm[z,]))
  }, collection.param.id = "th.param.real.na", additional.params = "th.param.nat")
)
