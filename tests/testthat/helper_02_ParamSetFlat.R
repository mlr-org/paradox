# ParamSetFlat
th.paramset.flat.full = ParamSetFlat$new(
  id = 'th.paramset.flat.full',
  params = list(
    th.param.int,
    th.param.real,
    th.param.factor,
    th.param.flag
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
  }
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
    x$th.param.real = x$th.param.real/dict$p
  }
)

th.paramset.flat.restricted = ParamSetFlat$new(
  id = 'th.param.flat.restricted',
  params = list(
    th.param.int,
    th.param.real
  ),
  restriction = quote(th.param.real > th.param.int)
)

th.paramset.flat.collection = ParamSetFlat$new(
  id = 'th.param.flat.collection',
  params = c(
    list(th.param.nat, th.param.factor),
    createCollectionParam(10L, th.param.real.na)
  ),
  trafo = function(x, dict, tags) {
    browser()
    ind = names(which(BBmisc::vlapply(tags, function(x) "th.param.real.na.collection" %in% x)))
    res = .mapply(function(...) {
      x1 = list(...)
      ind.active = ind[seq_len(x1$th.param.nat)]
      ind.inactive = setdiff(ind, ind.active)
      suma = do.call(sum, x1[ind.active])
      x1[ind.active] = lapply(x1[ind.active], function(x) x/suma)
      x1[ind.inactive] = NA
      x1$th.param.nat = NULL
      return(x1)
    }, x, list())
    as.list(rbindlist(res))
  }
)