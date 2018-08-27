# ParamSetFlat
th_paramset_flat_empty = ParamSetFlat$new()

th_paramset_flat_full = ParamSetFlat$new(
  id = 'th_paramset_flat_full',
  params = list(
    th_param_int,
    th_param_real,
    th_param_categorical,
    th_param_flag
  )
)

th_paramset_flat_untyped = ParamSetFlat$new(
  id = 'th_paramset_flat_untyped',
  params = list(th_param_untyped)
)

th_paramset_flat_numeric = ParamSetFlat$new(
  id = 'th_paramset_flat_numeric',
  params = list(
    th_param_int,
    th_param_real
  )
)

th_paramset_flat_trafo = ParamSetFlat$new(
  id = 'th_paramset_flat_trafo',
  params = list(
    th_param_int,
    th_param_real
  ),
  trafo = function(x, dict, tags) {
    x$th_param_int = x$th_param_int * 2L
    x$th_param_real = x$th_param_real * x$th_param_int
    return(x)
  }
)

th_paramset_flat_trafo_dictionary = ParamSetFlat$new(
  id = 'th_paramset_flat_trafo_dictionary',
  params = list(
    th_param_int,
    th_param_real
  ),
  dictionary = list(n = 100, p = 50),
  trafo = function(x, dict, tags) {
    x$th_param_int = dict$n * x$th_param_int
    x$th_param_real = x$th_param_real/dict$p
    return(x)
  }
)

th_paramset_flat_restricted = ParamSetFlat$new(
  id = 'th_paramset_flat_restricted',
  params = list(
    th_param_int,
    th_param_real,
    th_param_categorical
  ),
  restriction = quote(th_param_real > th_param_int)
)

th_paramset_flat_repeated = ParamSetFlat$new(
  id = 'th_paramset_flat_repeated',
  params = c(
    list(th_param_nat, th_param_categorical),
    repeatParam(4L, th_param_real_na)
  ),
  trafo = trafoOnRepeatedParam(fun = function(x, dict, tags) {
    xm = as.matrix(as.data.table(x))
    col_ind = seq_len(ncol(xm))
    ind.mat = sapply(dict$th_param_nat, function(z) col_ind <= z)
    ind.mat = t(ind.mat)
    xm[!ind.mat] = NA
    xm.rowsums = rowSums(xm, na.rm = TRUE)
    xm = xm / xm.rowsums
    xm[is.nan(xm)] = 1 # take care of dev by zero
    list(vector_param = lapply(seq_len(nrow(xm)), function(z) xm[z,]))
  }, repeated_param_id = "th_param_real_na", additional_params = "th_param_nat")
)
