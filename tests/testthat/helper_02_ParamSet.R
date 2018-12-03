# ParamSet
th_paramset_empty = function() ParamSet$new(id = "th_paramset_empty")

th_paramset_full = function() {
  ParamSet$new(
    id = 'th_paramset_full',
    params = list(
      th_param_int(),
      th_param_real(),
      th_param_categ(),
      th_param_bool()
      )
    )
}

th_paramset_untyped = function() {
  ParamSet$new(
    id = 'th_paramset_untyped',
    params = list(th_param_untyped())
    )
}

th_paramset_numeric = function() {
  ParamSet$new(
    id = 'th_paramset_numeric',
    params = list(
      th_param_int(),
      th_param_real()
      )
    )
}

th_paramset_trafo = function() {
  ParamSet$new(
    id = 'th_paramset_trafo',
    params = list(
      th_param_int(),
      th_param_real()
      ),
    trafo = function(x, tags) {
      x$th_param_int = x$th_param_int * 2L
      x$th_param_real = x$th_param_real * x$th_param_int
      return(x)
    }
    )
}


th_paramset_restricted = function() {
  ParamSet$new(
    id = 'th_paramset_restricted',
    params = list(
      th_param_int(),
      th_param_real(),
      th_param_categ()
      ),
    restriction = quote(th_param_real > th_param_int)
    )
}

th_paramset_repeated = function() {
  ParamSet$new(
    id = 'th_paramset_repeated',
    params = c(
      list(th_param_nat(), th_param_categ()),
      repeatParam(4L, th_param_real_na())
      )
    #   trafo = trafo_on_repeated_param(fun = function(x, tags) {
#     xm = as.matrix(as.data.table(x))
#     col_ind = seq_len(ncol(xm))
#     ind_mat = sapply(dict$th_param_nat, function(z) col_ind <= z)
#     ind_mat = t(ind_mat)
#     xm[!ind_mat] = NA
#     xm_rowsums = rowSums(xm, na.rm = TRUE)
#     xm = xm / xm_rowsums
#     xm[is.nan(xm)] = 1 # take care of dev by zero
#     list(vector_param = lapply(seq_len(nrow(xm)), function(z) xm[z,]))
#   }, repeated_param_id = "th_param_real_na")
    )
  }

