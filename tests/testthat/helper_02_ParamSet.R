# ParamSet
th_paramset_empty = function() ParamSet$new(id = "th_paramset_empty")

th_paramset_full = function() {
  ParamSet$new(
    id = 'th_paramset_full',
    params = list(
      th_param_int(),
      th_param_dbl(),
      th_param_fct(),
      th_param_lgl()
      )
    )
}

th_paramset_untyped = function() {
  ParamSet$new(
    id = 'th_paramset_untyped',
    params = list(th_param_uty())
    )
}

th_paramset_numeric = function() {
  ParamSet$new(
    id = 'th_paramset_numeric',
    params = list(
      th_param_int(),
      th_param_dbl()
      )
    )
}

th_paramset_trafo = function() {
  ParamSet$new(
    id = 'th_paramset_trafo',
    params = list(
      th_param_int(),
      th_param_dbl()
      ),
    trafo = function(x, tags) {
      x$th_param_int = x$th_param_int * 2L
      x$th_param_dbl = x$th_param_dbl * x$th_param_int
      return(x)
    }
    )
}

th_paramset_repeated = function() {
  ParamSet$new(
    id = 'th_paramset_repeated',
    params = c(
      list(th_param_nat(), th_param_fct()),
      repeatParam(4L, th_param_dbl_na())
      )
    )
  }

