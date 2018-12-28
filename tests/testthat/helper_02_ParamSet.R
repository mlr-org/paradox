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
  ps = ParamSet$new(
    id = 'th_paramset_trafo',
    params = list(
      th_param_int(),
      th_param_dbl()
    )
  )
  ps$trafo = function(x, param_set) {
    x$th_param_int = x$th_param_int * 2L
    x$th_param_dbl = x$th_param_dbl * x$th_param_int
    return(x)
  }
  return(ps)
}

th_paramset_repeated = function() {
  ps = ParamSet$new(
    id = 'th_paramset_repeated',
    params = c(
      list(th_param_nat(), th_param_fct())
    )
  )
  ps$add_param_set(th_param_dbl_na()$rep(4L))
}

th_paramset_deps = function() {
  ps = th_paramset_full()
  ps$add_dependency(Dependency$new(
    node_id = "th_param_fct", parent_id = "th_param_lgl",
    condition = cond_equal(TRUE)
  ))
  ps$add_dependency(Dependency$new(
    node_id = "th_param_dbl", parent_id = "th_param_fct",
    condition = cond_choice(c("a", "b"))
  ))
  return(ps)
}
