th_paramset_dbl1 = function() {
  ParamSet_legacy$new(
    params = list(
      th_param_dbl()
    )
  )
}

th_paramset_full = function() {
  ParamSet_legacy$new(
    params = list(
      th_param_int(),
      th_param_dbl(),
      th_param_fct(),
      th_param_lgl()
    )
  )
}

th_paramset_untyped = function() {
  ParamSet_legacy$new(
    params = list(th_param_uty())
  )
}

th_paramset_numeric = function() {
  ParamSet_legacy$new(
    params = list(
      th_param_int(),
      th_param_dbl()
    )
  )
}

th_paramset_categorical = function() {
  ParamSet_legacy$new(
    params = list(
      th_param_fct(),
      th_param_lgl()
    )
  )
}

th_paramset_repeated = function() {
  ps = ParamSet_legacy$new(
    params = c(
      list(th_param_nat(), th_param_fct())
    )
  )
  ps$add(th_param_dbl_na()$rep(4L))
}

th_paramset_deps = function() {
  ps = th_paramset_full()
  ps$add_dep("th_param_fct", on = "th_param_lgl", CondEqual$new(TRUE))
  ps$add_dep("th_param_dbl", on = "th_param_fct", CondAnyOf$new(c("a", "b")))
  return(ps)
}
