th_paramset_dbl1 = function() {
  th_param_dbl()
}

th_paramset_full = function() {
  c(
    th_param_int(),
    th_param_dbl(),
    th_param_fct(),
    th_param_lgl()
  )
}

th_paramset_untyped = function() {
  th_param_uty()
}

th_paramset_numeric = function() {
  c(
    th_param_int(),
    th_param_dbl()
  )
}

th_paramset_categorical = function() {
  c(
    th_param_fct(),
    th_param_lgl()
  )
}

th_paramset_repeated = function() {
  c(
    th_param_nat(),
    th_param_fct(),
    ps_replicate(th_param_dbl_na(), 4)
  )
}

th_paramset_deps = function() {
  ps = th_paramset_full()
  ps$add_dep("th_param_fct", on = "th_param_lgl", CondEqual(TRUE))
  ps$add_dep("th_param_dbl", on = "th_param_fct", CondAnyOf(c("a", "b")))
  ps
}
