#FIXME: export
assert_paramset = function(param_set, no_untyped = FALSE) {
  assert_r6(param_set, "ParamSet")
  if (no_untyped && ("ParamUty" %in% param_set$pclasses))
    stop("ParamSet contains untyped params!")
}
