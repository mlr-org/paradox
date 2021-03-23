
# compare ParamSets, but ignore Param ID

expect_equal_ps = function(a, b) {
  assert_class(a, "ParamSet")
  assert_class(b, "ParamSet")
  a = a$clone(deep = TRUE)
  a$params
  a$tags
  b = b$clone(deep = TRUE)
  b$params
  b$tags
  a$.__enclos_env__$private$.params_unid = lapply(a$.__enclos_env__$private$.params_unid, function(x) {
    x = x$clone(deep = TRUE)
    x$.__enclos_env__$private$.id = ""
    x
  })
  b$.__enclos_env__$private$.params_unid = lapply(b$.__enclos_env__$private$.params_unid, function(x) {
    x = x$clone(deep = TRUE)
    x$.__enclos_env__$private$.id = ""
    x
  })
  a = as.list(a)
  b = as.list(b)

  expect_equal(a, b)
}

