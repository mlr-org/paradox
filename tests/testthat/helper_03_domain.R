
# compare ParamSets, but ignore Param ID

expect_equal_ps = function(a, b) {
  assert_class(a, "ParamSet")
  assert_class(b, "ParamSet")

  normalize_ids = function(original) {
    acl = original$clone(deep = TRUE)
    acp = acl$.__enclos_env__$private
    acp$.params$id = sprintf("x%s", seq_len(original$length))
    names(acp$.values) = sprintf("x%s", match(names(original$values), original$ids()))
    acp$.tags = copy(acp$.tags)[, id := sprintf("x%s", match(id, original$ids()))]
    acp$.trafos = copy(acp$.trafos)[, id := sprintf("x%s", match(id, original$ids()))]
    acp$.deps[, id := sprintf("x%s", match(id, original$ids()))]
    setindexv(acp$.params, NULL)
    setindexv(acp$.trafos, NULL)
    setindexv(acp$.tags, NULL)
    acl
  }

  expect_equal(normalize_ids(a), normalize_ids(b))
}

