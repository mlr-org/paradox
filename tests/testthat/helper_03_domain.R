
# compare ParamSets, but ignore Param ID

expect_equal_ps = function(a, b) {
  assert_class(a, "ParamSet")
  assert_class(b, "ParamSet")

  normalize_ids = function(original) {
    ids = original$ids()
    params = data.table::copy(original$params)
    params$id = sprintf("x%s", seq_along(ids))
    params$.requirements = lapply(
      params$.requirements,
      function(requirement) {
        if (is.null(requirement)) return(NULL)
        list(match(requirement[[1L]], ids), requirement[[2L]])
      }
    )
    dependencies = original$deps
    list(
      params = params,
      values = unname(original$values[match(ids, names(original$values), nomatch = 0L)]),
      value_positions = match(names(original$values), ids),
      dependencies = list(
        id = match(dependencies$id, ids),
        on = match(dependencies$on, ids),
        cond = dependencies$cond
      ),
      extra_trafo = original$extra_trafo,
      constraint = original$constraint
    )
  }

  expect_equal(normalize_ids(a), normalize_ids(b))
}
