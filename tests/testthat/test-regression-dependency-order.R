# Deliberately simple reference: take the first currently eligible schema row.
# This describes the observable tie-break without depending on the native queue.
dependency_order_reference = function(ids, child, parent) {
  result = character()
  while (length(result) < length(ids)) {
    ready = ids[!ids %in% result & vapply(ids, function(id) {
      all(parent[child == id] %in% c(result, setdiff(parent, ids)))
    }, logical(1L))]
    if (!length(ready)) stop("cycle in test reference")
    result = c(result, ready[[1L]])
  }
  result
}

test_that("dependency plans prioritize newly ready parameters in schema order", {
  set.seed(9033)
  for (width in c(2L, 3L, 8L, 17L, 31L, 32L, 33L)) {
    ids = paste0("x", seq_len(width))
    for (iteration in seq_len(4L)) {
      topology = sample(ids)
      child = topology[-1L]
      parent = vapply(seq_along(child), function(index) {
        sample(topology[seq_len(index)], 1L)
      }, character(1L))
      # Parallel predicates must decrement separately. A dangling parent does
      # not hold a node in the queue, but makes its condition unsatisfied.
      child = c(child, child[[1L]], topology[[1L]])
      parent = c(parent, parent[[1L]], "absent")
      param_set = ParamSet$new(setNames(rep(list(p_int(0, 1)), width), ids))
      param_set$deps = data.table::data.table(
        id = child, on = parent, cond = rep(list(CondEqual(1L)), length(child))
      )
      input = data.table::as.data.table(setNames(rep(list(0L), width), ids))
      plan = .Call(paradox:::C_design_dependency_plan, input, param_set)
      expected = dependency_order_reference(ids, child, parent)
      expect_identical(plan$columns, expected)
      expect_identical(plan$rows, rep(list(1L), width))
      expect_identical(plan$values, rep(list(NA_integer_), width))
      data.table::setcolorder(input, rev(ids))
      expect_identical(
        .Call(paradox:::C_design_dependency_plan, input, param_set), plan
      )
    }
  }
})

test_that("dependent grid priority preserves complete-product row order", {
  param_set = ps(
    leaf = p_int(0, 2),
    wide = p_int(0, 3),
    gate = p_lgl(),
    fixed = p_int(0, 4),
    small = p_int(0, 1)
  )
  param_set$add_dep("leaf", "gate", CondEqual(TRUE))
  param_set$add_dep("wide", "small", CondEqual(1L))
  param_set$add_dep("leaf", "wide", CondAnyOf(c(1L, 2L)))
  param_set$values = list(fixed = 2L)
  expected = data.table::CJ(
    leaf = 0:2, wide = 0:3, gate = c(TRUE, FALSE), fixed = 0:4, small = 0:1,
    sorted = FALSE
  )
  expected$fixed = rep(2L, nrow(expected))
  expected$wide[expected$small != 1L] = NA_integer_
  expected$leaf[!expected$gate | is.na(expected$wide) |
    !expected$wide %in% c(1L, 2L)] = NA_integer_
  expected = unique(expected)
  expect_identical(generate_design_grid(param_set, 5L)$data, expected)
})

test_that("dependency ordering still diagnoses cycles with independent nodes", {
  param_set = ps(a = p_int(0, 1), b = p_int(0, 1), free = p_int(0, 1))
  param_set$deps = data.table::data.table(
    id = c("a", "b"), on = c("b", "a"),
    cond = list(CondEqual(1L), CondEqual(1L))
  )
  input = data.table::data.table(a = 0L, b = 0L, free = 0L)
  expect_error(.Call(paradox:::C_design_dependency_plan, input, param_set), "cycle")
  expect_error(generate_design_grid(param_set, 0L), "cycle")
})
