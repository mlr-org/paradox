# Collection graph readers keep bounded inline scratch for 16 nodes and then
# switch every coordinated array to operation-local storage. Only capsule
# construction and Shadow creation were covered past that boundary; these cases
# drive every other collection-graph consumer through it, in deep-chain,
# wide-fanout and mixed prefix/postfix shapes.

deep_chain = function(depth, ...) {
  graph = ps(x = p_dbl(0, 1), y = p_fct(c("a", "b")))
  for (level in seq_len(depth)) {
    graph = ParamSetCollection$new(list(node = graph), ...)
  }
  graph
}

test_that("every collection graph consumer works past the inline capacity", {
  for (depth in c(15L, 16L, 17L, 33L)) {
    graph = deep_chain(depth)
    prefix = paste(rep("node", depth), collapse = ".")
    x_id = paste0(prefix, ".x")
    y_id = paste0(prefix, ".y")

    expect_setequal(graph$ids(), c(x_id, y_id))
    expect_identical(nrow(as.data.table(graph)), 2L)
    expect_identical(length(graph$domains), 2L)
    expect_identical(nrow(graph$deps), 0L)

    graph$values = set_names(list(0.5, "a"), c(x_id, y_id))
    expect_setequal(names(graph$values), c(x_id, y_id))
    expect_setequal(names(graph$get_values()), c(x_id, y_id))
    expect_true(graph$check(graph$get_values()))
    expect_type(graph$trafo(graph$get_values()), "list")

    quantiles = matrix(
      c(0.25, 0.75),
      nrow = 1L,
      dimnames = list(NULL, c(x_id, y_id))
    )
    expect_identical(nrow(graph$qunif(quantiles)), 1L)

    expect_setequal(graph$subset(x_id)$ids(), x_id)
    expect_setequal(graph$flatten()$ids(), graph$ids())
    expect_setequal(ParamSetShadow$new(graph, y_id)$ids(), x_id)
    expect_identical(nrow(generate_design_random(graph, 3L)$data), 3L)
    expect_true(all.equal(unserialize(serialize(graph, NULL)), graph))
    expect_true(all.equal(graph$clone(deep = TRUE), graph))

    tuned = graph$clone(deep = TRUE)
    tuned$values = set_names(list(to_tune(0, 1), "a"), c(x_id, y_id))
    expect_setequal(tuned$search_space()$ids(), x_id)
  }
})

test_that("wide collection graphs work past the inline capacity", {
  for (width in c(15L, 16L, 17L, 40L)) {
    children = set_names(
      lapply(seq_len(width), function(index) ps(x = p_dbl(0, 1))),
      paste0("c", seq_len(width))
    )
    graph = ParamSetCollection$new(children)
    ids = paste0("c", seq_len(width), ".x")
    expect_setequal(graph$ids(), ids)

    values = seq_len(width) / (width + 1L)
    graph$values = set_names(as.list(values), ids)
    expect_setequal(names(graph$values), ids)
    expect_identical(
      vapply(graph$sets, function(child) child$values$x, numeric(1), USE.NAMES = FALSE),
      values
    )

    expect_setequal(graph$flatten()$ids(), ids)
    expect_setequal(graph$subset(ids[1:3])$ids(), ids[1:3])
    expect_true(all.equal(unserialize(serialize(graph, NULL)), graph))
  }
})

test_that("dependencies and dormant values survive a deep collection graph", {
  leaf = ps(gate = p_fct(c("on", "off")), value = p_dbl(0, 1))
  leaf$add_dep("value", "gate", CondEqual("on"))
  graph = leaf
  for (level in seq_len(20L)) {
    graph = ParamSetCollection$new(list(n = graph))
  }
  prefix = paste(rep("n", 20L), collapse = ".")
  gate_id = paste0(prefix, ".gate")
  value_id = paste0(prefix, ".value")

  expect_identical(nrow(graph$deps), 1L)
  expect_identical(graph$deps$id, value_id)

  graph$values = set_names(list("off", 0.5), c(gate_id, value_id))
  expect_setequal(names(graph$values), c(gate_id, value_id))
  expect_setequal(names(graph$get_values()), gate_id)

  graph$values = set_names(list("on", 0.5), c(gate_id, value_id))
  expect_setequal(names(graph$get_values()), c(gate_id, value_id))
})

test_that("nested collections may mix prefix and postfix naming", {
  leaf = function() ps(v = p_dbl(0, 1), w = p_int(1, 3))
  shapes = list(
    list(inner = FALSE, outer = FALSE, ids = c("hi.lo.v", "hi.lo.w")),
    list(inner = TRUE, outer = TRUE, ids = c("v.lo.hi", "w.lo.hi")),
    list(inner = TRUE, outer = FALSE, ids = c("hi.v.lo", "hi.w.lo")),
    list(inner = FALSE, outer = TRUE, ids = c("lo.v.hi", "lo.w.hi"))
  )

  for (shape in shapes) {
    inner = ParamSetCollection$new(
      list(lo = leaf()),
      postfix_names = shape$inner
    )
    graph = ParamSetCollection$new(
      list(hi = inner),
      postfix_names = shape$outer
    )

    expect_setequal(graph$ids(), shape$ids)
    expect_setequal(graph$flatten()$ids(), shape$ids)

    graph$values = set_names(list(0.5, 2L), shape$ids)
    expect_setequal(names(graph$values), shape$ids)
    expect_identical(graph$sets[[1L]]$sets[[1L]]$values, list(v = 0.5, w = 2L))
    expect_setequal(graph$subset(shape$ids[[1L]])$ids(), shape$ids[[1L]])
  }
})
