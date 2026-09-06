test_that("authoritative ParamSet trafo survives forced collection", {
  skip_on_cran()
  parameter_set = ps(
    x = p_dbl(trafo = function(value) c(value, value + 1)),
    y = p_int(trafo = function(value) as.integer(value + 2L)),
    .extra_trafo = function(x, param_set) {
      c(x, list(parameter_count = param_set$length))
    }
  )
  input = list(y = 3L, unknown = "kept", x = 0.5)

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  transformed = parameter_set$trafo(input)
  gctorture(previous)

  expect_identical(
    transformed,
    list(
      y = 5L,
      unknown = "kept",
      x = c(0.5, 1.5),
      parameter_count = 2L
    )
  )
})

test_that("batched nested collection name translation releases transient state", {
  skip_on_cran()
  leaf = ps(
    x = p_int(),
    .extra_trafo = function(x) list(
      x = x$x + 1L,
      derived = x$x * 2L
    )
  )
  nested = psc(unit = leaf)
  collection = psc(layer = nested)
  rows = replicate(
    16L,
    list(layer.unit.x = 3L),
    simplify = FALSE
  )

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  transformed = .Call(
    paradox:::C_design_transpose_trafos,
    rows,
    collection
  )
  gctorture(previous)

  expect_length(transformed, length(rows))
  expect_true(all(vapply(
    transformed,
    identical,
    logical(1L),
    list(layer.unit.x = 4L, layer.unit.derived = 6L)
  )))
})

test_that("live and detached mixed-encoding trafo names survive forced collection", {
  skip_on_cran()
  utf8_name = enc2utf8("caf\u00e9")
  latin1_name = iconv(utf8_name, from = "UTF-8", to = "latin1")
  skip_if(
    is.na(latin1_name),
    "this platform cannot represent the latin1 fixture"
  )
  Encoding(latin1_name) = "latin1"

  leaf = ps(x = p_int())
  leaf$extra_trafo = function(x) {
    setNames(list(x$x + 1L), latin1_name)
  }
  inner = ParamSetCollection$new(list(unit = leaf), postfix_names = TRUE)
  collection = psc(layer = inner)
  detached = collection$flatten()
  input = list(layer.x.unit = 2L)
  expected_name = enc2utf8(paste0("layer.", utf8_name, ".unit"))
  expected = setNames(list(3L), expected_name)

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  live_result = collection$trafo(input)
  detached_result = detached$trafo(input)
  gctorture(previous)

  expect_identical(live_result, expected)
  expect_identical(detached_result, expected)
  expect_identical(Encoding(names(live_result)), "UTF-8")
  expect_identical(Encoding(names(detached_result)), "UTF-8")
})
