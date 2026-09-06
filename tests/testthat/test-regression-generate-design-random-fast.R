random_design_space = function() {
  ps(
    fixed = p_int(1, 5),
    mode = p_fct(c("off", "on")),
    dependent = p_dbl(-10, 10, depends = mode == "on"),
    logical = p_lgl()
  )
}

random_design_expect_sampler = function(param_set, n, seed, info = NULL) {
  set.seed(seed)
  expected = SamplerUnif$new(param_set)$sample(n)
  expected_seed = .Random.seed

  set.seed(seed)
  actual = generate_design_random(param_set, n)
  actual_seed = .Random.seed

  expect_identical(as.list(actual$data), as.list(expected$data), info = info)
  expect_identical(names(actual$data), names(expected$data), info = info)
  expect_identical(row.names(actual$data), row.names(expected$data), info = info)
  expect_identical(class(actual$data), class(expected$data), info = info)
  expect_identical(
    vapply(actual$data, typeof, character(1L)),
    vapply(expected$data, typeof, character(1L)),
    info = info
  )
  expect_identical(actual_seed, expected_seed, info = info)
  actual
}

test_that("random designs and SamplerUnif share values and RNG streams", {
  fixed = random_design_space()
  fixed$values = list(fixed = 3L, logical = TRUE)
  dependent = random_design_space()
  collection = ParamSetCollection$new(list(
    left = fixed,
    right = dependent
  ))
  origin = ps(hidden = p_int(0, 2), mode = p_lgl(), x = p_dbl(
    0,
    1,
    depends = mode == TRUE
  ))
  shadow = ParamSetShadow$new(origin, "hidden")

  spaces = list(
    base = random_design_space(),
    fixed = fixed,
    dependent = dependent,
    collection = collection,
    shadow = shadow
  )
  for (space_name in names(spaces)) {
    for (n in c(0L, 1L, 17L)) {
      random_design_expect_sampler(
        spaces[[space_name]],
        n,
        9091L,
        sprintf("%s n=%d", space_name, n)
      )
    }
  }
})

test_that("fixed values and dependency masking are applied at Design boundary", {
  param_set = random_design_space()
  param_set$values = list(fixed = 3L)
  result = generate_design_random(param_set, 257L)

  expect_identical(result$data$fixed, rep(3L, 257L))
  expect_identical(
    is.na(result$data$dependent),
    result$data$mode == "off"
  )
  expect_true(any(result$data$mode == "off"))
  expect_true(any(result$data$mode == "on"))
  expect_type(result$data$fixed, "integer")
  expect_type(result$data$dependent, "double")
})

test_that("the generated Design owns one graph-aware deep clone", {
  origin = random_design_space()
  origin$values = list(fixed = 3L)
  graph = ParamSetCollection$new(list(left = origin, right = origin))
  design = generate_design_random(graph, 19L)

  expect_false(identical(design$param_set, graph))
  expect_false(identical(design$param_set$sets$left, origin))
  expect_identical(
    design$param_set$sets$left,
    design$param_set$sets$right
  )

  design$param_set$sets$left$values = list(fixed = 4L)
  expect_identical(design$param_set$sets$right$values$fixed, 4L)
  expect_identical(origin$values$fixed, 3L)
})

test_that("random designs return complete independently mutable data.tables", {
  result = generate_design_random(random_design_space(), 10L)
  values = result$data
  expect_type(attr(values, ".internal.selfref", exact = TRUE), "externalptr")
  expect_identical(data.table:::selfrefok(values, verbose = FALSE), 1L)
  expect_identical(values, values[])

  data.table::set(values, 1L, "fixed", 5L)
  expect_identical(result$data$fixed[[1L]], 5L)
  expect_identical(
    result$param_set$values,
    structure(list(), names = character())
  )
})

test_that("random-design validation is deterministic and consumes no RNG", {
  cases = list(
    unbounded = list(ps(x = p_dbl()), 1L, "unbounded"),
    untyped = list(ps(x = p_uty()), 1L, "untyped"),
    negative = list(random_design_space(), -1L, "`n`"),
    fractional = list(random_design_space(), 1.5, "`n`")
  )
  for (name in names(cases)) {
    set.seed(501L)
    before = .Random.seed
    expect_error(
      generate_design_random(cases[[name]][[1L]], cases[[name]][[2L]]),
      cases[[name]][[3L]],
      info = name
    )
    expect_identical(.Random.seed, before, info = name)
  }
})

test_that("zero-dimensional random designs keep the historical zero rows", {
  result = generate_design_random(ps(), 100L)
  expect_identical(dim(result$data), c(0L, 0L))
})

test_that("random designs preserve zero-level factor emptiness", {
  param_set = ps(choice = p_fct(character()))
  result = generate_design_random(param_set, 0L)
  expect_identical(dim(result$data), c(0L, 1L))
  expect_identical(result$data$choice, character())

  set.seed(702L)
  before = .Random.seed
  expect_error(
    generate_design_random(param_set, 1L),
    "Cannot sample a factor parameter with no levels",
    fixed = TRUE
  )
  expect_identical(.Random.seed, before)
})

test_that("row counts reject a factor rather than reading its level code", {
  param_set = ps(a = p_dbl(0, 1))
  levels = factor("b", levels = c("a", "b", "c"))

  expect_error(
    generate_design_random(param_set, levels),
    "must be one non-negative integer"
  )
  expect_error(SamplerUnif$new(param_set)$sample(levels), "not a factor|'count'")

  # The deliberately tolerated scalar attributes are unaffected.
  expect_identical(nrow(generate_design_random(param_set, c(rows = 3L))$data), 3L)
  expect_identical(
    nrow(generate_design_random(param_set, structure(2L, class = "myint"))$data),
    2L
  )
})
