sampler_unif_symbol = function() {
  getDLLRegisteredRoutines(
    getLoadedDLLs()[["paradox"]]
  )$.Call[["sampler_unif_sample_builtin"]]
}

sampler_unif_call = function(param_set, n) {
  .Call(sampler_unif_symbol(), param_set, n)
}

sampler_unif_mixed_space = function() {
  ps(
    double = p_dbl(-3, 8),
    integer = p_int(-2, 4),
    factor = p_fct(c("z", "a", "q")),
    logical = p_lgl()
  )
}

sampler_unif_dependent_space = function() {
  ps(
    mode = p_fct(c("off", "on")),
    value = p_dbl(-2, 2, depends = mode == "on"),
    flag = p_lgl()
  )
}

sampler_unif_oracle = function(param_set, n) {
  params = param_set$params
  ids = params$id
  units = matrix(
    runif(as.double(n) * length(ids)),
    nrow = n,
    ncol = length(ids),
    dimnames = list(NULL, ids)
  )
  columns = lapply(seq_along(ids), function(index) {
    unit = units[, index]
    switch(params$cls[[index]],
      ParamDbl = {
        mapped = unit * params$upper[[index]] -
          (unit - 1) * params$lower[[index]]
        pmax(pmin(mapped, params$upper[[index]]), params$lower[[index]])
      },
      ParamInt = {
        mapped = floor(
          unit * (params$upper[[index]] + 1) -
            (unit - 1) * params$lower[[index]]
        )
        as.integer(pmax(
          pmin(mapped, params$upper[[index]]),
          params$lower[[index]]
        ))
      },
      ParamFct = {
        levels = params$levels[[index]]
        levels[pmin(floor(unit * length(levels)) + 1L, length(levels))]
      },
      ParamLgl = unit < 0.5,
      stop("unsupported oracle kind")
    )
  })
  data.table::as.data.table(setNames(columns, ids))
}

sampler_unif_expect_table = function(actual, expected, info = NULL) {
  expect_true(data.table::is.data.table(actual), info = info)
  expect_identical(as.list(actual), as.list(expected), info = info)
  expect_identical(names(actual), names(expected), info = info)
  expect_identical(row.names(actual), row.names(expected), info = info)
  expect_identical(class(actual), class(expected), info = info)
  expect_identical(
    vapply(actual, typeof, character(1L)),
    vapply(expected, typeof, character(1L)),
    info = info
  )
  expect_identical(data.table:::selfrefok(actual, verbose = FALSE), 1L)
}

test_that("the native uniform engine exactly matches column-major qunif", {
  param_set = sampler_unif_mixed_space()
  for (n in c(0L, 1L, 17L)) {
    for (seed in c(1L, 20260716L)) {
      set.seed(seed)
      expected = sampler_unif_oracle(param_set, n)
      expected_seed = .Random.seed

      set.seed(seed)
      actual = sampler_unif_call(param_set, n)

      info = sprintf("n=%d seed=%d", n, seed)
      sampler_unif_expect_table(actual, expected, info)
      expect_identical(.Random.seed, expected_seed, info = info)
    }
  }
})

test_that("all built-in uniform RNG kinds retain the public stream", {
  saved_kind = RNGkind()
  on.exit(suppressWarnings(do.call(RNGkind, as.list(saved_kind))), add = TRUE)
  param_set = sampler_unif_mixed_space()
  kinds = c(
    "Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
    "Mersenne-Twister", "Knuth-TAOCP", "Knuth-TAOCP-2002",
    "L'Ecuyer-CMRG"
  )

  for (kind in kinds) {
    suppressWarnings(RNGkind(
      kind,
      normal.kind = "Inversion",
      sample.kind = "Rejection"
    ))
    set.seed(1801L)
    expected = sampler_unif_oracle(param_set, 9L)
    expected_seed = .Random.seed
    set.seed(1801L)
    actual = sampler_unif_call(param_set, 9L)
    sampler_unif_expect_table(actual, expected, kind)
    expect_identical(.Random.seed, expected_seed, info = kind)
  }
})

test_that("zero-row and zero-dimensional sampling never touches RNG state", {
  had_seed = exists(".Random.seed", .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed = .Random.seed
  on.exit({
    if (had_seed) {
      .Random.seed <<- old_seed
    } else if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }

  zero_rows = sampler_unif_call(sampler_unif_mixed_space(), 0L)
  expect_identical(dim(zero_rows), c(0L, 4L))
  expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))

  empty = sampler_unif_call(ps(), 100L)
  expect_identical(dim(empty), c(0L, 0L))
  expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
})

test_that("zero-level factors sample only typed zero-row designs", {
  param_set = ps(choice = p_fct(character()))
  sampler = SamplerUnif$new(param_set)

  direct = sampler_unif_call(param_set, 0L)
  expect_identical(dim(direct), c(0L, 1L))
  expect_identical(direct$choice, character())

  design = sampler$sample(0L)
  expect_identical(dim(design$data), c(0L, 1L))
  expect_identical(design$data$choice, character())

  set.seed(701L)
  before = .Random.seed
  expect_error(
    sampler_unif_call(param_set, 1L),
    "Cannot sample a factor parameter with no levels",
    fixed = TRUE
  )
  expect_identical(.Random.seed, before)
  expect_error(
    sampler$sample(1L),
    "Cannot sample a factor parameter with no levels",
    fixed = TRUE
  )
  expect_identical(.Random.seed, before)
})

test_that("SamplerUnif uses one engine for values and dependencies", {
  param_set = sampler_unif_dependent_space()
  param_set$values = list(flag = TRUE)
  sampler = SamplerUnif$new(param_set)

  set.seed(981L)
  expected_data = sampler_unif_oracle(sampler$param_set, 101L)
  expected_seed = .Random.seed
  expected = Design$new(
    sampler$param_set$clone(deep = TRUE),
    expected_data,
    remove_dupl = FALSE
  )

  set.seed(981L)
  actual = sampler$sample(101L)
  sampler_unif_expect_table(actual$data, expected$data)
  expect_identical(.Random.seed, expected_seed)
  expect_true(all(is.na(actual$data$value) == (actual$data$mode == "off")))
  expect_identical(actual$data$flag, rep(TRUE, 101L))
})

test_that("collections, shared nodes, and live shadows use the same engine", {
  shared = sampler_unif_mixed_space()
  collection = ParamSetCollection$new(list(left = shared, right = shared))
  origin = ps(hidden = p_int(0, 3), shown = p_dbl(-1, 1), flag = p_lgl())
  shadow = ParamSetShadow$new(origin, "hidden")
  origin$values = list(flag = TRUE)

  for (entry in list(collection = collection, shadow = shadow)) {
    set.seed(422L)
    expected = sampler_unif_oracle(entry, 13L)
    expected_seed = .Random.seed
    set.seed(422L)
    actual = sampler_unif_call(entry, 13L)
    sampler_unif_expect_table(actual, expected)
    expect_identical(.Random.seed, expected_seed)
  }

  design = SamplerUnif$new(shadow)$sample(13L)
  expect_identical(names(design$data), c("shown", "flag"))
  expect_identical(design$data$flag, rep(TRUE, 13L))
})

test_that("SamplerUnif sampler metadata has an explicit mutation boundary", {
  sampler = SamplerUnif$new(sampler_unif_mixed_space())
  expect_length(sampler$samplers, 4L)
  expect_true(all(vapply(sampler$samplers, inherits, logical(1L), "Sampler1DUnif")))

  # Child objects are descriptive; their state is not a second execution
  # graph. Altering one cannot silently select different sampling semantics.
  sampler$samplers[[1L]]$param_set$values = list(double = 0)
  set.seed(889L)
  expected = sampler_unif_call(sampler$param_set, 7L)
  expected_seed = .Random.seed
  set.seed(889L)
  actual = sampler$sample(7L)$data
  sampler_unif_expect_table(actual, expected)
  expect_identical(.Random.seed, expected_seed)

  sampler$samplers = rev(sampler$samplers)
  set.seed(234L)
  before = .Random.seed
  expect_error(sampler$sample(3L), "samplers.*read-only")
  expect_identical(.Random.seed, before)
})

test_that("deep-cloned and serialized SamplerUnif objects remain current", {
  original = SamplerUnif$new(sampler_unif_mixed_space())
  variants = list(
    deep = original$clone(deep = TRUE),
    serialized = unserialize(serialize(original, NULL))
  )
  for (name in names(variants)) {
    expect_s3_class(variants[[name]], "SamplerUnif")
    expect_false(identical(variants[[name]]$param_set, original$param_set))
    expect_identical(
      unname(vapply(
        variants[[name]]$samplers,
        function(component) class(component)[[1L]],
        ""
      )),
      rep("Sampler1DUnif", original$param_set$length),
      info = name
    )
    expect_identical(
      unname(vapply(
        variants[[name]]$samplers,
        function(component) component$param$ids(),
        ""
      )),
      original$param_set$ids(),
      info = name
    )

    set.seed(320L)
    expected = sampler_unif_call(variants[[name]]$param_set, 5L)
    expected_seed = .Random.seed
    set.seed(320L)
    actual = variants[[name]]$sample(5L)$data
    sampler_unif_expect_table(actual, expected, name)
    expect_identical(.Random.seed, expected_seed, info = name)
  }
})

test_that("invalid and corrupt support errors before consuming randomness", {
  cases = list(
    unbounded = ps(x = p_dbl()),
    untyped = ps(x = p_uty())
  )
  for (name in names(cases)) {
    set.seed(731L)
    before = .Random.seed
    expect_error(sampler_unif_call(cases[[name]], 3L), info = name)
    expect_identical(.Random.seed, before, info = name)
  }

  corrupt = ps(x = p_dbl(0, 1))
  private = mlr3misc::get_private(corrupt)
  params = paradox:::param_set_core_state(private)$.params
  params$lower[[1L]] = Inf
  paradox:::param_set_core_replace(private, params = params)
  set.seed(18L)
  before = .Random.seed
  expect_error(sampler_unif_call(corrupt, 3L), "unbounded")
  expect_identical(.Random.seed, before)

  for (n in list(-1L, 1.5, NA_integer_, Inf)) {
    set.seed(822L)
    before = .Random.seed
    expect_error(sampler_unif_call(sampler_unif_mixed_space(), n), "`n`")
    expect_identical(.Random.seed, before)
  }
})

test_that("an ALTREP count is materialized exactly once before the snapshot", {
  n = native_stateful_altrep(3L, 11L, elt_switch_after = 1L)
  set.seed(77L)
  result = sampler_unif_call(sampler_unif_mixed_space(), n)
  expect_identical(nrow(result), 3L)
})

test_that("nonsemantic scalar count attributes remain accepted", {
  n = structure(3L, names = "rows", class = "count")
  set.seed(78L)
  result = sampler_unif_call(sampler_unif_mixed_space(), n)
  expect_identical(nrow(result), 3L)
})
