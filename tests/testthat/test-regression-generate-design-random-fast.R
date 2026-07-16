random_design_reference = function(param_set, n) {
  sampler = SamplerUnif$new(param_set)
  # Keep this oracle independent of both bulk implementations.  The extra
  # class leaves the generated R6 methods and the historical hierarchical
  # sampler unchanged, but deliberately makes every exact-object native gate
  # fail closed.
  class(sampler) = c("SamplerUnifReference", class(sampler))
  sampler$sample(n)
}

random_design_dependent_space = function() {
  ps(
    mode = p_fct(c("off", "on")),
    value = p_dbl(-2, 2, depends = mode == "on"),
    flag = p_lgl()
  )
}

random_design_mixed_space = function() {
  ps(
    double = p_dbl(-10, 10),
    integer = p_int(-2, 4),
    factor = p_fct(c("a", "b", "c")),
    logical = p_lgl()
  )
}

random_design_expect_reference = function(param_set, n, seed, info = NULL) {
  set.seed(seed)
  expected = random_design_reference(param_set, n)
  expected_seed = .Random.seed

  set.seed(seed)
  actual = generate_design_random(param_set, n)
  actual_seed = .Random.seed

  expect_identical(as.list(actual$data), as.list(expected$data), info = info)
  expect_identical(names(actual$data), names(expected$data), info = info)
  expect_identical(row.names(actual$data), row.names(expected$data), info = info)
  expect_identical(class(actual$data), class(expected$data), info = info)
  expect_identical(
    names(attributes(actual$data)),
    names(attributes(expected$data)),
    info = info
  )
  expect_identical(
    vapply(actual$data, typeof, character(1L)),
    vapply(expected$data, typeof, character(1L)),
    info = info
  )
  expect_identical(actual_seed, expected_seed, info = info)
  expect_identical(class(actual$param_set), class(expected$param_set), info = info)
  expect_identical(actual$param_set$ids(), expected$param_set$ids(), info = info)
  expect_identical(actual$param_set$values, expected$param_set$values, info = info)
  invisible(actual)
}

random_design_capture_error = function(fun, param_set, n) {
  set.seed(501L)
  condition = tryCatch(fun(param_set, n), error = identity)
  stopifnot(inherits(condition, "error"))
  list(
    class = class(condition),
    message = conditionMessage(condition),
    random_seed = .Random.seed
  )
}

test_that("bulk random designs exactly match SamplerUnif data and RNG streams", {
  fixed = random_design_mixed_space()
  fixed$values = list(double = 3.5, factor = "b")
  collection = ParamSetCollection$new(list(
    left = fixed,
    right = random_design_dependent_space()
  ))
  spaces = list(
    mixed = random_design_mixed_space(),
    fixed = fixed,
    dependent = random_design_dependent_space(),
    collection = collection
  )

  for (space_name in names(spaces)) {
    for (n in c(0L, 1L, 17L)) {
      for (seed in c(1L, 20260713L)) {
        random_design_expect_reference(
          spaces[[space_name]],
          n,
          seed,
          info = sprintf("%s: n=%d seed=%d", space_name, n, seed)
        )
      }
    }
  }
})

test_that("bulk random designs are complete data.table objects", {
  param_set = random_design_mixed_space()
  values = generate_design_random(param_set, 10L)$data

  # Downstream packages use data.table row operations and then compare null
  # mutations or recombinations with the original design via identical().  A
  # class-only facade is insufficient: data.table repairs a missing selfref on
  # first use, making an otherwise unchanged result observably different.
  expect_type(attr(values, ".internal.selfref", exact = TRUE), "externalptr")
  expect_identical(data.table:::selfrefok(values, verbose = FALSE), 1L)
  expect_identical(values, values[])
})

test_that("zero-row bulk designs preserve an absent or existing RNG state", {
  had_seed = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed = get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  absent_result = generate_design_random(random_design_mixed_space(), 0L)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  expect_identical(nrow(absent_result$data), 0L)

  set.seed(811L)
  before = .Random.seed
  existing_result = generate_design_random(random_design_mixed_space(), 0L)
  expect_identical(.Random.seed, before)
  expect_identical(nrow(existing_result$data), 0L)
})

test_that("fixed values and dependency masking retain Sampler semantics", {
  param_set = ps(
    fixed = p_int(1, 5),
    mode = p_fct(c("off", "on")),
    dependent = p_dbl(-100, 100, depends = mode == "on"),
    logical = p_lgl()
  )
  param_set$values = list(fixed = 3L)

  result = random_design_expect_reference(param_set, 257L, 7331L)
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

test_that("the generated Design owns one detached ParamSet state", {
  param_set = ps(
    fixed = p_int(1, 5),
    mode = p_fct(c("off", "on")),
    dependent = p_dbl(-1, 1, depends = mode == "on")
  )
  param_set$values = list(fixed = 3L)
  ids_before = param_set$ids()
  values_before = param_set$values
  tags_before = param_set$tags
  deps_before = data.table::copy(param_set$deps)

  design = generate_design_random(param_set, 19L)
  detached = design$param_set

  expect_false(identical(detached, param_set))
  expect_identical(param_set$ids(), ids_before)
  expect_identical(param_set$values, values_before)
  expect_identical(param_set$tags, tags_before)
  expect_equal(param_set$deps, deps_before)

  detached$values = list(fixed = 4L)
  detached$deps = data.table::data.table(
    id = character(),
    on = character(),
    cond = list()
  )
  expect_identical(param_set$values, list(fixed = 3L))
  expect_identical(nrow(param_set$deps), 1L)

  param_set$values = list(fixed = 5L)
  expect_identical(detached$values, list(fixed = 4L))
  expect_identical(nrow(detached$deps), 0L)
})

test_that("ParamSetCollection designs deeply detach their child sets", {
  left = ps(x = p_dbl(0, 1), choice = p_fct(c("a", "b")))
  left$values = list(x = 0.25)
  right = random_design_dependent_space()
  collection = ParamSetCollection$new(list(left = left, right = right))

  design = random_design_expect_reference(collection, 31L, 919L)
  detached = design$param_set
  detached_left = detached$sets$left

  expect_identical(class(detached), c("ParamSetCollection", "ParamSet", "R6"))
  expect_false(identical(detached, collection))
  expect_false(identical(detached_left, collection$sets$left))
  expect_identical(design$data$left.x, rep(0.25, 31L))

  detached_left$values = list(x = 0.75)
  expect_identical(collection$sets$left$values, list(x = 0.25))
  collection$sets$left$values = list(x = 0.5)
  expect_identical(detached_left$values, list(x = 0.75))
})

test_that("ParamSet subclasses retain the complete SamplerUnif fallback", {
  calls = new.env(parent = emptyenv())
  calls$subspaces = 0L
  Subclass = R6::R6Class(
    "RandomDesignParamSetSubclass",
    inherit = ParamSet,
    public = list(
      subspaces = function(ids = self$ids()) {
        calls$subspaces = calls$subspaces + 1L
        super$subspaces(ids)
      },
      qunif = function(x) {
        stop("the subclass bulk qunif method must not be used")
      }
    )
  )
  param_set = Subclass$new(list(
    x = p_dbl(0, 1),
    y = p_int(1, 3)
  ))

  set.seed(2026L)
  expected = random_design_reference(param_set, 11L)
  expected_seed = .Random.seed
  calls$subspaces = 0L
  set.seed(2026L)
  actual = generate_design_random(param_set, 11L)

  expect_identical(calls$subspaces, 1L)
  expect_identical(as.list(actual$data), as.list(expected$data))
  expect_identical(.Random.seed, expected_seed)
  expect_s3_class(actual$param_set, "RandomDesignParamSetSubclass")

  unbounded = Subclass$new(list(x = p_dbl()))
  expect_identical(
    random_design_capture_error(generate_design_random, unbounded, -1),
    random_design_capture_error(random_design_reference, unbounded, -1)
  )
  expect_identical(
    random_design_capture_error(generate_design_random, param_set, 1.5),
    random_design_capture_error(random_design_reference, param_set, 1.5)
  )
})

test_that("exact objects with replaced sampling wrappers fail closed", {
  replace_method = function(param_set, name, replacement) {
    unlockBinding(name, param_set)
    assign(name, replacement, envir = param_set)
    lockBinding(name, param_set)
    invisible(param_set)
  }

  root_qunif = random_design_mixed_space()
  replace_method(
    root_qunif,
    "qunif",
    function(x) stop("root replacement qunif must not run on fallback")
  )
  expect_false(is_exact_random_design_space(root_qunif))
  random_design_expect_reference(root_qunif, 9L, 1701L)

  subspace_calls = 0L
  replaced_subspaces = random_design_mixed_space()
  inherited_subspaces = replaced_subspaces$subspaces
  replace_method(
    replaced_subspaces,
    "subspaces",
    function(ids = replaced_subspaces$ids()) {
      subspace_calls <<- subspace_calls + 1L
      inherited_subspaces(ids)
    }
  )
  expect_false(is_exact_random_design_space(replaced_subspaces))
  set.seed(1702L)
  generate_design_random(replaced_subspaces, 7L)
  expect_identical(subspace_calls, 1L)

  changed_length = random_design_mixed_space()
  unlockBinding("length", changed_length)
  makeActiveBinding(
    "length",
    function(value) {
      if (!missing(value)) stop("length is read-only")
      0L
    },
    changed_length
  )
  expect_false(is_exact_random_design_space(changed_length))
  set.seed(1703L)
  result = generate_design_random(changed_length, 5L)
  expect_identical(dim(result$data), c(5L, 4L))
})

test_that("random-design admission rejects delayed and active replacements lazily", {
  symbol = getDLLRegisteredRoutines(
    getLoadedDLLs()[["paradox"]]
  )$.Call[["param_set_surface_auth"]]

  parameter_set = random_design_mixed_space()
  state = new.env(parent = emptyenv())
  state$forced = 0L
  unlockBinding("qunif", parameter_set)
  delayedAssign(
    "qunif",
    {
      state$forced = state$forced + 1L
      function(x) stop("delayed root qunif")
    },
    assign.env = parameter_set
  )
  lockBinding("qunif", parameter_set)
  expect_false(.Call(symbol, parameter_set, 4L))
  expect_identical(state$forced, 0L)
  expect_false(is_exact_random_design_space(parameter_set))
  expect_identical(state$forced, 0L)
  set.seed(1704L)
  generate_design_random(parameter_set, 3L)
  # R6's historical clone implementation materializes ordinary delayed method
  # bindings while copying the object; native admission itself does not.
  expect_identical(state$forced, 1L)

  child = random_design_mixed_space()
  collection = ParamSetCollection$new(list(component = child))
  sets_reads = 0L
  stored_sets = collection$sets
  unlockBinding("sets", collection)
  makeActiveBinding(
    "sets",
    function(value) {
      if (!missing(value)) stop("sets is read-only")
      sets_reads <<- sets_reads + 1L
      stored_sets
    },
    collection
  )
  expect_false(.Call(symbol, collection, 4L))
  expect_identical(sets_reads, 0L)
  expect_false(is_exact_random_design_space(collection))
  expect_identical(sets_reads, 0L)
})

test_that("random-design fallback forces delayed private stores historically", {
  delay_private = function(param_set, name, state) {
    private = param_set$.__enclos_env__$private
    value = private[[name]]
    evaluation_environment = new.env(parent = baseenv())
    evaluation_environment$state = state
    evaluation_environment$value = value
    delayedAssign(
      name,
      {
        state$reads = state$reads + 1L
        value
      },
      assign.env = private,
      eval.env = evaluation_environment
    )
    invisible(param_set)
  }

  parameter_state = new.env(parent = emptyenv())
  parameter_state$reads = 0L
  parameter_set = random_design_mixed_space()
  delay_private(parameter_set, ".params", parameter_state)
  expect_false(is_exact_random_design_space(parameter_set))
  expect_identical(parameter_state$reads, 0L)
  set.seed(1705L)
  parameter_design = generate_design_random(parameter_set, 3L)
  expect_s3_class(parameter_design, "Design")
  expect_identical(nrow(parameter_design$data), 3L)
  expect_identical(parameter_state$reads, 1L)

  sets_state = new.env(parent = emptyenv())
  sets_state$reads = 0L
  collection = ParamSetCollection$new(list(
    component = random_design_mixed_space()
  ))
  delay_private(collection, ".sets", sets_state)
  expect_false(is_exact_random_design_space(collection))
  expect_identical(sets_state$reads, 0L)
  set.seed(1706L)
  collection_design = generate_design_random(collection, 3L)
  expect_s3_class(collection_design, "Design")
  expect_identical(nrow(collection_design$data), 3L)
  expect_gt(sets_state$reads, 0L)
})

test_that("collections with nested subclasses retain the complete fallback", {
  ChildSubclass = R6::R6Class(
    "RandomDesignNestedParamSetSubclass",
    inherit = ParamSet
  )
  child = ChildSubclass$new(list(
    x = p_dbl(0, 1),
    y = p_lgl()
  ))
  collection = ParamSetCollection$new(list(child = child))
  expect_false(is_exact_random_design_space(collection))

  # If the collection were admitted to the bulk path this exact-object method
  # would be called. SamplerUnif instead samples detached one-dimensional base
  # ParamSets and never calls the collection's qunif method.
  unlockBinding("qunif", collection)
  assign(
    "qunif",
    function(x) stop("nested subclass collection entered bulk qunif"),
    envir = collection
  )

  set.seed(9182L)
  expected = random_design_reference(collection, 13L)
  expected_seed = .Random.seed
  set.seed(9182L)
  actual = generate_design_random(collection, 13L)

  expect_identical(as.list(actual$data), as.list(expected$data))
  expect_identical(.Random.seed, expected_seed)
  expect_s3_class(actual$param_set$sets$child, "RandomDesignNestedParamSetSubclass")
})

test_that("custom Domain rows retain SamplerUnif validation", {
  class_name = "ParamRandomDesignCustom"
  calls = 0L
  registerS3method(
    "domain_qunif",
    class_name,
    function(param, x) {
      calls <<- calls + 1L
      x
    },
    envir = asNamespace("paradox")
  )
  make_custom_domain = function() {
    paradox:::Domain(
      cls = class_name,
      grouping = class_name,
      storage_type = "numeric"
    )
  }
  param_set = ParamSet$new(list(x = make_custom_domain()))

  expect_false(is_exact_random_design_space(param_set))
  expected = random_design_capture_error(
    random_design_reference,
    param_set,
    3L
  )
  actual = random_design_capture_error(
    generate_design_random,
    param_set,
    3L
  )
  expect_identical(actual, expected)
  expect_identical(actual$message, "ParamSet contains untyped params!")
  expect_identical(calls, 0L)
})

test_that("empty spaces retain their historical zero-row fallback", {
  spaces = list(
    ParamSet = ps(),
    ParamSetCollection = ParamSetCollection$new(list())
  )

  for (space_name in names(spaces)) {
    for (n in c(0L, 3L)) {
      set.seed(441L)
      before = .Random.seed
      actual = generate_design_random(spaces[[space_name]], n)
      actual_seed = .Random.seed

      set.seed(441L)
      expected = random_design_reference(spaces[[space_name]], n)
      expected_seed = .Random.seed

      expect_identical(as.list(actual$data), as.list(expected$data))
      expect_identical(dim(actual$data), c(0L, 0L))
      expect_identical(actual_seed, before)
      expect_identical(actual_seed, expected_seed)
    }
  }
})

test_that("bulk random-design errors and their priority match SamplerUnif", {
  cases = list(
    unbounded = list(param_set = ps(x = p_dbl()), n = 2L),
    utility = list(param_set = ps(x = p_uty()), n = 2L),
    negative_n = list(param_set = ps(x = p_dbl(0, 1)), n = -1),
    fractional_n = list(param_set = ps(x = p_dbl(0, 1)), n = 1.5),
    missing_n = list(param_set = ps(x = p_dbl(0, 1)), n = NA_real_),
    vector_n = list(param_set = ps(x = p_dbl(0, 1)), n = c(1, 2)),
    support_before_n = list(param_set = ps(x = p_dbl()), n = -1),
    invalid_object = list(param_set = list(), n = 1L),
    spoofed_class = list(
      param_set = structure(list(), class = c("ParamSet", "R6")),
      n = 1L
    )
  )

  for (case_name in names(cases)) {
    case = cases[[case_name]]
    expected = random_design_capture_error(
      random_design_reference,
      case$param_set,
      case$n
    )
    actual = random_design_capture_error(
      generate_design_random,
      case$param_set,
      case$n
    )
    expect_identical(actual, expected, info = case_name)
  }

  expect_identical(
    random_design_capture_error(generate_design_random, ps(x = p_dbl()), -1)$message,
    "ParamSet contains unbounded params!"
  )
  expect_identical(
    random_design_capture_error(generate_design_random, ps(x = p_uty()), 1L)$message,
    "ParamSet contains untyped params!"
  )
})
