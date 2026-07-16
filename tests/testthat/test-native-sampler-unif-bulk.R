sampler_bulk_symbol = function() {
  getDLLRegisteredRoutines(
    getLoadedDLLs()[["paradox"]]
  )$.Call[["sampler_unif_sample_builtin"]]
}

sampler_bulk_call = function(sampler, n) {
  .Call(
    sampler_bulk_symbol(),
    sampler,
    sampler$param_set,
    sampler$samplers,
    n
  )
}

sampler_bulk_mixed_space = function() {
  ps(
    double = p_dbl(-3, 8),
    integer = p_int(-2, 4),
    factor = p_fct(c("z", "a", "q")),
    logical = p_lgl()
  )
}

sampler_bulk_dependent_space = function() {
  ps(
    mode = p_fct(c("off", "on")),
    value = p_dbl(-2, 2, depends = mode == "on"),
    flag = p_lgl()
  )
}

sampler_bulk_oracle = function(sampler, n) {
  ids = sampler$param_set$ids()
  units = matrix(
    runif(as.double(n) * length(ids)),
    nrow = n,
    ncol = length(ids),
    dimnames = list(NULL, ids)
  )
  sampler$param_set$qunif(units)
}

sampler_bulk_expect_table = function(actual, expected, info = NULL) {
  expect_true(data.table::is.data.table(actual), info = info)
  expect_identical(as.list(actual), as.list(expected), info = info)
  expect_identical(names(actual), names(expected), info = info)
  expect_identical(row.names(actual), row.names(expected), info = info)
  expect_identical(class(actual), class(expected), info = info)
  expect_identical(
    names(attributes(actual)),
    names(attributes(expected)),
    info = info
  )
  expect_identical(
    vapply(actual, typeof, character(1L)),
    vapply(expected, typeof, character(1L)),
    info = info
  )
  expect_identical(data.table:::selfrefok(actual, verbose = FALSE), 1L)
  invisible(actual)
}

sampler_bulk_force_fallback = function(sampler) {
  class(sampler) = c("SamplerUnifBulkFallback", class(sampler))
  sampler
}

sampler_bulk_prime = function(sampler) {
  # The fail-closed lane does not force package lazy-load bindings that the
  # historical child calls would force. One zero-row public sample establishes
  # those canonical bindings without creating or advancing `.Random.seed`.
  sampler$sample(0L)
  invisible(sampler)
}

sampler_bulk_public_capture = function(param_set, n, seed, fallback) {
  sampler = SamplerUnif$new(param_set)
  if (fallback) sampler_bulk_force_fallback(sampler)
  sampler_bulk_prime(sampler)
  set.seed(seed)
  value = tryCatch(sampler$sample(n), error = identity)
  list(value = value, seed = .Random.seed)
}

sampler_bulk_expect_public_match = function(param_set, n, seed, info = NULL) {
  actual = sampler_bulk_public_capture(param_set, n, seed, FALSE)
  expected = sampler_bulk_public_capture(param_set, n, seed, TRUE)

  expect_identical(class(actual$value), class(expected$value), info = info)
  expect_identical(actual$seed, expected$seed, info = info)
  if (inherits(expected$value, "error")) {
    expect_identical(
      conditionMessage(actual$value),
      conditionMessage(expected$value),
      info = info
    )
  } else {
    sampler_bulk_expect_table(actual$value$data, expected$value$data, info)
    expect_identical(
      class(actual$value$param_set),
      class(expected$value$param_set),
      info = info
    )
    expect_identical(
      actual$value$param_set$ids(),
      expected$value$param_set$ids(),
      info = info
    )
    expect_identical(
      actual$value$param_set$values,
      expected$value$param_set$values,
      info = info
    )
  }
  invisible(actual$value)
}

sampler_bulk_expect_decline = function(sampler, info = NULL) {
  set.seed(8137L)
  before = .Random.seed
  expect_null(sampler_bulk_call(sampler, 5L), info = info)
  expect_identical(.Random.seed, before, info = info)
  invisible(sampler)
}

sampler_bulk_save_seed = function() {
  present = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  list(
    present = present,
    value = if (present) {
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
      NULL
    }
  )
}

sampler_bulk_remove_seed_binding = function() {
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    if (bindingIsLocked(".Random.seed", .GlobalEnv)) {
      unlockBinding(".Random.seed", .GlobalEnv)
    }
    rm(list = ".Random.seed", envir = .GlobalEnv)
  }
  invisible(NULL)
}

sampler_bulk_restore_seed = function(saved) {
  sampler_bulk_remove_seed_binding()
  if (saved$present) {
    assign(".Random.seed", saved$value, envir = .GlobalEnv)
  }
  invisible(NULL)
}

test_that("the direct bulk sampler matches one column-major uniform draw", {
  sampler = SamplerUnif$new(sampler_bulk_mixed_space())
  sampler_bulk_prime(sampler)

  for (n in c(0L, 1L, 17L)) {
    for (seed in c(1L, 20260716L)) {
      set.seed(seed)
      expected = sampler_bulk_oracle(sampler, n)
      expected_seed = .Random.seed

      set.seed(seed)
      actual = sampler_bulk_call(sampler, n)
      actual_seed = .Random.seed

      info = sprintf("n=%d seed=%d", n, seed)
      expect_false(is.null(actual), info = info)
      sampler_bulk_expect_table(actual, expected, info)
      expect_identical(actual_seed, expected_seed, info = info)
    }
  }
})

test_that("the public bulk lane exactly retains SamplerUnif behavior", {
  spaces = list(
    mixed = sampler_bulk_mixed_space(),
    dependent = sampler_bulk_dependent_space()
  )
  for (space_name in names(spaces)) {
    for (n in c(0L, 1L, 17L)) {
      sampler_bulk_expect_public_match(
        spaces[[space_name]],
        n,
        9091L,
        sprintf("%s n=%d", space_name, n)
      )
    }
  }
})

test_that("fixed values and collections keep the complete public fallback", {
  fixed = sampler_bulk_mixed_space()
  fixed$values = list(double = 1.25, factor = "a")
  collection = ParamSetCollection$new(list(
    left = fixed,
    right = sampler_bulk_dependent_space()
  ))

  for (space_name in c("fixed", "collection")) {
    space = get(space_name)
    for (n in c(0L, 1L, 11L)) {
      sampler_bulk_expect_public_match(
        space,
        n,
        781L,
        sprintf("%s n=%d", space_name, n)
      )
    }
  }
})

test_that("all built-in uniform RNG kinds retain values and streams", {
  saved_seed = sampler_bulk_save_seed()
  old_kind = RNGkind()
  on.exit({
    suppressWarnings(do.call(RNGkind, as.list(old_kind)))
    sampler_bulk_restore_seed(saved_seed)
  }, add = TRUE)

  kinds = c(
    "Wichmann-Hill",
    "Marsaglia-Multicarry",
    "Super-Duper",
    "Mersenne-Twister",
    "Knuth-TAOCP",
    "Knuth-TAOCP-2002",
    "L'Ecuyer-CMRG"
  )
  sampler = SamplerUnif$new(sampler_bulk_mixed_space())
  sampler_bulk_prime(sampler)
  for (kind in kinds) {
    suppressWarnings(RNGkind(
      kind,
      normal.kind = "Inversion",
      sample.kind = "Rejection"
    ))
    set.seed(1801L)
    expected = sampler_bulk_oracle(sampler, 9L)
    expected_seed = .Random.seed

    set.seed(1801L)
    actual = sampler_bulk_call(sampler, 9L)
    actual_seed = .Random.seed

    expect_false(is.null(actual), info = kind)
    sampler_bulk_expect_table(actual, expected, kind)
    expect_identical(actual_seed, expected_seed, info = kind)
  }
})

test_that("an aliased ordinary seed declines before consuming randomness", {
  sampler = SamplerUnif$new(sampler_bulk_mixed_space())
  sampler_bulk_prime(sampler)
  set.seed(7341L)
  alias = .Random.seed
  before = serialize(.Random.seed, NULL)

  expect_null(sampler_bulk_call(sampler, 5L))
  expect_identical(.Random.seed, alias)
  expect_identical(serialize(.Random.seed, NULL), before)
})

test_that("fresh, deep-cloned, and serialized exact samplers are admitted", {
  fresh = SamplerUnif$new(sampler_bulk_mixed_space())
  variants = list(
    fresh = fresh,
    deep = fresh$clone(deep = TRUE),
    serialized = unserialize(serialize(fresh, NULL))
  )

  for (variant_name in names(variants)) {
    sampler = variants[[variant_name]]
    sampler_bulk_prime(sampler)
    set.seed(212L)
    expected = sampler_bulk_oracle(sampler, 7L)
    expected_seed = .Random.seed
    set.seed(212L)
    actual = sampler_bulk_call(sampler, 7L)
    expect_false(is.null(actual), info = variant_name)
    sampler_bulk_expect_table(actual, expected, variant_name)
    expect_identical(.Random.seed, expected_seed, info = variant_name)
  }
})

test_that("altered child state fails closed before consuming randomness", {
  reordered = SamplerUnif$new(sampler_bulk_mixed_space())
  sampler_bulk_prime(reordered)
  reordered$samplers = rev(reordered$samplers)
  sampler_bulk_expect_decline(reordered, "reordered children")

  changed_bounds = SamplerUnif$new(sampler_bulk_mixed_space())
  sampler_bulk_prime(changed_bounds)
  child_params = changed_bounds$samplers[[1L]]$param_set$.__enclos_env__$private$.params
  data.table::set(child_params, j = "lower", value = -100)
  data.table::set(child_params, j = "upper", value = 100)
  sampler_bulk_expect_decline(changed_bounds, "changed child bounds")

  changed_values = SamplerUnif$new(sampler_bulk_mixed_space())
  sampler_bulk_prime(changed_values)
  changed_values$samplers[[1L]]$param_set$values = list(double = 0.5)
  sampler_bulk_expect_decline(changed_values, "changed child values")

  changed_deps = SamplerUnif$new(sampler_bulk_mixed_space())
  sampler_bulk_prime(changed_deps)
  changed_deps$samplers[[1L]]$param_set$deps = data.table::data.table(
    id = "double",
    on = "double",
    cond = list(CondEqual(0))
  )
  sampler_bulk_expect_decline(changed_deps, "changed child dependencies")
})

test_that("altered child execution wrappers fail closed without callbacks", {
  public_calls = 0L
  changed_public = SamplerUnif$new(sampler_bulk_mixed_space())
  sampler_bulk_prime(changed_public)
  child = changed_public$samplers[[1L]]
  inherited = child$sample
  unlockBinding("sample", child)
  assign(
    "sample",
    function(n) {
      public_calls <<- public_calls + 1L
      inherited(n)
    },
    envir = child
  )
  lockBinding("sample", child)
  sampler_bulk_expect_decline(changed_public, "public sample")
  expect_identical(public_calls, 0L)

  private_calls = 0L
  changed_private = SamplerUnif$new(sampler_bulk_mixed_space())
  sampler_bulk_prime(changed_private)
  private = changed_private$samplers[[1L]]$.__enclos_env__$private
  inherited = private$.sample
  unlockBinding(".sample", private)
  assign(
    ".sample",
    function(n) {
      private_calls <<- private_calls + 1L
      inherited(n)
    },
    envir = private
  )
  lockBinding(".sample", private)
  sampler_bulk_expect_decline(changed_private, "private sample")
  expect_identical(private_calls, 0L)

  active_calls = 0L
  changed_active = SamplerUnif$new(sampler_bulk_mixed_space())
  sampler_bulk_prime(changed_active)
  child = changed_active$samplers[[1L]]
  stored_param = child$param_set
  unlockBinding("param", child)
  makeActiveBinding(
    "param",
    function(value) {
      if (!missing(value)) stop("param is read-only")
      active_calls <<- active_calls + 1L
      stored_param
    },
    child
  )
  sampler_bulk_expect_decline(changed_active, "active param")
  expect_identical(active_calls, 0L)
})

test_that("malformed and delayed sampler surfaces decline without forcing", {
  malformed = SamplerUnif$new(sampler_bulk_mixed_space())
  sampler_bulk_prime(malformed)
  class(malformed$samplers) = c("SamplerAuditList", "list")
  sampler_bulk_expect_decline(malformed, "classed sampler list")

  delayed = SamplerUnif$new(sampler_bulk_mixed_space())
  sampler_bulk_prime(delayed)
  child = delayed$samplers[[1L]]
  inherited = child$sample
  state = new.env(parent = emptyenv())
  state$forced = 0L
  evaluation_environment = new.env(parent = baseenv())
  evaluation_environment$inherited = inherited
  evaluation_environment$state = state
  unlockBinding("sample", child)
  delayedAssign(
    "sample",
    {
      state$forced = state$forced + 1L
      inherited
    },
    assign.env = child,
    eval.env = evaluation_environment
  )
  lockBinding("sample", child)

  sampler_bulk_expect_decline(delayed, "delayed public sample")
  expect_identical(state$forced, 0L)
})

sampler_bulk_run_active_seed = function(sampler, seed, direct = FALSE) {
  sampler_bulk_remove_seed_binding()
  state = new.env(parent = emptyenv())
  state$seed = seed
  state$reads = 0L
  state$writes = 0L
  makeActiveBinding(
    ".Random.seed",
    function(value) {
      if (missing(value)) {
        state$reads = state$reads + 1L
        state$seed
      } else {
        state$writes = state$writes + 1L
        state$seed = value
        invisible(NULL)
      }
    },
    .GlobalEnv
  )
  on.exit(sampler_bulk_remove_seed_binding(), add = TRUE)

  declined = if (direct) sampler_bulk_call(sampler, 3L) else NULL
  reads_after_direct = state$reads
  writes_after_direct = state$writes
  value = tryCatch(sampler$sample(3L), error = identity)
  list(
    declined = declined,
    reads_after_direct = reads_after_direct,
    writes_after_direct = writes_after_direct,
    value = value,
    seed = state$seed,
    reads = state$reads,
    writes = state$writes
  )
}

sampler_bulk_run_locked_seed = function(sampler, seed, direct = FALSE) {
  sampler_bulk_remove_seed_binding()
  assign(".Random.seed", seed, envir = .GlobalEnv)
  lockBinding(".Random.seed", .GlobalEnv)
  on.exit(sampler_bulk_remove_seed_binding(), add = TRUE)

  declined = if (direct) sampler_bulk_call(sampler, 3L) else NULL
  seed_after_direct = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  value = tryCatch(sampler$sample(3L), error = identity)
  list(
    declined = declined,
    seed_after_direct = seed_after_direct,
    value = value,
    seed = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  )
}

sampler_bulk_run_delayed_seed = function(sampler, seed, direct = FALSE) {
  sampler_bulk_remove_seed_binding()
  state = new.env(parent = emptyenv())
  state$forced = 0L
  evaluation_environment = new.env(parent = baseenv())
  evaluation_environment$seed = seed
  evaluation_environment$state = state
  delayedAssign(
    ".Random.seed",
    {
      state$forced = state$forced + 1L
      seed
    },
    assign.env = .GlobalEnv,
    eval.env = evaluation_environment
  )
  on.exit(sampler_bulk_remove_seed_binding(), add = TRUE)

  declined = if (direct) sampler_bulk_call(sampler, 3L) else NULL
  forced_after_direct = state$forced
  value = tryCatch(sampler$sample(3L), error = identity)
  list(
    declined = declined,
    forced_after_direct = forced_after_direct,
    value = value,
    seed = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
    forced = state$forced
  )
}

test_that("nonordinary RNG bindings retain the historical fallback", {
  saved_seed = sampler_bulk_save_seed()
  on.exit(sampler_bulk_restore_seed(saved_seed), add = TRUE)
  set.seed(661L)
  initial_seed = .Random.seed

  fast = SamplerUnif$new(sampler_bulk_mixed_space())
  sampler_bulk_prime(fast)
  reference = sampler_bulk_force_fallback(
    SamplerUnif$new(sampler_bulk_mixed_space())
  )
  sampler_bulk_prime(reference)

  active = sampler_bulk_run_active_seed(fast, initial_seed, direct = TRUE)
  active_reference = sampler_bulk_run_active_seed(reference, initial_seed)
  expect_null(active$declined)
  expect_identical(active$reads_after_direct, 0L)
  expect_identical(active$writes_after_direct, 0L)
  sampler_bulk_expect_table(active$value$data, active_reference$value$data)
  expect_identical(active$seed, active_reference$seed)
  expect_identical(active$reads, active_reference$reads)
  expect_identical(active$writes, active_reference$writes)
  expect_gt(active$reads, 0L)
  expect_gt(active$writes, 0L)

  locked = sampler_bulk_run_locked_seed(fast, initial_seed, direct = TRUE)
  locked_reference = sampler_bulk_run_locked_seed(reference, initial_seed)
  expect_null(locked$declined)
  expect_identical(locked$seed_after_direct, initial_seed)
  expect_s3_class(locked$value, "error")
  expect_identical(class(locked$value), class(locked_reference$value))
  expect_identical(
    conditionMessage(locked$value),
    conditionMessage(locked_reference$value)
  )
  expect_identical(locked$seed, initial_seed)
  expect_identical(locked$seed, locked_reference$seed)

  delayed = sampler_bulk_run_delayed_seed(fast, initial_seed, direct = TRUE)
  delayed_reference = sampler_bulk_run_delayed_seed(reference, initial_seed)
  expect_null(delayed$declined)
  expect_identical(delayed$forced_after_direct, 0L)
  expect_identical(delayed$forced, 1L)
  expect_identical(delayed$forced, delayed_reference$forced)
  sampler_bulk_expect_table(delayed$value$data, delayed_reference$value$data)
  expect_identical(delayed$seed, delayed_reference$seed)

  sampler_bulk_restore_seed(saved_seed)
  if (saved_seed$present) {
    expect_false(bindingIsActive(".Random.seed", .GlobalEnv))
    expect_false(bindingIsLocked(".Random.seed", .GlobalEnv))
    expect_identical(.Random.seed, saved_seed$value)
  } else {
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  }
})
