with_restored_rng_state = function(code) {
  rng_env = globalenv()
  had_rng_state = exists(".Random.seed", envir = rng_env, inherits = FALSE)
  rng_state = if (had_rng_state) get(".Random.seed", envir = rng_env, inherits = FALSE)
  rng_kind = RNGkind()
  on.exit({
    do.call(RNGkind, as.list(rng_kind))
    if (had_rng_state) {
      assign(".Random.seed", rng_state, envir = rng_env)
    } else if (exists(".Random.seed", envir = rng_env, inherits = FALSE)) {
      rm(".Random.seed", envir = rng_env)
    }
  })
  force(code)
}

test_that("TuneToken ParamSet validation preserves an initialized RNG", {
  with_restored_rng_state({
    RNGkind("L'Ecuyer-CMRG", "Box-Muller", "Rejection")
    set.seed(20260713L)
    rng_kind = RNGkind()
    rng_state = .Random.seed

    target = ps(target = p_dbl(0, 1))
    candidate = ps(x = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) list(x$x))
    expect_s3_class(target$search_space(list(target = to_tune(candidate))), "ParamSet")

    expect_identical(RNGkind(), rng_kind)
    expect_identical(.Random.seed, rng_state)
  })
})

test_that("TuneToken ParamSet validation leaves an uninitialized RNG absent", {
  with_restored_rng_state({
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }

    target = ps(target = p_dbl(0, 1))
    invalid = ps(x = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) list(x$x, x$x))
    expect_error(
      target$search_space(list(target = to_tune(invalid))),
      "does not have a trafo that reduces output to one dimension",
      fixed = TRUE
    )

    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  })
})

test_that("TuneToken ParamSet plausibility validation is repeatable", {
  validation_message = function(seed, rng_kind) {
    RNGkind(rng_kind, "Inversion", "Rejection")
    set.seed(seed)
    target = ps(target = p_dbl(0, 0.4))
    invalid = ps(x = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) list(x$x))
    conditionMessage(expect_error(target$search_space(list(target = to_tune(invalid)))))
  }

  with_restored_rng_state({
    message_mt = validation_message(1L, "Mersenne-Twister")
    message_lecuyer = validation_message(999L, "L'Ecuyer-CMRG")

    expect_identical(message_mt, message_lecuyer)
    expect_match(message_mt, "generates points that are not compatible", fixed = TRUE)
    expect_match(message_mt, "Bad value", fixed = TRUE)
  })
})

test_that("restoring the caller's RNG kind is silent and always completes", {
  # RNGkind() warns unconditionally for the legacy non-uniform samplers. The
  # restore is the package's own bookkeeping, and under options(warn = 2) the
  # warning used to abort the exit handler before the seed was put back.
  previous_kind = RNGkind()
  on.exit(suppressWarnings(do.call(RNGkind, as.list(previous_kind))), add = TRUE)
  suppressWarnings(RNGkind(sample.kind = "Rounding"))

  set.seed(42L)
  before = .Random.seed
  set = ps(cp = p_dbl(0, 1), minsplit = p_int(1, 100))
  set$values = list(cp = to_tune(1e-4, 0.1), minsplit = to_tune(1, 64))

  expect_silent(search_space <- set$search_space())
  expect_setequal(search_space$ids(), c("cp", "minsplit"))
  expect_identical(RNGkind()[[3L]], "Rounding")
  expect_identical(.Random.seed, before)

  previous_warn = options(warn = 2L)
  on.exit(options(previous_warn), add = TRUE)
  set.seed(42L)
  strict_before = .Random.seed
  expect_setequal(set$search_space()$ids(), c("cp", "minsplit"))
  expect_identical(.Random.seed, strict_before)
})
