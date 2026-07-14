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
