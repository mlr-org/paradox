sampler_unif_handoff_symbols = function() {
  namespace = asNamespace("paradox")
  mget(
    paste0("C_", c(
      "sampler_unif_subspace_handoffs",
      "sampler_unif_take_subspace"
    )),
    envir = namespace,
    inherits = FALSE
  )
}

sampler_unif_handoffs = function(param_set, ids = param_set$ids()) {
  symbols = sampler_unif_handoff_symbols()
  .Call(
    symbols$C_sampler_unif_subspace_handoffs,
    param_set,
    ids,
    param_set$extra_trafo
  )
}

test_that("Sampler1DUnif uses the ordinary public R6 constructor contract", {
  namespace = asNamespace("paradox")
  retired = c(
    "C_sampler_1d_unif_bulk_register",
    "C_sampler_1d_unif_bulk_auth",
    "C_sampler_1d_unif_bulk_shells"
  )
  expect_false(any(vapply(
    retired,
    exists,
    logical(1L),
    envir = namespace,
    inherits = FALSE
  )))

  param = ps(x = p_dbl(0, 1))
  param$values = list(x = 0.25)
  sampler = Sampler1DUnif$new(param)
  expect_s3_class(sampler, "Sampler1DUnif")
  expect_s3_class(sampler, "Sampler1D")
  expect_false(identical(sampler$param, param))
  expect_identical(sampler$param$ids(), "x")
  expect_identical(sampler$param$values, list(x = 0.25))

  param$values = list(x = 0.75)
  expect_identical(sampler$param$values, list(x = 0.25))
  expect_error(Sampler1DUnif$new(ps()), "exactly 1 Param")
  expect_error(
    Sampler1DUnif$new(ps(x = p_dbl(), y = p_dbl())),
    "exactly 1 Param"
  )
  expect_error(Sampler1DUnif$new(ps(x = p_dbl(lower = 0))), "bounded")
  expect_error(Sampler1DUnif$new(new("externalptr")), "R6")
})

test_that("Sampler1DUnif never consumes a ParamSet subset capability", {
  namespace = asNamespace("paradox")
  subset_symbol = get("C_param_set_subset_state", envir = namespace)
  adopt_symbol = get("C_param_set_adopt_subset_state", envir = namespace)
  param = ps(x = p_dbl(0, 1))
  token = .Call(
    subset_symbol,
    param$.__enclos_env__$private,
    param,
    "x",
    FALSE,
    TRUE,
    param$constraint,
    param$extra_trafo,
    TRUE
  )

  expect_error(Sampler1DUnif$new(token), "R6")
  expect_true(.Call(adopt_symbol, NULL, token))
  expect_s3_class(Sampler1DUnif$new(ParamSet$new(token)), "Sampler1DUnif")
  expect_false(.Call(adopt_symbol, NULL, token))
})

test_that("SamplerUnif ownership handoffs are private and single-use", {
  symbols = sampler_unif_handoff_symbols()
  expect_identical(
    symbols$C_sampler_unif_subspace_handoffs$numParameters,
    3L
  )
  expect_identical(symbols$C_sampler_unif_take_subspace$numParameters, 1L)

  param_set = ps(x = p_dbl(0, 1), y = p_int(0, 4))
  param_set$values = list(x = 0.25, y = 2L)
  handoffs = sampler_unif_handoffs(param_set)
  expect_identical(names(handoffs), c("x", "y"))
  expect_true(all(vapply(handoffs, typeof, "") == "externalptr"))
  expect_true(all(vapply(
    handoffs,
    function(handoff) is.null(attributes(handoff)),
    logical(1L)
  )))

  x_handoff = handoffs[[1L]]
  child = Sampler1DUnif$new(x_handoff)
  expect_identical(child$param$ids(), "x")
  expect_identical(child$param$values, list(x = 0.25))
  expect_error(
    Sampler1DUnif$new(x_handoff),
    "already consumed internal SamplerUnif subspace handoff",
    fixed = TRUE
  )

  # A carrier cannot survive serialization because its authority is a
  # process-local native address. The still-live original remains consumable.
  y_handoff = handoffs[[2L]]
  restored = unserialize(serialize(y_handoff, NULL))
  expect_error(
    Sampler1DUnif$new(restored),
    "internal SamplerUnif subspace handoff",
    fixed = TRUE
  )
  expect_identical(Sampler1DUnif$new(y_handoff)$param$ids(), "y")
})

test_that("SamplerUnif ownership handoffs reject corruption and fabrication", {
  carrier = sampler_unif_handoffs(ps(x = p_dbl(0, 1)))[[1L]]
  attr(carrier, "forged") = TRUE
  expect_error(
    Sampler1DUnif$new(carrier),
    "malformed.*SamplerUnif subspace handoff"
  )

  # Generic external pointers and even genuine ParamSet subset capabilities
  # are not sampler ownership handoffs and retain the public constructor
  # diagnostic instead of bypassing its defensive clone boundary.
  expect_error(Sampler1DUnif$new(new("externalptr")), "R6")
  namespace = asNamespace("paradox")
  ordinary = ps(x = p_dbl(0, 1))
  token = .Call(
    get("C_param_set_subset_state", envir = namespace),
    ordinary$.__enclos_env__$private,
    ordinary,
    "x",
    FALSE,
    TRUE,
    ordinary$constraint,
    ordinary$extra_trafo,
    TRUE
  )
  expect_error(Sampler1DUnif$new(token), "R6")
})

test_that("SamplerUnif obtains independent singleton ParamSets via subspaces", {
  param_set = ps(
    x = p_dbl(0, 1),
    y = p_int(0, 3),
    flag = p_lgl()
  )
  param_set$values = list(x = 0.5, flag = TRUE)
  sampler = SamplerUnif$new(param_set)

  expect_s3_class(sampler, "SamplerUnif")
  expect_length(sampler$samplers, 3L)
  expect_identical(
    unname(vapply(
      sampler$samplers,
      function(component) component$param$ids(),
      ""
    )),
    c("x", "y", "flag")
  )
  expect_true(all(vapply(
    sampler$samplers,
    function(component) component$param$length == 1L,
    logical(1L)
  )))
  expect_true(all(vapply(
    sampler$samplers,
    function(component) nrow(component$param$deps) == 0L,
    logical(1L)
  )))

  second = sampler$samplers[[2L]]
  second_param = second$param
  second_param$values = list(y = 1L)
  expect_identical(sampler$samplers[[1L]]$param$values, list(x = 0.5))
  expect_identical(sampler$samplers[[2L]]$param$values, list(y = 1L))
  expect_identical(sampler$samplers[[3L]]$param$values, list(flag = TRUE))
  expect_identical(sampler$param_set$values, list(x = 0.5, flag = TRUE))
  second_param$values = named_list()

  param_set$values = list(y = 2L)
  expect_identical(sampler$samplers[[1L]]$param$values, list(x = 0.5))
  expect_identical(sampler$samplers[[2L]]$param$values, named_list())
  expect_identical(sampler$samplers[[3L]]$param$values, list(flag = TRUE))
})
