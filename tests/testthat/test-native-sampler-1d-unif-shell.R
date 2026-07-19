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

  param_set$values = list(y = 2L)
  expect_identical(sampler$samplers[[1L]]$param$values, list(x = 0.5))
  expect_identical(sampler$samplers[[2L]]$param$values, named_list())
  expect_identical(sampler$samplers[[3L]]$param$values, list(flag = TRUE))
})
