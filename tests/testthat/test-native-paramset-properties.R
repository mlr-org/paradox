property_symbol = function() {
  get("C_param_set_property", envir = asNamespace("paradox"), inherits = FALSE)
}

property_params = function(param_set) {
  paradox:::param_set_core_state(param_set$.__enclos_env__$private)$.params
}

test_that("static properties use one fixed native capsule interface", {
  symbol = property_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])

  empty = ParamSet$new()
  expect_identical(empty$nlevels, setNames(integer(), character()))
  expect_identical(empty$is_number, setNames(logical(), character()))
  expect_identical(empty$is_categ, setNames(logical(), character()))
  expect_identical(empty$is_bounded, setNames(logical(), character()))
})

test_that("native static properties cover the five maintained Domain kinds", {
  domains = list(
    fixed_double = p_dbl(4, 4),
    unbounded_double = p_dbl(),
    bounded_integer = p_int(-2, 2),
    half_bounded_integer = p_int(lower = 0),
    factor = p_fct(c("low", "medium", "high")),
    logical = p_lgl(),
    utility = p_uty(),
    logscale_integer = p_int(1, 16, logscale = TRUE)
  )
  param_set = ParamSet$new(domains)

  expect_identical(param_set$nlevels, c(
    fixed_double = 1,
    unbounded_double = Inf,
    bounded_integer = 5,
    half_bounded_integer = Inf,
    factor = 3,
    logical = 2,
    utility = Inf,
    logscale_integer = Inf
  ))
  expect_identical(param_set$is_number, c(
    fixed_double = TRUE,
    unbounded_double = TRUE,
    bounded_integer = TRUE,
    half_bounded_integer = TRUE,
    factor = FALSE,
    logical = FALSE,
    utility = FALSE,
    logscale_integer = TRUE
  ))
  expect_identical(param_set$is_categ, c(
    fixed_double = FALSE,
    unbounded_double = FALSE,
    bounded_integer = FALSE,
    half_bounded_integer = FALSE,
    factor = TRUE,
    logical = TRUE,
    utility = FALSE,
    logscale_integer = FALSE
  ))
  expect_identical(param_set$is_bounded, c(
    fixed_double = TRUE,
    unbounded_double = FALSE,
    bounded_integer = TRUE,
    half_bounded_integer = FALSE,
    factor = TRUE,
    logical = TRUE,
    utility = FALSE,
    logscale_integer = TRUE
  ))

  for (selector in 0:3) {
    observed = .Call(property_symbol(), property_params(param_set), selector)
    expect_identical(observed, list(
      param_set$nlevels,
      param_set$is_number,
      param_set$is_categ,
      param_set$is_bounded
    )[[selector + 1L]])
  }
})

test_that("numeric factor levels are materialized once at admission", {
  parameter_set = ps(value = p_fct(c(1, 2, 2.5)))
  levels = property_params(parameter_set)$levels[[1L]]

  expect_type(levels, "character")
  expect_identical(parameter_set$nlevels, c(value = 3))
  expect_identical(
    generate_design_grid(parameter_set)$transpose(),
    list(list(value = 1), list(value = 2), list(value = 2.5))
  )
})

test_that("native static properties handle an interrupt-sized table under forced collection", {
  skip_on_cran()

  size = 65537L
  params = list(
    id = sprintf("parameter_%05d", seq_len(size)),
    cls = rep("ParamInt", size),
    lower = rep(1L, size),
    upper = rep(3L, size),
    levels = rep(list(NULL), size)
  )

  symbol = property_symbol()
  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  nlevels = .Call(symbol, params, 0L)
  is_number = .Call(symbol, params, 1L)
  is_categ = .Call(symbol, params, 2L)
  is_bounded = .Call(symbol, params, 3L)
  gctorture(previous)

  expect_length(nlevels, size)
  expect_identical(unname(nlevels), rep(3, size))
  expect_identical(unname(is_number), rep(TRUE, size))
  expect_identical(unname(is_categ), rep(FALSE, size))
  expect_identical(unname(is_bounded), rep(TRUE, size))
})

test_that("static properties reject corrupt state instead of dispatching", {
  valid = list(
    id = "x",
    cls = "ParamInt",
    lower = 0,
    upper = 2,
    levels = list(NULL)
  )
  symbol = property_symbol()

  expect_error(.Call(symbol, 1, 0L), "`.params` must be a list", fixed = TRUE)
  expect_error(
    .Call(symbol, unname(valid), 0L),
    "`.params` must be a named list",
    fixed = TRUE
  )
  expect_error(
    .Call(symbol, within(valid, rm(lower)), 0L),
    "`.params` has no `lower` column",
    fixed = TRUE
  )
  expect_error(
    .Call(symbol, modifyList(valid, list(cls = "ParamThirdParty")), 0L),
    "unsupported parameter class",
    fixed = TRUE
  )
  expect_error(
    .Call(symbol, modifyList(valid, list(
      cls = "ParamFct",
      levels = list(new.env(parent = emptyenv()))
    )), 0L),
    "each `levels` element for `ParamFct` must be character",
    fixed = TRUE
  )

  for (selector in list(0, NA_integer_, -1L, 4L, 0:1)) {
    expect_error(
      .Call(symbol, valid, selector),
      "invalid ParamSet property selector",
      fixed = TRUE
    )
  }
})

test_that("static properties reject semantic ALTREP without observing it", {
  callbacks = 0L
  levels = native_stateful_altrep(
    c("a", "b"),
    "changed",
    elt_switch_after = 0L,
    length_switch_after = 0L,
    callback = function() {
      callbacks <<- callbacks + 1L
      stop("ALTREP provider ran", call. = FALSE)
    },
    callback_after = 0L
  )
  params = list(
    id = "x",
    cls = "ParamFct",
    lower = NA_real_,
    upper = NA_real_,
    levels = list(levels)
  )

  expect_error(
    .Call(property_symbol(), params, 0L),
    "factor levels must use ordinary representations",
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)
})
