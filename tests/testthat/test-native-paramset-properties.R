test_that("native ParamSet properties preserve empty vector structure", {
  param_set = ParamSet$new()

  expect_identical(param_set$nlevels, setNames(integer(), character()))
  expect_identical(param_set$is_number, setNames(logical(), character()))
  expect_identical(param_set$is_categ, setNames(logical(), character()))
  expect_identical(param_set$is_bounded, setNames(logical(), character()))

  dll = getLoadedDLLs()[["paradox"]]
  expect_false(dll[["dynamicLookup"]])
  expect_true(inherits(
    get("C_param_set_property", envir = asNamespace("paradox")),
    "NativeSymbolInfo"
  ))
  expect_error(
    .Call("param_set_property", list(), 0L, PACKAGE = "paradox"),
    "not available"
  )
})

test_that("native ParamSet properties equal built-in Domain methods", {
  domains = list(
    fixed_double = p_dbl(4, 4),
    unbounded_double = p_dbl(),
    bounded_integer = p_int(-2, 2),
    half_bounded_integer = p_int(lower = 0),
    factor = p_fct(factor(
      c("low", "medium", "high"),
      levels = c("low", "medium", "high")
    )),
    logical = p_lgl(),
    utility = p_uty(),
    logscale_integer = p_int(1, 16, logscale = TRUE)
  )
  param_set = ParamSet$new(domains)

  reference_property = function(generic) {
    setNames(
      unlist(lapply(domains, generic), use.names = FALSE),
      names(domains)
    )
  }

  expect_identical(param_set$nlevels, reference_property(domain_nlevels))
  expect_identical(param_set$is_number, reference_property(domain_is_number))
  expect_identical(param_set$is_categ, reference_property(domain_is_categ))
  expect_identical(param_set$is_bounded, reference_property(domain_is_bounded))

  expect_identical(
    param_set$nlevels,
    c(
      fixed_double = 1,
      unbounded_double = Inf,
      bounded_integer = 5,
      half_bounded_integer = Inf,
      factor = 3,
      logical = 2,
      utility = Inf,
      logscale_integer = Inf
    )
  )
  expect_identical(
    param_set$is_bounded,
    c(
      fixed_double = TRUE,
      unbounded_double = FALSE,
      bounded_integer = TRUE,
      half_bounded_integer = FALSE,
      factor = TRUE,
      logical = TRUE,
      utility = FALSE,
      logscale_integer = TRUE
    )
  )

  expect_type(param_set$nlevels, "double")
  expect_type(param_set$is_number, "logical")
  expect_type(param_set$is_categ, "logical")
  expect_type(param_set$is_bounded, "logical")
  expect_identical(names(param_set$nlevels), param_set$ids())
  expect_identical(names(param_set$is_number), param_set$ids())
  expect_identical(names(param_set$is_categ), param_set$ids())
  expect_identical(names(param_set$is_bounded), param_set$ids())

  data = param_set$data
  expect_identical(data$nlevels, unname(param_set$nlevels))
  expect_identical(data$is_bounded, unname(param_set$is_bounded))
})

test_that("native ParamSet properties accept canonical integer bound columns", {
  param_set = ParamInt$new("integer_bounds", lower = 1L, upper = 10L)

  expect_type(param_set$params$lower, "integer")
  expect_type(param_set$params$upper, "integer")
  expect_identical(param_set$nlevels, c(integer_bounds = 10))
  expect_identical(param_set$is_number, c(integer_bounds = TRUE))
  expect_identical(param_set$is_categ, c(integer_bounds = FALSE))
  expect_identical(param_set$is_bounded, c(integer_bounds = TRUE))
})

test_that("deferred factor levels retain the grouped property fallback", {
  parameter_set = ps(value = p_fct(c(1, 2, 2.5)))
  private_levels = parameter_set$.__enclos_env__$private$.params$levels[[1L]]

  # as.character(numeric) is a deferred-string ALTREP on current R. The
  # native property kernel must decline it without mistaking valid state for
  # corruption; the ordinary grouped Domain method then computes the result.
  expect_identical(
    .Call(
      get("C_param_set_property", envir = asNamespace("paradox")),
      parameter_set$.__enclos_env__$private$.params,
      0L
    ),
    NULL
  )
  expect_identical(parameter_set$nlevels, c(value = 3))
  expect_identical(
    generate_design_grid(parameter_set)$transpose(),
    list(list(value = 1), list(value = 2), list(value = 2.5))
  )
  expect_type(private_levels, "character")
})

test_that("native ParamSet properties handle input beyond an interrupt interval", {
  size = 65537L
  params = list(
    id = sprintf("parameter_%05d", seq_len(size)),
    cls = rep("ParamInt", size),
    lower = rep(1L, size),
    upper = rep(3L, size),
    levels = rep(list(NULL), size)
  )
  symbol = get("C_param_set_property", envir = asNamespace("paradox"))

  result = .Call(symbol, params, 0L)

  expect_length(result[[1L]], size)
  expect_identical(unname(result[[1L]]), rep(3, size))
  expect_true(all(result[[2L]]))
})

test_that("native ParamSet properties reject corrupt storage and selectors", {
  symbol = get("C_param_set_property", envir = asNamespace("paradox"))
  valid = list(
    id = "x",
    cls = "ParamInt",
    lower = 0,
    upper = 2,
    levels = list(NULL)
  )

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
    .Call(symbol, modifyList(valid, list(id = 1)), 0L),
    "`id` must have type `character`",
    fixed = TRUE
  )
  expect_error(
    .Call(symbol, modifyList(valid, list(cls = character())), 0L),
    "`cls` must have type `character` and length 1",
    fixed = TRUE
  )
  expect_error(
    .Call(symbol, modifyList(valid, list(lower = TRUE)), 0L),
    "`lower` must be numeric and have length 1",
    fixed = TRUE
  )
  expect_error(
    .Call(symbol, modifyList(valid, list(levels = "unused")), 0L),
    "`levels` must have type `list` and length 1",
    fixed = TRUE
  )
  corrupt_factor = modifyList(valid, list(
    cls = "ParamFct",
    levels = list(new.env(parent = emptyenv()))
  ))
  expect_error(
    .Call(symbol, corrupt_factor, 0L),
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

test_that("native ParamSet properties authenticate selectors before Length", {
  symbol = get("C_param_set_property", envir = asNamespace("paradox"))
  valid = list(
    id = "x",
    cls = "ParamInt",
    lower = 0,
    upper = 2,
    levels = list(NULL)
  )
  callbacks = 0L
  selector = native_stateful_altrep(
    0L,
    1L,
    callback = function() {
      callbacks <<- callbacks + 1L
      stop("selector Length callback ran", call. = FALSE)
    },
    callback_after = c(NA_integer_, 0L)
  )

  expect_error(
    .Call(symbol, valid, selector),
    "invalid ParamSet property selector",
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)
})

test_that("unknown Domain classes retain grouped S3 property dispatch", {
  calls = new.env(parent = emptyenv())
  calls$nlevels = integer()
  calls$is_number = integer()
  calls$is_categ = integer()
  calls$is_bounded = integer()

  register_fallback = function(generic, method) {
    registerS3method(
      generic,
      "ParamNativePropertyFallback",
      method,
      envir = asNamespace("paradox")
    )
  }
  register_fallback("domain_nlevels", function(param) {
    calls$nlevels = c(calls$nlevels, nrow(param))
    lengths(param$levels) + 10
  })
  register_fallback("domain_is_number", function(param) {
    calls$is_number = c(calls$is_number, nrow(param))
    FALSE
  })
  register_fallback("domain_is_categ", function(param) {
    calls$is_categ = c(calls$is_categ, nrow(param))
    TRUE
  })
  register_fallback("domain_is_bounded", function(param) {
    calls$is_bounded = c(calls$is_bounded, nrow(param))
    lengths(param$levels) > 1L
  })

  custom_domain = function(levels) {
    paradox:::Domain(
      cls = "ParamNativePropertyFallback",
      grouping = "native-property-fallback",
      levels = levels,
      storage_type = "character"
    )
  }
  param_set = ParamSet$new(list(
    integer = p_int(2, 4),
    custom_two = custom_domain(c("a", "b")),
    utility = p_uty(),
    custom_one = custom_domain("a")
  ))

  expect_identical(
    param_set$nlevels,
    c(integer = 3, custom_two = 12, utility = Inf, custom_one = 11)
  )
  expect_identical(
    param_set$is_number,
    c(integer = TRUE, custom_two = FALSE, utility = FALSE, custom_one = FALSE)
  )
  expect_identical(
    param_set$is_categ,
    c(integer = FALSE, custom_two = TRUE, utility = FALSE, custom_one = TRUE)
  )
  expect_identical(
    param_set$is_bounded,
    c(integer = TRUE, custom_two = TRUE, utility = FALSE, custom_one = FALSE)
  )

  expect_identical(calls$nlevels, 2L)
  expect_identical(calls$is_number, 2L)
  expect_identical(calls$is_categ, 2L)
  expect_identical(calls$is_bounded, 2L)
})

test_that("custom-only static properties retain their S3 return type", {
  class_name = "ParamNativePropertyIntegerFallback"
  registerS3method(
    "domain_nlevels",
    class_name,
    function(param) rep.int(2L, nrow(param)),
    envir = asNamespace("paradox")
  )

  p_custom_integer_fallback = function() {
    paradox:::Domain(
      cls = class_name,
      grouping = class_name,
      storage_type = "character"
    )
  }
  domain = p_custom_integer_fallback()
  parameter_set = ParamSet$new(list(custom = domain))

  expect_identical(parameter_set$nlevels, c(custom = 2L))
  expect_type(parameter_set$nlevels, "integer")
})

test_that("custom grouped static properties retain upstream S3 vectors", {
  class_name = "AuditPropVector"
  registerS3method(
    "domain_nlevels",
    class_name,
    function(param) structure(seq_len(nrow(param)), class = "audit_vec"),
    envir = asNamespace("paradox")
  )

  make_domain = function() {
    paradox:::Domain(
      cls = class_name,
      grouping = "shared-audit-property-group",
      storage_type = "character"
    )
  }
  parameter_set = ParamSet$new(list(
    first = make_domain(),
    second = make_domain()
  ))

  observed = parameter_set$nlevels
  expect_s3_class(observed, "audit_vec")
  expect_identical(unclass(observed), c(first = 1L, second = 2L))
})

test_that("custom static property errors retain the upstream dispatch call", {
  class_name = "AuditPropError"
  registerS3method(
    "domain_nlevels",
    class_name,
    function(param) stop("audit property sentinel"),
    envir = asNamespace("paradox")
  )
  make_domain = function() {
    paradox:::Domain(
      cls = class_name,
      grouping = class_name,
      storage_type = "character"
    )
  }
  parameter_set = ParamSet$new(list(value = make_domain()))

  condition = tryCatch(parameter_set$nlevels, error = identity)
  expect_s3_class(condition, "error")
  expect_identical(
    conditionCall(condition),
    quote(domain_nlevels.AuditPropError(recover_domain(.SD)))
  )
})
