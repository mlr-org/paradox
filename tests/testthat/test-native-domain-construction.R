domain2_symbol = function(name) {
  get(paste0("C_", name), envir = asNamespace("paradox"), inherits = FALSE)
}

domain2_construct = function(cls, grouping, cargo, lower, upper, tolerance,
    levels, special_vals, default_value, tags, trafo, storage_type,
    init_given, init_value, numeric_source_kind, numeric_logscale, id,
    requirements) {
  .Call(
    domain2_symbol("domain_construct"),
    cls,
    grouping,
    cargo,
    lower,
    upper,
    tolerance,
    levels,
    special_vals,
    default_value,
    tags,
    trafo,
    storage_type,
    init_given,
    init_value,
    numeric_source_kind,
    numeric_logscale,
    id,
    requirements
  )
}

domain2_args = function(domain) {
  list(
    cls = domain$cls,
    grouping = domain$grouping,
    cargo = domain$cargo[[1L]],
    lower = domain$lower,
    upper = domain$upper,
    tolerance = domain$tolerance,
    levels = domain$levels[[1L]],
    special_vals = domain$special_vals[[1L]],
    default_value = domain$default[[1L]],
    tags = domain$.tags[[1L]],
    trafo = domain$.trafo[[1L]],
    storage_type = domain$storage_type,
    init_given = domain$.init_given,
    init_value = domain$.init[[1L]],
    numeric_source_kind = switch(
      domain$cls[[1L]],
      ParamDbl = 1L,
      ParamInt = 2L,
      0L
    ),
    numeric_logscale = FALSE,
    id = domain$id,
    requirements = domain$.requirements[[1L]]
  )
}

domain2_construct_domain = function(domain) {
  do.call(domain2_construct, domain2_args(domain))
}

domain2_replace = function(arguments, name, value) {
  # Single-bracket replacement keeps an explicit NULL argument in the call.
  # `[[<- NULL` would delete the element and accidentally test a missing
  # promise in do.call() instead of native NULL admission.
  arguments[name] = list(value)
  arguments
}

domain2_altrep_helpers_available = function() {
  exists(
    "C_test_stateful_altrep",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

test_that("Domain construction uses fixed registered native interfaces", {
  arities = c(
    domain_construct = 18L,
    domain_uty_check_result = 1L,
    domain_simple_repr_id = 1L
  )
  for (name in names(arities)) {
    symbol = domain2_symbol(name)
    expect_s3_class(symbol, "NativeSymbolInfo")
    expect_identical(symbol$numParameters, arities[[name]], info = name)
  }
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
  expect_error(
    .Call("domain_construct", PACKAGE = "paradox"),
    "not available",
    fixed = TRUE
  )
})

test_that("all five public Domain kinds use the canonical native row", {
  marker = new.env(parent = emptyenv())
  transform = function(x) exp(x)
  domains = list(
    double = p_dbl(-2, 3, tags = c("numeric", "bounded"), trafo = transform),
    integer = p_int(-2L, 3L, tolerance = 0, tags = "integer", init = 1L),
    factor = p_fct(c("small", "large"), default = "small", tags = "choice"),
    logical = p_lgl(default = FALSE, tags = "flag"),
    utility = p_uty(default = marker, tags = "payload")
  )
  classes = c(
    double = "ParamDbl",
    integer = "ParamInt",
    factor = "ParamFct",
    logical = "ParamLgl",
    utility = "ParamUty"
  )
  storage = c(
    double = "numeric",
    integer = "integer",
    factor = "character",
    logical = "logical",
    utility = "list"
  )

  for (name in names(domains)) {
    domain = domains[[name]]
    expect_identical(
      class(domain),
      c(classes[[name]], "Domain", "data.table", "data.frame")
    )
    expect_identical(names(domain), paradox:::domain_names)
    expect_identical(dim(domain), c(1L, 16L))
    expect_identical(domain$storage_type, storage[[name]])
    expect_identical(
      domain$id,
      deparse1(attr(domain, "repr"), collapse = "\n", width.cutoff = 80)
    )
    expect_identical(data.table:::selfrefok(domain, FALSE), 1L)

    native = domain2_construct_domain(domain)
    expect_type(native, "list")
    expect_identical(
      class(native),
      c(classes[[name]], "Domain", "data.table", "data.frame")
    )
    expect_identical(names(native), paradox:::domain_names)
    expect_identical(dim(native), c(1L, 16L))
    expect_identical(data.table:::selfrefok(native, FALSE), 1L)
    expect_null(attr(native, "repr", exact = TRUE))
    expect_identical(native$id, domain$id)
    expect_identical(native$.requirements, domain$.requirements)
    for (field in names(native)) {
      expect_identical(native[[field]], domain[[field]], info = paste(name, field))
    }
  }

  expect_identical(domains$double$.trafo[[1L]], transform)
  expect_identical(domains$utility$default[[1L]], marker)
})

test_that("one native constructor owns numeric admission and logscale state", {
  double = domain2_args(p_dbl(1, exp(2)))
  double$numeric_logscale = TRUE
  mapped_double = do.call(domain2_construct, double)
  expect_identical(mapped_double$lower, 0)
  expect_equal(mapped_double$upper, 2)
  expect_identical(mapped_double$cargo, list(list(logscale = TRUE)))
  expect_identical(mapped_double$.trafo[[1L]], exp)

  integer = domain2_args(p_int(0L, 9L, tolerance = 0))
  integer$numeric_logscale = TRUE
  mapped_integer = do.call(domain2_construct, integer)
  expect_identical(mapped_integer$cls, "ParamDbl")
  expect_identical(mapped_integer$grouping, "ParamDbl")
  expect_identical(mapped_integer$storage_type, "numeric")
  expect_equal(mapped_integer$lower, log(0.5))
  expect_equal(mapped_integer$upper, log(10))
  expect_identical(mapped_integer$cargo, list(list(logscale = TRUE)))
  expect_identical(mapped_integer$.trafo[[1L]](mapped_integer$lower), 0L)
  expect_identical(mapped_integer$.trafo[[1L]](mapped_integer$upper), 9L)

  public_double = p_dbl(1, exp(2), logscale = TRUE)
  public_integer = p_int(0L, 9L, tolerance = 0, logscale = TRUE)
  expect_equal(public_double$upper, 2)
  expect_identical(public_integer$cls, "ParamDbl")
  expect_equal(public_integer$upper, log(10))
})

test_that("numeric constructor rejects malformed direct and public bounds", {
  double = domain2_args(p_dbl(-2, 3))
  integer = domain2_args(p_int(-2L, 3L, tolerance = 0.5))

  expect_error(
    do.call(domain2_construct, domain2_replace(double, "tolerance", -1)),
    "non-negative number"
  )
  expect_error(
    do.call(domain2_construct, domain2_replace(integer, "tolerance", 0.6)),
    "between 0 and 0.5"
  )
  expect_error(
    do.call(domain2_construct, domain2_replace(double, "lower", NULL)),
    "Invalid built-in Domain state|`lower` must be one number"
  )
  expect_error(
    do.call(domain2_construct, domain2_replace(integer, "lower", 0.5)),
    "integer-valued number"
  )
  expect_error(
    do.call(domain2_construct, domain2_replace(double, "lower", 4)),
    "must not be greater"
  )

  log_double = double
  log_double$numeric_logscale = TRUE
  expect_error(
    do.call(domain2_construct, domain2_replace(log_double, "lower", 0)),
    "strictly greater than 0"
  )
  log_integer = integer
  log_integer$numeric_logscale = TRUE
  expect_error(
    do.call(domain2_construct, domain2_replace(log_integer, "lower", -1L)),
    "greater or equal 0"
  )
  expect_error(
    do.call(domain2_construct, domain2_replace(double, "numeric_source_kind", 1)),
    "numeric admission flags"
  )
  expect_error(
    do.call(domain2_construct, domain2_replace(double, "numeric_logscale", NA)),
    "numeric admission flags"
  )

  expect_error(p_dbl(tolerance = -1), "non-negative number")
  expect_error(p_int(tolerance = 0.6), "between 0 and 0.5")
  expect_error(p_dbl(lower = 2, upper = 1), "must not be greater")
  expect_error(p_dbl(logscale = TRUE), "strictly greater than 0")
  expect_error(
    p_int(lower = -1, logscale = TRUE),
    "greater or equal 0"
  )
})

test_that("scalar Domain constructor names are representation-only", {
  lower = c(parameter = 0)
  upper = c(parameter = 10)
  tolerance = c(parameter = 0.125)
  tags = c(source = "bounded")

  domain = p_dbl(
    lower = lower,
    upper = upper,
    tolerance = tolerance,
    tags = tags
  )

  expect_identical(domain$lower, 0)
  expect_identical(domain$upper, 10)
  expect_identical(domain$tolerance, 0.125)
  expect_identical(domain$.tags[[1L]], "bounded")
  expect_null(names(domain$lower))
  expect_null(names(domain$upper))
  expect_null(names(domain$tolerance))
  expect_null(names(domain$.tags[[1L]]))

  args = domain2_args(p_int(0L, 10L, tolerance = 0))
  args$cls = c(parameter = args$cls)
  args$grouping = c(parameter = args$grouping)
  args$lower = c(parameter = args$lower)
  args$upper = c(parameter = args$upper)
  args$tolerance = c(parameter = args$tolerance)
  args$storage_type = c(parameter = args$storage_type)
  args$init_given = c(parameter = args$init_given)
  args$numeric_source_kind = c(parameter = args$numeric_source_kind)
  args$numeric_logscale = c(parameter = args$numeric_logscale)
  observed = do.call(domain2_construct, args)
  expect_identical(observed$lower, 0L)
  expect_identical(observed$upper, 10L)
})

test_that("typed Domain constructors reject S4 structure but retain exact specials", {
  structural_cases = list(
    lower = function() p_dbl(lower = asS4(0)),
    upper = function() p_dbl(upper = asS4(1)),
    tags = function() p_dbl(tags = asS4("numeric")),
    levels = function() p_fct(asS4(c("a", "b"))),
    special_vals = function() p_dbl(special_vals = asS4(list(Inf)))
  )
  for (case in names(structural_cases)) {
    expect_error(
      structural_cases[[case]](),
      "structural constructor fields|canonical",
      info = case
    )
  }

  expect_error(
    p_dbl(0, 1, default = asS4(0.5)),
    "Must be of type 'number', not 'double'",
    fixed = TRUE
  )
  expect_error(
    p_int(0L, 2L, init = asS4(1L)),
    "Must be of type 'single integerish value', not 'integer'",
    fixed = TRUE
  )

  special_default = asS4(0.5)
  special_init = asS4(0.75)
  domain = p_dbl(
    0,
    1,
    special_vals = list(special_default, special_init),
    default = special_default,
    init = special_init
  )
  expect_identical(domain$default[[1L]], special_default)
  expect_identical(domain$.init[[1L]], special_init)
  expect_true(isS4(domain$default[[1L]]))
  expect_true(isS4(domain$.init[[1L]]))

  utility_default = asS4(1L)
  utility_init = asS4(list(payload = 2L))
  utility = p_uty(default = utility_default, init = utility_init)
  expect_identical(utility$default[[1L]], utility_default)
  expect_identical(utility$.init[[1L]], utility_init)
})

test_that("semantic Domain atomic vectors materialize once before admission", {
  skip_if_not(
    domain2_altrep_helpers_available(),
    "the internal stateful ALTREP test class is unavailable"
  )

  events = character()
  nested = new.env(parent = emptyenv())
  marker = new.env(parent = emptyenv())
  callback = function(label) {
    force(label)
    function() {
      events <<- c(events, label)
      nested[[label]] = p_lgl(default = FALSE)
      invisible(gc())
    }
  }
  levels = native_stateful_altrep(
    c("a", "b"),
    c("later-a", "later-b"),
    elt_switch_after = 2L,
    callback = callback("levels"),
    callback_after = 0L
  )
  special_vals = list(marker)
  tags = native_stateful_altrep(
    c("first", "second"),
    c("changed", "changed"),
    elt_switch_after = 2L,
    callback = callback("tags"),
    callback_after = 0L
  )

  observed = domain2_construct(
    cls = "ParamFct",
    grouping = "\"a\",\"b\"",
    cargo = NULL,
    lower = NA_real_,
    upper = NA_real_,
    tolerance = NA_real_,
    levels = levels,
    special_vals = special_vals,
    default_value = paradox:::NO_DEF,
    tags = tags,
    trafo = NULL,
    storage_type = "character",
    init_given = FALSE,
    init_value = NULL,
    numeric_source_kind = 0L,
    numeric_logscale = FALSE,
    id = "x",
    requirements = NULL
  )
  expect_identical(events, c("levels", "tags"))
  expect_identical(observed$levels, list(c("a", "b")))
  expect_identical(observed$.tags, list(c("first", "second")))
  expect_identical(observed$special_vals[[1L]][[1L]], marker)
  expect_true(all(vapply(as.list(nested), inherits, logical(1L), "ParamLgl")))
})

test_that("interpreted Domain list shells reject ALTREP without observation", {
  skip_if_not(
    domain2_altrep_helpers_available(),
    "the internal stateful ALTREP test class is unavailable"
  )

  callbacks = 0L
  numeric = domain2_args(p_dbl())
  utility = domain2_args(p_uty())
  outer = native_stateful_altrep(
    utility$cargo,
    utility$cargo,
    callback = function() callbacks <<- callbacks + 1L,
    callback_after = 0L
  )
  expect_error(
    do.call(domain2_construct, domain2_replace(utility, "cargo", outer)),
    "interpreted cargo|unclassed canonical vectors"
  )
  expect_identical(callbacks, 0L)

  nested = native_stateful_altrep(
    structure(list(flag = TRUE), names = "flag"),
    structure(list(flag = FALSE), names = "flag"),
    callback = function() callbacks <<- callbacks + 1L,
    callback_after = 0L
  )
  cargo = utility$cargo
  cargo$disable_in_tune = nested
  expect_error(
    do.call(domain2_construct, domain2_replace(utility, "cargo", cargo)),
    "interpreted cargo entries",
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)

  special_values = native_stateful_altrep(
    list("automatic"),
    list("changed"),
    callback = function() callbacks <<- callbacks + 1L,
    callback_after = 0L
  )
  expect_error(
    do.call(
      domain2_construct,
      domain2_replace(numeric, "special_vals", special_values)
    ),
    "constructor fields must use unclassed canonical vectors",
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)
})

test_that("state-changing ALTREP representation capture is a safe non-contract", {
  skip_if_not(
    domain2_altrep_helpers_available(),
    "the internal stateful ALTREP test class is unavailable"
  )

  callbacks = 0L
  lower = native_stateful_altrep(
    1,
    9,
    elt_switch_after = 1L,
    callback = function() {
      callbacks <<- callbacks + 1L
      invisible(gc())
    },
    callback_after = 0L
  )

  # The wrapper may reject this hostile fixture while capturing the printable
  # call (R's deparser can request a data pointer that the fixture deliberately
  # does not provide). Safe rejection is part of the contract; successful
  # admission and exact agreement with representation text are not.
  expect_error(p_dbl(lower = lower, upper = 10))
  expect_true(callbacks %in% 0:1)
})

test_that("direct constructor admission is closed and structurally exact", {
  numeric = domain2_args(p_dbl(0, 1))
  factor = domain2_args(p_fct(c("a", "b")))
  logical = domain2_args(p_lgl())
  utility = domain2_args(p_uty())

  malformed = list(
    unknown_kind = domain2_replace(numeric, "cls", "ParamExtension"),
    storage_mismatch = domain2_replace(numeric, "storage_type", "integer"),
    classed_shell = domain2_replace(
      numeric,
      "tags",
      structure("tag", class = "DomainTagExtension")
    ),
    duplicate_tags = domain2_replace(numeric, "tags", c("tag", "tag")),
    wrong_lower = domain2_replace(numeric, "lower", list(0)),
    missing_init_flag = domain2_replace(numeric, "init_given", NA),
    unknown_cargo = domain2_replace(numeric, "cargo", list(extra = TRUE)),
    duplicate_levels = domain2_replace(factor, "levels", c("a", "a")),
    wrong_logical_levels = domain2_replace(logical, "levels", c(FALSE, TRUE)),
    incomplete_utility_cargo = domain2_replace(
      utility,
      "cargo",
      list(custom_check = NULL)
    )
  )
  both = domain2_replace(numeric, "special_vals", list("automatic"))
  malformed$special_and_trafo = domain2_replace(both, "trafo", identity)

  for (name in names(malformed)) {
    specific_lower = identical(name, "wrong_lower")
    expect_error(
      do.call(domain2_construct, malformed[[name]]),
      if (specific_lower) {
        "`lower` must be one number"
      } else {
        "Invalid built-in Domain state|cannot both be supplied"
      },
      fixed = specific_lower,
      info = name
    )
  }

  expect_error(
    do.call(domain2_construct, domain2_replace(
      numeric,
      "cls",
      new.env(parent = emptyenv())
    )),
    "Cannot snapshot semantic value of type `environment`",
    fixed = TRUE
  )
  expect_error(
    do.call(domain2_construct, domain2_replace(numeric, "tags", expression(x))),
    "Cannot snapshot semantic value of type `expression`",
    fixed = TRUE
  )
})

test_that("unknown kinds and invalid utility callbacks fail at admission", {
  p_extension = function(lower = 0, upper = 1, init) {
    paradox:::Domain(
      cls = "ParamExtension",
      grouping = "ParamExtension",
      lower = lower,
      upper = upper,
      tolerance = 0,
      storage_type = "numeric",
      init = init
    )
  }
  expect_error(
    p_extension(),
    "Paradox 2 supports only canonical p_dbl, p_int, p_fct, p_lgl, and p_uty",
    fixed = TRUE
  )

  check_result = domain2_symbol("domain_uty_check_result")
  for (value in list(TRUE, "diagnostic")) {
    expect_identical(.Call(check_result, value), TRUE)
  }
  for (value in list(FALSE, NA, NA_character_, character(), 1L, NULL)) {
    expect_identical(.Call(check_result, value), FALSE)
  }
  expect_s3_class(
    p_uty(custom_check = function(value) "diagnostic"),
    "ParamUty"
  )
  expect_error(
    p_uty(custom_check = function(value) FALSE),
    "must be TRUE or one non-missing string",
    fixed = TRUE
  )
})

test_that("Domain constructors remain rooted during GC and callback reentry", {
  skip_on_cran()

  marker = new.env(parent = emptyenv())
  existing = list(
    p_dbl(-1, 1, trafo = exp),
    p_int(-2L, 2L, init = 1L),
    p_fct(c("slow", "fast")),
    p_lgl(default = TRUE),
    p_uty(default = marker)
  )
  callback_count = 0L
  nested = NULL

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  native_facades = lapply(existing, domain2_construct_domain)
  reentrant = p_uty(custom_check = function(value) {
    callback_count <<- callback_count + 1L
    nested <<- p_int(0L, 2L)
    invisible(gc())
    TRUE
  })
  fresh = list(
    p_dbl(-3, 3),
    p_int(-3L, 3L),
    p_fct(c("a", "b")),
    p_lgl(),
    reentrant
  )
  gctorture(previous)

  expect_true(all(vapply(native_facades, inherits, logical(1L), "Domain")))
  expect_true(all(vapply(
    native_facades,
    function(domain) data.table:::selfrefok(domain, FALSE) == 1L,
    logical(1L)
  )))
  expect_identical(native_facades[[5L]]$default[[1L]], marker)
  expect_identical(vapply(fresh, `[[`, character(1L), "cls"), c(
    "ParamDbl", "ParamInt", "ParamFct", "ParamLgl", "ParamUty"
  ))
  expect_identical(callback_count, 1L)
  expect_s3_class(nested, "ParamInt")
})
