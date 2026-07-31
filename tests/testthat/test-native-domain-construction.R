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

test_that("the backports integration supplies deparse1 on old R", {
  if (getRversion() < "4.0.0") {
    expect_true(exists(
      "deparse1",
      envir = asNamespace("paradox"),
      inherits = FALSE
    ))
  }

  domain = p_dbl()
  token = to_tune()
  expect_identical(domain$id, "p_dbl()")
  expect_identical(token$call, "to_tune()")
  expect_identical(class(token), c("FullTuneToken", "TuneToken"))
})

test_that("Domain construction uses fixed registered native interfaces", {
  arities = c(
    domain_construct = 18L,
    domain_uty_check_result = 1L,
    domain_uty_validate_custom_check = 1L,
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

test_that("simple Domain representation rendering stays native on old R", {
  renderer = function(call) {
    .Call(domain2_symbol("domain_simple_repr_id"), call)
  }
  deparse_one = get(
    "deparse1",
    envir = asNamespace("paradox"),
    inherits = TRUE
  )
  named_call = function(constructor, name, value) {
    as.call(structure(
      list(as.name(constructor), value),
      names = c("", name)
    ))
  }
  calls = list(
    quote(p_dbl()),
    quote(p_lgl()),
    named_call("p_int", "lower", 1L),
    named_call("p_fct", "levels", c("small", "large")),
    named_call("p_dbl", "lower", 1)
  )

  old_scipen = getOption("scipen")
  on.exit(options(scipen = old_scipen), add = TRUE)
  options(scipen = 0)
  for (call in calls) {
    observed = renderer(call)
    expect_false(is.null(observed), info = deparse(call))
    expect_identical(
      observed,
      deparse_one(call, collapse = "\n", width.cutoff = 80)
    )
  }

  finite_real = named_call("p_dbl", "lower", 1)
  options(scipen = 100)
  expect_null(renderer(finite_real))
  expect_identical(renderer(quote(p_dbl())), "p_dbl()")
  domain = p_dbl(1, 2)
  expect_identical(
    domain$id,
    deparse_one(attr(domain, "repr"), collapse = "\n", width.cutoff = 80)
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
      get(
        "deparse1",
        envir = asNamespace("paradox"),
        inherits = TRUE
      )(attr(domain, "repr"), collapse = "\n", width.cutoff = 80)
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

test_that("typed ALTREP specials reject before aliased value leaves are observed", {
  skip_if_not(
    domain2_altrep_helpers_available(),
    "the internal stateful ALTREP test class is unavailable"
  )

  for (field in c("default_value", "init_value")) {
    callbacks = 0L
    hostile = native_stateful_altrep(
      0.5,
      0.5,
      callback = function() callbacks <<- callbacks + 1L,
      callback_after = 0L
    )
    arguments = domain2_args(p_dbl(0, 1))
    arguments$special_vals = list(hostile)
    arguments[[field]] = hostile
    if (field == "init_value") {
      arguments$init_given = TRUE
    }

    invoke = function() do.call(domain2_construct, arguments)
    expect_error(
      invoke(),
      "special_vals",
      fixed = TRUE,
      info = field
    )
    expect_identical(callbacks, 0L, info = field)
  }

  callbacks = 0L
  hostile_levels = native_stateful_altrep(
    c("a", "b"),
    c("a", "b"),
    callback = function() callbacks <<- callbacks + 1L,
    callback_after = 0L
  )
  arguments = domain2_args(p_fct(c("a", "b")))
  arguments$special_vals = list(hostile_levels)
  arguments$levels = hostile_levels
  invoke = function() do.call(domain2_construct, arguments)
  expect_error(invoke(), "special_vals", fixed = TRUE)
  expect_identical(callbacks, 0L)
})

test_that("semantic snapshots do not launder structure added by ALTREP reentry", {
  skip_on_cran()
  skip_if_not(
    domain2_altrep_helpers_available(),
    "the internal stateful ALTREP test class is unavailable"
  )

  exercise = function(attribute, replacement) {
    state = new.env(parent = emptyenv())
    state$callbacks = 0L
    state$value = native_stateful_altrep(
      "tag",
      "tag",
      callback = function() {
        state$callbacks = state$callbacks + 1L
        data.table::setattr(state$value, attribute, replacement)
      },
      callback_after = c(0L, NA_integer_)
    )
    arguments = domain2_replace(
      domain2_args(p_dbl()),
      "tags",
      state$value
    )
    expect_error(
      do.call(domain2_construct, arguments),
      "Semantic vector structure changed while being snapshotted",
      fixed = TRUE,
      info = attribute
    )
    expect_identical(state$callbacks, 1L)
  }

  exercise("class", "hostile_semantic_shell")
  exercise("probe", TRUE)

  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  state$tags = "tag"
  state$cls = native_stateful_altrep(
    "ParamDbl",
    "ParamDbl",
    callback = function() {
      state$callbacks = state$callbacks + 1L
      data.table::setattr(state$tags, "probe", TRUE)
    },
    callback_after = c(0L, NA_integer_)
  )
  arguments = domain2_args(p_dbl())
  arguments = domain2_replace(arguments, "cls", state$cls)
  arguments = domain2_replace(arguments, "tags", state$tags)
  expect_error(
    do.call(domain2_construct, arguments),
    "structural constructor fields may carry at most",
    fixed = TRUE
  )
  expect_identical(state$callbacks, 1L)
})

test_that("Domain requirements retain rows and row fields before RHS reentry", {
  skip_on_cran()
  skip_if_not(
    domain2_altrep_helpers_available(),
    "the internal stateful ALTREP test class is unavailable"
  )

  state = new.env(parent = emptyenv())
  state$callbacks = character()
  mutate_at_gc = function(target, index, replacement) {
    pointer = .Call(
      get("C_test_gc_column_mutator", envir = asNamespace("paradox")),
      target,
      as.integer(index - 1L),
      replacement
    )
    rm(pointer)
    for (iteration in 1:3) {
      invisible(gc(full = TRUE))
    }
  }

  deferred_rhs = native_stateful_altrep(
    FALSE,
    FALSE,
    callback = function() {
      state$callbacks = c(state$callbacks, "outer-row")
      mutate_at_gc(
        state$requirements,
        2L,
        list(on = "mutated_parent", cond = CondEqual(FALSE))
      )
    },
    callback_after = c(0L, NA_integer_)
  )
  original_condition = CondEqual(FALSE)
  original_condition[[1L]] = deferred_rhs
  deferred_on = native_stateful_altrep(
    "first_parent",
    "first_parent",
    callback = function() {
      state$callbacks = c(state$callbacks, "within-row")
      mutate_at_gc(
        state$requirements[[1L]],
        2L,
        CondEqual(TRUE)
      )
    },
    callback_after = c(0L, NA_integer_)
  )
  state$requirements = list(
    list(on = deferred_on, cond = original_condition),
    list(on = "second_parent", cond = CondEqual(TRUE))
  )

  arguments = domain2_replace(
    domain2_args(p_int()),
    "requirements",
    state$requirements
  )
  observed = do.call(domain2_construct, arguments)
  requirements = observed$.requirements[[1L]]

  expect_identical(state$callbacks, c("within-row", "outer-row"))
  expect_identical(
    vapply(requirements, `[[`, character(1L), "on"),
    c("first_parent", "second_parent")
  )
  expect_identical(
    lapply(requirements, function(requirement) requirement$cond$rhs),
    list(FALSE, TRUE)
  )
  expect_identical(
    state$requirements[[1L]]$cond$rhs,
    TRUE
  )
  expect_identical(state$requirements[[2L]]$on, "mutated_parent")
})

test_that("Domain requirement parent IDs cannot launder attributes", {
  arguments = domain2_args(p_int())
  requirement = list(
    on = structure("parent", probe = TRUE),
    cond = CondEqual(TRUE)
  )
  expect_error(
    do.call(
      domain2_construct,
      domain2_replace(arguments, "requirements", list(requirement))
    ),
    "Invalid built-in Domain requirements",
    fixed = TRUE
  )
})

test_that("interpreted Domain list shells reject ALTREP without observation", {
  skip_if_no_list_altrep()

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

test_that("interpreted Domain cargo cannot launder noncanonical attributes", {
  utility = domain2_args(p_uty())

  malformed_repr = utility$cargo
  attr(malformed_repr$repr, "probe") = TRUE
  expect_error(
    do.call(
      domain2_construct,
      domain2_replace(utility, "cargo", malformed_repr)
    ),
    "interpreted cargo entries must use canonical attributes",
    fixed = TRUE
  )

  tuned = domain2_args(p_uty(
    tags = "internal_tuning",
    aggr = function(x) x[[1L]],
    in_tune_fn = function(domain, param_vals) TRUE,
    disable_in_tune = list(blocked = TRUE)
  ))
  malformed_disable = tuned$cargo
  attr(malformed_disable$disable_in_tune, "probe") = TRUE
  expect_error(
    do.call(
      domain2_construct,
      domain2_replace(tuned, "cargo", malformed_disable)
    ),
    "interpreted cargo entries must use canonical attributes",
    fixed = TRUE
  )
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
        # Each canonical-state clause now names the argument it guards.
        paste(
          "must describe one canonical Domain",
          "`grouping` must be",
          "`levels` must be",
          "`special_vals` must be",
          "`tags` must be",
          "`trafo` must be",
          "`logscale` must be given as an argument",
          "invalid `init` admission flag",
          "Invalid built-in Domain state",
          "cannot both be supplied",
          sep = "|"
        )
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
    "Paradox 2 supports only p_dbl, p_int, p_fct, p_lgl, and p_uty",
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


test_that("a snapshotted value leaf keeps its own materialized names", {
  skip_on_cran()
  namespace = asNamespace("paradox")
  skip_if_not(
    exists("C_test_stateful_altrep", namespace, inherits = FALSE),
    "the internal stateful ALTREP test class is unavailable"
  )

  growing = native_stateful_altrep(
    "a", c("a", "b"),
    length_switch_after = 7L
  )
  value = 1
  names(value) = growing

  set = ps(a = p_dbl(0, 2, default = value), b = p_dbl(0, 1))
  stored = set$params$default[[1L]]
  expect_identical(length(stored), 1L)
  expect_identical(length(attr(stored, "names", exact = TRUE)), 1L)
})

test_that("built-in value snapshots own payload and arbitrary attributes", {
  marker = new.env(parent = emptyenv())
  replacement = new.env(parent = emptyenv())
  value = structure(0.5, names = "selected", marker = marker)

  # Initializing to the declared default is the exact pattern the advisory
  # warning exists to flag; the warning is part of the pinned behavior here,
  # and the identical carrier is what proves both snapshots detach from it.
  domain = NULL
  expect_warning(
    domain <- p_dbl(0, 1, default = value, init = value),
    "Initial value and 'default' value seem to be the same",
    fixed = TRUE
  )
  value[[1L]] = 0.75
  data.table::setattr(value, "names", "changed")
  data.table::setattr(value, "marker", replacement)

  expect_identical(domain$default[[1L]], structure(
    0.5,
    names = "selected",
    marker = marker
  ))
  expect_identical(domain$.init[[1L]], structure(
    0.5,
    names = "selected",
    marker = marker
  ))
})

test_that("ALTREP value snapshots reject an attribute-generation tear", {
  skip_on_cran()
  namespace = asNamespace("paradox")
  skip_if_not(
    exists("C_test_stateful_altrep", namespace, inherits = FALSE),
    "the internal stateful ALTREP test class is unavailable"
  )

  state = new.env(parent = emptyenv())
  state$before = new.env(parent = emptyenv())
  state$after = new.env(parent = emptyenv())
  state$value = native_stateful_altrep(
    0.5,
    0.5,
    callback = function() {
      data.table::setattr(state$value, "marker", state$after)
    },
    callback_after = 0L
  )
  data.table::setattr(state$value, "marker", state$before)
  native_stateful_altrep_rearm(state$value, 0L)

  # The public wrapper's printable-representation capture may reject this
  # fixture first (R's deparser requests a data pointer the fixture
  # deliberately does not provide), so the tear pin drives the native
  # constructor directly, like the other direct-admission tests here.
  numeric = domain2_args(p_dbl(0, 1))
  expect_error(
    do.call(
      domain2_construct,
      domain2_replace(numeric, "default_value", state$value)
    ),
    "Built-in value attributes changed while being snapshotted",
    fixed = TRUE
  )
})

test_that("a non-finite tolerance is reported as a `tolerance` argument error", {
  # A canonical numeric Domain stores a finite tolerance. Without the argument
  # gate, `Inf` reached the final canonical-state check, which can only name
  # the whole `lower/upper/tolerance` field group.
  expect_error(p_dbl(0, 1, tolerance = Inf), "`tolerance` must be one finite non-negative number", fixed = TRUE)
  expect_error(p_dbl(0, 1, tolerance = -Inf), "`tolerance` must be one finite non-negative number", fixed = TRUE)
  expect_error(p_dbl(1, 10, logscale = TRUE, tolerance = Inf), "`tolerance` must be one finite non-negative number", fixed = TRUE)
  expect_error(p_int(0L, 1L, tolerance = Inf), "`tolerance` must be one number between 0 and 0.5", fixed = TRUE)

  expect_silent(p_dbl(0, 1, tolerance = 0))
  expect_silent(p_dbl(0, 1, tolerance = 1e9))
  expect_silent(p_int(0L, 1L, tolerance = 0.5))
})

test_that("each canonical-state clause names the argument it guards", {
  # These all reported "Invalid built-in Domain state; Paradox 2 supports only
  # canonical p_dbl, p_int, p_fct, p_lgl, and p_uty Domains", which names no
  # argument at all and reads like an internal failure.
  expect_error(
    p_fct(c("a", "a")),
    "`levels` must be a character vector of unique, non-missing values",
    fixed = TRUE
  )
  expect_error(
    p_fct(c("a", NA_character_)),
    "`levels` must be a character vector of unique, non-missing values",
    fixed = TRUE
  )
  expect_error(
    p_dbl(0, 1, tags = NA_character_),
    "`tags` must be an attribute-free character vector of unique, non-missing values",
    fixed = TRUE
  )
  expect_error(
    p_dbl(0, 1, tags = c("x", "x")),
    "`tags` must be an attribute-free character vector of unique, non-missing values",
    fixed = TRUE
  )
  expect_error(
    p_dbl(0, 1, trafo = 1),
    "`trafo` must be a function or NULL",
    fixed = TRUE
  )
  expect_error(
    p_uty(custom_check = 1),
    "custom_check",
    ignore.case = TRUE
  )

  # The clause order of the single condition these replaced is preserved, so an
  # input that is wrong in two places still reports the earlier argument.
  expect_error(
    p_fct(c("a", "a"), tags = c("x", "x")),
    "`levels` must be",
    fixed = TRUE
  )

  expect_silent(p_fct(c("a", "b")))
  expect_silent(p_dbl(0, 1, tags = c("x", "y")))
  expect_silent(p_dbl(0, 1, trafo = function(x) x))
})
