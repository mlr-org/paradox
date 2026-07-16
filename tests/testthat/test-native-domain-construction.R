native_domain_construct_symbol = function() {
  get("C_domain_construct", envir = asNamespace("paradox"))
}

native_domain_construct_frame_symbol = function() {
  get("C_domain_construct_frame", envir = asNamespace("paradox"))
}

native_domain_fct_grouping_symbol = function() {
  get("C_domain_fct_grouping", envir = asNamespace("paradox"))
}

native_domain_numeric_bounds_symbol = function() {
  get("C_domain_numeric_bounds_admit", envir = asNamespace("paradox"))
}

native_domain_uty_check_result_symbol = function() {
  get("C_domain_uty_check_result", envir = asNamespace("paradox"))
}

native_domain_simple_repr_id_symbol = function() {
  get("C_domain_simple_repr_id", envir = asNamespace("paradox"))
}

native_domain_numeric_bounds = function(
    integer_kind = FALSE,
    lower = -Inf,
    upper = Inf,
    tolerance = sqrt(.Machine$double.eps),
    logscale = FALSE) {
  .Call(
    native_domain_numeric_bounds_symbol(),
    environment(),
    integer_kind
  )
}

native_domain_frame_plan = function(
    cls = "ParamLgl",
    grouping = "ParamLgl",
    cargo = list(),
    lower = NA_real_,
    upper = NA_real_,
    tolerance = NA_real_,
    levels = c(TRUE, FALSE),
    special_vals = list(),
    default = stop("default was forced", call. = FALSE),
    tags = character(),
    trafo = NULL,
    depends_expr = stop("depends_expr was forced", call. = FALSE),
    storage_type = stop("storage_type was forced", call. = FALSE),
    init = stop("init was forced", call. = FALSE)) {
  .Call(native_domain_construct_frame_symbol(), environment())
}

native_domain_construct_partial = function(domain) {
  .Call(
    native_domain_construct_symbol(),
    domain$cls,
    domain$grouping,
    domain$cargo[[1L]],
    domain$lower,
    domain$upper,
    domain$tolerance,
    domain$levels[[1L]],
    domain$special_vals[[1L]],
    domain$default[[1L]],
    domain$.tags[[1L]],
    domain$.trafo[[1L]],
    domain$storage_type,
    domain$.init_given,
    domain$.init[[1L]]
  )
}

native_domain_capture_error = function(expr) {
  tryCatch(
    {
      force(expr)
      NA_character_
    },
    error = conditionMessage
  )
}

test_that("native Domain construction is registered with a forced symbol", {
  symbol = native_domain_construct_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 14L)
  frame_symbol = native_domain_construct_frame_symbol()
  expect_s3_class(frame_symbol, "NativeSymbolInfo")
  expect_identical(frame_symbol$numParameters, 1L)
  grouping_symbol = native_domain_fct_grouping_symbol()
  expect_s3_class(grouping_symbol, "NativeSymbolInfo")
  expect_identical(grouping_symbol$numParameters, 1L)
  bounds_symbol = native_domain_numeric_bounds_symbol()
  expect_s3_class(bounds_symbol, "NativeSymbolInfo")
  expect_identical(bounds_symbol$numParameters, 2L)
  uty_symbol = native_domain_uty_check_result_symbol()
  expect_s3_class(uty_symbol, "NativeSymbolInfo")
  expect_identical(uty_symbol$numParameters, 1L)
  repr_symbol = native_domain_simple_repr_id_symbol()
  expect_s3_class(repr_symbol, "NativeSymbolInfo")
  expect_identical(repr_symbol$numParameters, 1L)
  expect_error(
    .Call("domain_construct", PACKAGE = "paradox"),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("exact zero-argument built-in calls bypass deparse1", {
  symbol = native_domain_simple_repr_id_symbol()
  native_enabled = getRversion() >= "4.5.0"
  expected = c(
    p_dbl = "p_dbl()",
    p_int = "p_int()",
    p_lgl = "p_lgl()",
    p_uty = "p_uty()"
  )
  domains = list(
    p_dbl = p_dbl(),
    p_int = p_int(),
    p_lgl = p_lgl(),
    p_uty = p_uty()
  )
  for (constructor in names(expected)) {
    representation = as.call(list(as.name(constructor)))
    encoded = .Call(symbol, representation)
    if (native_enabled) {
      expect_identical(encoded, expected[[constructor]])
    } else {
      expect_null(encoded)
    }
    domain = domains[[constructor]]
    expect_identical(domain$id, expected[[constructor]])
    expect_identical(attr(domain, "repr"), representation)
  }

  attributed = quote(p_dbl())
  attr(attributed, "probe") = TRUE
  classed = quote(p_dbl())
  class(classed) = "constructor_probe"
  for (representation in list(
    quote(p_dbl(,)),
    quote(paradox::p_dbl()),
    quote(paradox:::p_dbl()),
    quote(p_fct()),
    attributed,
    classed,
    as.name("p_dbl"),
    NULL
  )) {
    expect_null(.Call(symbol, representation))
  }
})

test_that("native short Domain representations are byte-identical to deparse1", {
  symbol = native_domain_simple_repr_id_symbol()
  native_enabled = getRversion() >= "4.5.0"
  previous_scipen = getOption("scipen")
  on.exit(options(scipen = previous_scipen), add = TRUE)
  options(scipen = 0L)

  admitted = list(
    as.call(c(list(as.name("p_dbl")), list(
      lower = -9999,
      upper = Inf,
      tags = c("train", "bounded")
    ))),
    as.call(c(list(as.name("p_dbl")), list(
      lower = -Inf,
      upper = -0
    ))),
    as.call(c(list(as.name("p_int")), list(
      lower = -20L,
      upper = 20L,
      tolerance = 0,
      tags = c("train", "bounded")
    ))),
    as.call(c(list(as.name("p_fct")), list(
      levels = character(),
      tags = c("choice", "safe")
    ))),
    as.call(c(list(as.name("p_fct")), list(
      levels = c("linear", "tree", "dart"),
      tags = "categorical"
    ))),
    as.call(c(list(as.name("p_lgl")), list(
      default = TRUE,
      init = FALSE,
      tags = c("flag", "required")
    ))),
    as.call(c(list(as.name("p_uty")), list(
      custom_check = NULL,
      repr = "plain value",
      tags = "payload"
    )))
  )
  for (representation in admitted) {
    encoded = .Call(symbol, representation)
    if (native_enabled) {
      expect_type(encoded, "character")
      expect_length(encoded, 1L)
      expect_lte(nchar(encoded, type = "bytes"), 80L)
      expect_identical(
        encoded,
        deparse1(representation, collapse = "\n", width.cutoff = 80)
      )
    } else {
      expect_null(encoded)
    }
  }

  domains = list(
    p_dbl(-10, 10, tags = c("train", "bounded")),
    p_int(-20L, 20L, tolerance = 0, tags = c("train", "bounded")),
    p_fct(c("linear", "tree", "dart"), tags = "categorical"),
    p_lgl(tags = c("flag", "required"), init = FALSE),
    p_uty(tags = "payload")
  )
  for (domain in domains) {
    representation = attr(domain, "repr")
    expected = deparse1(
      representation,
      collapse = "\n",
      width.cutoff = 80
    )
    encoded = .Call(symbol, representation)
    if (native_enabled) {
      expect_identical(encoded, expected)
    } else {
      expect_null(encoded)
    }
    expect_identical(domain$id, expected)
  }

  set.seed(20260716L)
  strings = c("a", "Z9", "two words", "dash-value", "x/y", "a'b")
  for (iteration in seq_len(250L)) {
    constructor = c("p_dbl", "p_int", "p_fct", "p_lgl", "p_uty")[[
      1L + (iteration - 1L) %% 5L
    ]]
    arguments = switch(
      constructor,
      p_dbl = list(
        lower = as.double(sample.int(19999L, 1L) - 10000L),
        upper = sample(c(-Inf, Inf), 1L),
        tags = sample(strings, sample.int(2L, 1L))
      ),
      p_int = list(
        lower = as.integer(sample.int(19999L, 1L) - 10000L),
        upper = as.integer(sample.int(19999L, 1L) - 10000L),
        tolerance = 0,
        tags = sample(strings, 1L)
      ),
      p_fct = list(
        levels = sample(strings, sample.int(2L, 1L)),
        tags = sample(strings, 1L)
      ),
      p_lgl = list(
        tags = sample(strings, sample.int(2L, 1L)),
        init = sample(c(TRUE, FALSE), 1L)
      ),
      p_uty = list(
        custom_check = NULL,
        repr = sample(strings, 1L),
        tags = sample(strings, 1L)
      )
    )
    arguments = arguments[sample.int(length(arguments))]
    representation = as.call(c(list(as.name(constructor)), arguments))
    encoded = .Call(symbol, representation)
    if (native_enabled) {
      expect_type(encoded, "character")
      expect_identical(
        encoded,
        deparse1(representation, collapse = "\n", width.cutoff = 80),
        info = sprintf("iteration %d", iteration)
      )
    } else {
      expect_null(encoded)
    }
  }
})

test_that("native short Domain representation grammar fails closed", {
  symbol = native_domain_simple_repr_id_symbol()
  native_enabled = getRversion() >= "4.5.0"
  previous_scipen = getOption("scipen")
  on.exit(options(scipen = previous_scipen), add = TRUE)
  options(scipen = 0L)

  attributed_call = quote(p_dbl(lower = 1))
  attr(attributed_call, "probe") = TRUE
  classed_call = quote(p_dbl(lower = 1))
  class(classed_call) = "constructor_probe"
  attributed_value = structure(1L, names = "value")
  classed_value = structure(1L, class = "value_probe")
  s4_call = asS4(quote(p_dbl()), TRUE, FALSE)
  s4_value = asS4(1L, TRUE, FALSE)
  bytes_value = rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xc3, 0xa9)))
  Encoding(bytes_value) = "bytes"
  duplicate_arguments = as.call(c(
    list(as.name("p_dbl")),
    setNames(list(1, 2), c("lower", "lower"))
  ))

  declined = list(
    quote(other(lower = 1)),
    quote(paradox::p_dbl(lower = 1)),
    quote(p_fct()),
    call("p_dbl", 1),
    quote(p_dbl(,)),
    as.call(c(list(as.name("p_dbl")), list(unknown = 1))),
    duplicate_arguments,
    attributed_call,
    classed_call,
    s4_call,
    as.call(c(list(as.name("p_dbl")), list(lower = attributed_value))),
    as.call(c(list(as.name("p_dbl")), list(lower = classed_value))),
    as.call(c(list(as.name("p_dbl")), list(lower = s4_value))),
    as.call(c(list(as.name("p_dbl")), list(lower = 0.5))),
    as.call(c(list(as.name("p_dbl")), list(lower = 10000))),
    as.call(c(list(as.name("p_dbl")), list(lower = NA_real_))),
    as.call(c(list(as.name("p_dbl")), list(lower = NaN))),
    as.call(c(list(as.name("p_int")), list(lower = NA_integer_))),
    as.call(c(list(as.name("p_int")), list(lower = c(1L, 3L)))),
    as.call(c(list(as.name("p_lgl")), list(init = NA))),
    as.call(c(list(as.name("p_lgl")), list(init = c(TRUE, FALSE)))),
    as.call(c(list(as.name("p_fct")), list(levels = 'a"b'))),
    as.call(c(list(as.name("p_fct")), list(levels = "a\\b"))),
    as.call(c(list(as.name("p_fct")), list(levels = "line\nbreak"))),
    as.call(c(list(as.name("p_fct")), list(levels = "café"))),
    as.call(c(list(as.name("p_fct")), list(levels = bytes_value))),
    as.call(c(list(as.name("p_fct")), list(levels = rep("abcdefghij", 8L)))),
    as.call(c(list(as.name("p_fct")), list(levels = rep("x", 17L)))),
    as.call(c(list(as.name("p_uty")), list(default = list()))),
    as.call(c(list(as.name("p_uty")), list(default = quote(x + 1))))
  )
  for (representation in declined) {
    expect_null(.Call(symbol, representation))
  }

  exactly_80 = as.call(c(
    list(as.name("p_uty")),
    list(repr = paste(rep("a", 64L), collapse = ""))
  ))
  encoded = .Call(symbol, exactly_80)
  if (native_enabled) {
    expect_identical(nchar(encoded, type = "bytes"), 80L)
    expect_identical(
      encoded,
      deparse1(exactly_80, collapse = "\n", width.cutoff = 80)
    )
  } else {
    expect_null(encoded)
  }
  over_80 = as.call(c(
    list(as.name("p_uty")),
    list(repr = paste(rep("a", 65L), collapse = ""))
  ))
  expect_null(.Call(symbol, over_80))

  finite_real = as.call(c(
    list(as.name("p_dbl")),
    list(lower = -10, upper = 10)
  ))
  encoded = .Call(symbol, finite_real)
  if (native_enabled) {
    expect_identical(
      encoded,
      deparse1(finite_real, collapse = "\n", width.cutoff = 80)
    )
  } else {
    expect_null(encoded)
  }
  options(scipen = -9L)
  expect_null(.Call(symbol, finite_real))
  domain = p_dbl(-10, 10)
  expect_identical(
    domain$id,
    deparse1(attr(domain, "repr"), collapse = "\n", width.cutoff = 80)
  )
})

test_that("utility callback results bypass general assertions only when valid", {
  symbol = native_domain_uty_check_result_symbol()
  for (value in list(
    TRUE,
    structure(TRUE, names = "valid"),
    structure(TRUE, class = "valid"),
    "diagnostic",
    structure("diagnostic", class = "valid")
  )) {
    expect_identical(.Call(symbol, value), TRUE)
  }
  for (value in list(FALSE, NA, NA_character_, character(), 1L, NULL)) {
    expect_identical(.Call(symbol, value), FALSE)
  }

  expect_s3_class(p_uty(custom_check = function(value) TRUE), "ParamUty")
  expect_s3_class(
    p_uty(custom_check = function(value) "invalid later"),
    "ParamUty"
  )
  expect_error(
    p_uty(custom_check = function(value) FALSE),
    "result of 'custom_check\\(\\)'",
    ignore.case = TRUE
  )
})

test_that("factor grouping uses a narrow exact native escape pass", {
  symbol = native_domain_fct_grouping_symbol()
  levels = c('a"b', "c\\d", "plain")
  expected = mlr3misc::str_collapse(
    gsub("([\\\\\"])", "\\\\\\1", levels),
    quote = '"',
    sep = ","
  )
  expect_identical(.Call(symbol, levels), expected)
  expect_identical(.Call(symbol, character()), "\"\"")

  expect_null(.Call(symbol, NA_character_))
  expect_null(.Call(symbol, "café"))
  expect_null(.Call(symbol, structure("plain", names = "named")))
  expect_null(.Call(symbol, structure("plain", class = "grouping_probe")))

  factor = p_fct(c("z\\last", 'a"first', "middle"))
  expect_identical(
    factor$grouping,
    mlr3misc::str_collapse(
      gsub(
        "([\\\\\"])",
        "\\\\\\1",
        sort(factor$levels[[1L]])
      ),
      quote = '"',
      sep = ","
    )
  )
})

test_that("factor aggregation skips its general assertion only for NULL", {
  without_aggregation = p_fct(c("a", "b"), aggr = NULL)
  expect_null(without_aggregation$cargo[[1L]])

  aggregation = function(value) value[[1L]]
  with_aggregation = p_fct(c("a", "b"), aggr = aggregation)
  expect_identical(with_aggregation$cargo[[1L]]$aggr, aggregation)

  expect_error(
    p_fct(c("a", "b"), aggr = function(x, y) x),
    "Assertion on 'aggr' failed: Must have exactly 1 formal arguments, but has 2.",
    fixed = TRUE
  )
  expect_error(
    p_fct(c("a", "b"), aggr = 1L),
    "Assertion on 'aggr' failed: Must be a function (or 'NULL'), not 'integer'.",
    fixed = TRUE
  )

  events = character()
  observe = function(name, value) {
    events <<- c(events, name)
    value
  }
  construct = function(
      levels = observe("levels", c("a", "b")),
      aggr = observe("aggr", NULL)) {
    p_fct(levels, aggr = aggr)
  }
  construct()
  expect_identical(events, c("aggr", "levels"))

  events = character()
  expect_error(
    construct(
      levels = observe("levels", stop("levels forced", call. = FALSE)),
      aggr = observe("aggr", 1L)
    ),
    "Assertion on 'aggr' failed: Must be a function (or 'NULL'), not 'integer'.",
    fixed = TRUE
  )
  expect_identical(events, "aggr")
})

test_that("numeric constructor admission is narrow and preserves forcing order", {
  expect_true(native_domain_numeric_bounds(
    lower = -2,
    upper = 3,
    tolerance = Inf
  ))
  expect_true(native_domain_numeric_bounds(
    integer_kind = TRUE,
    lower = -2L,
    upper = Inf,
    tolerance = 0.5
  ))
  expect_false(native_domain_numeric_bounds(logscale = TRUE))
  expect_false(native_domain_numeric_bounds(tolerance = -1))
  expect_false(native_domain_numeric_bounds(lower = 2, upper = 1))
  expect_false(native_domain_numeric_bounds(
    integer_kind = TRUE,
    lower = 0.5
  ))
  expect_false(native_domain_numeric_bounds(
    integer_kind = TRUE,
    lower = 1e-310
  ))
  expect_false(native_domain_numeric_bounds(
    integer_kind = TRUE,
    upper = .Machine$integer.max + 1
  ))
  expect_false(native_domain_numeric_bounds(
    lower = structure(0, class = "domain_bound_probe")
  ))
  expect_false(native_domain_numeric_bounds(lower = NULL))
  expect_false(native_domain_numeric_bounds(upper = NULL))
  expect_error(p_dbl(lower = NULL), "not 'NULL'", fixed = TRUE)
  expect_error(p_int(lower = NULL), "not 'NULL'", fixed = TRUE)
  expect_error(p_int(upper = NULL), "not 'NULL'", fixed = TRUE)

  events = character()
  observe = function(name, value) {
    events <<- c(events, name)
    value
  }
  constructor = function(
      tolerance = observe("tolerance", 0),
      lower = observe("lower", -1),
      upper = observe("upper", 1),
      logscale = observe("logscale", FALSE)) {
    .Call(native_domain_numeric_bounds_symbol(), environment(), FALSE)
  }
  expect_true(constructor())
  expect_identical(events, c("tolerance", "lower", "upper", "logscale"))

  expected = c("tolerance", "lower", "upper", "logscale")
  for (stop_at in expected) {
    events = character()
    observe_until = function(name, value) {
      events <<- c(events, name)
      if (identical(name, stop_at)) {
        stop(sprintf("forced:%s", name), call. = FALSE)
      }
      value
    }
    stopped = function(
        tolerance = observe_until("tolerance", 0),
        lower = observe_until("lower", -1),
        upper = observe_until("upper", 1),
        logscale = observe_until("logscale", FALSE)) {
      .Call(native_domain_numeric_bounds_symbol(), environment(), FALSE)
    }
    expect_error(stopped(), sprintf("forced:%s", stop_at), fixed = TRUE)
    expect_identical(events, expected[seq_len(match(stop_at, expected))])
  }

  decline = function(
      integer_kind = FALSE,
      tolerance = observe("tolerance", 0),
      lower = observe("lower", -1),
      upper = observe("upper", 1),
      logscale = observe("logscale", FALSE)) {
    .Call(
      native_domain_numeric_bounds_symbol(),
      environment(),
      integer_kind
    )
  }
  events = character()
  expect_false(decline(tolerance = observe("tolerance", -1)))
  expect_identical(events, "tolerance")

  events = character()
  expect_false(decline(
    integer_kind = TRUE,
    lower = observe("lower", 0.5)
  ))
  expect_identical(events, c("tolerance", "lower"))

  events = character()
  expect_false(decline(
    lower = observe("lower", 2),
    upper = observe("upper", 1)
  ))
  expect_identical(events, c("tolerance", "lower", "upper"))
})

test_that("frame admission builds a private plan without forcing opaque rows", {
  callback = function(...) stop("callback was invoked", call. = FALSE)
  plan = native_domain_frame_plan(
    cargo = list(
      aggr = callback,
      in_tune_fn = callback,
      disable_in_tune = list(enabled = FALSE)
    ),
    tags = "internal_tuning",
    trafo = callback
  )

  expect_type(plan, "list")
  expect_length(plan, 2L)
  expect_identical(plan[[2L]], "logical")
  expect_type(plan[[1L]], "list")
  expect_false(inherits(plan[[1L]], "Domain"))
  expect_identical(plan[[1L]]$storage_type, "logical")
  expect_identical(plan[[1L]]$cargo[[1L]]$aggr, callback)
  expect_identical(plan[[1L]]$.trafo[[1L]], callback)
  expect_null(.Call(native_domain_construct_frame_symbol(), 1L))
})

test_that("frame admission declines dispatch-capable metadata atomically", {
  events = character()
  callback = function(...) {
    events <<- c(events, "callback")
    stop("callback was invoked", call. = FALSE)
  }

  classed_tags = structure(character(), class = "domain_tags_probe")
  plan = native_domain_frame_plan(
    tags = {
      events = c(events, "tags")
      classed_tags
    },
    cargo = {
      events = c(events, "cargo")
      list()
    }
  )
  expect_null(plan)
  expect_identical(events, "tags")

  events = character()
  classed_cargo = structure(
    list(aggr = callback),
    class = "domain_cargo_probe"
  )
  plan = native_domain_frame_plan(
    tags = {
      events = c(events, "tags")
      character()
    },
    cargo = {
      events = c(events, "cargo")
      classed_cargo
    },
    cls = {
      events = c(events, "cls")
      "ParamLgl"
    }
  )
  expect_null(plan)
  expect_identical(events, c("tags", "cargo"))

  events = character()
  classed_callback = structure(callback, class = "domain_callback_probe")
  plan = native_domain_frame_plan(trafo = {
    events = c(events, "trafo")
    classed_callback
  })
  expect_null(plan)
  expect_identical(events, "trafo")

  events = character()
  active_frame = new.env(parent = emptyenv())
  makeActiveBinding("tags", function() {
    events <<- c(events, "active_binding")
    character()
  }, active_frame)
  expect_null(.Call(native_domain_construct_frame_symbol(), active_frame))
  expect_identical(events, character())
  expect_null(.Call(
    native_domain_construct_frame_symbol(),
    new.env(parent = emptyenv())
  ))
  expect_null(.Call(
    native_domain_construct_frame_symbol(),
    structure(new.env(parent = emptyenv()), class = "domain_frame_probe")
  ))
})

test_that("storage authentication preserves unsupported extension rows", {
  make_domain = function(storage_type) {
    paradox:::Domain(
      cls = "ParamLgl",
      grouping = "ParamLgl",
      cargo = list(),
      levels = c(TRUE, FALSE),
      storage_type = storage_type
    )
  }
  classed_storage = structure("logical", class = "domain_storage_probe")
  classed = make_domain(classed_storage)
  mismatched = make_domain("numeric")

  expect_identical(classed$storage_type, classed_storage)
  expect_identical(mismatched$storage_type, "numeric")
  expect_identical(
    class(classed),
    c("ParamLgl", "Domain", "data.table", "data.frame")
  )
  expect_identical(
    class(mismatched),
    c("ParamLgl", "Domain", "data.table", "data.frame")
  )
})

test_that("frame admission survives forced collection", {
  skip_on_cran()

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)

  plan = native_domain_frame_plan(
    cargo = list(aggr = identity),
    tags = "internal_tuning",
    trafo = identity
  )

  gctorture(previous)
  expect_type(plan, "list")
  expect_identical(plan[[2L]], "logical")
  expect_identical(plan[[1L]]$cargo[[1L]]$aggr, identity)
  expect_identical(plan[[1L]]$.trafo[[1L]], identity)
})

test_that("the native valid gate recognizes every canonical built-in Domain", {
  marker = new.env(parent = emptyenv())
  constructors = list(
    p_dbl(-2, 3, tags = c("numeric", "bounded"), trafo = exp),
    p_int(-2L, 3L, tolerance = 0L, tags = "integer", init = 1L),
    p_fct(c("small", "large"), default = "small", tags = "categorical"),
    p_lgl(default = FALSE, tags = "flag"),
    p_uty(default = marker, custom_check = function(x) TRUE, tags = "payload")
  )

  for (domain in constructors) {
    partial = native_domain_construct_partial(domain)
    expect_type(partial, "list")
    expect_false(inherits(partial, "Domain"))
    expect_identical(names(partial), names(domain))
    expect_identical(partial[-c(1L, 14L)], unclass(domain)[-c(1L, 14L)])
    expect_identical(partial[[1L]], NA_character_)
    expect_identical(partial[[14L]], list(NULL))
  }
})

test_that("built-in Domain tables preserve their exact visible shape", {
  marker = new.env(parent = emptyenv())
  check = function(x) TRUE
  transform = function(x) x * 2
  domain = p_uty(
    custom_check = check,
    special_vals = list(NULL, marker),
    default = marker,
    tags = c("payload", "common")
  )

  expected_names = c(
    "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
    "levels", "special_vals", "default", "storage_type", ".tags",
    ".trafo", ".requirements", ".init_given", ".init"
  )
  expect_identical(class(domain), c("ParamUty", "Domain", "data.table", "data.frame"))
  expect_identical(names(domain), expected_names)
  expect_identical(dim(domain), c(1L, 16L))
  expect_identical(
    vapply(domain, typeof, character(1L)),
    c(
      id = "character", cls = "character", grouping = "character",
      cargo = "list", lower = "double", upper = "double",
      tolerance = "double", levels = "list", special_vals = "list",
      default = "list", storage_type = "character", .tags = "list",
      .trafo = "list", .requirements = "list", .init_given = "logical",
      .init = "list"
    )
  )
  expect_identical(domain$cargo[[1L]]$custom_check, check)
  expect_identical(domain$special_vals[[1L]][[2L]], marker)
  expect_identical(domain$default[[1L]], marker)
  expected_repr = as.call(list(
    as.name("p_uty"),
    custom_check = check,
    special_vals = list(NULL, marker),
    default = marker,
    tags = c("payload", "common")
  ))
  expect_identical(attributes(domain), list(
    names = expected_names,
    class = c("ParamUty", "Domain", "data.table", "data.frame"),
    repr = expected_repr
  ))

  numeric = p_dbl(-1L, 2L, tolerance = 0L, trafo = transform)
  expect_type(numeric$lower, "integer")
  expect_type(numeric$upper, "integer")
  expect_type(numeric$tolerance, "integer")
  expect_identical(numeric$.trafo[[1L]], transform)
})

test_that("representation capture retains evaluated values and NSE expressions", {
  lower = 2L
  upper = 8L
  requirement = quote(parent == 3L)
  transform = function(x) x + 1

  cases = list(
    dbl0 = p_dbl(),
    dbl = p_dbl(lower, upper = upper, tags = "x"),
    intlog = p_int(1, 10, logscale = TRUE),
    fct = p_fct(list(a = 1L, b = list(x = 2L)), depends = requirement),
    lgl = p_lgl(default = TRUE, trafo = transform),
    uty = p_uty(default = list(a = 1L), repr = quote(payload)),
    dependency = p_int(depends = requirement)
  )
  expected_repr = list(
    dbl0 = quote(p_dbl()),
    dbl = quote(p_dbl(lower = 2L, upper = 8L, tags = "x")),
    intlog = quote(p_int(lower = 1, upper = 10, logscale = TRUE)),
    fct = as.call(list(
      as.name("p_fct"),
      levels = list(a = 1L, b = list(x = 2L)),
      depends = quote(parent == 3L)
    )),
    lgl = quote(p_lgl(default = TRUE, trafo = transform)),
    uty = as.call(list(
      as.name("p_uty"),
      default = list(a = 1L),
      repr = "payload"
    )),
    dependency = quote(p_int(depends = parent == 3L))
  )

  for (name in names(cases)) {
    expect_identical(attr(cases[[name]], "repr"), expected_repr[[name]])
    expect_identical(
      cases[[name]]$id,
      deparse1(expected_repr[[name]], collapse = "\n", width.cutoff = 80)
    )
  }
  expect_identical(cases$lgl$.trafo[[1L]], transform)
  expect_identical(cases$fct$.requirements[[1L]][[1L]]$on, "parent")
  expect_identical(
    cases$fct$.requirements[[1L]][[1L]]$cond,
    CondEqual(3L)
  )
})

test_that("categorical auto-trafos and callbacks retain their environments", {
  offset = 7L
  user_trafo = function(x) {
    if (is.list(x)) {
      x$offset = offset
      x
    } else {
      x + offset
    }
  }
  levels = list(one = 1L, two = list(value = 2L))
  factor_domain = p_fct(levels, trafo = user_trafo)

  expect_identical(factor_domain$levels[[1L]], c("one", "two"))
  expect_true(is.function(factor_domain$.trafo[[1L]]))
  expect_identical(factor_domain$.trafo[[1L]]("one"), 8L)
  expect_identical(
    factor_domain$.trafo[[1L]]("two"),
    list(value = 2L, offset = 7L)
  )

  aggr = function(x) x[[1L]]
  in_tune = function(domain, param_vals) domain$upper
  disable = list(early_stopping = NULL)
  internal = p_int(
    1,
    10,
    tags = "internal_tuning",
    aggr = aggr,
    in_tune_fn = in_tune,
    disable_in_tune = disable
  )
  expect_identical(internal$cargo[[1L]]$aggr, aggr)
  expect_identical(internal$cargo[[1L]]$in_tune_fn, in_tune)
  expect_identical(internal$cargo[[1L]]$disable_in_tune, disable)
  expect_false(is.null(native_domain_construct_partial(internal)))
})

test_that("invalid generic inputs retain exact historical diagnostics", {
  cases = list(
    tags_type = list(
      quote(p_lgl(tags = 1)),
      "Assertion on 'tags' failed: Must be of type 'character', not 'double'."
    ),
    tags_duplicate = list(
      quote(p_lgl(tags = c("x", "x"))),
      "Assertion on 'tags' failed: Contains duplicated values, position 2."
    ),
    specials_type = list(
      quote(p_lgl(special_vals = 1)),
      "Assertion on 'special_vals' failed: Must be of type 'list', not 'double'."
    ),
    specials_data_frame = list(
      quote(p_lgl(special_vals = data.frame(value = 1))),
      "Assertion on 'special_vals' failed: Must be of type 'list', not 'data.frame'."
    ),
    trafo_type = list(
      quote(p_lgl(trafo = 1)),
      "Assertion on 'trafo' failed: Must be a function (or 'NULL'), not 'double'."
    ),
    special_and_trafo = list(
      quote(p_lgl(special_vals = list(1), trafo = identity)),
      "trafo and special_values can not both be given at the same time."
    ),
    missing_aggr = list(
      quote(p_lgl(tags = "internal_tuning")),
      "Assertion on 'aggregation function exists' failed: Must be TRUE."
    ),
    tuning_without_tag = list(
      quote(p_lgl(in_tune_fn = identity, disable_in_tune = list(x = 1))),
      "Arguments in_tune_fn and disable_in_tune require the tag 'internal_tuning' to be present."
    ),
    tuning_data_frame = list(
      quote(p_int(1, 2, tags = "internal_tuning", aggr = identity,
        in_tune_fn = identity, disable_in_tune = data.frame(x = 1))),
      "Assertion on 'cargo$disable_in_tune' failed: Must be of type 'list' (or 'NULL'), not 'data.frame'."
    ),
    tuning_half = list(
      quote(p_lgl(tags = "internal_tuning", aggr = identity, in_tune_fn = identity)),
      "Arguments in_tune_fn and disable_tune_fn must both be present"
    ),
    invalid_default = list(
      quote(p_lgl(default = 1L)),
      "Assertion on 'param' failed: p_lgl(default = 1L): Must be of type 'logical flag', not 'integer'."
    ),
    invalid_init = list(
      quote(p_lgl(init = 1L)),
      "Assertion on 'param' failed: p_lgl(init = 1L): Must be of type 'logical flag', not 'integer'."
    )
  )

  for (case in cases) {
    expect_identical(native_domain_capture_error(eval(case[[1L]])), case[[2L]])
  }

  expect_identical(
    native_domain_capture_error(paradox:::Domain(
      cls = "ParamDbl",
      grouping = "ParamDbl",
      lower = factor(1),
      upper = 2,
      tolerance = 0,
      storage_type = "numeric"
    )),
    "Assertion on 'lower' failed: Must be of type 'number', not 'factor'."
  )
})

test_that("default, init, and transformation interactions remain unchanged", {
  required_message = paste0(
    "A 'required' parameter can not have a 'default'.\n",
    "When the method behaves the same as if the parameter value were 'X' whenever ",
    "the parameter is missing, then 'X' should be a 'default', but the 'required' ",
    "indicates that the parameter may not be missing."
  )
  expect_identical(
    native_domain_capture_error(p_lgl(default = TRUE, tags = "required")),
    required_message
  )
  expect_identical(
    native_domain_capture_error(p_lgl(init = TRUE, trafo = identity)),
    "Initial value and trafo can not both be given at the same time."
  )

  initialized = NULL
  expect_warning(
    initialized <- p_int(0, 5, default = 2L, init = 2L),
    "Initial value and 'default' value seem to be the same",
    fixed = TRUE
  )
  expect_identical(initialized$.init_given, TRUE)
  expect_identical(initialized$.init[[1L]], 2L)
  expect_identical(initialized$default[[1L]], 2L)

  special = p_dbl(0, 1, special_vals = list("automatic"), default = "automatic")
  expect_identical(special$default[[1L]], "automatic")
})

test_that("native admission preserves lazy init error and forcing order", {
  events = new.env(parent = emptyenv())
  events$seen = character()
  expect_error(
    p_lgl(
      tags = 1,
      init = {
        events$seen = c(events$seen, "init")
        TRUE
      }
    ),
    "Assertion on 'tags' failed",
    fixed = TRUE
  )
  expect_identical(events$seen, character())

  events$seen = character()
  delayedAssign(
    "invalid_dependency",
    {
      events$seen = c(events$seen, "depends")
      1L
    },
    assign.env = environment(),
    eval.env = environment()
  )
  expect_error(
    p_lgl(
      depends = invalid_dependency,
      init = {
        events$seen = c(events$seen, "init")
        TRUE
      }
    ),
    "'depends' argument must be an expression",
    fixed = TRUE
  )
  expect_identical(events$seen, "depends")

  events$seen = character()
  initialized = p_lgl(init = {
    events$seen = c(events$seen, "init")
    TRUE
  })
  expect_identical(events$seen, "init")
  expect_identical(initialized$.init_given, TRUE)
  expect_identical(initialized$.init[[1L]], TRUE)
})

test_that("native admission does not force opaque defaults before validation", {
  events = new.env(parent = emptyenv())
  events$seen = character()
  expect_error(
    p_lgl(
      tags = 1,
      default = {
        events$seen = c(events$seen, "default")
        TRUE
      }
    ),
    "Assertion on 'tags' failed",
    fixed = TRUE
  )
  expect_identical(events$seen, character())

  domain = p_lgl(default = {
    events$seen = c(events$seen, "default")
    TRUE
  })
  expect_identical(events$seen, "default")
  expect_identical(domain$default[[1L]], TRUE)
})

test_that("unknown Domain classes retain the extensible R construction path", {
  p_custom = function(lower = 0, upper = 1, init) {
    paradox:::Domain(
      cls = "ParamCustom",
      grouping = "ParamCustom",
      lower = lower,
      upper = upper,
      tolerance = 0,
      storage_type = "numeric",
      init = init
    )
  }

  custom = p_custom(-1, 2)
  expect_identical(class(custom), c("ParamCustom", "Domain", "data.table", "data.frame"))
  expect_identical(custom$lower, -1)
  expect_identical(custom$upper, 2)
  expect_null(native_domain_construct_partial(custom))
  restored = unserialize(serialize(custom, NULL, version = 3L))
  expect_identical(restored, custom)
})

test_that("random built-in Domain construction is stable under serialization", {
  set.seed(20260713)
  for (iteration in seq_len(100L)) {
    kind = (iteration - 1L) %% 5L
    tags = sample(c("train", "control", "common"), iteration %% 3L)
    domain = switch(
      as.character(kind),
      "0" = p_dbl(-iteration, iteration + 0.5, tolerance = iteration / 10000, tags = tags),
      "1" = p_int(-iteration, iteration, tolerance = 0, tags = tags),
      "2" = p_fct(letters[seq_len(1L + iteration %% 20L)], tags = tags),
      "3" = p_lgl(tags = tags),
      "4" = p_uty(custom_check = is.numeric, tags = tags)
    )

    expect_false(is.null(native_domain_construct_partial(domain)))
    restored = unserialize(serialize(domain, NULL, version = 3L))
    expect_identical(restored, domain)
  }
})
