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
  expect_identical(paradox:::C_param_set_get_property$numParameters, 3L)
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])

  empty = ParamSet$new()
  expect_identical(empty$nlevels, setNames(integer(), character()))
  expect_identical(empty$is_number, setNames(logical(), character()))
  expect_identical(empty$is_categ, setNames(logical(), character()))
  expect_identical(empty$is_bounded, setNames(logical(), character()))
})

test_that("capsule property entry preserves the table kernel and scalar reductions", {
  domains = list(
    x = p_dbl(-1, 1), i = p_int(), f = p_fct(c("a", "b")),
    l = p_lgl(), u = p_uty(default = new.env(parent = emptyenv()))
  )
  for (set in list(ParamSet$new(), ParamSet$new(domains),
    ps(x = p_dbl(-1, 1)), ps(f = p_fct("one"), l = p_lgl()))) {
    private = set$.__enclos_env__$private
    params = property_params(set)
    for (selector in 0:15) {
      expect_identical(
        .Call(paradox:::C_param_set_get_property, private, set, selector),
        .Call(property_symbol(), params, selector),
        info = paste("selector", selector)
      )
    }
    expect_identical(set$all_numeric, all(set$is_number))
    expect_identical(set$all_categorical, all(set$is_categ))
    expect_identical(set$all_bounded, all(set$is_bounded))
  }
})

test_that("scalar reductions stop after their answer is known", {
  params = list(id = c("first", "last"), cls = c("ParamUty", "Unknown"),
    lower = c(NA_real_, NA_real_), upper = c(NA_real_, NA_real_),
    levels = list(NULL, NULL))
  for (selector in 11:13) {
    expect_identical(.Call(property_symbol(), params, selector), FALSE)
  }
})

test_that("static getters refresh nested collections and Shadow views", {
  origin = ParamSetCollection$new(list(a = ps(x = p_dbl(0, 1))))
  view = ParamSetShadow$new(origin, "a.x")
  parent = ParamSetCollection$new(list(view = view, whole = origin))
  fields = c("class", "lower", "upper", "levels", "storage_type", "nlevels",
    "is_number", "is_categ", "is_bounded", "all_numeric", "all_categorical",
    "all_bounded", "length", "is_empty")
  for (field in fields) invisible(parent[[field]])
  origin$add(ps(y = p_int(-2, 2), z = p_fct(c("a", "b"))), n = "b")

  expected_origin = ParamSetCollection$new(list(a = ps(x = p_dbl(0, 1)),
    b = ps(y = p_int(-2, 2), z = p_fct(c("a", "b")))))
  expected = ParamSetCollection$new(list(
    view = ParamSetShadow$new(expected_origin, "a.x"), whole = expected_origin))
  for (actual in list(parent, parent$clone(deep = TRUE),
    unserialize(serialize(parent, NULL)))) {
    for (field in fields) {
      expect_identical(actual[[field]], expected[[field]], info = field)
    }
  }
})

test_that("uninterpreted private levels do not trigger uniqueness checks", {
  prototype = property_params(ps(choice = p_fct("one")))
  utf8 = enc2utf8("caf\u00e9")
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  bytes = utf8
  Encoding(bytes) = "bytes"
  cases = list(character(), "", NA_character_, c("", ""), c(NA, NA),
    c(utf8, latin1), c(utf8, bytes), c(latin1, bytes), c(bytes, bytes),
    c(strrep("a", 64L), strrep("a", 65L)), rep(strrep("a", 65L), 2L))
  for (n in 1:10) {
    levels = paste0("level", seq_len(n))
    cases[[length(cases) + 1L]] = levels
    for (i in seq_len(n)) cases[[length(cases) + 1L]] = append(levels, levels[i])
  }
  for (levels in cases) {
    params = prototype
    params$levels = list(levels)
    expect_identical(.Call(property_symbol(), params, 4L), c(choice = "ParamFct"))
    expect_identical(.Call(property_symbol(), params, 7L), list(choice = levels))
  }
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

test_that("equal infinite integer bounds expose empty level sets", {
  positive = p_int(Inf, Inf)
  negative = p_int(-Inf, -Inf)
  parameter_set = ps(positive = positive, negative = negative)

  expect_identical(domain_nlevels(positive), 0)
  expect_identical(domain_nlevels(negative), 0)
  expect_identical(
    parameter_set$nlevels,
    c(positive = 0, negative = 0)
  )
  expect_identical(
    parameter_set$is_bounded,
    c(positive = FALSE, negative = FALSE)
  )
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

  for (selector in list(0, NA_integer_, -1L, 100L, 0:1)) {
    expect_error(
      .Call(symbol, valid, selector),
      "invalid ParamSet property selector",
      fixed = TRUE
    )
  }
})

test_that("static properties reject S4 and attributed column shells", {
  valid = list(
    id = "x",
    cls = "ParamInt",
    lower = 0,
    upper = 2,
    levels = list(NULL)
  )
  symbol = property_symbol()

  expect_error(
    .Call(symbol, asS4(valid), 0L),
    "`.params` must be a list",
    fixed = TRUE
  )
  malformed_names = valid
  attr(malformed_names, "names") = asS4(names(malformed_names))
  expect_error(
    .Call(symbol, malformed_names, 0L),
    "`.params` must be a named list",
    fixed = TRUE
  )

  for (column in names(valid)) {
    malformed = valid
    malformed[[column]] = asS4(malformed[[column]])
    expect_error(
      .Call(symbol, malformed, 0L),
      "ordinary",
      fixed = TRUE,
      info = paste("S4", column)
    )

    attributed = valid
    attr(attributed[[column]], "rogue") = TRUE
    expect_error(
      .Call(symbol, attributed, 0L),
      "ordinary",
      fixed = TRUE,
      info = paste("attributed", column)
    )
  }

  # Each binding guards the columns it actually reads. Unused private columns
  # are outside that operation's contract, including their representation.
  for (column in c("id", "cls", "lower", "upper", "levels")) {
    set = ps(x = p_int(0L, 2L))
    private = set$.__enclos_env__$private
    state = unserialize(serialize(
      paradox:::param_set_core_state(private),
      NULL
    ))
    state$.params[[column]] = asS4(state$.params[[column]])
    private$.core = .Call(paradox:::C_param_set_core_new, 1L, state)
    properties = if (column %in% c("id", "cls")) {
      c("nlevels", "is_number", "is_categ", "is_bounded")
    } else if (column %in% c("lower", "upper")) {
      c("nlevels", "is_bounded")
    } else "nlevels"
    for (property in properties) {
      expect_error(
        set[[property]],
        "Corrupt ParamSet",
        info = paste(column, property)
      )
    }
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
