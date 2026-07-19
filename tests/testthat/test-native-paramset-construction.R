native_paramset_construct_symbol = function() {
  get("C_param_set_construct", envir = asNamespace("paradox"))
}

test_that("native ParamSet construction is registered and forced-symbol only", {
  symbol = native_paramset_construct_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 1L)
  expect_error(
    .Call("param_set_construct", list(), PACKAGE = "paradox"),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("native construction creates canonical plain-state bundles", {
  symbol = native_paramset_construct_symbol()

  empty = .Call(symbol, setNames(list(), character()))
  expect_named(
    empty,
    c("params", "tags", "trafos", "requirements", "init_values")
  )

  marker = new.env(parent = emptyenv())
  domains = list(
    dbl = p_dbl(-2, 3, tags = c("numeric", "bounded"), trafo = exp),
    int = p_int(-2L, 3L, tolerance = 0L, init = 1L),
    fct = p_fct(c("small", "large"), default = "small"),
    lgl = p_lgl(default = FALSE),
    uty = p_uty(init = marker, custom_check = function(x) TRUE)
  )
  bundle = .Call(symbol, domains)

  for (field in c("params", "tags", "trafos")) {
    expect_s3_class(bundle[[field]], "data.frame")
    expect_false(inherits(bundle[[field]], "data.table"))
    expect_null(attr(bundle[[field]], "index", exact = TRUE))
    expect_null(attr(bundle[[field]], "sorted", exact = TRUE))
  }
  expect_type(bundle$requirements, "list")
  expect_false(is.object(bundle$requirements))
  expect_length(bundle$requirements, length(domains))
  expect_identical(bundle$params$id, names(domains))
  expect_identical(names(bundle$init_values), c("int", "uty"))
  expect_identical(bundle$init_values$uty, marker)
})

test_that("construction without initial values skips an empty value transaction", {
  set = testthat::with_mocked_bindings(
    ParamSet$new(list(x = p_int(), flag = p_lgl())),
    C_param_set_store_values = NULL,
    .package = "paradox"
  )

  expect_s3_class(set, "ParamSet")
  expect_identical(set$values, named_list())
})

test_that("public ParamSet facades are detached from the opaque core", {
  set = ps(
    x = p_dbl(-1, 1, tags = "numeric", init = 0),
    y = p_fct(c("a", "b"), default = "a")
  )
  private = set$.__enclos_env__$private
  expect_true(exists(".core", private, inherits = FALSE))
  expect_false(any(c(".params", ".tags", ".trafos", ".deps", ".values") %in%
    ls(private, all.names = TRUE)))

  state = paradox:::param_set_core_state(private)
  expect_s3_class(state$.params, "data.frame")
  expect_false(inherits(state$.params, "data.table"))

  params = set$params
  expect_s3_class(params, "data.table")
  data.table::set(params, i = 1L, j = "lower", value = -999)
  expect_identical(set$lower[["x"]], -1)

  deps = set$deps
  data.table::set(deps, j = "id", value = "mutated")
  expect_false("mutated" %in% set$deps$id)
})

test_that("constructor preserves rich public semantics", {
  marker = new.env(parent = emptyenv())
  scale_trafo = function(x) x * 2
  set = ParamSet$new(list(
    dbl = p_dbl(-2, 3, tolerance = 1e-7, tags = c("numeric", "bounded"),
      trafo = scale_trafo),
    int = p_int(-2L, 3L, special_vals = list(99L), default = 0L),
    fct = p_fct(c("small", "large"), default = "small"),
    lgl = p_lgl(default = FALSE),
    uty = p_uty(init = marker, custom_check = function(x) TRUE)
  ))

  expect_identical(set$ids(), c("dbl", "int", "fct", "lgl", "uty"))
  expect_identical(set$values$uty, marker)
  expect_identical(set$trafo(list(dbl = 2))$dbl, 4)
  expect_true(set$test(list(int = 99L)))
  expect_true(set$test(list(fct = "large", lgl = TRUE)))
})

test_that("closed constructor rejects extension and malformed Domain rows", {
  symbol = native_paramset_construct_symbol()
  forge_column = function(domain, name, value) {
    attrs = attributes(domain)
    domain = unclass(domain)
    domain[[name]] = value
    attributes(domain) = attrs
    domain
  }
  unknown = p_dbl()
  class(unknown) = c("CustomDomain", class(unknown))
  expect_error(.Call(symbol, list(x = unknown)), "unsupported|Domain", ignore.case = TRUE)
  expect_error(ParamSet$new(list(x = unknown)), "unsupported|Domain", ignore.case = TRUE)

  malformed = p_dbl()
  malformed$storage_type = "logical"
  expect_error(.Call(symbol, list(x = malformed)), "storage|Domain|malformed", ignore.case = TRUE)
  expect_error(ParamSet$new(list(x = malformed)), "storage|Domain|malformed", ignore.case = TRUE)

  reversed = forge_column(
    forge_column(p_dbl(0, 1), "lower", 2),
    "upper",
    1
  )
  required_default = forge_column(
    forge_column(p_dbl(0, 1), "default", list(0.5)),
    ".tags",
    list("required")
  )
  init_trafo = forge_column(
    forge_column(p_dbl(0, 1, trafo = identity), ".init_given", TRUE),
    ".init",
    list(0.5)
  )
  integer_default_overflow = forge_column(
    p_int(),
    "default",
    list(1e20)
  )
  integer_init_overflow = forge_column(
    forge_column(p_int(), ".init_given", TRUE),
    ".init",
    list(1e20)
  )
  attributed_cargo = p_uty()$cargo[[1L]]
  attr(attributed_cargo, "external") = TRUE
  attributed_cargo = forge_column(
    p_uty(),
    "cargo",
    list(attributed_cargo)
  )
  forged = list(
    `lower/upper/tolerance` = reversed,
    `required/default` = required_default,
    `Initial value and trafo` = init_trafo,
    `default value` = integer_default_overflow,
    `initial value` = integer_init_overflow,
    cargo = attributed_cargo
  )
  for (field in names(forged)) {
    expect_error(
      .Call(symbol, list(x = forged[[field]])),
      field,
      fixed = TRUE,
      info = field
    )
    expect_error(
      ParamSet$new(list(x = forged[[field]])),
      field,
      fixed = TRUE,
      info = field
    )
  }
})

test_that("ParamSet Domain admission rejects S4 structural forgery", {
  symbol = native_paramset_construct_symbol()
  forge_column = function(domain, name, value) {
    attrs = attributes(domain)
    domain = unclass(domain)
    domain[[name]] = value
    attributes(domain) = attrs
    domain
  }

  malformed = list(
    outer = asS4(p_dbl(0, 1)),
    classes = local({
      domain = p_dbl(0, 1)
      attr(domain, "class") = asS4(class(domain))
      domain
    }),
    names = local({
      domain = p_dbl(0, 1)
      attr(domain, "names") = asS4(names(domain))
      domain
    }),
    lower = forge_column(p_dbl(0, 1), "lower", asS4(0)),
    tags = forge_column(p_dbl(0, 1), ".tags", list(asS4("numeric"))),
    default = forge_column(p_dbl(0, 1), "default", list(asS4(0.5))),
    init = forge_column(
      forge_column(p_dbl(0, 1), ".init_given", TRUE),
      ".init",
      list(asS4(0.5))
    )
  )

  for (case in names(malformed)) {
    expect_error(
      .Call(symbol, list(x = malformed[[case]])),
      "Domain|unsupported|malformed|default value|initial value",
      ignore.case = TRUE,
      info = case
    )
    expect_error(
      ParamSet$new(list(x = malformed[[case]])),
      "Domain|unsupported|malformed|default value|initial value",
      ignore.case = TRUE,
      info = case
    )
  }

  special = asS4(0.5)
  admitted = ParamSet$new(list(
    x = p_dbl(0, 1, special_vals = list(special), default = special)
  ))
  expect_identical(admitted$params$default[[1L]], special)
  expect_true(isS4(admitted$params$default[[1L]]))

  utility = asS4(list(payload = 1L))
  admitted = ParamSet$new(list(x = p_uty(default = utility)))
  expect_identical(admitted$params$default[[1L]], utility)
})

test_that("constructed ParamSets retain values across clone and serialization", {
  marker = list(payload = 1:3)
  set = ps(x = p_int(0L, 3L, init = 2L), y = p_uty(default = marker))
  set$add_dep("y", "x", CondAnyOf(c(1L, 2L)))

  clone = set$clone(deep = TRUE)
  restored = unserialize(serialize(set, NULL, version = 3L))
  for (candidate in list(clone, restored)) {
    expect_identical(candidate$ids(), set$ids())
    expect_identical(candidate$values, set$values)
    expect_identical(candidate$deps$id, "y")
    candidate$values = list(x = 1L, y = list(payload = 4L))
    expect_true(candidate$test(candidate$values, check_strict = TRUE))
  }
  expect_identical(set$values$x, 2L)
})
