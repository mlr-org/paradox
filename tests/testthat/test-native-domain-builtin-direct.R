native_domain_builtin_runtime_symbol = function() {
  get("C_domain_builtin_runtime", envir = asNamespace("paradox"))
}

native_domain_construct_builtin_symbol = function() {
  get("C_domain_construct_builtin", envir = asNamespace("paradox"))
}

test_that("direct built-in Domain construction has a forced registered surface", {
  runtime = native_domain_builtin_runtime_symbol()
  construct = native_domain_construct_builtin_symbol()
  expect_s3_class(runtime, "NativeSymbolInfo")
  expect_identical(runtime$numParameters, 3L)
  expect_s3_class(construct, "NativeSymbolInfo")
  expect_identical(construct$numParameters, 5L)
  expect_error(
    .Call("domain_construct_builtin", PACKAGE = "paradox"),
    "not available"
  )

  ns = asNamespace("paradox")
  constructors = list(
    p_dbl = ns$p_dbl,
    p_int = ns$p_int,
    p_fct = ns$p_fct,
    p_lgl = ns$p_lgl
  )
  expect_true(.Call(runtime, constructors, ns$NO_DEF, base::sort))
  altered = constructors
  altered$p_int = function(...) NULL
  expect_false(.Call(runtime, altered, ns$NO_DEF, base::sort))
})

test_that("direct built-in Domains preserve the complete visible shape", {
  domains = list(
    p_dbl(),
    p_int(-2L, 3L, tags = c("bounded", "integer"), tolerance = 0L),
    p_fct(c("tree", "linear"), tags = "choice"),
    p_lgl(tags = "flag")
  )
  expected_classes = c("ParamDbl", "ParamInt", "ParamFct", "ParamLgl")
  expected_names = c(
    "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
    "levels", "special_vals", "default", "storage_type", ".tags",
    ".trafo", ".requirements", ".init_given", ".init"
  )

  for (index in seq_along(domains)) {
    domain = domains[[index]]
    expect_identical(names(domain), expected_names)
    expect_identical(
      class(domain),
      c(expected_classes[[index]], "Domain", "data.table", "data.frame")
    )
    expect_identical(
      domain$id,
      deparse1(attr(domain, "repr"), collapse = "\n", width.cutoff = 80)
    )
    expect_null(attr(domain, "row.names", exact = TRUE))
    expect_null(domain$cargo[[1L]])
    expect_identical(domain$default[[1L]], NO_DEF)
    expect_null(domain$.trafo[[1L]])
    expect_null(domain$.requirements[[1L]])
    expect_false(domain$.init_given)
    expect_null(domain$.init[[1L]])
  }

  expect_identical(domains[[1L]]$lower, -Inf)
  expect_identical(domains[[1L]]$upper, Inf)
  expect_identical(domains[[1L]]$tolerance, sqrt(.Machine$double.eps))
  expect_identical(domains[[2L]]$lower, -2L)
  expect_identical(domains[[2L]]$upper, 3L)
  expect_identical(domains[[2L]]$tolerance, 0L)
  expect_identical(domains[[3L]]$levels[[1L]], c("tree", "linear"))
  expect_identical(domains[[3L]]$grouping, '"linear","tree"')
  expect_identical(domains[[4L]]$levels[[1L]], c(TRUE, FALSE))
})

test_that("direct built-in calls retain formal matching and force order", {
  events = character()
  observe = function(name, value) {
    events <<- c(events, name)
    value
  }

  domain = p_int(
    tags = observe("tags", "x"),
    observe("lower", -1L),
    observe("upper", 2L),
    tolerance = observe("tolerance", 0L)
  )
  expect_identical(events, c("tolerance", "lower", "upper", "tags"))
  expect_identical(
    attr(domain, "repr"),
    call("p_int", lower = -1L, upper = 2L, tags = "x", tolerance = 0L)
  )
  expect_identical(
    domain$id,
    'p_int(lower = -1L, upper = 2L, tags = "x", tolerance = 0L)'
  )

  events = character()
  factor = p_fct(
    observe("levels", c("b", "a")),
    tags = observe("tags", "choice")
  )
  expect_identical(events, c("levels", "tags"))
  expect_identical(
    attr(factor, "repr"),
    call("p_fct", levels = c("b", "a"), tags = "choice")
  )

  events = character()
  p_lgl(tags = observe("tags", "flag"))
  expect_identical(events, "tags")
})

test_that("fractional numeric representations remain exact", {
  previous = options(scipen = 0L, OutDec = ".")
  on.exit(options(previous), add = TRUE)

  tracker = new.env(parent = emptyenv())
  tracker$calls = 0L
  namespace = asNamespace("paradox")
  invisible(capture.output(suppressMessages(trace(
    "Domain",
    tracer = function() tracker$calls = tracker$calls + 1L,
    print = FALSE,
    where = namespace
  ))))
  traced = TRUE
  on.exit({
    if (traced) {
      invisible(capture.output(suppressMessages(untrace(
        "Domain",
        where = namespace
      ))))
    }
  }, add = TRUE)
  caller = new.env(parent = baseenv())
  caller$p_dbl = get("p_dbl", envir = namespace)
  domain = eval(
    quote(p_dbl(
      lower = -10,
      upper = 10,
      tags = c("train", "bounded"),
      tolerance = 1e-6
    )),
    envir = caller
  )
  invisible(capture.output(suppressMessages(untrace(
    "Domain",
    where = namespace
  ))))
  traced = FALSE
  expect_identical(tracker$calls, 0L)
  expect_identical(
    attr(domain, "repr"),
    call(
      "p_dbl",
      lower = -10,
      upper = 10,
      tags = c("train", "bounded"),
      tolerance = 1e-6
    )
  )

  pi_domain = p_dbl(lower = -pi, upper = pi, tolerance = 1e-6)
  expect_identical(
    pi_domain$id,
    deparse1(attr(pi_domain, "repr"), collapse = "\n", width.cutoff = 80)
  )
  expect_identical(
    domain$id,
    deparse1(attr(domain, "repr"), collapse = "\n", width.cutoff = 80)
  )

  # Numeric coercion drops zeroes that deparse() retains for some nearby
  # doubles.  Such values must decline to the ordinary R constructor without
  # evaluating an already forced expression again.
  evaluations = 0L
  awkward = 0x1.9c511dc3a41d3p-29
  for (scipen in c(-9L, 0L, 9L, 999L)) {
    options(scipen = scipen)
    awkward_domain = p_dbl(
      lower = {
        evaluations = evaluations + 1L
        awkward
      },
      upper = 1,
      tolerance = 1e-6
    )
    expect_identical(
      awkward_domain$id,
      deparse1(
        attr(awkward_domain, "repr"),
        collapse = "\n",
        width.cutoff = 80
      ),
      info = paste("scipen", scipen)
    )
  }
  expect_identical(evaluations, 4L)

  options(scipen = 0L, OutDec = ".")
  for (digits in c(1L, 22L)) {
    options(digits = digits)
    digits_domain = p_dbl(-10, 10, tolerance = 1e-6)
    expect_identical(
      digits_domain$id,
      deparse1(
        attr(digits_domain, "repr"),
        collapse = "\n",
        width.cutoff = 80
      ),
      info = paste("digits", digits)
    )
  }

  padded_large = p_dbl(
    lower = 0,
    upper = 0x1.1a1e5f775378ep+131,
    tolerance = 1e-6
  )
  expect_identical(
    padded_large$id,
    deparse1(
      attr(padded_large, "repr"),
      collapse = "\n",
      width.cutoff = 80
    )
  )

  signed_zero = p_dbl(lower = -0, upper = 1, tolerance = 1e-6)
  expect_identical(
    signed_zero$id,
    deparse1(attr(signed_zero, "repr"), collapse = "\n", width.cutoff = 80)
  )

  options(OutDec = ",", scipen = 0L)
  comma = p_dbl(lower = 0.5, upper = 1.5, tolerance = 1e-6)
  expect_identical(
    comma$id,
    deparse1(attr(comma, "repr"), collapse = "\n", width.cutoff = 80)
  )
})

test_that("simple aliases retain their head and overrides are not bypassed", {
  integer_alias = p_int
  domain = integer_alias(-1L, 2L, tags = "x")
  expect_identical(
    attr(domain, "repr"),
    call("integer_alias", lower = -1L, upper = 2L, tags = "x")
  )
  expect_identical(
    domain$id,
    'integer_alias(lower = -1L, upper = 2L, tags = "x")'
  )
  expect_match(capture.output(print(domain))[[1L]], "integer_alias\\(")

  qualified = paradox::p_int(-1L, 2L)
  expect_identical(
    attr(qualified, "repr")[[1L]],
    quote(paradox::p_int)
  )

  local({
    p_int = function(...) "local override"
    expect_identical(p_int(0L, 1L), "local override")
  })
})

test_that("evaluated factor levels cross the direct boundary exactly once", {
  evaluations = 0L
  make_levels = function(value) {
    evaluations <<- evaluations + 1L
    value
  }

  domain = p_fct(make_levels(c("b", "a")), tags = "choice")
  expect_identical(evaluations, 1L)
  expect_identical(domain$levels[[1L]], c("b", "a"))
  expect_identical(
    attr(domain, "repr"),
    call("p_fct", levels = c("b", "a"), tags = "choice")
  )

  evaluations = 0L
  transformed = p_fct(make_levels(list(a = 1L, b = 2L)))
  expect_identical(evaluations, 1L)
  expect_identical(transformed$levels[[1L]], c("a", "b"))
  expect_type(transformed$.trafo[[1L]], "closure")

  evaluations = 0L
  expect_error(
    p_fct(make_levels(c("duplicate", "duplicate"))),
    "duplicated",
    ignore.case = TRUE
  )
  expect_identical(evaluations, 1L)

  evaluations = 0L
  many_levels = sprintf("level-%02d", seq_len(17L))
  many = p_fct(make_levels(many_levels))
  expect_identical(evaluations, 1L)
  expect_identical(many$levels[[1L]], many_levels)

  evaluations = 0L
  long_level = strrep("x", 81L)
  long = p_fct(make_levels(long_level))
  expect_identical(evaluations, 1L)
  expect_identical(long$levels[[1L]], long_level)
})

test_that("factor grouping follows the active R collation", {
  previous = Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", previous), add = TRUE)

  levels = c("_", "-", "B", "a", "linear", "tree")
  locales = unique(c("C", "C.UTF-8", "en_US.UTF-8"))
  admitted = 0L
  for (locale in locales) {
    selected = suppressWarnings(Sys.setlocale("LC_COLLATE", locale))
    if (is.na(selected) || !nzchar(selected)) next
    admitted = admitted + 1L
    direct = p_fct(levels)
    fallback = paradox::p_fct(levels)
    expect_identical(direct$grouping, fallback$grouping, info = selected)
    expect_identical(direct$levels, fallback$levels, info = selected)
  }
  expect_gte(admitted, 1L)
})

test_that("factor sorting honors visible extensions without reevaluation", {
  existed = exists("sort.character", envir = .GlobalEnv, inherits = FALSE)
  if (existed) previous = get("sort.character", envir = .GlobalEnv)
  on.exit({
    if (existed) {
      assign("sort.character", previous, envir = .GlobalEnv)
    } else if (exists(
      "sort.character", envir = .GlobalEnv, inherits = FALSE
    )) {
      rm("sort.character", envir = .GlobalEnv)
    }
  }, add = TRUE)

  evaluations = 0L
  make_levels = function(value) {
    evaluations <<- evaluations + 1L
    value
  }
  assign(
    "sort.character",
    function(x, ...) rev(x),
    envir = .GlobalEnv
  )
  factor = p_fct(make_levels(c("a", "b")))
  expect_identical(evaluations, 1L)
  expect_identical(factor$grouping, '"b","a"')
})

test_that("post-force declines memoize numeric and tag expressions", {
  evaluations = integer()
  observe = function(name, value) {
    previous = evaluations[name]
    if (is.na(previous)) previous = 0L
    evaluations[[name]] <<- previous + 1L
    value
  }

  domain = p_dbl(
    lower = observe("lower", 0.5),
    upper = observe("upper", 2),
    tags = observe("tags", structure("tag", names = "named")),
    tolerance = observe("tolerance", 0)
  )
  expect_s3_class(domain, "ParamDbl")
  expect_identical(evaluations, c(tolerance = 1L, lower = 1L, upper = 1L, tags = 1L))

  evaluations = integer()
  expect_error(
    p_int(
      lower = observe("lower", 0.5),
      upper = observe("upper", stop("upper forced", call. = FALSE)),
      tolerance = observe("tolerance", 0)
    ),
    "integerish",
    ignore.case = TRUE
  )
  expect_identical(evaluations, c(tolerance = 1L, lower = 1L))
})

test_that("commit audit declines when a later promise rebinds an earlier value", {
  rebind_constructor_value = function(name, value, result) {
    candidates = Filter(
      function(frame) {
        identical(parent.env(frame), asNamespace("paradox")) &&
          all(vapply(
            c("lower", "upper", "tolerance", "tags"),
            exists,
            logical(1L),
            envir = frame,
            inherits = FALSE
          ))
      },
      sys.frames()
    )
    expect_true(length(candidates) >= 1L)
    assign(name, value, envir = candidates[[length(candidates)]])
    result
  }

  domain = p_int(
    lower = rebind_constructor_value("tolerance", 0.25, -1L),
    upper = 2L,
    tolerance = 0L,
    tags = "audited"
  )
  expect_identical(domain$tolerance, 0.25)
  expect_identical(
    attr(domain, "repr"),
    call(
      "p_int",
      lower = -1L,
      upper = 2L,
      tags = "audited",
      tolerance = 0.25
    )
  )
  expect_identical(
    domain$id,
    'p_int(lower = -1L, upper = 2L, tags = "audited", tolerance = 0.25)'
  )
})

test_that("commit audit detects replacement of an unforced default promise", {
  replace_constructor_promise = function(name, value, result) {
    candidates = Filter(
      function(frame) {
        identical(parent.env(frame), asNamespace("paradox")) &&
          all(vapply(
            c("lower", "upper", "tolerance", "tags"),
            exists,
            logical(1L),
            envir = frame,
            inherits = FALSE
          ))
      },
      sys.frames()
    )
    expect_true(length(candidates) >= 1L)
    delayedAssign(
      name,
      value,
      eval.env = environment(),
      assign.env = candidates[[length(candidates)]]
    )
    result
  }

  domain = p_int(
    lower = replace_constructor_promise("tags", "changed", -1L),
    upper = 2L
  )
  expect_identical(domain$.tags[[1L]], "changed")
  expect_identical(
    attr(domain, "repr"),
    call("p_int", lower = -1L, upper = 2L)
  )
})

test_that("malformed native calls and qualified heads fail closed", {
  construct = native_domain_construct_builtin_symbol()
  fake = function() NULL
  expect_null(.Call(
    construct,
    environment(), fake, quote(p_int()), environment(), 1L
  ))
  expect_null(.Call(
    construct,
    environment(), p_int, quote(paradox::p_int()), environment(), 1L
  ))
})

test_that("native runtime roots survive clean unload and reload cycles", {
  skip_if(getRversion() < "4.6.0")
  skip_if_not_installed("callr")

  observed = callr::r(
    function() {
      assert_native_roots = function() {
        ns = asNamespace("paradox")
        constructors = mget(
          c("p_dbl", "p_int", "p_fct", "p_lgl"),
          envir = ns,
          inherits = FALSE
        )
        generators = get(
          "sampler_1d_unif_generators",
          envir = ns,
          inherits = FALSE
        )()
        stopifnot(
          isTRUE(.Call(
            get("C_ps_builtin_runtime", envir = ns, inherits = FALSE),
            constructors,
            get("NO_DEF", envir = ns, inherits = FALSE)
          )),
          isTRUE(.Call(
            get("C_domain_builtin_runtime", envir = ns, inherits = FALSE),
            constructors,
            get("NO_DEF", envir = ns, inherits = FALSE),
            base::sort
          )),
          isTRUE(.Call(
            get(
              "C_param_set_bulk_generator_auth",
              envir = ns,
              inherits = FALSE
            ),
            get("ParamSet", envir = ns, inherits = FALSE)
          )),
          isTRUE(.Call(
            get(
              "C_sampler_1d_unif_bulk_auth",
              envir = ns,
              inherits = FALSE
            ),
            generators
          ))
        )
      }

      library(paradox)
      for (cycle in seq_len(2L)) {
        assert_native_roots()
        domain = p_dbl(0, 1, tags = "bounded")
        space = ps(
          x = p_dbl(0, 1),
          y = p_int(1, 3),
          z = p_lgl()
        )
        stopifnot(
          inherits(domain, "ParamDbl"),
          identical(space$ids(), c("x", "y", "z")),
          "paradox" %in% names(getLoadedDLLs())
        )

        unloadNamespace("paradox")
        stopifnot(
          !"paradox" %in% loadedNamespaces(),
          !"paradox" %in% names(getLoadedDLLs())
        )
        invisible(gc(FALSE))
        library(paradox)
      }
      unloadNamespace("paradox")
      !"paradox" %in% loadedNamespaces() &&
        !"paradox" %in% names(getLoadedDLLs())
    },
    libpath = .libPaths(),
    timeout = 30
  )
  expect_true(observed)
})
