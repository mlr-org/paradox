native_domains_available = function() {
  exists(
    "C_param_set_domains",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

native_domains_symbol = function() {
  get("C_param_set_domains", envir = asNamespace("paradox"), inherits = FALSE)
}

native_domains_call = function(param_set) {
  .Call(
    native_domains_symbol(),
    param_set$.__enclos_env__$private,
    param_set
  )
}

native_domains_with_private = function(param_set, name, value) {
  private = param_set$.__enclos_env__$private
  original = private[[name]]
  on.exit(private[[name]] <- original)
  private[[name]] = value
  native_domains_call(param_set)
}

native_domains_reference = function(param_set) {
  ids = param_set$ids()
  setNames(lapply(ids, param_set$get_domain), ids)
}

native_domains_replace = function(table, column, value) {
  result = unclass(table)
  result[[column]] = value
  attributes(result) = attributes(table)
  result
}

test_that("native domains is registered with a forced symbol", {
  skip_if_not(native_domains_available())
  symbol = native_domains_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)
  expect_error(
    .Call("param_set_domains", PACKAGE = "paradox"),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("native domains reproduces every rich built-in row in one pass", {
  skip_if_not(native_domains_available())
  marker = new.env(parent = emptyenv())
  transform = function(x) exp(x)
  param_set = ps(
    number = p_dbl(
      -2,
      2,
      tolerance = 1e-7,
      tags = c("numeric", "shared"),
      trafo = transform
    ),
    integer = p_int(
      -3L,
      3L,
      tolerance = 0L,
      tags = "shared",
      init = 2L
    ),
    factor = p_fct(c("small", "large"), default = "small"),
    flag = p_lgl(tags = character()),
    payload = p_uty(custom_check = function(x) TRUE, init = marker)
  )
  param_set$add_dep("factor", "integer", CondAnyOf(c(-1L, 2L)))
  param_set$add_dep("factor", "flag", CondEqual(TRUE))

  private = param_set$.__enclos_env__$private
  params_before = lapply(private$.params, identity)
  tags_before = lapply(private$.tags, identity)
  trafos_before = lapply(private$.trafos, identity)
  deps_before = lapply(private$.deps, identity)
  values_before = lapply(private$.values, identity)
  expected = native_domains_reference(param_set)
  direct = native_domains_call(param_set)
  observed = param_set$domains

  expect_identical(direct, expected)
  expect_identical(observed, expected)
  expect_identical(names(direct), param_set$ids())
  expect_identical(
    vapply(direct, function(domain) domain$id, character(1L), USE.NAMES = FALSE),
    param_set$ids()
  )
  for (domain in direct) {
    expect_identical(names(domain), paradox:::domain_names)
    expect_identical(attr(domain, "row.names"), 1L)
    expect_identical(
      names(attributes(domain)),
      c("class", "row.names", ".internal.selfref", "names")
    )
    expect_identical(data.table:::selfrefok(domain, verbose = FALSE), 1L)
  }
  expect_identical(direct$payload$.init[[1L]], marker)
  expect_identical(direct$number$.trafo[[1L]], transform)
  expect_identical(
    vapply(
      direct$factor$.requirements[[1L]],
      `[[`,
      character(1L),
      "on"
    ),
    c("integer", "flag")
  )
  expect_identical(lapply(private$.params, identity), params_before)
  expect_identical(lapply(private$.tags, identity), tags_before)
  expect_identical(lapply(private$.trafos, identity), trafos_before)
  expect_identical(lapply(private$.deps, identity), deps_before)
  expect_identical(lapply(private$.values, identity), values_before)
})

test_that("batch Domains are independently owned and safely mutable", {
  skip_if_not(native_domains_available())
  param_set = ps(
    x = p_dbl(
      0,
      1,
      tags = c("first", "second"),
      trafo = exp
    ),
    parent = p_int(0, 2),
    special = p_dbl(0, 1, special_vals = list(-1))
  )
  param_set$add_dep("x", "parent", CondEqual(1L))
  private = param_set$.__enclos_env__$private
  params_before = lapply(private$.params, identity)
  tags_before = lapply(private$.tags, identity)
  trafos_before = lapply(private$.trafos, identity)
  deps_before = lapply(private$.deps, identity)

  domains = native_domains_call(param_set)
  expect_true(all(vapply(
    domains,
    function(domain) data.table:::selfrefok(domain, FALSE) == 1L,
    logical(1L)
  )))
  expect_warning(
    data.table::set(domains$x, i = 1L, j = "lower", value = -10),
    NA
  )
  expect_warning(data.table::set(domains$x, j = "added", value = 1L), NA)
  domains$x$.tags[[1L]][[1L]] = "changed"
  domains$x$.requirements[[1L]][[1L]]$on = "changed"
  domains$x$.trafo[1L] = list(NULL)
  domains$special$special_vals[[1L]][[1L]] = -2
  names(domains)[[1L]] = "changed"

  fresh = native_domains_call(param_set)
  expect_identical(fresh$x$lower, 0)
  expect_false("added" %in% names(fresh$x))
  expect_identical(fresh$x$.tags[[1L]], c("first", "second"))
  expect_identical(fresh$x$.requirements[[1L]][[1L]]$on, "parent")
  expect_identical(fresh$x$.trafo[[1L]], exp)
  expect_identical(fresh$special$special_vals[[1L]], list(-1))
  expect_identical(names(fresh), c("x", "parent", "special"))
  expect_identical(names(domains$parent), paradox:::domain_names)
  expect_true(all(vapply(
    fresh,
    function(domain) data.table:::selfrefok(domain, FALSE) == 1L,
    logical(1L)
  )))
  expect_identical(lapply(private$.params, identity), params_before)
  expect_identical(lapply(private$.tags, identity), tags_before)
  expect_identical(lapply(private$.trafos, identity), trafos_before)
  expect_identical(lapply(private$.deps, identity), deps_before)
})

test_that("exact base collections batch rich and nested Domains", {
  skip_if_not(native_domains_available())
  left = ps(
    parent = p_int(0L, 3L, init = 2L),
    target = p_fct(c("a", "b"), tags = "target", trafo = toupper),
    payload = p_uty()
  )
  left$add_dep("target", "parent", CondEqual(2L))
  right = ps(flag = p_lgl(init = TRUE))
  inner = ParamSetCollection$new(
    list(left = left, right = right),
    postfix_names = TRUE
  )
  collection = ParamSetCollection$new(
    list(outer = inner),
    postfix_names = TRUE
  )
  collection$add_dep("target.left.outer", "flag.right.outer", CondEqual(TRUE))

  expected = native_domains_reference(collection)
  direct = native_domains_call(collection)
  expect_identical(direct, expected)
  expect_identical(collection$domains, expected)
  expect_named(direct, c(
    "target.left.outer",
    "parent.left.outer",
    "payload.left.outer",
    "flag.right.outer"
  ), ignore.order = TRUE)
  expect_identical(
    vapply(
      direct,
      `[[`,
      character(1L),
      "id",
      USE.NAMES = FALSE
    ),
    names(direct)
  )
  expect_true(all(vapply(
    direct,
    function(domain) data.table:::selfrefok(domain, FALSE) == 1L,
    logical(1L)
  )))
  expect_identical(
    vapply(
      direct$target.left.outer$.requirements[[1L]],
      `[[`,
      character(1L),
      "on"
    ),
    c("parent.left.outer", "flag.right.outer")
  )
  expect_identical(direct$parent.left.outer$.init[[1L]], 2L)
  expect_identical(direct$target.left.outer$.trafo[[1L]], toupper)
})

test_that("empty sets, extensions, and subclasses retain literal fallback", {
  skip_if_not(native_domains_available())
  empty = ParamSet$new()
  expect_null(native_domains_call(empty))
  expect_identical(empty$domains, setNames(list(), character()))

  make_custom_domain = function() {
    paradox:::Domain(
      cls = "ParamDomainsExtension",
      grouping = "ParamDomainsExtension",
      storage_type = "list"
    )
  }
  custom = make_custom_domain()
  custom_set = ParamSet$new(list(builtin = p_int(0, 1), custom = custom))
  expect_null(native_domains_call(custom_set))
  expect_s3_class(custom_set$domains$builtin, "ParamInt")
  expect_s3_class(custom_set$domains$custom, "ParamDomainsExtension")

  events = new.env(parent = emptyenv())
  events$seen = character()
  Subclass = R6::R6Class(
    "ParamSetDomainsSubclass",
    inherit = ParamSet,
    public = list(
      get_domain = function(id) {
        events$seen = c(events$seen, paste0("domain:", id))
        super$get_domain(id)
      }
    )
  )
  subclass = Subclass$new(list(a = p_dbl(0, 1), b = p_lgl()))
  expect_null(native_domains_call(subclass))
  subclass_domains = subclass$domains
  expect_named(subclass_domains, c("a", "b"))
  expect_identical(events$seen, c("domain:a", "domain:b"))

  CountingChild = R6::R6Class(
    "ParamSetDomainsCountingChild",
    inherit = ParamSet,
    active = list(
      values = function(xs) {
        if (!missing(xs)) {
          super$values <- xs
          return(xs)
        }
        events$seen = c(events$seen, "values")
        super$values
      },
      deps = function(value) {
        if (!missing(value)) {
          super$deps <- value
          return(value)
        }
        events$seen = c(events$seen, "deps")
        super$deps
      }
    )
  )
  child = CountingChild$new(list(x = p_dbl(0, 1), y = p_int(0, 2)))
  collection = ParamSetCollection$new(list(child = child))
  expect_null(native_domains_call(collection))
  events$seen = character()
  collection_domains = collection$domains
  expect_named(collection_domains, c("child.x", "child.y"))
  expect_identical(events$seen, c("values", "deps", "values", "deps"))
})

test_that("collection preflight rejects replaced R6 wrappers without callbacks", {
  skip_if_not(native_domains_available())
  make_fixture = function() {
    ParamSetCollection$new(list(
      child = ps(x = p_dbl(0, 1), y = p_int(0L, 2L))
    ))
  }
  events = new.env(parent = emptyenv())
  events$values = 0L
  events$deps = 0L
  events$ids = 0L
  events$get_values = 0L
  events$prefix = 0L

  collection = make_fixture()
  makeActiveBinding("values", function(value) {
    events$values = events$values + 1L
    setNames(list(0.5, 1L), c("child.x", "child.y"))
  }, collection)
  expect_null(native_domains_call(collection))
  expect_identical(events$values, 0L)
  expect_named(collection$domains, c("child.x", "child.y"))
  expect_identical(events$values, 2L)

  collection = make_fixture()
  child = collection$sets[[1L]]
  makeActiveBinding("values", function(value) {
    events$values = events$values + 1L
    setNames(list(0.5, 1L), c("x", "y"))
  }, child)
  expect_null(native_domains_call(collection))
  expect_identical(events$values, 2L)

  collection = make_fixture()
  makeActiveBinding("deps", function(value) {
    events$deps = events$deps + 1L
    data.table::data.table(id = character(), on = character(), cond = list())
  }, collection)
  expect_null(native_domains_call(collection))
  expect_identical(events$deps, 0L)

  collection = make_fixture()
  unlockBinding("ids", collection)
  collection$ids = function(...) {
    events$ids = events$ids + 1L
    character()
  }
  lockBinding("ids", collection)
  expect_null(native_domains_call(collection))
  expect_identical(events$ids, 0L)

  collection = make_fixture()
  private = collection$.__enclos_env__$private
  unlockBinding(".get_values", private)
  private$.get_values = function() {
    events$get_values = events$get_values + 1L
    setNames(list(), character())
  }
  lockBinding(".get_values", private)
  expect_null(native_domains_call(collection))
  expect_identical(events$get_values, 0L)

  collection = make_fixture()
  private = collection$.__enclos_env__$private
  unlockBinding(".add_name_prefix", private)
  private$.add_name_prefix = function(owner, id) {
    events$prefix = events$prefix + 1L
    paste(owner, id, sep = ".")
  }
  lockBinding(".add_name_prefix", private)
  expect_null(native_domains_call(collection))
  expect_identical(events$prefix, 0L)
})

test_that("base domains authenticates every generated fallback wrapper", {
  skip_if_not(native_domains_available())

  for (member in c("values", "deps", "ids", "get_domain", ".get_values")) {
    param_set = ps(x = p_int(0, 2, init = 1L), y = p_lgl())
    reads = new.env(parent = emptyenv())
    reads$count = 0L
    if (member %in% c("values", "deps")) {
      original = activeBindingFunction(member, param_set)
      makeActiveBinding(member, function(value) {
        reads$count = reads$count + 1L
        original(value)
      }, param_set)
    } else if (member == ".get_values") {
      private = param_set$.__enclos_env__$private
      original = private$.get_values
      unlockBinding(member, private)
      private$.get_values = function() {
        reads$count = reads$count + 1L
        original()
      }
      lockBinding(member, private)
    } else {
      original = param_set[[member]]
      unlockBinding(member, param_set)
      if (member == "ids") {
        param_set[[member]] = function(...) {
          reads$count = reads$count + 1L
          original(...)
        }
      } else {
        param_set[[member]] = function(id) {
          reads$count = reads$count + 1L
          original(id)
        }
      }
      lockBinding(member, param_set)
    }

    expect_null(native_domains_call(param_set), info = member)
    expect_identical(reads$count, 0L, info = member)
    expect_named(param_set$domains, c("x", "y"))
    expect_true(reads$count > 0L, info = member)
  }
})

test_that("domains authenticates only the root get_domain method", {
  skip_if_not(native_domains_available())
  child = ps(x = p_int(0, 2, init = 1L), y = p_lgl())
  collection = ParamSetCollection$new(list(child = child))
  reads = new.env(parent = emptyenv())
  reads$root = 0L
  root_get_domain = collection$get_domain
  unlockBinding("get_domain", collection)
  collection$get_domain = function(id) {
    reads$root = reads$root + 1L
    root_get_domain(id)
  }
  lockBinding("get_domain", collection)

  expect_null(native_domains_call(collection))
  expect_identical(reads$root, 0L)
  expect_named(collection$domains, c("child.x", "child.y"))
  expect_identical(reads$root, 2L)

  child = ps(x = p_int(0, 2, init = 1L), y = p_lgl())
  collection = ParamSetCollection$new(list(child = child))
  reads$child = 0L
  child_get_domain = child$get_domain
  unlockBinding("get_domain", child)
  child$get_domain = function(id) {
    reads$child = reads$child + 1L
    child_get_domain(id)
  }
  lockBinding("get_domain", child)

  expect_false(is.null(native_domains_call(collection)))
  expect_identical(reads$child, 0L)
})

test_that("domains rejects fancy locked-wrapper shadows without forcing", {
  skip_if_not(native_domains_available())
  namespace = asNamespace("paradox")

  for (kind in c("active", "delayed")) {
    for (target in c(".__ParamSet__ids", ".__ParamSet__.get_values")) {
      param_set = ps(x = p_int(0, 2, init = 1L))
      enclosure = param_set$.__enclos_env__
      reads = new.env(parent = emptyenv())
      reads$count = 0L
      value = get(target, envir = namespace, inherits = FALSE)
      if (kind == "active") {
        makeActiveBinding(target, function(replacement) {
          reads$count = reads$count + 1L
          value
        }, enclosure)
      } else {
        evaluation_environment = list2env(
          list(reads = reads, value = value),
          parent = baseenv()
        )
        delayedAssign(
          target,
          {
            reads$count = reads$count + 1L
            value
          },
          eval.env = evaluation_environment,
          assign.env = enclosure
        )
      }

      expect_null(native_domains_call(param_set), info = paste(kind, target))
      expect_identical(reads$count, 0L, info = paste(kind, target))
      expect_named(param_set$domains, "x")
      expect_true(reads$count > 0L, info = paste(kind, target))
    }
  }
})

test_that("collection graph checks distinguish cycles from shared DAGs", {
  skip_if_not(native_domains_available())
  child = ps(parent = p_int(0L, 2L), target = p_lgl())
  child$add_dep("target", "parent", CondEqual(1L))
  shared = ParamSetCollection$new(list(first = child, second = child))

  direct = native_domains_call(shared)
  expect_identical(direct, native_domains_reference(shared))
  expect_named(direct, c(
    "first.parent", "first.target", "second.parent", "second.target"
  ))
  expect_identical(
    direct$first.target$.requirements[[1L]][[1L]]$on,
    "first.parent"
  )
  expect_identical(
    direct$second.target$.requirements[[1L]][[1L]]$on,
    "second.parent"
  )

  cyclic = ParamSetCollection$new(list(loop = ps(x = p_dbl(0, 1))))
  cyclic$.__enclos_env__$private$.sets[[1L]] = cyclic
  expect_error(
    native_domains_call(cyclic),
    "ParamSetCollection nesting contains a cycle",
    fixed = TRUE
  )
  expect_error(
    cyclic$domains,
    "ParamSetCollection nesting contains a cycle",
    fixed = TRUE
  )
})

test_that("collection callbacks run values once before deps once", {
  skip_if_not(native_domains_available())
  collection = ParamSetCollection$new(list(
    child = ps(x = p_dbl(0, 1), y = p_int(0L, 2L))
  ))
  events = new.env(parent = emptyenv())
  events$seen = character()
  namespace = asNamespace("paradox")
  values_target = ".__ParamSetCollection__.get_values"
  deps_target = ".__ParamSetCollection__deps"
  suppressMessages(trace(
    values_target,
    tracer = function() events$seen = c(events$seen, "values"),
    where = namespace,
    print = FALSE
  ))
  suppressMessages(trace(
    deps_target,
    tracer = function() events$seen = c(events$seen, "deps"),
    where = namespace,
    print = FALSE
  ))
  on.exit(suppressMessages({
    untrace(values_target, where = namespace)
    untrace(deps_target, where = namespace)
  }), add = TRUE)

  direct = native_domains_call(collection)
  expect_named(direct, c("child.x", "child.y"))
  expect_identical(events$seen, c("values", "deps"))
})

test_that("collection snapshots permanent rows before live callback metadata", {
  skip_if_not(native_domains_available())
  collection = ParamSetCollection$new(list(
    child = ps(x = p_dbl(0, 1, tags = "before", trafo = exp))
  ))
  private = collection$.__enclos_env__$private
  original_ids = private$.params$id
  events = new.env(parent = emptyenv())
  events$calls = 0L
  namespace = asNamespace("paradox")
  target = ".__ParamSetCollection__.get_values"
  suppressMessages(trace(
    target,
    tracer = function() {
      events$calls = events$calls + 1L
      params = data.table::copy(private$.params)
      data.table::set(params, 1L, "lower", -100)
      private$.params = params
      private$.tags = data.table::data.table(
        id = "child.x", tag = "after", key = "id"
      )
      private$.trafos = data.table::data.table(
        id = "child.x", trafo = list(log), key = "id"
      )
    },
    where = namespace,
    print = FALSE
  ))
  on.exit(suppressMessages(untrace(target, where = namespace)), add = TRUE)

  direct = native_domains_call(collection)
  expect_identical(events$calls, 1L)
  expect_identical(direct$child.x$lower, 0)
  expect_identical(direct$child.x$.tags[[1L]], "after")
  expect_identical(direct$child.x$.trafo[[1L]], log)
  expect_identical(names(direct), original_ids)
  expect_false(identical(
    data.table::address(names(direct)),
    data.table::address(original_ids)
  ))
  expect_false(identical(
    data.table::address(names(direct)),
    data.table::address(private$.params$id)
  ))
})

test_that("collection result names survive in-place callback mutation", {
  skip_if_not(native_domains_available())
  collection = ParamSetCollection$new(list(
    child = ps(a = p_dbl(0, 1), b = p_int(0L, 2L))
  ))
  private = collection$.__enclos_env__$private
  events = new.env(parent = emptyenv())
  events$calls = 0L
  namespace = asNamespace("paradox")
  target = ".__ParamSetCollection__.get_values"
  suppressMessages(trace(
    target,
    tracer = function() {
      events$calls = events$calls + 1L
      data.table::set(private$.params, 1L, "id", "mutated")
    },
    where = namespace,
    print = FALSE
  ))
  on.exit(suppressMessages(untrace(target, where = namespace)), add = TRUE)

  direct = collection$domains
  expected_ids = c("child.a", "child.b")
  expect_identical(events$calls, 1L)
  expect_identical(names(direct), expected_ids)
  expect_identical(
    vapply(direct, `[[`, character(1L), "id", USE.NAMES = FALSE),
    expected_ids
  )
  expect_identical(private$.params$id, c("mutated", "child.b"))
  expect_false(identical(
    data.table::address(names(direct)),
    data.table::address(private$.params$id)
  ))
})

test_that("collection resolves the live deps binding after values", {
  skip_if_not(native_domains_available())
  collection = ParamSetCollection$new(list(
    child = ps(a = p_lgl(), b = p_lgl())
  ))
  collection$add_dep("child.b", "child.a", CondEqual(TRUE))
  replacement = data.table::copy(collection$deps)
  replacement$cond = list(CondEqual(FALSE))
  events = new.env(parent = emptyenv())
  events$values = 0L
  events$deps = 0L
  namespace = asNamespace("paradox")
  target = ".__ParamSetCollection__.get_values"
  suppressMessages(trace(
    target,
    tracer = function() {
      events$values = events$values + 1L
      makeActiveBinding("deps", function(value) {
        if (!missing(value)) {
          stop("replacement deps binding is read-only", call. = FALSE)
        }
        events$deps = events$deps + 1L
        replacement
      }, collection)
    },
    where = namespace,
    print = FALSE
  ))
  on.exit(suppressMessages(untrace(target, where = namespace)), add = TRUE)

  direct = collection$domains
  requirement = direct$child.b$.requirements[[1L]][[1L]]
  expect_identical(events$values, 1L)
  expect_identical(events$deps, 1L)
  expect_identical(requirement$on, "child.a")
  expect_identical(requirement$cond$rhs, FALSE)
})

test_that("post-callback corruption errors without fallback replay", {
  skip_if_not(native_domains_available())
  collection = ParamSetCollection$new(list(
    child = ps(x = p_dbl(0, 1), y = p_int(0L, 2L))
  ))
  private = collection$.__enclos_env__$private
  events = new.env(parent = emptyenv())
  events$calls = 0L
  namespace = asNamespace("paradox")
  target = ".__ParamSetCollection__.get_values"
  suppressMessages(trace(
    target,
    tracer = function() {
      events$calls = events$calls + 1L
      private$.tags = list()
    },
    where = namespace,
    print = FALSE
  ))
  on.exit(suppressMessages(untrace(target, where = namespace)), add = TRUE)

  expect_error(
    collection$domains,
    "Corrupt exact ParamSetCollection state after callbacks",
    fixed = TRUE
  )
  expect_identical(events$calls, 1L)
})

test_that("native domains rejects malformed storage atomically", {
  skip_if_not(native_domains_available())
  param_set = ps(
    x = p_dbl(0, 1, tags = "tag", trafo = exp),
    y = p_fct(c("a", "b"), init = "a")
  )
  private = param_set$.__enclos_env__$private
  params = data.table::copy(private$.params)
  tags = data.table::copy(private$.tags)
  trafos = data.table::copy(private$.trafos)
  deps = data.table::copy(private$.deps)

  expect_null(native_domains_with_private(param_set, ".params", 1L))
  expect_null(native_domains_with_private(param_set, ".params", unname(params)))
  expect_null(native_domains_with_private(
    param_set,
    ".params",
    native_domains_replace(params, "id", c("x", NA_character_))
  ))
  expect_null(native_domains_with_private(
    param_set,
    ".params",
    native_domains_replace(params, "cls", c("Unknown", "ParamFct"))
  ))
  expect_null(native_domains_with_private(
    param_set,
    ".params",
    native_domains_replace(params, "lower", list(0, NA))
  ))
  bad_levels = params$levels
  bad_levels[[2L]] = c("a", NA_character_)
  expect_null(native_domains_with_private(
    param_set,
    ".params",
    native_domains_replace(params, "levels", bad_levels)
  ))
  duplicate_params = data.table::rbindlist(list(params, params[1L]))
  expect_null(native_domains_with_private(
    param_set,
    ".params",
    duplicate_params
  ))

  unkeyed_tags = data.table::copy(tags)
  data.table::setattr(unkeyed_tags, "sorted", NULL)
  expect_null(native_domains_with_private(param_set, ".tags", unkeyed_tags))
  expect_null(native_domains_with_private(
    param_set,
    ".tags",
    native_domains_replace(tags, "tag", NA_character_)
  ))

  unkeyed_trafos = data.table::copy(trafos)
  data.table::setattr(unkeyed_trafos, "sorted", NULL)
  expect_null(native_domains_with_private(
    param_set,
    ".trafos",
    unkeyed_trafos
  ))
  expect_null(native_domains_with_private(
    param_set,
    ".trafos",
    native_domains_replace(trafos, "trafo", list(1L))
  ))
  duplicate_trafos = data.table::rbindlist(list(trafos, trafos))
  data.table::setkeyv(duplicate_trafos, "id")
  expect_null(native_domains_with_private(
    param_set,
    ".trafos",
    duplicate_trafos
  ))

  expect_null(native_domains_with_private(param_set, ".deps", unname(deps)))
  invalid_deps = data.table::data.table(id = "x", on = "y", cond = list(1L))
  expect_null(native_domains_with_private(param_set, ".deps", invalid_deps))
  expect_null(native_domains_with_private(
    param_set,
    ".values",
    unname(private$.values)
  ))
  duplicate_values = structure(list("a", "b"), names = c("y", "y"))
  expect_null(native_domains_with_private(
    param_set,
    ".values",
    duplicate_values
  ))
  expect_null(.Call(native_domains_symbol(), private, new.env()))
})

test_that("batch joins preserve encodings, unknown owners, and explicit NULL", {
  skip_if_not(native_domains_available())
  utf8 = enc2utf8("caf\u00e9")
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  Encoding(utf8) = "UTF-8"
  Encoding(latin1) = "latin1"
  skip_if(identical(charToRaw(utf8), charToRaw(latin1)))

  param_set = ps(x = p_dbl(0, 1, tags = "encoded", trafo = exp), y = p_lgl())
  private = param_set$.__enclos_env__$private
  data.table::set(private$.params, i = 1L, j = "id", value = utf8)
  private$.tags = data.table::data.table(
    id = c(latin1, "ghost"),
    tag = c("encoded", "ignored"),
    key = "id"
  )
  private$.trafos = data.table::data.table(
    id = c(latin1, "ghost", "ghost"),
    trafo = list(exp, identity, log),
    key = "id"
  )
  private$.deps = data.table::data.table(
    id = "ghost",
    on = "y",
    cond = list(CondEqual(TRUE))
  )
  private$.values = setNames(list(NULL, 1L), c(latin1, "ghost"))

  observed = native_domains_call(param_set)
  expect_identical(enc2utf8(names(observed)[[1L]]), enc2utf8(utf8))
  expect_identical(enc2utf8(observed[[1L]]$id), enc2utf8(utf8))
  expect_identical(observed[[1L]]$.tags[[1L]], "encoded")
  expect_identical(observed[[1L]]$.trafo[[1L]], exp)
  expect_true(observed[[1L]]$.init_given)
  expect_null(observed[[1L]]$.init[[1L]])
  expect_null(observed[[1L]]$.requirements[[1L]])
  expect_false(observed$y$.init_given)
})

test_that("native domains uses one cumulative interrupt budget", {
  skip_if_not(native_domains_available())
  size = 4000L
  ids = sprintf("parameter_%05d", seq_len(size))
  params = data.table::data.table(
    id = ids,
    cls = rep("ParamDbl", size),
    grouping = rep("ParamDbl", size),
    cargo = rep(list(NULL), size),
    lower = rep(0, size),
    upper = rep(1, size),
    tolerance = rep(0, size),
    levels = rep(list(NULL), size),
    special_vals = rep(list(list()), size),
    default = rep(list(paradox:::NO_DEF), size),
    storage_type = rep("numeric", size)
  )
  tags = data.table::data.table(id = ids, tag = rep("bulk", size))
  deps = data.table::data.table(
    id = ids,
    on = rep(ids[[1L]], size),
    cond = rep(list(CondEqual(0)), size)
  )
  trafos = data.table::data.table(id = character(), trafo = list())
  data.table::setkeyv(tags, "id")
  data.table::setkeyv(trafos, "id")

  param_set = ParamSet$new()
  private = param_set$.__enclos_env__$private
  private$.params = params
  private$.tags = tags
  private$.trafos = trafos
  private$.deps = deps
  private$.values = structure(list(), names = character())
  observed = native_domains_call(param_set)

  expect_length(observed, size)
  expect_identical(names(observed), ids)
  expect_identical(observed[[size]]$id, ids[[size]])
  expect_identical(observed[[size]]$.tags[[1L]], "bulk")
  expect_identical(
    observed[[size]]$.requirements[[1L]][[1L]]$on,
    ids[[1L]]
  )
  expect_identical(data.table:::selfrefok(observed[[size]], FALSE), 1L)
})

test_that("native ParamSet and collection domains survive forced collection", {
  skip_on_cran()

  skip_if_not(native_domains_available())
  param_set = ps(
    x = p_dbl(-1, 1, tags = c("a", "b"), trafo = exp),
    y = p_int(0, 3, init = 2L),
    z = p_fct(c("slow", "fast"))
  )
  param_set$add_dep("z", "y", CondEqual(2L))
  nested = ParamSetCollection$new(list(inner = param_set))
  collection = ParamSetCollection$new(list(
    nested = nested,
    shared = param_set
  ))
  collection$add_dep("nested.inner.z", "shared.y", CondEqual(2L))

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed = native_domains_call(param_set)
  observed_collection = native_domains_call(collection)
  gctorture(previous)

  expect_named(observed, c("x", "y", "z"))
  expect_identical(observed$x$.trafo[[1L]], exp)
  expect_identical(observed$y$.init[[1L]], 2L)
  expect_identical(observed$z$.requirements[[1L]][[1L]]$on, "y")
  expect_true(all(vapply(
    observed,
    function(domain) data.table:::selfrefok(domain, FALSE) == 1L,
    logical(1L)
  )))
  expect_named(observed_collection, c(
    "nested.inner.x", "nested.inner.y", "nested.inner.z",
    "shared.x", "shared.y", "shared.z"
  ))
  expect_identical(
    vapply(
      observed_collection$nested.inner.z$.requirements[[1L]],
      `[[`,
      character(1L),
      "on"
    ),
    c("nested.inner.y", "shared.y")
  )
  expect_true(all(vapply(
    observed_collection,
    function(domain) data.table:::selfrefok(domain, FALSE) == 1L,
    logical(1L)
  )))
})

native_domains_with_namespace_target = function(name, replacement, code) {
  namespace = asNamespace("paradox")
  original = get(name, envir = namespace, inherits = FALSE)
  unlockBinding(name, namespace)
  assign(name, replacement, envir = namespace)
  lockBinding(name, namespace)
  on.exit({
    unlockBinding(name, namespace)
    assign(name, original, envir = namespace)
    lockBinding(name, namespace)
  })
  force(code)
}

test_that("domains authenticates private collection controls before ALTREP", {
  skip_if_not(native_domains_available())
  collection = ParamSetCollection$new(list(
    left = ps(x = p_int()),
    right = ps(y = p_lgl())
  ))
  private = collection$.__enclos_env__$private
  original_sets = private$.sets
  original_postfix = private$.postfix
  on.exit({
    private$.sets = original_sets
    private$.postfix = original_postfix
  })

  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("private collection authentication invoked ALTREP", call. = FALSE)
  }
  stateful = function(first, later = first) {
    native_stateful_altrep(
      first,
      later,
      elt_switch_after = 0L,
      length_switch_after = 0L,
      callback = callback,
      callback_after = 0L
    )
  }

  # Both a correctly typed shell and a wrong-type ALTREP must decline before
  # asking Length, reading attributes, or dispatching an element method.
  private$.sets = stateful(original_sets, rev(original_sets))
  expect_null(native_domains_call(collection))
  private$.sets = stateful(names(original_sets))
  expect_null(native_domains_call(collection))
  private$.sets = original_sets

  changed_sets = original_sets
  changing_names = native_stateful_altrep(
    names(original_sets),
    rev(names(original_sets)),
    callback = callback,
    callback_after = c(NA_integer_, NA_integer_)
  )
  attr(changed_sets, "names") = changing_names
  native_stateful_altrep_rearm(changing_names, c(0L, 0L))
  private$.sets = changed_sets
  expect_null(native_domains_call(collection))
  private$.sets = original_sets

  private$.postfix = stateful(original_postfix, !original_postfix)
  expect_null(native_domains_call(collection))
  private$.postfix = stateful(as.integer(original_postfix))
  expect_null(native_domains_call(collection))
  expect_identical(callbacks, 0L)
})

test_that("domains rejects changing public callback lists without observing them", {
  skip_if_not(native_domains_available())
  namespace = asNamespace("paradox")
  target = ".__ParamSetCollection__.get_values"
  collection = ParamSetCollection$new(list(child = ps(x = p_int())))
  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("callback result ALTREP was observed", call. = FALSE)
  }

  growing_values = vector("list", 128L)
  for (index in seq_along(growing_values)) {
    growing_values[[index]] = 2L
  }
  names(growing_values) = sprintf("x%03d", seq_along(growing_values))
  cases = list(
    growing = list(
      structure(list(1L), names = "child.x"),
      growing_values
    ),
    shrinking = list(
      structure(list(1L, 2L), names = c("child.x", "extra")),
      structure(list(3L), names = "child.x")
    )
  )
  for (case in cases) {
    fixture = native_stateful_altrep(
      case[[1L]],
      case[[2L]],
      elt_switch_after = 0L,
      length_switch_after = 0L,
      callback = callback,
      callback_after = 0L
    )
    replacement = function(self, private, super) fixture
    expect_error(
      native_domains_with_namespace_target(
        target,
        replacement,
        native_domains_call(collection)
      ),
      "after callbacks: values",
      fixed = TRUE
    )
  }

  values = structure(list(1L), names = "child.x")
  changing_names = native_stateful_altrep(
    "child.x",
    rep("changed", 128L),
    callback = callback,
    callback_after = c(NA_integer_, NA_integer_)
  )
  attr(values, "names") = changing_names
  native_stateful_altrep_rearm(changing_names, c(0L, 0L))
  replacement = function(self, private, super) values
  expect_error(
    native_domains_with_namespace_target(
      target,
      replacement,
      native_domains_call(collection)
    ),
    "after callbacks: values",
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)
  expect_true(exists(target, envir = namespace, inherits = FALSE))
})

test_that("domains rejects changing public dependency tables before dispatch", {
  skip_if_not(native_domains_available())
  namespace = asNamespace("paradox")
  target = ".__ParamSetCollection__.get_values"
  original_target = get(target, envir = namespace, inherits = FALSE)
  collection = ParamSetCollection$new(list(child = ps(x = p_int())))
  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("dependency ALTREP was observed", call. = FALSE)
  }
  empty_deps = data.table::data.table(
    id = character(),
    on = character(),
    cond = list()
  )
  growing_deps = data.table::data.table(
    id = rep("child.x", 64L),
    on = rep("child.x", 64L),
    cond = rep(list(CondEqual(1L)), 64L)
  )
  fixture = native_stateful_altrep(
    empty_deps,
    growing_deps,
    elt_switch_after = 0L,
    length_switch_after = 0L,
    callback = callback,
    callback_after = 0L
  )
  replacement = function(self, private, super) {
    makeActiveBinding("deps", function(value) fixture, self)
    original_target(self, private, super)
  }

  expect_error(
    native_domains_with_namespace_target(
      target,
      replacement,
      native_domains_call(collection)
    ),
    "after callbacks: deps",
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)
})

test_that("collection domains grows rooted graph and frame plans", {
  skip_if_not(native_domains_available())
  leaf = ps(x = p_int(0L, 2L, init = 1L))

  # More than sixteen nested frames forces the native DFS stack to move.
  deep = leaf
  for (depth in seq_len(20L)) {
    deep = ParamSetCollection$new(setNames(
      list(deep),
      sprintf("level_%02d", depth)
    ))
  }
  observed_deep = native_domains_call(deep)
  expect_identical(observed_deep, native_domains_reference(deep))
  expect_length(observed_deep, 1L)
  expect_identical(observed_deep[[1L]]$.init[[1L]], 1L)

  # Forty references to one leaf exercise repeated DAG traversal and several
  # growths of the independently rooted graph carrier (initial capacity 32).
  owners = sprintf("owner_%02d", seq_len(40L))
  wide = ParamSetCollection$new(setNames(rep(list(leaf), 40L), owners))
  observed_wide = native_domains_call(wide)
  expect_identical(observed_wide, native_domains_reference(wide))
  expect_length(observed_wide, 40L)
  expect_setequal(names(observed_wide), paste0(owners, ".x"))
})

test_that("collection domains retains callback state across finalizer reentry", {
  skip_if_not(native_domains_available())
  namespace = asNamespace("paradox")
  target = ".__ParamSetCollection__.get_values"
  original_target = get(target, envir = namespace, inherits = FALSE)
  collection = ParamSetCollection$new(list(
    child = ps(x = p_int(tags = "before", init = 1L))
  ))
  collection_private = collection$.__enclos_env__$private
  after_tags = data.table::data.table(
    id = "child.x",
    tag = "after-finalizer",
    key = "id"
  )
  events = new.env(parent = emptyenv())
  events$ran = FALSE
  events$trigger = new.env(parent = emptyenv())
  reg.finalizer(events$trigger, function(ignored) {
    collection_private$.tags = after_tags
    events$ran = TRUE
  })

  replacement = function(self, private, super) {
    rm("trigger", envir = events)
    invisible(gc())
    original_target(self, private, super)
  }
  observed = native_domains_with_namespace_target(
    target,
    replacement,
    native_domains_call(collection)
  )
  expect_true(events$ran)
  expect_identical(observed$child.x$.tags[[1L]], "after-finalizer")
  expect_identical(observed$child.x$.init[[1L]], 1L)
})

test_that("collection domains detects private-owner replacement after callbacks", {
  skip_if_not(native_domains_available())
  namespace = asNamespace("paradox")
  target = ".__ParamSetCollection__.get_values"
  original_target = get(target, envir = namespace, inherits = FALSE)
  collection = ParamSetCollection$new(list(child = ps(x = p_int(init = 1L))))
  enclosure = collection$.__enclos_env__
  original_private = enclosure$private
  on.exit(enclosure$private <- original_private)
  calls = 0L

  replacement = function(self, private, super) {
    old_private = private
    calls <<- calls + 1L
    enclosure$private = new.env(parent = emptyenv())
    original_target(self, old_private, super)
  }
  expect_error(
    native_domains_with_namespace_target(
      target,
      replacement,
      native_domains_call(collection)
    ),
    "private environment changed",
    fixed = TRUE
  )
  expect_identical(calls, 1L)
})

test_that("deep collection domains survives focused frame-growth gctorture", {
  skip_on_cran()

  skip_if_not(native_domains_available())
  deep = ps(x = p_int(0L, 2L, init = 1L))
  for (depth in seq_len(18L)) {
    deep = ParamSetCollection$new(setNames(
      list(deep),
      sprintf("g%02d", depth)
    ))
  }

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed = native_domains_call(deep)
  gctorture(previous)

  expect_length(observed, 1L)
  expect_identical(observed[[1L]]$.init[[1L]], 1L)
  expect_identical(data.table:::selfrefok(observed[[1L]], FALSE), 1L)
})
