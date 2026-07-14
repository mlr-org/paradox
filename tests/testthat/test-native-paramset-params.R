native_params_available = function() {
  exists(
    "C_param_set_params",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

native_params_symbol = function() {
  get("C_param_set_params", envir = asNamespace("paradox"), inherits = FALSE)
}

native_params_call = function(param_set) {
  .Call(
    native_params_symbol(),
    param_set$.__enclos_env__$private,
    param_set
  )
}

# Frozen copy of the pre-native active binding. Keeping the data.table update
# joins here makes the differential independent of the native grouping code.
native_params_reference = function(param_set) {
  private = param_set$.__enclos_env__$private
  result = data.table::copy(private$.params)
  result[, .tags := list(param_set$tags)]
  result[private$.trafos, .trafo := list(trafo), on = "id"]
  result[
    param_set$deps,
    .requirements := mlr3misc::transpose_list(list(on, cond)),
    on = "id"
  ]
  vals = param_set$values
  result[, c(".init_given", ".init") := list(
    id %in% names(vals),
    unname(vals[id])
  )]
  result[]
}

native_params_replace = function(table, column, value) {
  result = unclass(table)
  result[[column]] = value
  attributes(result) = attributes(table)
  result
}

native_params_with_private = function(param_set, name, value) {
  private = param_set$.__enclos_env__$private
  original = private[[name]]
  on.exit(private[[name]] <- original)
  private[[name]] = value
  native_params_call(param_set)
}

native_params_clone_private = function(private, omit = character()) {
  result = new.env(parent = parent.env(private), hash = TRUE)
  bindings = setdiff(ls(private, all.names = TRUE), omit)
  for (binding in bindings) {
    assign(
      binding,
      get(binding, envir = private, inherits = FALSE),
      envir = result
    )
  }
  result
}

native_params_rich_set = function() {
  marker = new.env(parent = emptyenv())
  marker$value = 1L
  result = ps(
    factor = p_fct(
      c("slow", "fast"),
      tags = c("choice", "shared"),
      init = "fast"
    ),
    number = p_dbl(
      -2,
      2,
      tolerance = 1e-7,
      tags = c("numeric", "shared"),
      trafo = exp
    ),
    integer = p_int(
      -3L,
      3L,
      tags = "numeric",
      special_vals = list(-99L),
      init = 2L
    ),
    flag = p_lgl(tags = character()),
    payload = p_uty(custom_check = function(x) TRUE, init = marker)
  )
  tags = result$tags
  tags$integer = c("numeric", "numeric")
  result$tags = tags
  result$add_dep("factor", "integer", CondAnyOf(c(-1L, 2L)))
  # `$params` historically exposes the final update-join match when one child
  # has multiple dependency rows. This is distinct from `$get_domain()`.
  result$add_dep("factor", "flag", CondEqual(TRUE))
  result$add_dep("number", "integer", CondEqual(2L))
  result
}

test_that("native params is registered with a forced symbol", {
  skip_if_not(native_params_available())
  symbol = native_params_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)
  expect_error(
    .Call("param_set_params", PACKAGE = "paradox"),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("native params exactly reproduces rich data.table output", {
  skip_if_not(native_params_available())
  param_set = native_params_rich_set()
  private = param_set$.__enclos_env__$private
  params_before = lapply(private$.params, identity)
  tags_before = lapply(private$.tags, identity)
  trafos_before = lapply(private$.trafos, identity)
  deps_before = lapply(private$.deps, identity)
  values_before = lapply(private$.values, identity)

  expected = native_params_reference(param_set)
  direct = native_params_call(param_set)
  observed = param_set$params

  expect_identical(direct, expected)
  expect_identical(observed, expected)
  expect_identical(names(direct), paradox:::domain_names)
  expect_identical(class(direct), c("data.table", "data.frame"))
  expect_identical(data.table::key(direct), NULL)
  expect_identical(
    data.table::indices(direct),
    data.table::indices(private$.params)
  )
  expect_identical(data.table:::selfrefok(direct, FALSE), 1L)
  expect_identical(direct$.requirements[[1L]][[1L]], "flag")
  expect_identical(direct$.requirements[[1L]][[2L]], CondEqual(TRUE))
  expect_identical(direct$.requirements[[2L]][[1L]], "integer")
  expect_null(names(direct$.requirements[[1L]]))
  expect_identical(direct$.trafo[[2L]], exp)
  expect_identical(direct$.tags[[3L]], c("numeric", "numeric"))
  expect_true(direct$.init_given[[5L]])
  expect_identical(direct$.init[[5L]], private$.values$payload)

  expect_identical(lapply(private$.params, identity), params_before)
  expect_identical(lapply(private$.tags, identity), tags_before)
  expect_identical(lapply(private$.trafos, identity), trafos_before)
  expect_identical(lapply(private$.deps, identity), deps_before)
  expect_identical(lapply(private$.values, identity), values_before)
})

test_that("params output owns every mutable shell and keeps historical leaves", {
  skip_if_not(native_params_available())
  param_set = native_params_rich_set()
  private = param_set$.__enclos_env__$private
  first = native_params_call(param_set)
  second = native_params_call(param_set)
  original_indices = data.table::indices(private$.params)
  condition = private$.deps$cond[[3L]]
  marker = private$.values$payload

  expect_warning(
    data.table::set(first, i = 2L, j = "lower", value = -100),
    NA
  )
  expect_warning(
    data.table::set(first, i = 1L, j = ".tags", value = list("changed")),
    NA
  )
  expect_warning(data.table::set(first, j = "added", value = seq_len(nrow(first))), NA)
  first$levels[[1L]][[1L]] = "changed"
  first$special_vals[[3L]][[1L]] = -100L
  first$.requirements[[2L]][[1L]] = "changed"
  first$.requirements[[2L]][[2L]]$rhs = -1L
  data.table::setindexv(first, NULL)

  fresh = native_params_call(param_set)
  expect_identical(fresh, second)
  expect_identical(fresh$lower[[2L]], -2)
  expect_identical(fresh$.tags[[1L]], c("choice", "shared"))
  expect_identical(fresh$levels[[1L]], c("slow", "fast"))
  expect_identical(fresh$special_vals[[3L]], list(-99L))
  expect_identical(fresh$.requirements[[2L]][[1L]], "integer")
  expect_identical(private$.deps$cond[[3L]], condition)
  expect_false("added" %in% names(fresh))
  expect_identical(data.table::indices(private$.params), original_indices)

  # Environments and functions were already shared leaves after
  # data.table::copy(); preserving those identities is observable and useful.
  expect_identical(fresh$.init[[5L]], marker)
  expect_identical(fresh$.trafo[[2L]], private$.trafos$trafo[[1L]])
  first$.init[[5L]]$value = 42L
  expect_identical(marker$value, 42L)
  expect_identical(native_params_call(param_set)$.init[[5L]]$value, 42L)
  expect_identical(data.table:::selfrefok(fresh, FALSE), 1L)
})

test_that("empty and subset ParamSets preserve their exact table metadata", {
  skip_if_not(native_params_available())
  empty = ParamSet$new()
  empty_direct = native_params_call(empty)
  empty_expected = native_params_reference(empty)
  expect_identical(empty_direct, empty_expected)
  expect_identical(nrow(empty_direct), 0L)
  expect_identical(data.table:::selfrefok(empty_direct, FALSE), 1L)

  source = native_params_rich_set()
  index_free = source$subset(
    c("payload", "integer", "number"),
    allow_dangling_dependencies = TRUE
  )
  private = index_free$.__enclos_env__$private
  expect_identical(
    data.table::indices(private$.params),
    "id__cls__grouping"
  )
  observed = native_params_call(index_free)
  expected = native_params_reference(index_free)
  expect_identical(observed, expected)
  expect_identical(
    data.table::indices(observed),
    "id__cls__grouping"
  )
  expect_identical(data.table:::selfrefok(observed, FALSE), 1L)

  data.table::setindexv(private$.params, c("id", "cls"))
  observed = native_params_call(index_free)
  expected = native_params_reference(index_free)
  expect_identical(observed, expected)
  expect_identical(
    data.table::indices(observed),
    data.table::indices(private$.params)
  )
})

test_that("native params never observes callback-capable row-name metadata", {
  param_set = ps(first = p_dbl(), second = p_int())
  private = param_set$.__enclos_env__$private
  callbacks = 0L
  row_names = native_stateful_altrep(
    c(1L, 2L),
    integer(),
    elt_switch_after = 0L,
    length_switch_after = 0L,
    callback = function() {
      callbacks <<- callbacks + 1L
      stop("native params invoked inert row-name metadata", call. = FALSE)
    },
    callback_after = c(NA_integer_, NA_integer_)
  )
  original = private$.params
  on.exit(private$.params <- original)
  changed = private$.params
  attr(changed, "row.names") = row_names
  private$.params = changed
  attached = attr(private$.params, "row.names", exact = TRUE)
  native_stateful_altrep_rearm(attached, c(NA_integer_, 0L))

  observed = native_params_call(param_set)

  expect_identical(callbacks, 0L)
  expect_identical(observed$id, c("first", "second"))
  expect_identical(nrow(observed), 2L)
})

test_that("extensions, subclasses, and collections retain R dispatch", {
  skip_if_not(native_params_available())
  make_custom_domain = function() {
    paradox:::Domain(
      cls = "ParamParamsExtension",
      grouping = "ParamParamsExtension",
      storage_type = "list"
    )
  }
  custom = make_custom_domain()
  custom_set = ParamSet$new(list(custom = custom))
  expect_null(native_params_call(custom_set))
  expect_identical(custom_set$params, native_params_reference(custom_set))

  events = new.env(parent = emptyenv())
  events$seen = character()
  Subclass = R6::R6Class(
    "ParamSetParamsDispatchSubclass",
    inherit = ParamSet,
    active = list(
      tags = function(value) {
        if (!missing(value)) {
          super$tags <- value
          return(value)
        }
        events$seen = c(events$seen, "tags")
        super$tags
      },
      deps = function(value) {
        if (!missing(value)) {
          super$deps <- value
          return(value)
        }
        events$seen = c(events$seen, "deps")
        super$deps
      },
      values = function(value) {
        if (!missing(value)) {
          super$values <- value
          return(value)
        }
        events$seen = c(events$seen, "values")
        super$values
      }
    )
  )
  subclass = Subclass$new(list(x = p_int(0, 2, tags = "tag", init = 1L)))
  events$seen = character()
  expect_null(native_params_call(subclass))
  expect_identical(events$seen, character())
  subclass_params = subclass$params
  expect_s3_class(subclass_params, "data.table")
  expect_identical(events$seen, c("tags", "deps", "values"))

  child = ps(a = p_int(0, 3, tags = "snapshot", init = 1L), b = p_lgl())
  collection = ParamSetCollection$new(list(child = child))
  expect_null(native_params_call(collection))
  before = collection$params
  child$tags = list(a = "changed", b = character())
  child$values = list(a = 2L)
  child$add_dep("b", "a", CondEqual(2L))
  after = collection$params
  expect_identical(before$.tags[[1L]], "snapshot")
  expect_identical(after$.tags[[1L]], "snapshot")
  expect_identical(after$.init[[1L]], 2L)
  expect_identical(after$.requirements[[2L]][[1L]], "child.a")
  expect_identical(after$.requirements[[2L]][[2L]], CondEqual(2L))

  expect_null(.Call(
    native_params_symbol(),
    custom_set$.__enclos_env__$private,
    new.env()
  ))
  expect_error(custom_set$params <- NULL, "params is read-only.", fixed = TRUE)
})

test_that("fancy private bindings decline native params without evaluation", {
  skip_if_not(native_params_available())

  active_set = native_params_rich_set()
  active_expected = native_params_reference(active_set)
  enclosure = active_set$.__enclos_env__
  original_private = enclosure$private
  original_tags = original_private$.tags
  active_private = native_params_clone_private(original_private, ".tags")
  active_reads = 0L
  makeActiveBinding(".tags", function(value) {
    if (!missing(value)) {
      stop("unexpected write to active .tags binding")
    }
    active_reads <<- active_reads + 1L
    original_tags
  }, active_private)
  enclosure$private = active_private
  on.exit(enclosure$private <- original_private, add = TRUE)

  expect_null(native_params_call(active_set))
  expect_identical(active_reads, 0L)
  expect_identical(active_set$params, active_expected)
  expect_identical(active_reads, 1L)

  delayed_set = native_params_rich_set()
  delayed_expected = native_params_reference(delayed_set)
  delayed_private = delayed_set$.__enclos_env__$private
  delayed_tags = delayed_private$.tags
  delayed_reads = new.env(parent = emptyenv())
  delayed_reads$count = 0L
  promise_environment = list2env(
    list(reads = delayed_reads, value = delayed_tags),
    parent = baseenv()
  )
  delayedAssign(".tags", {
    reads$count = reads$count + 1L
    value
  }, eval.env = promise_environment, assign.env = delayed_private)
  on.exit(delayed_private$.tags <- delayed_tags, add = TRUE)

  expect_null(native_params_call(delayed_set))
  expect_identical(delayed_reads$count, 0L)
  expect_identical(delayed_set$params, delayed_expected)
  expect_identical(delayed_reads$count, 1L)
})

test_that("replaced public bindings decline native params before execution", {
  skip_if_not(native_params_available())
  param_set = native_params_rich_set()

  for (member in c("tags", "deps", "values")) {
    original = activeBindingFunction(member, param_set)
    reads = 0L
    replacement = function(value) {
      reads <<- reads + 1L
      original(value)
    }
    makeActiveBinding(member, replacement, param_set)

    expect_null(native_params_call(param_set), info = member)
    expect_identical(reads, 0L, info = member)
    observed = param_set$params
    expect_s3_class(observed, "data.table")
    expect_identical(reads, 1L, info = member)

    makeActiveBinding(member, original, param_set)
  }
})

test_that("generated ParamSet wrappers reject fancy local shadows without forcing", {
  skip_if_not(native_params_available())
  namespace = asNamespace("paradox")

  install_shadow = function(environment, name, value, kind, reads) {
    if (kind == "active") {
      makeActiveBinding(name, function(replacement) {
        reads$count = reads$count + 1L
        value
      }, environment)
    } else {
      evaluation_environment = list2env(
        list(reads = reads, value = value),
        parent = baseenv()
      )
      delayedAssign(
        name,
        {
          reads$count = reads$count + 1L
          value
        },
        eval.env = evaluation_environment,
        assign.env = environment
      )
    }
  }

  for (kind in c("active", "delayed")) {
    for (shadow in c(".__ParamSet__params", "super")) {
      param_set = ps(x = p_int(0, 2, init = 1L))
      wrapper_environment = environment(activeBindingFunction(
        "params",
        param_set
      ))
      reads = new.env(parent = emptyenv())
      reads$count = 0L
      value = if (shadow == "super") {
        NULL
      } else {
        get(shadow, envir = namespace, inherits = FALSE)
      }
      install_shadow(wrapper_environment, shadow, value, kind, reads)

      expect_null(native_params_call(param_set), info = paste(kind, shadow))
      expect_identical(reads$count, 0L, info = paste(kind, shadow))
      expect_s3_class(param_set$params, "data.table")
      if (shadow == "super") {
        expect_identical(reads$count, 0L, info = paste(kind, shadow))
      } else {
        expect_true(reads$count > 0L, info = paste(kind, shadow))
      }
    }
  }
})

test_that("reparented generated ParamSet wrappers retain literal fallback", {
  skip_if_not(native_params_available())
  param_set = ps(x = p_int(0, 2, init = 1L))
  namespace = asNamespace("paradox")
  target = get(
    ".__ParamSet__params",
    envir = namespace,
    inherits = FALSE
  )
  reads = new.env(parent = emptyenv())
  reads$count = 0L
  forged_parent = new.env(parent = namespace)
  forged_parent$.__ParamSet__params = function(
      self, private, super, rhs) {
    reads$count = reads$count + 1L
    target(self = self, private = private, super = super, rhs = rhs)
  }
  wrapper_environment = environment(activeBindingFunction(
    "params",
    param_set
  ))
  parent.env(wrapper_environment) = forged_parent

  expect_null(native_params_call(param_set))
  expect_identical(reads$count, 0L)
  expect_s3_class(param_set$params, "data.table")
  expect_identical(reads$count, 1L)
})

test_that("ParamSetCollection add updates a local native params snapshot", {
  skip_if_not(native_params_available())
  collection = ParamSetCollection$new(list(
    first = ps(x = p_int(0, 2, tags = "existing", init = 1L))
  ))
  added = ps(
    y = p_dbl(0, 1, tags = "added", trafo = exp),
    z = p_lgl(init = TRUE)
  )
  added_before = added$params

  expect_silent(collection$add(added, n = "second"))
  expect_identical(added$params, added_before)
  expect_identical(collection$ids(), c("first.x", "second.y", "second.z"))
  expect_identical(collection$params$.tags[[2L]], "added")
  expect_identical(collection$params$.trafo[[2L]], exp)
})

test_that("temporary data.table updates do not make params writable", {
  skip_if_not(native_params_available())
  param_set = ps(
    x = p_int(0, 2, init = 1L),
    y = p_dbl(-1, 1, tags = "numeric")
  )
  before = param_set$params

  added = param_set$params[, marker := seq_len(.N)]
  expect_identical(added$marker, c(1L, 2L))
  expect_identical(names(param_set$params), names(before))
  expect_identical(param_set$params, before)

  replaced = param_set$params[, lower := c(-10, -20)]
  expect_identical(replaced$lower, c(-10, -20))
  expect_identical(param_set$params, before)

  set_added = data.table::set(
    param_set$params,
    j = "set_marker",
    value = c("x", "y")
  )
  expect_identical(set_added$set_marker, c("x", "y"))
  expect_identical(param_set$params, before)

  expect_error(
    param_set$params <- added,
    "params is read-only.",
    fixed = TRUE
  )
  expect_error(
    assign("params", added, envir = param_set),
    "params is read-only.",
    fixed = TRUE
  )
  expect_identical(param_set$params, before)
})

test_that("native params rejects malformed state atomically", {
  skip_if_not(native_params_available())
  param_set = ps(
    x = p_dbl(0, 1, tags = "tag", trafo = exp),
    y = p_fct(c("a", "b"), init = "a")
  )
  private = param_set$.__enclos_env__$private
  params = data.table::copy(private$.params)
  tags = data.table::copy(private$.tags)
  trafos = data.table::copy(private$.trafos)
  deps = data.table::copy(private$.deps)

  expect_null(native_params_with_private(param_set, ".params", 1L))
  expect_null(native_params_with_private(param_set, ".params", unname(params)))
  expect_null(native_params_with_private(
    param_set,
    ".params",
    native_params_replace(params, "id", c("x", NA_character_))
  ))
  expect_null(native_params_with_private(
    param_set,
    ".params",
    native_params_replace(params, "cls", c("Unknown", "ParamFct"))
  ))
  expect_null(native_params_with_private(
    param_set,
    ".params",
    native_params_replace(params, "lower", list(0, NA))
  ))
  attributed_column = params$lower
  attr(attributed_column, "note") = TRUE
  expect_null(native_params_with_private(
    param_set,
    ".params",
    native_params_replace(params, "lower", attributed_column)
  ))
  duplicate_params = data.table::rbindlist(list(params, params[1L]))
  expect_null(native_params_with_private(
    param_set,
    ".params",
    duplicate_params
  ))
  attributed_params = data.table::copy(params)
  attr(attributed_params, "note") = TRUE
  expect_null(native_params_with_private(
    param_set,
    ".params",
    attributed_params
  ))
  no_row_names = data.table::copy(params)
  attr(no_row_names, "row.names") = NULL
  expect_null(native_params_with_private(param_set, ".params", no_row_names))
  explicit_row_names = data.table::copy(params)
  data.table::setattr(explicit_row_names, "row.names", c("a", "b"))
  expect_null(native_params_with_private(
    param_set,
    ".params",
    explicit_row_names
  ))

  unkeyed_tags = data.table::copy(tags)
  data.table::setattr(unkeyed_tags, "sorted", NULL)
  expect_null(native_params_with_private(param_set, ".tags", unkeyed_tags))
  unknown_tags = data.table::data.table(id = "ghost", tag = "tag", key = "id")
  expect_null(native_params_with_private(param_set, ".tags", unknown_tags))
  expect_null(native_params_with_private(
    param_set,
    ".tags",
    native_params_replace(tags, "tag", NA_character_)
  ))

  unkeyed_trafos = data.table::copy(trafos)
  data.table::setattr(unkeyed_trafos, "sorted", NULL)
  expect_null(native_params_with_private(
    param_set,
    ".trafos",
    unkeyed_trafos
  ))
  duplicate_trafos = data.table::rbindlist(list(trafos, trafos))
  data.table::setkeyv(duplicate_trafos, "id")
  expect_null(native_params_with_private(
    param_set,
    ".trafos",
    duplicate_trafos
  ))
  unknown_trafos = data.table::data.table(
    id = "ghost",
    trafo = list(identity),
    key = "id"
  )
  expect_null(native_params_with_private(
    param_set,
    ".trafos",
    unknown_trafos
  ))

  expect_null(native_params_with_private(param_set, ".deps", unname(deps)))
  invalid_deps = data.table::data.table(id = "x", on = "y", cond = list(1L))
  expect_null(native_params_with_private(param_set, ".deps", invalid_deps))
  unknown_deps = data.table::data.table(
    id = "ghost",
    on = "x",
    cond = list(CondEqual(1))
  )
  original_deps = private$.deps
  private$.deps = unknown_deps
  expected_unknown_deps = native_params_reference(param_set)
  private$.deps = original_deps
  expect_identical(
    native_params_with_private(param_set, ".deps", unknown_deps),
    expected_unknown_deps
  )

  expect_null(native_params_with_private(
    param_set,
    ".values",
    unname(private$.values)
  ))
  duplicate_values = structure(list("a", "b"), names = c("y", "y"))
  expect_null(native_params_with_private(
    param_set,
    ".values",
    duplicate_values
  ))
  unknown_values = structure(list(1L), names = "ghost")
  expect_null(native_params_with_private(
    param_set,
    ".values",
    unknown_values
  ))
  attributed_values = private$.values
  attr(attributed_values, "note") = TRUE
  expect_null(native_params_with_private(
    param_set,
    ".values",
    attributed_values
  ))
})

test_that("native joins use R encoding equality and preserve source spelling", {
  skip_if_not(native_params_available())
  utf8 = enc2utf8("caf\u00e9")
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  Encoding(utf8) = "UTF-8"
  Encoding(latin1) = "latin1"
  skip_if(identical(charToRaw(utf8), charToRaw(latin1)))

  param_set = ps(x = p_dbl(0, 1))
  private = param_set$.__enclos_env__$private
  data.table::set(private$.params, i = 1L, j = "id", value = utf8)
  private$.tags = data.table::data.table(id = latin1, tag = "encoded", key = "id")
  private$.trafos = data.table::data.table(id = latin1, trafo = list(exp), key = "id")
  private$.deps = data.table::data.table(
    id = latin1,
    on = "parent",
    cond = list(CondEqual(1))
  )
  private$.values = setNames(list(NULL), latin1)

  expected = native_params_reference(param_set)
  observed = native_params_call(param_set)
  expect_identical(observed, expected)
  expect_identical(Encoding(observed$id), "UTF-8")
  expect_identical(observed$.tags[[1L]], "encoded")
  expect_identical(observed$.trafo[[1L]], exp)
  expect_identical(observed$.requirements[[1L]][[1L]], "parent")
  expect_true(observed$.init_given[[1L]])
  expect_null(observed$.init[[1L]])
})

test_that("random canonical ParamSets match the frozen R implementation", {
  skip_if_not(native_params_available())
  set.seed(20260714)
  for (iteration in seq_len(40L)) {
    size = sample(0:48, 1L)
    domains = lapply(seq_len(size), function(index) {
      tags = sample(c("first", "second", "third"), sample(0:3, 1L))
      switch(
        as.character((index - 1L) %% 5L),
        "0" = p_dbl(0, 10, tags = tags, trafo = if (index %% 2L) sqrt),
        "1" = p_int(-3L, 7L, tags = tags, init = sample(-3:7, 1L)),
        "2" = {
          levels = sample(letters[1:6], sample(2:6, 1L))
          p_fct(levels, tags = tags, init = levels[[1L]])
        },
        "3" = p_lgl(tags = tags, init = sample(c(TRUE, FALSE), 1L)),
        "4" = p_uty(default = list(index = index))
      )
    })
    names(domains) = sprintf("parameter_%03d", seq_len(size))
    param_set = ParamSet$new(domains)
    if (size >= 4L) {
      for (start in seq.int(1L, size, by = 5L)) {
        if (start + 3L > size) next
        child = names(domains)[[start + 2L]]
        param_set$add_dep(child, names(domains)[[start + 3L]], CondEqual(TRUE))
        param_set$add_dep(
          child,
          names(domains)[[start + 1L]],
          CondAnyOf(c(-3L, 0L))
        )
      }
    }
    expect_identical(
      native_params_call(param_set),
      native_params_reference(param_set),
      info = sprintf("iteration %d, size %d", iteration, size)
    )
  }
})

test_that("native params survives forced collection", {
  skip_on_cran()

  skip_if_not(native_params_available())
  param_set = native_params_rich_set()
  private = param_set$.__enclos_env__$private
  data.table::setindexv(private$.params, c("id", "cls"))
  expected = native_params_reference(param_set)
  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed = native_params_call(param_set)
  gctorture(previous)

  expect_identical(observed, expected)
  expect_identical(
    data.table::indices(observed),
    data.table::indices(private$.params)
  )
  expect_identical(data.table:::selfrefok(observed, FALSE), 1L)
})
