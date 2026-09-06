# All changed storage below is made with ordinary R operations. Private edits
# have no semantic guarantee, but native readers must retain safe access and
# must not validate unrelated fields merely to diagnose those edits early.
operation_space = function() {
  ps(x = p_dbl(0, 1), y = p_int(1, 3), f = p_fct(c("a", "b")))
}

operation_replace = function(set, column, value) {
  private = set$.__enclos_env__$private
  state = unserialize(serialize(paradox:::param_set_core_state(private), NULL))
  params = unclass(state$.params)
  params[column] = list(value)
  state$.params = params
  private$.core = .Call(paradox:::C_param_set_core_new, 1L, state)
  invisible(set)
}

test_that("getters admit only their own private columns", {
  expected = operation_space()
  fields = c("class", "lower", "upper", "levels", "storage_type", "length",
    "is_empty", "is_number", "is_categ", "is_bounded", "nlevels")
  dependencies = list(
    class = "cls", lower = "lower", upper = "upper", levels = "levels",
    storage_type = "storage_type", length = character(), is_empty = character(),
    is_number = "cls", is_categ = "cls", is_bounded = c("cls", "lower", "upper"),
    nlevels = c("cls", "lower", "upper", "levels"))
  for (column in c("cls", "lower", "upper", "levels", "storage_type",
    "grouping", "cargo", "special_vals", "default", "tolerance")) {
    set = operation_replace(operation_space(), column, new.env(parent = emptyenv()))
    for (field in fields) {
      info = paste(column, field)
      if (column %in% dependencies[[field]]) {
        expect_error(set[[field]], "Corrupt ParamSet", info = info)
      } else {
        expect_identical(set[[field]], expected[[field]], info = info)
      }
    }
  }
})

test_that("selected column lengths and types remain guarded", {
  for (column in c("id", "cls", "lower", "upper", "levels", "storage_type")) {
    for (replacement in list(NULL, function() NULL, quote(x),
      new.env(parent = emptyenv()), raw(), list(1), character(), TRUE)) {
      set = operation_replace(operation_space(), column, replacement)
      field = switch(column, id = "class", cls = "class", column)
      expect_error(set[[field]], "Corrupt ParamSet", info = paste(column, typeof(replacement)))
    }
  }
})

test_that("value and transformation readers ignore unused Domain semantics", {
  for (column in c("grouping", "levels", "special_vals", "tolerance", "lower", "upper")) {
    set = operation_space()
    set$values = list(x = 0.5, y = 2L, f = "a")
    set = operation_replace(set, column, new.env(parent = emptyenv()))
    expect_identical(set$values, list(x = 0.5, y = 2L, f = "a"))
    expect_identical(set$get_values(), list(x = 0.5, y = 2L, f = "a"))
    expect_identical(set$trafo(list(x = 0.5)), list(x = 0.5))
  }
})

test_that("nested carriers are checked where their contents are read", {
  for (replacement in list(NULL, 1, function() NULL, quote(x), new.env())) {
    set = operation_replace(operation_space(), "levels", list(NULL, NULL, replacement))
    expect_error(set$test(list(f = "a")), "Corrupt ParamSet")
    expect_error(set$qunif(matrix(0.5, ncol = 1L, dimnames = list(NULL, "f"))),
      "Corrupt ParamSet")
    expect_error(generate_design_random(set, 1L), "Corrupt ParamSet")
    set = operation_replace(operation_space(), "special_vals", list(list(), list(), replacement))
    expect_error(set$test(list(f = "a")), "Corrupt ParamSet")
  }
})

test_that("Design planning admits the storage_type column it indexes", {
  # Fixed-value classification and typed missing patches read `storage_type`
  # by parameter row. The plan's reader mask must therefore cover that column:
  # a private edit that shortens or retypes it is rejected before indexing
  # rather than read out of bounds.
  data = data.table::data.table(x = 0.5, y = 2L, f = "a")
  for (replacement in list(character(), "numeric", new.env(parent = emptyenv()),
    NULL, 1:3, list("numeric", "integer", "character"))) {
    set = operation_space()
    set$values = list(x = 0.5)
    operation_replace(set, "storage_type", replacement)
    expect_error(
      Design$new(set, data.table::copy(data), remove_dupl = FALSE),
      "Corrupt ParamSet",
      info = typeof(replacement)
    )
  }
  set = operation_space()
  set$values = list(x = 0.5)
  design = Design$new(set, data.table::copy(data), remove_dupl = FALSE)
  expect_identical(design$data$x, 0.5)
})

test_that("shared native readers retain the column shells they consume", {
  all_columns = names(paradox:::param_set_core_state(
    operation_space()$.__enclos_env__$private)$.params)
  readers = list(
    values = function(x) x$values,
    get_values = function(x) x$get_values(),
    trafo = function(x) x$trafo(list(x = 0.5)),
    qunif = function(x) x$qunif(matrix(0.5, ncol = 1L,
      dimnames = list(NULL, "x"))),
    random = function(x) generate_design_random(x, 1L),
    check = function(x) x$test(list(x = 0.5)),
    params = function(x) x$params,
    domains = function(x) x$domains,
    get_domain = function(x) x$get_domain("x"),
    subset = function(x) x$subset("x"),
    collection = function(x) ParamSetCollection$new(list(child = x)),
    shadow = function(x) ParamSetShadow$new(x, "y")
  )
  required = list(
    values = c("id", "cls"),
    get_values = c("id", "cls", "default"),
    trafo = c("id", "default"),
    qunif = c("id", "cls", "lower", "upper", "tolerance", "levels", "storage_type"),
    random = c("id", "cls", "lower", "upper", "levels")
  )
  for (reader in names(readers)) {
    columns = required[[reader]]
    if (is.null(columns)) columns = all_columns
    for (column in columns) {
      for (replacement in list(new.env(parent = emptyenv()), character())) {
        set = operation_space()
        set$values = list(x = 0.5, y = 2L, f = "a")
        operation_replace(set, column, replacement)
        # Exact diagnostics are not promised after private mutation, but the
        # required carrier must be rejected before any native indexing.
        expect_error(readers[[reader]](set), info = paste(reader, column))
      }
    }
  }
})

test_that("private semantics are not a second public admission engine", {
  set = operation_replace(operation_space(), "levels", list(NULL, NULL, c("a", "a")))
  expect_true(set$test(list(f = "a")))
  expect_identical(set$nlevels, c(x = Inf, y = 3, f = 2))
  expect_identical(set$lower, c(x = 0, y = 1, f = NA_real_))
  expect_error(p_dbl(2, 1), "lower")
  expect_error(p_int(0.5, 2), "lower")
  expect_error(p_fct(c("a", "a")), "unique|duplicat")
  expect_error(operation_space()$assert(list(x = 2)), ">=|<=")
})

test_that("sampling admits private columns after count callbacks and finalizers", {
  set = operation_space()
  params = paradox:::param_set_core_state(set$.__enclos_env__$private)$.params
  state = new.env(parent = emptyenv())
  state$called = FALSE
  state$error = NULL
  # Define this outside `arm`: its enclosing environment must not keep the
  # finalizer's target alive. This is ordinary R-level private mutation.
  finalize = function(unused) {
    state$called = TRUE
    state$error = tryCatch({
      data.table::set(params, j = "lower", value = rep("changed", 3L))
      NULL
    }, error = identity)
  }
  arm = function() {
    target = new.env(parent = emptyenv())
    reg.finalizer(target, finalize)
    rm(target)
    gc()
  }
  # The ALTREP values and length never change. Only its public Elt callback
  # runs R; the selected private columns must be checked afterwards.
  n = native_stateful_altrep(3L, 3L, callback = arm, callback_after = 0L)
  expect_error(.Call(paradox:::C_sampler_unif_sample_builtin, set, n),
    "Corrupt ParamSet sampling state")
  expect_true(state$called)
  expect_null(state$error)
})

test_that("Collection value reads do not admit unused schema and translation fields", {
  child = operation_space()
  child$values = list(f = "a", x = 0.5, y = 2L)
  collection = ParamSetCollection$new(list(owner = child))
  expected = list(owner.x = 0.5, owner.y = 2L, owner.f = "a")
  expect_identical(collection$values, expected)
  for (node in list(child, collection)) {
    params = paradox:::param_set_core_state(node$.__enclos_env__$private)$.params
    data.table::set(params, j = "lower", value = rep("unused", 3L))
    data.table::set(params, j = "special_vals", value = rep(list(new.env()), 3L))
  }
  translation = paradox:::param_set_core_state(
    collection$.__enclos_env__$private)$.translation
  data.table::set(translation, j = "original_id", value = rep(NA_character_, 3L))
  data.table::set(translation, j = "owner_ps_index", value = rep(.Machine$integer.max, 3L))
  expect_identical(collection$values, expected)
  expect_identical(collection$get_values(), expected)
  expect_false(collection$has_deps)
  # Schema operations still guard the fields/indices they actually consume.
  expect_error(collection$subset("owner.x"), "Corrupt ParamSet")
})

test_that("dependency counts do not interpret private Conditions", {
  child = ps(x = p_int(init = 1L), y = p_dbl(init = 0.5, depends = x == 1L))
  collection = ParamSetCollection$new(list(owner = child))
  expect_true(collection$has_deps)
  deps = paradox:::param_set_core_state(child$.__enclos_env__$private)$.deps
  data.table::set(deps, j = "cond", value = list(new.env(parent = emptyenv())))
  expect_true(child$has_deps)
  expect_true(collection$has_deps)
  expect_identical(collection$values, list(owner.x = 1L, owner.y = 0.5))
  expect_error(collection$get_values(), "Corrupt|condition|Condition")
})

test_that("Collection callback flags and sampling read their selected fields only", {
  child = ps(x = p_dbl(0, 1), y = p_int(0, 3))
  collection = ParamSetCollection$new(list(owner = child))
  # Warm the public schema, then edit unrelated private value stores without
  # changing any field used by sampling or callback enumeration.
  expect_false(collection$has_trafo)
  expect_false(collection$has_constraint)
  paradox:::param_set_core_replace(child$.__enclos_env__$private,
    values = list(unknown = new.env(parent = emptyenv())))
  expect_false(collection$has_trafo)
  expect_false(collection$has_constraint)
  set.seed(99)
  expected = .Call(paradox:::C_sampler_unif_sample_builtin, child, 4L)
  set.seed(99)
  result = .Call(paradox:::C_sampler_unif_sample_builtin, collection, 4L)
  expect_identical(unname(as.list(result)), unname(as.list(expected)))
  expect_named(result, c("owner.x", "owner.y"))
  expect_length(result[[1L]], 4L)
  expect_true(all(result[[1L]] >= 0 & result[[1L]] <= 1))
  expect_true(all(result[[2L]] >= 0 & result[[2L]] <= 3))
  params = paradox:::param_set_core_state(collection$.__enclos_env__$private)$.params
  data.table::set(params, j = "lower", value = rep("invalid", 2L))
  expect_error(.Call(paradox:::C_sampler_unif_sample_builtin, collection, 4L),
    "Corrupt ParamSet sampling state")
})

test_that("stored-value mapping preserves order, subsets, encodings and opaque leaves", {
  id = "a"
  node = ParamSet$new(setNames(list(p_int(), p_dbl(), p_uty()), c(id, "b", "opaque")))
  opaque = new.env(parent = emptyenv())
  for (values in list(setNames(list(2L, 0.5, opaque), c(id, "b", "opaque")),
    setNames(list(opaque, 0.5, 2L), c("opaque", "b", id)),
    setNames(list(2L), id), list(b = 0.5))) {
    node$values = values
    expect_identical(node$values, values[intersect(node$ids(), names(values))])
    # Ordinary public assignment canonicalizes order. Exercise the general
    # read mapping with a reordered private store as well.
    paradox:::param_set_core_replace(node$.__enclos_env__$private, values = values)
    expect_identical(node$values, values)
    collection = ParamSetCollection$new(list(left = node, right = node))
    expected = c(setNames(values, paste0("left.", names(values))),
      setNames(values, paste0("right.", names(values))))
    expect_identical(collection$values, expected)
    expect_identical(collection$clone(deep = TRUE)$values, expected)
  }
  utf8 = enc2utf8("caf\u00e9")
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  if (!is.na(latin1)) {
    Encoding(latin1) = "latin1"
    encoded = operation_replace(ps(x = p_int()), "id", utf8)
    paradox:::param_set_core_replace(encoded$.__enclos_env__$private,
      values = setNames(list(3L), latin1))
    expect_identical(unname(encoded$values), list(3L))
    expect_identical(enc2utf8(names(encoded$values)), utf8)
  }
  expect_error({ node$values = list(b = 0.1, b = 0.2) }, "duplicat|unique")
  expect_error({ node$values = list(missing = 0.1) }, "not available|unknown")
})

test_that("public value and schema changes refresh nested shared Shadows", {
  origin = ps(hidden = p_int(init = 1L), x = p_dbl(0, 1, init = 0.5))
  shadow = ParamSetShadow$new(origin, "hidden")
  inner = ParamSetCollection$new(list(left = shadow, right = shadow))
  outer = ParamSetCollection$new(list(nested = inner))
  for (iteration in seq_len(4L)) {
    value = iteration / 4
    origin$values = list(hidden = iteration, x = value)
    expect_identical(shadow$values, list(x = value))
    expected = list(nested.left.x = value, nested.right.x = value)
    expect_identical(outer$values, expected)
    expect_identical(outer$get_values(), expected)
    expect_identical(outer$clone(deep = TRUE)$values, expected)
    restored = unserialize(serialize(outer, NULL))
    expect_identical(restored$values, expected)
    restored$sets[[1L]]$sets[[1L]]$origin$values = list(hidden = 5L, x = 0.125)
    expect_identical(restored$values, list(nested.left.x = 0.125, nested.right.x = 0.125))
    expect_identical(outer$values, expected)
  }
  origin$tags = list(hidden = character(), x = "required")
  expect_identical(shadow$tags$x, "required")
  expect_identical(outer$tags$nested.left.x, "required")
})

test_that("malformed stored-value carriers and unknown rows retain safe errors", {
  for (value in list(NULL, 1, new.env(), function() NULL, list(1),
    setNames(list(1), "missing"))) {
    node = ps(x = p_int())
    paradox:::param_set_core_replace(node$.__enclos_env__$private, values = value)
    expect_error(node$values, "Corrupt")
    collection = ParamSetCollection$new(list(owner = node))
    expect_error(collection$values, "Corrupt")
  }
})

test_that("the general stored-value matcher does not dispatch through name metadata", {
  node = ps(x = p_int(), y = p_int())
  values = list(y = 1L, x = 2L)
  attr(values, "names") = structure(c("y", "x"), class = "POSIXlt")
  paradox:::param_set_core_replace(node$.__enclos_env__$private, values = values)
  expect_error(node$values, "Corrupt ParamSet")
  collection = ParamSetCollection$new(list(owner = node))
  expect_error(collection$values, "Corrupt ParamSet")
})

test_that("fused Collection detachment roots the selected value generation across callbacks", {
  first = ps(x = p_int())
  second = ps(y = p_int(init = 2L))
  collection = ParamSetCollection$new(list(a = first, b = second))
  called = FALSE
  value = native_stateful_altrep(1L, 1L, callback = function() {
    called <<- TRUE
    second$values = list()
    collection$add(ps(z = p_int(init = 3L)), "c")
    gc(full = TRUE)
  }, callback_after = 0L)
  # Public assignment owns/materializes typed values. Inject a well-behaved
  # ALTREP leaf privately to exercise the reader's callback/lifetime boundary.
  paradox:::param_set_core_replace(first$.__enclos_env__$private, values = list(x = value))
  expect_identical(collection$values, list(a.x = 1L, b.y = 2L))
  expect_true(called)
  expect_identical(second$values, setNames(list(), character()))
  expect_identical(collection$ids(), c("a.x", "b.y", "c.z"))
})
