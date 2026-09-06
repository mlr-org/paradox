native_get_values_call = function(self,
    private = self$.__enclos_env__$private,
    class = NULL, tags = NULL, any_tags = NULL,
    type = "with_token", check_required = TRUE,
    remove_dependencies = TRUE) {
  .Call(C_param_set_get_values, private, self, environment())
}
environment(native_get_values_call) = asNamespace("paradox")

native_get_values_internal_domain = function() {
  p_int(
    0L,
    10L,
    tags = "internal_tuning",
    in_tune_fn = function(domain, param_vals) domain$upper,
    aggr = function(x) x[[1L]],
    disable_in_tune = list()
  )
}

native_get_values_copy_table = function(table) {
  structure(
    lapply(table, identity),
    names = names(table),
    # The internal spelling, not `attr(table, "row.names")`: the plain read
    # expands compact row names into an ALTREP sequence, and the capsule
    # validators reject ALTREP row names by design.
    row.names = .row_names_info(table, 0L),
    class = "data.frame"
  )
}

native_get_values_copy_state = function(param_set) {
  state = paradox:::param_set_core_state(
    param_set$.__enclos_env__$private
  )
  setNames(lapply(state, identity), names(state))
}

test_that("get_values is one registered native operation", {
  namespace = asNamespace("paradox")
  native = get("C_param_set_get_values", envir = namespace)
  expect_s3_class(native, "NativeSymbolInfo")
  expect_identical(native$numParameters, 3L)
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
  expect_error(
    .Call("param_set_get_values", NULL, NULL, NULL, PACKAGE = "paradox"),
    "not available"
  )
})

test_that("get_values filters built-in kinds, tags, tokens, and named NULL", {
  ordinary = to_tune()
  internal = to_tune(upper = 5, internal = TRUE)
  param_set = ps(
    integer = p_int(tags = c("required", "train")),
    logical = p_lgl(tags = "train"),
    ordinary = p_int(0L, 10L, tags = "tune"),
    internal = native_get_values_internal_domain(),
    nullable = p_uty(special_vals = list(NULL))
  )
  param_set$values = list(
    integer = 2L,
    logical = TRUE,
    ordinary = ordinary,
    internal = internal,
    nullable = NULL
  )

  expect_identical(param_set$get_values(), param_set$values)
  expect_identical(
    param_set$get_values(type = "without_token"),
    list(integer = 2L, logical = TRUE, nullable = NULL)
  )
  expect_identical(
    param_set$get_values(type = "only_token"),
    list(ordinary = ordinary, internal = internal)
  )
  expect_identical(
    param_set$get_values(type = "with_internal"),
    list(internal = internal)
  )
  expect_identical(
    param_set$get_values(class = "ParamInt", tags = "train"),
    list(integer = 2L)
  )
  expect_identical(
    param_set$get_values(any_tags = c("internal_tuning", "train")),
    list(integer = 2L, logical = TRUE, internal = internal)
  )

  param_set$values = named_list()
  expect_identical(
    param_set$get_values(check_required = FALSE),
    setNames(list(), character())
  )
})

test_that("dependencies are transitive and independent of row order", {
  param_set = ps(
    root = p_int(),
    middle = p_int(),
    leaf = p_int()
  )
  param_set$add_dep("middle", "root", CondEqual(1L))
  param_set$add_dep("leaf", "middle", CondEqual(1L))
  param_set$assert_values = FALSE
  param_set$values = list(root = 0L, middle = 1L, leaf = 2L)

  expect_identical(param_set$get_values(), list(root = 0L))
  expect_identical(
    param_set$get_values(remove_dependencies = FALSE),
    list(root = 0L, middle = 1L, leaf = 2L)
  )

  reversed = param_set$deps[c(2L, 1L)]
  param_set$deps = reversed
  expect_identical(param_set$get_values(), list(root = 0L))

  param_set$values = list(root = to_tune(), middle = 1L, leaf = 2L)
  expect_identical(
    names(param_set$get_values()),
    c("root", "middle", "leaf")
  )
})

test_that("wide chain and star dependency projections retain linear semantics", {
  make_wide_set = function(size, shape) {
    ids = sprintf("parameter_%04d", seq_len(size))
    domains = setNames(
      lapply(seq_len(size), function(index) p_lgl()),
      ids
    )
    param_set = ParamSet$new(domains)
    condition = CondEqual(TRUE)
    parents = if (identical(shape, "chain")) {
      ids[-size]
    } else {
      rep(ids[[1L]], size - 1L)
    }
    param_set$deps = data.table::data.table(
      id = ids[-1L],
      on = parents,
      cond = rep(list(condition), size - 1L)
    )
    values = setNames(as.list(rep(TRUE, size)), ids)
    param_set$values = values
    list(param_set = param_set, values = values)
  }

  chain = make_wide_set(512L, "chain")
  expect_identical(
    chain$param_set$get_values(remove_dependencies = FALSE),
    chain$values
  )
  expect_identical(chain$param_set$get_values(), chain$values)

  # Resolution must not rely on a topological dependency-table order.
  dependencies = chain$param_set$deps
  chain$param_set$deps = dependencies[rev(seq_len(nrow(dependencies)))]
  expect_identical(chain$param_set$get_values(), chain$values)

  star = make_wide_set(512L, "star")
  expect_identical(
    star$param_set$get_values(remove_dependencies = FALSE),
    star$values
  )
  expect_identical(star$param_set$get_values(), star$values)
})

test_that("ID indexing preserves equivalent and unknown mixed encodings", {
  utf8 = enc2utf8(c("caf\u00e9", "\u00e9l\u00e8ve", "inconnu\u00e9"))
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  skip_if(
    anyNA(latin1),
    "this platform cannot represent the latin1 fixtures"
  )
  Encoding(latin1) = "latin1"

  equivalent = ps(
    parent = p_lgl(tags = "required"),
    child = p_lgl()
  )
  equivalent$add_dep("child", "parent", CondEqual(TRUE))
  equivalent$values = list(parent = TRUE, child = TRUE)
  private = equivalent$.__enclos_env__$private
  state = native_get_values_copy_state(equivalent)
  state$.params = native_get_values_copy_table(state$.params)
  state$.params$id = utf8[1:2]
  state$.values = setNames(unname(state$.values), latin1[1:2])
  state$.tags = native_get_values_copy_table(state$.tags)
  state$.tags$id[[1L]] = latin1[[1L]]
  state$.deps = native_get_values_copy_table(state$.deps)
  state$.deps$id[[1L]] = latin1[[2L]]
  state$.deps$on[[1L]] = latin1[[1L]]
  private$.core = .Call(C_param_set_core_new, 1L, state)

  invisible(gc())
  observed = equivalent$get_values()
  expect_identical(unname(observed), list(TRUE, TRUE))
  expect_identical(enc2utf8(names(observed)), utf8[1:2])
  expect_identical(Encoding(names(observed)), c("UTF-8", "UTF-8"))

  missing = native_get_values_copy_state(equivalent)
  missing$.values = setNames(list(TRUE), latin1[[2L]])
  private$.core = .Call(C_param_set_core_new, 1L, missing)
  error = tryCatch(equivalent$get_values(), error = identity)
  expect_s3_class(error, "error")
  expect_identical(
    enc2utf8(conditionMessage(error)),
    paste0("Missing required parameters: ", utf8[[1L]])
  )

  unknown = latin1[[3L]]
  values = ps(cafe = p_lgl())
  values$values = list(cafe = TRUE)
  private = values$.__enclos_env__$private
  state = native_get_values_copy_state(values)
  names(state$.values) = unknown
  private$.core = .Call(C_param_set_core_new, 1L, state)
  expect_error(
    values$get_values(),
    "Corrupt ParamSet capsule: invalid `.values` field",
    fixed = TRUE
  )

  tags = ps(cafe = p_lgl(tags = "required"))
  private = tags$.__enclos_env__$private
  state = native_get_values_copy_state(tags)
  state$.tags = native_get_values_copy_table(state$.tags)
  state$.tags$id[[1L]] = unknown
  private$.core = .Call(C_param_set_core_new, 1L, state)
  expect_error(
    tags$get_values(),
    "Corrupt ParamSet capsule: invalid `.tags` field",
    fixed = TRUE
  )

  dependencies = ps(parent = p_lgl(), cafe = p_lgl())
  dependencies$add_dep("cafe", "parent", CondEqual(TRUE))
  private = dependencies$.__enclos_env__$private
  state = native_get_values_copy_state(dependencies)
  state$.deps = native_get_values_copy_table(state$.deps)
  state$.deps$id[[1L]] = unknown
  private$.core = .Call(C_param_set_core_new, 1L, state)
  expect_error(
    dependencies$get_values(remove_dependencies = FALSE),
    "Corrupt ParamSet capsule: dependency owner is unknown",
    fixed = TRUE
  )
})

test_that("raw dependency reads skip activity only after full admission", {
  cyclic = ps(a = p_lgl(), b = p_lgl())
  cyclic$add_dep("a", "b", CondEqual(TRUE))
  cyclic$add_dep("b", "a", CondEqual(TRUE))
  cyclic$assert_values = FALSE
  cyclic$values = list(a = TRUE, b = TRUE)
  cyclic$assert_values = TRUE

  expect_identical(
    cyclic$get_values(remove_dependencies = FALSE),
    list(a = TRUE, b = TRUE)
  )
  expect_error(cyclic$get_values(), "cycle", ignore.case = TRUE)

  required = ps(a = p_lgl(tags = "required"), b = p_lgl())
  required$add_dep("a", "b", CondEqual(TRUE))
  required$add_dep("b", "a", CondEqual(TRUE))
  required$assert_values = FALSE
  required$values = list(a = TRUE, b = TRUE)
  required$assert_values = TRUE
  expect_error(
    required$get_values(remove_dependencies = FALSE),
    "cycle",
    ignore.case = TRUE
  )

  malformed = native_get_values_copy_state(cyclic)
  malformed$.deps = native_get_values_copy_table(malformed$.deps)
  malformed$.deps$cond[1L] = list(list())
  cyclic$.__enclos_env__$private$.core = .Call(
    C_param_set_core_new,
    1L,
    malformed
  )
  expect_error(
    cyclic$get_values(remove_dependencies = FALSE),
    "Unsupported Condition class",
    fixed = TRUE
  )
})

test_that("required values are checked globally before result filtering", {
  param_set = ps(
    zeta = p_int(tags = "required"),
    alpha = p_lgl(tags = "required"),
    visible = p_int(init = 1L, tags = "visible")
  )
  expect_error(
    param_set$get_values(tags = "visible"),
    "Missing required parameters: zeta, alpha",
    fixed = TRUE
  )

  param_set$values = list(zeta = 2L, visible = 1L)
  expect_error(
    param_set$get_values(tags = "visible"),
    "Missing required parameters: alpha",
    fixed = TRUE
  )
  expect_identical(
    param_set$get_values(tags = "visible", check_required = FALSE),
    list(visible = 1L)
  )
})

test_that("arguments cross the native boundary once in formal order", {
  events = character()
  mark = function(label, value) {
    events <<- c(events, label)
    value
  }
  param_set = ps(a = p_int(init = 1L), b = p_lgl(init = TRUE))

  expect_identical(
    param_set$get_values(
      class = mark("class", NULL),
      tags = mark("tags", NULL),
      any_tags = mark("any_tags", NULL),
      type = mark("type", "with_token"),
      check_required = mark("check_required", TRUE),
      remove_dependencies = mark("remove_dependencies", TRUE)
    ),
    list(a = 1L, b = TRUE)
  )
  expect_identical(events, c(
    "class", "tags", "any_tags", "type", "check_required",
    "remove_dependencies"
  ))

  events = character()
  expect_error(
    param_set$get_values(
      class = mark("class", NULL),
      tags = mark("tags", 1),
      any_tags = mark("any_tags", stop("forced too late")),
      type = mark("type", "with_token")
    ),
    "Assertion on 'tags' failed"
  )
  expect_identical(events, c("class", "tags"))

  callback_state = new.env(parent = emptyenv())
  callback_state$count = 0L
  observed = param_set$get_values(class = {
    callback_state$count = callback_state$count + 1L
    param_set$values = list(b = FALSE)
    NULL
  })
  expect_identical(callback_state$count, 1L)
  expect_identical(observed, list(b = FALSE))
})

test_that("flags and type reject arbitrary historical quirks", {
  param_set = ps(a = p_int(init = 1L))
  expect_error(param_set$get_values(type = NULL), "Assertion on 'type' failed")
  expect_error(param_set$get_values(type = "token"), "Assertion on 'type' failed")
  expect_error(
    param_set$get_values(check_required = 1),
    "Assertion on 'check_required' failed"
  )
  expect_error(
    param_set$get_values(remove_dependencies = NA),
    "Assertion on 'remove_dependencies' failed"
  )
})

test_that("collections flatten shared and nested capsule graphs", {
  child = ps(a = p_int(init = 1L), token = p_int(0L, 10L))
  child$values$token = to_tune()
  shared = ParamSetCollection$new(list(left = child, right = child))
  outer = ParamSetCollection$new(list(
    nested = shared,
    tail = ps(z = p_dbl(init = 0.5))
  ))

  expect_identical(outer$get_values(), list(
    nested.left.a = 1L,
    nested.left.token = child$values$token,
    nested.right.a = 1L,
    nested.right.token = child$values$token,
    tail.z = 0.5
  ))
  expect_identical(
    outer$get_values(type = "without_token"),
    list(nested.left.a = 1L, nested.right.a = 1L, tail.z = 0.5)
  )
})

test_that("get_values refreshes SHADOW exactly through its capsule edge", {
  origin = ps(hidden = p_int(), x = p_int(), flag = p_lgl())
  origin$values = list(hidden = 9L, x = 1L, flag = TRUE)
  shadow = ParamSetShadow$new(origin, "hidden")

  expect_identical(shadow$get_values(), list(x = 1L, flag = TRUE))
  origin$values = list(hidden = 7L, x = 2L)
  expect_identical(shadow$get_values(), list(x = 2L))

  collection = ParamSetCollection$new(list(view = shadow))
  origin$values = list(hidden = 8L, flag = FALSE)
  expect_identical(collection$get_values(), list(view.flag = FALSE))
})

test_that("additive subclasses use the same capsule engine", {
  LabelledParamSet = R6::R6Class(
    "NativeGetValuesLabelledParamSet",
    inherit = ParamSet,
    public = list(
      label = NULL,
      initialize = function(params, label) {
        super$initialize(params)
        self$label = label
      }
    )
  )
  param_set = LabelledParamSet$new(list(x = p_int(init = 1L)), "fixture")
  expect_identical(param_set$label, "fixture")
  expect_identical(param_set$get_values(), list(x = 1L))
})

test_that("malformed capsules error instead of replaying or crashing", {
  param_set = ps(x = p_int(init = 1L))
  private = param_set$.__enclos_env__$private
  state = private$.state()
  state$.values = structure(list(1L), names = "ghost")
  private$.core = .Call(C_param_set_core_new, 1L, state)

  expect_error(
    param_set$get_values(),
    "Corrupt ParamSet capsule: invalid `.values` field",
    fixed = TRUE
  )

  reordered = ps(a = p_int(), b = p_int())
  reordered$values = list(a = 1L, b = 2L)
  private = reordered$.__enclos_env__$private
  state = native_get_values_copy_state(reordered)
  state$.values = state$.values[c(2L, 1L)]
  private$.core = .Call(C_param_set_core_new, 1L, state)
  expect_error(
    reordered$get_values(),
    "Corrupt ParamSet capsule: invalid `.values` field",
    fixed = TRUE
  )

  unknown_owner = ps(parent = p_lgl(), child = p_lgl())
  unknown_owner$add_dep("child", "parent", CondEqual(TRUE))
  unknown_owner$values = list(parent = TRUE, child = TRUE)
  private = unknown_owner$.__enclos_env__$private
  state = native_get_values_copy_state(unknown_owner)
  state$.deps = native_get_values_copy_table(state$.deps)
  state$.deps$id[[1L]] = "ghost"
  private$.core = .Call(C_param_set_core_new, 1L, state)
  expect_error(
    unknown_owner$get_values(remove_dependencies = FALSE),
    "Corrupt ParamSet capsule: dependency owner is unknown",
    fixed = TRUE
  )
})

test_that("results own their list and names shells", {
  marker = new.env(parent = emptyenv())
  param_set = ps(marker = p_uty(), value = p_int())
  param_set$values = list(marker = marker, value = 1L)
  first = param_set$get_values()
  second = param_set$get_values()

  expect_identical(first, second)
  expect_false(identical(data.table::address(first), data.table::address(second)))
  expect_false(identical(
    data.table::address(names(first)),
    data.table::address(names(second))
  ))
  expect_identical(first$marker, marker)
  names(first)[[1L]] = "changed"
  first$value = 3L
  expect_identical(param_set$get_values(), second)
})

test_that("serialized and cloned graphs remain capsule-readable", {
  child = ps(x = p_int(init = 1L), y = p_lgl(init = TRUE))
  collection = ParamSetCollection$new(list(left = child, right = child))
  cases = list(
    child$clone(deep = FALSE),
    child$clone(deep = TRUE),
    unserialize(serialize(child, NULL, version = 3L)),
    collection$clone(deep = FALSE),
    collection$clone(deep = TRUE),
    unserialize(serialize(collection, NULL, version = 3L))
  )
  for (object in cases) {
    expect_identical(native_get_values_call(object), object$get_values())
  }
})
