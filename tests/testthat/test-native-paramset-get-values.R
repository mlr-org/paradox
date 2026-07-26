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
