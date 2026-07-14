native_get_values_available = function() {
  exists(
    "C_param_set_get_values",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

native_get_values_call = function(self,
    private = self$.__enclos_env__$private,
    class = NULL, tags = NULL, any_tags = NULL,
    type = "with_token", check_required = TRUE,
    remove_dependencies = TRUE) {
  .Call(C_param_set_get_values, private, self, environment())
}
environment(native_get_values_call) = asNamespace("paradox")

native_get_values_condition = function(class_name, callback) {
  method = function(cond, x) cond$callback(x)
  registerS3method(
    "condition_test",
    class_name,
    method,
    envir = asNamespace("paradox")
  )
  condition = CondEqual(1L)
  condition$callback = callback
  class(condition) = c(class_name, class(condition))
  condition
}

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

test_that("native get_values and lazy ids use forced registered symbols", {
  skip_if_not(native_get_values_available())
  namespace = asNamespace("paradox")
  get_values = get("C_param_set_get_values", envir = namespace)
  lazy_ids = get("C_param_set_ids_lazy", envir = namespace)

  expect_s3_class(get_values, "NativeSymbolInfo")
  expect_identical(get_values$numParameters, 3L)
  expect_s3_class(lazy_ids, "NativeSymbolInfo")
  expect_identical(lazy_ids$numParameters, 2L)
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
  expect_error(
    .Call("param_set_get_values", NULL, NULL, NULL, PACKAGE = "paradox"),
    "not available"
  )
})

test_that("native get_values preserves base filtering and token semantics", {
  skip_if_not(native_get_values_available())
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

  expect_identical(native_get_values_call(param_set), param_set$values)
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
})

test_that("native get_values preserves collection affixes and nesting", {
  skip_if_not(native_get_values_available())
  left = ps(a = p_int(init = 1L), token = p_int(0L, 10L))
  left$values$token = to_tune()
  right = ps(b = p_lgl(init = TRUE))
  inner = ParamSetCollection$new(
    list(left = left, right = right),
    postfix_names = TRUE
  )
  outer = ParamSetCollection$new(list(nested = inner, tail = ps(
    z = p_dbl(init = 0.5)
  )))

  direct = native_get_values_call(outer)
  expect_false(is.null(direct))
  expect_identical(direct, list(
    nested.a.left = 1L,
    nested.token.left = left$values$token,
    nested.b.right = TRUE,
    tail.z = 0.5
  ))
  expect_identical(
    outer$get_values(type = "without_token"),
    list(nested.a.left = 1L, nested.b.right = TRUE, tail.z = 0.5)
  )
  expect_identical(
    outer$get_values(type = "only_token"),
    list(nested.token.left = left$values$token)
  )
})

test_that("ids and get_values force filters sequentially", {
  skip_if_not(native_get_values_available())
  events = new.env(parent = emptyenv())
  events$seen = character()
  mark = function(label, value) {
    events$seen = c(events$seen, label)
    value
  }
  param_set = ps(a = p_int(init = 1L), b = p_int(init = 2L))

  expect_error(
    param_set$ids(
      class = mark("class", 1),
      tags = mark("tags", stop("tags forced too early")),
      any_tags = mark("any_tags", NULL)
    ),
    "Must be of type 'character'"
  )
  expect_identical(events$seen, "class")

  events$seen = character()
  expect_error(
    param_set$ids(
      class = mark("class", "ParamInt"),
      tags = mark("tags", 1),
      any_tags = mark("any_tags", stop("any_tags forced too early"))
    ),
    "Assertion on 'tags' failed"
  )
  expect_identical(events$seen, c("class", "tags"))

  events$seen = character()
  expect_identical(
    param_set$ids(
      class = mark("class", "ParamInt"),
      tags = mark("tags", NULL),
      any_tags = mark("any_tags", NULL)
    ),
    c("a", "b")
  )
  expect_identical(events$seen, c("class", "tags", "any_tags"))
})

test_that("get_values retains the complete promise and callback order", {
  skip_if_not(native_get_values_available())
  events = new.env(parent = emptyenv())
  events$seen = character()
  mark = function(label, value) {
    events$seen = c(events$seen, label)
    value
  }
  condition = native_get_values_condition(
    "NativeGetValuesOrderCondition",
    function(x) {
      events$seen = c(events$seen, "condition")
      TRUE
    }
  )
  param_set = ps(a = p_int(init = 1L), b = p_int(init = 2L))
  param_set$add_dep("b", "a", condition)

  expect_error(
    param_set$get_values(
      type = mark("type", "with_token"),
      check_required = mark("check_required", TRUE),
      remove_dependencies = mark("remove_dependencies", TRUE),
      class = mark("class", 1),
      tags = mark("tags", stop("tags forced too early")),
      any_tags = mark("any_tags", NULL)
    ),
    "Assertion on 'class' failed"
  )
  expect_identical(events$seen, c(
    "type", "check_required", "remove_dependencies", "condition", "class"
  ))

  events$seen = character()
  expect_error(
    param_set$get_values(
      type = mark("type", "invalid"),
      check_required = mark("check_required", stop("forced too early")),
      remove_dependencies = mark("remove_dependencies", TRUE),
      class = mark("class", NULL)
    ),
    "Assertion on 'type' failed"
  )
  expect_identical(events$seen, "type")

  events$seen = character()
  expect_error(
    param_set$get_values(
      type = mark("type", "with_token"),
      check_required = mark("check_required", 1),
      remove_dependencies = mark("remove_dependencies", stop("forced")),
      class = mark("class", NULL)
    ),
    "Assertion on 'check_required' failed"
  )
  expect_identical(events$seen, c("type", "check_required"))
})

test_that("required diagnostics precede filters and use original names", {
  skip_if_not(native_get_values_available())
  events = new.env(parent = emptyenv())
  events$filters = 0L
  param_set = ps(
    zeta = p_int(tags = "required"),
    alpha = p_lgl(tags = "required"),
    visible = p_int(init = 1L)
  )

  expect_error(
    param_set$get_values(class = {
      events$filters = events$filters + 1L
      "ParamInt"
    }),
    "Missing required parameters: zeta, alpha",
    fixed = TRUE
  )
  expect_identical(events$filters, 0L)

  param_set$values = list(zeta = 2L, visible = 1L)
  expect_error(
    param_set$get_values(tags = "absent"),
    "Missing required parameters: alpha",
    fixed = TRUE
  )
  expect_identical(
    param_set$get_values(tags = "absent", check_required = FALSE),
    setNames(list(), character())
  )
})

test_that("dependency rows use one local values snapshot in row order", {
  skip_if_not(native_get_values_available())
  events = new.env(parent = emptyenv())
  events$seen = character()
  condition = function(label, answer) native_get_values_condition(
    paste0("NativeGetValuesSequence", label),
    function(x) {
      events$seen = c(events$seen, paste0(label, ":", deparse(x)))
      answer
    }
  )
  param_set = ps(
    root = p_int(),
    middle = p_int(),
    leaf = p_int()
  )
  param_set$add_dep("middle", "root", condition("first", FALSE))
  param_set$add_dep("leaf", "middle", condition("second", FALSE))
  param_set$assert_values = FALSE
  param_set$values = list(root = 0L, middle = 1L, leaf = 2L)

  expect_identical(param_set$get_values(), list(root = 0L))
  expect_identical(events$seen, c("first:0L", "second:NULL"))

  events$seen = character()
  expect_identical(
    param_set$get_values(remove_dependencies = FALSE),
    list(root = 0L, middle = 1L, leaf = 2L)
  )
  expect_identical(events$seen, character())

  events$seen = character()
  param_set$values = list(root = to_tune(), middle = 1L, leaf = 2L)
  expect_identical(names(param_set$get_values()), c("root", "middle"))
  expect_identical(events$seen, "second:1L")
})

test_that("dependency answers retain exact isTRUE semantics", {
  skip_if_not(native_get_values_available())
  answers = list(
    TRUE,
    FALSE,
    1,
    0,
    NA,
    logical(),
    c(TRUE, TRUE),
    structure(TRUE, class = "NativeGetValuesClassedAnswer"),
    structure(1, class = "NativeGetValuesClassedAnswer")
  )
  for (index in seq_along(answers)) {
    answer = answers[[index]]
    condition = native_get_values_condition(
      sprintf("NativeGetValuesAnswer%02d", index),
      function(x) answer
    )
    param_set = ps(root = p_int(init = 0L), child = p_int(init = 1L))
    param_set$add_dep("child", "root", condition)
    expected = if (isTRUE(answer)) {
      list(root = 0L, child = 1L)
    } else {
      list(root = 0L)
    }
    expect_identical(
      param_set$get_values(check_required = FALSE),
      expected,
      info = sprintf("answer %d", index)
    )
  }
})

test_that("remove_dependencies observes both live dependency row counts", {
  skip_if_not(native_get_values_available())
  events = new.env(parent = emptyenv())
  events$dim = 0L
  events$condition = 0L
  class_name = "NativeGetValuesChangingDependencyDim"
  registerS3method("dim", class_name, function(x) {
    events$dim = events$dim + 1L
    c(if (events$dim == 1L) 1L else 0L, length(x))
  }, envir = asNamespace("base"))
  condition = native_get_values_condition(
    "NativeGetValuesChangingDependencyCondition",
    function(x) {
      events$condition = events$condition + 1L
      FALSE
    }
  )
  param_set = ps(root = p_int(init = 0L), child = p_int(init = 1L))
  param_set$add_dep("child", "root", condition)
  dependencies = param_set$.__enclos_env__$private$.deps

  expect_identical(
    param_set$get_values(
      check_required = FALSE,
      remove_dependencies = {
        data.table::setattr(
          dependencies,
          "class",
          c(class_name, class(dependencies))
        )
        TRUE
      }
    ),
    list(root = 0L, child = 1L)
  )
  expect_identical(events$dim, 2L)
  expect_identical(events$condition, 0L)
})

test_that("dependency callbacks expose later live column and generic changes", {
  skip_if_not(native_get_values_available())
  events = new.env(parent = emptyenv())
  events$seen = character()
  holder = new.env(parent = emptyenv())
  param_set = ps(
    root = p_int(init = 0L),
    first = p_int(init = 1L),
    second = p_int(init = 2L)
  )
  private = param_set$.__enclos_env__$private
  holder$replacement = native_get_values_condition(
    "NativeGetValuesReplacementCondition",
    function(x) {
      events$seen = c(events$seen, "replacement-column")
      TRUE
    }
  )
  holder$stale = native_get_values_condition(
    "NativeGetValuesStaleCondition",
    function(x) {
      events$seen = c(events$seen, "stale-column")
      TRUE
    }
  )
  holder$first = native_get_values_condition(
    "NativeGetValuesColumnMutationCondition",
    function(x) {
      events$seen = c(events$seen, "first-column")
      data.table::set(
        private$.deps,
        j = "cond",
        value = list(holder$first, holder$replacement)
      )
      TRUE
    }
  )
  param_set$add_dep("first", "root", holder$first)
  param_set$add_dep("second", "root", holder$stale)
  expect_identical(
    param_set$get_values(check_required = FALSE),
    list(root = 0L, first = 1L, second = 2L)
  )
  expect_identical(events$seen, c("first-column", "replacement-column"))

  namespace = asNamespace("paradox")
  original = get("condition_test", envir = namespace, inherits = FALSE)
  was_locked = bindingIsLocked("condition_test", namespace)
  restore = function() {
    if (bindingIsLocked("condition_test", namespace)) {
      unlockBinding("condition_test", namespace)
    }
    assign("condition_test", original, envir = namespace)
    if (was_locked) lockBinding("condition_test", namespace)
  }
  on.exit(restore(), add = TRUE)
  replace_generic = function() {
    unlockBinding("condition_test", namespace)
    assign("condition_test", function(cond, x) {
      events$seen = c(events$seen, "replacement-generic")
      TRUE
    }, envir = namespace)
    lockBinding("condition_test", namespace)
  }
  first = native_get_values_condition(
    "NativeGetValuesGenericMutationCondition",
    function(x) {
      events$seen = c(events$seen, "first-generic")
      replace_generic()
      TRUE
    }
  )
  stale = native_get_values_condition(
    "NativeGetValuesStaleGenericCondition",
    function(x) {
      events$seen = c(events$seen, "stale-generic")
      TRUE
    }
  )
  param_set = ps(
    root = p_int(init = 0L),
    first = p_int(init = 1L),
    second = p_int(init = 2L)
  )
  param_set$add_dep("first", "root", first)
  param_set$add_dep("second", "root", stale)
  events$seen = character()
  expect_identical(
    param_set$get_values(check_required = FALSE),
    list(root = 0L, first = 1L, second = 2L)
  )
  expect_identical(events$seen, c("first-generic", "replacement-generic"))
})

test_that("in-place values-name mutation retains original membership safely", {
  skip_if_not(native_get_values_available())
  param_set = ps(first = p_int(init = 1L), second = p_int(init = 2L))
  private = param_set$.__enclos_env__$private
  condition = native_get_values_condition(
    "NativeGetValuesNamesMutationCondition",
    function(x) {
      data.table::setattr(private$.values, "names", c("x", "y"))
      gc(FALSE)
      TRUE
    }
  )
  param_set$add_dep("second", "first", condition)

  expect_identical(
    param_set$get_values(check_required = FALSE),
    setNames(list(), character())
  )
  expect_identical(names(param_set$values), c("x", "y"))
})

test_that("dependency removal detaches the in-flight values shell", {
  skip_if_not(native_get_values_available())
  param_set = ps(
    root = p_int(init = 0L),
    removed = p_int(init = 1L),
    later = p_int(init = 2L)
  )
  private = param_set$.__enclos_env__$private
  remove = native_get_values_condition(
    "NativeGetValuesDetachRemovalCondition",
    function(x) FALSE
  )
  mutate_private = native_get_values_condition(
    "NativeGetValuesDetachMutationCondition",
    function(x) {
      data.table::setattr(private$.values, "names", c("x", "y", "z"))
      gc(FALSE)
      TRUE
    }
  )
  param_set$add_dep("removed", "root", remove)
  param_set$add_dep("later", "root", mutate_private)

  expect_identical(
    param_set$get_values(check_required = FALSE),
    list(root = 0L, later = 2L)
  )
  expect_identical(names(param_set$values), c("x", "y", "z"))
})

test_that("an unmatched removal assignment still detaches its shell", {
  skip_if_not(native_get_values_available())
  param_set = ps(
    first = p_int(init = 1L),
    renamed = p_int(init = 2L),
    later = p_int(init = 3L)
  )
  private = param_set$.__enclos_env__$private
  rename_then_remove = native_get_values_condition(
    "NativeGetValuesUnmatchedRemovalCondition",
    function(x) {
      data.table::setattr(
        private$.values,
        "names",
        c("first", "other", "later")
      )
      FALSE
    }
  )
  mutate_private = native_get_values_condition(
    "NativeGetValuesAfterUnmatchedRemovalCondition",
    function(x) {
      data.table::setattr(private$.values, "names", c("x", "y", "z"))
      gc(FALSE)
      TRUE
    }
  )
  param_set$add_dep("renamed", "first", rename_then_remove)
  param_set$add_dep("later", "first", mutate_private)

  expect_identical(
    param_set$get_values(check_required = FALSE),
    list(first = 1L, later = 3L)
  )
  expect_identical(names(param_set$values), c("x", "y", "z"))
})

test_that("token filtering detaches the in-flight values shell", {
  skip_if_not(native_get_values_available())
  param_set = ps(first = p_int(init = 1L), second = p_int(init = 2L))
  private = param_set$.__enclos_env__$private

  expect_identical(
    param_set$get_values(
      type = "without_token",
      check_required = FALSE,
      class = {
        data.table::setattr(private$.values, "names", c("x", "y"))
        gc(FALSE)
        NULL
      }
    ),
    list(first = 1L, second = 2L)
  )
  expect_identical(names(param_set$values), c("x", "y"))
})

test_that("condition errors stop before filters without replay", {
  skip_if_not(native_get_values_available())
  events = new.env(parent = emptyenv())
  events$conditions = 0L
  events$filters = 0L
  condition = native_get_values_condition(
    "NativeGetValuesErrorCondition",
    function(x) {
      events$conditions = events$conditions + 1L
      stop("condition exploded")
    }
  )
  param_set = ps(a = p_int(init = 1L), b = p_int(init = 2L))
  param_set$add_dep("b", "a", condition)

  expect_error(
    param_set$get_values(class = {
      events$filters = events$filters + 1L
      NULL
    }),
    "condition exploded",
    fixed = TRUE
  )
  expect_identical(events$conditions, 1L)
  expect_identical(events$filters, 0L)
})

test_that("native Condition calls document the allowed introspection edge", {
  skip_if_not(native_get_values_available())
  captured = new.env(parent = emptyenv())
  class_name = "NativeGetValuesIntrospectionCondition"
  registerS3method("condition_test", class_name, function(cond, x) {
    captured$cond_expression = substitute(cond)
    captured$x_expression = substitute(x)
    captured$call = sys.call()
    captured$parent = parent.frame()
    TRUE
  }, envir = asNamespace("paradox"))
  condition = CondEqual(1L)
  class(condition) = c(class_name, class(condition))
  param_set = ps(root = p_int(init = 1L), child = p_int(init = 2L))
  param_set$add_dep("child", "root", condition)

  expect_identical(
    param_set$get_values(check_required = FALSE),
    list(root = 1L, child = 2L)
  )
  expect_false(is.symbol(captured$cond_expression))
  expect_identical(captured$x_expression, 1L)
  expect_identical(captured$call[[2L]], condition)
  expect_identical(captured$call[[3L]], 1L)
  expect_identical(captured$parent, asNamespace("paradox"))
})

test_that("reentrant dependency mutation retains the captured values", {
  skip_if_not(native_get_values_available())
  events = new.env(parent = emptyenv())
  events$inner = NULL
  param_set = ps(parent = p_int(), child = p_int())
  private = param_set$.__enclos_env__$private
  condition = native_get_values_condition(
    "NativeGetValuesReentrantCondition",
    function(x) {
      private$.store_values(list(parent = 0L, child = 9L))
      events$inner = param_set$get_values(remove_dependencies = FALSE)
      gc(FALSE)
      TRUE
    }
  )
  param_set$add_dep("child", "parent", condition)
  param_set$values = list(parent = 1L, child = 2L)

  expect_identical(
    param_set$get_values(),
    list(parent = 1L, child = 2L)
  )
  expect_identical(events$inner, list(parent = 0L, child = 9L))
  expect_identical(param_set$values, list(parent = 0L, child = 9L))
})

test_that("final ids observe table mutations made by callbacks and filters", {
  skip_if_not(native_get_values_available())
  param_set = ps(first = p_int(init = 1L), second = p_int(init = 2L))
  private = param_set$.__enclos_env__$private
  reverse_params = function() {
    replacement = unclass(private$.params)
    replacement = lapply(replacement, rev)
    names(replacement) = names(private$.params)
    private$.params = replacement
  }
  condition = native_get_values_condition(
    "NativeGetValuesMutationCondition",
    function(x) {
      reverse_params()
      TRUE
    }
  )
  param_set$add_dep("second", "first", condition)

  expect_identical(
    param_set$get_values(check_required = FALSE),
    list(second = 2L, first = 1L)
  )

  param_set = ps(first = p_int(init = 1L), second = p_int(init = 2L))
  private = param_set$.__enclos_env__$private
  expect_identical(
    param_set$get_values(check_required = FALSE, class = {
      reverse_params()
      NULL
    }),
    list(second = 2L, first = 1L)
  )
})

test_that("final matching preserves reordered, missing, and duplicate ids", {
  skip_if_not(native_get_values_available())
  utf8_id = enc2utf8("caf\u00e9")
  latin1_id = iconv(utf8_id, from = "UTF-8", to = "latin1")
  skip_if(is.na(latin1_id))
  Encoding(latin1_id) = "latin1"

  param_set = ps(
    a = p_int(init = 1L),
    b = p_int(init = 2L),
    cafe = p_int(init = 3L),
    d = p_int(init = 4L)
  )
  private = param_set$.__enclos_env__$private
  data.table::set(private$.params, 3L, "id", utf8_id)
  names(private$.values)[[3L]] = latin1_id

  observed = param_set$get_values(
    check_required = FALSE,
    remove_dependencies = FALSE,
    class = {
      data.table::set(
        private$.params,
        j = "id",
        value = c(utf8_id, "absent", utf8_id, "a")
      )
      NULL
    }
  )
  expect_identical(unname(observed), list(3L, 3L, 1L))
  expect_identical(enc2utf8(names(observed)), c(utf8_id, utf8_id, "a"))
})

test_that("aligned final matching preserves duplicate-name match semantics", {
  skip_if_not(native_get_values_available())
  param_set = ps(a = p_int(init = 1L), b = p_int(init = 2L))
  private = param_set$.__enclos_env__$private

  observed = native_get_values_call(
    param_set,
    check_required = FALSE,
    remove_dependencies = FALSE,
    class = {
      data.table::set(private$.params, j = "id", value = c("a", "a"))
      data.table::setattr(private$.values, "names", c("a", "a"))
      NULL
    }
  )

  expect_identical(observed, structure(list(1L, 1L), names = c("a", "a")))
})

test_that("final matching compacts removed duplicate names", {
  skip_if_not(native_get_values_available())
  param_set = ps(
    a = p_int(init = 1L),
    b = p_int(init = 2L),
    c = p_int(init = 3L)
  )
  private = param_set$.__enclos_env__$private
  condition = native_get_values_condition(
    "NativeGetValuesDuplicateRemovalCondition",
    function(x) {
      data.table::setattr(private$.values, "names", c("b", "b", "a"))
      FALSE
    }
  )
  param_set$add_dep("b", "a", condition)

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed = param_set$get_values(check_required = FALSE)
  gctorture(previous)

  expect_identical(
    observed,
    list(a = 3L, b = 2L)
  )
})

test_that("final matching rejects callback-capable ids without replay", {
  skip_if_not(native_get_values_available())
  callbacks = 0L
  class_name = "NativeGetValuesLateClassedIds"
  registerS3method(
    "mtfrm",
    class_name,
    function(x) {
      callbacks <<- callbacks + 1L
      stop("native final matching dispatched mtfrm", call. = FALSE)
    },
    envir = asNamespace("base")
  )

  param_set = ps(a = p_int(init = 1L), b = p_int(init = 2L))
  private = param_set$.__enclos_env__$private
  expect_error(
    param_set$get_values(
      check_required = FALSE,
      remove_dependencies = FALSE,
      class = {
        data.table::setattr(private$.params$id, "class", class_name)
        NULL
      }
    ),
    paste(
      "ParamSet ids changed to a callback-capable representation during",
      "native get_values()"
    ),
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)

  param_set = ps(a = p_int(init = 1L), b = p_int(init = 2L))
  private = param_set$.__enclos_env__$private
  expect_error(
    param_set$get_values(
      check_required = FALSE,
      remove_dependencies = FALSE,
      class = {
        data.table::setattr(private$.params$cls, "class", class_name)
        "ParamInt"
      }
    ),
    paste(
      "ParamSet ids changed to a callback-capable representation during",
      "native get_values()"
    ),
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)
})

test_that("subclasses and custom Domains fall back before callbacks", {
  skip_if_not(native_get_values_available())
  events = new.env(parent = emptyenv())
  events$reads = 0L
  CountingSet = R6::R6Class(
    "NativeGetValuesCountingSet",
    inherit = ParamSet,
    active = list(values = function(value) {
      if (!missing(value)) {
        super$values = value
        return(value)
      }
      events$reads = events$reads + 1L
      super$values
    })
  )
  child = CountingSet$new(list(x = p_int(init = 1L)))
  expect_null(native_get_values_call(child))
  expect_identical(events$reads, 0L)
  expect_identical(child$get_values(), list(x = 1L))
  expect_identical(events$reads, 1L)

  collection = ParamSetCollection$new(list(child = child))
  events$reads = 0L
  expect_null(native_get_values_call(collection))
  expect_identical(events$reads, 0L)
  expect_identical(collection$get_values(), list(child.x = 1L))
  expect_identical(events$reads, 1L)

  make_custom = function() paradox:::Domain(
      cls = "NativeGetValuesCustom",
      grouping = "NativeGetValuesCustom",
      storage_type = "list"
    )
  custom = make_custom()
  custom_set = ParamSet$new(list(custom = custom))
  custom_set$.__enclos_env__$private$.values = list(custom = 3L)
  expect_null(native_get_values_call(custom_set))
  expect_identical(custom_set$get_values(), list(custom = 3L))
})

test_that("replaced generated bindings decline before execution", {
  skip_if_not(native_get_values_available())
  events = new.env(parent = emptyenv())
  events$reads = 0L
  param_set = ps(x = p_int(init = 1L))
  original = activeBindingFunction("values", param_set)
  makeActiveBinding("values", function(value) {
    events$reads = events$reads + 1L
    original(value)
  }, param_set)

  expect_null(native_get_values_call(param_set))
  expect_identical(events$reads, 0L)
  expect_identical(param_set$get_values(), list(x = 1L))
  expect_identical(events$reads, 1L)

  param_set = ps(x = p_int(init = 2L))
  private = param_set$.__enclos_env__$private
  original = private$.get_values
  unlockBinding(".get_values", private)
  private$.get_values = function() {
    events$reads = events$reads + 1L
    original()
  }
  lockBinding(".get_values", private)
  events$reads = 0L

  expect_null(native_get_values_call(param_set))
  expect_identical(events$reads, 0L)
  expect_identical(param_set$get_values(), list(x = 2L))
  expect_identical(events$reads, 1L)
})

test_that("fancy wrapper locals decline without being forced", {
  skip_if_not(native_get_values_available())
  param_set = ps(x = p_int(init = 1L))
  wrapper_environment = environment(param_set$get_values)
  expect_identical(
    wrapper_environment,
    environment(param_set$ids)
  )
  expect_identical(
    wrapper_environment,
    environment(activeBindingFunction("values", param_set))
  )
  expect_identical(
    wrapper_environment,
    environment(param_set$.__enclos_env__$private$.get_values)
  )

  names = c(
    "super",
    ".__ParamSet__get_values",
    ".__ParamSet__ids",
    ".__ParamSet__values",
    ".__ParamSet__deps",
    ".__ParamSet__.get_values"
  )
  for (binding_name in names) {
    reads = 0L
    delayedAssign(
      binding_name,
      {
        reads <<- reads + 1L
        stop("delayed wrapper local was forced")
      },
      assign.env = wrapper_environment
    )
    expect_null(
      native_get_values_call(param_set),
      info = paste("delayed", binding_name)
    )
    expect_identical(reads, 0L, info = paste("delayed", binding_name))
    rm(list = binding_name, envir = wrapper_environment)

    makeActiveBinding(binding_name, function(value) {
      reads <<- reads + 1L
      stop("active wrapper local was forced")
    }, wrapper_environment)
    expect_null(
      native_get_values_call(param_set),
      info = paste("active", binding_name)
    )
    expect_identical(reads, 0L, info = paste("active", binding_name))
    rm(list = binding_name, envir = wrapper_environment)
  }
})

test_that("malformed state and unsupported encodings decline as a unit", {
  skip_if_not(native_get_values_available())
  param_set = ps(a = p_int(init = 1L), b = p_int(init = 2L))
  private = param_set$.__enclos_env__$private
  private$.values = list(b = 2L, a = 1L)
  expect_null(native_get_values_call(param_set))

  param_set = ps(a = p_int(init = 1L))
  private = param_set$.__enclos_env__$private
  private$.values = list(a = 1L, a = 2L)
  expect_null(native_get_values_call(param_set))

  param_set = ps(a = p_int(init = 1L))
  private = param_set$.__enclos_env__$private
  byte_name = "\u00e9"
  Encoding(byte_name) = "bytes"
  data.table::set(private$.params, 1L, "id", byte_name)
  names(private$.values) = byte_name
  expect_null(native_get_values_call(param_set))
  expect_identical(param_set$get_values(), structure(list(1L), names = byte_name))
})

test_that("native get_values ignores nested payloads it cannot consume", {
  skip_if_not(native_get_values_available())
  param_set = ps(
    first = p_int(init = 1L),
    later = p_fct(c("a", "b"), init = "a")
  )
  private = param_set$.__enclos_env__$private
  data.table::set(
    private$.params,
    i = 2L,
    j = "levels",
    value = list(new.env(parent = emptyenv()))
  )

  observed = native_get_values_call(
    param_set,
    check_required = FALSE,
    remove_dependencies = FALSE
  )
  expect_false(is.null(observed))
  expect_identical(observed, list(first = 1L, later = "a"))
})

test_that("tag-owner validation is deferred until get_values uses tags", {
  skip_if_not(native_get_values_available())
  param_set = ps(value = p_int(init = 1L, tags = "train"))
  private = param_set$.__enclos_env__$private
  private$.tags = data.table::data.table(
    id = "ghost",
    tag = "train",
    key = "id"
  )

  observed = native_get_values_call(
    param_set,
    check_required = FALSE,
    remove_dependencies = FALSE
  )
  expect_false(is.null(observed))
  expect_identical(observed, list(value = 1L))
  expect_error(
    native_get_values_call(
      param_set,
      tags = "train",
      check_required = FALSE,
      remove_dependencies = FALSE
    ),
    "contains an unknown parameter ID",
    fixed = TRUE
  )
})

test_that("supported encodings match semantically", {
  skip_if_not(native_get_values_available())
  utf8_id = enc2utf8("caf\u00e9")
  latin1_id = iconv(utf8_id, from = "UTF-8", to = "latin1")
  skip_if(is.na(latin1_id))
  Encoding(latin1_id) = "latin1"
  param_set = ps(cafe = p_int(init = 1L))
  private = param_set$.__enclos_env__$private
  data.table::set(private$.params, 1L, "id", utf8_id)
  names(private$.values) = latin1_id

  observed = native_get_values_call(param_set)
  expect_false(is.null(observed))
  expect_identical(unname(observed), list(1L))
  expect_identical(enc2utf8(names(observed)), utf8_id)
})

test_that("native outputs own shells and retain opaque leaves", {
  skip_if_not(native_get_values_available())
  marker = new.env(parent = emptyenv())
  param_set = ps(marker = p_uty(), value = p_int())
  param_set$values = list(marker = marker, value = 1L)
  first = native_get_values_call(param_set)
  second = native_get_values_call(param_set)

  expect_identical(first, second)
  expect_false(identical(data.table::address(first), data.table::address(second)))
  expect_false(identical(
    data.table::address(names(first)),
    data.table::address(names(second))
  ))
  expect_identical(first$marker, marker)
  names(first)[[1L]] = "changed"
  first$value = 3L
  expect_identical(native_get_values_call(param_set), second)
})

test_that("serialized and cloned exact objects remain native-admissible", {
  skip_if_not(native_get_values_available())
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
    direct = native_get_values_call(object)
    expect_false(is.null(direct))
    expect_identical(direct, object$get_values())
  }
})

test_that("native get_values survives forced collection and callbacks", {
  skip_if_not(native_get_values_available())
  condition = native_get_values_condition(
    "NativeGetValuesGctortureCondition",
    function(x) {
      gc(FALSE)
      identical(x, 1L)
    }
  )
  child = ps(a = p_int(init = 1L), b = p_int(init = 2L))
  child$add_dep("b", "a", condition)
  collection = ParamSetCollection$new(list(root = child))

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed = native_get_values_call(collection)
  serialized = unserialize(serialize(collection, NULL, version = 3L))
  gctorture(previous)

  expect_identical(observed, list(root.a = 1L, root.b = 2L))
  expect_identical(serialized$get_values(), observed)
})
