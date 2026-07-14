context("characterization: ParamSet values")

values_characterization_internal_domain = function() {
  p_int(
    0L,
    10L,
    tags = "internal_tuning",
    in_tune_fn = function(domain, param_vals) domain$upper,
    aggr = function(x) x[[1L]],
    disable_in_tune = list()
  )
}

test_that("values assignment sanitizes and stores in parameter order", {
  param_set = ps(
    count = p_int(0L, 4L),
    ratio = p_dbl(0, 4),
    payload = p_uty(special_vals = list(NULL))
  )

  assigned = list(payload = NULL, ratio = 2, count = 1)
  assignment_result = (param_set$values = assigned)
  expect_identical(assignment_result, assigned)
  expect_identical(
    param_set$values,
    list(count = 1L, ratio = 2, payload = NULL)
  )

  partial_result = (param_set$values$count = 3)
  expect_identical(partial_result, 3)
  expect_identical(param_set$values$count, 3L)

  param_set$values = NULL
  expect_identical(param_set$values, named_list())
  param_set$values = integer()
  expect_identical(param_set$values, named_list())

  expect_error(
    { param_set$values = list(1L) },
    "Must have names",
    fixed = TRUE
  )
  expect_error(
    { param_set$values = 1L },
    "Must be of type 'list', not 'integer'",
    fixed = TRUE
  )

  param_set$assert_values = FALSE
  param_set$values = structure(
    list(9L, 2L, 4L),
    names = c("count", "count", "unknown")
  )
  expect_identical(param_set$values, list(count = 9L))
})

test_that("values snapshots detach list edits but retain reference values", {
  param_set = ps(payload = p_uty(), count = p_int())
  payload = new.env(parent = emptyenv())
  payload$value = 1L
  param_set$values = list(payload = payload, count = 1L)

  snapshot = param_set$values
  snapshot$count = 2L
  snapshot$payload$value = 3L

  expect_identical(param_set$values$count, 1L)
  expect_identical(param_set$values$payload, payload)
  expect_identical(param_set$values$payload$value, 3L)
})

test_that("set_values validates both sources before merging", {
  param_set = ps(a = p_int(), b = p_int(), c = p_int())

  expect_error(
    param_set$set_values(1L),
    "Assertion on 'dots' failed: Must have names.",
    fixed = TRUE
  )
  expect_error(
    param_set$set_values(.values = list(1L)),
    "Assertion on '.values' failed: Must have names.",
    fixed = TRUE
  )
  expect_error(
    param_set$set_values(a = 1L, a = 2L),
    "Must have unique names, but element 2 is duplicated",
    fixed = TRUE
  )
  expect_error(
    param_set$set_values(.values = structure(
      list(1L, 2L),
      names = c("a", "a")
    )),
    "Must have unique names, but element 2 is duplicated",
    fixed = TRUE
  )
  expect_error(
    param_set$set_values(a = 1L, .values = list(a = 2L)),
    "Must be disjunct",
    fixed = TRUE
  )
  expect_error(
    param_set$set_values(.values = NULL),
    "Must be of type 'list', not 'NULL'",
    fixed = TRUE
  )

  expect_error(
    param_set$set_values(a = 1L, .insert = NULL),
    "argument is of length zero",
    fixed = TRUE
  )
  expect_error(
    param_set$set_values(a = 1L, .insert = NA),
    "missing value where TRUE/FALSE needed",
    fixed = TRUE
  )
  expect_error(
    param_set$set_values(a = 1L, .insert = c(TRUE, FALSE)),
    "the condition has length > 1",
    fixed = TRUE
  )
})

test_that("set_values insert and replacement preserve historical NULL rules", {
  param_set = ps(
    a = p_int(),
    b = p_int(),
    nullable = p_uty(special_vals = list(NULL))
  )
  param_set$values = list(a = 1L, b = 2L, nullable = NULL)

  returned = withVisible(param_set$set_values(
    b = 3L,
    .values = list(a = 4L),
    .insert = TRUE
  ))
  expect_false(returned$visible)
  expect_identical(returned$value, param_set)
  expect_identical(
    param_set$values,
    list(a = 4L, b = 3L, nullable = NULL)
  )

  param_set$set_values(a = NULL)
  expect_identical(param_set$values, list(b = 3L, nullable = NULL))

  # An unknown NULL is discarded before the active values setter validates
  # names.  The same name is invalid in replacement mode.
  expect_invisible(param_set$set_values(unknown = NULL))
  expect_error(
    param_set$set_values(unknown = NULL, .insert = FALSE),
    "Parameter 'unknown' not available",
    fixed = TRUE
  )

  param_set$set_values(
    nullable = NULL,
    .values = list(b = 1L),
    .insert = FALSE
  )
  expect_identical(param_set$values, list(b = 1L, nullable = NULL))

  # `.insert` is historically not passed through assert_flag().
  expect_invisible(param_set$set_values(a = 2L, .insert = 1))
  expect_identical(param_set$values, list(a = 2L, b = 1L, nullable = NULL))
})

test_that("get_values filters tokens after dependency processing", {
  param_set = ps(
    concrete = p_int(),
    ordinary = p_int(0L, 10L),
    internal = values_characterization_internal_domain(),
    explicit_null = p_uty(special_vals = list(NULL))
  )
  ordinary = to_tune()
  internal = to_tune(upper = 10, internal = TRUE)
  param_set$values = list(
    concrete = 1L,
    ordinary = ordinary,
    internal = internal,
    explicit_null = NULL
  )

  expect_identical(
    param_set$get_values(type = "with_token"),
    list(
      concrete = 1L,
      ordinary = ordinary,
      internal = internal,
      explicit_null = NULL
    )
  )
  expect_identical(
    param_set$get_values(type = "without_token"),
    list(concrete = 1L, explicit_null = NULL)
  )
  expect_identical(
    param_set$get_values(type = "only_token"),
    list(ordinary = ordinary, internal = internal)
  )
  expect_identical(
    param_set$get_values(type = "with_internal"),
    list(internal = internal)
  )
})

test_that("get_values required checks are global and use original names", {
  param_set = ps(
    hidden = p_uty(tags = c("required", "hidden"), special_vals = list(NULL)),
    visible = p_int(tags = "visible")
  )
  param_set$values = list(hidden = NULL, visible = 2L)

  expect_identical(
    param_set$get_values(tags = "visible"),
    list(visible = 2L)
  )

  param_set$values = list(visible = 2L)
  expect_error(
    param_set$get_values(tags = "visible"),
    "Missing required parameters: hidden",
    fixed = TRUE
  )
  expect_identical(
    param_set$get_values(tags = "visible", check_required = FALSE),
    list(visible = 2L)
  )
})

test_that("get_values retains argument validation and remove quirks", {
  param_set = ps(a = p_int(tags = "required"), b = p_int())
  param_set$values = list(a = 1L, b = 2L)

  expect_error(
    param_set$get_values(type = NULL),
    "Assertion on 'type' failed",
    fixed = TRUE
  )
  expect_error(
    param_set$get_values(type = "token"),
    "but is 'token'",
    fixed = TRUE
  )
  expect_error(
    param_set$get_values(check_required = 1),
    "Must be of type 'logical flag', not 'double'",
    fixed = TRUE
  )
  expect_error(
    param_set$get_values(class = 1),
    "Must be of type 'character' (or 'NULL'), not 'double'",
    fixed = TRUE
  )
  expect_error(
    param_set$get_values(remove_dependencies = NULL),
    "invalid 'x' type in 'x && y'",
    fixed = TRUE
  )

  # NA is tolerated only because `NA && FALSE` is false for empty deps.
  expect_identical(
    param_set$get_values(remove_dependencies = NA),
    list(a = 1L, b = 2L)
  )
  param_set$add_dep("b", "a", CondEqual(1L))
  expect_error(
    param_set$get_values(remove_dependencies = NA),
    "missing value where TRUE/FALSE needed",
    fixed = TRUE
  )
})

test_that("dependency rows are evaluated sequentially against a local snapshot", {
  param_set = ps(
    root = p_int(0L, 2L),
    middle = p_int(0L, 2L),
    leaf = p_int(0L, 2L)
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

  private = param_set$.__enclos_env__$private
  private$.deps = private$.deps[2:1]
  expect_identical(param_set$get_values(), list(root = 0L, leaf = 2L))

  token = to_tune()
  param_set$values = list(root = token, middle = 1L, leaf = 2L)
  expect_identical(
    param_set$get_values(),
    list(root = token, middle = 1L, leaf = 2L)
  )

  # With the reversed row order, a missing root removes middle only after leaf
  # has already been accepted against middle's local value.
  param_set$values = list(middle = 1L, leaf = 2L)
  expect_identical(param_set$get_values(), list(leaf = 2L))
})

test_that("condition callbacks can reenter while the outer getter stays coherent", {
  events = new.env(parent = emptyenv())
  events$seen = character()

  method = function(cond, x) {
    events$seen = c(events$seen, sprintf("condition:%s", x))
    cond$mutate()
    x == cond$rhs
  }
  registerS3method(
    "condition_test",
    "CharacterizationValuesCondition",
    method,
    envir = asNamespace("paradox")
  )

  param_set = ps(parent = p_int(0L, 2L), child = p_int(0L, 10L))
  private = param_set$.__enclos_env__$private
  condition = CondEqual(1L)
  condition$mutate = function() {
    private$.store_values(list(parent = 0L, child = 9L))
    gc(FALSE)
  }
  class(condition) = c("CharacterizationValuesCondition", class(condition))
  param_set$add_dep("child", "parent", condition)
  param_set$values = list(parent = 1L, child = 2L)
  events$seen = character()

  # The callback changes the live object, but the in-flight result is based on
  # the values list captured before dependency dispatch.
  expect_identical(
    param_set$get_values(),
    list(parent = 1L, child = 2L)
  )
  expect_identical(events$seen, "condition:1")
  expect_identical(param_set$values, list(parent = 0L, child = 9L))
})

test_that("subclass value, dependency, and id dispatch order is retained", {
  events = new.env(parent = emptyenv())
  events$seen = character()

  DispatchParamSet = R6::R6Class(
    "CharacterizationValuesDispatchParamSet",
    inherit = ParamSet,
    public = list(
      ids = function(class = NULL, tags = NULL, any_tags = NULL) {
        label = if (identical(tags, "required")) "ids:required" else "ids:final"
        events$seen = c(events$seen, label)
        super$ids(class = class, tags = tags, any_tags = any_tags)
      }
    ),
    active = list(
      values = function(value) {
        if (!missing(value)) {
          events$seen = c(events$seen, "values:set")
          super$values = value
          return(value)
        }
        events$seen = c(events$seen, "values:get")
        super$values
      },
      deps = function(value) {
        if (!missing(value)) {
          super$deps = value
          return(value)
        }
        events$seen = c(events$seen, "deps:get")
        super$deps
      }
    )
  )

  param_set = DispatchParamSet$new(list(a = p_int(init = 1L)))
  events$seen = character()
  expect_identical(param_set$get_values(), list(a = 1L))
  expect_identical(
    events$seen,
    c("values:get", "deps:get", "ids:required", "ids:final")
  )

  events$seen = character()
  param_set$set_values(a = 2L)
  expect_identical(events$seen, c("values:get", "values:set", "deps:get"))

  events$seen = character()
  param_set$set_values(a = 3L, .insert = FALSE)
  expect_identical(events$seen, c("values:set", "deps:get"))
})

test_that("delayed private values and active subclass values remain observable", {
  param_set = ps(a = p_int())
  private = param_set$.__enclos_env__$private
  events = new.env(parent = emptyenv())
  events$count = 0L
  delayedAssign(
    ".values",
    {
      events$count = events$count + 1L
      list(a = events$count)
    },
    assign.env = private,
    eval.env = environment()
  )

  expect_identical(events$count, 0L)
  expect_identical(param_set$values, list(a = 1L))
  expect_identical(param_set$get_values(), list(a = 1L))
  expect_identical(events$count, 1L)

  ActivePrivateValues = R6::R6Class(
    "CharacterizationActivePrivateValues",
    inherit = ParamSet,
    lock_objects = FALSE
  )
  active_set = ActivePrivateValues$new(list(a = p_int()))
  active_private = active_set$.__enclos_env__$private
  rm(".values", envir = active_private)
  makeActiveBinding(
    ".values",
    function(value) {
      if (!missing(value)) stop("active private values are read-only")
      events$count = events$count + 1L
      list(a = events$count)
    },
    active_private
  )
  before = events$count
  expect_identical(active_set$get_values(), list(a = before + 1L))
  expect_identical(events$count, before + 1L)
})

test_that("collection values delegate subclass stores through public bindings", {
  events = new.env(parent = emptyenv())
  events$seen = character()

  Child = R6::R6Class(
    "CharacterizationCollectionValueChild",
    inherit = ParamSet,
    public = list(
      initialize = function(label, params) {
        private$.label = label
        super$initialize(params)
      }
    ),
    active = list(
      values = function(value) {
        if (!missing(value)) {
          events$seen = c(events$seen, sprintf("%s:active-set", private$.label))
          super$values = value
          return(value)
        }
        events$seen = c(events$seen, sprintf("%s:get", private$.label))
        super$values
      }
    ),
    private = list(
      .label = NULL,
      .store_values = function(xs) {
        events$seen = c(events$seen, sprintf("%s:store", private$.label))
        super$.store_values(xs)
      }
    )
  )

  one = Child$new("one", list(a = p_int(init = 1L)))
  two = Child$new("two", list(b = p_int(init = 2L)))
  three = Child$new("three", list(c = p_int(init = 3L)))
  collection = ParamSetCollection$new(list(one = one, two = two, three = three))

  events$seen = character()
  expect_identical(
    collection$values,
    list(one.a = 1L, two.b = 2L, three.c = 3L)
  )
  expect_identical(events$seen, c("one:get", "two:get", "three:get"))

  events$seen = character()
  collection$values = list(three.c = 4L, one.a = 5L)
  expect_identical(
    events$seen,
    c(
      "one:active-set", "one:store",
      "three:active-set", "three:store",
      "two:active-set", "two:store"
    )
  )
  expect_identical(collection$values, list(one.a = 5L, three.c = 4L))

  events$seen = character()
  collection$set_values(two.b = 6L, .insert = FALSE)
  expect_identical(
    events$seen,
    c(
      "two:active-set", "two:store",
      "one:active-set", "one:store",
      "three:active-set", "three:store"
    )
  )

  events$seen = character()
  collection$set_values(one.a = 7L)
  expect_identical(
    events$seen,
    c(
      "one:get", "two:get", "three:get",
      "one:active-set", "one:store",
      "two:active-set", "two:store",
      "three:active-set", "three:store"
    )
  )
})

test_that("nested prefix and postfix collections expose live child values", {
  left = ps(a = p_int(init = 1L), z = p_lgl(init = TRUE))
  right = ps(b = p_int(init = 2L))
  inner = ParamSetCollection$new(list(left = left, right = right))
  tail = ps(q = p_dbl(init = 0.5))

  prefix = ParamSetCollection$new(list(nested = inner, tail = tail))
  expect_identical(
    prefix$values,
    list(
      nested.left.a = 1L,
      nested.left.z = TRUE,
      nested.right.b = 2L,
      tail.q = 0.5
    )
  )

  postfix = ParamSetCollection$new(
    list(nested = inner, tail = tail),
    postfix_names = TRUE
  )
  expect_identical(
    postfix$values,
    list(
      left.a.nested = 1L,
      left.z.nested = TRUE,
      right.b.nested = 2L,
      q.tail = 0.5
    )
  )

  left$values$a = 3L
  expect_identical(prefix$values$nested.left.a, 3L)
  expect_identical(postfix$values$left.a.nested, 3L)

  prefix$values = list(nested.right.b = 4L, tail.q = 0.75)
  expect_identical(right$values, list(b = 4L))
  expect_identical(tail$values, list(q = 0.75))
  expect_identical(left$values, named_list())
})

test_that("collection child getters observe mutations made by earlier children", {
  events = new.env(parent = emptyenv())
  events$seen = character()
  events$callback = NULL

  CallbackChild = R6::R6Class(
    "CharacterizationLiveCollectionValueChild",
    inherit = ParamSet,
    public = list(
      initialize = function(label, params) {
        private$.label = label
        super$initialize(params)
      }
    ),
    active = list(
      values = function(value) {
        if (!missing(value)) {
          events$seen = c(events$seen, sprintf("%s:set", private$.label))
          super$values = value
          return(value)
        }
        events$seen = c(events$seen, sprintf("%s:get", private$.label))
        callback = events$callback
        events$callback = NULL
        if (is.function(callback)) callback()
        super$values
      }
    ),
    private = list(.label = NULL)
  )

  left = CallbackChild$new("left", list(a = p_int(init = 1L)))
  right = CallbackChild$new("right", list(b = p_int(init = 2L)))
  collection = ParamSetCollection$new(list(left = left, right = right))
  events$seen = character()
  events$callback = function() {
    right$values = list(b = 9L)
  }

  expect_identical(collection$values, list(left.a = 1L, right.b = 9L))
  expect_identical(events$seen, c("left:get", "right:set", "right:get"))
})

test_that("shadow-like subclasses retain values dispatch and shared origins", {
  Shadow = R6::R6Class(
    "CharacterizationParamSetShadowLike",
    inherit = ParamSet,
    public = list(
      initialize = function(origin, shadowed) {
        private$.origin = origin
        private$.shadowed = shadowed
        kept = setdiff(origin$ids(), shadowed)
        super$initialize(origin$domains[kept])
      }
    ),
    active = list(
      values = function(value) {
        if (!missing(value)) {
          self$assert(value)
          all_values = private$.origin$values
          all_values = all_values[
            intersect(names(all_values), private$.shadowed)
          ]
          private$.origin$values = c(all_values, value)
        }
        values = private$.origin$values
        values[private$.shadowed] = NULL
        values
      },
      origin = function(value) {
        if (!missing(value) && !identical(value, private$.origin)) {
          stop("origin is read-only")
        }
        private$.origin
      }
    ),
    private = list(.origin = NULL, .shadowed = NULL)
  )

  origin = ps(x = p_int(), y = p_lgl())
  origin$values = list(x = 1L, y = TRUE)
  shadow = Shadow$new(origin, "x")

  expect_identical(shadow$values, list(y = TRUE))
  expect_identical(shadow$get_values(), list(y = TRUE))
  expect_invisible(shadow$set_values(y = FALSE))
  expect_identical(origin$values, list(x = 1L, y = FALSE))

  cloned = shadow$clone(deep = TRUE)
  expect_identical(cloned$origin, origin)
  cloned$values = list(y = TRUE)
  expect_identical(origin$values, list(x = 1L, y = TRUE))

  collection = ParamSetCollection$new(list(shadow = shadow))
  collection$values = list(shadow.y = FALSE)
  expect_identical(origin$values, list(x = 1L, y = FALSE))
  expect_identical(shadow$values, list(y = FALSE))
  expect_identical(collection$values, list(shadow.y = FALSE))

  collection$values = named_list()
  expect_identical(origin$values, list(x = 1L))
  expect_identical(shadow$values, named_list())
  expect_identical(collection$values, named_list())
})

test_that("deep clone and serialization retain established value aliasing", {
  Box = R6::R6Class(
    "CharacterizationValuesCloneBox",
    public = list(
      value = NULL,
      initialize = function(value) self$value = value
    )
  )

  box = Box$new(1L)
  environment_value = new.env(parent = emptyenv())
  environment_value$value = 1L
  param_set = ps(
    environment = p_uty(),
    first = p_uty(),
    second = p_uty(),
    nested = p_uty()
  )
  param_set$values = list(
    environment = environment_value,
    first = box,
    second = box,
    nested = list(box = box)
  )

  deep = param_set$clone(deep = TRUE)
  expect_identical(deep$values$environment, environment_value)
  expect_false(identical(deep$values$first, box))
  expect_false(identical(deep$values$first, deep$values$second))
  expect_identical(deep$values$nested$box, box)

  restored = unserialize(serialize(param_set, NULL, version = 3L))
  expect_false(identical(restored$values$environment, environment_value))
  expect_identical(restored$values$first, restored$values$second)
  expect_identical(restored$values$first, restored$values$nested$box)

  child = ps(value = p_uty())
  child$values = list(value = box)
  collection = ParamSetCollection$new(list(left = child, right = child))
  deep_collection = collection$clone(deep = TRUE)
  expect_false(identical(
    deep_collection$sets[[1L]],
    deep_collection$sets[[2L]]
  ))
  expect_false(identical(
    deep_collection$sets[[1L]]$values$value,
    deep_collection$sets[[2L]]$values$value
  ))

  restored_collection = unserialize(serialize(
    collection,
    NULL,
    version = 3L
  ))
  expect_identical(
    restored_collection$sets[[1L]],
    restored_collection$sets[[2L]]
  )
  expect_identical(
    restored_collection$sets[[1L]]$values$value,
    restored_collection$sets[[2L]]$values$value
  )
})

test_that("values getters and setters survive gctorture", {
  skip_if(
    identical(Sys.getenv("PARADOX_SKIP_CHARACTERIZATION_GCT"), "true"),
    "explicitly skipping the slow characterization gctorture case"
  )

  root = ps(
    switch = p_fct(c("off", "on"), init = "on"),
    dependent = p_int(0L, 10L, init = 2L, depends = switch == "on"),
    token = p_int(0L, 10L)
  )
  other = ps(flag = p_lgl(init = TRUE), count = p_int(init = 1L))
  root$values = list(switch = "on", dependent = 2L, token = to_tune())
  collection = ParamSetCollection$new(list(root = root, other = other))

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed = collection$get_values(type = "without_token")
  collection$set_values(.values = list(
    root.switch = "off",
    root.dependent = NULL,
    root.token = to_tune(),
    other.flag = FALSE,
    other.count = 3L
  ))
  after = collection$get_values(type = "only_token")
  serialized = unserialize(serialize(collection, NULL, version = 3L))
  gctorture(previous)

  expect_identical(
    observed,
    list(
      root.switch = "on",
      root.dependent = 2L,
      other.flag = TRUE,
      other.count = 1L
    )
  )
  expect_identical(names(after), "root.token")
  expect_identical(serialized$values$root.switch, "off")
  expect_identical(serialized$values$other.count, 3L)
})
