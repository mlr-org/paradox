context("contract: ParamSet values")

values_empty_named = function() {
  structure(list(), names = character())
}

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
  expect_identical(param_set$values, values_empty_named())
  param_set$values = integer()
  expect_identical(param_set$values, values_empty_named())

  expect_error(
    { param_set$values = list(1L) },
    "plain named list",
    fixed = TRUE
  )
  expect_error(
    { param_set$values = 1L },
    "plain named list",
    fixed = TRUE
  )

  param_set$assert_values = FALSE
  expect_error(
    param_set$values <- structure(
      list(9L, 2L, 4L),
      names = c("count", "count", "unknown")
    ),
    "unique and non-missing"
  )
  expect_identical(param_set$values, values_empty_named())
  param_set$values = list(count = 9L, unknown = 4L)
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
    "`...` values must be a plain named list",
    fixed = TRUE
  )
  expect_error(
    param_set$set_values(.values = list(1L)),
    "`.values` must be a plain named list",
    fixed = TRUE
  )
  expect_error(
    param_set$set_values(a = 1L, a = 2L),
    "ParamSet value inputs must have unique, disjoint names",
    fixed = TRUE
  )
  expect_error(
    param_set$set_values(.values = structure(
      list(1L, 2L),
      names = c("a", "a")
    )),
    "ParamSet value inputs must have unique, disjoint names",
    fixed = TRUE
  )
  expect_error(
    param_set$set_values(a = 1L, .values = list(a = 2L)),
    "ParamSet value inputs must have unique, disjoint names",
    fixed = TRUE
  )
  expect_error(
    param_set$set_values(.values = NULL),
    "`.values` must be a plain named list",
    fixed = TRUE
  )

  expect_error(
    param_set$set_values(a = 1L, .insert = NULL),
    "`.insert` must be TRUE or FALSE",
    fixed = TRUE
  )
  expect_error(
    param_set$set_values(a = 1L, .insert = NA),
    "`.insert` must be TRUE or FALSE",
    fixed = TRUE
  )
  expect_error(
    param_set$set_values(a = 1L, .insert = c(TRUE, FALSE)),
    "`.insert` must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("set_values insert and replacement preserve documented NULL rules", {
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

  expect_error(
    param_set$set_values(a = 2L, .insert = 1),
    "`.insert` must be TRUE or FALSE",
    fixed = TRUE
  )
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

test_that("get_values validates its closed argument contract", {
  param_set = ps(a = p_int(tags = "required"), b = p_int())
  param_set$values = list(a = 1L, b = 2L)

  expect_error(
    param_set$get_values(type = NULL),
    "Assertion on 'type' failed",
    fixed = TRUE
  )
  expect_error(
    param_set$get_values(type = "token"),
    "Assertion on 'type' failed"
  )
  expect_error(
    param_set$get_values(check_required = 1),
    "Assertion on 'check_required' failed"
  )
  expect_error(
    param_set$get_values(class = 1),
    "Must be of type 'character' (or 'NULL'), not 'double'",
    fixed = TRUE
  )
  expect_error(
    param_set$get_values(remove_dependencies = NULL),
    "Assertion on 'remove_dependencies' failed"
  )
  expect_error(
    param_set$get_values(remove_dependencies = NA),
    "Assertion on 'remove_dependencies' failed"
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

  param_set$deps = param_set$deps[c(2L, 1L)]
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

test_that("collection stores preserve public ordering and replacement rules", {
  one = ps(a = p_int(init = 1L))
  two = ps(b = p_int(init = 2L))
  three = ps(c = p_int(init = 3L))
  collection = ParamSetCollection$new(list(one = one, two = two, three = three))

  expect_identical(
    collection$values,
    list(one.a = 1L, two.b = 2L, three.c = 3L)
  )

  collection$values = list(three.c = 4L, one.a = 5L)
  expect_identical(collection$values, list(one.a = 5L, three.c = 4L))
  expect_identical(one$values, list(a = 5L))
  expect_identical(two$values, values_empty_named())
  expect_identical(three$values, list(c = 4L))

  collection$set_values(two.b = 6L, .insert = FALSE)
  expect_identical(collection$values, list(two.b = 6L))

  collection$set_values(one.a = 7L)
  expect_identical(
    collection$values,
    list(one.a = 7L, two.b = 6L)
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
  expect_identical(left$values, values_empty_named())
})

test_that("collection value reads reflect later child mutations", {
  left = ps(a = p_int(init = 1L))
  right = ps(b = p_int(init = 2L))
  collection = ParamSetCollection$new(list(left = left, right = right))
  before = collection$values

  left$values = list(a = 7L)
  right$values = values_empty_named()

  expect_identical(before, list(left.a = 1L, right.b = 2L))
  expect_identical(collection$values, list(left.a = 7L))
})

test_that("official ParamSetShadow retains live values and graph identity", {
  origin = ps(x = p_int(), y = p_lgl())
  origin$values = list(x = 1L, y = TRUE)
  shadow = ParamSetShadow$new(origin, "x")

  expect_identical(shadow$values, list(y = TRUE))
  expect_identical(shadow$get_values(), list(y = TRUE))
  expect_invisible(shadow$set_values(y = FALSE))
  expect_identical(origin$values, list(x = 1L, y = FALSE))

  cloned = shadow$clone(deep = TRUE)
  expect_false(identical(cloned$origin, origin))
  cloned$values = list(y = TRUE)
  expect_identical(cloned$origin$values, list(x = 1L, y = TRUE))
  expect_identical(origin$values, list(x = 1L, y = FALSE))

  collection = ParamSetCollection$new(list(shadow = shadow))
  collection$values = list(shadow.y = TRUE)
  expect_identical(origin$values, list(x = 1L, y = TRUE))
  expect_identical(shadow$values, list(y = TRUE))
  expect_identical(collection$values, list(shadow.y = TRUE))

  collection$values = values_empty_named()
  expect_identical(origin$values, list(x = 1L))
  expect_identical(shadow$values, values_empty_named())
  expect_identical(collection$values, values_empty_named())
})

test_that("deep clone preserves graph identity and established value aliasing", {
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
  expect_identical(
    deep_collection$sets[[1L]],
    deep_collection$sets[[2L]]
  )
  expect_identical(
    deep_collection$sets[[1L]]$values$value,
    deep_collection$sets[[2L]]$values$value
  )

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
  skip_on_cran()

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
