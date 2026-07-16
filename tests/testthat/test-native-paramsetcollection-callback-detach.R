native_collection_detach_available = function() {
  exists(
    "C_param_set_collection_detach_plan",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

native_collection_detach_plan = function(collection, ids = NULL) {
  .Call(
    C_param_set_collection_detach_plan,
    collection$.__enclos_env__$private,
    collection,
    ids
  )
}

test_that("callback-free collection subsets bypass detachment admission", {
  skip_if_not(native_collection_detach_available())
  plain = psc(component = ps(x = p_int(), y = p_lgl()))
  constrained_child = ps(x = p_int())
  constrained_child$constraint = function(x) TRUE
  constrained = psc(component = constrained_child)

  # Make entry into the native planner observable without adding production
  # instrumentation.  The callback-free gate must return the ParamSet already
  # produced by super$subset(); a retained callback still uses the planner and
  # is covered by the callback tests below.
  namespace = asNamespace("paradox")
  symbol = "C_param_set_collection_detach_plan"
  registered = get(symbol, envir = namespace, inherits = FALSE)
  unlockBinding(symbol, namespace)
  on.exit({
    assign(symbol, registered, envir = namespace)
    lockBinding(symbol, namespace)
  }, add = TRUE)
  assign(symbol, NULL, envir = namespace)

  observed = plain$subset(
    "component.x",
    allow_dangling_dependencies = TRUE
  )
  expect_identical(observed$ids(), "component.x")
  expect_null(observed$constraint)
  expect_null(observed$extra_trafo)

  omitted = constrained$subset(
    "component.x",
    allow_dangling_dependencies = TRUE,
    keep_constraint = FALSE
  )
  expect_null(omitted$constraint)
  expect_null(omitted$extra_trafo)
})

test_that("native collection callback plans retain immediate-child order", {
  skip_if_not(native_collection_detach_available())
  first = ps(x = p_int(), plain = p_lgl())
  second = ps(y = p_int())
  third = ps(z = p_int())
  first$constraint = function(x) TRUE
  second$extra_trafo = function(x) {
    x$y = x$y + 10L
    x
  }
  third$extra_trafo = function(x) {
    x$z = x$z + 100L
    x
  }
  collection = psc(first = first, second = second, third = third)

  plan = native_collection_detach_plan(
    collection,
    c("third.z", "first.x", "second.y", "second.y")
  )
  expect_type(plan, "list")
  expect_identical(plan$translation$id, c(
    "first.x", "second.y", "third.z"
  ))
  expect_identical(
    names(plan$translation),
    c("id", "original_id", "owner_ps_index", "owner_name")
  )
  expect_identical(plan$constraint_indices, 1L)
  expect_identical(plan$trafo_indices, c(2L, 3L))
  expect_identical(names(plan$constraint_sets), "first")
  expect_identical(names(plan$trafo_sets), c("second", "third"))

  snapshot = collection$subset(
    c("third.z", "first.x", "second.y"),
    allow_dangling_dependencies = TRUE
  )
  expect_identical(
    snapshot$trafo(list(
      third.z = 1L,
      unknown = 9L,
      second.y = 2L,
      first.x = 3L
    )),
    list(first.x = 3L, second.y = 12L, third.z = 101L)
  )

  postfix_child = ps(x = p_int())
  postfix_child$extra_trafo = function(x) {
    x$x = x$x + 1L
    x
  }
  postfix = ParamSetCollection$new(
    list(component = postfix_child),
    postfix_names = TRUE
  )$subset("x.component", allow_dangling_dependencies = TRUE)
  expect_identical(
    postfix$trafo(list(x.component = 1L)),
    list(x.component = 2L)
  )
})

test_that("native detached wrappers preserve callback frames and names", {
  skip_if_not(native_collection_detach_available())
  observed = new.env(parent = emptyenv())
  child = ps(x = p_int(), y = p_int())
  child$constraint = function(x) {
    observed$constraint_call = deparse(sys.call())
    observed$constraint_parent = ls(parent.frame(), all.names = TRUE)
    observed$constraint_names = names(x)
    TRUE
  }
  child$extra_trafo = function(x) {
    observed$trafo_call = deparse(sys.call())
    observed$trafo_parent = ls(parent.frame(), all.names = TRUE)
    observed$trafo_names = names(x)
    x
  }
  snapshot = psc(component = child)$subset(
    c("component.x", "component.y"),
    allow_dangling_dependencies = TRUE
  )

  expect_true(snapshot$constraint(list(
    component.y = 2L,
    ignored = 0L,
    component.x = 1L
  )))
  expect_identical(observed$constraint_call, "constraint(constraining_values)")
  expect_identical(observed$constraint_names, c("y", "x"))
  expect_true(all(c(
    "constraint", "constraining_ids", "constraining_values",
    "set_index", "sets_with_constraints", "translation", "x"
  ) %in% observed$constraint_parent))

  snapshot$trafo(list(
    component.y = 2L,
    ignored = 0L,
    component.x = 1L
  ))
  expect_identical(observed$trafo_call, "trafo(changing_values_in)")
  expect_identical(observed$trafo_names, c("y", "x"))
  expect_true(all(c(
    "changing_ids", "changing_values_in", "i", "set_index", "trafo"
  ) %in% observed$trafo_parent))
})

test_that("native callback snapshots detach slots but share closure state", {
  skip_if_not(native_collection_detach_available())
  closure_state = new.env(parent = emptyenv())
  closure_state$increment = 1L
  closure_state$limit = 5L
  child = ps(x = p_int())
  child$extra_trafo = function(x) {
    x$x = x$x + closure_state$increment
    x
  }
  child$constraint = function(x) x$x <= closure_state$limit
  collection = psc(component = child)
  snapshot = collection$subset(
    "component.x",
    allow_dangling_dependencies = TRUE
  )

  child$extra_trafo = function(x) {
    x$x = x$x + 100L
    x
  }
  child$constraint = function(x) FALSE
  expect_identical(
    snapshot$trafo(list(component.x = 1L))$component.x,
    2L
  )
  expect_true(snapshot$constraint(list(component.x = 4L)))

  closure_state$increment = 3L
  closure_state$limit = 2L
  expect_identical(
    snapshot$trafo(list(component.x = 1L))$component.x,
    4L
  )
  expect_false(snapshot$constraint(list(component.x = 4L)))
})

test_that("detached constraints short circuit and keep_constraint is honored", {
  skip_if_not(native_collection_detach_available())
  events = character()
  first = ps(x = p_int())
  second = ps(y = p_int())
  first$constraint = function(x) {
    events <<- c(events, "first")
    FALSE
  }
  second$constraint = function(x) {
    events <<- c(events, "second")
    TRUE
  }
  collection = psc(first = first, second = second)
  snapshot = collection$subset(
    c("second.y", "first.x"),
    allow_dangling_dependencies = TRUE
  )
  expect_false(snapshot$constraint(list(first.x = 1L, second.y = 2L)))
  expect_identical(events, "first")

  without = collection$subset(
    c("first.x", "second.y"),
    allow_dangling_dependencies = TRUE,
    keep_constraint = FALSE
  )
  expect_null(without$constraint)
})

test_that("unsupported collection callback graphs fail closed", {
  skip_if_not(native_collection_detach_available())
  two_argument = ps(x = p_int())
  two_argument$extra_trafo = function(x, param_set) x
  expect_null(native_collection_detach_plan(
    psc(component = two_argument),
    "component.x"
  ))

  nested = psc(layer = psc(component = ps(x = p_int())))
  expect_null(native_collection_detach_plan(nested, "layer.component.x"))

  ChildSubclass = R6::R6Class(
    "NativeDetachChildSubclass",
    inherit = ParamSet
  )
  child_subclass = ChildSubclass$new(list(x = p_int()))
  expect_null(native_collection_detach_plan(
    psc(component = child_subclass),
    "component.x"
  ))

  CollectionSubclass = R6::R6Class(
    "NativeDetachCollectionSubclass",
    inherit = ParamSetCollection
  )
  collection_subclass = CollectionSubclass$new(list(
    component = ps(x = p_int())
  ))
  expect_null(native_collection_detach_plan(
    collection_subclass,
    "component.x"
  ))

  public_environment = ps(x = p_int())
  public_callback = function(x) x
  environment(public_callback) = public_environment
  public_environment$extra_trafo = public_callback
  expect_null(native_collection_detach_plan(
    psc(component = public_environment),
    "component.x"
  ))

  private_environment = ps(x = p_int())
  private_callback = function(x) TRUE
  environment(private_callback) =
    private_environment$.__enclos_env__$private
  private_environment$constraint = private_callback
  expect_null(native_collection_detach_plan(
    psc(component = private_environment),
    "component.x"
  ))

  enclosure_environment = ps(x = p_int())
  enclosure_callback = function(x) TRUE
  environment(enclosure_callback) =
    enclosure_environment$.__enclos_env__
  enclosure_environment$constraint = enclosure_callback
  expect_null(native_collection_detach_plan(
    psc(component = enclosure_environment),
    "component.x"
  ))

  Box = R6::R6Class(
    "NativeDetachValueBox",
    public = list(value = NULL)
  )
  value_child = ps(payload = p_uty())
  value_child$values = list(payload = Box$new())
  value_child$extra_trafo = function(x) x
  expect_null(native_collection_detach_plan(
    psc(component = value_child),
    "component.payload"
  ))
})

test_that("feature factories retain only their own callback state", {
  skip_if_not(native_collection_detach_available())
  child = ps(x = p_int())
  child$constraint = function(x) TRUE
  child$extra_trafo = function(x) x
  snapshot = psc(component = child)$subset(
    "component.x",
    allow_dangling_dependencies = TRUE,
    keep_constraint = FALSE
  )
  bindings = ls(environment(snapshot$extra_trafo), all.names = TRUE)
  expect_setequal(
    bindings,
    c("postfix", "runner", "trafo_indices", "trafo_sets", "translation")
  )
  expect_false("plan" %in% bindings)
  expect_false("constraint_sets" %in% bindings)
})

test_that("native callback admission never forces delayed private slots", {
  skip_if_not(native_collection_detach_available())
  events = new.env(parent = emptyenv())
  events$forced = 0L
  child = ps(x = p_int())
  private = child$.__enclos_env__$private
  unlockBinding(".extra_trafo", private)
  delayedAssign(
    ".extra_trafo",
    {
      events$forced = events$forced + 1L
      function(x) x
    },
    assign.env = private
  )
  lockBinding(".extra_trafo", private)
  collection = psc(component = child)
  expect_null(native_collection_detach_plan(collection, "component.x"))
  expect_identical(events$forced, 0L)
})
