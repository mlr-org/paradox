plain_binding_snapshot = function(environment, name) {
  .Call(paradox:::C_plain_binding_snapshot, environment, name)
}

delayed_literal_binding = function(environment, name, expression) {
  eval(
    call(
      "delayedAssign",
      name,
      expression,
      baseenv(),
      environment
    ),
    baseenv()
  )
}

delayed_counted_binding = function(environment, name, value) {
  state = new.env(parent = emptyenv())
  state$evaluations = 0L
  evaluation = new.env(parent = baseenv())
  evaluation$state = state
  evaluation$value = value
  delayedAssign(
    name,
    {
      state$evaluations = state$evaluations + 1L
      value
    },
    eval.env = evaluation,
    assign.env = environment
  )
  state
}

test_that("plain binding snapshots retain ordinary realized values exactly", {
  environment = new.env(parent = emptyenv())
  values = list(
    TRUE,
    NULL,
    new.env(parent = emptyenv()),
    function() NULL,
    new("externalptr"),
    quote(a + b),
    as.name("x")
  )
  names(values) = c(
    "logical", "null", "environment", "function", "externalptr",
    "language", "symbol"
  )

  for (name in names(values)) {
    assign(name, values[[name]], envir = environment)
    expect_identical(
      plain_binding_snapshot(environment, name),
      list(ok = TRUE, value = values[[name]])
    )
  }
})

test_that("plain binding snapshots reject exact delayed literal values", {
  environment = new.env(parent = emptyenv())
  values = list(
    TRUE,
    NULL,
    new.env(parent = emptyenv()),
    function() NULL,
    new("externalptr"),
    quote(a + b),
    as.name("x")
  )
  names(values) = c(
    "logical", "null", "environment", "function", "externalptr",
    "language", "symbol"
  )

  for (name in names(values)) {
    delayed_literal_binding(environment, name, values[[name]])
  }

  for (index in seq_along(values)) {
    name = names(values)[[index]]
    expect_identical(
      plain_binding_snapshot(environment, name),
      list(ok = FALSE, value = NULL)
    )
    # The first five expressions are self-evaluating. The final two deliberately
    # remain unforced: their promise expressions are exactly the language and
    # symbol values that a substitute()-based probe cannot distinguish from
    # direct realized bindings.
    if (index <= 5L) {
      expect_identical(
        get(name, envir = environment, inherits = FALSE),
        values[[index]]
      )
    }
  }
})

test_that("plain binding snapshots reject promises even after forcing", {
  environment = new.env(parent = emptyenv())
  evaluation = list2env(
    list(value = quote(a + b)),
    parent = baseenv()
  )
  delayedAssign(
    "value",
    value,
    eval.env = evaluation,
    assign.env = environment
  )

  expect_identical(
    get("value", envir = environment, inherits = FALSE),
    quote(a + b)
  )
  expect_identical(
    plain_binding_snapshot(environment, "value"),
    list(ok = FALSE, value = NULL)
  )
})

test_that("plain binding snapshots never evaluate delayed expressions", {
  environment = new.env(parent = emptyenv())
  value = new.env(parent = emptyenv())
  state = delayed_counted_binding(environment, "value", value)

  expect_identical(
    plain_binding_snapshot(environment, "value"),
    list(ok = FALSE, value = NULL)
  )
  expect_identical(state$evaluations, 0L)
})

test_that("plain binding snapshots reject non-value frame bindings inertly", {
  parent = new.env(parent = emptyenv())
  parent$inherited = TRUE
  environment = new.env(parent = parent)
  active_reads = 0L
  makeActiveBinding("active", function(value) {
    if (!missing(value)) {
      stop("test binding is read-only")
    }
    active_reads <<- active_reads + 1L
    TRUE
  }, environment)

  expect_identical(
    plain_binding_snapshot(environment, "absent"),
    list(ok = FALSE, value = NULL)
  )
  expect_identical(
    plain_binding_snapshot(environment, "inherited"),
    list(ok = FALSE, value = NULL)
  )
  expect_identical(
    plain_binding_snapshot(environment, "active"),
    list(ok = FALSE, value = NULL)
  )
  missing = list2env(alist(value = ), parent = emptyenv())
  expect_identical(
    plain_binding_snapshot(missing, "value"),
    list(ok = FALSE, value = NULL)
  )
  expect_identical(active_reads, 0L)
})

test_that("plain binding snapshots distinguish locked and absent bindings", {
  environment = new.env(parent = emptyenv())
  environment$value = 42L
  lockBinding("value", environment)
  lockEnvironment(environment, bindings = FALSE)

  expect_identical(
    plain_binding_snapshot(environment, "value"),
    list(ok = TRUE, value = 42L)
  )
  expect_identical(
    plain_binding_snapshot(environment, "absent"),
    list(ok = FALSE, value = NULL)
  )
})

test_that("user-database environments are rejected before binding APIs", {
  # R's user-defined object tables store an external pointer where ordinary
  # environments store a hash vector. The native facade must recognize the
  # same class gate as R itself before entering either representation.
  environment = new.env(parent = emptyenv())
  environment$value = 42L
  class(environment) = "UserDefinedDatabase"

  expect_identical(
    plain_binding_snapshot(environment, "value"),
    list(ok = FALSE, value = NULL)
  )

  shell = ps(x = p_dbl())
  class(shell) = c("UserDefinedDatabase", class(shell))
  expect_false(paradox:::.paradox_gateway_current_core(shell))
})

test_that("plain binding snapshot arguments are structurally exact", {
  environment = new.env(parent = emptyenv())

  expect_error(
    plain_binding_snapshot(list(), "value"),
    "`environment` must be an ordinary environment",
    fixed = TRUE
  )
  for (name in list(NULL, character(), NA_character_, "", c("a", "b"), 1L)) {
    expect_error(
      plain_binding_snapshot(environment, name),
      "`name` must be an ordinary non-empty character scalar",
      fixed = TRUE
    )
  }
  expect_error(
    plain_binding_snapshot(environment, structure("value", label = "x")),
    "`name` must be an ordinary non-empty character scalar",
    fixed = TRUE
  )
})
