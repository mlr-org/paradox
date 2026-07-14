test_that("Design transpose batches canonical individual trafos exactly", {
  events = new.env(parent = emptyenv())
  events$value = character()
  record = function(id, transform) {
    force(id)
    force(transform)
    function(value) {
      events$value = c(events$value, id)
      transform(value)
    }
  }

  parameter_set = ps(
    z = p_dbl(trafo = record("z", function(value) value + 0.5)),
    missing = p_lgl(trafo = record("missing", identity)),
    nothing = p_uty(trafo = record("nothing", function(value) NULL)),
    many = p_int(trafo = record(
      "many",
      function(value) c(value, value + 10L)
    ))
  )
  design = Design$new(
    parameter_set,
    data.table(
      many = 1:2,
      nothing = list("first", "second"),
      z = c(2, 3),
      missing = c(NA, FALSE)
    ),
    remove_dupl = FALSE
  )
  original_data = copy(design$data)

  for (filter_na in c(FALSE, TRUE)) {
    rows = design$transpose(filter_na = filter_na, trafo = FALSE)
    events$value = character()
    expected = map(rows, parameter_set$trafo)
    expected_events = events$value

    events$value = character()
    actual = design$transpose(filter_na = filter_na, trafo = TRUE)
    expect_identical(actual, expected)
    expect_identical(events$value, expected_events)
    expect_identical(
      events$value,
      if (filter_na) {
        c("many", "nothing", "z", "many", "nothing", "z", "missing")
      } else {
        rep(c("many", "nothing", "z", "missing"), 2L)
      }
    )
    expect_identical(actual[[1L]]$nothing, NULL)
    expect_identical(actual[[1L]]$many, c(1L, 11L))
  }

  names(actual[[1L]])[[1L]] = "changed"
  actual[[1L]]$many[[1L]] = -1L
  expect_identical(design$data, original_data)
  expect_identical(
    names(actual[[2L]]),
    c("many", "nothing", "z", "missing")
  )
})

test_that("Design transpose retains trafo errors and empty-row behavior", {
  parameter_set = ps(
    absent = p_dbl(trafo = function(value) value),
    failing = p_int(trafo = function(value) stop("expected trafo failure"))
  )
  design = Design$new(
    parameter_set,
    data.table(absent = NA_real_, failing = 1L),
    remove_dupl = FALSE
  )

  reference_error = tryCatch(
    map(
      design$transpose(filter_na = TRUE, trafo = FALSE),
      parameter_set$trafo
    ),
    error = identity
  )
  actual_error = tryCatch(
    design$transpose(filter_na = TRUE, trafo = TRUE),
    error = identity
  )
  expect_s3_class(actual_error, "error")
  expect_identical(conditionMessage(actual_error), conditionMessage(reference_error))
  expect_identical(conditionCall(actual_error), quote(trafo(value)))
  expect_identical(conditionCall(actual_error), conditionCall(reference_error))

  empty_parameter_set = ps(x = p_dbl(trafo = function(value) value + 1))
  zero_rows = Design$new(
    empty_parameter_set,
    data.table(x = numeric()),
    remove_dupl = FALSE
  )
  expect_identical(zero_rows$transpose(), list())

  empty_row = Design$new(
    empty_parameter_set,
    data.table(x = NA_real_),
    remove_dupl = FALSE
  )
  expect_identical(
    empty_row$transpose(filter_na = TRUE),
    list(setNames(list(), character()))
  )
})

test_that("Design callbacks retain ParamSet trafo call frames and promises", {
  observations = list()
  callback = function(value) {
    callback_frame = parent.frame()
    observations[[length(observations) + 1L]] <<- list(
      call = sys.call(),
      parent_names = sort(ls(callback_frame, all.names = TRUE)),
      id = callback_frame$id,
      same_trafo = identical(callback_frame$trafo, callback),
      argument_expression = substitute(value, callback_frame)
    )
    local({
      delayed_value = value
      function() delayed_value
    })
  }
  parameter_set = ps(x = p_dbl(trafo = callback))
  design = Design$new(
    parameter_set,
    data.table(x = c(1, 2)),
    remove_dupl = FALSE
  )

  result = design$transpose()
  expect_identical(observations, rep(list(list(
    call = quote(trafo(value)),
    parent_names = c("id", "trafo", "value"),
    id = "x",
    same_trafo = TRUE,
    argument_expression = quote(value)
  )), 2L))
  expect_identical(
    vapply(result, function(row) row$x(), numeric(1L)),
    c(1, 2)
  )
})

test_that("Design trafo admission rejects replaced and reparented wrappers", {
  parameter_set = ps(x = p_dbl(trafo = function(value) value + 1))
  design = Design$new(
    parameter_set,
    data.table(x = c(1, 2)),
    remove_dupl = FALSE
  )
  unlockBinding("trafo", parameter_set)
  assign(
    "trafo",
    function(x, param_set = self) {
      x$replacement = TRUE
      x
    },
    envir = parameter_set
  )
  lockBinding("trafo", parameter_set)
  replaced = design$transpose()
  expect_true(all(map_lgl(replaced, function(row) row$replacement)))
  expect_identical(map_dbl(replaced, "x"), c(1, 2))

  error_set = ps(x = p_dbl(trafo = identity))
  error_design = Design$new(
    error_set,
    data.table(x = 1),
    remove_dupl = FALSE
  )
  unlockBinding("trafo", error_set)
  assign(
    "trafo",
    function(x, param_set = self) stop("replacement trafo failure"),
    envir = error_set
  )
  lockBinding("trafo", error_set)
  replacement_error = tryCatch(error_design$transpose(), error = identity)
  expect_identical(conditionCall(replacement_error), quote(ps$trafo(x)))

  reparented_set = ps(x = p_dbl(trafo = identity))
  reparented_design = Design$new(
    reparented_set,
    data.table(x = 3),
    remove_dupl = FALSE
  )
  original = reparented_set$trafo
  wrapper_environment = new.env(parent = asNamespace("paradox"))
  wrapper_environment$self = reparented_set
  wrapper_environment$private = reparented_set$.__enclos_env__$private
  wrapper_environment$.__ParamSet__trafo = function(
      self, private, super, x, param_set = self) {
    x$reparented = TRUE
    x
  }
  environment(original) = wrapper_environment
  unlockBinding("trafo", reparented_set)
  assign("trafo", original, envir = reparented_set)
  lockBinding("trafo", reparented_set)
  expect_true(reparented_design$transpose()[[1L]]$reparented)
})

test_that("Design admission does not force delayed trafo bindings", {
  parameter_set = ps(x = p_dbl(trafo = identity))
  state = new.env(parent = emptyenv())
  state$forced = 0L
  unlockBinding("trafo", parameter_set)
  delayedAssign(
    "trafo",
    {
      state$forced = state$forced + 1L
      function(x, param_set = self) {
        x$delayed = TRUE
        x
      }
    },
    assign.env = parameter_set
  )
  lockBinding("trafo", parameter_set)

  symbol = getDLLRegisteredRoutines(
    getLoadedDLLs()[["paradox"]]
  )$.Call[["param_set_surface_auth"]]
  expect_false(.Call(symbol, parameter_set, 1L))
  expect_identical(state$forced, 0L)

  empty = Design$new(
    parameter_set,
    data.table(x = numeric()),
    remove_dupl = FALSE
  )
  expect_identical(empty$transpose(), list())
  expect_identical(state$forced, 0L)

  nonempty = Design$new(
    parameter_set,
    data.table(x = 1),
    remove_dupl = FALSE
  )
  expect_true(nonempty$transpose()[[1L]]$delayed)
  expect_identical(state$forced, 1L)
})

test_that("Design forces a delayed private trafo table at its public access", {
  events = new.env(parent = emptyenv())
  events$value = character()
  parameter_set = ps(x = p_dbl(0, 1, trafo = function(value) {
    events$value = c(events$value, "callback")
    value + 1
  }))
  design = Design$new(
    parameter_set,
    data.table(x = 1),
    remove_dupl = FALSE
  )

  private = parameter_set$.__enclos_env__$private
  stored_trafos = private$.trafos
  evaluation_environment = new.env(parent = baseenv())
  evaluation_environment$events = events
  evaluation_environment$stored_trafos = stored_trafos
  delayedAssign(
    ".trafos",
    {
      events$value = c(events$value, ".trafos")
      stored_trafos
    },
    assign.env = private,
    eval.env = evaluation_environment
  )

  symbol = getDLLRegisteredRoutines(
    getLoadedDLLs()[["paradox"]]
  )$.Call[["param_set_surface_auth"]]
  expect_false(.Call(symbol, parameter_set, 1L))
  expect_identical(events$value, character())

  result = design$transpose()
  expect_identical(result, list(list(x = 2)))
  expect_identical(events$value, c(".trafos", "callback"))
})

test_that("Design transpose fails closed for overrides and extra trafos", {
  subclass_calls = new.env(parent = emptyenv())
  subclass_calls$n = 0L
  OverrideParamSet = R6::R6Class(
    "DesignTransposeOverrideParamSet",
    inherit = ParamSet,
    public = list(
      trafo = function(x, param_set = self) {
        subclass_calls$n = subclass_calls$n + 1L
        x$x = x$x + 100
        x$overridden = TRUE
        x
      }
    )
  )
  subclass = OverrideParamSet$new(list(
    x = p_dbl(trafo = function(value) value + 1)
  ))
  subclass_design = Design$new(
    subclass,
    data.table(x = c(1, 2)),
    remove_dupl = FALSE
  )
  rows = subclass_design$transpose(trafo = FALSE)
  subclass_calls$n = 0L
  expected = map(rows, subclass$trafo)
  expected_calls = subclass_calls$n
  subclass_calls$n = 0L
  actual = subclass_design$transpose()
  expect_identical(actual, expected)
  expect_identical(subclass_calls$n, expected_calls)
  expect_identical(subclass_calls$n, 2L)
  expect_true(all(map_lgl(actual, function(x) isTRUE(x$overridden))))

  events = new.env(parent = emptyenv())
  events$value = character()
  with_extra = ps(
    x = p_dbl(trafo = function(value) {
      events$value = c(events$value, "individual")
      value + 1
    }),
    .extra_trafo = function(x, param_set) {
      events$value = c(events$value, "extra")
      list(x = x$x, parameter_count = param_set$length)
    }
  )
  extra_design = Design$new(
    with_extra,
    data.table(x = c(1, 2)),
    remove_dupl = FALSE
  )
  rows = extra_design$transpose(trafo = FALSE)
  events$value = character()
  expected = map(rows, with_extra$trafo)
  expected_events = events$value
  events$value = character()
  actual = extra_design$transpose()
  expect_identical(actual, expected)
  expect_identical(events$value, expected_events)
  expect_identical(events$value, rep(c("individual", "extra"), 2L))
})

test_that("Design transpose observes ParamSet mutations between rows", {
  make_case = function() {
    state = new.env(parent = emptyenv())
    state$events = character()
    state$first = TRUE
    holder = new.env(parent = emptyenv())

    old_b = function(value) {
      state$events = c(state$events, "old-b")
      value + 10L
    }
    new_b = function(value) {
      state$events = c(state$events, "new-b")
      value + 100L
    }
    parameter_set = ps(
      a = p_int(trafo = function(value) {
        state$events = c(state$events, "a")
        if (state$first) {
          state$first = FALSE
          private = holder$parameter_set$.__enclos_env__$private
          set(
            private$.trafos,
            which(private$.trafos$id == "b"),
            "trafo",
            list(new_b)
          )
          holder$parameter_set$extra_trafo = function(x, param_set) {
            state$events = c(state$events, "extra")
            c(x, list(parameter_count = param_set$length))
          }
        }
        value + 1L
      }),
      b = p_int(trafo = old_b)
    )
    holder$parameter_set = parameter_set
    list(
      design = Design$new(
        parameter_set,
        data.table(a = 1:2, b = 3:4),
        remove_dupl = FALSE
      ),
      parameter_set = parameter_set,
      state = state
    )
  }

  reference = make_case()
  expected = map(
    reference$design$transpose(trafo = FALSE),
    reference$parameter_set$trafo
  )
  expected_events = reference$state$events

  candidate = make_case()
  actual = candidate$design$transpose()
  expect_identical(actual, expected)
  expect_identical(candidate$state$events, expected_events)
  expect_identical(
    candidate$state$events,
    c("a", "old-b", "extra", "a", "new-b", "extra")
  )
  expect_identical(actual[[1L]]$b, 13L)
  expect_identical(actual[[2L]]$b, 104L)
})

test_that("Design transpose observes public trafo replacements between rows", {
  make_case = function() {
    state = new.env(parent = emptyenv())
    state$events = character()
    state$first = TRUE
    holder = new.env(parent = emptyenv())

    replacement_two = function(x, param_set = self) {
      state$events = c(state$events, "replacement-two")
      x$x = x$x + 1000L
      x
    }
    replacement_one = function(x, param_set = self) {
      state$events = c(state$events, "replacement-one")
      unlockBinding("trafo", holder$parameter_set)
      assign("trafo", replacement_two, envir = holder$parameter_set)
      lockBinding("trafo", holder$parameter_set)
      x$x = x$x + 100L
      x
    }
    parameter_set = ps(x = p_int(trafo = function(value) {
      state$events = c(state$events, "individual")
      if (state$first) {
        state$first = FALSE
        unlockBinding("trafo", holder$parameter_set)
        assign("trafo", replacement_one, envir = holder$parameter_set)
        lockBinding("trafo", holder$parameter_set)
      }
      value + 1L
    }))
    holder$parameter_set = parameter_set
    list(
      design = Design$new(
        parameter_set,
        data.table(x = 1:3),
        remove_dupl = FALSE
      ),
      parameter_set = parameter_set,
      state = state
    )
  }

  reference = make_case()
  expected = map(
    reference$design$transpose(trafo = FALSE),
    function(x) reference$parameter_set$trafo(x)
  )
  expected_events = reference$state$events

  candidate = make_case()
  actual = candidate$design$transpose()
  expect_identical(actual, expected)
  expect_identical(candidate$state$events, expected_events)
  expect_identical(
    candidate$state$events,
    c("individual", "replacement-one", "replacement-two")
  )
  expect_identical(map_int(actual, "x"), c(2L, 102L, 1003L))
})

test_that("Design transpose rejects malformed private trafo tables", {
  events = new.env(parent = emptyenv())
  events$value = character()
  parameter_set = ps(x = p_dbl(trafo = identity))
  design = Design$new(
    parameter_set,
    data.table(x = 1),
    remove_dupl = FALSE
  )
  parameter_set$.__enclos_env__$private$.trafos = data.table(
    id = c("x", "x"),
    trafo = list(
      function(value) {
        events$value = c(events$value, "first")
        value + 1
      },
      function(value) {
        events$value = c(events$value, "second")
        value + 2
      }
    ),
    unexpected = c(TRUE, FALSE),
    key = "id"
  )

  rows = design$transpose(trafo = FALSE)
  events$value = character()
  reference_error = tryCatch(map(rows, parameter_set$trafo), error = identity)
  reference_events = events$value
  events$value = character()
  actual_error = tryCatch(design$transpose(), error = identity)

  expect_s3_class(actual_error, "error")
  expect_identical(conditionMessage(actual_error), conditionMessage(reference_error))
  expect_identical(events$value, reference_events)
  expect_identical(events$value, c("first", "second"))
})
