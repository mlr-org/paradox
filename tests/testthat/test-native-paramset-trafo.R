paramset_trafo_symbol = function() {
  get("C_param_set_trafo_plan", envir = asNamespace("paradox"))
}

paramset_trafo_private = function(param_set) {
  param_set$.__enclos_env__$private
}

# Frozen copy of the pre-native method.  It is intentionally kept in this test
# so malformed-storage fallbacks and observable callback behavior can be
# compared without teaching the production implementation a second API.
paramset_trafo_legacy = function(self, x, param_set = self) {
  if (is.data.frame(x)) x = as.list(x)
  checkmate::assert_list(x, names = "unique")
  private = paramset_trafo_private(self)
  id = trafo = NULL
  trafos = private$.trafos[names(x), list(id = id, trafo = trafo), nomatch = 0]
  value = NULL
  if (nrow(trafos)) {
    trafos[, value := x[id]]
    transformed = mlr3misc::pmap(
      trafos,
      function(id, trafo, value) trafo(value)
    )
    x = mlr3misc::insert_named(
      x,
      mlr3misc::set_names(transformed, trafos$id)
    )
  }
  extra_trafo = self$extra_trafo
  if (!is.null(extra_trafo)) {
    xin = x
    if (checkmate::test_function(extra_trafo, args = c("x", "param_set"))) {
      x = extra_trafo(x = xin, param_set = param_set)
    } else {
      x = extra_trafo(xin)
    }
  }
  x
}

paramset_trafo_without_srcref = function(call) {
  attr(call, "srcref") = NULL
  call
}

test_that("native ParamSet trafo planning snapshots matches in input order", {
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
    absent = p_lgl(trafo = record("absent", identity)),
    nothing = p_uty(trafo = record("nothing", function(value) NULL)),
    many = p_int(trafo = record(
      "many",
      function(value) c(value, value + 10L)
    )),
    choice = p_fct(list(one = 1L, two = 2L))
  )
  x = list(
    many = 1L,
    unknown = new.env(parent = emptyenv()),
    choice = "two",
    z = 2,
    nothing = "payload"
  )

  private = paramset_trafo_private(parameter_set)
  plan = .Call(paramset_trafo_symbol(), x, private$.trafos)
  expect_type(plan, "list")
  expect_length(plan, 3L)
  expect_identical(plan[[1L]], c("many", "choice", "z", "nothing"))
  expect_true(all(vapply(plan[[2L]], is.function, logical(1L))))
  expect_identical(plan[[3L]], unname(x[plan[[1L]]]))

  events$value = character()
  expected = paramset_trafo_legacy(parameter_set, x)
  expected_events = events$value
  events$value = character()
  actual = parameter_set$trafo(x)
  expect_identical(actual, expected)
  expect_identical(events$value, expected_events)
  expect_identical(events$value, c("many", "z", "nothing"))
  expect_identical(actual$many, c(1L, 11L))
  expect_identical(actual$choice, 2L)
  expect_identical(actual$nothing, NULL)
  expect_identical(actual$unknown, x$unknown)
  expect_identical(names(actual), names(x))

  no_match = ps(x = p_dbl(trafo = identity))
  expect_identical(no_match$trafo(list(other = 1)), list(other = 1))
  expect_identical(no_match$trafo(list()), list())
  expect_identical(
    no_match$trafo(setNames(list(), character())),
    setNames(list(), character())
  )
})

test_that("ParamSet trafo retains vector data-frame and ownership behavior", {
  parameter_set = ps(
    a = p_dbl(trafo = function(value) value + 1),
    b = p_int(trafo = function(value) as.integer(value * 2)),
    untouched = p_uty()
  )
  input = data.frame(a = 1:3, b = 4:6)
  expected = paramset_trafo_legacy(parameter_set, input)
  actual = parameter_set$trafo(input)
  expect_identical(actual, expected)
  expect_identical(actual$a, c(2, 3, 4))
  expect_identical(actual$b, c(8L, 10L, 12L))
  expect_identical(input$a, 1:3)
  expect_identical(input$b, 4:6)

  shared = new.env(parent = emptyenv())
  x = list(a = 1, untouched = shared)
  original_names = names(x)
  transformed = parameter_set$trafo(x)
  expect_identical(x, list(a = 1, untouched = shared))
  expect_identical(transformed$untouched, shared)
  names(transformed)[[1L]] = "changed"
  transformed[[1L]] = -1
  expect_identical(names(x), original_names)
  expect_identical(x$a, 1)
})

test_that("ParamSet trafo preserves callback calls promises and conditions", {
  observe = function(arg) {
    list(
      substituted = substitute(arg),
      matched = paramset_trafo_without_srcref(match.call()),
      called = paramset_trafo_without_srcref(sys.call()),
      parent_names = sort(ls(parent.frame()))
    )
  }
  parameter_set = ps(x = p_dbl(trafo = observe))
  result = parameter_set$trafo(list(x = 1))$x
  expect_identical(result$substituted, quote(value))
  expect_identical(result$matched, quote(trafo(arg = value)))
  expect_identical(result$called, quote(trafo(value)))
  expect_identical(result$parent_names, c("id", "trafo", "value"))

  delayed = ps(x = p_dbl(trafo = function(arg) function() arg))
  delayed_result = delayed$trafo(list(x = 7))$x
  expect_true(is.function(delayed_result))
  expect_identical(delayed_result(), 7)

  capture_delayed = function(arg) {
    callback_parent = parent.frame()
    function() list(value = arg, id = callback_parent$id)
  }
  delayed_many = ps(
    first = p_uty(trafo = capture_delayed),
    second = p_uty(trafo = capture_delayed),
    third = p_uty(trafo = capture_delayed)
  )
  delayed_input = list(second = "two", third = "three", first = "one")
  expected_delayed = paramset_trafo_legacy(delayed_many, delayed_input)
  actual_delayed = delayed_many$trafo(delayed_input)
  expected_forced = lapply(expected_delayed, function(callback) callback())
  actual_forced = lapply(actual_delayed, function(callback) callback())
  expect_identical(actual_forced, expected_forced)
  expect_identical(
    actual_forced,
    list(
      second = list(value = "two", id = "second"),
      third = list(value = "three", id = "third"),
      first = list(value = "one", id = "first")
    )
  )

  special = ps(x = p_uty(trafo = quote))
  expect_identical(special$trafo(list(x = 1))$x, quote(value))

  failing = ps(x = p_dbl(trafo = function(value) stop("planned failure")))
  events = character()
  failure = tryCatch(
    failing$trafo(list(x = 1)),
    error = function(condition) {
      events <<- c(events, "caught")
      condition
    }
  )
  expect_s3_class(failure, "error")
  expect_identical(conditionMessage(failure), "planned failure")
  expect_identical(conditionCall(failure), quote(trafo(value)))
  expect_identical(events, "caught")
})

test_that("ParamSet trafo retains callback side-effect warning and RNG order", {
  make_case = function() {
    state = new.env(parent = emptyenv())
    state$events = character()
    callback = function(id) {
      force(id)
      function(value) {
        state$events = c(state$events, id)
        if (id == "b") warning("ordered warning", call. = FALSE)
        value + stats::runif(1L)
      }
    }
    list(
      parameter_set = ps(
        a = p_dbl(trafo = callback("a")),
        b = p_dbl(trafo = callback("b")),
        c = p_dbl(trafo = callback("c"))
      ),
      state = state
    )
  }
  capture = function(fun) {
    warnings = character()
    value = withCallingHandlers(
      fun(),
      warning = function(condition) {
        warnings <<- c(warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    )
    list(value = value, warnings = warnings, seed = .Random.seed)
  }
  x = list(c = 3, a = 1, b = 2)

  reference = make_case()
  set.seed(921L)
  expected = capture(function() {
    paramset_trafo_legacy(reference$parameter_set, x)
  })
  expected_events = reference$state$events

  candidate = make_case()
  set.seed(921L)
  actual = capture(function() candidate$parameter_set$trafo(x))
  expect_identical(actual, expected)
  expect_identical(candidate$state$events, expected_events)
  expect_identical(candidate$state$events, c("c", "a", "b"))
  expect_identical(actual$warnings, "ordered warning")
})

test_that("ParamSet trafo snapshots callbacks and reads extra trafo afterwards", {
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
        private = paramset_trafo_private(holder$parameter_set)
        data.table::set(
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

  first = parameter_set$trafo(list(a = 1L, b = 3L))
  expect_identical(state$events, c("a", "old-b", "extra"))
  expect_identical(first$a, 2L)
  expect_identical(first$b, 13L)
  expect_identical(first$parameter_count, 2L)

  state$events = character()
  second = parameter_set$trafo(list(a = 2L, b = 4L))
  expect_identical(state$events, c("a", "new-b", "extra"))
  expect_identical(second$a, 3L)
  expect_identical(second$b, 104L)
})

test_that("ParamSet trafo retains live extra-trafo signatures and returns", {
  parameter_set = ps(x = p_dbl(trafo = function(value) value + 1))
  alternate = ps(marker = p_lgl())

  parameter_set$extra_trafo = function(x, param_set) {
    list(value = x$x, supplied_length = param_set$length)
  }
  expect_identical(
    parameter_set$trafo(list(x = 1), alternate),
    list(value = 2, supplied_length = 1L)
  )

  parameter_set$extra_trafo = function(x) unname(unlist(x))
  expect_identical(parameter_set$trafo(list(x = 2)), 3)

  parameter_set$extra_trafo = function(x) NULL
  expect_null(parameter_set$trafo(list(x = 3)))
})

test_that("native ParamSet trafo planning fails closed on extensions", {
  symbol = paramset_trafo_symbol()
  parameter_set = ps(x = p_dbl(trafo = function(value) value + 1))
  private = paramset_trafo_private(parameter_set)

  expect_null(.Call(symbol, structure(list(x = 1), class = "custom"), private$.trafos))
  attributed = structure(list(x = 1), note = TRUE)
  expect_null(.Call(symbol, attributed, private$.trafos))
  expect_identical(
    parameter_set$trafo(attributed),
    paramset_trafo_legacy(parameter_set, attributed)
  )
  expect_identical(attr(parameter_set$trafo(attributed), "note"), TRUE)
  expect_null(.Call(symbol, structure(list(1), names = NULL), private$.trafos))
  expect_null(.Call(symbol, setNames(list(1), ""), private$.trafos))
  expect_null(.Call(symbol, setNames(list(1), NA_character_), private$.trafos))
  expect_null(.Call(symbol, setNames(list(1, 2), c("x", "x")), private$.trafos))

  InheritedParamSet = R6::R6Class(
    "NativeTrafoInheritedParamSet",
    inherit = ParamSet
  )
  subclass = InheritedParamSet$new(list(
    x = p_dbl(trafo = function(value) value + 1)
  ))
  expect_identical(subclass$trafo(list(x = 1)), list(x = 2))

  child = ps(
    x = p_dbl(trafo = function(value) value + 1),
    .extra_trafo = function(x) c(x, list(child = TRUE))
  )
  collection = ParamSetCollection$new(list(component = child))
  expect_identical(
    collection$trafo(list(component.x = 1)),
    list(component.x = 2, component.child = TRUE)
  )
})

test_that("native ParamSet trafo planning rejects malformed storage softly", {
  symbol = paramset_trafo_symbol()
  parameter_set = ps(x = p_dbl(trafo = identity))
  valid = paramset_trafo_private(parameter_set)$.trafos
  x = list(x = 1)

  replace_column = function(table, column, value) {
    attributes_saved = attributes(table)
    result = unclass(table)
    result[[column]] = value
    attributes(result) = attributes_saved
    result
  }
  expect_null(.Call(symbol, 1L, valid))
  expect_null(.Call(symbol, x, unclass(valid)))

  unnamed = valid
  names(unnamed) = NULL
  expect_null(.Call(symbol, x, unnamed))
  extra_column = c(valid, list(unexpected = TRUE))
  expect_null(.Call(symbol, x, extra_column))

  unkeyed = data.table::copy(valid)
  data.table::setkeyv(unkeyed, NULL)
  expect_null(.Call(symbol, x, unkeyed))
  expect_identical(parameter_set$trafo(x), list(x = 1))

  wrong_ids = replace_column(valid, "id", 1L)
  expect_null(.Call(symbol, x, wrong_ids))
  attributed_ids = replace_column(
    valid,
    "id",
    structure(valid$id, note = TRUE)
  )
  expect_null(.Call(symbol, x, attributed_ids))
  attributed_callbacks = replace_column(
    valid,
    "trafo",
    structure(valid$trafo, note = TRUE)
  )
  expect_null(.Call(symbol, x, attributed_callbacks))

  for (bad_id in list(NA_character_, "")) {
    malformed = data.table::data.table(
      id = bad_id,
      trafo = list(identity),
      key = "id"
    )
    expect_null(.Call(symbol, x, malformed))
  }
  duplicate = data.table::data.table(
    id = c("x", "x"),
    trafo = list(identity, identity),
    key = "id"
  )
  expect_null(.Call(symbol, x, duplicate))
  non_function = data.table::data.table(
    id = "x",
    trafo = list(1L),
    key = "id"
  )
  expect_null(.Call(symbol, x, non_function))
})

test_that("malformed ParamSet trafo storage falls back without repeating callbacks", {
  events = new.env(parent = emptyenv())
  events$value = character()
  parameter_set = ps(x = p_dbl(trafo = identity))
  private = paramset_trafo_private(parameter_set)
  private$.trafos = data.table::data.table(
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
    key = "id"
  )

  events$value = character()
  expected_error = tryCatch(
    paramset_trafo_legacy(parameter_set, list(x = 1)),
    error = identity
  )
  expected_events = events$value
  events$value = character()
  actual_error = tryCatch(parameter_set$trafo(list(x = 1)), error = identity)

  expect_s3_class(actual_error, "error")
  expect_identical(conditionMessage(actual_error), conditionMessage(expected_error))
  expect_identical(events$value, expected_events)
  expect_identical(events$value, c("first", "second"))
})

test_that("native ParamSet trafo matching respects character encodings", {
  utf8 = enc2utf8("caf\u00e9")
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  Encoding(utf8) = "UTF-8"
  Encoding(latin1) = "latin1"
  skip_if(is.na(latin1), "latin1 conversion is unavailable")

  input = setNames(list(1), latin1)
  table = structure(
    list(id = utf8, trafo = list(function(value) value + 1)),
    class = c("data.table", "data.frame"),
    sorted = "id"
  )
  plan = .Call(paramset_trafo_symbol(), input, table)
  expect_identical(plan[[1L]], utf8)
  expect_identical(plan[[3L]], list(1))
})

test_that("native ParamSet trafo planner handles large canonical inputs", {
  size = 70000L
  ids = sprintf("x%05d", seq_len(size))
  callbacks = rep(list(identity), size)
  table = structure(
    list(id = ids, trafo = callbacks),
    class = c("data.table", "data.frame"),
    sorted = "id"
  )
  values = setNames(as.list(seq_len(size)), ids)
  plan = .Call(paramset_trafo_symbol(), values, table)
  expect_identical(plan[[1L]], ids)
  expect_length(plan[[2L]], size)
  expect_identical(plan[[3L]][c(1L, size)], unname(values[c(1L, size)]))
})
