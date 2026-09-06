test_that("ParamSet and Design share the registered transformation engine", {
  routines = getDLLRegisteredRoutines("paradox")$.Call
  expect_identical(routines$param_set_trafo$numParameters, 4L)
  expect_identical(routines$design_transpose_trafos$numParameters, 2L)
  expect_false("param_set_trafo_plan" %in% names(routines))
  expect_false("design_transpose_logscale_builtin" %in% names(routines))

  events = character()
  record = function(id, fn) {
    force(id)
    force(fn)
    function(value) {
      events <<- c(events, id)
      fn(value)
    }
  }
  parameter_set = ps(
    z = p_dbl(trafo = record("z", function(value) value + 0.5)),
    nothing = p_uty(trafo = record("nothing", function(value) NULL)),
    many = p_int(trafo = record("many", function(value) c(value, value + 10L)))
  )
  input = list(many = 1L, nothing = "x", z = 2)
  single = parameter_set$trafo(input)
  expect_identical(events, c("many", "nothing", "z"))
  expect_identical(single, list(
    many = c(1L, 11L),
    nothing = NULL,
    z = 2.5
  ))

  events = character()
  design = Design$new(
    parameter_set,
    data.table(many = 1:2, nothing = list("x", "y"), z = c(2, 3)),
    remove_dupl = FALSE
  )
  expect_identical(
    design$transpose(),
    list(
      list(many = c(1L, 11L), nothing = NULL, z = 2.5),
      list(many = c(2L, 12L), nothing = NULL, z = 3.5)
    )
  )
  expect_identical(events, rep(c("many", "nothing", "z"), 2L))
})

test_that("extra_trafo arity, replacement, warnings, and errors propagate once", {
  seen = NULL
  two_argument = ps(
    x = p_dbl(),
    .extra_trafo = function(x, param_set) {
      seen <<- param_set
      c(x, list(extra = TRUE))
    }
  )
  expect_identical(
    two_argument$trafo(list(x = 1)),
    list(x = 1, extra = TRUE)
  )
  expect_identical(seen, two_argument)

  one_argument = ps(
    x = p_dbl(),
    .extra_trafo = function(x) list(replaced = x$x + 1)
  )
  expect_identical(
    one_argument$trafo(list(x = 1)),
    list(replaced = 2)
  )

  warning_calls = 0L
  warning_set = ps(x = p_dbl(trafo = function(value) {
    warning_calls <<- warning_calls + 1L
    warning("once")
    value
  }))
  expect_warning(warning_set$trafo(list(x = 1)), "once")
  expect_identical(warning_calls, 1L)

  error_calls = 0L
  error_set = ps(x = p_dbl(trafo = function(value) {
    error_calls <<- error_calls + 1L
    stop("expected callback failure", call. = FALSE)
  }))
  expect_error(error_set$trafo(list(x = 1)), "expected callback failure")
  expect_identical(error_calls, 1L)
})

test_that("base extra_trafo retains legacy unnamed one-dimensional output", {
  parameter_set = ps(
    x = p_dbl(),
    .extra_trafo = function(x) list(x$x + 1)
  )
  expect_identical(parameter_set$trafo(list(x = 1)), list(2))

  design = Design$new(
    parameter_set,
    data.table(x = c(1, 2)),
    remove_dupl = FALSE
  )
  expect_identical(design$transpose(), list(list(2), list(3)))

  collection = psc(unit = parameter_set)
  expect_error(
    collection$trafo(list(unit.x = 1)),
    "one name for every element",
    fixed = TRUE
  )
})

test_that("Design freezes callback selection across reentry", {
  calls = character()
  parameter_set = ps(x = p_dbl())
  replacement = function(x) {
    calls <<- c(calls, "new")
    c(x, list(source = "new"))
  }
  original = function(x, param_set) {
    calls <<- c(calls, "old")
    if (length(calls) == 1L) param_set$extra_trafo = replacement
    c(x, list(source = "old"))
  }
  parameter_set$extra_trafo = original
  design = Design$new(
    parameter_set,
    data.table(x = c(1, 2)),
    remove_dupl = FALSE
  )

  first = design$transpose()
  expect_identical(map_chr(first, "source"), c("old", "old"))
  expect_identical(calls, c("old", "old"))

  second = design$transpose()
  expect_identical(map_chr(second, "source"), c("new", "new"))
  expect_identical(calls, c("old", "old", "new", "new"))
})

test_that("Design materializes ALTREP before selecting transformation state", {
  callbacks = 0L
  parameter_set = ps(
    x = p_dbl(),
    .extra_trafo = function(x) c(x, list(source = "old"))
  )
  column = native_stateful_altrep(
    c(1, 2),
    c(1, 2),
    callback = function() {
      callbacks <<- callbacks + 1L
      parameter_set$extra_trafo = function(x) c(x, list(source = "new"))
    },
    callback_after = 0L
  )
  data = structure(
    list(x = column),
    names = "x",
    row.names = c(NA_integer_, -2L),
    class = c("data.table", "data.frame")
  )
  design = Design$new(parameter_set, data, remove_dupl = FALSE)
  expect_identical(callbacks, 0L)

  result = design$transpose()
  expect_identical(callbacks, 1L)
  expect_identical(map_chr(result, "source"), c("new", "new"))
})

test_that("collection extra trafos use local names and deterministic translation", {
  left = ps(
    x = p_dbl(trafo = function(value) value + 1),
    .extra_trafo = function(x, param_set) {
      expect_identical(param_set, left)
      list(x = x$x * 2, bonus = x$x + 10)
    }
  )
  right = ps(y = p_int())
  collection = psc(left = left, right = right)
  result = collection$trafo(list(left.x = 1, right.y = 2L))
  expect_identical(result, list(
    right.y = 2L,
    left.x = 4,
    left.bonus = 12
  ))

  postfix = ParamSetCollection$new(list(unit = left), postfix_names = TRUE)
  expect_identical(
    postfix$trafo(list(x.unit = 1)),
    list(x.unit = 4, bonus.unit = 12)
  )
})

test_that("transformation outputs own shells while opaque leaves retain identity", {
  opaque = new.env(parent = emptyenv())
  parameter_set = ps(
    x = p_uty(trafo = identity),
    y = p_dbl(1, 10, logscale = TRUE)
  )
  input = list(x = opaque, y = 0)
  output = parameter_set$trafo(input)
  expect_identical(output, list(x = opaque, y = 1))
  names(output)[[1L]] = "changed"
  expect_named(input, c("x", "y"))
})
