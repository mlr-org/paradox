test_that("ParamSet trafo accepts named lists and data frames", {
  parameter_set = ps(
    a = p_dbl(trafo = function(value) value + 1),
    b = p_int(trafo = function(value) as.integer(value * 2)),
    untouched = p_uty()
  )
  input = data.frame(a = 1:3, b = 4:6)
  output = parameter_set$trafo(input)
  expect_identical(output, list(
    a = c(2, 3, 4),
    b = c(8L, 10L, 12L)
  ))
  expect_identical(input$a, 1:3)
  expect_identical(input$b, 4:6)

  expect_identical(parameter_set$trafo(list(other = 1)), list(other = 1))
  expect_identical(parameter_set$trafo(list()), setNames(list(), character()))
  expect_error(parameter_set$trafo(unname(list(1))), "name")
  expect_error(parameter_set$trafo(setNames(list(1), NA_character_)), "name")
  expect_error(parameter_set$trafo(setNames(list(1, 2), c("x", "x"))), "unique")
})

test_that("individual trafo callbacks preserve lazy results and RNG order", {
  delayed = ps(x = p_dbl(trafo = function(arg) function() arg))
  delayed_result = delayed$trafo(list(x = 7))$x
  expect_true(is.function(delayed_result))
  expect_identical(delayed_result(), 7)

  events = character()
  callback = function(id) {
    force(id)
    function(value) {
      events <<- c(events, id)
      value + stats::runif(1L)
    }
  }
  parameter_set = ps(
    a = p_dbl(trafo = callback("a")),
    b = p_dbl(trafo = callback("b")),
    c = p_dbl(trafo = callback("c"))
  )
  set.seed(921L)
  expected = stats::runif(3L)
  set.seed(921L)
  result = parameter_set$trafo(list(c = 3, a = 1, b = 2))
  expect_identical(events, c("c", "a", "b"))
  expect_equal(unname(unlist(result)), c(3, 1, 2) + expected)
})

test_that("a ParamSet transformation snapshots individual and extra callbacks", {
  events = character()
  holder = NULL
  replacement_extra = function(x) {
    events <<- c(events, "new-extra")
    c(x, list(extra = "new"))
  }
  old_extra = function(x) {
    events <<- c(events, "old-extra")
    c(x, list(extra = "old"))
  }
  first = function(value) {
    events <<- c(events, "first")
    holder$extra_trafo = replacement_extra
    value
  }
  holder = ps(
    first = p_dbl(trafo = first),
    second = p_dbl(trafo = function(value) {
      events <<- c(events, "second")
      value
    }),
    .extra_trafo = old_extra
  )

  result = holder$trafo(list(first = 1, second = 2))
  expect_identical(result$extra, "old")
  expect_identical(events, c("first", "second", "old-extra"))
  events = character()
  expect_identical(
    holder$trafo(list(first = 1, second = 2))$extra,
    "new"
  )
})

test_that("Shadow transformation state is live at operation entry", {
  origin = ps(
    visible = p_dbl(trafo = function(value) value + 1),
    hidden = p_int(),
    .extra_trafo = function(x, param_set) {
      expect_s3_class(param_set, "ParamSetShadow")
      c(x, list(marker = "first"))
    }
  )
  shadow = ParamSetShadow$new(origin, "hidden")
  expect_identical(
    shadow$trafo(list(visible = 1)),
    list(visible = 2, marker = "first")
  )
  origin$extra_trafo = function(x) c(x, list(marker = "second"))
  expect_identical(
    shadow$trafo(list(visible = 1)),
    list(visible = 2, marker = "second")
  )
})
