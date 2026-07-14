test_that("native ParamSet trafo planning survives forced collection", {
  symbol = get(
    "C_param_set_trafo_plan",
    envir = asNamespace("paradox")
  )
  parameter_set = ps(
    x = p_dbl(trafo = function(value) c(value, value + 1)),
    y = p_int(trafo = function(value) as.integer(value + 2L)),
    .extra_trafo = function(x, param_set) {
      c(x, list(parameter_count = param_set$length))
    }
  )
  private = parameter_set$.__enclos_env__$private
  input = list(y = 3L, unknown = "kept", x = 0.5)

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  plan = .Call(symbol, input, private$.trafos)
  transformed = parameter_set$trafo(input)
  gctorture(previous)

  expect_identical(plan[[1L]], c("y", "x"))
  expect_true(all(vapply(plan[[2L]], is.function, logical(1L))))
  expect_identical(plan[[3L]], list(3L, 0.5))
  expect_identical(
    transformed,
    list(
      y = 5L,
      unknown = "kept",
      x = c(0.5, 1.5),
      parameter_count = 2L
    )
  )
})
