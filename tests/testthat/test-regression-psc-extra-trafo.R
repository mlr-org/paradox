test_that("ParamSetCollection invokes each child extra_trafo once", {
  calls = new.env(parent = emptyenv())
  calls$one_arg = 0L
  calls$two_args = 0L
  calls$param_set = NULL

  one_arg = ps(
    x = p_int(),
    .extra_trafo = function(x) {
      calls$one_arg = calls$one_arg + 1L
      list(x = x$x + 1L)
    }
  )
  two_args = ps(
    y = p_int(),
    .extra_trafo = function(x, param_set) {
      calls$two_args = calls$two_args + 1L
      calls$param_set = param_set
      list(y = x$y * 2L)
    }
  )
  collection = ParamSetCollection$new(list(one = one_arg, two = two_args))

  expect_equal(
    collection$trafo(list(one.x = 1L, two.y = 2L)),
    list(one.x = 2L, two.y = 4L)
  )
  expect_identical(calls$one_arg, 1L)
  expect_identical(calls$two_args, 1L)
  expect_identical(calls$param_set, two_args)
})
