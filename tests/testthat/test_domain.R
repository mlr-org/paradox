context("domain")

test_that("p_xxx printers", {
  expect_output(print(p_int()), "p_int\\(\\)")
  expect_output(print(p_int(1)), "p_int\\(lower = 1\\)")
  expect_output(print(p_int(lower = 1)), "p_int\\(lower = 1\\)")

  expect_output(print(p_dbl()), "p_dbl\\(\\)")
  expect_output(print(p_dbl(1)), "p_dbl\\(lower = 1\\)")
  expect_output(print(p_dbl(lower = 1)), "p_dbl\\(lower = 1\\)")

  expect_output(print(p_lgl()), "p_lgl\\(\\)")
  expect_output(print(p_uty()), "p_uty\\(\\)")

  expect_output(print(p_fct("a")), "p_fct\\(levels = \"a\"\\)")
  expect_output(print(p_fct(1)), "p_fct\\(levels = \"1\"\\)")
  expect_output(print(p_fct(list(x = 1))), "p_fct\\(levels = \"x\"\\)")

  expect_output(print(p_fct(list(x = 1), requires = x == 1)), "p_fct\\(levels = \"x\"\\, requires = x == 1)")
  reqquote = quote(x == 1)
  expect_output(print(p_fct(list(x = 1), requires = reqquote)), "p_fct\\(levels = \"x\"\\, requires = x == 1)")
})

test_that("ps(p_xxx(...)) creates ParamSets", {

  expect_equal(ps(x = p_int()), ParamSet$new(list(ParamInt$new("x"))))
  expect_equal(ps(x = p_dbl()), ParamSet$new(list(ParamDbl$new("x"))))
  expect_equal(ps(x = p_uty()), ParamSet$new(list(ParamUty$new("x"))))
  expect_equal(ps(x = p_lgl()), ParamSet$new(list(ParamLgl$new("x"))))
  expect_equal(ps(x = p_fct(letters)), ParamSet$new(list(ParamFct$new("x", letters))))

  expect_equal(ps(x = p_int(upper = 1, lower = 0)), ParamSet$new(list(ParamInt$new("x", 0, 1))))
  expect_equal(ps(x = p_dbl(upper = 1, lower = 0)), ParamSet$new(list(ParamDbl$new("x", 0, 1))))

  expect_equal(ps(x = p_int(special_vals = list("x"), default = 0, tags = "required")),
    ParamSet$new(list(ParamInt$new("x", special_vals = list("x"), default = 0, tags = "required"))))
  expect_equal(ps(x = p_dbl(special_vals = list("x"), default = 0, tags = "required")),
    ParamSet$new(list(ParamDbl$new("x", special_vals = list("x"), default = 0, tags = "required"))))

  expect_equal(ps(x = p_lgl(special_vals = list("x"), default = TRUE, tags = "required")),
    ParamSet$new(list(ParamLgl$new("x", special_vals = list("x"), default = TRUE, tags = "required"))))

  expect_equal(ps(x = p_fct(letters, special_vals = list(0), default = 0, tags = "required")),
    ParamSet$new(list(ParamFct$new("x", letters, special_vals = list(0), default = 0, tags = "required"))))

  expect_equal(ps(x = p_uty(default = 1, tags = "required", custom_check = check_int)),
    ParamSet$new(list(ParamUty$new("x", default = 1, tags = "required", custom_check = check_int))))

  expect_error(ps(x = p_int(), x = p_int()), "unique names")

  expect_equal(ps(x = p_uty(default = 1, tags = "required", custom_check = check_int)),
    ps(x = ParamUty$new("y", default = 1, tags = "required", custom_check = check_int)))

  expect_error(p_int(id = 1), "unused argument.*id")

})

test_that("p_fct autotrafo", {

  expect_equal(generate_design_grid(ps(x = p_fct(c("a", "b", "c"))))$transpose(),
    list(list(x = "a"), list(x = "b"), list(x = "c")))

  expect_equal(generate_design_grid(ps(x = p_fct(c(1, 2, 2.5))))$transpose(),
    list(list(x = 1), list(x = 2), list(x = 2.5)))

  expect_equal(generate_design_grid(ps(x = p_fct(c(1, 2, 2.5))))$transpose(trafo = FALSE),
    list(list(x = "1"), list(x = "2"), list(x = "2.5")))

  expect_equal(generate_design_grid(ps(x = p_fct(c(a = 1, b = 2, c = 2.5))))$transpose(),
    list(list(x = 1), list(x = 2), list(x = 2.5)))

  expect_equal(generate_design_grid(ps(x = p_fct(c(a = 1, b = 2, c = 2.5))))$transpose(trafo = FALSE),
    list(list(x = "a"), list(x = "b"), list(x = "c")))

  expect_equal(generate_design_grid(ps(x = p_fct(list(a = log, b = exp, c = identity))))$transpose(),
    list(list(x = log), list(x = exp), list(x = identity)))

  expect_equal(generate_design_grid(ps(x = p_fct(list(a = log, b = exp, c = identity))))$transpose(trafo = FALSE),
    list(list(x = "a"), list(x = "b"), list(x = "c")))


  expect_equal(generate_design_grid(ps(x = p_fct(c(a = 1, b = 2, c = 2.5), trafo = function(x) x^2)))$transpose(),
    list(list(x = 1), list(x = 4), list(x = 100 / 16)))

  expect_equal(generate_design_grid(ps(x = p_fct(c(a = 1, b = 2, c = 2.5), trafo = function(x) x^2),
    .extra_trafo = function(x, param_set) list(y = exp(x$x))))$transpose(),
    list(list(y = exp(1)), list(y = exp(4)), list(y = exp(100 /16))))

})

test_that("requirements in domains", {

  simpleps = ParamSet$new(list(ParamInt$new("x"), ParamDbl$new("y")))$add_dep("y", "x", CondEqual$new(1))

  # basic equality expression
  expect_equal(
    ps(
      x = p_int(),
      y = p_dbl(requires = x == 1)
    ), simpleps)

  # using a expression variable
  reqquote = quote(x == 1)
  expect_equal(
    ps(
      x = p_int(),
      y = p_dbl(requires = reqquote)
    ), simpleps)

  # the same for `p_fct`, which behaves slightly differently from the rest
  simpleps = ParamSet$new(list(ParamInt$new("x"), ParamFct$new("y", letters)))$add_dep("y", "x", CondEqual$new(1))
  expect_equal(
    ps(
      x = p_int(),
      y = p_fct(letters, requires = x == 1)
    ), simpleps)
  reqquote = quote(x == 1)
  expect_equal(
    ps(
      x = p_int(),
      y = p_fct(letters, requires = reqquote)
    ), simpleps)

  # the same for `p_fct` involving autotrafo, which behaves slightly differently from the rest
  simpleps = ps(x = p_int(), y = p_fct(list(a = 1, b = 2)))$add_dep("y", "x", CondEqual$new(1))
  expect_equal(
    ps(
      x = p_int(),
      y = p_fct(list(a = 1, b = 2), requires = x == 1)
    ), simpleps)
  reqquote = quote(x == 1)
  expect_equal(
    ps(
      x = p_int(),
      y = p_fct(list(a = 1, b = 2), requires = reqquote)
    ), simpleps)

  # `&&`
  expect_equal(
    ps(
      x = p_int(),
      y = p_dbl(requires = x == 1 && x == 3)
    ),
    ParamSet$new(list(ParamInt$new("x"), ParamDbl$new("y")))$add_dep("y", "x", CondEqual$new(1))$add_dep("y", "x", CondEqual$new(3)))

  # `&&`, `%in%`
  expect_equal(
    ps(
      x = p_int(),
      z = p_fct(letters[1:3]),
      y = p_dbl(requires = x == 1 && z %in% c("a", "b"))
    ),
    ParamSet$new(list(ParamInt$new("x"), ParamFct$new("z", letters[1:3]), ParamDbl$new("y")))$
      add_dep("y", "x", CondEqual$new(1))$add_dep("y", "z", CondAnyOf$new(c("a", "b"))))

  # recursive dependencies
  expect_equal(
    ps(
      x = p_int(),
      z = p_fct(letters[1:3], requires = x == 2),
      y = p_dbl(requires = x == 1 && z %in% c("a", "b"))
    ),
    ParamSet$new(list(ParamInt$new("x"), ParamFct$new("z", letters[1:3]), ParamDbl$new("y")))$
      add_dep("z", "x", CondEqual$new(2))$add_dep("y", "x", CondEqual$new(1))$add_dep("y", "z", CondAnyOf$new(c("a", "b"))))

  # `fct` with complex requirements
  complexps = ParamSet$new(list(ParamInt$new("x"), ParamFct$new("z", letters[1:3]), ParamFct$new("y", letters[1:3])))$
      add_dep("y", "x", CondEqual$new(1))$add_dep("y", "z", CondAnyOf$new(c("a", "b")))
  expect_equal(
    ps(
      x = p_int(),
      z = p_fct(letters[1:3]),
      y = p_fct(letters[1:3], requires = x == 1 && z %in% c("a", "b"))
    ), complexps)

  # parentheses are ignored
  expect_equal(
    ps(
      x = p_int(),
      z = p_fct(letters[1:3]),
      y = p_fct(letters[1:3], requires = ((((x == 1)) && (z %in% c("a", "b")))))
    ), complexps)

  # multiple dependencies on the same value
  expect_equal(
    ps(
      x = p_int(),
      z = p_fct(letters[1:3]),
      y = p_fct(letters[1:3], requires = ((((x == 1)) && (z %in% c("a", "b") && z == "a"))))
    ),
    ParamSet$new(list(ParamInt$new("x"), ParamFct$new("z", letters[1:3]), ParamFct$new("y", letters[1:3])))$
      add_dep("y", "x", CondEqual$new(1))$add_dep("y", "z", CondAnyOf$new(c("a", "b")))$
      add_dep("y", "z", CondEqual$new("a")))

  expect_error(p_int(requires = 1 == x), "must be a parameter name")
  expect_error(p_int(requires = 1), "must be an expression")
  expect_error(p_int(requires = x != 1), "is broken")

})


test_that("trafos in domains", {

  expect_equal(ps(x = p_int(trafo = exp))$trafo(list(x = 2)), list(x = exp(2)))

  expect_equal(ps(x = p_int(trafo = function(x) list()))$trafo(list(x = 2)), list(x = list()))

  expect_equal(ps(x = p_int(trafo = exp), y = p_dbl())$trafo(list(x = 2, y = 3)), list(x = exp(2), y = 3))

  expect_equal(ps(x = p_int(trafo = exp))$trafo(list(x = 2, y = 3)), list(x = exp(2), y = 3))

  expect_equal(ps(x = p_int(trafo = exp), y = p_dbl(), z = p_dbl(), .extra_trafo = function(x, param_set) {
    x$x = sqrt(x$x)
    x$y = sqrt(x$y)
    x$n = 1
    x
  })$trafo(list(x = 2, y = 3, z = 4)), list(x = exp(1), y = sqrt(3), z = 4, n = 1))

  expect_equal(ps(x = p_int(trafo = exp), y = p_fct(c(1, 2, 3)), z = p_dbl(), .extra_trafo = function(x, param_set) {
    x$x = sqrt(x$x)
    x$y = sqrt(x$y)
    x$n = 1
    x
  })$trafo(list(x = 2, y = "3", z = 4)), list(x = exp(1), y = sqrt(3), z = 4, n = 1))

  expect_equal(ps(x = p_int(trafo = exp), y = p_fct(list(`alpha` = 1, `beta` = 2, `gamma` = 3)), z = p_dbl(), .extra_trafo = function(x, param_set) {
    x$x = sqrt(x$x)
    x$y = sqrt(x$y)
    x$n = 1
    x
  })$trafo(list(x = 2, y = "gamma", z = 4)), list(x = exp(1), y = sqrt(3), z = 4, n = 1))

  expect_equal(ps(x = p_int(trafo = exp), y = p_fct(list(`alpha` = 1, `beta` = 2, `gamma` = 3), trafo = sqrt), z = p_dbl(), .extra_trafo = function(x, param_set) {
    x$x = sqrt(x$x)
    x$y = 2 * x$y
    x$n = 1
    x
  })$trafo(list(x = 2, y = "gamma", z = 4)), list(x = exp(1), y = 2 * sqrt(3), z = 4, n = 1))

})