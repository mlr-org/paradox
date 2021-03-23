context("to_tune")

test_that("TuneToken printers", {

  expect_output(print(to_tune()), "<entire parameter range>")

  expect_output(print(to_tune(1, 2)), "range \\[1, 2\\]")

  expect_output(print(to_tune(upper = 2, 1)), "range \\[1, 2\\]")

  expect_output(print(to_tune(1)), "p_fct\\(levels = \"1\"\\)")

  expect_output(print(to_tune(c("a", "b"))), "p_fct\\(levels = c\\(\"a\", \"b\"\\)\\)")

  expect_output(print(to_tune(p_dbl(1, 2))), "p_dbl\\(lower = 1, upper = 2\\)")

  expect_output(print(to_tune(ps(x = p_int(0, 1)))), "ParamSet.*id.*class.*lower.*upper")

})

test_that("validity checks", {

  expect_error(to_tune(p_dbl(2, 1)), "lower <= upper")

  expect_error(to_tune(p_dbl()), "must be bounded")

  expect_error(to_tune(ParamDbl$new("x")), "must be bounded")

  expect_error(to_tune(ps(x = p_dbl())), "must be bounded")

  expect_error(ParamSet$new(list(ParamInt$new("x")))$search_space(list(x = to_tune())), "must give a range for unbounded parameter")

})

test_that("$check() works on TuneToken", {

  pars = ParamSet$new(list(
    ParamInt$new("x", lower = 0, upper = 10),
    ParamInt$new("xub"),
    ParamDbl$new("y", lower = 0, upper = 10, special_vals = list("x")),
    ParamUty$new("uty"),
    ParamUty$new("uty1", custom_check = function(x) if (!identical(x, 1)) "uty1 must be 1" else TRUE),
    ParamFct$new("fct", c("x", "y")),
    ParamLgl$new("lgl")
  ))

  expect_equal(pars$check(list(x = 1, xub = 1, y = 1, uty = 1, uty1 = 1, fct = "x", lgl = TRUE)), TRUE)
  expect_string(pars$check(list(x = 1, xub = 1, y = 1, uty = 1, uty1 = 1, fct = "x", lgl = 1)),
    pattern = "Must be of type .*logical.*not.*double")


  expect_equal(pars$check(list(x = to_tune(), xub = to_tune(0, 1), y = to_tune(p_int(1, 10)),
    uty = to_tune(c("a", "b")), uty1 = to_tune(1), fct = to_tune(), lgl = to_tune(c(TRUE, FALSE)))), TRUE)

  expect_class(pars$search_space(list(x = to_tune(), xub = to_tune(0, 1), y = to_tune(p_int(1, 10)),
    uty = to_tune(c("a", "b")), uty1 = to_tune(1), fct = to_tune(), lgl = to_tune(c(TRUE, FALSE)))), "ParamSet")


  expect_string(pars$check(list(x = to_tune(0, 11))), "not compatible with param x.*upper.*11")
  expect_error(pars$search_space(list(x = to_tune(0, 11))), "not compatible with param x.*upper.*11")

  expect_string(pars$check(list(xub = to_tune())), "must give a range for unbounded parameter xub")
  expect_error(pars$search_space(list(xub = to_tune())), "must give a range for unbounded parameter xub")

  expect_equal(pars$check(list(y = to_tune(list(1, 2, "x")))), TRUE)
  expect_string(pars$check(list(y = to_tune(list(1, 2, "z")))), "y: tune token invalid.*\"z\"")
  expect_error(pars$search_space(list(y = to_tune(list(1, 2, "z")))), "generates points that are not compatible.*\"z\"")

  expect_string(pars$check(list(uty = to_tune())), "must give a range for unbounded parameter uty")
  expect_error(pars$search_space(list(uty = to_tune())), "must give a range for unbounded parameter uty")
  expect_equal(pars$check(list(uty = to_tune(1))), TRUE)
  expect_equal(pars$check(list(uty1 = to_tune(1))), TRUE)
  expect_string(pars$check(list(uty1 = to_tune(2))), "not compatible with param uty1.*2")
  expect_error(pars$search_space(list(uty1 = to_tune(2))), "not compatible with param uty1.*2")

  expect_string(pars$check(list(fct = to_tune("z"))), "not compatible with param fct.*\"z\"")
  expect_error(pars$search_space(list(fct = to_tune("z"))), "not compatible with param fct.*\"z\"")

  expect_string(pars$check(list(lgl = to_tune("z"))), "not compatible with param lgl.*\"z\"")
  expect_error(pars$search_space(list(lgl = to_tune("z"))), "not compatible with param lgl.*\"z\"")

  expect_string(pars$check(list(lgl = to_tune(0, 1))), "must have zero or one argument")
  expect_error(pars$search_space(list(lgl = to_tune(0, 1))), "must have zero or one argument")

  expect_error(pars$search_space(list(xxx = to_tune())), "Must be a subset of .*x,xub,y,uty,uty1,fct,lgl")

})

test_that("Tune ParamSet is created", {

  pars = ParamSet$new(list(
    ParamInt$new("x", lower = 0, upper = 10),
    ParamDbl$new("y", lower = 0, upper = 10, special_vals = list("x")),
    ParamFct$new("fct", c("x", "y")),
    ParamLgl$new("lgl")
  ))

  pars_tune = pars$search_space(list(x = to_tune(), y = to_tune(), fct = to_tune(), lgl = to_tune()))

  # the following is necessary because $add modifies index of the .deps table, and one of the `$values` is not named.
  pars$add(ParamSet$new())
  pars$values = list()
  setindex(pars_tune$deps, NULL)
  pars_tune$values = list()

  expect_equal_ps(pars, pars_tune)

  pars_unbound = ParamSet$new(list(
    ParamDbl$new("x"),
    ParamDbl$new("y", special_vals = list("x")),
    ParamFct$new("fct", letters),
    ParamUty$new("lgl")
  ))

  pars_tune = pars_unbound$search_space(list(x = to_tune(p_int(0, 10)), y = to_tune(p_dbl(0, 10, special_vals = list("x"))), fct = to_tune(c("x", "y")), lgl = to_tune(p_lgl())))
  pars_tune$values = list()
  setindex(pars_tune$deps, NULL)
  expect_equal_ps(pars, pars_tune)

  pars_unbound_2 = ParamSet$new(list(
    ParamInt$new("x"),
    ParamDbl$new("y"),
    ParamFct$new("fct", letters),
    ParamUty$new("lgl")
  ))

  pars_tune = pars_unbound_2$search_space(list(x = to_tune(0, 10), y = to_tune(p_dbl(0, 10, special_vals = list("x"))), fct = to_tune(c("x", "y")), lgl = to_tune(p_lgl())))
  pars_tune$values = list()
  setindex(pars_tune$deps, NULL)
  expect_equal_ps(pars, pars_tune)

  pars_tune = pars_unbound$search_space(list(x = to_tune(ParamInt$new("z", 0, 10)), y = to_tune(ps(z = p_dbl(0, 10, special_vals = list("x")))),
    fct = to_tune(c("x", "y")), lgl = to_tune(p_lgl())))

  # to_tune from ps() generates messed up value order
  expect_equal(rbindlist(generate_design_grid(pars_tune, 2)$transpose()), rbindlist(generate_design_grid(pars, 2)$transpose()), ignore.col.order = TRUE)

  pars$values = list(x = 1, y = 2, fct = to_tune(), lgl = to_tune(TRUE))
  pars_tune = pars$search_space()
  expect_equal(generate_design_grid(pars_tune)$transpose(),
    list(list(fct = "x", lgl = TRUE), list(fct = "y", lgl = TRUE)))

})


test_that("Trafo works as expected", {

  pars = ParamSet$new(list(
    ParamInt$new("x", lower = 0, upper = 10),
    ParamDbl$new("y", lower = 0, upper = 10, special_vals = list("x")),
    ParamFct$new("fct", c("x", "y")),
    ParamLgl$new("lgl")
  ))

  grid = generate_design_grid(pars$search_space(list(lgl = to_tune(TRUE))))
  expect_equal(grid$data, data.table(lgl = "TRUE"))
  expect_equal(grid$transpose(), list(list(lgl = TRUE)))

  grid = generate_design_grid(pars$search_space(list(lgl = to_tune(p_fct(TRUE, trafo = function(x) !x)))))
  expect_equal(grid$data, data.table(lgl = "TRUE"))
  expect_equal(grid$transpose(), list(list(lgl = FALSE)))

  inpars = ParamSet$new(list(
    ParamFct$new("x", c("a", "b")),
    ParamFct$new("y", c("a", "b"))
  ))
  inpars$trafo = function(x, param_set) list(x$x != x$y)
  indesign = generate_design_grid(inpars)

  outdesign = generate_design_grid(pars$search_space(list(lgl = to_tune(inpars))))

  expect_equal(indesign$data, outdesign$data)

  expect_equal(outdesign$transpose(), list(list(lgl = FALSE), list(lgl = TRUE), list(lgl = TRUE), list(lgl = FALSE)))

})

test_that("Dependencies work", {

  pars = ParamSet$new(list(
    ParamInt$new("x", lower = 0, upper = 1),
    ParamInt$new("y", lower = 0, upper = 1),
    ParamInt$new("z", lower = 0, upper = 1),
    ParamInt$new("z2", lower = 0, upper = 1)
  ))

  pars$add_dep("x", "y", CondEqual$new(1))
  pars$add_dep("x", "z", CondEqual$new(1))
  pars$add_dep("z2", "x", CondEqual$new(1))

  # if the tune paramset contains a "z2" in place of "y2", then the x->"z2" dependency is actually kept. This may be useful.
  # if some parameter depends on y, that dependency is lost. nothing we can do here.

  tuneps = pars$search_space(list(x = to_tune(), y = to_tune(ps(y1 = p_int(0, 1), y2 = p_int(0, 1), .extra_trafo = function(x, param_set) list(abs(x$y1 - x$y2))))))

  # all dependencies lost
  expect_equal(nrow(tuneps$deps), 0)

  pars = ParamSet$new(list(
    ParamInt$new("x", lower = 0, upper = 1),
    ParamInt$new("y", lower = 0, upper = 1),
    ParamInt$new("z", lower = 0, upper = 1),
    ParamInt$new("z2", lower = 0, upper = 1)
  ))
  pars$add_dep("y", "x", CondEqual$new(1))
  pars$add_dep("x", "z", CondEqual$new(1))
  pars$add_dep("z2", "x", CondEqual$new(1))

  # only the relevant dependency is kept
  tuneps = pars$search_space(list(x = to_tune(), y = to_tune()))
  expect_equal(setindex(tuneps$deps, NULL), data.table(id = "y", on = "x", cond = list(CondEqual$new(1))))

  #dependencies are kept between params, even if the dependor is trafo'd from other params
  tuneps = pars$search_space(list(x = to_tune(), y = to_tune(ps(y1 = p_int(0, 1), y2 = p_int(0, 1), .extra_trafo = function(x, param_set) list(abs(x$y1 - x$y2))))))

  expect_equal(setindex(tuneps$deps, NULL), data.table(id = c("y1", "y2"), on = c("x", "x"), cond = list(CondEqual$new(1))))

  tuneps = pars$search_space(list(x = to_tune(), y = to_tune(ps(y1 = p_int(0, 1), y2 = p_int(0, 1, depends = y1 == 1),
    .extra_trafo = function(x, param_set) list(min(x$y1, x$y2, na.rm = TRUE))))))

  # mixing dependencies from inside and outside
  expect_equal(setindex(tuneps$deps, NULL), data.table(id = c("y2", "y1", "y2"), on = c("y1", "x", "x"), cond = list(CondEqual$new(1))))


  parsnodep = ParamSet$new(list(
    ParamInt$new("x", lower = 0, upper = 1),
    ParamInt$new("y", lower = 0, upper = 1),
    ParamInt$new("z", lower = 0, upper = 1),
    ParamInt$new("z2", lower = 0, upper = 1)
  ))

  # amazing: dependencies across to_tune
  tuneps = parsnodep$search_space(list(x = to_tune(p_int(0, 1, depends = y == 0)), y = to_tune()))

  expect_equal(setindex(tuneps$deps, NULL), data.table(id = "x", on = "y", cond = list(CondEqual$new(0))))


  tuneps = pars$search_space(list(x = to_tune(), y = to_tune(ps(y1 = p_int(0, 1), y2 = p_int(0, 1, depends = x == 1),
    .extra_trafo = function(x, param_set) list(min(x$y1, x$y2, na.rm = TRUE)), .allow_dangling_dependencies = TRUE))))

  # mixing dependencies from inside and across to_tune. I am the only person in this building capable of coding this.
  expect_equal(setindex(tuneps$deps, NULL), data.table(id = c("y2", "y1", "y2"), on = c("x", "x", "x"), cond = list(CondEqual$new(1))))

  expect_error(pars$search_space(list(x = to_tune(), y = to_tune(ps(y1 = p_int(0, 1), y2 = p_int(0, 1, depends = z == 1),
    .extra_trafo = function(x, param_set) list(min(x$y1, x$y2, na.rm = TRUE)), .allow_dangling_dependencies = TRUE)))),
    "Dependencies on z dangling")

  # make sure dependency check of `pars` work with tune tokens.
  pars$values$z = 0
  pars$values$x = to_tune()
  expect_equal(capture.output(print(pars$search_space())), capture.output(print(ps(x = p_int(0, 1)))))

  pars$values$z = to_tune(p_int(0, 1))
  pars$values$x = to_tune()
  expect_equal(capture.output(print(pars$search_space())),
    capture.output(print(ps(z = p_int(0, 1), x = p_int(0, 1, depends = z == 1)))))

  pars$values$z = to_tune(p_int(0, 1))
  pars$values$x = 1

  expect_equal(capture.output(print(pars$search_space())), capture.output(print(ps(z = p_int(0, 1)))))

  # dependency after subsetting factorials works, even if the dependency now
  # contains infeasible values
  largeps = ParamSet$new(list(
    ParamFct$new("x", c("a", "b", "c")),
    ParamLgl$new("y")
  ))
  largeps$add_dep("y", "x", CondAnyOf$new(c("a", "b")))

  res = largeps$search_space(list(x = to_tune(c("a", "b")), y = to_tune()))
  expect_equal(res$deps$cond[[1]]$rhs, c("a", "b"))

  res = largeps$search_space(list(x = to_tune("a"), y = to_tune()))
  expect_equal(res$deps$cond[[1]]$rhs, "a")

  res = largeps$search_space(list(x = to_tune("c"), y = to_tune()))
  expect_false(res$has_deps)

})

test_that("ParamSetCollection works", {

  ps1 = ParamSet$new(list(ParamInt$new("x"), ParamInt$new("y")))
  ps2 = ParamSet$new(list(ParamInt$new("a")))

  ps1$set_id = "prefix"

  psc = ParamSetCollection$new(list(ps1, ps2))

  ps1$values$x = to_tune(0, 10)
  ps1$values$y = to_tune(ps(y1 = p_int(0, 1), y2 = p_int(0, 1), .extra_trafo = function(x, param_set) list(y = x$y1 * x$y2)))

  expect_equal(generate_design_grid(ps1$search_space(), 2)$transpose()[[1]], list(x = 0, y = 0))
  expect_equal(generate_design_grid(ps1$search_space(), 2)$transpose(trafo = FALSE)[[1]], list(x = 0, y1 = 0, y2 = 0))

  expect_equal(generate_design_grid(psc$search_space(), 2)$transpose()[[1]], list(prefix.x = 0, prefix.y = 0))

  expect_equal(generate_design_grid(psc$search_space(), 2)$transpose(trafo = FALSE)[[1]], list(prefix.x = 0, y1 = 0, y2 = 0))

  psc$values$a = 1
  psc$values$a = to_tune(0, 1)

  expect_equal(generate_design_grid(psc$search_space(), 2)$transpose(trafo = FALSE)[[1]], list(prefix.x = 0, y1 = 0, y2 = 0, a = 0))

  psc$values$a = to_tune(p_int(0, 1, depends = prefix.x == 10))

  expect_equal(generate_design_grid(psc$search_space(), 2)$transpose(trafo = FALSE)[[1]], list(prefix.x = 0, y1 = 0, y2 = 0))
  expect_equal(generate_design_grid(psc$search_space(), 2)$transpose(trafo = FALSE)[[12]], list(prefix.x = 10, y1 = 1, y2 = 1, a = 1))

})

test_that("ParamSet$get_values() works", {
  pars = ParamSet$new(list(
    ParamInt$new("x", lower = 0, upper = 10),
    ParamDbl$new("y", lower = 0, upper = 10),
    ParamDbl$new("z", lower = 0, upper = 10)
  ))

  pars$values$x = to_tune()
  pars$values$y = 2
  pars$values$z = 2

  expect_named(pars$get_values(type = "with_token"), c("x", "y", "z"))
  expect_named(pars$get_values(type = "without_token"), c("y", "z"))
  expect_named(pars$get_values(type = "only_token"), "x")

  pars = ParamSet$new(list(
    ParamInt$new("x", lower = 0, upper = 10),
    ParamDbl$new("y", lower = 0, upper = 10),
    ParamDbl$new("z", lower = 0, upper = 10)
  ))

  pars$values$y = 2
  expect_list(pars$get_values(type = "only_token"), len = 0)

  pars$values$y = to_tune()
  expect_list(pars$get_values(type = "without_token"), len = 0)
})

test_that("partial bounds in tunetoken", {

  pars = ParamSet$new(list(
    ParamInt$new("x", lower = 0, upper = 10),
    ParamDbl$new("y", lower = 0),
    ParamDbl$new("z", upper = 10)
  ))

  expect_equal(pars$search_space(list(x = to_tune()))$params[[1]], ParamInt$new("x", lower = 0, upper = 10))

  expect_equal(pars$search_space(list(x = to_tune(lower = 1)))$params[[1]], ParamInt$new("x", lower = 1, upper = 10))
  expect_equal(pars$search_space(list(x = to_tune(upper = 1)))$params[[1]], ParamInt$new("x", lower = 0, upper = 1))
  expect_equal(pars$search_space(list(x = to_tune(lower = 1, upper = 2)))$params[[1]], ParamInt$new("x", lower = 1, upper = 2))

  expect_error(pars$search_space(list(y = to_tune(lower = 1))), "y range must be bounded, but is \\[1, Inf\\]")
  expect_equal(pars$search_space(list(y = to_tune(upper = 1)))$params[[1]], ParamDbl$new("y", lower = 0, upper = 1))
  expect_equal(pars$search_space(list(y = to_tune(lower = 1, upper = 2)))$params[[1]], ParamDbl$new("y", lower = 1, upper = 2))

  expect_error(pars$search_space(list(z = to_tune(upper = 1))), "z range must be bounded, but is \\[-Inf, 1\\]")
  expect_equal(pars$search_space(list(z = to_tune(lower = 1)))$params[[1]], ParamDbl$new("z", lower = 1, upper = 10))
  expect_equal(pars$search_space(list(z = to_tune(lower = 1, upper = 2)))$params[[1]], ParamDbl$new("z", lower = 1, upper = 2))

  expect_output(print(to_tune()), "entire parameter range")
  expect_output(print(to_tune(lower = 1)), "range \\[1, \\.\\.\\.]")
  expect_output(print(to_tune(upper = 1)), "range \\[\\.\\.\\., 1]")
  expect_output(print(to_tune(lower = 0, upper = 1)), "range \\[0, 1]")

  expect_output(print(to_tune(logscale = FALSE)), "<entire parameter range>")
  expect_output(print(to_tune(lower = 1, logscale = FALSE)), "range \\[1, \\.\\.\\.]\\n$")
  expect_output(print(to_tune(upper = 1, logscale = FALSE)), "range \\[\\.\\.\\., 1]\\n$")
  expect_output(print(to_tune(lower = 0, upper = 1, logscale = FALSE)), "range \\[0, 1]\\n$")

})

test_that("logscale in tunetoken", {

  pars = ParamSet$new(list(
    ParamInt$new("x", lower = 0, upper = 10),
    ParamDbl$new("y", lower = 0)
  ))

  expect_equal(pars$search_space(list(x = to_tune(logscale = TRUE)))$params[[1]], ParamDbl$new("x", lower = log(.5), upper = log(11)))
  expect_equal(pars$search_space(list(y = to_tune(lower = 1, upper = 10, logscale = TRUE)))$params[[1]], ParamDbl$new("y", lower = log(1), upper = log(10)))

  expect_error(pars$search_space(list(y = to_tune(upper = 10, logscale = TRUE))), "When logscale is TRUE then lower bound must be strictly greater than 0")

  expect_equal(
    generate_design_grid(pars$search_space(list(x = to_tune(logscale = TRUE))), 4)$transpose(),
    list(list(x = 0), list(x = 1), list(x = 3), list(x = 10))
  )

  expect_equal(
    generate_design_grid(pars$search_space(list(y = to_tune(lower = 1, upper = 100, logscale = TRUE))), 3)$transpose(),
    list(list(y = 1), list(y = 10), list(y = 100))
  )

  expect_output(print(to_tune(logscale = TRUE)), "entire parameter range \\(log scale\\)")
  expect_output(print(to_tune(lower = 1, logscale = TRUE)), "range \\[1, \\.\\.\\.] \\(log scale\\)")
  expect_output(print(to_tune(upper = 1, logscale = TRUE)), "range \\[\\.\\.\\., 1] \\(log scale\\)")
  expect_output(print(to_tune(lower = 0, upper = 1, logscale = TRUE)), "range \\[0, 1] \\(log scale\\)")

})
