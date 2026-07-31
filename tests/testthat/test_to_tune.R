context("to_tune")

test_that("TuneToken printers", {

  expect_output(print(to_tune()), "<entire parameter range>")

  expect_output(print(to_tune(1, 2)), "range \\[1, 2\\]")

  expect_output(print(to_tune(upper = 2, 1)), "range \\[1, 2\\]")

  expect_output(print(to_tune(1)), "p_fct\\(levels = c\\(`1` = 1\\)\\)")

  expect_output(print(to_tune(c("a", "b"))), "p_fct\\(levels = c\\(\"a\", \"b\"\\)\\)")

  expect_output(print(to_tune(p_dbl(1, 2))), "p_dbl\\(lower = 1, upper = 2\\)")

  expect_output(print(to_tune(ps(x = p_int(0, 1)))), "ParamSet.*id.*class.*lower.*upper")

})

test_that("validity checks", {

  expect_error(to_tune(p_dbl(2, 1)), "must not be greater than")

  expect_error(to_tune(p_dbl()), "must be bounded")

  unbounded_param_set = to_tune(ParamDbl$new("x"))
  expect_s3_class(unbounded_param_set, "ObjectTuneToken")
  target = ps(value = p_uty())
  expect_error(
    target$values$value <- unbounded_param_set,
    "nonempty and bounded"
  )

  expect_s3_class(to_tune(ps(x = p_dbl())), "ObjectTuneToken")

  expect_error(ParamSet_legacy$new(list(ParamInt$new("x")))$search_space(list(x = to_tune())), "must give a range for unbounded parameter")

})

test_that("$check() works on TuneToken", {

  pars = ParamSet_legacy$new(list(
    ParamInt$new("x", lower = 0, upper = 10),
    ParamInt$new("xub"),
    ParamDbl$new("y", lower = 0, upper = 10, special_vals = list("x")),
    ParamUty$new("uty"),
    ParamUty$new("uty1", custom_check = function(x) if (!identical(x, 1)) "uty1 must be 1" else TRUE),
    ParamFct$new("fct", c("x", "y")),
    ParamLgl$new("lgl")
  ))

  expect_equal(pars$check(list(x = 1, xub = 1, y = 1, uty = 1, uty1 = 1, fct = "x", lgl = TRUE)), TRUE)
  expect_identical(
    pars$check(list(x = 1, xub = 1, y = 1, uty = 1, uty1 = 1, fct = "x", lgl = 1)),
    "lgl: Must be of type 'logical flag', not 'double'"
  )


  expect_equal(pars$check(list(x = to_tune(), xub = to_tune(0, 1), y = to_tune(p_int(1, 10)),
    uty = to_tune(c("a", "b")), uty1 = to_tune(1), fct = to_tune(), lgl = to_tune(c(TRUE, FALSE)))), TRUE)

  expect_class(pars$search_space(list(x = to_tune(), xub = to_tune(0, 1), y = to_tune(p_int(1, 10)),
    uty = to_tune(c("a", "b")), uty1 = to_tune(1), fct = to_tune(), lgl = to_tune(c(TRUE, FALSE)))), "ParamSet")


  expect_string(pars$check(list(x = to_tune(0, 11))), "not compatible with param x.*upper.*11")
  expect_error(pars$search_space(list(x = to_tune(0, 11))), "not compatible with param x.*upper.*11")

  expect_string(pars$check(list(xub = to_tune())), "must give a range for unbounded parameter xub")
  expect_error(pars$search_space(list(xub = to_tune())), "must give a range for unbounded parameter xub")

  expect_equal(pars$check(list(y = to_tune(list(1, 2, "x")))), TRUE)
  expect_identical(pars$check(list(y = to_tune(list(1, 2, "z")))), TRUE)
  expect_error(pars$search_space(list(y = to_tune(list(1, 2, "z")))), "generates points that are not compatible.*\"z\"")

  expect_string(pars$check(list(uty = to_tune())), "must give a range for unbounded parameter uty")
  expect_error(pars$search_space(list(uty = to_tune())), "must give a range for unbounded parameter uty")
  expect_equal(pars$check(list(uty = to_tune(1))), TRUE)
  expect_equal(pars$check(list(uty1 = to_tune(1))), TRUE)
  expect_identical(pars$check(list(uty1 = to_tune(2))), TRUE)
  expect_error(pars$search_space(list(uty1 = to_tune(2))), "not compatible with param uty1.*2")

  expect_identical(pars$check(list(fct = to_tune("z"))), TRUE)
  expect_error(pars$search_space(list(fct = to_tune("z"))), "not compatible with param fct.*\"z\"")

  expect_identical(pars$check(list(lgl = to_tune("z"))), TRUE)
  expect_error(pars$search_space(list(lgl = to_tune("z"))), "not compatible with param lgl.*\"z\"")

  expect_string(pars$check(list(lgl = to_tune(0, 1))), "must have zero or one argument")
  expect_error(pars$search_space(list(lgl = to_tune(0, 1))), "must have zero or one argument")

  expect_error(
    pars$search_space(list(xxx = to_tune())),
    "Parameter 'xxx' not available"
  )
})

test_that("Tune ParamSet is created", {

  pars = ParamSet_legacy$new(list(
    ParamInt$new("x", lower = 0, upper = 10),
    ParamDbl$new("y", lower = 0, upper = 10, special_vals = list("x")),
    ParamFct$new("fct", c("x", "y")),
    ParamLgl$new("lgl")
  ))

  pars_tune = pars$search_space(list(x = to_tune(), y = to_tune(), fct = to_tune(), lgl = to_tune()))

  # Search spaces do not retain the source values.
  pars_tune$values = list()
  expect_equal_ps(pars, pars_tune)

  pars_unbound = ParamSet_legacy$new(list(
    ParamDbl$new("x"),
    ParamDbl$new("y", special_vals = list("x")),
    ParamFct$new("fct", letters),
    ParamUty$new("lgl")
  ))

  pars_tune = pars_unbound$search_space(list(x = to_tune(p_int(0, 10)), y = to_tune(p_dbl(0, 10, special_vals = list("x"))), fct = to_tune(c("x", "y")), lgl = to_tune(p_lgl())))
  pars_tune$values = list()
  expect_equal_ps(pars, pars_tune)

  pars_unbound_2 = ParamSet_legacy$new(list(
    ParamInt$new("x"),
    ParamDbl$new("y"),
    ParamFct$new("fct", letters),
    ParamUty$new("lgl")
  ))

  pars_tune = pars_unbound_2$search_space(list(x = to_tune(0, 10), y = to_tune(p_dbl(0, 10, special_vals = list("x"))), fct = to_tune(c("x", "y")), lgl = to_tune(p_lgl())))
  pars_tune$values = list()
  expect_equal_ps(pars, pars_tune)

  pars_tune = pars_unbound$search_space(list(x = to_tune(ParamInt$new("y", 0, 10)), y = to_tune(ps(z = p_dbl(0, 10, special_vals = list("x")))),
    fct = to_tune(c("x", "y")), lgl = to_tune(p_lgl())))

  # to_tune from ps() generates messed up value order
  expect_equal(rbindlist(generate_design_grid(pars_tune, 2)$transpose()), rbindlist(generate_design_grid(pars, 2)$transpose()), ignore.col.order = TRUE)

  pars$values = list(x = 1, y = 2, fct = to_tune(), lgl = to_tune(TRUE))
  pars_tune = pars$search_space()
  expect_equal(generate_design_grid(pars_tune)$transpose(),
    list(list(fct = "x", lgl = TRUE), list(fct = "y", lgl = TRUE)))

})

test_that("a recovered TuneToken is not a fixed search-space value", {
  domain = ps(x = p_dbl(-10, 10))
  domain$values$x = to_tune()

  search_space = domain$search_space()

  expect_identical(search_space$ids(), "x")
  expect_identical(search_space$values, named_list())
  expect_silent(generate_design_random(search_space, 10L))
})


test_that("Trafo works as expected", {

  pars = ParamSet_legacy$new(list(
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

  inpars = ParamSet_legacy$new(list(
    ParamFct$new("x", c("a", "b")),
    ParamFct$new("y", c("a", "b"))
  ))
  inpars$extra_trafo = function(x, param_set) list(x$x != x$y)
  indesign = generate_design_grid(inpars)

  outdesign = generate_design_grid(pars$search_space(list(lgl = to_tune(inpars))))

  expect_equal(indesign$data, outdesign$data)

  expect_equal(outdesign$transpose(), list(list(lgl = FALSE), list(lgl = TRUE), list(lgl = TRUE), list(lgl = FALSE)))

})

test_that("Dependencies work", {

  pars = ParamSet_legacy$new(list(
    ParamInt$new("x", lower = 0, upper = 1),
    ParamInt$new("y", lower = 0, upper = 1),
    ParamInt$new("z", lower = 0, upper = 1),
    ParamInt$new("z2", lower = 0, upper = 1)
  ))

  pars$add_dep("x", "y", CondEqual(1))
  pars$add_dep("x", "z", CondEqual(1))
  pars$add_dep("z2", "x", CondEqual(1))

  # if the tune paramset contains a "z2" in place of "y2", then the x->"z2" dependency is actually kept. This may be useful.
  # if some parameter depends on y, that dependency is lost. nothing we can do here.

  tuneps = pars$search_space(list(x = to_tune(), y = to_tune(ps(y1 = p_int(0, 1), y2 = p_int(0, 1), .extra_trafo = function(x, param_set) list(abs(x$y1 - x$y2))))))

  # Dependencies embedded in the recovered x Domain are temporary source
  # metadata. The transformed y part makes x's edge on y inapplicable, while
  # z is absent, so neither may leak into the combined search space.
  expect_equal(nrow(tuneps$deps), 0)

  pars = ParamSet_legacy$new(list(
    ParamInt$new("x", lower = 0, upper = 1),
    ParamInt$new("y", lower = 0, upper = 1),
    ParamInt$new("z", lower = 0, upper = 1),
    ParamInt$new("z2", lower = 0, upper = 1)
  ))
  pars$add_dep("y", "x", CondEqual(1))
  pars$add_dep("x", "z", CondEqual(1))
  pars$add_dep("z2", "x", CondEqual(1))

  # only the relevant dependency is kept
  tuneps = pars$search_space(list(x = to_tune(), y = to_tune()))
  expect_equal(setindex(tuneps$deps, NULL), data.table(id = "y", on = "x", cond = list(CondEqual(1))))

  #dependencies are kept between params, even if the dependor is trafo'd from other params
  tuneps = pars$search_space(list(x = to_tune(), y = to_tune(ps(y1 = p_int(0, 1), y2 = p_int(0, 1), .extra_trafo = function(x, param_set) list(abs(x$y1 - x$y2))))))

  expect_equal(setindex(tuneps$deps, NULL), data.table(id = c("y1", "y2"), on = c("x", "x"), cond = list(CondEqual(1))))

  tuneps = pars$search_space(list(x = to_tune(), y = to_tune(ps(y1 = p_int(0, 1), y2 = p_int(0, 1, depends = y1 == 1),
    .extra_trafo = function(x, param_set) list(min(x$y1, x$y2, na.rm = TRUE))))))

  # mixing dependencies from inside and outside
  expect_equal(setindex(tuneps$deps, NULL), data.table(id = c("y2", "y1", "y2"), on = c("y1", "x", "x"), cond = list(CondEqual(1))))


  parsnodep = ParamSet_legacy$new(list(
    ParamInt$new("x", lower = 0, upper = 1),
    ParamInt$new("y", lower = 0, upper = 1),
    ParamInt$new("z", lower = 0, upper = 1),
    ParamInt$new("z2", lower = 0, upper = 1)
  ))

  # amazing: dependencies across to_tune
  tuneps = parsnodep$search_space(list(x = to_tune(p_int(0, 1, depends = y == 0)), y = to_tune()))

  expect_equal(setindex(tuneps$deps, NULL), data.table(id = "x", on = "y", cond = list(CondEqual(0))))


  tuneps = pars$search_space(list(x = to_tune(), y = to_tune(ps(y1 = p_int(0, 1), y2 = p_int(0, 1, depends = x == 1),
    .extra_trafo = function(x, param_set) list(min(x$y1, x$y2, na.rm = TRUE)), .allow_dangling_dependencies = TRUE))))

  # mixing dependencies from inside and across to_tune. I am the only person in this building capable of coding this.
  expect_equal(setindex(tuneps$deps, NULL), data.table(id = c("y2", "y1", "y2"), on = c("x", "x", "x"), cond = list(CondEqual(1))))

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
  largeps = ParamSet_legacy$new(list(
    ParamFct$new("x", c("a", "b", "c")),
    ParamLgl$new("y")
  ))
  largeps$add_dep("y", "x", CondAnyOf(c("a", "b")))

  res = largeps$search_space(list(x = to_tune(c("a", "b")), y = to_tune()))
  expect_equal(res$deps$cond[[1]]$rhs, c("a", "b"))

  res = largeps$search_space(list(x = to_tune("a"), y = to_tune()))
  expect_equal(res$deps$cond[[1]]$rhs, "a")

  res = largeps$search_space(list(x = to_tune("c"), y = to_tune()))
  expect_false(res$has_deps)

})

test_that("ParamSetCollection works", {

  ps1 = ParamSet_legacy$new(list(ParamInt$new("x"), ParamInt$new("y")))
  ps2 = ParamSet_legacy$new(list(ParamInt$new("a")))

  psc = ParamSetCollection$new(list(prefix = ps1, ps2))

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
  pars = ParamSet_legacy$new(list(
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

  pars = ParamSet_legacy$new(list(
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

  pars = ParamSet_legacy$new(list(
    ParamInt$new("x", lower = 0, upper = 10),
    ParamDbl$new("y", lower = 0),
    ParamDbl$new("z", upper = 10)
  ))

  expect_equal_ps(pars$search_space(list(x = to_tune())), ParamInt$new("x", lower = 0, upper = 10))

  expect_equal_ps(pars$search_space(list(x = to_tune(lower = 1))), ParamInt$new("x", lower = 1, upper = 10))
  expect_equal_ps(pars$search_space(list(x = to_tune(upper = 1))), ParamInt$new("x", lower = 0, upper = 1))
  expect_equal_ps(pars$search_space(list(x = to_tune(lower = 1, upper = 2))), ParamInt$new("x", lower = 1, upper = 2))

  expect_error(pars$search_space(list(y = to_tune(lower = 1))), "y range must be bounded, but is \\[1, Inf\\]")
  expect_equal_ps(pars$search_space(list(y = to_tune(upper = 1))), ParamDbl$new("y", lower = 0, upper = 1))
  expect_equal_ps(pars$search_space(list(y = to_tune(lower = 1, upper = 2))), ParamDbl$new("y", lower = 1, upper = 2))

  expect_error(pars$search_space(list(z = to_tune(upper = 1))), "z range must be bounded, but is \\[-Inf, 1\\]")
  expect_equal_ps(pars$search_space(list(z = to_tune(lower = 1))), ParamDbl$new("z", lower = 1, upper = 10))
  expect_equal_ps(pars$search_space(list(z = to_tune(lower = 1, upper = 2))), ParamDbl$new("z", lower = 1, upper = 2))

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

  pars = ParamSet_legacy$new(list(
    ParamInt$new("x", lower = 0, upper = 10),
    ParamDbl$new("y", lower = 0)
  ))

  x_log = pars$search_space(list(x = to_tune(logscale = TRUE)))
  expect_equal(x_log$lower, c(x = log(.5)))
  expect_equal(x_log$upper, c(x = log(11)))
  expect_identical(x_log$is_logscale, c(x = TRUE))
  expect_equal(x_log$trafo(list(x = log(2)))$x, 2)

  y_log = pars$search_space(list(
    y = to_tune(lower = 1, upper = 10, logscale = TRUE)
  ))
  expect_equal(y_log$lower, c(y = log(1)))
  expect_equal(y_log$upper, c(y = log(10)))
  expect_identical(y_log$is_logscale, c(y = TRUE))
  expect_equal(y_log$trafo(list(y = log(5)))$y, 5)

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
  expect_output(print(to_tune(internal = TRUE)), "Internal")
})


test_that("internal and aggr", {
  param_set = ps(a = p_dbl(lower = 1, upper = 2, tags = "internal_tuning", in_tune_fn = function(domain, param_vals) domain$upper,
    disable_in_tune = list(), aggr = function(x) round(mean(unlist(x))))
  )


  # full tune token + internal
  expect_equal(
    param_set$set_values(a = to_tune(aggr = function(x) -99))$search_space()$aggr_internal_tuned_values(
      list(a = list(1, 2, 3))),
    list(a = -99)
  )

  # logscale + internal: now allowed
  expect_error(
    param_set$set_values(a = to_tune(logscale = TRUE, aggr = function(x) -99)),
    "internal tuning"
  )

  # other trafos + internal: not allowed
  expect_error(
    param_set$set_values(a = to_tune(ps(a = p_dbl(0, 1), .extra_trafo = function(x) 1L), aggr = function(x) -99)),
    "can currently not be combined"
  )

  # range + internal
  param_set$set_values(a = to_tune(lower = 1.2, upper = 1.3, aggr = function(x) 1.5))
  expect_equal(param_set$search_space()$aggr_internal_tuned_values(list(a = list(1, 2))), list(a = 1.5))
  expect_equal(param_set$convert_internal_search_space(param_set$search_space()), list(a = 1.3))

  # full + internal
  param_set$set_values(a = to_tune(internal = TRUE, aggr = function(x) 1.5))
  expect_equal(param_set$convert_internal_search_space(param_set$search_space()), list(a = 2))

  # domain + internal
  expect_error(
    param_set$set_values(a = to_tune(p_dbl(1.21, 1.22), aggr = function(x) 1.5, internal = TRUE)),
    "specify lower and upper"
  )

  # param set + internal
  param_set = ps(a = p_int(lower = 1, upper = 10000, tags = "internal_tuning", in_tune_fn = function(domain, param_vals) domain$upper,
    aggr = function(x) max(unlist(x)), disable_in_tune = list()))

  # default aggregation function is used when not overwritten
  param_set$set_values(
    a = to_tune(internal = TRUE)
  )
  expect_equal(param_set$search_space()$aggr_internal_tuned_values(list(a = list(1, 2, 3))), list(a = 3))

  # can overwrite existing aggregation function
  param_set$set_values(
    a = to_tune(internal = TRUE, aggr = function(x) -60)
  )
  expect_equal(param_set$search_space()$aggr_internal_tuned_values(list(a = list(1, 2, 3))), list(a = -60))
})

test_that("user ParamSet trafos use the direct one-value boundary", {
  parameter_set = ps(target = p_uty())

  classed_token = to_tune(ps(
    left = p_int(0L, 1L),
    right = p_int(0L, 1L),
    .extra_trafo = function(x) structure(
      list(x$left + x$right),
      class = "tune_result"
    )
  ))
  expect_error(
    parameter_set$search_space(list(target = classed_token)),
    "ordinary list",
    fixed = TRUE
  )

  search_space = parameter_set$search_space(list(
    target = to_tune(ps(
      left = p_int(0L, 1L),
      right = p_int(0L, 1L),
      .extra_trafo = function(x) list(x$left + x$right)
    ))
  ))
  expect_identical(search_space$ids(), c("left", "right"))
  expect_identical(
    search_space$trafo(list(left = 1L, right = 0L)),
    list(target = 1L)
  )
  utility_token = to_tune(ps(
    value = p_int(0L, 1L),
    .extra_trafo = function(x) list(x$value)
  ))
  expect_silent(parameter_set$values$target <- utility_token)
  expect_identical(
    parameter_set$search_space()$trafo(list(value = 1L)),
    list(target = 1L)
  )

  integer_parameter = ps(target = p_int(0L, 2L))
  integer_token = to_tune(ps(
    left = p_int(0L, 1L),
    right = p_int(0L, 1L),
    .extra_trafo = function(x) list(x$left + x$right)
  ))
  expect_silent(integer_parameter$values$target <- integer_token)
  expect_identical(
    integer_parameter$search_space()$trafo(list(left = 1L, right = 1L)),
    list(target = 2L)
  )

  double_parameter = ps(target = p_dbl(0, 2))
  double_token = to_tune(ps(
    left = p_dbl(0, 1),
    right = p_dbl(0, 1),
    .extra_trafo = function(x) list(x$left + x$right)
  ))
  expect_silent(double_parameter$values$target <- double_token)
  expect_identical(
    double_parameter$search_space()$trafo(list(left = 1, right = 1)),
    list(target = 2)
  )

  expect_error(
    parameter_set$search_space(list(
      target = to_tune(ps(
        value = p_int(0L, 1L),
        .extra_trafo = function(x) data.frame(value = x$value)
      ))
    )),
    "ordinary list",
    fixed = TRUE
  )
})

test_that("native search snapshots preserve empty and one-token Domain rows", {
  parameter_set = ps(value = p_int(0L, 1L))

  expect_identical(parameter_set$search_space(list())$ids(), character())
  expect_identical(
    parameter_set$search_space(list(value = to_tune()))$ids(),
    "value"
  )
})

test_that("ParamSet TuneTokens separate structural admission from output validation", {
  target = ps(value = p_int(0L, 1L))
  incompatible = to_tune(ps(candidate = p_int(2L, 3L)))

  # Atomic value assignment admits and snapshots the candidate graph without
  # running its transformations. The deterministic search-space conversion is
  # the sole place that evaluates output compatibility.
  expect_identical(target$check(list(value = incompatible)), TRUE)
  expect_silent(target$values$value <- incompatible)
  expect_error(
    target$search_space(),
    "generates points that are not compatible with param value",
    fixed = TRUE
  )

  corrupt = ps(candidate = p_int(0L, 1L), gate = p_lgl())
  corrupt$add_dep("candidate", "gate", CondEqual(TRUE))
  private = corrupt$.__enclos_env__$private
  state = paradox:::param_set_core_state(private)
  state$.deps$id[[1L]] = "missing"
  private$.core = .Call(paradox:::C_param_set_core_new, 1L, state)
  corrupt_token = to_tune(corrupt)
  before = target$values

  expect_error(
    target$check(list(value = corrupt_token)),
    "Corrupt ParamSet"
  )
  expect_error(target$values$value <- corrupt_token, "Corrupt ParamSet")
  expect_identical(target$values, before)
})

test_that("live ParamSet TuneToken generations cannot change during validation", {
  make_case = function(callback_kind) {
    candidate = ps(candidate = p_int(0L, 1L))
    armed = FALSE
    mutate_candidate = function() {
      if (armed) candidate$values$candidate = 1L
      TRUE
    }
    target = if (identical(callback_kind, "custom_check")) {
      ps(
        token = p_uty(),
        trigger = p_uty(custom_check = function(x) mutate_candidate())
      )
    } else {
      result = ps(token = p_uty(), trigger = p_lgl())
      result$constraint = function(x) mutate_candidate()
      result
    }
    armed = TRUE
    list(candidate = candidate, target = target, token = to_tune(candidate))
  }

  for (callback_kind in c("custom_check", "constraint")) {
    check_case = make_case(callback_kind)
    expect_error(
      check_case$target$check(list(token = check_case$token, trigger = TRUE)),
      "candidate changed during validation",
      fixed = TRUE,
      info = callback_kind
    )

    assignment_case = make_case(callback_kind)
    before = assignment_case$target$values
    expect_error(
      {
        assignment_case$target$values = list(
          token = assignment_case$token,
          trigger = TRUE
        )
      },
      "candidate changed during validation",
      fixed = TRUE,
      info = callback_kind
    )
    expect_identical(
      assignment_case$target$values,
      before,
      info = callback_kind
    )
    expect_identical(
      assignment_case$candidate$values$candidate,
      1L,
      info = callback_kind
    )
  }
})

test_that("TuneToken receipts reject delayed candidate cores without forcing", {
  forced = 0L
  candidate = ps(candidate = p_int(0L, 1L))
  candidate_private = candidate$.__enclos_env__$private
  candidate_core = candidate_private$.core
  token = to_tune(candidate)
  armed = FALSE
  target = ps(
    token = p_uty(),
    trigger = p_uty(custom_check = function(value) {
      if (armed) {
        delayedAssign(
          ".core",
          {
            forced <<- forced + 1L
            candidate_core
          },
          assign.env = candidate_private
        )
      }
      TRUE
    })
  )
  armed = TRUE

  expect_error(
    target$check(list(token = token, trigger = TRUE)),
    "candidate changed during validation",
    fixed = TRUE
  )
  expect_identical(forced, 0L)
  expect_type(substitute(.core, candidate_private), "language")
})

test_that("TuneToken receipts reject a literal-core delayed binding", {
  candidate = ps(candidate = p_int(0L, 1L))
  candidate_private = candidate$.__enclos_env__$private
  candidate_core = candidate_private$.core
  token = to_tune(candidate)
  armed = FALSE
  target = ps(
    token = p_uty(),
    trigger = p_uty(custom_check = function(value) {
      if (armed) {
        eval(as.call(list(
          quote(delayedAssign),
          ".core",
          candidate_core,
          candidate_private,
          candidate_private
        )), baseenv())
      }
      TRUE
    })
  )
  armed = TRUE

  expect_error(
    target$check(list(token = token, trigger = TRUE)),
    "candidate changed during validation",
    fixed = TRUE
  )
  expect_identical(substitute(.core, candidate_private), candidate_core)
})

test_that("TuneToken metadata has one closed nonrecursive shape", {
  make_token = function(content, next_token = NULL) {
    token = structure(
      list(content = content, call = "forged TuneToken"),
      class = c("ObjectTuneToken", "TuneToken")
    )
    if (!is.null(next_token)) token[["next_token"]] = next_token
    token
  }

  domain = p_int(0L, 1L)
  normal_target = ps(value = p_uty())
  expect_silent(normal_target$values$value <- unserialize(serialize(
    make_token(domain),
    NULL
  )))
  numeric_target = ps(value = p_dbl(0, 1))
  named_scalar_token = to_tune(
    lower = c(lower_name = 0),
    upper = c(upper_name = 1),
    logscale = c(scale_name = FALSE)
  )
  expect_identical(numeric_target$check(list(value = named_scalar_token)), TRUE)
  expect_silent(numeric_target$values$value <- named_scalar_token)
  stored_content = numeric_target$values$value$content
  expect_null(names(stored_content$lower))
  expect_null(names(stored_content$upper))
  expect_null(names(stored_content$logscale))
  target = ps(value = p_uty())

  # Extra metadata is outside the TuneToken contract. Rejecting the fixed root
  # shape means even a very deep forged tail is never traversed natively.
  deep = make_token(domain)
  for (index in seq_len(4096L)) deep = make_token(domain, deep)
  before = target$values
  expect_error(
    target$check(list(value = deep)),
    "expected the fixed {content, call} list",
    fixed = TRUE
  )
  expect_error(
    target$values$value <- deep,
    "expected the fixed {content, call} list",
    fixed = TRUE
  )
  expect_identical(target$values, before)
  while (length(deep) == 3L) {
    child = deep[["next_token"]]
    deep[["next_token"]] = NULL
    deep = child
  }
  rm(deep)
  gc()

  cyclic = make_token(domain)
  mutator = .Call(
    get("C_test_gc_column_mutator", envir = asNamespace("paradox")),
    cyclic,
    0L,
    cyclic
  )
  rm(mutator)
  gc()
  expect_identical(
    data.table::address(cyclic$content),
    data.table::address(cyclic)
  )
  before = target$values
  expect_error(
    target$check(list(value = cyclic)),
    "content must be a Domain or ParamSet",
    fixed = TRUE
  )
  assignment_error = tryCatch(
    {
      target$values$value <- cyclic
      NULL
    },
    error = identity
  )
  expect_match(
    conditionMessage(assignment_error),
    "content must be a Domain or ParamSet",
    fixed = TRUE
  )
  rm(assignment_error)
  breaker = .Call(
    get("C_test_gc_column_mutator", envir = asNamespace("paradox")),
    cyclic,
    0L,
    domain
  )
  rm(breaker)
  gc()
  expect_identical(target$values, before)
})

test_that("search_space admits exact TuneTokens before closed kind selection", {
  make_object_token = function(content) structure(
    list(content = content, call = "forged TuneToken"),
    class = c("ObjectTuneToken", "TuneToken")
  )
  target = ps(value = p_uty())
  domain = p_int(0L, 1L)

  subclass = to_tune(domain)
  class(subclass) = c("ExternalTuneToken", class(subclass))
  expect_error(
    target$search_space(list(value = subclass)),
    "unsupported class vector",
    fixed = TRUE
  )

  flatten_called = FALSE
  forged_param_set = new.env(parent = emptyenv())
  forged_param_set$flatten = function() {
    flatten_called <<- TRUE
    ps(value = p_int(0L, 1L))
  }
  class(forged_param_set) = c("ParamSet", "R6")
  expect_error(
    target$search_space(list(
      value = make_object_token(forged_param_set)
    )),
    "malformed exact BASE ParamSet",
    fixed = TRUE
  )
  expect_false(flatten_called)

  malformed_shells = list(
    missing_enclosure = local({
      shell = new.env(parent = emptyenv())
      class(shell) = c("ParamSet", "R6")
      shell
    }),
    missing_self = local({
      shell = ps(value = p_int())
      rm("self", envir = shell$.__enclos_env__)
      shell
    }),
    missing_private = local({
      shell = ps(value = p_int())
      rm("private", envir = shell$.__enclos_env__)
      shell
    }),
    missing_core = local({
      shell = ps(value = p_int())
      shell$.__enclos_env__$private = new.env(parent = emptyenv())
      shell
    })
  )
  for (shell in malformed_shells) {
    expect_error(
      target$search_space(list(value = make_object_token(shell))),
      "malformed exact BASE ParamSet",
      fixed = TRUE
    )
  }

  extra = make_object_token(domain)
  extra$metadata = TRUE
  expect_error(
    target$search_space(list(value = extra)),
    "expected the fixed {content, call} list",
    fixed = TRUE
  )

  deep = make_object_token(domain)
  for (index in seq_len(4096L)) {
    parent = make_object_token(domain)
    parent$metadata = deep
    deep = parent
  }
  expect_error(
    target$search_space(list(value = deep)),
    "expected the fixed {content, call} list",
    fixed = TRUE
  )
  while (length(deep) == 3L) {
    child = deep$metadata
    deep$metadata = NULL
    deep = child
  }
  rm(deep)
  gc()

  cyclic = make_object_token(domain)
  mutator = .Call(
    get("C_test_gc_column_mutator", envir = asNamespace("paradox")),
    cyclic,
    0L,
    cyclic
  )
  rm(mutator)
  gc()
  expect_error(
    target$search_space(list(value = cyclic)),
    "content must be a Domain or ParamSet",
    fixed = TRUE
  )
  breaker = .Call(
    get("C_test_gc_column_mutator", envir = asNamespace("paradox")),
    cyclic,
    0L,
    domain
  )
  rm(breaker, cyclic)
  gc()
})

test_that("TuneToken and search metadata use the closed ordinary shape", {
  target = ps(value = p_dbl(0, 1))
  token = to_tune(0, 1)

  expect_identical(
    target$search_space(structure(
      list(value = token),
      class = "configuration"
    ))$ids(),
    "value"
  )

  exact_hand_built = structure(
    list(
      content = list(lower = 0, upper = 1, logscale = FALSE),
      call = "exact hand-built token"
    ),
    class = c("RangeTuneToken", "TuneToken")
  )
  expect_identical(
    target$search_space(list(value = exact_hand_built))$ids(),
    "value"
  )

  forge = function(change) {
    candidate = unserialize(serialize(token, NULL))
    change(candidate)
  }
  malformed = list(
    outer = asS4(unserialize(serialize(token, NULL))),
    names = forge(function(x) {
      attr(x, "names") = asS4(names(x))
      x
    }),
    classes = forge(function(x) {
      attr(x, "class") = asS4(class(x))
      x
    }),
    content = forge(function(x) {
      x$content = asS4(x$content)
      x
    }),
    content_names = forge(function(x) {
      attr(x$content, "names") = asS4(names(x$content))
      x
    }),
    lower = forge(function(x) {
      x$content$lower = asS4(x$content$lower)
      x
    }),
    logscale = forge(function(x) {
      x$content$logscale = asS4(x$content$logscale)
      x
    }),
    call = forge(function(x) {
      x$call = asS4(x$call)
      x
    })
  )
  for (case in names(malformed)) {
    expect_error(
      target$search_space(list(value = malformed[[case]])),
      "Malformed|ordinary",
      info = case
    )
  }

  s4_values = asS4(list(value = token))
  expect_error(target$search_space(s4_values), "ordinary named list")
  s4_value_names = list(value = token)
  attr(s4_value_names, "names") = asS4(names(s4_value_names))
  expect_error(target$search_space(s4_value_names), "ordinary character")
  s4_value_classes = structure(list(value = token), class = "configuration")
  attr(s4_value_classes, "class") = asS4(class(s4_value_classes))
  expect_error(target$search_space(s4_value_classes), "malformed S3 class")

  object_target = ps(value = p_uty())
  s4_domain = asS4(p_int(0L, 1L))
  expect_error(
    object_target$search_space(list(value = structure(
      list(content = s4_domain, call = "S4 Domain"),
      class = c("ObjectTuneToken", "TuneToken")
    ))),
    "Malformed"
  )
  s4_param_set = asS4(ps(candidate = p_int(0L, 1L)))
  expect_error(
    object_target$search_space(list(value = structure(
      list(content = s4_param_set, call = "S4 ParamSet"),
      class = c("ObjectTuneToken", "TuneToken")
    ))),
    "Malformed"
  )
})

test_that("TuneToken snapshots revalidate after allocation finalizers", {
  token_snapshot = get(
    "C_test_tune_token_gc_mutation_snapshot",
    envir = asNamespace("paradox")
  )
  attribute_mutator = get(
    "C_test_gc_attribute_mutator",
    envir = asNamespace("paradox")
  )

  token = to_tune(0, 1)
  snapshot = .Call(
    token_snapshot,
    token,
    0L,
    asS4(0),
    "content"
  )
  expect_identical(snapshot$content$lower, 0)
  expect_true(isS4(token$content$lower))

  values = list(value = to_tune(0, 1))
  snapshot = .Call(
    token_snapshot,
    values,
    0L,
    0,
    "content"
  )
  expect_identical(names(snapshot), "value")
  expect_s3_class(snapshot$value, "RangeTuneToken")
  expect_identical(snapshot$value$content$lower, 0)
  expect_identical(values$value, 0)

  # The outward names belong to the value carrier selected before token
  # snapshotting, even if the caller rewrites the source list's names at the
  # allocation barrier.
  values = list(original = to_tune(0, 1))
  original_names = names(values)
  replacement_names = "renamed"
  attribute_name = "names"
  mutation_column = NULL
  mutation_replacement = NULL
  mutation_phase = "content"
  gc()
  pending = .Call(
    attribute_mutator,
    values,
    attribute_name,
    replacement_names
  )
  rm(pending)
  snapshot = .Call(
    token_snapshot,
    values,
    mutation_column,
    mutation_replacement,
    mutation_phase
  )
  expect_identical(names(snapshot), original_names)
  expect_identical(names(values), replacement_names)

  # Likewise, the content names are captured beside their matching cells.
  # Reordering only the live names after that capture must not relabel the
  # detached snapshot.
  token = to_tune(0, 1)
  content = token$content
  original_names = names(content)
  replacement_names = c("upper", "lower", "logscale")
  gc()
  pending = .Call(
    attribute_mutator,
    content,
    attribute_name,
    replacement_names
  )
  rm(pending)
  snapshot = .Call(
    token_snapshot,
    token,
    mutation_column,
    mutation_replacement,
    mutation_phase
  )
  expect_identical(names(snapshot$content), original_names)
  expect_identical(names(token$content), replacement_names)
})

test_that("Domain TuneTokens fail closed on forged candidate storage", {
  forge_column = function(domain, name, value) {
    attrs = attributes(domain)
    domain = unclass(domain)
    domain[[name]] = value
    attributes(domain) = attrs
    domain
  }
  drop_column = function(domain, name) {
    attrs = attributes(domain)
    domain = unclass(domain)
    domain[[name]] = NULL
    attrs$names = names(domain)
    attributes(domain) = attrs
    domain
  }
  forge_attribute = function(domain, name, value) {
    attr(domain, name) = value
    domain
  }

  bad_condition = CondAnyOf(c("a", "b"))
  bad_condition$rhs = c("a", "a")
  special_and_trafo = forge_column(
    p_dbl(1, 9, trafo = identity),
    "special_vals",
    list(list(Inf))
  )
  required_default = forge_column(
    forge_column(p_dbl(1, 9), "default", list(2)),
    ".tags",
    list("required")
  )
  initialized_trafo = forge_column(
    forge_column(p_dbl(1, 9, trafo = identity), ".init_given", TRUE),
    ".init",
    list(2)
  )
  attributed_cargo = p_uty()$cargo[[1L]]
  attr(attributed_cargo, "external") = TRUE
  attributed_disable = p_dbl(
    1,
    9,
    tags = "internal_tuning",
    aggr = identity,
    in_tune_fn = function(domain, values) domain$upper,
    disable_in_tune = list(flag = FALSE)
  )
  attributed_disable_cargo = attributed_disable$cargo[[1L]]
  attr(attributed_disable_cargo$disable_in_tune, "external") = TRUE
  attributed_logscale = p_dbl(1, 9, logscale = TRUE)
  attributed_logscale_cargo = attributed_logscale$cargo[[1L]]
  attr(attributed_logscale_cargo$logscale, "external") = TRUE

  malformed = list(
    reversed_bounds = forge_column(
      forge_column(p_dbl(1, 9), "lower", 8), "upper", 2
    ),
    logical_lower = forge_column(p_dbl(1, 9), "lower", TRUE),
    empty_factor = forge_column(p_fct(c("a", "b")), "levels", list(character())),
    list_lower = forge_column(p_dbl(1, 9), "lower", list(1)),
    environment_lower = forge_column(p_dbl(1, 9), "lower", new.env()),
    infinite_tolerance = forge_column(p_dbl(1, 9), "tolerance", Inf),
    reordered_logical = forge_column(p_lgl(), "levels", list(c(FALSE, TRUE))),
    invalid_trafo = forge_column(p_dbl(1, 9), ".trafo", list(1L)),
    invalid_cargo_column = forge_column(p_dbl(1, 9), "cargo", 1L),
    missing_cargo = drop_column(p_dbl(1, 9), "cargo"),
    attributed_cargo = forge_column(
      p_uty(), "cargo", list(attributed_cargo)
    ),
    attributed_disable_in_tune = forge_column(
      attributed_disable, "cargo", list(attributed_disable_cargo)
    ),
    attributed_logscale = forge_column(
      attributed_logscale, "cargo", list(attributed_logscale_cargo)
    ),
    missing_id = forge_column(p_dbl(1, 9), "id", NA_character_),
    empty_id = forge_column(p_dbl(1, 9), "id", ""),
    mismatched_grouping = forge_column(p_dbl(1, 9), "grouping", "ParamInt"),
    unknown_cargo = forge_column(
      p_dbl(1, 9), "cargo", list(list(unknown = 1L))
    ),
    invalid_aggr = forge_column(
      p_dbl(1, 9), "cargo", list(list(aggr = 1L))
    ),
    incomplete_internal_cargo = forge_column(
      p_dbl(1, 9),
      "cargo",
      list(list(in_tune_fn = identity, disable_in_tune = list(x = 1)))
    ),
    invalid_utility_repr = forge_column(
      p_uty(), "cargo", list(list(custom_check = NULL, repr = 1L))
    ),
    duplicate_cargo_names = forge_column(
      p_dbl(1, 9),
      "cargo",
      list(structure(list(NULL, NULL), names = c("aggr", "aggr")))
    ),
    classed_special_values = forge_column(
      p_dbl(1, 9),
      "special_vals",
      list(structure(list(Inf), class = "external"))
    ),
    attributed_special_values = forge_column(
      p_dbl(1, 9),
      "special_vals",
      list(structure(list(Inf), note = "external"))
    ),
    special_and_trafo = special_and_trafo,
    malformed_default_marker = forge_column(
      p_dbl(1, 9),
      "default",
      list(structure(list(2), class = "NoDefault"))
    ),
    required_default = required_default,
    duplicate_tags = forge_column(
      p_dbl(1, 9), ".tags", list(c("x", "x"))
    ),
    missing_tag = forge_column(
      p_dbl(1, 9), ".tags", list(NA_character_)
    ),
    malformed_requirement_names = forge_column(
      p_dbl(1, 9),
      ".requirements",
      list(list(list(parent = "x", cond = CondEqual(1))))
    ),
    empty_requirement_parent = forge_column(
      p_dbl(1, 9),
      ".requirements",
      list(list(list(on = "", cond = CondEqual(1))))
    ),
    unknown_requirement_condition = forge_column(
      p_dbl(1, 9),
      ".requirements",
      list(list(list(
        on = "x",
        cond = structure(list(), class = c("ExternalCondition", "Condition"))
      )))
    ),
    duplicate_requirement_rhs = forge_column(
      p_dbl(1, 9),
      ".requirements",
      list(list(list(on = "x", cond = bad_condition)))
    ),
    false_init_with_value = forge_column(
      p_dbl(1, 9), ".init", list(2)
    ),
    initialized_trafo = initialized_trafo,
    tune_token_init = forge_column(
      forge_column(p_dbl(1, 9), ".init_given", TRUE),
      ".init",
      list(to_tune())
    ),
    extra_table_attribute = forge_attribute(
      p_dbl(1, 9), "external", TRUE
    ),
    invalid_row_names = forge_attribute(
      p_dbl(1, 9), "row.names", 2L
    ),
    invalid_selfref = forge_attribute(
      p_dbl(1, 9), ".internal.selfref", 1L
    )
  )
  subclassed = p_dbl(1, 9)
  class(subclassed) = c("ExternalParamDbl", class(subclassed))
  malformed$subclassed = subclassed

  target = ps(value = p_uty())
  before = target$values
  for (case in names(malformed)) {
    domain = malformed[[case]]
    token = structure(
      list(content = domain, call = "forged Domain"),
      class = c("ObjectTuneToken", "TuneToken")
    )
    expect_error(
      target$check(list(value = token)),
      paste0(
        "Malformed ObjectTuneToken Domain|",
        "Unsupported Condition class"
      ),
      info = case
    )
    expect_error(
      target$values$value <- token,
      paste0(
        "Malformed ObjectTuneToken Domain|",
        "Unsupported Condition class"
      ),
      info = case
    )
    expect_identical(target$values, before, info = case)
  }
})

test_that("Domain TuneTokens accept canonical live and copied rows", {
  opaque_default = new.env(parent = emptyenv())
  opaque_init = new.env(parent = emptyenv())
  domains = list(
    p_dbl(0, 1, special_vals = list(Inf)),
    p_int(0L, 10L, logscale = TRUE),
    p_fct(list(one = 1L, two = 2L)),
    p_lgl(depends = gate == TRUE),
    p_fct(
      c("one", "two"),
      special_vals = list(opaque_default, opaque_init),
      default = opaque_default,
      init = opaque_init
    )
  )
  target = ps(value = p_uty())
  make_token = function(domain) structure(
    list(content = domain, call = "canonical Domain"),
    class = c("ObjectTuneToken", "TuneToken")
  )

  for (domain in domains) {
    variants = list(
      domain,
      unserialize(serialize(domain, NULL)),
      data.table::copy(domain)
    )
    for (variant in variants) {
      expect_identical(target$check(list(value = make_token(variant))), TRUE)
    }
  }

  unbounded_utility = make_token(p_uty())
  expect_error(
    target$check(list(value = unbounded_utility)),
    "lower/upper/tolerance"
  )

  token = make_token(domains[[5L]])
  expect_silent(target$values$value <- token)
  stored = target$values$value$content
  expect_identical(stored$default[[1L]], opaque_default)
  expect_identical(stored$.init[[1L]], opaque_init)
  expect_error(
    p_dbl(0, 1, default = 0.5, tags = "required"),
    "required.*default"
  )
  expect_error(
    p_dbl(0, 1, trafo = identity, init = 0.5),
    "Initial value and trafo"
  )
})

test_that("cannot mark non-int-tuneable parameters for int tuning", {
  p = ps(x = p_int(lower = 0L))
  expect_error(
    p$set_values(x = to_tune(upper = 1000, internal = TRUE, aggr = function(x) round(mean(unlist(x))))),
    "Trying to assign"
  )
})

test_that("to_tune(<Domain>) keeps the Domain's init as a fixed value", {
  # Paradox 1 wrapped the user-supplied Domain in ParamSet$new() unchanged, so
  # its `init` became a value of the generated search space.
  set = ps(x = p_dbl(0, 100))
  set$values$x = to_tune(p_int(1, 10, init = 5L))

  search_space = set$search_space()
  expect_equal(search_space$ids(), "x")
  expect_equal(search_space$values, list(x = 5L))
  expect_equal(nrow(generate_design_grid(search_space, 3)$data), 1L)

  factor_set = ps(x = p_fct(c("a", "b", "c")))
  factor_set$values$x = to_tune(p_fct(c("a", "b"), init = "a"))
  expect_equal(factor_set$search_space()$values, list(x = "a"))

  # A Domain without an init still contributes no value.
  set$values$x = to_tune(p_dbl(0, 10))
  expect_equal(set$search_space()$values, named_list())
})

test_that("to_tune(<ParamSet>) keeps the ParamSet's init as a fixed value", {
  set = ps(x = p_dbl(0, 100))
  set$values$x = to_tune(ps(a = p_dbl(0, 1, init = 0.5)))
  expect_equal(set$search_space()$values, list(a = 0.5))
})

test_that("tuning the whole parameter does not fix its own current value", {
  # `$domains` projects the ParamSet's current value into the recovered
  # Domain's `.init`; for a tuned parameter that value is the TuneToken.
  set = ps(x = p_dbl(0, 100, init = 7))
  expect_equal(set$values, list(x = 7))

  set$values$x = to_tune()
  expect_equal(set$search_space()$values, named_list())
  expect_equal(set$search_space()$ids(), "x")

  set$values$x = to_tune(1, 10)
  expect_equal(set$search_space()$values, named_list())

  set$values$x = to_tune(p_dbl(1, 10, logscale = TRUE))
  expect_equal(set$search_space()$values, named_list())
})

test_that("search_space() rejects values naming an unknown parameter", {
  # Paradox 1 asserted names(values) to be a subset of $ids(); without that a
  # misspelled non-token entry silently produced an empty search space. Like
  # that assertion, the diagnostic names the offending entry.
  set = ps(a = p_dbl(0, 1))
  expect_error(
    set$search_space(values = list(zzz = 1)),
    "Parameter 'zzz' not available"
  )
  expect_error(
    set$search_space(values = list(zzz = to_tune(0, 1))),
    "Parameter 'zzz' not available"
  )
  expect_error(
    set$search_space(values = list(a = to_tune(0, 1), zzz = 1)),
    "Parameter 'zzz' not available"
  )
  expect_equal(set$search_space(values = list(a = to_tune(0, 1)))$ids(), "a")
  expect_equal(length(set$search_space(values = list())$ids()), 0L)
  expect_equal(length(set$search_space(values = list(a = 0.5))$ids()), 0L)

  collection = ParamSetCollection$new(list(g = ps(a = p_dbl(0, 1))))
  expect_error(
    collection$search_space(values = list(a = 1)),
    "Parameter 'a' not available. Did you mean 'g.a'?",
    fixed = TRUE
  )
  expect_equal(
    collection$search_space(values = list(g.a = to_tune(0, 1)))$ids(),
    "g.a"
  )
})

test_that("search-space token selection is terminal", {
  token_snapshot = function(values, column = NULL, replacement = NULL,
      phase = "content") {
    .Call(
      get("C_test_tune_token_gc_mutation_snapshot", envir = asNamespace("paradox")),
      values,
      column,
      replacement,
      phase
    )
  }
  attribute_mutator = function(target, name, value) {
    .Call(
      get("C_test_gc_attribute_mutator", envir = asNamespace("paradox")),
      target,
      name,
      value
    )
  }

  # A value classified as an ordinary value, then turned into a TuneToken in
  # place while the result carriers were allocated, used to disappear from the
  # search space with no diagnostic at all.
  values = list(
    a = to_tune(0, 1),
    b = list(
      content = list(lower = 0, upper = 1, logscale = FALSE),
      call = "late token"
    )
  )
  pending = attribute_mutator(
    values$b,
    "class",
    c("RangeTuneToken", "TuneToken")
  )
  rm(pending)
  expect_error(
    token_snapshot(values, phase = "selection"),
    "Search-space values changed while their TuneToken snapshot was constructed",
    fixed = TRUE
  )
  expect_s3_class(values$b, "RangeTuneToken")

  # The mirror direction was already fail-closed: a selected value whose class
  # disappears reaches the established structural diagnostic.
  demoted = list(a = to_tune(0, 1))
  pending = attribute_mutator(demoted$a, "class", NULL)
  rm(pending)
  expect_error(
    token_snapshot(demoted, phase = "selection"),
    "unsupported class vector",
    fixed = TRUE
  )

  # A kind change that keeps the content shape admissible reaches neither of
  # those diagnostics; only the retained kind rejects it.
  retyped = list(a = to_tune(0, 1))
  pending = attribute_mutator(
    retyped$a,
    "class",
    c("InternalTuneToken", "RangeTuneToken", "TuneToken")
  )
  rm(pending)
  expect_error(
    token_snapshot(retyped, phase = "selection"),
    "Search-space values changed while their TuneToken snapshot was constructed",
    fixed = TRUE
  )

  # A class vector that merely contains "TuneToken" is still selected and still
  # reaches the established structural diagnostic.
  subclassed = list(a = to_tune(0, 1))
  class(subclassed$a) = c("ExternalTuneToken", class(subclassed$a))
  expect_error(
    token_snapshot(subclassed, phase = "selection"),
    "unsupported class vector",
    fixed = TRUE
  )

  # Without a mutation the same barrier must accept both tokens.
  unchanged = list(a = to_tune(0, 1), b = to_tune(2, 3))
  snapshot = token_snapshot(unchanged, phase = "selection")
  expect_named(snapshot, c("a", "b"))
  expect_s3_class(snapshot$a, "RangeTuneToken")
  expect_s3_class(snapshot$b, "RangeTuneToken")
})

test_that("a TuneToken snapshot owns its shell and never rereads its source", {
  token = to_tune(0.25, 0.75)
  snapshot = .Call(
    get("C_test_tune_token_gc_mutation_snapshot", envir = asNamespace("paradox")),
    token,
    0L,
    list(lower = 0, upper = 1, logscale = FALSE),
    "token"
  )
  # A supported mutation of the source after its generation was selected wins
  # on the source and leaves a coherent older snapshot behind.
  expect_identical(
    snapshot$content,
    list(lower = 0.25, upper = 0.75, logscale = FALSE)
  )
  expect_identical(token$content, list(lower = 0, upper = 1, logscale = FALSE))
  # The snapshot's own shell metadata is owned, not shared with the live token:
  # a shared class vector would remain the dispatch authority for every later
  # reader of the detached snapshot.
  expect_false(identical(
    data.table::address(attr(snapshot, "class")),
    data.table::address(attr(token, "class"))
  ))
  expect_false(identical(
    data.table::address(attr(snapshot, "names")),
    data.table::address(attr(token, "names"))
  ))
  expect_identical(class(snapshot), c("RangeTuneToken", "TuneToken"))
  expect_identical(names(snapshot), c("content", "call"))

  # Replacing the live class after the snapshot exists cannot reach it.
  data.table::setattr(token, "class", c("FullTuneToken", "TuneToken"))
  expect_identical(class(snapshot), c("RangeTuneToken", "TuneToken"))
})

test_that("TuneToken content fields come from one generation", {
  # `to_tune()` retains the exact scalar objects it was given, so a by-reference
  # write to the source columns is a same-object mutation of the token's own
  # payload.
  source = data.table::data.table(lo = 0.2, hi = 0.9)
  token = to_tune(source$lo, source$hi)
  token_snapshot = get(
    "C_test_tune_token_gc_mutation_snapshot",
    envir = asNamespace("paradox")
  )
  snapshot_column = NULL
  snapshot_replacement = NULL
  snapshot_phase = "content"
  finalizer_state = new.env(parent = emptyenv())
  finalizer_state$fired = FALSE
  expect_identical(
    data.table::address(token$content$lower),
    data.table::address(source$lo)
  )
  trigger = new.env(parent = emptyenv())
  reg.finalizer(trigger, function(e) {
    finalizer_state$fired = TRUE
    data.table::set(source, 1L, "lo", 0.4)
    data.table::set(source, 1L, "hi", 0.5)
  })
  rm(trigger)
  snapshot = .Call(
    token_snapshot,
    token,
    snapshot_column,
    snapshot_replacement,
    snapshot_phase
  )
  expect_true(finalizer_state$fired)
  # The payload copy is one allocation-free pass, so the two bounds are always
  # taken from the same generation. A per-leaf copy could pair the pre-mutation
  # lower bound with the post-mutation upper bound.
  expect_identical(
    c(snapshot$content$lower, snapshot$content$upper),
    c(source$lo, source$hi)
  )
  expect_false(identical(
    c(snapshot$content$lower, snapshot$content$upper),
    c(0.2, 0.5)
  ))
  # The owned destinations are canonical: representation names are dropped
  # without mutating the caller's leaves.
  named = to_tune(c(low = 0.1), c(high = 0.9))
  owned = .Call(
    token_snapshot,
    named,
    NULL,
    NULL,
    "content"
  )
  expect_null(names(owned$content$lower))
  expect_identical(names(named$content$lower), "low")
})
