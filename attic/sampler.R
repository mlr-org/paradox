

devtools::load_all()


ps = ParamSet$new(
  list(
    ParamDbl$new("test_cnt", lower = 0, upper = 1, special_vals = list("a"), default = "a", tags = letters[1:2]),
    ParamInt$new("test_int", lower = 0, upper = 10, special_vals = list("b"), default = 2, tags = letters[1:4]),
    ParamFct$new("test_dsc", values = letters[1:3], default = "d", special_vals = list("d"),  tags = letters[2:3]),
    ParamLgl$new("test_lgl", special_vals = list("c"), default = FALSE, tags = letters[3:4])
  )
)

ps2 = ps$clone(deep = TRUE)
ps2$params$test_lgl$id = "test_lgl_2"
SamplerUnif$new(ps2)  # XXX

# ------

s = SamplerUnif$new(ps)$sample(10000)
hist(s$test_cnt)
plot(table(s$test_int))
plot(table(s$test_lgl))

s = generate_design_lhs(ps, 1000)
hist(s$test_cnt)
plot(table(s$test_int))
plot(table(s$test_lgl))

# ------

ps2 = ParamSet$new(
  list(
    ParamDbl$new("test_cnt", lower = 0, upper = 10),
    ParamDbl$new("test_cnt", lower = 0, upper = 10)
  )
)  # FIXME: should give an error, but doesn.t

# ------

ps2 = ParamSet$new(
  list(
    ParamDbl$new("test_cnt", lower = 0, upper = 10)
  )
)

ps2$add(ParamDbl$new("test_cnt", lower = 0, upper = 10))  # should give an Error, and does, so good.

# ------

ps2 = ParamSet$new(
  list(
    ParamDbl$new("test_cnt1", lower = 0, upper = 10),
    ParamDbl$new("test_cnt2", lower = 0, upper = 10)
  )
)

s = generate_design_lhs(ps2, 1000)
plot(s$test_cnt1, s$test_cnt2)

s = generate_design_random(ps2, 1000)
plot(s$test_cnt1, s$test_cnt2)

# ------

ps3 = ParamSet$new(
  list(
    ParamFct$new("test_dsc", values = as.character(0:9)),
    ParamDbl$new("test_cnt", lower = 0, upper = 10)
  )
)

s = generate_design_lhs(ps3, 1000)
plot(s$test_dsc, s$test_cnt)

ps3$add_dep("test_dsc", on = "test_dsc", CondEqual$new("0"))  # FIXME: should probably give an error?

# ------


ps3 = ParamSet$new(
  list(
    ParamFct$new("test_dsc", values = as.character(0:9)),
    ParamDbl$new("test_cnt", lower = 0, upper = 10)
  )
)

ps3$add_dep("test_cnt", on = "test_dsc", CondEqual$new("0"))

s = generate_design_random(ps3, 1000)
plot(s$test_dsc, s$test_cnt)

s$test_cnt[is.na(s$test_cnt)] = runif(sum(is.na(s$test_cnt)))
plot(s$test_dsc, s$test_cnt)

s$test_dsc = as.numeric(s$test_dsc) + runif(nrow(s))
plot(s$test_dsc, s$test_cnt)

