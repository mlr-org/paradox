

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

s = SamplerUnif$new(ps)$sample(1000)
hist(s$test_cnt, freq = TRUE)
plot(table(s$test_int))
plot(table(s$test_lgl))
plot(table(s$test_dsc))

s = generate_design_lhs(ps, 1000)
hist(s$test_cnt)
plot(table(s$test_int))
plot(table(s$test_lgl))
plot(table(s$test_dsc))

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

ps2 = ParamSet$new(
  list(
    ParamInt$new("test_int1", lower = 0, upper = 10),
    ParamInt$new("test_int2", lower = 0, upper = 10)
  )
)

s = generate_design_lhs(ps2, 8000)
plot(s$test_int1 + rnorm(nrow(s), sd=.2), s$test_int2 + rnorm(nrow(s), sd=.2))

s = generate_design_random(ps2, 8000)
plot(s$test_int1 + rnorm(nrow(s), sd=.2), s$test_int2 + rnorm(nrow(s), sd=.2))


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

s$test_cnt[is.na(s$test_cnt)] = -2 + runif(sum(is.na(s$test_cnt)))
plot(s$test_dsc, s$test_cnt)

s$test_dsc = as.numeric(s$test_dsc) + runif(nrow(s)) / 2
plot(s$test_dsc, s$test_cnt)

# ------


ps = ParamSet$new(
  list(
    ParamDbl$new("test_cnt", lower = 0, upper = 1, special_vals = list("a"), default = "a", tags = letters[1:2]),
    ParamInt$new("test_int", lower = 0, upper = 10, special_vals = list("b"), default = 2, tags = letters[1:4]),
    ParamFct$new("test_dsc", values = letters[1:3], default = "d", special_vals = list("d"),  tags = letters[2:3]),
    ParamLgl$new("test_lgl", special_vals = list("c"), default = FALSE, tags = letters[3:4])
  )
)

plot(Sampler1DUnif$new(ps$params$test_cnt)$sample(10))
Sampler1DUnif$new(ps$params$test_int)
Sampler1DUnif$new(ps$params$test_dsc)
Sampler1DUnif$new(ps$params$test_lgl)

Sampler1DCateg$new(ps$params$test_dsc)
plot(Sampler1DCateg$new(ps$params$test_lgl, prob = c(.1, .9))$sample(1000))

Sampler1DTruncNorm$new(ps$params$test_int)  # XXX


Sampler1DTruncNorm$new(ps$params$test_dbl)







