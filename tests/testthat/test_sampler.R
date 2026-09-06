context("sampling")

test_that("1d samplers: basic tests", {
  samplers = list(
    ParamDbl = list(Sampler1DUnif, Sampler1DNormal),
    ParamInt = list(Sampler1DUnif),
    ParamFct = list(Sampler1DUnif, Sampler1DCateg),
    ParamLgl = list(Sampler1DUnif, Sampler1DCateg)
  )
  ps = th_paramset_full()
  for (p in ps$subspaces()) {
    ss = samplers[[p$class]]
    for (s in ss) {
      expect_error(s$new(ps()), "exactly 1 Param, but contains 0")
      expect_error(s$new(ps_union(list(x = p, y = p))), "exactly 1 Param, but contains 2")
      expect_class(s$new(p), "Sampler1D")
      s = s$new(p)
      info = paste(p$ids(), "-", class(s)[[1L]])
      n = 5L
      x = s$sample(n)
      d = x$data
      expect_data_table(d, ncols = 1L, nrows = n, info = info)
      d1 = d[[1]]
      expect_is(d1, p$storage_type, info = info)
      if (p$class %in% c("ParamInt", "ParamDbl")) {
        expect_true(all(d1 >= p$lower & d1 <= p$upper), info = info)
      }
      if (p$class %in% c("ParamFct")) {
        expect_true(all(d1 %in% p$levels[[1]]), info = info)
      }
      expect_output(print(s), "<Sampler")
    }
  }
})

test_that("sampling of unif requires finite bounds", {
  p = ParamInt$new(id = "x", lower = 1)
  s = expect_error(Sampler1DUnif$new(p), "bounded")

  p = ParamDbl$new(id = "x", lower = 1)
  s = expect_error(Sampler1DUnif$new(p), "bounded")
})

test_that("SamplerJointIndep", {
  p1 = th_param_fct()
  p2 = th_param_dbl()
  ps = ParamSet_legacy$new(list(p1, p2))
  s1 = Sampler1DCateg$new(p1)
  s2 = Sampler1DUnif$new(p2)
  s = SamplerJointIndep$new(list(s1, s2))
  # as the ps is constructed in the sampler, we cannot expect ps$id to be the same
  expect_equal(s$param_set$params, ps$params)
  d = s$sample(20)
  dd = d$data
  expect_data_table(dd, ncols = 2L, nrows = 20L)
  expect_equal(colnames(dd), ps$ids())
  expect_numeric(dd$th_param_dbl, lower = -10, upper = 10)
  expect_character(dd$th_param_fct)
  expect_true(all(map_lgl(d$transpose(), ps$test)))
  expect_output(print(s), "<SamplerJointIndep>")
  expect_output(print(s), "Independent comps: 2")
})

test_that("SamplerUnif", {
  ps_list = list(
    dbl = th_paramset_dbl1(),
    full = th_paramset_full(),
    repeated = th_paramset_repeated(),
    numeric = th_paramset_numeric()
  )

  for (info in names(ps_list)) {
    ps = ps_list[[info]]
    s = SamplerUnif$new(ps)
    # as the ps is constructed in the sampler, we cannot expect ps$id to be the same
    expect_equal(s$param_set$params, ps$params)
    d = s$sample(10)
    dd = d$data
    expect_data_table(dd, nrows = 10, any.missing = FALSE, info = info)
    expect_equal(colnames(dd), ps$ids(), info = info)
    expect_true(all(map_lgl(d$transpose(), ps$test)), info = info)
    expect_output(print(s), "<SamplerUnif>")
    expect_output(print(s), str_collapse(ps$ids()[1])) # check that we at least see an id
  }
})

test_that("SamplerUnif works with deps", {
  ps = th_paramset_deps()
  s = SamplerUnif$new(ps)
  d = s$sample(1000)
  dd = d$data
  expect_data_table(dd, nrows = 1000, ncols = 4L, any.missing = TRUE)
  expect_true(anyNA(dd))
  expect_true(all((dd$th_param_fct %in% c("c", NA_character_) & is.na(dd$th_param_dbl))
  | (dd$th_param_fct %in% c("a", "b") & !is.na(dd$th_param_dbl))))
  expect_names(names(dd), permutation.of = c("th_param_int", "th_param_dbl", "th_param_lgl", "th_param_fct"))
  expect_true(all(map_lgl(d$transpose(filter_na = TRUE), ps$test)))
})

test_that("we had a bug where creating the joint sampler changed the ps-ref of the 1d samplers", {
  p1 = th_param_fct()
  p2 = th_param_dbl()
  ps = ParamSet_legacy$new(list(p1, p2))
  s1 = Sampler1DCateg$new(p1)
  s2 = Sampler1DUnif$new(p2)
  s = SamplerJointIndep$new(list(s1, s2))

  s1_expected = ParamSet_legacy$new(list(th_param_fct()))

  expect_equal_ps(s1$param_set, s1_expected)

  s2_expected = ParamSet_legacy$new(list(th_param_dbl()))

  expect_equal_ps(s2$param_set, s2_expected)
})

test_that("Sampler1DRfun with 0 samples (#338)", {
  s = Sampler1DRfun$new(param = ParamDbl$new("x", 0, 10), rfun = function(n) numeric(0))
  x = s$sample(0)
  expect_data_table(x$data, nrows = 0L, ncols = 1L)
})

test_that("Sampler1DUnif rejects empty factors before RNG entry", {
  sampler = Sampler1DUnif$new(ps(choice = p_fct(character())))
  zero = sampler$sample(0L)
  expect_identical(dim(zero$data), c(0L, 1L))
  expect_identical(zero$data$choice, character())

  set.seed(421L)
  before = .Random.seed
  expect_error(
    sampler$sample(1L),
    "Cannot sample a factor parameter with no levels",
    fixed = TRUE
  )
  expect_identical(.Random.seed, before)
})

test_that("Sampler1DCateg preserves typed zero-row empty factors", {
  sampler = Sampler1DCateg$new(
    ps(choice = p_fct(character()))
  )
  zero = sampler$sample(0L)
  expect_identical(dim(zero$data), c(0L, 1L))
  expect_identical(zero$data$choice, character())

  explicit = Sampler1DCateg$new(
    ps(choice = p_fct(character())),
    prob = numeric()
  )
  expect_identical(explicit$sample(0L)$data$choice, character())

  set.seed(422L)
  before = .Random.seed
  expect_error(
    sampler$sample(1L),
    "Cannot sample a factor parameter with no levels",
    fixed = TRUE
  )
  expect_identical(.Random.seed, before)
})

test_that("SamplerHierarchical rejects duplicate sampler IDs at construction", {
  param_set = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
  x1 = Sampler1DUnif$new(param_set$subset("x"))
  x2 = Sampler1DUnif$new(param_set$subset("x"))
  y = Sampler1DUnif$new(param_set$subset("y"))

  expect_error(
    SamplerHierarchical$new(param_set, list(x1, x2, y)),
    "IDs of params in samplers do not correspond",
    fixed = TRUE
  )
  sampler = SamplerHierarchical$new(param_set, list(y, x1))
  expect_identical(names(sampler$sample(2L)$data), c("y", "x"))
})

test_that("SamplerHierarchical owns its graph before subclass reads", {
  source = psc(base = ps(x = p_dbl(0, 1)))
  mutate_source = function() {
    source$add(ps(y = p_dbl(0, 1)), "late")
  }
  MutatingSampler = R6::R6Class(
    "ParadoxAdversarialMutatingSampler",
    inherit = Sampler1D,
    public = list(
      initialize = function(param, callback) {
        private$.callback = callback
        super$initialize(param)
      }
    ),
    active = list(
      param = function() {
        if (!private$.fired) {
          private$.fired = TRUE
          private$.callback()
        }
        self$param_set
      }
    ),
    private = list(
      .callback = NULL,
      .fired = FALSE,
      .sample = function(n) {
        data.table::data.table(base.x = rep(0.5, n))
      }
    )
  )
  child = MutatingSampler$new(
    source$subset("base.x"),
    mutate_source
  )

  sampler = SamplerHierarchical$new(source, list(child))
  expect_identical(source$ids(), c("base.x", "late.y"))
  expect_identical(sampler$param_set$ids(), "base.x")
})

test_that("Sampler cannot pair sampled rows with a rebound support", {
  RebindingSampler = R6::R6Class(
    "ParadoxAdversarialRebindingSampler",
    inherit = Sampler,
    public = list(
      initialize = function(param_set, replacement) {
        private$.replacement = replacement
        super$initialize(param_set)
      }
    ),
    private = list(
      .replacement = NULL,
      .sample = function(n) {
        self$param_set = private$.replacement
        data.table::data.table(x = rep(0.5, n))
      }
    )
  )
  replacement = ps(y = p_dbl(0, 1))
  sampler = RebindingSampler$new(
    ps(x = p_dbl(0, 1)),
    replacement
  )

  expect_error(
    sampler$sample(1L),
    "changed its ParamSet while sampling",
    fixed = TRUE
  )
  # The inner mutation wins; the outer operation merely refuses to construct a
  # chimera Design from its rows.
  expect_identical(sampler$param_set, replacement)
})
