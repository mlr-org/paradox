native_domains_call = function(param_set) {
  .Call(
    paradox:::C_param_set_domains,
    param_set$.__enclos_env__$private,
    param_set
  )
}

test_that("domains native entry is registered and forced", {
  symbol = paradox:::C_param_set_domains
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)
  expect_error(
    .Call("param_set_domains", PACKAGE = "paradox"),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("one capsule snapshot reconstructs all closed Domain kinds", {
  marker = new.env(parent = emptyenv())
  transform = function(x) exp(x)
  param_set = ps(
    number = p_dbl(
      -2,
      2,
      tolerance = 1e-7,
      tags = c("numeric", "shared"),
      trafo = transform
    ),
    integer = p_int(-3L, 3L, tags = "shared", init = 2L),
    factor = p_fct(c("small", "large"), default = "small"),
    flag = p_lgl(),
    payload = p_uty(custom_check = function(x) TRUE, init = marker)
  )
  param_set$add_dep("factor", "integer", CondAnyOf(c(-1L, 2L)))
  param_set$add_dep("factor", "flag", CondEqual(TRUE))

  observed = native_domains_call(param_set)
  expect_identical(observed, param_set$domains)
  expect_identical(names(observed), param_set$ids())
  expect_identical(
    unname(vapply(observed, class, character(4L))[1L, ]),
    c("ParamDbl", "ParamInt", "ParamFct", "ParamLgl", "ParamUty")
  )
  expect_identical(observed$payload$.init[[1L]], marker)
  expect_identical(observed$number$.trafo[[1L]], transform)
  expect_identical(
    vapply(
      observed$factor$.requirements[[1L]],
      `[[`,
      character(1L),
      "on"
    ),
    c("integer", "flag")
  )
  expect_true(all(vapply(
    observed,
    function(domain) data.table:::selfrefok(domain, FALSE) == 1L,
    logical(1L)
  )))
})

test_that("Domain results and their names are detached from capsule tables", {
  param_set = ps(
    x = p_dbl(0, 1, tags = c("first", "second"), trafo = exp),
    parent = p_int(0, 2),
    special = p_dbl(0, 1, special_vals = list(-1))
  )
  param_set$add_dep("x", "parent", CondEqual(1L))
  observed = param_set$domains
  data.table::set(observed$x, i = 1L, j = "lower", value = -10)
  data.table::set(observed$x, j = "added", value = 1L)
  observed$x$.tags[[1L]][[1L]] = "changed"
  observed$x$.requirements[[1L]][[1L]]$on = "changed"
  observed$x$.trafo[1L] = list(NULL)
  observed$special$special_vals[[1L]][[1L]] = -2
  names(observed)[[1L]] = "changed"

  fresh = param_set$domains
  expect_identical(fresh$x$lower, 0)
  expect_false("added" %in% names(fresh$x))
  expect_identical(fresh$x$.tags[[1L]], c("first", "second"))
  expect_identical(fresh$x$.requirements[[1L]][[1L]]$on, "parent")
  expect_identical(fresh$x$.trafo[[1L]], exp)
  expect_identical(fresh$special$special_vals[[1L]], list(-1))
  expect_identical(names(fresh), c("x", "parent", "special"))
})

test_that("collection Domains use native live values and dependencies", {
  left = ps(
    parent = p_int(0L, 3L, init = 2L),
    target = p_fct(c("a", "b"), tags = "target", trafo = toupper)
  )
  left$add_dep("target", "parent", CondEqual(2L))
  right = ps(flag = p_lgl(init = TRUE))
  inner = ParamSetCollection$new(
    list(left = left, right = right),
    postfix_names = TRUE
  )
  collection = ParamSetCollection$new(
    list(outer = inner),
    postfix_names = TRUE
  )
  collection$add_dep("target.left.outer", "flag.right.outer", CondEqual(TRUE))

  observed = collection$domains
  expect_identical(names(observed), collection$ids())
  expect_identical(observed$parent.left.outer$.init[[1L]], 2L)
  expect_identical(observed$target.left.outer$.trafo[[1L]], toupper)
  expect_identical(
    vapply(
      observed$target.left.outer$.requirements[[1L]],
      `[[`,
      character(1L),
      "on"
    ),
    c("parent.left.outer", "flag.right.outer")
  )

  left$values = list(parent = 1L)
  left$deps = data.table::data.table(
    id = "target",
    on = "parent",
    cond = list(CondEqual(1L))
  )
  refreshed = collection$domains
  expect_identical(refreshed$parent.left.outer$.init[[1L]], 1L)
  expect_identical(
    refreshed$target.left.outer$.requirements[[1L]][[1L]]$cond$rhs,
    1L
  )
})

test_that("empty and additive-subclass capsule Domains stay supported", {
  expect_identical(ParamSet$new()$domains, setNames(list(), character()))

  Additive = R6::R6Class(
    "ContractDomainsAdditive",
    inherit = ParamSet,
    public = list(label = "kept")
  )
  object = Additive$new(list(x = p_int()))
  expect_identical(object$domains$x$id, "x")
  expect_identical(object$label, "kept")
})

test_that("closed and corrupt Domain capsule state errors without fallback", {
  object = ps(x = p_int())
  private = object$.__enclos_env__$private
  params = paradox:::param_set_core_state(private)$.params
  params$cls[[1L]] = "ThirdPartyParam"
  paradox:::param_set_core_replace(private, params = params)

  expect_error(object$domains, "Corrupt ParamSet Domain capsule state")
  expect_error(
    native_domains_call(object),
    "Corrupt ParamSet Domain capsule state"
  )
})

test_that("ParamSetShadow Domains refresh live values and dependencies", {
  origin = ps(
    hidden = p_int(),
    parent = p_dbl(0, 1),
    shown = p_dbl(0, 1, trafo = exp)
  )
  origin$values = list(hidden = 1L, parent = 0.75, shown = 0.25)
  shadow = ParamSetShadow$new(origin, "hidden")

  expect_identical(shadow$domains$shown$.init[[1L]], 0.25)
  origin$values = list(hidden = 2L, parent = 0.75, shown = 0.75)
  expect_identical(shadow$domains$shown$.init[[1L]], 0.75)

  origin$add_dep("shown", "parent", CondEqual(0.75))
  expect_identical(
    shadow$domains$shown$.requirements[[1L]][[1L]]$cond$rhs,
    0.75
  )
})
