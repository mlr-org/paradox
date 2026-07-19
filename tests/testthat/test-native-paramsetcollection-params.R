collection_params_symbol = function() {
  get(
    "C_param_set_collection_params",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

collection_params_native = function(x) {
  .Call(collection_params_symbol(), x$.__enclos_env__$private, x)
}

test_that("collection params native entry has one fixed capsule interface", {
  symbol = collection_params_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("collection params combine static and live dynamic snapshots", {
  marker = new.env(parent = emptyenv())
  left = ps(
    choice = p_fct(c("slow", "fast"), tags = "mode", init = "fast"),
    amount = p_dbl(-2, 2, tags = "numeric", trafo = exp)
  )
  left$add_dep("choice", "amount", CondAnyOf(c(-1, 1)))
  right = ps(payload = p_uty(tags = "opaque", init = marker))
  collection = ParamSetCollection$new(
    list(owner = left, other = right),
    tag_sets = TRUE,
    tag_params = TRUE
  )

  params = collection_params_native(collection)
  expect_identical(params, collection$params)
  expect_identical(params$id, c(
    "owner.choice", "owner.amount", "other.payload"
  ))
  expect_identical(params$.init_given, c(TRUE, FALSE, TRUE))
  expect_identical(params$.init[[1L]], "fast")
  expect_identical(params$.init[[3L]], marker)
  expect_identical(params$.requirements[[1L]][[1L]], "owner.amount")
  expect_identical(params$.trafo[[2L]], exp)
  expect_true(all(c("mode", "set_owner", "param_choice") %in%
    params$.tags[[1L]]))
  expect_identical(data.table:::selfrefok(params, FALSE), 1L)
})

test_that("collection params preserve construction order with sorted translation", {
  child = ps(
    zeta = p_int(init = 1L),
    alpha = p_lgl(init = TRUE),
    mu = p_dbl(init = 0.5)
  )
  collection = ParamSetCollection$new(list(owner = child))
  state = paradox:::param_set_core_state(
    collection$.__enclos_env__$private
  )

  expect_identical(state$.params$id, c(
    "owner.zeta", "owner.alpha", "owner.mu"
  ))
  expect_identical(collection$params$id, state$.params$id)
})

test_that("collection params read shared shadows live from one graph", {
  origin = ps(a = p_int(init = 1L), b = p_lgl(init = TRUE))
  shadow = ParamSetShadow$new(origin, character())
  collection = ParamSetCollection$new(list(left = shadow, right = shadow))

  origin$values = list(a = 4L)
  origin$add_dep("b", "a", CondEqual(4L))
  params = collection$params
  expect_identical(params$id, c("left.a", "left.b", "right.a", "right.b"))
  expect_identical(params$.init_given, c(TRUE, FALSE, TRUE, FALSE))
  expect_identical(params$.init[c(1L, 3L)], list(4L, 4L))
  expect_identical(params$.requirements[[2L]][[1L]], "left.a")
  expect_identical(params$.requirements[[4L]][[1L]], "right.a")
})

test_that("collection params detach every mutable output shell", {
  collection = ParamSetCollection$new(list(
    child = ps(x = p_int(init = 1L), y = p_dbl(-1, 1))
  ))
  first = collection$params
  second = collection$params
  expect_identical(first, second)
  expect_false(identical(data.table::address(first), data.table::address(second)))
  for (column in names(first)) {
    expect_false(identical(
      data.table::address(first[[column]]),
      data.table::address(second[[column]])
    ), info = column)
  }
  data.table::set(first, i = 1L, j = "lower", value = -100)
  first$.tags[[1L]] = "changed"
  expect_identical(collection$params, second)
})

test_that("collection params reject malformed graph translations", {
  collection = ParamSetCollection$new(list(child = ps(x = p_int(init = 1L))))
  private = collection$.__enclos_env__$private
  state = paradox:::param_set_core_state(private)
  translation = state$.translation
  translation$original_id[[1L]] = "not-x"
  paradox:::param_set_core_replace(private, translation = translation)
  expect_error(collection$params, "Corrupt ParamSetCollection")
})
