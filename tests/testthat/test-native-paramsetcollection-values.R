collection_values_symbol = function() {
  get(
    "C_param_set_collection_values",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

collection_values_native = function(x) {
  .Call(
    collection_values_symbol(),
    x$.__enclos_env__$private,
    x
  )
}

mixed_encoding_collection_values_fixture = function() {
  utf8_inner = enc2utf8("caf\u00e9")
  latin1_inner = iconv(utf8_inner, from = "UTF-8", to = "latin1")
  if (is.na(latin1_inner)) return(NULL)
  Encoding(latin1_inner) = "latin1"
  utf8_outer = enc2utf8(paste0("owner.", utf8_inner))
  latin1_outer = iconv(utf8_outer, from = "UTF-8", to = "latin1")
  if (is.na(latin1_outer)) return(NULL)
  Encoding(latin1_outer) = "latin1"

  child = ps(x = p_int(init = 1L))
  collection = ParamSetCollection$new(list(owner = child))
  child_private = child$.__enclos_env__$private
  child_state = paradox:::param_set_core_state(child_private)
  child_params = child_state$.params
  child_params$id[[1L]] = utf8_inner
  child_values = child_state$.values
  names(child_values) = utf8_inner
  paradox:::param_set_core_replace(
    child_private,
    params = child_params,
    values = child_values
  )

  private = collection$.__enclos_env__$private
  state = paradox:::param_set_core_state(private)
  params = state$.params
  params$id[[1L]] = latin1_outer
  translation = state$.translation
  translation$id[[1L]] = utf8_outer
  translation$original_id[[1L]] = latin1_inner
  # The forged flatten is a deliberate white-box state, so its edge record
  # must name the forged child generation too: otherwise the next read simply
  # re-flattens the collection and the hand-built encoding mix disappears.
  paradox:::param_set_core_replace(
    private,
    params = params,
    translation = translation,
    edges = list(
      cores = list(child_private$.core),
      tag_sets = FALSE,
      tag_params = FALSE,
      tag_override = NULL
    )
  )

  list(
    collection = collection,
    expected = setNames(list(1L), latin1_outer)
  )
}

test_that("collection values native entry has one fixed capsule interface", {
  symbol = collection_values_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("collection values preserve nested prefix/postfix order and NULL", {
  left = ps(
    zeta = p_int(init = 2L),
    alpha = p_lgl(init = FALSE),
    payload = p_uty()
  )
  left$values = list(zeta = 3L, alpha = TRUE, payload = NULL)
  right = ps(amount = p_dbl(init = 0.25))
  inner = ParamSetCollection$new(
    list(owner = left, other = right),
    postfix_names = TRUE
  )
  outer = ParamSetCollection$new(list(outer = inner))

  expected = list(
    outer.zeta.owner = 3L,
    outer.alpha.owner = TRUE,
    outer.payload.owner = NULL,
    outer.amount.other = 0.25
  )
  expect_identical(collection_values_native(outer), expected)
  expect_identical(outer$values, expected)
  expect_identical(
    ParamSetCollection$new(list())$values,
    setNames(list(), character())
  )
})

test_that("collection values use one live snapshot for shared DAG nodes", {
  shared = ps(x = p_int(init = 1L), y = p_lgl(init = TRUE))
  collection = ParamSetCollection$new(list(left = shared, right = shared))

  expect_identical(collection$values, list(
    left.x = 1L,
    left.y = TRUE,
    right.x = 1L,
    right.y = TRUE
  ))
  shared$values = list(x = 4L)
  expect_identical(collection$values, list(left.x = 4L, right.x = 4L))
})

test_that("collection values reuse admitted rows across nested shared paths", {
  child = ps(first = p_int(), second = p_lgl(), third = p_dbl())
  child_private = child$.__enclos_env__$private
  paradox:::param_set_core_replace(
    child_private,
    values = list(third = 3.5, first = 1L)
  )
  nested = ParamSetCollection$new(list(inner = child))
  collection = ParamSetCollection$new(list(left = nested, right = nested))

  expect_identical(collection$values, list(
    left.inner.third = 3.5,
    left.inner.first = 1L,
    right.inner.third = 3.5,
    right.inner.first = 1L
  ))
})

test_that("collection values refresh a shared ParamSetShadow live", {
  origin = ps(
    visible = p_int(init = 1L),
    flag = p_lgl(init = TRUE),
    hidden = p_dbl(init = 0.5)
  )
  shadow = ParamSetShadow$new(origin, "hidden")
  collection = ParamSetCollection$new(list(a = shadow, b = shadow))

  expect_identical(collection$values, list(
    a.visible = 1L,
    a.flag = TRUE,
    b.visible = 1L,
    b.flag = TRUE
  ))
  origin$values = list(visible = 5L, hidden = 0.75)
  expect_identical(collection$values, list(a.visible = 5L, b.visible = 5L))
})

test_that("collection stores do not couple translation and parameter row order", {
  make_child = function(shadow) {
    child = ps(hidden = p_int(), x = p_int(), flag = p_lgl())
    child$values = list(hidden = 9L, x = 1L, flag = TRUE)
    if (shadow) ParamSetShadow$new(child, "hidden") else child
  }

  for (postfix in c(FALSE, TRUE)) {
    for (shadow in c(FALSE, TRUE)) {
      collection = ParamSetCollection$new(
        list(view = make_child(shadow)),
        postfix_names = postfix
      )
      target = if (postfix) "flag.view" else "view.flag"
      collection$values = setNames(list(FALSE), target)
      expect_identical(collection$values[[target]], FALSE)
    }
  }

  inner = ParamSetCollection$new(list(
    a = ps(x = p_int()),
    b = ps(y = p_lgl())
  ))
  outer = ParamSetCollection$new(list(top = inner))
  outer$values = list(top.a.x = 3L, top.b.y = TRUE)
  expect_identical(outer$values, list(top.a.x = 3L, top.b.y = TRUE))
})

test_that("collection value shells detach while opaque leaves remain shallow", {
  marker = new.env(parent = emptyenv())
  child = ps(x = p_int(init = 1L), payload = p_uty())
  child$values = list(x = 2L, payload = marker)
  collection = ParamSetCollection$new(list(child = child))

  first = collection$values
  second = collection$values
  expect_identical(first, second)
  expect_false(identical(data.table::address(first), data.table::address(second)))
  expect_false(identical(
    data.table::address(names(first)),
    data.table::address(names(second))
  ))
  names(first)[[1L]] = "changed"
  first[[1L]] = 9L
  expect_identical(collection$values, second)
  expect_identical(first$child.payload, marker)
})

test_that("collection values reject malformed capsules and active-path cycles", {
  collection = ParamSetCollection$new(list())
  private = collection$.__enclos_env__$private
  paradox:::param_set_core_replace(private, sets = list(self = collection))
  expect_error(collection$values, "cycle", ignore.case = TRUE)

  valid = ParamSetCollection$new(list(child = ps(x = p_int(init = 1L))))
  private = valid$.__enclos_env__$private
  state = paradox:::param_set_core_state(private)
  bad = state$.translation
  bad$owner_ps_index[[1L]] = 99L
  paradox:::param_set_core_replace(private, translation = bad)
  # Value emission does not consume this private translation table.
  expect_identical(valid$values, list(child.x = 1L))
  expect_error(valid$subset("child.x"), "Corrupt ParamSetCollection")

  child = ps(x = p_int(init = 1L), y = p_lgl(init = TRUE))
  invalid_value = ParamSetCollection$new(list(child = child))
  child_private = child$.__enclos_env__$private
  paradox:::param_set_core_replace(
    child_private,
    values = setNames(list(1L), "missing")
  )
  expect_error(invalid_value$values, "Corrupt ParamSet")

  duplicate_translation = ParamSetCollection$new(list(
    child = ps(x = p_int(), y = p_lgl())
  ))
  duplicate_private = duplicate_translation$.__enclos_env__$private
  duplicate_state = paradox:::param_set_core_state(duplicate_private)
  bad = duplicate_state$.translation
  bad$id[[2L]] = bad$id[[1L]]
  paradox:::param_set_core_replace(duplicate_private, translation = bad)
  expect_identical(duplicate_translation$values, setNames(list(), character()))
})

test_that("collection values compare supported string encodings semantically", {
  fixture = mixed_encoding_collection_values_fixture()
  skip_if(
    is.null(fixture),
    "this platform cannot represent the latin1 fixture"
  )

  observed = fixture$collection$values
  expect_identical(observed, fixture$expected)
  expect_identical(Encoding(names(observed)), "latin1")
})

test_that("mixed-encoding collection value matching survives forced collection", {
  skip_on_cran()
  fixture = mixed_encoding_collection_values_fixture()
  skip_if(
    is.null(fixture),
    "this platform cannot represent the latin1 fixture"
  )

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed = fixture$collection$values
  gctorture(previous)

  expect_identical(observed, fixture$expected)
  expect_identical(Encoding(names(observed)), "latin1")
})
