test_that("unknown parameter suggestions use the misspelled ID and stay atomic", {
  param_set = ps(
    yes = p_int(),
    no = p_dbl(),
    nope = p_dbl()
  )
  param_set$values = list(yes = 1L, no = 2)
  before = param_set$values

  expect_identical(
    param_set$check(list(nop = 1)),
    "Parameter 'nop' not available. Did you mean 'nope' / 'no'?"
  )
  expect_identical(
    param_set$check(list(NOP = 1)),
    "Parameter 'NOP' not available. Did you mean 'nope' / 'no'?"
  )

  assignment_error = tryCatch(
    {
      param_set$values$nop = 1
      NULL
    },
    error = identity
  )
  expect_s3_class(assignment_error, "error")
  expect_identical(
    conditionMessage(assignment_error),
    paste0(
      "Assertion on 'xs' failed: Parameter 'nop' not available. ",
      # The hint already ends its own sentence, so the native assertion
      # wrapper adds no second terminator.
      "Did you mean 'nope' / 'no'?"
    )
  )
  expect_identical(param_set$values, before)

  merge_error = tryCatch(
    {
      param_set$set_values(yes = 2L, nop = 1)
      NULL
    },
    error = identity
  )
  expect_s3_class(merge_error, "error")
  expect_identical(
    conditionMessage(merge_error),
    paste0(
      "Assertion on 'xs' failed: Parameter 'nop' not available. ",
      # The hint already ends its own sentence, so the native assertion
      # wrapper adds no second terminator.
      "Did you mean 'nope' / 'no'?"
    )
  )
  expect_identical(param_set$values, before)
})

test_that("unknown parameter suggestions have a bounded stable candidate set", {
  tied = do.call(ps, setNames(
    lapply(seq_len(4L), function(index) p_int()),
    c("xbcde", "aycde", "abzde", "abcxe")
  ))

  expect_identical(
    tied$check(list(abcde = 1L)),
    paste0(
      "Parameter 'abcde' not available. Did you mean ",
      "'xbcde' / 'aycde' / 'abzde'?"
    )
  )

  threshold = ps(abxde = p_int(), abcxf = p_int())
  expect_identical(
    threshold$check(list(abcde = 1L)),
    "Parameter 'abcde' not available. Did you mean 'abxde'?"
  )
  expect_identical(
    ps(yes = p_int(), no = p_int(), nope = p_int())$check(
      list(banana = 1L)
    ),
    "Parameter 'banana' not available"
  )
})

test_that("collection prefixes have low suggestion cost", {
  pca = ps(
    center = p_lgl(init = TRUE),
    scale = p_lgl(init = TRUE),
    rank = p_int(init = 2L)
  )
  learner = ps(nrounds = p_int(init = 5L))
  collection = ParamSetCollection$new(list(
    pca = pca,
    classif.xgboost = learner
  ))

  for (query in c("nrounds", "nround", "NROUNDS")) {
    diagnostic = collection$check(setNames(list(10L), query))
    expect_identical(
      diagnostic,
      paste0(
        "Parameter '", query, "' not available. Did you mean ",
        "'classif.xgboost.nrounds'?"
      )
    )
    expect_false(grepl("pca\\.", diagnostic))
  }

  before = collection$values
  error = tryCatch(
    {
      collection$set_values(pca.center = FALSE, nrounds = 10L)
      NULL
    },
    error = identity
  )
  expect_s3_class(error, "error")
  expect_match(
    conditionMessage(error),
    "Did you mean 'classif.xgboost.nrounds'?",
    fixed = TRUE
  )
  expect_false(grepl("pca\\.", conditionMessage(error)))
  expect_identical(collection$values, before)

  nested = ParamSetCollection$new(list(graph = collection))
  expect_identical(
    nested$check(list(nround = 10L)),
    paste0(
      "Parameter 'nround' not available. Did you mean ",
      "'graph.classif.xgboost.nrounds'?"
    )
  )
})

test_that("Shadow suggestions expose only its visible schema and stay atomic", {
  origin = ps(
    hidden = p_int(),
    visible = p_int(),
    value = p_int()
  )
  origin$values = list(hidden = 9L, visible = 1L, value = 2L)
  shadow = ParamSetShadow$new(origin, "hidden")

  expect_identical(
    shadow$check(list(visibl = 2L)),
    "Parameter 'visibl' not available. Did you mean 'visible'?"
  )
  expect_identical(
    shadow$check(list(hidde = 2L)),
    "Parameter 'hidde' not available"
  )

  before = origin$values
  error = tryCatch(
    {
      shadow$set_values(value = 3L, visibl = 2L)
      NULL
    },
    error = identity
  )
  expect_s3_class(error, "error")
  expect_identical(
    conditionMessage(error),
    paste0(
      "Parameter 'visibl' not available in ParamSetShadow. ",
      "Did you mean 'visible'?"
    )
  )
  expect_false(grepl("'hidden'", conditionMessage(error), fixed = TRUE))
  expect_identical(origin$values, before)
})

test_that("suggestion distance counts text characters across encodings", {
  query = iconv("caf\u00e9", from = "UTF-8", to = "latin1")
  skip_if(
    is.na(query),
    "this platform cannot represent the latin1 fixture"
  )
  Encoding(query) = "latin1"

  diagnostic = ps(cafe = p_int())$check(setNames(list(1L), query))
  expect_identical(
    enc2utf8(diagnostic),
    "Parameter 'caf\u00e9' not available. Did you mean 'cafe'?"
  )
})
