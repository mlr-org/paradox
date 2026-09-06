# checkmate's convention, which `$assert()` relies on through
# `checkmate::makeAssertion()`, is that a `check_*` result is an unpunctuated
# sentence fragment and the assertion wrapper supplies the terminator. Paradox's
# own fragments carried their own period, so every one of them produced
# `... not available..` once an assertion wrapped it.

expect_no_double_terminator = function(text) {
  expect_false(grepl("[.?!][.]$", text), info = text)
}

test_that("check fragments carry no sentence terminator", {
  p = ps(alpha = p_dbl(0, 1), beta = p_dbl(0, 1))

  expect_equal(p$check(list(zzzzzz = 1)), "Parameter 'zzzzzz' not available")
  expect_equal(p$check(setNames(list(1, 2), c("alpha", "alpha"))),
    "Names must be unique")
  expect_equal(p$check(list(1)), "Must be a named list")
  expect_equal(p$check(1), "Must be a list, not 'double'")

  # A fragment that ends its own sentence keeps its question mark: it is two
  # sentences, and the separating period is the one this diagnostic owns.
  expect_equal(p$check(list(alpah = 1)),
    "Parameter 'alpah' not available. Did you mean 'alpha'?")
})

test_that("assertions terminate a fragment exactly once", {
  p = ps(alpha = p_dbl(0, 1))

  for (input in list(list(zzzzzz = 1), setNames(list(1, 2), c("alpha", "alpha")),
      list(1), 1)) {
    message = tryCatch(p$assert(input), error = conditionMessage)
    expect_match(message, "^Assertion on ")
    expect_no_double_terminator(message)
  }

  # checkmate-derived fragments were already unpunctuated and must stay so.
  expect_no_double_terminator(
    tryCatch(p$assert(list(alpha = 5)), error = conditionMessage)
  )
})

test_that("the native assertion wrapper terminates conditionally", {
  # `$values<-` uses paradox's own wrapper rather than checkmate's, so this is
  # the path where the conditional terminator is actually decided natively.
  p = ps(alpha = p_dbl(0, 1))
  message = tryCatch({p$values = list(zzzzzz = 1); NA_character_},
    error = conditionMessage)
  expect_match(message, "^Assertion on ")
  expect_no_double_terminator(message)
  expect_match(message, "not available[.]$")

  # A fragment already ending in a question mark gets no second terminator.
  suggestion = tryCatch({p$values = list(alpah = 1); NA_character_},
    error = conditionMessage)
  expect_no_double_terminator(suggestion)
  expect_match(suggestion, "Did you mean 'alpha'[?]$")
})

test_that("shadow and table diagnostics keep the convention", {
  origin = ps(alpha = p_dbl(0, 1), beta = p_dbl(0, 1))
  view = ParamSetShadow$new(origin, "beta")
  expect_error(view$values <- list(beta = 0.5),
    "not available in ParamSetShadow")
  expect_no_double_terminator(
    tryCatch({view$values = list(beta = 0.5); NA_character_},
      error = conditionMessage)
  )

  p = ps(alpha = p_dbl(0, 1))
  unnamed = setNames(data.frame(alpha = 0.5), NULL)
  expect_equal(p$check_dt(unnamed), "Table columns must be named")
  expect_no_double_terminator(
    tryCatch(p$assert_dt(unnamed), error = conditionMessage)
  )
  expect_equal(p$check_dt(1), "Must be a data.frame or data.table")
})
