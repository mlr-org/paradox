informative_value_cases = function() {
  list(
    dbl_type = list(
      domain = p_dbl(0, 1, tolerance = 0),
      value = "wrong",
      reason = "Must be of type 'number', not 'character'"
    ),
    dbl_length = list(
      domain = p_dbl(0, 1, tolerance = 0),
      value = c(0, 1),
      reason = "Must have length 1"
    ),
    dbl_missing = list(
      domain = p_dbl(0, 1, tolerance = 0),
      value = NA_real_,
      reason = "May not be NA"
    ),
    dbl_lower = list(
      domain = p_dbl(0, 1, tolerance = 0),
      value = -1,
      reason = "Element 1 is not >= 0"
    ),
    dbl_upper = list(
      domain = p_dbl(0, 1, tolerance = 0),
      value = 2,
      reason = "Element 1 is not <= 1"
    ),
    dbl_infinite_lower = list(
      domain = p_dbl(Inf, Inf, tolerance = 0),
      value = 0,
      reason = "Element 1 is not >= Inf"
    ),
    dbl_infinite_upper = list(
      domain = p_dbl(-Inf, -Inf, tolerance = 0),
      value = 0,
      reason = "Element 1 is not <= -Inf"
    ),
    int_type = list(
      domain = p_int(0, 5, tolerance = 0),
      value = "wrong",
      reason = "Must be of type 'single integerish value', not 'character'"
    ),
    int_length = list(
      domain = p_int(0, 5, tolerance = 0),
      value = c(1L, 2L),
      reason = "Must have length 1"
    ),
    int_missing = list(
      domain = p_int(0, 5, tolerance = 0),
      value = NA_integer_,
      reason = "May not be NA"
    ),
    int_noninteger = list(
      domain = p_int(0, 5, tolerance = 0),
      value = 0.5,
      reason = "Must be of type 'single integerish value', not 'double'"
    ),
    int_outside_storage = list(
      domain = p_int(),
      value = .Machine$integer.max + 1,
      reason = "Must be of type 'single integerish value', not 'double'"
    ),
    int_lower = list(
      domain = p_int(0, 5, tolerance = 0),
      value = -1L,
      reason = "Element 1 is not >= -0.5"
    ),
    int_upper = list(
      domain = p_int(0, 5, tolerance = 0),
      value = 6L,
      reason = "Element 1 is not <= 5.5"
    ),
    fct_type = list(
      domain = p_fct(c("a", "b")),
      value = 1L,
      reason = paste0(
        "Must be element of set {'a','b'}, ",
        "but types do not match (integer != character)"
      )
    ),
    fct_length = list(
      domain = p_fct(c("a", "b")),
      value = c("a", "b"),
      reason = "Must be element of set {'a','b'}, but is not atomic scalar"
    ),
    fct_missing = list(
      domain = p_fct(c("a", "b")),
      value = NA_character_,
      reason = "Must be element of set {'a','b'}, but is 'NA'"
    ),
    fct_level = list(
      domain = p_fct(c("a", "b")),
      value = "outside",
      reason = "Must be element of set {'a','b'}, but is 'outside'"
    ),
    fct_null = list(
      domain = p_fct(c("a", "b")),
      value = NULL,
      reason = "Must be element of set {'a','b'}, but is NULL"
    ),
    lgl_type = list(
      domain = p_lgl(),
      value = 1L,
      reason = "Must be of type 'logical flag', not 'integer'"
    ),
    lgl_length = list(
      domain = p_lgl(),
      value = c(TRUE, FALSE),
      reason = "Must have length 1"
    ),
    lgl_missing = list(
      domain = p_lgl(),
      value = NA,
      reason = "May not be NA"
    )
  )
}

test_that("Domain and ParamSet checks share informative scalar diagnostics", {
  cases = informative_value_cases()
  for (label in names(cases)) {
    case = cases[[label]]
    domain_id = case$domain$id[[1L]]

    expect_identical(
      domain_check(case$domain, list(case$value)),
      paste0(domain_id, ": ", case$reason),
      info = label
    )
    expect_identical(
      ps(value = case$domain)$check(list(value = case$value)),
      paste0("value: ", case$reason),
      info = label
    )
  }
})

test_that("native formatted diagnostics grow real buffers without truncation", {
  domain = p_dbl(0, 1, tolerance = 0)
  param_set = ps(value = domain)
  lower = -.Machine$double.xmax
  upper = .Machine$double.xmax
  token = to_tune(lower = lower, upper = upper)
  rendered_bounds = paste0("lower ", sprintf("%g, upper %g", lower, upper))

  returned = param_set$check(list(value = token))
  thrown = tryCatch(
    {
      param_set$values = list(value = token)
      NULL
    },
    error = conditionMessage
  )

  expect_match(returned, rendered_bounds, fixed = TRUE)
  expect_match(thrown, rendered_bounds, fixed = TRUE)
  expect_match(thrown, paste0(rendered_bounds, "."), fixed = TRUE)
})

test_that("scalar missingness takes precedence over irrelevant storage type", {
  cases = list(
    double = list(
      domain = p_dbl(),
      values = list(NA, NA_character_),
      reason = "May not be NA"
    ),
    integer = list(
      domain = p_int(),
      values = list(NA, NA_character_),
      reason = "May not be NA"
    ),
    factor = list(
      domain = p_fct(c("a", "b")),
      values = list(NA, NA_integer_, NA_real_, NA_complex_),
      reason = "Must be element of set {'a','b'}, but is 'NA'"
    ),
    logical = list(
      domain = p_lgl(),
      values = list(NA_integer_, NA_real_, NA_character_),
      reason = "May not be NA"
    )
  )

  for (label in names(cases)) {
    case = cases[[label]]
    domain_id = case$domain$id[[1L]]
    param_set = ps(value = case$domain)
    for (value in case$values) {
      expect_identical(
        domain_check(case$domain, list(value)),
        paste0(domain_id, ": ", case$reason),
        info = paste(label, typeof(value))
      )
      expect_identical(
        param_set$check(list(value = value)),
        paste0("value: ", case$reason),
        info = paste(label, typeof(value))
      )
    }
  }
})

test_that("Domain defaults and initial values report their precise failure", {
  expect_identical(
    conditionMessage(tryCatch(
      p_dbl(0, 1, tolerance = 0, default = 2),
      error = identity
    )),
    paste0(
      "Assertion on 'param' failed: ",
      "p_dbl(lower = 0, upper = 1, default = 2, tolerance = 0): ",
      "Element 1 is not <= 1."
    )
  )
  expect_identical(
    conditionMessage(tryCatch(
      p_int(0, 5, tolerance = 0, init = 0.5),
      error = identity
    )),
    paste0(
      "Assertion on 'param' failed: ",
      "p_int(lower = 0, upper = 5, tolerance = 0, init = 0.5): ",
      "Must be of type 'single integerish value', not 'double'."
    )
  )
  expect_identical(
    conditionMessage(tryCatch(
      p_fct(c("a", "b"), default = "outside"),
      error = identity
    )),
    paste0(
      "Assertion on 'param' failed: ",
      "p_fct(levels = c(\"a\", \"b\"), default = \"outside\"): ",
      "Must be element of set {'a','b'}, but is 'outside'."
    )
  )
  expect_identical(
    conditionMessage(tryCatch(
      p_lgl(init = NA),
      error = identity
    )),
    "Assertion on 'param' failed: p_lgl(init = NA): May not be NA."
  )
})

test_that("checked value assignment preserves the diagnostic and is atomic", {
  param_set = ps(
    number = p_dbl(0, 1, tolerance = 0),
    count = p_int(0, 5, tolerance = 0)
  )
  param_set$values = list(number = 0.5, count = 1L)

  assignment_error = tryCatch(
    {
      param_set$values = list(number = 0.75, count = 6L)
      NULL
    },
    error = identity
  )
  expect_s3_class(assignment_error, "error")
  expect_identical(
    conditionMessage(assignment_error),
    paste0(
      "Assertion on 'xs' failed: ",
      "count: Element 1 is not <= 5.5."
    )
  )
  expect_identical(param_set$values, list(number = 0.5, count = 1L))

  missing_error = tryCatch(
    {
      param_set$values = list(number = 0.75, count = NA)
      NULL
    },
    error = identity
  )
  expect_s3_class(missing_error, "error")
  expect_identical(
    conditionMessage(missing_error),
    "Assertion on 'xs' failed: count: May not be NA."
  )
  expect_identical(param_set$values, list(number = 0.5, count = 1L))
})

test_that("table checks use the same scalar failure reasons", {
  cases = list(
    dbl_bound = list(
      domain = p_dbl(0, 1, tolerance = 0),
      column = c(0.5, 2),
      reason = "Element 1 is not <= 1"
    ),
    int_noninteger = list(
      domain = p_int(0, 5, tolerance = 0),
      column = c(1, 0.5),
      reason = "Must be of type 'single integerish value', not 'double'"
    ),
    fct_level = list(
      domain = p_fct(c("a", "b")),
      column = c("a", "outside"),
      reason = "Must be element of set {'a','b'}, but is 'outside'"
    ),
    lgl_type = list(
      domain = p_lgl(),
      column = c(1L, 0L),
      reason = "Must be of type 'logical flag', not 'integer'"
    )
  )

  for (label in names(cases)) {
    case = cases[[label]]
    param_set = ps(value = case$domain)
    table = data.table::data.table(value = case$column)
    expect_identical(
      param_set$check_dt(table),
      paste0("value: ", case$reason),
      info = label
    )
  }
})

test_that("special values bypass typed checks without creating a fallback", {
  domain = p_dbl(
    0,
    1,
    tolerance = 0,
    special_vals = list("automatic")
  )
  param_set = ps(value = domain)

  expect_true(domain_check(domain, list("automatic")))
  expect_true(param_set$check(list(value = "automatic")))
  expect_identical(
    domain_check(domain, list("automatic"), internal = TRUE),
    paste0(
      domain$id[[1L]],
      ": Must be of type 'number', not 'character'"
    )
  )
})

test_that("classed and S4 typed values fail with useful native types", {
  classed = structure(0.5, class = "quantity")
  s4 = asS4(0.5)
  domain = p_dbl(0, 1, tolerance = 0)
  param_set = ps(value = domain)

  expect_identical(
    domain_check(domain, list(classed)),
    paste0(
      domain$id[[1L]],
      ": Must be of type 'number', not 'quantity'"
    )
  )
  expect_identical(
    param_set$check(list(value = classed)),
    "value: Must be of type 'number', not 'quantity'"
  )
  expect_identical(
    domain_check(domain, list(s4)),
    paste0(
      domain$id[[1L]],
      ": Must be of type 'number', not 'double'"
    )
  )
  expect_identical(
    param_set$check(list(value = s4)),
    "value: Must be of type 'number', not 'double'"
  )
})

test_that("factor diagnostics preserve text and escape bytes safely", {
  utf8 = enc2utf8("caf\u00e9")
  domain = p_fct(c(utf8, "it's", "slash\\"))
  param_set = ps(value = domain)

  text = param_set$check(list(value = "outside"))
  expect_identical(Encoding(text), "UTF-8")
  expect_match(text, utf8, fixed = TRUE)
  expect_match(text, "it\\'s", fixed = TRUE)
  expect_match(text, "slash\\\\", fixed = TRUE)
  expect_match(text, "but is 'outside'", fixed = TRUE)

  bytes = rawToChar(as.raw(c(0x66, 0xff)))
  Encoding(bytes) = "bytes"
  bytes_text = param_set$check(list(value = bytes))
  expect_identical(Encoding(bytes_text), "UTF-8")
  expect_match(bytes_text, "but is 'f\\xff'", fixed = TRUE)
})

test_that("Collection and Shadow checks retain visible translated IDs", {
  child = ps(value = p_dbl(0, 1, tolerance = 0))
  collection = ParamSetCollection$new(list(block = child))
  expect_identical(
    collection$check(list(block.value = 2)),
    "block.value: Element 1 is not <= 1"
  )

  origin = ps(
    hidden = p_int(),
    visible = p_dbl(0, 1, tolerance = 0)
  )
  shadow = ParamSetShadow$new(origin, "hidden")
  expect_identical(
    shadow$check(list(visible = 2)),
    "visible: Element 1 is not <= 1"
  )
})
