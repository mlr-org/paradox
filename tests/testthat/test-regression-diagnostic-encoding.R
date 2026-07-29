# Diagnostics are minted as UTF-8. A fragment that reaches them from outside --
# an unknown parameter name, or whatever a user `custom_check` returned -- may
# carry bytes that are not valid UTF-8 in the current locale. Those fragments
# are escaped, so the operation still reports its own diagnostic instead of an
# internal message-builder error, and the result really is the encoding it
# declares.

test_that("an unknown parameter name with invalid UTF-8 bytes still diagnoses", {
  invalid = "\xe9"
  skip_if(validUTF8(invalid), "the test locale accepts this byte as UTF-8")

  set = ps(alpha = p_dbl(0, 1), beta = p_dbl(0, 1))
  point = set_names(list(1), invalid)

  expect_identical(set$check(point), "Parameter '\\xe9' not available")
  expect_false(set$test(point))
  expect_error(set$values <- point, "Parameter '\\\\xe9' not available", fixed = FALSE)
  expect_error(set$subset(invalid), "unknown parameter '\\\\xe9'")
  expect_error(set$get_domain(invalid), "No param with id '\\\\xe9'")
})

test_that("declared encodings still take their documented path", {
  latin1 = "\xe9"
  Encoding(latin1) = "latin1"
  bytes = "\xe9"
  Encoding(bytes) = "bytes"

  set = ps(alpha = p_dbl(0, 1), beta = p_dbl(0, 1))
  expect_match(set$check(set_names(list(1), latin1)), "not available")
  expect_identical(
    set$check(set_names(list(1), bytes)),
    "Parameter '\\xe9' not available"
  )
  expect_error(set$subset(bytes), "Unknown bytes-encoded parameter ID")
  expect_error(set$get_domain(bytes), "Unknown bytes-encoded parameter ID")

  expect_identical(set$check(list(zzz = 1)), "Parameter 'zzz' not available")
})

test_that("a custom_check diagnostic is the encoding it declares", {
  invalid = "\xe9 rejected"
  skip_if(validUTF8(invalid), "the test locale accepts this byte as UTF-8")

  domain = p_uty(custom_check = function(x) invalid)
  result = domain_check(domain, list(1))

  expect_string(result)
  expect_true(validUTF8(result))
  expect_match(result, "\\\\xe9 rejected")
  # A declared-but-false encoding makes even nchar() fail.
  expect_gt(nchar(result), 0L)
})
