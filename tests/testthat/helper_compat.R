# compatibility to with broken testthat v3 behaviour
expect_equal = function(object, expected, ..., info = NULL, label = NULL) {
  expect_true(all.equal(object, expected, ...), info = info, label = label)
  expect_true(all.equal(object, expected, check.environment = FALSE, ...), info = info, label = label)
}