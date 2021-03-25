
# compatibility to with broken testthat v3 behaviour
expect_equal = function(object, expected, ..., info = NULL, label = NULL) {
  expect_true(all.equal(object, expected, check.environment = FALSE, ...), info = info, label = label)
}

# suppress warnings as long as half the world still uses v2
context = function(...) suppressWarnings(testthat::context(...))
expect_is = function(...) suppressWarnings(testthat::expect_is(...))
expect_equivalent = function(...) suppressWarnings(testthat::expect_equivalent(...))
library("checkmate")


ParamInt = list(
  new = function(id, ...) {
    ParamSet$new(set_names(list(p_int(...)), id))
  }
)

ParamDbl = list(
  new = function(id, ...) {
    ParamSet$new(set_names(list(p_dbl(...)), id))
  }
)

ParamFct = list(
  new = function(id, ...) {
    ParamSet$new(set_names(list(p_fct(...)), id))
  }
)

ParamLgl = list(
  new = function(id, ...) {
    ParamSet$new(set_names(list(p_lgl(...)), id))
  }
)

ParamUty = list(
  new = function(id, ...) {
    ParamSet$new(set_names(list(p_uty(...)), id))
  }
)

ParamSet_legacy = list(
  new = function(params) {
    ps_union(params)
  }
)
