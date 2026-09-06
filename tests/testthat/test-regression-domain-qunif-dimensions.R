test_that("domain_qunif rejects values with incompatible dimensions", {
  domain = ps(
    x = p_dbl(0, 1),
    y = p_dbl(0, 1)
  )$params
  data.table::setattr(domain, "class", c("ParamDbl", "Domain", class(domain)))

  expect_equal(domain_qunif(domain, c(0, 0.25, 0.5, 1)), c(0, 0.25, 0.5, 1))
  expect_error(domain_qunif(domain, c(0, 0.25, 0.5)), "must be a multiple")
})
