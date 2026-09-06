test_that("grid axes deduplicate nonadjacent rounded quantiles", {
  eps = .Machine$double.eps
  intervals = list(c(1, 1 + eps), c(1, 1 + 2 * eps),
    c(0.1, 0.1 + 2e-17), c(-1 - eps, -1))
  for (bounds in intervals) {
    domain = p_dbl(bounds[[1L]], bounds[[2L]])
    expected = unique(domain_qunif(domain, seq(0, 1, length.out = 101L)))
    param_set = ps(x = domain)
    observed = generate_design_grid(param_set, 101L,
      upper_limit = length(expected))$data$x
    expect_identical(observed, expected)
    expect_identical(anyDuplicated(observed), 0L)
    expect_error(generate_design_grid(param_set, 101L,
      upper_limit = length(expected) - 1L), "upper_limit")
  }
})

test_that("rounded dependent grids retain first complete-product occurrence", {
  param_set = ps(
    jitter = p_dbl(1, 1 + .Machine$double.eps),
    gate = p_lgl(),
    fixed = p_int(0, 4)
  )
  param_set$add_dep("jitter", "gate", CondEqual(TRUE))
  param_set$values = list(fixed = 3L)
  mapped = domain_qunif(p_dbl(1, 1 + .Machine$double.eps),
    seq(0, 1, length.out = 101L))
  expected = data.table::CJ(jitter = mapped, gate = c(TRUE, FALSE),
    fixed = 0:4, sorted = FALSE)
  expected$jitter[!expected$gate] = NA_real_
  expected$fixed = rep(3L, nrow(expected))
  expected = unique(expected)
  observed = generate_design_grid(param_set, 101L,
    upper_limit = nrow(expected))$data
  expect_identical(observed, expected)
})

test_that("axis equality retains infinite, NaN, and signed-zero semantics", {
  intervals = list(c(-Inf, Inf), c(-Inf, 0), c(0, Inf),
    c(-Inf, -Inf), c(Inf, Inf), c(-0, 0))
  for (bounds in intervals) {
    domain = p_dbl(bounds[[1L]], bounds[[2L]])
    expected = unique(domain_qunif(domain, seq(0, 1, length.out = 17L)))
    observed = generate_design_grid(ps(x = domain), 17L,
      upper_limit = length(expected))$data$x
    expect_identical(observed, expected)
  }
})
