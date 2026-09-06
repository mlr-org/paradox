test_that("rejection sampling counts only known in-range draws", {
  sampler = Sampler1DRfun$new(
    ps(x = p_dbl(0, 1)),
    function(n) rep(c(NA_real_, NaN, -1, 0, 0.5, 1, 2), length.out = n)
  )
  expect_identical(sampler$sample(4L)$data$x, c(0, 0.5, 1, 0))
})

test_that("rejection sampling preserves batch sizes, order, and RNG state", {
  for (upper in c(1, 0.03)) {
    reference = function(n) {
      accepted = numeric()
      for (iteration in seq_len(1000L)) {
        draw = runif(2 * n)
        accepted = c(accepted, draw[draw <= upper])
        if (length(accepted) >= n) return(accepted[seq_len(n)])
      }
      stop("test reference exhausted")
    }
    set.seed(9031)
    expected = reference(31L)
    expected_seed = .Random.seed
    sampler = Sampler1DRfun$new(ps(x = p_dbl(0, upper)), runif)
    set.seed(9031)
    observed = sampler$sample(31L)$data$x
    expect_identical(observed, expected)
    expect_identical(.Random.seed, expected_seed)
  }
})

test_that("empty and entirely missing rejection batches remain bounded", {
  for (draw in list(numeric(), c(NA_real_, NaN))) {
    calls = 0L
    batch_sizes_ok = TRUE
    sampler = Sampler1DRfun$new(ps(x = p_dbl(0, 1)), function(n) {
      calls <<- calls + 1L
      batch_sizes_ok <<- batch_sizes_ok && identical(n, 2)
      draw
    })
    expect_identical(sampler$sample(0L)$data$x, numeric())
    expect_identical(calls, 0L)
    expect_error(sampler$sample(1L), "Tried rejection sampling 1000x", fixed = TRUE)
    expect_identical(calls, 1000L)
    expect_true(batch_sizes_ok)
  }
})

test_that("truncated draws include the closed endpoints", {
  sampler = Sampler1DRfun$new(ps(x = p_dbl(-Inf, Inf)), function(n) {
    rep(c(-Inf, Inf, NA_real_), length.out = n)
  })
  expect_identical(sampler$sample(3L)$data$x, c(-Inf, Inf, -Inf))
})

test_that("default normal scale stays finite for wide finite intervals", {
  sampler = Sampler1DNormal$new(ps(x = p_dbl(-1e308, 1e308)))
  expect_identical(unname(sampler$mean), 0)
  # Computing the scale and parsing its huge decimal spelling can differ by
  # an ULP across platforms. Scale the comparison to avoid overflow as well.
  expect_equal(unname(sampler$sd) / 5e307, 1, tolerance = 4 * .Machine$double.eps)
  set.seed(9032)
  values = sampler$sample(32L)$data$x
  expect_true(all(is.finite(values)))
  expect_true(all(values >= -1e308 & values <= 1e308))

  ordinary = Sampler1DNormal$new(ps(x = p_dbl(-2, 6)))
  expect_identical(unname(ordinary$mean), 2)
  expect_identical(unname(ordinary$sd), 2)
})
