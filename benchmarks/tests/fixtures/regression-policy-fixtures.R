benchmark_regression_test_fixture <- function() {
  cases <- c(
    "pass_stable", "marginal_noisy", "fail_timing",
    "pass_zero_allocation", "fail_zero_baseline_allocation",
    "margin_integrity_shadow", "margin_integrity_collection",
    "fail_integrity_collection"
  )
  policy <- data.frame(
    scope = rep("paired", length(cases)),
    case = cases,
    operation = rep("-", length(cases)),
    tier = c(
      rep("hot", 5L), "integrity-shadow-read",
      "integrity-collection-read", "integrity-collection-read"
    ),
    rationale = rep("deterministic-policy-fixture", length(cases)),
    stringsAsFactors = FALSE
  )
  baseline_pattern <- rep(c(
    96, 98, 99, 100, 101, 102, 104, 97, 103, 100
  ), 6L)
  candidate_patterns <- list(
    pass_stable = baseline_pattern * 1.03,
    marginal_noisy = baseline_pattern * rep(c(
      0.88, 0.96, 1.02, 1.08, 1.12, 1.16, 1.24, 1.32, 1.40, 1.08
    ), 6L),
    fail_timing = baseline_pattern * 1.50,
    pass_zero_allocation = baseline_pattern,
    fail_zero_baseline_allocation = baseline_pattern,
    margin_integrity_shadow = baseline_pattern * 4.00,
    margin_integrity_collection = baseline_pattern * 2.40,
    fail_integrity_collection = baseline_pattern * 3.30
  )
  make_samples <- function(patterns) {
    do.call(rbind, lapply(cases, function(case) {
      elapsed <- patterns[[case]]
      data.frame(
        scope = rep("paired", length(elapsed)),
        case = rep(case, length(elapsed)),
        operation = rep("-", length(elapsed)),
        iteration = seq_along(elapsed),
        elapsed_ns = elapsed,
        stringsAsFactors = FALSE
      )
    }))
  }
  baseline_patterns <- setNames(
    rep(list(baseline_pattern), length(cases)), cases
  )
  baseline_allocations <- c(4096, 4096, 4096, 0, 0, 4096, 4096, 4096)
  candidate_allocations <- c(4096, 4096, 4096, 1024, 32768, 4096, 4096, 4096)
  allocation_table <- function(values) {
    data.frame(
      scope = rep("paired", length(cases)),
      case = cases,
      operation = rep("-", length(cases)),
      mem_alloc_bytes = values,
      stringsAsFactors = FALSE
    )
  }
  list(
    policy = policy,
    baseline_samples = make_samples(baseline_patterns),
    candidate_samples = make_samples(candidate_patterns),
    baseline_allocations = allocation_table(baseline_allocations),
    candidate_allocations = allocation_table(candidate_allocations),
    expected_decisions = c(
      "pass", "marginal", "fail", "pass", "fail",
      "marginal", "marginal", "fail"
    )
  )
}
