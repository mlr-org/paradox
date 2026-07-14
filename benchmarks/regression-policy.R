# Pure, dependency-free release benchmark regression policy.  The release
# driver sources this file in a fresh environment and authenticates its bytes
# as one of the sealed policy inputs.

benchmark_regression_policy_schema <- 1L

benchmark_regression_policy_spec <- function() {
  list(
    schema = benchmark_regression_policy_schema,
    minimum_samples = 50L,
    bootstrap_replicates = 2000L,
    confidence = 0.95,
    marginal_fraction = 0.50,
    marginal_probability = 0.55,
    tiers = data.frame(
      tier = c("hot", "standard"),
      median_ratio_limit = c(1.20, 1.35),
      q75_ratio_limit = c(1.35, 1.60),
      slower_probability_limit = c(0.75, 0.80),
      allocation_ratio_limit = c(1.25, 1.50),
      allocation_min_delta_bytes = c(16384, 65536),
      stringsAsFactors = FALSE
    )
  )
}

benchmark_regression_required_policy_columns <- function() {
  c("scope", "case", "operation", "tier", "rationale")
}

benchmark_regression_key <- function(scope, case, operation) {
  paste(scope, case, operation, sep = "\037")
}

benchmark_regression_expected_inventory <- function(workloads) {
  consumer_cases <- c(
    "mies_mutator_maybe", "mies_optimizer", "mlr3pipelines_graph"
  )
  consumer_operations <- c("params", "values", "get_values_unchecked")
  rbind(
    data.frame(
      scope = rep("paired", length(workloads)),
      case = workloads,
      operation = rep("-", length(workloads)),
      stringsAsFactors = FALSE
    ),
    data.frame(
      scope = rep("consumer", length(consumer_cases) * length(consumer_operations)),
      case = rep(consumer_cases, each = length(consumer_operations)),
      operation = rep(consumer_operations, times = length(consumer_cases)),
      stringsAsFactors = FALSE
    )
  )
}

benchmark_regression_validate_spec <- function(spec) {
  required_names <- c(
    "schema", "minimum_samples", "bootstrap_replicates", "confidence",
    "marginal_fraction", "marginal_probability", "tiers"
  )
  if (!identical(names(spec), required_names) ||
      !identical(spec$schema, benchmark_regression_policy_schema) ||
      length(spec$minimum_samples) != 1L || spec$minimum_samples < 20L ||
      length(spec$bootstrap_replicates) != 1L ||
        spec$bootstrap_replicates < 500L ||
      length(spec$confidence) != 1L ||
        spec$confidence <= 0.5 || spec$confidence >= 1 ||
      length(spec$marginal_fraction) != 1L ||
        spec$marginal_fraction <= 0 || spec$marginal_fraction >= 1 ||
      length(spec$marginal_probability) != 1L ||
        spec$marginal_probability <= 0.5 || spec$marginal_probability >= 1) {
    stop("invalid benchmark regression policy specification", call. = FALSE)
  }
  expected_tier_columns <- c(
    "tier", "median_ratio_limit", "q75_ratio_limit",
    "slower_probability_limit", "allocation_ratio_limit",
    "allocation_min_delta_bytes"
  )
  tiers <- spec$tiers
  if (!is.data.frame(tiers) || !identical(names(tiers), expected_tier_columns) ||
      !nrow(tiers) || anyDuplicated(tiers$tier) ||
      any(!nzchar(tiers$tier)) ||
      any(!is.finite(as.matrix(tiers[-1L]))) ||
      any(tiers$median_ratio_limit <= 1) ||
      any(tiers$q75_ratio_limit <= tiers$median_ratio_limit) ||
      any(tiers$slower_probability_limit <= 0.5) ||
      any(tiers$slower_probability_limit >= 1) ||
      any(tiers$allocation_ratio_limit <= 1) ||
      any(tiers$allocation_min_delta_bytes <= 0)) {
    stop("invalid benchmark regression policy tiers", call. = FALSE)
  }
  invisible(spec)
}

benchmark_regression_validate_policy <- function(policy, spec) {
  required_columns <- benchmark_regression_required_policy_columns()
  if (!is.data.frame(policy) || !identical(names(policy), required_columns) ||
      !nrow(policy) || anyNA(policy) || any(!nzchar(as.matrix(policy))) ||
      any(grepl("[\r\n\t]", as.matrix(policy))) ||
      any(!policy$scope %in% c("paired", "consumer")) ||
      any(!policy$tier %in% spec$tiers$tier) ||
      anyDuplicated(benchmark_regression_key(
        policy$scope, policy$case, policy$operation
      ))) {
    stop("invalid benchmark regression policy table", call. = FALSE)
  }
  invisible(policy)
}

benchmark_regression_read_policy <- function(path, workloads) {
  spec <- benchmark_regression_policy_spec()
  benchmark_regression_validate_spec(spec)
  policy <- utils::read.delim(
    path, header = TRUE, quote = "", comment.char = "",
    stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character", na.strings = NULL
  )
  benchmark_regression_validate_policy(policy, spec)
  keys <- benchmark_regression_key(policy$scope, policy$case, policy$operation)
  expected <- benchmark_regression_expected_inventory(workloads)
  expected_keys <- benchmark_regression_key(
    expected$scope, expected$case, expected$operation
  )
  missing <- setdiff(expected_keys, keys)
  extra <- setdiff(keys, expected_keys)
  if (length(missing) || length(extra) || !identical(keys, expected_keys)) {
    display <- function(values) {
      if (length(values)) paste(values, collapse = ", ") else "-"
    }
    stop(
      paste0(
        "regression policy does not exactly cover the ordered benchmark inventory; ",
        "missing=[", display(missing), "], extra=[", display(extra), "]"
      ),
      call. = FALSE
    )
  }
  rownames(policy) <- NULL
  policy
}

benchmark_regression_preserve_rng <- function(seed, expression) {
  global <- globalenv()
  had_seed <- exists(".Random.seed", envir = global, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = global, inherits = FALSE)
  }
  old_kind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = global)
    } else if (exists(".Random.seed", envir = global, inherits = FALSE)) {
      rm(".Random.seed", envir = global)
    }
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  force(expression)
}

benchmark_regression_seed <- function(key) {
  bytes <- utf8ToInt(enc2utf8(key))
  weights <- (seq_along(bytes) * 104729) %% 2147483629
  as.integer((20260714 + sum(bytes * weights)) %% 2147483646) + 1L
}

benchmark_regression_ratio <- function(numerator, denominator) {
  if (denominator == 0) {
    if (numerator == 0) 1 else Inf
  } else {
    numerator / denominator
  }
}

benchmark_regression_bootstrap <- function(
  baseline, candidate, key, replicates, confidence
) {
  draws <- benchmark_regression_preserve_rng(
    benchmark_regression_seed(key),
    {
      result <- matrix(NA_real_, nrow = replicates, ncol = 2L)
      for (index in seq_len(replicates)) {
        baseline_draw <- baseline[
          sample.int(length(baseline), length(baseline), replace = TRUE)
        ]
        candidate_draw <- candidate[
          sample.int(length(candidate), length(candidate), replace = TRUE)
        ]
        result[index, 1L] <- stats::median(candidate_draw) /
          stats::median(baseline_draw)
        result[index, 2L] <- unname(stats::quantile(
          candidate_draw, 0.75, type = 8
        )) / unname(stats::quantile(baseline_draw, 0.75, type = 8))
      }
      result
    }
  )
  alpha <- 1 - confidence
  list(
    median = unname(stats::quantile(
      draws[, 1L], c(alpha, 1 - alpha), type = 8
    )),
    q75 = unname(stats::quantile(
      draws[, 2L], c(alpha, 1 - alpha), type = 8
    ))
  )
}

benchmark_regression_validate_measurements <- function(
  policy, samples, allocations, label, minimum_samples
) {
  sample_columns <- c("scope", "case", "operation", "iteration", "elapsed_ns")
  allocation_columns <- c("scope", "case", "operation", "mem_alloc_bytes")
  if (!is.data.frame(samples) || !identical(names(samples), sample_columns) ||
      !is.data.frame(allocations) ||
      !identical(names(allocations), allocation_columns)) {
    stop(label, " benchmark measurements have invalid columns", call. = FALSE)
  }
  sample_keys <- benchmark_regression_key(
    samples$scope, samples$case, samples$operation
  )
  allocation_keys <- benchmark_regression_key(
    allocations$scope, allocations$case, allocations$operation
  )
  policy_keys <- benchmark_regression_key(
    policy$scope, policy$case, policy$operation
  )
  if (anyNA(samples) || anyNA(allocations) ||
      any(!is.finite(samples$elapsed_ns)) || any(samples$elapsed_ns <= 0) ||
      any(!is.finite(allocations$mem_alloc_bytes)) ||
      any(allocations$mem_alloc_bytes < 0) ||
      anyDuplicated(allocation_keys) ||
      !identical(allocation_keys, policy_keys)) {
    stop(label, " benchmark measurements are incomplete or invalid", call. = FALSE)
  }
  grouped_keys <- unique(sample_keys)
  if (!identical(grouped_keys, policy_keys)) {
    stop(label, " benchmark samples do not cover the ordered policy", call. = FALSE)
  }
  for (key in policy_keys) {
    rows <- which(sample_keys == key)
    if (length(rows) < minimum_samples ||
        !identical(samples$iteration[rows], seq_along(rows))) {
      stop(
        label, " benchmark case '", key,
        "' has too few or non-sequential samples",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

benchmark_regression_evaluate <- function(
  policy,
  baseline_samples,
  candidate_samples,
  baseline_allocations,
  candidate_allocations
) {
  spec <- benchmark_regression_policy_spec()
  benchmark_regression_validate_spec(spec)
  benchmark_regression_validate_policy(policy, spec)
  benchmark_regression_validate_measurements(
    policy, baseline_samples, baseline_allocations, "baseline",
    spec$minimum_samples
  )
  benchmark_regression_validate_measurements(
    policy, candidate_samples, candidate_allocations, "candidate",
    spec$minimum_samples
  )
  baseline_keys <- benchmark_regression_key(
    baseline_samples$scope, baseline_samples$case, baseline_samples$operation
  )
  candidate_keys <- benchmark_regression_key(
    candidate_samples$scope, candidate_samples$case, candidate_samples$operation
  )
  baseline_allocation_keys <- benchmark_regression_key(
    baseline_allocations$scope, baseline_allocations$case,
    baseline_allocations$operation
  )
  candidate_allocation_keys <- benchmark_regression_key(
    candidate_allocations$scope, candidate_allocations$case,
    candidate_allocations$operation
  )
  policy_keys <- benchmark_regression_key(
    policy$scope, policy$case, policy$operation
  )
  for (key in policy_keys) {
    if (sum(baseline_keys == key) != sum(candidate_keys == key)) {
      stop(
        "baseline and candidate sample counts differ for '", key, "'",
        call. = FALSE
      )
    }
  }

  rows <- vector("list", nrow(policy))
  for (index in seq_len(nrow(policy))) {
    key <- policy_keys[[index]]
    baseline <- baseline_samples$elapsed_ns[baseline_keys == key]
    candidate <- candidate_samples$elapsed_ns[candidate_keys == key]
    tier <- spec$tiers[spec$tiers$tier == policy$tier[[index]], , drop = FALSE]
    baseline_median <- stats::median(baseline)
    candidate_median <- stats::median(candidate)
    baseline_q75 <- unname(stats::quantile(baseline, 0.75, type = 8))
    candidate_q75 <- unname(stats::quantile(candidate, 0.75, type = 8))
    median_ratio <- candidate_median / baseline_median
    q75_ratio <- candidate_q75 / baseline_q75
    bootstrap <- benchmark_regression_bootstrap(
      baseline, candidate, key, spec$bootstrap_replicates, spec$confidence
    )
    pairwise_difference <- outer(candidate, baseline, `-`)
    slower_probability <- (
      sum(pairwise_difference > 0) + 0.5 * sum(pairwise_difference == 0)
    ) / length(pairwise_difference)

    baseline_allocation <- baseline_allocations$mem_alloc_bytes[
      baseline_allocation_keys == key
    ]
    candidate_allocation <- candidate_allocations$mem_alloc_bytes[
      candidate_allocation_keys == key
    ]
    allocation_delta <- candidate_allocation - baseline_allocation
    allocation_ratio <- benchmark_regression_ratio(
      candidate_allocation, baseline_allocation
    )
    median_budget_fraction <- max(
      0, (median_ratio - 1) / (tier$median_ratio_limit - 1)
    )
    q75_budget_fraction <- max(
      0, (q75_ratio - 1) / (tier$q75_ratio_limit - 1)
    )
    allocation_delta_fraction <- max(
      0, allocation_delta / tier$allocation_min_delta_bytes
    )
    allocation_budget_fraction <- if (baseline_allocation == 0) {
      allocation_delta_fraction
    } else {
      max(0, min(
        allocation_delta_fraction,
        (allocation_ratio - 1) / (tier$allocation_ratio_limit - 1)
      ))
    }

    median_failure <-
      median_ratio >= tier$median_ratio_limit &&
      bootstrap$median[[1L]] >= tier$median_ratio_limit &&
      slower_probability >= tier$slower_probability_limit
    q75_failure <-
      q75_ratio >= tier$q75_ratio_limit &&
      bootstrap$q75[[1L]] >= tier$q75_ratio_limit &&
      slower_probability >= tier$slower_probability_limit
    allocation_failure <-
      allocation_delta >= tier$allocation_min_delta_bytes &&
      (baseline_allocation == 0 ||
        allocation_ratio >= tier$allocation_ratio_limit)

    marginal_median_limit <- 1 +
      spec$marginal_fraction * (tier$median_ratio_limit - 1)
    marginal_q75_limit <- 1 +
      spec$marginal_fraction * (tier$q75_ratio_limit - 1)
    marginal_allocation_ratio <- 1 +
      spec$marginal_fraction * (tier$allocation_ratio_limit - 1)
    median_marginal <-
      slower_probability >= spec$marginal_probability &&
      (median_ratio >= marginal_median_limit ||
        bootstrap$median[[2L]] >= tier$median_ratio_limit)
    q75_marginal <-
      slower_probability >= spec$marginal_probability &&
      (q75_ratio >= marginal_q75_limit ||
        bootstrap$q75[[2L]] >= tier$q75_ratio_limit)
    allocation_marginal <-
      allocation_delta >= tier$allocation_min_delta_bytes *
        spec$marginal_fraction &&
      (baseline_allocation == 0 ||
        allocation_ratio >= marginal_allocation_ratio)

    failures <- c(
      if (median_failure) "median-time" else character(),
      if (q75_failure) "upper-quartile-time" else character(),
      if (allocation_failure) "allocation" else character()
    )
    marginals <- c(
      if (median_marginal && !median_failure) "median-time" else character(),
      if (q75_marginal && !q75_failure) "upper-quartile-time" else character(),
      if (allocation_marginal && !allocation_failure) "allocation" else character()
    )
    decision <- if (length(failures)) {
      "fail"
    } else if (length(marginals)) {
      "marginal"
    } else {
      "pass"
    }
    reasons <- if (length(failures)) {
      paste(paste0("fail:", failures), collapse = ";")
    } else if (length(marginals)) {
      paste(paste0("review:", marginals), collapse = ";")
    } else {
      "within-policy"
    }

    rows[[index]] <- data.frame(
      scope = policy$scope[[index]],
      case = policy$case[[index]],
      operation = policy$operation[[index]],
      tier = policy$tier[[index]],
      rationale = policy$rationale[[index]],
      decision = decision,
      reasons = reasons,
      baseline_samples = length(baseline),
      candidate_samples = length(candidate),
      baseline_median_ns = baseline_median,
      candidate_median_ns = candidate_median,
      median_ratio = median_ratio,
      median_ratio_ci_low = bootstrap$median[[1L]],
      median_ratio_ci_high = bootstrap$median[[2L]],
      median_ratio_limit = tier$median_ratio_limit,
      median_budget_fraction = median_budget_fraction,
      baseline_q75_ns = baseline_q75,
      candidate_q75_ns = candidate_q75,
      q75_ratio = q75_ratio,
      q75_ratio_ci_low = bootstrap$q75[[1L]],
      q75_ratio_ci_high = bootstrap$q75[[2L]],
      q75_ratio_limit = tier$q75_ratio_limit,
      q75_budget_fraction = q75_budget_fraction,
      slower_probability = slower_probability,
      slower_probability_limit = tier$slower_probability_limit,
      baseline_mem_alloc_bytes = baseline_allocation,
      candidate_mem_alloc_bytes = candidate_allocation,
      allocation_delta_bytes = allocation_delta,
      allocation_ratio = allocation_ratio,
      allocation_ratio_limit = tier$allocation_ratio_limit,
      allocation_min_delta_bytes = tier$allocation_min_delta_bytes,
      allocation_budget_fraction = allocation_budget_fraction,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

benchmark_regression_summarize <- function(ledger) {
  if (!is.data.frame(ledger) || !nrow(ledger) ||
      any(!ledger$decision %in% c("pass", "marginal", "fail"))) {
    stop("invalid benchmark regression decision ledger", call. = FALSE)
  }
  display_key <- ifelse(
    ledger$scope == "paired",
    ledger$case,
    paste(ledger$case, ledger$operation, sep = "/")
  )
  worst_time <- which.max(ledger$median_budget_fraction)
  worst_q75 <- which.max(ledger$q75_budget_fraction)
  worst_allocation <- which.max(ledger$allocation_budget_fraction)
  list(
    row_count = nrow(ledger),
    pass_count = sum(ledger$decision == "pass"),
    marginal_count = sum(ledger$decision == "marginal"),
    fail_count = sum(ledger$decision == "fail"),
    worst_median_case = display_key[[worst_time]],
    worst_median_ratio = ledger$median_ratio[[worst_time]],
    worst_median_budget_fraction = ledger$median_budget_fraction[[worst_time]],
    worst_q75_case = display_key[[worst_q75]],
    worst_q75_ratio = ledger$q75_ratio[[worst_q75]],
    worst_q75_budget_fraction = ledger$q75_budget_fraction[[worst_q75]],
    worst_allocation_case = display_key[[worst_allocation]],
    worst_allocation_ratio = ledger$allocation_ratio[[worst_allocation]],
    worst_allocation_delta_bytes = ledger$allocation_delta_bytes[[worst_allocation]],
    worst_allocation_budget_fraction =
      ledger$allocation_budget_fraction[[worst_allocation]]
  )
}
