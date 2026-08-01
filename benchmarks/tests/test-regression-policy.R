arguments <- commandArgs(FALSE)
main <- function() {
script_argument <- grep("^--file=", arguments, value = TRUE)
if (length(script_argument) != 1L) {
  stop("could not identify regression policy test location", call. = FALSE)
}
script <- normalizePath(
  substring(script_argument, nchar("--file=") + 1L),
  winslash = "/", mustWork = TRUE
)
benchmark_root <- normalizePath(
  file.path(dirname(script), ".."), winslash = "/", mustWork = TRUE
)

source(file.path(benchmark_root, "regression-policy.R"), local = TRUE)
source(
  file.path(benchmark_root, "tests", "fixtures", "regression-policy-fixtures.R"),
  local = TRUE
)

fail <- function(...) stop(..., call. = FALSE)
expect_error <- function(expression, pattern) {
  observed <- tryCatch(
    {
      force(expression)
      NULL
    },
    error = function(condition) conditionMessage(condition)
  )
  if (is.null(observed) || !grepl(pattern, observed, fixed = TRUE)) {
    fail("expected error containing '", pattern, "'; observed: ", observed %||% "-")
  }
}
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

fixture <- benchmark_regression_test_fixture()
if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
  rm(".Random.seed", envir = globalenv())
}
invisible(benchmark_regression_preserve_rng(11L, stats::runif(4L)))
if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
  fail("regression RNG guard created caller state when none existed")
}
set.seed(1441)
rng_before <- .Random.seed
ledger <- benchmark_regression_evaluate(
  fixture$policy,
  fixture$baseline_samples,
  fixture$candidate_samples,
  fixture$baseline_allocations,
  fixture$candidate_allocations
)
if (!identical(.Random.seed, rng_before)) {
  fail("regression evaluator changed the caller's RNG state")
}
if (!identical(ledger$decision, fixture$expected_decisions)) {
  fail(
    "unexpected fixture decisions: ",
    paste(ledger$case, ledger$decision, sep = "=", collapse = ", ")
  )
}
if (!identical(ledger$reasons[[1L]], "within-policy") ||
    !grepl("review:", ledger$reasons[[2L]], fixed = TRUE) ||
    !grepl("fail:median-time", ledger$reasons[[3L]], fixed = TRUE) ||
    !identical(ledger$allocation_ratio[[4L]], Inf) ||
    !grepl("fail:allocation", ledger$reasons[[5L]], fixed = TRUE)) {
  fail("fixture reasons or zero-allocation behavior are incorrect")
}
ledger_again <- benchmark_regression_evaluate(
  fixture$policy,
  fixture$baseline_samples,
  fixture$candidate_samples,
  fixture$baseline_allocations,
  fixture$candidate_allocations
)
if (!identical(ledger, ledger_again)) {
  fail("regression decisions are not deterministic")
}
summary <- benchmark_regression_summarize(ledger)
if (!identical(summary$row_count, 8L) ||
    !identical(summary$pass_count, 2L) ||
    !identical(summary$marginal_count, 3L) ||
    !identical(summary$fail_count, 3L) ||
    !identical(summary$worst_median_case, "fail_timing") ||
    !identical(
      summary$worst_allocation_case, "fail_zero_baseline_allocation"
    )) {
  fail("regression summary aggregates are incorrect")
}

missing_samples <- fixture$baseline_samples[-1L, , drop = FALSE]
expect_error(
  benchmark_regression_evaluate(
    fixture$policy, missing_samples, fixture$candidate_samples,
    fixture$baseline_allocations, fixture$candidate_allocations
  ),
  "too few or non-sequential samples"
)
duplicate_allocations <- rbind(
  fixture$baseline_allocations, fixture$baseline_allocations[1L, ]
)
expect_error(
  benchmark_regression_evaluate(
    fixture$policy, fixture$baseline_samples, fixture$candidate_samples,
    duplicate_allocations, fixture$candidate_allocations
  ),
  "incomplete or invalid"
)
short_candidate <- fixture$candidate_samples[-nrow(fixture$candidate_samples), ]
expect_error(
  benchmark_regression_evaluate(
    fixture$policy, fixture$baseline_samples, short_candidate,
    fixture$baseline_allocations, fixture$candidate_allocations
  ),
  "sample counts differ"
)

workload_environment <- new.env(parent = baseenv())
sys.source(file.path(benchmark_root, "workloads.R"), envir = workload_environment)
registered_workloads <- workload_environment$benchmark_workload_names()
reviewed_policy <- benchmark_regression_read_policy(
  file.path(benchmark_root, "regression-policy.tsv"), registered_workloads
)
expected_inventory <- benchmark_regression_expected_inventory(registered_workloads)
if (nrow(reviewed_policy) != nrow(expected_inventory) ||
    !identical(
      benchmark_regression_key(
        reviewed_policy$scope, reviewed_policy$case, reviewed_policy$operation
      ),
      benchmark_regression_key(
        expected_inventory$scope, expected_inventory$case,
        expected_inventory$operation
      )
    )) {
  fail("reviewed policy does not cover the full registered inventory")
}
integrity_rows <- reviewed_policy$tier %in% c(
  "integrity-shadow-read", "integrity-collection-read"
)
expected_integrity_scopes <- c(rep("paired", 4L), rep("consumer", 3L))
expected_integrity_cases <- c(
  "shadow_values_live", "collection_values_plain",
  "collection_values_rich", "collection_values_nested",
  "mies_mutator_maybe", "mies_optimizer", "mlr3pipelines_graph"
)
expected_integrity_operations <- c(rep("-", 4L), rep("values", 3L))
expected_integrity_tiers <- c(
  "integrity-shadow-read", rep("integrity-collection-read", 6L)
)
if (!identical(
      reviewed_policy$scope[integrity_rows], expected_integrity_scopes
    ) ||
    !identical(reviewed_policy$case[integrity_rows], expected_integrity_cases) ||
    !identical(
      reviewed_policy$operation[integrity_rows], expected_integrity_operations
    ) ||
    !identical(
      reviewed_policy$tier[integrity_rows], expected_integrity_tiers
    )) {
  fail("integrity-read tiers differ from the seven reviewed rows")
}
consumer_policy <- reviewed_policy[
  reviewed_policy$scope == "consumer", , drop = FALSE
]
consumer_values <- consumer_policy$operation == "values"
expected_consumer_cases <- c(
  "mies_mutator_maybe", "mies_optimizer", "mlr3pipelines_graph"
)
if (!identical(consumer_policy$case[consumer_values], expected_consumer_cases) ||
    any(
      consumer_policy$tier[consumer_values] != "integrity-collection-read"
    ) ||
    any(
      consumer_policy$rationale[consumer_values] !=
        "integrity-validated-live-collection-value-read"
    ) ||
    any(consumer_policy$tier[!consumer_values] != "hot") ||
    !identical(
      consumer_policy$operation[!consumer_values],
      rep(c("params", "get_values_unchecked"), length(expected_consumer_cases))
    )) {
  fail("consumer value-read tier exception differs from the reviewed boundary")
}
integrity_spec <- benchmark_regression_policy_spec()$tiers
shadow_tier <- integrity_spec[
  integrity_spec$tier == "integrity-shadow-read", , drop = FALSE
]
collection_tier <- integrity_spec[
  integrity_spec$tier == "integrity-collection-read", , drop = FALSE
]
if (nrow(shadow_tier) != 1L || nrow(collection_tier) != 1L ||
    !identical(shadow_tier$median_ratio_limit, 5.50) ||
    !identical(shadow_tier$q75_ratio_limit, 6.00) ||
    !identical(shadow_tier$slower_probability_limit, 0.80) ||
    !identical(shadow_tier$allocation_ratio_limit, 1.25) ||
    !identical(shadow_tier$allocation_min_delta_bytes, 16384) ||
    !identical(collection_tier$median_ratio_limit, 2.75) ||
    !identical(collection_tier$q75_ratio_limit, 3.00) ||
    !identical(collection_tier$slower_probability_limit, 0.80) ||
    !identical(collection_tier$allocation_ratio_limit, 1.25) ||
    !identical(collection_tier$allocation_min_delta_bytes, 16384)) {
  fail("integrity-read tier ceilings changed without fixture review")
}

temporary_policy <- tempfile("paradox-regression-policy-", fileext = ".tsv")
on.exit(unlink(temporary_policy), add = TRUE)
utils::write.table(
  reviewed_policy[-1L, ], temporary_policy, sep = "\t", quote = FALSE,
  row.names = FALSE, fileEncoding = "UTF-8"
)
expect_error(
  benchmark_regression_read_policy(temporary_policy, registered_workloads),
  "does not exactly cover"
)
utils::write.table(
  reviewed_policy[c(2L, 1L, seq.int(3L, nrow(reviewed_policy))), ],
  temporary_policy, sep = "\t", quote = FALSE, row.names = FALSE,
  fileEncoding = "UTF-8"
)
expect_error(
  benchmark_regression_read_policy(temporary_policy, registered_workloads),
  "does not exactly cover"
)

cat(
  "PASS: deterministic pass, marginal/noisy, timing-fail, integrity-read, ",
  "and zero-allocation regression policy fixtures\n",
  sep = ""
)
}

main()
