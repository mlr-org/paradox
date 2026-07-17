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
if (!identical(summary$row_count, 5L) ||
    !identical(summary$pass_count, 2L) ||
    !identical(summary$marginal_count, 1L) ||
    !identical(summary$fail_count, 2L) ||
    !identical(summary$worst_median_case, "fail_timing") ||
    !identical(
      summary$worst_allocation_case, "fail_zero_baseline_allocation"
    )) {
  fail("regression summary aggregates are incorrect")
}

spec <- benchmark_regression_policy_spec()
authenticated_tier <- spec$tiers[
  spec$tiers$tier == "authenticated-read", , drop = FALSE
]
expected_authenticated_tier <- data.frame(
  tier = "authenticated-read",
  median_ratio_limit = 1.60,
  q75_ratio_limit = 1.70,
  slower_probability_limit = 0.80,
  allocation_ratio_limit = 1.50,
  allocation_min_delta_bytes = 16384,
  stringsAsFactors = FALSE
)
rownames(authenticated_tier) <- NULL
if (!identical(authenticated_tier, expected_authenticated_tier)) {
  fail("authenticated-read tier differs from its reviewed release budget")
}
authenticated_policy <- data.frame(
  scope = "paired", case = "authenticated", operation = "-",
  tier = "authenticated-read", rationale = "release-safety-fixture",
  stringsAsFactors = FALSE
)
authenticated_baseline <- data.frame(
  scope = rep("paired", 60L), case = rep("authenticated", 60L),
  operation = rep("-", 60L), iteration = seq_len(60L),
  elapsed_ns = rep(seq.int(96, 105), 6L), stringsAsFactors = FALSE
)
authenticated_candidate <- authenticated_baseline
authenticated_candidate$elapsed_ns <-
  authenticated_candidate$elapsed_ns * 1.50
authenticated_allocation <- data.frame(
  scope = "paired", case = "authenticated", operation = "-",
  mem_alloc_bytes = 0, stringsAsFactors = FALSE
)
authenticated_candidate_allocation <- authenticated_allocation
authenticated_candidate_allocation$mem_alloc_bytes <- 10600
authenticated_ledger <- benchmark_regression_evaluate(
  authenticated_policy,
  authenticated_baseline,
  authenticated_candidate,
  authenticated_allocation,
  authenticated_candidate_allocation
)
if (!identical(authenticated_ledger$decision, "marginal") ||
    !grepl("review:median-time", authenticated_ledger$reasons, fixed = TRUE) ||
    !grepl("review:upper-quartile-time", authenticated_ledger$reasons,
      fixed = TRUE) ||
    !grepl("review:allocation", authenticated_ledger$reasons, fixed = TRUE)) {
  fail("authenticated-read safety cost is not retained as a marginal review")
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
  "PASS: deterministic pass, marginal/noisy, timing-fail, and ",
  "zero-allocation/authenticated-read regression policy fixtures\n",
  sep = ""
)
}

main()
