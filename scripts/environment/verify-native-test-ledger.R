#!/usr/bin/env Rscript

fail <- function(...) stop(..., call. = FALSE)

parse_options <- function(arguments) {
  values <- list()
  for (argument in arguments[arguments != "--args"]) {
    if (!startsWith(argument, "--") || !grepl("=", argument, fixed = TRUE)) {
      fail("arguments must use --name=value: ", argument)
    }
    pieces <- strsplit(sub("^--", "", argument), "=", fixed = TRUE)[[1L]]
    name <- pieces[[1L]]
    value <- paste(pieces[-1L], collapse = "=")
    if (!nzchar(name) || name %in% names(values)) fail("duplicate option: ", name)
    values[[name]] <- value
  }
  required <- c(
    "source", "ledger", "selection", "expected-dso-sha256"
  )
  optional <- "expected-requested-jobs"
  if (!all(required %in% names(values)) ||
      any(!names(values) %in% c(required, optional))) {
    fail(paste(
      "expected source, ledger, selection, and expected-dso-sha256 options,",
      "with optional expected-requested-jobs"
    ))
  }
  values
}

canonical_integer <- function(values, label) {
  parsed <- suppressWarnings(as.integer(values))
  if (anyNA(parsed) || any(parsed < 0L) ||
      !identical(as.character(parsed), values)) {
    fail(label, " contains a non-canonical non-negative integer")
  }
  parsed
}

children <- function(value) {
  output <- list()
  for (index in seq_along(value)) {
    if (rlang::is_missing(value[[index]])) next
    output[[length(output) + 1L]] <- value[[index]]
  }
  output
}

contains_call <- function(value, target) {
  if (!is.call(value) && !is.expression(value)) return(FALSE)
  if (is.call(value) && is.symbol(value[[1L]]) &&
      identical(as.character(value[[1L]]), target)) return(TRUE)
  any(vapply(children(value), contains_call, logical(1L), target = target))
}

not_cran_scopes <- function(path) {
  expression <- parse(path, keep.source = FALSE)
  tests <- character()
  total_calls <- 0L
  walk <- function(value) {
    if (is.call(value) && is.symbol(value[[1L]]) &&
        identical(as.character(value[[1L]]), "skip_on_cran")) {
      total_calls <<- total_calls + 1L
    }
    if (is.call(value) && is.symbol(value[[1L]]) &&
        identical(as.character(value[[1L]]), "test_that") &&
        contains_call(value, "skip_on_cran")) {
      description <- value[[2L]]
      if (!is.character(description) || length(description) != 1L ||
          !nzchar(description)) {
        fail("skip_on_cran test has a non-literal description in ", path)
      }
      tests <<- c(tests, description)
    }
    if (is.call(value) || is.expression(value)) {
      for (child in children(value)) walk(child)
    }
  }
  walk(expression)
  list(tests = tests, file_scope = total_calls > length(tests))
}

options <- parse_options(commandArgs(trailingOnly = TRUE))
selection <- options$selection
if (!selection %in% c("analyzer", "focused", "full")) {
  fail("selection must be analyzer, focused, or full")
}
source <- normalizePath(options$source, mustWork = TRUE)
ledger_path <- normalizePath(options$ledger, mustWork = TRUE)
expected_dso_sha256 <- options[["expected-dso-sha256"]]
if (!grepl("^[0-9a-f]{64}$", expected_dso_sha256)) {
  fail("expected DSO SHA-256 is malformed")
}
expected_requested_jobs <- NULL
if (!is.null(options[["expected-requested-jobs"]])) {
  expected_requested_jobs <- canonical_integer(
    options[["expected-requested-jobs"]],
    "expected requested jobs"
  )
  if (expected_requested_jobs < 1L || expected_requested_jobs > 16L) {
    fail("expected requested jobs is outside the reviewed range")
  }
}
test_directory <- file.path(source, "tests", "testthat")
if (!dir.exists(test_directory)) fail("source testthat directory is absent")
if (!requireNamespace("rlang", quietly = TRUE)) fail("rlang is required to audit test source")

ledger <- read.delim(
  ledger_path, sep = "\t", quote = "", comment.char = "",
  colClasses = "character", check.names = FALSE
)
expected_ledger_columns <- c(
  "kind", "schema", "selection", "not_cran", "dso_sha256", "file",
  "context", "test",
  "expectations", "passed", "failed", "skipped", "errors", "warnings",
  "skip_reason", "detail"
)
if (!identical(names(ledger), expected_ledger_columns) || !nrow(ledger) ||
    anyNA(ledger) || any(!nzchar(as.matrix(ledger)))) {
  fail("functional-test ledger is empty, truncated, or malformed")
}
expected_not_cran <- if (selection == "analyzer") "FALSE" else "TRUE"
if (any(ledger$schema != "1") || any(ledger$selection != selection) ||
    any(ledger$not_cran != expected_not_cran) ||
    any(ledger$dso_sha256 != expected_dso_sha256) ||
    !identical(tail(ledger$kind, 1L), "summary") ||
    sum(ledger$kind == "summary") != 1L ||
    any(!ledger$kind %in% c("test", "summary"))) {
  fail("functional-test ledger identity or terminal summary differs")
}
integer_columns <- c(
  "expectations", "passed", "failed", "skipped", "errors", "warnings"
)
for (name in integer_columns) {
  ledger[[name]] <- canonical_integer(ledger[[name]], paste("ledger", name))
}
tests <- ledger[ledger$kind == "test", , drop = FALSE]
summary <- ledger[ledger$kind == "summary", , drop = FALSE]
if (!nrow(tests) || any(tests$failed != 0L) || any(tests$errors != 0L) ||
    any(tests$warnings != 0L) || summary$failed != 0L ||
    summary$errors != 0L || summary$warnings != 0L) {
  fail("functional-test ledger contains a failure, error, or warning")
}
for (name in integer_columns) {
  if (!identical(summary[[name]], sum(tests[[name]]))) {
    fail("functional-test summary aggregate differs for ", name)
  }
}
if (any(tests$expectations != tests$passed + tests$failed + tests$skipped)) {
  fail("functional-test expectation accounting differs")
}
if (any(tests$skipped > 1L) ||
    any((tests$skipped == 0L) != (tests$skip_reason == "-"))) {
  fail("functional-test skip accounting differs")
}
optional_skips <- data.frame(
  file = "test-ParamSetShadow.R",
  test = "old-R Shadow generation receipts never enter the evaluator",
  skip_reason =
    "Reason: R >= 4.2 has a public non-evaluating binding-existence operation",
  stringsAsFactors = FALSE
)
analyzer_required_skips <- data.frame(
  file = c(
    "test-native-altrep-lifetimes.R",
    "test-native-domain-kernels.R",
    "test-native-gctorture.R",
    "test-native-paramset-qunif.R",
    "test-native-paramset-qunif.R",
    "test-native-paramset-trafo-gctorture.R",
    "test-native-paramset-trafo-gctorture.R",
    "test-native-paramset-trafo-gctorture.R",
    "test-native-paramset-value-mutation.R",
    "test-native-paramsetcollection-construction.R"
  ),
  test = c(
    "materialized and rejected inputs remain safe under forced collection",
    "translated ParamUty diagnostics survive forced collection",
    "every allocating native entry point survives forced collection",
    "bulk qunif remains rooted under adversarial collection",
    "frame input names and columns come from one generation",
    "authoritative ParamSet trafo survives forced collection",
    "batched nested collection name translation releases transient state",
    "live and detached mixed-encoding trafo names survive forced collection",
    "ObjectTuneToken receipts stay rooted across later validation work",
    "collection construction remains rooted under forced collection"
  ),
  skip_reason = rep("Reason: On CRAN", 10L),
  stringsAsFactors = FALSE
)
analyzer_files <- c(
  "test-native-adversarial-storage.R",
  "test-native-altrep-lifetimes.R",
  "test-native-domain-kernels.R",
  "test-native-gctorture.R",
  "test-native-paramset-qunif.R",
  "test-native-paramset-trafo-gctorture.R",
  "test-native-paramset-value-mutation.R",
  "test-native-paramsetcollection-construction.R"
)
sort_bytes <- function(x) sort(x, method = "radix")
available_files <- sort_bytes(dir(
  test_directory, "^test.*\\.[rR]$", full.names = FALSE
))
if (selection == "analyzer") {
  if (!identical(sort_bytes(intersect(available_files, analyzer_files)),
      analyzer_files)) {
    fail("analyzer source inventory is absent or incomplete")
  }
  selected_files <- analyzer_files
} else {
  selected_files <- available_files
}
if (selection == "focused") {
  selected_files <- selected_files[
    grepl(
      paste0(
        "(characterization|native|regression|operation-validation|",
        "ParamSetShadow|core-state-contract|paramset-equality|to_tune|",
        "upgrade-paradox-object|upgrade-registry)"
      ),
      selected_files
    )
  ]
}
allowed_skips <- if (selection == "analyzer") {
  analyzer_required_skips
} else {
  optional_skips
}
observed_skips <- tests[tests$skipped == 1L,
  c("file", "test", "skip_reason"), drop = FALSE]
observed_skip_keys <- do.call(paste, c(observed_skips, sep = "\t"))
allowed_skip_keys <- do.call(paste, c(allowed_skips, sep = "\t"))
if (nrow(observed_skips)) {
  if (anyDuplicated(observed_skip_keys) ||
      any(!observed_skip_keys %in% allowed_skip_keys)) {
    fail("functional-test ledger contains an unreviewed skip")
  }
}
if (selection == "analyzer") {
  required_skip_keys <- do.call(
    paste, c(analyzer_required_skips, sep = "\t")
  )
  if (!all(required_skip_keys %in% observed_skip_keys)) {
    fail("analyzer ledger did not retain every reviewed NOT_CRAN skip")
  }
} else {
  optional_skip_keys <- do.call(paste, c(optional_skips, sep = "\t"))
  observed_optional_keys <- intersect(observed_skip_keys, optional_skip_keys)
  expected_optional_keys <- if (
      optional_skips$file[[1L]] %in% selected_files &&
      getRversion() >= "4.2.0"
    ) {
    optional_skip_keys
  } else {
    character()
  }
  if (!identical(
      sort_bytes(observed_optional_keys),
      sort_bytes(expected_optional_keys)
    )) {
    fail("functional-test runtime capability skip differs")
  }
}
observed_files <- sort_bytes(unique(tests$file))
if (!identical(observed_files, selected_files)) {
  fail("functional-test ledger did not execute the exact selected file inventory")
}
detail_match <- regexec(
  paste0(
    "^files=([0-9]+);test_blocks=([0-9]+)",
    ";requested_jobs=([0-9]+);effective_jobs=([0-9]+)",
    ";scheduler_backend=(serial|serial-fallback|linux-isolated)",
    "(?:;worker_evidence_sha256=([0-9a-f]{64}))?$"
  ),
  summary$detail,
  perl = TRUE
)
detail <- regmatches(summary$detail, detail_match)[[1L]]
if (!length(detail) %in% c(6L, 7L) ||
    as.integer(detail[[2L]]) != length(selected_files) ||
    as.integer(detail[[3L]]) != nrow(tests)) {
  fail("functional-test terminal file/block counts differ")
}
requested_jobs <- canonical_integer(detail[[4L]], "ledger requested jobs")
effective_jobs <- canonical_integer(detail[[5L]], "ledger effective jobs")
scheduler_backend <- detail[[6L]]
if (requested_jobs < 1L || requested_jobs > 16L ||
    effective_jobs < 1L || effective_jobs > requested_jobs ||
    (!is.null(expected_requested_jobs) &&
      requested_jobs != expected_requested_jobs)) {
  fail("functional-test scheduler job identity differs")
}

worker_path <- sub("[.]tsv$", "-workers.tsv", ledger_path)
has_worker_hash <- length(detail) == 7L && nzchar(detail[[7L]])
worker_link <- Sys.readlink(worker_path)
worker_symbolic <- !is.na(worker_link) && nzchar(worker_link)
if (!has_worker_hash) {
  valid_serial <- scheduler_backend == "serial" &&
    requested_jobs == 1L && effective_jobs == 1L
  valid_fallback <- scheduler_backend == "serial-fallback" &&
    requested_jobs > 1L && effective_jobs == 1L
  if (!valid_serial && !valid_fallback) {
    fail("functional-test serial scheduler evidence differs")
  }
  if (file.exists(worker_path) || worker_symbolic) {
    fail("unbound native-test worker evidence accompanies a serial ledger")
  }
} else {
  if (scheduler_backend != "linux-isolated" || requested_jobs <= 1L) {
    fail("functional-test isolated scheduler evidence differs")
  }
  if (identical(worker_path, ledger_path) || !file.exists(worker_path) ||
      dir.exists(worker_path) || worker_symbolic) {
    fail("bound native-test worker evidence is absent, non-regular, or symbolic")
  }
  if (!identical(unname(tools::sha256sum(worker_path)), detail[[7L]])) {
    fail("native-test worker evidence differs from the terminal ledger binding")
  }
  workers <- read.delim(
    worker_path, sep = "\t", quote = "", comment.char = "",
    colClasses = "character", check.names = FALSE
  )
  expected_worker_columns <- c(
    "schema", "selection", "jobs", "task", "lane", "files",
    "dso_sha256_before", "dso_sha256_after", "test_blocks",
    "expectations", "passed", "failed", "skipped", "errors", "warnings",
    "status"
  )
  if (!identical(names(workers), expected_worker_columns) || !nrow(workers) ||
      anyNA(workers) || any(!nzchar(as.matrix(workers))) ||
      any(workers$schema != "1") || any(workers$selection != selection) ||
      length(unique(workers$jobs)) != 1L ||
      any(workers$dso_sha256_before != expected_dso_sha256) ||
      any(workers$dso_sha256_after != expected_dso_sha256) ||
      any(workers$status != "complete")) {
    fail("native-test worker evidence identity or schema differs")
  }
  worker_integer_columns <- c(
    "jobs", "test_blocks", "expectations", "passed", "failed", "skipped",
    "errors", "warnings"
  )
  for (name in worker_integer_columns) {
    workers[[name]] <- canonical_integer(
      workers[[name]], paste("worker evidence", name)
    )
  }
  if (workers$jobs[[1L]] < 1L || workers$jobs[[1L]] > 16L ||
      any(workers$jobs != workers$jobs[[1L]]) ||
      workers$jobs[[1L]] != effective_jobs ||
      !identical(workers$task, sprintf("%03d", seq_len(nrow(workers))))) {
    fail("native-test worker limit or deterministic task order differs")
  }

  configspace_lane <- c(
    "test_paramset_to_configspace.R",
    "test_paramset_to_configspace_old.R"
  )
  lane_files <- intersect(configspace_lane, selected_files)
  parallel_files <- setdiff(selected_files, lane_files)
  expected_effective_jobs <- min(
    requested_jobs, max(1L, length(parallel_files))
  )
  if (effective_jobs != expected_effective_jobs) {
    fail("functional-test isolated scheduler used an unexpected worker count")
  }
  expected_task_files <- parallel_files
  expected_lanes <- rep("parallel", length(parallel_files))
  if (length(lane_files)) {
    if (!identical(lane_files, configspace_lane)) {
      fail("ConfigSpace serialized worker lane inventory is incomplete")
    }
    expected_task_files <- c(
      expected_task_files,
      paste(configspace_lane, collapse = ",")
    )
    expected_lanes <- c(expected_lanes, "exclusive")
  }
  if (!identical(workers$files, expected_task_files) ||
      !identical(workers$lane, expected_lanes)) {
    fail("native-test worker task partition or serialized lane differs")
  }
  observed_worker_files <- unlist(
    strsplit(workers$files, ",", fixed = TRUE),
    use.names = FALSE
  )
  if (!identical(sort_bytes(observed_worker_files),
      sort_bytes(selected_files)) ||
      anyDuplicated(observed_worker_files)) {
    fail("native-test worker evidence did not cover every selected file once")
  }
  for (index in seq_len(nrow(workers))) {
    task_files <- strsplit(workers$files[[index]], ",", fixed = TRUE)[[1L]]
    task_tests <- tests[tests$file %in% task_files, , drop = FALSE]
    expected_counts <- c(
      test_blocks = nrow(task_tests),
      expectations = sum(task_tests$expectations),
      passed = sum(task_tests$passed),
      failed = sum(task_tests$failed),
      skipped = sum(task_tests$skipped),
      errors = sum(task_tests$errors),
      warnings = sum(task_tests$warnings)
    )
    observed_counts <- unlist(
      workers[index, names(expected_counts), drop = FALSE],
      use.names = TRUE
    )
    if (!identical(observed_counts, expected_counts)) {
      fail("native-test worker evidence aggregate differs for task ", index)
    }
  }
}
if (!length(selected_files) || !nrow(tests) || sum(tests$passed) < 1L) {
  fail("functional-test corpus is empty or contains no passing expectation")
}

scope_count <- 0L
source_scope_keys <- character()
for (file in selected_files) {
  scopes <- not_cran_scopes(file.path(test_directory, file))
  scope_count <- scope_count + length(scopes$tests) + as.integer(scopes$file_scope)
  for (description in scopes$tests) {
    source_scope_keys <- c(source_scope_keys, paste(file, description, sep = "\t"))
    row <- tests[tests$file == file & tests$test == description, , drop = FALSE]
    if (!nrow(row)) fail("NOT_CRAN test is absent from ledger: ", file, " / ", description)
  }
}
if (selection == "analyzer") {
  required_source_keys <- paste(
    analyzer_required_skips$file,
    analyzer_required_skips$test,
    sep = "\t"
  )
  if (anyDuplicated(source_scope_keys) ||
      !identical(sort_bytes(source_scope_keys),
        sort_bytes(required_source_keys)) ||
      scope_count != length(required_source_keys)) {
    fail("analyzer source differs from its reviewed NOT_CRAN inventory")
  }
} else if (any(grepl("cran", tests$skip_reason, ignore.case = TRUE))) {
  fail("NOT_CRAN scopes were not all admitted by the local full-test policy")
}

cat(
  "native functional-test ledger verified: files=", length(selected_files),
  "; blocks=", nrow(tests), "; passed=", sum(tests$passed),
  "; not_cran_scopes=", scope_count, "\n",
  sep = ""
)
