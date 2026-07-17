#!/usr/bin/env Rscript

fail <- function(...) stop(..., call. = FALSE)
`%||%` <- function(left, right) if (is.null(left)) right else left
arguments <- commandArgs(trailingOnly = TRUE)
root <- if (length(arguments)) arguments[[1L]] else getwd()
root <- normalizePath(root, winslash = "/", mustWork = TRUE)
mode <- if (length(arguments) >= 2L) arguments[[2L]] else "general"
if (!mode %in% c("general", "release")) {
  fail("workflow test mode must be `general` or `release`")
}
workflow_path <- if (length(arguments) >= 3L) {
  arguments[[3L]]
} else {
  file.path(root, ".github", "workflows", "r-cmd-check.yml")
}
workflow_path <- normalizePath(
  workflow_path, winslash = "/", mustWork = TRUE
)

if (!file.exists(workflow_path) || dir.exists(workflow_path) ||
    nzchar(Sys.readlink(workflow_path))) {
  fail("portability workflow is absent, non-regular, or symbolic")
}
if (!requireNamespace("yaml", quietly = TRUE)) {
  fail("repository-local yaml package is required")
}

workflow <- yaml::read_yaml(workflow_path)
job <- workflow$jobs[["r-cmd-check"]]
configs <- job$strategy$matrix$config
steps <- job$steps
expected_rows <- if (mode == "general") 6L else 2L
if (!is.list(configs) || length(configs) != expected_rows || !is.list(steps)) {
  fail("portability workflow has an unexpected matrix size for mode ", mode)
}

step_by_name <- function(name) {
  matches <- Filter(
    function(step) identical(step$name %||% NULL, name),
    steps
  )
  if (length(matches) != 1L) {
    fail("expected exactly one workflow step named: ", name)
  }
  matches[[1L]]
}

checkout <- Filter(
  function(step) startsWith(step$uses %||% "", "actions/checkout@"),
  steps
)
if (length(checkout) != 1L) {
  fail("workflow must contain exactly one checkout step")
}
if (mode == "general") {
  if (!identical(checkout[[1L]]$uses, "actions/checkout@v6") ||
      !is.null(checkout[[1L]]$with$ref)) {
    fail("general workflow does not check out the triggering ref")
  }
} else {
  if (!identical(
      checkout[[1L]]$uses,
      "actions/checkout@df4cb1c069e1874edd31b4311f1884172cec0e10"
    ) ||
      !identical(checkout[[1L]]$with$ref, "paradox-2.0.0-ci-afa5668") ||
      !identical(checkout[[1L]]$with[["persist-credentials"]], FALSE)) {
    fail("release workflow does not pin a credential-free frozen checkout")
  }
  identity <- step_by_name("Verify frozen candidate checkout")
  if (!grepl(
      "afa56689e4037ee14a75b32811686f563f95effe",
      identity$run,
      fixed = TRUE
    )) {
    fail("workflow checkout assertion is not bound to the frozen commit")
  }
}

check_step <- step_by_name("Run R CMD check")
if (!identical(check_step$shell, "Rscript {0}") ||
    !is.null(check_step$uses) ||
    !grepl("rcmdcheck::rcmdcheck", check_step$run, fixed = TRUE)) {
  fail("R CMD check is not a direct fail-visible Rscript step")
}
depends_expression <- check_step$env[["_R_CHECK_DEPENDS_ONLY_"]]
if (mode == "general") {
  if (!is.character(depends_expression) ||
      length(depends_expression) != 1L ||
      !grepl("&& 'TRUE' || 'FALSE'", depends_expression, fixed = TRUE) ||
      grepl("|| ''", depends_expression, fixed = TRUE)) {
    fail("depends-only matrix expression can still emit an empty logical value")
  }

  depends_only <- vapply(
    configs,
    function(config) isTRUE(config$depends_only),
    logical(1L)
  )
  emitted <- ifelse(depends_only, "TRUE", "FALSE")
  parsed <- vapply(
    emitted,
    tools:::config_val_to_logical,
    logical(1L),
    USE.NAMES = FALSE
  )
  if (anyNA(parsed) || !identical(parsed, depends_only)) {
    fail("depends-only matrix values do not round-trip as exact logicals")
  }

  r_versions <- vapply(configs, function(config) config$r, character(1L))
  no_tests <- vapply(
    configs,
    function(config) isTRUE(config$no_tests),
    logical(1L)
  )
  if (sum(no_tests) != 1L || !identical(r_versions[no_tests], "4.3") ||
      !depends_only[no_tests]) {
    fail("only the reviewed R 4.3 depends-only row may omit package tests")
  }
  if (!grepl("PARADOX_CHECK_NO_TESTS", check_step$run, fixed = TRUE) ||
      !grepl('args = c(args, "--no-tests")', check_step$run, fixed = TRUE)) {
    fail("R 4.3 no-tests matrix flag does not reach rcmdcheck arguments")
  }
} else {
  os <- vapply(configs, function(config) config$os, character(1L))
  arch <- vapply(configs, function(config) config$arch, character(1L))
  r_versions <- vapply(configs, function(config) config$r, character(1L))
  expected_platforms <- c("macos-15/arm64", "windows-latest/x86_64")
  if (!identical(paste(os, arch, sep = "/"), expected_platforms) ||
      any(r_versions != "release") ||
      !identical(depends_expression, "FALSE") ||
      !identical(check_step$env$NOT_CRAN, "TRUE")) {
    fail("release workflow is not the exact two-platform ordinary-check matrix")
  }
  setup_uses <- vapply(
    Filter(
      function(step) startsWith(step$uses %||% "", "r-lib/actions/setup-r"),
      steps
    ),
    `[[`,
    character(1L),
    "uses"
  )
  if (length(setup_uses) != 2L || any(setup_uses !=
      c(
        "r-lib/actions/setup-r@d3c5be51b12e724e68f33216ca3c148b66d5f0b6",
        "r-lib/actions/setup-r-dependencies@d3c5be51b12e724e68f33216ca3c148b66d5f0b6"
      ))) {
    fail("release workflow does not pin both reviewed r-lib actions")
  }
  provenance <- step_by_name("Retain run provenance")
  upload <- step_by_name("Upload check evidence")
  if (!identical(provenance[["if"]], "always()") ||
      !identical(upload[["if"]], "always()") ||
      !identical(
        upload$uses,
        "actions/upload-artifact@043fb46d1a93c77aae656e7c1c64a875d1fc6a0a"
      )) {
    fail("release workflow does not retain provenance and check evidence")
  }
}

completion <- step_by_name("Verify R CMD check completion")
if (!identical(completion[["if"]], "always()") ||
    !identical(completion$shell, "Rscript {0}")) {
  fail("R CMD check completion is not an independent always-run R step")
}
completion_code <- parse(text = completion$run, keep.source = FALSE)
scratch <- tempfile("portability-workflow-")
if (!dir.create(scratch, recursive = FALSE, showWarnings = FALSE)) {
  fail("could not create portability workflow fixture")
}
on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)
old_working_directory <- setwd(scratch)
on.exit(setwd(old_working_directory), add = TRUE)

evaluate_completion <- function() {
  try(
    capture.output(eval(
      completion_code,
      envir = new.env(parent = globalenv())
    )),
    silent = TRUE
  )
}
if (!inherits(evaluate_completion(), "try-error")) {
  fail("completion verifier accepts a missing check log")
}
log_directory <- file.path("check", "paradox.Rcheck")
dir.create(log_directory, recursive = TRUE)
log_path <- file.path(log_directory, "00check.log")
writeLines(c("* checking tests ...", "Execution halted"), log_path)
if (!inherits(evaluate_completion(), "try-error")) {
  fail("completion verifier accepts a truncated check log")
}
writeLines(c("* DONE", "Status: ERROR"), log_path)
if (!inherits(evaluate_completion(), "try-error")) {
  fail("completion verifier accepts a non-OK check status")
}
writeLines(c("* DONE", "Status: OK", "Status: OK"), log_path)
if (!inherits(evaluate_completion(), "try-error")) {
  fail("completion verifier accepts duplicate status lines")
}
writeLines(c("Execution halted", "Status: OK"), log_path)
if (!inherits(evaluate_completion(), "try-error")) {
  fail("completion verifier accepts an execution halt before an OK status")
}
writeLines(c("Status: OK", "unexpected trailing output"), log_path)
if (!inherits(evaluate_completion(), "try-error")) {
  fail("completion verifier accepts output after the final status")
}
writeLines(c("* DONE", "Status: OK"), log_path)
if (inherits(evaluate_completion(), "try-error")) {
  fail("completion verifier rejects one exact successful check status")
}

cat("portability workflow regression tests passed\n")
