#!/usr/bin/env Rscript

fail <- function(...) stop(..., call. = FALSE)
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 1L) {
  fail("usage: test-r36-declared-floor-smoke.R REPOSITORY_ROOT")
}

root <- normalizePath(arguments[[1L]], winslash = "/", mustWork = TRUE)
helper <- file.path(
  root,
  "scripts",
  "environment",
  "run-r36-declared-floor-smoke.R"
)
lock <- file.path(
  root,
  "environment",
  "runtime-r-3.6.3-declared-floor-packages.lock"
)
for (input in c(helper, lock, file.path(root, "DESCRIPTION"))) {
  if (!file.exists(input) || dir.exists(input) ||
      nzchar(Sys.readlink(input))) {
    fail("declared-floor self-test input is absent or symbolic: ", input)
  }
}
invisible(parse(helper, keep.source = FALSE))

run_helper <- function(
    repository,
    selected_lock,
    mode,
    trailing = character()) {
  output <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      "--vanilla",
      shQuote(helper),
      mode,
      shQuote(repository),
      shQuote(selected_lock),
      shQuote(trailing)
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  list(
    output = output,
    status = {
      status <- attr(output, "status")
      if (is.null(status)) 0L else as.integer(status)
    }
  )
}

valid <- run_helper(root, lock, "validate")
if (valid$status != 0L ||
    !identical(valid$output, "declared_floor_inputs=passed")) {
  cat(valid$output, sep = "\n")
  fail("reviewed declared-floor inputs did not validate")
}

fixture <- tempfile("paradox-r36-floor-self-test-")
if (!dir.create(fixture)) {
  fail("could not create declared-floor fixture directory")
}
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)
fixture_description <- file.path(fixture, "DESCRIPTION")
fixture_lock <- file.path(fixture, basename(lock))
if (!file.copy(file.path(root, "DESCRIPTION"), fixture_description) ||
    !file.copy(lock, fixture_lock)) {
  fail("could not copy declared-floor fixture inputs")
}

expect_rejection <- function(label, pattern) {
  observed <- run_helper(fixture, fixture_lock, "validate")
  if (observed$status == 0L ||
      !any(grepl(pattern, observed$output, fixed = TRUE))) {
    cat(observed$output, sep = "\n")
    fail(label, " fixture was not rejected")
  }
  invisible(TRUE)
}

original_lock <- readLines(fixture_lock, warn = FALSE)
changed_version <- sub(
  "backports\t1.1.7\t",
  "backports\t1.5.1\t",
  original_lock,
  fixed = TRUE
)
writeLines(changed_version, fixture_lock, useBytes = TRUE)
expect_rejection("changed dependency version", "exact reviewed closure")

changed_hash <- sub(
  "b277e28716059d29841ccc1d2411accb4c2b9e8f95a1ae70f0a4f0192a9e6e0b",
  paste0(
    "a",
    substring(
      "b277e28716059d29841ccc1d2411accb4c2b9e8f95a1ae70f0a4f0192a9e6e0b",
      2L
    )
  ),
  original_lock,
  fixed = TRUE
)
writeLines(changed_hash, fixture_lock, useBytes = TRUE)
expect_rejection("changed archive digest", "exact reviewed closure")

writeLines(c(original_lock, original_lock[[2L]]), fixture_lock, useBytes = TRUE)
expect_rejection("duplicate dependency row", "exact reviewed closure")

writeLines(original_lock, fixture_lock, useBytes = TRUE)
description <- readLines(fixture_description, warn = FALSE)
description <- sub(
  "backports (>= 1.1.7)",
  "backports (>= 1.5.1)",
  description,
  fixed = TRUE
)
writeLines(description, fixture_description, useBytes = TRUE)
expect_rejection("changed DESCRIPTION floor", "reviewed direct floors")

if (!identical(as.character(getRversion()), "3.6.3")) {
  runtime_rejection <- run_helper(
    root,
    lock,
    "smoke",
    c(root, root, root)
  )
  if (runtime_rejection$status == 0L ||
      !any(grepl(
        "requires exact R 3.6.3",
        runtime_rejection$output,
        fixed = TRUE
      ))) {
    cat(runtime_rejection$output, sep = "\n")
    fail("non-R-3.6 smoke invocation was not rejected")
  }
}

cat("r36_declared_floor_smoke_fixture=passed\n")
