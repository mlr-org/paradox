#!/usr/bin/env Rscript

fail <- function(...) stop(..., call. = FALSE)
`%||%` <- function(left, right) if (is.null(left)) right else left
script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  fail("could not identify release-workflow renderer test")
}
script <- normalizePath(
  sub("^--file=", "", script_argument),
  winslash = "/",
  mustWork = TRUE
)
root <- normalizePath(
  file.path(dirname(script), "..", ".."),
  winslash = "/",
  mustWork = TRUE
)
if (!identical(Sys.getenv("PARADOX_ACTIVE_ROOT"), root)) {
  fail("activate the exact repository-local toolchain first: . scripts/activate")
}
if (!requireNamespace("yaml", quietly = TRUE)) {
  fail("repository-local yaml package is required")
}

renderer <- file.path(
  root, "scripts", "environment", "render-portability-release-workflow.R"
)
validator <- file.path(
  root, "scripts", "environment", "test-portability-workflow.R"
)
source <- file.path(root, ".github", "workflows", "r-cmd-check.yml")
rscript <- file.path(root, ".local", "toolchain", "bin", "Rscript")
candidate_tag <- "paradox-2.0.0-ci-renderer-test"
candidate_commit <- paste(rep("a", 40L), collapse = "")
scratch <- tempfile("portability-release-renderer-")
if (!dir.create(scratch, recursive = FALSE, showWarnings = FALSE)) {
  fail("could not create release-renderer scratch directory")
}
on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)

invoke <- function(output, tag = candidate_tag, commit = candidate_commit,
    input = source) {
  result <- suppressWarnings(system2(
    rscript,
    args = c(
      "--vanilla",
      shQuote(renderer),
      tag,
      commit,
      shQuote(output),
      shQuote(input)
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  list(
    status = if (is.null(attr(result, "status"))) 0L else attr(result, "status"),
    output = result
  )
}

source_sha256 <- unname(tools::sha256sum(source))
first <- file.path(scratch, "first.yml")
result <- invoke(first)
if (result$status != 0L || !file.exists(first)) {
  fail("valid release workflow rendering failed:\n",
    paste(result$output, collapse = "\n"))
}
if (!identical(unname(tools::sha256sum(source)), source_sha256)) {
  fail("release renderer modified its general-workflow input")
}

workflow <- yaml::read_yaml(first)
if (!identical(sort(names(workflow$jobs)), sort(c(
      "r-cmd-check", "r36-windows", "portability-complete"
    ))) ||
    length(workflow$jobs[["r-cmd-check"]]$strategy$matrix$config) != 2L) {
  fail("rendered release workflow changed the required job topology")
}
for (job_name in c("r-cmd-check", "r36-windows")) {
  steps <- workflow$jobs[[job_name]]$steps
  checkout <- Filter(
    function(step) startsWith(step$uses %||% "", "actions/checkout@"),
    steps
  )
  identity <- Filter(
    function(step) identical(
      step$name %||% NULL,
      "Verify frozen harness checkout"
    ),
    steps
  )
  if (length(checkout) != 1L ||
      !identical(
        checkout[[1L]]$uses,
        "actions/checkout@df4cb1c069e1874edd31b4311f1884172cec0e10"
      ) ||
      !identical(checkout[[1L]]$with$ref, "${{ github.sha }}") ||
      !identical(checkout[[1L]]$with[["fetch-depth"]], 2L) ||
      !identical(checkout[[1L]]$with[["persist-credentials"]], FALSE) ||
      length(identity) != 1L ||
      !grepl(candidate_commit, identity[[1L]]$run, fixed = TRUE)) {
    fail("rendered release workflow did not bind both jobs to the companion")
  }
}

validation <- suppressWarnings(system2(
  rscript,
  args = c(
    "--vanilla",
    shQuote(validator),
    shQuote(root),
    "release",
    shQuote(first),
    candidate_tag,
    candidate_commit
  ),
  stdout = TRUE,
  stderr = TRUE
))
if (!is.null(attr(validation, "status")) && attr(validation, "status") != 0L) {
  fail("existing release validator rejected rendered workflow:\n",
    paste(validation, collapse = "\n"))
}

mutated_helper_identity <- file.path(
  scratch, "mutated-helper-identity.yml"
)
mutated_helper_lines <- readLines(first, warn = FALSE)
helper_blob_positions <- grep(
  "^          readonly expected_helper_blob=[0-9a-f]{40}$",
  mutated_helper_lines
)
if (length(helper_blob_positions) != 2L) {
  fail("rendered workflow does not bind two exact installer helper blobs")
}
mutated_helper_lines[[helper_blob_positions[[1L]]]] <-
  paste0("          readonly expected_helper_blob=", strrep("0", 40L))
writeLines(mutated_helper_lines, mutated_helper_identity)
mutated_helper_validation <- suppressWarnings(system2(
  rscript,
  args = c(
    "--vanilla",
    shQuote(validator),
    shQuote(root),
    "release",
    shQuote(mutated_helper_identity),
    candidate_tag,
    candidate_commit
  ),
  stdout = TRUE,
  stderr = TRUE
))
if (is.null(attr(mutated_helper_validation, "status")) ||
    attr(mutated_helper_validation, "status") == 0L) {
  fail("release validator accepted a changed installer helper blob")
}

expected_changed_line <- paste0(
  "          expected_changed=\"$(printf '%s\\n' ",
  ".github/workflows/r-cmd-check.yml)\""
)
expected_changed_positions <- which(
  mutated_helper_lines == expected_changed_line
)
if (length(expected_changed_positions) != 2L) {
  fail("rendered workflow does not bind two exact one-path diff policies")
}
mutated_changed_identity <- file.path(
  scratch, "mutated-changed-identity.yml"
)
mutated_changed_lines <- mutated_helper_lines
mutated_changed_lines[[expected_changed_positions[[1L]]]] <- paste0(
  "          expected_changed=\"$(printf '%s\\n' ",
  "scripts/environment/install-hosted-r36-windows.ps1)\""
)
writeLines(mutated_changed_lines, mutated_changed_identity)
mutated_changed_validation <- suppressWarnings(system2(
  rscript,
  args = c(
    "--vanilla",
    shQuote(validator),
    shQuote(root),
    "release",
    shQuote(mutated_changed_identity),
    candidate_tag,
    candidate_commit
  ),
  stdout = TRUE,
  stderr = TRUE
))
if (is.null(attr(mutated_changed_validation, "status")) ||
    attr(mutated_changed_validation, "status") == 0L) {
  fail("release validator accepted a changed companion diff policy")
}
actionlint <- file.path(root, ".local", "tools", "bin", "actionlint")
if (!file.exists(actionlint)) {
  fail("repository-local actionlint is required")
}
lint <- suppressWarnings(system2(
  actionlint,
  args = shQuote(first),
  stdout = TRUE,
  stderr = TRUE
))
if (!is.null(attr(lint, "status")) && attr(lint, "status") != 0L) {
  fail("actionlint rejected rendered release workflow:\n",
    paste(lint, collapse = "\n"))
}

second <- file.path(scratch, "second.yml")
result <- invoke(second)
if (result$status != 0L ||
    !identical(
      readBin(first, "raw", n = file.info(first)$size),
      readBin(second, "raw", n = file.info(second)$size)
    )) {
  fail("release workflow rendering is not deterministic")
}

before <- unname(tools::sha256sum(first))
result <- invoke(first)
if (result$status == 0L ||
    !identical(unname(tools::sha256sum(first)), before)) {
  fail("release renderer overwrote an existing output")
}

invalid_tag_output <- file.path(scratch, "invalid-tag.yml")
if (invoke(invalid_tag_output, tag = "refs/tags/unsafe")$status == 0L ||
    file.exists(invalid_tag_output)) {
  fail("release renderer accepted an unsafe candidate tag")
}
invalid_commit_output <- file.path(scratch, "invalid-commit.yml")
if (invoke(invalid_commit_output, commit = "ABC")$status == 0L ||
    file.exists(invalid_commit_output)) {
  fail("release renderer accepted an inexact candidate commit")
}

mutated_source <- file.path(scratch, "mutated-source.yml")
mutated_lines <- readLines(source, warn = FALSE)
checkout_position <- which(mutated_lines == "      - uses: actions/checkout@v6")
mutated_lines[[checkout_position[[1L]]]] <- "      - uses: actions/checkout@v5"
writeLines(mutated_lines, mutated_source)
mutated_output <- file.path(scratch, "mutated-output.yml")
if (invoke(mutated_output, input = mutated_source)$status == 0L ||
    file.exists(mutated_output)) {
  fail("release renderer accepted a structurally unreviewed source workflow")
}

missing_isolation_source <- file.path(
  scratch, "missing-isolation-source.yml"
)
missing_isolation_lines <- readLines(source, warn = FALSE)
isolation_positions <- grep(
  'enter-hosted-r36-windows.ps1',
  missing_isolation_lines,
  fixed = TRUE
)
if (length(isolation_positions) != 3L) {
  fail("general workflow does not contain three exact isolation entries")
}
missing_isolation_lines[[isolation_positions[[2L]]]] <- sub(
  "enter-hosted-r36-windows.ps1",
  "missing-hosted-r36-isolation.ps1",
  missing_isolation_lines[[isolation_positions[[2L]]]],
  fixed = TRUE
)
writeLines(missing_isolation_lines, missing_isolation_source)
missing_isolation_output <- file.path(
  scratch, "missing-isolation-output.yml"
)
if (invoke(
      missing_isolation_output,
      input = missing_isolation_source
    )$status == 0L ||
    file.exists(missing_isolation_output)) {
  fail("release renderer accepted an old-Windows phase without isolation")
}

mutated_architecture_source <- file.path(
  scratch, "mutated-architecture-source.yml"
)
mutated_architecture_lines <- readLines(source, warn = FALSE)
mutated_architecture_position <- which(mutated_architecture_lines ==
  "            'stopifnot(',")
if (length(mutated_architecture_position) != 1L) {
  fail("general workflow has no exact old-Windows architecture payload")
}
mutated_architecture_lines[[mutated_architecture_position]] <-
  "            '',"
writeLines(mutated_architecture_lines, mutated_architecture_source)
mutated_architecture_output <- file.path(
  scratch, "mutated-architecture-output.yml"
)
if (invoke(
      mutated_architecture_output,
      input = mutated_architecture_source
    )$status == 0L ||
    file.exists(mutated_architecture_output)) {
  fail("release renderer accepted a mutated old-Windows architecture payload")
}

empty_platform_source <- file.path(scratch, "empty-platform-source.yml")
empty_platform_lines <- readLines(source, warn = FALSE)
empty_platform_position <- which(empty_platform_lines ==
  "            'cat(R.version$platform)',")
if (length(empty_platform_position) != 1L) {
  fail("general workflow has no exact old-Windows platform payload")
}
empty_platform_lines[[empty_platform_position]] <-
  "            '',"
writeLines(empty_platform_lines, empty_platform_source)
empty_platform_output <- file.path(scratch, "empty-platform-output.yml")
if (invoke(empty_platform_output, input = empty_platform_source)$status == 0L ||
    file.exists(empty_platform_output)) {
  fail("release renderer accepted an empty old-Windows platform probe")
}

implicit_r_source <- file.path(scratch, "implicit-r-source.yml")
implicit_r_lines <- readLines(source, warn = FALSE)
implicit_r_position <- which(implicit_r_lines == "            -RExe $rExe `")
if (length(implicit_r_position) != 1L) {
  fail("general workflow has no exact old-Windows R executable handoff")
}
implicit_r_lines[[implicit_r_position]] <- "            -RExe R.exe `"
writeLines(implicit_r_lines, implicit_r_source)
implicit_r_output <- file.path(scratch, "implicit-r-output.yml")
if (invoke(implicit_r_output, input = implicit_r_source)$status == 0L ||
    file.exists(implicit_r_output)) {
  fail("release renderer accepted name-based old-Windows R discovery")
}

implicit_rscript_source <- file.path(
  scratch, "implicit-rscript-source.yml"
)
implicit_rscript_lines <- readLines(source, warn = FALSE)
implicit_rscript_position <- which(implicit_rscript_lines ==
  "            -RScriptExe $rScriptExe")
if (length(implicit_rscript_position) != 1L) {
  fail("general workflow has no exact old-Windows Rscript handoff")
}
implicit_rscript_lines[[implicit_rscript_position]] <-
  "            -RScriptExe Rscript.exe"
writeLines(implicit_rscript_lines, implicit_rscript_source)
implicit_rscript_output <- file.path(
  scratch, "implicit-rscript-output.yml"
)
if (invoke(
      implicit_rscript_output,
      input = implicit_rscript_source
    )$status == 0L ||
    file.exists(implicit_rscript_output)) {
  fail("release renderer accepted name-based old-Windows Rscript discovery")
}

symbolic_source <- file.path(scratch, "symbolic-source.yml")
if (!file.symlink(source, symbolic_source)) {
  fail("could not construct release-renderer symbolic-source fixture")
}
symbolic_source_output <- file.path(scratch, "symbolic-source-output.yml")
if (invoke(symbolic_source_output, input = symbolic_source)$status == 0L ||
    file.exists(symbolic_source_output)) {
  fail("release renderer accepted a symbolic source workflow")
}

symbolic_output <- file.path(scratch, "symbolic-output.yml")
if (!file.symlink(file.path(scratch, "absent-target"), symbolic_output)) {
  fail("could not construct release-renderer symbolic-output fixture")
}
if (invoke(symbolic_output)$status == 0L || file.exists(symbolic_output)) {
  fail("release renderer accepted a symbolic output path")
}

cat("portability release-workflow renderer tests passed\n")
