#!/usr/bin/env Rscript

fail <- function(...) stop(..., call. = FALSE)
`%||%` <- function(left, right) if (is.null(left)) right else left
count_fixed <- function(needle, haystack) {
  matches <- gregexpr(needle, haystack, fixed = TRUE)[[1L]]
  if (identical(matches, -1L)) 0L else length(matches)
}
contains_contiguous <- function(haystack, needle) {
  if (!length(needle)) {
    return(TRUE)
  }
  if (length(needle) > length(haystack)) {
    return(FALSE)
  }
  starts <- seq_len(length(haystack) - length(needle) + 1L)
  any(vapply(starts, function(start) {
    identical(
      haystack[start + seq_along(needle) - 1L],
      needle
    )
  }, logical(1L)))
}
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) > 5L) {
  fail(paste0(
    "usage: test-portability-workflow.R [ROOT [MODE [WORKFLOW ",
    "[CANDIDATE_TAG CANDIDATE_COMMIT]]]]"
  ))
}
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
if (!file.exists(workflow_path) || dir.exists(workflow_path) ||
    (!is.na(Sys.readlink(workflow_path)) &&
      nzchar(Sys.readlink(workflow_path)))) {
  fail("portability workflow is absent, non-regular, or symbolic")
}
workflow_path <- normalizePath(
  workflow_path, winslash = "/", mustWork = TRUE
)
candidate_tag <- if (length(arguments) >= 4L) arguments[[4L]] else NULL
candidate_commit <- if (length(arguments) >= 5L) arguments[[5L]] else NULL
if (mode == "release") {
  if (is.null(candidate_tag) || is.null(candidate_commit) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", candidate_tag) ||
      !grepl("^[0-9a-f]{40}$", candidate_commit)) {
    fail("release workflow tests require an exact candidate tag and commit")
  }
} else if (!is.null(candidate_tag) || !is.null(candidate_commit)) {
  fail("candidate identity arguments are valid only in release mode")
}

helper_relative <- "scripts/environment/install-hosted-r36-windows.ps1"
helper_blob <- NULL
lock_relative <- "environment/runtime-r-3.6.3-packages.lock"
lock_blob <- NULL
lock_sha256 <- NULL
if (mode == "release") {
  candidate_blob <- function(relative, label) {
    result <- suppressWarnings(system2(
      "git",
      args = c(
        "-C", shQuote(root), "ls-tree", candidate_commit, "--",
        shQuote(relative)
      ),
      stdout = TRUE,
      stderr = TRUE
    ))
    status <- attr(result, "status")
    if (is.null(status)) {
      status <- 0L
    }
    if (status != 0L || length(result) != 1L) {
      fail("could not derive the exact candidate ", label, " tree entry")
    }
    fields <- strsplit(result[[1L]], "\t", fixed = TRUE)[[1L]]
    metadata <- if (length(fields) == 2L) {
      strsplit(fields[[1L]], " ", fixed = TRUE)[[1L]]
    } else {
      character()
    }
    if (length(fields) != 2L || !identical(fields[[2L]], relative) ||
        length(metadata) != 3L || !identical(metadata[[1L]], "100644") ||
        !identical(metadata[[2L]], "blob") ||
        !grepl("^[0-9a-f]{40}$", metadata[[3L]])) {
      fail("candidate ", label, " is not one exact 100644 Git blob")
    }
    metadata[[3L]]
  }
  helper_blob <- candidate_blob(helper_relative, "old-Windows installer")
  helper <- file.path(root, helper_relative)
  if (!file.exists(helper) || dir.exists(helper) ||
      (!is.na(Sys.readlink(helper)) && nzchar(Sys.readlink(helper)))) {
    fail("current old-Windows installer is absent, non-regular, or symbolic")
  }
  current_helper_blob <- suppressWarnings(system2(
    "git",
    args = c(
      "-C", shQuote(root), "hash-object",
      paste0("--path=", helper_relative), "--", shQuote(helper)
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  current_helper_status <- attr(current_helper_blob, "status")
  if (is.null(current_helper_status)) {
    current_helper_status <- 0L
  }
  if (current_helper_status != 0L ||
      !identical(current_helper_blob, helper_blob)) {
    fail("current old-Windows installer differs from the candidate blob")
  }
  reviewed_lock_blob <- "5e9fb484b63cff6ee51ab2101dcaa37defd0e603"
  reviewed_lock_sha256 <- paste0(
    "9007e3a2d7eecb1057bf9610a2f2ffac",
    "f617c224b9aeb9b91bd1ef5ae85f59c5"
  )
  lock_blob <- candidate_blob(lock_relative, "old-Windows runtime-lock")
  if (!identical(lock_blob, reviewed_lock_blob)) {
    fail("candidate runtime-lock Git blob differs from the reviewed blob")
  }
  lock_blob_copy <- tempfile("paradox-runtime-lock-blob-")
  lock_blob_error <- tempfile("paradox-runtime-lock-error-")
  on.exit(unlink(c(lock_blob_copy, lock_blob_error), force = TRUE), add = TRUE)
  lock_blob_status <- suppressWarnings(system2(
    "git",
    args = c("-C", shQuote(root), "cat-file", "blob", lock_blob),
    stdout = lock_blob_copy,
    stderr = lock_blob_error
  ))
  if (!identical(lock_blob_status, 0L) || !file.exists(lock_blob_copy) ||
      dir.exists(lock_blob_copy) ||
      (!is.na(Sys.readlink(lock_blob_copy)) &&
        nzchar(Sys.readlink(lock_blob_copy)))) {
    fail("could not materialize the exact candidate runtime-lock Git blob")
  }
  lock_sha256 <- unname(tools::sha256sum(lock_blob_copy))
  if (length(lock_sha256) != 1L || is.na(lock_sha256) ||
      !grepl("^[0-9a-f]{64}$", lock_sha256)) {
    fail("could not derive the exact candidate runtime-lock SHA-256")
  }
  if (!identical(lock_sha256, reviewed_lock_sha256)) {
    fail("candidate runtime-lock SHA-256 differs from the reviewed lock")
  }
}

if (!requireNamespace("yaml", quietly = TRUE)) {
  fail("repository-local yaml package is required")
}

workflow <- yaml::read_yaml(workflow_path)
if (!identical(sort(names(workflow$jobs)),
    sort(c("r-cmd-check", "r36-windows", "portability-complete")))) {
  fail(
    "portability workflow must contain only the matrix, old-Windows, and ",
    "completion jobs"
  )
}
job <- workflow$jobs[["r-cmd-check"]]
configs <- job$strategy$matrix$config
steps <- job$steps
expected_rows <- if (mode == "general") 6L else 2L
if (!identical(job$strategy[["fail-fast"]], FALSE) ||
    !is.null(job[["continue-on-error"]]) ||
    !is.list(configs) ||
    length(configs) != expected_rows ||
    !is.list(steps) ||
    length(steps) != (if (mode == "general") 7L else 10L)) {
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

check_checkout <- function(target_steps, label) {
  checkout <- Filter(
    function(step) startsWith(step$uses %||% "", "actions/checkout@"),
    target_steps
  )
  if (length(checkout) != 1L) {
    fail(label, " must contain exactly one checkout step")
  }
  if (mode == "general") {
    if (!identical(checkout[[1L]]$uses, "actions/checkout@v6") ||
        !is.null(checkout[[1L]]$with$ref)) {
      fail(label, " does not check out the triggering ref")
    }
  } else {
    if (!identical(
        checkout[[1L]]$uses,
        "actions/checkout@df4cb1c069e1874edd31b4311f1884172cec0e10"
      ) ||
        !identical(checkout[[1L]]$with$ref, "${{ github.sha }}") ||
        !identical(checkout[[1L]]$with[["fetch-depth"]], 2L) ||
        !identical(checkout[[1L]]$with[["persist-credentials"]], FALSE)) {
      fail(label, " does not pin the credential-free companion checkout")
    }
  }
  invisible(checkout[[1L]])
}

check_release_identity <- function(identity, label, materialize_lock = FALSE) {
  identity_lines <- trimws(strsplit(
    identity$run, "\n", fixed = TRUE
  )[[1L]])
  expected_prefix <- c(
    "set -euo pipefail",
    paste0("readonly candidate=", candidate_commit),
    'readonly harness="${GITHUB_SHA:?}"',
    'head="$(git rev-parse HEAD)"',
    "readonly head",
    'parents="$(git rev-list --parents -n 1 "$harness")"',
    "readonly parents",
    'test "$head" = "$harness"',
    'test "$parents" = "$harness $candidate"',
    paste0("readonly helper=", helper_relative),
    paste0("readonly expected_helper_blob=", helper_blob),
    'helper_entry="$(git ls-tree "$harness" -- "$helper")"',
    "readonly helper_entry",
    paste0(
      "expected_helper_entry=\"$(printf '100644 blob %s\\t%s' ",
      "\"$expected_helper_blob\" \"$helper\")\""
    ),
    "readonly expected_helper_entry",
    'test "$helper_entry" = "$expected_helper_entry"'
  )
  expected_lock_identity <- c(
    paste0("readonly lock=", lock_relative),
    paste0("readonly expected_lock_blob=", lock_blob),
    paste0("readonly expected_lock_sha256=", lock_sha256),
    'lock_entry="$(git ls-tree "$harness" -- "$lock")"',
    "readonly lock_entry",
    paste0(
      "expected_lock_entry=\"$(printf '100644 blob %s\\t%s' ",
      "\"$expected_lock_blob\" \"$lock\")\""
    ),
    "readonly expected_lock_entry",
    'test "$lock_entry" = "$expected_lock_entry"'
  )
  expected_companion_identity <- c(
    'changed="$(git diff --name-only "$candidate" "$harness")"',
    "readonly changed",
    paste0(
      "expected_changed=\"$(printf '%s\\n' ",
      ".github/workflows/r-cmd-check.yml)\""
    ),
    "readonly expected_changed",
    'test "$changed" = "$expected_changed"',
    'test -z "$(git status --porcelain=v1 --untracked-files=all)"'
  )
  expected_lock_materialization <- c(
    'lock_tmp="$(mktemp "${lock}.raw.XXXXXX")"',
    "readonly lock_tmp",
    'trap \'rm -f -- "$lock_tmp"\' EXIT',
    'git cat-file blob "$expected_lock_blob" > "$lock_tmp"',
    'chmod 0644 "$lock_tmp"',
    paste0(
      "test \"$(git hash-object --no-filters -- ",
      "\"$lock_tmp\")\" = \"$expected_lock_blob\""
    ),
    paste0(
      "test \"$(sha256sum -- \"$lock_tmp\" | ",
      "cut -d ' ' -f 1)\" = \"$expected_lock_sha256\""
    ),
    'mv -f -- "$lock_tmp" "$lock"',
    "trap - EXIT",
    paste0(
      "test \"$(git hash-object --no-filters -- ",
      "\"$lock\")\" = \"$expected_lock_blob\""
    ),
    paste0(
      "test \"$(sha256sum -- \"$lock\" | ",
      "cut -d ' ' -f 1)\" = \"$expected_lock_sha256\""
    )
  )
  expected_identity <- if (isTRUE(materialize_lock)) {
    c(
      expected_prefix,
      expected_lock_identity,
      expected_companion_identity,
      expected_lock_materialization
    )
  } else {
    c(expected_prefix, expected_companion_identity)
  }
  if (!identical(identity$shell, "bash") ||
      !identical(identity_lines, expected_identity)) {
    fail(label, " is not bound to the candidate/companion relation")
  }
}

checkout <- Filter(
  function(step) startsWith(step$uses %||% "", "actions/checkout@"),
  steps
)
check_checkout(steps, "matrix job")
if (mode == "release") {
  identity <- step_by_name("Verify frozen harness checkout")
  check_release_identity(identity, "matrix checkout assertion")
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
      !identical(
        job$name,
        paste0(
          "${{ matrix.config.os }} / ${{ matrix.config.arch }} ",
          "(${{ matrix.config.r }})"
        )
      ) ||
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

old_job <- workflow$jobs[["r36-windows"]]
old_steps <- old_job$steps
old_step_by_name <- function(name) {
  matches <- Filter(
    function(step) identical(step$name %||% NULL, name),
    old_steps
  )
  if (length(matches) != 1L) {
    fail("expected exactly one old-Windows step named: ", name)
  }
  matches[[1L]]
}
if (!identical(
    old_job$name,
    "windows-latest / x86_64 (R 3.6.3 / Rtools35)"
  ) ||
    !identical(old_job[["runs-on"]], "windows-latest") ||
    !identical(old_job[["timeout-minutes"]], 75L) ||
    !is.null(old_job[["continue-on-error"]]) ||
    !is.list(old_steps) ||
    length(old_steps) != (if (mode == "general") 7L else 8L) ||
    !identical(old_job$env$PARADOX_R36_WORK, "C:\\p36") ||
    !identical(old_job$env$PARADOX_R36_EVIDENCE, "C:\\p36-evidence") ||
    !identical(
      old_job$env$PARADOX_R36_ISOLATION,
      "C:\\p36-isolation"
    )) {
  fail(
    "the separate exact old-Windows job has an unexpected shape: mode=",
    mode,
    ", steps=",
    length(old_steps),
    ", name=",
    old_job$name %||% "<missing>",
    ", runs-on=",
    old_job[["runs-on"]] %||% "<missing>",
    ", timeout=",
    old_job[["timeout-minutes"]] %||% "<missing>",
    ", work=",
    old_job$env$PARADOX_R36_WORK %||% "<missing>",
    ", evidence=",
    old_job$env$PARADOX_R36_EVIDENCE %||% "<missing>",
    ", names=",
    paste(vapply(
      old_steps,
      function(step) step$name %||% step$uses %||% "<unnamed>",
      character(1L)
    ), collapse = " | ")
  )
}
check_checkout(old_steps, "exact old-Windows job")

old_setup <- Filter(
  function(step) startsWith(step$uses %||% "", "r-lib/actions/setup-r@"),
  old_steps
)
if (length(old_setup) != 1L ||
    !identical(
      old_setup[[1L]]$uses,
      "r-lib/actions/setup-r@d3c5be51b12e724e68f33216ca3c148b66d5f0b6"
    ) ||
    !identical(old_setup[[1L]]$with[["r-version"]], "3.6.3") ||
    !identical(old_setup[[1L]]$with[["rtools-version"]], "35") ||
    !identical(old_setup[[1L]]$with[["use-public-rspm"]], FALSE) ||
    any(vapply(
      old_steps,
      function(step) grepl(
        "setup-r-dependencies",
        step$uses %||% "",
        fixed = TRUE
      ),
      logical(1L)
    ))) {
  fail("old-Windows setup is not pinned to exact R 3.6.3/Rtools35")
}

toolchain <- old_step_by_name("Verify exact R 3.6 Windows toolchain")
closure <- old_step_by_name("Install exact R 3.6 source closure")
old_check <- old_step_by_name("Build, smoke, and check on R 3.6 Windows")
toolchain_lines <- trimws(strsplit(
  toolchain$run, "\n", fixed = TRUE
)[[1L]])
expected_toolchain_r_definitions <- c(
  '$rBin = "C:\\R\\bin\\x64"',
  '$rExe = Join-Path $rBin "R.exe"',
  '$rScriptExe = Join-Path $rBin "Rscript.exe"'
)
expected_architecture_payload <- c(
  "$rArchitectureCode = @(",
  "'stopifnot(',",
  "'  identical(as.character(getRversion()), \"3.6.3\"),',",
  "'  identical(.Platform$OS.type, \"windows\"),',",
  "'  grepl(\"^(x86_64|x64)$\", tolower(R.version$arch))',",
  "')',",
  "'cat(R.version$version.string, \"\\n\", R.version$platform, \"\\n\")'",
  ")"
)
expected_architecture_file <- c(
  "$rArchitectureScript = Join-Path `",
  "$env:TEMP `",
  '"verify-r36-architecture.R"',
  "if (Test-Path -LiteralPath $rArchitectureScript) {",
  'throw "R architecture probe path is not fresh"',
  "}",
  expected_architecture_payload,
  "$utf8NoBom = New-Object Text.UTF8Encoding($false)",
  "[IO.File]::WriteAllLines(",
  "$rArchitectureScript,",
  "$rArchitectureCode,",
  "$utf8NoBom",
  ")"
)
expected_architecture_execution <- c(
  "try {",
  "& $rScriptExe --vanilla $rArchitectureScript",
  "$rArchitectureStatus = $LASTEXITCODE",
  "} finally {",
  "Remove-Item -LiteralPath $rArchitectureScript -Force",
  "}",
  "if ($rArchitectureStatus -ne 0 -or",
  "(Test-Path -LiteralPath $rArchitectureScript)) {"
)
if (sum(toolchain_lines == '$rBin = "C:\\R\\bin\\x64"') != 1L ||
    sum(grepl(
      "^\\$rBin[[:space:]]*=",
      toolchain_lines
    )) != 1L ||
    sum(grepl(
      "^\\$rExe[[:space:]]*=",
      toolchain_lines
    )) != 1L ||
    sum(grepl(
      "^\\$rScriptExe[[:space:]]*=",
      toolchain_lines
    )) != 1L ||
    sum(grepl(
      "^\\$env:PATH[[:space:]]*=",
      toolchain_lines
    )) != 1L ||
    sum(grepl(
      "^\\$rArchitectureScript[[:space:]]*=",
      toolchain_lines
    )) != 1L ||
    any(grepl("Get-Command R.exe", toolchain_lines, fixed = TRUE)) ||
    any(grepl("Get-Command Rscript.exe", toolchain_lines, fixed = TRUE)) ||
    sum(toolchain_lines == "$rArchitectureScript = Join-Path `") != 1L ||
    sum(toolchain_lines ==
      "& $rScriptExe --vanilla $rArchitectureScript") != 1L ||
    !contains_contiguous(
      toolchain_lines,
      expected_toolchain_r_definitions
    ) ||
    !contains_contiguous(toolchain_lines, expected_architecture_file) ||
    !contains_contiguous(toolchain_lines, expected_architecture_execution) ||
    !grepl("[IO.File]::WriteAllLines(", toolchain$run, fixed = TRUE) ||
    !grepl("Text.UTF8Encoding($false)", toolchain$run, fixed = TRUE) ||
    !grepl(
      "Remove-Item -LiteralPath $rArchitectureScript -Force",
      toolchain$run,
      fixed = TRUE
    ) ||
    grepl(" --vanilla -e", toolchain$run, fixed = TRUE)) {
  fail(
    "old-Windows architecture admission does not use one fresh, no-BOM ",
    "file through the direct x86-64 Rscript"
  )
}
isolation_helper_literal <-
  "scripts\\environment\\enter-hosted-r36-windows.ps1"
isolation_calls <- c(
  toolchain = toolchain$run,
  closure = closure$run,
  candidate = old_check$run
)
closure_lines <- trimws(strsplit(closure$run, "\n", fixed = TRUE)[[1L]])
old_check_lines <- trimws(strsplit(
  old_check$run, "\n", fixed = TRUE
)[[1L]])
expected_closure_r_definitions <- c(
  '$rBin = "C:\\R\\bin\\x64"',
  '$rExe = Join-Path $rBin "R.exe"',
  '$rScriptExe = Join-Path $rBin "Rscript.exe"',
  "if (-not (Test-Path -LiteralPath $rExe -PathType Leaf) -or",
  "-not (Test-Path -LiteralPath $rScriptExe -PathType Leaf)) {",
  'throw "closure phase lacks exact R 3.6 x86-64 executables"',
  "}"
)
expected_closure_installer <- c(
  '& "$env:GITHUB_WORKSPACE\\scripts\\environment\\install-hosted-r36-windows.ps1" `',
  '-RepositoryRoot "$env:GITHUB_WORKSPACE" `',
  '-WorkRoot "$env:PARADOX_R36_WORK" `',
  '-EvidenceRoot "$env:PARADOX_R36_EVIDENCE" `',
  "-RExe $rExe `",
  "-RScriptExe $rScriptExe"
)
expected_closure_path <- c(
  "$env:PATH = (",
  '"$rBin;C:\\Rtools\\bin;C:\\Rtools\\mingw_64\\bin;" + $env:PATH',
  ")"
)
expected_candidate_path <- c(
  "$env:PATH = (",
  '"C:\\R\\bin\\x64;C:\\Rtools\\bin;C:\\Rtools\\mingw_64\\bin;" +',
  "$env:PATH",
  ")"
)
if (sum(closure_lines == '$rBin = "C:\\R\\bin\\x64"') != 1L ||
    sum(grepl(
      "^\\$rBin[[:space:]]*=",
      closure_lines
    )) != 1L ||
    sum(grepl(
      "^\\$env:PATH[[:space:]]*=",
      closure_lines
    )) != 1L ||
    sum(grepl("^\\$rExe[[:space:]]*=", closure_lines)) != 1L ||
    sum(grepl("^\\$rScriptExe[[:space:]]*=", closure_lines)) != 1L ||
    any(grepl("PSNativeCommandArgumentPassing", closure_lines, fixed = TRUE)) ||
    any(grepl("Get-Command R.exe", closure_lines, fixed = TRUE)) ||
    any(grepl("Get-Command Rscript.exe", closure_lines, fixed = TRUE)) ||
    !contains_contiguous(closure_lines, expected_closure_r_definitions) ||
    !contains_contiguous(closure_lines, expected_closure_installer) ||
    !contains_contiguous(closure_lines, expected_closure_path) ||
    sum(grepl(
      "^\\$rScriptExe[[:space:]]*=",
      old_check_lines
    )) != 1L ||
    sum(old_check_lines ==
      '$rScriptExe = "C:\\R\\bin\\x64\\Rscript.exe"') != 1L ||
    !contains_contiguous(old_check_lines, expected_candidate_path) ||
    sum(old_check_lines == "& $rScriptExe --vanilla `") != 1L ||
    any(grepl(" --vanilla -e", c(closure$run, old_check$run), fixed = TRUE)) ||
    any(grepl("& R.exe", isolation_calls, fixed = TRUE)) ||
    any(grepl("& Rscript.exe", isolation_calls, fixed = TRUE))) {
  fail("old-Windows closure/candidate phases can traverse the Rfe wrapper")
}
closure_installer_position <- grep(
  "install-hosted-r36-windows.ps1", closure_lines, fixed = TRUE
)
closure_admission_positions <- c(
  which(closure_lines == '$rBin = "C:\\R\\bin\\x64"'),
  which(closure_lines == '$rExe = Join-Path $rBin "R.exe"'),
  which(closure_lines == '$rScriptExe = Join-Path $rBin "Rscript.exe"'),
  which(closure_lines == "$env:PATH = (")
)
if (length(closure_installer_position) != 1L ||
    length(closure_admission_positions) != 4L ||
    any(closure_admission_positions >= closure_installer_position)) {
  fail("old-Windows direct-R admission does not precede closure execution")
}
expected_phases <- names(isolation_calls)
if (any(!vapply(
      isolation_calls,
      function(source) grepl(
        isolation_helper_literal, source, fixed = TRUE
      ),
      logical(1L)
    )) ||
    any(!vapply(seq_along(isolation_calls), function(index) {
      grepl(
        paste0("-Phase ", expected_phases[[index]]),
        isolation_calls[[index]],
        fixed = TRUE
      )
    }, logical(1L))) ||
    !grepl("-Initialize", toolchain$run, fixed = TRUE) ||
    grepl("-Initialize", closure$run, fixed = TRUE) ||
    grepl("-Initialize", old_check$run, fixed = TRUE)) {
  fail("old-Windows execution phases do not enter one exact isolation helper")
}
if (!identical(toolchain$shell, "pwsh") ||
    !grepl('"3.6.3"', toolchain$run, fixed = TRUE) ||
    !grepl("mingw_64\\bin\\gcc.exe", toolchain$run, fixed = TRUE) ||
    !grepl("mingw_64\\bin\\g++.exe", toolchain$run, fixed = TRUE) ||
    !grepl("mingw_64\\bin\\objdump.exe", toolchain$run, fixed = TRUE) ||
    !grepl("Get-FileHash", toolchain$run, fixed = TRUE) ||
    grepl(
      'Get-Command gcc.exe -CommandType Application',
      toolchain$run,
      fixed = TRUE
    ) ||
    grepl(
      'Get-Command g++.exe -CommandType Application',
      toolchain$run,
      fixed = TRUE
    ) ||
    !grepl(
      paste0(
        '"gcc.exe ',
        '(x86_64-posix-seh, Built by MinGW-W64 project) 4.9.3"'
      ),
      toolchain$run,
      fixed = TRUE
    ) ||
    !grepl(
      paste0(
        '"g++.exe ',
        '(x86_64-posix-seh, Built by MinGW-W64 project) 4.9.3"'
      ),
      toolchain$run,
      fixed = TRUE
    ) ||
    !grepl(
      'Assert-RConfiguredToolPath $rConfiguredCc $gcc "R CMD config CC"',
      toolchain$run,
      fixed = TRUE
    ) ||
    !grepl(
      '$rConfiguredCxxTokens.Count -ne 2',
      toolchain$run,
      fixed = TRUE
    ) ||
    !grepl(
      '$rConfiguredCxxTokens[1] -cne "-std=gnu++11"',
      toolchain$run,
      fixed = TRUE
    ) ||
    !grepl(
      'Assert-RConfiguredToolPath $rConfiguredCxx11 $gxx',
      toolchain$run,
      fixed = TRUE
    ) ||
    !grepl(
      '$rConfiguredCxx11Std -cne "-std=gnu++11"',
      toolchain$run,
      fixed = TRUE
    ) ||
    !identical(closure$shell, "pwsh") ||
    !grepl(
      "scripts\\environment\\install-hosted-r36-windows.ps1",
      closure$run,
      fixed = TRUE
    ) ||
    !identical(old_check$shell, "pwsh") ||
    !grepl(
      "scripts\\environment\\run-hosted-r36-windows.R",
      old_check$run,
      fixed = TRUE
    ) ||
    !grepl("LASTEXITCODE", old_check$run, fixed = TRUE)) {
  fail("old-Windows source validation is not fail-visible and script-owned")
}

installer_path <- file.path(
  root, "scripts", "environment", "install-hosted-r36-windows.ps1"
)
runner_path <- file.path(
  root, "scripts", "environment", "run-hosted-r36-windows.R"
)
isolation_helper_path <- file.path(
  root, "scripts", "environment", "enter-hosted-r36-windows.ps1"
)
if (!file.exists(isolation_helper_path) ||
    dir.exists(isolation_helper_path) ||
    nzchar(Sys.readlink(isolation_helper_path))) {
  fail("old-Windows hostile-environment isolation helper is absent or symbolic")
}
installer_lines <- readLines(installer_path, warn = FALSE)
installer <- paste(installer_lines, collapse = "\n")
runner_lines <- readLines(runner_path, warn = FALSE)
runner <- paste(runner_lines, collapse = "\n")
expected_runner_check_environment <- c(
  "Sys.setenv(",
  "R_LIBS_USER = dependency_library,",
  '"_R_CHECK_FORCE_SUGGESTS_" = "false",',
  '"_R_CHECK_CRAN_INCOMING_" = "false",',
  '"_R_CHECK_DEPENDS_ONLY_" = "TRUE",',
  '"_R_CHECK_RD_XREFS_" = "false",',
  'NOT_CRAN = "false"',
  ")"
)
trimmed_installer_lines <- trimws(installer_lines)
expected_r_exe_parameter <- c(
  "[Parameter(Mandatory = $true)]",
  "[ValidateNotNullOrEmpty()]",
  "[string]$RExe,"
)
expected_rscript_parameter <- c(
  "[Parameter(Mandatory = $true)]",
  "[ValidateNotNullOrEmpty()]",
  "[string]$RScriptExe"
)
expected_installer_r_admission <- c(
  "$RExe = [IO.Path]::GetFullPath($RExe)",
  "$RScriptExe = [IO.Path]::GetFullPath($RScriptExe)",
  '$ExpectedRExe = [IO.Path]::GetFullPath("C:\\R\\bin\\x64\\R.exe")',
  paste0(
    '$ExpectedRScriptExe = [IO.Path]::GetFullPath(',
    '"C:\\R\\bin\\x64\\Rscript.exe")'
  ),
  "if (-not [String]::Equals(",
  "$RExe,",
  "$ExpectedRExe,",
  "[StringComparison]::OrdinalIgnoreCase",
  ") -or -not [String]::Equals(",
  "$RScriptExe,",
  "$ExpectedRScriptExe,",
  "[StringComparison]::OrdinalIgnoreCase",
  ") -or -not (Test-Path -LiteralPath $RExe -PathType Leaf) -or",
  "-not (Test-Path -LiteralPath $RScriptExe -PathType Leaf)) {",
  'throw "dependency installer requires exact R 3.6 x86-64 executables"',
  "}",
  "foreach ($Executable in @($RExe, $RScriptExe)) {",
  "$Item = Get-Item -LiteralPath $Executable -Force",
  "if (($Item.Attributes -band [IO.FileAttributes]::ReparsePoint) -ne 0) {",
  'throw "dependency installer R executable is a reparse point"',
  "}",
  "}"
)
expected_installed_verifier_creation <- c(
  '$VerifyInstalledScript = Join-Path $WorkRoot "verify-installed-version.R"',
  "if (Test-Path -LiteralPath $VerifyInstalledScript) {",
  'throw "installed-version verifier path is not fresh"',
  "}",
  "[IO.File]::WriteAllText(",
  "$VerifyInstalledScript,",
  "$VerifyInstalledCode,",
  "$Utf8NoBom",
  ")",
  'Remove-Item -LiteralPath "Env:R_PKG_CXX_STD" -ErrorAction SilentlyContinue',
  "try {",
  "foreach ($Package in $InstallOrder) {"
)
expected_installed_verifier_invocation <- c(
  "Invoke-Native `",
  "-FilePath $RScriptExe `",
  "-Arguments @(",
  '"--vanilla",',
  "$VerifyInstalledScript,",
  "$LibraryRoot,",
  "$Package,",
  "$Row.Version",
  ") `",
  '-Label "installed-version verification for $Package"'
)
expected_installed_verifier_cleanup <- c(
  "Remove-Item -LiteralPath $VerifyInstalledScript -Force",
  "}",
  "if (Test-Path -LiteralPath $VerifyInstalledScript) {",
  'throw "installed-version verifier was not removed"',
  "}"
)
expected_closure_verifier <- c(
  '$VerifyClosureScript = Join-Path $WorkRoot "verify-runtime-closure.R"',
  "if (Test-Path -LiteralPath $VerifyClosureScript) {",
  'throw "runtime-closure verifier path is not fresh"',
  "}",
  "[IO.File]::WriteAllText(",
  "$VerifyClosureScript,",
  "$VerifyClosureCode,",
  "$Utf8NoBom",
  ")",
  "try {",
  "Invoke-Native `",
  "-FilePath $RScriptExe `",
  "-Arguments @(",
  '"--vanilla",',
  "$VerifyClosureScript,",
  "$LibraryRoot",
  ") `",
  '-Label "complete runtime-closure verification"',
  "} finally {",
  "Remove-Item -LiteralPath $VerifyClosureScript -Force",
  "}",
  "if (Test-Path -LiteralPath $VerifyClosureScript) {",
  'throw "runtime-closure verifier was not removed"',
  "}"
)
if (sum(trimmed_installer_lines == '[string]$RExe,') != 1L ||
    sum(trimmed_installer_lines == '[string]$RScriptExe') != 1L ||
    sum(trimmed_installer_lines == '[ValidateNotNullOrEmpty()]') != 2L ||
    !contains_contiguous(
      trimmed_installer_lines,
      expected_r_exe_parameter
    ) ||
    !contains_contiguous(
      trimmed_installer_lines,
      expected_rscript_parameter
    ) ||
    !contains_contiguous(
      trimmed_installer_lines,
      expected_installer_r_admission
    ) ||
    sum(trimmed_installer_lines ==
      '$RExe = [IO.Path]::GetFullPath($RExe)') != 1L ||
    sum(trimmed_installer_lines ==
      '$RScriptExe = [IO.Path]::GetFullPath($RScriptExe)') != 1L ||
    sum(trimmed_installer_lines ==
      '$ExpectedRExe = [IO.Path]::GetFullPath("C:\\R\\bin\\x64\\R.exe")') !=
      1L ||
    sum(trimmed_installer_lines == paste0(
      '$ExpectedRScriptExe = [IO.Path]::GetFullPath(',
      '"C:\\R\\bin\\x64\\Rscript.exe")'
    )) != 1L ||
    any(grepl('Get-Command "R.exe"', installer_lines, fixed = TRUE)) ||
    any(grepl('Get-Command "Rscript.exe"', installer_lines, fixed = TRUE)) ||
    sum(trimmed_installer_lines == '"-e",') != 0L ||
    sum(trimmed_installer_lines == "-FilePath $RScriptExe `") != 2L ||
    sum(trimmed_installer_lines ==
      '$InstallOutput = @(& $RExe @InstallArguments 2>&1)') != 1L ||
    any(grepl("& R.exe", installer_lines, fixed = TRUE)) ||
    any(grepl("& Rscript.exe", installer_lines, fixed = TRUE)) ||
    sum(trimmed_installer_lines ==
      '$VerifyInstalledScript = Join-Path $WorkRoot "verify-installed-version.R"') !=
      1L ||
    sum(trimmed_installer_lines ==
      '$VerifyClosureScript = Join-Path $WorkRoot "verify-runtime-closure.R"') !=
      1L ||
    sum(trimmed_installer_lines == "$VerifyInstalledScript,") != 2L ||
    sum(trimmed_installer_lines == "$VerifyClosureScript,") != 2L ||
    sum(trimmed_installer_lines ==
      "Remove-Item -LiteralPath $VerifyInstalledScript -Force") != 1L ||
    sum(trimmed_installer_lines ==
      "Remove-Item -LiteralPath $VerifyClosureScript -Force") != 1L ||
    !contains_contiguous(
      trimmed_installer_lines,
      expected_installed_verifier_creation
    ) ||
    !contains_contiguous(
      trimmed_installer_lines,
      expected_installed_verifier_invocation
    ) ||
    !contains_contiguous(
      trimmed_installer_lines,
      expected_installed_verifier_cleanup
    ) ||
    !contains_contiguous(trimmed_installer_lines, expected_closure_verifier) ||
    sum(trimmed_installer_lines == "args <- commandArgs(TRUE)") != 2L ||
    !grepl("utils::packageVersion", installer, fixed = TRUE) ||
    !grepl('loadNamespace("data.table"', installer, fixed = TRUE) ||
    !grepl('getLoadedDLLs()[["data_table"]]', installer, fixed = TRUE) ||
    sum(trimmed_installer_lines ==
      '$Tool.Path = [IO.Path]::GetFullPath($Tool.ExpectedPath)') != 1L ||
    sum(trimmed_installer_lines ==
      'if (-not (Test-Path -LiteralPath $Tool.Path -PathType Leaf)) {') != 1L ||
    any(grepl("Get-Command gcc.exe", installer_lines, fixed = TRUE)) ||
    any(grepl("Get-Command g++.exe", installer_lines, fixed = TRUE)) ||
    any(grepl("Get-Command objdump.exe", installer_lines, fixed = TRUE)) ||
    any(grepl("Get-Command make.exe", installer_lines, fixed = TRUE)) ||
    grepl("$env:PATH", installer, fixed = TRUE)) {
  fail("old-Windows installer native R/Rscript inventory changed")
}
isolation_helper_lines <- readLines(isolation_helper_path, warn = FALSE)
isolation_helper <- paste(isolation_helper_lines, collapse = "\n")
reset_start <- which(trimws(isolation_helper_lines) ==
  "$ResetVariables = @(")
reset_end <- if (length(reset_start) == 1L) {
  candidates <- which(
    seq_along(isolation_helper_lines) > reset_start &
      trimws(isolation_helper_lines) == ")"
  )
  if (length(candidates)) candidates[[1L]] else integer()
} else {
  integer()
}
if (length(reset_start) != 1L || length(reset_end) != 1L ||
    reset_end <= reset_start + 1L) {
  fail("old-Windows reset-variable policy array is not structurally exact")
}
reset_lines <- trimws(isolation_helper_lines[
  seq.int(reset_start + 1L, reset_end - 1L)
])
reset_matches <- regexec('^"([A-Z0-9_]+)",?$', reset_lines)
reset_pieces <- regmatches(reset_lines, reset_matches)
if (any(lengths(reset_pieces) != 2L)) {
  fail("old-Windows reset-variable policy contains a malformed row")
}
observed_reset_variables <- vapply(
  reset_pieces, `[[`, character(1L), 2L
)
expected_reset_variables <- c(
  "R_HOME", "R_LIBS", "R_LIBS_USER", "R_LIBS_SITE",
  "R_DEFAULT_PACKAGES", "R_ENVIRON", "R_ENVIRON_USER", "R_PROFILE",
  "R_PROFILE_USER", "R_BUILD_ENVIRON", "R_CHECK_ENVIRON",
  "R_INSTALL_ENVIRON", "R_MAKEVARS_SITE", "R_MAKEVARS_USER", "R_USER",
  "R_HISTFILE", "R_ARCH", "R_INSTALL_TAR", "R_PKG_CFLAGS",
  "R_PKG_CPPFLAGS", "R_PKG_CXXFLAGS", "R_PKG_CXX_STD",
  "R_PKG_FFLAGS", "R_PKG_FCFLAGS", "R_PKG_LIBS", "CC", "CPP",
  "CXX", "CXX11", "CXX14", "CXX17", "CXX20", "CXX23", "FC",
  "F77", "F90", "F95", "OBJC", "OBJCXX", "CC_FOR_BUILD",
  "CPP_FOR_BUILD", "CXX_FOR_BUILD", "FC_FOR_BUILD", "AR", "AS",
  "LD", "NM", "OBJCOPY", "OBJDUMP", "RANLIB", "READELF", "SIZE",
  "STRINGS", "STRIP", "WINDRES", "DLLTOOL", "BINPREF", "BINPREF64",
  "M_ARCH", "CFLAGS", "CPPFLAGS", "CXXFLAGS", "CXX11FLAGS",
  "CXX14FLAGS", "CXX17FLAGS", "CXX20FLAGS", "CXX23FLAGS",
  "FCFLAGS", "FFLAGS", "FORTRANFLAGS", "LDFLAGS", "CPATH",
  "C_INCLUDE_PATH", "CPLUS_INCLUDE_PATH", "OBJC_INCLUDE_PATH",
  "LIBRARY_PATH", "COMPILER_PATH", "GCC_EXEC_PREFIX", "MAKE",
  "MAKEFLAGS", "MFLAGS", "GNUMAKEFLAGS", "MAKEFILES", "CONFIG_SITE",
  "PKG_CONFIG", "PKG_CONFIG_PATH", "PKG_CONFIG_LIBDIR",
  "PKG_CONFIG_SYSROOT_DIR"
)
expected_empty_files <- c(
  'R_ENVIRON = "Renviron.site"',
  'R_ENVIRON_USER = "Renviron.user"',
  'R_PROFILE = "Rprofile.site"',
  'R_PROFILE_USER = "Rprofile.user"',
  'R_MAKEVARS_SITE = "Makevars.site"',
  'R_MAKEVARS_USER = "Makevars.user"'
)
if (!identical(observed_reset_variables, expected_reset_variables) ||
    anyDuplicated(observed_reset_variables) ||
    count_fixed(
      'Remove-Item `',
      isolation_helper
    ) != 1L ||
    count_fixed(
      '-LiteralPath "Env:\\$Variable" `',
      isolation_helper
    ) != 1L ||
    count_fixed(
      "[Environment]::SetEnvironmentVariable(",
      isolation_helper
    ) != 1L ||
    any(vapply(
      expected_empty_files,
      count_fixed,
      integer(1L),
      haystack = isolation_helper
    ) != 1L) ||
    !grepl(
      '$ObservedHash -cne $EmptySHA256',
      isolation_helper,
      fixed = TRUE
    ) ||
    !grepl(
      '[IO.FileAttributes]::ReadOnly',
      isolation_helper,
      fixed = TRUE
    )) {
  fail("old-Windows hostile-environment policy differs from the reviewed list")
}
expected_rtools35_hashes <- c(
  "2d415b0fd5eacb43268e2ddf080b50f706d9fa2465b1e32d04f54ce936fac3da",
  "0d3d581bca702c777fc045a2fe69696e5979d86e819efe2350e2ac43f33f2b7f",
  "cbf5f996ef759be73502387c9d1296176f8bb7b6320b63cfb61371f7a98e7b59",
  "ce462e4ca812718a077ae4b67ebec0bd2df0e7a3bc1e31897e40895023e13c72"
)
if (any(vapply(
    expected_rtools35_hashes,
    count_fixed,
    integer(1L),
    haystack = toolchain$run
  ) != 1L) ||
    any(vapply(
      expected_rtools35_hashes,
      count_fixed,
      integer(1L),
      haystack = installer
    ) != 1L)) {
  fail("old-Windows workflow/installer do not bind exact Rtools35 executables")
}
summary_key_literals <- paste0(
  'paste0("',
  c(
    "r_version", "r_platform", "r_arch", "package_archive",
    "package_archive_sha256", "dll", "registered_call_routines",
    "compiler", "compiler_first_line"
  ),
  '=",'
)
summary_key_literals <- c(
  summary_key_literals,
  '"formatted_diagnostics=pass"',
  '"smoke=pass"'
)
if (any(vapply(
    summary_key_literals,
    count_fixed,
    integer(1L),
    haystack = runner
  ) != 1L)) {
  fail("old-Windows runner does not emit each exact summary key once")
}
native_process_owners <- function(source) {
  records <- list()
  character_literals <- function(expression) {
    if (is.character(expression)) {
      return(expression)
    }
    if (!is.call(expression) && !is.pairlist(expression) &&
        !is.expression(expression)) {
      return(character())
    }
    unlist(
      lapply(as.list(expression), character_literals),
      use.names = FALSE
    )
  }
  walk <- function(expression, owner = NULL) {
    if (!is.call(expression)) {
      return(invisible(NULL))
    }
    head <- if (is.symbol(expression[[1L]])) {
      as.character(expression[[1L]])
    } else {
      ""
    }
    if (head %in% c("<-", "=") && length(expression) == 3L) {
      assigned <- if (is.symbol(expression[[2L]])) {
        as.character(expression[[2L]])
      } else {
        NA_character_
      }
      walk(expression[[3L]], assigned)
      return(invisible(NULL))
    }
    if (identical(head, "run_native")) {
      literals <- character_literals(expression)
      kind <- if (all(c("CMD", "build") %in% literals)) {
        "build"
      } else if (all(c("CMD", "check") %in% literals)) {
        "check"
      } else {
        NULL
      }
      if (!is.null(kind)) {
        records[[length(records) + 1L]] <<- c(
          kind = kind,
          owner = if (is.null(owner)) NA_character_ else owner
        )
      }
    }
    for (element in as.list(expression)[-1L]) {
      walk(element, NULL)
    }
    invisible(NULL)
  }
  for (expression in parse(text = source, keep.source = FALSE)) {
    walk(expression)
  }
  if (!length(records)) {
    return(setNames(character(), character()))
  }
  kinds <- vapply(records, `[[`, character(1L), "kind")
  owners <- vapply(records, `[[`, character(1L), "owner")
  if (anyDuplicated(kinds)) {
    return(setNames(owners, kinds))
  }
  setNames(owners, kinds)
}
has_exact_process_ownership <- function(source) {
  owners <- native_process_owners(source)
  identical(
    owners[c("build", "check")],
    c(build = "build_process", check = "check_process")
  )
}
native_process_field_references <- function(source) {
  references <- character()
  walk <- function(expression) {
    if (!is.call(expression)) {
      return(invisible(NULL))
    }
    if (identical(expression[[1L]], as.name("$")) &&
        length(expression) == 3L &&
        is.symbol(expression[[2L]]) &&
        is.symbol(expression[[3L]])) {
      owner <- as.character(expression[[2L]])
      if (owner %in% c("build_process", "check_process")) {
        references <<- c(
          references,
          paste(owner, as.character(expression[[3L]]), sep = "$")
        )
      }
    }
    for (element in as.list(expression)[-1L]) {
      walk(element)
    }
    invisible(NULL)
  }
  for (expression in parse(text = source, keep.source = FALSE)) {
    walk(expression)
  }
  references
}
has_exact_check_result_consumption <- function(source) {
  references <- native_process_field_references(source)
  counts <- table(factor(
    references,
    levels = c(
      "check_process$status",
      "check_process$stdout",
      "check_process$stderr"
    )
  ))
  length(references) == 5L &&
    identical(as.integer(counts), c(3L, 1L, 1L)) &&
    !any(startsWith(references, "build_process$"))
}
if (!has_exact_process_ownership(runner) ||
    !has_exact_check_result_consumption(runner)) {
  fail("old-Windows runner does not retain separate build/check child results")
}
wrong_build_owner <- sub(
  "build_process <- run_native(",
  "check_process <- run_native(",
  runner,
  fixed = TRUE
)
missing_check_owner <- sub(
  "check_process <- run_native(",
  "run_native(",
  runner,
  fixed = TRUE
)
wrong_status_owner <- gsub(
  "check_process$",
  "build_process$",
  runner,
  fixed = TRUE
)
if (identical(wrong_build_owner, runner) ||
    identical(missing_check_owner, runner) ||
    identical(wrong_status_owner, runner) ||
    has_exact_process_ownership(wrong_build_owner) ||
    has_exact_process_ownership(missing_check_owner) ||
    has_exact_check_result_consumption(wrong_status_owner)) {
  fail("old-Windows process-ownership audit accepts a build/check result mix-up")
}
expected_closure <- c(
  "backports", "checkmate", "data.table", "R6", "cli", "digest", "mlr3misc"
)
if (any(!vapply(
      expected_closure,
      function(package) grepl(
        paste0('"', package, '"'),
        installer,
        fixed = TRUE
      ),
      logical(1L)
    )) ||
    !grepl("runtime-r-3.6.3-packages.lock", installer, fixed = TRUE) ||
    !grepl("Get-FileHash", installer, fixed = TRUE) ||
    !grepl('"CMD",', installer, fixed = TRUE) ||
    !grepl('"INSTALL",', installer, fixed = TRUE) ||
    grepl('$env:R_PKG_CXX_STD = "CXX11"', installer, fixed = TRUE) ||
    !grepl('$HadPkgCxxStd', installer, fixed = TRUE) ||
    !grepl(
      '$env:R_PKG_CXX_STD = $OriginalPkgCxxStd',
      installer,
      fixed = TRUE
    ) ||
    !grepl(
      '-LiteralPath "Env:R_PKG_CXX_STD"',
      installer,
      fixed = TRUE
    ) ||
    !grepl('"digest-install.log"', installer, fixed = TRUE) ||
    !grepl('"digest-cxx11.tsv"', installer, fixed = TRUE) ||
    !grepl('"rtools35.tsv"', installer, fixed = TRUE) ||
    !grepl(
      '"C:/Rtools/mingw_64/bin/g++.exe"',
      installer,
      fixed = TRUE
    ) ||
    !grepl(
      paste0(
        '"g++.exe ',
        '(x86_64-posix-seh, Built by MinGW-W64 project) 4.9.3"'
      ),
      installer,
      fixed = TRUE
    ) ||
    grepl("install.packages", installer, fixed = TRUE) ||
    !grepl('"--no-tests"', runner, fixed = TRUE) ||
    !grepl('"--no-examples"', runner, fixed = TRUE) ||
    !grepl('"--ignore-vignettes"', runner, fixed = TRUE) ||
    !grepl('"--no-manual"', runner, fixed = TRUE) ||
    !contains_contiguous(
      trimws(runner_lines),
      expected_runner_check_environment
    ) ||
    count_fixed(
      '"_R_CHECK_RD_XREFS_" = "false"',
      runner
    ) != 1L ||
    !grepl('"Status: 1 NOTE"', runner, fixed = TRUE) ||
    !grepl(
      "Packages suggested but not available for checking:",
      runner,
      fixed = TRUE
    ) ||
    !grepl('"dynamicLookup"', runner, fixed = TRUE) ||
    !grepl("objdump", runner, fixed = TRUE) ||
    !grepl("saveRDS", runner, fixed = TRUE) ||
    !grepl("generate_design_grid", runner, fixed = TRUE) ||
    !grepl(
      'to_tune <- get("to_tune", envir = namespace)',
      runner,
      fixed = TRUE
    ) ||
    !grepl(
      'identical(compiler, "C:/Rtools/mingw_64/bin/gcc.exe")',
      runner,
      fixed = TRUE
    ) ||
    !grepl(
      paste0(
        '"gcc.exe ',
        '(x86_64-posix-seh, Built by MinGW-W64 project) 4.9.3"'
      ),
      runner,
      fixed = TRUE
    ) ||
    !grepl("growth_token <- to_tune(", runner, fixed = TRUE) ||
    !grepl(
      "Assertion on 'xs' failed: value: Element 1 is not <=",
      runner,
      fixed = TRUE
    ) ||
    !grepl(
      'PARADOX_R36_ISOLATION_PHASE"), "candidate"',
      runner,
      fixed = TRUE
    ) ||
    !grepl(
      "PSNativeCommandUseErrorActionPreference",
      installer,
      fixed = TRUE
    )) {
  fail("reviewed old-Windows scripts lost source-lock, DLL, or smoke coverage")
}

old_provenance <- old_step_by_name("Retain old-Windows provenance")
old_upload <- old_step_by_name("Upload old-Windows evidence")
old_provenance_lines <- trimws(strsplit(
  old_provenance$run, "\n", fixed = TRUE
)[[1L]])
expected_platform_payload <- c(
  "[IO.File]::WriteAllText(",
  "$rPlatformScript,",
  "'cat(R.version$platform)',",
  "$encoding",
  ")"
)
expected_platform_file <- c(
  "$rPlatformScript = Join-Path `",
  "$env:RUNNER_TEMP `",
  '"retain-r36-platform.R"',
  "if (Test-Path -LiteralPath $rPlatformScript) {",
  'throw "R platform probe path is not fresh"',
  "}",
  "$encoding = New-Object Text.UTF8Encoding($false)",
  expected_platform_payload
)
expected_platform_execution <- c(
  "try {",
  "$rPlatformOutput = @(",
  "& $rScriptExe --vanilla $rPlatformScript",
  ")",
  "$rPlatformStatus = $LASTEXITCODE",
  "} finally {",
  "Remove-Item -LiteralPath $rPlatformScript -Force",
  "}",
  '$rPlatform = ($rPlatformOutput -join "`n").Trim()',
  "if ($rPlatformStatus -ne 0 -or",
  "(Test-Path -LiteralPath $rPlatformScript) -or",
  "-not $rPlatform) {"
)
if (sum(old_provenance_lines ==
      '$rScriptExe = "C:\\R\\bin\\x64\\Rscript.exe"') != 1L ||
    sum(grepl(
      "^\\$rScriptExe[[:space:]]*=",
      old_provenance_lines
    )) != 1L ||
    sum(grepl(
      "^\\$rPlatformScript[[:space:]]*=",
      old_provenance_lines
    )) != 1L ||
    sum(old_provenance_lines == "$rPlatformScript = Join-Path `") != 1L ||
    sum(old_provenance_lines ==
      "& $rScriptExe --vanilla $rPlatformScript") != 1L ||
    !contains_contiguous(old_provenance_lines, expected_platform_file) ||
    !contains_contiguous(old_provenance_lines, expected_platform_execution) ||
    !grepl("[IO.File]::WriteAllText(", old_provenance$run, fixed = TRUE) ||
    !grepl("Text.UTF8Encoding($false)", old_provenance$run, fixed = TRUE) ||
    !grepl(
      "Remove-Item -LiteralPath $rPlatformScript -Force",
      old_provenance$run,
      fixed = TRUE
    ) ||
    grepl(" --vanilla -e", old_provenance$run, fixed = TRUE) ||
    grepl("& Rscript.exe", old_provenance$run, fixed = TRUE)) {
  fail(
    "old-Windows provenance does not use one fresh, no-BOM file through ",
    "the direct x86-64 Rscript"
  )
}
if (!identical(old_provenance[["if"]], "always()") ||
    !identical(old_provenance$shell, "pwsh") ||
    !grepl("checked_out_sha=", old_provenance$run, fixed = TRUE) ||
    !identical(old_upload[["if"]], "always()") ||
    !identical(
      old_upload$uses,
      "actions/upload-artifact@043fb46d1a93c77aae656e7c1c64a875d1fc6a0a"
    ) ||
    !identical(
      old_upload$with$name,
      "paradox-2.0.0-portability-windows-r3.6.3-x86_64"
    ) ||
    !grepl("environment-isolation", old_provenance$run, fixed = TRUE) ||
    any(!vapply(
      c("toolchain.tsv", "closure.tsv", "candidate.tsv"),
      grepl,
      logical(1L),
      x = old_provenance$run,
      fixed = TRUE
    )) ||
    !identical(old_upload$with$path, "C:\\p36-evidence") ||
    !identical(old_upload$with[["if-no-files-found"]], "error")) {
  fail("old-Windows provenance and artifact retention are not fail-closed")
}

if (mode == "release") {
  old_identity <- old_step_by_name("Verify frozen harness checkout")
  check_release_identity(
    old_identity,
    "old-Windows checkout assertion",
    materialize_lock = TRUE
  )
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

completion_job <- workflow$jobs[["portability-complete"]]
if (!identical(completion_job$name, "Verify required check jobs") ||
    !identical(completion_job[["if"]], "${{ always() }}") ||
    !identical(
      completion_job$needs,
      c("r-cmd-check", "r36-windows")
    ) ||
    !identical(completion_job[["runs-on"]], "ubuntu-latest") ||
    !is.list(completion_job$steps) || length(completion_job$steps) != 1L) {
  fail("required-job completion gate has an unexpected shape")
}
required_results <- completion_job$steps[[1L]]
if (!identical(required_results$name, "Verify required job conclusions") ||
    !identical(required_results$shell, "bash") ||
    !identical(
      required_results$env$R_CMD_CHECK_RESULT,
      "${{ needs.r-cmd-check.result }}"
    ) ||
    !identical(
      required_results$env$R36_WINDOWS_RESULT,
      "${{ needs.r36-windows.result }}"
    ) ||
    !grepl("set -euo pipefail", required_results$run, fixed = TRUE)) {
  fail("required-job completion step is not bound to both required jobs")
}

evaluate_required_results <- function(matrix_result, old_windows_result) {
  suppressWarnings(system2(
    "/usr/bin/bash",
    args = c("-c", shQuote(required_results$run)),
    env = c(
      paste0("R_CMD_CHECK_RESULT=", matrix_result),
      paste0("R36_WINDOWS_RESULT=", old_windows_result)
    ),
    stdout = FALSE,
    stderr = FALSE
  ))
}
outcomes <- c("success", "failure", "cancelled", "skipped", "")
cases <- expand.grid(
  matrix = outcomes,
  old_windows = outcomes,
  stringsAsFactors = FALSE
)
accepted <- mapply(
  function(matrix, old_windows) {
    evaluate_required_results(matrix, old_windows) == 0L
  },
  cases$matrix,
  cases$old_windows,
  USE.NAMES = FALSE
)
expected_acceptance <- cases$matrix == "success" &
  cases$old_windows == "success"
if (!identical(accepted, expected_acceptance)) {
  fail("required-job completion gate does not reject every non-success result")
}

cat("portability workflow regression tests passed\n")
