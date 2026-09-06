#!/usr/bin/env Rscript

fail <- function(...) stop(..., call. = FALSE)
is_symbolic <- function(path) {
  target <- Sys.readlink(path)
  length(target) == 1L && !is.na(target) && nzchar(target)
}
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) < 3L || length(arguments) > 4L) {
  fail(paste0(
    "usage: render-portability-release-workflow.R ",
    "CANDIDATE_TAG CANDIDATE_COMMIT OUTPUT [SOURCE_WORKFLOW]"
  ))
}

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  fail("could not identify the release-workflow renderer")
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

candidate_tag <- arguments[[1L]]
candidate_commit <- arguments[[2L]]
output_argument <- arguments[[3L]]
source_argument <- if (length(arguments) == 4L) {
  arguments[[4L]]
} else {
  file.path(root, ".github", "workflows", "r-cmd-check.yml")
}
if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", candidate_tag)) {
  fail("candidate tag is not one exact safe Git tag spelling")
}
if (!grepl("^[0-9a-f]{40}$", candidate_commit)) {
  fail("candidate commit must be one lowercase 40-hex object ID")
}

if (!file.exists(source_argument) || dir.exists(source_argument) ||
    is_symbolic(source_argument)) {
  fail(
    "source workflow is absent, non-regular, or symbolic: ",
    source_argument
  )
}
source <- normalizePath(
  source_argument, winslash = "/", mustWork = TRUE
)
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
helper_relative <- "scripts/environment/install-hosted-r36-windows.ps1"
helper_blob <- candidate_blob(helper_relative, "old-Windows installer")
lock_relative <- "environment/runtime-r-3.6.3-packages.lock"
reviewed_lock_blob <- "5e9fb484b63cff6ee51ab2101dcaa37defd0e603"
reviewed_lock_sha256 <-
  "9007e3a2d7eecb1057bf9610a2f2ffacf617c224b9aeb9b91bd1ef5ae85f59c5"
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
    dir.exists(lock_blob_copy) || is_symbolic(lock_blob_copy)) {
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

output_parent_argument <- dirname(output_argument)
output_name <- basename(output_argument)
if (!nzchar(output_name) || output_name %in% c(".", "..")) {
  fail("output workflow path has no ordinary file name")
}
if (!dir.exists(output_parent_argument) ||
    is_symbolic(output_parent_argument)) {
  fail("output workflow parent is absent, non-directory, or symbolic")
}
output_parent <- normalizePath(
  output_parent_argument, winslash = "/", mustWork = TRUE
)
output <- file.path(output_parent, output_name)
if (file.exists(output) || dir.exists(output) || is_symbolic(output)) {
  fail("refusing to overwrite output workflow: ", output)
}

rscript <- file.path(R.home("bin"), "Rscript")
validator <- file.path(
  root, "scripts", "environment", "test-portability-workflow.R"
)
run_validator <- function(mode, workflow) {
  args <- c(
    "--vanilla",
    shQuote(validator),
    shQuote(root),
    mode,
    shQuote(workflow)
  )
  if (identical(mode, "release")) {
    args <- c(args, candidate_tag, candidate_commit)
  }
  result <- suppressWarnings(system2(
    rscript,
    args = args,
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(result, "status")
  if (is.null(status)) {
    status <- 0L
  }
  if (status != 0L) {
    fail(
      mode,
      " portability workflow validation failed:\n",
      paste(result, collapse = "\n")
    )
  }
  invisible(TRUE)
}
run_validator("general", source)

lines <- readLines(source, warn = FALSE, encoding = "UTF-8")
replace_exact <- function(lines, old, replacement, label, count = 1L) {
  positions <- which(lines == old)
  if (!identical(length(positions), count)) {
    fail(label, " anchor count differs: expected ", count, ", observed ",
      length(positions))
  }
  for (position in rev(positions)) {
    before <- if (position > 1L) lines[seq_len(position - 1L)] else character()
    after <- if (position < length(lines)) {
      lines[seq.int(position + 1L, length(lines))]
    } else {
      character()
    }
    lines <- c(before, replacement, after)
  }
  lines
}

first_removed_row <- which(lines ==
  "          - {os: ubuntu-latest,   arch: x86_64, r: 'devel'}")
last_removed_row <- which(lines ==
  paste0(
    "          - {os: ubuntu-latest,   arch: x86_64, r: '4.3', ",
    "depends_only: true, no_tests: true}"
  ))
if (length(first_removed_row) != 1L ||
    length(last_removed_row) != 1L ||
    first_removed_row >= last_removed_row) {
  fail("ordinary portability matrix reduction anchors differ")
}
lines <- lines[-seq.int(first_removed_row, last_removed_row)]

lines <- replace_exact(
  lines,
  paste0(
    "    name: ${{ matrix.config.os }} / ${{ matrix.config.arch }} ",
    "(${{ matrix.config.r }})",
    "${{ matrix.config.depends_only && ' – noSuggests' || '' }}",
    "${{ matrix.config.no_tests && ' – noTests' || '' }}"
  ),
  paste0(
    "    name: ${{ matrix.config.os }} / ${{ matrix.config.arch }} ",
    "(${{ matrix.config.r }})"
  ),
  "release matrix job name"
)

checkout_prefix <- c(
  "      - uses: actions/checkout@df4cb1c069e1874edd31b4311f1884172cec0e10",
  "        with:",
  "          # Check out the immutable companion whose package-facing source",
  "          # is proved below to equal the exact frozen candidate.",
  "          ref: ${{ github.sha }}",
  "          fetch-depth: 2",
  "          persist-credentials: false",
  "",
  "      - name: Verify frozen harness checkout",
  "        shell: bash",
  "        run: |",
  "          set -euo pipefail",
  paste0("          readonly candidate=", candidate_commit),
  '          readonly harness="${GITHUB_SHA:?}"',
  '          head="$(git rev-parse HEAD)"',
  "          readonly head",
  '          parents="$(git rev-list --parents -n 1 "$harness")"',
  "          readonly parents",
  '          test "$head" = "$harness"',
  '          test "$parents" = "$harness $candidate"',
  paste0("          readonly helper=", helper_relative),
  paste0("          readonly expected_helper_blob=", helper_blob),
  '          helper_entry="$(git ls-tree "$harness" -- "$helper")"',
  "          readonly helper_entry",
  paste0(
    "          expected_helper_entry=\"$(printf ",
    "'100644 blob %s\\t%s' ",
    "\"$expected_helper_blob\" \"$helper\")\""
  ),
  "          readonly expected_helper_entry",
  '          test "$helper_entry" = "$expected_helper_entry"'
)
lock_identity_block <- c(
  paste0("          readonly lock=", lock_relative),
  paste0("          readonly expected_lock_blob=", lock_blob),
  paste0("          readonly expected_lock_sha256=", lock_sha256),
  '          lock_entry="$(git ls-tree "$harness" -- "$lock")"',
  "          readonly lock_entry",
  paste0(
    "          expected_lock_entry=\"$(printf ",
    "'100644 blob %s\\t%s' ",
    "\"$expected_lock_blob\" \"$lock\")\""
  ),
  "          readonly expected_lock_entry",
  '          test "$lock_entry" = "$expected_lock_entry"'
)
companion_identity_block <- c(
  paste0(
    "          changed=\"$(git diff --name-only ",
    "\"$candidate\" \"$harness\")\""
  ),
  "          readonly changed",
  paste0(
    "          expected_changed=\"$(printf '%s\\n' ",
    ".github/workflows/r-cmd-check.yml)\""
  ),
  "          readonly expected_changed",
  '          test "$changed" = "$expected_changed"',
  '          test -z "$(git status --porcelain=v1 --untracked-files=all)"'
)
lock_materialization_block <- c(
  '          lock_tmp="$(mktemp "${lock}.raw.XXXXXX")"',
  "          readonly lock_tmp",
  '          trap \'rm -f -- "$lock_tmp"\' EXIT',
  '          git cat-file blob "$expected_lock_blob" > "$lock_tmp"',
  '          chmod 0644 "$lock_tmp"',
  paste0(
    "          test \"$(git hash-object --no-filters -- ",
    "\"$lock_tmp\")\" = \"$expected_lock_blob\""
  ),
  paste0(
    "          test \"$(sha256sum -- \"$lock_tmp\" | ",
    "cut -d ' ' -f 1)\" = \"$expected_lock_sha256\""
  ),
  '          mv -f -- "$lock_tmp" "$lock"',
  "          trap - EXIT",
  paste0(
    "          test \"$(git hash-object --no-filters -- ",
    "\"$lock\")\" = \"$expected_lock_blob\""
  ),
  paste0(
    "          test \"$(sha256sum -- \"$lock\" | ",
    "cut -d ' ' -f 1)\" = \"$expected_lock_sha256\""
  )
)
checkout_block <- c(checkout_prefix, companion_identity_block)
old_windows_checkout_block <- c(
  checkout_prefix,
  lock_identity_block,
  companion_identity_block,
  lock_materialization_block
)
checkout_positions <- which(lines == "      - uses: actions/checkout@v6")
if (!identical(length(checkout_positions), 2L)) {
  fail("trigger-ref checkout anchor count differs")
}
for (index in rev(seq_along(checkout_positions))) {
  position <- checkout_positions[[index]]
  replacement <- if (index == 1L) {
    checkout_block
  } else {
    old_windows_checkout_block
  }
  before <- if (position > 1L) lines[seq_len(position - 1L)] else character()
  after <- if (position < length(lines)) {
    lines[seq.int(position + 1L, length(lines))]
  } else {
    character()
  }
  lines <- c(before, replacement, after)
}
lines <- replace_exact(
  lines,
  "      - uses: r-lib/actions/setup-r@v2",
  paste0(
    "      - uses: r-lib/actions/setup-r@",
    "d3c5be51b12e724e68f33216ca3c148b66d5f0b6"
  ),
  "matrix setup-r action"
)
lines <- replace_exact(
  lines,
  "      - uses: r-lib/actions/setup-r-dependencies@v2",
  paste0(
    "      - uses: r-lib/actions/setup-r-dependencies@",
    "d3c5be51b12e724e68f33216ca3c148b66d5f0b6"
  ),
  "matrix setup-r-dependencies action"
)
lines <- replace_exact(
  lines,
  paste0(
    "          _R_CHECK_DEPENDS_ONLY_: ",
    "${{ matrix.config.depends_only && 'TRUE' || 'FALSE' }}"
  ),
  "          _R_CHECK_DEPENDS_ONLY_: 'FALSE'",
  "release depends-only value"
)
lines <- replace_exact(
  lines,
  paste0(
    "          NOT_CRAN: ",
    "${{ matrix.config.depends_only && 'FALSE' || 'TRUE' }}"
  ),
  "          NOT_CRAN: 'TRUE'",
  "release NOT_CRAN value"
)
lines <- replace_exact(
  lines,
  paste0(
    "          PARADOX_CHECK_NO_TESTS: ",
    "${{ matrix.config.no_tests && 'TRUE' || 'FALSE' }}"
  ),
  "          PARADOX_CHECK_NO_TESTS: 'FALSE'",
  "release package-test value"
)

matrix_evidence <- c(
  "      - name: Retain run provenance",
  "        if: always()",
  "        shell: bash",
  "        run: |",
  "          set -euo pipefail",
  "          test ! -e ci-evidence",
  "          mkdir ci-evidence",
  "          {",
  "            printf 'workflow_sha=%s\\n' \"$GITHUB_SHA\"",
  "            printf 'workflow_ref=%s\\n' \"$GITHUB_REF\"",
  "            printf 'checked_out_sha=%s\\n' \"$(git rev-parse HEAD)\"",
  "            printf 'runner_os=%s\\n' \"$RUNNER_OS\"",
  "            printf 'runner_arch=%s\\n' \"$RUNNER_ARCH\"",
  "            # The single-quoted text is R code, not shell.",
  "            # shellcheck disable=SC2016",
  paste0(
    "            printf 'r_platform=%s\\n' ",
    "\"$(Rscript --vanilla -e 'cat(R.version$platform)')\""
  ),
  "          } > ci-evidence/provenance.txt",
  "",
  "      - name: Upload check evidence",
  "        if: always()",
  paste0(
    "        uses: actions/upload-artifact@",
    "043fb46d1a93c77aae656e7c1c64a875d1fc6a0a"
  ),
  "        with:",
  paste0(
    "          name: paradox-2.0.0-portability-",
    "${{ matrix.config.os }}-${{ matrix.config.arch }}"
  ),
  "          path: |",
  "            ci-evidence/provenance.txt",
  "            check",
  "          if-no-files-found: error",
  "          retention-days: 30",
  ""
)
old_job_position <- which(lines == "  r36-windows:")
if (length(old_job_position) != 1L) {
  fail("separate old-Windows job anchor differs")
}
lines <- append(lines, matrix_evidence, after = old_job_position - 1L)

temporary <- tempfile(
  pattern = paste0(".", output_name, "."),
  tmpdir = output_parent
)
on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)
writeLines(lines, temporary, useBytes = TRUE)
if (!file.exists(temporary) || dir.exists(temporary) ||
    is_symbolic(temporary)) {
  fail("could not create a plain temporary release workflow")
}
run_validator("release", temporary)
if (!file.rename(temporary, output)) {
  fail("could not atomically publish rendered release workflow")
}

cat("rendered and validated release portability workflow: ", output, "\n",
  sep = "")
