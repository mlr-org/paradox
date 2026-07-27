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

checkout_block <- c(
  "      - uses: actions/checkout@df4cb1c069e1874edd31b4311f1884172cec0e10",
  "        with:",
  "          # The workflow comes from this harness-only companion, but build",
  "          # and check inputs always come from the exact frozen candidate.",
  paste0("          ref: ", candidate_tag),
  "          persist-credentials: false",
  "",
  "      - name: Verify frozen candidate checkout",
  "        shell: bash",
  "        run: |",
  "          set -euo pipefail",
  paste0("          readonly expected=", candidate_commit),
  '          test "$(git rev-parse HEAD)" = "$expected"',
  '          test -z "$(git status --porcelain=v1 --untracked-files=all)"'
)
lines <- replace_exact(
  lines,
  "      - uses: actions/checkout@v6",
  checkout_block,
  "trigger-ref checkout",
  count = 2L
)
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
