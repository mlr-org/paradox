#!/usr/bin/env Rscript

started <- proc.time()[["elapsed"]]
command <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command, value = TRUE)
if (length(file_argument) != 1L) {
  stop("could not identify CRAN refresh self-test", call. = FALSE)
}
script <- normalizePath(
  sub("^--file=", "", file_argument), winslash = "/", mustWork = TRUE
)
directory <- dirname(script)
sys.source(file.path(directory, "source-fetch-common.R"), envir = environment())
sys.source(file.path(directory, "cran-refresh-common.R"), envir = environment())

fail <- function(...) stop(..., call. = FALSE)
expect_identical <- function(current, expected, label) {
  if (!identical(current, expected)) fail(label)
}
expect_error <- function(expression, pattern) {
  message <- tryCatch({ force(expression); NULL }, error = conditionMessage)
  if (is.null(message)) fail("expected an error containing: ", pattern)
  if (!grepl(pattern, message, fixed = TRUE)) {
    fail("unexpected error; expected '", pattern, "', got '", message, "'")
  }
  invisible(message)
}

temporary <- tempfile("paradox-cran-refresh-self-test-")
if (!dir.create(temporary, mode = "0700")) fail("could not create fixture root")
on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)

packages <- file.path(temporary, "PACKAGES.gz")
connection <- gzfile(packages, open = "wt", encoding = "UTF-8")
writeLines(c(
  "Package: alpha",
  "Version: 1.0.0",
  paste0("MD5sum: ", strrep("a", 32L)),
  "Depends: R (>= 3.6), paradox (>= 1.0.0)",
  "",
  "Package: beta",
  "Version: 2.0.0",
  paste0("MD5sum: ", strrep("b", 32L)),
  "Imports: R6,",
  "    paradox",
  "",
  "Package: gamma",
  "Version: 3.0.0",
  paste0("MD5sum: ", strrep("c", 32L)),
  "Suggests: paradox, tinytest",
  "",
  "Package: unrelated",
  "Version: 1.0.0",
  paste0("MD5sum: ", strrep("d", 32L)),
  "Imports: paradoxical"
), connection, useBytes = TRUE)
close(connection)
metadata <- cran_refresh_read_packages(packages)
live_from_dcf <- cran_refresh_direct_reverses(metadata)
expect_identical(
  live_from_dcf$package, c("alpha", "beta", "gamma"),
  "DCF projection did not select the exact reverse set"
)
expect_identical(
  live_from_dcf$relation, c("depends", "imports", "suggests"),
  "DCF projection changed dependency relations"
)
expect_identical(
  live_from_dcf$md5,
  c(strrep("a", 32L), strrep("b", 32L), strrep("c", 32L)),
  "DCF projection changed CRAN MD5 values"
)

duplicate_relation <- metadata
duplicate_relation$Imports[duplicate_relation$Package == "alpha"] <- "paradox"
expect_error(
  cran_refresh_direct_reverses(duplicate_relation),
  "declares paradox in multiple dependency fields"
)
bad_md5 <- metadata
bad_md5$MD5sum[bad_md5$Package == "beta"] <- "not-an-md5"
expect_error(
  cran_refresh_direct_reverses(bad_md5),
  "reverse-dependency metadata is malformed"
)
malformed_dependency <- metadata
malformed_dependency$Imports[malformed_dependency$Package == "unrelated"] <-
  "not a package (>= 1.0)"
expect_error(
  cran_refresh_direct_reverses(malformed_dependency),
  "dependency metadata is malformed"
)
unsupported_relation <- metadata
unsupported_relation$LinkingTo[
  unsupported_relation$Package == "unrelated"
] <- "paradox"
expect_error(
  cran_refresh_direct_reverses(unsupported_relation),
  "unsupported relation (linkingto): unrelated"
)

inventory <- data.frame(
  relation = c("depends", "imports", "suggests", "imports"),
  package = c("alpha", "beta", "gamma", "bio"),
  source = c("CRAN", "CRAN", "CRAN", "Bioconductor"),
  priority = c("1", "1", "3", "2"),
  notes = c("Alpha old metadata.", "Beta metadata.", "Gamma metadata.",
    "Bioconductor metadata."),
  stringsAsFactors = FALSE
)
snapshot <- data.frame(
  package = c("alpha", "beta", "gamma"),
  relation = c("depends", "imports", "suggests"),
  source = rep("CRAN", 3L),
  priority = c("1", "1", "3"),
  notes = inventory$notes[1:3],
  version = rep("1.0.0", 3L),
  repository = rep(cran_refresh_repository, 3L),
  archive = paste0(c("alpha", "beta", "gamma"), "_1.0.0.tar.gz"),
  md5 = c(strrep("a", 32L), strrep("b", 32L), strrep("c", 32L)),
  sha256 = c(strrep("a", 64L), strrep("b", 64L), strrep("c", 64L)),
  stringsAsFactors = FALSE
)
live <- data.frame(
  package = c("alpha", "beta", "delta"),
  relation = c("imports", "imports", "imports"),
  version = c("1.0.0", "2.0.0", "1.0.0"),
  md5 = c(strrep("a", 32L), strrep("d", 32L), strrep("e", 32L)),
  stringsAsFactors = FALSE
)
review <- data.frame(
  action = c("relation", "add", "remove"),
  package = c("alpha", "delta", "gamma"),
  relation = c("imports", "imports", "suggests"),
  priority = c("1", "1", "3"),
  notes = c("Alpha reviewed relation.", "Delta reviewed addition.",
    "Gamma metadata."),
  stringsAsFactors = FALSE
)

expect_error(
  cran_refresh_plan(inventory, snapshot, live),
  "lack an exact reviewed row"
)
missing_review <- review[review$package != "gamma", , drop = FALSE]
expect_error(
  cran_refresh_plan(inventory, snapshot, live, missing_review),
  "lack an exact reviewed row"
)
wrong_relation <- review
wrong_relation$relation[wrong_relation$package == "delta"] <- "suggests"
expect_error(
  cran_refresh_plan(inventory, snapshot, live, wrong_relation),
  "actions or relations disagree"
)
wrong_removal <- review
wrong_removal$notes[wrong_removal$package == "gamma"] <- "Rewritten removal."
expect_error(
  cran_refresh_plan(inventory, snapshot, live, wrong_removal),
  "removals must repeat the exact old metadata"
)
duplicate_review <- rbind(review, review[1L, , drop = FALSE])
expect_error(
  cran_refresh_plan(inventory, snapshot, live, duplicate_review),
  "malformed or duplicated"
)
bad_priority <- review
bad_priority$priority[bad_priority$package == "delta"] <- "01"
expect_error(
  cran_refresh_plan(inventory, snapshot, live, bad_priority),
  "canonical non-negative integers"
)

plan <- cran_refresh_plan(inventory, snapshot, live, review)
expect_identical(
  plan$inventory$package, c("alpha", "beta", "delta", "bio"),
  "reviewed addition/removal changed deterministic inventory placement"
)
expect_identical(
  plan$inventory$relation, rep("imports", 4L),
  "reviewed relation was not installed"
)
expect_identical(
  plan$inventory$notes[[1L]], "Alpha reviewed relation.",
  "reviewed relation metadata was not installed"
)
expect_identical(
  plan$snapshot$package, c("alpha", "beta", "delta"),
  "proposed snapshot does not match the proposed CRAN inventory"
)
expect_identical(
  plan$snapshot$version, c("1.0.0", "2.0.0", "1.0.0"),
  "proposed snapshot did not select live versions"
)
expect_identical(
  plan$archive_plan$action, c("copy", "download", "download"),
  "archive reuse/download planning is incorrect"
)
expect_identical(
  plan$changes$structural_change,
  c("relation", "", "remove", "add"),
  "structural-change report is incorrect"
)
expect_identical(
  plan$changes$version_change,
  c("", "version", "", ""),
  "version-change report is incorrect"
)

stable_live <- data.frame(
  package = snapshot$package,
  relation = snapshot$relation,
  version = c("1.0.0", "1.1.0", "1.0.0"),
  md5 = c(snapshot$md5[[1L]], strrep("f", 32L), snapshot$md5[[3L]]),
  stringsAsFactors = FALSE
)
stable_plan <- cran_refresh_plan(
  inventory, snapshot, stable_live, cran_refresh_empty_review()
)
expect_identical(
  stable_plan$archive_plan$action, c("copy", "download", "copy"),
  "ordinary version-only refresh unexpectedly required structural review"
)
unexpected_review <- data.frame(
  action = "relation", package = "alpha", relation = "depends",
  priority = "1", notes = "Alpha old metadata.", stringsAsFactors = FALSE
)
expect_error(
  cran_refresh_plan(inventory, snapshot, stable_live, unexpected_review),
  "lack an exact reviewed row"
)

expect_identical(
  cran_refresh_validate_run_id("release-20260801.r1"),
  "release-20260801.r1",
  "safe refresh run ID was rejected"
)
for (unsafe in c("", ".", "..", "../escape", "/absolute", "white space")) {
  expect_error(cran_refresh_validate_run_id(unsafe), "safe name")
}
parsed <- cran_refresh_parse_arguments(c(
  "--root", "/tmp/root", "--run-id=refresh-r1",
  "--reviewed-changes", "/tmp/review.tsv"
))
expect_identical(parsed$root, "/tmp/root", "--root parsing failed")
expect_identical(parsed$run_id, "refresh-r1", "--run-id parsing failed")
expect_identical(parsed$reviewed_changes, "/tmp/review.tsv",
  "--reviewed-changes parsing failed")
expect_error(
  cran_refresh_parse_arguments(c("--run-id", "one", "--run-id", "two")),
  "may be supplied only once"
)
expect_error(cran_refresh_parse_arguments("--network-mode"), "unknown option")

retained_source <- file.path(temporary, "review-input.tsv")
writeLines(c("action\tpackage\trelation\tpriority\tnotes",
  "add\tdelta\timports\t1\tReviewed."), retained_source, useBytes = TRUE)
retained_hash <- compat_fetch_file_hashes(
  retained_source, "review fixture"
)[[1L]]
retained_copy <- file.path(temporary, "review-input.retained.tsv")
cran_refresh_retain_input(
  retained_source, retained_copy, retained_hash, "review fixture"
)
expect_identical(
  readBin(retained_copy, "raw", n = file.info(retained_copy)$size),
  readBin(retained_source, "raw", n = file.info(retained_source)$size),
  "retained review input did not preserve exact bytes"
)
expect_error(
  cran_refresh_retain_input(
    retained_source, retained_copy, retained_hash, "review fixture"
  ),
  "refusing to overwrite retained input"
)
expect_error(
  cran_refresh_retain_input(
    retained_source, file.path(temporary, "wrong-hash.tsv"),
    strrep("0", 64L), "review fixture"
  ),
  "retained review fixture changed"
)

written_tsv <- file.path(temporary, "written.tsv")
cran_refresh_write_tsv(
  data.frame(field = "value", stringsAsFactors = FALSE), written_tsv
)
expect_identical(
  readLines(written_tsv, warn = FALSE), c("field", "value"),
  "fresh TSV output was not published exactly"
)
expect_error(
  cran_refresh_write_tsv(
    data.frame(field = "other", stringsAsFactors = FALSE), written_tsv
  ),
  "refusing to overwrite refresh output"
)

entry_lines <- readLines(file.path(directory, "refresh-cran-snapshot.R"),
  warn = FALSE)
common_lines <- readLines(file.path(directory, "cran-refresh-common.R"),
  warn = FALSE)
if (!any(grepl('cran_refresh_main(commandArgs(trailingOnly = TRUE), script)',
      entry_lines, fixed = TRUE)) ||
    sum(grepl("utils::download.file(", common_lines, fixed = TRUE)) != 2L ||
    !any(grepl('file.path(compat_root, "cran-sources")', common_lines,
      fixed = TRUE)) ||
    any(grepl("file.remove(canonical", common_lines, fixed = TRUE)) ||
    any(grepl("unlink(canonical", common_lines, fixed = TRUE))) {
  fail("tracked refresh entry point lost its staged-only I/O boundary")
}

elapsed <- proc.time()[["elapsed"]] - started
cat("cran_refresh_self_test=passed\n")
cat("elapsed_seconds=", format(elapsed, digits = 6L), "\n", sep = "")
