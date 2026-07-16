#!/usr/bin/env Rscript

fail <- function(...) stop(..., call. = FALSE)
arguments <- commandArgs(trailingOnly = TRUE)
root <- if (length(arguments)) arguments[[1L]] else getwd()
root <- normalizePath(root, winslash = "/", mustWork = TRUE)
harness <- file.path(root, "compat", "test-documentation")
if (!file.exists(harness) || dir.exists(harness) || nzchar(Sys.readlink(harness))) {
  fail("documentation harness is absent, non-regular, or symbolic")
}
if (!requireNamespace("fs", quietly = TRUE)) {
  fail("repository-local fs package is required")
}
temporary_parent <- file.path(root, ".local", "tmp")
if (!dir.exists(temporary_parent) || nzchar(Sys.readlink(temporary_parent))) {
  fail("repository-local temporary directory is absent or symbolic")
}

tree <- parse(harness, keep.source = FALSE)
binding <- function(name) {
  matches <- Filter(function(expression) {
    is.call(expression) && length(expression) == 3L &&
      identical(expression[[1L]], quote(`<-`)) &&
      identical(expression[[2L]], as.name(name))
  }, as.list(tree))
  if (length(matches) != 1L) {
    fail("expected one direct harness binding for ", name)
  }
  matches[[1L]][[3L]]
}
function_binding <- function(name) {
  value <- binding(name)
  if (!is.call(value) || !identical(value[[1L]], quote(`function`))) {
    fail("harness binding is not a direct function: ", name)
  }
  value
}
call_count <- function(value, head) {
  count <- 0L
  walk <- function(node) {
    if (is.call(node) && identical(node[[1L]], as.name(head))) {
      count <<- count + 1L
    }
    if (is.expression(node) || is.call(node) || is.pairlist(node)) {
      lapply(as.list(node), walk)
    }
    invisible(NULL)
  }
  walk(value)
  count
}
ast_sha256 <- function(value) {
  payload <- serialize(value, NULL, version = 3L, xdr = TRUE)
  temporary <- tempfile(
    "documentation-economy-ast-", tmpdir = temporary_parent
  )
  on.exit(unlink(temporary), add = TRUE)
  connection <- file(temporary, open = "wb")
  writeBin(payload, connection)
  close(connection)
  unname(tools::sha256sum(temporary))
}

run_retained <- function_binding("run_retained")
row_boundary <- function_binding("verify_row_boundary")
full_boundary <- function_binding("verify_full_boundary")
candidate_authenticator <- function_binding("authenticate_candidate_git")
if (!"candidate_source" %in% all.names(
    candidate_authenticator, functions = TRUE, unique = TRUE
  )) {
  fail("documentation candidate authentication omits the detached source")
}
locale_environment <- eval(
  function_binding("documentation_locale_environment")
)()
expected_locale_environment <- c(
  LC_ALL = "C.UTF-8", LANG = "C.UTF-8", LANGUAGE = "C", TZ = "UTC"
)
if (!identical(locale_environment, expected_locale_environment) ||
    !"documentation_locale_environment" %in% all.names(
      function_binding("base_environment"), functions = TRUE, unique = TRUE
    ) || !"documentation_locale_environment" %in% all.names(
      function_binding("retain_environment_evidence"),
      functions = TRUE, unique = TRUE
    )) {
  fail("documentation workload locale is not fixed and receipt-bound")
}
heavy_names <- c(
  "compat_tree_content_sha256", "compat_system_verify_evidence",
  "live_toolchain_explicit", "quarto_bootstrap", "verify_tinytex_tree",
  "retained_tree_helper", "verify_full_boundary"
)
for (entry in list(run_retained = run_retained, row_boundary = row_boundary)) {
  observed <- intersect(
    heavy_names, all.names(entry, functions = TRUE, unique = TRUE)
  )
  if (length(observed)) {
    fail(
      "per-workload path reaches heavyweight verification: ",
      paste(observed, collapse = ", ")
    )
  }
}
if (call_count(run_retained, "verify_row_boundary") != 2L) {
  fail("run_retained must have exactly one pre and one post row boundary")
}
if (call_count(run_retained, "sample_classification_log") != 1L ||
    "readLines" %in% all.names(run_retained, functions = TRUE, unique = TRUE)) {
  fail("run_retained does not use exactly one bounded log sample")
}
if (call_count(tree, "verify_full_boundary") != 1L) {
  fail("the full post-workload verifier must be invoked exactly once")
}
required_full_names <- c(
  "compat_tree_content_sha256", "compat_system_verify_evidence",
  "live_toolchain_explicit", "quarto_bootstrap", "verify_tinytex_tree",
  "retained_tree_helper"
)
missing_full <- setdiff(
  required_full_names, all.names(full_boundary, functions = TRUE, unique = TRUE)
)
if (length(missing_full)) {
  fail("full post-workload boundary lacks: ", paste(missing_full, collapse = ", "))
}
hardening <- file.path(root, "scripts", "environment", "test-validation-hardening")
if (!file.exists(hardening) || dir.exists(hardening) || nzchar(Sys.readlink(hardening))) {
  fail("validation-hardening test is absent, non-regular, or symbolic")
}
hardening_text <- readLines(hardening, warn = FALSE)
expected_hardening_snapshots <- c(
  unname(tools::sha256sum(harness)),
  ast_sha256(run_retained),
  ast_sha256(function_binding("documentation_locale_environment")),
  ast_sha256(function_binding("classify_failure")),
  ast_sha256(function_binding("retain_environment_evidence")),
  ast_sha256(row_boundary),
  ast_sha256(function_binding("sample_classification_log")),
  ast_sha256(candidate_authenticator)
)
for (snapshot in expected_hardening_snapshots) {
  if (sum(grepl(snapshot, hardening_text, fixed = TRUE)) != 1L) {
    fail("validation-hardening lacks one exact documentation snapshot: ", snapshot)
  }
}

helper_environment <- new.env(parent = globalenv())
helper_environment$fail <- fail
helper_environment$is_symbolic <- function(path) {
  link <- Sys.readlink(path)
  length(link) == 1L && !is.na(link) && nzchar(link)
}
for (name in c(
  "documentation_write_tsv", "documentation_format_metadata_number",
  "documentation_metadata_ledger", "documentation_assert_metadata",
  "sample_classification_log", "classify_failure"
)) {
  assign(name, eval(function_binding(name), helper_environment),
    envir = helper_environment)
}
helper_environment$classification_log_head_bytes <- 64L
helper_environment$classification_log_tail_bytes <- 128L
helper_environment$classification_log_max_bytes <- 192L

scratch <- tempfile("documentation-economy-", tmpdir = temporary_parent)
if (!dir.create(scratch, recursive = FALSE, showWarnings = FALSE)) {
  fail("could not create documentation economy fixture")
}
on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)
first <- file.path(scratch, "first")
second <- file.path(scratch, "second")
outside <- file.path(scratch, "outside")
for (path in c(first, second, outside, file.path(first, "directory"))) {
  if (!dir.create(path, recursive = FALSE, showWarnings = FALSE)) {
    fail("could not create metadata fixture directory: ", path)
  }
}
ordinary <- file.path(first, "directory", "ordinary")
hardlink <- file.path(first, "hardlink")
outside_file <- file.path(outside, "must-not-be-followed")
writeLines("fixture", ordinary, useBytes = TRUE)
writeLines("outside", outside_file, useBytes = TRUE)
if (!file.link(ordinary, hardlink)) fail("could not create fixture hard link")
directory_link <- file.path(second, "directory-link")
if (!file.symlink(outside, directory_link)) {
  fail("could not create fixture directory symlink")
}

ledger <- helper_environment$documentation_metadata_ledger(c(
  first = first, second = second
))
expected_columns <- c(
  "root", "root_path", "path", "type", "mode", "size", "mtime", "ctime",
  "device", "inode", "nlink", "link_target"
)
if (!identical(names(ledger), expected_columns)) {
  fail("protected metadata ledger has an unexpected schema")
}
if (!all(c(".", "directory") %in% ledger$path[ledger$root == "first"])) {
  fail("protected metadata ledger omits roots or descendant directories")
}
if (!identical(
    ledger$type[ledger$root == "second" & ledger$path == "directory-link"],
    "symlink"
  ) || any(grepl("must-not-be-followed", ledger$path, fixed = TRUE))) {
  fail("protected metadata traversal followed a directory symlink")
}
ordinary_row <- ledger[ledger$root == "first" &
  ledger$path == "directory/ordinary", , drop = FALSE]
hardlink_row <- ledger[ledger$root == "first" &
  ledger$path == "hardlink", , drop = FALSE]
if (nrow(ordinary_row) != 1L || nrow(hardlink_row) != 1L ||
    !identical(ordinary_row$device, hardlink_row$device) ||
    !identical(ordinary_row$inode, hardlink_row$inode) ||
    as.numeric(ordinary_row$nlink) < 2) {
  fail("protected metadata ledger does not retain hard-link identity")
}
helper_environment$documentation_assert_metadata(
  helper_environment$documentation_metadata_ledger(c(
    first = first, second = second
  )),
  ledger,
  "in unchanged fixture"
)

old_mode <- ordinary_row$mode[[1L]]
new_mode <- if (endsWith(old_mode, "600")) "0644" else "0600"
if (!isTRUE(Sys.chmod(ordinary, new_mode))) {
  fail("could not mutate fixture mode")
}
mutated <- helper_environment$documentation_metadata_ledger(c(
  first = first, second = second
))
diagnostic <- file.path(scratch, "metadata-mismatch.tsv")
condition <- tryCatch(
  {
    helper_environment$documentation_assert_metadata(
      mutated, ledger, "after fixture tampering", diagnostic
    )
    NULL
  },
  error = identity
)
if (!inherits(condition, "error") || !file.exists(diagnostic) ||
    identical(mutated, ledger)) {
  fail("metadata tampering was not rejected with retained diagnostics")
}

oversized_log <- file.path(scratch, "oversized.log")
payload <- c(
  as.raw(0L), charToRaw(paste(rep("h", 79L), collapse = "")),
  charToRaw(paste(rep("m", 240L), collapse = "")),
  charToRaw("there is no package called 'fixtureDependency'")
)
connection <- file(oversized_log, open = "wb")
writeBin(payload, connection)
close(connection)
sample <- helper_environment$sample_classification_log(oversized_log)
if (!identical(sample$log_bytes, as.numeric(length(payload))) ||
    !identical(sample$sample_bytes, 192L) ||
    !identical(sample$head_bytes, 64L) ||
    !identical(sample$tail_bytes, 128L) || !isTRUE(sample$truncated) ||
    !identical(sample$nul_bytes, 1L) ||
    !identical(sample$pathological, "nul_bytes_sanitized") ||
    !identical(
      helper_environment$classify_failure(sample$text),
      "missing_r_dependency"
    ) || !identical(
      as.numeric(fs::file_info(oversized_log, follow = FALSE)$size),
      as.numeric(length(payload))
    )) {
  fail("bounded oversized-log classification fixture failed")
}

cat("documentation_economy_test=passed\n")
