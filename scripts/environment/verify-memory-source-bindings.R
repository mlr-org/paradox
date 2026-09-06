#!/usr/bin/env Rscript

# Prove that the Git-visible source manifest copied from the native run and the
# generic tree receipt sealed by memory-check describe the same source bytes.
# The caller separately verifies the tree receipt against the retained tree.

fail <- function(...) stop(..., call. = FALSE)

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 2L) {
  fail(
    paste(
      "usage: verify-memory-source-bindings.R",
      "SOURCE_MANIFEST MEMORY_SOURCE_TREE_RECEIPT"
    )
  )
}

regular_input <- function(path, label) {
  if (!file.exists(path) || dir.exists(path) || nzchar(Sys.readlink(path))) {
    fail(label, " is absent, non-regular, or symbolic")
  }
  normalizePath(path, mustWork = TRUE)
}

manifest_path <- regular_input(arguments[[1L]], "source manifest")
tree_path <- regular_input(arguments[[2L]], "memory source-tree receipt")

read_character_table <- function(path) {
  read.delim(
    path,
    header = TRUE,
    sep = "\t",
    quote = "\"",
    colClasses = "character",
    check.names = FALSE,
    na.strings = character(),
    stringsAsFactors = FALSE
  )
}

manifest <- read_character_table(manifest_path)
tree <- read_character_table(tree_path)
columns <- c("path", "kind", "sha256_or_target", "size", "mode")
if (!identical(names(manifest), columns) || !nrow(manifest)) {
  fail("source manifest has an unexpected schema or is empty")
}
if (!identical(names(tree), columns) || !nrow(tree)) {
  fail("memory source-tree receipt has an unexpected schema or is empty")
}

safe_paths <- function(paths, label) {
  if (anyNA(paths) || any(paths == "") || anyDuplicated(paths) ||
      any(startsWith(paths, "/")) ||
      any(grepl("(^|/)\\.\\.(/|$)", paths)) ||
      any(grepl("[\t\r\n]", paths))) {
    fail(label, " contains an unsafe or duplicate path")
  }
}
safe_paths(manifest$path, "source manifest")
safe_paths(tree$path, "memory source-tree receipt")
if (any(grepl("^(\\.git|\\.local|\\.cache)(/|$)", manifest$path))) {
  fail("source manifest contains a private path")
}

if (anyNA(manifest) ||
    any(!manifest$kind %in% c("file", "symlink", "deleted"))) {
  fail("source manifest contains an unknown kind or missing value")
}
manifest_files <- manifest$kind == "file"
manifest_links <- manifest$kind == "symlink"
manifest_deleted <- manifest$kind == "deleted"
manifest_sizes <- suppressWarnings(as.numeric(manifest$size[manifest_files]))
if (any(!grepl("^[0-9a-f]{64}$", manifest$sha256_or_target[manifest_files])) ||
    anyNA(manifest_sizes) ||
    any(!is.finite(manifest_sizes) | manifest_sizes < 0 |
      manifest_sizes != floor(manifest_sizes)) ||
    any(!grepl("^[0-7]{4}$", manifest$mode[manifest_files])) ||
    any(!startsWith(
      manifest$sha256_or_target[manifest_links], "target:"
    )) ||
    any(nchar(
      manifest$sha256_or_target[manifest_links], type = "bytes"
    ) <= 7L) ||
    any(manifest$size[manifest_links] != "-" |
      manifest$mode[manifest_links] != "-") ||
    any(manifest$sha256_or_target[manifest_deleted] != "-" |
      manifest$size[manifest_deleted] != "-" |
      manifest$mode[manifest_deleted] != "-")) {
  fail("source manifest contains invalid kind-specific metadata")
}

if (anyNA(tree) ||
    any(!tree$kind %in% c("file", "directory", "symlink"))) {
  fail("memory source-tree receipt contains an unknown kind or missing value")
}
tree_files <- tree$kind == "file"
tree_directories <- tree$kind == "directory"
tree_links <- tree$kind == "symlink"
tree_sizes <- suppressWarnings(as.numeric(tree$size[tree_files]))
if (any(!grepl("^[0-9a-f]{64}$", tree$sha256_or_target[tree_files])) ||
    anyNA(tree_sizes) ||
    any(!is.finite(tree_sizes) | tree_sizes < 0 |
      tree_sizes != floor(tree_sizes)) ||
    any(!grepl("^[0-7]{4}$", tree$mode[tree_files])) ||
    any(tree$sha256_or_target[tree_directories] != "-" |
      tree$size[tree_directories] != "-" |
      !grepl("^[0-7]{4}$", tree$mode[tree_directories])) ||
    any(!startsWith(tree$sha256_or_target[tree_links], "target:") |
      nchar(tree$sha256_or_target[tree_links], type = "bytes") <= 7L |
      tree$size[tree_links] != "-" | tree$mode[tree_links] != "-")) {
  fail("memory source-tree receipt contains invalid kind-specific metadata")
}

retained <- manifest[!manifest_deleted, , drop = FALSE]
tree_leaves <- tree[!tree_directories, , drop = FALSE]
if (!identical(
    sort(retained$path, method = "radix"),
    sort(tree_leaves$path, method = "radix")
  )) {
  fail("source manifest and memory source-tree receipt leaf inventories differ")
}
tree_leaves <- tree_leaves[match(retained$path, tree_leaves$path), , drop = FALSE]
rownames(retained) <- NULL
rownames(tree_leaves) <- NULL
if (!identical(retained, tree_leaves)) {
  fail("source manifest and memory source-tree receipt leaf metadata differ")
}
if (any(manifest$path[manifest_deleted] %in% tree$path)) {
  fail("a source-manifest deletion is present in the memory source tree")
}

ancestor_paths <- function(paths) {
  unique(unlist(lapply(paths, function(path) {
    components <- strsplit(path, "/", fixed = TRUE)[[1L]]
    if (length(components) < 2L) return(character())
    vapply(
      seq_len(length(components) - 1L),
      function(index) paste(components[seq_len(index)], collapse = "/"),
      character(1L)
    )
  }), use.names = FALSE))
}
expected_directories <- sort(ancestor_paths(retained$path), method = "radix")
observed_directories <- sort(tree$path[tree_directories], method = "radix")
# testthat creates this directory while discovering snapshot tests, even when
# no snapshot is written.  It has no leaf in the tree receipt, and therefore
# cannot carry source content.  Keep the exception exact: any file, symlink,
# nested directory, or different empty directory still changes the inventories
# above or the unexplained-directory set below.
allowed_empty_directories <- "tests/testthat/_snaps"
missing_directories <- setdiff(expected_directories, observed_directories)
unexplained_directories <- setdiff(observed_directories, expected_directories)
if (length(missing_directories) ||
    any(!unexplained_directories %in% allowed_empty_directories)) {
  fail("memory source tree contains an empty or unexplained directory")
}

cat("memory source manifest/tree binding verified\n")
cat("source_leaves=", nrow(retained), "\n", sep = "")
cat("source_deleted=", sum(manifest_deleted), "\n", sep = "")
