#!/usr/bin/env Rscript

# Reproduce a previously retained native-check source tree without consulting
# the changing worktree.  The retained manifest is verified both before and
# after the copy, and copied unchanged into the new run's provenance metadata.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) {
  stop(
    paste(
      "usage: copy-check-snapshot.R ORIGIN_SOURCE ORIGIN_METADATA",
      "DESTINATION METADATA_DIRECTORY"
    ),
    call. = FALSE
  )
}

origin <- normalizePath(args[[1L]], mustWork = TRUE)
origin_metadata <- normalizePath(args[[2L]], mustWork = TRUE)
destination <- normalizePath(args[[3L]], mustWork = FALSE)
metadata <- normalizePath(args[[4L]], mustWork = FALSE)

manifest_path <- file.path(origin_metadata, "source-manifest.tsv")
if (!file.exists(manifest_path)) {
  stop("origin metadata has no source manifest", call. = FALSE)
}
if (dir.exists(destination) && length(list.files(
    destination, all.files = TRUE, no.. = TRUE
  )) != 0L) {
  stop("snapshot destination is not empty", call. = FALSE)
}
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
dir.create(metadata, recursive = TRUE, showWarnings = FALSE)

manifest_hash_before <- unname(tools::sha256sum(manifest_path))
manifest <- read.delim(
  manifest_path,
  header = TRUE,
  sep = "\t",
  quote = "\"",
  colClasses = "character",
  check.names = FALSE,
  na.strings = character(),
  stringsAsFactors = FALSE
)
expected_columns <- c("path", "kind", "sha256_or_target", "size", "mode")
if (!identical(names(manifest), expected_columns) || nrow(manifest) == 0L) {
  stop("origin source manifest has an unexpected shape", call. = FALSE)
}

paths <- manifest$path
if (anyNA(paths) || any(paths == "" | startsWith(paths, "/") |
    grepl("(^|/)\\.\\.(/|$)", paths)) || anyDuplicated(paths) != 0L) {
  stop("origin source manifest contains an unsafe path", call. = FALSE)
}
if (any(grepl("^(\\.git|\\.local|\\.cache)(/|$)", paths)) ||
    any(grepl("[\t\r\n]", paths))) {
  stop("origin source manifest contains a private or unsupported path",
    call. = FALSE)
}
if (any(!manifest$kind %in% c("file", "symlink", "deleted"))) {
  stop("origin source manifest contains an unknown entry kind", call. = FALSE)
}
file_rows <- manifest$kind == "file"
link_rows <- manifest$kind == "symlink"
deleted_rows <- manifest$kind == "deleted"
file_sizes <- suppressWarnings(as.numeric(manifest$size[file_rows]))
if (any(!grepl("^[[:xdigit:]]{64}$", manifest$sha256_or_target[file_rows])) ||
    anyNA(file_sizes) || any(!is.finite(file_sizes) | file_sizes < 0 |
      file_sizes != floor(file_sizes)) ||
    any(!grepl("^[0-7]{4}$", manifest$mode[file_rows])) ||
    any(!startsWith(manifest$sha256_or_target[link_rows], "target:")) ||
    any(nchar(manifest$sha256_or_target[link_rows], type = "bytes") <= 7L) ||
    any(manifest$size[link_rows] != "-" | manifest$mode[link_rows] != "-") ||
    any(manifest$sha256_or_target[deleted_rows] != "-" |
      manifest$size[deleted_rows] != "-" | manifest$mode[deleted_rows] != "-")) {
  stop("origin source manifest contains invalid entry metadata", call. = FALSE)
}

collect_members <- function(directory, prefix = "") {
  entries <- list.files(
    directory,
    all.files = TRUE,
    full.names = FALSE,
    recursive = FALSE,
    no.. = TRUE
  )
  members <- character()
  for (entry in entries) {
    relative <- if (nzchar(prefix)) file.path(prefix, entry) else entry
    full_path <- file.path(directory, entry)
    if (nzchar(Sys.readlink(full_path)) || !dir.exists(full_path)) {
      members <- c(members, relative)
    } else {
      members <- c(members, collect_members(full_path, relative))
    }
  }
  members
}

verify_tree <- function(base, phase) {
  full_paths <- file.path(base, paths)
  links <- Sys.readlink(full_paths)

  members <- collect_members(base)
  expected_members <- paths[!deleted_rows]
  if (!identical(
      sort(members, method = "radix"),
      sort(expected_members, method = "radix")
    )) {
    stop("source tree membership mismatch during ", phase, call. = FALSE)
  }

  bad_files <- file_rows & (
    !file.exists(full_paths) | dir.exists(full_paths) | nzchar(links)
  )
  expected_links <- sub(
    "^target:", "", manifest$sha256_or_target[link_rows]
  )
  bad_links <- rep(FALSE, length(paths))
  bad_links[link_rows] <- links[link_rows] != expected_links
  bad_deleted <- deleted_rows & (
    file.exists(full_paths) | dir.exists(full_paths) | nzchar(links)
  )
  if (any(bad_files | bad_links | bad_deleted)) {
    stop("source tree does not match its manifest during ", phase,
      call. = FALSE)
  }

  if (any(file_rows)) {
    hashes <- unname(tools::sha256sum(full_paths[file_rows]))
    if (anyNA(hashes) ||
        !identical(hashes, manifest$sha256_or_target[file_rows])) {
      stop("source file hash mismatch during ", phase, call. = FALSE)
    }
    info <- file.info(full_paths[file_rows])
    expected_sizes <- as.numeric(manifest$size[file_rows])
    actual_modes <- sprintf("%04o", as.integer(info$mode))
    if (anyNA(expected_sizes) || anyNA(info$size) ||
        !identical(as.numeric(info$size), expected_sizes) ||
        !identical(actual_modes, manifest$mode[file_rows])) {
      stop("source file metadata mismatch during ", phase, call. = FALSE)
    }
  }
  invisible(TRUE)
}

verify_tree(origin, "pre-copy verification")

for (index in seq_along(paths)) {
  if (manifest$kind[[index]] == "deleted") {
    next
  }
  source <- file.path(origin, paths[[index]])
  target <- file.path(destination, paths[[index]])
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  copied <- if (manifest$kind[[index]] == "symlink") {
    file.symlink(Sys.readlink(source), target)
  } else {
    file.copy(source, target, copy.mode = TRUE, copy.date = TRUE)
  }
  if (!isTRUE(copied)) {
    stop("failed to copy ", paths[[index]], call. = FALSE)
  }
}

verify_tree(origin, "post-copy origin verification")
verify_tree(destination, "post-copy destination verification")
manifest_hash_after <- unname(tools::sha256sum(manifest_path))
if (!identical(manifest_hash_before, manifest_hash_after)) {
  stop("origin source manifest changed during replay", call. = FALSE)
}

provenance_files <- c(
  "git-diff.patch",
  "git-head.txt",
  "git-status.porcelain-v2.z",
  "git-status.txt",
  "source-files.zlist",
  "source-manifest.tsv"
)
for (name in provenance_files) {
  source <- file.path(origin_metadata, name)
  target <- file.path(metadata, name)
  if (!file.exists(source) || file.exists(target) ||
      !isTRUE(file.copy(source, target, copy.mode = TRUE, copy.date = TRUE)) ||
      !identical(
        unname(tools::sha256sum(source)),
        unname(tools::sha256sum(target))
      )) {
    stop("failed to retain origin provenance file: ", name, call. = FALSE)
  }
  if (name == "source-manifest.tsv" && !identical(
      unname(tools::sha256sum(target)), manifest_hash_before
    )) {
    stop("copied source manifest does not match the verified manifest",
      call. = FALSE)
  }
}
if (!identical(
    unname(tools::sha256sum(manifest_path)), manifest_hash_before
  )) {
  stop("origin source manifest changed while retaining provenance",
    call. = FALSE)
}

origin_run <- basename(dirname(origin))
writeLines(c(
  paste0("origin_run=", origin_run),
  paste0("origin_source=", origin),
  paste0("origin_manifest_sha256=", manifest_hash_before)
), file.path(metadata, "source-origin.txt"), useBytes = TRUE)

cat("snapshot_manifest_sha256=", manifest_hash_before, "\n", sep = "")
cat("snapshot_files=", sum(manifest$kind != "deleted"), "\n", sep = "")
cat("snapshot_deleted_tracked=", sum(manifest$kind == "deleted"), "\n",
  sep = "")
cat("snapshot_origin_run=", origin_run, "\n", sep = "")
