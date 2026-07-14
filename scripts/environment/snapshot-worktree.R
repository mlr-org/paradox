#!/usr/bin/env Rscript

# Copy exactly the Git-visible worktree (tracked plus non-ignored untracked
# files) into an immutable check input.  Hashing before and after the copy
# rejects a worktree that changes while the snapshot is being made.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    "usage: snapshot-worktree.R ROOT DESTINATION METADATA_DIRECTORY",
    call. = FALSE
  )
}

root <- normalizePath(args[[1L]], mustWork = TRUE)
destination <- normalizePath(args[[2L]], mustWork = FALSE)
metadata <- normalizePath(args[[3L]], mustWork = FALSE)

if (!dir.exists(file.path(root, ".git"))) {
  stop("ROOT is not the paradox Git worktree", call. = FALSE)
}
if (dir.exists(destination) && length(list.files(destination, all.files = TRUE,
    no.. = TRUE)) != 0L) {
  stop("snapshot destination is not empty", call. = FALSE)
}
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
dir.create(metadata, recursive = TRUE, showWarnings = FALSE)

old_directory <- setwd(root)
on.exit(setwd(old_directory), add = TRUE)

run_git <- function(arguments, output) {
  error_output <- tempfile("paradox-git-stderr-")
  on.exit(unlink(error_output), add = TRUE)
  status <- system2("git", arguments, stdout = output, stderr = error_output)
  if (!identical(status, 0L)) {
    detail <- if (file.exists(error_output)) {
      paste(readLines(error_output, warn = FALSE), collapse = "\n")
    } else {
      ""
    }
    stop("git command failed: ", detail, call. = FALSE)
  }
  invisible(output)
}

read_raw <- function(path) {
  size <- file.info(path)$size
  if (is.na(size) || size == 0) {
    return(raw())
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  readBin(connection, what = "raw", n = size)
}

split_nul <- function(value) {
  if (length(value) == 0L) {
    return(character())
  }
  terminators <- which(value == as.raw(0L))
  if (length(terminators) == 0L || tail(terminators, 1L) != length(value)) {
    stop("git emitted a malformed NUL-delimited file list", call. = FALSE)
  }
  starts <- c(1L, head(terminators, -1L) + 1L)
  ends <- terminators - 1L
  nonempty <- ends >= starts
  vapply(which(nonempty), function(index) {
    rawToChar(value[starts[[index]]:ends[[index]]])
  }, character(1L), USE.NAMES = FALSE)
}

listing_before <- file.path(metadata, "source-files.zlist")
status_before <- file.path(metadata, "git-status.porcelain-v2.z")
diff_before <- file.path(metadata, "git-diff.patch")
run_git(c("ls-files", "-z", "--cached", "--others", "--exclude-standard"),
  listing_before)
run_git(c("status", "--porcelain=v2", "-z", "--untracked-files=all"),
  status_before)
run_git(c("diff", "--binary", "--no-ext-diff", "HEAD", "--"), diff_before)
run_git(c("rev-parse", "--verify", "HEAD"), file.path(metadata, "git-head.txt"))
run_git(c("status", "--short", "--untracked-files=all"),
  file.path(metadata, "git-status.txt"))

paths <- sort(unique(split_nul(read_raw(listing_before))), method = "radix")
if (any(paths == "" | startsWith(paths, "/") |
    grepl("(^|/)\\.\\.(/|$)", paths))) {
  stop("git file list contains an unsafe path", call. = FALSE)
}
if (any(grepl("^(\\.git|\\.local|\\.cache)(/|$)", paths))) {
  stop("refusing to copy a repository-private or bulky path", call. = FALSE)
}
if (any(grepl("[\t\r\n]", paths))) {
  stop("control characters in paths are unsupported by the text manifest",
    call. = FALSE)
}

source_paths <- file.path(root, paths)
link_targets <- Sys.readlink(source_paths)
is_link <- nzchar(link_targets)
exists <- file.exists(source_paths) | is_link
is_directory <- exists & !is_link & dir.exists(source_paths)
if (any(is_directory)) {
  stop(
    "Git submodules/directories are not supported by this package snapshot: ",
    paste(paths[is_directory], collapse = ", "),
    call. = FALSE
  )
}
is_regular <- exists & !is_link
is_deleted <- !exists

hash_before <- rep(NA_character_, length(paths))
if (any(is_regular)) {
  hash_before[is_regular] <- unname(tools::sha256sum(source_paths[is_regular]))
  if (anyNA(hash_before[is_regular])) {
    stop("could not hash every source file", call. = FALSE)
  }
}

for (index in which(exists)) {
  target <- file.path(destination, paths[[index]])
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  copied <- if (is_link[[index]]) {
    file.symlink(link_targets[[index]], target)
  } else {
    file.copy(source_paths[[index]], target, copy.mode = TRUE, copy.date = TRUE)
  }
  if (!isTRUE(copied)) {
    stop("failed to copy ", paths[[index]], call. = FALSE)
  }
}

listing_after <- tempfile("paradox-files-after-")
status_after <- tempfile("paradox-status-after-")
diff_after <- tempfile("paradox-diff-after-")
on.exit(unlink(c(listing_after, status_after, diff_after)), add = TRUE)
run_git(c("ls-files", "-z", "--cached", "--others", "--exclude-standard"),
  listing_after)
run_git(c("status", "--porcelain=v2", "-z", "--untracked-files=all"),
  status_after)
run_git(c("diff", "--binary", "--no-ext-diff", "HEAD", "--"), diff_after)

if (!identical(read_raw(listing_before), read_raw(listing_after)) ||
    !identical(read_raw(status_before), read_raw(status_after)) ||
    !identical(read_raw(diff_before), read_raw(diff_after))) {
  stop("worktree membership or tracked state changed during snapshot",
    call. = FALSE)
}

if (any(is_regular)) {
  hash_after <- unname(tools::sha256sum(source_paths[is_regular]))
  hash_copy <- unname(tools::sha256sum(
    file.path(destination, paths[is_regular])
  ))
  if (!identical(hash_before[is_regular], hash_after) ||
      !identical(hash_before[is_regular], hash_copy)) {
    stop("a regular file changed while the snapshot was copied", call. = FALSE)
  }
}
if (any(is_link)) {
  copied_links <- Sys.readlink(file.path(destination, paths[is_link]))
  current_links <- Sys.readlink(source_paths[is_link])
  if (!identical(link_targets[is_link], current_links) ||
      !identical(link_targets[is_link], copied_links)) {
    stop("a symbolic link changed while the snapshot was copied", call. = FALSE)
  }
}
if (any(is_deleted) && any(file.exists(source_paths[is_deleted]) |
    nzchar(Sys.readlink(source_paths[is_deleted])))) {
  stop("a deleted tracked path reappeared during snapshot", call. = FALSE)
}

info <- file.info(source_paths)
kind <- ifelse(is_deleted, "deleted", ifelse(is_link, "symlink", "file"))
content <- hash_before
content[is_link] <- paste0("target:", link_targets[is_link])
content[is_deleted] <- "-"
size <- ifelse(is_regular, info$size, NA_real_)
mode <- ifelse(is_regular, sprintf("%04o", as.integer(info$mode)), "-")
manifest <- data.frame(
  path = paths,
  kind = kind,
  sha256_or_target = content,
  size = size,
  mode = mode,
  stringsAsFactors = FALSE
)
manifest_path <- file.path(metadata, "source-manifest.tsv")
write.table(
  manifest,
  file = manifest_path,
  sep = "\t",
  quote = TRUE,
  row.names = FALSE,
  na = "-",
  fileEncoding = "UTF-8"
)

cat("snapshot_manifest_sha256=", unname(tools::sha256sum(manifest_path)),
  "\n", sep = "")
cat("snapshot_files=", sum(exists), "\n", sep = "")
cat("snapshot_deleted_tracked=", sum(is_deleted), "\n", sep = "")
