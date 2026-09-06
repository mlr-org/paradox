#!/usr/bin/env Rscript

# Create or verify a deterministic manifest for a directory tree.  Receipts
# deliberately include directories and modes as well as file contents and
# symbolic-link targets: adding an empty directory or changing an executable
# bit must invalidate the receipt too.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L || !args[[1L]] %in% c("create", "verify")) {
  stop(
    "usage: tree-receipt.R create|verify DIRECTORY MANIFEST",
    call. = FALSE
  )
}

operation <- args[[1L]]
root_argument <- args[[2L]]
if (!dir.exists(root_argument) || nzchar(Sys.readlink(root_argument))) {
  stop("receipt root must be a real, non-symbolic directory", call. = FALSE)
}
root <- normalizePath(root_argument, mustWork = TRUE)
manifest_path <- normalizePath(args[[3L]], mustWork = FALSE)

if (startsWith(
    paste0(manifest_path, .Platform$file.sep),
    paste0(root, .Platform$file.sep)
  )) {
  stop("tree manifest must live outside the received tree", call. = FALSE)
}

collect_members <- function(directory, prefix = "") {
  entries <- list.files(
    directory,
    all.files = TRUE,
    full.names = FALSE,
    recursive = FALSE,
    no.. = TRUE
  )
  if (!length(entries)) {
    return(character())
  }
  entries <- sort(entries, method = "radix")
  members <- character()
  for (entry in entries) {
    relative <- if (nzchar(prefix)) file.path(prefix, entry) else entry
    full_path <- file.path(directory, entry)
    members <- c(members, relative)
    if (!nzchar(Sys.readlink(full_path)) && dir.exists(full_path)) {
      members <- c(members, collect_members(full_path, relative))
    }
  }
  members
}

# Receipts may retain internal and broken-internal links, but they must never
# authenticate a path which silently delegates outside the received tree.
# Resolve targets lexically and follow existing in-tree link chains; this also
# catches a harmless-looking first hop whose second hop escapes.
split_path <- function(path) {
  if (!nzchar(path)) character() else strsplit(path, "/", fixed = TRUE)[[1L]]
}

resolve_link_target <- function(link_path, target) {
  if (startsWith(target, "/") || grepl("^[A-Za-z]:[/\\\\]", target)) {
    stop("tree contains an absolute symbolic-link target", call. = FALSE)
  }

  parent <- dirname(link_path)
  resolved <- if (identical(parent, ".")) character() else split_path(parent)
  pending <- split_path(target)
  expansions <- 0L

  while (length(pending)) {
    component <- pending[[1L]]
    pending <- pending[-1L]
    if (!nzchar(component) || identical(component, ".")) {
      next
    }
    if (identical(component, "..")) {
      if (!length(resolved)) {
        stop("tree contains a symbolic link escaping its root", call. = FALSE)
      }
      resolved <- resolved[-length(resolved)]
      next
    }

    resolved <- c(resolved, component)
    candidate <- file.path(root, paste(resolved, collapse = "/"))
    nested_target <- Sys.readlink(candidate)
    if (is.na(nested_target)) {
      nested_target <- ""
    }
    if (nzchar(nested_target)) {
      expansions <- expansions + 1L
      if (expansions > 256L) {
        stop("tree contains a symbolic-link cycle", call. = FALSE)
      }
      if (startsWith(nested_target, "/") ||
          grepl("^[A-Za-z]:[/\\\\]", nested_target)) {
        stop(
          "tree contains a chained symbolic link escaping its root",
          call. = FALSE
        )
      }
      resolved <- resolved[-length(resolved)]
      pending <- c(split_path(nested_target), pending)
    }
  }

  invisible(TRUE)
}

describe_tree <- function() {
  paths <- collect_members(root)
  if (!length(paths)) {
    return(data.frame(
      path = character(), kind = character(), sha256_or_target = character(),
      size = character(), mode = character(), stringsAsFactors = FALSE
    ))
  }
  if (anyNA(paths) || any(paths == "") || any(grepl("[\t\r\n]", paths))) {
    stop("tree contains a path unsupported by the receipt format", call. = FALSE)
  }
  full_paths <- file.path(root, paths)
  targets <- Sys.readlink(full_paths)
  info <- file.info(full_paths)
  kinds <- ifelse(nzchar(targets), "symlink", ifelse(info$isdir, "directory", "file"))
  non_links <- kinds != "symlink"
  if (anyNA(info$mode[non_links]) || anyNA(kinds)) {
    stop("could not stat every non-symlink tree member", call. = FALSE)
  }
  if (any(kinds == "file" & !file.exists(full_paths))) {
    stop("tree changed while it was being described", call. = FALSE)
  }
  values <- rep("-", length(paths))
  file_rows <- kinds == "file"
  link_rows <- kinds == "symlink"
  if (any(link_rows)) {
    invisible(Map(resolve_link_target, paths[link_rows], targets[link_rows]))
  }
  if (any(file_rows)) {
    values[file_rows] <- unname(tools::sha256sum(full_paths[file_rows]))
  }
  values[link_rows] <- paste0("target:", targets[link_rows])
  if (any(grepl("[\t\r\n]", values))) {
    stop("tree contains a link target unsupported by the receipt format", call. = FALSE)
  }
  sizes <- rep("-", length(paths))
  sizes[file_rows] <- format(info$size[file_rows], scientific = FALSE, trim = TRUE)
  modes <- rep("-", length(paths))
  modes[non_links] <- sprintf("%04o", as.integer(info$mode[non_links]))
  data.frame(
    path = paths,
    kind = kinds,
    sha256_or_target = values,
    size = sizes,
    mode = modes,
    stringsAsFactors = FALSE
  )
}

write_manifest <- function(value, path) {
  parent <- dirname(path)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile("tree-receipt-", tmpdir = parent)
  on.exit(unlink(temporary), add = TRUE)
  write.table(
    value,
    file = temporary,
    sep = "\t",
    quote = TRUE,
    row.names = FALSE,
    col.names = TRUE,
    na = ""
  )
  if (!file.rename(temporary, path)) {
    stop("could not atomically select tree receipt", call. = FALSE)
  }
}

read_manifest <- function(path) {
  if (!file.exists(path) || nzchar(Sys.readlink(path))) {
    stop("tree receipt is missing or symbolic", call. = FALSE)
  }
  value <- read.delim(
    path,
    header = TRUE,
    sep = "\t",
    quote = "\"",
    colClasses = "character",
    check.names = FALSE,
    na.strings = character(),
    stringsAsFactors = FALSE
  )
  expected <- c("path", "kind", "sha256_or_target", "size", "mode")
  if (!identical(names(value), expected)) {
    stop("tree receipt has an unexpected shape", call. = FALSE)
  }
  if (anyNA(value) || anyDuplicated(value$path) ||
      any(value$path == "" | startsWith(value$path, "/") |
        grepl("(^|/)\\.\\.(/|$)", value$path) |
        grepl("[\t\r\n]", value$path)) ||
      any(!value$kind %in% c("file", "directory", "symlink")) ||
      any(!grepl("^[0-7]{4}$", value$mode[value$kind != "symlink"])) ||
      any(value$mode[value$kind == "symlink"] != "-")) {
    stop("tree receipt contains invalid metadata", call. = FALSE)
  }
  files <- value$kind == "file"
  directories <- value$kind == "directory"
  links <- value$kind == "symlink"
  numeric_sizes <- suppressWarnings(as.numeric(value$size[files]))
  if (any(!grepl("^[0-9a-f]{64}$", value$sha256_or_target[files])) ||
      anyNA(numeric_sizes) || any(!is.finite(numeric_sizes) |
        numeric_sizes < 0 | numeric_sizes != floor(numeric_sizes)) ||
      any(value$sha256_or_target[directories] != "-" |
        value$size[directories] != "-") ||
      any(!startsWith(value$sha256_or_target[links], "target:") |
        nchar(value$sha256_or_target[links], type = "bytes") <= 7L |
        value$size[links] != "-" | value$mode[links] != "-")) {
    stop("tree receipt contains invalid kind-specific metadata", call. = FALSE)
  }
  value
}

if (operation == "create") {
  write_manifest(describe_tree(), manifest_path)
  cat("tree_receipt_created=", manifest_path, "\n", sep = "")
  quit(save = "no", status = 0L)
}

expected <- read_manifest(manifest_path)
actual <- describe_tree()
if (!identical(actual, expected)) {
  expected_paths <- expected$path
  actual_paths <- actual$path
  missing <- setdiff(expected_paths, actual_paths)
  added <- setdiff(actual_paths, expected_paths)
  common <- intersect(expected_paths, actual_paths)
  changed <- common[vapply(common, function(path) {
    !identical(
      expected[match(path, expected_paths), , drop = FALSE],
      actual[match(path, actual_paths), , drop = FALSE]
    )
  }, logical(1L))]
  detail <- c(
    if (length(missing)) paste0("missing: ", paste(missing, collapse = ", ")),
    if (length(added)) paste0("added: ", paste(added, collapse = ", ")),
    if (length(changed)) paste0("changed: ", paste(changed, collapse = ", "))
  )
  stop(
    paste(c("tree does not match its receipt", detail), collapse = "; "),
    call. = FALSE
  )
}
cat("tree_receipt_verified=", manifest_path, "\n", sep = "")
cat("tree_members=", nrow(expected), "\n", sep = "")
