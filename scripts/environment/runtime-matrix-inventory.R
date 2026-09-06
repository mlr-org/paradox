#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || !args[[1L]] %in% c("explicit", "packages")) {
  stop(
    "usage: runtime-matrix-inventory.R explicit|packages PREFIX",
    call. = FALSE
  )
}

operation <- args[[1L]]
prefix_argument <- args[[2L]]
if (!dir.exists(prefix_argument) || nzchar(Sys.readlink(prefix_argument))) {
  stop("runtime prefix must be a real, non-symbolic directory", call. = FALSE)
}
prefix <- normalizePath(prefix_argument, mustWork = TRUE)
metadata_directory <- file.path(prefix, "conda-meta")
metadata <- sort(list.files(
  metadata_directory,
  pattern = "[.]json$",
  full.names = TRUE
))
if (!length(metadata) || !requireNamespace("jsonlite", quietly = TRUE)) {
  stop("runtime conda metadata or its locked jsonlite package is absent",
    call. = FALSE)
}

records <- lapply(metadata, function(path) {
  value <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  required <- c("name", "version", "build", "subdir", "url", "sha256")
  if (!all(required %in% names(value)) ||
      any(!vapply(value[required], function(item) {
        is.character(item) && length(item) == 1L && !is.na(item) && nzchar(item)
      }, logical(1L))) ||
      !grepl("^[0-9a-f]{64}$", value$sha256) ||
      !grepl(
        "^https://conda[.]anaconda[.]org/conda-forge/(linux-64|noarch)/",
        value$url
      )) {
    stop("invalid or incomplete conda metadata: ", path, call. = FALSE)
  }
  value
})

if (operation == "explicit") {
  rows <- sort(vapply(records, function(value) {
    paste0(value$url, "#", value$sha256)
  }, character(1L)), method = "radix")
  if (anyDuplicated(rows)) {
    stop("duplicate installed explicit package identity", call. = FALSE)
  }
  writeLines(rows)
  quit(save = "no", status = 0L)
}

rows <- do.call(rbind, lapply(records, function(value) {
  data.frame(
    package = value$name,
    version = value$version,
    build = value$build,
    subdir = value$subdir,
    url = value$url,
    sha256 = value$sha256,
    stringsAsFactors = FALSE
  )
}))
rows <- rows[order(rows$package, rows$version, rows$build, method = "radix"), ,
  drop = FALSE]
write.table(
  rows,
  file = stdout(),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE
)
