repository_evidence_manifest_relative <- file.path(
  "metadata", "evidence-manifest.tsv"
)
repository_evidence_seal_relative <- file.path(
  "metadata", "completion.seal"
)

repository_evidence_is_symbolic <- function(path) {
  link <- Sys.readlink(path)
  length(link) == 1L && !is.na(link) && nzchar(link)
}

repository_evidence_require_plain_directory <- function(path, label) {
  if (!dir.exists(path) || repository_evidence_is_symbolic(path)) {
    stop(label, " is missing, not a directory, or symbolic: ", path, call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

repository_evidence_require_regular_file <- function(path, label) {
  if (!file.exists(path) || dir.exists(path) ||
      repository_evidence_is_symbolic(path)) {
    stop(label, " is missing, not a regular file, or symbolic: ", path, call. = FALSE)
  }
  invisible(path)
}

repository_evidence_write_tsv <- function(value, path) {
  temporary <- paste0(path, ".new")
  if (file.exists(path) || dir.exists(path) ||
      repository_evidence_is_symbolic(path) || file.exists(temporary) ||
      dir.exists(temporary) || repository_evidence_is_symbolic(temporary)) {
    stop("evidence output path already exists: ", path, call. = FALSE)
  }
  on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)
  utils::write.table(
    value,
    temporary,
    quote = FALSE,
    sep = "\t",
    row.names = FALSE,
    na = "-",
    fileEncoding = "UTF-8"
  )
  if (!file.rename(temporary, path)) {
    stop("could not atomically write evidence: ", path, call. = FALSE)
  }
  invisible(path)
}

repository_evidence_write_lines <- function(value, path) {
  temporary <- paste0(path, ".new")
  if (file.exists(path) || dir.exists(path) ||
      repository_evidence_is_symbolic(path) || file.exists(temporary) ||
      dir.exists(temporary) || repository_evidence_is_symbolic(temporary)) {
    stop("evidence output path already exists: ", path, call. = FALSE)
  }
  on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(value, temporary, useBytes = TRUE)
  if (!file.rename(temporary, path)) {
    stop("could not atomically write evidence: ", path, call. = FALSE)
  }
  invisible(path)
}

repository_evidence_inventory <- function(stage_directory) {
  stage_directory <- repository_evidence_require_plain_directory(
    stage_directory,
    "evidence stage"
  )
  entries <- list.files(
    stage_directory,
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )
  if (length(entries)) {
    symbolic <- vapply(entries, repository_evidence_is_symbolic, logical(1L))
    if (any(symbolic)) {
      stop(
        "evidence stage contains a symbolic path: ",
        entries[[which(symbolic)[[1L]]]],
        call. = FALSE
      )
    }
  }
  info <- file.info(entries, extra_cols = FALSE)
  if (nrow(info) && anyNA(info$isdir)) {
    stop("evidence stage contains an unreadable path", call. = FALSE)
  }
  files <- entries[!info$isdir]
  if (!length(files)) {
    return(data.frame(
      path = character(), size = character(), sha256 = character(),
      stringsAsFactors = FALSE
    ))
  }
  relative <- substring(files, nchar(stage_directory, type = "chars") + 2L)
  excluded <- relative %in% c(
    repository_evidence_manifest_relative,
    repository_evidence_seal_relative
  )
  files <- files[!excluded]
  relative <- relative[!excluded]
  ordering <- order(relative, method = "radix")
  files <- files[ordering]
  relative <- relative[ordering]
  sizes <- file.info(files, extra_cols = FALSE)$size
  data.frame(
    path = relative,
    size = format(sizes, scientific = FALSE, trim = TRUE),
    sha256 = unname(tools::sha256sum(files)),
    stringsAsFactors = FALSE
  )
}

repository_seal_evidence <- function(stage_directory) {
  stage_directory <- repository_evidence_require_plain_directory(
    stage_directory,
    "evidence stage"
  )
  metadata_directory <- repository_evidence_require_plain_directory(
    file.path(stage_directory, "metadata"),
    "evidence metadata directory"
  )
  if (!identical(dirname(metadata_directory), stage_directory)) {
    stop("evidence metadata directory escaped its stage", call. = FALSE)
  }
  manifest_path <- file.path(stage_directory, repository_evidence_manifest_relative)
  seal_path <- file.path(stage_directory, repository_evidence_seal_relative)
  inventory <- repository_evidence_inventory(stage_directory)
  if (!nrow(inventory)) {
    stop("cannot seal an empty evidence stage", call. = FALSE)
  }
  repository_evidence_write_tsv(inventory, manifest_path)
  manifest_sha256 <- unname(tools::sha256sum(manifest_path))
  repository_evidence_write_lines(
    paste0("evidence_manifest_sha256=", manifest_sha256),
    seal_path
  )
  repository_verify_evidence(stage_directory)
}

repository_verify_evidence <- function(stage_directory) {
  stage_directory <- repository_evidence_require_plain_directory(
    stage_directory,
    "evidence stage"
  )
  manifest_path <- file.path(stage_directory, repository_evidence_manifest_relative)
  seal_path <- file.path(stage_directory, repository_evidence_seal_relative)
  repository_evidence_require_regular_file(manifest_path, "evidence manifest")
  repository_evidence_require_regular_file(seal_path, "evidence completion seal")

  seal <- readLines(seal_path, warn = FALSE)
  expected_seal <- paste0(
    "evidence_manifest_sha256=",
    unname(tools::sha256sum(manifest_path))
  )
  if (!identical(seal, expected_seal)) {
    stop("evidence completion seal is malformed or does not match", call. = FALSE)
  }

  manifest <- utils::read.delim(
    manifest_path,
    header = TRUE,
    sep = "\t",
    quote = "",
    comment.char = "",
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (!identical(names(manifest), c("path", "size", "sha256")) ||
      !nrow(manifest) || anyNA(manifest) || any(!nzchar(manifest$path)) ||
      anyDuplicated(manifest$path) ||
      !identical(manifest$path, sort(manifest$path, method = "radix")) ||
      any(!grepl("^(0|[1-9][0-9]*)$", manifest$size)) ||
      any(!grepl("^[0-9a-f]{64}$", manifest$sha256))) {
    stop("evidence manifest has an invalid schema or contents", call. = FALSE)
  }
  unsafe <- startsWith(manifest$path, "/") |
    grepl("(^|/)\\.\\.(/|$)", manifest$path) |
    grepl("\r", manifest$path, fixed = TRUE) |
    grepl("\n", manifest$path, fixed = TRUE) |
    grepl("\t", manifest$path, fixed = TRUE)
  if (any(unsafe)) {
    stop("evidence manifest contains an unsafe relative path", call. = FALSE)
  }

  observed <- repository_evidence_inventory(stage_directory)
  if (!identical(manifest, observed)) {
    stop("evidence stage contents do not match the sealed manifest", call. = FALSE)
  }
  list(
    stage = stage_directory,
    manifest = manifest_path,
    seal = seal_path,
    manifest_sha256 = unname(tools::sha256sum(manifest_path)),
    seal_sha256 = unname(tools::sha256sum(seal_path)),
    files = nrow(manifest)
  )
}
