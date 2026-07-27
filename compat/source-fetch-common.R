compat_fetch_stop <- function(...) {
  stop(..., call. = FALSE)
}

compat_fetch_is_symbolic <- function(path) {
  target <- Sys.readlink(path)
  !is.na(target) && nzchar(target)
}

compat_fetch_is_regular <- function(path) {
  # Base R's file_test("-f", ...) follows links and, on this platform, also
  # reports FIFOs as regular files.  POSIX test gives the file-type predicate
  # needed before any hash or connection can block on a special file.
  status <- suppressWarnings(system2(
    "/usr/bin/test",
    c("-f", shQuote(path)),
    stdout = FALSE,
    stderr = FALSE
  ))
  identical(as.integer(status), 0L)
}

compat_fetch_require_regular <- function(path, label) {
  info <- file.info(path)
  if (compat_fetch_is_symbolic(path) || !compat_fetch_is_regular(path) ||
      is.na(info$isdir) || info$isdir) {
    compat_fetch_stop(label, " is missing, non-regular, or symbolic: ", path)
  }
  invisible(path)
}

compat_fetch_file_hashes <- function(paths, label) {
  for (path in paths) compat_fetch_require_regular(path, label)
  first <- unname(tools::sha256sum(paths))
  for (path in paths) compat_fetch_require_regular(path, label)
  second <- unname(tools::sha256sum(paths))
  if (anyNA(first) || !identical(first, second)) {
    compat_fetch_stop(label, " changed while it was authenticated")
  }
  second
}

compat_fetch_require_plain_chain <- function(root, path, label) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  path <- sub("/+$", "", path)
  if (!identical(path, root) && !startsWith(path, paste0(root, "/"))) {
    compat_fetch_stop(label, " escaped the repository: ", path)
  }

  relative <- substring(path, nchar(root) + 2L)
  current <- root
  if (nzchar(relative)) {
    for (component in strsplit(relative, "/", fixed = TRUE)[[1L]]) {
      if (!nzchar(component) || component %in% c(".", "..")) {
        compat_fetch_stop(label, " has an unsafe path component: ", path)
      }
      current <- file.path(current, component)
      if (compat_fetch_is_symbolic(current)) {
        compat_fetch_stop(label, " follows a symbolic path: ", current)
      }
      if (file.exists(current) || dir.exists(current)) {
        info <- file.info(current)
        if (is.na(info$isdir) || !info$isdir) {
          compat_fetch_stop(label, " contains a non-directory component: ", current)
        }
      }
    }
  }
  invisible(path)
}

compat_fetch_make_directory <- function(root, path, label) {
  compat_fetch_require_plain_chain(root, path, label)
  if (!dir.exists(path)) {
    parent <- dirname(path)
    if (!dir.exists(parent) || !dir.create(path, recursive = FALSE, mode = "0700")) {
      compat_fetch_stop("could not create ", label, ": ", path)
    }
  }
  resolved <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!identical(resolved, path)) {
    compat_fetch_stop(label, " resolved outside its literal managed path: ", path)
  }
  invisible(path)
}

compat_fetch_read_tsv <- function(path, expected_names, label) {
  compat_fetch_require_regular(path, label)
  value <- utils::read.delim(
    path,
    header = TRUE,
    sep = "\t",
    quote = "",
    comment.char = "",
    colClasses = "character",
    check.names = FALSE,
    na.strings = character(),
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )
  if (!identical(names(value), expected_names) || !nrow(value) ||
      anyNA(value) || any(vapply(value, function(column) {
        any(grepl("[\t\r\n]", column))
      }, logical(1L)))) {
    compat_fetch_stop(label, " has an unexpected or invalid shape: ", path)
  }
  value
}

compat_fetch_tar_octal <- function(field, archive, label) {
  bytes <- as.integer(field)
  while (length(bytes) && bytes[[1L]] %in% c(0L, 32L)) bytes <- bytes[-1L]
  while (length(bytes) && bytes[[length(bytes)]] %in% c(0L, 32L)) {
    bytes <- bytes[-length(bytes)]
  }
  if (!length(bytes)) return(0)
  if (any(bytes < 48L | bytes > 55L)) {
    compat_fetch_stop("source archive has an unsupported ", label, ": ", archive)
  }
  value <- 0
  for (byte in bytes) {
    value <- value * 8 + byte - 48L
    if (!is.finite(value) || value > 2^53 - 1) {
      compat_fetch_stop("source archive has an excessive ", label, ": ", archive)
    }
  }
  value
}

compat_fetch_tar_skip <- function(connection, count, archive) {
  remaining <- count
  while (remaining > 0) {
    requested <- as.integer(min(1024^2, remaining))
    bytes <- readBin(connection, what = "raw", n = requested)
    if (length(bytes) != requested) {
      compat_fetch_stop("source archive has a truncated tar payload: ", archive)
    }
    remaining <- remaining - requested
  }
  invisible(TRUE)
}

compat_fetch_tar_header_count <- function(archive) {
  connection <- gzfile(archive, open = "rb")
  on.exit(close(connection), add = TRUE)
  headers <- 0L
  zero_blocks <- 0L
  ended <- FALSE

  repeat {
    header <- readBin(connection, what = "raw", n = 512L)
    if (!length(header)) break
    if (length(header) != 512L) {
      compat_fetch_stop("source archive has a truncated tar header: ", archive)
    }
    if (all(header == as.raw(0L))) {
      zero_blocks <- zero_blocks + 1L
      ended <- TRUE
      next
    }
    if (ended) {
      compat_fetch_stop("source archive has data after its tar end marker: ", archive)
    }

    declared_checksum <- compat_fetch_tar_octal(
      header[149L:156L], archive, "tar checksum"
    )
    checksum_header <- header
    checksum_header[149L:156L] <- as.raw(32L)
    unsigned_checksum <- sum(as.integer(checksum_header))
    signed_bytes <- as.integer(checksum_header)
    signed_bytes[signed_bytes > 127L] <- signed_bytes[signed_bytes > 127L] - 256L
    signed_checksum <- sum(signed_bytes)
    if (!(declared_checksum %in% c(unsigned_checksum, signed_checksum))) {
      compat_fetch_stop("source archive has an invalid tar header checksum: ", archive)
    }

    size <- compat_fetch_tar_octal(header[125L:136L], archive, "tar member size")
    type <- as.integer(header[[157L]])
    if (!(type %in% c(0L, 48L, 53L))) {
      compat_fetch_stop(
        "source archive contains a link, special file, or extended tar member: ",
        archive
      )
    }
    if (identical(type, 53L) && size != 0) {
      compat_fetch_stop("source archive contains a directory with a payload: ", archive)
    }
    headers <- headers + 1L
    if (headers > 1000000L) {
      compat_fetch_stop("source archive contains too many tar members: ", archive)
    }
    padded_size <- if (size == 0) 0 else (floor((size - 1) / 512) + 1) * 512
    compat_fetch_tar_skip(connection, padded_size, archive)
  }

  if (zero_blocks < 2L) {
    compat_fetch_stop("source archive lacks a complete tar end marker: ", archive)
  }
  headers
}

compat_fetch_archive_metadata <- function(archive, package, version) {
  compat_fetch_require_regular(archive, "source archive")
  header_count <- compat_fetch_tar_header_count(archive)
  members <- utils::untar(archive, list = TRUE, tar = "internal")
  if (!length(members) || length(members) != header_count ||
      anyNA(members) || any(!nzchar(members)) || anyDuplicated(members) ||
      any(startsWith(members, "/")) ||
      any(grepl("//|\\\\", members)) ||
      any(grepl("(^|/)\\.(/|$)", members)) ||
      any(grepl("(^|/)\\.\\.(/|$)", members)) ||
      any(grepl("[\t\r\n]", members))) {
    compat_fetch_stop("source archive has unsafe or malformed members: ", archive)
  }
  description_member <- paste0(package, "/DESCRIPTION")
  if (sum(members == description_member) != 1L ||
      any(!(members == package | startsWith(members, paste0(package, "/"))))) {
    compat_fetch_stop("source archive has an unexpected package tree: ", archive)
  }

  # Archive authentication is read-only with respect to the pinned source
  # cache.  Extract the single DESCRIPTION member below R's process-private
  # temporary root so a read-only cache can be verified inside a worker.
  inspection <- compat_fetch_reserve_directory(
    normalizePath(tempdir(), winslash = "/", mustWork = TRUE),
    "compat-archive-inspect-"
  )
  on.exit(unlink(inspection, recursive = TRUE, force = FALSE), add = TRUE)
  utils::untar(
    archive,
    files = description_member,
    exdir = inspection,
    tar = "internal"
  )
  description <- file.path(inspection, description_member)
  compat_fetch_require_regular(description, "extracted DESCRIPTION")
  fields <- read.dcf(description, fields = c("Package", "Version"))
  if (nrow(fields) != 1L ||
      !identical(unname(fields[1L, "Package"]), package) ||
      !identical(unname(fields[1L, "Version"]), version)) {
    compat_fetch_stop("source archive DESCRIPTION disagrees with its snapshot: ", archive)
  }
  invisible(TRUE)
}

compat_fetch_verify_archive <- function(path, row) {
  compat_fetch_require_regular(path, "pinned source archive")
  observed_md5 <- unname(tools::md5sum(path))
  observed_sha256 <- unname(tools::sha256sum(path))
  declared_md5 <- row$md5[[1L]]
  if ((nzchar(declared_md5) &&
      !identical(observed_md5, tolower(declared_md5))) ||
      !identical(observed_sha256, tolower(row$sha256[[1L]]))) {
    compat_fetch_stop("pinned source archive checksum mismatch: ", path)
  }
  compat_fetch_archive_metadata(path, row$package[[1L]], row$version[[1L]])
  compat_fetch_require_regular(path, "pinned source archive")
  final_md5 <- unname(tools::md5sum(path))
  final_sha256 <- unname(tools::sha256sum(path))
  if (!identical(final_md5, observed_md5) ||
      !identical(final_sha256, observed_sha256)) {
    compat_fetch_stop("pinned source archive changed while it was verified: ", path)
  }
  invisible(TRUE)
}

compat_fetch_reserve_directory <- function(parent, prefix) {
  for (attempt in seq_len(128L)) {
    candidate <- tempfile(prefix, tmpdir = parent)
    if (!file.exists(candidate) && !dir.exists(candidate) &&
        !compat_fetch_is_symbolic(candidate) &&
        dir.create(candidate, recursive = FALSE, mode = "0700")) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }
  compat_fetch_stop("could not reserve an exclusive staging directory in ", parent)
}

compat_fetch_release_lock <- function(lock, strict = FALSE) {
  entries <- if (dir.exists(lock) && !compat_fetch_is_symbolic(lock)) {
    list.files(lock, all.files = TRUE, no.. = TRUE)
  } else {
    NA_character_
  }
  released <- length(entries) == 0L && isTRUE(file.remove(lock)) &&
    !file.exists(lock) && !dir.exists(lock) && !compat_fetch_is_symbolic(lock)
  if (!released && strict) {
    compat_fetch_stop("source-fetch lock changed or could not be released: ", lock)
  }
  invisible(released)
}

compat_fetch_select_archive <- function(staged, destination) {
  if (file.exists(destination) || dir.exists(destination) ||
      compat_fetch_is_symbolic(destination)) {
    compat_fetch_stop("refusing to overwrite an existing archive path: ", destination)
  }
  if (!file.link(staged, destination)) {
    compat_fetch_stop("could not atomically select downloaded archive: ", destination)
  }
  compat_fetch_require_regular(destination, "selected source archive")
  invisible(TRUE)
}

compat_fetch_sources <- function(root, source, destination_name, offline = FALSE) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  expected_home <- file.path(root, ".local", "toolchain", "lib", "R")
  expected_rscript <- file.path(root, ".local", "toolchain", "bin", "Rscript")
  if (!identical(Sys.getenv("PARADOX_ACTIVE_ROOT"), root) ||
      !identical(normalizePath(R.home(), winslash = "/", mustWork = TRUE), expected_home) ||
      !identical(unname(Sys.which("Rscript")), expected_rscript)) {
    compat_fetch_stop("activate the exact repository-local R first: . scripts/activate")
  }
  compat_fetch_require_regular(expected_rscript, "repository-local Rscript")

  inventory_path <- file.path(root, "compat", "reverse-dependencies.tsv")
  snapshot_path <- file.path(
    root,
    "compat",
    if (identical(source, "CRAN")) "cran-snapshot.tsv" else "bioconductor-snapshot.tsv"
  )
  manifest_paths <- c(inventory_path, snapshot_path)
  input_hashes_before <- compat_fetch_file_hashes(
    manifest_paths, "source-fetch manifest"
  )
  inventory <- compat_fetch_read_tsv(
    inventory_path,
    c("relation", "package", "source", "priority", "notes"),
    "reverse-dependency inventory"
  )
  snapshot_names <- if (identical(source, "CRAN")) {
    c(
      "package", "relation", "source", "priority", "notes", "version",
      "repository", "archive", "md5", "sha256"
    )
  } else {
    c("package", "version", "repository", "archive", "sha256")
  }
  snapshot <- compat_fetch_read_tsv(snapshot_path, snapshot_names, "source snapshot")
  input_hashes <- compat_fetch_file_hashes(manifest_paths, "source-fetch manifest")
  if (!identical(input_hashes_before, input_hashes)) {
    compat_fetch_stop("source manifests changed while they were parsed")
  }
  if (!"md5" %in% names(snapshot)) {
    # The Bioconductor snapshot is authenticated by SHA-256.  CRAN retains its
    # published MD5 value as a second, independently checked declaration.
    snapshot$md5 <- rep.int("", nrow(snapshot))
  }

  selected <- inventory[inventory$source == source, , drop = FALSE]
  if (!nrow(selected) || anyDuplicated(selected$package) ||
      anyDuplicated(snapshot$package) ||
      !setequal(selected$package, snapshot$package)) {
    compat_fetch_stop("inventory and ", source, " snapshot package sets disagree")
  }
  selected <- selected[match(snapshot$package, selected$package), , drop = FALSE]
  if (identical(source, "CRAN")) {
    compared <- c("relation", "source", "priority", "notes")
    if (any(vapply(compared, function(field) {
      !identical(snapshot[[field]], selected[[field]])
    }, logical(1L)))) {
      compat_fetch_stop("CRAN snapshot metadata disagrees with the reviewed inventory")
    }
  }

  package_pattern <- "^[A-Za-z][A-Za-z0-9.]*$"
  version_pattern <- "^[A-Za-z0-9][A-Za-z0-9.+-]*$"
  archive_pattern <- "^[A-Za-z0-9][A-Za-z0-9._+-]*[.]tar[.]gz$"
  if (any(!grepl(package_pattern, snapshot$package)) ||
      any(!grepl(version_pattern, snapshot$version)) ||
      any(!grepl(archive_pattern, snapshot$archive)) ||
      any(snapshot$archive != basename(snapshot$archive)) ||
      any(snapshot$archive != paste0(snapshot$package, "_", snapshot$version, ".tar.gz")) ||
      anyDuplicated(snapshot$archive) ||
      any(!grepl("^https://[^/?#]+(/[^?#]*)?$", snapshot$repository)) ||
      any(!grepl("^[0-9a-f]{64}$", tolower(snapshot$sha256))) ||
      (identical(source, "CRAN") &&
        any(!grepl("^[0-9a-f]{32}$", tolower(snapshot$md5))))) {
    compat_fetch_stop(source, " snapshot contains unsafe or malformed values")
  }
  expected_repository <- if (identical(source, "CRAN")) {
    "https://cloud.r-project.org/src/contrib"
  } else {
    "https://bioconductor.org/packages/3.23/bioc/src/contrib"
  }
  if (any(snapshot$repository != expected_repository)) {
    compat_fetch_stop(source, " snapshot contains an unexpected repository")
  }

  compat_root <- file.path(root, ".local", "compat")
  destination <- file.path(compat_root, destination_name)
  for (path in c(file.path(root, ".local"), compat_root, destination)) {
    compat_fetch_make_directory(root, path, "managed compatibility directory")
  }

  lock <- file.path(compat_root, paste0(".", destination_name, ".lock"))
  if (file.exists(lock) || dir.exists(lock) || compat_fetch_is_symbolic(lock) ||
      !dir.create(lock, recursive = FALSE, mode = "0700")) {
    compat_fetch_stop("source fetch is already running or has a stale lock: ", lock)
  }
  lock_active <- TRUE
  on.exit({
    if (lock_active) compat_fetch_release_lock(lock, strict = FALSE)
  }, add = TRUE)
  lock_resolved <- normalizePath(lock, winslash = "/", mustWork = TRUE)
  if (!identical(lock_resolved, lock) ||
      !identical(dirname(lock_resolved), compat_root)) {
    compat_fetch_stop("source-fetch lock escaped its managed parent: ", lock)
  }

  initial_entries <- list.files(destination, all.files = TRUE, no.. = TRUE)
  if (any(!initial_entries %in% snapshot$archive)) {
    compat_fetch_stop("source directory contains an unpinned entry: ", destination)
  }

  for (index in seq_len(nrow(snapshot))) {
    compat_fetch_make_directory(root, destination, "managed compatibility directory")
    row <- snapshot[index, , drop = FALSE]
    final <- file.path(destination, row$archive[[1L]])
    if (file.exists(final) || dir.exists(final) || compat_fetch_is_symbolic(final)) {
      compat_fetch_verify_archive(final, row)
      next
    }
    if (offline) {
      compat_fetch_stop("offline mode is missing pinned source archive: ", final)
    }

    staging <- compat_fetch_reserve_directory(destination, ".fetch-")
    staged <- file.path(staging, row$archive[[1L]])
    on.exit(unlink(staging, recursive = TRUE, force = FALSE), add = TRUE)
    urls <- paste0(
      sub("/+$", "", row$repository[[1L]]), "/", row$archive[[1L]]
    )
    if (identical(source, "CRAN")) {
      urls <- c(
        urls,
        paste0(
          expected_repository, "/Archive/", row$package[[1L]], "/",
          row$archive[[1L]]
        )
      )
    }
    downloaded <- FALSE
    for (url in urls) {
      status <- tryCatch(
        utils::download.file(
          url,
          destfile = staged,
          method = "libcurl",
          mode = "wb",
          quiet = FALSE
        ),
        error = identity
      )
      if (!inherits(status, "error") && identical(status, 0L)) {
        downloaded <- TRUE
        break
      }
      if (file.exists(staged) || dir.exists(staged) ||
          compat_fetch_is_symbolic(staged)) {
        compat_fetch_require_regular(staged, "partial source download")
        if (!file.remove(staged)) {
          compat_fetch_stop("could not remove a partial source download: ", staged)
        }
      }
    }
    if (!downloaded) {
      compat_fetch_stop(
        "download failed without selecting a final archive: ",
        paste(urls, collapse = ", ")
      )
    }
    compat_fetch_verify_archive(staged, row)
    compat_fetch_make_directory(root, destination, "managed compatibility directory")
    compat_fetch_select_archive(staged, final)
    compat_fetch_verify_archive(final, row)
    unlink(staging, recursive = TRUE, force = FALSE)
  }

  entries <- list.files(destination, all.files = TRUE, no.. = TRUE)
  if (!setequal(entries, snapshot$archive) || length(entries) != nrow(snapshot)) {
    compat_fetch_stop("source directory is not exactly the pinned snapshot: ", destination)
  }
  final_paths <- file.path(destination, snapshot$archive)
  invisible(Map(compat_fetch_verify_archive, final_paths, split(snapshot, seq_len(nrow(snapshot)))))
  final_input_hashes <- compat_fetch_file_hashes(
    manifest_paths, "source-fetch manifest"
  )
  if (!identical(input_hashes, final_input_hashes)) {
    compat_fetch_stop("source manifests changed while archives were fetched")
  }

  compat_fetch_release_lock(lock, strict = TRUE)
  lock_active <- FALSE

  message(
    "Authenticated ", nrow(snapshot), " pinned ", source,
    " source archive", if (nrow(snapshot) == 1L) "" else "s", "."
  )
  invisible(destination)
}

compat_fetch_entry <- function(source, destination_name) {
  command <- commandArgs(trailingOnly = FALSE)
  file_argument <- grep("^--file=", command, value = TRUE)
  if (length(file_argument) != 1L) {
    compat_fetch_stop("could not identify the source-fetch entry point")
  }
  script <- normalizePath(sub("^--file=", "", file_argument), winslash = "/", mustWork = TRUE)
  repository_root <- normalizePath(file.path(dirname(script), ".."), winslash = "/", mustWork = TRUE)
  arguments <- commandArgs(trailingOnly = TRUE)
  if (length(arguments) == 1L && arguments[[1L]] %in% c("-h", "--help")) {
    cat(
      "Usage: ", basename(script), " [REPOSITORY_ROOT] [--offline]\n\n",
      "Fetch or verify only the exact archives in the checked-in source snapshot.\n",
      "The command never refreshes or rewrites that snapshot.\n",
      sep = ""
    )
    return(invisible(NULL))
  }
  offline <- "--offline" %in% arguments
  arguments <- arguments[arguments != "--offline"]
  if (length(arguments) > 1L || any(startsWith(arguments, "-"))) {
    compat_fetch_stop("usage: ", basename(script), " [REPOSITORY_ROOT] [--offline]")
  }
  root <- if (length(arguments)) {
    normalizePath(arguments[[1L]], winslash = "/", mustWork = TRUE)
  } else {
    repository_root
  }
  if (!identical(root, repository_root)) {
    compat_fetch_stop("repository root does not own this fetch script: ", root)
  }
  compat_fetch_sources(root, source, destination_name, offline)
}
