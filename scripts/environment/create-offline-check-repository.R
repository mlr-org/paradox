# This metadata builder belongs only to native-check's pinned current-R gate.
# The supported-runtime matrix, including R 3.6, has separate authenticated
# package closures and never executes this helper.

args = commandArgs(trailingOnly = TRUE)
if (length(args) != 5L) {
  stop(
    paste(
      "usage: create-offline-check-repository.R",
      "CRAN_REPOSITORY BIOC_REPOSITORY LOCK ARCHIVE_CACHE",
      "CANDIDATE_ARCHIVE"
    ),
    call. = FALSE
  )
}

cran_repository = args[[1L]]
bioc_repository = args[[2L]]
lock_path = args[[3L]]
archive_cache = args[[4L]]
candidate_archive = args[[5L]]

if (any(!grepl("^/", c(
      cran_repository, bioc_repository, lock_path, archive_cache,
      candidate_archive
    )))) {
  stop("all paths must be absolute",
    call. = FALSE
  )
}
if (file.exists(cran_repository) || dir.exists(cran_repository) ||
    file.exists(bioc_repository) || dir.exists(bioc_repository)) {
  stop("an offline check repository already exists", call. = FALSE)
}

is_symbolic = function(path) {
  target = Sys.readlink(path)
  length(target) == 1L && !is.na(target) && nzchar(target)
}

for (path in c(lock_path, candidate_archive)) {
  if (!file.exists(path) || is_symbolic(path)) {
    stop("offline check input is missing or symbolic: ", path,
      call. = FALSE
    )
  }
}
if (!dir.exists(archive_cache) || is_symbolic(archive_cache)) {
  stop("archive cache is missing or symbolic", call. = FALSE)
}
candidate_name = basename(candidate_archive)
candidate_match = regmatches(
  candidate_name,
  regexec(
    "^paradox_([A-Za-z0-9][A-Za-z0-9.-]*)[.]tar[.]gz$",
    candidate_name
  )
)[[1L]]
if (length(candidate_match) != 2L) {
  stop("candidate archive does not have a standard Paradox source name",
    call. = FALSE
  )
}
candidate_version = candidate_match[[2L]]

lock = utils::read.delim(
  lock_path,
  header = TRUE,
  quote = "",
  comment.char = "",
  colClasses = "character",
  check.names = FALSE
)
if (!identical(names(lock), c("Package", "Version", "Role", "SHA256")) ||
    !nrow(lock) ||
    anyDuplicated(lock$Package) ||
    any(!grepl("^[A-Za-z][A-Za-z0-9.]*$", lock$Package)) ||
    any(!grepl("^[A-Za-z0-9][A-Za-z0-9.-]*$", lock$Version)) ||
    any(!lock$Role %in% c("direct", "dependency", "bootstrap-only")) ||
    any(!grepl("^[0-9a-f]{64}$", lock$SHA256)) ||
    !identical(lock$Package[lock$Role == "bootstrap-only"], "paradox")) {
  stop("R package lock is malformed", call. = FALSE)
}
lock = lock[lock$Role != "bootstrap-only", , drop = FALSE]

archives = file.path(
  archive_cache,
  sprintf("%s_%s.tar.gz", lock$Package, lock$Version)
)
for (index in seq_along(archives)) {
  archive = archives[[index]]
  if (!file.exists(archive) || is_symbolic(archive)) {
    stop("locked package archive is missing or symbolic: ", archive,
      call. = FALSE
    )
  }
  if (!identical(unname(tools::sha256sum(archive)), lock$SHA256[[index]])) {
    stop("locked package archive SHA-256 differs: ", archive,
      call. = FALSE
    )
  }
}

cran_contrib = file.path(cran_repository, "src", "contrib")
bioc_contrib = file.path(bioc_repository, "src", "contrib")
if (!dir.create(cran_contrib, recursive = TRUE, showWarnings = FALSE) ||
    !dir.create(bioc_contrib, recursive = TRUE, showWarnings = FALSE)) {
  stop("could not create offline check repositories", call. = FALSE)
}
if (!identical(
      normalizePath(cran_repository, mustWork = TRUE),
      cran_repository
    ) ||
    !identical(
      normalizePath(bioc_repository, mustWork = TRUE),
      bioc_repository
    )) {
  stop("an offline check repository did not retain its absolute path",
    call. = FALSE
  )
}

staged_archives = file.path(cran_contrib, basename(archives))
candidate_staged = file.path(cran_contrib, candidate_name)
if (any(file.exists(c(staged_archives, candidate_staged))) ||
    !isTRUE(all(file.symlink(archives, staged_archives))) ||
    !isTRUE(file.symlink(candidate_archive, candidate_staged))) {
  stop("could not stage locked source archives", call. = FALSE)
}

written = tools::write_PACKAGES(cran_contrib, type = "source")
if (!identical(written, nrow(lock) + 1L)) {
  stop("offline CRAN index did not contain the exact locked closure",
    call. = FALSE
  )
}

# Delete only the exact, validated symlinks created above.  The retained
# evidence needs repository metadata, not 126 absolute cache links.
staged_archives = c(staged_archives, candidate_staged)
if (any(!vapply(staged_archives, is_symbolic, logical(1L))) ||
    any(unlink(staged_archives, force = TRUE) != 0L) ||
    any(file.exists(staged_archives))) {
  stop("could not remove staged source-archive links", call. = FALSE)
}

if (!file.create(file.path(bioc_contrib, "PACKAGES"))) {
  stop("could not create empty Bioconductor package index", call. = FALSE)
}

cran_available = utils::available.packages(
  repos = paste0("file://", cran_repository),
  filters = list()
)
expected_packages = sort(c(lock$Package, "paradox"), method = "radix")
if (!identical(
      sort(rownames(cran_available), method = "radix"),
      expected_packages
    ) ||
    !identical(
      unname(cran_available[lock$Package, "Version"]),
      lock$Version
    ) ||
    !identical(
      unname(cran_available["paradox", "Version"]),
      candidate_version
    )) {
  stop("offline CRAN package database does not round-trip", call. = FALSE)
}
bioc_available = utils::available.packages(
  repos = paste0("file://", bioc_repository),
  filters = list()
)
if (nrow(bioc_available) != 0L) {
  stop("offline Bioconductor package database is not empty", call. = FALSE)
}
cat("offline_check_repository_packages=", length(expected_packages), "\n",
  sep = ""
)
