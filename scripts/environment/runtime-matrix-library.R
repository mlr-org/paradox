args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 5L || !args[[1L]] %in% c("install", "inventory")) {
  stop(
    paste(
      "usage: runtime-matrix-library.R install|inventory",
      "RUNTIME LOCK CACHE LIBRARY"
    ),
    call. = FALSE
  )
}

operation <- args[[1L]]
runtime <- args[[2L]]
lock_path <- args[[3L]]
cache <- args[[4L]]
library <- args[[5L]]
if (!grepl("^[0-9]+[.][0-9]+[.][0-9]+$", runtime)) {
  stop("runtime-library version has an invalid shape", call. = FALSE)
}
if (!identical(as.character(getRversion()), runtime)) {
  stop(
    "package closure must be inspected by exact R ", runtime,
    call. = FALSE
  )
}

is_symbolic <- function(path) {
  target <- Sys.readlink(path)
  length(target) == 1L && !is.na(target) && nzchar(target)
}

root_input <- Sys.getenv("PARADOX_ROOT", unset = "")
if (!nzchar(root_input) || !dir.exists(root_input) || is_symbolic(root_input)) {
  stop("PARADOX_ROOT must identify a plain repository root", call. = FALSE)
}
root <- normalizePath(root_input, winslash = "/", mustWork = TRUE)
if (!identical(root_input, root)) {
  stop("PARADOX_ROOT must be canonical: ", root_input, call. = FALSE)
}

assert_plain_directory_chain <- function(path, label) {
  if (!nzchar(path) || (nchar(path) > 1L && endsWith(path, "/"))) {
    stop(label, " has a non-canonical trailing separator: ", path,
      call. = FALSE)
  }
  if (!identical(path, root) && !startsWith(path, paste0(root, "/"))) {
    stop(label, " escaped the repository: ", path, call. = FALSE)
  }
  relative <- if (identical(path, root)) {
    ""
  } else {
    substring(path, nchar(root) + 2L)
  }
  components <- if (nzchar(relative)) {
    strsplit(relative, "/", fixed = TRUE)[[1L]]
  } else {
    character()
  }
  if (any(!nzchar(components)) || any(components %in% c(".", ".."))) {
    stop("invalid managed path component in ", label, ": ", path,
      call. = FALSE)
  }
  current <- root
  for (component in components) {
    current <- file.path(current, component)
    if (is_symbolic(current)) {
      stop("refusing symbolic ", label, " component: ", current,
        call. = FALSE)
    }
    info <- file.info(current)
    if (is.na(info$isdir) || !isTRUE(info$isdir)) {
      stop(label, " component is absent or not a directory: ", current,
        call. = FALSE)
    }
    if (!identical(
      normalizePath(current, winslash = "/", mustWork = TRUE),
      current
    )) {
      stop(label, " component is not canonical: ", current, call. = FALSE)
    }
  }
  invisible(path)
}

require_plain_file <- function(path, label) {
  assert_plain_directory_chain(dirname(path), paste0(label, " parent"))
  mode <- suppressWarnings(system2(
    "/usr/bin/stat",
    c("-c", "%f", "--", shQuote(path)),
    stdout = TRUE,
    stderr = FALSE
  ))
  regular <- length(mode) == 1L && is.null(attr(mode, "status")) &&
    grepl("^[0-9a-fA-F]+$", mode) &&
    bitwAnd(strtoi(mode, base = 16L), 61440L) == 32768L
  if (!isTRUE(regular) || is_symbolic(path)) {
    stop(label, " is absent, non-regular, or symbolic: ", path,
      call. = FALSE)
  }
  if (!identical(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    path
  )) {
    stop(label, " is not canonical: ", path, call. = FALSE)
  }
  invisible(path)
}

require_plain_file(lock_path, "runtime package lock")
assert_plain_directory_chain(library, "runtime-library library")

lock <- utils::read.delim(
  lock_path,
  header = TRUE,
  quote = "",
  comment.char = "",
  colClasses = "character",
  check.names = FALSE
)
expected_columns <- c(
  "Package", "Version", "Role", "SHA256", "URL", "FallbackURL"
)
if (!identical(names(lock), expected_columns) || !nrow(lock) ||
    anyDuplicated(lock$Package)) {
  stop("runtime package lock is empty, duplicated, or malformed",
    call. = FALSE)
}

installed_inventory <- function() {
  assert_plain_directory_chain(library, "runtime-library library")
  entries <- list.dirs(library, full.names = FALSE, recursive = FALSE)
  entries <- entries[nzchar(entries)]
  if (anyDuplicated(entries) || !setequal(entries, lock$Package)) {
    stop("installed package names differ from the exact lock", call. = FALSE)
  }
  rows <- lapply(lock$Package, function(package) {
    path <- file.path(library, package)
    description_path <- file.path(path, "DESCRIPTION")
    assert_plain_directory_chain(path, "installed package")
    require_plain_file(description_path, "installed package DESCRIPTION")
    description <- read.dcf(description_path)
    if (nrow(description) != 1L ||
        !all(c("Package", "Version", "Built") %in% colnames(description))) {
      stop("installed package DESCRIPTION is incomplete: ", package,
        call. = FALSE)
    }
    data.frame(
      Package = unname(description[1L, "Package"]),
      Version = unname(description[1L, "Version"]),
      Built = unname(description[1L, "Built"]),
      stringsAsFactors = FALSE
    )
  })
  inventory <- do.call(rbind, rows)
  row.names(inventory) <- NULL
  if (!identical(inventory$Package, lock$Package) ||
      !identical(inventory$Version, lock$Version) ||
      any(!startsWith(inventory$Built, paste0("R ", runtime, ";")))) {
    stop("installed package version or Built runtime differs from the lock",
      call. = FALSE)
  }
  inventory
}

if (operation == "inventory") {
  utils::write.table(
    installed_inventory(),
    file = stdout(),
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )
  quit(save = "no", status = 0L)
}

assert_plain_directory_chain(cache, "runtime-library cache")
archive_paths <- file.path(
  cache, sprintf("%s_%s.tar.gz", lock$Package, lock$Version)
)
for (archive in archive_paths) {
  require_plain_file(archive, "authenticated package archive")
}

read_archive_description <- function(package, archive) {
  member <- paste0(package, "/DESCRIPTION")
  members <- utils::untar(archive, list = TRUE)
  if (sum(members == member) != 1L) {
    stop("archive does not contain one exact ", member, call. = FALSE)
  }
  extraction <- tempfile("runtime-matrix-description-")
  if (!dir.create(extraction)) {
    stop("could not create DESCRIPTION extraction directory", call. = FALSE)
  }
  on.exit(unlink(extraction, recursive = TRUE, force = TRUE), add = TRUE)
  status <- utils::untar(archive, files = member, exdir = extraction)
  if (!is.null(status) && !identical(status, 0L)) {
    stop("could not extract DESCRIPTION from ", archive, call. = FALSE)
  }
  value <- read.dcf(file.path(extraction, member))
  if (nrow(value) != 1L) {
    stop("invalid DESCRIPTION in ", archive, call. = FALSE)
  }
  value[1L, , drop = TRUE]
}

descriptions <- Map(read_archive_description, lock$Package, archive_paths)
names(descriptions) <- lock$Package
for (index in seq_len(nrow(lock))) {
  description <- descriptions[[index]]
  if (!identical(unname(description[["Package"]]), lock$Package[[index]]) ||
      !identical(unname(description[["Version"]]), lock$Version[[index]])) {
    stop("archive metadata disagrees with the package lock", call. = FALSE)
  }
}

if (length(list.files(library, all.files = TRUE, no.. = TRUE)) != 0L) {
  stop("install destination is not empty", call. = FALSE)
}
dependency_names <- function(description) {
  fields <- intersect(c("Depends", "Imports", "LinkingTo"), names(description))
  if (!length(fields)) return(character())
  text <- paste(unname(description[fields]), collapse = ",")
  entries <- trimws(strsplit(text, ",", fixed = TRUE)[[1L]])
  names <- sub("[[:space:]]*[(].*$", "", entries)
  unique(names[nzchar(names) & names != "R"])
}

dependencies <- lapply(descriptions, dependency_names)
locked_names <- lock$Package
base_names <- row.names(utils::installed.packages(lib.loc = .Library))
external <- unique(setdiff(unlist(dependencies, use.names = FALSE), locked_names))
if (length(setdiff(external, base_names))) {
  stop(
    "package lock omits required packages: ",
    paste(sort(setdiff(external, base_names)), collapse = ", "),
    call. = FALSE
  )
}

remaining <- locked_names
installed <- character()
while (length(remaining)) {
  ready <- remaining[vapply(
    remaining,
    function(package) {
      all(intersect(dependencies[[package]], locked_names) %in% installed)
    },
    logical(1L)
  )]
  if (!length(ready)) {
    stop("package lock dependency graph is cyclic", call. = FALSE)
  }
  for (package in ready) {
    archive <- archive_paths[match(package, locked_names)]
    assert_plain_directory_chain(library, "runtime-library library")
    require_plain_file(archive, "authenticated package archive")
    utils::install.packages(
      archive,
      repos = NULL,
      type = "source",
      lib = library,
      dependencies = FALSE,
      INSTALL_opts = c("--preclean", "--clean", "--no-multiarch"),
      quiet = TRUE
    )
    installed <- c(installed, package)
  }
  remaining <- setdiff(remaining, ready)
}

invisible(installed_inventory())
