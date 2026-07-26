args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L || !args[[1L]] %in% c("install", "inventory")) {
  stop(
    "usage: runtime-matrix-library.R install|inventory LOCK CACHE LIBRARY",
    call. = FALSE
  )
}

operation <- args[[1L]]
lock_path <- args[[2L]]
cache <- args[[3L]]
library <- args[[4L]]

is_symbolic <- function(path) {
  target <- Sys.readlink(path)
  length(target) == 1L && !is.na(target) && nzchar(target)
}

for (path in c(lock_path, cache, library)) {
  if (!file.exists(path) || is_symbolic(path)) {
    stop("runtime-library input is absent or symbolic: ", path, call. = FALSE)
  }
}
if (!dir.exists(cache) || !dir.exists(library)) {
  stop("runtime-library cache and library must be directories", call. = FALSE)
}

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
  stop("R 3.6 package lock is empty, duplicated, or malformed", call. = FALSE)
}

archive_paths <- file.path(
  cache, sprintf("%s_%s.tar.gz", lock$Package, lock$Version)
)
if (any(!file.exists(archive_paths)) || any(dir.exists(archive_paths)) ||
    any(vapply(archive_paths, is_symbolic, logical(1L)))) {
  stop("one or more authenticated package archives are absent", call. = FALSE)
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

installed_inventory <- function() {
  entries <- list.dirs(library, full.names = FALSE, recursive = FALSE)
  entries <- entries[nzchar(entries)]
  if (anyDuplicated(entries) || !setequal(entries, lock$Package)) {
    stop("installed package names differ from the exact lock", call. = FALSE)
  }
  rows <- lapply(lock$Package, function(package) {
    path <- file.path(library, package)
    description_path <- file.path(path, "DESCRIPTION")
    if (!dir.exists(path) || is_symbolic(path) ||
        !file.exists(description_path) || is_symbolic(description_path)) {
      stop("installed package is absent or symbolic: ", package, call. = FALSE)
    }
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
      any(!grepl("^R 3[.]6[.]3;", inventory$Built))) {
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

if (length(list.files(library, all.files = TRUE, no.. = TRUE)) != 0L) {
  stop("install destination is not empty", call. = FALSE)
}
if (!identical(as.character(getRversion()), "3.6.3")) {
  stop("package closure must be installed by exact R 3.6.3", call. = FALSE)
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
