root <- Sys.getenv("PARADOX_ROOT", unset = "")
library <- Sys.getenv("R_LIBS_USER", unset = "")

if (!nzchar(root) || !dir.exists(root) || nzchar(Sys.readlink(root))) {
  stop("PARADOX_ROOT must identify the repository root")
}
root <- normalizePath(root, winslash = "/", mustWork = TRUE)
if (!nzchar(library)) {
  stop("R_LIBS_USER must identify the repository-local package library")
}

assert_plain_directory_chain <- function(path, label) {
  path <- sub("/+$", "", path)
  if (!identical(path, root) && !startsWith(path, paste0(root, "/"))) {
    stop(label, " escaped the repository: ", path)
  }
  relative <- substring(path, nchar(root) + 2L)
  components <- if (nzchar(relative)) strsplit(relative, "/", fixed = TRUE)[[1L]] else character()
  current <- root
  for (component in components) {
    if (!nzchar(component) || component %in% c(".", "..")) {
      stop("Invalid managed path component in ", label, ": ", path)
    }
    current <- file.path(current, component)
    if (nzchar(Sys.readlink(current))) {
      stop("Refusing symbolic ", label, " component: ", current)
    }
    info <- file.info(current)
    if (!is.na(info$isdir) && !isTRUE(info$isdir)) {
      stop(label, " path component is not a directory: ", current)
    }
  }
  invisible(path)
}

library <- sub("/+$", "", library)
expected_library <- file.path(root, ".local", "R", "library")
if (!identical(library, expected_library)) {
  stop("R_LIBS_USER must be the exact repository-local package library: ", expected_library)
}
assert_plain_directory_chain(library, "R package library")
dir.create(library, recursive = TRUE, showWarnings = FALSE)
library <- normalizePath(library, winslash = "/", mustWork = TRUE)

lock_path <- file.path(root, "environment", "r-packages-linux-64.lock")
if (!file.exists(lock_path) || nzchar(Sys.readlink(lock_path))) {
  stop("Pinned R package lock is missing or symbolic: ", lock_path)
}
lock <- utils::read.delim(
  lock_path,
  header = TRUE,
  quote = "",
  comment.char = "",
  colClasses = "character",
  check.names = FALSE
)
expected_columns <- c("Package", "Version", "Role", "SHA256")
if (!identical(names(lock), expected_columns) || !nrow(lock)) {
  stop("R package lock must have columns: ", paste(expected_columns, collapse = ", "))
}
if (anyDuplicated(lock$Package)) {
  stop("R package lock contains duplicate package names")
}
if (any(!grepl("^[A-Za-z][A-Za-z0-9.]*$", lock$Package))) {
  stop("R package lock contains an invalid package name")
}
if (any(!grepl("^[A-Za-z0-9][A-Za-z0-9.-]*$", lock$Version))) {
  stop("R package lock contains an invalid package version")
}
if (any(!lock$Role %in% c("direct", "dependency", "bootstrap-only"))) {
  stop("R package lock contains an invalid role")
}
if (!identical(lock$Package[lock$Role == "bootstrap-only"], "paradox")) {
  stop("The sole bootstrap-only input must be the pinned CRAN paradox baseline")
}
if (any(!grepl("^[0-9a-f]{64}$", lock$SHA256))) {
  stop("R package lock contains an invalid SHA-256")
}

cache <- file.path(root, ".cache", "downloads", "r-packages")
assert_plain_directory_chain(cache, "R package source cache")
dir.create(cache, recursive = TRUE, showWarnings = FALSE)
cache <- normalizePath(cache, winslash = "/", mustWork = TRUE)

archive_path <- function(package, version) {
  file.path(cache, sprintf("%s_%s.tar.gz", package, version))
}

archive_urls <- function(package, version) {
  filename <- sprintf("%s_%s.tar.gz", package, version)
  c(
    sprintf("https://cloud.r-project.org/src/contrib/%s", filename),
    sprintf(
      "https://cloud.r-project.org/src/contrib/Archive/%s/%s",
      package,
      filename
    )
  )
}

download_locked_archive <- function(package, version, destination) {
  partial <- paste0(destination, ".part")
  unlink(partial)
  on.exit(unlink(partial), add = TRUE)
  for (url in archive_urls(package, version)) {
    ok <- tryCatch({
      utils::download.file(url, partial, mode = "wb", quiet = FALSE)
      TRUE
    }, error = function(...) FALSE, warning = function(...) FALSE)
    if (ok && file.exists(partial)) {
      if (!file.rename(partial, destination)) {
        stop("Could not atomically place downloaded archive: ", destination)
      }
      return(invisible(destination))
    }
    unlink(partial)
  }
  stop("Could not download locked CRAN source archive for ", package, " ", version)
}

archives <- character(nrow(lock))
for (i in seq_len(nrow(lock))) {
  archive <- archive_path(lock$Package[[i]], lock$Version[[i]])
  if (nzchar(Sys.readlink(archive))) {
    stop("Refusing symbolic R package archive: ", archive)
  }
  if (!file.exists(archive)) {
    download_locked_archive(lock$Package[[i]], lock$Version[[i]], archive)
  }
  actual <- unname(tools::sha256sum(archive))
  if (!identical(actual, lock$SHA256[[i]])) {
    stop("SHA-256 mismatch for locked R package archive: ", archive)
  }
  archives[[i]] <- archive
}
names(archives) <- lock$Package

read_archive_description <- function(package, archive) {
  member <- paste0(package, "/DESCRIPTION")
  members <- utils::untar(archive, list = TRUE)
  if (sum(members == member) != 1L) {
    stop("Locked archive does not contain exactly one ", member, ": ", archive)
  }
  extraction <- tempfile("paradox-r-package-description-")
  dir.create(extraction)
  on.exit(unlink(extraction, recursive = TRUE, force = TRUE), add = TRUE)
  status <- utils::untar(archive, files = member, exdir = extraction)
  if (!is.null(status) && !identical(status, 0L)) {
    stop("Could not extract DESCRIPTION from ", archive)
  }
  description <- read.dcf(file.path(extraction, member))
  if (nrow(description) != 1L) {
    stop("Invalid DESCRIPTION in ", archive)
  }
  description[1L, , drop = TRUE]
}

descriptions <- Map(read_archive_description, lock$Package, archives)
for (i in seq_len(nrow(lock))) {
  description <- descriptions[[i]]
  if (!identical(unname(description[["Package"]]), lock$Package[[i]]) ||
      !identical(unname(description[["Version"]]), lock$Version[[i]])) {
    stop("Locked archive metadata disagrees with the lock: ", archives[[i]])
  }
}
names(descriptions) <- lock$Package

dependency_requirements <- function(description, consumer) {
  fields <- intersect(c("Depends", "Imports", "LinkingTo"), names(description))
  if (!length(fields)) {
    return(data.frame(
      Consumer = character(), Package = character(), Operator = character(),
      Version = character(), stringsAsFactors = FALSE
    ))
  }
  specifications <- trimws(unlist(strsplit(
    paste(description[fields], collapse = ","),
    ",",
    fixed = TRUE
  )))
  specifications <- specifications[nzchar(specifications)]
  pattern <- paste0(
    "^([A-Za-z][A-Za-z0-9.]*)",
    "(?:[[:space:]]*\\(([><]=?|==)[[:space:]]*([^()[:space:]]+)[[:space:]]*\\))?$"
  )
  matches <- regexec(pattern, specifications, perl = TRUE)
  pieces <- regmatches(specifications, matches)
  if (any(lengths(pieces) == 0L)) {
    stop(
      "Could not parse hard dependency specification for ", consumer, ": ",
      paste(specifications[lengths(pieces) == 0L], collapse = ", ")
    )
  }
  data.frame(
    Consumer = rep(consumer, length(pieces)),
    Package = vapply(pieces, `[[`, character(1L), 2L),
    Operator = vapply(pieces, function(piece) if (length(piece) >= 3L) piece[[3L]] else "", character(1L)),
    Version = vapply(pieces, function(piece) if (length(piece) >= 4L) piece[[4L]] else "", character(1L)),
    stringsAsFactors = FALSE
  )
}

requirements <- Map(dependency_requirements, descriptions, names(descriptions))
all_requirements <- do.call(rbind, unname(requirements))
if (is.null(all_requirements)) {
  all_requirements <- data.frame(
    Consumer = character(), Package = character(), Operator = character(),
    Version = character(), stringsAsFactors = FALSE
  )
}
dependencies <- lapply(requirements, function(value) unique(setdiff(value$Package, "R")))
runtime <- utils::installed.packages(lib.loc = .Library)
runtime_packages <- rownames(runtime)[
  !is.na(runtime[, "Priority"]) & runtime[, "Priority"] == "base"
]
unlocked <- sort(unique(setdiff(unlist(dependencies, use.names = FALSE), c(
  lock$Package,
  runtime_packages
))))
if (length(unlocked)) {
  stop("R package lock has an incomplete dependency closure: ", paste(unlocked, collapse = ", "))
}

version_satisfies <- function(actual, operator, required) {
  if (!nzchar(operator)) {
    return(TRUE)
  }
  comparison <- utils::compareVersion(actual, required)
  switch(
    operator,
    ">" = comparison > 0L,
    ">=" = comparison >= 0L,
    "<" = comparison < 0L,
    "<=" = comparison <= 0L,
    "==" = comparison == 0L,
    stop("Unsupported dependency version operator: ", operator)
  )
}

constraints_for <- function(package) {
  all_requirements[
    all_requirements$Package == package & nzchar(all_requirements$Operator),
    , drop = FALSE
  ]
}

constraints_satisfied <- function(package, version) {
  constraints <- constraints_for(package)
  if (!nrow(constraints)) {
    return(TRUE)
  }
  all(mapply(
    version_satisfies,
    actual = rep(version, nrow(constraints)),
    operator = constraints$Operator,
    required = constraints$Version,
    USE.NAMES = FALSE
  ))
}

constraint_text <- function(package) {
  constraints <- constraints_for(package)
  if (!nrow(constraints)) {
    return("no explicit version constraint")
  }
  paste(unique(sprintf(
    "%s requires %s %s %s",
    constraints$Consumer, package, constraints$Operator, constraints$Version
  )), collapse = "; ")
}

r_version <- as.character(getRversion())
if (!constraints_satisfied("R", r_version)) {
  stop("Local R ", r_version, " violates the locked package closure: ", constraint_text("R"))
}

for (package in runtime_packages) {
  if (!constraints_satisfied(package, runtime[[package, "Version"]])) {
    stop(
      "Local base package ", package, " ", runtime[[package, "Version"]],
      " violates the locked package closure: ", constraint_text(package)
    )
  }
}

for (package in lock$Package) {
  version <- lock$Version[match(package, lock$Package)]
  if (!constraints_satisfied(package, version)) {
    stop(
      if (lock$Role[match(package, lock$Package)] == "bootstrap-only") {
        "Locked bootstrap-only baseline "
      } else {
        "Locked package "
      },
      package, " ", version,
      " violates the locked package closure: ", constraint_text(package)
    )
  }
}

installed_local <- function() {
  matrix <- utils::installed.packages(lib.loc = library)
  stats::setNames(matrix[, "Version"], rownames(matrix))
}

is_satisfied <- function(package, installed) {
  row <- match(package, lock$Package)
  if (is.na(row) || !package %in% names(installed)) {
    return(FALSE)
  }
  if (lock$Role[[row]] == "bootstrap-only") {
    return(constraints_satisfied(package, unname(installed[[package]])))
  }
  identical(unname(installed[[package]]), lock$Version[[row]]) &&
    constraints_satisfied(package, unname(installed[[package]]))
}

installed <- installed_local()
bootstrap_package <- lock$Package[lock$Role == "bootstrap-only"]
bootstrap_was_present <- bootstrap_package %in% names(installed)
if (bootstrap_was_present && !is_satisfied(bootstrap_package, installed)) {
  stop(
    "Installed bootstrap-only ", bootstrap_package, " ",
    unname(installed[[bootstrap_package]]),
    " violates the locked package closure; refusing to overwrite it: ",
    constraint_text(bootstrap_package)
  )
}
pending <- lock$Package[!vapply(lock$Package, is_satisfied, logical(1L), installed)]
while (length(pending)) {
  ready <- pending[vapply(pending, function(package) {
    all(vapply(dependencies[[package]], function(dependency) {
      if (dependency %in% runtime_packages) {
        return(TRUE)
      }
      is_satisfied(dependency, installed)
    }, logical(1L)))
  }, logical(1L))]
  if (!length(ready)) {
    stop("Locked R package dependency graph could not be installed: ", paste(pending, collapse = ", "))
  }
  package <- ready[[1L]]
  message("Installing locked R package ", package, " ", lock$Version[match(package, lock$Package)])
  utils::install.packages(
    archives[[package]],
    lib = library,
    repos = NULL,
    type = "source",
    dependencies = FALSE,
    Ncpus = 1L
  )
  installed <- installed_local()
  if (!is_satisfied(package, installed)) {
    stop("Failed to install locked R package: ", package)
  }
  pending <- lock$Package[!vapply(lock$Package, is_satisfied, logical(1L), installed)]
}

available <- utils::installed.packages(lib.loc = c(library, .Library))
missing_roots <- setdiff(lock$Package[lock$Role == "direct"], rownames(available))
if (length(missing_roots)) {
  stop("Required local R package roots are unavailable: ", paste(missing_roots, collapse = ", "))
}

selected_bootstrap_version <- unname(installed_local()[[bootstrap_package]])
if (bootstrap_was_present) {
  message(
    "Verified ", sum(lock$Role != "bootstrap-only"),
    " exact locked R packages and authenticated the bootstrap-only ",
    bootstrap_package, " ", lock$Version[match(bootstrap_package, lock$Package)],
    " archive; preserved the already selected ", bootstrap_package, " ",
    selected_bootstrap_version, " without installing the baseline over it"
  )
} else {
  message(
    "Verified ", sum(lock$Role != "bootstrap-only"),
    " exact locked R packages and installed the authenticated bootstrap-only ",
    bootstrap_package, " baseline ", selected_bootstrap_version
  )
}
