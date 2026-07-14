#!/usr/bin/env Rscript

usage <- paste(
  "usage: install-reverse-dependency-dependencies.R",
  "--root ROOT --max-priority N --dependency-library LIBRARY",
  "--run-id ID [--package NAME ...] [--plan-only]"
)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 1L && args[[1L]] %in% c("-h", "--help")) {
  cat(
    usage, "\n\n",
    "Authenticate pinned reverse-package sources, resolve only hard R ",
    "dependencies, and retain a sealed dependency-library endpoint.\n",
    sep = ""
  )
  quit(save = "no", status = 0L)
}
values <- list(
  root = NULL, max_priority = NULL, dependency_library = NULL,
  run_id = NULL, packages = character(), plan_only = FALSE
)
index <- 1L
while (index <= length(args)) {
  argument <- args[[index]]
  if (identical(argument, "--plan-only")) {
    if (values$plan_only) stop("--plan-only may be supplied only once", call. = FALSE)
    values$plan_only <- TRUE
    index <- index + 1L
    next
  }
  matched <- FALSE
  for (option in c(
      "--root", "--max-priority", "--dependency-library", "--run-id",
      "--package")) {
    prefix <- paste0(option, "=")
    if (startsWith(argument, prefix)) {
      value <- substring(argument, nchar(prefix) + 1L)
      next_index <- index + 1L
    } else if (identical(argument, option)) {
      if (index == length(args)) stop(usage, call. = FALSE)
      value <- args[[index + 1L]]
      next_index <- index + 2L
    } else {
      next
    }
    if (!nzchar(value)) stop(option, " requires a value", call. = FALSE)
    if (identical(option, "--root")) {
      if (!is.null(values$root)) stop("--root may be supplied only once", call. = FALSE)
      values$root <- value
    } else if (identical(option, "--max-priority")) {
      if (!is.null(values$max_priority)) {
        stop("--max-priority may be supplied only once", call. = FALSE)
      }
      values$max_priority <- value
    } else if (identical(option, "--dependency-library")) {
      if (!is.null(values$dependency_library)) {
        stop("--dependency-library may be supplied only once", call. = FALSE)
      }
      values$dependency_library <- value
    } else if (identical(option, "--run-id")) {
      if (!is.null(values$run_id)) stop("--run-id may be supplied only once", call. = FALSE)
      values$run_id <- value
    } else {
      values$packages <- c(values$packages, value)
    }
    index <- next_index
    matched <- TRUE
    break
  }
  if (!matched) stop("unknown option: ", argument, "\n", usage, call. = FALSE)
}
required <- c("root", "max_priority", "dependency_library", "run_id")
missing <- required[vapply(required, function(name) {
  is.null(values[[name]]) || !nzchar(values[[name]])
}, logical(1L))]
if (length(missing)) {
  stop("missing required options: ", paste(missing, collapse = ", "), call. = FALSE)
}

root <- normalizePath(values$root, winslash = "/", mustWork = TRUE)
started_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
max_priority <- suppressWarnings(as.integer(values$max_priority))
if (length(max_priority) != 1L || is.na(max_priority) || max_priority < 0L ||
    !identical(as.character(max_priority), values$max_priority)) {
  stop("--max-priority must be one non-negative integer", call. = FALSE)
}
run_id <- values$run_id
if (length(run_id) != 1L ||
    !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", run_id) ||
    run_id %in% c(".", "..")) {
  stop("--run-id must be a safe name of at most 128 characters", call. = FALSE)
}
if (anyDuplicated(values$packages) ||
    any(!grepl("^[A-Za-z][A-Za-z0-9.]*$", values$packages))) {
  stop("--package values must be unique valid R package names", call. = FALSE)
}
if (!identical(Sys.getenv("PARADOX_ACTIVE_ROOT", unset = ""), root)) {
  stop("activate the repository-local environment first: . scripts/activate", call. = FALSE)
}
expected_r_home <- file.path(root, ".local", "toolchain", "lib", "R")
expected_rscript <- file.path(root, ".local", "toolchain", "bin", "Rscript")
if (!identical(normalizePath(R.home(), winslash = "/", mustWork = TRUE),
    expected_r_home) || !identical(unname(Sys.which("Rscript")), expected_rscript)) {
  stop("dependency preparation is not running under repository-local R", call. = FALSE)
}

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("could not identify harness", call. = FALSE)
harness_path <- normalizePath(
  substring(script_argument, nchar("--file=") + 1L),
  winslash = "/", mustWork = TRUE
)
if (!identical(harness_path, file.path(root, "compat", basename(harness_path)))) {
  stop("--root does not own this dependency-preparation harness", call. = FALSE)
}

source_fetch_path <- file.path(root, "compat", "source-fetch-common.R")
prep_helper_path <- file.path(root, "compat", "reverse-dependency-prep-common.R")
fingerprint_path <- file.path(root, "compat", "fingerprint.R")
evidence_helper_path <- file.path(root, "compat", "repository-evidence.R")
evidence_verifier_path <- file.path(root, "compat", "verify-repository-evidence.R")
compat_system_path <- file.path(root, "compat", "compat-system-evidence.R")
for (path in c(
    source_fetch_path, prep_helper_path, fingerprint_path, evidence_helper_path,
    evidence_verifier_path, compat_system_path)) {
  info <- file.info(path)
  regular <- suppressWarnings(system2(
    "/usr/bin/test", c("-f", shQuote(path)), stdout = FALSE, stderr = FALSE
  ))
  if (nzchar(Sys.readlink(path)) || !identical(as.integer(regular), 0L) ||
      is.na(info$isdir) || info$isdir) {
    stop("required helper is missing, non-regular, or symbolic: ", path,
      call. = FALSE)
  }
}
sys.source(source_fetch_path, envir = environment(), keep.source = FALSE)
sys.source(prep_helper_path, envir = environment(), keep.source = FALSE)
sys.source(fingerprint_path, envir = environment(), keep.source = FALSE)
sys.source(evidence_helper_path, envir = environment(), keep.source = FALSE)
sys.source(compat_system_path, envir = environment(), keep.source = FALSE)

inventory_path <- file.path(root, "compat", "reverse-dependencies.tsv")
cran_snapshot_path <- file.path(root, "compat", "cran-snapshot.tsv")
bioc_snapshot_path <- file.path(root, "compat", "bioconductor-snapshot.tsv")
input_paths <- c(
  inventory_path, cran_snapshot_path, bioc_snapshot_path, harness_path,
  source_fetch_path, prep_helper_path, fingerprint_path, evidence_helper_path,
  evidence_verifier_path, compat_system_path
)
input_hashes <- compat_fetch_file_hashes(input_paths, "preparation input")
names(input_hashes) <- input_paths

inventory <- compat_fetch_read_tsv(
  inventory_path,
  c("relation", "package", "source", "priority", "notes"),
  "reverse-dependency inventory"
)
cran_snapshot <- compat_fetch_read_tsv(
  cran_snapshot_path,
  c(
    "package", "relation", "source", "priority", "notes", "version",
    "repository", "archive", "md5", "sha256"
  ),
  "CRAN source snapshot"
)
bioc_snapshot <- compat_fetch_read_tsv(
  bioc_snapshot_path,
  c("package", "version", "repository", "archive", "sha256"),
  "Bioconductor source snapshot"
)
if (anyDuplicated(inventory$package) || anyDuplicated(cran_snapshot$package) ||
    anyDuplicated(bioc_snapshot$package)) {
  stop("reverse-dependency manifests contain duplicate packages", call. = FALSE)
}
priority <- suppressWarnings(as.integer(inventory$priority))
if (anyNA(priority) || !identical(as.character(priority), inventory$priority) ||
    any(priority < 0L) ||
    !all(inventory$source %in% c("CRAN", "Bioconductor"))) {
  stop("reverse-dependency inventory has invalid source or priority values",
    call. = FALSE)
}
inventory$priority <- priority
if (!setequal(inventory$package[inventory$source == "CRAN"],
      cran_snapshot$package) ||
    !setequal(inventory$package[inventory$source == "Bioconductor"],
      bioc_snapshot$package)) {
  stop("inventory and source snapshot package sets disagree", call. = FALSE)
}
cran_inventory <- inventory[
  match(cran_snapshot$package, inventory$package), , drop = FALSE
]
for (field in c("relation", "source", "priority", "notes")) {
  if (!identical(as.character(cran_inventory[[field]]), cran_snapshot[[field]])) {
    stop("CRAN snapshot disagrees with inventory field ", field, call. = FALSE)
  }
}

selected <- inventory[inventory$priority <= max_priority, , drop = FALSE]
if (length(values$packages)) {
  unknown <- setdiff(values$packages, selected$package)
  if (length(unknown)) {
    stop(
      "selected package is absent at this priority: ",
      paste(unknown, collapse = ", "), call. = FALSE
    )
  }
  selected <- selected[match(values$packages, selected$package), , drop = FALSE]
}
if (!nrow(selected)) stop("priority selection is empty", call. = FALSE)

is_symbolic <- function(path) {
  link <- Sys.readlink(path)
  length(link) == 1L && !is.na(link) && nzchar(link)
}
require_plain_local_directory <- function(path, label) {
  path <- sub("/+$", "", path)
  local_root <- file.path(root, ".local")
  if (!identical(path, local_root) &&
      !startsWith(path, paste0(local_root, "/"))) {
    stop(label, " must be an absolute path below ", local_root, call. = FALSE)
  }
  relative <- substring(path, nchar(root) + 2L)
  components <- strsplit(relative, "/", fixed = TRUE)[[1L]]
  if (any(!nzchar(components) | components %in% c(".", ".."))) {
    stop(label, " has a non-canonical path component", call. = FALSE)
  }
  current <- root
  for (component in components) {
    current <- file.path(current, component)
    if (!dir.exists(current) || is_symbolic(current)) {
      stop(label, " has a missing, non-directory, or symbolic component: ",
        current, call. = FALSE)
    }
  }
  normalized <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!identical(normalized, path)) {
    stop(label, " must use its canonical absolute path", call. = FALSE)
  }
  normalized
}
source_directories <- c(
  CRAN = require_plain_local_directory(
    file.path(root, ".local", "compat", "cran-sources"),
    "CRAN source directory"
  ),
  Bioconductor = require_plain_local_directory(
    file.path(root, ".local", "compat", "bioconductor-sources"),
    "Bioconductor source directory"
  )
)

source_plan_rows <- vector("list", nrow(selected))
for (row_index in seq_len(nrow(selected))) {
  inventory_row <- selected[row_index, , drop = FALSE]
  source <- inventory_row$source[[1L]]
  snapshot <- if (identical(source, "CRAN")) cran_snapshot else bioc_snapshot
  snapshot_index <- which(snapshot$package == inventory_row$package[[1L]])
  if (length(snapshot_index) != 1L) {
    stop("snapshot does not contain one row for ", inventory_row$package[[1L]],
      call. = FALSE)
  }
  snapshot_row <- snapshot[snapshot_index, , drop = FALSE]
  package <- inventory_row$package[[1L]]
  version <- snapshot_row$version[[1L]]
  expected_repository <- if (identical(source, "CRAN")) {
    "https://cloud.r-project.org/src/contrib"
  } else {
    "https://bioconductor.org/packages/3.23/bioc/src/contrib"
  }
  if (!grepl("^[A-Za-z][A-Za-z0-9.]*$", package) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9.+-]*$", version) ||
      !identical(snapshot_row$repository[[1L]], expected_repository) ||
      !identical(snapshot_row$archive[[1L]],
        paste0(package, "_", version, ".tar.gz"))) {
    stop("snapshot contains unsafe or inconsistent metadata for ", package,
      call. = FALSE)
  }
  verify_row <- snapshot_row
  if (!"md5" %in% names(verify_row)) verify_row$md5 <- ""
  archive <- file.path(source_directories[[source]], snapshot_row$archive[[1L]])
  compat_fetch_verify_archive(archive, verify_row)
  source_plan_rows[[row_index]] <- data.frame(
    package = package,
    source = source,
    relation = inventory_row$relation[[1L]],
    priority = inventory_row$priority[[1L]],
    version = version,
    repository = snapshot_row$repository[[1L]],
    archive = archive,
    archive_name = snapshot_row$archive[[1L]],
    archive_md5 = if (identical(source, "CRAN")) {
      tolower(snapshot_row$md5[[1L]])
    } else {
      "-"
    },
    archive_sha256 = tolower(snapshot_row$sha256[[1L]]),
    stringsAsFactors = FALSE
  )
}
source_plan <- do.call(rbind, source_plan_rows)

local_root <- require_plain_local_directory(file.path(root, ".local"),
  "repository-local state root")
compat_root <- require_plain_local_directory(file.path(local_root, "compat"),
  "compatibility state root")
runs_root <- file.path(compat_root, "runs")
if (!dir.exists(runs_root)) {
  if (file.exists(runs_root) || is_symbolic(runs_root) ||
      !dir.create(runs_root, recursive = FALSE, mode = "0700")) {
    stop("could not create retained compatibility root", call. = FALSE)
  }
}
runs_root <- require_plain_local_directory(runs_root,
  "retained compatibility root")
run_directory <- file.path(runs_root, run_id)
if (file.exists(run_directory) || dir.exists(run_directory) ||
    is_symbolic(run_directory) ||
    !dir.create(run_directory, recursive = FALSE, mode = "0700")) {
  stop("run directory already exists: ", run_directory, call. = FALSE)
}
stage_name <- sprintf("reverse-dependency-dependencies-priority-%d", max_priority)
stage_directory <- file.path(run_directory, stage_name)
if (!dir.create(stage_directory, recursive = FALSE, mode = "0700")) {
  stop("could not create reverse-dependency preparation stage", call. = FALSE)
}
metadata_directory <- file.path(stage_directory, "metadata")
resolver_root <- file.path(stage_directory, "resolver-inputs")
logs_directory <- file.path(stage_directory, "logs")
retained_sources_directory <- file.path(stage_directory, "resolved-sources")
for (path in c(
    metadata_directory, resolver_root, logs_directory,
    retained_sources_directory)) {
  if (!dir.create(path, recursive = FALSE, mode = "0700")) {
    stop("could not create evidence directory: ", path, call. = FALSE)
  }
}

write_tsv <- function(value, path) {
  temporary <- paste0(path, ".new")
  if (file.exists(path) || dir.exists(path) || is_symbolic(path) ||
      file.exists(temporary) || dir.exists(temporary) || is_symbolic(temporary)) {
    stop("evidence output path already exists: ", path, call. = FALSE)
  }
  on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)
  utils::write.table(
    value, temporary, quote = FALSE, sep = "\t", row.names = FALSE,
    na = "-", fileEncoding = "UTF-8"
  )
  if (!file.rename(temporary, path)) {
    stop("could not atomically write evidence: ", path, call. = FALSE)
  }
  invisible(path)
}
write_tsv(source_plan, file.path(stage_directory, "source-plan.tsv"))

copied <- file.copy(
  input_paths, metadata_directory, copy.mode = TRUE, copy.date = TRUE
)
if (!all(copied)) stop("could not retain preparation inputs", call. = FALSE)
retained_inputs <- file.path(metadata_directory, basename(input_paths))
if (!identical(unname(tools::sha256sum(retained_inputs)),
    unname(input_hashes))) {
  stop("retained preparation inputs differ from authenticated inputs",
    call. = FALSE)
}
compat_system_evidence <- compat_system_capture_evidence(root, metadata_directory)

parsed_targets <- vector("list", nrow(source_plan))
synthetic_packages <- sprintf("paradoxrevdep%03d", seq_len(nrow(source_plan)))
synthetic_lock_packages <- paste0(synthetic_packages, "-deps")
for (row_index in seq_len(nrow(source_plan))) {
  package <- source_plan$package[[row_index]]
  resolver_directory <- file.path(
    resolver_root, sprintf("%03d-%s", row_index, package)
  )
  if (!dir.create(resolver_directory, recursive = FALSE, mode = "0700")) {
    stop("could not create resolver input for ", package, call. = FALSE)
  }
  description_member <- paste0(package, "/DESCRIPTION")
  extraction <- file.path(resolver_directory, "extracted")
  if (!dir.create(extraction, recursive = FALSE, mode = "0700")) {
    stop("could not create DESCRIPTION extraction directory", call. = FALSE)
  }
  utils::untar(
    source_plan$archive[[row_index]], files = description_member,
    exdir = extraction, tar = "internal"
  )
  extracted_description <- file.path(extraction, description_member)
  parsed <- reverse_prep_description_dependencies(extracted_description, package)
  if (!identical(parsed$version, source_plan$version[[row_index]])) {
    stop("extracted DESCRIPTION version disagrees with snapshot for ", package,
      call. = FALSE)
  }
  retained_description <- file.path(resolver_directory, "DESCRIPTION.original")
  if (!file.copy(extracted_description, retained_description, copy.mode = TRUE)) {
    stop("could not retain extracted DESCRIPTION for ", package, call. = FALSE)
  }
  unlink(extraction, recursive = TRUE, force = FALSE)
  if (file.exists(extraction) || dir.exists(extraction) || is_symbolic(extraction)) {
    stop("could not remove transient DESCRIPTION extraction", call. = FALSE)
  }
  reverse_prep_write_resolver_description(
    parsed, synthetic_packages[[row_index]],
    file.path(resolver_directory, "DESCRIPTION")
  )
  parsed_targets[[row_index]] <- parsed
}
direct_dependencies <- do.call(rbind, lapply(parsed_targets, `[[`, "dependencies"))
direct_dependencies$exclusion_reason <- ifelse(
  direct_dependencies$excluded, "candidate_paradox", "-"
)
write_tsv(direct_dependencies,
  file.path(stage_directory, "direct-hard-dependencies.tsv"))

library <- sub("/+$", "", values$dependency_library)
if (!startsWith(library, paste0(local_root, "/")) ||
    grepl("(^|/)\\.\\.?(/|$)", substring(library, nchar(root) + 2L))) {
  stop("dependency library must be a canonical absolute path below .local",
    call. = FALSE)
}
if (dir.exists(library)) {
  library <- require_plain_local_directory(library, "dependency library")
} else {
  if (file.exists(library) || is_symbolic(library)) {
    stop("dependency library exists but is not a plain directory", call. = FALSE)
  }
  parent <- require_plain_local_directory(dirname(library),
    "dependency library parent")
  if (!identical(file.path(parent, basename(library)), library) ||
      !dir.create(library, recursive = FALSE, mode = "0700")) {
    stop("could not create dependency library", call. = FALSE)
  }
  library <- require_plain_local_directory(library, "dependency library")
}

mutation_lock <- file.path(compat_root, ".reverse-dependency-preparation.lock")
if (file.exists(mutation_lock) || dir.exists(mutation_lock) ||
    is_symbolic(mutation_lock) ||
    !dir.create(mutation_lock, recursive = FALSE, mode = "0700")) {
  stop("reverse-dependency dependency preparation is already running or stale",
    call. = FALSE)
}
lock_active <- TRUE
on.exit({
  if (lock_active && dir.exists(mutation_lock) && !is_symbolic(mutation_lock) &&
      !length(list.files(mutation_lock, all.files = TRUE, no.. = TRUE))) {
    unlink(mutation_lock, recursive = FALSE, force = FALSE)
  }
}, add = TRUE)

package_state <- function(package) {
  path <- file.path(library, package)
  if (!file.exists(path) && !dir.exists(path) && !is_symbolic(path)) return("absent")
  if (!dir.exists(path) || is_symbolic(path)) {
    stop("protected package path is not a plain directory: ", path, call. = FALSE)
  }
  paste0("sha256:", compat_tree_content_sha256(path))
}
library_content_before <- compat_tree_content_sha256(library)
paradox_before <- package_state("paradox")
installed_before <- reverse_prep_installed_snapshot(library)
write_tsv(installed_before, file.path(metadata_directory, "installed-before.tsv"))

pak_path <- normalizePath(find.package("pak"), winslash = "/", mustWork = TRUE)
jsonlite_path <- normalizePath(find.package("jsonlite"), winslash = "/", mustWork = TRUE)
project_library <- file.path(root, ".local", "R", "library")
if (!identical(dirname(pak_path), project_library) ||
    !identical(dirname(jsonlite_path), project_library)) {
  stop("pak and jsonlite must resolve from the repository-local project library",
    call. = FALSE)
}

run_logged <- function(path, expression) {
  connection <- file(path, open = "wt", encoding = "UTF-8")
  sink(connection, type = "output")
  sink(connection, type = "message")
  on.exit({
    sink(type = "message")
    sink(type = "output")
    close(connection)
  }, add = TRUE)
  force(expression)
}

resolver_references <- paste0(
  "deps::", file.path(
    resolver_root,
    sprintf("%03d-%s", seq_len(nrow(source_plan)), source_plan$package)
  )
)
resolved_lockfile <- file.path(stage_directory, "pak-resolved.lock")
lockfile <- file.path(stage_directory, "pak.lock")
old_options <- options(
  repos = c(
    CRAN = "https://cloud.r-project.org",
    BioCsoft = "https://bioconductor.org/packages/3.23/bioc"
  ),
  pkgType = "source"
)
on.exit(options(old_options), add = TRUE)
run_logged(file.path(logs_directory, "pak-resolve.log"), {
  pak::lockfile_create(
    pkg = resolver_references,
    lockfile = resolved_lockfile,
    lib = library,
    upgrade = FALSE,
    dependencies = NA
  )
})
resolved_lock_sha256 <- unname(tools::sha256sum(resolved_lockfile))
enriched_sources <- run_logged(file.path(logs_directory, "source-authentication.log"), {
  reverse_prep_enrich_lockfile(
    resolved_lockfile, lockfile, retained_sources_directory
  )
})
write_tsv(enriched_sources,
  file.path(stage_directory, "enriched-source-checksums.tsv"))
base_packages <- rownames(utils::installed.packages(
  lib.loc = .Library, priority = c("base", "recommended"), noCache = TRUE
))
lock_plan <- reverse_prep_lock_plan(
  lockfile, synthetic_lock_packages, source_plan$package, library,
  direct_dependencies, base_packages, retained_sources_directory
)
write_tsv(lock_plan, file.path(stage_directory, "lock-plan.tsv"))
lock_sha256 <- unname(tools::sha256sum(lockfile))

if (values$plan_only) {
  library_content_after <- compat_tree_content_sha256(library)
  if (!identical(library_content_after, library_content_before)) {
    stop("pak resolution changed the dependency library in plan-only mode",
      call. = FALSE)
  }
} else {
  run_logged(file.path(logs_directory, "pak-install.log"), {
    pak::lockfile_install(lockfile = lockfile, lib = library, update = FALSE)
  })
  if (!identical(unname(tools::sha256sum(lockfile)), lock_sha256)) {
    stop("pak lockfile changed during installation", call. = FALSE)
  }
  reverse_prep_lock_plan(
    lockfile, synthetic_lock_packages, source_plan$package, library,
    direct_dependencies, base_packages, retained_sources_directory
  )
  observed <- reverse_prep_installed_snapshot(library)
  expected <- lock_plan[!lock_plan$type %in% "deps", c("package", "version"),
    drop = FALSE]
  expected <- expected[!expected$package %in% base_packages, , drop = FALSE]
  observed_index <- match(expected$package, observed$package)
  if (anyNA(observed_index) ||
      !identical(expected$version, observed$version[observed_index])) {
    stop("installed dependency library disagrees with the authenticated lock",
      call. = FALSE)
  }
  if (any(c(synthetic_packages, synthetic_lock_packages) %in% observed$package)) {
    stop("synthetic resolver package was installed", call. = FALSE)
  }
  library_content_after <- compat_tree_content_sha256(library)
}

paradox_after <- package_state("paradox")
if (!identical(paradox_after, paradox_before)) {
  stop("paradox was installed, removed, or changed during dependency preparation",
    call. = FALSE)
}
installed_after <- reverse_prep_installed_snapshot(library)
write_tsv(installed_after, file.path(metadata_directory, "installed-after.tsv"))

for (row_index in seq_len(nrow(source_plan))) {
  source <- source_plan$source[[row_index]]
  snapshot <- if (identical(source, "CRAN")) cran_snapshot else bioc_snapshot
  snapshot_row <- snapshot[
    snapshot$package == source_plan$package[[row_index]], , drop = FALSE
  ]
  if (!"md5" %in% names(snapshot_row)) snapshot_row$md5 <- ""
  compat_fetch_verify_archive(source_plan$archive[[row_index]], snapshot_row)
}
final_input_hashes <- compat_fetch_file_hashes(input_paths, "preparation input")
if (!identical(unname(final_input_hashes), unname(input_hashes))) {
  stop("preparation inputs changed during dependency preparation", call. = FALSE)
}
if (!identical(unname(tools::sha256sum(retained_inputs)),
    unname(input_hashes))) {
  stop("retained preparation inputs changed", call. = FALSE)
}
compat_system_verify_evidence(
  compat_system_evidence, "during reverse-dependency dependency completion"
)
final_lock_plan <- reverse_prep_lock_plan(
  lockfile, synthetic_lock_packages, source_plan$package, library,
  direct_dependencies, base_packages, retained_sources_directory
)
if (!identical(final_lock_plan, lock_plan)) {
  stop("authenticated pak lock plan changed before evidence completion",
    call. = FALSE)
}
if (!identical(unname(tools::sha256sum(resolved_lockfile)),
      resolved_lock_sha256) ||
    !identical(unname(tools::sha256sum(lockfile)), lock_sha256)) {
  stop("pak lockfile changed before evidence completion", call. = FALSE)
}
library_content_final <- compat_tree_content_sha256(library)
paradox_final <- package_state("paradox")
if (!identical(library_content_final, library_content_after) ||
    !identical(paradox_final, paradox_after)) {
  stop("dependency library changed before evidence completion", call. = FALSE)
}

run_metadata <- data.frame(
  field = c(
    "schema", "stage_kind", "run_id", "started_utc", "root", "max_priority", "plan_only",
    "selected_packages", "dependency_library", "dependency_library_content_before",
    "r", "r_version", "pak_version", "pak_content_sha256", "jsonlite_version",
    "jsonlite_content_sha256", "inventory_sha256", "cran_snapshot_sha256",
    "bioconductor_snapshot_sha256", "harness_sha256", "prep_helper_sha256",
    "source_fetch_helper_sha256", "fingerprint_sha256", "evidence_helper_sha256",
    "evidence_verifier_sha256", "pak_resolved_lock_sha256", "pak_lock_sha256",
    "paradox_state_before"
  ),
  value = c(
    "1", "reverse_dependency_dependencies", run_id, started_utc, root,
    as.character(max_priority), as.character(values$plan_only),
    paste(source_plan$package, collapse = ","), library, library_content_before,
    normalizePath(file.path(R.home(), "bin", "R"), winslash = "/", mustWork = TRUE),
    as.character(getRversion()), as.character(utils::packageVersion("pak")),
    compat_tree_content_sha256(pak_path),
    as.character(utils::packageVersion("jsonlite")),
    compat_tree_content_sha256(jsonlite_path), input_hashes[[inventory_path]],
    input_hashes[[cran_snapshot_path]], input_hashes[[bioc_snapshot_path]],
    input_hashes[[harness_path]], input_hashes[[prep_helper_path]],
    input_hashes[[source_fetch_path]], input_hashes[[fingerprint_path]],
    input_hashes[[evidence_helper_path]], input_hashes[[evidence_verifier_path]],
    resolved_lock_sha256, lock_sha256, paradox_before
  ),
  stringsAsFactors = FALSE
)
write_tsv(run_metadata, file.path(metadata_directory, "run.tsv"))
completion <- data.frame(
  field = c(
    "schema", "stage_kind", "run_id", "status", "finished_utc",
    "selected_packages", "direct_dependency_rows", "resolved_package_rows",
    "dependency_library_content_after", "paradox_state_after",
    "source_plan_sha256", "direct_dependencies_sha256", "lock_plan_sha256",
    "pak_resolved_lock_sha256", "pak_lock_sha256",
    "enriched_source_checksums_sha256", "installed_before_sha256",
    "installed_after_sha256"
  ),
  value = c(
    "1", "reverse_dependency_dependencies", run_id,
    if (values$plan_only) "planned" else "passed",
    format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    as.character(nrow(source_plan)), as.character(nrow(direct_dependencies)),
    as.character(nrow(lock_plan)), library_content_after, paradox_after,
    unname(tools::sha256sum(file.path(stage_directory, "source-plan.tsv"))),
    unname(tools::sha256sum(file.path(
      stage_directory, "direct-hard-dependencies.tsv"
    ))),
    unname(tools::sha256sum(file.path(stage_directory, "lock-plan.tsv"))),
    resolved_lock_sha256, lock_sha256,
    unname(tools::sha256sum(file.path(
      stage_directory, "enriched-source-checksums.tsv"
    ))),
    unname(tools::sha256sum(file.path(metadata_directory, "installed-before.tsv"))),
    unname(tools::sha256sum(file.path(metadata_directory, "installed-after.tsv")))
  ),
  stringsAsFactors = FALSE
)
write_tsv(completion, file.path(metadata_directory, "completion.tsv"))
if (!unlink(mutation_lock, recursive = FALSE, force = FALSE) ||
    file.exists(mutation_lock) || dir.exists(mutation_lock) ||
    is_symbolic(mutation_lock)) {
  stop("could not release reverse-dependency preparation lock", call. = FALSE)
}
lock_active <- FALSE
repository_seal_evidence(stage_directory)
cat(
  if (values$plan_only) "Resolved" else "Installed",
  " hard dependencies for ", nrow(source_plan), " reverse package",
  if (nrow(source_plan) == 1L) "" else "s", ": ", stage_directory, "\n",
  sep = ""
)
