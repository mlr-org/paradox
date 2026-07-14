#!/usr/bin/env Rscript

command <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command, value = TRUE)
if (length(file_argument) != 1L) stop("could not identify fixture test", call. = FALSE)
script <- normalizePath(
  sub("^--file=", "", file_argument), winslash = "/", mustWork = TRUE
)
root <- normalizePath(file.path(dirname(script), "..", ".."),
  winslash = "/", mustWork = TRUE)
if (!identical(Sys.getenv("PARADOX_ACTIVE_ROOT", unset = ""), root) ||
    !identical(
      normalizePath(R.home(), winslash = "/", mustWork = TRUE),
      file.path(root, ".local", "toolchain", "lib", "R")
    )) {
  stop("activate the repository-local environment first: . scripts/activate",
    call. = FALSE)
}

sys.source(file.path(root, "compat", "source-fetch-common.R"),
  envir = environment(), keep.source = FALSE)
sys.source(file.path(root, "compat", "reverse-dependency-prep-common.R"),
  envir = environment(), keep.source = FALSE)
sys.source(file.path(root, "compat", "repository-evidence.R"),
  envir = environment(), keep.source = FALSE)

test_root <- tempfile("reverse-dependency-preparation-",
  tmpdir = file.path(root, ".local", "tmp"))
if (!dir.create(test_root, recursive = FALSE, mode = "0700")) {
  stop("could not create fixture root", call. = FALSE)
}
on.exit(unlink(test_root, recursive = TRUE, force = TRUE), add = TRUE)

expect_failure <- function(expression, pattern = NULL) {
  condition <- tryCatch({
    force(expression)
    NULL
  }, error = identity)
  if (is.null(condition)) stop("fixture unexpectedly succeeded", call. = FALSE)
  if (!is.null(pattern) && !grepl(pattern, conditionMessage(condition))) {
    stop(
      "fixture failed for an unexpected reason (expected ", pattern, "): ",
      conditionMessage(condition),
      call. = FALSE
    )
  }
  invisible(condition)
}

description <- file.path(test_root, "DESCRIPTION")
writeLines(c(
  "Package: Target",
  "Version: 1.2.3",
  "Title: Fixture",
  "Description: Fixture.",
  "License: MIT",
  "Depends: R (>= 4.3), paradox (>= 1.0.0), methods",
  "Imports: hardpkg (>= 2.0), utility.pkg",
  "LinkingTo: linkpkg (== 1.1)",
  "Suggests: optionalpkg"
), description, useBytes = TRUE)
parsed <- reverse_prep_description_dependencies(description, "Target")
stopifnot(
  identical(parsed$version, "1.2.3"),
  identical(
    parsed$dependencies$dependency,
    c("R", "paradox", "methods", "hardpkg", "utility.pkg", "linkpkg")
  ),
  identical(parsed$dependencies$excluded,
    c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)),
  !"optionalpkg" %in% parsed$dependencies$dependency
)

resolver <- file.path(test_root, "resolver")
dir.create(resolver, mode = "0700")
resolver_description <- file.path(resolver, "DESCRIPTION")
reverse_prep_write_resolver_description(
  parsed, "paradoxrevdep001", resolver_description
)
resolver_fields <- read.dcf(resolver_description)
resolver_payload <- paste(resolver_fields, collapse = "\n")
resolver_dependencies <- paste(
  resolver_fields[1L, intersect(reverse_prep_hard_fields, colnames(resolver_fields))],
  collapse = "\n"
)
stopifnot(
  identical(resolver_fields[[1L, "Package"]], "paradoxrevdep001"),
  !grepl("paradox", resolver_dependencies, fixed = TRUE),
  !grepl("optionalpkg", resolver_payload, fixed = TRUE),
  grepl("hardpkg (>= 2.0)", resolver_payload, fixed = TRUE),
  grepl("linkpkg (== 1.1)", resolver_payload, fixed = TRUE)
)

empty_description <- file.path(test_root, "DESCRIPTION-empty")
writeLines(c(
  "Package: EmptyTarget", "Version: 1.0", "Title: Fixture",
  "Description: Fixture.", "License: MIT"
), empty_description)
empty_parsed <- reverse_prep_description_dependencies(
  empty_description, "EmptyTarget"
)
stopifnot(
  !nrow(empty_parsed$dependencies),
  identical(names(empty_parsed$dependencies), names(parsed$dependencies))
)

malformed <- file.path(test_root, "DESCRIPTION-malformed")
writeLines(c(
  "Package: Malformed", "Version: 1.0", "Title: Fixture",
  "Description: Fixture.", "License: MIT", "Imports: badpkg (!= 1.0)"
), malformed)
expect_failure(
  reverse_prep_description_dependencies(malformed, "Malformed"),
  "unsupported or malformed"
)
self_dependency <- file.path(test_root, "DESCRIPTION-self")
writeLines(c(
  "Package: Recursive", "Version: 1.0", "Title: Fixture",
  "Description: Fixture.", "License: MIT", "Imports: Recursive"
), self_dependency)
expect_failure(
  reverse_prep_description_dependencies(self_dependency, "Recursive"),
  "declares itself"
)
description_link <- file.path(test_root, "DESCRIPTION-link")
file.symlink(description, description_link)
expect_failure(
  reverse_prep_description_dependencies(description_link, "Target"),
  "non-regular.*symbolic"
)

library <- file.path(test_root, "library")
dir.create(library, mode = "0700")
dir.create(file.path(library, "linkpkg"), mode = "0700")
dir.create(file.path(library, "paradox"), mode = "0700")
hard_types <- as.list(reverse_prep_hard_fields)
valid_lock <- list(
  lockfile_version = 1L,
  os = "fixture",
  r_version = "fixture",
  platform = "fixture",
  packages = list(
    list(
      ref = paste0("deps::", resolver), package = "paradoxrevdep001-deps",
      version = "1.0.0", type = "deps", direct = TRUE,
      dep_types = hard_types
    ),
    list(
      ref = "hardpkg", package = "hardpkg", version = "2.1.0",
      type = "standard", direct = FALSE,
      dep_types = hard_types,
      sha256 = paste(rep.int("a", 64L), collapse = ""),
      sources = list("https://example.invalid/hardpkg_2.1.0.tar.gz")
    ),
    list(
      ref = paste0("installed::", file.path(library, "linkpkg")),
      package = "linkpkg", version = "1.1", type = "installed",
      direct = FALSE, dep_types = hard_types
    ),
    list(
      ref = "utility.pkg", package = "utility.pkg", version = "1.0",
      type = "standard", direct = FALSE, dep_types = hard_types,
      sha256 = paste(rep.int("b", 64L), collapse = ""),
      sources = list("https://example.invalid/utility.pkg_1.0.tar.gz")
    )
  )
)
lock_path <- file.path(test_root, "pak.lock")
write_lock <- function(value, path = lock_path) {
  jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE)
}
write_lock(valid_lock)
lock_plan <- reverse_prep_lock_plan(
  lock_path, "paradoxrevdep001-deps", "Target", library,
  parsed$dependencies, base_packages = c("methods")
)
stopifnot(
  identical(lock_plan$package,
    c("paradoxrevdep001-deps", "hardpkg", "linkpkg", "utility.pkg")),
  !any(lock_plan$selected_reverse_target),
  !"paradox" %in% lock_plan$package
)

installed_paradox_lock <- valid_lock
installed_paradox_lock$packages[[length(installed_paradox_lock$packages) + 1L]] <-
  list(
    ref = paste0("installed::", file.path(library, "paradox")),
    package = "paradox", version = "1.0.1", type = "installed",
    direct = FALSE, dep_types = hard_types
  )
write_lock(installed_paradox_lock)
installed_paradox_plan <- reverse_prep_lock_plan(
  lock_path, "paradoxrevdep001-deps", "Target", library,
  parsed$dependencies, base_packages = c("methods")
)
stopifnot(
  identical(installed_paradox_plan$type[
    installed_paradox_plan$package == "paradox"
  ], "installed")
)

tampered <- valid_lock
tampered$packages[[2L]]$package <- "paradox"
write_lock(tampered)
expect_failure(
  reverse_prep_lock_plan(
    lock_path, "paradoxrevdep001-deps", "Target", library,
    parsed$dependencies, c("methods")
  ),
  "paradox"
)
tampered <- valid_lock
tampered$packages[[3L]]$ref <- paste0("installed::", file.path(test_root, "escape"))
write_lock(tampered)
expect_failure(
  reverse_prep_lock_plan(
    lock_path, "paradoxrevdep001-deps", "Target", library,
    parsed$dependencies, c("methods")
  ),
  "outside the dependency library"
)
tampered <- valid_lock
tampered$packages[[2L]]$sha256 <- "not-a-sha256"
write_lock(tampered)
expect_failure(
  reverse_prep_lock_plan(
    lock_path, "paradoxrevdep001-deps", "Target", library,
    parsed$dependencies, c("methods")
  ),
  "lacks authenticated source metadata"
)
tampered <- valid_lock
tampered$packages[[2L]]$version <- "1.9.9"
write_lock(tampered)
expect_failure(
  reverse_prep_lock_plan(
    lock_path, "paradoxrevdep001-deps", "Target", library,
    parsed$dependencies, c("methods")
  ),
  "violates the hard dependency constraint"
)
tampered <- valid_lock
tampered$packages <- tampered$packages[-4L]
write_lock(tampered)
expect_failure(
  reverse_prep_lock_plan(
    lock_path, "paradoxrevdep001-deps", "Target", library,
    parsed$dependencies, c("methods")
  ),
  "omitted hard dependencies"
)
tampered <- valid_lock
tampered$packages[[2L]]$direct <- TRUE
write_lock(tampered)
expect_failure(
  reverse_prep_lock_plan(
    lock_path, "paradoxrevdep001-deps", "Target", library,
    parsed$dependencies, c("methods")
  ),
  "unexpected root"
)
tampered <- valid_lock
tampered$packages[[2L]]$dep_types <- c(hard_types, list("Suggests"))
write_lock(tampered)
expect_failure(
  reverse_prep_lock_plan(
    lock_path, "paradoxrevdep001-deps", "Target", library,
    parsed$dependencies, c("methods")
  ),
  "non-hard dependency policy"
)
write_lock(valid_lock)

package_tree <- file.path(test_root, "ArchiveTarget")
dir.create(package_tree, mode = "0700")
writeLines(c(
  "Package: ArchiveTarget", "Version: 1.0.0", "Title: Fixture",
  "Description: Fixture.", "License: MIT", "Imports: hardpkg"
), file.path(package_tree, "DESCRIPTION"), useBytes = TRUE)
archive <- file.path(test_root, "ArchiveTarget_1.0.0.tar.gz")
old <- setwd(test_root)
on.exit(setwd(old), add = TRUE)
utils::tar(
  archive, files = "ArchiveTarget", compression = "gzip", tar = "internal"
)
archive_row <- data.frame(
  package = "ArchiveTarget", version = "1.0.0",
  md5 = unname(tools::md5sum(archive)),
  sha256 = unname(tools::sha256sum(archive)), stringsAsFactors = FALSE
)
compat_fetch_verify_archive(archive, archive_row)

bioc_lock <- list(
  lockfile_version = 1L,
  os = "fixture",
  r_version = "fixture",
  platform = "fixture",
  packages = list(
    list(
      ref = paste0("deps::", resolver), package = "biocfixture-deps",
      version = "1.0", type = "deps", direct = TRUE,
      dep_types = hard_types
    ),
    list(
      ref = "ArchiveTarget", package = "ArchiveTarget", version = "1.0.0",
      type = "standard", direct = FALSE, repotype = "bioc",
      dep_types = hard_types,
      sources = list(
        "https://bioconductor.example.invalid/ArchiveTarget_1.0.0.tar.gz"
      )
    )
  )
)
bioc_input <- file.path(test_root, "bioc-resolved.lock")
bioc_output <- file.path(test_root, "bioc.lock")
bioc_sources <- file.path(test_root, "bioc-sources")
dir.create(bioc_sources, mode = "0700")
write_lock(bioc_lock, bioc_input)
fixture_download <- function(url, destination, ...) {
  stopifnot(startsWith(url, "https://"))
  if (!file.copy(archive, destination)) stop("fixture copy failed")
  0L
}
enriched <- reverse_prep_enrich_lockfile(
  bioc_input, bioc_output, bioc_sources, download = fixture_download
)
enriched_lock <- reverse_prep_read_lockfile(bioc_output)
enriched_row <- enriched_lock$packages[[2L]]
stopifnot(
  identical(enriched$package, "ArchiveTarget"),
  identical(enriched$sha256, archive_row$sha256),
  identical(enriched_row$sha256, archive_row$sha256),
  startsWith(unlist(enriched_row$sources)[[1L]], "file://"),
  identical(
    unname(tools::sha256sum(enriched$retained_archive)),
    archive_row$sha256
  )
)
bioc_dependencies <- reverse_prep_parse_dependency_field(
  "ArchiveTarget", "Imports", "BiocRoot"
)
reverse_prep_lock_plan(
  bioc_output, "biocfixture-deps", "BiocRoot", library,
  bioc_dependencies, character(), bioc_sources
)

bad_bioc_lock <- bioc_lock
bad_bioc_lock$packages[[2L]]$sources <- list(
  "http://insecure.invalid/ArchiveTarget_1.0.0.tar.gz"
)
bad_bioc_input <- file.path(test_root, "bioc-bad.lock")
bad_bioc_output <- file.path(test_root, "bioc-bad-enriched.lock")
bad_bioc_sources <- file.path(test_root, "bioc-bad-sources")
dir.create(bad_bioc_sources, mode = "0700")
write_lock(bad_bioc_lock, bad_bioc_input)
expect_failure(
  reverse_prep_enrich_lockfile(
    bad_bioc_input, bad_bioc_output, bad_bioc_sources,
    download = fixture_download
  ),
  "lacks an exact HTTPS source"
)

connection <- file(archive, open = "ab")
writeBin(charToRaw("tamper"), connection)
close(connection)
expect_failure(
  compat_fetch_verify_archive(archive, archive_row),
  "checksum mismatch"
)

manifest <- file.path(test_root, "manifest.tsv")
writeLines(c("package\tversion", "ArchiveTarget\t1.0.0"), manifest,
  useBytes = TRUE)
manifest_hash <- compat_fetch_file_hashes(manifest, "fixture manifest")
parsed_manifest <- compat_fetch_read_tsv(
  manifest, c("package", "version"), "fixture manifest"
)
stopifnot(identical(parsed_manifest$package, "ArchiveTarget"))
writeLines(c("package\tversion", "ArchiveTarget\t2.0.0"), manifest,
  useBytes = TRUE)
stopifnot(!identical(
  manifest_hash, compat_fetch_file_hashes(manifest, "fixture manifest")
))

stage <- file.path(test_root, "evidence")
dir.create(stage, mode = "0700")
dir.create(file.path(stage, "metadata"), mode = "0700")
writeLines("fixture", file.path(stage, "result.txt"), useBytes = TRUE)
repository_seal_evidence(stage)
repository_verify_evidence(stage)
writeLines("tampered", file.path(stage, "result.txt"), useBytes = TRUE)
expect_failure(repository_verify_evidence(stage), "do not match")

stage_link <- file.path(test_root, "evidence-link")
dir.create(stage_link, mode = "0700")
dir.create(file.path(stage_link, "metadata"), mode = "0700")
writeLines("fixture", file.path(stage_link, "result.txt"), useBytes = TRUE)
file.symlink(file.path(test_root, "outside"), file.path(stage_link, "escape"))
expect_failure(repository_seal_evidence(stage_link), "symbolic path")

harness <- readLines(
  file.path(root, "compat", "install-reverse-dependency-dependencies.R"),
  warn = FALSE
)
required_fragments <- c(
  "dependencies = NA", "update = FALSE", "compat_fetch_verify_archive",
  "reverse_prep_enrich_lockfile", "resolved-sources",
  "paradox was installed, removed, or changed", "repository_seal_evidence",
  "compat_system_verify_evidence"
)
for (fragment in required_fragments) {
  if (!any(grepl(fragment, harness, fixed = TRUE))) {
    stop("preparation harness lacks required policy: ", fragment, call. = FALSE)
  }
}

cat("reverse-dependency preparation fixtures: PASS\n")
