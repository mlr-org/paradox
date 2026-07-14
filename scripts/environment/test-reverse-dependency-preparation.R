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
sys.source(file.path(root, "compat", "fingerprint.R"),
  envir = environment(), keep.source = FALSE)
sys.source(file.path(root, "compat", "repository-evidence.R"),
  envir = environment(), keep.source = FALSE)

run_reverse_dependency_preparation_fixtures <- function() {
test_root <- tempfile("reverse-dependency-preparation-",
  tmpdir = file.path(root, ".local", "tmp"))
if (!dir.create(test_root, recursive = FALSE, mode = "0700")) {
  stop("could not create fixture root", call. = FALSE)
}
fixture_owned_paths <- test_root
cleanup_fixture_paths <- function() {
  for (path in rev(fixture_owned_paths)) {
    if (!file.exists(path) && !dir.exists(path) &&
        !reverse_prep_is_symbolic(path)) {
      next
    }
    status <- unlink(path, recursive = TRUE, force = TRUE)
    if (!identical(as.integer(status), 0L) || file.exists(path) ||
        dir.exists(path) || reverse_prep_is_symbolic(path)) {
      stop("could not remove fixture-owned path: ", path, call. = FALSE)
    }
  }
}
on.exit(cleanup_fixture_paths(), add = TRUE)

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
  "Imports: hardpkg (>= 2.0), utility.pkg, differentpkg (!= 1.0)",
  "LinkingTo: linkpkg (== 1.1)",
  "Suggests: optionalpkg"
), description, useBytes = TRUE)
parsed <- reverse_prep_description_dependencies(description, "Target")
stopifnot(
  identical(parsed$version, "1.2.3"),
  identical(
    parsed$dependencies$dependency,
    c(
      "R", "paradox", "methods", "hardpkg", "utility.pkg",
      "differentpkg", "linkpkg"
    )
  ),
  identical(parsed$dependencies$excluded,
    c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)),
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
  grepl("differentpkg (!= 1.0)", resolver_payload, fixed = TRUE),
  grepl("linkpkg (== 1.1)", resolver_payload, fixed = TRUE)
)
stopifnot(
  reverse_prep_version_satisfies("1.0.1", "!= 1.0"),
  !reverse_prep_version_satisfies("1.0", "!= 1.0")
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
  "Description: Fixture.", "License: MIT", "Imports: badpkg (~> 1.0)"
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
stopifnot(file.symlink(description, description_link))
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
    ),
    list(
      ref = "differentpkg", package = "differentpkg", version = "1.1",
      type = "standard", direct = FALSE, dep_types = hard_types,
      sha256 = paste(rep.int("c", 64L), collapse = ""),
      sources = list("https://example.invalid/differentpkg_1.1.tar.gz")
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
    c(
      "paradoxrevdep001-deps", "hardpkg", "linkpkg", "utility.pkg",
      "differentpkg"
    )),
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
tampered$packages[[5L]]$version <- "1.0"
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
      type = "standard", direct = FALSE, repotype = "cranlike",
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
  identical(enriched$repository_type, "cranlike"),
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
invisible(reverse_prep_lock_plan(
  bioc_output, "biocfixture-deps", "BiocRoot", library,
  bioc_dependencies, character(), bioc_sources
))

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
invisible(repository_seal_evidence(stage))
invisible(repository_verify_evidence(stage))
writeLines("tampered", file.path(stage, "result.txt"), useBytes = TRUE)
expect_failure(repository_verify_evidence(stage), "do not match")

stage_link <- file.path(test_root, "evidence-link")
dir.create(stage_link, mode = "0700")
dir.create(file.path(stage_link, "metadata"), mode = "0700")
writeLines("fixture", file.path(stage_link, "result.txt"), useBytes = TRUE)
stopifnot(file.symlink(
  file.path(test_root, "outside"), file.path(stage_link, "escape")
))
expect_failure(repository_seal_evidence(stage_link), "symbolic path")

harness_path <- file.path(
  root, "compat", "install-reverse-dependency-dependencies.R"
)
harness_library <- file.path(test_root, "harness-library")
stopifnot(dir.create(harness_library, recursive = FALSE, mode = "0700"))
harness_library_before <- compat_tree_content_sha256(harness_library)
fixture_nonce <- gsub("[^A-Za-z0-9]", "", basename(test_root))
fixture_run_prefix <- paste0(
  "reverse-dependency-preparation-fixture-", fixture_nonce
)
fixture_run_ids <- paste0(
  fixture_run_prefix, c("-success", "-failure", "-retry", "-missing-library")
)
names(fixture_run_ids) <- c("success", "failure", "retry", "missing")
fixture_run_directories <- file.path(
  root, ".local", "compat", "runs", fixture_run_ids
)
if (any(file.exists(fixture_run_directories) |
    dir.exists(fixture_run_directories) |
    vapply(fixture_run_directories, reverse_prep_is_symbolic, logical(1L)))) {
  stop("disposable harness fixture run path already exists", call. = FALSE)
}
fixture_owned_paths <- c(fixture_owned_paths, fixture_run_directories)

run_harness <- function(run_id, dependency_library = harness_library,
    failure_hook = "") {
  stdout <- file.path(test_root, paste0(run_id, ".stdout"))
  stderr <- file.path(test_root, paste0(run_id, ".stderr"))
  arguments <- c(
    "--vanilla", shQuote(harness_path),
    "--root", shQuote(root),
    "--max-priority", "3",
    "--dependency-library", shQuote(dependency_library),
    "--run-id", shQuote(run_id),
    "--package", "drape", "--plan-only"
  )
  command <- file.path(root, ".local", "toolchain", "bin", "Rscript")
  status <- suppressWarnings(if (nzchar(failure_hook)) {
    system2(
      command, arguments,
      stdout = stdout, stderr = stderr,
      env = paste0("PARADOX_REVERSE_PREP_TEST_FAILURE=", failure_hook)
    )
  } else {
    system2(command, arguments, stdout = stdout, stderr = stderr)
  })
  list(
    status = as.integer(status),
    stdout = readLines(stdout, warn = FALSE),
    stderr = readLines(stderr, warn = FALSE)
  )
}
assert_lock_absent <- function() {
  lock <- file.path(
    root, ".local", "compat", ".reverse-dependency-preparation.lock"
  )
  if (file.exists(lock) || dir.exists(lock) ||
      reverse_prep_is_symbolic(lock)) {
    stop("real preparation harness retained its global mutation lock",
      call. = FALSE)
  }
}
assert_successful_plan <- function(run_id, result) {
  if (!identical(result$status, 0L)) {
    stop(
      "real preparation harness failed unexpectedly: ",
      paste(c(result$stdout, result$stderr), collapse = " | "),
      call. = FALSE
    )
  }
  stage <- file.path(
    root, ".local", "compat", "runs", run_id,
    "reverse-dependency-dependencies-priority-3"
  )
  invisible(repository_verify_evidence(stage))
  completion <- utils::read.delim(
    file.path(stage, "metadata", "completion.tsv"),
    sep = "\t", quote = "", comment.char = "", colClasses = "character",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  if (!identical(
      completion$value[match("status", completion$field)], "planned")) {
    stop("real preparation harness did not retain planned completion",
      call. = FALSE)
  }
  if (!identical(
      compat_tree_content_sha256(harness_library), harness_library_before)) {
    stop("real plan-only harness changed its disposable dependency library",
      call. = FALSE)
  }
  assert_lock_absent()
}

missing_library <- file.path(test_root, "missing-library")
missing_result <- run_harness(
  fixture_run_ids[["missing"]], dependency_library = missing_library
)
if (identical(missing_result$status, 0L) || file.exists(missing_library) ||
    dir.exists(missing_library) || reverse_prep_is_symbolic(missing_library) ||
    !any(grepl(
      "--plan-only requires a pre-existing dependency library",
      missing_result$stderr, fixed = TRUE
    ))) {
  stop("plan-only absent-library fixture did not fail without mutation",
    call. = FALSE)
}
assert_lock_absent()

success_result <- run_harness(fixture_run_ids[["success"]])
assert_successful_plan(fixture_run_ids[["success"]], success_result)

failure_result <- run_harness(
  fixture_run_ids[["failure"]], failure_hook = "after-lock"
)
if (identical(failure_result$status, 0L) ||
    !any(grepl(
      "injected failure after reverse-dependency lock acquisition",
      failure_result$stderr, fixed = TRUE
    ))) {
  stop("injected post-lock stop did not fail through the real harness",
    call. = FALSE)
}
if (!identical(
    compat_tree_content_sha256(harness_library), harness_library_before)) {
  stop("injected post-lock stop changed the disposable dependency library",
    call. = FALSE)
}
assert_lock_absent()

retry_result <- run_harness(fixture_run_ids[["retry"]])
assert_successful_plan(fixture_run_ids[["retry"]], retry_result)

harness <- readLines(
  harness_path,
  warn = FALSE
)
required_fragments <- c(
  "dependencies = NA", "update = FALSE", "compat_fetch_verify_archive",
  "reverse_prep_enrich_lockfile", "resolved-sources",
  "paradox was installed, removed, or changed", "repository_seal_evidence",
  "compat_system_verify_evidence", "run_dependency_preparation <- function()",
  "PARADOX_REVERSE_PREP_TEST_FAILURE", "recursive = TRUE",
  "identical(as.integer(status), 0L)"
)
for (fragment in required_fragments) {
  if (!any(grepl(fragment, harness, fixed = TRUE))) {
    stop("preparation harness lacks required policy: ", fragment, call. = FALSE)
  }
}

cat("reverse-dependency preparation fixtures: PASS\n")
}

run_reverse_dependency_preparation_fixtures()
