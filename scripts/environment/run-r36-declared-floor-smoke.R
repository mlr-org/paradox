#!/usr/bin/env Rscript

# This is deliberately a smoke probe, not a second R-3.6 test suite.  The
# ordinary runtime stage owns complete behavior coverage.  The caller installs
# the frozen package against this exact floor library; this helper authenticates
# those installed inputs, loads the package, and touches one representative
# path through each direct import.

fail <- function(...) stop(..., call. = FALSE)

expected_lock <- data.frame(
  Package = c(
    "backports", "checkmate", "data.table", "mlr3misc", "R6", "digest"
  ),
  Version = c("1.1.7", "2.0.0", "1.18.4", "0.10.0", "2.6.1", "0.6.39"),
  Role = c(
    rep("runtime-import", 5L),
    "runtime-dependency"
  ),
  SHA256 = c(
    "b277e28716059d29841ccc1d2411accb4c2b9e8f95a1ae70f0a4f0192a9e6e0b",
    "0dc25b0e20c04836359df1885d099c6e4ad8ae0e585a9e4107f7ea945d9c6fa4",
    "d65256f0050a6443770d5982b5098a2a8c894f9675ed17053416c7815cbddeb9",
    "2154c39887d473d23b3a2c68405db3aaf878ee7d69c937d6acfa981e64f4dff2",
    "59c6eba8b1b912eb7e104f65053235604be853425ee67c152ac4e86a1f2073b4",
    "8bf048b49b2d17077138fae758bda56bbd53278d9437f2fdeaedf979c90a13c9"
  ),
  URL = c(
    "https://cloud.r-project.org/src/contrib/backports_1.1.7.tar.gz",
    "https://cloud.r-project.org/src/contrib/checkmate_2.0.0.tar.gz",
    "https://cloud.r-project.org/src/contrib/data.table_1.18.4.tar.gz",
    "https://cloud.r-project.org/src/contrib/mlr3misc_0.10.0.tar.gz",
    "https://cloud.r-project.org/src/contrib/R6_2.6.1.tar.gz",
    "https://cloud.r-project.org/src/contrib/digest_0.6.39.tar.gz"
  ),
  FallbackURL = c(
    paste0(
      "https://cloud.r-project.org/src/contrib/Archive/backports/",
      "backports_1.1.7.tar.gz"
    ),
    paste0(
      "https://cloud.r-project.org/src/contrib/Archive/checkmate/",
      "checkmate_2.0.0.tar.gz"
    ),
    paste0(
      "https://cloud.r-project.org/src/contrib/Archive/data.table/",
      "data.table_1.18.4.tar.gz"
    ),
    paste0(
      "https://cloud.r-project.org/src/contrib/Archive/mlr3misc/",
      "mlr3misc_0.10.0.tar.gz"
    ),
    paste0(
      "https://cloud.r-project.org/src/contrib/Archive/R6/",
      "R6_2.6.1.tar.gz"
    ),
    paste0(
      "https://cloud.r-project.org/src/contrib/Archive/digest/",
      "digest_0.6.39.tar.gz"
    )
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

expected_direct <- expected_lock[expected_lock$Role == "runtime-import", ]
expected_direct_versions <- stats::setNames(
  expected_direct$Version,
  expected_direct$Package
)

is_symbolic <- function(path) {
  target <- Sys.readlink(path)
  length(target) == 1L && !is.na(target) && nzchar(target)
}

canonical_directory <- function(path, label) {
  if (!nzchar(path) || !dir.exists(path) || is_symbolic(path)) {
    fail(label, " must be an existing plain directory: ", path)
  }
  normalized <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!identical(path, normalized)) {
    fail(label, " must be canonical: ", path)
  }
  normalized
}

assert_below_root <- function(root, path, label, expect_directory) {
  if (!identical(path, root) && !startsWith(path, paste0(root, "/"))) {
    fail(label, " escaped the repository: ", path)
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
    fail(label, " contains a non-canonical component: ", path)
  }
  current <- root
  for (component in components) {
    current <- file.path(current, component)
    if (is_symbolic(current)) {
      fail(label, " contains a symbolic component: ", current)
    }
  }
  if (isTRUE(expect_directory)) {
    canonical_directory(path, label)
  } else {
    if (!file.exists(path) || dir.exists(path) || is_symbolic(path)) {
      fail(label, " must be an existing plain file: ", path)
    }
    normalized <- normalizePath(path, winslash = "/", mustWork = TRUE)
    if (!identical(path, normalized)) {
      fail(label, " must be canonical: ", path)
    }
  }
  invisible(path)
}

description_dependencies <- function(field) {
  specifications <- trimws(strsplit(
    gsub("[[:space:]]+", " ", field),
    ",",
    fixed = TRUE
  )[[1L]])
  pattern <- paste0(
    "^([A-Za-z][A-Za-z0-9.]*)",
    "[[:space:]]*[(]>=[[:space:]]*([^()[:space:]]+)[[:space:]]*[)]$"
  )
  matches <- regexec(pattern, specifications)
  pieces <- regmatches(specifications, matches)
  if (any(lengths(pieces) != 3L)) {
    fail("could not parse an exact declared dependency floor")
  }
  stats::setNames(
    vapply(pieces, `[[`, character(1L), 3L),
    vapply(pieces, `[[`, character(1L), 2L)
  )
}

validate_inputs <- function(repository, lock_path) {
  repository <- canonical_directory(repository, "repository")
  assert_below_root(repository, lock_path, "declared-floor lock", FALSE)
  description_path <- file.path(repository, "DESCRIPTION")
  assert_below_root(repository, description_path, "package DESCRIPTION", FALSE)

  lock <- utils::read.delim(
    lock_path,
    header = TRUE,
    quote = "",
    comment.char = "",
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  row.names(lock) <- NULL
  if (!identical(lock, expected_lock)) {
    fail("declared-floor lock differs from the exact reviewed closure")
  }

  description <- read.dcf(description_path)
  if (nrow(description) != 1L ||
      !all(c("Depends", "Imports") %in% colnames(description))) {
    fail("package DESCRIPTION lacks Depends or Imports")
  }
  depends <- gsub(
    "[[:space:]]+",
    " ",
    unname(description[1L, "Depends"])
  )
  if (!identical(depends, "R (>= 3.6.0)")) {
    fail("package DESCRIPTION no longer declares exact R >= 3.6.0")
  }
  imports <- description_dependencies(unname(description[1L, "Imports"]))
  if (!identical(imports, expected_direct_versions)) {
    fail("package DESCRIPTION differs from the reviewed direct floors")
  }

  invisible(list(repository = repository, lock = lock))
}

library_entries <- function(path) {
  sort(list.files(
    path,
    all.files = TRUE,
    no.. = TRUE,
    full.names = FALSE
  ))
}

validate_installed_package <- function(package, version, library) {
  package_path <- file.path(library, package)
  if (!dir.exists(package_path) || is_symbolic(package_path)) {
    fail("installed package is absent or symbolic: ", package)
  }
  description <- utils::packageDescription(package, lib.loc = library)
  if (!identical(description$Package, package) ||
      !identical(description$Version, version) ||
      is.null(description$Built) ||
      !startsWith(description$Built, "R 3.6.3; ")) {
    fail("unexpected installed identity for declared-floor package ", package)
  }
  invisible(description)
}

run_smoke <- function(
    repository,
    lock_path,
    candidate_library,
    floor_library,
    managed_root) {
  validated <- validate_inputs(repository, lock_path)
  if (!identical(as.character(getRversion()), "3.6.3")) {
    fail(
      "declared-floor smoke requires exact R 3.6.3; observed ",
      R.version$version.string
    )
  }

  managed_root <- canonical_directory(managed_root, "managed root")
  assert_below_root(
    managed_root,
    validated$repository,
    "repository",
    TRUE
  )
  candidate_library <- canonical_directory(
    candidate_library,
    "candidate library"
  )
  floor_library <- canonical_directory(floor_library, "floor library")
  assert_below_root(
    managed_root,
    candidate_library,
    "candidate library",
    TRUE
  )
  assert_below_root(
    managed_root,
    floor_library,
    "floor library",
    TRUE
  )
  if (!identical(library_entries(candidate_library), "paradox")) {
    fail("candidate library must contain only paradox")
  }
  if (!identical(
    library_entries(floor_library),
    sort(expected_lock$Package)
  )) {
    fail("floor library differs from the exact locked package set")
  }

  for (index in seq_len(nrow(expected_lock))) {
    validate_installed_package(
      expected_lock$Package[[index]],
      expected_lock$Version[[index]],
      floor_library
    )
  }
  candidate_description <- validate_installed_package(
    "paradox",
    "2.0.0",
    candidate_library
  )

  already_loaded <- intersect(
    expected_lock$Package,
    loadedNamespaces()
  )
  if (length(already_loaded)) {
    fail(
      "declared-floor namespace was loaded before the smoke: ",
      paste(already_loaded, collapse = ", ")
    )
  }
  .libPaths(c(candidate_library, floor_library, .Library))
  namespace <- loadNamespace("paradox", lib.loc = candidate_library)
  if (is.null(namespace)) {
    fail("could not load the candidate paradox namespace")
  }
  observed_candidate <- normalizePath(
    getNamespaceInfo(namespace, "path"),
    winslash = "/",
    mustWork = TRUE
  )
  expected_candidate <- normalizePath(
    file.path(candidate_library, "paradox"),
    winslash = "/",
    mustWork = TRUE
  )
  if (!identical(observed_candidate, expected_candidate)) {
    fail("paradox namespace did not load from the candidate library")
  }
  for (package in expected_lock$Package) {
    observed <- normalizePath(
      getNamespaceInfo(asNamespace(package), "path"),
      winslash = "/",
      mustWork = TRUE
    )
    expected <- normalizePath(
      file.path(floor_library, package),
      winslash = "/",
      mustWork = TRUE
    )
    if (!identical(observed, expected)) {
      fail("dependency namespace did not load from the floor library: ", package)
    }
  }

  dll <- getLoadedDLLs()[["paradox"]]
  registered <- if (is.null(dll)) {
    NULL
  } else {
    getDLLRegisteredRoutines(dll)[[".Call"]]
  }
  if (is.null(dll) || !file.exists(dll[["path"]]) ||
      !identical(dll[["dynamicLookup"]], FALSE) || !length(registered)) {
    fail("candidate paradox DLL is absent or incorrectly registered")
  }
  observed_dll <- normalizePath(
    dll[["path"]],
    winslash = "/",
    mustWork = TRUE
  )
  expected_dll <- normalizePath(
    file.path(
      candidate_library,
      "paradox",
      "libs",
      paste0("paradox", .Platform$dynlib.ext)
    ),
    winslash = "/",
    mustWork = TRUE
  )
  if (!identical(observed_dll, expected_dll)) {
    fail("paradox DLL did not load from the candidate library")
  }

  get_export <- function(name) {
    get(name, envir = namespace, inherits = FALSE)
  }
  ps <- get_export("ps")
  p_lgl <- get_export("p_lgl")
  p_int <- get_export("p_int")
  p_dbl <- get_export("p_dbl")
  CondEqual <- get_export("CondEqual")
  generate_design_grid <- get_export("generate_design_grid")
  to_tune <- get_export("to_tune")

  simple_domain <- p_dbl()
  token <- to_tune()
  if (!identical(simple_domain$id, "p_dbl()") ||
      !identical(token$call, "to_tune()")) {
    fail("backported deparse1 constructor path disagrees with the contract")
  }

  parameter_set <- ps(
    parent = p_lgl(),
    child = p_int(0L, 9L)
  )
  parameter_set$add_dep("child", "parent", CondEqual(TRUE))
  parameter_set$values <- list(parent = FALSE, child = 4L)
  if (!identical(
    parameter_set$values,
    list(parent = FALSE, child = 4L)
  ) || !identical(
    parameter_set$get_values(),
    list(parent = FALSE)
  )) {
    fail("checked dormant-value storage or dependency filtering failed")
  }
  valid_point <- list(parent = TRUE, child = 4L)
  if (!identical(parameter_set$check(valid_point), TRUE) ||
      !isTRUE(parameter_set$test(valid_point))) {
    fail("representative ParamSet check path rejected a valid point")
  }
  parameter_set$assert(valid_point)
  invalid <- parameter_set$check(list(parent = TRUE, child = 10L))
  if (!is.character(invalid) || length(invalid) != 1L || !nzchar(invalid)) {
    fail("representative invalid ParamSet check lacked a diagnostic")
  }
  parameter_set$values <- valid_point
  if (!identical(parameter_set$get_values(), valid_point)) {
    fail("checked value assignment or readback failed")
  }

  grid <- generate_design_grid(
    ps(number = p_dbl(0, 1), flag = p_lgl()),
    resolution = 2L
  )
  if (!data.table::is.data.table(grid$data) ||
      !identical(dim(grid$data), c(4L, 2L)) ||
      !identical(names(grid$data), c("number", "flag")) ||
      !R6::is.R6(parameter_set)) {
    fail("representative data.table/mlr3misc/R6 constructor path failed")
  }

  cat(
    "format\t1\n",
    "runtime\t3.6.3\n",
    "candidate_package\tparadox\n",
    "candidate_version\t", candidate_description$Version, "\n",
    "dependency_count\t", nrow(expected_lock), "\n",
    sep = ""
  )
  for (index in seq_len(nrow(expected_lock))) {
    cat(
      "dependency\t",
      expected_lock$Package[[index]],
      "=",
      expected_lock$Version[[index]],
      "\n",
      sep = ""
    )
  }
  cat(
    "registered_call_routines\t", length(registered), "\n",
    "constructor_deparse\tpassed\n",
    "paramset_check_values\tpassed\n",
    "grid_facade\tpassed\n",
    "declared_floor_smoke\tpassed\n",
    sep = ""
  )
  invisible(TRUE)
}

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) == 3L && identical(arguments[[1L]], "validate")) {
  validate_inputs(arguments[[2L]], arguments[[3L]])
  cat("declared_floor_inputs=passed\n")
} else if (length(arguments) == 6L &&
    identical(arguments[[1L]], "smoke")) {
  run_smoke(
    arguments[[2L]],
    arguments[[3L]],
    arguments[[4L]],
    arguments[[5L]],
    arguments[[6L]]
  )
} else {
  fail(
    paste(
      "usage: run-r36-declared-floor-smoke.R validate REPOSITORY LOCK",
      "   or: run-r36-declared-floor-smoke.R smoke",
      paste(
        "REPOSITORY LOCK CANDIDATE_LIBRARY FLOOR_LIBRARY",
        "MANAGED_ROOT"
      )
    )
  )
}
