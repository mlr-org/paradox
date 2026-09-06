#!/usr/bin/env Rscript

fail <- function(...) stop(..., call. = FALSE)
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 3L) {
  fail(
    "usage: run-hosted-r36-windows.R REPOSITORY_ROOT WORK_ROOT EVIDENCE_ROOT"
  )
}

repository <- normalizePath(
  arguments[[1L]], winslash = "/", mustWork = TRUE
)
work_root <- normalizePath(
  arguments[[2L]], winslash = "/", mustWork = TRUE
)
evidence_root <- normalizePath(
  arguments[[3L]], winslash = "/", mustWork = TRUE
)
if (.Platform$OS.type != "windows" ||
    !identical(as.character(getRversion()), "3.6.3") ||
    !grepl("^(x86_64|x64)$", tolower(R.version$arch))) {
  fail(
    "hosted old-Windows validation requires exact x86-64 R 3.6.3; observed ",
    R.version$version.string, " on ", R.version$platform
  )
}

dependency_library <- file.path(work_root, "library")
if (!dir.exists(dependency_library)) {
  fail("the authenticated runtime dependency library is absent")
}
isolated_library <- Sys.getenv("R_LIBS_USER", unset = NA_character_)
isolation_receipt <- Sys.getenv(
  "PARADOX_R36_ISOLATION_RECEIPT",
  unset = NA_character_
)
if (!identical(
      Sys.getenv("PARADOX_R36_ISOLATION_SCHEMA"),
      "hosted-r36-isolation-v1"
    ) ||
    !identical(Sys.getenv("PARADOX_R36_ISOLATION_PHASE"), "candidate") ||
    !identical(Sys.getenv("R_KEEP_PKG_SOURCE"), "yes") ||
    is.na(isolated_library) ||
    !identical(
      tolower(normalizePath(
        isolated_library, winslash = "/", mustWork = TRUE
      )),
      tolower(normalizePath(
        dependency_library, winslash = "/", mustWork = TRUE
      ))
    ) ||
    is.na(isolation_receipt) ||
    !file.exists(isolation_receipt) ||
    dir.exists(isolation_receipt)) {
  fail("hosted old-Windows validation is not in its isolated candidate phase")
}
expected <- c(
  backports = "1.5.1",
  checkmate = "2.3.4",
  "data.table" = "1.18.4",
  R6 = "2.6.1",
  cli = "3.6.6",
  digest = "0.6.39",
  mlr3misc = "0.22.0"
)
if (!identical(sort(list.files(dependency_library)), sort(names(expected)))) {
  fail("the runtime dependency library is not the exact selected closure")
}
for (package in names(expected)) {
  description <- utils::packageDescription(
    package,
    lib.loc = dependency_library
  )
  if (!identical(description$Version, unname(expected[[package]])) ||
      is.null(description$Built) ||
      !startsWith(description$Built, "R 3.6.3; ")) {
    fail("unexpected installed identity for runtime package ", package)
  }
}
.libPaths(c(dependency_library, .Library))

logs <- file.path(evidence_root, "logs")
if (!dir.create(logs, recursive = FALSE, showWarnings = FALSE)) {
  fail("could not create the hosted old-Windows log directory")
}

show_log <- function(path) {
  if (file.exists(path)) {
    cat(readLines(path, warn = FALSE), sep = "\n")
    cat("\n")
  }
}

run_native <- function(
    command,
    args,
    label,
    stem,
    wd = NULL,
    fail_on_error = TRUE) {
  stdout <- file.path(logs, paste0(stem, ".stdout"))
  stderr <- file.path(logs, paste0(stem, ".stderr"))
  old <- if (is.null(wd)) NULL else setwd(wd)
  if (!is.null(old)) {
    on.exit(setwd(old), add = TRUE)
  }
  status <- suppressWarnings(system2(
    command,
    args = args,
    stdout = stdout,
    stderr = stderr,
    wait = TRUE
  ))
  valid_status <- length(status) == 1L && !is.na(status)
  if ((!valid_status || status != 0L) && fail_on_error) {
    show_log(stdout)
    show_log(stderr)
    fail(label, " exited with status ", paste(status, collapse = ", "))
  }
  invisible(list(
    status = if (valid_status) as.integer(status) else NA_integer_,
    stdout = stdout,
    stderr = stderr
  ))
}

r_executable <- file.path(R.home("bin"), "R.exe")
if (!file.exists(r_executable)) {
  fail("could not locate exact R.exe")
}
build_root <- file.path(work_root, "package-build")
install_library <- file.path(work_root, "package-library")
check_root <- file.path(work_root, "package-check")
for (path in c(build_root, install_library, check_root)) {
  if (!dir.create(path, recursive = FALSE, showWarnings = FALSE)) {
    fail("hosted old-Windows subdirectory is not fresh: ", path)
  }
}

Sys.setenv(
  R_LIBS_USER = dependency_library,
  "_R_CHECK_FORCE_SUGGESTS_" = "false",
  "_R_CHECK_CRAN_INCOMING_" = "false",
  "_R_CHECK_DEPENDS_ONLY_" = "TRUE",
  "_R_CHECK_RD_XREFS_" = "false",
  NOT_CRAN = "false"
)

build_process <- run_native(
  r_executable,
  c(
    "CMD", "build",
    "--no-build-vignettes",
    "--no-manual",
    shQuote(repository)
  ),
  "R CMD build",
  "package-build",
  wd = build_root
)
archives <- Sys.glob(file.path(build_root, "paradox_*.tar.gz"))
if (length(archives) != 1L ||
    basename(archives) != "paradox_2.0.0.tar.gz") {
  fail("R CMD build did not create exactly paradox_2.0.0.tar.gz")
}
archive <- normalizePath(archives, winslash = "/", mustWork = TRUE)
archive_sha256 <- digest::digest(
  archive,
  algo = "sha256",
  serialize = FALSE,
  file = TRUE
)

run_native(
  r_executable,
  c(
    "CMD", "INSTALL",
    "--preclean",
    "--clean",
    "--no-multiarch",
    paste0("--library=", shQuote(install_library)),
    shQuote(archive)
  ),
  "source installation of the built Paradox archive",
  "package-install"
)
.libPaths(c(install_library, dependency_library, .Library))
namespace <- loadNamespace("paradox", lib.loc = install_library)
dll <- getLoadedDLLs()[["paradox"]]
registered <- if (is.null(dll)) NULL else getDLLRegisteredRoutines(dll)[[".Call"]]
if (is.null(namespace) ||
    is.null(dll) ||
    !file.exists(dll[["path"]]) ||
    !grepl("\\.dll$", dll[["path"]], ignore.case = TRUE) ||
    !identical(dll[["dynamicLookup"]], FALSE) ||
    !length(registered)) {
  fail("the installed Paradox DLL is absent or incorrectly registered")
}

objdump_candidates <- Sys.which(c(
  "x86_64-w64-mingw32-objdump.exe",
  "objdump.exe"
))
objdump_candidates <- unname(objdump_candidates[nzchar(objdump_candidates)])
if (!length(objdump_candidates)) {
  fail("Rtools35 objdump is not on PATH")
}
objdump_output <- suppressWarnings(system2(
  objdump_candidates[[1L]],
  c("-f", shQuote(dll[["path"]])),
  stdout = TRUE,
  stderr = TRUE
))
objdump_status <- attr(objdump_output, "status")
if ((!is.null(objdump_status) && objdump_status != 0L) ||
    !any(grepl(
      "pei-x86-64|architecture:[[:space:]]*i386:x86-64",
      objdump_output,
      ignore.case = TRUE
    ))) {
  cat(objdump_output, sep = "\n")
  fail("Rtools35 objdump did not authenticate an x86-64 PE DLL")
}

ps <- get("ps", envir = namespace)
p_lgl <- get("p_lgl", envir = namespace)
p_int <- get("p_int", envir = namespace)
p_dbl <- get("p_dbl", envir = namespace)
CondEqual <- get("CondEqual", envir = namespace)
generate_design_grid <- get("generate_design_grid", envir = namespace)
to_tune <- get("to_tune", envir = namespace)

parameter_set <- ps(
  parent = p_lgl(),
  child = p_int(0L, 9L)
)
parameter_set$add_dep("child", "parent", CondEqual(TRUE))
parameter_set$values <- list(parent = FALSE, child = 4L)
stopifnot(
  identical(parameter_set$values, list(parent = FALSE, child = 4L)),
  identical(parameter_set$get_values(), list(parent = FALSE))
)
parameter_set$values <- list(parent = TRUE, child = 4L)
stopifnot(identical(
  parameter_set$get_values(),
  list(parent = TRUE, child = 4L)
))

ids_before <- parameter_set$ids()
params_facade <- parameter_set$params
deps_facade <- parameter_set$deps
stopifnot(
  data.table::is.data.table(params_facade),
  data.table::is.data.table(deps_facade)
)
params_facade$id[[1L]] <- "detached"
stopifnot(identical(parameter_set$ids(), ids_before))

grid <- generate_design_grid(
  ps(number = p_dbl(0, 1), flag = p_lgl()),
  resolution = 2L
)
stopifnot(
  data.table::is.data.table(grid$data),
  identical(dim(grid$data), c(4L, 2L)),
  identical(names(grid$data), c("number", "flag"))
)

serialized <- file.path(work_root, "paramset-smoke.rds")
saveRDS(parameter_set, serialized, version = 2)
restored <- readRDS(serialized)
stopifnot(
  identical(restored$get_values(), list(parent = TRUE, child = 4L)),
  identical(restored$check(restored$values), TRUE)
)
for (iteration in seq_len(25L)) {
  candidate <- ps(value = p_int(0L, 10L))
  candidate$values <- list(value = iteration %% 11L)
  stopifnot(identical(candidate$get_values()$value, iteration %% 11L))
}

diagnostic_set <- ps(value = p_int(0L, 1L))
check_diagnostic <- diagnostic_set$check(list(value = 2L))
assignment_diagnostic <- tryCatch(
  {
    diagnostic_set$values <- list(value = 2L)
    NULL
  },
  error = function(error) conditionMessage(error)
)
stopifnot(
  is.character(check_diagnostic),
  length(check_diagnostic) == 1L,
  !is.na(check_diagnostic),
  grepl("value: Element 1 is not <=", check_diagnostic, fixed = TRUE),
  is.character(assignment_diagnostic),
  length(assignment_diagnostic) == 1L,
  !is.na(assignment_diagnostic),
  grepl(
    "Assertion on 'xs' failed: value: Element 1 is not <=",
    assignment_diagnostic,
    fixed = TRUE
  )
)

# This rendered bounds fragment exceeds the formatter's first real buffer.
# Exercise both the returned and thrown paths under Rtools35/MSVCRT so the
# evidence cannot pass with a valid-only smoke or a truncated retry.
growth_set <- ps(value = p_dbl(0, 1, tolerance = 0))
growth_lower <- -.Machine$double.xmax
growth_upper <- .Machine$double.xmax
growth_token <- to_tune(lower = growth_lower, upper = growth_upper)
growth_bounds <- paste0(
  "lower ",
  sprintf("%g, upper %g", growth_lower, growth_upper)
)
growth_check_diagnostic <- growth_set$check(list(value = growth_token))
growth_assignment_diagnostic <- tryCatch(
  {
    growth_set$values <- list(value = growth_token)
    NULL
  },
  error = function(error) conditionMessage(error)
)
stopifnot(
  is.character(growth_check_diagnostic),
  length(growth_check_diagnostic) == 1L,
  !is.na(growth_check_diagnostic),
  grepl(growth_bounds, growth_check_diagnostic, fixed = TRUE),
  is.character(growth_assignment_diagnostic),
  length(growth_assignment_diagnostic) == 1L,
  !is.na(growth_assignment_diagnostic),
  grepl(growth_bounds, growth_assignment_diagnostic, fixed = TRUE),
  endsWith(growth_assignment_diagnostic, paste0(growth_bounds, "."))
)

compiler <- Sys.which("gcc.exe")
compiler_output <- if (nzchar(compiler)) {
  system2(compiler, "--version", stdout = TRUE, stderr = TRUE)
} else {
  "gcc.exe not found"
}
compiler <- if (nzchar(compiler)) {
  normalizePath(compiler, winslash = "/", mustWork = TRUE)
} else {
  compiler
}
stopifnot(
  identical(compiler, "C:/Rtools/mingw_64/bin/gcc.exe"),
  length(compiler_output) >= 1L,
  identical(
    compiler_output[[1L]],
    "gcc.exe (x86_64-posix-seh, Built by MinGW-W64 project) 4.9.3"
  )
)
writeLines(
  c(
    paste0("r_version=", as.character(getRversion())),
    paste0("r_platform=", R.version$platform),
    paste0("r_arch=", R.version$arch),
    paste0("package_archive=", basename(archive)),
    paste0("package_archive_sha256=", archive_sha256),
    paste0("dll=", normalizePath(
      dll[["path"]], winslash = "/", mustWork = TRUE
    )),
    paste0("registered_call_routines=", length(registered)),
    paste0("compiler=", compiler),
    paste0("compiler_first_line=", compiler_output[[1L]]),
    "formatted_diagnostics=pass",
    "smoke=pass"
  ),
  file.path(evidence_root, "old-windows-summary.txt"),
  useBytes = TRUE
)
capture.output(
  sessionInfo(),
  file = file.path(evidence_root, "session-info.txt")
)
writeLines(
  objdump_output,
  file.path(evidence_root, "paradox-dll-objdump.txt"),
  useBytes = TRUE
)

check_process <- run_native(
  r_executable,
  c(
    "CMD", "check",
    "--no-tests",
    "--no-examples",
    "--ignore-vignettes",
    "--no-manual",
    "--no-multiarch",
    shQuote(archive)
  ),
  "runtime-import-only R CMD check",
  "package-check",
  wd = check_root,
  fail_on_error = FALSE
)

retained_check_root <- file.path(evidence_root, "check")
check_directories <- Sys.glob(file.path(check_root, "*.Rcheck"))
if (length(check_directories)) {
  if (!dir.create(
      retained_check_root,
      recursive = FALSE,
      showWarnings = FALSE
    ) ||
      any(!vapply(check_directories, function(directory) {
        isTRUE(file.copy(
          directory,
          retained_check_root,
          recursive = TRUE,
          copy.mode = TRUE,
          copy.date = TRUE
        ))
      }, logical(1L)))) {
    fail("could not retain the old-Windows check tree")
  }
}
if (is.na(check_process$status) || check_process$status != 0L) {
  show_log(check_process$stdout)
  show_log(check_process$stderr)
  fail(
    "runtime-import-only R CMD check exited with status ",
    check_process$status
  )
}

check_logs <- Sys.glob(file.path(
  check_root, "*.Rcheck", "00check.log"
))
expected_log <- file.path(check_root, "paradox.Rcheck", "00check.log")
if (!identical(check_logs, expected_log) || !file.exists(expected_log)) {
  fail("R CMD check did not create exactly paradox.Rcheck/00check.log")
}
lines <- readLines(expected_log, warn = FALSE)
expected_unavailable_suggests <- c(
  "callr", "reticulate", "rmarkdown", "mlr3learners", "e1071",
  "knitr", "lhs", "spacefillr", "testthat"
)
status <- grep("^Status:", lines, value = TRUE)
nonempty <- lines[nzchar(trimws(lines))]
problem_markers <- grep(
  "^\\* checking .* \\.\\.\\. (NOTE|WARNING|ERROR)$",
  lines
)
expected_marker <- "* checking package dependencies ... NOTE"
marker <- if (length(problem_markers) == 1L) problem_markers[[1L]] else NA_integer_
later_checks <- if (!is.na(marker)) {
  which(seq_along(lines) > marker & startsWith(lines, "* checking "))
} else {
  integer()
}
block_end <- if (length(later_checks)) later_checks[[1L]] - 1L else NA_integer_
note_block <- if (!is.na(marker) && !is.na(block_end) &&
    block_end >= marker + 1L) {
  trimws(lines[seq.int(marker + 1L, block_end)])
} else {
  character()
}
note_block <- note_block[nzchar(note_block)]
suggest_tokens <- if (length(note_block) >= 2L) {
  token_text <- paste(note_block[-1L], collapse = " ")
  matches <- gregexpr("[A-Za-z][A-Za-z0-9.]*", token_text, perl = TRUE)
  regmatches(token_text, matches)[[1L]]
} else {
  character()
}
note_words <- grepl("(^|[[:space:]])NOTE($|[[:space:]])", lines)
warning_or_error <- grepl(
  "(^|[[:space:]])(WARNING|ERROR)(:|[[:space:]]|$)",
  lines
)
if (any(grepl("Execution halted", lines, fixed = TRUE)) ||
    !identical(status, "Status: 1 NOTE") ||
    !identical(tail(nonempty, 1L), "Status: 1 NOTE") ||
    !identical(lines[problem_markers], expected_marker) ||
    sum(note_words) != 2L ||
    any(warning_or_error) ||
    !length(note_block) ||
      !identical(
        note_block[[1L]],
        "Packages suggested but not available for checking:"
      ) ||
    length(suggest_tokens) != length(expected_unavailable_suggests) ||
    anyDuplicated(suggest_tokens) ||
    !setequal(suggest_tokens, expected_unavailable_suggests)) {
  cat(lines, sep = "\n")
  fail(paste0(
    "old-Windows R CMD check must contain only the exact unavailable-",
    "Suggests NOTE and one final `Status: 1 NOTE`"
  ))
}

cat(
  "Exact R 3.6.3/Rtools35 source build, DLL smoke, and bounded check passed.\n"
)
