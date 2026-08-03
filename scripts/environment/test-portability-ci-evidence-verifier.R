#!/usr/bin/env Rscript

fail <- function(...) stop(..., call. = FALSE)

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  fail("could not identify portability evidence verifier test")
}
script_path <- normalizePath(
  sub("^--file=", "", script_argument), winslash = "/", mustWork = TRUE
)
root <- normalizePath(
  file.path(dirname(script_path), "..", ".."),
  winslash = "/", mustWork = TRUE
)
if (!identical(Sys.getenv("PARADOX_ACTIVE_ROOT"), root)) {
  fail("activate the exact repository-local toolchain first: . scripts/activate")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  fail("repository-local jsonlite package is required")
}

verifier <- file.path(
  root, "scripts", "environment", "verify-portability-ci-evidence.R"
)
if (!file.exists(verifier) || nzchar(Sys.readlink(verifier))) {
  fail("portability evidence verifier is absent or symbolic")
}

fixture <- tempfile("portability-ci-evidence-")
dir.create(fixture)
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

run_id <- "123456789"
run_attempt <- "1"
harness_commit <- paste(rep("a", 40L), collapse = "")
candidate_commit <- paste(rep("b", 40L), collapse = "")
harness_tag <- "paradox-test-harness-aaaaaaaa"
workflow_ref <- paste0("refs/tags/", harness_tag)
workflow <- file.path(fixture, "r-cmd-check.yml")
writeLines(c(
  "name: fixture",
  "on: workflow_dispatch",
  paste0("          readonly candidate=", candidate_commit),
  paste0("          readonly candidate=", candidate_commit),
  "          readonly helper=scripts/environment/install-hosted-r36-windows.ps1",
  paste0("          readonly expected_helper_blob=", strrep("c", 40L)),
  paste0(
    "          expected_helper_entry=\"$(printf ",
    "'100644 blob %s\\t%s' ",
    "\"$expected_helper_blob\" \"$helper\")\""
  ),
  '          test "$helper_entry" = "$expected_helper_entry"',
  "          readonly helper=scripts/environment/install-hosted-r36-windows.ps1",
  paste0("          readonly expected_helper_blob=", strrep("c", 40L)),
  paste0(
    "          expected_helper_entry=\"$(printf ",
    "'100644 blob %s\\t%s' ",
    "\"$expected_helper_blob\" \"$helper\")\""
  ),
  '          test "$helper_entry" = "$expected_helper_entry"'
), workflow)
workflow_sha256 <- unname(tools::sha256sum(workflow))

write_json <- function(value, path) {
  jsonlite::write_json(
    value, path, auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
}

write_old_windows_isolation_fixture <- function(artifact_root) {
  isolation <- file.path(artifact_root, "environment-isolation")
  if (!dir.create(isolation, recursive = FALSE, showWarnings = FALSE)) {
    fail("could not create synthetic old-Windows isolation evidence")
  }
  phases <- c("toolchain", "closure", "candidate")
  empty_sha256 <-
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  empty_files <- c(
    R_ENVIRON = "Renviron.site",
    R_ENVIRON_USER = "Renviron.user",
    R_PROFILE = "Rprofile.site",
    R_PROFILE_USER = "Rprofile.user",
    R_MAKEVARS_SITE = "Makevars.site",
    R_MAKEVARS_USER = "Makevars.user"
  )
  reset_variables <- c(
    "R_HOME", "R_LIBS", "R_LIBS_USER", "R_LIBS_SITE",
    "R_DEFAULT_PACKAGES", "R_ENVIRON", "R_ENVIRON_USER", "R_PROFILE",
    "R_PROFILE_USER", "R_BUILD_ENVIRON", "R_CHECK_ENVIRON",
    "R_INSTALL_ENVIRON", "R_MAKEVARS_SITE", "R_MAKEVARS_USER", "R_USER",
    "R_HISTFILE", "R_ARCH", "R_INSTALL_TAR", "R_PKG_CFLAGS",
    "R_PKG_CPPFLAGS", "R_PKG_CXXFLAGS", "R_PKG_CXX_STD",
    "R_PKG_FFLAGS", "R_PKG_FCFLAGS", "R_PKG_LIBS", "CC", "CPP",
    "CXX", "CXX11", "CXX14", "CXX17", "CXX20", "CXX23", "FC",
    "F77", "F90", "F95", "OBJC", "OBJCXX", "CC_FOR_BUILD",
    "CPP_FOR_BUILD", "CXX_FOR_BUILD", "FC_FOR_BUILD", "AR", "AS",
    "LD", "NM", "OBJCOPY", "OBJDUMP", "RANLIB", "READELF", "SIZE",
    "STRINGS", "STRIP", "WINDRES", "DLLTOOL", "BINPREF", "BINPREF64",
    "M_ARCH", "CFLAGS", "CPPFLAGS", "CXXFLAGS", "CXX11FLAGS",
    "CXX14FLAGS", "CXX17FLAGS", "CXX20FLAGS", "CXX23FLAGS",
    "FCFLAGS", "FFLAGS", "FORTRANFLAGS", "LDFLAGS", "CPATH",
    "C_INCLUDE_PATH", "CPLUS_INCLUDE_PATH", "OBJC_INCLUDE_PATH",
    "LIBRARY_PATH", "COMPILER_PATH", "GCC_EXEC_PREFIX", "MAKE",
    "MAKEFLAGS", "MFLAGS", "GNUMAKEFLAGS", "MAKEFILES", "CONFIG_SITE",
    "PKG_CONFIG", "PKG_CONFIG_PATH", "PKG_CONFIG_LIBDIR",
    "PKG_CONFIG_SYSROOT_DIR"
  )
  for (phase in phases) {
    home <- paste0("C:/p36-isolation/home/", phase)
    temporary <- paste0("C:/p36-isolation/tmp/", phase)
    user_library <- if (identical(phase, "toolchain")) {
      "C:/p36-isolation/libraries/toolchain"
    } else {
      "C:/p36/library"
    }
    site_library <- "C:/p36-isolation/libraries/site"
    controlled <- c(
      R_ENVIRON = "C:/p36-isolation/files/Renviron.site",
      R_ENVIRON_USER = "C:/p36-isolation/files/Renviron.user",
      R_PROFILE = "C:/p36-isolation/files/Rprofile.site",
      R_PROFILE_USER = "C:/p36-isolation/files/Rprofile.user",
      R_BUILD_ENVIRON = "C:/p36-isolation/files/Renviron.site",
      R_CHECK_ENVIRON = "C:/p36-isolation/files/Renviron.site",
      R_INSTALL_ENVIRON = "C:/p36-isolation/files/Renviron.site",
      R_MAKEVARS_SITE = "C:/p36-isolation/files/Makevars.site",
      R_MAKEVARS_USER = "C:/p36-isolation/files/Makevars.user",
      R_USER = home,
      HOME = home,
      USERPROFILE = home,
      TMP = temporary,
      TEMP = temporary,
      TMPDIR = temporary,
      R_HISTFILE = paste0(home, "/Rhistory"),
      R_LIBS_USER = user_library,
      R_LIBS_SITE = site_library,
      R_KEEP_PKG_SOURCE = "yes",
      PARADOX_R36_ISOLATION_SCHEMA = "hosted-r36-isolation-v1",
      PARADOX_R36_ISOLATION_ROOT = "C:/p36-isolation",
      PARADOX_R36_ISOLATION_PHASE = phase,
      PARADOX_R36_ISOLATION_RECEIPT = paste0(
        "C:/p36-isolation/receipts/", phase, ".tsv"
      )
    )
    rows <- c(
      "Kind\tName\tValue\tSHA256",
      "metadata\tschema\thosted-r36-isolation-v1\t-",
      paste0("metadata\tphase\t", phase, "\t-"),
      paste0("directory\thome\t", home, "\t-"),
      paste0("directory\ttmp\t", temporary, "\t-"),
      paste0("directory\tuser_library\t", user_library, "\t-"),
      paste0("directory\tsite_library\t", site_library, "\t-"),
      paste0(
        "file\t", names(empty_files), "\tC:/p36-isolation/files/",
        unname(empty_files), "\t", empty_sha256
      ),
      paste0(
        "controlled\t", names(controlled), "\t",
        unname(controlled), "\t-"
      ),
      paste0(
        "cleared\t",
        reset_variables[!reset_variables %in% names(controlled)],
        "\tabsent\t-"
      )
    )
    writeLines(
      rows,
      file.path(isolation, paste0(phase, ".tsv")),
      useBytes = TRUE
    )
  }
}

run <- list(
  id = as.numeric(run_id),
  run_attempt = 1L,
  event = "workflow_dispatch",
  status = "completed",
  conclusion = "success",
  head_branch = harness_tag,
  head_sha = harness_commit,
  path = ".github/workflows/r-cmd-check.yml",
  name = "r-cmd-check",
  url = paste0(
    "https://api.github.com/repos/mlr-org/paradox/actions/runs/", run_id
  ),
  html_url = paste0(
    "https://github.com/mlr-org/paradox/actions/runs/", run_id
  ),
  repository = list(full_name = "mlr-org/paradox"),
  head_repository = list(full_name = "mlr-org/paradox")
)
write_json(run, file.path(fixture, "run.json"))

required_steps <- c(
  "Verify frozen harness checkout" = 3L,
  "Verify runner and R architecture" = 6L,
  "Verify native source compilation" = 7L,
  "Run R CMD check" = 8L,
  "Verify R CMD check completion" = 9L,
  "Retain run provenance" = 10L,
  "Upload check evidence" = 11L
)
old_windows_steps <- c(
  "Verify frozen harness checkout" = 3L,
  "Verify exact R 3.6 Windows toolchain" = 5L,
  "Install exact R 3.6 source closure" = 6L,
  "Build, smoke, and check on R 3.6 Windows" = 7L,
  "Retain old-Windows provenance" = 8L,
  "Upload old-Windows evidence" = 9L
)
make_steps <- function(specification) {
  unname(lapply(seq_along(specification), function(index) {
    list(
      name = names(specification)[[index]],
      number = unname(specification[[index]]),
      status = "completed",
      conclusion = "success"
    )
  }))
}
job_specs <- list(
  list(id = 2001, name = "macos-15 / arm64 (release)"),
  list(id = 2002, name = "windows-latest / x86_64 (release)"),
  list(
    id = 2003,
    name = "windows-latest / x86_64 (R 3.6.3 / Rtools35)"
  ),
  list(id = 2004, name = "Verify required check jobs")
)
jobs <- lapply(job_specs, function(spec) {
  list(
    id = spec$id,
    run_id = as.numeric(run_id),
    run_attempt = 1L,
    head_sha = harness_commit,
    name = spec$name,
    status = "completed",
    conclusion = "success",
    url = paste0(
      "https://api.github.com/repos/mlr-org/paradox/actions/jobs/", spec$id
    ),
    html_url = paste0(
      "https://github.com/mlr-org/paradox/actions/runs/", run_id,
      "/job/", spec$id
    ),
    steps = if (identical(spec$name, "Verify required check jobs")) {
      list(list(
        name = "Verify required job conclusions",
        number = 2L,
        status = "completed",
        conclusion = "success"
      ))
    } else if (identical(
        spec$name,
        "windows-latest / x86_64 (R 3.6.3 / Rtools35)"
      )) {
      make_steps(old_windows_steps)
    } else {
      make_steps(required_steps)
    }
  )
})
write_json(
  list(total_count = length(jobs), jobs = jobs),
  file.path(fixture, "jobs.json")
)
for (spec in job_specs) {
  writeLines("retained fixture job log", file.path(
    fixture, paste0("job-", spec$id, ".log")
  ))
}

artifact_specs <- list(
  list(
    id = 3001,
    name = "paradox-2.0.0-portability-macos-15-arm64",
    runner_os = "macOS",
    runner_arch = "ARM64",
    r_platform = "aarch64-apple-darwin23"
  ),
  list(
    id = 3002,
    name = "paradox-2.0.0-portability-windows-latest-x86_64",
    runner_os = "Windows",
    runner_arch = "X64",
    r_platform = "x86_64-w64-mingw32"
  ),
  list(
    id = 3003,
    name = "paradox-2.0.0-portability-windows-r3.6.3-x86_64",
    runner_os = "Windows",
    runner_arch = "X64",
    r_platform = "x86_64-w64-mingw32"
  )
)
artifacts <- lapply(artifact_specs, function(spec) {
  artifact_root <- file.path(fixture, "artifacts", spec$name)
  dir.create(file.path(artifact_root, "ci-evidence"), recursive = TRUE)
  dir.create(file.path(
    artifact_root, "check", "paradox.Rcheck"
  ), recursive = TRUE)
  writeLines(c(
    paste0("workflow_sha=", harness_commit),
    paste0("workflow_ref=", workflow_ref),
    paste0("checked_out_sha=", harness_commit),
    paste0("runner_os=", spec$runner_os),
    paste0("runner_arch=", spec$runner_arch),
    paste0("r_platform=", spec$r_platform)
  ), file.path(artifact_root, "ci-evidence", "provenance.txt"))
  check_log <- file.path(
    artifact_root, "check", "paradox.Rcheck", "00check.log"
  )
  if (identical(
      spec$name,
      "paradox-2.0.0-portability-windows-r3.6.3-x86_64"
    )) {
    writeLines(c(
      "* checking package dependencies ... NOTE",
      "Packages suggested but not available for checking:",
      paste0(
        "  'callr', 'reticulate', 'rmarkdown', 'mlr3learners', ",
        "'e1071', 'knitr',"
      ),
      "  'lhs', 'spacefillr', 'testthat'",
      "* checking whether package can be loaded ... OK",
      "* DONE",
      "Status: 1 NOTE"
    ), check_log)
  } else {
    writeLines(c("* DONE", "Status: OK"), check_log)
  }
  if (identical(
      spec$name,
      "paradox-2.0.0-portability-windows-r3.6.3-x86_64"
    )) {
    write_old_windows_isolation_fixture(artifact_root)
    closure <- file.path(artifact_root, "runtime-closure")
    dir.create(closure)
    lock <- file.path(
      root, "environment", "runtime-r-3.6.3-packages.lock"
    )
    retained_lock <- file.path(
      closure, "runtime-r-3.6.3-packages.lock"
    )
    if (!file.copy(lock, retained_lock)) {
      fail("could not retain the synthetic old-Windows runtime lock")
    }
    lock_sha256 <- unname(tools::sha256sum(retained_lock))
    writeLines(lock_sha256, file.path(closure, "lock-sha256.txt"))
    locked <- utils::read.delim(
      retained_lock,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      quote = "",
      comment.char = ""
    )
    packages <- c(
      "backports", "checkmate", "data.table", "R6",
      "cli", "digest", "mlr3misc"
    )
    selected <- locked[match(packages, locked$Package), , drop = FALSE]
    ledger <- data.frame(
      Package = selected$Package,
      Version = selected$Version,
      Role = selected$Role,
      SHA256 = selected$SHA256,
      SourceURL = selected$URL,
      Archive = paste0(
        selected$Package, "_", selected$Version, ".tar.gz"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    utils::write.table(
      ledger,
      file.path(closure, "sources.tsv"),
      sep = "\t",
      quote = FALSE,
      row.names = FALSE
    )
    writeLines(c(
      "Tool\tPath\tSHA256",
      paste0(
        "gcc\tC:/Rtools/mingw_64/bin/gcc.exe\t",
        "2d415b0fd5eacb43268e2ddf080b50f706d9fa2465b1e32d04f54ce936fac3da"
      ),
      paste0(
        "g++\tC:/Rtools/mingw_64/bin/g++.exe\t",
        "0d3d581bca702c777fc045a2fe69696e5979d86e819efe2350e2ac43f33f2b7f"
      ),
      paste0(
        "objdump\tC:/Rtools/mingw_64/bin/objdump.exe\t",
        "cbf5f996ef759be73502387c9d1296176f8bb7b6320b63cfb61371f7a98e7b59"
      ),
      paste0(
        "make\tC:/Rtools/bin/make.exe\t",
        "ce462e4ca812718a077ae4b67ebec0bd2df0e7a3bc1e31897e40895023e13c72"
      )
    ), file.path(closure, "rtools35.tsv"))
    writeLines(c(
      "Field\tValue",
      "gxx_path\tC:/Rtools/mingw_64/bin/g++.exe",
      paste0(
        "gxx_first_line\tg++.exe ",
        "(x86_64-posix-seh, Built by MinGW-W64 project) 4.9.3"
      ),
      "standard\t-std=gnu++11",
      "source\tSpookyV2.cpp",
      "source\tcrc32c.cpp",
      "source\tcrc32c_portable.cpp",
      "source\tspooky_serialize.cpp"
    ), file.path(closure, "digest-cxx11.tsv"))
    writeLines(c(
      "g++ -I. -std=gnu++11 -c SpookyV2.cpp -o SpookyV2.o",
      "g++ -I. -std=gnu++11 -c crc32c.cpp -o crc32c.o",
      paste0(
        "g++ -I. -std=gnu++11 -c crc32c_portable.cpp ",
        "-o crc32c_portable.o"
      ),
      paste0(
        "g++ -I. -std=gnu++11 -c spooky_serialize.cpp ",
        "-o spooky_serialize.o"
      )
    ), file.path(closure, "digest-install.log"))
    writeLines(c(
      "r_version=3.6.3",
      "r_platform=x86_64-w64-mingw32",
      "r_arch=x86_64",
      "package_archive=paradox_2.0.0.tar.gz",
      paste0("package_archive_sha256=", paste(rep("c", 64L), collapse = "")),
      "dll=C:/p36/package-library/paradox/libs/x64/paradox.dll",
      "registered_call_routines=79",
      "compiler=C:/Rtools/mingw_64/bin/gcc.exe",
      paste0(
        "compiler_first_line=gcc.exe ",
        "(x86_64-posix-seh, Built by MinGW-W64 project) 4.9.3"
      ),
      "formatted_diagnostics=pass",
      "smoke=pass"
    ), file.path(artifact_root, "old-windows-summary.txt"))
    writeLines(c(
      "paradox.dll: file format pei-x86-64",
      "architecture: i386:x86-64, flags 0x0000012f"
    ), file.path(artifact_root, "paradox-dll-objdump.txt"))
    writeLines(c(
      "R version 3.6.3 (2020-02-29)",
      "Platform: x86_64-w64-mingw32/x64 (64-bit)"
    ), file.path(artifact_root, "session-info.txt"))
  }
  archive_root <- file.path(fixture, "archives")
  dir.create(archive_root, showWarnings = FALSE)
  archive_path <- file.path(archive_root, paste0(spec$name, ".zip"))
  python <- file.path(root, ".local", "toolchain", "bin", "python")
  if (!file.exists(python)) {
    fail("the pinned local Python is required for the portability verifier fixture")
  }
  archive_inputs <- list.files(
    artifact_root,
    recursive = FALSE,
    full.names = FALSE,
    all.files = TRUE,
    no.. = TRUE
  )
  old_working_directory <- setwd(artifact_root)
  zip_status <- tryCatch(
    system2(
      python,
      args = c(
        "-m", "zipfile", "-c", shQuote(archive_path), shQuote(archive_inputs)
      ),
      stdout = FALSE,
      stderr = FALSE
    ),
    finally = setwd(old_working_directory)
  )
  if (!file.exists(archive_path) || zip_status != 0L) {
    fail("could not create a synthetic portability artifact ZIP")
  }
  list(
    id = spec$id,
    name = spec$name,
    size_in_bytes = unname(file.info(archive_path, extra_cols = FALSE)$size),
    expired = FALSE,
    digest = paste0("sha256:", unname(tools::sha256sum(archive_path))),
    url = paste0(
      "https://api.github.com/repos/mlr-org/paradox/actions/artifacts/",
      spec$id
    ),
    archive_download_url = paste0(
      "https://api.github.com/repos/mlr-org/paradox/actions/artifacts/",
      spec$id, "/zip"
    ),
    workflow_run = list(
      id = as.numeric(run_id),
      head_branch = harness_tag,
      head_sha = harness_commit
    )
  )
})
write_json(
  list(total_count = length(artifacts), artifacts = artifacts),
  file.path(fixture, "artifacts.json")
)

retained_verifier <- file.path(fixture, "verify-portability-ci-evidence.R")
if (!file.copy(verifier, retained_verifier)) {
  fail("could not retain the synthetic verifier source")
}
write_manifest <- function(name, members) {
  paths <- file.path(fixture, members)
  if (any(!file.exists(paths)) || any(dir.exists(paths))) {
    fail("synthetic manifest member is absent: ", name)
  }
  writeLines(
    paste0(unname(tools::sha256sum(paths)), "  ", members),
    file.path(fixture, name)
  )
}
archive_members <- file.path("archives", paste0(
  vapply(artifact_specs, `[[`, character(1L), "name"), ".zip"
))
artifact_files <- list.files(
  file.path(fixture, "artifacts"),
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  no.. = TRUE
)
artifact_files <- artifact_files[!dir.exists(artifact_files)]
artifact_members <- substring(
  normalizePath(artifact_files, winslash = "/", mustWork = TRUE),
  nchar(normalizePath(fixture, winslash = "/", mustWork = TRUE)) + 2L
)
job_members <- paste0(
  "job-", vapply(job_specs, `[[`, numeric(1L), "id"), ".log"
)
metadata_members <- c(
  "run.json", "jobs.json", "artifacts.json", "r-cmd-check.yml"
)
manifest_member_count <- length(archive_members) + length(artifact_members) +
  length(job_members) + length(metadata_members) + 2L + 5L
writeLines(c(
  "portability_ci_evidence=passed",
  paste0("run_id=", run_id),
  paste0("run_attempt=", run_attempt),
  "jobs=4",
  "artifacts=3",
  "sha_manifests=6",
  paste0("sha_manifest_members=", manifest_member_count)
), file.path(fixture, "verifier-acceptance.log"))
write_manifest("ARCHIVE-SHA256SUMS", archive_members)
write_manifest("ARTIFACT-SHA256SUMS", artifact_members)
write_manifest("JOB-LOG-SHA256SUMS", job_members)
write_manifest("METADATA-SHA256SUMS", metadata_members)
write_manifest(
  "VERIFIER-SHA256SUMS",
  c("verify-portability-ci-evidence.R", "verifier-acceptance.log")
)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)

arguments <- c(
  verifier,
  fixture,
  "--run-id", run_id,
  "--run-attempt", run_attempt,
  "--harness-commit", harness_commit,
  "--harness-tag", harness_tag,
  "--candidate-commit", candidate_commit,
  "--workflow-sha256", workflow_sha256
)
invoke <- function() {
  suppressWarnings(system2(
    file.path(root, ".local", "toolchain", "bin", "Rscript"),
    args = c("--vanilla", shQuote(arguments)),
    stdout = TRUE,
    stderr = TRUE
  ))
}
`%||%` <- function(left, right) if (is.null(left)) right else left
status <- function(result) attr(result, "status") %||% 0L

result <- invoke()
if (status(result) != 0L ||
    !any(result == "portability_ci_evidence=passed")) {
  fail("valid synthetic portability evidence was rejected:\n", paste(
    result, collapse = "\n"
  ))
}

old_windows_closure <- file.path(
  fixture,
  "artifacts",
  "paradox-2.0.0-portability-windows-r3.6.3-x86_64",
  "runtime-closure"
)
expect_rejection <- function(fragment, label) {
  result <- invoke()
  if (status(result) == 0L ||
      !any(grepl(fragment, result, fixed = TRUE))) {
    fail(label, ":\n", paste(result, collapse = "\n"))
  }
  invisible(result)
}
old_windows_isolation <- file.path(
  fixture,
  "artifacts",
  "paradox-2.0.0-portability-windows-r3.6.3-x86_64",
  "environment-isolation"
)
closure_isolation <- file.path(old_windows_isolation, "closure.tsv")
closure_isolation_lines <- readLines(closure_isolation, warn = FALSE)
writeLines(
  sub(
    "^cleared\tCC\tabsent\t-$",
    "cleared\tCC\tC:/hostile/cc.exe\t-",
    closure_isolation_lines
  ),
  closure_isolation
)
expect_rejection(
  "closure isolation receipt differs from the reviewed hostile-input policy",
  "verifier accepted an ambient old-Windows compiler override"
)
writeLines(closure_isolation_lines, closure_isolation)

writeLines(
  sub(
    paste0(
      "^file\tR_MAKEVARS_USER\t",
      "C:/p36-isolation/files/Makevars.user\t[0-9a-f]{64}$"
    ),
    paste0(
      "file\tR_MAKEVARS_USER\tC:/p36-isolation/files/Makevars.user\t",
      paste(rep("0", 64L), collapse = "")
    ),
    closure_isolation_lines
  ),
  closure_isolation
)
expect_rejection(
  "closure isolation receipt differs from the reviewed hostile-input policy",
  "verifier accepted changed empty-Makevars evidence"
)
writeLines(closure_isolation_lines, closure_isolation)

candidate_isolation <- file.path(old_windows_isolation, "candidate.tsv")
candidate_isolation_bytes <- readBin(
  candidate_isolation,
  what = "raw",
  n = file.info(candidate_isolation, extra_cols = FALSE)$size
)
unlink(candidate_isolation)
expect_rejection(
  "isolation receipt inventory differs from three phases",
  "verifier accepted missing candidate isolation evidence"
)
writeBin(candidate_isolation_bytes, candidate_isolation)

rtools35 <- file.path(old_windows_closure, "rtools35.tsv")
rtools35_lines <- readLines(rtools35, warn = FALSE)
for (tool in c("gcc", "g++", "objdump", "make")) {
  row <- which(startsWith(rtools35_lines, paste0(tool, "\t")))
  if (length(row) != 1L) {
    fail("synthetic Rtools35 evidence lacks exact row for ", tool)
  }
  changed <- rtools35_lines
  changed[[row]] <- sub("[0-9a-f]$", "0", changed[[row]])
  writeLines(changed, rtools35)
  expect_rejection(
    "Rtools35 executable identities differ from official bytes",
    paste0("verifier accepted a forged Rtools35 ", tool, " SHA-256")
  )
  writeLines(rtools35_lines, rtools35)
}
writeLines(
  sub(
    "g\\+\\+\tC:/Rtools/mingw_64/bin/g\\+\\+\\.exe",
    "g++\tC:/other/mingw_64/bin/g++.exe",
    rtools35_lines
  ),
  rtools35
)
expect_rejection(
  "Rtools35 executable identities differ from official bytes",
  "verifier accepted a forged Rtools35 G++ path"
)
writeLines(rtools35_lines, rtools35)

digest_cxx11 <- file.path(old_windows_closure, "digest-cxx11.tsv")
digest_cxx11_lines <- readLines(digest_cxx11, warn = FALSE)
writeLines(
  digest_cxx11_lines[!startsWith(digest_cxx11_lines, "standard\t")],
  digest_cxx11
)
expect_rejection(
  "digest C++11 evidence differs from exact Rtools35 policy",
  "verifier accepted missing digest language-standard evidence"
)
writeLines(digest_cxx11_lines, digest_cxx11)

writeLines(
  digest_cxx11_lines[
    seq_along(digest_cxx11_lines) !=
      which(digest_cxx11_lines == "source\tspooky_serialize.cpp")
  ],
  digest_cxx11
)
expect_rejection(
  "digest C++11 evidence differs from exact Rtools35 policy",
  "verifier accepted an omitted digest translation unit"
)
writeLines(digest_cxx11_lines, digest_cxx11)

writeLines(
  sub(
    "^source\tspooky_serialize\\.cpp$",
    "source\tcrc32c_portable.cpp",
    digest_cxx11_lines
  ),
  digest_cxx11
)
expect_rejection(
  "digest C++11 evidence differs from exact Rtools35 policy",
  "verifier accepted duplicated digest translation-unit evidence"
)
writeLines(digest_cxx11_lines, digest_cxx11)

writeLines(
  sub(
    "^gxx_path\tC:/Rtools/mingw_64/bin/g\\+\\+\\.exe$",
    "gxx_path\tC:/other/mingw_64/bin/g++.exe",
    digest_cxx11_lines
  ),
  digest_cxx11
)
expect_rejection(
  "digest C++11 evidence differs from exact Rtools35 policy",
  "verifier accepted a forged digest compiler path"
)
writeLines(digest_cxx11_lines, digest_cxx11)

writeLines(
  sub(
    paste0(
      "^gxx_first_line\tg\\+\\+\\.exe ",
      "\\(x86_64-posix-seh, Built by MinGW-W64 project\\) 4\\.9\\.3$"
    ),
    "gxx_first_line\tg++.exe (GCC) 14.9.3-forged",
    digest_cxx11_lines
  ),
  digest_cxx11
)
expect_rejection(
  "digest C++11 evidence differs from exact Rtools35 policy",
  "verifier accepted a forged digest compiler banner"
)
writeLines(digest_cxx11_lines, digest_cxx11)

digest_install_log <- file.path(old_windows_closure, "digest-install.log")
digest_install_lines <- readLines(digest_install_log, warn = FALSE)
writeLines(
  sub("-std=gnu\\+\\+11", "", digest_install_lines),
  digest_install_log
)
expect_rejection(
  "digest compile command is not exact Rtools35 C++11",
  "verifier accepted a digest compile command without its C++11 flag"
)
writeLines(digest_install_lines, digest_install_log)

writeLines(
  sub("-std=gnu\\+\\+11", "-std=gnu++14", digest_install_lines),
  digest_install_log
)
expect_rejection(
  "digest compile command is not exact Rtools35 C++11",
  "verifier accepted a changed digest compile-command standard"
)
writeLines(digest_install_lines, digest_install_log)

writeLines(
  sub("spooky_serialize\\.cpp", "unreviewed.cpp", digest_install_lines),
  digest_install_log
)
expect_rejection(
  "digest install log does not contain one command for spooky_serialize.cpp",
  "verifier accepted a compile log that omitted one reviewed translation unit"
)
writeLines(digest_install_lines, digest_install_log)

old_windows_summary <- file.path(
  fixture,
  "artifacts",
  "paradox-2.0.0-portability-windows-r3.6.3-x86_64",
  "old-windows-summary.txt"
)
old_windows_summary_lines <- readLines(old_windows_summary, warn = FALSE)
writeLines(
  sub(
    "^r_version=3\\.6\\.3$",
    "r_version=3.6.4",
    old_windows_summary_lines
  ),
  old_windows_summary
)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl(
      "summary does not authenticate R 3.6.3/Rtools35",
      result,
      fixed = TRUE
    ))) {
  fail("verifier accepted forged old-Windows runtime evidence")
}
writeLines(old_windows_summary_lines, old_windows_summary)

writeLines(
  sub(
    "^compiler=C:/Rtools/mingw_64/bin/gcc\\.exe$",
    "compiler=C:/other/mingw_64/bin/gcc.exe",
    old_windows_summary_lines
  ),
  old_windows_summary
)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl(
      "summary does not authenticate R 3.6.3/Rtools35",
      result,
      fixed = TRUE
    ))) {
  fail("verifier accepted a compiler from an unauthenticated Rtools path")
}
writeLines(old_windows_summary_lines, old_windows_summary)

writeLines(
  sub(
    paste0(
      "^compiler_first_line=gcc\\.exe ",
      "\\(x86_64-posix-seh, Built by MinGW-W64 project\\) 4\\.9\\.3$"
    ),
    "compiler_first_line=gcc.exe (GCC) 14.9.3-forged",
    old_windows_summary_lines
  ),
  old_windows_summary
)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl(
      "summary does not authenticate R 3.6.3/Rtools35",
      result,
      fixed = TRUE
    ))) {
  fail("verifier accepted an unbounded old-Windows GCC version match")
}
writeLines(old_windows_summary_lines, old_windows_summary)

old_windows_log <- file.path(
  fixture,
  "artifacts",
  "paradox-2.0.0-portability-windows-r3.6.3-x86_64",
  "check",
  "paradox.Rcheck",
  "00check.log"
)
old_windows_log_lines <- readLines(old_windows_log, warn = FALSE)
writeLines(
  sub(
    "'testthat'$",
    "'testthat', 'unexpected'",
    old_windows_log_lines
  ),
  old_windows_log
)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl(
      "does not contain only the exact unavailable-Suggests NOTE",
      result,
      fixed = TRUE
    ))) {
  fail("verifier accepted an unreviewed old-Windows check NOTE")
}
writeLines(old_windows_log_lines, old_windows_log)

failed_completion_jobs <- jobs
completion_index <- which(vapply(
  failed_completion_jobs,
  function(job) identical(job$name, "Verify required check jobs"),
  logical(1L)
))
failed_completion_jobs[[completion_index]]$conclusion <- "failure"
write_json(
  list(total_count = length(failed_completion_jobs), jobs = failed_completion_jobs),
  file.path(fixture, "jobs.json")
)
write_manifest("METADATA-SHA256SUMS", metadata_members)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("job.conclusion differs", result, fixed = TRUE))) {
  fail("verifier accepted a failed required-job completion gate")
}
write_json(
  list(total_count = length(jobs), jobs = jobs),
  file.path(fixture, "jobs.json")
)
write_manifest("METADATA-SHA256SUMS", metadata_members)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)

mac_log <- file.path(
  fixture, "artifacts", artifact_specs[[1L]]$name,
  "check", "paradox.Rcheck", "00check.log"
)
writeLines(c("Status: OK", "unexpected trailing output"), mac_log)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("lacks one sole, final clean status", result, fixed = TRUE))) {
  fail("verifier accepted a check log with output after Status: OK")
}
writeLines(c("* DONE", "Status: OK"), mac_log)

mac_archive <- file.path(
  fixture, "archives", paste0(artifact_specs[[1L]]$name, ".zip")
)
archive_bytes <- readBin(mac_archive, what = "raw", n = file.info(mac_archive)$size)
writeBin(c(archive_bytes, as.raw(0L)), mac_archive)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("differs from its REST digest", result, fixed = TRUE))) {
  fail("verifier accepted an artifact archive with the wrong digest")
}
writeBin(archive_bytes, mac_archive)

job_log <- file.path(fixture, "job-2001.log")
writeLines(c(readLines(job_log), "tamper"), job_log)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("manifest content differs", result, fixed = TRUE))) {
  fail("verifier accepted a file that differs from its SHA-256 manifest")
}
writeLines("retained fixture job log", job_log)

writeLines(c("* ALTERED BUT VALID", "Status: OK"), mac_log)
write_manifest("ARTIFACT-SHA256SUMS", artifact_members)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("content differs from its retained extracted tree", result,
      fixed = TRUE))) {
  fail("verifier did not bind the extracted artifact tree to its raw ZIP")
}
writeLines(c("* DONE", "Status: OK"), mac_log)
write_manifest("ARTIFACT-SHA256SUMS", artifact_members)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)

wrong_size_artifacts <- artifacts
wrong_size_artifacts[[1L]]$size_in_bytes <-
  wrong_size_artifacts[[1L]]$size_in_bytes + 1
write_json(
  list(total_count = length(wrong_size_artifacts), artifacts = wrong_size_artifacts),
  file.path(fixture, "artifacts.json")
)
write_manifest("METADATA-SHA256SUMS", metadata_members)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("archive size differs from REST metadata", result,
      fixed = TRUE))) {
  fail("verifier accepted an artifact archive with the wrong REST size")
}
write_json(
  list(total_count = length(artifacts), artifacts = artifacts),
  file.path(fixture, "artifacts.json")
)
write_manifest("METADATA-SHA256SUMS", metadata_members)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)

acceptance_log <- file.path(fixture, "verifier-acceptance.log")
acceptance_lines <- readLines(acceptance_log, warn = FALSE)
writeLines(c(acceptance_lines, "unexpected trailing output"), acceptance_log)
write_manifest(
  "VERIFIER-SHA256SUMS",
  c("verify-portability-ci-evidence.R", "verifier-acceptance.log")
)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("acceptance receipt differs from deterministic output", result,
      fixed = TRUE))) {
  fail("verifier accepted a non-deterministic success receipt")
}
writeLines(acceptance_lines, acceptance_log)
write_manifest(
  "VERIFIER-SHA256SUMS",
  c("verify-portability-ci-evidence.R", "verifier-acceptance.log")
)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)

evidence_manifest <- file.path(fixture, "EVIDENCE-SHA256SUMS")
evidence_manifest_bytes <- readBin(
  evidence_manifest, what = "raw", n = file.info(evidence_manifest)$size
)
unlink(evidence_manifest)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("inventory differs from the six required", result, fixed = TRUE))) {
  fail("verifier accepted a missing required SHA-256 manifest")
}
writeBin(evidence_manifest_bytes, evidence_manifest)

writeLines(
  paste0(paste(rep("0", 64L), collapse = ""), "  run.json"),
  file.path(fixture, "EXTRA-SHA256SUMS")
)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("inventory differs from the six required", result, fixed = TRUE))) {
  fail("verifier accepted an extra SHA-256 manifest")
}
unlink(file.path(fixture, "EXTRA-SHA256SUMS"))

artifact_manifest <- file.path(fixture, "ARTIFACT-SHA256SUMS")
artifact_manifest_lines <- readLines(artifact_manifest, warn = FALSE)
writeLines(head(artifact_manifest_lines, -1L), artifact_manifest)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("does not cover its exact required", result, fixed = TRUE))) {
  fail("verifier accepted an incomplete artifact SHA-256 manifest")
}

cat("portability CI evidence verifier tests passed\n")
