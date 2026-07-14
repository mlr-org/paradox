args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 6L) {
  stop(
    paste0(
      "Usage: compare.R BASELINE.rds CANDIDATE.rds REPORT.rds REPORT.txt ",
      "HARNESS-SHA256.tsv EXPECTED.tsv"
    ),
    call. = FALSE
  )
}

harness_manifest <- normalizePath(args[[5L]], mustWork = TRUE)
normalizer_path <- file.path(dirname(harness_manifest), "normalize.R")
normalizer <- new.env(parent = baseenv())
sys.source(normalizePath(normalizer_path, mustWork = TRUE), envir = normalizer)
if (!identical(normalizer$.differential_normalization_version, 3L)) {
  stop("Comparison requires differential normalization version 3", call. = FALSE)
}
harness <- normalizer$.differential_validate_harness_manifest(harness_manifest)
normalizer$.differential_assert_harness_path(harness, "normalizer", normalizer_path)
normalizer$.differential_assert_harness_path(
  harness,
  "compare",
  normalizer$.differential_current_script_path()
)
normalizer$.differential_assert_harness_path(
  harness,
  "expected-differences",
  args[[6L]]
)

baseline <- readRDS(args[[1L]])
candidate <- readRDS(args[[2L]])

if (!identical(baseline$format_version, 2L) ||
    !identical(candidate$format_version, 2L)) {
  stop("Comparison requires capture format version 2 on both sides", call. = FALSE)
}
if (!identical(baseline$normalization_version, normalizer$.differential_normalization_version) ||
    !identical(candidate$normalization_version, normalizer$.differential_normalization_version)) {
  stop("Capture normalization versions do not match the authenticated normalizer", call. = FALSE)
}
normalizer$.differential_assert_harness_record(
  baseline$metadata$harness,
  harness,
  "Baseline"
)
normalizer$.differential_assert_harness_record(
  candidate$metadata$harness,
  harness,
  "Candidate"
)
if (!identical(baseline$metadata$case_file_sha256, harness$file_sha256[["cases"]]) ||
    !identical(candidate$metadata$case_file_sha256, harness$file_sha256[["cases"]])) {
  stop("Capture case-file hashes do not match the authenticated harness", call. = FALSE)
}
if (!identical(names(baseline$cases), names(candidate$cases))) {
  stop("Baseline and candidate case names/order differ", call. = FALSE)
}
if (!identical(baseline$metadata$available_cases, candidate$metadata$available_cases) ||
    !identical(baseline$metadata$selected_cases, candidate$metadata$selected_cases)) {
  stop("Baseline and candidate case inventories/selections differ", call. = FALSE)
}

case_names <- names(baseline$cases)
equal <- vapply(case_names, function(name) {
  identical(baseline$cases[[name]], candidate$cases[[name]])
}, logical(1L))

details <- lapply(case_names[!equal], function(name) {
  comparison <- all.equal(
    baseline$cases[[name]],
    candidate$cases[[name]],
    check.attributes = TRUE
  )
  list(case = name, difference = as.character(comparison))
})
names(details) <- case_names[!equal]

fingerprint <- function(x) {
  bytes <- serialize(x, connection = NULL, ascii = FALSE, xdr = TRUE, version = 3L)
  path <- tempfile("paradox-differential-fingerprint-")
  connection <- NULL
  on.exit({
    if (!is.null(connection)) close(connection)
    unlink(path)
  })
  connection <- file(path, open = "wb")
  writeBin(bytes, connection)
  close(connection)
  connection <- NULL
  unname(tools::md5sum(path))
}

case_fingerprints <- data.frame(
  case = case_names,
  baseline_fingerprint = vapply(baseline$cases, fingerprint, character(1L)),
  candidate_fingerprint = vapply(candidate$cases, fingerprint, character(1L)),
  stringsAsFactors = FALSE
)

expected <- data.frame(
  case = character(),
  baseline_fingerprint = character(),
  candidate_fingerprint = character(),
  reason = character(),
  stringsAsFactors = FALSE
)
if (nzchar(args[[6L]])) {
  expected <- utils::read.delim(
    args[[6L]],
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character"
  )
  required_columns <- c("case", "baseline_fingerprint", "candidate_fingerprint", "reason")
  valid_fingerprint <- function(x) !is.na(x) & grepl("^[[:xdigit:]]{32}$", x)
  if (!identical(names(expected), required_columns) ||
      anyNA(expected$case) || any(!nzchar(expected$case)) || anyDuplicated(expected$case) ||
      any(!valid_fingerprint(expected$baseline_fingerprint)) ||
      any(!valid_fingerprint(expected$candidate_fingerprint)) ||
      anyNA(expected$reason) || any(!nzchar(expected$reason))) {
    stop(
      "Expected-difference manifest must contain unique nonempty cases, 32-digit baseline/candidate fingerprints, and reasons",
      call. = FALSE
    )
  }
  expected$baseline_fingerprint <- tolower(expected$baseline_fingerprint)
  expected$candidate_fingerprint <- tolower(expected$candidate_fingerprint)
  unknown_expected <- setdiff(expected$case, baseline$metadata$available_cases)
  if (length(unknown_expected)) {
    stop("Expected-difference manifest names unavailable cases: ", paste(unknown_expected, collapse = ", "), call. = FALSE)
  }
}
selected_expected <- intersect(expected$case, case_names)
observed_differences <- case_names[!equal]
allowlisted_observed <- intersect(observed_differences, selected_expected)
fingerprints_match <- vapply(allowlisted_observed, function(name) {
  actual <- case_fingerprints[match(name, case_fingerprints$case), ]
  reviewed <- expected[match(name, expected$case), ]
  identical(actual$baseline_fingerprint, reviewed$baseline_fingerprint) &&
    identical(actual$candidate_fingerprint, reviewed$candidate_fingerprint)
}, logical(1L))
expected_observed <- allowlisted_observed[fingerprints_match]
fingerprint_mismatches <- allowlisted_observed[!fingerprints_match]
unexpected <- setdiff(observed_differences, selected_expected)
missing_expected <- setdiff(selected_expected, observed_differences)

fingerprint_mismatch_details <- lapply(fingerprint_mismatches, function(name) {
  actual <- case_fingerprints[match(name, case_fingerprints$case), , drop = FALSE]
  reviewed <- expected[match(name, expected$case), , drop = FALSE]
  list(
    case = name,
    expected_baseline = reviewed$baseline_fingerprint,
    actual_baseline = actual$baseline_fingerprint,
    expected_candidate = reviewed$candidate_fingerprint,
    actual_candidate = actual$candidate_fingerprint
  )
})
names(fingerprint_mismatch_details) <- fingerprint_mismatches

report <- list(
  format_version = 2L,
  fingerprint_algorithm = "md5-r-serialize-v3-xdr",
  harness = normalizer$.differential_harness_record(harness),
  baseline_metadata = baseline$metadata,
  candidate_metadata = candidate$metadata,
  equal = equal,
  case_fingerprints = case_fingerprints,
  differences = details,
  expected_differences = expected[match(expected_observed, expected$case), , drop = FALSE],
  expected_difference_fingerprint_mismatches = fingerprint_mismatch_details,
  unexpected_differences = unexpected,
  missing_expected_differences = missing_expected
)
saveRDS(report, args[[3L]], version = 3L)

lines <- c(
  "paradox differential comparison",
  sprintf("baseline:  %s %s (%s)", baseline$metadata$label, baseline$metadata$revision, baseline$metadata$package_version),
  sprintf("candidate: %s %s (%s)", candidate$metadata$label, candidate$metadata$revision, candidate$metadata$package_version),
  sprintf(
    "cases: %d; equal: %d; different: %d (exactly expected: %d; fingerprint mismatch: %d; unexpected: %d)",
    length(equal), sum(equal), sum(!equal), length(expected_observed),
    length(fingerprint_mismatches), length(unexpected)
  )
)

if (any(!equal)) {
  for (name in case_names[!equal]) {
    classification <- if (name %in% expected_observed) {
      "EXACT EXPECTED DIFFERENCE"
    } else if (name %in% fingerprint_mismatches) {
      "EXPECTED CASE, DELTA FINGERPRINT MISMATCH"
    } else {
      "UNEXPECTED DIFFERENCE"
    }
    lines <- c(lines, "", sprintf("[%s] %s", name, classification))
    if (name %in% c(expected_observed, fingerprint_mismatches)) {
      lines <- c(lines, sprintf("  reason: %s", expected$reason[match(name, expected$case)]))
    }
    if (name %in% expected_observed) {
      fp <- case_fingerprints[match(name, case_fingerprints$case), ]
      lines <- c(
        lines,
        sprintf("  baseline fingerprint:  %s", fp$baseline_fingerprint),
        sprintf("  candidate fingerprint: %s", fp$candidate_fingerprint)
      )
    } else if (name %in% fingerprint_mismatches) {
      mismatch <- fingerprint_mismatch_details[[name]]
      lines <- c(
        lines,
        sprintf("  baseline expected:  %s", mismatch$expected_baseline),
        sprintf("  baseline actual:    %s", mismatch$actual_baseline),
        sprintf("  candidate expected: %s", mismatch$expected_candidate),
        sprintf("  candidate actual:   %s", mismatch$actual_candidate)
      )
    }
    case_details <- details[[name]]$difference
    if (length(case_details) > 40L) {
      case_details <- c(case_details[seq_len(40L)], "... difference output truncated; inspect report.rds")
    }
    lines <- c(lines, paste0("  ", case_details))
  }
} else {
  lines <- c(lines, "all selected observations are identical")
}

if (length(missing_expected)) {
  lines <- c(lines, "", "Expected differences that were not observed:")
  for (name in missing_expected) {
    lines <- c(lines, sprintf("  [%s] %s", name, expected$reason[match(name, expected$case)]))
    fp <- case_fingerprints[match(name, case_fingerprints$case), ]
    lines <- c(
      lines,
      sprintf("    baseline actual:  %s", fp$baseline_fingerprint),
      sprintf("    candidate actual: %s", fp$candidate_fingerprint)
    )
  }
}

gate_passes <- !length(unexpected) && !length(missing_expected) && !length(fingerprint_mismatches)
lines <- c(lines, "", if (gate_passes) "compatibility gate: PASS" else "compatibility gate: FAIL")

writeLines(lines, con = args[[4L]], useBytes = TRUE)
writeLines(lines)
quit(save = "no", status = if (gate_passes) 0L else 1L)
