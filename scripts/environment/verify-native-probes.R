#!/usr/bin/env Rscript

fail <- function(...) stop(..., call. = FALSE)

parse_options <- function(arguments) {
  arguments <- arguments[arguments != "--args"]
  values <- list()
  for (argument in arguments) {
    if (!startsWith(argument, "--") || !grepl("=", argument, fixed = TRUE)) {
      fail("arguments must use --name=value: ", argument)
    }
    pieces <- strsplit(sub("^--", "", argument), "=", fixed = TRUE)[[1L]]
    name <- pieces[[1L]]
    value <- paste(pieces[-1L], collapse = "=")
    if (!nzchar(name) || name %in% names(values)) fail("duplicate option: ", name)
    values[[name]] <- value
  }
  required <- c("manifest", "result", "mode")
  missing <- setdiff(required, names(values))
  if (length(missing)) fail("missing options: ", paste(missing, collapse = ", "))
  allowed <- c(required, "expected-dso-sha256")
  if (length(setdiff(names(values), allowed))) fail("unknown options supplied")
  values
}

required_hazards <- c("hazard_altrep_snapshot_reentry",
  "hazard_callback_reentry_rooting", "hazard_finalizer_column_mutation",
  "hazard_finalize_names_alias")

validate_manifest <- function(path) {
  manifest <- read.delim(path, sep = "\t", quote = "", comment.char = "",
    colClasses = "character", check.names = FALSE)
  columns <- c("routine", "arity", "surface", "coverage", "probe_ids", "reviewed_basis")
  if (!identical(names(manifest), columns)) fail("manifest columns differ")
  if (!nrow(manifest) || anyNA(manifest) ||
      any(!nzchar(as.matrix(manifest)))) fail("manifest is incomplete")
  if (anyDuplicated(manifest$routine)) fail("manifest contains duplicate routines")
  if (any(!grepl("^[a-z][a-z0-9_]*$", manifest$routine))) {
    fail("manifest contains an invalid routine name")
  }
  arity <- suppressWarnings(as.integer(manifest$arity))
  if (anyNA(arity) || any(arity < 0L) ||
      !identical(as.character(arity), manifest$arity)) {
    fail("manifest arity inventory differs")
  }
  expected_routines <- stats::setNames(arity, manifest$routine)
  expected_fixture <- startsWith(manifest$routine, "test_")
  expected_surface <- ifelse(expected_fixture, "fixture", "production")
  expected_coverage <- ifelse(expected_fixture, "fixture-dynamic", "production-dynamic")
  if (!identical(manifest$surface, expected_surface)) fail("manifest surface classification differs")
  if (!identical(manifest$coverage, expected_coverage)) fail("manifest dynamic coverage classification differs")
  split_ids <- strsplit(manifest$probe_ids, ",", fixed = TRUE)
  if (any(vapply(split_ids, function(ids) any(!grepl("^[a-z][a-z0-9_]*$", ids)) || anyDuplicated(ids), logical(1L)))) {
    fail("manifest has invalid or duplicate probe IDs")
  }
  direct <- paste0("direct_", manifest$routine)
  if (any(!vapply(seq_along(split_ids), function(index) direct[[index]] %in% split_ids[[index]], logical(1L)))) {
    fail("a routine lacks its independent direct probe")
  }
  all_ids <- unique(unlist(split_ids, use.names = FALSE))
  if (!setequal(intersect(all_ids, required_hazards), required_hazards)) fail("manifest lacks a required hazard")
  list(manifest = manifest, routines = expected_routines, probe_ids = all_ids)
}

validate_result <- function(path, mode, manifest_info,
    expected_dso_sha256 = NULL) {
  expected_routines <- manifest_info$routines
  if (!mode %in% c("plain", "gct", "valgrind")) fail("invalid expected mode")
  result <- read.delim(path, sep = "\t", quote = "", comment.char = "",
    colClasses = "character", check.names = FALSE)
  if (!identical(names(result), c("kind", "id", "status", "detail"))) fail("result columns differ")
  if (!nrow(result) || anyNA(result) || any(!nzchar(as.matrix(result)))) fail("result is empty or truncated")
  keys <- paste(result$kind, result$id, sep = "\t")
  if (anyDuplicated(keys)) fail("result contains duplicate records")
  allowed_kinds <- c("meta", "registration", "probe", "hazard", "gct", "summary")
  if (any(!result$kind %in% allowed_kinds) || any(!result$status %in% c("pass", "fail"))) fail("result has invalid kind/status")
  if (!identical(tail(keys, 1L), "summary\tcomplete")) fail("result lacks its terminal completion record")
  summaries <- result[result$kind == "summary", , drop = FALSE]
  if (nrow(summaries) != 1L || !identical(summaries$id, "complete")) {
    fail("result has an unexpected summary inventory")
  }
  summary <- result[nrow(result), , drop = FALSE]
  expected_summary <- paste0("records_before_summary=", nrow(result) - 1L, ";failures=0")
  if (!identical(summary$status, "pass") || !identical(summary$detail, expected_summary)) fail("result summary differs")
  if (any(result$status != "pass")) fail("result contains a failing record")

  required_meta <- c(schema = "1", mode = mode, candidate_version = "2.0.0",
    dynamic_lookup = "false", force_symbols = "true")
  for (name in names(required_meta)) {
    row <- result[result$kind == "meta" & result$id == name, , drop = FALSE]
    if (nrow(row) != 1L || !identical(row$detail, required_meta[[name]])) fail("meta record differs: ", name)
  }
  round_row <- result[result$kind == "meta" & result$id == "rounds", , drop = FALSE]
  rounds <- suppressWarnings(as.integer(round_row$detail))
  if (nrow(round_row) != 1L || is.na(rounds) || rounds < 1L || rounds > 100L) fail("round count is invalid")
  dso_row <- result[result$kind == "meta" & result$id == "dso_sha256", , drop = FALSE]
  if (nrow(dso_row) != 1L || !grepl("^[0-9a-f]{64}$", dso_row$detail)) fail("DSO hash meta record is invalid")
  if (!is.null(expected_dso_sha256)) {
    if (!grepl("^[0-9a-f]{64}$", expected_dso_sha256) ||
        !identical(dso_row$detail, expected_dso_sha256)) {
      fail("DSO hash meta record differs from the installed native library")
    }
  }
  gct_step_row <- result[result$kind == "meta" & result$id == "gct_step", , drop = FALSE]
  if (nrow(gct_step_row) != 1L) fail("GCT step meta record is absent or duplicate")
  if (mode == "gct") {
    gct_step <- suppressWarnings(as.integer(gct_step_row$detail))
    if (is.na(gct_step) || gct_step < 1L ||
        !identical(as.character(gct_step), gct_step_row$detail)) {
      fail("GCT step meta record is invalid")
    }
  } else if (!identical(gct_step_row$detail, "disabled")) {
    fail("GCT is unexpectedly enabled in a non-GCT result")
  }
  expected_meta_ids <- c("schema", "mode", "rounds", "gct_step", "candidate_version",
    "dso_sha256", "dynamic_lookup", "force_symbols")
  if (!setequal(result$id[result$kind == "meta"], expected_meta_ids)) fail("meta inventory differs")

  registrations <- result[result$kind == "registration", , drop = FALSE]
  if (!identical(registrations$id, names(expected_routines))) fail("registration inventory/order differs")
  if (!identical(registrations$detail, paste0("arity=", expected_routines))) fail("registration arity evidence differs")
  probes <- result[result$kind == "probe", , drop = FALSE]
  expected_direct <- paste0("direct_", names(expected_routines))
  if (!identical(probes$id, expected_direct)) fail("direct probe inventory/order differs")
  if (any(probes$detail != paste0("rounds=", rounds))) fail("direct probe round evidence differs")
  hazards <- result[result$kind == "hazard", , drop = FALSE]
  if (!identical(hazards$id, required_hazards)) fail("hazard inventory/order differs")
  if (any(hazards$detail != paste0("rounds=", rounds))) fail("hazard round evidence differs")
  gct <- result[result$kind == "gct", , drop = FALSE]
  if (mode == "gct") {
    expected_transition <- paste0("previous=0;active=", gct_step, ";restored=0")
    if (nrow(gct) != 1L || !identical(gct$id, "transition") ||
        !identical(gct$status, "pass") || !identical(gct$detail, expected_transition)) {
      fail("GCT activation/restoration evidence differs")
    }
  } else if (nrow(gct)) {
    fail("non-GCT result contains GCT transition evidence")
  }
  expected_all_probes <- c(expected_direct, required_hazards)
  if (!setequal(manifest_info$probe_ids, expected_all_probes)) fail("manifest includes an unknown or missing dynamic probe")
  invisible(TRUE)
}

options <- parse_options(commandArgs(trailingOnly = TRUE))
manifest_info <- validate_manifest(normalizePath(options$manifest, mustWork = TRUE))
validate_result(normalizePath(options$result, mustWork = TRUE), options$mode,
  manifest_info, options$`expected-dso-sha256`)
cat("targeted memory result verified\n")
