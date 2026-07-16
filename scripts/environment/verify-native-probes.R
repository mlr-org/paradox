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

expected_routines <- c(
  design_transpose = 2L, design_transpose_logscale_builtin = 2L,
  design_dependency_runtime = 1L,
  design_dependency_plan_builtin = 2L, finalize_data_table = 1L,
  domain_check_builtin = 2L, domain_construct = 14L,
  domain_construct_frame = 1L, domain_builtin_runtime = 3L,
  domain_construct_builtin = 5L, domain_fct_grouping = 1L,
  domain_numeric_bounds_admit = 2L, domain_uty_check_result = 1L,
  domain_simple_repr_id = 1L, ps_builtin_runtime = 2L,
  ps_builtin_domains = 2L,
  domain_qunif_builtin = 2L, domain_sanitize_builtin = 2L,
  param_set_index_layout = 5L, param_set_construct = 1L,
  param_set_collection_construct = 4L,
  param_set_collection_detach_plan = 3L,
  param_set_collection_check_builtin = 5L,
  param_set_check_builtin = 3L,
  param_set_check_dt_builtin = 2L,
  param_set_check_dt_plan_builtin = 2L,
  param_set_check_dt_complete_builtin = 2L,
  param_set_check_dt_all_builtin = 2L, param_set_surface_auth = 2L,
  param_set_ids = 5L, param_set_ids_lazy = 2L,
  param_set_get_values = 3L, param_set_values_merge = 4L,
  param_set_store_values = 3L, param_set_assign_values_checked = 3L,
  param_set_collection_store_plan = 4L, param_set_property = 2L,
  param_set_qunif_builtin = 2L, sampler_unif_sample_builtin = 4L,
  generate_design_grid_builtin = 2L,
  param_set_trafo_plan = 2L,
  param_set_get_domain = 3L, param_set_domains = 2L,
  param_set_params = 2L, param_set_collection_params = 2L,
  param_set_collection_deps = 2L, param_set_collection_values = 2L,
  param_set_subset_state = 4L, param_set_subspace_state = 5L,
  param_set_subspace_states = 4L, param_set_adopt_subset_state = 2L,
  param_set_bulk_shell_register = 2L,
  param_set_bulk_generator_auth = 1L, param_set_bulk_shells = 2L,
  sampler_1d_unif_bulk_register = 2L,
  sampler_1d_unif_bulk_auth = 1L,
  sampler_1d_unif_bulk_shells = 4L,
  test_checked_affixed_size = 2L, test_stateful_altrep = 6L,
  test_stateful_altrep_rearm = 2L, test_gc_column_mutator = 3L
)
expected_fixture <- startsWith(names(expected_routines), "test_")
required_hazards <- c("hazard_altrep_snapshot_reentry",
  "hazard_callback_reentry_rooting", "hazard_finalizer_column_mutation",
  "hazard_finalize_names_alias")
expected_probe_ids <- stats::setNames(
  paste0("direct_", names(expected_routines)),
  names(expected_routines)
)
expected_probe_ids[["finalize_data_table"]] <- paste(
  expected_probe_ids[["finalize_data_table"]],
  "hazard_finalize_names_alias",
  sep = ","
)
for (routine in c("domain_check_builtin", "domain_qunif_builtin")) {
  expected_probe_ids[[routine]] <- paste(
    expected_probe_ids[[routine]],
    "hazard_callback_reentry_rooting",
    sep = ","
  )
}
expected_probe_ids[["param_set_values_merge"]] <- paste(
  expected_probe_ids[["param_set_values_merge"]],
  "hazard_altrep_snapshot_reentry",
  sep = ","
)
expected_probe_ids[["test_stateful_altrep"]] <- paste(
  expected_probe_ids[["test_stateful_altrep"]],
  "hazard_altrep_snapshot_reentry",
  "hazard_callback_reentry_rooting",
  sep = ","
)
expected_probe_ids[["test_gc_column_mutator"]] <- paste(
  expected_probe_ids[["test_gc_column_mutator"]],
  "hazard_finalizer_column_mutation",
  sep = ","
)

validate_manifest <- function(path) {
  manifest <- read.delim(path, sep = "\t", quote = "", comment.char = "",
    colClasses = "character", check.names = FALSE)
  columns <- c("routine", "arity", "surface", "coverage", "probe_ids", "reviewed_basis")
  if (!identical(names(manifest), columns)) fail("manifest columns differ")
  if (nrow(manifest) != length(expected_routines) || anyNA(manifest) ||
      any(!nzchar(as.matrix(manifest)))) fail("manifest is incomplete")
  if (anyDuplicated(manifest$routine)) fail("manifest contains duplicate routines")
  if (!identical(manifest$routine, names(expected_routines))) fail("manifest routine inventory/order differs")
  arity <- suppressWarnings(as.integer(manifest$arity))
  if (anyNA(arity) || !identical(unname(arity), unname(expected_routines)) ||
      !identical(as.character(arity), manifest$arity)) fail("manifest arity inventory differs")
  expected_surface <- ifelse(expected_fixture, "fixture", "production")
  expected_coverage <- ifelse(expected_fixture, "fixture-dynamic", "production-dynamic")
  if (!identical(manifest$surface, expected_surface)) fail("manifest surface classification differs")
  if (!identical(manifest$coverage, expected_coverage)) fail("manifest dynamic coverage classification differs")
  if (!identical(manifest$probe_ids, unname(expected_probe_ids))) {
    fail("manifest routine-to-probe mapping differs")
  }
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
  list(manifest = manifest, probe_ids = all_ids)
}

validate_result <- function(path, mode, manifest_info,
    expected_dso_sha256 = NULL) {
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
