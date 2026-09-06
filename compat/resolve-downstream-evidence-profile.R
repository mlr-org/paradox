arguments <- commandArgs(trailingOnly = TRUE)
if (!length(arguments) %in% 2:3) {
  stop("usage: resolve-downstream-evidence-profile.R ROOT PROFILE [AXIS]",
    call. = FALSE)
}
root <- normalizePath(arguments[[1L]], winslash = "/", mustWork = TRUE)
helper <- file.path(root, "compat", "downstream-evidence-profile.R")
if (!file.exists(helper) || dir.exists(helper) || nzchar(Sys.readlink(helper))) {
  stop("downstream evidence profile helper is absent or symbolic", call. = FALSE)
}
sys.source(helper, envir = environment(), keep.source = FALSE)
axis <- if (length(arguments) == 3L) arguments[[3L]] else "paradox2"
value <- downstream_evidence_profile(root, arguments[[2L]], axis)
fields <- c(
  profile = value$profile,
  axis = value$axis,
  version_major = value$version_major,
  candidate_ref = value$candidate_ref,
  candidate_commit = value$candidate_commit,
  candidate_tree = value$candidate_tree,
  candidate_version = value$candidate_version,
  suffix = if (nzchar(value$suffix)) value$suffix else "-",
  profile_suffix = if (nzchar(value$profile_suffix)) value$profile_suffix else "-",
  axis_suffix = if (nzchar(value$axis_suffix)) value$axis_suffix else "-",
  registry = value$registry,
  axis_registry = value$axis_registry,
  checkout_namespace = value$checkout_namespace,
  consumer_root = value$consumer_root,
  repository_manifest = value$repository_manifest,
  snapshot = value$snapshot,
  bridge_provenance = value$bridge_provenance,
  dependency_checkout_namespace = value$dependency_checkout_namespace,
  dependency_consumer_root = value$dependency_consumer_root,
  dependency_repository_manifest = value$dependency_repository_manifest,
  dependency_snapshot = value$dependency_snapshot,
  install_order = paste(value$install_order, collapse = ","),
  value$hashes
)
utils::write.table(
  data.frame(field = names(fields), value = unname(fields),
    stringsAsFactors = FALSE),
  stdout(), quote = FALSE, sep = "\t", row.names = FALSE,
  fileEncoding = "UTF-8"
)
