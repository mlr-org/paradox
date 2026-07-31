downstream_evidence_profile_is_symbolic <- function(path) {
  link <- Sys.readlink(path)
  length(link) == 1L && !is.na(link) && nzchar(link)
}

downstream_evidence_profile_require_file <- function(path, label) {
  if (length(path) != 1L || is.na(path) || !nzchar(path) ||
      !file.exists(path) || dir.exists(path) ||
      downstream_evidence_profile_is_symbolic(path)) {
    stop(label, " is absent, non-regular, or symbolic: ", path,
      call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

downstream_evidence_profile_read <- function(path, columns, label) {
  value <- utils::read.delim(
    path, header = TRUE, sep = "\t", quote = "", comment.char = "",
    colClasses = "character", check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (!identical(names(value), columns) || !nrow(value) || anyNA(value) ||
      any(!nzchar(unlist(value, use.names = FALSE))) ||
      any(grepl("[\r\n\t]", unlist(value, use.names = FALSE)))) {
    stop(label, " has an unexpected schema or empty value: ", path,
      call. = FALSE)
  }
  value
}

downstream_evidence_assert_candidate <- function(profile, ref, commit, tree,
                                                  version) {
  observed <- c(ref = ref, commit = commit, tree = tree, version = version)
  expected <- c(
    ref = profile$candidate_ref,
    commit = profile$candidate_commit,
    tree = profile$candidate_tree,
    version = profile$candidate_version
  )
  if (!identical(observed, expected)) {
    stop("candidate differs from the exact selected Paradox evidence axis",
      call. = FALSE)
  }
  invisible(TRUE)
}

downstream_evidence_profile <- function(root, profile = "default",
                                        axis = "paradox2") {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  if (downstream_evidence_profile_is_symbolic(root) ||
      length(profile) != 1L || is.na(profile) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$", profile) ||
      profile %in% c(".", "..") || length(axis) != 1L || is.na(axis) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,31}$", axis) ||
      axis %in% c(".", "..")) {
    stop("downstream evidence root or profile is unsafe", call. = FALSE)
  }
  compat <- normalizePath(file.path(root, "compat"), winslash = "/",
    mustWork = TRUE)
  if (!identical(dirname(compat), root) ||
      downstream_evidence_profile_is_symbolic(compat)) {
    stop("compatibility input directory is unsafe", call. = FALSE)
  }
  registry <- downstream_evidence_profile_require_file(file.path(
    compat, "downstream-evidence-profiles.tsv"), "profile registry")
  axis_registry <- downstream_evidence_profile_require_file(file.path(
    compat, "paradox-evidence-axes.tsv"), "Paradox evidence axis registry")
  profiles <- downstream_evidence_profile_read(
    registry,
    c("profile", "checkout_namespace", "repository_manifest", "snapshot",
      "bridge_provenance", "dependency_checkout_namespace",
      "dependency_repository_manifest", "dependency_snapshot",
      "install_order"),
    "profile registry"
  )
  safe_profile <- grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$",
    profiles$profile) & !profiles$profile %in% c(".", "..")
  safe_file <- function(value) {
    grepl("^[A-Za-z0-9][A-Za-z0-9._-]*[.]tsv$", value) &
      !value %in% c(".", "..")
  }
  safe_namespace <- function(value) {
    grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,95}$", value) &
      !value %in% c(".", "..")
  }
  if (any(!safe_profile) || anyDuplicated(profiles$profile) ||
      any(!safe_namespace(profiles$checkout_namespace)) ||
      any(!safe_namespace(profiles$dependency_checkout_namespace)) ||
      any(!safe_file(profiles$repository_manifest)) ||
      any(!safe_file(profiles$snapshot)) ||
      any(!safe_file(profiles$bridge_provenance)) ||
      any(!safe_file(profiles$dependency_repository_manifest)) ||
      any(!safe_file(profiles$dependency_snapshot))) {
    stop("profile registry contains an unsafe or duplicate identity",
      call. = FALSE)
  }
  index <- match(profile, profiles$profile)
  if (is.na(index)) {
    stop("unknown downstream evidence profile: ", profile, call. = FALSE)
  }
  row <- profiles[index, , drop = FALSE]
  axes <- downstream_evidence_profile_read(
    axis_registry,
    c("axis", "stage_suffix", "version_major", "candidate_ref",
      "candidate_commit", "candidate_tree", "candidate_version",
      "description"),
    "Paradox evidence axis registry"
  )
  normalized_axis_suffix <- axes$stage_suffix
  normalized_axis_suffix[normalized_axis_suffix == "-"] <- ""
  if (anyDuplicated(axes$axis) || any(!grepl(
      "^[A-Za-z0-9][A-Za-z0-9._-]{0,31}$", axes$axis)) ||
      any(!grepl("^(-[A-Za-z0-9][A-Za-z0-9._-]*)?$",
        normalized_axis_suffix)) || anyDuplicated(normalized_axis_suffix) ||
      any(!grepl("^[0-9]+$", axes$version_major)) ||
      any(!grepl("^refs/[A-Za-z0-9][A-Za-z0-9._/-]*$",
        axes$candidate_ref)) ||
      any(!grepl("^[0-9a-f]{40}$", axes$candidate_commit)) ||
      any(!grepl("^[0-9a-f]{40}$", axes$candidate_tree)) ||
      any(!grepl("^[0-9]+([.][0-9]+)+$", axes$candidate_version)) ||
      any(sub("[.].*$", "", axes$candidate_version) != axes$version_major)) {
    stop("Paradox evidence axis registry contains an unsafe or duplicate identity",
      call. = FALSE)
  }
  axis_index <- match(axis, axes$axis)
  if (is.na(axis_index)) {
    stop("unknown Paradox evidence axis: ", axis, call. = FALSE)
  }
  resolve <- function(name, label) {
    path <- downstream_evidence_profile_require_file(file.path(compat, name),
      label)
    if (!identical(dirname(path), compat) || !identical(basename(path), name)) {
      stop(label, " escaped the compatibility input directory", call. = FALSE)
    }
    path
  }
  repository_manifest <- resolve(row$repository_manifest[[1L]],
    "repository manifest")
  snapshot <- resolve(row$snapshot[[1L]], "repository snapshot")
  bridge_provenance <- resolve(row$bridge_provenance[[1L]],
    "bridge provenance")
  dependency_repository_manifest <- resolve(
    row$dependency_repository_manifest[[1L]],
    "dependency repository manifest"
  )
  dependency_snapshot <- resolve(row$dependency_snapshot[[1L]],
    "dependency repository snapshot")
  install_order <- strsplit(row$install_order[[1L]], ",", fixed = TRUE)[[1L]]
  if (!length(install_order) || any(!grepl(
      "^[A-Za-z0-9][A-Za-z0-9._-]*$", install_order)) ||
      anyDuplicated(install_order) ||
      !identical(paste(install_order, collapse = ","),
        row$install_order[[1L]])) {
    stop("profile install order is unsafe or duplicated", call. = FALSE)
  }

  repositories <- downstream_evidence_profile_read(
    repository_manifest,
    c("repository", "url", "relation", "priority", "action", "notes"),
    "repository manifest"
  )
  snapshots <- downstream_evidence_profile_read(
    snapshot,
    c("repository", "url", "priority", "commit", "commit_date", "branch"),
    "repository snapshot"
  )
  bridges <- downstream_evidence_profile_read(
    bridge_provenance,
    c("repository", "origin", "upstream_commit", "upstream_tree",
      "upstream_commit_date", "upstream_branch", "bridge_commit",
      "bridge_tree", "bridge_commit_date", "bridge_branch", "rationale"),
    "bridge provenance"
  )
  dependency_repositories <- downstream_evidence_profile_read(
    dependency_repository_manifest,
    c("repository", "url", "relation", "priority", "action", "notes"),
    "dependency repository manifest"
  )
  dependency_snapshots <- downstream_evidence_profile_read(
    dependency_snapshot,
    c("repository", "url", "priority", "commit", "commit_date", "branch"),
    "dependency repository snapshot"
  )
  safe_repository <- function(value) grepl(
    "^[A-Za-z0-9][A-Za-z0-9._-]*$", value)
  if (any(!safe_repository(repositories$repository)) ||
      any(!safe_repository(snapshots$repository)) ||
      any(!safe_repository(bridges$repository)) ||
      anyDuplicated(repositories$repository) ||
      anyDuplicated(snapshots$repository) || anyDuplicated(bridges$repository) ||
      any(!safe_repository(dependency_repositories$repository)) ||
      any(!safe_repository(dependency_snapshots$repository)) ||
      anyDuplicated(dependency_repositories$repository) ||
      anyDuplicated(dependency_snapshots$repository) ||
      !setequal(bridges$repository, install_order)) {
    stop("profile manifests contain an unsafe, duplicate, or incomplete repository set",
      call. = FALSE)
  }
  repository_index <- match(install_order, repositories$repository)
  snapshot_index <- match(install_order, snapshots$repository)
  bridge_index <- match(install_order, bridges$repository)
  if (anyNA(repository_index) || anyNA(snapshot_index) || anyNA(bridge_index)) {
    stop("profile install order is absent from one or more manifests",
      call. = FALSE)
  }
  ordered_repositories <- repositories[repository_index, , drop = FALSE]
  ordered_snapshots <- snapshots[snapshot_index, , drop = FALSE]
  ordered_bridges <- bridges[bridge_index, , drop = FALSE]
  if (!identical(ordered_repositories$url, ordered_snapshots$url) ||
      !identical(ordered_repositories$priority, ordered_snapshots$priority) ||
      !identical(ordered_bridges$origin, ordered_snapshots$url) ||
      !identical(ordered_bridges$bridge_commit, ordered_snapshots$commit) ||
      !identical(ordered_bridges$bridge_commit_date,
        ordered_snapshots$commit_date) ||
      !identical(ordered_bridges$bridge_branch, ordered_snapshots$branch) ||
      any(!grepl("^[0-9a-f]{40}$", c(ordered_bridges$upstream_commit,
        ordered_bridges$upstream_tree, ordered_bridges$bridge_commit,
        ordered_bridges$bridge_tree)))) {
    stop("profile snapshot and bridge provenance disagree", call. = FALSE)
  }
  dependency_repositories <- dependency_repositories[
    dependency_repositories$action == "clone", , drop = FALSE
  ]
  if (!identical(profile, "default") &&
      !setequal(dependency_repositories$repository, install_order)) {
    stop("non-default profile dependency manifest differs from its install order",
      call. = FALSE)
  }
  dependency_index <- match(dependency_repositories$repository,
    dependency_snapshots$repository)
  if (anyNA(dependency_index) ||
      !identical(dependency_repositories$url,
        dependency_snapshots$url[dependency_index]) ||
      !identical(dependency_repositories$priority,
        dependency_snapshots$priority[dependency_index]) ||
      any(!grepl("^[0-9a-f]{40}$",
        dependency_snapshots$commit[dependency_index]))) {
    stop("dependency manifest and snapshot disagree", call. = FALSE)
  }

  profile_suffix <- if (identical(profile, "default")) "" else
    paste0("-", profile)
  axis_suffix <- normalized_axis_suffix[[axis_index]]
  suffix <- if (identical(profile, "default")) axis_suffix else
    paste0(profile_suffix, "-", axis)
  all_suffixes <- unlist(lapply(profiles$profile, function(profile_name) {
    if (identical(profile_name, "default")) normalized_axis_suffix else
      paste0("-", profile_name, "-", axes$axis)
  }), use.names = FALSE)
  if (anyDuplicated(all_suffixes)) {
    stop("profile and axis stage identities collide", call. = FALSE)
  }

  list(
    profile = profile,
    axis = axis,
    profile_suffix = profile_suffix,
    axis_suffix = axis_suffix,
    suffix = suffix,
    version_major = axes$version_major[[axis_index]],
    candidate_ref = axes$candidate_ref[[axis_index]],
    candidate_commit = axes$candidate_commit[[axis_index]],
    candidate_tree = axes$candidate_tree[[axis_index]],
    candidate_version = axes$candidate_version[[axis_index]],
    registry = registry,
    axis_registry = axis_registry,
    checkout_namespace = row$checkout_namespace[[1L]],
    consumer_root = file.path(root, ".local", "compat",
      row$checkout_namespace[[1L]]),
    repository_manifest = repository_manifest,
    snapshot = snapshot,
    bridge_provenance = bridge_provenance,
    dependency_checkout_namespace =
      row$dependency_checkout_namespace[[1L]],
    dependency_consumer_root = file.path(root, ".local", "compat",
      row$dependency_checkout_namespace[[1L]]),
    dependency_repository_manifest = dependency_repository_manifest,
    dependency_snapshot = dependency_snapshot,
    install_order = install_order,
    hashes = c(
      profile_registry_sha256 = unname(tools::sha256sum(registry)),
      axis_registry_sha256 = unname(tools::sha256sum(axis_registry)),
      repository_manifest_sha256 = unname(tools::sha256sum(repository_manifest)),
      snapshot_sha256 = unname(tools::sha256sum(snapshot)),
      bridge_provenance_sha256 = unname(tools::sha256sum(bridge_provenance)),
      dependency_repository_manifest_sha256 =
        unname(tools::sha256sum(dependency_repository_manifest)),
      dependency_snapshot_sha256 =
        unname(tools::sha256sum(dependency_snapshot))
    )
  )
}
