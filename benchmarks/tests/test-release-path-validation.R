arguments <- commandArgs(FALSE)
script_argument <- grep("^--file=", arguments, value = TRUE)
if (length(script_argument) != 1L) {
  stop("could not identify release path-validation test location", call. = FALSE)
}
script <- normalizePath(
  substring(script_argument, nchar("--file=") + 1L),
  winslash = "/", mustWork = TRUE
)
release_script <- file.path(dirname(dirname(script)), "release.R")
tree <- parse(release_script, keep.source = FALSE)

is_control_binding <- function(expression) {
  is.call(expression) && length(expression) == 3L &&
    identical(expression[[1L]], quote(`<-`)) &&
    identical(expression[[2L]], quote(release_contains_control))
}
bindings <- Filter(is_control_binding, as.list(tree))
if (length(bindings) != 1L || !is.call(bindings[[1L]][[3L]]) ||
    !identical(bindings[[1L]][[3L]][[1L]], quote(`function`))) {
  stop("release control-character validator has an unexpected shape", call. = FALSE)
}

environment <- new.env(parent = baseenv())
eval(bindings[[1L]], envir = environment)
contains_control <- environment$release_contains_control

ordinary <- c(
  "/home/mewse/paradox_neo/.local/compat/differential/runs/example",
  "/home/mewse/paradox_neo/.local/R/library",
  "release-consumers-p1-afa5668-20260716",
  "return-runtime-candidate"
)
if (!identical(contains_control(ordinary), rep(FALSE, length(ordinary)))) {
  stop("ordinary release paths are classified as controls", call. = FALSE)
}

controls <- c("carriage\rreturn", "line\nfeed", "horizontal\ttab", "nul\001byte")
if (!identical(contains_control(controls), rep(TRUE, length(controls)))) {
  stop("release path controls are not rejected", call. = FALSE)
}

is_error_compactor_binding <- function(expression) {
  is.call(expression) && length(expression) == 3L &&
    identical(expression[[1L]], quote(`<-`)) &&
    identical(expression[[2L]], quote(release_compact_error))
}
compactor_bindings <- Filter(is_error_compactor_binding, as.list(tree))
if (length(compactor_bindings) != 1L) {
  stop("release error compactor has an unexpected shape", call. = FALSE)
}
eval(compactor_bindings[[1L]], envir = environment)
message <- "return runtime candidate\rline\nnext\ttab\001byte"
expected_message <- "return runtime candidate line next tab byte"
if (!identical(environment$release_compact_error(message), expected_message)) {
  stop("release error compactor changes letters or retains controls", call. = FALSE)
}

is_role_binding <- function(expression) {
  is.call(expression) && length(expression) == 3L &&
    identical(expression[[1L]], quote(`<-`)) &&
    identical(expression[[2L]], quote(release_library_roles))
}
role_bindings <- Filter(is_role_binding, as.list(tree))
if (length(role_bindings) != 1L) {
  stop("release library-role binding has an unexpected shape", call. = FALSE)
}
library_roles <- function(dependencies, protected) {
  fixture <- list2env(list(
    release_dependency_libraries = dependencies,
    release_extra_libraries = protected
  ), parent = baseenv())
  eval(role_bindings[[1L]], envir = fixture)
  fixture$release_library_roles
}
fixed_roles <- c("baseline", "candidate", "downstream-bridges")
suffix_roles <- c("ordinary-project", "r-base-library")
role_fixtures <- list(
  list(
    dependencies = "dependency",
    protected = character(),
    expected = c(fixed_roles[1:2], "dependency-1", fixed_roles[3], suffix_roles)
  ),
  list(
    dependencies = "dependency",
    protected = "protected",
    expected = c(
      fixed_roles[1:2], "dependency-1", fixed_roles[3], "protected-1",
      suffix_roles
    )
  ),
  list(
    dependencies = c("dependency-a", "dependency-b"),
    protected = c("protected-a", "protected-b", "protected-c"),
    expected = c(
      fixed_roles[1:2], "dependency-1", "dependency-2", fixed_roles[3],
      "protected-1", "protected-2", "protected-3", suffix_roles
    )
  )
)
for (fixture in role_fixtures) {
  observed <- library_roles(fixture$dependencies, fixture$protected)
  expected_length <- 5L + length(fixture$dependencies) + length(fixture$protected)
  if (!identical(observed, fixture$expected) ||
      length(observed) != expected_length) {
    stop("release library roles do not match their path inventory", call. = FALSE)
  }
}

is_library_binding <- function(expression, symbol) {
  is.call(expression) && length(expression) == 3L &&
    identical(expression[[1L]], quote(`<-`)) &&
    identical(expression[[2L]], symbol)
}
evaluate_library_binding <- function(symbol, values) {
  bindings <- Filter(
    function(expression) is_library_binding(expression, symbol),
    as.list(tree)
  )
  if (length(bindings) != 1L) {
    stop("release library binding has an unexpected shape: ", symbol,
      call. = FALSE)
  }
  fixture <- list2env(list(
    release_arguments = list(
      dependency_libraries = values,
      protected_libraries = values
    ),
    release_require_local_directory = function(path, label) path
  ), parent = baseenv())
  eval(bindings[[1L]], envir = fixture)
  fixture[[as.character(symbol)]]
}
for (symbol in list(
  quote(release_dependency_libraries), quote(release_extra_libraries)
)) {
  for (values in list(character(), "one", c("one", "two", "three"))) {
    observed <- evaluate_library_binding(symbol, values)
    if (!identical(observed, unname(values)) || !is.null(names(observed))) {
      stop("release library paths retain input names: ", symbol,
        call. = FALSE)
    }
  }
}

support_bindings <- Filter(
  function(expression) is_library_binding(
    expression, quote(release_support_libraries)
  ),
  as.list(tree)
)
if (length(support_bindings) != 1L) {
  stop("release support-library binding has an unexpected shape",
    call. = FALSE)
}
support_fixture <- list2env(list(
  release_bridge_library = "exact-downstream-bridge",
  release_dependency_libraries = c(
    "dependency-with-stale-bridges", "dependency-two"
  ),
  release_extra_libraries = "protected",
  release_ordinary_library = "ordinary"
), parent = baseenv())
eval(support_bindings[[1L]], envir = support_fixture)
expected_support_order <- c(
  "exact-downstream-bridge", "dependency-with-stale-bridges",
  "dependency-two", "protected", "ordinary"
)
if (!identical(
    support_fixture$release_support_libraries, expected_support_order
  )) {
  stop("the exact downstream bridge does not precede stale dependency copies",
    call. = FALSE)
}

pipeline_bindings <- Filter(
  function(expression) is_library_binding(
    expression, quote(release_pipeline_package)
  ),
  as.list(tree)
)
if (length(pipeline_bindings) != 1L ||
    !identical(pipeline_bindings[[1L]][[3L]][[1L]],
      quote(release_direct_package)) ||
    !identical(pipeline_bindings[[1L]][[3L]][[2L]], "mlr3pipelines") ||
    !identical(pipeline_bindings[[1L]][[3L]][[3L]],
      quote(release_bridge_library))) {
  stop("release benchmark does not attest reviewed mlr3pipelines from the bridge",
    call. = FALSE)
}

binding_for <- function(symbol) {
  bindings <- Filter(
    function(expression) is_library_binding(expression, symbol),
    as.list(tree)
  )
  if (length(bindings) != 1L) {
    stop("release binding has an unexpected shape: ", symbol, call. = FALSE)
  }
  bindings[[1L]]
}

expected_bridge <- binding_for(quote(release_expected_bridge_library))
if (!all(c("release_candidate_run_id", "release_profile", "suffix") %in%
    all.names(expected_bridge[[3L]], functions = TRUE, unique = TRUE))) {
  stop("release bridge path is not derived from the run/profile/axis suffix",
    call. = FALSE)
}
if ("mies_library" %in% all.names(tree, functions = TRUE, unique = TRUE)) {
  stop("release benchmark retains the caller-selected unsuffixed bridge seam",
    call. = FALSE)
}

tooling_authenticator <- binding_for(quote(release_require_tooling_files))
tooling_names <- all.names(tooling_authenticator[[3L]], functions = TRUE,
  unique = TRUE)
if (!"release_tooling_commit" %in% tooling_names ||
    "release_candidate_commit" %in% tooling_names) {
  stop("validation helpers are not authenticated against the tooling commit",
    call. = FALSE)
}
candidate_authenticator <- binding_for(quote(release_require_candidate_files))
candidate_names <- all.names(candidate_authenticator[[3L]], functions = TRUE,
  unique = TRUE)
if (!all(c("release_candidate_source", "release_candidate_commit") %in%
    candidate_names)) {
  stop("candidate differential helpers are not bound to the frozen source",
    call. = FALSE)
}

bridge_verification <- binding_for(quote(release_bridge_verification))
bridge_text <- paste(deparse(bridge_verification[[3L]], width.cutoff = 500L),
  collapse = "\n")
for (required in c(
    "--candidate-source", "--evidence-profile", "--paradox-axis"
  )) {
  if (!grepl(required, bridge_text, fixed = TRUE)) {
    stop("bridge verifier invocation omits ", required, call. = FALSE)
  }
}

source_text <- paste(readLines(release_script, warn = FALSE), collapse = "\n")
if (grepl("identical(head, release_candidate_commit)", source_text,
    fixed = TRUE) || !grepl(
    "list(commit = head, tree = tree, status = \"clean\")", source_text,
    fixed = TRUE
  )) {
  stop("validation root authentication still aliases tooling to the candidate",
    call. = FALSE)
}
for (required in c(
    "candidate-snapshots", "tooling_commit=", "tooling_tree=",
    "tooling_status=", "candidate_source_status=clean",
    "evidence_profile=", "paradox_axis=",
    "paired_metadata$repository$head, release_tooling_commit"
  )) {
  if (!grepl(required, source_text, fixed = TRUE)) {
    stop("release evidence omits validation identity binding: ", required,
      call. = FALSE)
  }
}

cat(
  paste(
    "PASS: release paths reject controls, library roles match optional paths,",
    "profile bridges have precedence, and candidate/tooling identities are",
    "separate and sealed\n"
  )
)
