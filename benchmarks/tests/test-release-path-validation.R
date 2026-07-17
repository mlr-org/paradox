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
fixed_roles <- c("baseline", "candidate", "miesmuschel")
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

cat(
  "PASS: release paths reject controls and library roles match optional paths\n"
)
