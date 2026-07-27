#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("usage: runtime-matrix-probe.R CANDIDATE_LIBRARY", call. = FALSE)
}
candidate_library <- normalizePath(args[[1L]], mustWork = TRUE)
.libPaths(unique(c(candidate_library, .libPaths())))
library("paradox", character.only = TRUE, lib.loc = candidate_library)

runtime <- as.character(getRversion())
registry_path <- Sys.getenv("PARADOX_RUNTIME_MATRIX_REGISTRY_FILE", unset = "")
if (!nzchar(registry_path) || !file.exists(registry_path) ||
    nzchar(Sys.readlink(registry_path))) {
  stop("authenticated runtime registry is absent or symbolic", call. = FALSE)
}
registry <- read.delim(
  registry_path,
  header = TRUE,
  quote = "",
  comment.char = "",
  colClasses = "character",
  check.names = FALSE
)
if (!identical(
    names(registry),
    c(
      "ordinal", "runtime", "platform", "runtime_lock", "dependency_mode",
      "dependency_lock", "api_branch", "prefix_repair"
    )
  ) || anyNA(registry) || anyDuplicated(registry$runtime) ||
    sum(registry$runtime == runtime) != 1L) {
  stop("probe must run under an exact matrix runtime", call. = FALSE)
}
branch <- registry$api_branch[registry$runtime == runtime]

cases <- 0L
check <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
  cases <<- cases + 1L
  invisible(TRUE)
}

# These operations jointly exercise closure formals, attribute allow-lists,
# callbacks, capsule-backed R6 shells, and outward facades through the native
# engine. R 4.3 exercises the ledgered hot-formals/raw-attribute branches and
# the public cold closure bridge; ordinary operations are not replayed through
# a second semantic engine.
left <- ps(
  width = p_dbl(lower = 0, upper = 4, default = 1,
    tags = "numeric", trafo = function(x) x * 2),
  count = p_int(lower = 1, upper = 5, default = 2),
  mode = p_fct(levels = c("a", "b"), default = "a")
)
check(identical(left$ids(), c("width", "count", "mode")), "ids facade failed")
check(inherits(left$params, "data.table") && nrow(left$params) == 3L,
  "params facade failed")
check(length(left$domains) == 3L && inherits(left$domains$width, "Domain"),
  "domains facade failed")
left$values <- list(width = 1.5, count = 3L, mode = "b")
check(identical(left$get_values(), list(width = 1.5, count = 3L, mode = "b")),
  "get_values facade failed")
check(left$check(left$values), "check facade failed")
check(identical(left$trafo(left$values)$width, 3), "closure facade failed")
unit = matrix(
  c(0, 0.5, 1),
  nrow = 1L,
  dimnames = list(NULL, left$ids())
)
check(length(left$qunif(unit)) == 3L, "qunif facade failed")

subset <- left$clone(deep = TRUE)$subset(c("width", "mode"))
check(identical(subset$ids(), c("width", "mode")), "subset facade failed")
right <- ps(flag = p_lgl(default = TRUE), depth = p_int(0, 3, default = 1))
right$values <- list(flag = FALSE, depth = 2L)
collection <- ParamSetCollection$new(list(left = left, right = right))
check(identical(
  collection$ids(),
  c("left.width", "left.count", "left.mode", "right.flag", "right.depth")
), "collection ids facade failed")
check(nrow(collection$params) == 5L && inherits(collection$params, "data.table"),
  "collection params facade failed")
check(nrow(collection$deps) == 0L, "collection dependency facade failed")
values <- collection$values
check(identical(values[["left.width"]], 1.5) &&
  identical(values[["right.depth"]], 2L), "collection values facade failed")
collection$values <- list(left.width = 2, right.flag = TRUE)
check(identical(left$values, list(width = 2)) &&
  identical(right$values, list(flag = TRUE)), "collection store facade failed")

roundtrip <- unserialize(serialize(collection, NULL, version = 3L))
check(identical(roundtrip$ids(), collection$ids()), "serialization facade failed")
for (iteration in seq_len(25L)) {
  candidate <- ps(x = p_dbl(-iteration, iteration), y = p_int(0, iteration))
  check(candidate$check(list(x = 0, y = 0L)), "repeated facade authentication failed")
}

cat("runtime=", runtime, "\n", sep = "")
cat("r_api_compat_branch=", branch, "\n", sep = "")
cat(
  "r_api_compatibility_policy=public-current-ledgered-old-runtime-exceptions\n"
)
cat("focused_cases=", cases, "\n", sep = "")
cat("runtime_matrix_focused_probe=passed\n")
