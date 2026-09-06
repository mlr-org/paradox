#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    "usage: audit-registration.R INSTALLED_LIBRARY SOURCE_SNAPSHOT",
    call. = FALSE
  )
}

library_path <- normalizePath(args[[1L]], mustWork = TRUE)
snapshot <- normalizePath(args[[2L]], mustWork = TRUE)
.libPaths(unique(c(library_path, .libPaths())))
library("paradox", character.only = TRUE, lib.loc = library_path)

installed <- normalizePath(find.package("paradox", lib.loc = library_path),
  mustWork = TRUE)
if (!startsWith(installed, paste0(library_path, .Platform$file.sep))) {
  stop("paradox did not resolve from the isolated run library", call. = FALSE)
}

dll <- getLoadedDLLs()[["paradox"]]
if (is.null(dll)) {
  stop("the paradox DLL is not loaded", call. = FALSE)
}
if (isTRUE(dll[["dynamicLookup"]])) {
  stop("dynamic native-symbol lookup is enabled", call. = FALSE)
}
if (!isTRUE(dll[["forceSymbols"]])) {
  stop("registered native routines do not require symbol objects", call. = FALSE)
}
dll_path <- normalizePath(dll[["path"]], mustWork = TRUE)
if (!startsWith(dll_path, paste0(installed, .Platform$file.sep))) {
  stop("the loaded DLL is not from the isolated installation", call. = FALSE)
}

routines <- getDLLRegisteredRoutines(dll)
if (any(lengths(routines[c(".C", ".Fortran", ".External")]) != 0L)) {
  stop("paradox must expose native entry points through .Call only",
    call. = FALSE)
}
call_routines <- routines[[".Call"]]
if (length(call_routines) == 0L || is.null(names(call_routines)) ||
    any(!nzchar(names(call_routines))) || anyDuplicated(names(call_routines))) {
  stop("the .Call registration table is empty or malformed", call. = FALSE)
}

namespace <- asNamespace("paradox")
native_bindings <- ls(namespace, pattern = "^C_", all.names = TRUE)
expected_bindings <- paste0("C_", names(call_routines))
if (!setequal(native_bindings, expected_bindings)) {
  stop(
    "namespace native-symbol bindings differ from the registration table; ",
    "bindings=", paste(sort(native_bindings), collapse = ","),
    "; expected=", paste(sort(expected_bindings), collapse = ","),
    call. = FALSE
  )
}

for (routine_name in names(call_routines)) {
  registered <- call_routines[[routine_name]]
  binding <- get(paste0("C_", routine_name), envir = namespace,
    inherits = FALSE)
  if (!inherits(binding, "NativeSymbolInfo") ||
      !identical(binding[["name"]], routine_name) ||
      !identical(binding[["numParameters"]], registered[["numParameters"]]) ||
      !identical(binding[["dll"]][["name"]], "paradox") ||
      is.na(binding[["numParameters"]]) || binding[["numParameters"]] < 0L) {
    stop("invalid namespace binding for registered routine ", routine_name,
      call. = FALSE)
  }
}

inspect_call <- function(expression, source_file) {
  if (is.call(expression) && identical(expression[[1L]], quote(.Call)) &&
      length(expression) >= 2L && is.character(expression[[2L]])) {
    stop("literal-string .Call found in ", source_file,
      "; use a registered NativeSymbolInfo binding", call. = FALSE)
  }
  if (is.call(expression) || is.expression(expression) || is.pairlist(expression)) {
    elements <- as.list(expression)
    for (index in seq_along(elements)) {
      # Calls and function formals may contain R's missing-argument sentinel.
      # Passing that sentinel as a function argument makes the formal itself
      # missing, so exclude it before recursing.
      if (!identical(elements[[index]], quote(expr = ))) {
        inspect_call(elements[[index]], source_file)
      }
    }
  }
  invisible(NULL)
}

r_files <- sort(list.files(file.path(snapshot, "R"), pattern = "\\.[Rr]$",
  full.names = TRUE))
if (length(r_files) == 0L) {
  stop("source snapshot contains no R files", call. = FALSE)
}
for (r_file in r_files) {
  inspect_call(parse(r_file, keep.source = FALSE), r_file)
}

cat("dll=", dll_path, "\n", sep = "")
cat("dynamic_lookup=false\n")
cat("force_symbols=true\n")
for (routine_name in sort(names(call_routines))) {
  cat(".Call\t", routine_name, "\t",
    call_routines[[routine_name]][["numParameters"]], "\n", sep = "")
}
