#!/usr/bin/env Rscript

# Paired development measurements; invoke in A/B/B/A fresh-process order.
# Uses the preceding installed development build as A, not a release claim.
args = commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L, !file.exists(args[[2L]]))
.libPaths(c(normalizePath(args[[1L]]), .libPaths()))
library(paradox)
data.table::setDTthreads(1L)
cases = list()
add = function(name, fun, validate = identity) {
  cases[[name]] <<- list(fun = fun, validate = validate, expected = validate(fun()))
}
for (n in c(0L, 5L, 64L, 500L)) for (type in c("numeric", "categorical")) local({
  domains = if (type == "numeric") list(p_dbl(0, 1), p_int(0, 5)) else
    list(p_fct(c("a", "b")), p_lgl())
  node = ParamSet$new(setNames(rep(domains, length.out = n), sprintf("x%d", seq_len(n))))
  values = setNames(rep(if (type == "numeric") list(0.5, 2L) else list("a", TRUE),
    length.out = n), node$ids())
  node$values = values
  key = paste(type, n, sep = "/")
  add(paste0(key, "/values"), function() node$values)
  add(paste0(key, "/get_values"), function() node$get_values())
  add(paste0(key, "/class_control"), function() node$class)
  if (n == 64L || n == 500L) {
    reverse_node = node$clone(deep = TRUE)
    paradox:::param_set_core_replace(reverse_node$.__enclos_env__$private, values = rev(values))
    add(paste0(key, "/reversed_values"), function() reverse_node$values)
  }
})
source("benchmarks/workloads.R")
for (n in c(5L, 64L, 500L)) local({
  parameter_count = n
  child = ParamSet$new(setNames(rep(list(p_dbl(0, 1)), n), sprintf("x%d", seq_len(n))))
  collection = ParamSetCollection$new(list(owner = child))
  add(paste0("collection/", n, "/has_trafo"), function() collection$has_trafo)
  add(paste0("collection/", n, "/has_constraint"), function() collection$has_constraint)
  add(paste0("collection/", n, "/sample_builtin"),
    function() .Call(paradox:::C_sampler_unif_sample_builtin, collection, 8L),
    function(result) {
      stopifnot(ncol(result) == parameter_count, nrow(result) == 8L,
        identical(names(result), paste0("owner.", child$ids())),
        all(vapply(result, function(x) all(x >= 0 & x <= 1), logical(1L))))
      c(ncol(result), nrow(result))
    })
})
inputs = benchmark_make_inputs(64L, 128L)
workloads = benchmark_make_workloads(inputs)
for (name in c("shadow_values_live", "collection_values_plain", "collection_values_rich",
    "collection_values_nested", "collection_get_values_rich", "collection_get_values_nested")) local({
  workload = workloads[[name]]
  add(name, function() eval(workload$expression), workload$validate)
})
local({
  origin = ps(hidden = p_int(init = 1L), visible = p_dbl(0, 1, init = 0.5))
  shadow = ParamSetShadow$new(origin, "hidden")
  collection = ParamSetCollection$new(list(a = shadow, b = shadow))
  nested = ParamSetCollection$new(list(outer = collection))
  plain = ParamSetCollection$new(list(a = origin, b = origin))
  add("shadow/small_values", function() shadow$values)
  add("shadow/shared_collection_values", function() collection$values)
  add("shadow/nested_values", function() nested$values)
  add("shadow/public_value_refresh", function() {
    origin$values = list(hidden = 1L, visible = 0.5)
    nested$values
  })
  add("collection/schema_after_value_write", function() {
    origin$values = list(hidden = 1L, visible = 0.5)
    plain$class
  })
  add("collection/construct", function() {
    ParamSetCollection$new(list(a = origin, b = origin))$ids()
  })
  add("collection/has_deps", function() plain$has_deps)
  add("collection/deps", function() as.list(plain$deps))
})
cat("R:", as.character(getRversion()), "Paradox:", find.package("paradox"), "\n")
cat("DSO:", tools::sha256sum(getLoadedDLLs()[["paradox"]][["path"]]), "\n")
deadline = proc.time()[["elapsed"]] + 10
while (proc.time()[["elapsed"]] < deadline)
  for (i in seq_len(1000L)) invisible(cases[[1L]]$fun())
rows = list()
for (name in names(cases)) {
  case = cases[[name]]
  fun = case$fun
  for (i in seq_len(5L)) stopifnot(identical(case$validate(fun()), case$expected))
  for (block in seq_len(3L)) {
    invisible(gc())
    m = bench::mark(fun(), iterations = 300L, check = FALSE, memory = TRUE, filter_gc = FALSE)
    rows[[length(rows) + 1L]] = data.frame(case = name, block = block,
      median_seconds = as.numeric(m$median), mean_seconds = mean(as.numeric(m$time[[1L]])),
      allocation_bytes = as.numeric(m$mem_alloc),
      result_key = digest::digest(case$expected, algo = "sha256", serializeVersion = 2L))
  }
  stopifnot(identical(case$validate(fun()), case$expected))
}
write.table(do.call(rbind, rows), args[[2L]], sep = "\t", quote = FALSE, row.names = FALSE)
