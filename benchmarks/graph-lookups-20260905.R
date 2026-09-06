#!/usr/bin/env Rscript

# Paired development measurements for the September 2026 hashed graph-lookup
# pass; invoke in baseline/candidate/candidate/baseline fresh-process order and
# summarize with summarize-getters-20260905.R. Uses the preceding installed
# development build as the baseline, not a release claim.
args = commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L, !file.exists(args[[2L]]))
.libPaths(c(normalizePath(args[[1L]]), .libPaths()))
library(paradox)
data.table::setDTthreads(1L)
cases = list()
add = function(name, fun, validate = identity) {
  cases[[name]] <<- list(fun = fun, validate = validate, expected = validate(fun()))
}
domains_of = function(n) {
  domains = rep(list(p_dbl(0, 1), p_int(0L, 5L), p_fct(c("a", "b")), p_lgl()),
    length.out = n)
  names(domains) = sprintf("x%d", seq_len(n))
  domains
}
values_of = function(n, ids) {
  values = rep(list(0.5, 2L, "a", TRUE), length.out = n)
  names(values) = ids
  values
}
for (n in c(5L, 64L, 500L)) local({
  # Closures below must not read the loop variable at call time.
  size = n
  base = ParamSet$new(domains_of(n))
  values = values_of(n, base$ids())
  one = values[1L]
  stored = ParamSet$new(domains_of(n))
  stored$values = values
  key = paste0("base/", n)
  add(paste0(key, "/check_full"), function() base$check(values))
  add(paste0(key, "/check_one"), function() base$check(one))
  add(paste0(key, "/values_set_full"), function() {
    base$values = values
    base$values
  })
  add(paste0(key, "/values_set_one"), function() {
    base$values = one
    base$values
  })
  add(paste0(key, "/set_values_one"), function() {
    stored$set_values(x1 = 0.25)
    stored$values$x1
  })
  add(paste0(key, "/trafo"), function() base$trafo(values))
  add(paste0(key, "/construct"), function() ParamSet$new(domains_of(size))$length)

  collection = ParamSetCollection$new(list(a = ParamSet$new(domains_of(n))))
  collection_values = values
  names(collection_values) = paste0("a.", names(values))
  collection_one = collection_values[1L]
  stored_collection = ParamSetCollection$new(list(a = ParamSet$new(domains_of(n))))
  stored_collection$values = collection_values
  key = paste0("collection/", n)
  add(paste0(key, "/check_full"), function() collection$check(collection_values))
  add(paste0(key, "/check_one"), function() collection$check(collection_one))
  add(paste0(key, "/values_set_full"), function() {
    collection$values = collection_values
    collection$values
  })
  add(paste0(key, "/set_values_one"), function() {
    stored_collection$set_values(a.x1 = 0.25)
    stored_collection$values$a.x1
  })
  add(paste0(key, "/design"), function() {
    Design$new(stored_collection, data.table::as.data.table(collection_values),
      remove_dupl = FALSE)$data$a.x1
  })

  origin = ParamSet$new(domains_of(n))
  origin$values = values
  hidden = origin$ids()[seq_len(n %/% 2)]
  shadow = ParamSetShadow$new(origin, hidden)
  visible_values = values[-seq_len(n %/% 2)]
  add(paste0("shadow/", n, "/values"), function() shadow$values)
  add(paste0("shadow/", n, "/check_full"), function() shadow$check(visible_values))
})
local({
  sets = lapply(seq_len(100L), function(index) ParamSet$new(domains_of(5L)))
  names(sets) = sprintf("s%d", seq_len(100L))
  wide = ParamSetCollection$new(sets)
  wide_values = do.call(c, lapply(seq_len(100L), function(index) {
    values = values_of(5L, sprintf("x%d", seq_len(5L)))
    names(values) = paste0("s", index, ".", names(values))
    values
  }))
  stopifnot(identical(names(wide_values), wide$ids()))
  add("collection/100x5/check_full", function() wide$check(wide_values))
  add("collection/100x5/values_set_full", function() {
    wide$values = wide_values
    wide$values
  })
  add("collection/100x5/construct", function() ParamSetCollection$new(sets)$length)
})
cat("R:", as.character(getRversion()), "Paradox:", find.package("paradox"), "\n")
cat("DSO:", tools::sha256sum(getLoadedDLLs()[["paradox"]][["path"]]), "\n")
deadline = proc.time()[["elapsed"]] + 10
while (proc.time()[["elapsed"]] < deadline)
  for (i in seq_len(1000L)) invisible(cases[[1L]]$fun())
iterations = as.integer(Sys.getenv("PARADOX_BENCH_ITERATIONS", "200"))
rows = list()
for (name in names(cases)) {
  case = cases[[name]]
  fun = case$fun
  for (i in seq_len(5L)) {
    if (!identical(case$validate(fun()), case$expected)) {
      stop("result changed for case ", name)
    }
  }
  for (block in seq_len(3L)) {
    invisible(gc())
    m = bench::mark(fun(), iterations = iterations, check = FALSE, memory = TRUE, filter_gc = FALSE)
    rows[[length(rows) + 1L]] = data.frame(case = name, block = block,
      median_seconds = as.numeric(m$median), mean_seconds = mean(as.numeric(m$time[[1L]])),
      allocation_bytes = as.numeric(m$mem_alloc),
      result_key = digest::digest(case$expected, algo = "sha256", serializeVersion = 2L))
  }
  if (!identical(case$validate(fun()), case$expected)) {
    stop("result changed for case ", name)
  }
}
write.table(do.call(rbind, rows), args[[2L]], sep = "\t", quote = FALSE, row.names = FALSE)
