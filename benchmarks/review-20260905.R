#!/usr/bin/env Rscript

# Small development comparisons, run in alternating fresh processes against
# baseline/development installations built with identical compiler flags.
# Usage: Rscript --vanilla benchmarks/review-20260905.R LIBRARY OUTPUT.tsv
args = commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
library_path = normalizePath(args[[1L]], mustWork = TRUE)
output = args[[2L]]
stopifnot(!file.exists(output), dir.exists(dirname(output)))
library(paradox, lib.loc = library_path)
data.table::setDTthreads(1L)

cases = list()
add_case = function(name, fun, iterations) {
  cases[[name]] <<- list(fun = fun, iterations = iterations)
}
for (width in c(8L, 128L, 1024L)) {
  for (shape in c("independent", "chain", "fanout")) {
    local({
      ids = paste0("x", seq_len(width))
      param_set = ParamSet$new(setNames(rep(list(p_int(0, 0)), width), ids))
      if (shape != "independent") {
        parents = if (shape == "chain") ids[-width] else rep(ids[[1L]], width - 1L)
        param_set$deps = data.table::data.table(
          id = ids[-1L], on = parents,
          cond = rep(list(CondEqual(0L)), width - 1L)
        )
      }
      data = data.table::as.data.table(setNames(rep(list(0L), width), ids))
      iterations = if (width == 8L) 1500L else if (width == 128L) 100L else 10L
      add_case(paste("plan", shape, width, sep = "_"), function() {
        .Call(paradox:::C_design_dependency_plan, data, param_set)
      }, iterations)
      if (shape == "independent") {
        reversed = data.table::copy(data)
        data.table::setcolorder(reversed, rev(ids))
        add_case(paste("plan_reversed", width, sep = "_"), function() {
          .Call(paradox:::C_design_dependency_plan, reversed, param_set)
        }, iterations)
      }
      add_case(paste("grid", shape, width, sep = "_"), function() {
        generate_design_grid(param_set, resolution = 1L)
      }, iterations)
    })
  }
}
for (width in c(1L, 3L)) {
  local({
    param_set = ParamSet$new(setNames(rep(list(p_dbl(0, 1)), width),
      paste0("x", seq_len(width))))
    resolution = if (width == 1L) 20001L else 10L
    add_case(paste0("grid_numeric_", width), function() {
      generate_design_grid(param_set, resolution)
    }, 100L)
  })
}
for (upper in c(1, 0.03)) {
  local({
    sampler = Sampler1DRfun$new(ps(x = p_dbl(0, upper)), runif)
    add_case(paste0("rejection_", upper), function() sampler$sample(1000L), 30L)
  })
}
normal_support = ps(x = p_dbl(-2, 6))
add_case("normal_constructor", function() Sampler1DNormal$new(normal_support), 70L)

result = list()
for (name in names(cases)) {
  case = cases[[name]]
  set.seed(9034L)
  for (warmup in seq_len(3L)) invisible(case$fun())
  allocation_log = tempfile("review-allocation-", tmpdir = dirname(output))
  Rprofmem(allocation_log)
  invisible(case$fun())
  Rprofmem(NULL)
  lines = readLines(allocation_log, warn = FALSE)
  allocations = as.double(sub(" .*", "", lines[grepl("^[0-9]+ ", lines)]))
  unlink(allocation_log)
  for (block in seq_len(5L)) {
    set.seed(9034L + block)
    invisible(gc())
    elapsed = system.time({
      for (iteration in seq_len(case$iterations)) invisible(case$fun())
    })[["elapsed"]]
    result[[length(result) + 1L]] = data.frame(
      case = name, block = block, iterations = case$iterations,
      elapsed = elapsed, seconds_per_call = elapsed / case$iterations,
      allocation_bytes = sum(allocations), allocation_count = length(allocations)
    )
  }
}
write.table(do.call(rbind, result), output, sep = "\t", row.names = FALSE, quote = FALSE)
cat("benchmark_output=", output, "\n", sep = "")
cat("paradox_dso=", getLoadedDLLs()[["paradox"]][["path"]], "\n", sep = "")
cat("paradox_dso_sha256=", unname(tools::sha256sum(
  getLoadedDLLs()[["paradox"]][["path"]]
)), "\n", sep = "")
