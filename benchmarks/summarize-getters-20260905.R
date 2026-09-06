#!/usr/bin/env Rscript

# Summarize the balanced baseline-a1/candidate-b1/candidate-b2/baseline-a2
# process blocks produced by getters-20260905.R. Optional v1-v1/v1-v2 files
# contain the same column workloads on Paradox 1.0.1.
args = commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
root = normalizePath(args[[1L]], mustWork = TRUE)
read_block = function(name) read.delim(file.path(root, paste0(name, ".tsv")))
a1 = read_block("baseline-a1")
a2 = read_block("baseline-a2")
b1 = read_block("candidate-b1")
b2 = read_block("candidate-b2")
stopifnot(identical(a1$case, a2$case), identical(a1$case, b1$case),
  identical(a1$case, b2$case), identical(a1$result_key, a2$result_key),
  identical(a1$result_key, b1$result_key), identical(a1$result_key, b2$result_key))
summarize = function(data) {
  by_case = split(data, data$case)
  data.frame(case = names(by_case),
    median = vapply(by_case, function(x) median(x$median_seconds), numeric(1L)),
    mean = vapply(by_case, function(x) median(x$mean_seconds), numeric(1L)),
    allocation = vapply(by_case, function(x) median(x$allocation_bytes), numeric(1L)),
    row.names = NULL)
}
a = summarize(a1)
ar = summarize(a2)
b = summarize(b1)
br = summarize(b2)
comparison = data.frame(case = a$case,
  baseline_us = sqrt(a$median * ar$median) * 1e6,
  candidate_us = sqrt(b$median * br$median) * 1e6,
  speedup = sqrt(a$median * ar$median / (b$median * br$median)),
  first_order_speedup = a$median / b$median,
  second_order_speedup = ar$median / br$median,
  mean_speedup = sqrt(a$mean * ar$mean / (b$mean * br$mean)),
  baseline_bytes = a$allocation, candidate_bytes = b$allocation)
write.table(comparison, file.path(root, "comparison.tsv"), sep = "\t",
  quote = FALSE, row.names = FALSE)
if (file.exists(file.path(root, "v1-v1.tsv")) &&
    file.exists(file.path(root, "v1-v2.tsv"))) {
  v1 = read_block("v1-v1")
  v2 = read_block("v1-v2")
  stopifnot(identical(v1$case, v2$case), identical(v1$result_key, v2$result_key))
  selected = b1[b1$case %in% v1$case, ]
  stopifnot(identical(v1$case, selected$case),
    identical(v1$result_key, selected$result_key))
  v = summarize(v1)
  vr = summarize(v2)
  comparison = comparison[match(v$case, comparison$case), ]
  comparison$v1_us = sqrt(v$median * vr$median) * 1e6
  comparison$speedup_over_v1 = comparison$v1_us / comparison$candidate_us
  write.table(comparison, file.path(root, "comparison-v1.tsv"), sep = "\t",
    quote = FALSE, row.names = FALSE)
}
