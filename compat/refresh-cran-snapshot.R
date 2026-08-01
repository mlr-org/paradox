#!/usr/bin/env Rscript

command <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command, value = TRUE)
if (length(file_argument) != 1L) {
  stop("could not identify refresh-cran-snapshot.R", call. = FALSE)
}
script <- normalizePath(
  sub("^--file=", "", file_argument), winslash = "/", mustWork = TRUE
)
helpers <- file.path(
  dirname(script), c("source-fetch-common.R", "cran-refresh-common.R")
)
for (helper in helpers) {
  info <- file.info(helper)
  link <- Sys.readlink(helper)
  regular <- suppressWarnings(system2(
    "/usr/bin/test", c("-f", shQuote(helper)), stdout = FALSE, stderr = FALSE
  ))
  if ((!is.na(link) && nzchar(link)) ||
      !identical(as.integer(regular), 0L) ||
      is.na(info$isdir) || info$isdir) {
    stop("CRAN refresh helper is missing, non-regular, or symbolic: ", helper,
      call. = FALSE)
  }
}
sys.source(helpers[[1L]], envir = environment(), keep.source = FALSE)
sys.source(helpers[[2L]], envir = environment(), keep.source = FALSE)
cran_refresh_main(commandArgs(trailingOnly = TRUE), script)
