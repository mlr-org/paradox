local({
  cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (length(cores) != 1L || is.na(cores) || cores < 1L) cores <- 1L

  options(
    repos = c(CRAN = "https://cloud.r-project.org"),
    Ncpus = min(8L, cores)
  )
})
