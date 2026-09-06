local({
  repositories = c(
    CRAN = Sys.getenv("PARADOX_OFFLINE_CHECK_CRAN", unset = ""),
    BioCsoft = Sys.getenv("PARADOX_OFFLINE_CHECK_BIOC", unset = ""),
    BioCann = Sys.getenv("PARADOX_OFFLINE_CHECK_BIOC", unset = ""),
    BioCexp = Sys.getenv("PARADOX_OFFLINE_CHECK_BIOC", unset = "")
  )
  if (any(!nzchar(repositories)) ||
      any(!grepl("^file:///", repositories))) {
    stop(
      "offline checks require absolute CRAN and Bioconductor file repositories",
      call. = FALSE
    )
  }
  options(repos = repositories)
})
