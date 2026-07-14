sys.source(file.path(Sys.getenv("PARADOX_ROOT"), "environment", "Rprofile.R"), envir = globalenv())

options(
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)
