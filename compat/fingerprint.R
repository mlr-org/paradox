compat_tree_content_sha256 <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  files <- list.files(
    path,
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE,
    no.. = TRUE
  )
  info <- file.info(files, extra_cols = FALSE)
  keep <- !is.na(info$isdir) & !info$isdir
  files <- files[keep]
  info <- info[keep, , drop = FALSE]
  relative <- substring(files, nchar(path, type = "chars") + 2L)
  ordering <- order(relative, method = "radix")
  manifest <- list(
    path = relative[ordering],
    size = unname(info$size[ordering]),
    sha256 = unname(tools::sha256sum(files[ordering]))
  )
  payload <- serialize(manifest, NULL, version = 3L, xdr = TRUE)
  temporary <- tempfile("paradox-compat-content-")
  on.exit(unlink(temporary), add = TRUE)
  connection <- file(temporary, open = "wb")
  writeBin(payload, connection)
  close(connection)
  unname(tools::sha256sum(temporary))
}
