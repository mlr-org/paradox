arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 1L || is.na(arguments[[1L]]) ||
    !nzchar(arguments[[1L]])) {
  stop("usage: reverse-activation-probe.R ROOT", call. = FALSE)
}

root <- normalizePath(arguments[[1L]], winslash = "/", mustWork = TRUE)
expected <- c(
  R = file.path(root, ".local", "toolchain", "bin", "R"),
  Rscript = file.path(root, ".local", "toolchain", "bin", "Rscript"),
  pdflatex = file.path(
    root, ".local", "tinytex", "bin", "x86_64-linux", "pdflatex"
  ),
  kpsewhich = file.path(
    root, ".local", "tinytex", "bin", "x86_64-linux", "kpsewhich"
  ),
  makeindex = file.path(
    root, ".local", "tinytex", "bin", "x86_64-linux", "makeindex"
  ),
  quarto = file.path(root, ".local", "quarto", "1.9.38", "bin", "quarto"),
  texi2dvi = file.path(root, ".local", "toolchain", "bin", "texi2dvi")
)

if (!identical(
      normalizePath(R.home(), winslash = "/", mustWork = TRUE),
      file.path(root, ".local", "toolchain", "lib", "R")
    ) ||
    !identical(unname(Sys.which(names(expected))), unname(expected))) {
  stop("activated executable resolution does not match the repository contract",
    call. = FALSE)
}
