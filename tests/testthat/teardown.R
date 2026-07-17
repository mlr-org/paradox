options(old_opts)

# reticulate's uv resolver may leave this setuptools coordination lock in the
# Python temporary directory on macOS.  R CMD check treats it as test detritus.
# Only inspect Python after a test has already initialized it; teardown must not
# create an environment merely to clean one up.
if (requireNamespace("reticulate", quietly = TRUE) &&
    reticulate::py_available(initialize = FALSE)) {
  python_temp_dir = tryCatch(
    dirname(reticulate::py_run_string(
      "import tempfile; x=tempfile.NamedTemporaryFile().name",
      local = TRUE
    )$x),
    error = function(...) NULL
  )
  if (!is.null(python_temp_dir)) {
    uv_locks = list.files(
      python_temp_dir,
      pattern = "^uv-setuptools-[0-9A-Fa-f]{16}\\.lock$",
      full.names = TRUE
    )
    if (length(uv_locks)) {
      unlink(uv_locks, force = TRUE)
    }
  }
}
