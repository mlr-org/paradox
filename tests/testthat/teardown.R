options(old_opts)

if (requireNamespace("reticulate", quietly = TRUE)) {
  # created by reticulate and uv
  python_temp_dir = dirname(
    reticulate::py_run_string("import tempfile; x=tempfile.NamedTemporaryFile().name", local = TRUE)$x)
  detritus = list.files(python_temp_dir, pattern = "^uv-setuptools-[0-9A-Fa-f]{16}\\.lock$", full.names = TRUE)
  if (length(detritus)) unlink(detritus)
}
