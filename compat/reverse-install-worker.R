# Exact process boundary used to populate the reverse-dependency install cache.
# This file is a cache-key input; verifier, report, plan, and row-order code is
# intentionally not.

reverse_install_worker <- function(command, arguments, environment, directory,
                                   timeout_seconds, log) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("processx is required by the exact reverse install worker",
      call. = FALSE)
  }
  result <- processx::run(
    "/usr/bin/env",
    c("-i", paste0(names(environment), "=", unname(environment)),
      command, arguments),
    error_on_status = FALSE,
    wd = directory,
    timeout = timeout_seconds,
    stdout = log,
    stderr = "2>&1",
    cleanup_tree = TRUE,
    supervise = TRUE,
    linux_pdeathsig = TRUE,
    windows_hide_window = TRUE
  )
  list(status = as.integer(result$status), timeout = isTRUE(result$timeout))
}
