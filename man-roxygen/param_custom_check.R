#' @param custom_check (`function()`)\cr
#'   Custom function to check the feasibility.
#'   Function which checks the input.
#'   Must return 'TRUE' if the input is valid and a `character(1)` with the error message otherwise.
#'   This function should *not* throw an error.
#'   Defaults to `NULL`, which means that no check is performed.
#'   Paradox removes source-reference attributes from the stored callback at
#'   construction time. See `options(paradox.strip_srcrefs = FALSE)` under
#'   [paradox-package] for the debugging opt-out. Source-reference
#'   normalization does not modify function-valued parameter values or other
#'   opaque value payloads.
