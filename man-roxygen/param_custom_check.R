#' @param custom_check (`function()`)\cr
#'   Custom function to check the feasibility.
#'   Function which checks the input.
#'   Must return 'TRUE' if the input is valid and a `character(1)` with the error message otherwise.
#'   This function should *not* throw an error.
#'   Defaults to `NULL`, which means that no check is performed.
