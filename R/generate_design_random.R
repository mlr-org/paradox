is_exact_random_design_space = function(param_set, seen = list()) {
  if (!isTRUE(.Call(C_param_set_surface_auth, param_set, 4L))) {
    return(FALSE)
  }
  classes = class(param_set)
  is_param_set = identical(classes, c("ParamSet", "R6"))
  is_collection = identical(
    classes,
    c("ParamSetCollection", "ParamSet", "R6")
  )
  required_public = c(
    "clone", "ids", "qunif", "length", "class",
    if (is_collection) "sets"
  )
  if ((!is_param_set && !is_collection) ||
      !test_r6(
        param_set,
        "ParamSet",
        public = required_public,
        private = ".params"
      )) {
    return(FALSE)
  }
  if (is_collection &&
      any(vapply(seen, identical, logical(1L), y = param_set))) {
    return(FALSE)
  }
  built_in_classes = c("ParamDbl", "ParamInt", "ParamFct", "ParamLgl")
  if (!all(param_set$class %in% built_in_classes)) {
    return(FALSE)
  }
  if (is_param_set) {
    return(TRUE)
  }

  seen[[length(seen) + 1L]] = param_set
  all(vapply(
    param_set$sets,
    is_exact_random_design_space,
    logical(1L),
    seen = seen
  ))
}

#' @title Generate a Random Design
#'
#' @description
#' Generates a design with randomly drawn points.
#' Canonical base [ParamSet] and [ParamSetCollection] objects use one bulk
#' uniform draw followed by quantile mapping. The values and random-number
#' stream match [`SamplerUnif`], including dependency handling. Extended or
#' zero-dimensional parameter spaces retain the complete [`SamplerUnif`] path.
#' If dependencies do not hold, values are set to `NA` in the resulting
#' data.table.
#'
#' @param param_set ([`ParamSet`]).
#' @param n (`integer(1)`)\cr
#'   Number of points to draw randomly.
#' @return [`Design`].
#'
#' @family generate_design
#' @export
#' @examples
#' pset = ps(
#'   ratio = p_dbl(lower = 0, upper = 1),
#'   letters = p_fct(levels = letters[1:3])
#' )
#' generate_design_random(pset, 10)
generate_design_random = function(param_set, n) {
  # A zero-dimensional design historically has zero rows even when n is
  # positive. Keep that unusual contract, subclasses, and altered object
  # shapes on the complete Sampler path.
  if (!is_exact_random_design_space(param_set) || param_set$length == 0L) {
    return(SamplerUnif$new(param_set)$sample(n))
  }

  # Match SamplerUnif's validation and ordering: validate the support, detach
  # it once, and only then validate the requested sample count.
  assert_param_set(
    param_set,
    must_bounded = TRUE,
    no_deps = FALSE,
    no_untyped = TRUE
  )
  sampled_param_set = param_set$clone(deep = TRUE)
  assert_count(n)

  ids = sampled_param_set$ids()
  unit_count = as.double(n) * length(ids)
  unit_values = matrix(
    runif(unit_count),
    nrow = n,
    ncol = length(ids),
    dimnames = list(NULL, ids)
  )
  data = sampled_param_set$qunif(unit_values)
  Design$new(sampled_param_set, data, remove_dupl = FALSE)
}
