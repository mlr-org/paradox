#' @include helper.R

#' @title Construct a ParamSet using Short Forms
#'
#' @description
#' The `ps()` short form constructor uses [`Domain`] objects (`p_dbl`, `p_fct`, ...) to construct [`ParamSet`]s in a
#' succinct and readable way.
#'
#' For more specifics also see the documentation of [`Domain`].
#'
#' @param ... ([`Domain`] | [`Param`])\cr
#'   Named arguments of [`Domain`] or [`Param`] objects. The [`ParamSet`] will be constructed of the given [`Param`]s,
#'   or of [`Param`]s constructed from the given domains. The names of the arguments will be used as `$id`
#'   (the `$id` of [`Param`] arguments are ignored).
#' @param .extra_trafo (`function(x, param_set)`)\cr
#'   Transformation to set the resulting [`ParamSet`]'s `$trafo` value to. This is in addition to any `trafo` of
#'   [`Domain`] objects given in `...`, and will be run *after* transformations of individual parameters were performed.
#' @param .allow_dangling_dependencies (`logical`)\cr
#'   Whether dependencies depending on parameters that are not present should be allowed. A parameter `x` having
#'   `depends = y == 0` if `y` is not present in the `ps()` call would usually throw an error, but if dangling
#'   dependencies are allowed, the dependency is added regardless. This is usually a bad idea and mainly for internal
#'   use. Dependencies between [`ParamSet`]s when using [`to_tune()`] can be realized using this.
#' @return A [`ParamSet`] object.
#' @examples
#' pars = ps(
#'   a = p_int(0, 10),
#'   b = p_int(upper = 20),
#'   c = p_dbl(),
#'   e = p_fct(letters[1:3]),
#'   f = p_uty(custom_check = checkmate::check_function)
#' )
#' print(pars)
#'
#' pars = ps(
#'   a = p_dbl(0, 1, trafo = exp),
#'   b = p_dbl(0, 1, trafo = exp),
#'   .extra_trafo = function(x, ps) {
#'     x$c <- x$a + x$b
#'     x
#'   }
#' )
#'
#' # See how the addition happens after exp()ing:
#' pars$trafo(list(a = 0, b = 0))
#'
#' pars$values = list(
#'   a = to_tune(ps(x = p_int(0, 1),
#'     .extra_trafo = function(x, param_set) list(a = x$x)
#'   )),
#'   # make 'y' depend on 'x', but they are defined in different ParamSets
#'   # Therefore we need to allow dangling dependencies here.
#'   b = to_tune(ps(y = p_int(0, 1, depends = x == 1),
#'     .extra_trafo = function(x, param_set) list(b = x$y),
#'     .allow_dangling_dependencies = TRUE
#'   ))
#' )
#'
#' pars$search_space()
#' @family ParamSet construction helpers
#' @export
ps = function(..., .extra_trafo = NULL, .constraint = NULL, .allow_dangling_dependencies = FALSE) {
  param_set = ParamSet$new(list(...), allow_dangling_dependencies = .allow_dangling_dependencies)  # if length is 0 then no names are present
  param_set$extra_trafo = .extra_trafo
  param_set$constraint = .constraint
  param_set
}
