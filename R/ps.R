#' @include helper.R

#' @title Construct a ParamSet using Short Forms
#'
#' The `ps()` short form constructor uses [`Domain`] objects to construct [`ParamSet`]s in a succinct and readable way.
#' @param ... ([`Domain`] | [`Param`])\cr
#'   Named arguments of [`Domain`] or [`Param`] objects. The [`ParamSet`] will be constructed of the given [`Param`]s,
#'   or of [`Params`]s constructed from the given domains. The names of the arguments will be used as `$id`
#'   (the `$id` of [`Param`] arguments are ignored).
#' @param .extra_trafo (`function(x, param_set)`)\cr
#'   Transformation to set the resulting [`ParamSet`]'s `$trafo` value to. This is in addition to any `trafo` of
#'   [`Domain`] objects given in `...`, and will be run *after* transformations of individual parameters were performed.
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
#' @family ParamSet construction helpers
#' @export
ps = function(..., .extra_trafo = NULL) {
  args = list(...)
  assert_list(args, names = "unique", types = c("Param", "Domain"))
  assert_function(.extra_trafo, null.ok = TRUE)

  # generate Params (with correct id) from Domain objects
  params = imap(args, function(p, name) {
    if (inherits(p, "Param")) {
      p = p$clone(deep = TRUE)
    } else {
      p = p$param$clone(deep = TRUE)
    }
    p$id = name
    p
  })

  paramset = ParamSet$new(params)

  # add Dependencies
  imap(args, function(p, name) {
    if (inherits(p, "Param") || is.null(p$requirements)) return(NULL)
    map(p$requirements, function(req) {
      if (!req$on %in% names(args) || req$on == name) {
        stopf("Parameter %s can not depend on %s.", name, req$on)
      }
      invoke(paramset$add_dep, id = name, .args = req)
    })
  })

  # add trafos
  trafos = map(discard(args, function(x) inherits(x, "Param") || is.null(x$trafo)),
    function(p) {
      assert_function(p$trafo)
    })
  if (length(trafos) || !is.null(.extra_trafo)) {
    # the $trafo function iterates through the trafos and applies them
    # We put the $trafo in a crate() (helper.R) to avoid having a function
    # with lots of things in its environment.
    paramset$trafo = crate(function(x, param_set) {
      for (trafoing in names(trafos)) {
        if (!is.null(x[[trafoing]])) {
          x[[trafoing]] = trafos[[trafoing]](x[[trafoing]])
        }
      }
      if (!is.null(.extra_trafo)) x = .extra_trafo(x, param_set)
      x
    }, trafos, .extra_trafo)
  }
  paramset
}
