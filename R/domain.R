#' @include helper.R

#' @title Domain: Parameter Range without an Id
#'
#' @description
#' A `Domain` object is a representation of a single dimension of a [`ParamSet`]. `Domain` objects are used to construct
#' [`ParamSet`]s, either through the [`ps()`] short form, or through the [`ParamSet`]`$tune_ps()` mechanism (see
#' [`to_tune()`]). `Domain` corresponds to a [`Param`] object, except it does not have an `$id`, and it *does* have a
#' `trafo` and dependencies (`requires`) associated with it. For each of the basic [`Param`] classes ([`ParamInt`],
#' [`ParamDbl`], [`ParamLgl`], [`ParamFct`], and [`ParamUty`]) there is a function constructing a `Domain` object
#' (`p_int()`, `p_dbl()`, `p_lgl()`, `p_fct()`, `p_uty()`). They each have the same arguments as the corresponding
#' [`Param`] `$new()` function, except without the `id` argument, and with the the additional parameters `trafo`, and
#' `requires`.
#'
#' The `p_fct` function admits a `levels` argument that goes beyond the `levels` accepted by [`ParamFct`]`$new()`.
#' Instead of a `character` vector, any atomic vector or list (optionally named) may be given. (If the value is a list
#' that is not named, the names are inferred using `as.character()` on the values.) The resulting `Domain` will
#' correspond to a range of values given by the names of the `levels` argument with a `trafo` that maps the `character`
#' names to the arbitrary values of the `levels` argument.
#'
#' `Domain` objects are representations of parameter ranges and are intermediate objects to be used in short form
#' constructions in [`to_tune()`] and [`ps()`]. Because of their nature, they should not be modified by the user.
#' The `Domain` object's internals are subject to change and should not be relid upon.
#'
#' @param ... (any)\cr
#'   Parameters as they would be given to the corresponding constructors, e.g. `ParamInt$new()` for `p_int()`. These
#'   may be named or unnamed. The order is the same as for `ParamXxx$new(...)`, except that the `id` must not be given.
#' @param trafo (`function`)\cr
#'   Single argument function performing the transformation of a parameter. When the `Domain` is used to construct a
#'   [`ParamSet`], this transformation will be applied to the corresponding parameter as part of the `$trafo` function.
#' @param requires (`call` | `expression`)\cr
#'   An expression indicating a requirement for the parameter that will be constructed from this. Can be given as an
#'   expression (using `quote()`), or the expression can be entered directly and will be parsed using NSE (see
#'   examples). The expression may be of the form `<Param> == <value>` or `<Param> %in% <values>`, which will result in
#'   dependencies according to `ParamSet$add_dep(on = "<Param>", cond = CondEqual$new(<value>))` or
#'   `ParamSet$add_dep(on = "<Param>", cond = CondAnyOf$new(<values>))`, respectively (see [`CondEqual`],
#'   [`CondAnyOf`]). The expression may also contain multiple conditions separated by `&&`.
#' @examples
#' params = ps(
#'   unbounded_integer = p_int(),
#'   bounded_double = p_dbl(0, 10),
#'   half_bounded_integer = p_dbl(1),
#'   half_bounded_double = p_dbl(upper = 1),
#'   double_with_trafo = p_dbl(-1, 1, trafo = exp),
#'   extra_double = p_dbl(0, 1, special_vals = list("xxx"), tags = "tagged"),
#'   factor_param = p_fct(c("a", "b", "c")),
#'   factor_param_with_implicit_trafo = p_fct(list(a = 1, b = 2, c = list()))
#' )
#' print(params)
#'
#' params$trafo(list(
#'   bounded_double = 1,
#'   double_with_trafo = 1,
#'   factor_param = "c",
#'   factor_param_with_implicit_trafo = "c"
#' ))
#' @family ParamSet construction helpers
#' @name Domain
NULL

#' @rdname Domain
#' @export
p_int = function(..., requires = NULL, trafo = NULL) {
  domain(constructor = ParamInt, ..., requires_expr = substitute(requires), trafo = trafo)
}

#' @rdname Domain
#' @export
p_dbl = function(..., requires = NULL, trafo = NULL) {
  domain(constructor = ParamDbl, ..., requires_expr = substitute(requires), trafo = trafo)
}

#' @rdname Domain
#' @export
p_uty = function(..., requires = NULL, trafo = NULL) {
  domain(constructor = ParamUty, ..., requires_expr = substitute(requires), trafo = trafo)
}

#' @rdname Domain
#' @export
p_lgl = function(..., requires = NULL, trafo = NULL) {
  domain(constructor = ParamLgl, ..., requires_expr = substitute(requires), trafo = trafo)
}

#' @rdname Domain
#' @export
p_fct = function(..., requires = NULL, trafo = NULL) {
  extracted = (function(id, levels, ...) {
    list(levels = levels, rest = list(...))
  })(id = "ID", ...)
  levels = extracted$levels
  if (!is.character(levels)) {
    # if the "levels" argument is not a character vector, then
    # we add a trafo.
    assert(check_atomic_vector(levels), check_list(levels))
    if (is.null(names(levels))) {
      names(levels) = as.character(levels)
    }
    trafo = crate(function(x) {
      x = levels[[x]]
      if (!is.null(trafo)) x = trafo(x)
      x
    }, trafo, levels)
    constargs = extracted$rest
    constargs$levels = names(levels)
    constargs$requires = NULL
    domain(constructor = ParamFct, requires_expr = substitute(requires), trafo = trafo, .constargs = constargs)
  } else {
    domain(constructor = ParamFct, ..., requires_expr = substitute(requires), trafo = trafo)
  }
}

# Construct the actual `Domain` object
# @param Constructor: The ParamXxx to call `$new()` for.
# @param .constargs: alternative to `...`.
domain = function(constructor, ..., requires_expr = NULL, trafo = NULL, .constargs = NULL) {
  constargs = c(.constargs, list(...))
  if ("id" %in% names(constargs)) stop("id must not be given to p_xxx")

  # check that `...` are valid by constructing and making sure this doesn't error
  # The object generated here is thrown away, this is only for checks.
  param = invoke(constructor$new, id = "ID", .args = constargs)

  # requires may be an expression, but may also be quote() or expression()
  if (length(requires_expr) == 1 ||
      isTRUE(as.character(requires_expr[[1]]) %in% c("quote", "expression"))) {
    requires_expr = eval(requires_expr, envir = parent.frame(2))
  }

  # repr: what to print
  repr = sys.call(-1)
  traforep = repr$trafo

  repr = as.call(c(as.list(repr)[[1]], constargs))  # use cleaned up constargs
  repr$requires = requires_expr  # put `requires` at the end, but only if not NULL
  repr$trafo = traforep  # put `trafo` at the end, but only if not NULL

  set_class(list(
    param = param,
    trafo = assert_function(trafo, null.ok = TRUE),
    requirements = parse_requires(requires_expr, parent.frame(2)),
    repr = repr
  ), "Domain")
}

#' @export
print.Domain = function(x, ...) {
  print(x$repr)
}

# Parse the expression for requirements, as they are given to p_int, p_dbl etc.
# We allow `==`, `%in%` `&&`, and `(`/`)` to occur in such expressions.
# We construct a list of `Condition` objects with an additional `on` element of what
# the condition should refer to.
#
# @example
# parse_requires(quote(x == 1 && y %in% c("b", "c")), environment())
# # same as:
# list(
#   list(on = "x", CondEqual$new(1)),
#   list(on = "y", CondAnyOf$new(c("b", "c")))
# )
parse_requires = function(requires_expr, evalenv) {
  if (is.null(requires_expr)) return(NULL)

  # throw(): Give generic helpful error message.
  throw = function(msg = NULL) stopf("Requirement '%s' is broken%s", deparse1(requires_expr), if (!is.null(msg)) sprintf(":\n%s", msg) else "")

  if (!is.language(requires_expr)) throw()
  if (is.expression(requires_expr)) {
    if (length(requires_expr) != 1) {
      throw("given 'expression' objects must have length 1.")
    }
    requires_expr = requires_expr[[1]]
  }

  symbol_paren = as.symbol("(")
  symbol_equal = as.symbol("==")
  symbol_and = as.symbol("&&")
  symbol_in = as.symbol("%in%")

  # unpack_parens(): turn (((x))) into x
  unpack_parens = function(expr) {
    while (is.recursive(expr) && length(expr) && identical(expr[[1]], symbol_paren)) expr = expr[[2]]
    expr
  }

  # recurse_expression: turn an expression into a list of conditions / referents (i.e. `on`)
  recurse_expression = function(cur_expr) {
    cur_expr = unpack_parens(cur_expr)
    constructor = NULL

    # found something unexpected, e.g. a single term when all we should find should be at least a binary operator
    if (!is.recursive(cur_expr) || length(cur_expr) <= 1) throw()

    # recurse on `&&`: combine LHS and RHS
    if (identical(cur_expr[[1]], symbol_and)) {
      return(c(recurse_expression(cur_expr[[2]]), recurse_expression(cur_expr[[3]])))
    }

    # go by case: `==` (CondEqual), `%in%` (CondAnyOf), or error if anything else
    if (identical(cur_expr[[1]], symbol_equal)) {
      constructor = CondEqual
    } else if (identical(cur_expr[[1]], symbol_in)) {
      constructor = CondAnyOf
    } else {
      throw()
    }

    # get value and referent
    comparand = unpack_parens(cur_expr[[2]])
    value = unpack_parens(cur_expr[[3]])

    # referent must always be on the LHS. This is necessary because otherwise
    # we would not know which side is a value and which side is a Parameter name.
    # E.g.
    # a = 1.0
    # b = 2.0
    # ps(a = p_dbl(), b = p_dbl(),
    #   c = p_int(requires = a == b)
    # )
    # would be ambiguous if we did not demand that `a` is the name of the parameter, and
    # `b` is just the value (2.0).
    if (!is.symbol(comparand)) throw("LHS must be a parameter name")
    comparand = as.character(comparand)
    value = eval(value, envir = evalenv)
    list(list(on = comparand, cond = constructor$new(value)))
  }

  recurse_expression(requires_expr)
}
