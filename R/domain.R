#' @include helper.R

#' @title Domain: Parameter Range without an Id
#'
#' @description
#' A `Domain` object is a representation of a single dimension of a [`ParamSet`]. `Domain` objects are used to construct
#' [`ParamSet`]s, either through the [`ps()`] short form, or through the [`ParamSet`]`$search_space()` mechanism (see
#' [`to_tune()`]). `Domain` corresponds to a [`Param`] object, except it does not have an `$id`, and it *does* have a
#' `trafo` and dependencies (`depends`) associated with it. For each of the basic [`Param`] classes ([`ParamInt`],
#' [`ParamDbl`], [`ParamLgl`], [`ParamFct`], and [`ParamUty`]) there is a function constructing a `Domain` object
#' (`p_int()`, `p_dbl()`, `p_lgl()`, `p_fct()`, `p_uty()`). They each have the same arguments as the corresponding
#' [`Param`] `$new()` function, except without the `id` argument, and with the the additional parameters `trafo`, and
#' `depends`.
#'
#' `Domain` objects are representations of parameter ranges and are intermediate objects to be used in short form
#' constructions in [`to_tune()`] and [`ps()`]. Because of their nature, they should not be modified by the user.
#' The `Domain` object's internals are subject to change and should not be relied upon.
#'
#' @template param_lower
#' @template param_upper
#' @param levels (`character` | `atomic` | `list`)\cr
#'   Allowed categorical values of the parameter. If this is not a `character`, then a `trafo` is generated that
#'   converts the names (if not given: `as.character()` of the values) of the `levels` argument to the values.
#'   This trafo is then performed *before* the function given as the `trafo` argument.
#' @template param_special_vals
#' @template param_default
#' @template param_tags
#' @template param_custom_check
#' @template param_tolerance
#' @param trafo (`function`)\cr
#'   Single argument function performing the transformation of a parameter. When the `Domain` is used to construct a
#'   [`ParamSet`], this transformation will be applied to the corresponding parameter as part of the `$trafo` function.\cr
#'   Note that the trafo is *not* inherited by [`TuneToken`]s! Defining a parameter
#'   with e.g. `p_dbl(..., trafo = ...)` will *not* automatically give the `to_tune()` assigned to it a transformation.
#'   `trafo` only makes sense for [`ParamSet`]s that get used as search spaces for optimization or tuning, it is not useful when
#'   defining domains or hyperparameter ranges of learning algorithms, because these do not use trafos.
#' @param depends (`call` | `expression`)\cr
#'   An expression indicating a requirement for the parameter that will be constructed from this. Can be given as an
#'   expression (using `quote()`), or the expression can be entered directly and will be parsed using NSE (see
#'   examples). The expression may be of the form `<Param> == <value>` or `<Param> %in% <values>`, which will result in
#'   dependencies according to `ParamSet$add_dep(on = "<Param>", cond = CondEqual$new(<value>))` or
#'   `ParamSet$add_dep(on = "<Param>", cond = CondAnyOf$new(<values>))`, respectively (see [`CondEqual`],
#'   [`CondAnyOf`]). The expression may also contain multiple conditions separated by `&&`.
#' @param logscale (`logical(1)`)\cr
#'   Put numeric domains on a log scale. Default `FALSE`. Log-scale `Domain`s represent parameter ranges where lower and upper bounds
#'   are logarithmized, and where a `trafo` is added that exponentiates sampled values to the original scale. This is
#'   *not* the same as setting `trafo = exp`, because `logscale = TRUE` will handle parameter bounds internally:
#'   a `p_dbl(1, 10, logscale = TRUE)` results in a [`ParamDbl`] that has lower bound `0`, upper bound `log(10)`,
#'   and uses `exp` transformation on these. Therefore, the given bounds represent the bounds *after* the transformation.
#'   (see examples).\cr
#'   `p_int()` with `logscale = TRUE` results in a [`ParamDbl`], not a [`ParamInt`], but with bounds `log(max(lower, 0.5))` ...
#'   `log(upper + 1)` and a trafo similar to "`as.integer(exp(x))`" (with additional bounds correction). The lower bound
#'   is lifted to `0.5` if `lower` 0 to handle the `lower == 0` case. The upper bound is increased to `log(upper + 1)`
#'   because the trafo would otherwise almost never generate a value of `upper`.\cr
#'   When `logscale` is `TRUE`, then upper bounds may be infinite, but lower bounds should be greater than 0 for `p_dbl()`
#'   or greater or equal 0 for `p_int()`.\cr
#'   Note that "logscale" is *not* inherited by [`TuneToken`]s! Defining a parameter
#'   with `p_dbl(... logscale = TRUE)` will *not* automatically give the `to_tune()` assigned to it log-scale. `logscale`
#'   only makes sense for [`ParamSet`]s that get used as search spaces for optimization or tuning, it is not useful when
#'   defining domains or hyperparameter ranges of learning algorithms, because these do not use trafos.\cr
#'   `logscale` happens on a natural (`e == 2.718282...`) basis. Be aware that using a different base (`log10()`/`10^`,
#'   `log2()`/`2^`) is completely equivalent and does not change the values being sampled after transformation.
#'
#' @return A `Domain` object.
#'
#' @details
#' The `p_fct` function admits a `levels` argument that goes beyond the `levels` accepted by [`ParamFct`]`$new()`.
#' Instead of a `character` vector, any atomic vector or list (optionally named) may be given. (If the value is a list
#' that is not named, the names are inferred using `as.character()` on the values.) The resulting `Domain` will
#' correspond to a range of values given by the names of the `levels` argument with a `trafo` that maps the `character`
#' names to the arbitrary values of the `levels` argument.
#'
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
#'
#' # logscale:
#' params = ps(x = p_dbl(1, 100, logscale = TRUE))
#'
#' # The ParamSet has bounds log(1) .. log(100):
#' print(params)
#'
#' # When generating a equidistant grid, it is equidistant within log values
#' grid = generate_design_grid(params, 3)
#' print(grid)
#'
#' # But the values are on a log scale with desired bounds after trafo
#' print(grid$transpose())
#'
#' # Integer parameters with logscale are `ParamDbl`s pre-trafo
#' params = ps(x = p_int(0, 10, logscale = TRUE))
#' print(params)
#'
#' grid = generate_design_grid(params, 4)
#' print(grid)
#'
#' # ... but get transformed to integers.
#' print(grid$transpose())
#'
#' @family ParamSet construction helpers
#' @name Domain
NULL

#' @rdname Domain
#' @export
p_int = function(lower = -Inf, upper = Inf, special_vals = list(), default = NO_DEF, tags = character(), depends = NULL, trafo = NULL, logscale = FALSE) {
  if (assert_flag(logscale)) {
    if (!is.null(trafo)) stop("When a trafo is given then logscale must be FALSE")
    # assert_int will stop for `Inf` values
    if (!isTRUE(is.infinite(lower))) assert_int(lower)
    if (!isTRUE(is.infinite(upper))) assert_int(upper)
    if (lower < 0) stop("When logscale is TRUE then lower bound must be greater or equal 0")
    trafo = crate(function(x) as.integer(max(min(exp(x), upper), lower)), lower, upper)
    constargs_override = list(lower = log(max(lower, 0.5)), upper = log(upper + 1))
    constructor = ParamDbl
  } else {
    constructor = ParamInt
    constargs_override = NULL
  }

  domain(constructor = constructor, constargs = as.list(match.call()[-1]),
    depends_expr = substitute(depends), trafo = trafo, constargs_override = constargs_override)
}

#' @rdname Domain
#' @export
p_dbl = function(lower = -Inf, upper = Inf, special_vals = list(), default = NO_DEF, tags = character(), tolerance = sqrt(.Machine$double.eps), depends = NULL, trafo = NULL, logscale = FALSE) {
  if (assert_flag(logscale)) {
    if (!is.null(trafo)) stop("When a trafo is given then logscale must be FALSE")
    if (assert_number(lower) <= 0) stop("When logscale is TRUE then lower bound must be strictly greater than 0")
    trafo = exp
    constargs_override = list(lower = log(lower), upper = log(assert_number(upper)))
  } else {
    constargs_override = NULL
  }

  domain(constructor = ParamDbl, constargs = as.list(match.call()[-1]),
    depends_expr = substitute(depends), trafo = trafo, constargs_override = constargs_override)
}

#' @rdname Domain
#' @export
p_uty = function(default = NO_DEF, tags = character(), custom_check = NULL, depends = NULL, trafo = NULL) {
  domain(constructor = ParamUty, constargs = as.list(match.call()[-1]),
    depends_expr = substitute(depends), trafo = trafo)
}

#' @rdname Domain
#' @export
p_lgl = function(special_vals = list(), default = NO_DEF, tags = character(), depends = NULL, trafo = NULL) {
  domain(constructor = ParamLgl, constargs = as.list(match.call()[-1]),
    depends_expr = substitute(depends), trafo = trafo)
}

#' @rdname Domain
#' @export
p_fct = function(levels, special_vals = list(), default = NO_DEF, tags = character(), depends = NULL, trafo = NULL) {
  constargs = as.list(match.call()[-1])
  levels = eval.parent(constargs$levels)
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
    constargs$levels = names(levels)
  }
  domain(constructor = ParamFct, constargs = constargs, depends_expr = substitute(depends), trafo = trafo)
}

# Construct the actual `Domain` object
# @param Constructor: The ParamXxx to call `$new()` for.
# @param constargs: arguments of constructor
# @param constargs_override: replace these in `constargs`, but don't represent this in printer
domain = function(constructor, constargs, depends_expr = NULL, trafo = NULL, constargs_override = NULL) {
  constargs$trafo = NULL
  constargs$depends = NULL
  constargs = map(constargs, eval, envir = parent.frame(2))
  reprargs = constargs

  constargs$logscale = NULL
  constargs = insert_named(constargs, constargs_override)


  if ("id" %in% names(constargs)) stop("id must not be given to p_xxx")

  # check that `...` are valid by constructing and making sure this doesn't error
  # The object generated here is thrown away, this is only for checks.
  param = invoke(constructor$new, id = "ID", .args = constargs)

  # depends may be an expression, but may also be quote() or expression()
  if (length(depends_expr) == 1) {
    depends_expr = eval(depends_expr, envir = parent.frame(2))
    if (!is.language(depends_expr)) {
      stop("'depends' argument must be an expression involving `==` or `%in%`, or must be a single variable containing a quoted expression.")
    }
  }

  # repr: what to print
  repr = sys.call(-1)
  traforep = repr$trafo

  repr = as.call(c(as.list(repr)[[1]], reprargs))  # use cleaned up constargs
  repr$depends = depends_expr  # put `depends` at the end, but only if not NULL
  repr$trafo = traforep  # put `trafo` at the end, but only if not NULL
  if (isTRUE(repr$logscale)) repr$trafo = NULL  # when the user declared logscale then the trafo stays hidden.

  set_class(list(
    param = param,
    trafo = assert_function(trafo, null.ok = TRUE),
    requirements = parse_depends(depends_expr, parent.frame(2)),
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
# parse_depends(quote(x == 1 && y %in% c("b", "c")), environment())
# # same as:
# list(
#   list(on = "x", CondEqual$new(1)),
#   list(on = "y", CondAnyOf$new(c("b", "c")))
# )
parse_depends = function(depends_expr, evalenv) {
  if (is.null(depends_expr)) return(NULL)

  # throw(): Give generic helpful error message.
  throw = function(msg = NULL) stopf("Requirement '%s' is broken%s", deparse1(depends_expr), if (!is.null(msg)) sprintf(":\n%s", msg) else "")

  if (!is.language(depends_expr)) throw()
  if (identical(depends_expr[[1]], quote(quote))) {
    depends_expr = depends_expr[[2]]
  }
  if (is.expression(depends_expr)) {
    if (length(depends_expr) != 1) {
      throw("given 'expression' objects must have length 1.")
    }
    depends_expr = depends_expr[[1]]
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
    #   c = p_int(depends = a == b)
    # )
    # would be ambiguous if we did not demand that `a` is the name of the parameter, and
    # `b` is just the value (2.0).
    if (!is.symbol(comparand)) throw("LHS must be a parameter name")
    comparand = as.character(comparand)
    value = eval(value, envir = evalenv)
    list(list(on = comparand, cond = constructor$new(value)))
  }

  recurse_expression(depends_expr)
}
