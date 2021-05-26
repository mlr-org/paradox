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
#' The `Domain` object's internals are subject to change and should not be relid upon.
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


# Construct the actual `Domain` object
# @param Constructor: The ParamXxx to call `$new()` for.
# @param constargs: arguments of constructor
# @param constargs_override: replace these in `constargs`, but don't represent this in printer
domain = function(cls, grouping, cargo = NULL, lower = NA_real_, upper = NA_real_, levels = NULL, special_vals = list(), default = NO_DEF, tags = character(0),
                  tolerance = NA_real_, trafo = NULL, storage_type = "list", depends_expr = NULL, init) {

  assert_string(cls)
  assert_string(grouping)
  assert_number(lower, na.ok = TRUE)
  assert_number(upper, na.ok = TRUE)
  # assert_character(levels, null.ok = TRUE)
  assert_list(special_vals)
  if (length(special_vals) && !is.null(trafo)) stop("trafo and special_values can not both be given at the same time.")
  assert_character(tags, any.missing = FALSE, unique = TRUE)
  assert_number(tolerance, na.ok = TRUE)
  assert_function(trafo, null.ok = TRUE)


  # depends may be an expression, but may also be quote() or expression()
  if (length(depends_expr) == 1) {
    depends_expr = eval(depends_expr, envir = parent.frame(2))
    if (!is.language(depends_expr)) {
      stop("'depends' argument must be an expression involving `==` or `%in%`, or must be a single variable containing a quoted expression.")
    }
  }


  # domain is a data.table with a few classes.
  # setting `id` to something preliminary so that `domain_assert()` works.
  param = data.table(cls = cls, grouping = grouping, cargo = list(cargo), lower = lower, upper = upper, tolerance = tolerance, levels = list(levels),
    special_vals = list(special_vals),
    default = list(default), tags = list(tags), trafo = list(trafo), requirements = list(parse_depends(depends_expr, parent.frame(2))),
    storage_type = storage_type, init_given = !missing(init), init = list(if (!missing(init)) init), id = "domain being constructed")
  class(param) = c(cls, "Domain", class(param))

  if (!is_nodefault(default)) {
    domain_assert(param, list(default))
    if ("required" %in% tags) stop("A 'required' parameter can not have a 'default'.\nWhen the method behaves the same as if the parameter value were 'X' whenever the parameter is missing, then 'X' should be a 'default', but the 'required' indicates that the parameter may not be missing.")
  }
  if (!missing(init)) {
    if (!is.null(trafo)) stop("Initial value and trafo can not both be given at the same time.")
    domain_assert(param, list(init))
    if (identical(init, default)) warning("Initial value and 'default' value seem to be the same, this is usually a mistake due to a misunderstanding of the meaning of 'default'.\nWhen the method behaves the same as if the parameter value were 'X' whenever the parameter is missing, then 'X' should be a 'default' (but then there is no point in setting it as initial value). 'default' should not be used to indicate the value with which values are initialized.")
  }

  # repr: what to print
  constructorcall = match.call(sys.function(-1), sys.call(-1), envir = parent.frame(2))
  trafoexpr = constructorcall$trafo
  constructorcall$trafo = NULL
  constructorcall$depends = NULL
  reprargs = sapply(names(constructorcall)[-1], get, pos = parent.frame(1), simplify = FALSE)
  reprargs$depends = depends_expr
  reprargs$trafo = trafoexpr
  if (isTRUE(reprargs$logscale)) reprargs$trafo = NULL
  attr(param, "repr") = as.call(c(constructorcall[[1]], reprargs))
  param$id = repr(attr(param, "repr"))  # some ID for consistency with ParamSet$params, only for error messages.
  param
}

#' @export
print.Domain = function(x, ...) {
  repr = attr(x, "repr")
  if (!is.null(repr)) {
    print(repr)
  } else {
    plural_rows =
    classes = class(x)
    if ("Domain" %in% classes) {
      domainidx = which("Domain" == classes)[[1]]
      classes = first(classes, domainidx - 1)
      class(x) = last(class(x), -domainidx)
    }
    catf("Param%s of class%s %s:\n",
      if (NROW(x) > 1) "s" else "",
      if (length(classes) > 1) "es" else "",
      str_collapse(classes, sep = ", ", quote = '"')
    )
    print(x)
  }
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
