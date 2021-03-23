#' @include helper.R

#' @title Indicate that a Parameter Value should be Tuned
#'
#' @description
#' `to_tune()` creates a `TuneToken` object which can be assigned to the `$values` slot of a [`ParamSet`] as an
#' alternative to a concrete value. This indicates that the value is not given directly but should be tuned using
#' \CRANpkg{bbotk} or \CRANpkg{mlr3tuning}. If the thus parameterized object
#' is invoked directly, without being wrapped by or given to a tuner, it will give an error.
#'
#' The tuning range [`ParamSet`] that is constructed from the `TuneToken` values in a [`ParamSet`]'s `$values` slot
#' can be accessed through the `ParamSet$search_space()` method. This is done automatically by tuners if no tuning range
#' is given, but it is also possible to access the `$search_space()` method, modify it further, and give the modified
#' [`ParamSet`] to a tuning function (or do anything else with it, nobody is judging you).
#'
#' A `TuneToken` represents the range over which the parameter whose `$values` slot it occupies should be tuned over. It
#' can be constructed via the `to_tune()` function in one of several ways:
#'
#' * **`to_tune()`**: Indicates a parameter should be tuned over its entire range. Only applies to finite parameters
#'   (i.e. discrete or bounded numeric parameters)
#' * **`to_tune(lower, upper, logscale)`**: Indicates a numeric parameter should be tuned in the inclusive interval spanning
#'   `lower` to `upper`, possibly on a log scale if `logscale` is se to `TRUE`. All parameters are optional, and the
#'   parameter's own lower / upper bounds are used without log scale, by default. Depending on the parameter,
#'   integer (if it is a [`ParamInt`]) or real values (if it is a [`ParamDbl`]) are used.\cr
#'   `lower`, `upper`, and `logscale` can be given by position, except when only one of them is given, in which case
#'   it must be named to disambiguate from the following cases.\cr
#'   When `logscale` is `TRUE`, then a `trafo` is generated automatically that transforms to the given bounds. The
#'   bounds are log()'d pre-trafo (see examples). See the `logscale` argument of [`Domain`] functions for more info.\cr
#'   Note that "logscale" is *not* inherited from the [`Param`] that the `TuneToken` belongs to! Defining a parameter
#'   with `p_dbl(... logscale = TRUE)` will *not* automatically give the `to_tune()` assigned to it log-scale.
#' * **`to_tune(levels)`**: Indicates a parameter should be tuned through the given discrete values. `levels` can be any
#'   named or unnamed atomic vector or list (although in the unnamed case it must be possible to construct a
#'   corresponding `character` vector with distinct values using `as.character`).
#' * **`to_tune(<Domain>)`**: The given [`Domain`] object (constructed e.g. with [`p_int()`] or [`p_fct()`]) indicates
#'   the range which should be tuned over. The supplied `trafo` function is used for parameter transformation.
#' * **`to_tune(<Param>)`**: The given [`Param`] object indicates the range which should be tuned over.
#' * **`to_tune(<ParamSet>)`**: The given [`ParamSet`] is used to tune over a single `Param`. This is useful for cases
#'   where a single evaluation-time parameter value (e.g. [`ParamUty`]) is constructed from multiple tuner-visible
#'   parameters (which may not be `ParamUty`). The supplied [`ParamSet`] should always contain a `$trafo` function,
#'   which must always return a `list` with a single entry.
#'
#' The `TuneToken` object's internals are subject to change and should not be relied upon. `TuneToken` objects should
#' only be constructed via `to_tune()`, and should only be used by giving them to `$values` of a [`ParamSet`].
#' @param ... if given, restricts the range to be tuning over, as described above.
#' @return A `TuneToken` object.
#' @examples
#' params = ParamSet$new(list(
#'   ParamInt$new("int", 0, 10),
#'   ParamInt$new("int_unbounded"),
#'   ParamDbl$new("dbl", 0, 10),
#'   ParamDbl$new("dbl_unbounded"),
#'   ParamDbl$new("dbl_bounded_below", lower = 1),
#'   ParamFct$new("fct", c("a", "b", "c")),
#'   ParamUty$new("uty1"),
#'   ParamUty$new("uty2"),
#'   ParamUty$new("uty3"),
#'   ParamUty$new("uty4"),
#'   ParamUty$new("uty5")
#' ))
#'
#' params$values = list(
#'
#'   # tune over entire range of `int`, 0..10:
#'   int = to_tune(),
#'
#'   # tune over 2..7:
#'   int_unbounded = to_tune(2, 7),
#'
#'   # tune on a log scale in range 1..10;
#'   # recognize upper bound of 10 automatically, but restrict lower bound to 1:
#'   dbl = to_tune(lower = 1, logscale = TRUE),
#'   ## This is equivalent to the following:
#'   # dbl = to_tune(p_dbl(log(1), log(10), trafo = exp)),
#'
#'   # nothing keeps us from tuning a dbl over integer values
#'   dbl_unbounded = to_tune(p_int(1, 10)),
#'
#'   # tune over values "a" and "b" only
#'   fct = to_tune(c("a", "b")),
#'
#'   # tune over integers 2..8.
#'   # ParamUty needs type information in form of p_xxx() in to_tune.
#'   uty1 = to_tune(p_int(2, 8)),
#'
#'   # tune uty2 like a factor, trying 1, 10, and 100:
#'   uty2 = to_tune(c(1, 10, 100)),
#'
#'   # tune uty3 like a factor. The factor levels are the names of the list
#'   # ("exp", "square"), but the trafo will generate the values from the list.
#'   # This way you can tune an objective that has function-valued inputs.
#'   uty3 = to_tune(list(exp = exp, square = function(x) x^2)),
#'
#'   # tune through multiple parameters. When doing this, the ParamSet in tune()
#'   # must have the trafo that generates a list with one element and the right
#'   # name:
#'   uty4 = to_tune(ps(
#'     base = p_dbl(0, 1),
#'     exp = p_int(0, 3),
#'     .extra_trafo = function(x, param_set) {
#'       list(uty4 = x$base ^ x$exp)
#'     }
#'   )),
#'
#'   # not all values need to be tuned!
#'   uty5 = 100
#' )
#'
#' print(params$values)
#'
#' print(params$search_space())
#'
#' # Change `$values` directly and generate new `$search_space()` to play around
#' params$values$uty3 = 8
#' params$values$uty2 = to_tune(c(2, 4, 8))
#'
#' print(params$search_space())
#'
#' # Notice how `logscale` applies `log()` to lower and upper bound pre-trafo:
#' params = ParamSet$new(list(ParamDbl$new("x")))
#'
#' params$values$x = to_tune(1, 100, logscale = TRUE)
#'
#' print(params$search_space())
#'
#' grid = generate_design_grid(params$search_space(), 3)
#'
#' # The grid is equidistant within log-bounds pre-trafo:
#' print(grid)
#'
#' # But the values are on a log scale scale with desired bounds after trafo:
#' print(grid$transpose())
#'
#' @family ParamSet construction helpers
#' @aliases TuneToken
#' @export
to_tune = function(...) {
  call = sys.call()
  if (...length() > 3) {
    stop("to_tune() must have zero arguments (tune entire parameter range), one argument (a Domain/Param, or a vector/list of values to tune over), or up to three arguments (any of `lower`, `upper`, `logscale`).")
  }
  args = list(...)
  if (...length() > 1 || any(names(args) %in% c("lower", "upper"))) {
    # Two arguments: tune over a range
    type = "RangeTuneToken"
    content = (function(lower = NULL, upper = NULL, logscale = FALSE) {
      list(lower = assert_number(lower, null.ok = TRUE), upper = assert_number(upper, null.ok = TRUE), logscale = assert_flag(logscale))}
    )(...)
  } else if (...length() == 1) {
    if (identical(names(args), "logscale")) {
      assert_flag(args[[1]], .var.name = "logscale")
      type = "FullTuneToken"
      content = list(logscale = args[[1]])
    } else {
      content = args[[1]]
      # one argument: tune over an object. that object can be something
      # that can be converted to a ParamSet (ParamSet itself, Param, or Domain),
      # otherwise it must be something that can be converted to a ParamFct Domain.
      if (!test_multi_class(content, c("ParamSet", "Param", "Domain"))) {
        assert(
          check_atomic_vector(content, names = "unnamed"),
          check_atomic_vector(content, names = "unique"),
          check_list(content, names = "unique"),
          check_list(content, names = "unnamed")
        )
        content = p_fct(levels = content)
      } else {
        if (inherits(content, "Domain")) {
          bounded = ps(x = content, .allow_dangling_dependencies = TRUE)$is_bounded
        } else {
          bounded = content$is_bounded
        }
        if (!bounded) {
          stop("tuning range must be bounded.")
        }
      }
      type = "ObjectTuneToken"
    }
  } else {
    # Zero arguments: Tune over whole parameter
    type = "FullTuneToken"
    content = list(logscale = FALSE)
  }

  set_class(list(content = content, call = deparse1(call)), c(type, "TuneToken"))
}

#' @export
print.FullTuneToken = function(x, ...) {
  catf("Tuning over:\n<entire parameter range%s>\n",
    if (isTRUE(x$content$logscale)) " (log scale)" else "")
}

#' @export
print.RangeTuneToken = function(x, ...) {
  catf("Tuning over:\nrange [%s, %s]%s\n", x$content$lower %??% "...", x$content$upper %??% "...",
    if (isTRUE(x$content$logscale)) " (log scale)" else "")
}

#' @export
print.ObjectTuneToken = function(x, ...) {
  cat("Tuning over:\n")
  print(x$content)
}

# tunetoken_to_ps: Convert a `TuneToken` to a `ParamSet` that tunes over this.
# Needs the corresponding `Param` to which the `TuneToken` refers, both to
# get the range (e.g. if `to_tune()` was used) and to verify that the `TuneToken`
# does not go out of range.
#
# Makes liberal use to `pslike_to_ps` (converting Param, ParamSet, Domain to ParamSet)
tunetoken_to_ps = function(tt, param, id) {
  UseMethod("tunetoken_to_ps")
}

tunetoken_to_ps.FullTuneToken = function(tt, param, id) {
  if (!param$is_bounded) {
    stopf("%s must give a range for unbounded parameter %s.", tt$call, id)
  }
  if (isTRUE(tt$content$logscale)) {
    if (!param$is_number) stop("%s (%s): logscale only valid for numeric / integer parameters.", tt$call, id)
    tunetoken_to_ps.RangeTuneToken(list(content = list(logscale = tt$content$logscale), tt$call), param, id)
  } else {
    pslike_to_ps(param, tt$call, param, id)
  }
}

tunetoken_to_ps.RangeTuneToken = function(tt, param, id) {
  if (!param$is_number) {
    stopf("%s for non-numeric param must have zero or one argument.", tt$call)
  }
  invalidpoints = discard(tt$content, function(x) is.null(x) || param$test(x))
  invalidpoints$logscale = NULL
  if (length(invalidpoints)) {
    stopf("%s range not compatible with param %s.\nBad value(s):\n%s\nParameter:\n%s",
      tt$call, id, repr(invalidpoints), repr(param))
  }

  lower = tt$content$lower %??% param$lower
  upper = tt$content$upper %??% param$upper

  if (!is.finite(lower) || !is.finite(upper)) stopf("%s range must be bounded, but is [%s, %s]", id, lower, upper)

  if (isTRUE(tt$content$logscale)) {
    # for logscale: create p_int / p_dbl object. Doesn't work if there is a numeric param class that we don't know about.
    constructor = switch(param$class, ParamInt = p_int, ParamDbl = p_dbl,
      stopf("%s: logscale for parameter %s of class %s not supported", tt$call, id, param$class))
    content = constructor(lower = lower, upper = upper, logscale = tt$content$logscale)
  } else {
    # general approach: create Param object
    content = get_r6_constructor(param$class)$new(id = id, lower = lower, upper = upper)
  }
  pslike_to_ps(content, tt$call, param, id)
}

tunetoken_to_ps.ObjectTuneToken = function(tt, param, id) {
  pslike_to_ps(tt$content, tt$call, param, id)
}

# Convert something that is `ParamSet`-like (ParamSet, Param, or Domain) to a `ParamSet`.
# * content is ParamSet --> verify that it is compatible with given `Param`
# * content is Param --> Wrap in ParamSet
# * content is Domain --> Wrap in ParamSet, using ps()
# @param pslike: thing to convert
# @param call: to_tune()-call, for better debug message
# @param param: `Param`, that the `pslike` refers to, and therefore needs to be compatible to
# @param usersupplied: whether the `pslike` is supplied by the user (and should therefore be checked more thoroughly)
#   This is currently used for user-supplied ParamSets, for which the trafo must be adjusted.
pslike_to_ps = function(pslike, call, param, id, usersupplied = TRUE) {
  UseMethod("pslike_to_ps")
}

pslike_to_ps.Domain = function(pslike, call, param, id, usersupplied = TRUE) {
  pslike = invoke(ps, .allow_dangling_dependencies = TRUE, .args = set_names(list(pslike), id))
  pslike_to_ps(pslike, call, param, id, usersupplied = FALSE)
}

pslike_to_ps.Param = function(pslike, call, param, id, usersupplied = TRUE) {
  pslike = pslike$with_id(id)
  pslike = ParamSet$new(list(pslike))
  pslike_to_ps(pslike, call, param, id, usersupplied = FALSE)
}

pslike_to_ps.ParamSet = function(pslike, call, param, id, usersupplied = TRUE) {
  pslike = pslike$clone(deep = TRUE)
  alldeps = pslike$deps
  # temporarily hide dangling deps
  on = NULL  # pacify static code check
  pslike$deps = pslike$deps[on %in% pslike$ids()]
  testpoints = generate_design_grid(pslike, 2)$transpose()
  pslike$deps = alldeps
  invalidpoints = discard(testpoints, function(x) length(x) == 1)
  if (length(invalidpoints)) {
    stopf("%s for param %s does not have a trafo that reduces output to one dimension.\nExample:\n%s",
      call, id, repr(invalidpoints[[1]]))
  }
  invalidpoints = discard(testpoints, function(x) param$test(x[[1]]))
  if (length(invalidpoints)) {
    stopf("%s generates points that are not compatible with param %s.\nBad value:\n%s\nParameter:\n%s",
      call, id, repr(invalidpoints[[1]][[1]]), repr(param))
  }
  if (usersupplied) {
    trafo = pslike$trafo %??% identity
    pname = id
    pslike$trafo = crate(function(x, param_set) {
      mlr3misc::set_names(
        checkmate::assert_list(trafo(x), len = 1, .var.name = sprintf("Trafo for tuning ParamSet for parameter %s", pname)),
        pname
      )
    }, trafo, pname)
  }
  pslike$set_id = ""
  pslike
}
