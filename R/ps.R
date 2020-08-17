#' @include helper.R
#' @export
ps = function(..., .extra_trafo = NULL) {
  args = list(...)
  assert_list(args, names = "unique", types = c("Param", "Domain"))
  assert_function(.extra_trafo, null.ok = TRUE)
  params = imap(args, function(p, name) {
    if (inherits(p, "Param")) {
      p = p$clone(deep = TRUE)
      p$id = name
      p
    } else {
      invoke(p$constructor$new, id = name, .args = p$constargs)
    }
  })

  paramset = ParamSet$new(params)

  imap(args, function(p, name) {
    if (inherits(p, "Param") || is.null(p$requirements)) return(NULL)
    map(p$requirements, function(req) {
      if (!req$on %in% names(args) || req$on == name) {
        stopf("Parameter %s can not depend on %s.", name, req$on)
      }
      invoke(paramset$add_dep, id = name, .args = req)
    })
  })

  trafos = map(discard(args, function(x) inherits(x, "Param") || is.null(x$trafo)),
    function(p) {
      assert_function(p$trafo)
    })

  if (length(trafos) || !is.null(.extra_trafo)) {
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


#' @export
p_int = p_dbl = p_fct = p_lgl = p_uty = p_lgl = function(..., requires = NULL, trafo = NULL) {
  constargs = list(...)
  if ("id" %in% names(constargs)) stop("id must not be given to p_xxx")
  constructor = switch(as.character(sys.call()[[1]]),
    p_int = ParamInt,
    p_dbl = ParamDbl,
    p_uty = ParamUty,
    p_lgl = ParamLgl,
    p_fct = {
      extracted = (function(id, levels, ...) {
        list(levels = levels, rest = list(...))
      })(id = "ID", ...)
      levels = extracted$levels
      if (!is.character(levels)) {
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
      }
      ParamFct
    },
    stop("Function must not be renamed.")
  )

  # check that this doesn't error
  invoke(constructor$new, id = "ID", .args = constargs)

  requires_expr = substitute(requires)
  if (length(requires_expr) == 1 ||
      isTRUE(as.character(requires_expr[[1]]) %in% c("quote", "expression"))) {
    requires_expr = requires
  }

  structure(list(
    constructor = constructor,
    constargs = constargs,
    trafo = assert_function(trafo, null.ok = TRUE),
    requirements = parse_requires(requires_expr, environment()),
    callsymbol = sys.call()[[1]],
    reqtraforep = discard(as.list(sys.call())[c("trafo", "requires")], is.null)
  ), class = "Domain")
}

#' @export
print.Domain = function(x, ...) {
  print(as.call(c(list(x$callsymbol), x$constargs, x$reqtraforep)))
}

parse_requires = function(requires_expr, evalenv) {
  if (is.null(requires_expr)) return(NULL)
  throw = function(msg = NULL) stopf("Requirement '%s' is broken%s", deparse1(requires_expr), sprintf(":\n%s", msg))

  if (!is.language(requires_expr)) throw()

  symbol_paren = as.symbol("(")
  symbol_equal = as.symbol("==")
  symbol_and = as.symbol("&&")
  symbol_in = as.symbol("%in%")

  unpack_parens = function(expr) {
    while (is.recursive(expr) && length(expr) && identical(expr[[1]], symbol_paren)) expr = expr[[2]]
    expr
  }

  recurse_expression = function(cur_expr) {
    cur_expr = unpack_parens(cur_expr)
    constructor = NULL
    if (!is.recursive(cur_expr) || length(cur_expr) <= 1) throw()
    if (identical(cur_expr[[1]], symbol_and)) {
      return(c(recurse_expression(cur_expr[[2]]), recurse_expression(cur_expr[[3]])))
    }

    if (identical(cur_expr[[1]], symbol_equal)) {
      constructor = CondEqual
    } else if (identical(cur_expr[[1]], symbol_in)) {
      constructor = CondAnyOf
    } else {
      throw()
    }

    comparand = unpack_parens(cur_expr[[2]])
    value = unpack_parens(cur_expr[[3]])

    if (!is.symbol(comparand)) throw("LHS must be a parameter name")
    comparand = as.character(comparand)
    value = eval(value, envir = evalenv)
    list(list(on = comparand, cond = constructor$new(value)))
  }

  recurse_expression(requires_expr)
}

