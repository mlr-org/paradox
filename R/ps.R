ps = function(..., .extra_trafo = NULL) {
  args = list(...)
  assert_list(args, names = "unique")
  assert_function(.extra_trafo, null.ok = TRUE)
  params = imap(args, function(p, name) {
    if (inherits(p, "Param")) return(p)
    assert_class(p$constructor, "R6ClassGenerator")
    do.call(p$constructor$new, c(list(id = name), p$constargs))
  })

  paramset = ParamSet$new(params)

  requirements = unlist(unname(imap(discard(args, function(x) inherits(x, "Param") || is.null(x$requirements)),
    function(p, name) {
      reqinfo = assert_list(p$requirements)
      reduce_requires(reqinfo$requires_expr, names(args), name, reqinfo$env)
    })), recursive = FALSE)

  map(requirements, do.call, what = paramset$add_dep)

  trafos = map(discard(args, function(x) inherits(x, "Param") || is.null(x$trafo)),
    function(p) {
      assert_function(p$trafo)
    })
  if (length(trafos) || !is.null(.extra_trafo)) {
    trafoenv = new.env(parent = .GlobalEnv)
    trafoenv$trafos = trafos
    trafoenv$.extra_trafo = .extra_trafo

    trafofun = function(x, param_set) {
      for (trafoing in names(trafos)) {
        if (!is.null(x[[trafoing]])) {
          x[[trafoing]] = trafos[[trafoing]](x[[trafoing]])
        }
      }
      if (!is.null(.extra_trafo)) x = .extra_trafo(x, param_set)
      x
    }
    environment(trafofun) = trafoenv
    paramset$trafo = trafofun
  }
  paramset
}

reduce_requires = function(requires_expr, paramset_names, own_name, evalenv) {
  if (tryCatch(length(requires_expr) == 1 ||
        as.character(requires_expr[[1]]) %in% c("quote", "expression"),
      error = function(e) FALSE)) {
    requires_expr = eval(requires_expr, evalenv)
  }

  throw = function() stopf("Requirement of parameter %s is broken: '%s'", own_name, deparse1(requires_expr))

  if (!is.language(requires_expr)) throw()

  unpack_parens = function(expr) {
    while (is.recursive(expr) && length(expr) && identical(expr[[1]], as.symbol("("))) expr = expr[[2]]
    expr
  }

  recurse_expression = function(cur_expr) {
    cur_expr = unpack_parens(cur_expr)
    constructor = NULL
    if (!is.recursive(cur_expr) || length(cur_expr) <= 1) throw()
    if (identical(cur_expr[[1]], as.symbol("&&"))) return(c(recurse_expression(cur_expr[[2]]), recurse_expression(cur_expr[[3]])))
    if (identical(cur_expr[[1]], as.symbol("=="))) {
      constructor = CondEqual
    } else if (identical(cur_expr[[1]], as.symbol("%in%"))) {
      constructor = CondAnyOf
    } else {
      throw()
    }

    comparand = unpack_parens(cur_expr[[2]])
    value = unpack_parens(cur_expr[[3]])
    if (identical(cur_expr[[1]], as.symbol("==")) && is.symbol(value) && as.character(value) %in% paramset_names) {
      tmp <- comparand
      comparand <- value
      value <- tmp
    }
    if (!is.symbol(comparand)) throw()
    comparand <- as.character(comparand)
    if (!comparand %in% paramset_names || comparand == own_name) throw()
    value <- eval(value, evalenv)
    list(list(own_name, comparand, constructor$new(value)))
  }

  recurse_expression(requires_expr)
}

p_int = p_dbl = p_fct = p_uty = function(..., requires, trafo) {
  constructor = switch(as.character(sys.call()[[1]]),
    p_int = ParamInt,
    p_dbl = ParamDbl,
    p_fct = ParamFct,
    p_uty = ParamUty,
    stop("Function must not be renamed.")
  )

  list(
    constructor = constructor,
    constargs = list(...),
    trafo = if (!missing(trafo)) assert_function(trafo),
    requirements = if (!missing(requires)) list(requires_expr = substitute(requires), env = parent.frame())
  )
}
