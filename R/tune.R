


# Tune token
#' @export
tune = function(...) {
  structure(list(...), call = sys.call(), class = "TuneToken")
}

print.TuneToken = function(x, ...) {
  if (!is.null(attr(x, "ps"))) {
    cat("Tuning over:\n")
    print(attr(x, "ps"))
  } else {
    print(attr(x, "call"))
  }
}

tunetoken_to_ps = function(tt, param) {
  ttstr = deparse1(attr(tt, "call"))
  if (length(tt) > 2) {
    stopf("%s must have zero, one or two arguments.", ttstr)
  }
  if (length(tt) == 0) {
    if (!param$is_bounded) {
      stopf("%s must give a range for unbounded parameter %s.", ttstr, param$id)
    }
    tt = list(param$clone(deep = TRUE))
  }
  if (length(tt) == 2) {
    if (!param$is_number) {
      stopf("%s for non-numeric param must have one argument.", ttstr)
    }
    if (!is.null(names(tt)) && !test_subset(names(tt),
        c(names(formals(get(param$class)$public_methods$initialize))[2:3], ""))) {
      stopf("%s should only have arguments `lower` and `upper`, or `nlevels`.", ttstr)
    }
    if (!param$test(tt[[1]]) || !param$test(tt[[2]])) {
      stopf("%s not compatible with param %s", ttstr, param$id)
    }
    tt = list(do.call(get(param$class)$new, c(list(id = param$id), tt)))
  }
  if (is.list(tt[[1]]) && identical(names(tt[[1]]), c("constructor", "constargs", "trafo", "requirements"))) {
    names(tt) = param$id
    tt = list(do.call(ps, tt))
  }
  if (inherits(tt[[1]], "Param")) {
    tt = list(ParamSet$new(tt))
  }
  if (!inherits(tt[[1]], "ParamSet")) {
    if (!all(map_lgl(tt[[1]], param$test))) {
      stopf("%s not compatible with param %s", ttstr, param$id)
    }
    if (!is.null(names(tt)) && !identical(names(tt), "levels")) {
      stopf("%s should only have arguments `lower` and `upper`, or `nlevels`.", ttstr)
    }
    if (is.character(tt[[1]])) {
      tt = list(ParamSet$new(list(ParamFct$new(param$id, tt[[1]]))))
    } else {
      if (!test_atomic_vector(tt[[1]]) && !is.list(tt[[1]])) {
        stopf("%s nlevels is not a vector or list.", ttstr)
      }
      if (is.null(names(tt[[1]]))) {
        names(tt[[1]]) = as.character(tt[[1]])
      }
      fenv = new.env(parent = .GlobalEnv)
      fenv$tt = tt[[1]]
      trafo = function(x) tt[[x]]
      environment(trafo) = fenv
      tt = list(p_fct(names(tt[[1]]), trafo = trafo))
      names(tt) = param$id
      tt = list(do.call(ps, tt))
    }
  } else {
    testpoints = generate_design_grid(tt[[1]], 2)$transpose()
    if (!all(map_lgl(testpoints, function(x) {
        identical(names(x), param$id) && param$test(x[[1]])
      }))) {
      stopf("%s not compatible with param %s", ttstr, param$id)
    }
  }
  tt[[1]]$set_id = ""
  tt[[1]]
}

