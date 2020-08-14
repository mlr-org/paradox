# Tune token
#' @export
tune = function(...) {
  call = sys.call()
  if (...length() > 2) {
    stop("tune() must have zero, one or two arguments.")
  }
  if (...length() == 2) {
    type = "range"
    content = (function(lower, upper) {
      list(assert_number(lower), assert_number(upper))}
    )(...)
  } else if (...length() == 1) {
    type = "other"
    content = list(...)[[1]]
    assert(
      check_atomic_vector(content),
      check_list(content, names = "unique"),
      check_list(content, names = "unnamed"),
      check_class(content, "ParamSet"),
      check_class(content, "Param"),
      check_class(content, "Domain")
    )
  } else {
    type = "full"
    content = list()
  }

  structure(list(content = content, call = call, type = type), class = "TuneToken")
}

#' @export
print.TuneToken = function(x, ...) {
  if (!is.null(attr(x, "ps"))) {
    cat("Tuning over:\n")
    print(attr(x, "ps"))
  } else {
    print(x$call)
  }
}

tunetoken_to_ps = function(tt, param) {
  ttstr = deparse1(tt$call)
  content = tt$content
  if (tt$type == "full") {
    if (!param$is_bounded) {
      stopf("%s must give a range for unbounded parameter %s.", ttstr, param$id)
    }
    content = param$clone(deep = TRUE)
  } else if (tt$type == "range") {
    if (!param$is_number) {
      stopf("%s for non-numeric param must have one argument.", ttstr)
    }
    if (!all(map_lgl(content, param$test))) {
      stopf("%s not compatible with param %s", ttstr, param$id)
    }
    content = get(param$class)$new(id = param$id, lower = content[[1]], upper = content[[2]])
  }
  if (inherits(content, "Domain")) {
    content = do.call(ps, structure(list(content), names = param$id))
  }
  if (inherits(content, "Param")) {
    content = ParamSet$new(list(content))
  }
  if (!inherits(content, "ParamSet")) {
    if (!all(map_lgl(content, param$test))) {
      stopf("%s not compatible with param %s", ttstr, param$id)
    }
    content = do.call(ps, structure(list(p_fct(levels = content)), names = param$id))
  } else {
    testpoints = generate_design_grid(content, 2)$transpose()
    if (!all(map_lgl(testpoints, function(x) {
        identical(names(x), param$id) && param$test(x[[1]])
      }))) {
      stopf("%s not compatible with param %s", ttstr, param$id)
    }
  }
  content$set_id = ""
  content
}

