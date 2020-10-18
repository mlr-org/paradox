# Tune token
#' @export
to_tune = function(...) {
  call = sys.call()
  if (...length() > 2) {
    stop("to_tune() must have zero, one or two arguments.")
  }
  if (...length() == 2) {
    type = "RangeTuneToken"
    content = (function(lower, upper) {
      list(lower = assert_number(lower), upper = assert_number(upper))}
    )(...)
  } else if (...length() == 1) {
    content = list(...)[[1]]
    if (!test_multi_class(content, c("ParamSet", "Param", "Domain"))) {
      assert(
        check_atomic_vector(content, names = "unnamed"),
        check_atomic_vector(content, names = "unique"),
        check_list(content, names = "unique"),
        check_list(content, names = "unnamed")
      )
      content = p_fct(levels = content)
    }
    type = "ObjectTuneToken"
  } else {
    type = "FullTuneToken"
    content = list()
  }

  structure(list(content = content, call = deparse1(call)), class = c(type, "TuneToken"))
}

#' @export
print.FullTuneToken = function(x, ...) {
  cat("Tuning over:\n<entire parameter range>\n")
}

#' @export
print.RangeTuneToken = function(x, ...) {
  catf("Tuning over:\nrange [%s, %s]\n", x$content$lower, x$content$upper)
}

#' @export
print.ObjectTuneToken = function(x, ...) {
  cat("Tuning over:\n")
  print(x$content)
}

tunetoken_to_ps = function(tt, param) {
  UseMethod("tunetoken_to_ps")
}

ttcontent_to_ps = function(content, tt, param) {
  UseMethod("ttcontent_to_ps")
}

tunetoken_to_ps.FullTuneToken = function(tt, param) {
  if (!param$is_bounded) {
    stopf("%s must give a range for unbounded parameter %s.", tt$call, param$id)
  }
  ttcontent_to_ps(param$clone(deep = TRUE), tt, param)
}

tunetoken_to_ps.RangeTuneToken = function(tt, param) {
  if (!param$is_number) {
    stopf("%s for non-numeric param must have one argument.", tt$call)
  }
  if (!all(map_lgl(tt$content, param$test))) {
    stopf("%s not compatible with param %s", tt$call, param$id)
  }
  content = get(param$class)$new(id = param$id, lower = tt$content$lower, upper = tt$content$upper)
  ttcontent_to_ps(content, tt, param)
}

tunetoken_to_ps.ObjectTuneToken = function(tt, param) {
  ttcontent_to_ps(tt$content, tt, param)
}

ttcontent_to_ps.Domain = function(content, tt, param) {
  content = do.call(ps, structure(list(content), names = param$id))
  ttcontent_to_ps(content, tt, param)
}

ttcontent_to_ps.Param = function(content, tt, param) {
  content = content$clone(deep = TRUE)
  content$id = param$id
  content = ParamSet$new(list(content))
  ttcontent_to_ps(content, tt, param)
}

ttcontent_to_ps.ParamSet = function(content, tt, param) {
  testpoints = generate_design_grid(content, 2)$transpose()
  if (!all(map_lgl(testpoints, function(x) {
    identical(names(x), param$id) && param$test(x[[1]])
  }))) {
    stopf("%s not compatible with param %s", tt$call, param$id)
  }
  content$set_id = ""
  content
}

