domain_promise_probe = function(style = c("direct", "hidden", "exposed"), stop_at = NULL) {
  style = match.arg(style)
  state = new.env(parent = emptyenv())
  state$events = character()
  observe = function(name, value) {
    state$events = c(state$events, name)
    if (identical(name, stop_at)) {
      stop(sprintf("forced:%s", name), call. = FALSE)
    }
    value
  }

  direct = function() {
    Domain(
      cls = observe("cls", "ParamLgl"),
      grouping = observe("grouping", "ParamLgl"),
      cargo = observe("cargo", list()),
      lower = observe("lower", NA_real_),
      upper = observe("upper", NA_real_),
      tolerance = observe("tolerance", NA_real_),
      levels = observe("levels", c(TRUE, FALSE)),
      special_vals = observe("special_vals", list()),
      default = observe("default", NO_DEF),
      tags = observe("tags", character()),
      trafo = observe("trafo", NULL),
      depends_expr = observe("depends_expr", NULL),
      storage_type = observe("storage_type", "logical"),
      init = observe("init", TRUE)
    )
  }
  wrapper = function(
      cls = observe("cls", "ParamLgl"),
      grouping = observe("grouping", "ParamLgl"),
      cargo = observe("cargo", list()),
      lower = observe("lower", NA_real_),
      upper = observe("upper", NA_real_),
      tolerance = observe("tolerance", NA_real_),
      levels = observe("levels", c(TRUE, FALSE)),
      special_vals = observe("special_vals", list()),
      default = observe("default", NO_DEF),
      tags = observe("tags", character()),
      trafo = observe("trafo", NULL),
      depends_expr = observe("depends_expr", NULL),
      storage_type = observe("storage_type", "logical"),
      init = observe("init", TRUE)) {
    Domain(
      cls = cls,
      grouping = grouping,
      cargo = cargo,
      lower = lower,
      upper = upper,
      tolerance = tolerance,
      levels = levels,
      special_vals = special_vals,
      default = default,
      tags = tags,
      trafo = trafo,
      depends_expr = depends_expr,
      storage_type = storage_type,
      init = init
    )
  }

  error = tryCatch(
    {
      result = switch(
        style,
        direct = direct(),
        hidden = wrapper(),
        exposed = wrapper(
          cls = observe("cls", "ParamLgl"),
          grouping = observe("grouping", "ParamLgl"),
          cargo = observe("cargo", list()),
          lower = observe("lower", NA_real_),
          upper = observe("upper", NA_real_),
          tolerance = observe("tolerance", NA_real_),
          levels = observe("levels", c(TRUE, FALSE)),
          special_vals = observe("special_vals", list()),
          default = observe("default", NO_DEF),
          tags = observe("tags", character()),
          trafo = observe("trafo", NULL),
          depends_expr = observe("depends_expr", NULL),
          storage_type = observe("storage_type", "logical"),
          init = observe("init", TRUE)
        )
      )
      stopifnot(inherits(result, "Domain"))
      NA_character_
    },
    error = conditionMessage
  )

  list(events = state$events, error = error)
}

test_that("Domain forces direct and wrapper promises in historical order", {
  order = c(
    "tags", "cargo", "cls", "grouping", "lower", "upper", "tolerance",
    "levels", "special_vals", "trafo", "depends_expr", "default",
    "storage_type", "init"
  )

  for (style in c("direct", "hidden", "exposed")) {
    success = domain_promise_probe(style)
    expect_identical(success$events, order, info = style)
    expect_true(is.na(success$error), info = style)

    for (position in seq_along(order)) {
      stopped = domain_promise_probe(style, order[[position]])
      expect_identical(
        stopped$events,
        order[seq_len(position)],
        info = sprintf("%s:%s", style, order[[position]])
      )
      expect_identical(
        stopped$error,
        sprintf("forced:%s", order[[position]]),
        info = sprintf("%s:%s", style, order[[position]])
      )
    }
  }
})

test_that("Domain assembles default, storage, requirements, and init in row order", {
  state = new.env(parent = emptyenv())
  state$events = character()
  observe = function(name, value) {
    state$events = c(state$events, name)
    value
  }
  wrapper = function(
      default = observe("default", NO_DEF),
      storage_type = observe("storage_type", "logical"),
      depends_expr = observe("depends_expr", quote(parent > 1L)),
      init = observe("init", TRUE)) {
    Domain(
      cls = "ParamLgl",
      grouping = "ParamLgl",
      cargo = list(),
      levels = c(TRUE, FALSE),
      default = default,
      storage_type = storage_type,
      depends_expr = depends_expr,
      init = init
    )
  }

  expect_error(wrapper(), "Requirement 'parent > 1L' is broken", fixed = TRUE)
  expect_identical(
    state$events,
    c("depends_expr", "default", "storage_type")
  )
})

test_that("Domain preserves recursively missing init promises", {
  hidden = function(init) {
    Domain(
      cls = "ParamLgl",
      grouping = "ParamLgl",
      cargo = list(),
      levels = c(TRUE, FALSE),
      storage_type = "logical",
      init = init
    )
  }
  direct = function() {
    Domain(
      cls = "ParamLgl",
      grouping = "ParamLgl",
      cargo = list(),
      levels = c(TRUE, FALSE),
      storage_type = "logical"
    )
  }

  for (domain in list(hidden(), direct())) {
    expect_identical(domain$.init_given, FALSE)
    expect_identical(domain$.init, list(NULL))
  }
})
