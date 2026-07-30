test_that("the general data.table finalizer owns every column spine", {
  leaf = new.env(parent = emptyenv())
  factor_column = factor(c("left", "right"))
  attr(factor_column, "metadata") = list(labels = c("L", "R"))
  source = data.table::data.table(
    atomic = 1:2,
    factor = factor_column,
    listed = I(list(leaf, list(y = 2L)))
  )
  result = .Call(paradox:::C_finalize_data_table, source)

  expect_false(identical(
    data.table::address(result$atomic),
    data.table::address(source$atomic)
  ))
  expect_false(identical(
    data.table::address(result$factor),
    data.table::address(source$factor)
  ))
  expect_false(identical(
    data.table::address(levels(result$factor)),
    data.table::address(levels(source$factor))
  ))
  expect_false(identical(
    data.table::address(attr(result$factor, "metadata")),
    data.table::address(attr(source$factor, "metadata"))
  ))
  expect_false(identical(
    data.table::address(attr(result$factor, "metadata")$labels),
    data.table::address(attr(source$factor, "metadata")$labels)
  ))
  expect_false(identical(
    data.table::address(result$listed),
    data.table::address(source$listed)
  ))
  # Generic list-column leaves are semantic/opaque, not recursively copied.
  expect_true(identical(result$listed[[1L]], source$listed[[1L]]))
  expect_identical(result$listed[[1L]], leaf)

  data.table::set(result, i = 1L, j = "atomic", value = 99L)
  data.table::setattr(result$factor, "levels", c("changed", "right"))
  attr(result$factor, "metadata")$labels[[1L]] = "changed"
  data.table::set(
    result,
    i = 1L,
    j = "listed",
    value = list(list(replaced = TRUE))
  )
  expect_identical(source$atomic, 1:2)
  expect_identical(levels(source$factor), c("left", "right"))
  expect_identical(
    attr(source$factor, "metadata")$labels,
    c("L", "R")
  )
  expect_identical(source$listed[[1L]], leaf)
})

test_that("the general data.table finalizer materializes stable ALTREP columns", {
  skip_if_not(
    exists("C_test_stateful_altrep", asNamespace("paradox"), inherits = FALSE),
    "the internal stateful ALTREP test class is unavailable"
  )
  column = native_stateful_altrep(
    structure(1:3, metadata = list(labels = letters[1:3])),
    structure(1:3, metadata = list(labels = letters[1:3]))
  )
  source = structure(
    list(value = column),
    names = "value",
    row.names = 1:3,
    class = c("data.table", "data.frame")
  )

  result = .Call(paradox:::C_finalize_data_table, source)
  expect_identical(as.integer(result$value), 1:3)
  expect_identical(
    attr(result$value, "metadata"),
    list(labels = letters[1:3])
  )
  expect_false(identical(
    data.table::address(result$value),
    data.table::address(source$value)
  ))
  expect_false(identical(
    data.table::address(attr(result$value, "metadata")),
    data.table::address(attr(source$value, "metadata"))
  ))
})

test_that("ParamSet schema accessors own interpreted and typed leaves", {
  aggregate = function(x) sum(unlist(x))
  convert = function(domain, param_vals) param_vals[[1L]]
  utility = new.env(parent = emptyenv())
  typed_s4 = asS4(7L)
  factor_special = structure(
    NA_character_,
    metadata = list(labels = structure("special", marker = TRUE))
  )
  factor_default = structure(
    "a",
    metadata = list(labels = structure("default", marker = TRUE))
  )
  factor_init = structure(
    "b",
    metadata = list(labels = structure("init", marker = TRUE))
  )
  set = ps(
    factor = p_fct(
      c("a", "b"),
      special_vals = list(factor_special, typed_s4),
      default = factor_default,
      init = factor_init
    ),
    integer = p_int(
      0,
      10,
      tags = "internal_tuning",
      aggr = aggregate,
      in_tune_fn = convert,
      disable_in_tune = list(control = 1L)
    ),
    utility = p_uty(
      special_vals = list(utility),
      default = utility
    )
  )
  set$add_dep("factor", "integer", CondEqual(1L))

  params = set$params
  expect_identical(params$cargo[[2L]]$aggr, aggregate)
  expect_identical(params$cargo[[2L]]$in_tune_fn, convert)
  expect_identical(params$special_vals[[1L]][[2L]], typed_s4)
  expect_identical(params$special_vals[[3L]][[1L]], utility)
  expect_identical(params$default[[3L]], utility)

  data.table::setattr(params$levels[[1L]], "adversarial", TRUE)
  data.table::setattr(
    params$cargo[[2L]]$disable_in_tune,
    "names",
    "changed"
  )
  data.table::setattr(
    params$special_vals[[1L]][[1L]],
    "adversarial",
    TRUE
  )
  data.table::setattr(
    attr(params$special_vals[[1L]][[1L]], "metadata")$labels,
    "adversarial",
    TRUE
  )
  data.table::setattr(params$default[[1L]], "adversarial", TRUE)
  data.table::setattr(
    attr(params$default[[1L]], "metadata")$labels,
    "adversarial",
    TRUE
  )
  data.table::setattr(params$.init[[1L]], "adversarial", TRUE)
  data.table::setattr(
    attr(params$.init[[1L]], "metadata")$labels,
    "adversarial",
    TRUE
  )
  data.table::setattr(
    params$.requirements[[1L]][[2L]],
    "names",
    c("changed", "condition_format_string")
  )

  current = set$params
  expect_null(attr(current$levels[[1L]], "adversarial"))
  expect_identical(
    names(current$cargo[[2L]]$disable_in_tune),
    "control"
  )
  expect_null(attr(current$special_vals[[1L]][[1L]], "adversarial"))
  expect_null(attr(
    attr(current$special_vals[[1L]][[1L]], "metadata")$labels,
    "adversarial"
  ))
  expect_null(attr(current$default[[1L]], "adversarial"))
  expect_null(attr(
    attr(current$default[[1L]], "metadata")$labels,
    "adversarial"
  ))
  expect_null(attr(current$.init[[1L]], "adversarial"))
  expect_null(attr(
    attr(current$.init[[1L]], "metadata")$labels,
    "adversarial"
  ))
  expect_identical(names(set$deps$cond[[1L]]), c(
    "rhs",
    "condition_format_string"
  ))

  domains = set$domains
  factor = domains$factor
  data.table::setattr(factor$levels[[1L]], "adversarial", TRUE)
  data.table::setattr(
    factor$special_vals[[1L]][[1L]],
    "adversarial",
    TRUE
  )
  data.table::setattr(
    attr(factor$special_vals[[1L]][[1L]], "metadata")$labels,
    "adversarial",
    TRUE
  )
  data.table::setattr(factor$default[[1L]], "adversarial", TRUE)
  data.table::setattr(
    attr(factor$default[[1L]], "metadata")$labels,
    "adversarial",
    TRUE
  )
  data.table::setattr(factor$.init[[1L]], "adversarial", TRUE)
  data.table::setattr(
    attr(factor$.init[[1L]], "metadata")$labels,
    "adversarial",
    TRUE
  )
  data.table::setattr(
    factor$.requirements[[1L]][[1L]]$cond,
    "names",
    c("changed", "condition_format_string")
  )
  expect_null(attr(set$domains$factor$levels[[1L]], "adversarial"))
  expect_null(attr(
    set$domains$factor$special_vals[[1L]][[1L]],
    "adversarial"
  ))
  expect_null(attr(
    attr(
      set$domains$factor$special_vals[[1L]][[1L]],
      "metadata"
    )$labels,
    "adversarial"
  ))
  expect_null(attr(set$domains$factor$default[[1L]], "adversarial"))
  expect_null(attr(
    attr(set$domains$factor$default[[1L]], "metadata")$labels,
    "adversarial"
  ))
  expect_null(attr(set$domains$factor$.init[[1L]], "adversarial"))
  expect_null(attr(
    attr(set$domains$factor$.init[[1L]], "metadata")$labels,
    "adversarial"
  ))
  expect_identical(names(set$deps$cond[[1L]]), c(
    "rhs",
    "condition_format_string"
  ))

  data = set$data
  data.table::set(data, i = 1L, j = "lower", value = 99)
  expect_identical(set$lower[["factor"]], NA_real_)

  detached = list(
    class = set$class,
    lower = set$lower,
    upper = set$upper,
    levels = set$levels,
    storage_type = set$storage_type,
    special_vals = set$special_vals,
    default = set$default
  )
  data.table::setattr(detached$lower, "adversarial", TRUE)
  data.table::setattr(detached$levels[[1L]], "adversarial", TRUE)
  data.table::setattr(
    detached$special_vals[[1L]][[1L]],
    "adversarial",
    TRUE
  )
  data.table::setattr(detached$default[[1L]], "adversarial", TRUE)
  expect_null(attr(set$lower, "adversarial"))
  expect_null(attr(set$levels[[1L]], "adversarial"))
  expect_null(attr(set$special_vals[[1L]][[1L]], "adversarial"))
  expect_null(attr(set$default[[1L]], "adversarial"))
})

test_that("typed special ingress is owned while opaque identity is retained", {
  typed = structure(
    3L,
    source_marker = TRUE,
    metadata = list(labels = structure("typed", marker = TRUE))
  )
  typed_domain = p_int(0, 5, special_vals = list(typed))
  data.table::setattr(typed, "after_construction", TRUE)
  data.table::setattr(
    attr(typed, "metadata")$labels,
    "after_construction",
    TRUE
  )
  expect_null(attr(
    typed_domain$special_vals[[1L]][[1L]],
    "after_construction"
  ))
  expect_null(attr(
    attr(typed_domain$special_vals[[1L]][[1L]], "metadata")$labels,
    "after_construction"
  ))

  set = ps(value = typed_domain)
  data.table::setattr(
    typed_domain$special_vals[[1L]][[1L]],
    "after_paramset",
    TRUE
  )
  data.table::setattr(
    attr(
      typed_domain$special_vals[[1L]][[1L]],
      "metadata"
    )$labels,
    "after_paramset",
    TRUE
  )
  expect_null(attr(
    set$special_vals$value[[1L]],
    "after_paramset"
  ))
  expect_null(attr(
    attr(set$special_vals$value[[1L]], "metadata")$labels,
    "after_paramset"
  ))

  opaque = new.env(parent = emptyenv())
  utility_domain = p_uty(special_vals = list(opaque), default = opaque)
  utility_set = ps(value = utility_domain)
  expect_identical(utility_set$special_vals$value[[1L]], opaque)
  expect_identical(utility_set$default$value, opaque)
  expect_identical(utility_set$domains$value$special_vals[[1L]][[1L]], opaque)
})

test_that("raw and filtered value accessors detach typed values on every graph", {
  opaque = new.env(parent = emptyenv())
  opaque_nodefault = structure(
    list(marker = opaque),
    class = "NoDefault"
  )
  base = ps(
    typed = p_int(),
    absent = p_lgl(),
    opaque = p_uty(),
    opaque_nodefault = p_uty()
  )
  # A partial raw store exercises the value-to-parameter match direction.
  # Unchecked storage deliberately retains arbitrary typed value metadata so
  # the outward accessor, rather than checked sanitization, owns this boundary.
  base$assert_values = FALSE
  base$values = list(
    typed = structure(
      1L,
      metadata = list(labels = structure("typed", marker = TRUE))
    ),
    opaque = opaque,
    opaque_nodefault = opaque_nodefault
  )
  collection = ParamSetCollection$new(list(child = base))
  shadow = ParamSetShadow$new(base, "absent")

  accessors = list(
    raw = function(x) x$values,
    filtered = function(x) x$get_values(
      check_required = FALSE,
      remove_dependencies = FALSE
    )
  )
  for (node in list(base, collection, shadow)) {
    for (access in accessors) {
      values = access(node)
      original_names = names(access(node))
      data.table::setattr(values, "names", paste0(names(values), ".changed"))
      expect_identical(names(access(node)), original_names)

      values = access(node)
      typed_name = names(values)[grepl("typed", names(values), fixed = TRUE)]
      opaque_name = names(values)[grepl("opaque", names(values), fixed = TRUE)]
      nodefault_name = names(values)[grepl(
        "opaque_nodefault",
        names(values),
        fixed = TRUE
      )]
      opaque_name = setdiff(opaque_name, nodefault_name)
      data.table::setattr(values[[typed_name]], "adversarial", TRUE)
      data.table::setattr(
        attr(values[[typed_name]], "metadata")$labels,
        "adversarial",
        TRUE
      )
      expect_null(attr(base$values$typed, "adversarial"))
      expect_null(attr(
        attr(base$values$typed, "metadata")$labels,
        "adversarial"
      ))
      expect_identical(values[[opaque_name]], opaque)
      expect_identical(
        data.table::address(values[[nodefault_name]]),
        data.table::address(opaque_nodefault)
      )
    }
  }
})

test_that("collection sets detach the carrier but retain exact children", {
  child = ps(x = p_int())
  collection = ParamSetCollection$new(list(child = child))

  sets = collection$sets
  expect_identical(sets[[1L]], child)
  data.table::setattr(sets, "names", "changed")
  expect_identical(names(collection$sets), "child")
  expect_identical(collection$sets[[1L]], child)
})

test_that("unfiltered ids own their public vector", {
  base = ps(alpha = p_int(), beta = p_dbl())
  nodes = list(
    base,
    ParamSetCollection$new(list(child = base)),
    ParamSetShadow$new(base, "beta")
  )
  expected = list(
    c("alpha", "beta"),
    c("child.alpha", "child.beta"),
    "alpha"
  )
  for (index in seq_along(nodes)) {
    ids = nodes[[index]]$ids()
    data.table::setattr(ids, "adversarial", TRUE)
    expect_identical(nodes[[index]]$ids(), expected[[index]])
    expect_null(attr(nodes[[index]]$ids(), "adversarial"))
  }
  expect_identical(base$ids(class = "ParamInt"), "alpha")
})

test_that("R-computed property names do not alias the capsule ID column", {
  set = ps(
    plain = p_int(),
    transformed = p_dbl(1, 10, logscale = TRUE)
  )
  for (property in list(set$has_trafo_param, set$is_logscale)) {
    data.table::setattr(names(property), "adversarial", TRUE)
    expect_identical(set$ids(), c("plain", "transformed"))
  }
  expect_identical(
    set$has_trafo_param,
    c(plain = FALSE, transformed = TRUE)
  )
  expect_identical(
    set$is_logscale,
    c(plain = FALSE, transformed = TRUE)
  )
})
