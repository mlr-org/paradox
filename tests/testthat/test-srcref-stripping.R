srcref_test_function = function(text, environment = new.env(parent = baseenv())) {
  parsed = parse(text = text, keep.source = TRUE)
  result = eval(parsed[[1L]], envir = environment)
  stopifnot(
    is.function(result),
    paradox:::.paradox_has_srcref(result)
  )
  result
}

srcref_test_clean_function = function(text,
    environment = new.env(parent = baseenv())) {
  result = eval(parse(text = text, keep.source = FALSE)[[1L]],
    envir = environment)
  stopifnot(
    is.function(result),
    !paradox:::.paradox_has_srcref(result)
  )
  result
}

srcref_test_domain_trafo = function(domain) {
  .subset2(domain, ".trafo")[[1L]]
}

srcref_test_raw_contains = function(haystack, text) {
  needle = charToRaw(text)
  if (!length(needle)) return(TRUE)
  if (length(needle) > length(haystack)) return(FALSE)
  starts = which(haystack == needle[[1L]])
  any(vapply(starts[starts <= length(haystack) - length(needle) + 1L],
    function(start) {
      identical(haystack[seq.int(start, length.out = length(needle))], needle)
    },
    logical(1L)
  ))
}

test_that("the helper strips complete callback syntax without changing behavior", {
  callback = srcref_test_function(
    paste(
      "function(x = (function(value) {",
      "  # formal source marker",
      "  value + 1",
      "})(2)) {",
      "  inner = function(value) {",
      "    # body source marker",
      "    value * 2",
      "  }",
      "  inner(x)",
      "}",
      sep = "\n"
    )
  )
  attr(callback, "paradox_test_attribute") = "retained"
  callback_environment = environment(callback)
  captured_callback = srcref_test_function("function(value) value - 1")
  assign(
    "captured_callback",
    captured_callback,
    envir = callback_environment
  )
  retained_attribute_names = setdiff(
    names(attributes(callback)),
    paradox:::.paradox_srcref_attributes
  )

  stripped = paradox:::.paradox_strip_srcref(callback)

  expect_true(paradox:::.paradox_has_srcref(callback))
  expect_false(paradox:::.paradox_has_srcref(stripped))
  expect_false(identical(
    data.table::address(stripped),
    data.table::address(callback)
  ))
  expect_identical(
    data.table::address(environment(stripped)),
    data.table::address(callback_environment)
  )
  expect_identical(attr(stripped, "paradox_test_attribute"), "retained")
  expect_identical(names(attributes(stripped)), retained_attribute_names)
  expect_identical(
    data.table::address(get(
      "captured_callback",
      envir = environment(stripped),
      inherits = FALSE
    )),
    data.table::address(captured_callback)
  )
  expect_true(paradox:::.paradox_has_srcref(get(
    "captured_callback",
    envir = environment(stripped),
    inherits = FALSE
  )))
  expect_identical(stripped(3), callback(3))
  expect_identical(stripped(), callback())
  # `identical(ignore.srcref = TRUE)` does not ignore every nested source
  # attribute inside a formals-default AST on all supported R releases.
  expect_equal(stripped, callback)
  expect_identical(
    paradox:::.paradox_strip_srcref(stripped),
    stripped
  )
  expect_identical(
    data.table::address(paradox:::.paradox_strip_srcref(stripped)),
    data.table::address(stripped)
  )

  reference_leaf = new.env(parent = emptyenv())
  attr(reference_leaf, "srcref") = "opaque reference metadata"
  programmatic = function() NULL
  body(programmatic) = as.call(list(quote(identity), reference_leaf))
  attr(programmatic, "srcref") = "syntax metadata"
  stripped_programmatic = paradox:::.paradox_strip_srcref(programmatic)
  expect_false(paradox:::.paradox_has_srcref(stripped_programmatic))
  expect_identical(attr(reference_leaf, "srcref"), "opaque reference metadata")
  expect_identical(
    data.table::address(body(stripped_programmatic)[[2L]]),
    data.table::address(reference_leaf)
  )
})

test_that("stripping preserves empty formals and literal NULL call cells", {
  callback = srcref_test_function(
    paste(
      "function(x) {",
      "  increment = function() x <<- x + 1L",
      "  increment()",
      "  list(x, NULL)",
      "}",
      sep = "\n"
    )
  )

  stripped = paradox:::.paradox_strip_srcref(callback)
  nested = body(stripped)[[2L]][[3L]]

  expect_false(paradox:::.paradox_has_srcref(stripped))
  expect_identical(length(formals(eval(nested, environment(stripped)))), 0L)
  expect_identical(stripped(1L), list(2L, NULL))
  expect_equal(stripped, callback)
})

test_that("stripping preserves named formals, missing arguments, and node attributes", {
  callback = srcref_test_function(
    paste(
      "function(a, b = NULL, c = (function(value) {",
      "  # source-bearing formal default",
      "  value + 1L",
      "})(2L)) target(first = a, , last = b)",
      sep = "\n"
    )
  )
  callback_formals = formals(callback)
  attr(callback_formals, "paradox_pairlist_marker") = "retained"
  callback_body = body(callback)
  attr(callback_body, "paradox_call_marker") = "retained"
  attr(callback_body, "srcref") = "synthetic call source"
  expect_true(paradox:::.paradox_has_srcref(callback))

  stripped = paradox:::.paradox_strip_srcref(callback)
  stripped_formals = formals(stripped)
  stripped_body = body(stripped)
  attributed_formals = paradox:::.paradox_strip_srcref(callback_formals)
  attributed_body = paradox:::.paradox_strip_srcref(callback_body)

  expect_false(paradox:::.paradox_has_srcref(stripped))
  expect_false(paradox:::.paradox_has_srcref(attributed_formals))
  expect_false(paradox:::.paradox_has_srcref(attributed_body))
  expect_identical(names(stripped_formals), c("a", "b", "c"))
  expect_identical(length(stripped_formals), 3L)
  expect_true(
    typeof(stripped_formals[[1L]]) == "symbol" &&
      !nzchar(as.character(stripped_formals[[1L]]))
  )
  expect_null(stripped_formals[[2L]])
  expect_identical(
    attr(attributed_formals, "paradox_pairlist_marker"),
    "retained"
  )
  expect_identical(names(stripped_body), c("", "first", "", "last"))
  expect_identical(length(stripped_body), 4L)
  expect_true(
    typeof(stripped_body[[3L]]) == "symbol" &&
      !nzchar(as.character(stripped_body[[3L]]))
  )
  expect_identical(attr(attributed_body, "paradox_call_marker"), "retained")
  expect_null(attr(attributed_body, "srcref", exact = TRUE))
  expect_equal(stripped, callback)
})

test_that("clean callbacks retain exact pointer identity", {
  callback = srcref_test_clean_function("function(x) x + 1")
  domain = p_dbl(0, 1, trafo = callback)
  stored = srcref_test_domain_trafo(domain)

  expect_identical(data.table::address(stored), data.table::address(callback))
  expect_identical(stored(1), 2)
})

test_that("all Domain callback families strip at admission", {
  trafo = srcref_test_function("function(x) x")
  aggr = srcref_test_function("function(x) x[[1L]]")
  in_tune_fn = srcref_test_function(
    "function(domain, param_vals) domain$upper"
  )
  custom_check = srcref_test_function("function(x) TRUE")

  domains = list(
    dbl = p_dbl(
      0, 1, trafo = trafo, tags = "internal_tuning",
      aggr = aggr, in_tune_fn = in_tune_fn,
      disable_in_tune = list(enabled = FALSE)
    ),
    int = p_int(
      0, 1, trafo = trafo, tags = "internal_tuning",
      aggr = aggr, in_tune_fn = in_tune_fn,
      disable_in_tune = list(enabled = FALSE)
    ),
    lgl = p_lgl(
      trafo = trafo, tags = "internal_tuning",
      aggr = aggr, in_tune_fn = in_tune_fn,
      disable_in_tune = list(enabled = FALSE)
    ),
    fct = p_fct(
      c("a", "b"), trafo = trafo, tags = "internal_tuning",
      aggr = aggr, in_tune_fn = in_tune_fn,
      disable_in_tune = list(enabled = FALSE)
    ),
    uty = p_uty(
      custom_check = custom_check, trafo = trafo,
      tags = "internal_tuning", aggr = aggr, in_tune_fn = in_tune_fn,
      disable_in_tune = list(enabled = FALSE)
    )
  )

  for (domain in domains) {
    expect_false(paradox:::.paradox_has_srcref(
      srcref_test_domain_trafo(domain)
    ))
    expect_false(paradox:::.paradox_has_srcref(
      domain$cargo[[1L]]$aggr
    ))
    expect_false(paradox:::.paradox_has_srcref(
      domain$cargo[[1L]]$in_tune_fn
    ))
  }
  expect_false(paradox:::.paradox_has_srcref(
    domains$uty$cargo[[1L]]$custom_check
  ))
  expect_true(domains$uty$cargo[[1L]]$custom_check(list()))

  integer_logscale = p_int(1L, 10L, logscale = TRUE)
  integer_trafo = srcref_test_domain_trafo(integer_logscale)
  expect_false(paradox:::.paradox_has_srcref(
    integer_trafo
  ))
  expect_setequal(
    ls(environment(integer_trafo), all.names = TRUE),
    c("lower", "upper")
  )
})

test_that("categorical embedding cannot hide the user callback source", {
  callback = srcref_test_function("function(x) x * 10")
  domain = p_fct(list(a = 1, b = 2), trafo = callback)
  stored = srcref_test_domain_trafo(domain)
  embedded = get("trafo", envir = environment(stored), inherits = FALSE)

  expect_false(paradox:::.paradox_has_srcref(stored))
  expect_setequal(
    ls(environment(stored), all.names = TRUE),
    c("levels", "trafo")
  )
  expect_false(paradox:::.paradox_has_srcref(embedded))
  expect_false(identical(
    data.table::address(embedded),
    data.table::address(callback)
  ))
  expect_identical(
    data.table::address(environment(embedded)),
    data.table::address(environment(callback))
  )
  expect_identical(stored("b"), 20)
})

test_that("ParamSet callback setters and constructor routes strip once", {
  extra = srcref_test_function("function(x, param_set) x")
  constraint = srcref_test_function("function(x) TRUE")
  parameter_set = ps(x = p_int())

  parameter_set$extra_trafo = extra
  parameter_set$constraint = constraint
  expect_false(paradox:::.paradox_has_srcref(parameter_set$extra_trafo))
  expect_false(paradox:::.paradox_has_srcref(parameter_set$constraint))
  expect_identical(parameter_set$trafo(list(x = 1L)), list(x = 1L))
  expect_true(parameter_set$test_constraint(list(x = 1L)))

  constructed = ps(
    x = p_int(),
    .extra_trafo = extra,
    .constraint = constraint
  )
  expect_false(paradox:::.paradox_has_srcref(constructed$extra_trafo))
  expect_false(paradox:::.paradox_has_srcref(constructed$constraint))
})

test_that("Shadow write-through and Collection views retain stripped state", {
  extra = srcref_test_function(
    "function(x, param_set) { x$result = x$visible; x }"
  )
  constraint = srcref_test_function("function(x) x$visible >= 0")
  origin = ps(visible = p_int(0, 2), hidden = p_lgl())
  origin$constraint = constraint
  shadow = ParamSetShadow$new(origin, "hidden")

  shadow$extra_trafo = extra
  expect_false(paradox:::.paradox_has_srcref(origin$extra_trafo))
  expect_false(paradox:::.paradox_has_srcref(origin$constraint))
  expect_false(paradox:::.paradox_has_srcref(shadow$constraint))
  expect_setequal(
    ls(environment(shadow$constraint), all.names = TRUE),
    "plan"
  )
  expect_identical(
    shadow$trafo(list(visible = 1L)),
    list(visible = 1L, result = 1L)
  )
  expect_true(shadow$test_constraint(list(visible = 1L)))
  detached_shadow = shadow$subset("visible")
  expect_false(paradox:::.paradox_has_srcref(detached_shadow$constraint))

  collection = ParamSetCollection$new(list(child = origin))
  expect_false(paradox:::.paradox_has_srcref(
    collection$sets[[1L]]$extra_trafo
  ))
  expect_false(paradox:::.paradox_has_srcref(
    collection$sets[[1L]]$constraint
  ))
  expect_false(paradox:::.paradox_has_srcref(collection$extra_trafo))
  expect_false(paradox:::.paradox_has_srcref(collection$constraint))
  expect_identical(
    collection$trafo(list(child.visible = 1L, child.hidden = TRUE)),
    list(child.visible = 1L, child.hidden = TRUE, child.result = 1L)
  )
  detached_collection = collection$subset(
    c("child.visible", "child.hidden")
  )
  expect_false(paradox:::.paradox_has_srcref(
    detached_collection$extra_trafo
  ))
  expect_false(paradox:::.paradox_has_srcref(
    detached_collection$constraint
  ))

  collection_shadow = ParamSetShadow$new(collection, "child.hidden")
  expect_false(paradox:::.paradox_has_srcref(
    collection_shadow$extra_trafo
  ))
  expect_false(paradox:::.paradox_has_srcref(
    collection_shadow$constraint
  ))
  detached_collection_shadow = collection_shadow$subset("child.visible")
  expect_false(paradox:::.paradox_has_srcref(
    detached_collection_shadow$extra_trafo
  ))
  expect_false(paradox:::.paradox_has_srcref(
    detached_collection_shadow$constraint
  ))
})

test_that("internal TuneToken aggregation is normalized before storage", {
  aggregation = srcref_test_function("function(x) max(unlist(x))")
  token = to_tune(upper = 10, aggr = aggregation)
  expect_false(paradox:::.paradox_has_srcref(token$content$aggr))

  parameter_set = ps(value = p_int(
    0, 20,
    tags = "internal_tuning",
    aggr = srcref_test_clean_function("function(x) x[[1L]]"),
    in_tune_fn = srcref_test_clean_function(
      "function(domain, param_vals) domain$upper"
    ),
    disable_in_tune = list(enabled = FALSE)
  ))
  parameter_set$values = list(value = token)
  search_space = parameter_set$search_space()
  stored = search_space$domains$value$cargo[[1L]]$aggr
  expect_false(paradox:::.paradox_has_srcref(stored))
  expect_identical(stored(list(1L, 3L, 2L)), 3L)

  converter = srcref_test_function(
    "function(domain, param_vals) domain$upper + as.integer(param_vals$gate)"
  )
  child = ps(
    value = p_int(
      0, 20,
      tags = "internal_tuning",
      aggr = srcref_test_clean_function("function(x) x[[1L]]"),
      in_tune_fn = converter,
      disable_in_tune = list(gate = FALSE)
    ),
    gate = p_lgl()
  )
  flattened = ParamSetCollection$new(list(child = child))$flatten()
  flattened_domain = flattened$domains[["child.value"]]
  flattened_converter = flattened_domain$cargo[[1L]]$in_tune_fn
  expect_false(paradox:::.paradox_has_srcref(flattened_converter))
  expect_identical(
    flattened_converter(
      flattened_domain,
      list(child.gate = TRUE, unrelated = 100L)
    ),
    21
  )
})

test_that("Domain representations are source-independent", {
  first_text = paste(
    "p_dbl(0, 1, trafo = function(x) {",
    "  # PARADOX_REPR_SOURCE_MARKER",
    "  x + 1",
    "})",
    sep = "\n"
  )
  second_text = "p_dbl(0,1,trafo=function(x){x+1})"
  evaluation_environment = new.env(parent = asNamespace("paradox"))
  first = eval(parse(text = first_text, keep.source = TRUE)[[1L]],
    envir = evaluation_environment)
  second = eval(parse(text = second_text, keep.source = TRUE)[[1L]],
    envir = evaluation_environment)

  expect_false(paradox:::.paradox_has_srcref(
    attr(first, "repr", exact = TRUE)
  ))
  expect_false(paradox:::.paradox_has_srcref(
    attr(second, "repr", exact = TRUE)
  ))
  expect_identical(first$id, second$id)
  expect_identical(
    first$id,
    paste(
      "p_dbl(lower = 0, upper = 1, trafo = function(x) {",
      "    x + 1",
      "})",
      sep = "\n"
    )
  )
  expect_identical(
    capture.output(print(first)),
    c(
      "p_dbl(lower = 0, upper = 1, trafo = function(x) {",
      "    x + 1",
      "})"
    )
  )
  expect_false(grepl(
    "PARADOX_REPR_SOURCE_MARKER",
    paste(deparse(attr(first, "repr", exact = TRUE)), collapse = "\n"),
    fixed = TRUE
  ))

  token = to_tune(first)
  expect_false(srcref_test_raw_contains(
    serialize(token, NULL),
    "PARADOX_REPR_SOURCE_MARKER"
  ))
})

test_that("compiled callbacks are stripped and remain callable", {
  callback = srcref_test_function("function(x) x + 2")
  compiled = compiler::cmpfun(callback)
  expect_true(paradox:::.paradox_has_srcref(compiled))

  stored = srcref_test_domain_trafo(p_dbl(0, 1, trafo = compiled))
  expect_false(paradox:::.paradox_has_srcref(stored))
  expect_identical(stored(3), 5)
  expect_identical(environment(stored), environment(compiled))
})

test_that("the option is admission-time and preserves source debugging", {
  callback = srcref_test_function("function(x) x")
  old = options(paradox.strip_srcrefs = FALSE)
  on.exit(options(old), add = TRUE)

  domain = p_dbl(0, 1, trafo = callback)
  stored = srcref_test_domain_trafo(domain)
  expect_true(paradox:::.paradox_has_srcref(stored))
  expect_identical(data.table::address(stored), data.table::address(callback))

  options(paradox.strip_srcrefs = TRUE)
  expect_true(paradox:::.paradox_has_srcref(
    srcref_test_domain_trafo(domain)
  ))
  normalized = p_dbl(0, 1, trafo = callback)
  expect_false(paradox:::.paradox_has_srcref(
    srcref_test_domain_trafo(normalized)
  ))
})

test_that("stripping removes retained source-file payload", {
  marker = paste0("PARADOX_LARGE_SOURCE_", paste(rep("x", 200L),
    collapse = ""))
  source_lines = c(
    "function(x) {",
    rep(paste0("  # ", marker), 500L),
    "  TRUE",
    "}"
  )
  source_file = srcfilecopy("paradox-large-callback.R", source_lines)
  callback = eval(parse(
    text = source_lines,
    srcfile = source_file,
    keep.source = TRUE
  )[[1L]], envir = new.env(parent = baseenv()))
  expect_true(paradox:::.paradox_has_srcref(callback))

  old = options(paradox.strip_srcrefs = FALSE)
  on.exit(options(old), add = TRUE)
  retained = ps(value = p_uty(custom_check = callback))
  retained_size = length(serialize(retained, NULL))

  options(paradox.strip_srcrefs = TRUE)
  stripped = ps(value = p_uty(custom_check = callback))
  stripped_bytes = serialize(stripped, NULL)
  stripped_size = length(stripped_bytes)

  expect_gt(retained_size - stripped_size, nchar(marker) * 300L)
  expect_false(srcref_test_raw_contains(stripped_bytes, marker))
})

test_that("function-valued opaque leaves remain byte-for-byte untouched", {
  value = srcref_test_function("function(x) { # opaque value marker\n x }")
  value_bytes = serialize(value, NULL)
  domain = p_uty(
    special_vals = list(value),
    default = value
  )

  expect_true(paradox:::.paradox_has_srcref(
    domain$special_vals[[1L]][[1L]]
  ))
  expect_identical(
    data.table::address(domain$special_vals[[1L]][[1L]]),
    data.table::address(value)
  )
  expect_true(paradox:::.paradox_has_srcref(domain$default[[1L]]))
  expect_identical(
    data.table::address(domain$default[[1L]]),
    data.table::address(value)
  )
  expect_identical(serialize(domain$default[[1L]], NULL), value_bytes)

  parameter_set = ps(payload = p_uty(init = value))
  expect_true(paradox:::.paradox_has_srcref(parameter_set$values$payload))
  expect_identical(
    data.table::address(parameter_set$values$payload),
    data.table::address(value)
  )
  replacement = srcref_test_function(
    "function(x) { # replacement opaque marker\n x + 1 }"
  )
  parameter_set$values = list(payload = replacement)
  expect_true(paradox:::.paradox_has_srcref(parameter_set$values$payload))
  expect_identical(
    data.table::address(parameter_set$values$payload),
    data.table::address(replacement)
  )

  token_aggr = srcref_test_function(
    "function(x) { # opaque token marker\n max(unlist(x)) }"
  )
  old = options(paradox.strip_srcrefs = FALSE)
  on.exit(options(old), add = TRUE)
  token = to_tune(upper = 10, aggr = token_aggr)
  options(paradox.strip_srcrefs = TRUE)
  token_holder = ps(payload = p_int(
    0, 20,
    tags = "internal_tuning",
    aggr = srcref_test_clean_function("function(x) x[[1L]]"),
    in_tune_fn = srcref_test_clean_function(
      "function(domain, param_vals) domain$upper"
    ),
    disable_in_tune = list(enabled = FALSE)
  ))
  token_holder$values = list(payload = token)
  stored_token = token_holder$values$payload
  expect_s3_class(stored_token, "InternalTuneToken")
  expect_identical(
    data.table::address(stored_token$content$aggr),
    data.table::address(token_aggr)
  )
  expect_true(paradox:::.paradox_has_srcref(stored_token$content$aggr))
})

test_that("clone and serialization cannot reintroduce callback sources", {
  trafo = srcref_test_function("function(x) x + 1")
  extra = srcref_test_function("function(x, param_set) x")
  constraint = srcref_test_function("function(x) TRUE")
  parameter_set = ps(x = p_dbl(0, 1, trafo = trafo))
  parameter_set$extra_trafo = extra
  parameter_set$constraint = constraint

  observed = list(
    parameter_set,
    parameter_set$clone(deep = TRUE),
    unserialize(serialize(parameter_set, NULL))
  )
  for (candidate in observed) {
    expect_false(paradox:::.paradox_has_srcref(
      srcref_test_domain_trafo(candidate$domains$x)
    ))
    expect_false(paradox:::.paradox_has_srcref(candidate$extra_trafo))
    expect_false(paradox:::.paradox_has_srcref(candidate$constraint))
    expect_identical(candidate$trafo(list(x = 0)), list(x = 1))
    expect_true(candidate$test_constraint(list(x = 0)))
  }
  extra_first = parameter_set$extra_trafo
  extra_second = parameter_set$extra_trafo
  constraint_first = parameter_set$constraint
  constraint_second = parameter_set$constraint
  trafo_first = srcref_test_domain_trafo(parameter_set$domains$x)
  trafo_second = srcref_test_domain_trafo(parameter_set$domains$x)
  expect_identical(
    data.table::address(extra_first),
    data.table::address(extra_second)
  )
  expect_identical(
    data.table::address(constraint_first),
    data.table::address(constraint_second)
  )
  expect_identical(
    data.table::address(trafo_first),
    data.table::address(trafo_second)
  )
})

test_that("a stripped function node keeps no partial srcref cell", {
  # A parsed `function(...)` node carries its srcref as a fourth positional
  # cell. Stripping only that cell's `srcfile` leaves an integer vector still
  # classed "srcref", which `function` installs on the closure it builds, so
  # the closure prints as a source descriptor instead of its body.
  source_text = parse(
    text = paste(
      "set = ps(x = p_dbl(0, 1), .extra_trafo = function(x, param_set) {",
      "  helper = function(v) v + 1",
      "  list(x = helper(x$x), h = helper)",
      "})",
      sep = "\n"
    ),
    keep.source = TRUE
  )
  eval(source_text)

  produced = set$trafo(list(x = 0.5))$h
  expect_null(attr(produced, "srcref", exact = TRUE))
  expect_identical(produced(1), 2)
  expect_match(paste(format(produced), collapse = " "), "v \\+ 1")
})
