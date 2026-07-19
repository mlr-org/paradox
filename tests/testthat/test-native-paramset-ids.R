native_ids_param_set = function() {
  ps(
    zeta = p_int(1, 2, tags = c("red", "common")),
    alpha = p_dbl(0, 1, tags = c("blue", "common")),
    middle = p_fct(c("x", "y"), tags = c("red", "blue")),
    bare = p_lgl()
  )
}

test_that("ids entry points are registered with fixed arities", {
  direct = get("C_param_set_ids", envir = asNamespace("paradox"))
  lazy = get("C_param_set_ids_lazy", envir = asNamespace("paradox"))

  expect_s3_class(direct, "NativeSymbolInfo")
  expect_s3_class(lazy, "NativeSymbolInfo")
  expect_identical(direct$numParameters, 5L)
  expect_identical(lazy$numParameters, 2L)
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
  expect_error(
    .Call(
      "param_set_ids",
      list(id = character(), cls = character()),
      list(id = character(), tag = character()),
      NULL,
      NULL,
      NULL,
      PACKAGE = "paradox"
    ),
    "not available"
  )
})

test_that("lazy IDs reject malformed direct-call environments", {
  symbol = get("C_param_set_ids_lazy", envir = asNamespace("paradox"))
  set = native_ids_param_set()
  private = set$.__enclos_env__$private

  expect_error(
    .Call(symbol, NULL, new.env(parent = emptyenv())),
    "requires private and method environments",
    fixed = TRUE
  )
  expect_error(
    .Call(symbol, private, NULL),
    "requires private and method environments",
    fixed = TRUE
  )
})

test_that("ids preserve parameter order and combine filters", {
  set = native_ids_param_set()

  expect_identical(set$ids(), c("zeta", "alpha", "middle", "bare"))
  expect_identical(set$ids(class = "ParamInt"), "zeta")
  expect_identical(
    set$ids(class = c("ParamFct", "ParamInt")),
    c("zeta", "middle")
  )
  expect_identical(set$ids(tags = "red"), c("zeta", "middle"))
  expect_identical(set$ids(tags = c("red", "blue")), "middle")
  expect_identical(
    set$ids(class = c("ParamInt", "ParamFct"), tags = "red"),
    c("zeta", "middle")
  )
  expect_identical(set$ids(tags = "common", any_tags = "blue"), "alpha")
})

test_that("empty and duplicate filters have stable character semantics", {
  set = native_ids_param_set()

  expect_identical(set$ids(class = character()), character())
  expect_identical(set$ids(tags = character()), character())
  expect_identical(set$ids(any_tags = character()), character())
  expect_identical(
    set$ids(tags = character(), any_tags = "blue"),
    c("alpha", "middle")
  )
  expect_identical(
    set$ids(tags = "common", any_tags = character()),
    character()
  )
  expect_identical(set$ids(tags = c("red", "red")), c("zeta", "middle"))
  expect_identical(
    set$ids(any_tags = c("blue", "red")),
    c("zeta", "alpha", "middle")
  )

  empty = ParamSet$new()
  for (arguments in list(
    list(),
    list(class = character()),
    list(tags = character()),
    list(any_tags = character()),
    list(class = "ParamInt"),
    list(tags = "tag")
  )) {
    expect_identical(do.call(empty$ids, arguments), character())
  }
})

test_that("filter promises are forced once, left-to-right, before the capsule snapshot", {
  set = native_ids_param_set()
  forced = character()

  result = set$ids(
    class = {
      forced = c(forced, "class")
      set$tags = list(
        zeta = "later",
        alpha = character(),
        middle = character(),
        bare = character()
      )
      NULL
    },
    tags = {
      forced = c(forced, "tags")
      "later"
    },
    any_tags = {
      forced = c(forced, "any_tags")
      NULL
    }
  )

  expect_identical(forced, c("class", "tags", "any_tags"))
  expect_identical(result, "zeta")

  later_forced = FALSE
  expect_error(
    set$ids(
      class = 1,
      tags = {
        later_forced = TRUE
        "later"
      }
    ),
    "character"
  )
  expect_false(later_forced)
})

test_that("ids validate public filters without S3 dispatch", {
  set = native_ids_param_set()

  expect_error(set$ids(class = NA_character_), "missing")
  expect_error(set$ids(tags = c("red", NA_character_)), "missing")
  expect_error(set$ids(any_tags = factor("red")), "character")
  expect_error(set$ids(class = 1), "character")
  expect_error(set$ids(any_tags = list("red")), "character")

  expect_identical(set$ids(class = matrix("ParamInt", 1L, 1L)), "zeta")
  expect_identical(set$ids(tags = c(named = "red")), c("zeta", "middle"))
  expect_identical(set$ids(class = I("ParamInt")), "zeta")

  callbacks = 0L
  filter = structure("ParamInt", class = "IdsFilterWithMethod")
  registerS3method(
    "as.character",
    "IdsFilterWithMethod",
    function(x, ...) {
      callbacks <<- callbacks + 1L
      stop("dispatched", call. = FALSE)
    },
    envir = asNamespace("base")
  )
  expect_identical(set$ids(class = filter), "zeta")
  expect_identical(callbacks, 0L)
})

test_that("ids type diagnostics retain textual classes and fail closed on bytes", {
  class_utf8 = enc2utf8("fa\u00e7ade")
  class_latin1 = iconv(class_utf8, from = "UTF-8", to = "latin1")
  skip_if(
    is.na(class_latin1),
    "this platform cannot represent the latin1 fixture"
  )
  Encoding(class_latin1) = "latin1"
  class_bytes = class_utf8
  Encoding(class_bytes) = "bytes"

  set = native_ids_param_set()
  latin1_value = structure(1L, class = class_latin1)
  error = tryCatch(set$ids(class = latin1_value), error = identity)
  expect_s3_class(error, "error")
  expect_identical(
    enc2utf8(conditionMessage(error)),
    paste0(
      "Assertion on 'class' failed: Must be of type 'character' ",
      "(or 'NULL'), not 'fa\u00e7ade'."
    )
  )

  bytes_value = 1L
  attr(bytes_value, "class") = class_bytes
  expect_error(
    set$ids(class = bytes_value),
    paste0(
      "Assertion on 'class' failed: Must be of type 'character' ",
      "(or 'NULL'), not 'integer'."
    ),
    fixed = TRUE
  )

  malformed_value = 1L
  attr(malformed_value, "class") = NA_character_
  expect_error(
    set$ids(class = malformed_value),
    "not 'integer'",
    fixed = TRUE
  )
})

test_that("the direct engine validates canonical tables without dispatch", {
  symbol = get("C_param_set_ids", envir = asNamespace("paradox"))
  params = list(
    id = c("zeta", "alpha", "middle"),
    cls = c("ParamInt", "ParamDbl", "ParamFct")
  )
  tags = list(
    id = c("middle", "zeta", "middle", "alpha", "middle", "zeta"),
    tag = c("red", "red", "red", "blue", "blue", "common")
  )

  expect_identical(
    .Call(symbol, params, tags, NULL, "red", NULL),
    c("zeta", "middle")
  )
  expect_identical(
    .Call(symbol, params, tags, NULL, NULL, c("blue", "red")),
    c("zeta", "alpha", "middle")
  )
  expect_identical(.Call(symbol, params, tags, NULL, c("red", "blue"), NULL), "middle")

  expect_error(.Call(symbol, unname(params), tags, NULL, NULL, NULL), "named list")
  expect_error(.Call(symbol, list(id = 1, cls = params$cls), tags, NULL, NULL, NULL), "character")
  expect_error(.Call(symbol, params, list(id = "unknown", tag = "red"), NULL, "red", NULL), "unknown parameter ID")
  expect_error(.Call(symbol, params, list(id = "zeta", tag = NA_character_), NULL, "red", NULL), "missing value")

  callbacks = 0L
  class(params$cls) = "IdsCallbackColumn"
  registerS3method(
    "mtfrm",
    "IdsCallbackColumn",
    function(x) {
      callbacks <<- callbacks + 1L
      stop("dispatched", call. = FALSE)
    },
    envir = asNamespace("base")
  )
  expect_error(
    .Call(symbol, params, tags, "ParamInt", NULL, NULL),
    "callback-free"
  )
  expect_identical(callbacks, 0L)
})

test_that("native matching follows R character encoding equality", {
  utf8 = enc2utf8("fa\u00e7ade")
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  skip_if(is.na(latin1), "this platform cannot represent the latin1 fixture")
  Encoding(latin1) = "latin1"
  bytes = utf8
  Encoding(bytes) = "bytes"

  symbol = get("C_param_set_ids", envir = asNamespace("paradox"))
  params = list(id = c(utf8, "plain"), cls = c(latin1, bytes))
  tags = list(id = c(latin1, "plain"), tag = c(latin1, bytes))

  expect_identical(.Call(symbol, params, tags, utf8, NULL, NULL), utf8)
  expect_identical(.Call(symbol, params, tags, bytes, NULL, NULL), "plain")
  expect_identical(.Call(symbol, params, tags, NULL, utf8, NULL), utf8)
  expect_identical(.Call(symbol, params, tags, NULL, bytes, NULL), "plain")
  expect_identical(
    .Call(symbol, params, tags, NULL, NULL, c(bytes, utf8)),
    c(utf8, "plain")
  )
})

test_that("mixed-encoding native matching survives forced collection", {
  skip_on_cran()

  utf8 = enc2utf8("fa\u00e7ade")
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  skip_if(is.na(latin1), "this platform cannot represent the latin1 fixture")
  Encoding(latin1) = "latin1"

  symbol = get("C_param_set_ids", envir = asNamespace("paradox"))
  params = list(
    id = c("first", "second"),
    cls = c(latin1, "ParamInt")
  )
  tags = list(
    id = c("first", "second"),
    tag = c(latin1, "plain")
  )

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  by_class = .Call(symbol, params, tags, utf8, NULL, NULL)
  by_tag = .Call(symbol, params, tags, NULL, utf8, NULL)
  gctorture(previous)

  expect_identical(by_class, "first")
  expect_identical(by_tag, "first")
})

test_that("ids agree with a scalar reference across filter combinations", {
  ids = sprintf("id_%02d", c(19:24, 7:12, 1:6, 13:18))
  classes = rep(c("ParamInt", "ParamDbl", "ParamFct", "ParamLgl"), 6L)
  tag_sets = lapply(seq_along(ids), function(index) {
    c("common", c("red", "blue", "green")[(index %% 3L) + 1L])[
      seq_len(1L + (index %% 2L))
    ]
  })
  constructors = list(
    ParamInt = function(tags) p_int(1, 4, tags = tags),
    ParamDbl = function(tags) p_dbl(-1, 1, tags = tags),
    ParamFct = function(tags) p_fct(letters[1:3], tags = tags),
    ParamLgl = function(tags) p_lgl(tags = tags)
  )
  domains = Map(function(class, tags) constructors[[class]](tags), classes, tag_sets)
  set = ParamSet$new(setNames(domains, ids))

  reference = function(class = NULL, tags = NULL, any_tags = NULL) {
    keep = rep(TRUE, length(ids))
    if (!is.null(class)) keep = keep & classes %in% class
    if (!is.null(tags) && length(tags)) {
      keep = keep & vapply(tag_sets, function(available) all(tags %in% available), logical(1L))
    }
    if (!is.null(any_tags)) {
      keep = keep & vapply(tag_sets, function(available) any(any_tags %in% available), logical(1L))
    }
    if (!is.null(tags) && !length(tags) && is.null(any_tags)) keep[] = FALSE
    ids[keep]
  }

  class_filters = list(NULL, character(), "ParamInt", c("ParamFct", "ParamInt"), "ParamUnknown")
  tag_filters = list(NULL, character(), "red", c("red", "blue"), c("red", "red"), "absent")
  any_filters = list(NULL, character(), "green", c("blue", "red"), "absent")
  for (class in class_filters) {
    for (tags in tag_filters) {
      for (any_tags in any_filters) {
        expect_identical(
          set$ids(class = class, tags = tags, any_tags = any_tags),
          reference(class, tags, any_tags)
        )
      }
    }
  }
})

test_that("direct ids handles work beyond an interrupt interval", {
  size = 65537L
  ids = sprintf("parameter_%05d", seq_len(size))
  params = list(id = ids, cls = rep("ParamInt", size))
  tags = list(id = rev(ids), tag = rep("bulk", size))
  symbol = get("C_param_set_ids", envir = asNamespace("paradox"))

  expect_identical(.Call(symbol, params, tags, NULL, "bulk", NULL), ids)
})
