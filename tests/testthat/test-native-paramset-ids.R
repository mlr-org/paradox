native_ids_param_set = function() {
  ParamSet$new(list(
    zeta = p_int(1, 2, tags = c("red", "common")),
    alpha = p_dbl(0, 1, tags = c("blue", "common")),
    middle = p_fct(c("x", "y"), tags = c("red", "blue")),
    bare = p_lgl()
  ))
}

test_that("native ids routine is registered with forced symbols", {
  symbol = get("C_param_set_ids", envir = asNamespace("paradox"))

  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 5L)
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

test_that("native ParamSet ids preserve order across filters", {
  param_set = native_ids_param_set()

  expect_identical(param_set$ids(), c("zeta", "alpha", "middle", "bare"))
  expect_identical(param_set$ids(class = "ParamInt"), "zeta")
  expect_identical(
    param_set$ids(class = c("ParamFct", "ParamInt")),
    c("zeta", "middle")
  )
  expect_identical(param_set$ids(class = c("ParamInt", "ParamInt")), "zeta")
  expect_identical(param_set$ids(tags = "red"), c("zeta", "middle"))
  expect_identical(param_set$ids(tags = c("red", "blue")), "middle")
  expect_identical(
    param_set$ids(class = c("ParamInt", "ParamFct"), tags = "red"),
    c("zeta", "middle")
  )
  expect_identical(param_set$ids(tags = "common", any_tags = "blue"), "alpha")
})

test_that("empty and duplicate filters have stable character results", {
  param_set = native_ids_param_set()

  expect_identical(param_set$ids(class = character()), character())
  # Upstream returned NULL here; ids() documents an always-character result.
  expect_identical(param_set$ids(tags = character()), character())
  expect_identical(param_set$ids(any_tags = character()), character())
  expect_identical(
    param_set$ids(tags = character(), any_tags = "blue"),
    c("alpha", "middle")
  )
  expect_identical(
    param_set$ids(tags = "common", any_tags = character()),
    character()
  )

  expect_identical(param_set$ids(tags = c("red", "red")), c("zeta", "middle"))
  expect_identical(param_set$ids(any_tags = c("red", "red")), c("zeta", "middle"))
  # Upstream returned `middle` twice because it had two matching tags.
  expect_identical(
    param_set$ids(any_tags = c("blue", "red")),
    c("zeta", "alpha", "middle")
  )
  expect_identical(
    param_set$ids(class = c("ParamDbl", "ParamFct"), any_tags = c("red", "blue")),
    c("alpha", "middle")
  )
})

test_that("empty ParamSet ids retain their documented type", {
  param_set = ParamSet$new()

  for (arguments in list(
    list(),
    list(class = character()),
    list(tags = character()),
    list(any_tags = character()),
    list(class = "ParamInt"),
    list(tags = "tag"),
    list(any_tags = "tag")
  )) {
    expect_identical(do.call(param_set$ids, arguments), character())
  }
})

test_that("native ids support custom Domain class names", {
  custom_domain = function(levels, tags = character()) {
    paradox:::Domain(
      cls = "ParamNativeIdsCustom",
      grouping = "native-ids-custom",
      levels = levels,
      storage_type = "character",
      tags = tags
    )
  }
  param_set = ParamSet$new(list(
    integer = p_int(1, 3, tags = "common"),
    custom_b = custom_domain(c("b1", "b2"), c("foreign", "common")),
    logical = p_lgl(tags = "common"),
    custom_a = custom_domain("a", "foreign")
  ))

  expect_identical(
    param_set$ids(class = "ParamNativeIdsCustom"),
    c("custom_b", "custom_a")
  )
  expect_identical(
    param_set$ids(class = "ParamNativeIdsCustom", tags = "foreign"),
    c("custom_b", "custom_a")
  )
  expect_identical(
    param_set$ids(class = c("ParamLgl", "ParamNativeIdsCustom"), tags = "common"),
    c("custom_b", "logical")
  )
})

test_that("native ids preserve character argument validation", {
  param_set = native_ids_param_set()

  expect_error(
    param_set$ids(class = NA_character_),
    "Assertion on 'class' failed: Contains missing values \\(element 1\\).",
    fixed = FALSE
  )
  expect_error(
    param_set$ids(tags = c("red", NA_character_)),
    "Assertion on 'tags' failed: Contains missing values \\(element 2\\).",
    fixed = FALSE
  )
  expect_error(
    param_set$ids(any_tags = factor("red")),
    "Must be of type 'character' \\(or 'NULL'\\), not 'factor'.",
    fixed = FALSE
  )
  expect_error(
    param_set$ids(class = 1),
    "Must be of type 'character' \\(or 'NULL'\\), not 'double'.",
    fixed = FALSE
  )
  expect_error(
    param_set$ids(any_tags = list("red")),
    "Must be of type 'character' \\(or 'NULL'\\), not 'list'.",
    fixed = FALSE
  )

  expect_identical(param_set$ids(class = matrix("ParamInt", 1L, 1L)), "zeta")
  expect_identical(param_set$ids(tags = c(named = "red")), c("zeta", "middle"))
  expect_identical(param_set$ids(class = I("ParamInt")), "zeta")
})

test_that("native ids never dispatch matching methods from live columns", {
  callbacks = 0L
  class_name = "NativeIdsCallbackCapableColumn"
  registerS3method(
    "mtfrm",
    class_name,
    function(x) {
      callbacks <<- callbacks + 1L
      stop("native ids dispatched mtfrm", call. = FALSE)
    },
    envir = asNamespace("base")
  )
  message = paste(
    "Corrupt ParamSet storage: matching columns must use callback-free",
    "character representations"
  )

  cases = list(
    list(table = ".params", column = "cls", arguments = list(class = "ParamInt")),
    list(table = ".params", column = "id", arguments = list(tags = "red")),
    list(table = ".tags", column = "id", arguments = list(tags = "red")),
    list(table = ".tags", column = "tag", arguments = list(tags = "red"))
  )
  for (case in cases) {
    param_set = native_ids_param_set()
    private = param_set$.__enclos_env__$private
    column = private[[case$table]][[case$column]]
    data.table::setattr(column, "class", class_name)
    expect_error(
      do.call(param_set$ids, case$arguments),
      message,
      fixed = TRUE,
      info = paste(case$table, case$column)
    )
    expect_identical(
      callbacks,
      0L,
      info = paste(case$table, case$column)
    )
  }

  param_set = native_ids_param_set()
  private = param_set$.__enclos_env__$private
  data.table::setattr(
    private$.params$id,
    "names",
    paste0("id_", seq_along(private$.params$id))
  )
  data.table::setattr(
    private$.params$cls,
    "names",
    paste0("cls_", seq_along(private$.params$cls))
  )
  data.table::setattr(
    private$.tags$id,
    "names",
    paste0("tag_id_", seq_along(private$.tags$id))
  )
  data.table::setattr(
    private$.tags$tag,
    "names",
    paste0("tag_", seq_along(private$.tags$tag))
  )
  expect_identical(
    param_set$ids(class = "ParamInt", tags = "red"),
    "zeta"
  )
  expect_identical(callbacks, 0L)
})

test_that("native ids close finalizer mutations before matching", {
  callbacks = 0L
  finalizers = 0L
  class_name = "NativeIdsFinalizerClassedColumn"
  registerS3method(
    "mtfrm",
    class_name,
    function(x) {
      callbacks <<- callbacks + 1L
      stop("native ids dispatched finalizer-installed mtfrm", call. = FALSE)
    },
    envir = asNamespace("base")
  )

  param_set = native_ids_param_set()
  private = param_set$.__enclos_env__$private
  victim = new.env(parent = emptyenv())
  reg.finalizer(victim, function(unused) {
    finalizers <<- finalizers + 1L
    data.table::setattr(private$.params$cls, "class", class_name)
  }, onexit = FALSE)
  holder = list(victim)
  rm(victim)

  expect_error(
    param_set$ids(class = {
      holder[[1L]] = NULL
      invisible(gc())
      "ParamInt"
    }),
    paste(
      "Corrupt ParamSet storage: matching columns must use callback-free",
      "character representations"
    ),
    fixed = TRUE
  )
  expect_identical(finalizers, 1L)
  expect_identical(callbacks, 0L)
})

test_that("native ids preserve R character encoding equality", {
  utf8 = enc2utf8("fa\u00e7ade")
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  skip_if(is.na(latin1), "this platform cannot represent the latin1 fixture")
  Encoding(latin1) = "latin1"
  bytes = utf8
  Encoding(bytes) = "bytes"

  expect_identical(match(utf8, latin1), 1L)
  expect_identical(match(latin1, utf8), 1L)
  expect_identical(match(bytes, utf8), NA_integer_)

  symbol = get("C_param_set_ids", envir = asNamespace("paradox"))
  params = list(
    id = c(utf8, "plain"),
    cls = c(latin1, bytes)
  )
  tags = list(
    id = c(latin1, "plain"),
    tag = c(latin1, bytes)
  )

  expect_identical(.Call(symbol, params, tags, utf8, NULL, NULL), utf8)
  expect_identical(.Call(symbol, params, tags, bytes, NULL, NULL), "plain")
  expect_identical(.Call(symbol, params, tags, NULL, utf8, NULL), utf8)
  expect_identical(.Call(symbol, params, tags, NULL, bytes, NULL), "plain")
  expect_identical(
    .Call(symbol, params, tags, NULL, c(utf8, latin1), NULL),
    utf8
  )
  expect_identical(
    .Call(symbol, params, tags, NULL, NULL, c(bytes, utf8)),
    c(utf8, "plain")
  )
})

test_that("native ids reject structurally corrupt canonical storage", {
  corrupt = function(params = NULL, tags = NULL) {
    param_set = native_ids_param_set()
    private = param_set$.__enclos_env__$private
    if (!is.null(params)) private$.params = params(private$.params)
    if (!is.null(tags)) private$.tags = tags(private$.tags)
    param_set
  }

  expect_error(
    corrupt(params = function(x) 1)$ids(),
    "`.params` must be a list",
    fixed = TRUE
  )
  expect_error(
    corrupt(params = unname)$ids(),
    "`.params` must be a named list",
    fixed = TRUE
  )
  expect_error(
    corrupt(params = function(x) { x$id = NULL; x })$ids(),
    "`.params` has no `id` column",
    fixed = TRUE
  )
  expect_error(
    corrupt(params = function(x) { x$id = seq_along(x$id); x })$ids(),
    "`id` must have type `character`",
    fixed = TRUE
  )
  expect_error(
    corrupt(params = function(x) {
      x = unclass(x)
      x$cls = x$cls[-1L]
      x
    })$ids(),
    "`cls` must have type `character` and length 4",
    fixed = TRUE
  )
  expect_error(
    corrupt(tags = function(x) 1)$ids(tags = "red"),
    "`.tags` must be a list",
    fixed = TRUE
  )
  expect_error(
    corrupt(tags = function(x) unname(unclass(x)))$ids(tags = "red"),
    "`.tags` must be a named list",
    fixed = TRUE
  )
  expect_error(
    corrupt(tags = function(x) { x$tag = NULL; x })$ids(tags = "red"),
    "`.tags` has no `tag` column",
    fixed = TRUE
  )
  expect_error(
    corrupt(tags = function(x) { x$id[[1L]] = "unknown"; x })$ids(tags = "red"),
    "`.tags$id` contains an unknown parameter ID",
    fixed = TRUE
  )
  expect_error(
    corrupt(tags = function(x) { x$tag[[1L]] = NA_character_; x })$ids(tags = "red"),
    "`.tags$tag` contains a missing value",
    fixed = TRUE
  )
})

test_that("native ids handle shuffled and repeated tag rows defensively", {
  param_set = native_ids_param_set()
  param_set$.__enclos_env__$private$.tags = structure(
    list(
      id = c("middle", "zeta", "middle", "alpha", "middle", "zeta"),
      tag = c("red", "red", "red", "blue", "blue", "common")
    ),
    class = c("data.table", "data.frame")
  )

  expect_identical(param_set$ids(tags = "red"), c("zeta", "middle"))
  expect_identical(
    param_set$ids(any_tags = c("blue", "red")),
    c("zeta", "alpha", "middle")
  )
  expect_identical(param_set$ids(tags = c("red", "blue")), "middle")
})

test_that("native ids agree with filter semantics across combinations", {
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
  param_set = ParamSet$new(setNames(domains, ids))

  reference = function(class = NULL, tags = NULL, any_tags = NULL) {
    keep = rep(TRUE, length(ids))
    if (!is.null(class)) keep = keep & classes %in% class
    if (!is.null(tags) && length(tags)) {
      keep = keep & vapply(tag_sets, function(available) {
        all(tags %in% available)
      }, logical(1L))
    }
    if (!is.null(any_tags)) {
      keep = keep & vapply(tag_sets, function(available) {
        any(any_tags %in% available)
      }, logical(1L))
    }
    if (!is.null(tags) && !length(tags) && is.null(any_tags)) {
      keep[] = FALSE
    }
    ids[keep]
  }

  class_filters = list(
    NULL, character(), "ParamInt", c("ParamFct", "ParamInt"),
    c("ParamInt", "ParamInt"), "ParamUnknown"
  )
  tag_filters = list(
    NULL, character(), "red", c("red", "blue"),
    c("red", "red"), "absent"
  )
  any_tag_filters = list(
    NULL, character(), "green", c("blue", "red"),
    c("green", "green"), "absent"
  )

  for (class in class_filters) {
    for (tags in tag_filters) {
      for (any_tags in any_tag_filters) {
        expect_identical(
          param_set$ids(class = class, tags = tags, any_tags = any_tags),
          reference(class = class, tags = tags, any_tags = any_tags)
        )
      }
    }
  }
})

test_that("native ids handle input beyond an interrupt interval", {
  size = 65537L
  ids = sprintf("parameter_%05d", seq_len(size))
  params = list(id = ids, cls = rep("ParamInt", size))
  tag_table = list(id = rev(ids), tag = rep("bulk", size))
  symbol = get("C_param_set_ids", envir = asNamespace("paradox"))

  result = .Call(symbol, params, tag_table, NULL, "bulk", NULL)

  expect_identical(result, ids)
})
