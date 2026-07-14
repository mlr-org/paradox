native_collection_values_available = function() {
  exists(
    "C_param_set_collection_values",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

native_collection_values_symbol = function() {
  get(
    "C_param_set_collection_values",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

native_collection_values_call = function(collection, private = NULL) {
  if (is.null(private)) private = collection$.__enclos_env__$private
  .Call(native_collection_values_symbol(), private, collection)
}

native_collection_values_reference = function(collection) {
  private = collection$.__enclos_env__$private
  sets = private$.sets
  if (private$.postfix && !is.null(names(sets))) {
    values = lapply(seq_along(sets), function(index) {
      value = sets[[index]]$values
      owner = names(sets)[[index]]
      if (nchar(owner)) {
        names(value) = sprintf("%s.%s", names(value), owner)
      }
      value
    })
    values = unlist(unname(values), recursive = FALSE)
  } else {
    values = lapply(sets, function(set) set$values)
    values = unlist(values, recursive = FALSE)
  }
  if (length(values)) values else setNames(list(), character())
}

native_collection_values_rich = function(postfix = FALSE) {
  marker = new.env(parent = emptyenv())
  marker$value = 1L
  left = ps(
    zeta = p_int(init = 1L),
    alpha = p_lgl(init = TRUE),
    payload = p_uty()
  )
  left$values = list(zeta = 2L, alpha = FALSE, payload = NULL)
  right = ps(amount = p_dbl(-2, 2), marker = p_uty())
  right$values = list(amount = 0.25, marker = marker)
  ParamSetCollection$new(
    list(owner = left, other = right),
    postfix_names = postfix
  )
}

test_that("native collection values is registered with forced arity two", {
  skip_if_not(native_collection_values_available())
  symbol = native_collection_values_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)
  expect_error(
    .Call("param_set_collection_values", PACKAGE = "paradox"),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("native values preserves empty, prefix, postfix, and nested order", {
  skip_if_not(native_collection_values_available())
  prefix = native_collection_values_rich(FALSE)
  postfix = native_collection_values_rich(TRUE)
  nested = ParamSetCollection$new(list(
    outer = postfix,
    tail = ps(last = p_dbl(init = 0.5))
  ))
  unnamed = ParamSetCollection$new(setNames(list(
    ps(first = p_int(init = 1L)),
    ps(second = p_lgl(init = TRUE))
  ), c("", "")))
  empty_children = ParamSetCollection$new(setNames(
    replicate(20L, ParamSet$new(), simplify = FALSE),
    c("", "", sprintf("empty%02d", seq_len(18L)))
  ))

  for (collection in list(
      ParamSetCollection$new(list()),
      empty_children,
      unnamed,
      prefix,
      postfix,
      nested
    )) {
    expected = native_collection_values_reference(collection)
    direct = native_collection_values_call(collection)
    expect_false(is.null(direct))
    expect_identical(direct, expected)
    expect_identical(collection$values, expected)
  }
  expect_identical(
    names(prefix$values),
    c(
      "owner.zeta", "owner.alpha", "owner.payload",
      "other.amount", "other.marker"
    )
  )
  expect_identical(
    names(postfix$values),
    c(
      "zeta.owner", "alpha.owner", "payload.owner",
      "amount.other", "marker.other"
    )
  )
  expect_identical(names(nested$values), c(
    "outer.zeta.owner", "outer.alpha.owner", "outer.payload.owner",
    "outer.amount.other", "outer.marker.other", "tail.last"
  ))
  expect_type(ParamSetCollection$new(list())$values, "list")
  expect_identical(names(ParamSetCollection$new(list())$values), character())
})

test_that("native values owns output shells while retaining opaque leaves", {
  skip_if_not(native_collection_values_available())
  collection = native_collection_values_rich()
  marker = collection$sets$other$values$marker
  first = native_collection_values_call(collection)
  second = native_collection_values_call(collection)

  expect_identical(first, second)
  expect_false(identical(
    data.table::address(first),
    data.table::address(second)
  ))
  expect_false(identical(
    data.table::address(names(first)),
    data.table::address(names(second))
  ))
  expect_identical(first$other.marker, marker)

  names(first)[[1L]] = "changed"
  first[[2L]] = TRUE
  expect_identical(native_collection_values_call(collection), second)
  first$other.marker$value = 42L
  expect_identical(marker$value, 42L)
})

test_that("repeated sibling references are values DAG occurrences", {
  skip_if_not(native_collection_values_available())
  shared = ps(x = p_int(init = 1L), y = p_lgl(init = TRUE))
  repeated = ParamSetCollection$new(list(left = shared, right = shared))
  direct = native_collection_values_call(repeated)

  expect_false(is.null(direct))
  expect_identical(direct, list(
    left.x = 1L,
    left.y = TRUE,
    right.x = 1L,
    right.y = TRUE
  ))
  shared$values = list(x = 2L)
  expect_identical(repeated$values, list(left.x = 2L, right.x = 2L))
})

test_that("collection values cycles error without rejecting sibling reuse", {
  skip_if_not(native_collection_values_available())
  cyclic = ParamSetCollection$new(list())
  cyclic$.__enclos_env__$private$.sets = list(self = cyclic)
  expect_error(
    native_collection_values_call(cyclic),
    "Cyclic ParamSetCollection values graph is unsupported",
    fixed = TRUE
  )
})

test_that("subclass values fall back before invoking extension callbacks", {
  skip_if_not(native_collection_values_available())
  events = new.env(parent = emptyenv())
  events$reads = 0L
  CountingValues = R6::R6Class(
    "NativeCollectionValuesCountingValues",
    inherit = ParamSet,
    active = list(
      values = function(value) {
        if (!missing(value)) {
          super$values = value
          return(value)
        }
        events$reads = events$reads + 1L
        super$values
      }
    )
  )
  child = CountingValues$new(list(x = p_int(init = 1L)))
  collection = ParamSetCollection$new(list(child = child))

  events$reads = 0L
  expect_null(native_collection_values_call(collection))
  expect_identical(events$reads, 0L)
  expect_identical(collection$values, list(child.x = 1L))
  expect_identical(events$reads, 1L)

  SubCollection = R6::R6Class(
    "NativeCollectionValuesSubCollection",
    inherit = ParamSetCollection
  )
  subclass = SubCollection$new(list(child = ps(x = p_int(init = 1L))))
  expect_null(native_collection_values_call(subclass))
  expect_null(native_collection_values_call(
    ParamSetCollection$new(list(outer = subclass))
  ))

  make_custom = function() paradox:::Domain(
    cls = "NativeCollectionValuesExtension",
    grouping = "NativeCollectionValuesExtension",
    storage_type = "list"
  )
  custom_child = ParamSet$new(list(custom = make_custom()))
  custom_child$.__enclos_env__$private$.values = list(custom = 3L)
  custom_collection = ParamSetCollection$new(list(child = custom_child))
  expect_null(native_collection_values_call(custom_collection))
  expect_identical(custom_collection$values, list(child.custom = 3L))
})

test_that("replaced public and private getters decline before execution", {
  skip_if_not(native_collection_values_available())
  child = ps(x = p_int(init = 1L))
  collection = ParamSetCollection$new(list(child = child))
  original_values = activeBindingFunction("values", child)
  public_reads = 0L
  makeActiveBinding("values", function(value) {
    public_reads <<- public_reads + 1L
    original_values(value)
  }, child)

  expect_null(native_collection_values_call(collection))
  expect_identical(public_reads, 0L)
  expect_identical(collection$values, list(child.x = 1L))
  expect_identical(public_reads, 1L)

  child = ps(x = p_int(init = 2L))
  collection = ParamSetCollection$new(list(child = child))
  private = child$.__enclos_env__$private
  original_getter = private$.get_values
  private_reads = 0L
  unlockBinding(".get_values", private)
  private$.get_values = function() {
    private_reads <<- private_reads + 1L
    original_getter()
  }
  lockBinding(".get_values", private)

  expect_null(native_collection_values_call(collection))
  expect_identical(private_reads, 0L)
  expect_identical(collection$values, list(child.x = 2L))
  expect_identical(private_reads, 1L)
})

test_that("reparented R6 getter wrappers decline before execution", {
  skip_if_not(native_collection_values_available())
  namespace = asNamespace("paradox")

  collection = native_collection_values_rich()
  events = new.env(parent = emptyenv())
  events$public = 0L
  forged_parent = new.env(parent = namespace)
  forged_parent$.__ParamSet__values = function(self, private, super, xs) {
    events$public = events$public + 1L
    list(forged = 11L)
  }
  wrapper_environment = environment(activeBindingFunction(
    "values",
    collection
  ))
  parent.env(wrapper_environment) = forged_parent

  expect_null(native_collection_values_call(collection))
  expect_identical(events$public, 0L)
  expect_identical(collection$values, list(forged = 11L))
  expect_identical(events$public, 1L)

  collection = native_collection_values_rich()
  events$private = 0L
  forged_parent = new.env(parent = namespace)
  forged_parent$.__ParamSetCollection__.get_values = function(
      self, private, super) {
    events$private = events$private + 1L
    list(forged = 12L)
  }
  wrapper_environment = environment(
    collection$.__enclos_env__$private$.get_values
  )
  parent.env(wrapper_environment) = forged_parent

  expect_null(native_collection_values_call(collection))
  expect_identical(events$private, 0L)
  expect_identical(collection$values, list(forged = 12L))
  expect_identical(events$private, 1L)
})

test_that("delayed private values decline without forcing", {
  skip_if_not(native_collection_values_available())
  child = ps(x = p_int())
  collection = ParamSetCollection$new(list(child = child))
  private = child$.__enclos_env__$private
  reads = new.env(parent = emptyenv())
  reads$count = 0L
  delayedAssign(
    ".values",
    {
      reads$count = reads$count + 1L
      list(x = 3L)
    },
    assign.env = private,
    eval.env = environment()
  )

  expect_null(native_collection_values_call(collection))
  expect_identical(reads$count, 0L)
  expect_identical(collection$values, list(child.x = 3L))
  expect_identical(reads$count, 1L)
})

test_that("collection values rejects public wrapper shadows without forcing", {
  skip_if_not(native_collection_values_available())
  namespace = asNamespace("paradox")

  for (kind in c("active", "delayed")) {
    for (shadow in c(".__ParamSet__values", "super")) {
      child = ps(x = p_int(init = 3L))
      collection = ParamSetCollection$new(list(child = child))
      wrapper_environment = environment(activeBindingFunction(
        "values",
        child
      ))
      reads = new.env(parent = emptyenv())
      reads$count = 0L
      value = if (shadow == "super") {
        NULL
      } else {
        get(shadow, envir = namespace, inherits = FALSE)
      }
      if (kind == "active") {
        makeActiveBinding(shadow, function(replacement) {
          reads$count = reads$count + 1L
          value
        }, wrapper_environment)
      } else {
        evaluation_environment = list2env(
          list(reads = reads, value = value),
          parent = baseenv()
        )
        delayedAssign(
          shadow,
          {
            reads$count = reads$count + 1L
            value
          },
          eval.env = evaluation_environment,
          assign.env = wrapper_environment
        )
      }

      expect_null(
        native_collection_values_call(collection),
        info = paste(kind, shadow)
      )
      expect_identical(reads$count, 0L, info = paste(kind, shadow))
      expect_identical(collection$values, list(child.x = 3L))
      if (shadow == "super") {
        expect_identical(reads$count, 0L, info = paste(kind, shadow))
      } else {
        expect_true(reads$count > 0L, info = paste(kind, shadow))
      }
    }
  }
})

test_that("collection values authenticates private superclass captures", {
  skip_if_not(native_collection_values_available())
  namespace = asNamespace("paradox")

  for (kind in c("active", "delayed")) {
    for (shadow in c(".__ParamSetCollection__.get_values", "super")) {
      collection = ParamSetCollection$new(list(
        child = ps(x = p_int(init = 4L))
      ))
      enclosure = collection$.__enclos_env__
      saved_super = get("super", envir = enclosure, inherits = FALSE)
      reads = new.env(parent = emptyenv())
      reads$count = 0L
      value = if (shadow == "super") {
        rm(list = "super", envir = enclosure)
        saved_super
      } else {
        get(shadow, envir = namespace, inherits = FALSE)
      }
      if (kind == "active") {
        makeActiveBinding(shadow, function(replacement) {
          reads$count = reads$count + 1L
          value
        }, enclosure)
      } else {
        evaluation_environment = list2env(
          list(reads = reads, value = value),
          parent = baseenv()
        )
        delayedAssign(
          shadow,
          {
            reads$count = reads$count + 1L
            value
          },
          eval.env = evaluation_environment,
          assign.env = enclosure
        )
      }

      expect_null(
        native_collection_values_call(collection),
        info = paste(kind, shadow)
      )
      expect_identical(reads$count, 0L, info = paste(kind, shadow))
      expect_identical(collection$values, list(child.x = 4L))
      if (shadow == "super") {
        expect_identical(reads$count, 0L, info = paste(kind, shadow))
      } else {
        expect_true(reads$count > 0L, info = paste(kind, shadow))
      }
    }
  }
})

test_that("malformed value order and collection metadata decline", {
  skip_if_not(native_collection_values_available())
  child = ps(a = p_int(init = 1L), b = p_lgl(init = TRUE))
  collection = ParamSetCollection$new(list(child = child))
  child_private = child$.__enclos_env__$private
  child_private$.values = list(b = FALSE, a = 2L)

  expect_null(native_collection_values_call(collection))
  expect_identical(collection$values, list(child.b = FALSE, child.a = 2L))

  collection = native_collection_values_rich()
  private = collection$.__enclos_env__$private
  with_private_value = function(name, value, code) {
    original = private[[name]]
    on.exit(private[[name]] <- original)
    private[[name]] = value
    force(code)
  }
  expect_null(with_private_value(
    ".postfix",
    structure(FALSE, note = TRUE),
    native_collection_values_call(collection)
  ))
  sets = private$.sets
  attr(sets, "note") = TRUE
  expect_null(with_private_value(
    ".sets",
    sets,
    native_collection_values_call(collection)
  ))
  translation = data.table::copy(private$.translation)
  translation$owner_name[[1L]] = "wrong"
  expect_null(with_private_value(
    ".translation",
    translation,
    native_collection_values_call(collection)
  ))
  params = data.table::copy(private$.params)
  params$id[[1L]] = "wrong.affix"
  expect_null(with_private_value(
    ".params",
    params,
    native_collection_values_call(collection)
  ))
})

test_that("unsupported bytes names decline before a child read", {
  skip_if_not(native_collection_values_available())
  events = new.env(parent = emptyenv())
  events$reads = 0L
  ByteCounting = R6::R6Class(
    "NativeCollectionValuesByteCounting",
    inherit = ParamSet,
    active = list(
      values = function(value) {
        if (!missing(value)) {
          super$values = value
          return(value)
        }
        events$reads = events$reads + 1L
        super$values
      }
    )
  )
  child = ByteCounting$new(list(x = p_int(init = 1L)))
  collection = ParamSetCollection$new(list(owner = child))
  private = collection$.__enclos_env__$private
  owner = "owner"
  Encoding(owner) = "bytes"
  names(private$.sets) = owner

  expect_null(native_collection_values_call(collection))
  expect_identical(events$reads, 0L)
  expect_identical(collection$values, list(owner.x = 1L))
  expect_identical(events$reads, 1L)
})

test_that("semantically equal supported encodings remain admissible", {
  skip_if_not(native_collection_values_available())
  utf8_owner = enc2utf8("caf\u00e9")
  latin1_owner = iconv(utf8_owner, from = "UTF-8", to = "latin1")
  skip_if(is.na(latin1_owner))
  Encoding(latin1_owner) = "latin1"

  collection = ParamSetCollection$new(list(
    cafe = ps(x = p_int(init = 1L))
  ))
  private = collection$.__enclos_env__$private
  names(private$.sets) = latin1_owner
  utf8_id = paste0(utf8_owner, ".x")
  params_attributes = attributes(private$.params)
  data.table::set(private$.params, 1L, "id", utf8_id)
  attributes(private$.params) = params_attributes
  translation_attributes = attributes(private$.translation)
  data.table::set(private$.translation, 1L, "id", utf8_id)
  data.table::set(private$.translation, 1L, "owner_name", utf8_owner)
  attributes(private$.translation) = translation_attributes
  direct = native_collection_values_call(collection)

  expect_false(is.null(direct))
  expect_identical(unname(direct), list(1L))
  expect_identical(enc2utf8(names(direct)), "caf\u00e9.x")
})

test_that("serialized and cloned exact values graphs remain admissible", {
  skip_if_not(native_collection_values_available())
  original = native_collection_values_rich()
  cases = list(
    unserialize(serialize(original, NULL)),
    original$clone(deep = FALSE),
    original$clone(deep = TRUE)
  )
  for (collection in cases) {
    direct = native_collection_values_call(collection)
    expect_false(is.null(direct))
    expect_identical(direct, native_collection_values_reference(collection))
  }
})

test_that("native collection values survives forced collection", {
  skip_on_cran()

  skip_if_not(native_collection_values_available())
  collection = ParamSetCollection$new(list(
    outer = native_collection_values_rich(TRUE),
    tail = ps(last = p_int(init = 4L))
  ))
  expected = native_collection_values_reference(collection)
  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed = native_collection_values_call(collection)
  gctorture(previous)
  expect_identical(observed, expected)
})

test_that("native collection values rejects mismatched entry arguments", {
  skip_if_not(native_collection_values_available())
  first = native_collection_values_rich()
  second = native_collection_values_rich()
  expect_null(.Call(
    native_collection_values_symbol(),
    second$.__enclos_env__$private,
    first
  ))
  expect_null(.Call(
    native_collection_values_symbol(),
    first$.__enclos_env__$private,
    new.env(parent = emptyenv())
  ))
})
