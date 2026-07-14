context("characterization: ParamSetCollection params")

psc_params_reference = function(param_set) {
  private = param_set$.__enclos_env__$private
  result = data.table::copy(private$.params)
  result[, .tags := list(param_set$tags)]
  result[private$.trafos, .trafo := list(trafo), on = "id"]
  result[
    param_set$deps,
    .requirements := mlr3misc::transpose_list(list(on, cond)),
    on = "id"
  ]
  values = param_set$values
  result[, c(".init_given", ".init") := list(
    id %in% names(values),
    unname(values[id])
  )]
  result[]
}

psc_params_custom_domain = function() {
  paradox:::Domain(
    cls = "ParamCollectionParamsExtension",
    grouping = "ParamCollectionParamsExtension",
    storage_type = "list"
  )
}

psc_params_random_child = function(seed, size) {
  ids = sprintf("p%02d", seq_len(size))
  domains = lapply(seq_len(size), function(index) {
    tag_count = (seed + index) %% 4L
    tags = if (tag_count) {
      sample(c("alpha", "beta", "gamma"), tag_count, replace = FALSE)
    } else {
      character()
    }
    if ((seed + index) %% 5L == 0L) {
      p_int(-5L, 5L, tags = tags, trafo = function(x) x + 1L)
    } else {
      p_int(
        -5L,
        5L,
        tags = tags,
        init = as.integer((seed + index) %% 7L - 3L)
      )
    }
  })
  names(domains) = ids
  result = ParamSet$new(domains)
  if (size >= 2L) {
    for (index in seq.int(2L, size, by = 3L)) {
      result$add_dep(
        ids[[index]],
        ids[[index - 1L]],
        CondAnyOf(c(-2L, 0L, 2L))
      )
    }
  }
  result
}

test_that("collection params preserve child callback order and count", {
  events = new.env(parent = emptyenv())
  events$seen = character()

  CountingParamSet = R6::R6Class(
    "ParamCollectionParamsCountingChild",
    inherit = ParamSet,
    public = list(
      initialize = function(label, params) {
        private$.label = label
        super$initialize(params)
      }
    ),
    active = list(
      tags = function(value) {
        if (!missing(value)) {
          super$tags = value
          return(value)
        }
        events$seen = c(events$seen, sprintf("%s:tags", private$.label))
        super$tags
      },
      deps = function(value) {
        if (!missing(value)) {
          super$deps = value
          return(value)
        }
        events$seen = c(events$seen, sprintf("%s:deps", private$.label))
        super$deps
      },
      values = function(value) {
        if (!missing(value)) {
          super$values = value
          return(value)
        }
        events$seen = c(events$seen, sprintf("%s:values", private$.label))
        super$values
      }
    ),
    private = list(.label = NULL)
  )

  left = CountingParamSet$new(
    "left",
    list(x = p_int(0L, 3L, tags = "left", init = 1L))
  )
  right = CountingParamSet$new(
    "right",
    list(y = p_lgl(tags = "right", init = TRUE))
  )
  events$seen = character()
  collection = ParamSetCollection$new(list(left = left, right = right))
  expect_identical(events$seen, character())

  observed = collection$params
  expect_identical(
    events$seen,
    c("left:deps", "right:deps", "left:values", "right:values")
  )
  expect_identical(observed$id, c("left.x", "right.y"))
  expect_identical(observed$.init, list(1L, TRUE))
})

test_that("collection subclasses retain their active binding dispatch", {
  events = new.env(parent = emptyenv())
  events$seen = character()

  CountingCollection = R6::R6Class(
    "ParamCollectionParamsCountingCollection",
    inherit = ParamSetCollection,
    active = list(
      tags = function(value) {
        if (!missing(value)) {
          super$tags = value
          return(value)
        }
        events$seen = c(events$seen, "tags")
        super$tags
      },
      deps = function(value) {
        if (!missing(value)) stop("deps is read-only")
        events$seen = c(events$seen, "deps")
        super$deps
      },
      values = function(value) {
        if (!missing(value)) {
          super$values = value
          return(value)
        }
        events$seen = c(events$seen, "values")
        super$values
      }
    )
  )

  collection = CountingCollection$new(list(
    child = ps(x = p_int(0L, 2L, tags = "tag", init = 1L))
  ))
  events$seen = character()
  observed = collection$params
  expect_identical(events$seen, c("tags", "deps", "values"))

  events$seen = character()
  expected = psc_params_reference(collection)
  expect_identical(events$seen, c("tags", "deps", "values"))
  expect_identical(observed, expected)
})

test_that("prefix, postfix, and nested collection order stays stable", {
  zeta = ps(
    b = p_int(init = 2L, tags = "b"),
    a = p_lgl(init = TRUE, tags = "a")
  )
  unnamed = ps(c = p_fct(c("x", "y"), init = "y", tags = "c"))
  children = setNames(list(zeta, unnamed), c("zeta", ""))

  prefix = ParamSetCollection$new(children)
  postfix = ParamSetCollection$new(children, postfix_names = TRUE)
  expect_identical(prefix$ids(), c("zeta.b", "zeta.a", "c"))
  expect_identical(prefix$params$id, c("zeta.b", "zeta.a", "c"))
  expect_identical(names(prefix$values), c("zeta.b", "zeta.a", "c"))
  expect_identical(postfix$ids(), c("b.zeta", "a.zeta", "c"))
  expect_identical(postfix$params$id, c("b.zeta", "a.zeta", "c"))
  expect_identical(names(postfix$values), c("b.zeta", "a.zeta", "c"))
  expect_identical(prefix$params, psc_params_reference(prefix))
  expect_identical(postfix$params, psc_params_reference(postfix))

  nested = ParamSetCollection$new(list(
    outer = prefix,
    tail = ps(q = p_dbl(-1, 1, init = 0.25))
  ))
  expected_ids = c(
    "outer.zeta.b", "outer.zeta.a", "outer.c", "tail.q"
  )
  expect_identical(nested$ids(), expected_ids)
  expect_identical(nested$params$id, expected_ids)
  expect_identical(names(nested$values), expected_ids)
  expect_identical(nested$params, psc_params_reference(nested))
})

test_that("empty collections retain the complete table facade", {
  empty = ParamSetCollection$new(list())
  observed = empty$params
  private = empty$.__enclos_env__$private

  expect_identical(observed, psc_params_reference(empty))
  expect_identical(dim(observed), c(0L, 16L))
  expect_identical(names(observed), paradox:::domain_names)
  expect_identical(class(observed), c("data.table", "data.frame"))
  expect_identical(data.table::key(observed), NULL)
  expect_identical(
    data.table::indices(observed),
    data.table::indices(private$.params)
  )
  expect_identical(data.table:::selfrefok(observed, FALSE), 1L)

  empty_children = ParamSetCollection$new(setNames(
    replicate(16L, ParamSet$new(), simplify = FALSE),
    sprintf("empty%02d", seq_len(16L))
  ))
  expect_identical(empty_children$params, psc_params_reference(empty_children))
  expect_identical(dim(empty_children$params), c(0L, 16L))
})

test_that("custom Domains retain the established collection params path", {
  custom = psc_params_custom_domain()
  child = ParamSet$new(list(custom = custom))
  collection = ParamSetCollection$new(list(extension = child))
  observed = collection$params

  expect_identical(observed, psc_params_reference(collection))
  expect_identical(observed$id, "extension.custom")
  expect_identical(observed$cls, "ParamCollectionParamsExtension")
  expect_identical(observed$grouping, "ParamCollectionParamsExtension")
})

test_that("collection params combine snapshot metadata with live child state", {
  left = ps(
    parent = p_int(0L, 3L, tags = "before", init = 1L),
    target = p_fct(c("a", "b"), tags = "target", init = "a")
  )
  right = ps(flag = p_lgl(tags = "right", init = TRUE))
  collection = ParamSetCollection$new(list(left = left, right = right))

  left$tags = list(parent = "after", target = "changed")
  left$values = list(parent = 2L, target = "b")
  left$add_dep("target", "parent", CondEqual(2L))
  collection$add_dep("left.target", "right.flag", CondEqual(TRUE))

  observed = collection$params
  expect_identical(observed, psc_params_reference(collection))
  expect_identical(observed$.tags[[1L]], "before")
  expect_identical(observed$.tags[[2L]], "target")
  expect_identical(observed$.init, list(2L, "b", TRUE))

  target_requirement = observed$.requirements[[2L]]
  expect_identical(target_requirement[[1L]], "right.flag")
  expect_identical(target_requirement[[2L]], CondEqual(TRUE))
  expect_null(names(target_requirement))
  expect_identical(collection$deps$id, c("left.target", "left.target"))
  expect_identical(
    collection$deps$on,
    c("left.parent", "right.flag")
  )
})

test_that("collection params own output shells and share opaque leaves", {
  marker = new.env(parent = emptyenv())
  marker$value = 1L
  child = ps(
    factor = p_fct(
      c("slow", "fast"),
      tags = c("choice", "shared"),
      special_vals = list("other"),
      init = "fast"
    ),
    number = p_dbl(
      -2,
      2,
      tags = c("numeric", "shared"),
      trafo = exp
    ),
    parent = p_int(-3L, 3L, init = 2L),
    payload = p_uty(init = marker)
  )
  child$add_dep("factor", "parent", CondEqual(2L))
  collection = ParamSetCollection$new(list(child = child))
  private = collection$.__enclos_env__$private

  first = collection$params
  second = collection$params
  expect_identical(first, second)
  expect_false(identical(data.table::address(first), data.table::address(second)))
  for (column in names(first)) {
    expect_false(identical(
      data.table::address(first[[column]]),
      data.table::address(second[[column]])
    ), info = column)
  }
  expect_identical(data.table::key(first), NULL)
  expect_identical(
    data.table::indices(first),
    data.table::indices(private$.params)
  )
  expect_identical(data.table:::selfrefok(first, FALSE), 1L)
  expect_identical(first$.init[[4L]], marker)
  expect_identical(first$.trafo[[2L]], private$.trafos$trafo[[1L]])

  data.table::set(first, i = 2L, j = "lower", value = -100)
  data.table::set(first, j = "added", value = seq_len(nrow(first)))
  first$.tags[[1L]][[1L]] = "changed"
  first$levels[[1L]][[1L]] = "changed"
  first$special_vals[[1L]][[1L]] = "changed"
  first$.requirements[[1L]][[1L]] = "changed"
  first$.requirements[[1L]][[2L]]$rhs = -1L
  data.table::setindexv(first, NULL)

  expect_identical(collection$params, second)
  expect_identical(child$deps$cond[[1L]], CondEqual(2L))
  expect_identical(data.table::indices(private$.params), "id__cls__grouping")

  first$.init[[4L]]$value = 42L
  expect_identical(marker$value, 42L)
  expect_identical(collection$params$.init[[4L]]$value, 42L)
})

test_that("collection params use R encoding equality and preserve ID spelling", {
  utf8 = enc2utf8("caf\u00e9")
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  skip_if(is.na(latin1))
  Encoding(utf8) = "UTF-8"
  Encoding(latin1) = "latin1"
  skip_if(identical(charToRaw(utf8), charToRaw(latin1)))

  child = ps(x = p_int(0L, 2L, tags = "encoded", init = 1L))
  private = child$.__enclos_env__$private
  data.table::set(private$.params, i = 1L, j = "id", value = utf8)
  data.table::set(private$.tags, i = 1L, j = "id", value = latin1)
  names(private$.values) = latin1

  collection = ParamSetCollection$new(list(outer = child))
  observed = collection$params
  expected = psc_params_reference(collection)
  expect_identical(observed, expected)
  expect_identical(enc2utf8(observed$id), "outer.caf\u00e9")
  expect_identical(observed$.tags[[1L]], "encoded")
  expect_identical(observed$.init[[1L]], 1L)
})

test_that("randomized exact collections match the frozen params reference", {
  had_seed = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed = get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  set.seed(20260714L)

  for (seed in seq_len(20L)) {
    child_count = seed %% 5L
    children = lapply(seq_len(child_count), function(index) {
      psc_params_random_child(
        seed * 100L + index,
        1L + (seed + index) %% 5L
      )
    })
    names(children) = if (child_count) {
      sprintf("set%02d", seq_len(child_count))
    } else {
      character()
    }
    if (child_count && seed %% 4L == 0L) {
      names(children)[[child_count]] = ""
    }

    collection = ParamSetCollection$new(
      children,
      tag_sets = seed %% 2L == 0L,
      tag_params = seed %% 3L == 0L,
      postfix_names = seed %% 4L == 1L
    )
    ids = collection$ids()
    if (length(ids) >= 2L && seed %% 3L == 0L) {
      collection$add_dep(ids[[length(ids)]], ids[[1L]], CondEqual(0L))
    }
    if (child_count && seed %% 5L == 0L) {
      collection = ParamSetCollection$new(list(
        outer = collection,
        other = psc_params_random_child(seed + 10000L, 3L)
      ), tag_sets = TRUE, tag_params = seed %% 2L == 0L)
    }

    expect_identical(
      collection$params,
      psc_params_reference(collection),
      info = sprintf("seed %d", seed)
    )
  }
})
