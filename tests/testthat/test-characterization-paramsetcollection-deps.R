context("characterization: ParamSetCollection deps")

psc_deps_reference = function(param_set) {
  private = param_set$.__enclos_env__$private
  child_deps = Map(function(child, owner) {
    result = if (identical(
        class(child),
        c("ParamSetCollection", "ParamSet", "R6")
      )) {
      psc_deps_reference(child)
    } else {
      child$deps
    }
    if (owner != "" && nrow(result)) {
      old_ids = child$ids()
      new_ids = private$.add_name_prefix(owner, old_ids)
      result$id = mlr3misc::map_values(result$id, old_ids, new_ids)
      result$on = mlr3misc::map_values(result$on, old_ids, new_ids)
    }
    result
  }, private$.sets, names(private$.sets))
  data.table::rbindlist(c(child_deps, list(private$.deps)), use.names = TRUE)
}

psc_deps_child = function(ids, dependencies = list()) {
  domains = setNames(
    lapply(ids, function(id) p_int(0L, 9L)),
    ids
  )
  result = ParamSet$new(domains)
  for (dependency in dependencies) {
    result$add_dep(
      dependency[[1L]],
      dependency[[2L]],
      CondEqual(dependency[[3L]])
    )
  }
  result
}

test_that("collection deps preserve child callback order and count", {
  events = new.env(parent = emptyenv())
  events$seen = character()

  CountingParamSet = R6::R6Class(
    "ParamCollectionDepsCountingChild",
    inherit = ParamSet,
    public = list(
      initialize = function(label, params) {
        private$.label = label
        super$initialize(params)
      },
      ids = function(...) {
        events$seen = c(events$seen, sprintf("%s:ids", private$.label))
        super$ids(...)
      }
    ),
    active = list(
      deps = function(value) {
        if (!missing(value)) {
          super$deps = value
          return(value)
        }
        events$seen = c(events$seen, sprintf("%s:deps", private$.label))
        super$deps
      }
    ),
    private = list(.label = NULL)
  )

  named = CountingParamSet$new(
    "named",
    list(a = p_int(0L, 9L), b = p_int(0L, 9L))
  )
  named$add_dep("b", "a", CondEqual(1L))
  empty = CountingParamSet$new("empty", list(x = p_int(0L, 9L)))
  unnamed = CountingParamSet$new(
    "unnamed",
    list(a = p_int(0L, 9L), b = p_int(0L, 9L))
  )
  unnamed$add_dep("b", "a", CondEqual(2L))
  collection = ParamSetCollection$new(setNames(
    list(named, empty, unnamed),
    c("named", "empty", "")
  ))

  events$seen = character()
  observed = collection$deps
  expect_identical(
    events$seen,
    c("named:deps", "named:ids", "empty:deps", "unnamed:deps")
  )
  expect_identical(observed$id, c("named.b", "b"))
  expect_identical(observed$on, c("named.a", "a"))

  events$seen = character()
  expect_identical(observed, psc_deps_reference(collection))
  expect_identical(
    events$seen,
    c("named:deps", "named:ids", "empty:deps", "unnamed:deps")
  )

  expect_error(
    {
      collection$deps = data.table::data.table()
    },
    "deps is read-only in ParamSetCollection.",
    fixed = TRUE
  )
})

test_that("collection deps retain prefix, postfix, nesting, and row order", {
  left = psc_deps_child(c("a", "b", "c"), list(
    list("b", "a", 1L),
    list("c", "b", 2L)
  ))
  right = psc_deps_child(
    c("x", "y"),
    list(list("y", "x", 3L))
  )

  prefix = ParamSetCollection$new(list(left = left, right = right))
  prefix$add_dep("right.y", "left.a", CondEqual(4L))
  prefix_observed = prefix$deps
  expect_identical(prefix_observed, psc_deps_reference(prefix))
  expect_identical(
    prefix_observed$id,
    c("left.b", "left.c", "right.y", "right.y")
  )
  expect_identical(
    prefix_observed$on,
    c("left.a", "left.b", "right.x", "left.a")
  )

  postfix = ParamSetCollection$new(
    list(left = left, right = right),
    postfix_names = TRUE
  )
  postfix$add_dep("y.right", "a.left", CondEqual(4L))
  postfix_observed = postfix$deps
  expect_identical(postfix_observed, psc_deps_reference(postfix))
  expect_identical(
    postfix_observed$id,
    c("b.left", "c.left", "y.right", "y.right")
  )

  inner = ParamSetCollection$new(list(inner = left))
  inner$add_dep("inner.c", "inner.a", CondEqual(5L))
  nested = ParamSetCollection$new(list(outer = inner, sibling = right))
  nested$add_dep("sibling.y", "outer.inner.a", CondEqual(6L))
  nested_observed = nested$deps
  expect_identical(nested_observed, psc_deps_reference(nested))
  expect_identical(nested_observed$id, c(
    "outer.inner.b",
    "outer.inner.c",
    "outer.inner.c",
    "sibling.y",
    "sibling.y"
  ))

  postfix_inner = ParamSetCollection$new(
    list(inner = left),
    postfix_names = TRUE
  )
  mixed = ParamSetCollection$new(list(outer = postfix_inner))
  expect_identical(mixed$deps$id, c("outer.b.inner", "outer.c.inner"))

  shared = ParamSetCollection$new(list(first = left, second = left))
  expect_identical(shared$deps$id, c(
    "first.b", "first.c", "second.b", "second.c"
  ))
  expect_identical(shared$deps, psc_deps_reference(shared))
})

test_that("collection deps translate every nesting layer independently", {
  leaf = psc_deps_child(c("x", "y"))
  private = leaf$.__enclos_env__$private
  private$.deps = data.table::data.table(
    id = c("inner.x", "inner.x", "foreign"),
    on = c("inner.y", "inner.y", "foreign-on"),
    cond = list(CondEqual(1L), CondEqual(1L), CondEqual(2L))
  )

  inner = ParamSetCollection$new(list(inner = leaf))
  outer = ParamSetCollection$new(list(outer = inner))
  observed = outer$deps
  expect_identical(observed, psc_deps_reference(outer))
  expect_identical(
    observed$id,
    c("outer.inner.x", "outer.inner.x", "foreign")
  )
  expect_identical(
    observed$on,
    c("outer.inner.y", "outer.inner.y", "foreign-on")
  )
  expect_identical(observed$cond[[1L]], observed$cond[[2L]])
})

test_that("empty collection deps retain their complete table facade", {
  empty = ParamSetCollection$new(list())
  observed = empty$deps

  expect_identical(observed, psc_deps_reference(empty))
  expect_identical(dim(observed), c(0L, 3L))
  expect_identical(names(observed), c("id", "on", "cond"))
  expect_identical(
    vapply(observed, typeof, character(1L)),
    c(id = "character", on = "character", cond = "list")
  )
  expect_identical(class(observed), c("data.table", "data.frame"))
  expect_identical(attr(observed, "row.names"), integer())
  expect_identical(data.table::key(observed), NULL)
  expect_identical(data.table::indices(observed), NULL)
  expect_identical(data.table:::selfrefok(observed, FALSE), 1L)

  children = ParamSetCollection$new(setNames(
    replicate(16L, ParamSet$new(), simplify = FALSE),
    sprintf("empty%02d", seq_len(16L))
  ))
  expect_identical(children$deps, psc_deps_reference(children))
  expect_identical(dim(children$deps), c(0L, 3L))
})

test_that("collection deps own ordinary shells and share opaque leaves", {
  marker = new.env(parent = emptyenv())
  shared = list(value = 1L)
  condition = structure(list(
    first = shared,
    second = shared,
    marker = marker
  ), class = "Condition")
  child = psc_deps_child(c("a", "b"))
  private = child$.__enclos_env__$private
  private$.deps = data.table::data.table(
    id = "b",
    on = "a",
    cond = list(condition)
  )
  collection = ParamSetCollection$new(list(child = child))

  first = collection$deps
  second = collection$deps
  expect_identical(first, second)
  expect_false(identical(
    data.table::address(first),
    data.table::address(second)
  ))
  for (column in names(first)) {
    expect_false(identical(
      data.table::address(first[[column]]),
      data.table::address(second[[column]])
    ), info = column)
  }
  expect_false(identical(
    data.table::address(first$cond[[1L]]),
    data.table::address(private$.deps$cond[[1L]])
  ))
  expect_false(identical(
    data.table::address(first$cond[[1L]]$first),
    data.table::address(first$cond[[1L]]$second)
  ))
  expect_identical(first$cond[[1L]]$marker, marker)

  first$id[[1L]] = "changed"
  first$cond[[1L]]$first$value = 9L
  expect_identical(private$.deps$id, "b")
  expect_identical(private$.deps$cond[[1L]]$first$value, 1L)
  expect_identical(first$cond[[1L]]$second$value, 1L)
  expect_identical(collection$deps, second)
})

test_that("custom Domains and collection subclasses retain public behavior", {
  make_custom = function() {
    paradox:::Domain(
      cls = "ParamCollectionDepsExtension",
      grouping = "ParamCollectionDepsExtension",
      storage_type = "numeric"
    )
  }
  child = ParamSet$new(list(
    custom = make_custom(),
    control = p_int(0L, 9L)
  ))
  private = child$.__enclos_env__$private
  private$.deps = data.table::data.table(
    id = "custom",
    on = "control",
    cond = list(CondEqual(1L))
  )
  collection = ParamSetCollection$new(list(extension = child))
  expect_identical(collection$deps, psc_deps_reference(collection))
  expect_identical(collection$deps$id, "extension.custom")

  events = new.env(parent = emptyenv())
  events$count = 0L
  CountingCollection = R6::R6Class(
    "ParamCollectionDepsCountingCollection",
    inherit = ParamSetCollection,
    active = list(
      deps = function(value) {
        if (!missing(value)) stop("deps is read-only")
        events$count = events$count + 1L
        super$deps
      }
    )
  )
  subclass = CountingCollection$new(list(child = child))
  expect_identical(subclass$deps$id, "child.custom")
  expect_identical(events$count, 1L)
})

test_that("collection deps preserve fallback string encoding", {
  utf8_id = enc2utf8("caf\u00e9")
  latin1_id = iconv(utf8_id, from = "UTF-8", to = "latin1")
  utf8_owner = enc2utf8("gr\u00f6\u00dfe")
  latin1_owner = iconv(utf8_owner, from = "UTF-8", to = "latin1")
  skip_if(anyNA(c(latin1_id, latin1_owner)))
  Encoding(latin1_id) = "latin1"
  Encoding(latin1_owner) = "latin1"

  child = psc_deps_child(
    c("source", "target"),
    list(list("target", "source", 1L))
  )
  collection = ParamSetCollection$new(list(owner = child))
  child_private = child$.__enclos_env__$private
  data.table::set(child_private$.params, 1L, "id", latin1_id)
  data.table::setindexv(child_private$.params, c("id", "cls", "grouping"))
  data.table::set(child_private$.deps, 1L, "on", latin1_id)
  collection_private = collection$.__enclos_env__$private
  names(collection_private$.sets) = latin1_owner

  observed = collection$deps
  expected = psc_deps_reference(collection)
  expect_identical(observed, expected)
  expect_identical(Encoding(observed$id), Encoding(expected$id))
  expect_identical(Encoding(observed$on), Encoding(expected$on))
  expect_identical(enc2utf8(observed$id), "gr\u00f6\u00dfe.target")
  expect_identical(enc2utf8(observed$on), "gr\u00f6\u00dfe.caf\u00e9")
})

test_that("randomized exact collections match the frozen deps reference", {
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

  for (seed in seq_len(30L)) {
    child_count = seed %% 5L
    children = lapply(seq_len(child_count), function(child_index) {
      size = 1L + (seed + child_index) %% 5L
      ids = sprintf("p%02d", seq_len(size))
      result = psc_deps_child(ids)
      if (size >= 2L) {
        row_count = (seed + child_index) %% 4L
        for (row in seq_len(row_count)) {
          pair = sample(ids, 2L)
          private = result$.__enclos_env__$private
          private$.deps = rbind(
            private$.deps,
            data.table::data.table(
              id = pair[[1L]],
              on = pair[[2L]],
              cond = list(CondEqual(sample.int(9L, 1L)))
            )
          )
        }
      }
      result
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
      postfix_names = seed %% 2L == 0L
    )
    ids = collection$ids()
    if (length(ids) >= 2L && seed %% 3L == 0L) {
      collection$add_dep(
        ids[[length(ids)]],
        ids[[1L]],
        CondEqual(0L)
      )
    }
    if (child_count && seed %% 5L == 0L) {
      collection = ParamSetCollection$new(
        list(outer = collection),
        postfix_names = seed %% 10L == 0L
      )
    }

    expect_identical(
      collection$deps,
      psc_deps_reference(collection),
      info = sprintf("seed %d", seed)
    )
  }
})
