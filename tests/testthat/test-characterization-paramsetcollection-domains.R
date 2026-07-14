context("characterization: ParamSetCollection domains")

psc_domains_reference = function(param_set) {
  ids = param_set$ids()
  setNames(lapply(ids, param_set$get_domain), ids)
}

psc_domains_native = function(param_set,
    private = param_set$.__enclos_env__$private) {
  .Call(paradox:::C_param_set_domains, private, param_set)
}

psc_domains_expect_facade = function(domain, id, class) {
  expect_identical(domain$id, id)
  expect_identical(
    base::class(domain),
    c(class, "Domain", "data.table", "data.frame")
  )
  expect_identical(names(domain), paradox:::domain_names)
  expect_identical(attr(domain, "row.names"), 1L)
  expect_identical(
    names(attributes(domain)),
    c("class", "row.names", ".internal.selfref", "names")
  )
  expect_identical(attr(domain, "repr", exact = TRUE), NULL)
  expect_identical(data.table::key(domain), NULL)
  expect_identical(data.table::indices(domain), NULL)
  expect_identical(data.table:::selfrefok(domain, FALSE), 1L)
}

psc_domains_random_child = function(seed, size) {
  ids = sprintf("p%02d", seq_len(size))
  domains = lapply(seq_len(size), function(index) {
    type = (seed + index) %% 4L
    tags = if (index %% 2L) c("odd", sprintf("seed%d", seed)) else "even"
    switch(as.character(type),
      "0" = p_dbl(-5, 5, tags = tags, trafo = exp),
      "1" = p_int(-5L, 5L, tags = tags, init = as.integer(index - 3L)),
      "2" = p_fct(letters[1:4], tags = tags),
      "3" = p_lgl(tags = tags, init = index %% 2L == 0L)
    )
  })
  names(domains) = ids
  result = ParamSet$new(domains)
  if (size >= 2L) {
    for (index in seq.int(2L, size, by = 3L)) {
      parent = domains[[index - 1L]]
      value = if (inherits(parent, "ParamFct")) {
        parent$levels[[1L]][[1L]]
      } else if (inherits(parent, "ParamLgl")) {
        TRUE
      } else if (inherits(parent, "ParamInt")) {
        0L
      } else {
        0
      }
      result$add_dep(
        ids[[index]],
        ids[[index - 1L]],
        CondEqual(value)
      )
    }
  }
  result
}

test_that("collection domains preserve repeated callback order and count", {
  events = new.env(parent = emptyenv())
  events$seen = character()

  CountingParamSet = R6::R6Class(
    "ParamCollectionDomainsCountingChild",
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
      values = function(value) {
        if (!missing(value)) {
          super$values = value
          return(value)
        }
        events$seen = c(events$seen, sprintf("%s:values", private$.label))
        super$values
      },
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

  left = CountingParamSet$new(
    "left",
    list(x = p_int(0L, 9L), y = p_int(0L, 9L))
  )
  left$add_dep("y", "x", CondEqual(1L))
  right = CountingParamSet$new("right", list(z = p_int(0L, 9L)))
  collection = ParamSetCollection$new(list(left = left, right = right))
  expected_events = rep(c(
    "left:values", "right:values",
    "left:deps", "left:ids", "right:deps"
  ), collection$length)

  events$seen = character()
  observed = collection$domains
  expect_identical(events$seen, expected_events)
  expect_identical(names(observed), c("left.x", "left.y", "right.z"))

  events$seen = character()
  expect_identical(observed, psc_domains_reference(collection))
  expect_identical(events$seen, expected_events)
})

test_that("collection subclasses retain ids, get_domain, and error dispatch", {
  events = new.env(parent = emptyenv())
  events$seen = character()

  CountingCollection = R6::R6Class(
    "ParamCollectionDomainsCountingCollection",
    inherit = ParamSetCollection,
    public = list(
      ids = function(...) {
        events$seen = c(events$seen, "ids")
        super$ids(...)
      },
      get_domain = function(id) {
        events$seen = c(events$seen, sprintf("domain:%s", id))
        super$get_domain(id)
      }
    )
  )
  collection = CountingCollection$new(list(
    child = ps(x = p_dbl(0, 1), y = p_lgl())
  ))

  events$seen = character()
  observed = collection$domains
  expect_identical(events$seen, c("ids", "domain:child.x", "domain:child.y"))
  expect_identical(names(observed), c("child.x", "child.y"))

  ErrorChild = R6::R6Class(
    "ParamCollectionDomainsErrorChild",
    inherit = ParamSet,
    active = list(
      values = function(value) {
        if (!missing(value)) {
          super$values = value
          return(value)
        }
        private$.calls = private$.calls + 1L
        events$seen = c(events$seen, sprintf("values:%d", private$.calls))
        if (private$.calls == 2L) stop("values failed on row 2")
        super$values
      },
      deps = function(value) {
        if (!missing(value)) {
          super$deps = value
          return(value)
        }
        events$seen = c(events$seen, "deps")
        super$deps
      }
    ),
    private = list(.calls = 0L)
  )
  failing = ParamSetCollection$new(list(
    child = ErrorChild$new(list(x = p_dbl(0, 1), y = p_dbl(0, 1)))
  ))
  events$seen = character()
  expect_error(failing$domains, "values failed on row 2", fixed = TRUE)
  expect_identical(events$seen, c("values:1", "deps", "values:2"))

  expect_error(
    {
      collection$domains = list()
    },
    "domains is read-only.",
    fixed = TRUE
  )
})

test_that("collection domains preserve all dependency rows and rich state", {
  left = ps(
    parent = p_int(0L, 3L, tags = "left", init = 2L),
    target = p_fct(c("a", "b"), tags = "target", trafo = toupper),
    payload = p_uty()
  )
  left$add_dep("target", "parent", CondEqual(2L))
  right = ps(flag = p_lgl(tags = "right", init = TRUE))
  collection = ParamSetCollection$new(list(left = left, right = right))
  collection$add_dep("left.target", "right.flag", CondEqual(TRUE))
  left_private = left$.__enclos_env__$private
  left_private$.values = structure(
    list(2L, NULL),
    names = c("parent", "payload")
  )

  observed = collection$domains
  expect_identical(observed, psc_domains_reference(collection))
  expect_identical(
    names(observed),
    c("left.parent", "left.target", "left.payload", "right.flag")
  )
  psc_domains_expect_facade(observed$left.parent, "left.parent", "ParamInt")
  psc_domains_expect_facade(observed$left.target, "left.target", "ParamFct")
  psc_domains_expect_facade(observed$left.payload, "left.payload", "ParamUty")
  psc_domains_expect_facade(observed$right.flag, "right.flag", "ParamLgl")

  requirements = observed$left.target$.requirements[[1L]]
  expect_length(requirements, 2L)
  expect_identical(
    vapply(requirements, `[[`, character(1L), "on"),
    c("left.parent", "right.flag")
  )
  expect_identical(names(requirements[[1L]]), c("on", "cond"))
  expect_identical(requirements[[1L]]$cond, CondEqual(2L))
  expect_identical(requirements[[2L]]$cond, CondEqual(TRUE))

  # The params update join deliberately keeps only the final dependency row.
  expect_identical(collection$params$.requirements[[2L]][[1L]], "right.flag")
  expect_identical(collection$params$.requirements[[2L]][[2L]], CondEqual(TRUE))
  expect_null(names(collection$params$.requirements[[2L]]))

  expect_true(observed$left.parent$.init_given)
  expect_identical(observed$left.parent$.init[[1L]], 2L)
  expect_false(observed$left.target$.init_given)
  expect_null(observed$left.target$.init[[1L]])
  expect_true(observed$left.payload$.init_given)
  expect_null(observed$left.payload$.init[[1L]])
  expect_true(observed$right.flag$.init_given)
  expect_identical(observed$right.flag$.init[[1L]], TRUE)
  expect_identical(observed$left.target$.trafo[[1L]], toupper)
})

test_that("collection domains retain affixes, nesting, and shared-child DAGs", {
  child = ps(
    parent = p_int(0L, 3L),
    target = p_fct(c("a", "b"))
  )
  child$add_dep("target", "parent", CondEqual(2L))

  prefix = ParamSetCollection$new(list(left = child))
  expect_identical(names(prefix$domains), c("left.parent", "left.target"))
  expect_identical(
    prefix$domains$left.target$.requirements[[1L]][[1L]]$on,
    "left.parent"
  )

  postfix = ParamSetCollection$new(list(left = child), postfix_names = TRUE)
  expect_identical(names(postfix$domains), c("parent.left", "target.left"))
  expect_identical(
    postfix$domains$target.left$.requirements[[1L]][[1L]]$on,
    "parent.left"
  )

  nested = ParamSetCollection$new(list(outer = postfix))
  expect_identical(
    names(nested$domains),
    c("outer.parent.left", "outer.target.left")
  )
  expect_identical(
    nested$domains$outer.target.left$.requirements[[1L]][[1L]]$on,
    "outer.parent.left"
  )
  expect_identical(nested$domains, psc_domains_reference(nested))

  shared = ParamSetCollection$new(list(first = child, second = child))
  expect_identical(names(shared$domains), c(
    "first.parent", "first.target", "second.parent", "second.target"
  ))
  expect_identical(
    shared$domains$first.target$.requirements[[1L]][[1L]]$on,
    "first.parent"
  )
  expect_identical(
    shared$domains$second.target$.requirements[[1L]][[1L]]$on,
    "second.parent"
  )
  expect_identical(shared$domains, psc_domains_reference(shared))
})

test_that("collection Domains combine snapshot metadata with live child state", {
  child = ps(
    x = p_dbl(0, 1, tags = "before", trafo = exp),
    parent = p_int(0L, 3L, init = 1L)
  )
  collection = ParamSetCollection$new(list(child = child))
  child_private = child$.__enclos_env__$private

  child$tags = list(x = "after", parent = "after-parent")
  data.table::set(child_private$.trafos, 1L, "trafo", list(log))
  data.table::set(child_private$.params, 1L, "lower", -100)
  child$values = list(parent = 3L)
  child$add_dep("x", "parent", CondEqual(3L))

  observed = collection$domains
  expect_identical(observed, psc_domains_reference(collection))
  expect_identical(observed$child.x$lower, 0)
  expect_identical(observed$child.x$.tags[[1L]], "before")
  expect_identical(observed$child.x$.trafo[[1L]], exp)
  expect_identical(observed$child.parent$.init[[1L]], 3L)
  expect_identical(
    observed$child.x$.requirements[[1L]][[1L]]$on,
    "child.parent"
  )

  collection$tags = list(child.x = "outer", child.parent = character())
  expect_identical(collection$domains$child.x$.tags[[1L]], "outer")
  expect_identical(collection$domains$child.x$.trafo[[1L]], exp)
})

test_that("earlier Domains remain rooted across later extension callbacks", {
  collection = NULL
  state = new.env(parent = emptyenv())
  state$row = 0L
  state$events = character()

  RebindingChild = R6::R6Class(
    "ParamCollectionDomainsRebindingChild",
    inherit = ParamSet,
    active = list(
      values = function(value) {
        if (!missing(value)) {
          super$values = value
          return(value)
        }
        state$row = state$row + 1L
        state$events = c(state$events, sprintf("values:%d", state$row))
        private = collection$.__enclos_env__$private
        if (state$row == 1L) {
          params = data.table::copy(private$.params)
          data.table::set(params, which(params$id == "y"), "lower", -2)
          private$.params = params
        }
        setNames(list(state$row, state$row), c("x", "y"))
      },
      deps = function(value) {
        if (!missing(value)) {
          super$deps = value
          return(value)
        }
        state$events = c(state$events, sprintf("deps:%d", state$row))
        result = super$deps
        private = collection$.__enclos_env__$private
        id = c("x", "y")[[state$row]]

        tags = data.table::copy(private$.tags)
        data.table::set(tags, which(tags$id == id), "tag", paste0("d", state$row))
        private$.tags = tags

        replacement = function(value) value + 100
        trafos = data.table::copy(private$.trafos)
        data.table::set(
          trafos,
          which(trafos$id == id),
          "trafo",
          list(replacement)
        )
        private$.trafos = trafos
        result
      }
    )
  )

  child = RebindingChild$new(list(
    x = p_dbl(0, 1, tags = "x0", trafo = identity),
    y = p_dbl(0, 1, tags = "y0", trafo = identity)
  ))
  collection = ParamSetCollection$new(list(child))
  observed = collection$domains

  expect_identical(
    state$events,
    c("values:1", "deps:1", "values:2", "deps:2")
  )
  expect_identical(observed$x$lower, 0)
  expect_identical(observed$y$lower, -2)
  expect_identical(observed$x$.tags[[1L]], "d1")
  expect_identical(observed$y$.tags[[1L]], "d2")
  expect_identical(typeof(observed$x$.trafo[[1L]]), "closure")
  expect_identical(typeof(observed$y$.trafo[[1L]]), "closure")
  expect_identical(observed$x$.trafo[[1L]](1), 101)
  expect_identical(observed$y$.trafo[[1L]](1), 101)
  expect_identical(observed$x$.init[[1L]], 1L)
  expect_identical(observed$y$.init[[1L]], 2L)
})

test_that("collection Domains own output shells and share opaque leaves", {
  marker = new.env(parent = emptyenv())
  child = ps(
    x = p_dbl(
      0,
      1,
      tags = c("first", "second"),
      trafo = exp
    ),
    parent = p_int(0L, 2L),
    special = p_dbl(0, 1, special_vals = list(-1)),
    payload = p_uty()
  )
  child$add_dep("x", "parent", CondEqual(1L))
  child_private = child$.__enclos_env__$private
  child_private$.values = structure(list(marker), names = "payload")
  collection = ParamSetCollection$new(list(child = child))
  private = collection$.__enclos_env__$private

  first = collection$domains
  second = collection$domains
  expect_identical(first, second)
  expect_false(identical(
    data.table::address(first),
    data.table::address(second)
  ))
  expect_identical(names(first), private$.params$id)
  expect_false(identical(
    data.table::address(names(first)),
    data.table::address(private$.params$id)
  ))
  expect_false(identical(
    data.table::address(names(first)),
    data.table::address(names(second))
  ))
  for (id in names(first)) {
    expect_false(identical(
      data.table::address(first[[id]]),
      data.table::address(second[[id]])
    ), info = id)
    for (column in names(first[[id]])) {
      expect_false(identical(
        data.table::address(first[[id]][[column]]),
        data.table::address(second[[id]][[column]])
      ), info = sprintf("%s/%s", id, column))
    }
  }
  expect_false(identical(
    data.table::address(first$child.x$.requirements[[1L]][[1L]]$cond),
    data.table::address(child_private$.deps$cond[[1L]])
  ))
  expect_false(identical(
    data.table::address(first$child.x$.requirements[[1L]][[1L]]$cond),
    data.table::address(second$child.x$.requirements[[1L]][[1L]]$cond)
  ))
  expect_identical(first$child.x$.trafo[[1L]], exp)
  expect_identical(first$child.payload$.init[[1L]], marker)
  expect_identical(data.table:::selfrefok(first$child.x, FALSE), 1L)

  data.table::set(first$child.x, 1L, "lower", -10)
  data.table::set(first$child.x, j = "added", value = 1L)
  first$child.x$.tags[[1L]][[1L]] = "changed"
  first$child.special$special_vals[[1L]][[1L]] = -2
  first$child.x$.requirements[[1L]][[1L]]$on = "changed"
  names(first)[[1L]] = "changed"

  expect_identical(collection$domains, second)
  expect_identical(child_private$.deps$cond[[1L]], CondEqual(1L))
  first$child.payload$.init[[1L]]$value = 42L
  expect_identical(marker$value, 42L)
})

test_that("empty, custom, and encoded collection Domains retain fallback shape", {
  empty = ParamSetCollection$new(list())$domains
  expect_identical(empty, setNames(list(), character()))
  expect_identical(names(empty), character())

  make_custom = function() {
    paradox:::Domain(
      cls = "ParamCollectionDomainsExtension",
      grouping = "ParamCollectionDomainsExtension",
      storage_type = "numeric"
    )
  }
  custom_child = ParamSet$new(list(
    custom = make_custom(),
    control = p_int(0L, 3L)
  ))
  custom = ParamSetCollection$new(list(extension = custom_child))
  expect_identical(custom$domains, psc_domains_reference(custom))
  expect_s3_class(custom$domains$extension.custom, "ParamCollectionDomainsExtension")

  utf8 = enc2utf8("caf\u00e9")
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  skip_if(is.na(latin1))
  Encoding(utf8) = "UTF-8"
  Encoding(latin1) = "latin1"

  child = ps(x = p_dbl(0, 1, tags = "encoded", trafo = exp))
  encoded = ParamSetCollection$new(list(child))
  child_private = child$.__enclos_env__$private
  encoded_private = encoded$.__enclos_env__$private
  data.table::set(child_private$.params, 1L, "id", utf8)
  data.table::setindexv(child_private$.params, c("id", "cls", "grouping"))
  child_private$.values = structure(list(0.5), names = latin1)
  data.table::set(encoded_private$.params, 1L, "id", utf8)
  data.table::setindexv(encoded_private$.params, c("id", "cls", "grouping"))
  data.table::set(encoded_private$.tags, 1L, "id", latin1)
  data.table::setkeyv(encoded_private$.tags, "id")
  data.table::set(encoded_private$.trafos, 1L, "id", latin1)
  data.table::setkeyv(encoded_private$.trafos, "id")

  observed = encoded$domains
  expected = psc_domains_reference(encoded)
  expect_identical(observed, expected)
  expect_identical(Encoding(names(observed)), Encoding(names(expected)))
  expect_identical(enc2utf8(names(observed)), "caf\u00e9")
  expect_identical(enc2utf8(observed[[1L]]$id), "caf\u00e9")
  expect_identical(observed[[1L]]$.tags[[1L]], "encoded")
  expect_identical(observed[[1L]]$.trafo[[1L]], exp)
  expect_identical(observed[[1L]]$.init[[1L]], 0.5)
})

test_that("collection batching rejects noncanonical storage without forcing it", {
  make_fixture = function() {
    child = ps(x = p_dbl(0, 1), y = p_int(0L, 2L))
    list(
      child = child,
      collection = ParamSetCollection$new(list(child = child))
    )
  }

  fixture = make_fixture()
  expect_null(psc_domains_native(
    fixture$collection,
    fixture$child$.__enclos_env__$private
  ))

  outer_mutations = list(
    .params = list(),
    .tags = list(),
    .trafos = list(),
    .deps = list(),
    .values = unname(list()),
    .sets = unname(list(ps(x = p_dbl(0, 1)))),
    .postfix = NA
  )
  for (field in names(outer_mutations)) {
    fixture = make_fixture()
    fixture$collection$.__enclos_env__$private[[field]] =
      outer_mutations[[field]]
    expect_null(psc_domains_native(fixture$collection), info = field)
  }

  child_mutations = list(
    .params = list(),
    .tags = list(),
    .trafos = list(),
    .deps = list(),
    .values = unname(list())
  )
  for (field in names(child_mutations)) {
    fixture = make_fixture()
    fixture$child$.__enclos_env__$private[[field]] =
      child_mutations[[field]]
    expect_null(psc_domains_native(fixture$collection), info = field)
  }

  counter = new.env(parent = emptyenv())
  counter$active = 0L
  counter$delayed = 0L
  forge_child = function(kind) {
    source = ps(x = p_dbl(0, 1))
    source_private = source$.__enclos_env__$private
    private = new.env(parent = emptyenv())
    for (field in setdiff(ls(source_private, all.names = TRUE), ".values")) {
      assign(
        field,
        get(field, envir = source_private, inherits = FALSE),
        private
      )
    }
    if (identical(kind, "active")) {
      makeActiveBinding(".values", function(value) {
        counter$active = counter$active + 1L
        list(x = 0.5)
      }, private)
    } else {
      delayedAssign(".values", {
        counter$delayed = counter$delayed + 1L
        list(x = 0.5)
      }, assign.env = private)
    }
    enclosure = new.env(parent = emptyenv())
    enclosure$private = private
    child = new.env(parent = emptyenv())
    child$.__enclos_env__ = enclosure
    class(child) = c("ParamSet", "R6")
    child
  }
  make_outer = function(child) {
    outer = ParamSetCollection$new(list(child = ps(x = p_dbl(0, 1))))
    outer$.__enclos_env__$private$.sets[[1L]] = child
    outer
  }

  expect_null(psc_domains_native(make_outer(forge_child("active"))))
  expect_identical(counter$active, 0L)
  expect_null(psc_domains_native(make_outer(forge_child("delayed"))))
  expect_identical(counter$delayed, 0L)
})

test_that("randomized exact collections match repeated get_domain", {
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
      psc_domains_random_child(seed * 100L + index, 1L + (seed + index) %% 5L)
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
    if (child_count && seed %% 5L == 0L) {
      collection = ParamSetCollection$new(list(outer = collection))
    }

    expect_identical(
      collection$domains,
      psc_domains_reference(collection),
      info = sprintf("seed %d", seed)
    )
  }
})
