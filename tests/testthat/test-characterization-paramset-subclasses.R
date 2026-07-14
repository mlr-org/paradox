test_that("ParamSet subclasses can initialize through super", {
  CompatCodomain = R6::R6Class(
    "CompatCodomain",
    inherit = ParamSet,
    public = list(
      initialize = function(params) {
        super$initialize(params)
        if (!length(self$ids(any_tags = c("minimize", "maximize")))) {
          stop("no target")
        }
      }
    ),
    active = list(
      target_ids = function() self$ids(any_tags = c("minimize", "maximize"))
    )
  )

  codomain = CompatCodomain$new(list(
    loss = p_dbl(tags = "minimize"),
    score = p_dbl(tags = "maximize"),
    runtime = p_dbl(lower = 0)
  ))

  expect_identical(class(codomain), c("CompatCodomain", "ParamSet", "R6"))
  expect_identical(codomain$ids(), c("loss", "score", "runtime"))
  expect_identical(codomain$target_ids, c("loss", "score"))
  expect_identical(codomain$is_number, c(loss = TRUE, score = TRUE, runtime = TRUE))
  expect_true(codomain$check(list(loss = 1, score = 2, runtime = 3)))

  restored = unserialize(serialize(codomain, NULL, version = 3L))
  expect_identical(class(restored), class(codomain))
  expect_identical(restored$target_ids, codomain$target_ids)
  expect_true(restored$check(list(loss = 1, score = 2, runtime = 3)))
})

test_that("subclass add_dep observes the historical transient Domain table", {
  observed = NULL
  TransientDependencyProbe = R6::R6Class(
    "CharacterizationTransientDependencyProbe",
    inherit = ParamSet,
    public = list(
      add_dep = function(...) {
        # Copy the names while the callback is running: data.table removes the
        # transient columns by reference after add_dep() returns.
        observed <<- list(
          names = base::c(names(private$.params)),
          class = class(private$.params),
          key = data.table::key(private$.params),
          indices = data.table::indices(private$.params),
          rows = nrow(private$.params),
          dependency_rows = nrow(private$.deps)
        )
        super$add_dep(...)
      }
    )
  )

  param_set = TransientDependencyProbe$new(list(
    parent = p_lgl(),
    child = p_int(0, 2, depends = parent == TRUE)
  ))

  expect_identical(observed, list(
    names = c(
      "id", "cls", "grouping", "cargo", "lower", "upper",
      "tolerance", "levels", "special_vals", "default", "storage_type",
      ".tags", ".trafo", ".requirements", ".init_given", ".init"
    ),
    class = c("data.table", "data.frame"),
    key = NULL,
    indices = NULL,
    rows = 2L,
    dependency_rows = 0L
  ))
  expect_identical(
    names(param_set$.__enclos_env__$private$.params),
    paradox:::domain_names_permanent
  )
  expect_identical(param_set$deps$id, "child")
  expect_identical(param_set$deps$on, "parent")
})

test_that("ParamSet subclasses can install canonical private tables directly", {
  DirectTableSubset = R6::R6Class(
    "DirectTableSubset",
    inherit = ParamSet,
    public = list(
      initialize = function(origin, ids) {
        source = origin$.__enclos_env__$private
        positions = match(ids, source$.params$id)
        if (anyNA(positions)) stop("unknown id")

        private$.params = data.table::copy(source$.params[positions])
        data.table::setindexv(private$.params, c("id", "cls", "grouping"))

        private$.tags = data.table::copy(source$.tags[source$.tags$id %in% ids])
        data.table::setkeyv(private$.tags, "id")
        data.table::setindexv(private$.tags, "tag")

        private$.trafos = data.table::copy(source$.trafos[source$.trafos$id %in% ids])
        data.table::setkeyv(private$.trafos, "id")

        private$.deps = data.table::copy(
          source$.deps[source$.deps$id %in% ids & source$.deps$on %in% ids]
        )
        private$.values = source$.values[
          match(ids, names(source$.values), nomatch = 0L)
        ]
      }
    )
  )

  origin = ps(
    count = p_int(0, 5, tags = c("control", "kept"), init = 2L),
    mode = p_fct(
      c("small", "large"),
      tags = "kept",
      depends = count %in% 1:5
    ),
    scale = p_dbl(0, 1, tags = "dropped", trafo = sqrt)
  )
  subset = DirectTableSubset$new(origin, c("mode", "count"))

  expect_identical(class(subset), c("DirectTableSubset", "ParamSet", "R6"))
  expect_identical(subset$ids(), c("mode", "count"))
  expect_identical(subset$ids(tags = "kept"), c("mode", "count"))
  expect_identical(subset$class, c(mode = "ParamFct", count = "ParamInt"))
  expect_identical(subset$nlevels, c(mode = 2, count = 6))
  expect_identical(subset$values, list(count = 2L))
  expect_identical(subset$deps$id, "mode")
  expect_identical(subset$deps$on, "count")
  expect_true(subset$check(list(mode = "large", count = 2L)))
  expect_false(subset$test(list(mode = "large")))

  cloned = subset$clone(deep = TRUE)
  restored = unserialize(serialize(subset, NULL, version = 3L))
  for (copy in list(cloned, restored)) {
    expect_identical(class(copy), class(subset))
    expect_identical(copy$ids(), subset$ids())
    expect_identical(copy$values, subset$values)
    expect_true(copy$check(list(mode = "small", count = 1L)))
  }
})
