context("contract: additive ParamSet subclasses")

test_that("additive subclasses can initialize through super", {
  CompatCodomain = R6::R6Class(
    "CompatCodomain",
    inherit = ParamSet,
    public = list(
      initialize = function(params, label = "objectives") {
        super$initialize(params)
        private$.label = label
        if (!length(self$ids(any_tags = c("minimize", "maximize")))) {
          stop("no target")
        }
      },
      summary_label = function() {
        sprintf("%s:%s", private$.label, paste(self$target_ids, collapse = ","))
      }
    ),
    active = list(
      target_ids = function() self$ids(any_tags = c("minimize", "maximize"))
    ),
    private = list(.label = NULL)
  )

  codomain = CompatCodomain$new(list(
    loss = p_dbl(tags = "minimize"),
    score = p_dbl(tags = "maximize"),
    runtime = p_dbl(lower = 0)
  ))

  expect_identical(class(codomain), c("CompatCodomain", "ParamSet", "R6"))
  expect_identical(codomain$ids(), c("loss", "score", "runtime"))
  expect_identical(codomain$target_ids, c("loss", "score"))
  expect_identical(codomain$summary_label(), "objectives:loss,score")
  expect_identical(
    codomain$is_number,
    c(loss = TRUE, score = TRUE, runtime = TRUE)
  )
  expect_true(codomain$check(list(loss = 1, score = 2, runtime = 3)))

  codomain$values = list(loss = 1, score = 2, runtime = 3)
  codomain$add_dep("runtime", "loss", CondEqual(1))
  expect_identical(codomain$values, list(loss = 1, score = 2, runtime = 3))
  expect_identical(codomain$deps$id, "runtime")
})

test_that("additive subclass state survives clone and serialization", {
  AdditiveParamSet = R6::R6Class(
    "AdditiveParamSet",
    inherit = ParamSet,
    public = list(
      initialize = function(params, note) {
        super$initialize(params)
        private$.note = note
      },
      note = function() private$.note,
      bounded_ids = function() names(self$is_bounded)[self$is_bounded]
    ),
    private = list(.note = NULL)
  )

  original = AdditiveParamSet$new(
    list(x = p_int(0L, 4L, init = 2L), payload = p_uty()),
    note = "kept"
  )
  original$values$payload = new.env(parent = emptyenv())

  shallow = original$clone(deep = FALSE)
  deep = original$clone(deep = TRUE)
  restored = unserialize(serialize(original, NULL, version = 3L))

  for (copy in list(shallow, deep, restored)) {
    expect_identical(class(copy), class(original))
    expect_identical(copy$note(), "kept")
    expect_identical(copy$ids(), c("x", "payload"))
    expect_identical(copy$bounded_ids(), "x")
    expect_true(copy$check(list(x = 3L, payload = copy$values$payload)))
  }

  deep$values$x = 4L
  expect_identical(original$values$x, 2L)
  restored$values$x = 1L
  expect_identical(original$values$x, 2L)
})
