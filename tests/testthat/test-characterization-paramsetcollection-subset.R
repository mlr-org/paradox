context("contract: ParamSetCollection subset and flatten")

psc_subset_characterization_fixture = function(postfix = FALSE,
    nested = FALSE) {
  left = ps(
    enabled = p_lgl(tags = "switch", init = TRUE),
    amount = p_int(0, 10, tags = c("numeric", "shared"), init = 2L)
  )
  left$add_dep("amount", "enabled", CondEqual(TRUE))
  right = ps(
    score = p_dbl(0, 1, tags = c("numeric", "shared"), trafo = exp),
    mode = p_fct(c("small", "large"), tags = "choice", init = "large")
  )
  right$values$score = 0.5
  inner = ParamSetCollection$new(
    list(left = left, right = right),
    tag_sets = TRUE,
    tag_params = TRUE,
    postfix_names = postfix
  )
  left_enabled = if (postfix) "enabled.left" else "left.enabled"
  right_mode = if (postfix) "mode.right" else "right.mode"
  inner$add_dep(right_mode, left_enabled, CondEqual(TRUE))

  if (!nested) return(inner)
  ParamSetCollection$new(list(
    layer = inner,
    spare = ps(flag = p_lgl(tags = "spare", init = FALSE))
  ))
}

psc_subset_characterization_projection = function(param_set) {
  dependencies = param_set$deps
  list(
    class = class(param_set),
    ids = param_set$ids(),
    values = param_set$values,
    tags = param_set$tags,
    classes = param_set$class,
    storage = param_set$storage_type,
    dependencies = list(
      id = dependencies$id,
      on = dependencies$on,
      class = lapply(dependencies$cond, class),
      rhs = lapply(dependencies$cond, function(condition) condition$rhs)
    ),
    params_class = class(param_set$params),
    params_names = names(param_set$params),
    params_key = data.table::key(param_set$params),
    params_indices = data.table::indices(param_set$params)
  )
}

test_that("collection subsets preserve selection order and facades", {
  prefix = psc_subset_characterization_fixture()
  selected = prefix$subset(
    c("right.mode", "left.enabled", "right.score"),
    allow_dangling_dependencies = TRUE
  )
  expect_identical(
    selected$ids(),
    c("right.mode", "left.enabled", "right.score")
  )
  expect_identical(
    selected$values,
    list(
      right.mode = "large",
      left.enabled = TRUE,
      right.score = 0.5
    )
  )
  expect_identical(selected$deps$id, "right.mode")
  expect_identical(selected$deps$on, "left.enabled")
  expect_identical(
    lapply(selected$deps$cond, function(condition) condition$rhs),
    list(TRUE)
  )
  expect_error(
    prefix$subset(c("right.mode", "right.mode")),
    "must not contain duplicates",
    fixed = TRUE
  )
  unique_selected = prefix$subset(
    c("right.mode", "left.enabled", "right.score"),
    allow_dangling_dependencies = TRUE
  )
  expect_identical(unique_selected$tags[["right.mode"]], c(
    "choice", "set_right", "param_mode"
  ))
  expect_identical(class(selected), c("ParamSet", "R6"))
  expect_identical(class(selected$params), c("data.table", "data.frame"))
  expect_identical(data.table::key(selected$params), NULL)
  expect_identical(data.table::indices(selected$params), NULL)
  expect_identical(data.table:::selfrefok(selected$params, FALSE), 1L)

  postfix = psc_subset_characterization_fixture(postfix = TRUE)
  postfix_subset = postfix$subset(
    c("mode.right", "enabled.left", "score.right"),
    allow_dangling_dependencies = TRUE
  )
  expect_identical(
    postfix_subset$ids(),
    c("mode.right", "enabled.left", "score.right")
  )
  expect_identical(postfix_subset$deps$id, "mode.right")
  expect_identical(postfix_subset$deps$on, "enabled.left")

  nested = psc_subset_characterization_fixture(nested = TRUE)
  nested_subset = nested$subset(
    c("layer.right.mode", "layer.left.enabled", "spare.flag"),
    allow_dangling_dependencies = TRUE
  )
  expect_identical(
    nested_subset$ids(),
    c("layer.right.mode", "layer.left.enabled", "spare.flag")
  )
  expect_identical(nested_subset$deps$id, "layer.right.mode")
  expect_identical(nested_subset$deps$on, "layer.left.enabled")

  empty = nested$subset(character())
  expect_identical(empty$ids(), character())
  expect_identical(empty$values, structure(list(), names = character()))
  expect_identical(nrow(empty$deps), 0L)
})

test_that("collection subset and flatten are detached snapshots", {
  child = ps(
    x = p_dbl(0, 10, tags = "old", init = 1),
    enabled = p_lgl(init = TRUE)
  )
  child$add_dep("x", "enabled", CondEqual(TRUE))
  child$extra_trafo = function(x) {
    x$x = x$x + 1
    x
  }
  child$constraint = function(x) TRUE
  collection = psc(component = child)

  subset = collection$subset(
    c("component.x", "component.enabled"),
    allow_dangling_dependencies = TRUE
  )
  flat = collection$flatten()

  child$values = list(x = 3, enabled = TRUE)
  child$deps = child$deps[0L]
  child$tags = list(x = "new", enabled = character())
  child$extra_trafo = function(x) {
    x$x = x$x + 100
    x
  }
  child$constraint = function(x) FALSE

  for (snapshot in list(subset, flat)) {
    expect_identical(
      snapshot$values,
      list(component.x = 1, component.enabled = TRUE)
    )
    expect_identical(snapshot$deps$id, "component.x")
    expect_identical(snapshot$tags[["component.x"]], "old")
    expect_identical(
      snapshot$trafo(list(component.x = 1))$component.x,
      2
    )
    expect_true(snapshot$test_constraint(list(component.x = 4)))
  }

  params = subset$params
  data.table::set(params, 1L, "lower", -100)
  expect_identical(collection$params$lower[[1L]], 0)
  subset$tags = list(
    component.x = "mutated",
    component.enabled = character()
  )
  expect_false("mutated" %in% collection$tags[["component.x"]])
  subset$values$component.x = 9
  expect_identical(collection$values$component.x, 3)
})

test_that("collection subset forces arguments in public order", {
  collection = psc(
    left = ps(parent = p_lgl(init = TRUE)),
    right = ps(child = p_int(0, 1, init = 1L))
  )
  collection$add_dep("right.child", "left.parent", CondEqual(TRUE))

  events = character()
  observed = collection$subset(
    {
      events = c(events, "ids")
      c("left.parent", "right.child")
    },
    {
      events = c(events, "allow")
      TRUE
    },
    {
      events = c(events, "keep")
      FALSE
    }
  )
  expect_identical(observed$ids(), c("left.parent", "right.child"))
  expect_identical(events, c("ids", "allow", "keep"))

  expect_error(
    collection$subset(
      "right.child",
      FALSE,
      stop("keep_constraint was forced")
    ),
    "keep_constraint was forced",
    fixed = TRUE
  )
})

test_that("collection flatten preserves the complete visible state", {
  for (collection in list(
    psc_subset_characterization_fixture(),
    psc_subset_characterization_fixture(postfix = TRUE),
    psc_subset_characterization_fixture(nested = TRUE)
  )) {
    flattened = collection$flatten()
    expect_identical(class(flattened), c("ParamSet", "R6"))
    expect_identical(flattened$ids(), collection$ids())
    expect_identical(flattened$values, collection$values)
    expect_identical(flattened$tags, collection$tags)
    expect_identical(flattened$class, collection$class)
    expect_identical(flattened$storage_type, collection$storage_type)
    expect_identical(flattened$deps$id, collection$deps$id)
    expect_identical(flattened$deps$on, collection$deps$on)
    expect_identical(
      lapply(flattened$deps$cond, function(condition) condition$rhs),
      lapply(collection$deps$cond, function(condition) condition$rhs)
    )
    expect_identical(
      psc_subset_characterization_projection(flattened),
      psc_subset_characterization_projection(
        collection$subset(
          collection$ids(),
          allow_dangling_dependencies = TRUE
        )
      )
    )
  }
})
