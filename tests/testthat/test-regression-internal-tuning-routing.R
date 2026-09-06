internal_tuning_search_space = function(id, upper = 10L) {
  do.call(ps, setNames(list(p_int(0L, upper)), id))
}

internal_tuning_nested_collection = function(
    outer_postfix,
    inner_postfix
) {
  leaf = ps(
    tune = p_int(
      tags = "internal_tuning",
      in_tune_fn = function(domain, param_vals) {
        domain$upper + as.integer(param_vals[["gate"]])
      },
      aggr = function(x) x[[1L]],
      disable_in_tune = list(gate = FALSE)
    ),
    gate = p_lgl()
  )
  leaf$values = list(gate = TRUE)
  inner = ParamSetCollection$new(
    list(inner = leaf),
    postfix_names = inner_postfix
  )
  outer = ParamSetCollection$new(
    list(outer = inner),
    postfix_names = outer_postfix
  )
  ids = outer$ids()
  list(
    collection = outer,
    leaf = leaf,
    tune = ids[grepl("tune", ids, fixed = TRUE)],
    gate = ids[grepl("gate", ids, fixed = TRUE)]
  )
}

test_that("internal-tuning routes compose exact prefix and postfix edges", {
  modes = expand.grid(
    outer_postfix = c(FALSE, TRUE),
    inner_postfix = c(FALSE, TRUE)
  )

  for (mode in seq_len(nrow(modes))) {
    fixture = internal_tuning_nested_collection(
      modes$outer_postfix[[mode]],
      modes$inner_postfix[[mode]]
    )
    search_space = internal_tuning_search_space(fixture$tune)

    expect_identical(
      fixture$collection$convert_internal_search_space(search_space),
      setNames(list(11L), fixture$tune)
    )

    flattened = fixture$collection$flatten()
    flattened$set_values(.values = setNames(list(TRUE), fixture$gate))
    expect_identical(
      flattened$convert_internal_search_space(search_space),
      setNames(list(11L), fixture$tune)
    )
    flattened$disable_internal_tuning(fixture$tune)
    expect_identical(flattened$values[[fixture$gate]], FALSE)

    fixture$collection$disable_internal_tuning(fixture$tune)
    expect_identical(fixture$collection$values[[fixture$gate]], FALSE)
    expect_identical(fixture$leaf$values$gate, FALSE)
  }
})

test_that("Shadow conversion follows a Collection to its ultimate owner", {
  leaf = ps(
    tune = p_int(
      tags = "internal_tuning",
      in_tune_fn = function(domain, param_vals) {
        domain$upper + as.integer(param_vals[["gate"]])
      },
      aggr = function(x) x[[1L]],
      disable_in_tune = list(gate = FALSE)
    ),
    gate = p_lgl(),
    hidden = p_int()
  )
  leaf$values = list(gate = TRUE, hidden = 1L)
  origin = ParamSetCollection$new(list(owner = leaf))
  shadow = ParamSetShadow$new(
    origin,
    c("owner.gate", "owner.hidden")
  )
  search_space = internal_tuning_search_space("owner.tune")

  expect_identical(
    shadow$convert_internal_search_space(search_space),
    list(owner.tune = 11L)
  )
  shadow$disable_internal_tuning("owner.tune")
  expect_identical(origin$values$owner.gate, FALSE)
  expect_identical(leaf$values$gate, FALSE)
})

test_that("Shadow-hidden values remain callback context and flatten snapshots them", {
  leaf = ps(
    tune = p_int(
      tags = "internal_tuning",
      in_tune_fn = function(domain, param_vals) {
        domain$upper + as.integer(param_vals[["gate"]])
      },
      aggr = function(x) x[[1L]],
      disable_in_tune = list()
    ),
    gate = p_lgl()
  )
  leaf$values = list(gate = TRUE)
  shadow = ParamSetShadow$new(leaf, "gate")
  search_space = internal_tuning_search_space("tune")

  expect_identical(
    shadow$convert_internal_search_space(search_space),
    list(tune = 11L)
  )
  flattened = shadow$flatten()
  expect_identical(
    flattened$convert_internal_search_space(search_space),
    list(tune = 11L)
  )

  leaf$values = list(gate = FALSE)
  expect_identical(
    shadow$convert_internal_search_space(search_space),
    list(tune = 10L)
  )
  expect_identical(
    flattened$convert_internal_search_space(search_space),
    list(tune = 11L)
  )
})

test_that("a Collection containing a Shadow uses hidden owner context", {
  leaf = ps(
    tune = p_int(
      tags = "internal_tuning",
      in_tune_fn = function(domain, param_vals) {
        domain$upper + as.integer(param_vals[["gate"]])
      },
      aggr = function(x) x[[1L]],
      disable_in_tune = list()
    ),
    gate = p_lgl()
  )
  leaf$values = list(gate = TRUE)
  shadow = ParamSetShadow$new(leaf, "gate")
  collection = ParamSetCollection$new(list(outer = shadow))
  search_space = internal_tuning_search_space("outer.tune")

  expect_identical(
    collection$convert_internal_search_space(search_space),
    list(outer.tune = 11L)
  )
  flattened = collection$flatten()
  expect_identical(
    flattened$convert_internal_search_space(search_space),
    list(outer.tune = 11L)
  )
})

test_that("legacy prefix-only internal-tuning crates retain v1 behavior", {
  legacy = paradox:::param_set_collection_in_tune_fn_factory(
    function(domain, param_vals) names(param_vals),
    "owner",
    c("owner.tune", "owner.gate")
  )
  expect_identical(
    legacy(
      NULL,
      list(owner.gate = TRUE, unrelated = 1L, owner.tune = 2L)
    ),
    c("gate", "tune")
  )
  migrated = paradox:::.upgrade_paradox_strip_legacy_in_tune_fn(legacy)
  expect_identical(
    ls(environment(migrated), all.names = TRUE),
    c("in_tune_fn", "prefix", "prefixed_set_ids")
  )
  expect_identical(
    migrated(
      NULL,
      list(owner.gate = TRUE, unrelated = 1L, owner.tune = 2L)
    ),
    c("gate", "tune")
  )
})

test_that("restored prerelease Shadow base targets route to Shadow behavior", {
  leaf = ps(
    tune = p_int(
      tags = "internal_tuning",
      in_tune_fn = function(domain, param_vals) {
        domain$upper + as.integer(param_vals[["gate"]])
      },
      aggr = function(x) x[[1L]],
      disable_in_tune = list(gate = FALSE)
    ),
    gate = p_lgl()
  )
  leaf$values = list(gate = TRUE)
  shadow = unserialize(serialize(
    ParamSetShadow$new(leaf, "gate"),
    NULL
  ))
  enclosure = shadow$.__enclos_env__
  namespace = asNamespace("paradox")
  base_convert = get(
    ".__paradox2_ParamSet__convert_internal_search_space",
    envir = namespace,
    inherits = FALSE
  )
  base_disable = get(
    ".__paradox2_ParamSet__disable_internal_tuning",
    envir = namespace,
    inherits = FALSE
  )

  expect_identical(
    base_convert(
      self = shadow,
      private = enclosure$private,
      super = enclosure$super,
      search_space = internal_tuning_search_space("tune")
    ),
    list(tune = 11L)
  )
  expect_identical(
    base_disable(
      self = shadow,
      private = enclosure$private,
      super = enclosure$super,
      ids = "tune"
    ),
    shadow
  )
  expect_identical(shadow$origin$values$gate, FALSE)

  flatten_leaf = ps(
    tune = p_int(
      tags = "internal_tuning",
      in_tune_fn = function(domain, param_vals) {
        domain$upper + as.integer(param_vals[["gate"]])
      },
      aggr = function(x) x[[1L]],
      disable_in_tune = list()
    ),
    gate = p_lgl()
  )
  flatten_leaf$values = list(gate = TRUE)
  flatten_shadow = unserialize(serialize(
    ParamSetShadow$new(flatten_leaf, "gate"),
    NULL
  ))
  flatten_enclosure = flatten_shadow$.__enclos_env__
  base_flatten = get(
    ".__paradox2_ParamSet__flatten",
    envir = namespace,
    inherits = FALSE
  )
  flattened = base_flatten(
    self = flatten_shadow,
    private = flatten_enclosure$private,
    super = flatten_enclosure$super
  )
  expect_s3_class(flattened, "ParamSet")
  expect_false(inherits(flattened, "ParamSetShadow"))
  expect_identical(
    flattened$convert_internal_search_space(
      internal_tuning_search_space("tune")
    ),
    list(tune = 11L)
  )
})

test_that("in_tune_fn receives detached owner values", {
  # `param_vals` crosses the package boundary into a user callback. Before the
  # snapshot detached it, this exact callback corrupted the child's canonical
  # `.values` store in place and permanently bricked the set.
  hostile = function(domain, param_vals) {
    if (length(param_vals)) {
      data.table::setattr(
        param_vals,
        "names",
        rep("HACKED", length(param_vals))
      )
      data.table::setattr(param_vals[[1L]], "rogue", TRUE)
    }
    domain$upper
  }
  child = ps(
    a = p_dbl(
      1, 2,
      tags = "internal_tuning",
      in_tune_fn = hostile,
      disable_in_tune = list(),
      aggr = function(x) x[[1L]]
    ),
    b = p_dbl(0, 1)
  )
  child$values = list(b = 0.5)
  collection = ParamSetCollection$new(list(sub = child))

  converted = collection$convert_internal_search_space(
    ps(sub.a = p_dbl(1, 2))
  )
  expect_identical(converted, list(sub.a = 2))
  expect_identical(child$values, list(b = 0.5))
  expect_identical(collection$values, list(sub.b = 0.5))
})
