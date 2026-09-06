# The native detachment planner reduces a nested collection graph to tiny
# callback carriers and a leaf-to-visible ID translation. Authoritative graph
# check/assignment code filters activity before invoking these schema-free
# carriers; the carriers must not grow a second activity engine. Returned
# callbacks retain an ordinary immutable plan and enter the native evaluator
# immediately; there is no second R implementation of collection callback
# semantics.
param_set_collection_constraint_closure = function(plan) {
  # Assignment after forcing stores the plan as a direct frame value. These
  # carriers are retained in user-serialized ParamSets, and a formal promise
  # cell would manufacture R 4.5's fail-closed recursive-migration promise
  # boundary inside a package-owned callback (see `.make_p_fct_trafo`).
  plan = force(plan)
  .paradox_strip_srcref(function(x) {
    .Call(C_param_set_collection_detached_constraint, plan, x)
  })
}

param_set_collection_constraint_factory = function(
    translation,
    constraint_indices,
    constraint_sets) {
  if (!length(constraint_indices)) return(NULL)
  param_set_collection_constraint_closure(list(
    translation = translation,
    indices = constraint_indices,
    sets = constraint_sets
  ))
}

param_set_collection_extra_trafo_closure = function(plan) {
  # Same direct-value rule as the constraint closure above.
  plan = force(plan)
  .paradox_strip_srcref(function(x) {
    .Call(C_param_set_collection_detached_extra_trafo, plan, x)
  })
}

param_set_collection_extra_trafo_factory = function(
    translation,
    trafo_indices,
    trafo_sets) {
  if (!length(trafo_indices)) return(NULL)
  for (index in seq_along(trafo_sets)) {
    carrier = trafo_sets[[index]]
    unit = trafo_indices[[index]]
    ids = unique(translation$original_id[
      translation$owner_ps_index == unit
    ])
    token = .Call(
      C_param_set_collection_owner_subset_state,
      carrier$extra_trafo,
      carrier$.core,
      ids
    )
    carrier["param_set"] = list(
      if (is.null(token)) NULL else ParamSet$new(token)
    )
    carrier$.core = NULL
    trafo_sets[[index]] = carrier
  }
  param_set_collection_extra_trafo_closure(list(
    translation = translation,
    indices = trafo_indices,
    sets = trafo_sets
  ))
}

param_set_collection_in_tune_fn_factory = function(
    in_tune_fn,
    prefix,
    prefixed_set_ids
) {
  force(in_tune_fn)
  force(prefix)
  force(prefixed_set_ids)
  crate(.paradox_strip_srcref(function(domain, param_vals) {
    param_vals = param_vals[names(param_vals) %in% prefixed_set_ids]
    names(param_vals) = gsub(
      sprintf("^\\Q%s.\\E", prefix),
      "",
      names(param_vals)
    )
    in_tune_fn(domain, param_vals)
  }), in_tune_fn, prefix, prefixed_set_ids)
}

# New Paradox-2 flattening never infers a leaf ID from its printed collection
# ID.  The native detachment planner has already authenticated the complete
# graph and returns the exact visible-to-leaf translation; retain that tiny
# translation in the detached callback.  The legacy prefix-only factory above
# deliberately remains unchanged because the v1 upgrader authenticates and
# rebuilds its historical three-binding crate.
param_set_collection_in_tune_fn_exact_factory = function(
    in_tune_fn,
    visible_ids,
    original_ids,
    hidden_values = named_list()
) {
  force(in_tune_fn)
  force(visible_ids)
  force(original_ids)
  force(hidden_values)
  crate(.paradox_strip_srcref(function(domain, param_vals) {
    translated = match(names(param_vals), visible_ids, nomatch = 0L)
    present = translated != 0L
    param_vals = param_vals[present]
    names(param_vals) = original_ids[translated[present]]
    if (length(hidden_values)) {
      param_vals = c(hidden_values, param_vals)
    }
    in_tune_fn(domain, param_vals)
  }), in_tune_fn, visible_ids, original_ids, hidden_values)
}

# Capture root cargo, exact Collection/Shadow routes, and every ultimate BASE
# value generation in one admitted native graph snapshot. The receipt is a
# cold-operation barrier for flatten/disable; conversion deliberately keeps
# using the immutable entry snapshot if a callback later mutates live state.
param_set_internal_tuning_plan = function(
    param_set,
    ids,
    include_root_values = FALSE
) {
  .Call(
    C_param_set_internal_tuning_snapshot,
    get_private(param_set),
    param_set,
    ids,
    include_root_values
  )
}

param_set_internal_tuning_convert = function(
    param_set,
    private,
    search_space
) {
  assert_class(search_space, "ParamSet")
  domains = search_space$domains
  plan = param_set_internal_tuning_plan(param_set, names(domains))
  converters = lapply(seq_along(domains), function(row) {
    converter = plan$cargo[[row]]$in_tune_fn
    if (!is.function(converter)) {
      stopf("No converter exists for parameter '%s'", plan$id[[row]])
    }
    converter
  })
  names(converters) = names(domains)

  imap(domains, function(token, .id) {
    row = match(.id, plan$id)
    converters[[.id]](
      token,
      plan$owner_values[[plan$owner_ps_index[[row]]]]
    )
  })
}

param_set_internal_tuning_append_updates = function(current, updates) {
  if (!length(updates)) return(current)
  for (index in seq_along(updates)) {
    position = match(names(updates)[[index]], names(current))
    if (is.na(position)) {
      position = length(current) + 1L
      current[position] = updates[index]
      names(current)[[position]] = names(updates)[[index]]
    } else {
      current[position] = updates[index]
    }
  }
  current
}

param_set_internal_tuning_disable = function(
    param_set,
    private,
    ids,
    receipts = list(),
    validate = NULL
) {
  plan = param_set_internal_tuning_plan(
    param_set,
    NULL,
    include_root_values = TRUE
  )
  receipts[[length(receipts) + 1L]] = plan$receipt
  if (is.null(validate)) {
    validate = plan$assert_values
  }
  assert_subset(ids, plan$id[plan$internal_tuning])
  if (!length(ids)) return(invisible(param_set))

  root_updates = named_list()
  owner_updates = lapply(plan$owners, function(owner) named_list())
  has_hidden_control = FALSE
  for (id in ids) {
    row = match(id, plan$id)
    updates = plan$cargo[[row]]$disable_in_tune
    if (!length(updates)) next

    owner = plan$owner_ps_index[[row]]
    unknown = match(names(updates), plan$owner_ids[[owner]])
    if (anyNA(unknown)) {
      stopf(
        paste(
          "Cannot disable internal tuning for parameter '%s':",
          "control parameter '%s' is not available in its owner"
        ),
        id,
        names(updates)[which(is.na(unknown))[[1L]]]
      )
    }
    # Selected IDs are processed in caller order. If two internal-tuning
    # parameters intentionally target the same control, the later selected ID
    # wins deterministically instead of leaking a duplicate-name error from
    # the structural value boundary.
    owner_updates[[owner]] = param_set_internal_tuning_append_updates(
      owner_updates[[owner]],
      updates
    )

    route_rows = which(plan$route_index == plan$route_index[[row]])
    controls = match(names(updates), plan$original_id[route_rows])
    if (anyNA(controls)) {
      has_hidden_control = TRUE
    } else {
      names(updates) = plan$id[route_rows][controls]
      root_updates = param_set_internal_tuning_append_updates(
        root_updates,
        updates
      )
    }
  }

  if (has_hidden_control && inherits(param_set, "ParamSetShadow")) {
    # A direct Shadow's origin is the complete semantic schema. Re-enter this
    # planner there, but retain the outer receipt through the origin's final
    # atomic commit so neither view can move in between.
    origin = param_set$origin
    param_set_internal_tuning_disable(
      origin,
      get_private(origin),
      ids,
      receipts,
      validate
    )
    return(invisible(param_set))
  }

  if (has_hidden_control) {
    if (isTRUE(plan$has_constraint)) {
      stop(
        paste(
          "Cannot atomically disable internal tuning through a nested",
          "ParamSetShadow when the composed graph has a constraint;",
          "disable it through the Shadow origin instead"
        ),
        call. = FALSE
      )
    }
    affected = which(lengths(owner_updates) != 0L)
    complete = lapply(affected, function(owner) {
      .Call(
        C_param_set_values_merge,
        list(),
        owner_updates[[owner]],
        plan$owner_values[[owner]],
        TRUE
      )
    })
    invisible(.Call(
      C_param_set_internal_tuning_store_owners,
      plan$owners[affected],
      complete,
      validate,
      receipts
    ))
    return(invisible(param_set))
  }

  complete = .Call(
    C_param_set_values_merge,
    list(),
    root_updates,
    plan$root_values,
    TRUE
  )
  invisible(.Call(
    C_param_set_internal_tuning_store,
    private,
    param_set,
    complete,
    validate,
    receipts
  ))
  invisible(param_set)
}

param_set_shadow_disable_internal_tuning = function(param_set, ids) {
  param_set_internal_tuning_disable(
    param_set,
    get_private(param_set),
    ids
  )
}

param_set_internal_tuning_rows = function(params) {
  which(vapply(params$cargo, function(cargo) {
    # Only plain list cargo can name internal tuning: on a classed cargo the
    # `$` and `length()` reads below would dispatch user methods inside this
    # orchestration path, before any plan receipt exists.
    is.list(cargo) && !is.object(cargo) &&
      (!is.null(cargo$in_tune_fn) || length(cargo$disable_in_tune))
  }, logical(1L)))
}

# Rebind only cargo rows whose internal-tuning metadata names a detached graph
# context.  Unrelated cargo leaves remain pointer-identical.  Hidden Shadow
# values are snapshotted exactly as a detached flatten operation requires;
# visible values supplied later by the flattened ParamSet take precedence.
param_set_internal_tuning_rebind_flat = function(flatps, plan) {
  flat_private = get_private(flatps)
  flat_state = param_set_core_state(flat_private, flatps)
  flat_params = param_set_table_rows(
    flat_state$.params,
    seq_len(nrow(flat_state$.params))
  )
  rows = param_set_internal_tuning_rows(flat_params)
  if (!length(rows)) return(flatps)
  if (is.null(plan)) {
    stop(
      "Internal-tuning metadata changed while the ParamSet was flattened",
      call. = FALSE
    )
  }

  for (row in rows) {
    plan_row = match(flat_params$id[[row]], plan$id)
    route = plan$route_index[[plan_row]]
    route_rows = which(plan$route_index == route)
    visible_ids = plan$id[route_rows]
    original_ids = plan$original_id[route_rows]
    route_owners = unique(plan$owner_ps_index[route_rows])
    if (length(route_owners) != 1L) {
      stop(
        "Internal error: internal-tuning route has multiple owners",
        call. = FALSE
      )
    }

    values = plan$owner_values[[route_owners]]
    hidden_values = values[!names(values) %in% original_ids]
    cargo = flat_params$cargo[[row]]
    if (is.function(cargo$in_tune_fn) &&
        (!identical(visible_ids, original_ids) ||
          length(hidden_values))) {
      cargo$in_tune_fn = param_set_collection_in_tune_fn_exact_factory(
        cargo$in_tune_fn,
        visible_ids,
        original_ids,
        hidden_values
      )
    }

    if (length(cargo$disable_in_tune)) {
      controls = match(names(cargo$disable_in_tune), original_ids)
      if (anyNA(controls)) {
        stop(
          "Cannot flatten internal-tuning metadata that disables a hidden parameter",
          call. = FALSE
        )
      }
      names(cargo$disable_in_tune) = visible_ids[controls]
    }
    flat_params$cargo[[row]] = cargo
  }

  param_set_core_replace(flat_private, params = flat_params)
  flatps
}

param_set_graph_flatten = function(param_set, private) {
  state = param_set_core_state(private, param_set)
  rows = param_set_internal_tuning_rows(state$.params)
  if (!length(rows)) {
    # The no-callback path still selects every ID inside the native subset
    # transaction. A preliminary R-side ID vector could become stale if a
    # Collection/Shadow derived schema moved before the subset began.
    bundle = .Call(C_param_set_flatten_state, private, param_set)
    flatps = param_set_from_subset_bundle(bundle)
    return(param_set_internal_tuning_rebind_flat(flatps, NULL))
  }

  # NULL is the package-private "select every current root ID" request. The
  # native plan chooses those IDs, routes, cargo, hidden owner values, and the
  # complete graph receipt from one generation.
  plan = param_set_internal_tuning_plan(param_set, NULL)
  flatps = param_set$subset(
    plan$id,
    allow_dangling_dependencies = TRUE
  )
  .Call(C_param_set_internal_tuning_receipt, plan$receipt)
  param_set_internal_tuning_rebind_flat(flatps, plan)
}

param_set_shadow_flatten = function(param_set) {
  param_set_graph_flatten(param_set, get_private(param_set))
}

#' @title ParamSetCollection
#'
#' @description
#' A collection of multiple [`ParamSet`] objects.
#' * The collection is basically a light-weight wrapper / container around references to multiple sets.
#' * In order to ensure unique param names, every param in the collection is referred to with
#'   "<set_id>.<param_id>", where `<set_id>` is the name of the entry a given [`ParamSet`] in the named list given during construction.
#'   Parameters from [`ParamSet`] with empty (i.e. `""`) `set_id` are referenced
#'   directly. Multiple [`ParamSet`]s with `set_id` `""` can be combined, but their parameter names
#'   may not overlap to avoid name clashes.
#' * Value reads reflect the current contained [`ParamSet`] references; the
#'   collection does not maintain a second value state. Assignments are planned
#'   over the complete collection/shadow graph and committed atomically to the
#'   ultimate base sets. Direct child mutations remain visible through the
#'   collection. `$values` is the translated raw store and may include dormant
#'   values. The default `$get_values()` view filters activity across the
#'   complete collection graph. A collection-level cross-set dependency is
#'   therefore applied when reading the collection, not when reading either
#'   child directly.
#' * Checked assignment validates every supplied value but may store it while
#'   dependency-inactive. If constraints are present, activity is computed
#'   over the complete translated resulting configuration. Each child
#'   constraint receives only its active child-scope entries, with collection
#'   prefixes or postfixes removed. The graph transaction remains atomic if a
#'   callback fails or mutates a planned target.
#' * Dependencies: It is possible to currently handle dependencies
#'      * regarding parameters inside of the same set - in this case simply
#'        add the dependency to the set, best before adding the set to the collection
#'      * across sets, where a param from one set depends on the state
#'        of a param from another set - in this case add call `add_dep` on the collection.
#'
#'   If you call `deps` on the collection, you are returned a complete table of dependencies, from sets and across sets.
#'
#' @include ParamSet.R
#' @export
ParamSetCollection = R6Class("ParamSetCollection", inherit = ParamSet,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param sets (named `list()` of [ParamSet])\cr
    #'   The outer set list, names, and list metadata must be ordinary
    #'   non-S4; a top-level ALTREP plain list is materialized exactly once.
    #'   ParamSet objects are not cloned.
    #'   Names are used as "set_id" for the naming scheme of delegated parameters.
    #'   A name is therefore part of every parameter ID it contributes and must
    #'   keep that ID inside the ID grammar: a prefix must itself match
    #'   `^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$`, while a postfix (see `postfix_names`)
    #'   only has to use ASCII letters, digits, `.`, and `_`.
    #'   The empty name is also allowed and delegates the set's parameters
    #'   without affixing them.
    #' @param tag_sets (`logical(1)`)\cr
    #'   Whether to add tags of the form `"set_<set_id>"` to each parameter originating from a given `ParamSet` given with name `<set_id>`.
    #' @param tag_params (`logical(1)`)\cr
    #'   Whether to add tags of the form `"param_<param_id>"` to each parameter with original ID `<param_id>`.
    #' @param postfix_names (`logical(1)`)\cr
    #'   Whether to use the names inside `sets` as postfixes, rather than prefixes.
    initialize = function(sets, tag_sets = FALSE, tag_params = FALSE, postfix_names = FALSE) {
      if (typeof(sets) == "list" && is.null(names(sets))) {
        # `names<-` has to duplicate the referenced argument, and R returns a
        # wrapper ALTREP for a list of 64 or more elements. The native
        # boundary admits only ordinary containers, so take the ordinary copy
        # explicitly instead of relying on the duplicate's representation.
        # `$search_space()` reaches this through `ps_union()` with one part per
        # TuneToken, so without it a set with 64 tuned parameters cannot
        # produce a search space at all.
        sets = sets[seq_along(sets)]
        names(sets) = rep("", length(sets))
      }

      native = .Call(
        C_param_set_collection_construct,
        sets,
        tag_sets,
        tag_params,
        postfix_names
      )
      private$.core = param_set_core_new(
        2L,
        params = native$params,
        tags = native$tags,
        deps = new_empty_deps(),
        trafos = native$trafos,
        sets = native$sets,
        translation = native$translation,
        postfix = postfix_names,
        edges = native$edges
      )
      invisible(sets)
    },

    #' @description
    #' Adds a [`ParamSet`] to this collection.
    #'
    #' @param p ([ParamSet]).
    #' @param n (`character(1)`)\cr
    #'   Name to use. Must keep the IDs it creates inside the ID grammar --
    #'   `^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$` when this collection prefixes, ASCII
    #'   letters, digits, `.`, and `_` when it postfixes -- or be `""` to
    #'   delegate `p`'s parameters without affixing them. Default `""`.
    #' @param tag_sets (`logical(1)`)\cr
    #'   Whether to add tags of the form `"set_<n>"` to the newly added parameters.
    #' @param tag_params (`logical(1)`)\cr
    #'   Whether to add tags of the form `"param_<param_id>"` to each parameter with original ID `<param_id>`.
    add = function(p, n = "", tag_sets = FALSE, tag_params = FALSE) {
      invisible(.Call(
        C_param_set_collection_add,
        private,
        self,
        p,
        n,
        tag_sets,
        tag_params
      ))
    },

    #' @description
    #' Create a new `ParamSet` restricted to the passed IDs.
    #' @param ids (`character()`).
    #' @param allow_dangling_dependencies (`logical(1)`)\cr
    #'   Whether to allow subsets that cut across parameter dependencies.
    #'   Dependencies that point to dropped parameters are kept (but will be "dangling", i.e. their `"on"` will not be present).
    #' @param keep_constraint (`logical(1)`)\cr
    #'   Whether to keep the `$constraint` function.
    #' @param keep_trafo (`logical(1)`)\cr
    #'   Whether to keep per-parameter transformations and the `$extra_trafo`
    #'   function. All three subset control flags must be unclassed,
    #'   attribute-free, non-missing logical scalars.
    #' @return `ParamSet`.
    subset = function(ids, allow_dangling_dependencies = FALSE,
      keep_constraint = TRUE, keep_trafo = TRUE) {
      super$subset(
        ids,
        allow_dangling_dependencies = allow_dangling_dependencies,
        keep_constraint = keep_constraint,
        keep_trafo = keep_trafo
      )
    },

    #' @description
    #'
    #' Set the parameter values so that internal tuning for the selected parameters is disabled.
    #'
    #' @param ids (`character()`)\cr
    #'   The ids of the parameters for which to disable internal tuning.
    #' @return `Self`
    disable_internal_tuning = function(ids) {
      param_set_internal_tuning_disable(self, private, ids)
    },

    #' @description
    #' Convert all parameters from the search space to parameter values using the transformation given by
    #' `in_tune_fn`.
    #' @param search_space ([`ParamSet`])\cr
    #'   The internal search space.
    #' @return (named `list()`)
    convert_internal_search_space = function(search_space) {
      param_set_internal_tuning_convert(self, private, search_space)
    },

    #' @description
    #' Create a `ParamSet` from this `ParamSetCollection`.
    flatten = function() {
      param_set_graph_flatten(self, private)
    }
  ),

  active = list(
    #' @template field_params
    params = function(rhs) {
      if (!missing(rhs)) {
        if (params_data_table_temporary_reassignment()) {
          return(rhs)
        }
        stop("params is read-only.")
      }

      .Call(C_param_set_collection_params, private, self)
    },

    #' @template field_deps
    deps = function(v) {
      if (!missing(v)) {
        stop("deps is read-only in ParamSetCollection.")
      }
      .Call(C_param_set_collection_deps, private, self)
    },

    #' @template field_extra_trafo
    extra_trafo = function(f) {
      if (!missing(f)) stop("extra_trafo is read-only in ParamSetCollection.")
      if (!.Call(
          C_param_set_collection_has_callback,
          private,
          self,
          0L
        )) return(NULL)

      # The private method reselects the capsule graph on every invocation, so
      # child callback replacement remains live without exposing R semantics.
      private$.extra_trafo_explicit
    },

    #' @template field_constraint
    constraint = function(f) {
      if (!missing(f)) stop("constraint is read-only in ParamSetCollection.")
      if (!.Call(
          C_param_set_collection_has_callback,
          private,
          self,
          1L
        )) return(NULL)
      private$.constraint_explicit
    },

    #' @field sets (named `list()`)\cr
    #' Read-only `list` of of [`ParamSet`]s contained in this `ParamSetCollection`.
    #' The list shell and names are detached; its elements are the exact
    #' contained [`ParamSet`] objects.
    sets = function(v) {
      sets = private$.state()$.sets
      detached = sets[seq_along(sets)]
      if (!missing(v) && !identical(v, detached)) stop("sets is read-only")
      detached
    }
  ),

  private = list(
    .add_name_prefix = function(owner, id) {
      if (private$.state()$.postfix) sprintf("%s.%s", id, owner) else sprintf("%s.%s", owner, id)
    },
    .get_values = function() {
      .Call(C_param_set_collection_values, private, self)
    },
    .store_values = function(xs) {
      invisible(.Call(C_param_set_store_values, private, self, xs))
    },
    .extra_trafo_explicit = function(x) {
      .Call(C_param_set_collection_extra_trafo, private, self, x)
    },
    .constraint_explicit = function(x) {
      .Call(C_param_set_collection_constraint, private, self, x)
    },
    deep_clone = function(name, value) {
      switch(name,
        .core = param_set_core_deep_clone(self, value),
        value
      )
    }
  )
)
