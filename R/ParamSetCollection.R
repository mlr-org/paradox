# The native detachment planner reduces a nested collection graph to tiny
# callback carriers and a leaf-to-visible ID translation. Returned callbacks
# retain an ordinary immutable plan and enter the native evaluator immediately;
# there is no second R implementation of collection callback semantics.
param_set_collection_constraint_closure = function(plan) {
  force(plan)
  function(x) {
    .Call(C_param_set_collection_detached_constraint, plan, x)
  }
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
  force(plan)
  function(x) {
    .Call(C_param_set_collection_detached_extra_trafo, plan, x)
  }
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
      carrier$.source,
      ids
    )
    carrier["param_set"] = list(
      if (is.null(token)) NULL else ParamSet$new(token)
    )
    carrier$.source = NULL
    trafo_sets[[index]] = carrier
  }
  param_set_collection_extra_trafo_closure(list(
    translation = translation,
    indices = trafo_indices,
    sets = trafo_sets
  ))
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
#'   collection.
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
    #'   non-ALTREP/non-S4. ParamSet objects are not cloned.
    #'   Names are used as "set_id" for the naming scheme of delegated parameters.
    #' @param tag_sets (`logical(1)`)\cr
    #'   Whether to add tags of the form `"set_<set_id>"` to each parameter originating from a given `ParamSet` given with name `<set_id>`.
    #' @param tag_params (`logical(1)`)\cr
    #'   Whether to add tags of the form `"param_<param_id>"` to each parameter with original ID `<param_id>`.
    #' @param postfix_names (`logical(1)`)\cr
    #'   Whether to use the names inside `sets` as postfixes, rather than prefixes.
    initialize = function(sets, tag_sets = FALSE, tag_params = FALSE, postfix_names = FALSE) {
      if (typeof(sets) == "list" && is.null(names(sets))) {
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
        sets = sets,
        translation = native$translation,
        postfix = postfix_names
      )
      invisible(sets)
    },

    #' @description
    #' Adds a [`ParamSet`] to this collection.
    #'
    #' @param p ([ParamSet]).
    #' @param n (`character(1)`)\cr
    #'   Name to use. Default `""`.
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
      # need to take care of extra_trafo and constraint.
      result = super$subset(
        ids,
        allow_dangling_dependencies = allow_dangling_dependencies,
        keep_constraint = keep_constraint,
        keep_trafo = keep_trafo
      )

      # Callback-free subsets need no graph detachment plan. A retained
      # callback is snapshotted once by the capsule graph planner below.
      detach_constraint = !is.null(result$constraint)
      detach_trafo = !is.null(result$extra_trafo)
      if (!detach_constraint && !detach_trafo) {
        return(result)
      }

      detached = .Call(
        C_param_set_collection_detach_plan,
        private,
        self,
        ids
      )
      if (detach_constraint) {
        result$constraint = param_set_collection_constraint_factory(
          detached$translation,
          detached$constraint_indices,
          detached$constraint_sets
        )
      }
      if (detach_trafo) {
        result$extra_trafo = param_set_collection_extra_trafo_factory(
          detached$translation,
          detached$trafo_indices,
          detached$trafo_sets
        )
      }
      result
    },

    #' @description
    #'
    #' Set the parameter values so that internal tuning for the selected parameters is disabled.
    #'
    #' @param ids (`character()`)\cr
    #'   The ids of the parameters for which to disable internal tuning.
    #' @return `Self`
    disable_internal_tuning = function(ids) {
      assert_subset(ids, self$ids(tags = "internal_tuning"))

      full_prefix = function(param_set, id_, prefix = "") {
        nested_state = param_set_core_state(get_private(param_set))
        row = match(id_, nested_state$.translation$id)
        info = nested_state$.translation[row, , drop = FALSE]
        subset = nested_state$.sets[[info$owner_ps_index[[1L]]]]
        prefix = if (info$owner_name[[1L]] == "") {
          prefix
        } else if (prefix == "") {
          info$owner_name[[1L]]
        } else {
          private$.add_name_prefix(prefix, info$owner_name[[1L]])
        }

        if (!test_class(subset, "ParamSetCollection")) return(prefix)

        full_prefix(subset, info$original_id[[1L]], prefix)
      }

      pvs = Reduce(c, map(ids, function(id_) {
        xs = param_set_table_first(
          private$.state()$.params, id_, "cargo"
        )$disable_in_tune
        prefix = full_prefix(self, id_)
        if (prefix == "") return(xs)
        set_names(xs, private$.add_name_prefix(full_prefix(self, id_), names(xs)))
      })) %??% named_list()
      self$set_values(.values = pvs)
    },

    #' @description
    #' Convert all parameters from the search space to parameter values using the transformation given by
    #' `in_tune_fn`.
    #' @param search_space ([`ParamSet`])\cr
    #'   The internal search space.
    #' @return (named `list()`)
    convert_internal_search_space = function(search_space) {
      assert_class(search_space, "ParamSet")
      state = private$.state()
      domains = search_space$domains
      converters = lapply(names(domains), function(.id) {
        converter = param_set_table_first(
          state$.params, .id, "cargo"
        )$in_tune_fn
        if (!is.function(converter)) {
          stopf("No converter exists for parameter '%s'", .id)
        }
        converter
      })
      names(converters) = names(domains)
      owner_indices = vapply(names(domains), function(.id) {
        param_set_table_first(
          state$.translation, .id, "owner_ps_index"
        )
      }, integer(1L))
      names(owner_indices) = names(domains)
      owner_values = lapply(state$.sets, function(owner) owner$values)

      imap(domains, function(token, .id) {
        converters[[.id]](token, owner_values[[owner_indices[[.id]]]])
      })
    },

    #' @description
    #' Create a `ParamSet` from this `ParamSetCollection`.
    flatten = function() {
      # Native flattening has already detached and validated the structural
      # state. This cold R pass only migrates lexical cargo callbacks into the
      # flattened namespace before replacing that one canonical column.
      flatps = super$flatten()

      recurse_prefix = function(id_, param_set, prefix = "") {
        nested_state = param_set_core_state(get_private(param_set))
        info = nested_state$.translation[
          match(id_, nested_state$.translation$id), , drop = FALSE
        ]
        prefix = if (info$owner_name[[1L]] == "") {
          prefix
        } else if (prefix == "") {
          info$owner_name[[1L]]
        } else {
          private$.add_name_prefix(prefix, info$owner_name[[1L]])
        }
        subset = nested_state$.sets[[info$owner_ps_index[[1L]]]]
        if (!test_class(subset, "ParamSetCollection")) {
          return(list(prefix = prefix, ids = subset$ids()))
        }
        if (prefix != "") {
          id_ = gsub(sprintf("^\\Q%s.\\E", prefix), "", id_)
        }
        recurse_prefix(id_, subset, prefix)
      }

      detach_cargo = function(cargo, id_) {
        if (all(map_lgl(cargo[c("disable_in_tune", "in_tune_fn")], is.null))) return(cargo)

        info = recurse_prefix(id_, self)
        prefix = info$prefix
        if (prefix == "") return(cargo)

        in_tune_fn = cargo$in_tune_fn

        prefixed_set_ids = private$.add_name_prefix(prefix, info$ids)
        cargo$in_tune_fn = crate(function(domain, param_vals) {
          param_vals = param_vals[names(param_vals) %in% prefixed_set_ids]
          names(param_vals) = gsub(sprintf("^\\Q%s.\\E", prefix), "", names(param_vals))
          in_tune_fn(domain, param_vals)
        }, in_tune_fn, prefix, prefixed_set_ids)

        if (length(cargo$disable_in_tune)) {
          cargo$disable_in_tune = set_names(
            cargo$disable_in_tune,
            private$.add_name_prefix(prefix, names(cargo$disable_in_tune))
          )
        }
        cargo
      }

      flat_private = flatps$.__enclos_env__$private
      flat_state = param_set_core_state(flat_private)
      flat_params = param_set_table_rows(
        flat_state$.params,
        seq_len(nrow(flat_state$.params))
      )
      cargos = flat_params$cargo
      plain_cargos = all(vapply(
        cargos,
        function(cargo) is.null(cargo) ||
          (is.list(cargo) && !is.object(cargo)),
        logical(1L)
      ))
      rows = if (plain_cargos) {
        which(vapply(
          cargos,
          function(cargo) !all(vapply(
            cargo[c("disable_in_tune", "in_tune_fn")],
            is.null,
            logical(1L)
          )),
          logical(1L)
        ))
      } else {
        seq_along(cargos)
      }
      if (length(rows)) {
        flat_params$cargo[rows] = Map(
          detach_cargo,
          flat_params$cargo[rows],
          flat_params$id[rows]
        )
        param_set_core_replace(flat_private, params = flat_params)
      }

      flatps
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
    #' This field provides direct references to the [`ParamSet`] objects.
    sets = function(v) {
      if (!missing(v) && !identical(v, private$.state()$.sets)) stop("sets is read-only")
      private$.state()$.sets
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
