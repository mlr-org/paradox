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
#' * When you either ask for 'values' or set them, the operation is delegated to the individual,
#'   contained [`ParamSet`] references. The collection itself does not maintain a `values` state.
#'   This also implies that if you directly change `values` in one of the referenced sets,
#'   this change is reflected in the collection.
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
    #'   ParamSet objects are not cloned.
    #'   Names are used as "set_id" for the naming scheme of delegated parameters.
    #' @param tag_sets (`logical(1)`)\cr
    #'   Whether to add tags of the form `"set_<set_id>"` to each parameter originating from a given `ParamSet` given with name `<set_id>`.
    #' @param tag_params (`logical(1)`)\cr
    #'   Whether to add tags of the form `"param_<param_id>"` to each parameter with original ID `<param_id>`.
    #' @param postfix_names (`logical(1)`)\cr
    #'   Whether to use the names inside `sets` as postfixes, rather than prefixes.
    initialize = function(sets, tag_sets = FALSE, tag_params = FALSE, postfix_names = FALSE) {
      assert_list(sets, types = "ParamSet")
      assert_flag(tag_sets)
      assert_flag(tag_params)
      private$.postfix = assert_flag(postfix_names)

      if (is.null(names(sets))) names(sets) = rep("", length(sets))

      assert_names(names(sets)[names(sets) != ""], type = "strict")

      paramtbl = rbindlist_proto(map(seq_along(sets), function(i) {
        s = sets[[i]]
        n = names(sets)[[i]]
        params_child = s$.__enclos_env__$private$.params
        if (nrow(params_child)) {
          params_child = copy(params_child)
          set(params_child, , "original_id", params_child$id)
          set(params_child, , "owner_ps_index", i)
          set(params_child, , "owner_name", n)
          if (n != "") {
            set(params_child, , "id",
              private$.add_name_prefix(n, params_child$id)
            )
          }
          params_child
        }
      }), prototype = {
        paramtbl = copy(empty_domain)
        set(paramtbl, , "original_id", character(0))
        set(paramtbl, , "owner_ps_index", integer(0))
        set(paramtbl, , "owner_name", character(0))
      })

      dups = duplicated(paramtbl$id)
      if (any(dups)) {
        stopf("ParamSetCollection would contain duplicated parameter names: %s",
          str_collapse(unique(paramtbl$id[dups])))
      }

      alltagstables = map(seq_along(sets), function(i) {
        s = sets[[i]]
        n = names(sets)[[i]]
        if (tag_sets || tag_params) {
          ids = s$.__enclos_env__$private$.params$id
          newids = ids
          if (n != "") newids = private$.add_name_prefix(n, ids)
        }
        tags_child = s$.__enclos_env__$private$.tags
        list(
          if (nrow(tags_child)) {
            tags_child = copy(tags_child)
            if (n != "") set(tags_child, , "id", private$.add_name_prefix(n, tags_child$id))
            tags_child
          },
          if (tag_sets && n != "" && length(newids)) {
            structure(list(id = newids, tag = rep_len(sprintf("set_%s", n), length(ids))),
              class = c("data.table", "data.frame"))
          },
          if (tag_params && length(newids)) {
            structure(list(id = newids, tag = sprintf("param_%s", ids)),
              class = c("data.table", "data.frame"))
          }
        )
      })

      # this may introduce duplicate tags, if param_... or set_... is present before...
      private$.tags = rbindlist_proto(unlist(alltagstables, recursive = FALSE, use.names = FALSE),
        prototype = structure(list(
            id = character(0), tag = character(0)
          ), class = c("data.table", "data.frame")
        )
      )
      setkeyv(private$.tags, "id")

      private$.trafos = rbindlist_proto(map(seq_along(sets), function(i) {
        s = sets[[i]]
        n = names(sets)[[i]]
        trafos_child = s$.__enclos_env__$private$.trafos
        if (nrow(trafos_child)) {
          trafos_child = copy(trafos_child)
          if (n != "" && nrow(trafos_child)) set(trafos_child, , "id", private$.add_name_prefix(n, trafos_child$id))
          trafos_child
        }
      }), prototype = structure(list(
            id = character(0), trafo = list()
          ), class = c("data.table", "data.frame")
        )
      )
      setkeyv(private$.trafos, "id")

      private$.translation = structure(unclass(copy(paramtbl))[c("id", "original_id", "owner_ps_index", "owner_name")],
        class = c("data.table", "data.frame"))
      setkeyv(private$.translation, "id")
      setindexv(private$.translation, "original_id")

      set(paramtbl, , setdiff(colnames(paramtbl), domain_names_permanent), NULL)
      assert_names(colnames(paramtbl), identical.to = domain_names_permanent)
      setindexv(paramtbl, c("id", "cls", "grouping"))
      private$.params = paramtbl

      private$.sets = sets
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
      assert_r6(p, "ParamSet")
      if (n != "" && n %in% names(private$.sets)) {
        stopf("Set name '%s' already present in collection!", n)
      }
      pnames = p$ids()
      nameclashes = intersect(
        ifelse(n != "", private$.add_name_prefix(n, pnames), pnames),
        self$ids()
      )
      if (length(nameclashes)) {
        stopf("Adding parameter set would lead to nameclashes: %s", str_collapse(nameclashes))
      }

      new_index = length(private$.sets) + 1
      paramtbl = p$params[, `:=`(original_id = id, owner_ps_index = new_index, owner_name = n)]
      if (n != "") set(paramtbl, , "id", private$.add_name_prefix(n, paramtbl$id))

      if (!nrow(paramtbl)) {
        # when paramtbl is empty, use special setup to make sure information about the `.tags` column is present.
        paramtbl = copy(empty_domain)[, `:=`(original_id = character(0), owner_ps_index = integer(0), owner_name = character(0))]
      }
      if (tag_sets && n != "") paramtbl[, .tags := map(.tags, function(x) c(x, sprintf("set_%s", n)))]
      if (tag_params) paramtbl[, .tags := pmap(list(.tags, original_id), function(x, n) c(x, sprintf("param_%s", n)))]
      newtags = paramtbl[, .(tag = unique(unlist(.tags, use.names = FALSE))), by = "id"]
      if (nrow(newtags)) {
        private$.tags = setkeyv(rbind(private$.tags, newtags), "id")
      }

      newtrafos = paramtbl[!map_lgl(.trafo, is.null), .(id, trafo = .trafo)]
      if (nrow(newtrafos)) {
        private$.trafos = setkeyv(rbind(private$.trafos, newtrafos), "id")
      }

      private$.translation = rbind(private$.translation, paramtbl[, c("id", "original_id", "owner_ps_index", "owner_name"), with = FALSE])
      setkeyv(private$.translation, "id")
      setindexv(private$.translation, "original_id")

      set(paramtbl, , setdiff(colnames(paramtbl), domain_names_permanent), NULL)
      assert_names(colnames(paramtbl), identical.to = domain_names_permanent)
      private$.params = rbind(private$.params, paramtbl)
      setindexv(private$.params, c("id", "cls", "grouping"))

      entry = if (n == "") length(private$.sets) + 1 else n
      private$.sets[[n]] = p
      invisible(self)
    },

    #' @description
    #' Create a new `ParamSet` restricted to the passed IDs.
    #' @param ids (`character()`).
    #' @param allow_dangling_dependencies (`logical(1)`)\cr
    #'   Whether to allow subsets that cut across parameter dependencies.
    #'   Dependencies that point to dropped parameters are kept (but will be "dangling", i.e. their `"on"` will not be present).
    #' @param keep_constraint (`logical(1)`)\cr
    #'   Whether to keep the `$constraint` function.
    #' @return `ParamSet`.
    subset = function(ids, allow_dangling_dependencies = FALSE, keep_constraint = TRUE) {
      # need to take care of extra_trafo and constraint.
      result = super$subset(ids, allow_dangling_dependencies = allow_dangling_dependencies, keep_constraint = keep_constraint)
      if (keep_constraint) result$constraint = private$.get_constraint_detached(ids)
      result$extra_trafo = private$.get_extra_trafo_detached(ids)
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
        info = get_private(param_set)$.translation[id_, c("owner_name", "original_id", "owner_ps_index"), on = "id"]
        subset = get_private(param_set)$.sets[[info$owner_ps_index]]
        prefix = if (info$owner_name == "") {
          prefix
        } else if (prefix == "") {
          info$owner_name
        } else {
          private$.add_name_prefix(prefix, info$owner_name)
        }

        if (!test_class(subset, "ParamSetCollection")) return(prefix)

        full_prefix(subset, info$original_id, prefix)
      }

      pvs = Reduce(c, map(ids, function(id_) {
        xs = private$.params[list(id_), "cargo", on = "id"][[1]][[1]]$disable_in_tune
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
      imap(search_space$domains, function(token, .id) {
        converter = private$.params[list(.id), "cargo", on = "id"][[1L]][[1L]]$in_tune_fn
        if (!is.function(converter)) {
          stopf("No converter exists for parameter '%s'", .id)
        }
        set_index = private$.translation[list(.id), "owner_ps_index", on = "id"][[1L]]
        converter(token, private$.sets[[set_index]]$values)
      })
    },

    #' @description
    #' Create a `ParamSet` from this `ParamSetCollection`.
    flatten = function() {
      flatps = super$flatten()

      # This function is a mistake. It should not have been written. Sorry for allowing it to be merged.

      recurse_prefix = function(id_, param_set, prefix = "") {
        info = get_private(param_set)$.translation[list(id_), c("owner_name", "owner_ps_index"), on = "id"]
        prefix = if (info$owner_name == "") {
          prefix
        } else if (prefix == "") {
          info$owner_name
        } else {
          private$.add_name_prefix(prefix, info$owner_name)
        }
        subset = get_private(param_set)$.sets[[info$owner_ps_index]]
        if (!test_class(subset, "ParamSetCollection")) {
          return(list(prefix = prefix, ids = subset$ids()))
        }
        if (prefix != "") {
          id_ = gsub(sprintf("^\\Q%s.\\E", prefix), "", id_)
        }
        recurse_prefix(id_, get_private(param_set)$.sets[[info$owner_ps_index]], prefix)
      }

      flatps$.__enclos_env__$private$.params[, let(
        cargo = pmap(list(cargo = cargo, id_ = id), function(cargo, id_) {
          if (all(map_lgl(cargo[c("disable_in_tune", "in_tune_fn")], is.null))) return(cargo)

          info = recurse_prefix(id_, self)
          prefix = info$prefix
          if (prefix == "") return(cargo)

          in_tune_fn = cargo$in_tune_fn

          set_ids = info$ids
          cargo$in_tune_fn = crate(function(domain, param_vals) {
            param_vals = param_vals[names(param_vals) %in% private$.add_name_prefix(prefix, set_ids)]
            names(param_vals) = gsub(sprintf("^\\Q%s.\\E", prefix), "", names(param_vals))
            in_tune_fn(domain, param_vals)
          }, in_tune_fn, prefix, set_ids)

          if (length(cargo$disable_in_tune)) {
            cargo$disable_in_tune = set_names(
              cargo$disable_in_tune,
              private$.add_name_prefix(prefix, names(cargo$disable_in_tune))
            )
          }
          cargo
        })
      )]

      flatps
    }
  ),

  active = list(
    #' @template field_deps
    deps = function(v) {
      if (!missing(v)) {
        stop("deps is read-only in ParamSetCollection.")
      }
      d_all = imap(private$.sets, function(s, id) {
        # copy all deps and rename ids to prefixed versions
        dd = s$deps
        if (id != "" && nrow(dd)) {
          ids_old = s$ids()
          ids_new = private$.add_name_prefix(id, ids_old)
          dd$id = map_values(dd$id, ids_old, ids_new)
          dd$on = map_values(dd$on, ids_old, ids_new)
        }
        dd
      })
      rbindlist(c(d_all, list(private$.deps)), use.names = TRUE)
    },

    #' @template field_extra_trafo
    extra_trafo = function(f) {
      if (!missing(f)) stop("extra_trafo is read-only in ParamSetCollection.")
      if (!length(private$.children_with_trafos())) return(NULL)

      # The reason why we don't crate a function here is that the extra_trafo of private$.sets could change.
      private$.extra_trafo_explicit
    },

    #' @template field_constraint
    constraint = function(f) {
      if (!missing(f)) stop("constraint is read-only in ParamSetCollection.")
      if (!length(private$.children_with_constraints())) return(NULL)
      private$.constraint_explicit
    },

    #' @field sets (named `list()`)\cr
    #' Read-only `list` of of [`ParamSet`]s contained in this `ParamSetCollection`.
    #' This field provides direct references to the [`ParamSet`] objects.
    sets = function(v) {
      if (!missing(v) && !identical(v, private$.sets)) stop("sets is read-only")
      private$.sets
    }
  ),

  private = list(
    .postfix = FALSE,
    .add_name_prefix = function(owner, id) {
      if (private$.postfix) sprintf("%s.%s", id, owner) else sprintf("%s.%s", owner, id)
    }
    .get_values = function() {
      vals = unlist(map(private$.sets, "values"), recursive = FALSE)
      if (length(vals)) vals else named_list()
    },
    .store_values = function(xs) {
      sets = private$.sets
      # %??% character(0) in case xs is an empty unnamed list
      idx = match(names(xs) %??% character(0), private$.translation$id)
      translate = private$.translation[idx, c("original_id", "owner_ps_index"), with = FALSE]
      set(translate, , j = "values", list(xs))
      for (xtl in split(translate, f = translate$owner_ps_index)) {
        sets[[xtl$owner_ps_index[[1]]]]$.__enclos_env__$private$.store_values(set_names(xtl$values, xtl$original_id))
      }
      # clear the values of all sets that are not touched by xs
      for (clearing in setdiff(seq_along(sets), translate$owner_ps_index)) {
        sets[[clearing]]$.__enclos_env__$private$.store_values(named_list())
      }
    },
    .sets = NULL,
    .translation = data.table(id = character(0), original_id = character(0), owner_ps_index = integer(0), owner_name = character(0), key = "id"),
    .children_with_trafos = function() {
      which(!map_lgl(map(private$.sets, "extra_trafo"), is.null))
    },
    .children_with_constraints = function() {
      which(!map_lgl(map(private$.sets, "constraint"), is.null))
    },
    .extra_trafo_explicit = function(x) {
      children_with_trafos = private$.children_with_trafos()
      sets_with_trafos = private$.sets[children_with_trafos]
      translation = private$.translation
      psc_extra_trafo(x, children_with_trafos, sets_with_trafos, translation, private$.postfix)
    },
    # get an extra_trafo function that does not have any references to the PSC object or any of its contained sets.
    # This is used for flattening.
    # `ids`: subset of params to consider
    .get_extra_trafo_detached = function(ids = NULL) {
      translation = if (is.null(ids)) copy(private$.translation) else private$.translation[id %in% ids]
      children_with_trafos = private$.children_with_trafos()  # just an integer vector, no need to worry here
      if (!is.null(ids)) {
        children_with_trafos = intersect(children_with_trafos, translation$owner_ps_index)
      }
      if (!length(children_with_trafos)) return(NULL)
      sets_with_trafos = lapply(private$.sets[children_with_trafos], function(x) x$clone(deep = TRUE))  # get new objects that are detached from PSC
      postfix = private$.postfix
      crate(function(x) psc_extra_trafo(x, children_with_trafos, sets_with_trafos, translation, postfix), children_with_trafos, sets_with_trafos, translation, psc_extra_trafo, postfix)
    },
    .constraint_explicit = function(x) {
      children_with_constraints = private$.children_with_constraints()
      sets_with_constraints = private$.sets[children_with_constraints]
      translation = private$.translation
      psc_constraint(x, children_with_constraints, sets_with_constraints, translation)
    },
    # same as with extra_trafo above
    .get_constraint_detached = function(ids = NULL) {
      translation = if (is.null(ids)) copy(private$.translation) else private$.translation[id %in% ids]
      children_with_constraints = private$.children_with_constraints()
      if (!is.null(ids)) {
        children_with_constraints = intersect(children_with_constraints, translation$owner_ps_index)
      }
      if (!length(children_with_constraints)) return(NULL)
      sets_with_constraints = lapply(private$.sets[children_with_constraints], function(x) x$clone(deep = TRUE))
      crate(function(x) psc_constraint(x, children_with_constraints, sets_with_constraints, translation), children_with_constraints, sets_with_constraints, translation, psc_constraint)
    },
    deep_clone = function(name, value) {
      switch(name,
        .deps = copy(value),
        .sets = map(value, function(x) {
          x$clone(deep = TRUE)
        }),
        value
      )
    }
  )
)

# extra_trafo function for ParamSetCollection
# This function is used as extra_trafo for ParamSetCollection, in the case that any of its children has an extra_trafo.
# Arguments:
# - children_with_trafos: set-indices (i.e. index inside PSC's private$.sets, and inside `translation`) of children with extra_trafo
# - sets_with_trafos: subset of PSC's private$.sets of children with extra_trafo
# - translation: PSC's private$.translation
#
# We have this functoin outside of the ParamSetCollection class, because we anticipate that PSC can be "flattened", i.e. turned into
# a normal ParamSet. In that case, the resulting ParamSet's extra_trafo should be a function that can stand on its own, without
# referring to private$<anything>.
psc_extra_trafo = function(x, children_with_trafos, sets_with_trafos, translation, postfix) {
  changed = unlist(lapply(seq_along(children_with_trafos), function(i) {
    set_index = children_with_trafos[[i]]
    changing_ids = translation[J(set_index), id, on = "owner_ps_index"]
    trafo = sets_with_trafos[[i]]$extra_trafo
    changing_values_in = x[names(x) %in% changing_ids]
    names(changing_values_in) = translation[names(changing_values_in), original_id]
    # input of trafo() must not be changed after the call; otherwise the trafo would have to `force()` it in
    # some circumstances.
    if (test_function(trafo, args = c("x", "param_set"))) {
      changing_values = trafo(x = changing_values_in, param_set = sets_with_trafos[[i]])
    } else {
      changing_values = trafo(changing_values_in)
    }
    changing_values = trafo(changing_values_in)
    prefix = names(sets_with_trafos)[[i]]
    if (prefix != "") {
      names(changing_values) = if (postfi) sprintf("%s.%s", names(changing_values), prefix) else sprintf("%s.%s", prefix, names(changing_values))
    }
    changing_values
  }), recursive = FALSE)
  unchanged_ids = translation[!J(children_with_trafos), id, on = "owner_ps_index"]
  unchanged = x[names(x) %in% unchanged_ids]
  c(unchanged, changed)
}

psc_constraint = function(x, children_with_constraints, sets_with_constraints, translation) {
  for (i in seq_along(children_with_constraints)) {
    set_index = children_with_constraints[[i]]
    constraining_ids = translation[J(set_index), id, on = "owner_ps_index"]
    constraint = sets_with_constraints[[i]]$constraint
    constraining_values = x[names(x) %in% constraining_ids]
    names(constraining_values) = translation[names(constraining_values), original_id]
    if (!constraint(x)) return(FALSE)
  }
  TRUE
}


