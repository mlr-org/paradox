#' @title ParamSetCollection
#'
#' @description
#' A collection of multiple [ParamSet] objects.
#' * The collection is basically a light-weight wrapper / container around references to multiple sets.
#' * In order to ensure unique param names, every param in the collection is referred to with
#'   "<set_id>.<param_id>". Parameters from ParamSets with empty (i.e. `""`) `$set_id` are referenced
#'   directly. Multiple ParamSets with `$set_id` `""` can be combined, but their parameter names
#'   must be unique.
#' * When you either ask for 'values' or set them, the operation is delegated to the individual,
#'   contained param set references. The collection itself does not maintain a `values` state.
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
    #' @param sets (`list()` of [ParamSet])\cr
    #'   ParamSet objects are not cloned.
    #' @param set_id (`character(1)`)\cr
    #'   `$set_id` of the resulting `ParamSet`. This determines the
    #'   prefix inside [`ParamSetCollection`]. Default `""` (no prefix).
    #' @param ignore_ids (`logical(1)`)\cr
    #'   Ignore `$set_id` slots of `sets` and instead use the names instead.
    #'   When this is `TRUE`, then `sets` should be named when id prefixes are desired.
    #'   This can be used to create a `ParamSetCollection` with changed [`ParamSet`] `set_id`s
    #'   without having to clone said [`ParamSet`]s. However, when underlying [`ParamSet`]s'
    #'   `$set_id`s change, then this change is no longer reflected in the ParamSetCollection.
    #'   Also note that the `$values` will have undefined behavior when `sets` contains a
    #'   single [`ParamSet`] multiple times (by reference).
    #'   Default `FALSE`.
    initialize = function(sets, tag_set = FALSE, tag_params = FALSE) {
      assert_list(sets, types = "ParamSet")
      assert_flag(tag_set)
      assert_flag(tag_params)
      split_sets = split(sets, names(sets) == "")
      nameless_sets = split_sets$`TRUE`
      named_sets = split_sets$`FALSE`
      assert_names(names2(named_sets), type = "strict")
      assert_ids(names2(named_sets))
      if (is.null(names(sets))) names(sets) = rep("", length(sets))

      params = rbindlist(map(seq_along(sets), function(i) {
        s = sets[[i]]
        n = names2(sets)[[n]]
        params_child = cbind(s$params, orig_id = s$params$id, owner_ps = i)
        if (n != "") set(params_child, , "id", sprintf("%s.%s", n, params_child$id))
        params_child
      }))

      dups = duplicated(params$id)
      if (any(dups)) {
        stopf("ParamSetCollection would contain duplicated parameter names: %s",
          str_collapse(unique(pnames[dups])))
      }

      private$.tags = combine_tags(sets, tag_sets, tag_params)
      private$.extra_trafo = combine_trafos(sets, params)
      private$.constraint = combine_constraints(sets, params)
      private$.params = params
      private$.sets = sets
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
          ids_new = sprintf("%s.%s", id, ids_old)
          dd$id = map_values(dd$id, ids_old, ids_new)
          dd$on = map_values(dd$on, ids_old, ids_new)
        }
        dd
      })
      rbindlist(c(d_all, list(private$.deps)), use.names = TRUE)
    },

    #' @template field_values
    values = function(xs) {
      sets = private$.sets
      if (!missing(xs)) {
        assert_list(xs)
        # make sure everything is valid and feasible.
        # We do this here because we don't want the loop to be aborted early and have half an update.
        self$assert(xs)

        translate = private$.params[names(xs), list(orig_id, owner_ps), on = "id"]
        set(translate, , j = "values", xs)
        for (xtl in split(translate, by = "owner_ps")) {
          sets[[xtl$owner_ps]]$values = set_names(xtl$values, xtl$orig_id)
        }
      }
      vals = unlist(map(sets, "values"), recursive = FALSE)
      if (!length(vals)) return(named_list())
      vals
    },

    extra_trafo = function(f) {
      if (!missing(f)) stop("extra_trafo is read-only in ParamSetCollection.")
      private$.extra_trafo
    },

    constraint = function(f) {
      if (!missing(f)) stop("constraint is read-only in ParamSetCollection.")
      private$.constraint
    }
  ),

  private = list(
    .sets = NULL,
    .extra_trafo = NULL,
    .constraint = NULL,
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

combine_trafos = function(sets, params) {
  child_trafos = map(sets, "extra_trafo")
  child_with_trafos = which(!map_lgl(child_trafos, is.null))
  if (!length(child_with_trafos)) return(NULL)

  child_trafos = child_trafos[child_with_trafos]
  crate(function(x) {
    unlist(imap(child_with_trafos, function(child, trafoi) {
      child = child_with_trafos[[trafoi]]
      trafo = child_trafos[[trafoi]]
      setinfo = params[data.table(owner_ps = child), c("id", "orig_id"), with = FALSE, on = "owner_ps"]
      subx = x[match(setinfo$id, names(x), nomatch = 0)]
      names(subx) = params[names(subx), orig_id, on = "id"]

      transformed = trafo(subx)

      prefix = names(sets)[[child]]
      if (prefix != "") {
        names(transformed) = sprintf("%s.%s", prefix, names(transformed))
      }
    }), recursive = FALSE)
  }, params, sets, child_trafos, child_with_trafos)
}

combine_constraints = function(sets, params) {
  child_constraints = map(sets, "constraint")
  child_with_constraints = which(!map_lgl(child_constraints, is.null))
  if (!length(child_with_constraints)) return(NULL)

  child_constraints = child_constraints[child_with_constraints]
  crate(function(x) {
    for (constrainti in seq_along(child_with_constraints)) {
      child = child_with_constraints[[constrainti]]
      constraint = child_constraints[[constrainti]]
      setinfo = params[data.table(owner_ps = child), c("id", "orig_id"), with = FALSE, on = "owner_ps"]
      subx = x[match(setinfo$id, names(x), nomatch = 0)]
      names(subx) = params[names(subx), orig_id, on = "id"]

      if (!constraint(x)) return(FALSE)
    }
    TRUE
  }, params, sets, child_constraints, child_with_constraints)
}

combine_tags = function(sets, tag_sets, tag_params) {
  if (tag_set) {
    if (tag_params) {
      unlist(imap(sets, function(s, n) {
        add_tags = sprintf("set_%s", n)
        imap(s$tags, function(t, tn) unique(c(t, add_tags, sprintf("param_%s", tn))))
      }), recursive = FALSE)
    } else {
      unlist(imap(sets, function(s, n) {
        add_tags = sprintf("set_%s", n)
        imap(s$tags, function(t) unique(c(t, add_tags)))
      }), recursive = FALSE)
    }
  } else {
    if (tag_params) {
      unlist(map(sets, function(s) {
        imap(s$tags, function(t, tn) unique(c(t, sprintf("param_%s", tn))))
      }), recursive = FALSE)
    } else {
      unlist(map(sets, "tags"), recursive = FALSE)
    }
  }
}
