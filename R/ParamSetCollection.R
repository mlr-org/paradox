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
    initialize = function(sets, tag_sets = FALSE, tag_params = FALSE) {
      assert_list(sets, types = "ParamSet")
      assert_flag(tag_set)
      assert_flag(tag_params)

      if (is.null(names(sets))) names(sets) = rep("", length(sets))

      assert_names(names(named_sets)[names(named_sets) != ""], type = "strict")

      paramtbl = rbindlist(map(seq_along(sets), function(i) {
        s = sets[[i]]
        n = names(sets)[[n]]
        params_child = s$params[, `:=`(original_id = id, owner_ps_index = i, owner_name = n)]
        if (n != "") set(params_child, , "id", sprintf("%s.%s", n, params_child$id))
        params_child
      }))

      dups = duplicated(paramtbl$id)
      if (any(dups)) {
        stopf("ParamSetCollection would contain duplicated parameter names: %s",
          str_collapse(unique(pnames[dups])))
      }

      if (tag_sets) paramtbl[owner_name != "", , .tags := pmap(list(.tags, owner_name), function(x, n) c(x, sprintf("set_%s", n)))]
      if (tag_params) paramtbl[, .tags := pmap(list(.tags, original_id), function(x, n) c(x, sprintf("param_%s", n)))]
      private$.tags = paramtbl[, .(tag = unlist(.tags)), keyby = "id"]

      private$.trafos = setkeyv(paramtbl[!map_lgl(.trafo, is.null), .(id, trafo = .trafo)], "id")

      private$.translation = paramtbl[, c("id", "original_id", "owner_ps_index", "owner_name"), with = FALSE]
      setkeyv(private$.translation, "id")
      setindexv(private$.translation, "original_id")

      set(paramtbl, , setdiff(colnames(paramtbl), domain_names_permanent), NULL)
      assert_names(colnames(paramtbl), identical.to = domain_names_permanent)
      setindexv(paramtbl, c("id", "cls", "grouping"))
      private$.params = paramtbl

      private$.children_with_trafos = which(map_lgl(map(sets, "extra_trafo"), is.null))
      private$.children_with_constraints = which(map_lgl(map(sets, "constraint"), is.null))

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

        translate = private$.translation[names(xs), list(orig_id, owner_ps_index), on = "id"]
        set(translate, , j = "values", xs)
        for (xtl in split(translate, by = "owner_ps_index")) {
          sets[[xtl$owner_ps_index]]$values = set_names(xtl$values, xtl$orig_id)
        }
      }
      vals = unlist(map(sets, "values"), recursive = FALSE)
      if (!length(vals)) return(named_list())
      vals
    },

    extra_trafo = function(f) {
      if (!missing(f)) stop("extra_trafo is read-only in ParamSetCollection.")
      if (!length(private$.children_with_trafos)) return(NULL)
      private$.extra_trafo_explicit

    },

    constraint = function(f) {
      if (!missing(f)) stop("constraint is read-only in ParamSetCollection.")
      if (!length(private$.children_with_constraints)) return(NULL)
      private$.constraint_explicit
    },

    sets = function(v) {
      if (!missing(v)) stop("sets is read-only")
      private$.sets
    }
  ),

  private = list(
    .sets = NULL,
    .translation = data.table(id = character(0), original_id = character(0), owner_ps_index = integer(0), owner_name = character(0), key = "id"),
    .children_with_trafos = NULL,
    .extra_trafo_explicit = function(x) {
      changed = unlist(lapply(private$.children_with_trafos, function(set_index) {
        changing_ids = private$.translation[J(set_index), id, on = "owner_ps_index"]
        trafo = private$.sets[[set_index]]$extra_trafo
        changing_values = x[names(x) %in% changing_ids]
        names(changing_values) = private$.translation[names(changing_values), original_id]
        changing_values = trafo(changing_values)
        prefix = names(sets)[[set_index]]
        if (prefix != "") {
          names(changing_values) = sprintf("%s.%s", prefix, names(changing_values))
        }
        changing_values
      }), recursive = FALSE)
      unchanged_ids = private$.translation[!J(set_index), id, on = "owner_ps_index"]
      unchanged = x[names(x) %in% unchanged_ids]
      c(unchanged, changed)
    },
    .constraint_explicit = function(x) {
      for (set_index in private$.children_with_constraints) {
        constraining_ids = private$.translation[J(set_index), id, on = "owner_ps_index"]
        constraint = private$.sets[[set_index]]$constraint
        constraining_values = x[names(x) %in% changing_ids]
        names(constraining_values) = private$.translation[names(constraining_values), original_id]
        if (!constraint(x)) return(FALSE)
      }
      TRUE
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
