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
    initialize = function(sets, set_id = "", ignore_ids = FALSE) {
      assert_list(sets, types = "ParamSet")
      if (!ignore_ids) {
        names(sets) = map(sets, "set_id")
      }
      split_sets = split(sets, names(sets) == "")
      nameless_sets = split_sets$`TRUE`
      named_sets = split_sets$`FALSE`
      assert_names(names2(named_sets), type = "strict")

      private$.tags = unlist(map(sets, "tags"), recursive = FALSE) %??% named_list()
      private$.sets = sets

      if (!identical(set_id, "")) {
        assert_id(set_id)
        assert_names(set_id, type = "strict")
      }
      private$.set_id = set_id

      pnames = names(self$params_unid)
      dups = duplicated(pnames)
      if (any(dups)) {
        stopf("ParamSetCollection would contain duplicated parameter names: %s",
          str_collapse(unique(pnames[dups])))
      }
    }
  ),
  active = list(
    #' @template field_params_unid
    params_unid = function(v) {
      sets = private$.sets
      if (length(sets) == 0L) {
        return(named_list())
      }
      # clone each param into new params-list and prefix id
      ps_all = lapply(sets, function(s) s$params_unid)
      ps_all = unlist(ps_all, recursive = FALSE)
      if (!length(ps_all)) ps_all = named_list()
      ps_all
    },
    #' @template field_deps
    deps = function(v) {
      if (!missing(v)) {
        stop("deps is read-only in ParamSetCollection.")
      }
      sets = private$.sets
      d_all = imap(sets, function(s, id) {
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

    #' @field tags (named `list()` of `character()`)\cr
    #' Can be used to group and subset parameters.
    #' Named with parameter IDs.
    tags = function(v) {
      if (!missing(v)) {
        assert_list(v, any.missing = FALSE, types = "character")
        assert_names(names(v), identical.to = names(self$params_unid))
        private$.tags = v
      }
      private$.tags
    },

    #' @template field_values
    values = function(xs) {
      sets = private$.sets
      if (is.null(names(sets))) {
        names(sets) = map(private$.sets, "set_id")
      }
      if (!missing(xs)) {
        assert_list(xs)
        self$assert(xs) # make sure everything is valid and feasible

        for (i in seq_along(sets)) {
          s = sets[[i]]
          sid = names(sets)[[i]]
          # retrieve sublist for each set, then assign it in set (after removing prefix)
          psids = names(s$params_unid)
          if (sid != "") {
            psids = sprintf("%s.%s", sid, psids)
          }
          pv = xs[intersect(psids, names(xs))]
          if (sid != "") {
            names(pv) = substr(names(pv), nchar(sid) + 2, nchar(names(pv)))
          }
          s$values = pv
        }
      }
      vals = map(sets, "values")
      vals = unlist(vals, recursive = FALSE)
      if (!length(vals)) vals = named_list()
      vals
    }
  ),

  private = list(
    .sets = NULL,
    deep_clone = function(name, value) {
      switch(name,
        .params = named_list(),
        .deps = {
          value = copy(value)
          value$cond = lapply(value$cond, function(x) x$clone(deep = TRUE))
          value
        },
        .sets = map(value, function(x) {
          x$clone(deep = TRUE)
        }),
        value
      )
    }
  )
)
