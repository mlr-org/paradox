#' @title ParamSetCollection
#'
#' @description
#' A collection of multiple [ParamSet] objects.
#' * The collection is basically a light-weight wrapper / container around references to multiple sets.
#' * In order to ensure unique param names, every param in the collection is referred to with
#'   "<set_id>.<param_id>". Parameters from ParamSets with empty (i.e. `""`) `$set_id` are referenced
#'   directly. Multiple ParamSets with `$set_id` `""` can be combined, but their parameter names
#'   must be unique.
#' * Operation `subset` is currently not allowed.
#' * Operation `add` currently only works when adding complete sets not single params.
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
    #' @param ignore_ids (`logical(1)`)\cr
    #'   Ignore `$set_id` slots of `sets` and instead use the names instead.
    #'   When this is `TRUE`, then `sets` should be named when id prefixes are desired.
    #'   This can be used to create a `ParamSetCollection` with changed [`ParamSet`] `set_id`s
    #'   without having to clone said [`ParamSet`]s. However, when underlying [`ParamSet`]s'
    #'   `$set_id`s change, then this change is no longer reflected in the ParamSetCollection.
    #'   Default `FALSE`.
    initialize = function(sets, ignore_ids = FALSE) {

      assert_list(sets, types = "ParamSet")
      if (!ignore_ids) {
        names(sets) = map(sets, "set_id")
      }
      split_sets = split(sets, names(sets) == "")
      nameless_sets = split_sets$`TRUE`
      named_sets = split_sets$`FALSE`
      assert_names(names2(named_sets), type = "strict")
      if (!ignore_ids) sets = unname(sets)  # when private$.sets is unnamed, then the set_ids are used.
      private$.sets = sets
      self$set_id = ""
      pnames = names(self$params_unid)
      dups = duplicated(pnames)
      if (any(dups)) {
        stopf("ParamSetCollection would contain duplicated parameter names: %s",
          str_collapse(unique(pnames[dups])))
      }
    },

    #' @description
    #' Adds a set to this collection.
    #'
    #' @param p ([ParamSet]).
    add = function(p) {
      assert_r6(p, "ParamSet")
      setnames = names(private$.sets) %??% map_chr(private$.sets, "set_id")
      if (p$set_id == "") {
        unnamed_set_parnames = map(private$.sets[setnames == ""], function(x) names(x$params_unid))
      } else if (p$set_id %in% setnames) {
        stopf("Setid '%s' already present in collection!", p$set_id)
      }
      if (p$has_trafo) {
        stop("Building a collection out sets, where a ParamSet has a trafo is currently unsupported!")
      }
      pnames = names(p$params_unid)
      nameclashes = intersect(
        ifelse(p$set_id != "", sprintf("%s.%s", p$set_id, pnames), pnames),
        names(self$params_unid)
      )
      if (length(nameclashes)) {
        stopf("Adding parameter set would lead to nameclashes: %s", str_collapse(nameclashes))
      }
      set_addition = list(p)
      if (!is.null(names(private$.sets))) {
        # ignoring the other ParamSet's set_id in favor of names(private$.sets), so add the name here as well.
        names(set_addition) = p$set_id
      }
      private$.sets = c(private$.sets, set_addition)
      invisible(self)
    },

    #' @description
    #' Removes sets of given ids from collection.
    #'
    #' @param ids (`character()`).
    remove_sets = function(ids) {
      setnames = names(private$.sets) %??% map_chr(private$.sets, "set_id")
      assert_subset(ids, setnames)
      private$.sets[setnames %in% ids] = NULL
      invisible(self)
    },

    #' @description
    #' Only included for consistency. Not allowed to perform on [ParamSetCollection]s.
    #'
    #' @param ids (`character()`).
    subset = function(ids) stop("not allowed")

  ),

  active = list(
    #' @template field_params
    params = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.params)) {
        stop("$params is read-only.")
      }
      # clone on-demand, whenever underlying class changed
      punid = self$params_unid
      truenames = names(punid)

      # 'changed' can be either because an underlying ParamSet's $params changed though $add or so,
      # or because we are calling $params for the first time (in which case all are 'changed'
      # as the original is NULL).
      changed = as.logical(imap(punid, function(x, n) !identical(private$.params_cloned[[n]], x)))

      if (any(changed) || length(punid) != length(private$.params_cloned)) {  # if there was a strict subset operation we wouldn't notice it otherwise
        changed_names = truenames[changed]
        private$.params = c(private$.params[setdiff(names(private$.params), changed_names)],  # don't regenerate Params that were not changed.
          sapply(changed_names, function(u) {
            p = punid[[u]]$clone(deep = TRUE)
            p$id = u
            p
          }, simplify = FALSE)
        )[truenames]
      }
      private$.params_cloned = punid
      private$.params
    },
    #' @template field_params_unid
    params_unid = function(v) {
      sets = private$.sets
      if (is.null(names(sets))) {
        names(sets) = map(private$.sets, "set_id")
      }
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
      sets = private$.sets
      if (is.null(names(sets))) {
        names(sets) = map(private$.sets, "set_id")
      }
      d_all = imap(sets, function(s, id) {
        # copy all deps and rename ids to prefixed versions
        dd = copy(s$deps)
        ids_old = s$ids()
        if (id != "" && nrow(dd)) {
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
    .params_cloned = NULL,
    deep_clone = function(name, value) {
      switch(name,
        .params = named_list(),
        .params_cloned = NULL,
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
