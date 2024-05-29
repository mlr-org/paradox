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
    initialize = function(sets, tag_sets = FALSE, tag_params = FALSE) {
      assert_list(sets, types = "ParamSet")
      assert_flag(tag_sets)
      assert_flag(tag_params)

      if (is.null(names(sets))) names(sets) = rep("", length(sets))

      assert_names(names(sets)[names(sets) != ""], type = "strict")

      paramtbl = rbindlist(map(seq_along(sets), function(i) {
        s = sets[[i]]
        n = names(sets)[[i]]
        params_child = s$params[, `:=`(original_id = id, owner_ps_index = i, owner_name = n)]
        if (n != "") set(params_child, , "id", sprintf("%s.%s", n, params_child$id))
        params_child
      }))

      dups = duplicated(paramtbl$id)
      if (any(dups)) {
        stopf("ParamSetCollection would contain duplicated parameter names: %s",
          str_collapse(unique(paramtbl$id[dups])))
      }

      if (!nrow(paramtbl)) {
        # when paramtbl is empty, use special setup to make sure information about the `.tags` column is present.
        paramtbl = copy(empty_domain)[, `:=`(original_id = character(0), owner_ps_index = integer(0), owner_name = character(0))]
      }
      if (tag_sets) paramtbl[owner_name != "", .tags := pmap(list(.tags, owner_name), function(x, n) c(x, sprintf("set_%s", n)))]
      if (tag_params) paramtbl[, .tags := pmap(list(.tags, original_id), function(x, n) c(x, sprintf("param_%s", n)))]
      private$.tags = paramtbl[, .(tag = unique(unlist(.tags))), keyby = "id"]

      private$.trafos = setkeyv(paramtbl[!map_lgl(.trafo, is.null), .(id, trafo = .trafo)], "id")

      private$.translation = paramtbl[, c("id", "original_id", "owner_ps_index", "owner_name"), with = FALSE]
      setkeyv(private$.translation, "id")
      setindexv(private$.translation, "original_id")

      set(paramtbl, , setdiff(colnames(paramtbl), domain_names_permanent), NULL)
      assert_names(colnames(paramtbl), identical.to = domain_names_permanent)
      setindexv(paramtbl, c("id", "cls", "grouping"))
      private$.params = paramtbl

      private$.children_with_trafos = which(!map_lgl(map(sets, "extra_trafo"), is.null))
      private$.children_with_constraints = which(!map_lgl(map(sets, "constraint"), is.null))

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
        ifelse(n != "", sprintf("%s.%s", n, pnames), pnames),
        self$ids()
      )
      if (length(nameclashes)) {
        stopf("Adding parameter set would lead to nameclashes: %s", str_collapse(nameclashes))
      }

      new_index = length(private$.sets) + 1
      paramtbl = p$params[, `:=`(original_id = id, owner_ps_index = new_index, owner_name = n)]
      if (n != "") set(paramtbl, , "id", sprintf("%s.%s", n, paramtbl$id))

      if (!nrow(paramtbl)) {
        # when paramtbl is empty, use special setup to make sure information about the `.tags` column is present.
        paramtbl = copy(empty_domain)[, `:=`(original_id = character(0), owner_ps_index = integer(0), owner_name = character(0))]
      }
      if (tag_sets && n != "") paramtbl[, .tags := map(.tags, function(x) c(x, sprintf("set_%s", n)))]
      if (tag_params) paramtbl[, .tags := pmap(list(.tags, original_id), function(x, n) c(x, sprintf("param_%s", n)))]
      newtags = paramtbl[, .(tag = unique(unlist(.tags))), by = "id"]
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

      if (!is.null(p$extra_trafo)) {
        entry = if (n == "") length(private$.children_with_trafos) + 1 else n
        private$.children_with_trafos[[entry]] = new_index
      }

      if (!is.null(p$constraint)) {
        entry = if (n == "") length(private$.children_with_constraints) + 1 else n
        private$.children_with_constraints[[entry]] = new_index
      }

      entry = if (n == "") length(private$.sets) + 1 else n
      private$.sets[[n]] = p
      invisible(self)
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

      pvs = Reduce(c, map(ids, function(id_) {
        info = private$.translation[id_, c("original_id", "owner_name"), on = "id"]
        xs = get_private(private$.sets[[info$owner_name]])$.params[info$original_id, "cargo", on = "id"][[1L]][[1]]$disable_in_tune

        if (info$owner_name == "" || is.null(xs)) return(xs)

        set_names(xs, paste0(info$owner_name, ".", names(xs)))
      })) %??% named_list()
      self$set_values(.values = pvs)
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

        # %??% character(0) in case xs is an empty unnamed list
        translate = private$.translation[names(xs) %??% character(0), list(original_id, owner_ps_index), on = "id"]
        set(translate, , j = "values", list(xs))
        for (xtl in split(translate, by = "owner_ps_index")) {
          sets[[xtl$owner_ps_index[[1]]]]$values = set_names(xtl$values, xtl$original_id)
        }
        # clear the values of all sets that are not touched by xs
        for (clearing in setdiff(seq_along(sets), translate$owner_ps_index)) {
          sets[[clearing]]$values = named_list()
        }
      }
      vals = unlist(map(sets, "values"), recursive = FALSE)
      if (!length(vals)) return(named_list())
      vals
    },

    #' @template field_extra_trafo
    extra_trafo = function(f) {
      if (!missing(f)) stop("extra_trafo is read-only in ParamSetCollection.")
      if (!length(private$.children_with_trafos)) return(NULL)
      private$.extra_trafo_explicit

    },

    #' @template field_constraint
    constraint = function(f) {
      if (!missing(f)) stop("constraint is read-only in ParamSetCollection.")
      if (!length(private$.children_with_constraints)) return(NULL)
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
    .sets = NULL,
    .translation = data.table(id = character(0), original_id = character(0), owner_ps_index = integer(0), owner_name = character(0), key = "id"),
    .children_with_trafos = NULL,
    .children_with_constraints = NULL,
    .extra_trafo_explicit = function(x) {
      changed = unlist(lapply(private$.children_with_trafos, function(set_index) {
        changing_ids = private$.translation[J(set_index), id, on = "owner_ps_index"]
        trafo = private$.sets[[set_index]]$extra_trafo
        changing_values_in = x[names(x) %in% changing_ids]
        names(changing_values_in) = private$.translation[names(changing_values_in), original_id]
        # input of trafo() must not be changed after the call; otherwise the trafo would have to `force()` it in
        # some circumstances.
        changing_values = trafo(changing_values_in)
        prefix = names(private$.sets)[[set_index]]
        if (prefix != "") {
          names(changing_values) = sprintf("%s.%s", prefix, names(changing_values))
        }
        changing_values
      }), recursive = FALSE)
      unchanged_ids = private$.translation[!J(private$.children_with_trafos), id, on = "owner_ps_index"]
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
