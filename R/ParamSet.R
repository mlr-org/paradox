#' @title ParamSet
#'
#' @description
#' A set of [Param] objects.
#' Please note that when creating a set or adding to it, the parameters of the
#' resulting set have to be uniquely named with IDs with valid R names.
#' The set also contains a member variable `values` which can be used to store an active configuration /
#' or to partially fix
#' some parameters to constant values (regarding subsequent sampling or generation of designs).
#'
#' @section S3 methods and type converters:
#' * `as.data.table()`\cr
#'   [ParamSet] -> [data.table::data.table()]\cr
#'   Compact representation as datatable. Col types are:\cr
#'     - id: character
#'     - lower, upper: double
#'     - levels: list col, with NULL elements
#'     - special_vals: list col of list
#'     - is_bounded: logical
#'     - default: list col, with NULL elements
#'     - storage_type: character
#'     - tags: list col of character vectors
#' @examples
#' ps = ParamSet$new(
#'   params = list(
#'     ParamDbl$new("d", lower = -5, upper = 5, default = 0),
#'     ParamFct$new("f", levels = letters[1:3])
#'   )
#' )
#'
#' ps$trafo = function(x, param_set) {
#'   x$d = 2^x$d
#'   return(x)
#' }
#'
#' ps$add(ParamInt$new("i", lower = 0L, upper = 16L))
#'
#' ps$check(list(d = 2.1, f = "a", i = 3L))
#' @export
ParamSet = R6Class("ParamSet",
  public = list(

    #' @field assert_values (`logical(1)`)\cr
    #' Should values be checked for validity during assigment to active binding `$values`?
    #' Default is `TRUE`, only switch this off if you know what you are doing.
    assert_values = TRUE,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param params (`list()`)\cr
    #'   List of [Param], named with their respective ID.
    #'   `params` are cloned on-demand, so the input does not need
    #'   to be cloned.
    #' @param set_id (`character(1)`)\cr
    #'   `$set_id` of the resulting `ParamSet`. This determines the
    #'   prefix inside [`ParamSetCollection`]. Default `""` (no prefix).
    #' @param ignore_ids (`logical(1)`)\cr
    #'   Ignore `$id` slots of `params` and instead use the names instead.
    #'   When this is `TRUE`, then `params` must be named.
    #'   Thisdo can be used to create a `ParamSet` with certain [`Param`] `id`s
    #'   without having to clone said [`Param`]s.
    #'   Default `FALSE`.
    initialize = function(params = named_list(), allow_dangling_dependencies = FALSE) {
      assert_list(params, types = "Domain")

      if (length(params)) assert_names(names(params), type = "strict")

      if (!length(params)) {
        paramtbl = copy(empty_domain)
      } else {
        paramtbl = rbindlist(params)
        set(paramtbl, , "id", names(params))
        if (".tags" %in% colnames(paramtbl)) {
          private$.tags = paramtbl[, .(tag = unlist(.tags)), keyby = "id"]
          setindexv(private$.tags, "tag")
        }
      }

      # get initvalues here, so we can delete the relevant column.
      # we only assign it later, so checks can run normally.
      initvalues = if (".init" %in% names(paramtbl)) with(paramtbl[(.init_given), .(.init, id)], set_names(.init, id))

      if (".trafo" %in% names(paramtbl)) {
        private$.trafos = setkeyv(paramtbl[!map_lgl(.trafo, is.null), .(id, trafo = .trafo)], "id")
      }

      if (".requirements" %in% names(paramtbl)) {
        requirements = paramtbl$.requirements
        private$.params = paramtbl  # self$add_dep needs this
       for (row in seq_len(nrow(paramtbl))) {
          for (req in requirements[[row]]) {
            invoke(self$add_dep, id = paramtbl$id[[row]], allow_dangling_dependencies = allow_dangling_dependencies,
              .args = req)
          }
        }
      }

      delendum_cols = setdiff(colnames(paramtbl), domain_names_permanent)
      if (length(delendum_cols)) set(paramtbl, , delendum_cols, NULL)
      assert_names(colnames(paramtbl), identical.to = domain_names_permanent)

      setindexv(paramtbl, c("id", "cls", "grouping"))

      private$.params = paramtbl  # I am 99% sure this is not necessary, but maybe set() creates a copy when deleting too many cols?

      if (!is.null(initvalues)) self$values = initvalues
    },

    #' @description
    #' Retrieves IDs of contained parameters based on some filter criteria
    #' selections, `NULL` means no restriction.
    #' Only returns IDs of parameters that satisfy all conditions.
    #'
    #' @param class (`character()`).
    #' @param tags (`character()`).
    #' @return `character()`.
    ids = function(class = NULL, tags = NULL, any_tags = NULL) {
      assert_character(class, any.missing = FALSE, null.ok = TRUE)
      assert_character(tags, any.missing = FALSE, null.ok = TRUE)
      assert_character(any_tags, any.missing = FALSE, null.ok = TRUE)

      if (is.null(class) && is.null(tags) && is.null(any_tags)) {
        return(private$.params$id)
      }
      ptbl = if (is.null(class)) private$.params else private$.params[cls %in% class, .(id)]
      if (is.null(tags) && is.null(any_tags)) {
        return(ptbl$id)
      }
      tagtbl = private$.tags[ptbl, nomatch = 0]
      idpool = if (is.null(any_tags)) list() else list(tagtbl[tag %in% any_tags, id])
      idpool = c(idpool, lapply(tags, function(t) tagtbl[t, id, on = "tag", nomatch = 0]))
      Reduce(intersect, idpool)
    },

    #' @description
    #' Retrieves parameter values based on some selections, `NULL` means no
    #' restriction and is equivalent to `$values`.
    #' Only returns values of parameters that satisfy all conditions.
    #'
    #' @param class (`character()`).
    #' @param is_bounded (`logical(1)`).
    #' @param tags (`character()`).
    #' @param type (`character(1)`)\cr
    #' Return values `with_token`, `without_token` or `only_token`?
    #' @param check_required (`logical(1)`)\cr
    #' Check if all required parameters are set?
    #' @return Named `list()`.
    get_values = function(class = NULL, tags = NULL, any_tags = NULL,
      type = "with_token", check_required = TRUE, remove_dependencies = TRUE) {
      assert_choice(type, c("with_token", "without_token", "only_token"))

      assert_flag(check_required)

      values = self$values
      ns = names(values)

      if (type == "without_token") {
        values = discard(values, is, "TuneToken")
      } else if (type == "only_token") {
        values = keep(values, is, "TuneToken")
      }

      if (check_required) {
        required = setdiff(self$ids(tags = "required"), ns)
        if (length(required) > 0L) {
          stop(sprintf("Missing required parameters: %s", str_collapse(required)))
        }
      }

      deps = self$deps
      if (remove_dependencies && nrow(deps)) {
        for (j in seq_row(deps)) {
          p1id = deps$id[[j]]
          p2id = deps$on[[j]]
          cond = deps$cond[[j]]
          if (p1id %in% ns && !inherits(values[[p2id]], "TuneToken") && !isTRUE(condition_test(cond, values[[p2id]]))) {
            values[p1id] = NULL
          }
        }
      }

      values[match(self$ids(class = class, tags = tags, any_tags = any_tags), names(values), nomatch = 0)]
    },

    #' @description
    #' Allows to to modify (and overwrite) or replace the parameter values.
    #' Per default already set values are being kept unless new values are being provided.
    #'
    #' @param ... (any)\cr
    #'   Named parameter values.
    #' @param .values (named `list()`)\cr
    #'   Named list with parameter values. Names must not already appear in `...`.
    #' @param .insert (`logical(1)`)\cr
    #'   Whether to insert the values (old values are being kept, if not overwritten), or to
    #'   replace all values. Default is TRUE.
    #'
    set_values = function(..., .values = list(), .insert = TRUE) {
      dots = list(...)
      assert_list(dots, names = "unique")
      assert_list(.values, names = "unique")
      assert_disjunct(names(dots), names(.values))
      new_values = insert_named(dots, .values)
      if (.insert) {
        discarding = names(keep(new_values, is.null))
        new_values = insert_named(self$values, new_values)
        new_values = new_values[names(new_values) %nin% discarding]
      }
      self$values = new_values
      invisible(self)
    },

    trafo = function(x, param_set = self) {
      if (is.data.frame(x)) x = as.list(x)
      assert_list(x, names = "unique")
      trafos = private$.trafos[names(x), .(id, trafo), nomatch = 0]
      trafos[, value := x[id]]
      if (nrow(trafos)) {
        transformed = pmap(trafos, function(id, trafo, value) trafo(value))
        x = insert_named(x, set_names(transformed, trafos$id))
      }
      extra_trafo = self$extra_trafo
      if (!is.null(extra_trafo)) {
        # need to give the input of extra_trafo a different name than the output; otherwise the user would have to
        # "force()" the x-argument of extra_trafo.
        xin = x
        if (test_function(extra_trafo, args = c("x", "param_set"))) {
          x = extra_trafo(x = xin, param_set = param_set)
        } else {
          x = extra_trafo(xin)
        }
      }
      x
    },

    # assert_value: used internally, to avoid touble-asserts
    test_constraint = function(x, assert_value = TRUE) {
      if (assert_value) self$assert(x)
      assert_flag(is.null(private$.constraint) || private$.constraint(x))
    },

    test_constraint_dt = function(x, assert_value = TRUE) {
      assert_data_table(x)
      if (assert_value) self$assert_dt(x)
      map_lgl(transpose(x), self$test_constraint, assert_value = FALSE)
    },

    #' @description
    #' \pkg{checkmate}-like check-function. Takes a named list.
    #' A point x is feasible, if it configures a subset of params,
    #' all individual param constraints are satisfied and all dependencies are satisfied.
    #' Params for which dependencies are not satisfied should not be part of `x`.
    #'
    #' @param xs (named `list()`).
    #' @return If successful `TRUE`, if not a string with the error message.
    check = function(xs, check_strict = FALSE) {
      assert_flag(check_strict)
      ok = check_list(xs, names = "unique")
      if (!isTRUE(ok)) {
        return(ok)
      }

      params = private$.params
      ns = names(xs)
      ids = private$.params$id

      extra = wf(ns %nin% ids)
      if (length(extra)) {
        return(sprintf("Parameter '%s' not available.%s", ns[extra], did_you_mean(extra, ids)))
      }

      if (length(xs) && test_list(xs, types = "TuneToken")) {
        tunecheck = tryCatch({
          private$get_tune_ps(xs)
          TRUE
        }, error = function(e) paste("tune token invalid:", conditionMessage(e)))
        if (!isTRUE(tunecheck)) return(tunecheck)
      }

      # check each parameter group's feasibility
      xs_nontune = discard(xs, inherits, "TuneToken")

      # need to make sure we index w/ empty character instead of NULL
      params = params[names(xs_nontune) %??% character(0), on = "id"]

      set(params, , "values", list(xs_nontune))
      pgroups = split(params, by = c("cls", "grouping"))
      checkresults = map(pgroups, function(x) {
        domain_check(set_class(x, c(x$cls[[1]], "Domain", class(x))), x$values)
      })
      checkresults = discard(checkresults, isTRUE)
      if (length(checkresults)) {
        return(str_collapse(checkresults, sep = "\n"))
      }

      if (check_strict) {
        required = setdiff(self$ids(tags = "required"), ns)
        if (length(required) > 0L) {
          return(sprintf("Missing required parameters: %s", str_collapse(required)))
        }
        return(self$check_dependencies(xs))
        if (!self$test_constraint(xs, assert_value = FALSE)) return(sprintf("Constraint not fulfilled."))
      }

      TRUE # we passed all checks
    },

    check_dependencies = function(xs) {
      deps = self$deps
      if (!nrow(deps)) return(TRUE)
      params = private$.params
      ns = names(xs)
      errors = pmap(deps[id %in% ns], function(id, on, cond) {
        onval = xs[[on]]
        if (inherits(xs[[id]], "TuneToken") || inherits(onval, "TuneToken")) return(NULL)

        # we are ONLY ok if:
        # - if 'id' is there, then 'on' must be there, and cond must be true
        # - if 'id' is not there. but that is skipped (deps[id %in% ns] filter)
        if (on %in% ns && condition_test(cond, onval)) return(NULL)
        msg = sprintf("%s: can only be set if the following condition is met '%s'.",
          id, condition_as_string(cond, on))
        if (is.null(onval)) {
          msg = sprintf(paste("%s Instead the parameter value for '%s' is not set at all.",
              "Try setting '%s' to a value that satisfies the condition"), msg, on, on)
        } else {
          msg = sprintf("%s Instead the current parameter value is: %s == %s", msg, on, as_short_string(onval))
        }
        msg
      })
      errors = unlist(errors)
      if (!length(errors)) return(TRUE)
      str_collapse(errors, sep = "\n")
    },

    #' @description
    #' \pkg{checkmate}-like test-function. Takes a named list.
    #' A point x is feasible, if it configures a subset of params,
    #' all individual param constraints are satisfied and all dependencies are satisfied.
    #' Params for which dependencies are not satisfied should not be part of `x`.
    #'
    #' @param xs (named `list()`).
    #' @return If successful `TRUE`, if not `FALSE`.
    test = function(xs, check_strict = FALSE) makeTest(self$check(xs, check_strict = check_strict)),

    #' @description
    #' \pkg{checkmate}-like assert-function. Takes a named list.
    #' A point x is feasible, if it configures a subset of params,
    #' all individual param constraints are satisfied and all dependencies are satisfied.
    #' Params for which dependencies are not satisfied should not be part of `x`.
    #'
    #' @param xs (named `list()`).
    #' @param .var.name (`character(1)`)\cr
    #'   Name of the checked object to print in error messages.\cr
    #'   Defaults to the heuristic implemented in [vname][checkmate::vname].
    #' @return If successful `xs` invisibly, if not an error message.
    assert = function(xs, check_strict = FALSE, .var.name = vname(xs)) makeAssertion(xs, self$check(xs, check_strict = check_strict), .var.name, NULL),  # nolint

    #' @description
    #' \pkg{checkmate}-like check-function. Takes a [data.table::data.table]
    #' where rows are points and columns are parameters. A point x is feasible,
    #' if it configures a subset of params, all individual param constraints are
    #' satisfied and all dependencies are satisfied. Params for which
    #' dependencies are not satisfied should be set to `NA` in `xdt`.
    #'
    #' @param xdt ([data.table::data.table] | `data.frame()`).
    #' @return If successful `TRUE`, if not a string with the error message.
    check_dt = function(xdt, check_strict = FALSE) {
      xss = map(transpose_list(xdt), discard, is.na)
      msgs = list()
      for (i in seq_along(xss)) {
        xs = xss[[i]]
        ok = self$check(xs, check_strict = check_strict)
        if (!isTRUE(ok)) {
          return(ok)
        }
      }
      TRUE
    },

    #' @description
    #' \pkg{checkmate}-like test-function (s. `$check_dt()`).
    #'
    #' @param xdt ([data.table::data.table]).
    #' @return If successful `TRUE`, if not `FALSE`.
    test_dt = function(xdt) makeTest(res = self$check_dt(xdt, check_strict = check_strict)),

    #' @description
    #' \pkg{checkmate}-like assert-function (s. `$check_dt()`).
    #'
    #' @param xdt ([data.table::data.table]).
    #' @param .var.name (`character(1)`)\cr
    #'   Name of the checked object to print in error messages.\cr
    #'   Defaults to the heuristic implemented in [vname][checkmate::vname].
    #' @return If successful `xs` invisibly, if not an error message.
    assert_dt = function(xdt, check_strict = FALSE, .var.name = vname(xdt)) makeAssertion(xdt, self$check_dt(xdt, check_strict = check_strict), .var.name, NULL), # nolint

    qunif = function(x) {
      assert(check_data_frame(x, types = "numeric", min.cols = 1), check_matrix(x, mode = "numeric", min.cols = 1))
      if (is.matrix(x)) {
        qassert(x, "N[0,1]")
      } else {
        qassertr(x, "N[0,1]")
        x = as.matrix(x)
      }
      assert_names(colnames(x), type = "unique", subset.of = private$.params$id)

      x = t(x)
      params = private$.params[rownames(x), on = "id"]
      params$result = list()
      params[, result := list(as.list(as.data.frame(t(matrix(domain_qunif(recover_domain(.SD, .BY), x[id, ]), nrow = .N))))),
        by = c("cls", "grouping")]
      as.data.table(set_names(params$result, params$id))
    },

    #' @description
    #' get the [`Domain`] object that could be used to create a given parameter.
    #'
    #' @param id (`character(1)`).
    #' @return [`Domain`].
    get_domain = function(id) {
      assert_string(id)
      paramrow = private$.params[id, on = "id", nomatch = NULL]

      if (!nrow(paramrow)) stopf("No param with id '%s'", id)

      vals = self$values
      depstbl = self$deps[id, .(on, cond), on = "id", nomatch = 0]
      paramrow[, `:=`(
        .tags = list(private$.tags[id, tag, nomatch = 0]),
        .trafo = private$.trafos[id, trafo],
        .requirements = list(if (nrow(depstbl)) transpose_list(depstbl)),  # NULL if no deps
        .init_given = id %in% names(vals),
        .init = unname(vals[id]))
      ]

      set_class(paramrow, c(paramrow$cls, "Domain", class(paramrow)))
    },

    #' @description
    #' Create a new `ParamSet` restricted to the passed IDs.
    #' @param ids (`character()`).
    #' @return `ParamSet`.
    subset = function(ids, allow_dangling_dependencies = FALSE, keep_constraint = TRUE) {
      param_ids = private$.params$id

      assert_subset(ids, param_ids)
      deps = self$deps
      if (!allow_dangling_dependencies && nrow(deps)) { # check that all required / leftover parents are still in new ids
        on = NULL
        parents = unique(deps[ids, on, on = "id", nomatch = NULL])
        pids_not_there = setdiff(parents, ids)
        if (length(pids_not_there) > 0L) {
          stopf(paste0("Subsetting so that dependencies on params exist which would be gone: %s.",
              "\nIf you still want to subset, set allow_dangling_dependencies to TRUE."), str_collapse(pids_not_there))
        }
      }
      result = ParamSet$new()


      result$.__enclos_env__$private$.params = setindexv(private$.params[ids, on = "id"], c("id", "cls", "grouping"))
      result$.__enclos_env__$private$.trafos = setkeyv(private$.trafos[ids, on = "id", nomatch = NULL], "id")
      result$.__enclos_env__$private$.tags = setkeyv(private$.tags[ids, on = "id", nomatch = NULL], "id")
      result$assert_values = FALSE
      result$deps = deps[ids, on = "id", nomatch = NULL]
      if (keep_constraint) result$constraint = self$constraint
      # TODO: ParamSetCollection trafo currently drags along the entire original paramset in its environment
      result$extra_trafo = self$extra_trafo
      # restrict to ids already in pvals
      values = self$values
      result$values = values[match(ids, names(values), nomatch = 0)]
      result$assert_values = TRUE
      result
    },

    subspaces = function(ids = private$.params$id) {
      values = self$values
      sapply(ids, simplify = FALSE, function(get_id) {
        result = ParamSet$new()
        result$extra_trafo = self$extra_trafo
        # constraint make no sense here, basically by definition
        result$.__enclos_env__$private$.params = setindexv(private$.params[get_id, on = "id"], c("id", "cls", "grouping"))
        # setkeyv not strictly necessary since get_id is scalar, but we do it for consistency
        result$.__enclos_env__$private$.trafos = setkeyv(private$.trafos[get_id, on = "id", nomatch = NULL], "id")
        result$.__enclos_env__$private$.tags = setkeyv(private$.tags[get_id, on = "id", nomatch = NULL], "id")
        result$assert_values = FALSE
        result$values = values[match(get_id, names(values), nomatch = 0)]
        result$assert_values = TRUE
        result
      })
    },

    #' @description
    #' Create a `ParamSet` from this object, even if this object itself is not
    #' a `ParamSet`.
    flatten = function() self$subset(private$.params$id, allow_dangling_dependencies = TRUE),

    #' @description
    #' Construct a [`ParamSet`] to tune over. Constructed from [`TuneToken`] in `$values`, see [`to_tune()`].
    #'
    #' @param  values (`named list`): optional named list of [`TuneToken`] objects to convert, in place of `$values`.
    search_space = function(values = self$values) {
      assert_list(values)
      assert_names(names(values), subset.of = self$ids())
      pars = private$get_tune_ps(values)
      on = NULL  # pacify static code check
      dangling_deps = pars$deps[!pars$ids(), on = "on"]
      if (nrow(dangling_deps)) {
        stopf("Dangling dependencies not allowed: Dependencies on %s dangling.", str_collapse(dangling_deps$on))
      }
      pars
    },

    #' @description
    #' Adds a dependency to this set, so that param `id` now depends on param `on`.
    #'
    #' @param id (`character(1)`).
    #' @param on (`character(1)`).
    #' @param allow_dangling_dependencies (`logical(1)`): Whether to allow dependencies on parameters that are not present.
    #' @param cond ([Condition]).
    add_dep = function(id, on, cond, allow_dangling_dependencies = FALSE) {
      params = private$.params
      ids = params$id
      assert_choice(id, ids)
      if (allow_dangling_dependencies) assert_string(on) else assert_choice(on, ids)
      assert_class(cond, "Condition")
      if (id == on) {
        stopf("A param cannot depend on itself!")
      }

      if (on %in% ids) {  # not necessarily true when allow_dangling_dependencies
        feasible_on_values = map_lgl(cond$rhs, function(x) domain_test(self$get_domain(on), list(x)))
        if (any(!feasible_on_values)) {
          stopf("Condition has infeasible values for %s: %s", on, str_collapse(cond$rhs[!feasible_on_values]))
        }
      }
      private$.deps = rbind(private$.deps, data.table(id = id, on = on, cond = list(cond)))
      invisible(self)
    },

    #' @description
    #' Helper for print outputs.
    #' @param ... (ignored).
    format = function() {
      sprintf("<%s(%s)>", class(self)[[1L]], self$length)
    },

    #' @description
    #' Printer.
    #'
    #' @param ... (ignored).
    #' @param hide_cols (`character()`)\cr
    #'   Which fields should not be printed? Default is `"levels"`,
    #'   `"is_bounded"`, `"special_vals"`, `"tags"`, and `"storage_type"`.
    # printer, prints the set as a datatable, with the option to hide some cols
    print = function(..., hide_cols = c("levels", "is_bounded", "special_vals", "tags", "storage_type")) {
      catf(format(self))
      d = as.data.table(self)
      if (!nrow(d)) {
        catf("Empty.")
      } else {
        assert_subset(hide_cols, names(d))
        deps = self$deps
        if (nrow(deps)) { # add a nice extra charvec-col to the tab, which lists all parents-ids
          on = NULL
          dd = deps[, list(parents = list(unlist(on))), by = "id"]
          d = merge(d, dd, on = "id", all.x = TRUE)
        }
        v = named_list(d$id) # add values to last col of print-dt as list col
        v = insert_named(v, self$values)
        d$value = list(v)
        print(d[, setdiff(colnames(d), hide_cols), with = FALSE])
      }
      if (self$has_trafo) {
        catf("Trafo is set.")
      } # printing the trafa functions sucks (can be very long). dont see a nother option then to suppress it for now
    }
  ),

  active = list(

    #' @field data (`data.table`) `data.table` representation of the `ParamSet`.
    data = function(v) {
      if (!missing(v)) stop("data is read-only")
      private$.params[, list(id, class = cls, lower, upper, levels, nlevels = self$nlevels,
        is_bounded = self$is_bounded, special_vals, default, storage_type = self$storage_type, tags = self$tags)]
    },

    #' @template field_values
    values = function(xs) {
      if (missing(xs)) {
        return(private$.values)
      }
      if (self$assert_values) {
        self$assert(xs)
      }
      if (length(xs) == 0L) {
        xs = named_list()
      } else if (self$assert_values) {  # this only makes sense when we have asserts on
        # convert all integer params really to storage type int, move doubles to within bounds etc.
        # solves issue #293, #317
        nontt = discard(xs, inherits, "TuneToken")

        sanitized = set(private$.params[names(nontt), on = "id"], , "values", list(nontt))[
          !pmap_lgl(list(special_vals, values), has_element),
          .(id, values = domain_sanitize(recover_domain(.SD, .BY), values)), by = c("cls", "grouping")]
        xs = insert_named(xs, with(sanitized, set_names(values, id)))
      }
      # store with param ordering, return value with original ordering
      private$.values = xs[match(private$.params$id, names(xs), nomatch = 0)]
      xs
    },

    #' @template field_tags
    tags = function(v) {
      if (!missing(v)) {
        assert_names(names(v), permutation.of = private$.params$id)
        assert_list(v, any.missing = FALSE, types = "character")
        private$.tags = data.table(id = rep(names(v), map_int(v, length)), tag = unlist(v), key = "id")
        setindexv(private$.tags, "tag")
        # return value with original ordering
        return(v)
      }
      insert_named(named_list(private$.params$id, character(0)), with(private$.tags[, list(tag = list(tag)), by = "id"], set_names(tag, id)))
    },

    #' @template field_params
    params = function(rhs) {
      if (!missing(rhs)) {
        stop("params is read-only.")
      }

      result = copy(private$.params)
      result[, .tags := list(self$tags)]
      result[private$.trafos, .trafo := list(trafo), on = "id"]
      result[self$deps, .requirements := transpose_list(.(on, cond)), on = "id"]
      vals = self$values
      result[, `:=`(
        .init_given = id %in% names(vals),
        .init = unname(vals[id])
      )]

      result[]
    },

    #' @field domains (named `list` of [`Domain`])
    #' List of [`Domain`] objects that could be used to initialize this `ParamSet`.
    domains = function(rhs) {
      if (!missing(rhs)) {
        stop("domains is read-only.")
      }
      nm = self$ids()
      set_names(map(nm, self$get_domain), nm)
    },


    #' @field extra_trafo (`function(x, param_set)`)\cr
    #' Transformation function. Settable.
    #' User has to pass a `function(x)`, of the form\cr
    #' (named `list()`, [ParamSet]) -> named `list()`.\cr
    #' The function is responsible to transform a feasible configuration into another encoding,
    #' before potentially evaluating the configuration with the target algorithm.
    #' For the output, not many things have to hold.
    #' It needs to have unique names, and the target algorithm has to accept the configuration.
    #' For convenience, the self-paramset is also passed in, if you need some info from it (e.g. tags).
    #' Is NULL by default, and you can set it to NULL to switch the transformation off.
    extra_trafo = function(f) {
      if (missing(f)) {
        private$.extra_trafo
      } else {
        assert(check_function(f, args = c("x", "param_set"), null.ok = TRUE), check_function(f, args = "x", null.ok = TRUE))
        private$.extra_trafo = f
      }
    },

    constraint = function(f) {
      if (missing(f)) {
        private$.constraint
      } else {
        assert_function(f, args = "x", null.ok = TRUE)
        private$.constraint = f
      }
    },


    #' @template field_deps
    deps = function(v) {
      if (missing(v)) {
        private$.deps
      } else {
        assert_data_table(v)
        if (nrow(v)) {
          # only test for things without which things would seriously break
          assert_names(colnames(v), identical.to = c("id", "on", "cond"))
          assert_subset(v$id, private$.params$id)
          assert_character(v$on, any.missing = FALSE)
          assert_list(v$cond, types = "Condition", any.missing = FALSE)
        } else {
          v = data.table(id = character(0), on = character(0), cond = list())  # make sure we have the right columns
        }
        private$.deps = v
      }
    },

    ############################
    # ParamSet flags

    #' @field length (`integer(1)`)\cr Number of contained [Param]s.
    length = function() nrow(private$.params),
    #' @field is_empty (`logical(1)`)\cr Is the `ParamSet` empty? Named with parameter IDs.
    is_empty = function() nrow(private$.params) == 0L,
    #' @field has_trafo (`logical(1)`)\cr Whether a `trafo` function is present, in parameters or in `extra_trafo`.
    has_trafo = function() !is.null(self$extra_trafo) || nrow(private$.trafos),
    #' @field has_extra_trafo (`logical(1)`)\cr Whether `extra_trafo` is set.
    has_extra_trafo = function() !is.null(self$extra_trafo),
    #' @field has_deps (`logical(1)`)\cr Whether the parameter dependencies are present
    has_deps = function() nrow(self$deps) > 0L,
    #' @field has_constraint (`logical(1)`)\cr Whether parameter constraint is set.
    has_constraint = function() !is.null(private$.constraint),
    #' @field all_numeric (`logical(1)`)\cr Is `TRUE` if all parameters are [ParamDbl] or [ParamInt].
    all_numeric = function() all(self$is_number),
    #' @field all_categorical (`logical(1)`)\cr Is `TRUE` if all parameters are [ParamFct] and [ParamLgl].
    all_categorical = function() all(self$is_categ),
    #' @field all_bounded (`logical(1)`)\cr Is `TRUE` if all parameters are bounded.
    all_bounded = function() all(self$is_bounded),

    ############################
    # Per-Parameter properties

    #' @field class (named `character()`)\cr Classes of contained parameters. Named with parameter IDs.
    class = function() with(private$.params, set_names(cls, id)),
    #' @field lower (named `double()`)\cr Lower bounds of numeric parameters (`NA` for non-numerics). Named with parameter IDs.
    lower = function() with(private$.params, set_names(lower, id)),
    #' @field upper (named `double()`)\cr Upper bounds of numeric parameters (`NA` for non-numerics). Named with parameter IDs.
    upper = function() with(private$.params, set_names(upper, id)),
    #' @field levels (named `list()` of `character`)\cr Allowed levels of categorical parameters (`NULL` for non-categoricals).
    #' Named with parameter IDs.
    levels = function() with(private$.params, set_names(levels, id)),
    #' @field storage_type (`character()`)\cr Data types of parameters when stored in tables. Named with parameter IDs.
    storage_type = function() with(private$.params, set_names(storage_type, id)),
    #' @field special_vals (named `list()` of `list()`)\cr Special values for all parameters. Named with parameter IDs.
    special_vals = function() with(private$.params, set_names(special_vals, id)),
    #' @field default (named `list()`)\cr Default values of all parameters. If no default exists, element is not present.
    #' Named with parameter IDs.
    default = function() with(private$.params[!map_lgl(default, is_nodefault), .(default, id)], set_names(default, id)),
    #' @field has_trafo_param (`logical()`)\cr Whether `trafo` is set for any parameter.
    has_trafo_param = function() with(private$.params, set_names(id %in% private$.trafos$id, id)),
    #' @field is_logscale (`logical()`)\cr Whether `trafo` was set to `logscale` during construction.\cr
    #' Note that this only refers to the `logscale` flag set during construction, e.g. `p_dbl(logscale = TRUE)`.
    #' If the parameter was set to logscale manually, e.g. through `p_dbl(trafo = exp)`,
    #' this `is_logscale` will be `FALSE`.
    is_logscale = function() with(private$.params, set_names(cls %in% c("ParamDbl", "ParamInt") & cargo == "logscale", id)),

    ############################
    # Per-Parameter class properties (S3 method call)

    #' @field nlevels (named `integer()`)\cr Number of distinct levels of parameters. `Inf` for double parameters or unbounded integer parameters.
    #' Named with param IDs.
    nlevels = function() {
      tmp = private$.params[,
        list(id, nlevels = domain_nlevels(recover_domain(.SD, .BY))),
        by = c("cls", "grouping")
      ]
      with(tmp[private$.params$id, on = "id"], set_names(nlevels, id))
    },

    #' @field is_number (named `logical()`)\cr Whether parameter is [ParamDbl] or [ParamInt]. Named with parameter IDs.
    is_number = function() {
      tmp = private$.params[,
        list(id, is_number = rep(domain_is_number(recover_domain(.SD, .BY)), .N)),
        by = c("cls", "grouping")
      ]
      with(tmp[private$.params$id, on = "id"], set_names(is_number, id))
    },

    #' @field is_categ (named `logical()`)\cr Whether parameter is [ParamFct] or [ParamLgl]. Named with parameter IDs.
    is_categ = function() {
      tmp = private$.params[,
        list(id, is_categ = rep(domain_is_categ(recover_domain(.SD, .BY)), .N)),
        by = c("cls", "grouping")
      ]
      with(tmp[private$.params$id, on = "id"], set_names(is_categ, id))
    },

    #' @field is_bounded (named `logical()`)\cr Whether parameters have finite bounds. Named with parameter IDs.
    is_bounded = function() {
      tmp = private$.params[,
        list(id, is_bounded = domain_is_bounded(recover_domain(.SD, .BY))),
        by = c("cls", "grouping")
      ]
      with(tmp[private$.params$id, on = "id"], set_names(is_bounded, id))
    }
  ),

  private = list(
    .extra_trafo = NULL,
    .constraint = NULL,
    .params = NULL,
    .values = named_list(),
    .tags = data.table(id = character(0L), tag = character(0), key = "id"),
    .deps = data.table(id = character(0L), on = character(0L), cond = list()),
    .trafos = data.table(id = character(0L), trafo = list(), key = "id"),

    get_tune_ps = function(values) {
      values = keep(values, inherits, "TuneToken")
      if (!length(values)) return(ParamSet$new())
      params = map(names(values), function(pn) {
        domain = private$.params[pn, on = "id"]
        set_class(domain, c(domain$cls, "Domain", class(domain)))
      })
      names(params) = names(values)

      # package-internal S3 fails if we don't call the function indirectly here
      partsets = pmap(list(values, params), function(...) tunetoken_to_ps(...))
      pars = ps_union(partsets)  # partsets does not have names here, wihch is what we want.

      names(partsets) = names(values)
      idmapping = map(partsets, function(x) x$ids())

      # only add the dependencies that are also in the tuning PS
      on = id = NULL  # pacify static code check
      pmap(self$deps[id %in% names(idmapping) & on %in% names(partsets), c("on", "id", "cond")], function(on, id, cond) {
        onpar = partsets[[on]]
        if (onpar$has_trafo || !identical(onpar$ids(), on)) {
          # cannot have dependency on a parameter that is being trafo'd
          return(NULL)
        }
        # remove infeasible values from condition
        cond$rhs = keep(cond$rhs, function(x) partsets[[on]]$test(set_names(list(x), on)))
        if (!length(cond$rhs)) {
          # no value is feasible, but there may be a trafo that fixes this
          # so we are forgiving here.
          return(NULL)
        }
        for (idname in idmapping[[id]]) {
          pars$add_dep(idname, on, cond)
        }
      })
      pars
    },

    deep_clone = function(name, value) {
      switch(name,
        .deps = copy(value),
        .values = map(value, function(x) {
          # clones R6 objects in values, leave other things as they are

          # safely get .__enclos_env, errors if packages overwrite `$` i.e. in reticulate.
          # https://github.com/rstudio/reticulate/blob/master/R/python.R L 343
          if (is.environment(x) && !is.null(tryCatch(x$.__enclos_env__, error = function(e) NULL))) {
            x$clone(deep = TRUE)
          } else {
            x
          }
        }),
        value
      )
    }
  )
)

recover_domain = function(sd, by) {
  domain = as.data.table(c(by, sd))
  class(domain) = c(domain$cls, "Domain", class(domain))
  domain
}

#' @export
as.data.table.ParamSet = function(x, ...) { # nolint
  x$data
}

#' @export
rd_info.ParamSet = function(obj, descriptions = character(), ...) { # nolint
  if (obj$length == 0L) {
    return("Empty ParamSet")
  }

  params = as.data.table(obj)[, c("id", "storage_type", "default", "lower", "upper", "levels"), with = FALSE]
  cargo = obj$params$cargo

  if (length(descriptions)) {
    params = merge(params, enframe(descriptions, name = "id", value = "description"), all.x = TRUE, by = "id")
    description = NULL
    params[is.na(description), description := ""]
    setcolorder(params, c("id", "description"))
  }
  is_default = map_lgl(params$default, inherits, "NoDefault")
  is_uty = params$storage_type == "list"
  set(params, i = which(is_uty & !is_default), j = "default",
      value = map(cargo[!is_default & is_uty], function(x) x$repr))
  set(params, i = which(is_uty), j = "storage_type", value = list("untyped"))
  set(params, i = which(is_default), j = "default", value = list("-"))

  if (!allMissing(params$lower) || !allMissing(params$upper)) {
    set(params, j = "range", value = pmap_chr(params[, c("lower", "upper"), with = FALSE], rd_format_range))
  }
  remove_named(params, c("lower", "upper"))

  if (all(lengths(params$levels) == 0L)) {
    remove_named(params, "levels")
  } else {
    set(params, j = "levels", value = map_chr(params$levels, str_collapse, n = 10L))
  }
  setnames(params, "storage_type", "type")
  x = c("", knitr::kable(params, col.names = capitalize(names(params))))
  paste(x, collapse = "\n")
}

