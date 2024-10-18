#' @title ParamSet
#'
#' @description
#' An object representing the space of possible parametrizations of a function or another object.
#' `ParamSet`s are used on the side of objects being parameterized, where they function as a configuration space determining the set of possible configurations accepted by these objects.
#' They can also be used to specify search spaces for optimization, indicating the set of legal configurations to try out.
#' It is often convenient to generate search spaces from configuration spaces, which can be done using the `$search_space()` method in combination with `to_tune()` / [`TuneToken`] objects.
#'
#' Individual dimensions of a `ParamSet` are specified by [`Domain`] objects, created as [`p_dbl()`], [`p_lgl()`] etc.
#' The field `$values` can be used to store an active configuration or to partially fix
#' some parameters to constant values -- the precise effect can be determined by the object being parameterized.
#'
#' Constructing a `ParamSet` can be done using `ParamSet$new()` in combination with a named list of [`Domain`] objects.
#' This route is recommended when the set of dimensions (i.e. the members of this named list) is dynamically created, such as when the number of parameters is variable.
#' `ParamSet`s can also be created using the [`ps()`] shorthand, which is the recommended way when the set of parameters is fixed.
#' In practice, the majority of cases where a `ParamSet` is created, the [`ps()`] should be used.
#'
#' @section S3 methods and type converters:
#' * `as.data.table()`\cr
#'   `ParamSet` -> [data.table::data.table()]\cr
#'   Compact representation as datatable. Col types are:\cr
#'     - id: character
#'     - class: character
#'     - lower, upper: numeric
#'     - levels: list col, with NULL elements
#'     - nlevels: integer valued numeric
#'     - is_bounded: logical
#'     - special_vals: list col of list
#'     - default: list col
#'     - storage_type: character
#'     - tags: list col of character vectors
#' @examples
#' pset = ParamSet$new(
#'   params = list(
#'     d = p_dbl(lower = -5, upper = 5, default = 0, trafo = function(x) 2^x),
#'     f = p_fct(levels = letters[1:3])
#'   )
#' )
#'
#' # alternative, recommended way of construction in this case since the
#' # parameter list is not dynamic:
#' pset = ps(
#'   d = p_dbl(lower = -5, upper = 5, default = 0, trafo = function(x) 2^x),
#'   f = p_fct(levels = letters[1:3])
#' )
#'
#' pset$check(list(d = 2.1, f = "a"))
#'
#' pset$check(list(d = 2.1, f = "d"))
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
    #' @param params (named `list()`)\cr
    #'   List of [`Domain`], named with their respective ID.
    #' @param allow_dangling_dependencies (`character(1)`)\cr
    #'   Whether dependencies depending on parameters that are not present should be allowed. A parameter `x` having
    #'   `depends = y == 0` if `y` is not present would usually throw an error, but if dangling
    #'   dependencies are allowed, the dependency is added regardless. This is mainly for internal
    #'   use.
    initialize = function(params = named_list(), allow_dangling_dependencies = FALSE) {
      assert_list(params, types = "Domain")

      if (length(params)) assert_names(names(params), type = "strict")

      if (!length(params)) {
        paramtbl = copy(empty_domain)
      } else {
        paramtbl = rbindlist(params)
        set(paramtbl, , "id", names(params))
      }
      if (".tags" %in% colnames(paramtbl)) {
        # fastest way to init a data.table
        private$.tags = structure(list(
            id = rep(paramtbl$id, lengths(paramtbl$.tags)),
            tag = unlist(paramtbl$.tags, use.names = FALSE)
          ), class = c("data.table", "data.frame")
        )
      } else {
        private$.tags = structure(list(
            id = character(0), tag = character(0)
          ), class = c("data.table", "data.frame")
        )
      }
      setkeyv(private$.tags, "id")
      setindexv(private$.tags, "tag")


      # get initvalues here, so we can delete the relevant column.
      # we only assign it later, so checks can run normally.
      .init_given = .init = NULL  # pacify checks
      initvalues = if (".init" %in% names(paramtbl)) structure(
          paramtbl$.init[paramtbl$.init_given],
          names = paramtbl$id[paramtbl$.init_given]
        )

      if (".trafo" %in% names(paramtbl)) {
        trafo_given = lengths(paramtbl$.trafo) != 0
        private$.trafos = structure(list(
            id = paramtbl$id[trafo_given],
            trafo = paramtbl$.trafo[trafo_given]
          ), class = c("data.table", "data.frame")
        )
      } else {
        private$.trafos = structure(list(
            id = character(0), trafo = list()
          ), class = c("data.table", "data.frame")
        )
      }
      setkeyv(private$.trafos, "id")

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
    #' @param class (`character()`)\cr
    #'   Typically a subset of `"ParamDbl"`, `"ParamInt"`, `"ParamFct"`, `"ParamLgl"`, `"ParamUty"`.
    #'   Other classes are possible if implemented by 3rd party packages.
    #'   Return only IDs of dimensions with the given class.
    #' @param tags (`character()`).
    #'   Return only IDs of dimensions that have *all* tags given in this argument.
    #' @param any_tags (`character()`).
    #'   Return only IDs of dimensions that have at least one of the tags given in this argument.
    #' @return `character()`.
    ids = function(class = NULL, tags = NULL, any_tags = NULL) {
      assert_character(class, any.missing = FALSE, null.ok = TRUE)
      assert_character(tags, any.missing = FALSE, null.ok = TRUE)
      assert_character(any_tags, any.missing = FALSE, null.ok = TRUE)

      if (is.null(class) && is.null(tags) && is.null(any_tags)) {
        return(private$.params$id)
      }
      if (length(tags) == 1 && is.null(any_tags) && is.null(class)) {
        # very typical case: only 'tags' is given.
        rv = private$.tags$id[private$.tags$tag == tags]
        # keep original order
        return(rv[match(private$.params$id, rv, nomatch = 0)])
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
    #' @param class (`character()`). See `$ids()`.
    #' @param tags (`character()`). See `$ids()`.
    #' @param any_tags (`character()`). See `$ids()`.
    #' @param type (`character(1)`)\cr
    #'   Return values `"with_token"` (i.e. all values),
    #    `"without_token"` (all values that are not [`TuneToken`] objects), `"only_token"` (only [`TuneToken`] objects)
    #    or `"with_internal"` (all values that are no not `InternalTuneToken`)?
    #' @param check_required (`logical(1)`)\cr
    #'   Check if all required parameters are set?
    #' @param remove_dependencies (`logical(1)`)\cr
    #'   If `TRUE`, set values with dependencies that are not fulfilled to `NULL`.
    #' @return Named `list()`.
    get_values = function(class = NULL, tags = NULL, any_tags = NULL,
      type = "with_token", check_required = TRUE, remove_dependencies = TRUE) {
      assert_choice(type, c("with_token", "without_token", "only_token", "with_internal"))

      assert_flag(check_required)

      values = self$values
      ns = names(values)

      if (type == "without_token") {
        values = discard(values, is, "TuneToken")
      } else if (type == "only_token") {
        values = keep(values, is, "TuneToken")
      } else if (type == "with_internal") {
        values = keep(values, is, "InternalTuneToken")
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

    #' @description
    #' Perform transformation specified by the `trafo` of [`Domain`] objects, as well as the `$extra_trafo` field.
    #' @param x (named `list()` | `data.frame`)\cr
    #'   The value(s) to be transformed.
    #' @param param_set (`ParamSet`)\cr
    #'   Passed to `extra_trafo()`. Note that the `extra_trafo` of `self` is used, not the `extra_trafo` of the
    #'   `ParamSet` given in the `param_set` argument.
    #'   In almost all cases, the default `param_set = self` should be used.
    trafo = function(x, param_set = self) {
      if (is.data.frame(x)) x = as.list(x)
      assert_list(x, names = "unique")
      trafos = private$.trafos[names(x), .(id, trafo), nomatch = 0]
      value = NULL  # static checks
      if (nrow(trafos)) {
        trafos[, value := x[id]]
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

    #' @description
    #'
    #' Aggregate parameter values according to their aggregation rules.
    #'
    #' @param x (named `list()` of `list()`s)\cr
    #'   The value(s) to be aggregated. Names are parameter values.
    #'   The aggregation function is selected based on the parameter.
    #'
    #' @return (named `list()`)
    aggr_internal_tuned_values = function(x) {
      assert_list(x, types = "list")
      aggrs = private$.params[map_lgl(get("cargo"), function(cargo) is.function(cargo$aggr)), list(id = get("id"), aggr = map(get("cargo"), "aggr"))]
      assert_subset(names(x), aggrs$id)
      if (!length(x)) {
        return(named_list())
      }
      imap(x, function(value, .id) {
        if (!length(value)) {
          stopf("Trying to aggregate values of parameters '%s', but there are no values", .id)
        }
        aggr = aggrs[list(.id), "aggr", on = "id"][[1L]][[1L]](value)
      })
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
      pvs = Reduce(c, map(private$.params[ids, "cargo", on = "id"][[1]], "disable_in_tune")) %??% named_list()
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
      param_vals = self$values

      imap(search_space$domains, function(token, .id) {
        converter = private$.params[list(.id), "cargo", on = "id"][[1L]][[1L]]$in_tune_fn
        if (!is.function(converter)) {
          stopf("No converter exists for parameter '%s'", .id)
        }
        converter(token, param_vals)
      })
    },

    #' @description
    #' \pkg{checkmate}-like test-function. Takes a named list.
    #' Return `FALSE` if the given `$constraint` is not satisfied, `TRUE` otherwise.
    #' Note this is different from satisfying the bounds or types given by the `ParamSet` itself:
    #' If `x` does not satisfy these, an error will be thrown, given that `assert_value` is `TRUE`.
    #' @param x (named `list()`)\cr
    #'   The value to test.
    #' @param assert_value (`logical(1)`)\cr
    #'   Whether to verify that `x` satisfies the bounds and types given by this `ParamSet`.
    #'   Should be `TRUE` unless this was already checked before.
    #' @return `logical(1)`: Whether `x` satisfies the `$constraint`.
    test_constraint = function(x, assert_value = TRUE) {
      if (assert_value) self$assert(x, check_strict = FALSE)
      assert_flag(is.null(private$.constraint) || private$.constraint(x))
    },

    #' @description
    #' \pkg{checkmate}-like test-function. Takes a [`data.table`][data.table::data.table].
    #' For each row, return `FALSE` if the given `$constraint` is not satisfied, `TRUE` otherwise.
    #' Note this is different from satisfying the bounds or types given by the `ParamSet` itself:
    #' If `x` does not satisfy these, an error will be thrown, given that `assert_value` is `TRUE`.
    #' @param x (`data.table`)\cr
    #'   The values to test.
    #' @param assert_value (`logical(1)`)\cr
    #'   Whether to verify that `x` satisfies the bounds and types given by this `ParamSet`.
    #'   Should be `TRUE` unless this was already checked before.
    #' @return `logical`: For each row in `x`, whether it satisfies the `$constraint`.
    test_constraint_dt = function(x, assert_value = TRUE) {
      assert_data_table(x)
      if (assert_value) self$assert_dt(x, check_strict = FALSE)
      map_lgl(transpose(x), self$test_constraint, assert_value = FALSE)
    },

    #' @description
    #' \pkg{checkmate}-like check-function. Takes a named list.
    #' A point x is feasible, if it configures a subset of params,
    #' all individual param constraints are satisfied and all dependencies are satisfied.
    #' Params for which dependencies are not satisfied should not be part of `x`.
    #' Constraints and dependencies are not checked when `check_strict` is `FALSE`.
    #'
    #' @param xs (named `list()`).
    #' @param check_strict (`logical(1)`)\cr
    #'   Whether to check that constraints and dependencies are satisfied.
    #' @param sanitize (`logical(1)`)\cr
    #'   Whether to move values that are slightly outside bounds to valid values.
    #'   These values are accepted independent of `sanitize` (depending on the
    #'   `tolerance` arguments of `p_dbl()` and `p_int()`) . If `sanitize`
    #'   is `TRUE`, the additional effect is that, should checks pass, the
    #'   sanitized values of `xs` are added to the result as attribute `"sanitized"`.
    #' @return If successful `TRUE`, if not a string with an error message.
    check = function(xs, check_strict = TRUE, sanitize = FALSE) {
      assert_flag(check_strict)
      ok = check_list(xs, names = "unique")
      if (!isTRUE(ok)) {
        return(ok)
      }

      trueret = TRUE
      if (sanitize) {
        attr(trueret, "sanitized") = xs
      }

      # return early, this makes the following code easier since we don't need to consider edgecases with empty vectors.
      if (!length(xs)) return(trueret)


      params = private$.params
      ns = names(xs)
      ids = private$.params$id

      extra = wf(ns %nin% ids)
      if (length(extra)) {
        return(sprintf("Parameter '%s' not available.%s", ns[extra], did_you_mean(extra, ids)))
      }

      if (some(xs, inherits, "TuneToken")) {
        tunecheck = tryCatch({
          private$get_tune_ps(xs)
          TRUE
        }, error = function(e) paste("tune token invalid:", conditionMessage(e)))
        if (!isTRUE(tunecheck)) return(tunecheck)
        xs_nontune = discard(xs, inherits, "TuneToken")

        # only had TuneTokens, nothing else to check here.
        if (!length(xs_nontune) && !some(xs, is, "InternalTuneToken")) {
          return(trueret)
        }
      } else {
        xs_nontune = xs
      }

      xs_internaltune = keep(xs, is, "InternalTuneToken")
      walk(names(xs_internaltune), function(pid) {
        if ("internal_tuning" %nin% self$tags[[pid]]) {
          stopf("Trying to assign InternalTuneToken to parameter '%s' which is not tagged with 'internal_tuning'.", pid)
        }
      })


      # check each parameter group's feasibility
      pidx = match(names(xs_nontune), params$id)
      nonspecial = !pmap_lgl(list(params$special_vals[pidx], xs_nontune), has_element)
      pidx = pidx[nonspecial]

      if (sanitize) {
        bylevels = paste0(params$cls[pidx], params$grouping[pidx])
        if (length(unique(bylevels)) <= 7) {
          # if we do few splits, it is faster to do the subsetting of `params` manually instead of using data.table `by`.
          checkresults = list()
          sanitized_list = list()
          for (spl in split(pidx, bylevels)) {
            values = xs[params$id[spl]]
            spltbl = params[spl]
            spltbl = recover_domain(spltbl)
            cr = domain_check(spltbl, values, internal = TRUE)
            if (isTRUE(cr)) {
              sanitized_list[[length(sanitized_list) + 1]] = structure(domain_sanitize(spltbl, values), names = names(values))
            }
            checkresults[[length(checkresults) + 1]] = cr
          }
        } else {

          params = params[pidx]
          set(params, , "values", list(xs_nontune[nonspecial]))

          checks = params[, {
              domain = recover_domain(.SD)
              cr = domain_check(domain, values, internal = TRUE)
              if (isTRUE(cr)) {
                values = domain_sanitize(domain, values)
              }
              list(list(cr), list(structure(values, names = id)))
            }, by = c("cls", "grouping"),
           .SDcols = colnames(params)]
          checkresults = checks[[3]]
          sanitized_list = checks[[4]]
        }
        sanitized = unlist(sanitized_list, recursive = FALSE)
        sanitized_all = xs
        sanitized_all[names(sanitized)] = sanitized
        attr(trueret, "sanitized") = sanitized_all
      } else {
        params = params[pidx]
        set(params, , "values", list(xs_nontune[nonspecial]))

        checkresults = params[, list(list(domain_check(recover_domain(.SD), values))), by = c("cls", "grouping"),
          .SDcols = colnames(params)][[3]]  # first two cols are 'cls' and 'grouping'
      }
      checkresults = discard(checkresults, isTRUE)
      if (length(checkresults)) {
        return(str_collapse(checkresults, sep = "\n"))
      }

      if (check_strict) {
        ## required = setdiff(self$ids(tags = "required"), ns)
        ## if (length(required) > 0L) {
        ##   return(sprintf("Missing required parameters: %s", str_collapse(required)))
        ## }
        if (!self$test_constraint(xs, assert_value = FALSE)) return(sprintf("Constraint not fulfilled."))
        cd = self$check_dependencies(xs)
        if (!isTRUE(cd)) return(cd)
      }
      trueret # we passed all checks
    },

    #' @description
    #' \pkg{checkmate}-like check-function. Takes a named list.
    #' Checks that all individual param dependencies are satisfied.
    #'
    #' @param xs (named `list()`).
    #' @return If successful `TRUE`, if not a string with an error message.
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
      errors = unlist(errors, use.names = FALSE)
      if (!length(errors)) return(TRUE)
      str_collapse(errors, sep = "\n")
    },

    #' @description
    #' \pkg{checkmate}-like test-function. Takes a named list.
    #' A point x is feasible, if it configures a subset of params,
    #' all individual param constraints are satisfied and all dependencies are satisfied.
    #' Params for which dependencies are not satisfied should not be part of `x`.
    #' Constraints and dependencies are not checked when `check_strict` is `FALSE`.
    #'
    #' @param xs (named `list()`).
    #' @param check_strict (`logical(1)`)\cr
    #'   Whether to check that constraints and dependencies are satisfied.
    #' @return If successful `TRUE`, if not `FALSE`.
    test = function(xs, check_strict = TRUE) makeTest(self$check(xs, check_strict = check_strict)),

    #' @description
    #' \pkg{checkmate}-like assert-function. Takes a named list.
    #' A point x is feasible, if it configures a subset of params,
    #' all individual param constraints are satisfied and all dependencies are satisfied.
    #' Params for which dependencies are not satisfied should not be part of `x`.
    #' Constraints and dependencies are not checked when `check_strict` is `FALSE`.
    #'
    #' @param xs (named `list()`).
    #' @param check_strict (`logical(1)`)\cr
    #'   Whether to check that constraints and dependencies are satisfied.
    #' @param .var.name (`character(1)`)\cr
    #'   Name of the checked object to print in error messages.\cr
    #'   Defaults to the heuristic implemented in [vname][checkmate::vname].
    #' @param sanitize (`logical(1)`)\cr
    #'   Whether to move values that are slightly outside bounds to valid values.
    #'   These values are accepted independent of `sanitize` (depending on the
    #'   `tolerance` arguments of `p_dbl()` and `p_int()`) . If `sanitize`
    #'   is `TRUE`, the additional effect is that `xs` is converted to within bounds.
    #' @return If successful `xs` invisibly, if not an error message.
    assert = function(xs, check_strict = TRUE, .var.name = vname(xs), sanitize = FALSE) {
      checkresult = self$check(xs, check_strict = check_strict, sanitize = sanitize)
      makeAssertion(if (sanitize) attr(checkresult, "sanitized") else xs, checkresult, .var.name, NULL)  # nolint
    },

    #' @description
    #' \pkg{checkmate}-like check-function. Takes a [data.table::data.table]
    #' where rows are points and columns are parameters.
    #' A point x is feasible, if it configures a subset of params,
    #' all individual param constraints are satisfied and all dependencies are satisfied.
    #' Params for which dependencies are not satisfied should not be part of `x`.
    #' Constraints and dependencies are not checked when `check_strict` is `FALSE`.
    #'
    #' @param xdt ([data.table::data.table] | `data.frame()`).
    #' @param check_strict (`logical(1)`)\cr
    #'   Whether to check that constraints and dependencies are satisfied.
    #' @return If successful `TRUE`, if not a string with the error message.
    check_dt = function(xdt, check_strict = TRUE) {
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
    #' @param check_strict (`logical(1)`)\cr
    #'   Whether to check that constraints and dependencies are satisfied.
    #' @return If successful `TRUE`, if not `FALSE`.
    test_dt = function(xdt, check_strict = TRUE) makeTest(res = self$check_dt(xdt, check_strict = check_strict)),

    #' @description
    #' \pkg{checkmate}-like assert-function (s. `$check_dt()`).
    #'
    #' @param xdt ([data.table::data.table]).
    #' @param check_strict (`logical(1)`)\cr
    #'   Whether to check that constraints and dependencies are satisfied.
    #' @param .var.name (`character(1)`)\cr
    #'   Name of the checked object to print in error messages.\cr
    #'   Defaults to the heuristic implemented in [vname][checkmate::vname].
    #' @return If successful `xs` invisibly, if not, an error is generated.
    assert_dt = function(xdt, check_strict = TRUE, .var.name = vname(xdt)) makeAssertion(xdt, self$check_dt(xdt, check_strict = check_strict), .var.name, NULL), # nolint

    #' @description
    #' Map a `matrix` or `data.frame` of values between 0 and 1 to proportional values inside the feasible intervals of individual parameters.
    #'
    #' @param x (`matrix` | `data.frame`)\cr
    #'   Values to map. Column names must be a subset of the names of parameters.
    #' @return `data.table`.
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
      result = NULL  # static checks
      params[, result := list(as.list(as.data.frame(t(matrix(domain_qunif(recover_domain(.SD), x[id, ]), nrow = .N))))),
        by = c("cls", "grouping"),
        .SDcols = colnames(private$.params)]
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
    #' @param allow_dangling_dependencies (`logical(1)`)\cr
    #'   Whether to allow subsets that cut across parameter dependencies.
    #'   Dependencies that point to dropped parameters are kept (but will be "dangling", i.e. their `"on"` will not be present).
    #' @param keep_constraint (`logical(1)`)\cr
    #'   Whether to keep the `$constraint` function.
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
      result$extra_trafo = self$extra_trafo
      # restrict to ids already in pvals
      values = self$values
      result$values = values[match(ids, names(values), nomatch = 0)]
      result$assert_values = TRUE
      result
    },

    #' @description
    #' Create new one-dimensional `ParamSet`s for each dimension.
    #' @param ids (`character()`)\cr
    #'   IDs for which to create `ParamSet`s. Defaults to all IDs.
    #' @return named `list()` of `ParamSet`.
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
    #' a `ParamSet` but e.g. a [`ParamSetCollection`].
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
          dd = deps[, list(parents = list(unlist(on, use.names = FALSE))), by = "id"]
          d = merge(d, dd, by = "id", all.x = TRUE)
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
      lower = upper = levels = special_vals = default = NULL  # static check
      private$.params[, list(id, class = cls, lower, upper, levels, nlevels = self$nlevels,
        is_bounded = self$is_bounded, special_vals, default, storage_type = self$storage_type, tags = self$tags)]
    },

    #' @template field_values
    values = function(xs) {
      if (missing(xs)) {
        return(private$.get_values())
      }
      if (length(xs) == 0L) {
        xs = named_list()
      } else if (self$assert_values) {
        # this only makes sense when we have asserts on
        # convert all integer params really to storage type int, move doubles to within bounds etc.
        # solves issue #293, #317
        xs = self$assert(xs, sanitize = TRUE)
      }
      private$.store_values(xs)
      xs
    },

    #' @template field_tags
    tags = function(v) {
      if (!missing(v)) {
        assert_list(v, any.missing = FALSE, types = "character")
        if (length(v)) assert_names(names(v), permutation.of = private$.params$id)
        # as.character() to handle empty lists and resulting NULL-valures.
        private$.tags = data.table(id = rep(as.character(names(v)), map_int(v, length)), tag = as.character(unlist(v, use.names = FALSE)), key = "id")
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
      .requirements = NULL  # pacify static check
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

    #' @template field_extra_trafo
    extra_trafo = function(f) {
      if (missing(f)) {
        private$.extra_trafo
      } else {
        if (!is.null(f)) {  # for speed, since asserts below are slow apparently
          assert(check_function(f, args = c("x", "param_set"), null.ok = TRUE), check_function(f, args = "x", null.ok = TRUE))
        }
        private$.extra_trafo = f
      }
    },

    #' @template field_constraint
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

    #' @field length (`integer(1)`)\cr Number of contained parameters.
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
    #' @field all_numeric (`logical(1)`)\cr Is `TRUE` if all parameters are [`p_dbl()`] or [`p_int()`].
    all_numeric = function() all(self$is_number),
    #' @field all_categorical (`logical(1)`)\cr Is `TRUE` if all parameters are [`p_fct()`] and [`p_lgl()`].
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
    is_logscale = function() with(private$.params, set_names(cls %in% c("ParamDbl", "ParamInt") & map_lgl(cargo, function(x) isTRUE(x$logscale)), id)),

    ############################
    # Per-Parameter class properties (S3 method call)

    #' @field nlevels (named `integer()`)\cr Number of distinct levels of parameters. `Inf` for double parameters or unbounded integer parameters.
    #' Named with param IDs.
    nlevels = function() {
      tmp = private$.params[,
        list(id, nlevels = domain_nlevels(recover_domain(.SD))),
        by = c("cls", "grouping"),
        .SDcols = colnames(private$.params)
      ]
      with(tmp[private$.params$id, on = "id"], set_names(nlevels, id))
    },

    #' @field is_number (named `logical()`)\cr Whether parameter is [`p_dbl()`] or [`p_int()`]. Named with parameter IDs.
    is_number = function() {
      tmp = private$.params[,
        list(id, is_number = rep(domain_is_number(recover_domain(.SD)), .N)),
        by = c("cls", "grouping"),
        .SDcols = colnames(private$.params)
      ]
      with(tmp[private$.params$id, on = "id"], set_names(is_number, id))
    },

    #' @field is_categ (named `logical()`)\cr Whether parameter is [`p_fct()`] or [`p_lgl()`]. Named with parameter IDs.
    is_categ = function() {
      tmp = private$.params[,
        list(id, is_categ = rep(domain_is_categ(recover_domain(.SD)), .N)),
        by = c("cls", "grouping"),
        .SDcols = colnames(private$.params)
      ]
      with(tmp[private$.params$id, on = "id"], set_names(is_categ, id))
    },

    #' @field is_bounded (named `logical()`)\cr Whether parameters have finite bounds. Named with parameter IDs.
    is_bounded = function() {
      tmp = private$.params[,
        list(id, is_bounded = domain_is_bounded(recover_domain(.SD))),
        by = c("cls", "grouping"),
        .SDcols = colnames(private$.params)
      ]
      with(tmp[private$.params$id, on = "id"], set_names(is_bounded, id))
    }
  ),

  private = list(
    .store_values = function(xs) {
      # store with param ordering
      private$.values = xs[match(private$.params$id, names(xs), nomatch = 0)]
    },
    .get_values = function() private$.values,
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

recover_domain = function(sd) {
  class(sd) = c(sd$cls[1], "Domain", class(sd))
  sd
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

