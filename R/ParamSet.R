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
    #'   This can be used to create a `ParamSet` with certain [`Param`] `id`s
    #'   without having to clone said [`Param`]s.
    #'   Default `FALSE`.
    initialize = function(params = named_list(), allow_dangling_dependencies = FALSE) {
      assert_list(params, types = "Domain")

      assert_names(names(params), type = "strict")
      assert_ids(names(params))

      if (!length(params)) {
        private$.params = data.table(cls = character(0), grouping = character(0), cargo = list(), lower = numeric(0), upper = numeric(0), tolerance = numeric(0),
          levels = list(), special_vals = list(), default = list(), trafo = list(), requirements = list(), id = character(0))
        initvalues = named_list()
      } else {
        private$.params = rbindlist(params)
        set(private$.params, , "id", names(params))
        set(private$.params, , "has_trafo", !map_lgl(private$.params$trafo, is.null))

        initvalues = col_to_nl(private$.params[(init_given), list(init, id)])
        private$.tags = col_to_nl(private$.params, "tags", "id")

        set(private$.params, , setdiff(colnames(private$.params), paramcols), NULL)

      }
      setindexv(private$.params, c("id", "cls", "grouping"))
      assert_names(colnames(private$.params), permutation.of = paramcols)

      # add Dependencies
      imap(params, function(p, name) {
        if (is.null(p$requirements[[1]])) return(NULL)
        map(p$requirements[[1]], function(req) {
          if (!req$on %in% names(params) || req$on == name) {
            if (allow_dangling_dependencies) {
              if (name == req$on) stop("A param cannot depend on itself!")
              self$deps = rbind(self$deps, data.table(id = name, on = req$on, cond = list(req$cond)))
            } else {
              stopf("Parameter %s can not depend on %s.", name, req$on)
            }
          } else {
            invoke(self$add_dep, id = name, .args = req)
          }
        })
      })
      self$values = initvalues
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
      selftags = private$.tags
      ptbl[(is.null(class) | cls %in% class) &
           (is.null(tags) | map_lgl(selftags, function(tg) all(tags %in% tg))) &
           (is.null(any_tags) | map_lgl(selftags, function(tg) any(any_tags %in% tg))),
        id]
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
        required = setdiff(self$ids(tags = "required"), nds)
        if (length(required) > 0L) {
          stop(sprintf("Missing required parameters: %s", str_collapse(required)))
        }
      }

      if (remove_dependencies && nrow(deps)) {
        for (j in seq_row(deps)) {
          p1id = deps$id[[j]]
          p2id = deps$on[[j]]
          cond = deps$cond[[j]]
          if (p1id %in% ns && !inherits(values[[p2id]], "TuneToken") && !isTRUE(cond$test(values[[p2id]]))) {
            values[p1id] = NULL
          }
        }
      }

      values[match(self$ids(class = class, tags = tags, any_tags = any_tags), names(values), nomatch = 0)]
    },

    trafo = function(x, param_set = self) {
      trafos = private$.params[names(x), list(id, trafo, has_trafo, value = x), on = "id", nomatch = 0][(has_trafo)]
      if (nrow(trafos)) {
        trafos = trafos[, list(value = trafo[[1]](value[[1]])), by = "id"]
        insert_named(x, set_names(trafos$value, trafos$id))
      }
      extra_trafo = private$.extra_trafo
      if (!is.null(extra_trafo)) {
        if (test_function(extra_trafo, args = c("x", "param_set"))) {
          x = extra_trafo(x, param_set)
        } else {
          x = extra_trafo(x)
        }
      }
      x
    },

    test_constraint = function(x, assert_value = TRUE) {
      if (assert_value) self$assert(x)
      assert_flag(is.null(private$.constraint) || private$.constraint(x))
    },

    test_constraint_dt = function(x, assert_value = TRUE) {
      assert_data_table(x)
      if (assert_value) self$assert_dt(x)
      map_dbl(transpose(x), self$test_constraint)
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

      # check each parameter group's feasibility
      xs_nontune = discard(xs, inherits, "TuneToken")

      params = params[names(xs_nontune), on = "id"]
      set(params, , "values", list(xs_nontune))
      pgroups = split(params, by = c("cls", "grouping"))
      checkresults = map(pgroups, function(x) {
        domain_check(set_class(x, c(x$cls[[1]], class(x))), x$values)
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
        return(check_dependencies(xs))
        if (!self$test_constraint(xs)) return(sprintf("Constraint not fulfilled."))
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
        if (on %in% ns && cond$test(onval)) return(NULL)
        msg = sprintf("%s: can only be set if the following condition is met '%s'.",
          id, cond$as_string(on))
        if (is.null(onval)) {
          msg = sprintf(paste("%s Instead the parameter value for '%s' is not set at all.",
              "Try setting '%s' to a value that satisfies the condition"), msg, on, on)
        } else {
          msg = sprintf("%s Instead the current parameter value is: %s=%s", msg, on, as_short_string(onval))
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
    assert = function(xs, check_strict = FALSE, .var.name = vname(xs)) makeAssertion(xs, self$check(xs, check_strict = check_strict), .var.name, NULL), # nolint

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

    get_param = function(id) {
      .id = id
      paramrow = private$.params[id == .id]
      if (!nrow(paramrow)) stopf("No param with id '%s'", id)
      set_class(paramrow, c(paramrow$cls, class(paramrow)))
    },

    #' @description
    #' Create a new `ParamSet` restricted to the passed IDs.
    #'
    #' @param ids (`character()`).
    #' @return `ParamSet`.
    subset = function(ids, allow_dangling_dependencies = FALSE) {
      param_ids = private$.params$id

      assert_subset(ids, param_ids)
      deps = self$deps
      if (!allow_dangling_dependencies && nrow(deps)) { # check that all required / leftover parents are still in new ids
        on = NULL
        parents = unique(deps[id %in% ids, on])
        pids_not_there = setdiff(parents, ids)
        if (length(pids_not_there) > 0L) {
          stopf(paste0("Subsetting so that dependencies on params exist which would be gone: %s.",
              "\nIf you still want to subset, set allow_dangling_dependencies to TRUE."), str_collapse(pids_not_there))
        }
      }
      result = ParamSet$new(extra_trafo = self$extra_trafo)
      result$.__enclos_env__$private$.params = private$.params[id %in% ids, paramcols, with = FALSE]
      result$assert_values = FALSE
      result$deps = deps
      result$constraint = private$.constraint
      # restrict to ids already in pvals
      values = self$values
      result$values = values[match(ids, names(values), nomatch = 0)]
      result$assert_values = TRUE
      result
    },

    subspaces = function(ids = private$.params$id) {
      sapply(ids, simplify = FALSE, function(get_id) {
        result = ParamSet$new(extra_trafo = self$extra_trafo)
        # constraint make no sense here, basically by definition
        result$.__enclos_env__$private$.params = private$.params[get_id, on = "id"]
        result$assert_values = FALSE
        result$values = values[match(get_id, names(values), nomatch = 0)]
        result$assert_values = TRUE
        result
      })
    },

    #' @description
    #' Create a `ParamSet` from this object, even if this object itself is not
    #' a `ParamSet`.
    flatten = function() self$subset(private$.params$id),

    #' @description
    #' Construct a [`ParamSet`] to tune over. Constructed from [`TuneToken`] in `$values`, see [`to_tune()`].
    #'
    #' @param  values (`named list`): optional named list of [`TuneToken`] objects to convert, in place of `$values`.
    search_space = function(values = self$values) {
      assert_list(values)
      assert_names(names(values), subset.of = self$ids())
      pars = private$get_tune_ps(values)
      on = NULL  # pacify static code check
      dangling_deps = pars$deps[!on %in% pars$ids()]
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
    #' @param cond ([Condition]).
    add_dep = function(id, on, cond) {
      params = private$.params
      ids = params$id
      assert_choice(id, ids)
      assert_choice(on, ids)
      assert_r6(cond, "Condition")
      if (id == on) {
        stopf("A param cannot depend on itself!")
      }

      feasible_on_values = map_lgl(cond$rhs, domain_check, param = self$get_param(on))
      if (any(!feasible_on_values)) {
        stopf("Condition has infeasible values for %s: %s", on, str_collapse(cond$rhs[!feasible_on_values]))
      }
      private$.deps = rbind(private$.deps, data.table(id = id, on = on, cond = list(cond)))
      invisible(self)
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s[%s]>", class(self)[[1L]], self$length)
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
      if (!missing(v)) stop("v is read-only")
      private$.params[, list(id, class = cls, lower, upper, levels, nlevels = self$nlevels,
        is_bounded = self$is_bounded, special_vals, default, storage_type = self$storage_type, tags = private$.tags)]
    },

    #' @template field_values
    values = function(xs) {
      if (missing(xs)) {
        return(private$.values)
      }
      if (self$assert_values) {
        self$assert(xs)
        if (test_list(xs, types = "TuneToken")) {
          private$get_tune_ps(xs)  # check that to_tune() are valid
        }
      }
      if (length(xs) == 0L) {
        xs = named_list()
      } else if (self$assert_values) {  # this only makes sense when we have asserts on
        # convert all integer params really to storage type int, move doubles to within bounds etc.
        # solves issue #293, #317
        nontt = discard(xs, inherits, "TuneToken")
        sanitised = set(private$.params[names(nontt), on = "id"], , "values", list(nontt))[
          !pmap_lgl(list(special_vals, values), has_element),
          list(id, values = domain_sanitize(recover_domain(.SD, .BY), values)), by = c("cls", "grouping")]
        insert_named(xx, set_names(sanitised$values, sanitised$id))
      }
      # store with param ordering, return value with original ordering
      private$.values = xs[private$.params[id %in% names(xs), id]]
      xs
    },

    #' @template field_tags
    tags = function(v) {
      if (!missing(v)) {
        assert_list(v, any.missing = FALSE, types = "character")
        assert_names(names(v), permutation.of = private$.params$id)
        private$.tags = v[match(private$.params$id, names(v), nomatch = 0)]
        return(v)
      }
      private$.tags
    },

    #' @template field_params
    params = function(rhs) {
      if (!missing(rhs)) {
        stop("$params is read-only.")
      }
      private$.params[, paramcols, with = FALSE]
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
        assert_function(f, args = x, null.ok = TRUE)
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
    has_trafo = function() !is.null(private$.extra_trafo) || any(private$.params$has_trafo),
    #' @field has_deps (`logical(1)`)\cr Whether the parameter dependencies are present
    has_deps = function() nrow(self$deps) > 0L,
    #' @field has_constraint (`logical(1)`)\cr Whether parameter constraint is set.
    has_constraint = function() !is.null(private$.constraint),
    #' @field all_numeric (`logical(1)`)\cr Is `TRUE` if all parameters are [ParamDbl] or [ParamInt].
    all_numeric = function() all(self$is_number),
    #' @field all_categorical (`logical(1)`)\cr Is `TRUE` if all parameters are [ParamFct] and [ParamLgl].
    all_categorical = function() all(self$is_categ),


    ############################
    # Per-Parameter properties

    #' @field class (named `character()`)\cr Classes of contained parameters. Named with parameter IDs.
    class = function() col_to_nl(private$.params, "class", "id"),
    #' @field lower (named `double()`)\cr Lower bounds of numeric parameters (`NA` for non-numerics). Named with parameter IDs.
    lower = function() col_to_nl(private$.params, "lower", "id"),
    #' @field upper (named `double()`)\cr Upper bounds of numeric parameters (`NA` for non-numerics). Named with parameter IDs.
    upper = function() col_to_nl(private$.params, "upper", "id"),
    #' @field levels (named `list()` of `character`)\cr Allowed levels of categorical parameters (`NULL` for non-categoricals).
    #' Named with parameter IDs.
    levels = function() col_to_nl(private$.params, "levels", "id"),
    #' @field storage_type (`character()`)\cr Data types of parameters when stored in tables. Named with parameter IDs.
    storage_type = function() col_to_nl(private$.params, "storage_type", "id"),
    #' @field special_vals (named `list()` of `list()`)\cr Special values for all parameters. Named with parameter IDs.
    special_vals = function() col_to_nl(private$.params, "special_vals", "id"),
    #' @field default (named `list()`)\cr Default values of all parameters. If no default exists, element is not present.
    #' Named with parameter IDs.
    default = function() col_to_nl(private$.params[!map_lgl(default, is_nodefault), list(default, id)]),

    ############################
    # Per-Parameter class properties (S3 method call)

    #' @field nlevels (named `integer()`)\cr Number of distinct levels of parameters. `Inf` for double parameters or unbounded integer parameters.
    #' Named with param IDs.
    nlevels = function() {
      info = private$.params[,
        list(id, nlevels = domain_nlevels(recover_domain(.SD, .BY))),
        by = c("cls", "grouping")
      ][
        private$.params$id, on = "id", list(nlevels, id)
      ]
      col_to_nl(info)
    },

    #' @field is_number (named `logical()`)\cr Whether parameter is [ParamDbl] or [ParamInt]. Named with parameter IDs.
    is_number = function() {
      info = private$.params[,
        list(id, is_number = rep(domain_is_number(recover_domain(.SD, .BY)), .N)),
        by = c("cls", "grouping")
      ][
        private$.params$id, on = "id", list(is_number, id)
      ]
      col_to_nl(info)
    },

    #' @field is_categ (named `logical()`)\cr Whether parameter is [ParamFct] or [ParamLgl]. Named with parameter IDs.
    is_categ = function() {
      info = private$.params[,
        list(id, is_categ = rep(domain_is_categ(recover_domain(.SD, .BY)), .N)),
        by = c("cls", "grouping")
      ][
        private$.params$id, on = "id", list(is_categ, id)
      ]
      col_to_nl(info)
    },

    #' @field is_bounded (named `logical()`)\cr Whether parameters have finite bounds. Named with parameter IDs.
    is_bounded = function() {
      info = private$.params[,
        list(id, is_bounded = domain_is_bounded(recover_domain(.SD, .BY))),
        by = c("cls", "grouping")
      ][
        private$.params$id, on = "id", list(is_bounded, id)
      ]
      col_to_nl(info)
    }
  ),

  private = list(
    .extra_trafo = NULL,
    .constraint = NULL,
    .params = NULL,
    .values = named_list(),
    .tags = named_list(),
    .deps = data.table(id = character(0L), on = character(0L), cond = list()),
    get_tune_ps = function(values) {
      return(NULL)  # TODO
      selfparams = private$.params
      partsets = imap(keep(values, inherits, "TuneToken"), function(value, pn) {
        tunetoken_to_ps(value, selfparams[[pn]], pn)
      })
      if (!length(partsets)) return(ParamSet$new())
      idmapping = map(partsets, function(x) x$ids())
      pars = ps_union(partsets)
      pars$set_id = self$set_id
      parsparams = pars$params_unid
      parsnames = names(parsparams)
      # only add the dependencies that are also in the tuning PS
      on = id = NULL  # pacify static code check
      pmap(self$deps[id %in% names(idmapping) & on %in% names(partsets), c("on", "id", "cond")], function(on, id, cond) {
        onpar = partsets[[on]]
        if (onpar$has_trafo || !identical(onpar$ids(), on)) {
          # cannot have dependency on a parameter that is being trafo'd
          return(NULL)
        }
        # remove infeasible values from condition
        cond = cond$clone(deep = TRUE)
        cond$rhs = keep(cond$rhs, parsparams[[on]]$test)
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

paramcols = c("id", "cls", "grouping", "cargo", "lower", "upper", "tolerance", "levels", "special_vals", "default", "trafo", "has_trafo", "storage_type")

recover_domain = function(sd, by) {
  domain = as.data.table(c(by, sd))
  class(domain) = c(domain$cls, class(domain))
  domain
}

#' @export
as.data.table.ParamSet = function(x, ...) { # nolint
  x$data
}

#' @export
rd_info.ParamSet = function(ps) { # nolint
  params = as.data.table(ps)
  if (nrow(params) == 0L)
    return("Empty ParamSet")
  params$default = replace(params$default, map_lgl(params$default, inherits, "NoDefault"), list("-"))
  params$levels = replace(params$levels, lengths(params$levels) == 0L, list("-"))
  params$levels = map_chr(params$levels, str_collapse, n = 10L)
  params$range = pmap_chr(params[, c("lower", "upper"), with = FALSE], rd_format_range)
  params = params[, c("id", "storage_type", "default", "range", "levels")]
  setnames(params, c("Id", "Type", "Default", "Range", "Levels"))
  c(
    "",
    knitr::kable(params)
  )
}
