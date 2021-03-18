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
    #'   Parameters are cloned.
    initialize = function(params = named_list()) {
      assert_list(params, types = "Param")
      ids = map_chr(params, "id")
      assert_names(ids, type = "strict")
      private$.params = set_names(map(params, function(p) p$clone(deep = TRUE)), ids)
      self$set_id = ""
    },

    #' @description
    #' Adds a single param or another set to this set, all params are cloned.
    #'
    #' @param p ([Param] | [ParamSet]).
    add = function(p) {

      assert_multi_class(p, c("Param", "ParamSet"))
      p = if (inherits(p, "Param")) { # level-up param to set
        ParamSet$new(list(p))
      } else {
        p$clone(deep = TRUE)
      }
      pparams = p$params
      nn = c(names(private$.params), names(pparams))
      assert_names(nn, type = "strict")
      if (!is.null(p$trafo)) {
        stop("Cannot add a param set with a trafo.")
      }
      private$.params = c(private$.params, pparams)
      private$.values = c(private$.values, p$values)
      private$.deps = rbind(private$.deps, p$deps)
      invisible(self)
    },

    #' @description
    #' Retrieves IDs of contained parameters based on some filter criteria
    #' selections, `NULL` means no restriction.
    #' Only returns IDs of parameters that satisfy all conditions.
    #'
    #' @param class (`character()`).
    #' @param is_bounded (`logical(1)`).
    #' @param tags (`character()`).
    #' @return `character()`.
    ids = function(class = NULL, is_bounded = NULL, tags = NULL) {
      assert_character(class, any.missing = FALSE, null.ok = TRUE)
      assert_flag(is_bounded, null.ok = TRUE)
      assert_character(tags, any.missing = FALSE, null.ok = TRUE)

      params = self$params_unid
      ids = names(params)
      if (is.null(class) && is.null(is_bounded) && is.null(tags)) {
        return(ids)
      }

      ii = rep(TRUE, length(ids))

      if (!is.null(class)) {
        ii = ii & map_chr(params, "class") %in% class
      }

      if (!is.null(is_bounded)) {
        ii = ii & map_lgl(params, "is_bounded")
      }

      if (!is.null(tags)) {
        ii = ii & map_lgl(params, function(p) all(tags %in% p$tags))
      }

      ids[ii]
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
    #' @param remove_dependencies (`logical(1)`)\cr
    #' Determines if values of parameters with unsatisfied dependencies are removed.
    #' @return Named `list()`.
    get_values = function(class = NULL, is_bounded = NULL, tags = NULL, type = "with_token", check_required = TRUE, 
      remove_dependencies = TRUE) {
      assert_choice(type, c("with_token", "without_token", "only_token"))
      assert_flag(check_required)
      assert_flag(remove_dependencies)
      values = self$values
      params = self$params_unid
      ns = names(values)
      deps = self$deps

      if (type == "without_token") {
        values = discard(values, is, "TuneToken")
      } else if (type == "only_token") {
        values = keep(values, is, "TuneToken")
      }

      if (check_required) {
        required = setdiff(names(keep(params, function(p) "required" %in% p$tags)), ns)
        if (length(required) > 0L) {
          stop(sprintf("Missing required parameters: %s", str_collapse(required)))
        }
      }

      if (remove_dependencies) {
        if (nrow(deps)) {
          for (j in seq_row(deps)) {
            p1id = deps$id[j]
            p2id = deps$on[j]
            cond = deps$cond[[j]]
            if (p1id %in% ns && !inherits(values[[p2id]], "TuneToken") && !isTRUE(cond$test(values[[p2id]]))) {
              values[p1id] = NULL
              }
          }
        }
      }
      
      values[intersect(names(values), self$ids(class = class, is_bounded = is_bounded, tags = tags))]
    },

    #' @description
    #' Changes the current set to the set of passed IDs.
    #'
    #' @param ids (`character()`).
    subset = function(ids) {
      param_ids = names(self$params_unid)
      assert_subset(ids, param_ids)
      deps = self$deps
      if (nrow(deps)) { # check that all required / leftover parents are still in new ids
        parents = unique(deps[get("id") %in% ids, "on"][[1L]])
        pids_not_there = setdiff(parents, ids)
        if (length(pids_not_there) > 0L) {
          stopf(paste0("Subsetting so that dependencies on params exist which would be gone: %s.",
              "\nIf you still want to do that, manipulate '$deps' yourself."), str_collapse(pids_not_there))
        }
      }
      private$.params = private$.params[ids]
      # restrict to ids already in pvals
      ids2 = union(intersect(ids, names(private$.values)), setdiff(names(private$.values), param_ids))
      private$.values = private$.values[ids2]
      invisible(self)
    },

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
        stopf("Dangling dependencies not allowed: Dependencies on %s dangling", str_collapse(dangling_deps$on))
      }
      pars
    },

    #' @description
    #' \pkg{checkmate}-like check-function. Takes a named list.
    #' A point x is feasible, if it configures a subset of params,
    #' all individual param constraints are satisfied and all dependencies are satisfied.
    #' Params for which dependencies are not satisfied should not be part of `x`.
    #'
    #' @param xs (named `list()`).
    #' @param check_strict  (`logical(1)`)\cr
    #' Determines if dependencies and required parameters are checked.
    #' @return If successful `TRUE`, if not a string with the error message.
    check = function(xs, check_strict = FALSE) {
      assert_flag(check_strict)

      ok = check_list(xs, names = "unique")
      if (!isTRUE(ok)) {
        return(ok)
      }
      params = self$params_unid
      ns = names(xs)
      ids = names(params)

      extra = wf(ns %nin% ids)
      if (length(extra)) {
        return(sprintf("Parameter '%s' not available.%s", ns[extra], did_you_mean(extra, ids)))
      }

      # check each parameters feasibility
      for (n in ns) {
        ch = params[[n]]$check(xs[[n]])
        if (test_string(ch)) { # we failed a check, return string
          return(paste0(n, ": ", ch))
        }
      }

      if (check_strict) {
         # check required
        required = setdiff(names(keep(params, function(p) "required" %in% p$tags)), ns)
        if (length(required) > 0L) {
          stop(sprintf("Missing required parameters: %s", str_collapse(required)))
        }

        # check dependencies
        deps = self$deps
        if (nrow(deps)) {
          for (j in seq_row(deps)) {

            p1id = deps$id[j]
            p2id = deps$on[j]
            if (inherits(xs[[p1id]], "TuneToken") || inherits(xs[[p2id]], "TuneToken")) {
              next  # be lenient with dependencies when any parameter involved is a TuneToken
            }
            # we are ONLY ok if:
            # - if param is there, then parent must be there, then cond must be true
            # - if param is not there
            cond = deps$cond[[j]]
            ok = (p1id %in% ns && p2id %in% ns && cond$test(xs[[p2id]])) ||
              (p1id %nin% ns)
            if (isFALSE(ok)) {
              message = sprintf("The parameter '%s' can only be set if the following condition is met '%s'.",
                p1id, cond$as_string(p2id))
              val = xs[[p2id]]
              if (is.null(val)) {
                message = sprintf(paste("%s Instead the parameter value for '%s' is not set at all.",
                    "Try setting '%s' to a value that satisfies the condition"), message, p2id, p2id)
              } else {
                message = sprintf("%s Instead the current parameter value is: %s=%s", message, p2id, val)
              }
              return(message)
            }
          }
        }
      }

      return(TRUE) # we passed all checks
    },

    #' @description
    #' \pkg{checkmate}-like test-function. Takes a named list.
    #' A point x is feasible, if it configures a subset of params,
    #' all individual param constraints are satisfied and all dependencies are satisfied.
    #' Params for which dependencies are not satisfied should not be part of `x`.
    #'
    #' @param xs (named `list()`).
    #' @param check_strict (`logical(1)`)\cr
    #' Determines if dependencies and required parameters are checked.
    #' @return If successful `TRUE`, if not `FALSE`.
    test = function(xs, check_strict = FALSE) makeTest(res = self$check(xs, check_strict)),

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
    #' @param check_strict (`logical(1)`)\cr
    #' Determines if dependencies and required parameters are checked.
    #' @return If successful `xs` invisibly, if not an error message.
    assert = function(xs, .var.name = vname(xs), check_strict = FALSE) {
      makeAssertion(xs, self$check(xs, check_strict), .var.name, NULL) # nolint
    },

    #' @description
    #' \pkg{checkmate}-like check-function. Takes a [data.table::data.table]
    #' where rows are points and columns are parameters. A point x is feasible,
    #' if it configures a subset of params, all individual param constraints are
    #' satisfied and all dependencies are satisfied. Params for which
    #' dependencies are not satisfied should be set to `NA` in `xdt`.
    #'
    #' @param xdt ([data.table::data.table] | `data.frame()`).
    #' @param check_strict (`logical(1)`)\cr
    #' Determines if dependencies and required parameters are checked.
    #' @return If successful `TRUE`, if not a string with the error message.
    check_dt = function(xdt, check_strict = FALSE) {
      xss = map(transpose_list(xdt), discard, is.na)
      for (xs in xss) {
        ok = self$check(xs, check_strict)
        if (!isTRUE(ok)) {
          return(ok)
        }
      }

      return(TRUE)
    },

    #' @description
    #' \pkg{checkmate}-like test-function (s. `$check_dt()`).
    #'
    #' @param xdt ([data.table::data.table]).
    #' @return If successful `TRUE`, if not `FALSE`.
    test_dt = function(xdt) makeTest(res = self$check_dt(xdt)),

    #' @description
    #' \pkg{checkmate}-like assert-function (s. `$check_dt()`).
    #'
    #' @param xdt ([data.table::data.table]).
    #' @param .var.name (`character(1)`)\cr
    #'   Name of the checked object to print in error messages.\cr
    #'   Defaults to the heuristic implemented in [vname][checkmate::vname].
    #' @return If successful `xs` invisibly, if not an error message.
    assert_dt = function(xdt, .var.name = vname(xdt)) makeAssertion(xdt, self$check_dt(xdt), .var.name, NULL), # nolint

    #' @description
    #' Adds a dependency to this set, so that param `id` now depends on param `on`.
    #'
    #' @param id (`character(1)`).
    #' @param on (`character(1)`).
    #' @param cond ([Condition]).
    add_dep = function(id, on, cond) {
      params = self$params_unid
      ids = names(params)
      assert_choice(id, ids)
      assert_choice(on, ids)
      assert_r6(cond, "Condition")
      if (id == on) {
        stopf("A param cannot depend on itself!")
      }
      feasible_on_values = map_lgl(cond$rhs, params[[on]]$test)
      if (any(!feasible_on_values)) {
        stopf("Condition has infeasible values for %s: %s", on, str_collapse(cond$rhs[!feasible_on_values]))
      }
      private$.deps = rbind(private$.deps, data.table(id = id, on = on, cond = list(cond)))
      invisible(self)
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      set_id = self$set_id
      if (!nzchar(set_id)) {
        sprintf("<%s>", class(self)[1L])
      } else {
        sprintf("<%s:%s>", class(self)[1L], set_id)
      }
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
          dd = deps[, list(parents = list(unlist(get("on")))), by = "id"]
          d = merge(d, dd, on = "id", all.x = TRUE)
        }
        v = named_list(d$id) # add values to last col of print-dt as list col
        v = insert_named(v, self$values)
        d$value = list(v)
        print(d[, setdiff(colnames(d), hide_cols), with = FALSE])
      }
      if (!is.null(self$trafo)) {
        catf("Trafo is set.")
      } # printing the trafa functions sucks (can be very long). dont see a nother option then to suppress it for now
    }
  ),

  active = list(
    #' @template field_params
    params = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.params)) {
        stop("$params is read-only.")
      }
      private$.params
    },
    #' @template field_params_unid
    params_unid = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.params)) {
        stop("$params_unid is read-only.")
      }
      self$params
    },

    #' @template field_deps
    deps = function(v) {
      if (missing(v)) {
        private$.deps
      } else {
        private$.deps = assert_data_table(v)
      }
    },

    #' @field set_id (`character(1)`)\cr
    #' ID of this param set. Default `""`. Settable.
    set_id = function(v) {
      if (missing(v)) {
        private$.set_id
      } else {
        if (!identical(v, "")) {
          assert_id(v)
          assert_names(v, type = "strict")
        }
        private$.set_id = v
      }
    },

    #' @field length (`integer(1)`)\cr
    #' Number of contained [Param]s.
    length = function() {
      length(self$params_unid)
    },

    #' @field is_empty (`logical(1)`)\cr
    #' Is the `ParamSet` empty?
    is_empty = function() {
      length(self$params_unid) == 0L
    },

    #' @field class (named `character()`)\cr
    #' Classes of contained parameters, named with parameter IDs.
    class = function() {
      private$get_member_with_idnames("class", as.character)
    },

    #' @field lower (named `double()`)\cr
    #' Lower bounds of parameters (`NA` if parameter is not numeric).
    #' Named with parameter IDs.
    lower = function() {
      private$get_member_with_idnames("lower", as.double)
    },

    #' @field upper (named `double()`)\cr
    #' Upper bounds of parameters (`NA` if parameter is not numeric).
    #' Named with parameter IDs.
    upper = function() {
      private$get_member_with_idnames("upper", as.double)
    },

    #' @field levels (named `list()`)\cr
    #' List of character vectors of allowed categorical values of contained parameters.
    #' `NULL` if the parameter is not categorical.
    #' Named with parameter IDs.
    levels = function() {
      private$get_member_with_idnames("levels", as.list)
    },

    #' @field nlevels (named `integer()`)\cr
    #' Number of categorical levels per parameter, `Inf` for double parameters or unbounded integer parameters.
    #' Named with param IDs.
    nlevels = function() {
      private$get_member_with_idnames("nlevels", as.double)
    },

    #' @field is_bounded (named `logical()`)\cr
    #' Do all parameters have finite bounds?
    #' Named with parameter IDs.
    is_bounded = function() {
      all(map_lgl(self$params_unid, "is_bounded"))
    },

    #' @field special_vals (named `list()` of `list()`)\cr
    #' Special values for all parameters.
    #' Named with parameter IDs.
    special_vals = function() {
      private$get_member_with_idnames("special_vals", as.list)
    },

    #' @field default (named `list()`)\cr
    #' Default values of all parameters.
    #' If no default exists, element is not present.
    #' Named with parameter IDs.
    default = function() {
      discard(private$get_member_with_idnames("default", as.list), is_nodefault)
    },

    #' @field tags (named `list()` of `character()`)\cr
    #' Can be used to group and subset parameters.
    #' Named with parameter IDs.
    tags = function() {
      private$get_member_with_idnames("tags", as.list)
    },

    #' @field storage_type (`character()`)\cr
    #' Data types of parameters when stored in tables.
    #' Named with parameter IDs.
    storage_type = function() {
      private$get_member_with_idnames("storage_type", as.character)
    },

    #' @field is_number (named `logical()`)\cr
    #' Position is TRUE for [ParamDbl] and [ParamInt].
    #' Named with parameter IDs.
    is_number = function() {
      private$get_member_with_idnames("is_number", as.logical)
    },

    #' @field is_categ (named `logical()`)\cr
    #' Position is TRUE for [ParamFct] and [ParamLgl].
    #' Named with parameter IDs.
    is_categ = function() {
      private$get_member_with_idnames("is_categ", as.logical)
    },

    #' @field all_numeric (`logical(1)`)\cr
    #' Is `TRUE` if all parameters are [ParamDbl] or [ParamInt].
    all_numeric = function() {
      all(self$is_number)
    },

    #' @field all_categorical (`logical(1)`)\cr
    #' Is `TRUE` if all parameters are [ParamFct] and [ParamLgl].
    all_categorical = function() {
      all(self$is_categ)
    },

    #' @field trafo (`function(x, param_set)`)\cr
    #' Transformation function. Settable.
    #' User has to pass a `function(x, param_set)`, of the form\cr
    #' (named `list()`, [ParamSet]) -> named `list()`.\cr
    #' The function is responsible to transform a feasible configuration into another encoding,
    #' before potentially evaluating the configuration with the target algorithm.
    #' For the output, not many things have to hold.
    #' It needs to have unique names, and the target algorithm has to accept the configuration.
    #' For convenience, the self-paramset is also passed in, if you need some info from it (e.g. tags).
    #' Is NULL by default, and you can set it to NULL to switch the transformation off.
    trafo = function(f) {
      if (missing(f)) {
        private$.trafo
      } else {
        assert_function(f, args = c("x", "param_set"), null.ok = TRUE)
        private$.trafo = f
      }
    },

    #' @field has_trafo (`logical(1)`)\cr
    #' Has the set a `trafo` function?
    has_trafo = function() {
      !is.null(private$.trafo)
    },

    #' @template field_values
    values = function(xs) {
      if (missing(xs)) {
        return(private$.values)
      }
      if (self$assert_values) {
        self$assert(xs)
        private$get_tune_ps(xs)  # check that to_tune() are valid
      }
      if (length(xs) == 0L) {
        xs = named_list()
      } else if (self$assert_values) {  # this only makes sense when we have asserts on
        # convert all integer params really to storage type int, move doubles to within bounds etc.
        # solves issue #293, #317
        params = self$params_unid # cache the AB
        for (n in names(xs)) {
          p = params[[n]]
          x = xs[[n]]
          if (inherits(x, "TuneToken")) next
          if (has_element(p$special_vals, x)) next
          xs[[n]] = p$convert(x)
        }
      }
      private$.values = xs
    },

    #' @field has_deps (`logical(1)`)\cr
    #' Has the set parameter dependencies?
    has_deps = function() {
      nrow(self$deps) > 0L
    }
  ),

  private = list(
    .set_id = NULL,
    .trafo = NULL,
    .params = NULL,
    .values = named_list(),
    .deps = data.table(id = character(0L), on = character(0L), cond = list()),
    # return a slot / AB, as a named vec, named with id (and can enforce a certain vec-type)
    get_member_with_idnames = function(member, astype) {
      params = self$params
      set_names(astype(map(params, member)), names(params))
    },
    get_tune_ps = function(values) {
      selfparams = self$params_unid # cache to avoid performance hit in ParamSetCollection
      partsets = imap(keep(values, inherits, "TuneToken"), function(value, pn) {
        tunetoken_to_ps(value, selfparams[[pn]], pn)
      })
      if (!length(partsets)) return(ParamSet$new())
      idmapping = map(partsets, function(x) x$ids())
      pars = ps_union(partsets)
      pars$set_id = self$set_id
      parsnames = names(pars$params)
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
        cond$rhs = keep(cond$rhs, pars$params[[on]]$test)
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
        .params = map(value, function(x) x$clone(deep = TRUE)),
        .deps = {
          value = copy(value)
          value$cond = lapply(value$cond, function(x) x$clone(deep = TRUE))
          value
        },
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

#' @export
as.data.table.ParamSet = function(x, ...) { # nolint
  map_dtr(x$params, as.data.table)
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
