#' @title ParamSet
#'
#' @description
#' A set of [Param] objects.
#' Please note that when creating a set or adding to it, the parameters of the
#' resulting set have to be uniquely named with IDs with valid R names.
#' The set also contains a member variable `values` which can be used to store an active configuration / or to partially fix
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

      nn = c(names(self$params), names(p$params))
      assert_names(nn, type = "strict")
      if (!is.null(p$trafo)) {
        stop("Cannot add a param set with a trafo.")
      }
      private$.params = c(private$.params, p$params)
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

      ids = names(self$params)
      if (is.null(class) && is.null(is_bounded) && is.null(tags)) {
        return(ids)
      }

      ii = rep(TRUE, length(ids))

      if (!is.null(class)) {
        assert_character(class, any.missing = FALSE)
        ii = ii & self$class %in% class
      }

      if (!is.null(is_bounded)) {
        assert_flag(is_bounded)
        ii = ii & map_lgl(self$params, "is_bounded")
      }

      if (!is.null(tags)) {
        assert_character(tags, any.missing = FALSE)
        ii = ii & map_lgl(self$tags, function(required, set) all(required %in% set), required = tags)
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
    #' @return Named `list()`.
    get_values = function(class = NULL, is_bounded = NULL, tags = NULL) {
      values = self$values
      values[intersect(names(values), self$ids(class = class, is_bounded = is_bounded, tags = tags))]
    },

    #' @description
    #' Changes the current set to the set of passed IDs.
    #'
    #' @param ids (`character()`).
    subset = function(ids) {
      param_ids = names(self$params)
      assert_subset(ids, param_ids)
      if (self$has_deps) { # check that all required / leftover parents are still in new ids
        parents = unique(self$deps[id %in% ids, "on"][[1L]])
        pids_not_there = setdiff(parents, ids)
        if (length(pids_not_there) > 0L) {
          stopf("Subsetting so that dependencies on params exist which would be gone: %s.\nIf you still want to do that, manipulate '$deps' yourself.", str_collapse(pids_not_there))
        }
      }
      private$.params = private$.params[ids]
      ids2 = union(intersect(ids, names(private$.values)), setdiff(names(private$.values), param_ids)) # restrict to ids already in pvals
      private$.values = private$.values[ids2]
      invisible(self)
    },

    #' @description
    #' \pkg{checkmate}-like check-function. Takes a named list.
    #' A point x is feasible, if it configures a subset of params,
    #' all individual param constraints are satisfied and all dependencies are satisfied.
    #' Params for which dependencies are not satisfied should not be part of `x`.
    #'
    #' @param xs (named `list()`).
    #' @return If successful `TRUE`, if not a string with the error message.
    check = function(xs) {

      ok = check_list(xs, names = "unique")
      if (!isTRUE(ok)) {
        return(ok)
      }
      ns = names(xs)
      ids = names(self$params)

      # check that all 'required' params are there
      required = setdiff(self$ids(tags = "required"), ns)
      if (length(required) > 0L) {
        return(sprintf("Missing required parameters: %s", str_collapse(required)))
      }
      if (length(xs) == 0) {
        return(TRUE)
      } # a empty list is always feasible, if all req params are there

      extra = wf(ns %nin% ids)
      if (length(extra)) {
        return(sprintf("Parameter '%s' not available.%s", ns[extra], did_you_mean(extra, ids)))
      }

      # check each parameters feasibility
      for (n in ns) {
        ch = self$params[[n]]$check(xs[[n]])
        if (test_string(ch)) { # we failed a check, return string
          return(paste0(n, ": ", ch))
        }
      }

      # check dependencies
      if (self$has_deps) {
        deps = self$deps
        for (j in seq_row(self$deps)) {
          p1id = deps$id[j]
          p2id = deps$on[j]
          # we are ONLY ok if:
          # - if param is there, then parent must be there, then cond must be true
          # - if param is not there
          cond = deps$cond[[j]]
          ok = (p1id %in% ns && p2id %in% ns && cond$test(xs[[p2id]])) ||
            (p1id %nin% ns)
          if (isFALSE(ok)) {
            message = sprintf("The parameter '%s' can only be set if the following condition is met '%s'.", p1id, cond$as_string(p2id))
            val = xs[[p2id]]
            if (is.null(val)) {
              message = sprintf("%s Instead the parameter value for '%s' is not set at all. Try setting '%s' to a value that satisfies the condition", message, p2id, p2id)
            } else {
              message = sprintf("%s Instead the current parameter value is: %s=%s", message, p2id, val)
            }
            return(message)
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
    #' @return If successful `TRUE`, if not `FALSE`.
    test = function(xs) makeTest(res = self$check(xs)),

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
    assert = function(xs, .var.name = vname(xs)) makeAssertion(xs, self$check(xs), .var.name, NULL),

    #' @description
    #' \pkg{checkmate}-like check-function. Takes a [data.table::data.table]
    #' where rows are points and columns are parameters. A point x is feasible,
    #' if it configures a subset of params, all individual param constraints are
    #' satisfied and all dependencies are satisfied. Params for which
    #' dependencies are not satisfied should not be part of `x`.
    #'
    #' @param xdt ([data.table::data.table]).
    #' @return If successful `TRUE`, if not a string with the error message.
    check_dt = function(xdt) {
      xss = transpose_list(xdt)
      for (xs in xss) {
        ok = self$check(xs)
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
    assert_dt = function(xdt, .var.name = vname(xdt)) makeAssertion(xdt, self$check_dt(xdt), .var.name, NULL),

    #' @description
    #' Adds a dependency to this set, so that param `id` now depends on param `on`.
    #'
    #' @param id (`character(1)`).
    #' @param on (`character(1)`).
    #' @param cond ([Condition]).
    add_dep = function(id, on, cond) {
      ids = names(self$params)
      assert_choice(id, ids)
      assert_choice(on, ids)
      assert_r6(cond, "Condition")
      if (id == on) {
        stopf("A param cannot depend on itself!")
      }
      feasible_on_values = map_lgl(cond$rhs, self$params[[on]]$test)
      if (any(!feasible_on_values)) {
        stopf("Condition has infeasible values for %s: %s", on, str_collapse(cond$rhs[!feasible_on_values]))
      }
      private$.deps = rbind(private$.deps, data.table(id = id, on = on, cond = list(cond)))
      invisible(self)
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      if (!nzchar(self$set_id)) {
        sprintf("<%s>", class(self)[1L])
      } else {
        sprintf("<%s:%s>", class(self)[1L], self$set_id)
      }
    },

    #' @description
    #' Printer.
    #'
    #' @param ... (ignored).
    #' @param hide_cols (`character()`)\cr
    #'   Which fields should not be printed? Default is `"nlevels"`,
    #'   `"is_bounded"`, `"special_vals"`, `"tags"`, and `"storage_type"`.
    # printer, prints the set as a datatable, with the option to hide some cols
    print = function(..., hide_cols = c("nlevels", "is_bounded", "special_vals", "tags", "storage_type")) {
      catf(format(self))
      if (self$is_empty) {
        catf("Empty.")
      } else {
        d = as.data.table(self)
        assert_subset(hide_cols, names(d))
        if (self$has_deps) { # add a nice extra charvec-col to the tab, which lists all parents-ids
          dd = self$deps[, .(parents = list(unlist(on))), by = id]
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
      length(self$params)
    },

    #' @field is_empty (`logical(1)`)\cr
    #' Is the `ParamSet` empty?
    is_empty = function() {
      length(self$params) == 0L
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
      all(map_lgl(self$params, "is_bounded"))
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

    #' @field trafo (`function(x, param_set)`)\cr
    #' Transformation function. Settable.
    #' User has to pass a `function(x, param_set)`, of the form\cr
    #' (named `list()`, [ParamSet]) -> named `list()`.\cr
    #' The function is responsible to transform a feasible configuration into another encoding, before potentially evaluating the configuration with the target algorithm.
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

      self$assert(xs)
      if (length(xs) == 0L) xs = named_list()
      private$.values = xs
    },

    #' @field has_deps (`logical(1)`)\cr
    #' Has the set parameter dependencies?
    has_deps = function() {
      nrow(private$.deps) > 0L
    }
  ),

  private = list(
    .set_id = NULL,
    .trafo = NULL,
    .params = NULL,
    .values = named_list(),
    .deps = data.table(id = character(0L), on = character(0L), cond = list()),
    # return a slot / AB, as a named vec, named with id (and can enforce a certain vec-type)
    get_member_with_idnames = function(member, astype) set_names(astype(map(self$params, member)), names(self$params)),

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
as.data.table.ParamSet = function(x, ...) {
  map_dtr(x$params, as.data.table)
}
