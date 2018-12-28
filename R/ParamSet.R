#FIXME: doc and unit test trafo und transform better, expecially in and out datatypes
#    \item{trafo}{[\code{function(x, param_set)}] \cr
#      \code{x} is a \code{data.table}, each row contains one parameter setting.
#      \code{param_set} is the param_set. Can be useful to access tags.
#      This function is called from \code{ParamSet$transform()}.
#      It has to return a \code{data.table} object with the same number of rows as \code{x}, the number and names of the columns can be completely different.
#
# FIXME: doc and unit tests dependencies better

#' @title ParamSet
#'
#' @description A set of [Param] objects.
#'
#' @section Public members / active bindings:
#' * `id`               :: `character(1)`
#'   ID of this param set. Settable.
#' * `params`           :: named list of [Param]
#'   Contained parameters, named with their respective IDs.
#'   NB: The returned list contains references, so you can potentially change the objects of the param set by writing to them.
#' * `length`           :: `integer(1)`
#'   Number of contained params. Read-only.
#' * `is_empty`         :: `logical(1)`
#'   Is the param set empty? Read-only.
#' * `ids`              :: `character`
#'   IDs of contained parameters. Read-only.
#' * `pclasses`         :: named `character`
#'   Param classes of contained parameters.
#'   Named with param IDs. Read-only.
#' * `lowers`           :: named [double]
#'   Lower bounds of parameters, NA if param is not a number.
#'   Named with param IDs. Read-only.
#' * `uppers`           :: named [double]
#'   Upper bounds of parameters, NA if param is not a number.
#'   Named with param IDs. Read-only.
#' * `values`           :: named `list`
#'   List of character vectors of allowed categorical values of contained parameters, NULL if param is not categorical.
#'   Named with param IDs. Read-only.
#' * `nlevels`          :: named [double]
#'   Number of categorical levels per parameter, Inf for unbounded ints or any dbl with lower != upper.
#'   Named with param IDs. Read-only.
#' * `is_bounded`       :: named `logical(1)`
#'   Do all parameters have finite bounds?
#'   Named with param IDs. Read-only.
#' * `storage_types`     :: `character` \cr
#'   Data types of params when stored in tables.
#'   Named with param IDs. Read-only.
#' * `tags`              :: named `list` of `character` \cr
#'   Can be used to group and subset params.
#'   Named with param IDs. Read-only.
#' * `defaults`          :: named `list` \cr
#'   Default values of all params.
#'   Named with param IDs. Read-only.
#' * `trafo`            :: `function(x)` \cr
#'   Transformation function.
#'
#' @section Public methods:
#' * `new(params)` \cr
#'   list of [Param] -> `self`
#' * `add_param(param)` \cr
#'   [Param] -> `self`
#'   Adds a param to this set, param is cloned.
#' * `add_param_set(param_set)` \cr
#'   [ParamSet] -> `self`
#'   Adds the contents of another set to this set, all params are cloned.
#' * `subset(ids)` \cr
#'   `character` -> `self`
#'   Changes the current set to the set of passed IDs.
#' * `transform(x)` \cr
#'   [data.table] -> [data.table]
#'   Transforms a collections of configurations (rows) via the associated transformation of the param set,
#'   so each row in the returned data.table corresponds to the origin-row with the same row-index.
#' * `test(x)`, `check(x)`, `assert(x)` \cr
#'   Three checkmate-like check-functions. Take a named list.
#'   A point x is feasible, if it configures a subset of params,
#'   all individual param constraints are satisfied and all dependencies are satisfied.
#' * `fix(xs)` \cr
#'   `named `list`` -> `self`
#' * `add_dependency(dep)` \cr
#'   [Dependency] -> `self`
#'
#' @section S3 methods and type converters:
#' * `as.data.table()` \cr
#'   Compact representation as datatable. Col types are: \cr
#'     - id: character
#'     - lower, upper: double
#'     - values: list col, with NULL elements
#'     - special_vals: list col of list
#'     - default: list col, with NULL elements
#'     - storage_type: character
#'     - tags: list col of character vectors
#' @name ParamSet
#' @export
ParamSet = R6Class("ParamSet",
  public = list(
    params = NULL,
    trafo = NULL, # function to transform the value before evaluation
    deps = NULL, # a list of Dependency objects

    # this does a deep copy of all passed param objects
    # FIXME: trafo should be an AB
    # FIXME: id should be an AB
    initialize = function(params = list(), id = "paramset", trafo = NULL) {
      assert_list(params, types = "Param")
      self$params = map(params, function(p) p$clone(deep = TRUE))
      names(self$params) = map_chr(params, "id")
      assert_string(id)
      assert_names(id, type = "strict")
      self$id = id
      self$trafo = assert_function(trafo, args = c("x", "param_set"), null.ok = TRUE)
    },

    add_param = function(param) {
      assert_r6(param, "Param")
      self$params[[param$id]] = param$clone()
      invisible(self)
    },

    add_param_set = function(param_set) {
      ids_inboth = intersect(self$ids, param_set$ids)
      if (length(ids_inboth) > 0L)
        stop("Name clash when adding a set. These ids are in both sets: %s", str_collapse(ids_inboth))
      if (!is.null(param_set$trafo))
        stop("Cannot add a param set with a trafo.")
      if (!is.null(param_set$deps))
        stop("Cannot add a param set with dependencies.")
      ps2 = param_set$clone(deep = TRUE)
      self$params = c(self$params, ps2$params)
      invisible(self)
    },

    # takes data.table and calls self$trafo on this data.table. Returns data.table.
    transform = function(x) {
      assert_data_table(x)
      assert_set_equal(names(x), self$ids)
      if (is.null(self$trafo))
        return(x)
      xs = self$trafo(x = x, param_set = self)
      assert_data_table(xs)
      return(xs)
    },

    # fix (named list of parameter values to keep fixed)
    # creates a subset of self (cloned) with all params that are not mentioned in fix
    # adds ParamFix param for all dropped Params
    # out: ParamSet
    # FIXME: doc and unit test
    fix = function(xs) {
      assert_list(x, names = "named")
      assert_subset(names(x), self$ids)
      for (param_id in names(x)) {
        ps = self$get_param(param_id)
        ps$fix(x[[param_id]])
      }
      invisible(self)
    },

    subset = function(ids) {
      assert_subset(ids, self$ids)
      self$params = self$params[ids]
      invisible(self)
    },

    check = function(xs) {

      ok = check_list(xs)
      if (!isTRUE(ok))
        return(ok)
      if (length(xs) == 0)
        return(TRUE) # a empty list is always feasible
      ok = check_names(names(xs), subset.of = self$ids)
      if (!isTRUE(ok))
        return(ok)

      # check each parameters feasibility
      for (id in names(xs)) {
        ch = self$params[[id]]$check(xs[[id]])
        if (test_string(ch)) # we failed a check, return string
          return(paste0(id,": ",ch))
      }

      # check dependencies
      if (length(self$deps) > 0) {
        for (dep in self$deps) {
          dep_res = dep$condition$eval(xs[[dep$parent_id]])
          if (isFALSE(dep_res) && dep$node_id %in% names(xs)) {
            # FIXME: we should say something about what the condition is on / which params
            return(sprintf("Param '%s' present, but condition '%s' not fulfilled.", dep$node_id, dep$condition$id))
          }
        }
      }

      return(TRUE) # we passed all checks
    },

    test = function(xs) {
      makeTest(res = self$check(xs))
    },

    assert = function(xs, .var.name = vname(xs)) {
      makeAssertion(xs, self$check(xs), .var.name, NULL)
    },

    add_dependency = function(dep) {
      assert_r6(dep, "Dependency")
      # check that dependency makes sense
      assert_choice(dep$parent_id, self$ids)
      assert_choice(dep$node_id, self$ids)
      # add dependency to member list
      self$deps = c(self$deps, list(dep))
      invisible(self)
    },

    # printer, prints the set as a datatable, with the option to hide some cols
    print = function(..., hide.cols = c("tags")) {
      catf("ParamSet: %s", self$id)
      if (self$is_empty) {
        catf("Empty.")
      } else {
        catf("Params:")
        d = as.data.table(self)
        assert_subset(hide.cols, names(d))
        print(d[, setdiff(colnames(d), hide.cols), with = FALSE])
      }
      if (!is.null(self$trafo)) {
        catf("Trafo is set:")
        print(self$trafo)
      }
    }
  ),

  active = list(
    id = function(v) if (missing(v)) private$.id else private$.id = v,
    length = function() length(self$params),
    is_empty = function() self$length == 0L,
    ids = function() unname(map_chr(self$params, "id")),
    pclasses = function() private$get_member_with_idnames("pclass", as.character),
    lowers = function() private$get_member_with_idnames("lower", as.double),
    uppers = function() private$get_member_with_idnames("upper", as.double),
    values = function() private$get_member_with_idnames("values", as.list),
    nlevels = function() private$get_member_with_idnames("nlevels", as.double),
    is_bounded = function() all(map_lgl(self$params, "is_bounded")),
    defaults = function() private$get_member_with_idnames("default", as.list),
    tags = function() private$get_member_with_idnames("tags", as.list),
    storage_types = function() private$get_member_with_idnames("storage_type", as.character),
    # FIXME: doc is_number and is_categ
    is_number = function() self$pclasses %in% c("ParamDbl", "ParamInt"),
    is_categ = function() self$pclasses %in% c("ParamFct", "ParamLgl")
  ),

  private = list(
    .id = NULL,
    # FIXME: doc
    get_member_with_idnames = function(member, astype) set_names(astype(map(self$params, member)), self$ids),

    # FIXME: we need to copy dep objects too, and check that this works in deep clone
    deep_clone = function(name, value) {
      if (name == "params") map(value, function(x) x$clone(deep = TRUE)) else value
    }
  )
)

#' @export
as.data.table.ParamSet = function(x, ...) {
  rbindlist(map(x$params, as.data.table))
}

