#FIXME: doc and unit test trafo und transform better, expecially in and out datatypes
#    \item{trafo}{[\code{function(x, param_set)}] \cr
#      \code{x} is a \code{data.table}, each row contains one parameter setting.
#      \code{param_set} is the param_set. Can be useful to access tags.
#      This function is called from \code{ParamSet$transform()}.
#      It has to return a \code{data.table} object with the same number of rows as \code{x}, the number and names of the columns can be completely different.
#

#' @title ParamSet
#'
#' @description
#' A set of [Param] objects.
#'
#' @section Public members / active bindings:
#' * `set_id`            :: `character(1)`
#'   ID of this param set. Settable.
#' * `params`            :: named list of [Param]
#'   Contained parameters, named with their respective IDs.
#'   NB: The returned list contains references, so you can potentially change the objects of the param set by writing to them.
#' * `length`            :: `integer(1)`
#'   Number of contained params. Read-only.
#' * `is_empty`          :: `logical(1)`
#'   Is the param set empty? Read-only.
#' * `class`             :: named `character`
#'   Param classes of contained parameters.
#'   Named with param IDs. Read-only.
#' * `lower`             :: named [double]
#'   Lower bounds of parameters, NA if param is not a number.
#'   Named with param IDs. Read-only.
#' * `upper`             :: named [double]
#'   Upper bounds of parameters, NA if param is not a number.
#'   Named with param IDs. Read-only.
#' * `values`            :: named `list`
#'   List of character vectors of allowed categorical values of contained parameters, NULL if param is not categorical.
#'   Named with param IDs. Read-only.
#' * `nlevels`           :: named [double]
#'   Number of categorical levels per parameter, Inf for unbounded ints or any dbl.
#'   Named with param IDs. Read-only.
#' * `is_bounded`        :: named `logical(1)`
#'   Do all parameters have finite bounds?
#'   Named with param IDs. Read-only.
#' * `special_vals`      :: named `list` of `list` \cr
#'   Special values for all parameters.
#'   Named with param IDs. Read-only.
#' * `storage_type`      :: `character` \cr
#'   Data types of params when stored in tables.
#'   Named with param IDs. Read-only.
#' * `tags`              :: named `list` of `character` \cr
#'   Can be used to group and subset params.
#'   Named with param IDs. Read-only.
#' * `defaults`          :: named `list` \cr
#'   Default values of all params. If no default exists, element is not present.
#'   Named with param IDs. Read-only.
#' * `trafo`             :: `function(x, param_set)` -> named `list` \cr
#'   Transformation function. Settable.
#'   The function is responsible to transform a single (feasible) configuration list into
#'   another encoding, before evaluating the configuration with the target algorithm.
#'   For the output-list, not many things have to hold.
#'   It needs to be a uniquely-named list, and the target algorithm has to accept the configuarion.
#'   NB: For convenience, the self-paramset is also passed, if you need some info from it.
#' * `has_trafo`         :: `logical(1)` \cr
#'   Has the set a trafo` function?
#' * `deps`              :: list of [Dependency] \cr
#'   Parameter dependency objects, each element of the list is internally created by a call to `add_dep`.
#' * `has_deps`          :: `logical(1)` \cr
#'   Has the set param dependencies?
#' * `deps_on`          :: `data.table` \cr
#'   Table has cols `id` (`character(1)`) and `dep_parent` (`list` of `character`).
#'   List all (direct) dependency parents of a param, through parameter IDs.
#'
#' @section Public methods:
#' * `new(params)` \cr
#'   list of [Param] -> `self`
#'   Deep-clones all passed param objects.
#' * `ids(class = NULL, is_bounded = NULL, tags = NULL)` \cr
#'   `character`, `logical(1)`, `character` -> `character`
#'   Retrieves IDs of contained params based on some selections, `NULL` means no restriction.
#'   `class` and `tags` can be sets.
#' * `add(param_set)` \cr
#'   [Param] | [ParamSet] -> `self`
#'   Adds a single param or another set to this set, all params are cloned.
#' * `subset(ids)` \cr
#'   `character` -> `self`
#'   Changes the current set to the set of passed IDs.
#' * `transform(x)` \cr
#'   [data.table] -> [data.table]
#'   Transforms a collection of configurations (rows) via the associated `trafo` of the param set,
#'   so each row in the returned data.table corresponds to the origin-row with the same row-index.
#' * `test(x)`, `check(x)`, `assert(x)` \cr
#'   Three checkmate-like check-functions. Take a named list.
#'   A point x is feasible, if it configures a subset of params,
#'   all individual param constraints are satisfied and all dependencies are satisfied.
#' * `add_dep(id, on, cond)` \cr
#'   `character(1)`, `character(1)`, [Condition] -> `self`
#'    Adds a [Dependency] to this set, so that param `id` now depends on param `on`.
#'
#' @section S3 methods and type converters:
#' * `as.data.table()` \cr
#'   Compact representation as datatable. Col types are: \cr
#'     - id: character
#'     - lower, upper: double
#'     - values: list col, with NULL elements
#'     - special_vals: list col of list
#'     - is_bounded: logical
#'     - default: list col, with NULL elements
#'     - storage_type: character
#'     - tags: list col of character vectors
#' @name ParamSet
#' @export
ParamSet = R6Class("ParamSet",
  public = list(
    params = NULL,
    deps = NULL, # a list of Dependency objects

    initialize = function(params = list()) {
      assert_list(params, types = "Param")
      self$params = map(params, function(p) p$clone(deep = TRUE))
      names(self$params) = map_chr(params, "id")
      self$set_id = "paramset"
    },

    add = function(p) {
      assert_multi_class(p, c("Param", "ParamSet"))
      if (test_r6(p, "Param")) # level-up param to set
        p = ParamSet$new(list(p))
      ids_inboth = intersect(self$ids(), p$ids())
      if (length(ids_inboth) > 0L)
        stopf("Name clash when adding. These ids are in both sets: %s", str_collapse(ids_inboth))
      if (!is.null(p$trafo))
        stop("Cannot add a param set with a trafo.")
      ps2 = p$clone(deep = TRUE)
      self$params = c(self$params, ps2$params)
      self$deps = c(self$deps, ps2$deps)
      invisible(self)
    },

    transform = function(x) {
      assert_data_table(x)
      assert_set_equal(names(x), self$ids())
      if (is.null(self$trafo)) # if no trafo is there, we spit out the input - and assume its feasible
        return(x)
      xs = self$trafo(x = x, param_set = self)
      assert_data_table(xs)
      return(xs)
    },

    ids = function(class = NULL, is_bounded = NULL, tags = NULL) {
      if (is.null(class) && is.null(is_bounded) && is.null(tags))
        return(names(self$params))
      assert_character(class, any.missing = FALSE, null.ok = TRUE)
      assert_flag(is_bounded, null.ok = TRUE)
      assert_character(tags, any.missing = FALSE, null.ok = TRUE)
      d = as.data.table(self)
      pc = class; isb = is_bounded; tgs = tags # rename for dt, sucks
      d[  (is.null(pc) | d$class %in% pc) &
          (is.null(isb) | is_bounded %in% isb) &
          (is.null(tgs) | d$tags %in% tgs), id]
    },

    subset = function(ids) {
      assert_subset(ids, self$ids())
      self$params = self$params[ids]
      invisible(self)
    },

    check = function(xs) {
      ok = check_list(xs)
      if (!isTRUE(ok))
        return(ok)
      if (length(xs) == 0)
        return(TRUE) # a empty list is always feasible
      ok = check_names(names(xs), subset.of = self$ids())
      if (!isTRUE(ok))
        return(ok)
      # check each parameters feasibility
      for (id in names(xs)) {
        ch = self$params[[id]]$check(xs[[id]])
        if (test_string(ch)) # we failed a check, return string
          return(paste0(id,": ",ch))
      }
      # check dependencies
      nxs = names(xs)
      if (self$has_deps) {
        for (dep in self$deps) {
          p1id = dep$param$id
          p2id = dep$parent$id
          # we are ONLY ok if:
          # - if param is there, then parent must be there, then cond must be true
          # - if param is not there
          ok = (p1id %in% nxs && p2id %in% nxs && dep$cond$eval(xs[[p2id]])) ||
               (p1id %nin% nxs)
          if (isFALSE(ok)) {
            val = xs[[p2id]]
            val = ifelse(is.null(val), "<not-there>", val)
            return(sprintf("Condition for '%s' not ok: %s %s %s; instead: %s=%s",
              dep$param$id, dep$parent$id, dep$cond$type, str_collapse(dep$cond$rhs), p2id, val))
          }
        }
      }
      return(TRUE) # we passed all checks
    },

    test = function(xs) makeTest(res = self$check(xs)),

    assert = function(xs, .var.name = vname(xs)) makeAssertion(xs, self$check(xs), .var.name, NULL),

    add_dep = function(id, on, cond) {
      assert_choice(id, self$ids())
      assert_choice(on, self$ids())
      p1 = self$params[[id]]
      p2 = self$params[[on]]
      dep = Dependency$new(p1, p2, cond)
      self$deps = c(self$deps, list(dep))
      invisible(self)
    },

    # printer, prints the set as a datatable, with the option to hide some cols
    print = function(..., hide.cols = c("nlevels", "is_bounded", "special_vals", "tags", "storage_type")) {
      catf("ParamSet: %s", self$set_id)
      if (self$is_empty) {
        catf("Empty.")
      } else {
        d = as.data.table(self)
        assert_subset(hide.cols, names(d))
        if (self$has_deps) {  # add a nice extra charvec-col to the tab, which lists all parents-ids
          dtab = self$deps_on
          d = merge(d, dtab, by = "id", all.x = TRUE) # left outer join d, dtab (dtab is incomplete)
        }
        print(d[, setdiff(colnames(d), hide.cols), with = FALSE])
      }
      if (!is.null(self$trafo))
        catf("Trafo is set.") # printing the trafa functions sucks (can be very long). dont see a nother option then to suppress it for now
    }
  ),

  active = list(
    set_id = function(v) {
      if (missing(v)) {
        private$.set_id
      } else {
        assert_string(v)
        assert_names(v, type = "strict")
        private$.set_id = v
      }
    },
    length = function() length(self$params),
    is_empty = function() self$length == 0L,
    class = function() private$get_member_with_idnames("class", as.character),
    lower = function() private$get_member_with_idnames("lower", as.double),
    upper = function() private$get_member_with_idnames("upper", as.double),
    values = function() private$get_member_with_idnames("values", as.list),
    nlevels = function() private$get_member_with_idnames("nlevels", as.double),
    is_bounded = function() all(map_lgl(self$params, "is_bounded")),
    special_vals = function() private$get_member_with_idnames("special_vals", as.list),
    defaults = function() Filter(is_proper_default, private$get_member_with_idnames("default", as.list)),
    tags = function() private$get_member_with_idnames("tags", as.list),
    storage_type = function() private$get_member_with_idnames("storage_type", as.character),
    # FIXME: doc is_number and is_categ? or can we remove these 2? they suck a bit
    is_number = function() self$class %in% c("ParamDbl", "ParamInt"),
    is_categ = function() self$class %in% c("ParamFct", "ParamLgl"),
    trafo = function(f) {
      if (missing(f))
        private$.trafo
      else
        private$.trafo = assert_function(f, args = c("x", "param_set"), null.ok = TRUE)
    },
    has_trafo = function() !is.null(private$.trafo),
    has_deps = function() length(self$deps) > 0L,
    deps_on = function() {
      dtab = map_dtr(self$deps, function(d) data.table(id = d$param$id, dep_parent = list(d$parent$id)))
      dtab[, .(dep_parent = list(unlist(dep_parent))), by = id] # join par-charvecs rows with same ids
    }
  ),

  private = list(
    .set_id = NULL,
    .trafo = NULL,

    # return a slot / AB, as a named vec, named with id (and can enfore a certain vec-type)
    get_member_with_idnames = function(member, astype) set_names(astype(map(self$params, member)), self$ids()),

    deep_clone = function(name, value) {
      switch(name,
        "params" = map(value, function(x) x$clone(deep = TRUE)),
        "deps" = map(value, function(x) x$clone(deep = TRUE)),
        value
      )
    }
  )
)

#' @export
as.data.table.ParamSet = function(x, ...) {
  rbindlist(map(x$params, as.data.table))
}

