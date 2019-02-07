#FIXME: was ist mit so krams wie nvlevels usw? das wollen wir nicht änern oder?

#' @title ParamSet
#'
#' @description
#' A set of [Param] objects. Please note that when creating a set or adding to it, the params of the
#' resulting set have to be uniquely named with IDs with valid R names. The set also contains a member
#' variable `param_vals` which can be used to store an active configuration / or to partially fix
#' some parameters to constant values (regarding subsequent sampling or generation of designs).
#'
#' @section Public members / active bindings:
#' * `set_id`            :: `character(1)` \cr
#'   ID of this param set. Settable.
#' * `params`            :: named list of [Param] \cr
#'   Contained parameters, named with their respective IDs.
#'   NB: The returned list contains references, so you can potentially change the objects of the param set by writing to them.
#' * `length`            :: `integer(1)` \cr
#'   Number of contained params. Read-only.
#' * `is_empty`          :: `logical(1)` \cr
#'   Is the param set empty? Read-only.
#' * `class`             :: named `character` \cr
#'   Param classes of contained parameters.
#'   Named with param IDs. Read-only.
#' * `lower`             :: named [double] \cr
#'   Lower bounds of parameters, NA if param is not a number.
#'   Named with param IDs. Read-only.
#' * `upper`             :: named [double] \cr
#'   Upper bounds of parameters, NA if param is not a number.
#'   Named with param IDs. Read-only.
#' * `values`            :: named `list` \cr
#'   List of character vectors of allowed categorical values of contained parameters, NULL if param is not categorical.
#'   Named with param IDs. Read-only.
#' * `nlevels`           :: named [double] \cr
#'   Number of categorical levels per parameter, Inf for unbounded ints or any dbl.
#'   Named with param IDs. Read-only.
#' * `is_bounded`        :: named `logical(1)` \cr
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
#' * `default`          :: named `list` \cr
#'   Default values of all params. If no default exists, element is not present.
#'   Named with param IDs. Read-only.
#' * is_number           :: named `logical` \cr
#'   Position is TRUE iff Param is dbl or int.
#'   Named with param IDs. Read-only.
#' * is_categ          :: named `logical` \cr
#'   Position is TRUE iff Param is fct or lgl.
#'   Named with param IDs. Read-only.
#' * `trafo`             :: `function(x, param_set)` -> named `list` \cr
#'   Transformation function. Settable.
#'   User has to pass a `function(x, param_set)`, of the form `named list`, [ParamSet] -> `named list`.
#'   The function is responsible to transform a feasible configuration into
#'   another encoding, before potentially evaluating the configuration with the target algorithm.
#'   For the output, not many things have to hold.
#'   It needs to have unique names, and the target algorithm has to accept the configuration.
#'   For convenience, the self-paramset is also passed in, if you need some info from it (e.g. tags).
#'   Is NULL by default, and you can set it to NULL to switch the transformation off.
#' * `has_trafo`         :: `logical(1)` \cr
#'   Has the set a trafo` function?
#' * `has_deps`          :: `logical(1)` \cr
#'   Has the set param dependencies?
#' * `deps`          :: `data.table` \cr
#'   Table has cols `id` (`character(1)`) and `on` (`character(1)`) and `cond` ([Condition]).
#'   Lists all (direct) dependency parents of a param, through parameter IDs.
#'   Internally created by a call to `add_dep`.
#'   Settable, if you want to remove dependencies or perform other changes.
#' * `param_vals`         :: named `list` \cr
#'   Currently set / fixed parameter values.
#'   Settable, and feasibility of values will be checked when you set them.
#'   You do not have to set values for all parameters, but only for a subset.
#'   When you set values, all previously set values will be unset / removed.
#'
#' @section Public methods:
#' * `new(params)` \cr
#'   list of [Param] -> `self` \cr
#'   Deep-clones all passed param objects.
#' * `ids(class = NULL, is_bounded = NULL, tags = NULL)` \cr
#'   `character`, `logical(1)`, `character` -> `character` \cr
#'   Retrieves IDs of contained params based on some selections, `NULL` means no restriction.
#'   `class` and `tags` can be sets.
#' * `add(param_set)` \cr
#'   [Param] | [ParamSet] -> `self` \cr
#'   Adds a single param or another set to this set, all params are cloned.
#' * `subset(ids)` \cr
#'   `character` -> `self` \cr
#'   Changes the current set to the set of passed IDs.
#' * `test(x)`, `check(x)`, `assert(x)` \cr
#'   Three checkmate-like check-functions. Take a named list.
#'   A point x is feasible, if it configures a subset of params,
#'   all individual param constraints are satisfied and all dependencies are satisfied.
#'   Params for which dependencies are not satisfied should not be part of `x`.
#' * `add_dep(id, on, cond)` \cr
#'   `character(1)`, `character(1)`, [Condition] -> `self` \cr
#'    Adds a dependency to this set, so that param `id` now depends on param `on`.
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

    initialize = function(params = named_list()) {
      assert_list(params, types = "Param")
      ids = map_chr(params, "id")
      assert_names(ids, type = "strict")
      private$.params = map(params, function(p) p$clone(deep = TRUE))
      names(private$.params) = ids
      self$set_id = "paramset"
    },

    add = function(p) {
      assert_multi_class(p, c("Param", "ParamSet"))
      if (test_r6(p, "Param")) # level-up param to set
        p = ParamSet$new(list(p))
      assert_names(c(self$ids(), p$ids()), type = "strict")
      if (!is.null(p$trafo))
        stop("Cannot add a param set with a trafo.")
      ps2 = p$clone(deep = TRUE)
      private$.params = c(private$.params, ps2$params)
      private$.param_vals = c(private$.param_vals, ps2$param_vals)
      private$.deps = rbind(private$.deps, ps2$deps)
      invisible(self)
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
      if (self$has_deps) { # check that all required / leftover parents are still in new ids
        parents = unique(self$deps[id %in% ids, "on"][[1L]])
        pids_not_there = setdiff(parents, ids)
        if (length(pids_not_there) > 0L)
         stopf("Subsetting so that dependencies on params exist which would be gone: %s.\nIf you still want to do that, manipulate '$deps' yourself.", str_collapse(pids_not_there))
      }
      private$.params = private$.params[ids]
      ids2 = intersect(ids, names(private$.param_vals)) # restrict to ids already in pvals
      private$.param_vals = private$.param_vals[ids2]
      invisible(self)
    },

    check = function(xs) {
      ok = check_list(xs, names = "unique")
      if (!isTRUE(ok))
        return(ok)
      if (length(xs) == 0)
        return(TRUE) # a empty list is always feasible
      ns = names(xs)
      ids = self$ids()
      # check that all 'required' params are there
      required = self$ids(tags = "required")
      required = setdiff(required, ns)
      if (length(required) > 0L)
        stopf("Missing required parameters: %s", str_collapse(required))
      # check each parameters feasibility
      for (n in ns) {
        if (n %nin% ids)
          return(sprintf("Parameter '%s' not available.%s", n, did_you_mean(n, ids)))
        ch = self$params[[n]]$check(xs[[n]])
        if (test_string(ch)) # we failed a check, return string
          return(paste0(n,": ",ch))
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
            val = xs[[p2id]]
            val = ifelse(is.null(val), "<not-there>", val)
            return(sprintf("Condition for '%s' not ok: %s %s %s; instead: %s=%s",
              p1id, p2id, cond$type, str_collapse(cond$rhs), p2id, val))
          }
        }
      }
      return(TRUE) # we passed all checks
    },

    test = function(xs) makeTest(res = self$check(xs)),

    assert = function(xs, .var.name = vname(xs)) makeAssertion(xs, self$check(xs), .var.name, NULL),

    add_dep = function(id, on, cond) {
      ids = self$ids()
      assert_choice(id, ids)
      assert_choice(on, ids)
      if (id == on)
        stopf("A param cannot depend on itself!")
      private$.deps = rbind(private$.deps, data.table(id = id, on = on, cond = list(cond)))
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
        if (self$has_deps) { # add a nice extra charvec-col to the tab, which lists all parents-ids
          dd = self$deps[, .(parents = list(unlist(on))), by = id]
          d = merge(d, dd, on = "id", all.x = TRUE)
        }
        v = named_list(d$id) # add param_vals to last col of print-dt as list col
        v = insert_named(v, self$param_vals)
        d$value = list(v)
        print(d[, setdiff(colnames(d), hide.cols), with = FALSE])
      }
      if (!is.null(self$trafo))
        catf("Trafo is set.") # printing the trafa functions sucks (can be very long). dont see a nother option then to suppress it for now
    }
  ),

  active = list(
    params = function() private$.params,
    deps = function(v) {
      if (missing(v)) {
        private$.deps
      } else {
        assert_data_table(v)
        private$.deps = v
      }
    },
    set_id = function(v) {
      if (missing(v)) {
        private$.set_id
      } else {
        assert_id(v)
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
    default = function() Filter(Negate(is_nodefault), private$get_member_with_idnames("default", as.list)),
    tags = function() private$get_member_with_idnames("tags", as.list),
    storage_type = function() private$get_member_with_idnames("storage_type", as.character),
    is_number = function() private$get_member_with_idnames("is_number", as.logical),
    is_categ = function() private$get_member_with_idnames("is_categ", as.logical),
    trafo = function(f) {
      if (missing(f)) {
        private$.trafo
      } else {
          assert_function(f, args = c("x", "param_set"), null.ok = TRUE)
        private$.trafo = f
      }
    },
    has_trafo = function() !is.null(private$.trafo),
    param_vals = function(xs) {
      if (missing(xs)) {
        return(private$.param_vals)
      } else {
        self$assert(xs)
      }
      if (length(xs) == 0L) xs = named_list()
      private$.param_vals = xs
    },
    has_deps = function() nrow(private$.deps) > 0L
  ),

  private = list(
    .set_id = NULL,
    .trafo = NULL,
    .params = NULL,
    .param_vals = named_list(),
    .deps = data.table(id = character(0L), on = character(0L), cond = list()),
    # return a slot / AB, as a named vec, named with id (and can enfore a certain vec-type)
    get_member_with_idnames = function(member, astype) set_names(astype(map(self$params, member)), self$ids()),

    deep_clone = function(name, value) {
      switch(name,
        ".params" = map(value, function(x) x$clone(deep = TRUE)),
        ".deps" = copy(value),
        value
      )
    }
  )
)

#' @export
as.data.table.ParamSet = function(x, ...) {
  rbindlist(map(x$params, as.data.table))
}

