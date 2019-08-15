#' @title ParamSet
#'
#' @description
#' A set of [Param] objects.
#' Please note that when creating a set or adding to it, the parameters of the
#' resulting set have to be uniquely named with IDs with valid R names.
#' The set also contains a member variable `values` which can be used to store an active configuration / or to partially fix
#' some parameters to constant values (regarding subsequent sampling or generation of designs).
#'
#' @section Construction:
#' ```
#' ParamSet$set(params = named_list())
#' ```
#'
#' * `params` :: named `list()`\cr
#'   List of [Param], named with their respective ID.
#'   Parameters are cloned.
#'
#' @section Fields:
#' * `set_id` :: `character(1)` \cr
#'   ID of this param set. Default `""`. Settable.
#' * `length` :: `integer(1)` \cr
#'   Number of contained [Param]s.
#' * `is_empty` :: `logical(1)` \cr
#'   Is the `ParamSet` empty?
#' * `class` :: named `character()` \cr
#'   Classes of contained parameters, named with parameter IDs.
#' * `lower` :: named `double()` \cr
#'   Lower bounds of parameters (`NA` if parameter is not numeric).
#'   Named with parameter IDs.
#' * `upper` :: named `double()` \cr
#'   Upper bounds of parameters (`NA` if parameter is not numeric).
#'   Named with parameter IDs.
#' * `levels` :: named `list()` \cr
#'   List of character vectors of allowed categorical values of contained parameters.
#'   `NULL` if the parameter is not categorical.
#'   Named with parameter IDs.
#' * `nlevels` :: named `integer()` \cr
#'   Number of categorical levels per parameter, `Inf` for double parameters or unbounded integer parameters.
#'   Named with param IDs.
#' * `is_bounded` :: named `logical(1)` \cr
#'   Do all parameters have finite bounds?
#'   Named with parameter IDs.
#' * `special_vals` :: named `list()` of `list()` \cr
#'   Special values for all parameters.
#'   Named with parameter IDs.
#' * `storage_type` :: `character()` \cr
#'   Data types of parameters when stored in tables.
#'   Named with parameter IDs.
#' * `tags` :: named `list()` of `character()` \cr
#'   Can be used to group and subset parameters.
#'   Named with parameter IDs.
#' * `default` :: named `list()` \cr
#'   Default values of all parameters.
#'   If no default exists, element is not present.
#'   Named with parameter IDs.
#' * is_number :: named `logical()` \cr
#'   Position is TRUE for [ParamDbl] and [ParamInt].
#'   Named with parameter IDs.
#' * is_categ          :: named `logical` \cr
#'   Position is TRUE for [ParamFct] and [ParamLgl].
#'   Named with parameter IDs.
#' * `has_deps` :: `logical(1)` \cr
#'   Has the set parameter dependencies?
#' * `deps` :: [data.table::data.table()] \cr
#'   Table has cols `id` (`character(1)`) and `on` (`character(1)`) and `cond` ([Condition]).
#'   Lists all (direct) dependency parents of a param, through parameter IDs.
#'   Internally created by a call to `add_dep`.
#'   Settable, if you want to remove dependencies or perform other changes.
#' * `values`         :: named `list()` \cr
#'   Currently set / fixed parameter values.
#'   Settable, and feasibility of values will be checked when you set them.
#'   You do not have to set values for all parameters, but only for a subset.
#'   When you set values, all previously set values will be unset / removed.
#' * `trafo` :: `function(x, param_set)`\cr
#'   Transformation function. Settable.
#'   User has to pass a `function(x, param_set)`, of the form \cr
#'   (named `list()`, [ParamSet]) -> named `list()`.\cr
#'   The function is responsible to transform a feasible configuration into another encoding, before potentially evaluating the configuration with the target algorithm.
#'   For the output, not many things have to hold.
#'   It needs to have unique names, and the target algorithm has to accept the configuration.
#'   For convenience, the self-paramset is also passed in, if you need some info from it (e.g. tags).
#'   Is NULL by default, and you can set it to NULL to switch the transformation off.
#' * `has_trafo` :: `logical(1)` \cr
#'   Has the set a `trafo` function?
#'
#' @section Public methods:
#' * `ids(class = NULL, is_bounded = NULL, tags = NULL)` \cr
#'   (`character`, `logical(1)`, `character()`) -> `character()` \cr
#'   Retrieves IDs of contained parameters based on some filter criteria selections, `NULL` means no restriction.
#' * `get_values(class = NULL, is_bounded = NULL, tags = NULL)` \cr
#'   (`character()`, `logical(1)`, `character()`) -> named `list()` \cr
#'   Retrieves parameter values based on some selections, `NULL` means no restriction and is
#'   equivalent to `$values`.
#' * `add(param_set)` \cr
#'   ([Param] | [ParamSet]) -> `self` \cr
#'   Adds a single param or another set to this set, all params are cloned.
#' * `subset(ids)` \cr
#'   `character()` -> `self` \cr
#'   Changes the current set to the set of passed IDs.
#' * `test(x)`, `check(x)`, `assert(x)` \cr
#'   Three \pkg{checkmate}-like check-functions. Takes a named list.
#'   A point x is feasible, if it configures a subset of params,
#'   all individual param constraints are satisfied and all dependencies are satisfied.
#'   Params for which dependencies are not satisfied should not be part of `x`.
#' * `add_dep(id, on, cond)` \cr
#'   (`character(1)`, `character(1)`, [Condition]) -> `self` \cr
#'    Adds a dependency to this set, so that param `id` now depends on param `on`.
#'
#' @section S3 methods and type converters:
#' * `as.data.table()` \cr
#'   Compact representation as datatable. Col types are: \cr
#'     - id: character
#'     - lower, upper: double
#'     - levels: list col, with NULL elements
#'     - special_vals: list col of list
#'     - is_bounded: logical
#'     - default: list col, with NULL elements
#'     - storage_type: character
#'     - tags: list col of character vectors
#' @examples
#' ps = ParamSet$set(
#'   params = list(
#'     ParamDbl$set("d", lower = -5, upper = 5, default = 0),
#'     ParamFct$set("f", levels = letters[1:3])
#'   )
#' )
#'
#' ps$trafo = function(x, param_set) {
#'   x$d = 2^d
#'   return(x)
#' }
#'
#' ps$add(ParamInt$set("i", lower = 0L, upper = 16L))
#'
#' ps$check(list(d = 2.1, f = "a", i = 3L))
#' @export
ParamSet <- R6Class("ParamSet")
ParamSet$set("public","initialize",function(...){
      params = as.list(sys.call(1))
      params[[1]] = NULL
      if(length(params)==0)
        stop("ParamSet can't be empty.")
      params = map(params, eval)
      invisible(map(params, assertSetInterval))
      assert_names(names(params), type = "strict")
      private$.params = params
      self$set_id = ""
    }) # DONE
ParamSet$set("public","add",function(p) {

      assert_multi_class(p, c("Param", "ParamSet"))
      p = if (inherits(p, "Param")) { # level-up param to set
        ParamSet$set(list(p))
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
    }) # TO DO
ParamSet$set("public","ids",function(class = NULL, is_bounded = NULL, tags = NULL) {

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
        ii = ii & self$is_bounded
      }

      if (!is.null(tags)) {
        assert_character(tags, any.missing = FALSE)
        ii = ii & map_lgl(self$tags, function(required, set) all(required %in% set), required = tags)
      }

      ids[ii]
    }) # TO DO
ParamSet$set("public","get_values",function(class = NULL, is_bounded = NULL, tags = NULL) {
      values = self$values
      values[intersect(names(values), self$ids(class = class, is_bounded = is_bounded, tags = tags))]
    }) # TO DO
ParamSet$set("public","subset",function(ids) {
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
    }) # TO DO
ParamSet$set("public","check",function(xs) {

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
        ch = self$params[[n]]$liesInSetInterval(xs[[n]])
        if (!ch) { # we failed a check, return string
          return(sprintf("The value %s is not contained in the Set/Interval %s", xs[[n]],
                         self$params[[n]]$strprint()))
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
          ok = (p1id %in% ns && p2id %in% ns && xs[[p2id]] == cond) ||
            (p1id %nin% ns)
          if (isFALSE(ok)) {
            val = xs[[p2id]]
            val = ifelse(is.null(val), "<not-there>", val)
            return(sprintf("Condition for '%s' not ok: %s -> %s = %s; instead: %s=%s",
              p1id, p1id, p2id, cond, p2id, val))
          }
        }
      }

      return(TRUE) # we passed all checks
    }) # UNCHANGED
ParamSet$set("public","test",function(xs){
  makeTest(res = self$check(xs))
}) # UNCHANGED
ParamSet$set("public","assert",function(xs, .var.name = vname(xs)){
  makeAssertion(xs, self$check(xs), .var.name, NULL)
}) # UNCHANGED
ParamSet$set("public","add_dep",function(id, on, cond) {
      id <- as.character(substitute(id))
      on <- as.character(substitute(on))
      ids = names(self$params)
      assert_choice(id, ids)
      assert_choice(on, ids)
      if (id == on) {
        stopf("A param cannot depend on itself!")
      }
      private$.deps = rbind(private$.deps, data.table(id = id, on = on, cond = list(cond)))
      invisible(self)
    }) # DONE - Now uses substitute
ParamSet$set("public","print",function(..., hide_cols = c("special_vals","tags","parents")) {
      catf("ParamSet: %s", self$set_id)
      if (self$is_empty) {
        catf("Empty.")
      } else {
        d = as.data.table(self)
        if (self$has_deps) { # add a nice extra charvec-col to the tab, which lists all parents-ids
          dd = self$deps[, .(parents = list(unlist(on))), by = id]
          d = merge(d, dd, on = "id", all.x = TRUE, sort = FALSE)
        }
        # v = named_list(d$id) # add values to last col of print-dt as list col
        # v = insert_named(v, self$values)
        # d$value = list(v)
        d$support <- map(d$support, strprint)
        d[d == "NULL"] = "-"

        assert_subset(hide_cols, names(d))
        print(d[, setdiff(colnames(d), hide_cols), with = FALSE])
      }
      if (!is.null(self$trafo)) {
        catf("Trafo is set.")
      } # printing the trafo functions sucks (can be very long). dont see a nother option then to suppress it for now
    }) # DONE

ParamSet$set("active","params",function() {
      private$.params
    }) # DONE - UNCHANGED
ParamSet$set("active","deps",function() {
   return(private$.deps)
    }) # DONE - Now getter only, setter above
ParamSet$set("active","set_id",function(v) {
      if (missing(v)) {
        private$.set_id
      } else {
        if (!identical(v, "")) {
          assert_id(v)
          assert_names(v, type = "strict")
        }
        private$.set_id = v
      }
    }) # Is this the id of the ParamSet? Or is it a setter for Param IDs?
ParamSet$set("active","length", function() {
      length(self$params)
    }) # What's the point of this?
ParamSet$set("active","is_empty",function() {
      length(self$params) == 0L
    }) # Deprecated? ParamSet can't be constructed empty
ParamSet$set("active","class",function() {
  map(self$params,getR6Class)
}) # DONE - Now returns R6Class name
ParamSet$set("active","lower",function() {
  lapply(self$params, function(y) y$inf())
}) # DONE
ParamSet$set("active","upper",function() {
  lapply(self$params, function(y) y$sup())
}) # DONE
ParamSet$set("active","levels",function() {
  lapply(self$params, function(y) y$elements())
    }) # DONE
ParamSet$set("active","nlevels",function() {
  lapply(self$params, function(y) length(y$elements()))
    }) # DONE
ParamSet$set("active","is_bounded", function() {
  map(self$params, testBounded)
}) # DONE
ParamSet$set("active", "special_vals", function(xs) {
  if (missing(xs)) {
    return(private$.special_vals)
  }

  self$assert(xs)
  if (length(xs) == 0L) xs = named_list()
  private$.special_vals = xs
    }) # DONE - Acts like tag
ParamSet$set("active","default",function() {
      discard(private$get_member_with_idnames("default", as.list), is_nodefault)
    }) # Suggestion: Remove default and instead initialize value when possible
ParamSet$set("active","tags",function(xs) {
  if (missing(xs)) {
    return(private$.tags)
  }

  self$assert(xs)
  if (length(xs) == 0L) xs = named_list()
  private$.tags = xs
    }) # Can tags be anything? Are they a vector? A specific set of values?
ParamSet$set("active","storage_type",function() {
      private$get_member_with_idnames("storage_type", as.character)
    }) # What's this for?
ParamSet$set("active","is_number",function() {
  lapply(self$params, function(x) return(x$class() %in% c("numeric", "integer")))
}) # DONE
ParamSet$set("active","is_categ",function() {
  lapply(self$params, function(x) return(!(x$class() %in% c("numeric", "integer"))))
}) # DONE
ParamSet$set("active","trafo",function(f) {
      if (missing(f)) {
        private$.trafo
      } else {
        assert_function(f, args = c("x", "param_set"), null.ok = TRUE)
        private$.trafo = f
      }
    }) # TO DO
ParamSet$set("active","has_trafo",function() {
      !is.null(private$.trafo)
    }) # DONE - UNCHANGED
ParamSet$set("active","values",function(xs) {
      if (missing(xs)) {
        return(private$.values)
      }

      self$assert(xs)
      if (length(xs) == 0L) xs = named_list()
      private$.values = xs
    }) # DONE - UNCHANGED
ParamSet$set("active","has_deps",function() {
      nrow(private$.deps) > 0L
    }) # DONE - UNCHANGED
ParamSet$set("active","extra_values",function() {
      private$.values[names(private$.values) %nin% names(private$.params)]
    }) # What's the point of this?

ParamSet$set("private",".set_id",NULL) # UNCHANGED
ParamSet$set("private",".trafo", NULL) # UNCHANGED
ParamSet$set("private",".special_vals", named_list()) # DONE - Added special_vals to ParamSet
ParamSet$set("private",".tags", named_list()) # DONE - Added tags to ParamSet
ParamSet$set("private",".params", NULL) # UNCHANGED
ParamSet$set("private",".values", named_list()) # UNCHANGED
ParamSet$set("private",".deps",data.table(id = character(0L), on = character(0L), cond = list())) # UNCHANGED
# return a slot / AB, as a named vec, named with id (and can enforce a certain vec-type)
ParamSet$set("private","get_member_with_idnames", function(member, astype){
  set_names(astype(map(self$params, member)), names(self$params))
}) # UNCHANGED
ParamSet$set("private","deep_clone", function(name, value) {
      switch(name,
        ".params" = map(value, function(x) x$clone(deep = TRUE)),
        ".deps" = copy(value),
        value
      )
    }) # UNCHANGED

#' @export
as.data.table.ParamSet = function(x, ...) {
  supp <- map(x$params, strprint)
  val = spe.vals = tags = dep.lst = named_list(x$ids())

  val <- insert_named(val, x$values)
  spe.vals <- insert_named(spe.vals, x$special_vals)
  tags <- insert_named(tags, x$tags)

  return(data.table(id = x$ids(), value = val, support = x$params, special_vals = spe.vals, tags = tags))
} # DONE
