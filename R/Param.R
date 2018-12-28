#FIXME: we need to handle the specail case of fixing / mapunitint. maybe that auto-works, maybe not
# we also need to be able to ask for the fixed value?
# we should probably add a note for each param what fixing means

#FIXME: superclass for dbl/int, lgl/fct?

# FIXME: document args of all constructors better, it might be unclear what they mean

#' @title Param Object
#' @format [R6Class] object.
#'
#' @description
#' Abstract base class for params.
#'
#' @section Public members / active bindings:
#' * `id`               :: `character(1)` \cr
#'    ID of this param.
#' * `pclass`           :: `character(1)` \cr
#'    Param R6 class name. Read-only.
#' * `lower`            :: `numeric(1)` \cr
#'    Lower bound for dbl/int params, can be -Inf. NA if param is not a number.
#' * `upper`            :: `numeric(1)` \cr
#'    Upper bound for dbl/int params, can be +Inf. NA if param is not a number.
#' * `values`           :: `character` | `logical` | `NULL` \cr
#'    Allowed values for categorical params, NULL if param is not categorical.
#' * `nlevels`          :: `numeric(1)` \cr
#'    Number of categorical levels per parameter, Inf for unbounded ints or any dbl. Read-only.
#' * `is_bounded`       :: `logical(1)` \cr
#'    Does param have a finitely bounded domain? Read-only.
#' * `special_vals`     :: `list` \cr
#'   Arbitrary special values this parameter is allowed to take, to make it feasible.
#'   This allows extending the domain of the param.
#'   This is only used in feasibility checks, neither in generating designs nor sampling.
#' * `default`          :: `any` \cr
#'    Default value. Can be from param domain or `special_vals`.
#'    Has value `NO_DEF` if no default is there - `NULL` could be a valid default.
#' * `storage_type`     :: `character(1)` \cr
#'    Data type when values of this param is stored in a data table or sampled. Read-only.
#' * `tags`             :: `character` \cr
#'   Can be used to group and subset params.
#'
#' @section Public methods:
#' * `new(id, special_vals, default, tags)` \cr
#'   `character(1)`, `list`, `any`, `character` -> self \cr
#'   Constructor of abstract base class, only called by inheriting classes.
#' * `test(x)`, `check(x)`, `assert(x)` \cr
#'    Three checkmate-like check-functions. Take a value from the domain of the param, and check if it is feasible.
#'    A value is feasible if it is inside of the bounds or from `special_vals`.
#' * `qunif(x)` \cr
#'   `numeric(n)` -> `vector(n)` \cr
#'   Takes values from \[0,1\] and map them, regularly distributed, to the domain of the param.
#'   Think of: quantile function or the usecse to map a uniform-\[0,1\] random variable into a uniform sample from this param.
#' * `rep(n)` \cr
#'   `integer(1)` -> [ParamSet] \cr
#'   Repeats this param n-times (by cloning); each param is named "<id>_rep_<k>" and gets additional tag "<id>_rep".
#'
#' @section S3 methods and type converters:
#' * `as.data.table()` \cr
#'   Converts param to datatable with 1 row. See [ParamSet].
#'
#' @name Param
#' @family Param
#' @export
Param = R6Class("Param",
  public = list(
    id = NULL,
    special_vals = NULL,
    # FIXME: what if deafult is NULL? this is the DEFAULT for 'default' in all param subclasses....
    default = NULL,
    tags = NULL,

    # FIXME: should default of tags be char(0)? so no tags?
    initialize = function(id, special_vals, default, tags) {
      assert_string(id)
      assert_names(id, type = "strict")
      assert_list(special_vals)
      # FIXME: do we allow NULL here? or just a charvec(0)?
      assert_character(tags, any.missing = TRUE, unique = TRUE, null.ok = TRUE)

      self$id = id
      self$special_vals = special_vals
      self$default = default
      self$tags = tags
      if (is_proper_default(default)) # check that default is feasible
        self$assert(default)
    },

    check = function(x) {
      # either we are directly feasible, or in special vals, if both are untrue return errmsg from 1st check
      ch = private$.check(x)
      if (isTRUE(ch))
        return(TRUE)
      #FIXME: shuld use purrr::has_element, opened issue in mlr3misc
      else if (any(map_lgl(self$special_vals, identical, x)))
        return(TRUE)
      else
        return(ch)
    },

    assert = function(x) makeAssertionFunction(self$check)(x),

    test = function(x) makeTestFunction(self$check)(x),

    rep = function(n) {
      assert_count(n)
      pid = self$id
      join_id = paste0(pid, "_rep")
      ps = replicate(n, self$clone(), simplify = FALSE)
      for (i in 1:n) {
        p = ps[[i]]
        p$id = paste0(join_id, "_", i)
        p$tags = c(p$tags, join_id)
      }
      ParamSet$new(ps)
    },

    print = function(..., hide.cols = c("nlevels", "is_bounded", "special_vals", "tags", "storage_type")) {
      # this is bit bullshitty, but works by delegating to the printer of the PS
      d = as.data.table(ParamSet$new(list(self)))
      assert_subset(hide.cols, names(d))
      print(d[, setdiff(colnames(d), hide.cols), with = FALSE])
    },

    qunif = function(x) {
      assert_numeric(x, lower = 0, upper = 1)
      assert_true(self$is_bounded)
      private$.qunif(x)
    }
  ),

  active = list(
    pclass = function() class(self)[[1L]]
  ),

  private = list(
    .check = function(x) stop("abstract"),
    .qunif = function(x) stop("abstract") # should be implemented by subclasses, argcheck happens in Param$qunif
  )
)

#' @export
as.data.table.Param = function(x, ...) {
  data.table(
    id = x$id,
    pclass = x$pclass,
    lower = x$lower,
    upper = x$upper,
    values = list(x$values),
    nlevels = x$nlevels,
    is_bounded = x$is_bounded,
    special_vals = list(x$special_vals),
    default = list(x$default),
    storage_type = x$storage_type,
    tags = list(x$tags)
  )
}
