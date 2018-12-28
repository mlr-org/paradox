#FIXME: we need to handle the specail case of fixing / mapunitint. maybe that auto-works, maybe not
# we also need to be able to ask for the fixed value?

#' @title Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' Abstract base class \code{\link[R6]{R6Class}} to represent a parameter.
#'
#' @section Public members / active bindings:
#' * `new(id, special_vals, default, tags)` \cr
#'   [character(1)], [list], [any], [character] -> self
#'   Constructor of abstract base class, only called by inheriting classes.
#' * `id`               :: [character(1)]
#'    ID of this param.
#' * `pclass`           :: [character(1)]
#'    Parameter R6 class name. Read-only.
#' * `lower`            :: [double(1)]
#'    Lower bounds of parameters, can be -Inf. NA if param is not a number.
#' * `upper`            :: [double(1)]
#'    Upper bounds of parameters, can be +Inf. NA if param is not a number.
#' * `values`           :: [character]
#'    Allowed categorical values, NULL if param is not categorical.
#' * `nlevels`          :: [double(1)]
#'    Number of categorical levels per parameter, Inf for unbounded ints or any dbl with lower != upper. Read-only.
#' * `is_bounded`       :: [logical(1)]
#'    Does param have a finitely bounded domain? Read-only.
#' * `special_vals`     :: [list] \cr
#'   Arbitrary special values this parameter is allowed to take, to make it feasible.
#'   This allows extending the domain of the param.
#' * `default`          :: [any]
#'    Default value. Can be from param domain or `special_vals`.
#'
#' @section Public methods:
#' * `test(x)`, `check(x)`, `assert(x)` \cr
#'    Three checkmate-like check-functions. Take a value from the domain of the param, and check if it is feasible.
#'    A value is feasible if it is inside of the bounds or from `special_vals`.
#' * `map_unitint_to_values(x)` \cr
#'   [numeric(n)] -> [vector(n)]
#'   Takes values from [0, 1] and maps them to a vector of feasible values, so that the values are regular distributed.
#'   Use case: Sample a uniform-[0,1] random variable, an turn it into a uniform sample from this Parameter.
#' * `rep(n)`
#'   [integer(1)] -> [ParamSet]
#'   Repeats this param n-times (by cloning); each param is named "<id>_rep_<k>" and gets additional tag "<id>_rep".
#' * `as_dt()`
#'   self -> [data.table]
#'   Converts param to datatable with 1 row. Col types are:
#'     - id: character
#'     - lower, upper: double
#'     - values: list col, with NULL elements
#'     - special_vals: list col of list
#'     - default: list col, with NULL elements
#'     - tags: list col of character vectors
#'
#' @name Parameter
#' @export

Parameter = R6Class("Parameter",
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

    assert = function(x) makeAssertFunction(self$check)(x),

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

    # FIXME: manually test the printer so it looks good
    print = function(..., hide.cols = c("tags")) {
      # this is bit bullshitty, but works by delegating to the printer of the PS
      d = ParamSet$new(list(self))$as_dt()
      assert_subset(hide.cols, names(d))
      print(d[, setdiff(colnames(d), hide.cols), with = FALSE])
    },

    map_unitint_to_values = function(x) {
      assert_numeric(x, lower = 0, upper = 1)
      private$.map_unitint_to_values(x)
    },

    fix = function(x) {
      self$check(x)
      private$.check(x)
    },

    # FIXME: S3?
    # FIXME: add a few cols to make it more usable?
    # FIXME: doc this
    as_dt = function() {
      data.table(
        id = self$id,
        pclass = self$pclass,
        lower = self$lower,
        upper = self$upper,
        values = list(self$values),
        special_vals = list(self$special_vals),
        default = list(self$default),
        tags = list(self$tags)
      )
    }
  ),

  active = list(
    pclass = function() class(self)[[1L]]
    # FIXME: spec_vals was broken and needs a proper unit test
    # FIXME: default was broken and needs a proper unit test
  ),

  private = list(
    .check = function(x) stop("abstract"),
    .map_unitint_to_values = function(x) stop("abstract"), # should be implemented by subclasses, argcheck happens in Parameter$map_unitint_to_values
    .fix = function(x) stop("abstract")
  )
)






