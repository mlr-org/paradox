#' @title Param Object
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Abstract base class for parameters.
#'
#' @section Construction:
#' ```
#' Param$new(id, special_vals, default, tags)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   ID of this parameter.
#' * `special_vals` :: `list()`\cr
#'   Arbitrary special values this parameter is allowed to take, to make it feasible.
#'   This allows extending the domain of the parameter.
#'   Note that these values are only used in feasibility checks, neither in generating designs nor sampling.
#' * `default` :: `any` \cr
#'   Default value. Can be from the domain of the parameter or an element of `special_vals`.
#'   Has value [NO_DEF] if no default exists. `NULL` can be a valid default.
#' * `tags` :: `character()`\cr
#'   Arbitrary tags to group and subset parameters. Some tags serve a special purpose:
#'   * `"required"` implies that the parameters has to be given when setting `values` in [ParamSet].
#'
#'
#' @section Fields:
#' * `class` :: `character(1)`\cr
#'    R6 class name. Read-only.
#' * `is_number` :: `logical(1)`\cr
#'   TRUE if the parameter is of type `"dbl"` or `"int"`.
#' * `is_categ` :: `logical(1)`\cr
#'   TRUE if the parameter is of type `"fct"` or `"lgl"`.
#' * `has_default` :: `logical(1)`\cr
#'    Is there a default value?
#' * `storage_type` :: `character(1)` \cr
#'    Data type when values of this parameter are stored in a data table or sampled.
#'
#' @section Methods:
#' * `test(x)`, `check(x)`, `assert(x)`\cr
#'    Three \pkg{checkmate}-like check-functions.
#'    Take a value from the domain of the parameter, and check if it is feasible.
#'    A value is feasible if it is of the same `storage_type`, inside of the bounds or element of `special_vals`.
#' * `qunif(x)`\cr
#'   `numeric(n)` -> `vector(n)` \cr
#'   Takes values from \[0,1\] and map them, regularly distributed, to the domain of the parameter.
#'   Think of: quantile function or the use case to map a uniform-\[0,1\] random variable into a uniform sample from this param.
#' * `rep(n)`\cr
#'   `integer(1)` -> [ParamSet]\cr
#'   Repeats this parameter n-times (by cloning).
#'   Each parameter is named "<id>_rep_<k>" and gets the additional tag "<id>_rep".
#'
#' @section S3 methods:
#' * `as.data.table()` \cr
#'   Converts param to `data.table()` with 1 row. See [ParamSet].
#'
#' @family Params
#' @export
Param = R6Class("Param",
  public = list(
    id = NULL,
    special_vals = NULL,
    default = NULL,
    tags = NULL,

    initialize = function(id, special_vals, default, tags) {
      assert_id(id)
      assert_names(id, type = "strict")
      assert_list(special_vals)
      assert_character(tags, any.missing = FALSE, unique = TRUE)

      self$id = id
      self$special_vals = special_vals
      self$default = default
      self$tags = tags
      if (!is_nodefault(default)) { # check that default is feasible
        self$assert(default)
      }
    },

    check = function(x) {
      # either we are directly feasible, or in special vals, if both are untrue return errmsg from 1st check
      ch = private$.check(x)
      ifelse(isTRUE(ch) || has_element(self$special_vals, x), TRUE, ch)
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

    print = function(..., hide_cols = c("nlevels", "is_bounded", "special_vals", "tags", "storage_type")) {
      # this is bit bullshitty, but works by delegating to the printer of the PS
      d = as.data.table(ParamSet$new(list(self)))
      assert_subset(hide_cols, names(d))
      print(d[, setdiff(colnames(d), hide_cols), with = FALSE])
    },

    qunif = function(x) {
      assert_numeric(x, lower = 0, upper = 1)
      assert_true(self$is_bounded)
      private$.qunif(x)
    }
  ),

  active = list(
    class = function() class(self)[[1L]],
    is_number = function() self$class %in% c("ParamDbl", "ParamInt"),
    is_categ = function() self$class %in% c("ParamFct", "ParamLgl"),
    has_default = function() !is_nodefault(self$default)
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
    class = x$class,
    lower = x$lower,
    upper = x$upper,
    levels = list(x$levels),
    nlevels = x$nlevels,
    is_bounded = x$is_bounded,
    special_vals = list(x$special_vals),
    default = list(x$default),
    storage_type = x$storage_type,
    tags = list(x$tags)
  )
}
