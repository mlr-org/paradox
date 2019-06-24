#' @title Param Object
#' @format [R6Class] object.
#'
#' @description
#' Abstract base class for params and inheriting concrete param subclasses.
#' * Tags: Currently, tags can be used by users in a very custom manner, but one tag is
#'   specifically handled: 'required' implied that the parameters has to be given
#'   when setting `values` in [ParamSet].
#'
#' @section Public members / active bindings:
#' * `id`               :: `character(1)` \cr
#'    ID of this param.
#' * `class`           :: `character(1)` \cr
#'    Param R6 class name. Read-only.
#' * `lower`            :: `numeric(1)` \cr
#'    Lower bound for dbl/int params, can be -Inf. NA if param is not a number.
#' * `upper`            :: `numeric(1)` \cr
#'    Upper bound for dbl/int params, can be +Inf. NA if param is not a number.
#' * `levels`           :: `character` | `logical` | `NULL` \cr
#'    Allowed levels for categorical params, NULL if param is not categorical.
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
#' * `has_default`      :: `logical(1)` \cr
#'    Is a default there?
#' * `storage_type`     :: `character(1)` \cr
#'    Data type when values of this param are stored in a data table or sampled. Read-only.
#' * `tags`             :: `character` \cr
#'   Can be used to group and subset params.
#' * is_number          :: `logical(1)` \cr
#'   TRUE iff Param is dbl or int.
#' * is_categ           :: `logical(1)` \cr
#'   TRUE iff Param is fct or lgl.
#'
#' @section Public methods:
#' * `Param$new(id, special_vals = list(), default = NO_DEF, tags = character(0L))` \cr
#'   `character(1)`, `list`, `any`, `character` -> self \cr
#'   Constructor of abstract base class, only called by inheriting classes.
#'   See meaning of `id`, `special_vals`, `default`, `tags` in member section.
#' * `Param$Dbl$new(id, lower, upper, special_vals, default, tags)` \cr
#'   `character(1)`, `numeric(1)`, `numeric(1)`, `list`, `any`, `character` -> self \cr
#'    Constructor for double-scalar-params. Box-constraint bounds can be set, or be Inf.
#' * `Param$Int$new(id, lower, upper, special_vals, default, tags)` \cr
#'   `character(1)`, `numeric(1)`, `numeric(1)`, `list`, `any`, `character` -> self \cr
#'    Constructor for int-scalar-params. Box-constraint bounds can be set, or be Inf;
#'   `lower` is set to its integer ceiling and 'upper' to its integer floor value.
#' * `ParamFct$new(id, values, special_vals, default, tags)` \cr
#'   `character(1)`, `character`, `list`, `any`, `character` -> self \cr
#'    Constructor for categorical/factor-like params; slight misnomer as it accepts only strings,
#'    from its defined set of categorical values.
#' * `Param$Lgl$new(id, special_vals, default, tags)` \cr
#'   `character(1)`, `list`, `any`, `character` -> self \cr
#'    Constructor for logical-scalar-params.
#' * `ParamUty$new(id, default, tags, custom_check)` \cr
#'   `character(1)`, `any`, `character`, `function(x)` -> self \cr
#'   Untyped parameters, can be used to bypass any complicated feasibility checks, when
#'   a param is of truly complex type, as checks for this param are always feasible.
#'   OTOH we cannot perform meaningful operations like sampling or generating designs with this param.
#'   User can pass a `custom_check` to specify feasibility checks for custom types.
#' * `test(x)`, `check(x)`, `assert(x)` \cr
#'    Three checkmate-like check-functions. Take a value from the domain of the param, and check if it is feasible.
#'    A value is feasible if it is of the same `storage_type`, inside of the bounds or from `special_vals`.
#' * `qunif(x)` \cr
#'   `numeric(n)` -> `vector(n)` \cr
#'   Takes values from \[0,1\] and map them, regularly distributed, to the domain of the param.
#'   Think of: quantile function or the usecse to map a uniform-\[0,1\] random variable into a uniform sample from this param.
#' * `rep(n)` \cr
#'   `integer(1)` -> [ParamSet] \cr
#'   Repeats this param n-times (by cloning); each param is named "<id>_rep_<k>" and gets additional tag "<id>_rep".
#'
#' @section Further public methods for ParamDbl, ParamInt:
#' * `range`            :: `numeric(2)` \cr
#'   Lower and upper bound as 2-dim-vector.
#' * `span`            :: `numeric(1)` \cr
#'   Difference of `upper - lower`.
#'
#' @section S3 methods and type converters:
#' * `as.data.table()` \cr
#'   Converts param to datatable with 1 row. See [ParamSet].
#'
#' @name Param
#' @aliases ParamDbl ParamInt ParamFct ParamLgl ParamUty
#' @family Param
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
