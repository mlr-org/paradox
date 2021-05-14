#' @title Param Class
#'
#' @description
#' This is the abstract base class for parameter objects like [ParamDbl] and
#' [ParamFct].
#'
#' @template param_id
#' @template param_special_vals
#' @template param_default
#' @template param_tags
#'
#' @section S3 methods:
#' * `as.data.table()`\cr
#'   [Param] -> [data.table::data.table()]\cr
#'   Converts param to [data.table::data.table()] with 1 row. See [ParamSet].
#'
#' @family Params
#' @export
Param = R6Class("Param",
  public = list(
    #' @field id (`character(1)`)\cr
    #' Identifier of the object.
    id = NULL,

    #' @field special_vals (`list()`)\cr
    #' Arbitrary special values this parameter is allowed to take.
    special_vals = NULL,

    #' @field default (`any`)\cr
    #' Default value.
    default = NULL,

    #' @field tags (`character()`)\cr
    #' Arbitrary tags to group and subset parameters.
    tags = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' Note that this object is typically constructed via derived classes,
    #' e.g., [ParamDbl].
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

    #' @description
    #' \pkg{checkmate}-like check-function. Take a value from the domain of the
    #' parameter, and check if it is feasible. A value is feasible if it is of
    #' the same `storage_type`, inside of the bounds or element of
    #' `special_vals`.
    #'
    #' @param x (`any`).
    #' @return If successful `TRUE`, if not a string with the error message.
    check = function(x) {
      # either we are directly feasible, or in special vals, if both are untrue return errmsg from 1st check
      if (inherits(x, "TuneToken")) {
        return(tryCatch({
          tunetoken_to_ps(x, self, self$id)
          TRUE
        }, error = function(e) paste("tune token invalid:", conditionMessage(e))))
      }
      if (inherits(x, "ContextPV")) return(TRUE)
      ch = private$.check(x)
      ifelse(isTRUE(ch) || has_element(self$special_vals, x), TRUE, ch)
    },

    #' @description
    #' \pkg{checkmate}-like assert-function. Take a value from the domain of
    #' the parameter, and assert if it is feasible. A value is feasible if it
    #' is of the same `storage_type`, inside of the bounds or element of
    #' `special_vals`.
    #'
    #' @param x (`any`).
    #' @return If successful `x` invisibly, if not an exception is raised.
    assert = function(x) makeAssertionFunction(self$check)(x),

    #' @description
    #' \pkg{checkmate}-like test-function. Take a value from the domain of the
    #' parameter, and test if it is feasible. A value is feasible if it is of
    #' the same `storage_type`, inside of the bounds or element of
    #' `special_vals`.
    #'
    #' @param x (`any`).
    #' @return If successful `TRUE`, if not `FALSE`.
    test = function(x) makeTestFunction(self$check)(x),

    #' @description
    #' Repeats this parameter n-times (by cloning).
    #' Each parameter is named "\[id\]_rep_\[k\]" and gets the additional tag "\[id\]_rep".
    #'
    #' @param n (`integer(1)`).
    #' @return [ParamSet].
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

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    #' @description
    #' Printer.
    #'
    #' @param ... (ignored).
    #' @param hide_cols (`character()`)\cr
    #'   Which fields should not be printed? Default is `"nlevels"`,
    #'   `"is_bounded"`, `"special_vals"`, `"tags"`, and `"storage_type"`.
    print = function(..., hide_cols = c("nlevels", "is_bounded", "special_vals", "tags", "storage_type")) {
      # this is bit bullshitty, but works by delegating to the printer of the PS
      d = as.data.table(ParamSet$new(list(self)))
      assert_subset(hide_cols, names(d))
      print(d[, setdiff(colnames(d), hide_cols), with = FALSE])
    },

    #' @description
    #' Takes values from \[0,1\] and maps them, regularly distributed, to the
    #' domain of the parameter. Think of: quantile function or the use case to
    #' map a uniform-\[0,1\] random variable into a uniform sample from this
    #' param.
    #'
    #' @param x (`numeric(1)`).
    #' @return Value of the domain of the parameter.
    qunif = function(x) {
      assert_numeric(x, lower = 0, upper = 1)
      assert_true(self$is_bounded)
      private$.qunif(x)
    },

    #' @description
    #' Converts a value to the closest valid param. Only for values that
    #' pass `$check()` and mostly used internally.
    #' @param x (`any`).
    #' @return `x` converted to a valid type for the `Param`.
    convert = function(x) {
      x
    }
  ),

  active = list(
    #' @field class (`character(1)`)\cr
    #' R6 class name. Read-only.
    class = function() class(self)[[1L]],

    #' @field is_number (`logical(1)`)\cr
    #' `TRUE` if the parameter is of type `"dbl"` or `"int"`.
    is_number = function() self$class %in% c("ParamDbl", "ParamInt"),

    #' @field is_categ (`logical(1)`)\cr
    #' `TRUE` if the parameter is of type `"fct"` or `"lgl"`.
    is_categ = function() self$class %in% c("ParamFct", "ParamLgl"),

    #' @field has_default (`logical(1)`)\cr
    #' Is there a default value?
    has_default = function() !is_nodefault(self$default)
  ),

  private = list(
    .check = function(x) stop("abstract"),
    .qunif = function(x) stop("abstract") # should be implemented by subclasses, argcheck happens in Param$qunif
  )
)

#' @export
as.data.table.Param = function(x, ...) { # nolint
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
