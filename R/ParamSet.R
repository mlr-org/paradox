#' @title ParamSet
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of parameters.
#'
#' @section Member Variables:
#'
#' \describe{
#'   \item{params}{[\code{list}] \cr
#'   List of the Params}
#'   \item{trafo}{[\code{function(x, tags)}] \cr
#'     A function that returns a list of transformed x values.
#'     Has to work vectorized and also return untransformed x values.
#'     The function takes a list \code{x} of all parameter values.
#'     \code{tags} is a named list that contains the tags for each Param in \code{x}.}
#'   \item{restriction}{[\code{quote}] \cr
#'     A quoted expression (\code{quote()}) that is evaluated on all parameter values to check if they are feasible.
#'     It has to be evaluated to \code{TRUE} so that the parameter value is valid.
#'     The expression has to work on vectors of values.}
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{generate_lhs_design(n, lhs_function)}{[\code{function}] \cr
#'     Function to generate a LHS design.}
#'   \item{generate_grid_design(resolution, param_resolutions, n)}{[\code{function}] \cr
#'     \describe{
#'       \item{resolution}{[\code{integer(1)}] for each parameter universally}
#'       \item{param_resolutions}{[\code{integer}] for each parameter individually. Has to be a named vector.}
#'       \item{n}{[\code{integer(1)}] size of design. Will be tried to match by optimizing \eqn{r^k * (r-1)^(p-k) - n}. \code{r} = resolution, \code{p} = total number of parameters.}
#'     }
#'   }
#' }
#'
#' @section Active Bindings:
#'
#' \describe{
#'   \item{ids}{[\code{character}] \cr
#'     ids of the Parameters in this ParamSet.}
#'   \item{storage_types}{[\code{character}] \cr
#'     How is a Value of this Parameter stored as an R-object?}
#'   \item{values}{[\code{list}] \cr
#'     For any discrete Parameter return the values. Also works for Integers.}
#'   \item{lower}{[\code{numeric}] \cr
#'     For each numeric Parameter return the lower boundary. \code{NA} for other Parameters.}
#'   \item{upper}{[\code{numeric}] \cr
#'     Same as for \code{lower}}
#'   \item{param_classes}{[\code{character}] \cr
#'     The \code{R6} class name of each Parameter.}
#'   \item{range}{[\code{data.table}] \cr
#'     A \code{data.table} with the columns \code{id}, \code{lower}, \code{upper}.}
#'   \item{length}{[\code{integer(1)}] \cr
#'     The number of parameters.}
#'   \item{nlevels}{[\code{integer}] \cr
#'     For each discrete Parameter return the number of different values.}
#'   \item{member_tags}{[\code{list}] \cr
#'     The \code{tags} of each Parameter.}
#' }
#'
#' @return [\code{\link{ParamSet}}].
#' @family ParamSet
#' @export
ParamSet = R6Class( "ParamSet",
  public = list(

    # member variables
    params = NULL,  # a list of ParamNodes
    id = NULL,
    trafo = NULL, # function to transform the value before evaluation
    restriction = NULL, # quote that states if certain conditions have to be met
    check = NULL, # a checkmate check function to validate if a value is valid for this Param
    assert = NULL, # assert_ion generated from the above check
    test = NULL, # test generated from the above check

    # constructor
    initialize = function(params = list(), id = "paramset", tags = NULL, restriction = NULL, trafo = NULL) {
      # set member variables
      assert_list(params, types = "Parameter")
      names(params) = map_chr(params, "id") # ensure we have a named list, with par ids
      self$params = params
      self$id = assert_string(id)
      self$trafo = assert_function(trafo, args = c("x", "tags"), null.ok = TRUE)
      self$restriction = assert_class(restriction, "call", null.ok = TRUE)

      # check function that checks the whole param set by simply iterating
      self$check = function(x, na.ok = FALSE, null.ok = FALSE) {
        assert_set_equal(names(x), self$ids)
        if (is.data.table(x)) x = as.list(x)
        assert_list(x, names = "named")
        for (par_name in names(x)) {
          res = self$params[[par_name]]$check(x[[par_name]], na.ok = na.ok, null.ok = null.ok)
          if(!isTRUE(res)) return(res)
        }
        return(res)
      }
      self$test = makeTestFunction(self$check)
      self$assert = makeAssertionFunction(self$check)

    },

    # public methods

    denorm = function(x) {
      assert_list(x, names = 'strict')
      assert_set_equal(names(x), self$ids)
      xs = lapply(self$ids, function(id) self$params[[id]]$denorm(x = x[id]))
      names(xs) = NULL
      as.data.table(xs)
    },

    transform = function(x) {
      x = ensure_data_table(x)
      assert_set_equal(names(x), self$ids)
      if (is.null(self$trafo))
        return(x)
      # We require trafos to be vectorized! That's why we dont need the following
      #.mapply(function(x) {
      #  eval(self$trafo, envir = c(x, as.list(self$dictionary)))
      #}, x, list())
      xs = self$trafo(x = x, tags = self$member_tags)
      xs = ensure_data_table(xs)
      return(xs)
    },



    # in: * ids (character)
    #       ids of Parameter
    #     * fix (named list)
    #       names = ids of Parameter
    #       values = values of respective param
    # out: ParamSet
    subset = function(ids = NULL, fix = NULL) {
      if (is.null(ids)) {
        keep_ids = self$ids
      } else {
        assert_subset(ids, self$ids)
        keep_ids = ids
      }
      if (!is.null(fix)) {
        if (any(names(fix) %in% ids)) {
          stop("You cannot keep ids and fix them at the same time!")
        }
        assert_list(fix, names = "named")
        keep_ids = setdiff(keep_ids, names(fix))
      }
      # if we have fixed parameters we have to supply them to the trafo function in case there are needed there.
      new_trafo = self$trafo
      if (!is.null(fix) && !is.null(new_trafo)) {
        assert_list(fix, names = "named")
        old_trafo = force(self$trafo)
        new_trafo = function(x, tags) {
          x = cbind(x, as.data.table(fix))
          res = old_trafo(x, tags)
          res = x[, !names(fix), with = FALSE]
          return(res)
        }
      }
      # if we have fixed parameters we can substitute them in the restriction quote
      new_restriction = self$restriction
      if (!is.null(fix) && !is.null(new_restriction)) {
        new_restriction = substituteDirect(new_restriction, fix)
      }
      ParamSet$new(
        id = paste0(self$id,"_subset"),
        params = self$params[keep_ids],
        tags = self$tags,
        restriction = new_restriction,
        trafo = new_trafo)
    },

    combine = function(param_set) {
      if (self$length == 0) {
        return(param_set$clone())
      } else if (param_set$length == 0) {
        return(self$clone())
      }
      if (length(intersect(self$ids, param_set$ids)) > 0) {
        stop ("Combine failed, because new param_set has at least one Param with the same id as in this ParamSet.")
      }
      new_restriction = self$restriction %??% param_set$restriction
      if (!is.null(self$restriction) && !is.null(param_set$restriction)) {
        new_restriction = substitute((a) && (b), list(a = self$restriction %??% TRUE, b = param_set$restriction %??% TRUE))
      }
      new_trafo = self$trafo %??% param_set$trafo
      if (!is.null(self$trafo) && !is.null(param_set$trafo)) {
        new_trafo = function(x, tags) {
          x = self$trafo(x, tags)
          x = param_set$trafo(x, tags)
          return(x)
        }
      }
      ParamSet$new(
        id = paste0(self$id, "_", param_set$id),
        params = c(self$params, param_set$params),
        tags = union(self$tags, param_set$tags),
        restriction = new_restriction,
        trafo = new_trafo
      )
    },

    print = function(...) {
      cat("ParamSet:", self$id, "\n")
      cat("Parameters:", "\n")
      for (param in self$params) {
        param$print(...)
      }
      if (!is.null(self$tags)) {
        cat("Tags are set:", "\n")
        print(self$tags)
      }
      if (!is.null(self$restriction)) {
        cat("Restriction is set:", "\n")
        print(self$restriction)
      }
      if (!is.null(self$trafo)) {
        cat("Trafo is set:", "\n")
        print(self$trafo)
      }
    }
  ),

  active = list(
    ids = function() names(self$params),
    storage_types = function() map_chr(self$params, "storage_type"),
    values = function() lapply(self$params, function(param) param$values),
    lower = function() map_dbl(self$params, function(param) param$lower %??% NA_real_),
    upper = function() map_dbl(self$params, function(param) param$upper %??% NA_real_),
    param_classes = function() map_chr(self$params, function(param) class(param)[1]),
    range = function() data.table(id = self$ids, upper = self$upper, lower = self$lower),
    has_finite_bounds = function() all(map_lgl(self$params, function(param) param$has_finite_bounds)),
    length = function() length(self$params),
    nlevels = function() map_int(self$params, function(param) param$nlevels %??% NA_integer_),
    member_tags = function() lapply(self$params, function(param) param$tags)
  )
)
