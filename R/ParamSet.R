#' @title ParamSet
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of parameters.
#'
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
#' @section Further comments:
#' Note that you can construct an empty ParamSet by passing an empty list during construction.
#' Such a ParamSet has length 0, and getter will always return NULL.
#'
#' @return [\code{\link{ParamSet}}].
#' @family ParamSet
#' @export
ParamSet = R6Class("ParamSet",
  public = list(
    id = NULL,
    data = NULL,  # a list of ParamNodes
    trafo = NULL, # function to transform the value before evaluation
    deps = NULL,

   # FIXME: constructor which takes a dt?

    initialize = function(params = list(), id = "paramset", tags = NULL, trafo = NULL) {
      assert_list(params, types = "Parameter")
      if (length(params) > 0L) {
        self$data = rbindlist(map(params, "data"))
        # we set index not key, so we dont resort the table
        setindex(self$data, "id")
      } else {
        self$data = data.table()
      }
      assert_string(id)
      assert_names(id, type = "strict")
      self$id = id
      self$trafo = assert_function(trafo, args = c("x", "tags"), null.ok = TRUE)
      # create depnodes as graph with allocated nodes, but no current edges
      private$.dep_nodes = vector("list", length(params))
      names(private$.dep_nodes) = names(params)
      for (p in params) {
        private$.dep_nodes[[p$id]] = DependencyNode$new(p)
      }
    },

    # list --> list, named
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

    # FIXME: subset und fix trennen

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
      ParamSet$new(
        id = paste0(self$id,"_subset"),
        params = self$params[keep_ids],
        tags = self$tags,
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
        trafo = new_trafo
      )
    },

    # check function that checks whether a named list is a feasible point from the set
    check = function(xs) {
      assert(self$length > 0L) # we cannot really check feasible x for empty sets
      ids = self$ids
      assert_list(xs)
      assert_names(names(xs), permutation.of = ids)
      for (id in ids) {
        ch = self$get_param(id)$check(xs[[id]])
        if (test_string(ch)) # we failed a check, return string
          return(ch)
      }
      return(TRUE) # we passed all checks
    },

    test = function(xs) makeTestFunction(self$check)(xs),

    assert = function(xs) makeAssertionFunction(self$check)(xs),

    add_dependency = function(dep) {
      assert_r6(dep, "Dependency")
      # add dependency to member list
      self$deps = c(self$deps, list(dep))
      # connect subordinate to super param in depnode graph
      dnc = private$.dep_nodes[[dep$child$id]]
      dnp = private$.dep_nodes[[dep$parent$id]]
      dnc$parents = c(dnc$parents, list(dnp))
      dnp$children = c(dnp$children, list(dnc))
    },


    # printer, simply prints datatable contents, with the option to hide some cols
    print = function(..., hide.cols = c("storage_type", "tags")) {
      catf("ParamSet: %s", self$id)
      if (self$is_empty) {
        catf("Empty.")
      } else {
        d = self$data
        assert_subset(hide.cols, names(d))
        print(d[, setdiff(colnames(d), hide.cols), with = FALSE])
      }
    },

    # print = function(...) {
    #   cat("ParamSet:", self$id, "\n")
    #   cat("Parameters:", "\n")
    #   for (param in self$params) {
    #     param$print(...)
    #   }
    #   if (!is.null(self$tags)) {
    #     cat("Tags are set:", "\n")
    #     print(self$tags)
    #   }
    #   if (!is.null(self$trafo)) {
    #     cat("Trafo is set:", "\n")
    #     print(self$trafo)
    #   }
    # },


    # return a Parameter of the set, by id
    # FIXME: careful, this returns a reference! by chanhing that we might
    # hurt the integrity of the paramset. we might at least offer a "deep" copy as option and document this?
    get_param = function(id) {
      assert_choice(id, self$ids)
      r = self$data[id, on = "id"] # index single row by id
      new_param_from_dt(r)
    },

    # return a list of params, for given ids or all ids
    get_params = function(ids = NULL) {
      sids = self$ids
      if (is.null(ids))
        ids = sids
      assert_subset(ids, sids)
      map(ids, self$get_param)
    }
  ),

  active = list(
    length = function() nrow(self$data),
    is_empty = function() self$length == 0L,
    ids = function() self$data$id,
    pclasses = function() private$get_col_with_idnames("pclass"),
    storage_types = function() private$get_col_with_idnames("storage_type"),
    lowers = function() private$get_col_with_idnames("lower"),
    uppers = function() private$get_col_with_idnames("upper"),
    nlevels = function() {
      # this is a bit slow, we can write worse code on the data dt for speed
      nlevs = map_int(self$get_params(), function(p) p$nlevels)
      set_names(nlevs, self$ids)
    },
    values = function() private$get_col_with_idnames("values"),
    tags = function() private$get_col_with_idnames("tags"),
    ids_num = function() self$data[pclass %in% c("ParamDbl", "ParamInt"), id],
    ids_cat = function() self$data[pclass %in% c("ParamFct", "ParamLgl"), id]
  ),

  private = list(
    .dep_nodes = NULL,
    get_col_with_idnames = function(col) set_names(self$data[[col]], self$data[["id"]])
  )
)
