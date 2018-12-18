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
#'     \code{x} is a \code{data.table}, each row contains one parameter setting.
#'     \code{tags} is a named list that contains the tags for each Param in \code{x}.
#'     This function is called from \code{ParamSet$transform()}.
#'     It has to return a \code{data.table} object with the same number of rows as \code{x}, the number and names of the columns can be completely different.
#'     }
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
    data = NULL,  # a datatable which consists of rows of Param-data elements
    trafo = NULL, # function to transform the value before evaluation
    deps = NULL,

   # FIXME: constructor which takes a dt?

    # this does a deep copy of all passed param objects
    initialize = function(params = list(), id = "paramset", trafo = NULL) {
      assert_list(params, types = "Parameter")
      if (length(params) > 0L) {
        self$data = rbindlist(map(params, "data"))
        # we set index not key, so we dont resort the table
        setindex(self$data, "id")
      } else {
        self$data = data.table(id = character(0L), pclass = character(0L), storage_type = character(0L), lower = numeric(0L), upper = numeric(0L), values = list(), special_vals = list(), default = list(), tags = list())
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

    # add a param to the current self-set, deep copies it
    add_param = function(param) {
      assert_r6(param, "Parameter")
      self$data = rbind(self$data, param$data)
      return(self)
    },

    # takes data.table and calls self$trafo on this data.table. Returns data.table.
    transform = function(x) {
      assert_data_table(x)
      assert_set_equal(names(x), self$ids)
      if (is.null(self$trafo))
        return(x)
      xs = self$trafo(x = x, tags = self$member_tags)
      assert_data_table(xs)
      return(xs)
    },

    # fix (named list of parameter values to keep fixed)
    # creates a subset of self (cloned) with all params that are not mentioned in fix
    # adds ParamFix param for all dropped Params
    # out: ParamSet
    fix = function(fix) {
      assert_list(fix, names = "named")
      assert_subset(names(fix), self$ids)
      fix_ids = names(fix)
      keep_ids = setdiff(self$ids, fix_ids)
      new_paramset = self$subset(keep_ids) # creates clone
      for (fix_ids in fix_ids) {
        param_old = self$get_param(keep_id)
        param_fix = ParamFix$new(
          id = paste0(param_old$id),
          storage_type = param_old$storage_type,
          default = fix[[keep_id]],
          tags = param_old$tags
        )
        new_paramset$add_param(param_fix)
      }
      return(new_paramset)
    },

    # ids to keep in a cloned ParamSet
    subset = function(ids) {
      assert_subset(ids, self$ids)
      new_paramset = self$clone()
      new_paramset$id = paste0(new_paramset$id,"_subset")
      new_paramset$data = new_paramset$data[ids, on = "id"]
      return(new_paramset)
    },

    # returnes a cloned param_set of both
    combine = function(param_set) {
      if (self$length == 0) {
        return(param_set$clone())
      } else if (param_set$length == 0) {
        return(self$clone())
      }
      if (length(intersect(self$ids, param_set$ids)) > 0) {
        stop ("Combine failed, because new param_set has at least one Param with the same id as in this ParamSet.")
      }
      if (!is.null(param_set$trafo)) {
        stop ("The new param_set can not have a trafo.")
      }
      if (!is.null(param_set$deps)) {
        stop ("The new param_set can not have any dependency.")
      }
      result = ParamSet$new(
        id = paste0(self$id, "_", param_set$id),
        params = rbind(self$data, param_set$data),
        tags = union(self$tags, param_set$tags),
        trafo = self$trafo,
      )
    },

    # check function that checks whether a named list is a feasible point from the set
    check = function(xs) {
      assert_list(xs)
      if (length(xs) == 0) return(TRUE) # a empty list is always feasible
      assert_names(names(xs), subset.of = self$ids)
      for (id in names(xs)) {
        ch = self$get_param(id)$check(xs[[id]])
        if (test_string(ch)) # we failed a check, return string
          return(paste0(id,": ",ch))
      }
      return(TRUE) # we passed all checks
    },

    test = function(xs) {
      makeTest(res = self$check(xs), check.fun = self$check)
    },

    assert = function(xs) {
      makeAssertion(x = xs, res = self$check(xs))
    },

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
        catf("Parameters:")
        d = self$data
        assert_subset(hide.cols, names(d))
        print(d[, setdiff(colnames(d), hide.cols), with = FALSE])
      }
      if (!is.null(self$trafo)) {
        catf("Trafo is set:")
        print(self$trafo)
      }
    },

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
      set_names(map(ids, self$get_param), ids)
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
    ids_cat = function() self$data[pclass %in% c("ParamFct", "ParamLgl"), id],
    is_bounded = function() all(map_lgl(self$get_params(), function(p) p$is_bounded)),
    defaults = function() private$get_col_with_idnames("default")
  ),

  private = list(
    .dep_nodes = NULL,

    get_col_with_idnames = function(col) set_names(self$data[[col]], self$data[["id"]]),

    deep_clone = function(name, value) {
      if (name == "data") copy(value) else value
    }
  )
)
