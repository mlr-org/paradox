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
#'   \item{trafo}{[\code{function(x, dict, tags)}] \cr
#'     A function that returns a list of transformed x values.
#'     Has to work vectorized and also return untransformed x values.
#'     The function takes a list \code{x} of all parameter values, additionally the dictionary linked to the \code{ParamSet}.
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
    initialize = function(params = list(), id = "paramset", dictionary = NULL, tags = NULL, restriction = NULL, trafo = NULL) {
      # set member variables
      assert_list(params, types = "ParamBase")
      names(params) = map_chr(params, "id") # ensure we have a named list, with par ids
      self$params = params
      self$id = assert_string(id)
      self$trafo = assert_function(trafo, args = c("x", "dict", "tags"), null.ok = TRUE)
      self$restriction = assert_class(restriction, "call", null.ok = TRUE)
      self$dictionary = assert_list(dictionary, names = "strict", null.ok = TRUE)

      # check function that checks the whole param set by simply iterating
      self$check = function(x, na.ok = FALSE, null.ok = FALSE) {
        assert_set_equal(names(x), self$ids)
        if (is.data.table(x)) x = as.list(x)
        assert_list(x, names = "named")
        if (!is.null(self$restriction)) {
          x_n_dictionary = c(as.list(self$dictionary), x)
          if (!isTRUE(eval(self$restriction, envir = x_n_dictionary))) {
            return(sprintf("Value %s not allowed by restriction: %s", as_short_string(x), deparse(restriction)))
          }
        }
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
    sample = function(n = 1L) {
      assert_int(n, lower = 1L)
      sample_generator = function(n, ...) {
        xs = lapply(self$params, function(param) param$sample(n = n))
        names(xs) = NULL
        as.data.table(xs)
      }
      if (!is.null(self$restriction)) {
        sample_validator = function(x) vectorized_for_param_set_flat(x, self$test)
        oversample_forbidden2(n = n, param = param, sample_generator = sample_generator, sample_validator = sample_validator)
      } else {
        sample_generator(n)
      }
    },

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
      xs = self$trafo(x = x, dict = self$dictionary, tags = self$member_tags)
      xs = ensure_data_table(xs)
      return(xs)
    },

    generate_lhs_design = function(n, lhs_function = lhs::maximinLHS) {
      assert_int(n, lower = 1L)
      assert_function(lhs_function, args = c("n", "k"))
      lhs_des = lhs_function(n, k = self$length)
      # converts the LHS output to values of the parameters
      sample_converter = function(lhs_des) {
        vec_cols = lapply(seq_len(ncol(lhs_des)), function(z) lhs_des[,z])
        names(vec_cols) = self$ids
        self$denorm(vec_cols)
      }
      if (!is.null(self$restriction)) {
        # we work on the matrix that is the LHS output to be able to use augmentLHS to sample additional values.
        sample_generator = function(n, old_x = NULL) {
          if (is.null(old_x)) return(lhs_des)
          lhs_des = lhs::augmentLHS(lhs = old_x, m = n)
          tail(lhs_des, n)
        }
        # validates the LHS output, according to the param restrictions
        sample_validator = function(lhs_des) {
          vectorized_for_param_set_flat(sample_converter(lhs_des), self$test)
        }
        lhs_des = oversample_forbidden2(n = n, param = param, oversample_rate = 1, sample_generator = sample_generator, sample_validator = sample_validator)
      }
      sample_converter(lhs_des)
    },

    # resolution int(1) - resolution used for each parameter
    # param_resolutions int() - resolution given per parameter (named vector)
    # n int(1) - approx. maximum number of samples in grid
    generate_grid_design = function(resolution = NULL, param_resolutions = NULL, n = NULL) {
      if (sum(!is.null(resolution), !is.null(param_resolutions), !is.null(n)) != 1) {
        stop("You can only specify one of the arguments!")
      }

      seqGen = function(r) seq(0, 1, length.out = r)

      if (!is.null(resolution)) {
        # build for resolution
        assert_int(resolution, lower = 1L)
        grid_vec = replicate(self$length, seqGen(resolution), simplify = FALSE)
        names(grid_vec) = self$ids
        res = as.list(self$denorm(grid_vec))
      } else {
        # build for n: calculate param_resolutions
        if (!is.null(n)) {
          assert_int(n, lower = 1L)
          param_resolutions = opt_grid_res(n, self$nlevels)
        }
        # build for param_resolutions
        assert_integerish(param_resolutions, lower = 1L, any.missing = FALSE, names = "strict")
        assert_set_equal(names(param_resolutions), self$ids)
        grid_vec = lapply(param_resolutions, seqGen)
        res = lapply(names(grid_vec), function(z) self$params[[z]]$denorm_vector(x = grid_vec[[z]]))
        names(res) = names(grid_vec)
      }
      res = lapply(res, unique)
      res = do.call(CJ, as.list(res))
      if (!is.null(self$restriction)) {
        ind_valid = vectorized_for_param_set_flat(res, self$test)
        return(res[ind_valid, ])
      } else {
        return(res)
      }
    },

    # in: * ids (character)
    #       ids of ParamBase
    #     * fix (named list)
    #       names = ids of ParamBase
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
        new_trafo = function(x, dict, tags) {
          x = cbind(x, as.data.table(fix))
          res = old_trafo(x, dict, tags)
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
        dictionary = as.list(self$dictionary),
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
        new_trafo = function(x, dict, tags) {
          x = self$trafo(x, dict, tags)
          x = param_set$trafo(x, dict, tags)
          return(x)
        }
      }
      ParamSet$new(
        id = paste0(self$id, "_", param_set$id),
        params = c(self$params, param_set$params),
        dictionary = c(as.list(self$dictionary), as.list(param_set$dictionary)),
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
      if (!is.null(self$dictionary)) {
        cat("Dictonary is set:", "\n")
        print(self$dictionary)
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
    },
    value_to_string = function(x, ...) {
      paste0(names(self$params), sapply(self$params, function(param) {
        # Convert every parameter individually with its value_to_string function.
        param_id = param$id
        param$value_to_string(x$param_id, ...)
      }), sep = " = ", collapse=", ")
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
    member_tags = function() lapply(self$params, function(param) param$tags),
    dictionary = function(x) {
      if (missing(x)) {
        return(private$priv_dictionary)
      } else if (!is.null(x)) {
        x = as.environment(x)
        private$priv_dictionary = x
      }
    }
  ),
  private = list(
    priv_dictionary = NULL
  )
)
