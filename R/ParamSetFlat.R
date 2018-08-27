#' @title ParamSetFlat
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of parameters in a flat form.
#' 
#' @section Member Variables:
#'   \emph{none}
#' 
#' Inherited from \code{ParamSet}:
#' @inheritSection ParamSet Member Variables
#'
#' @section Methods:
#' 
#' \describe{
#'   \item{generateLHSDesign(n, lhs.function)}{[\code{function}] \cr
#'     Function to generate a LHS design.}
#'   \item{generateGridDesign(resolution, param.resolutions, n)}{[\code{function}] \cr
#'     \describe{
#'       \item{resolution}{[\code{integer(1)}] for each parameter universally}
#'       \item{param.resolutions}{[\code{integer}] for each parameter individually. Has to be a named vector.}
#'       \item{n}{[\code{integer(1)}] size of design. Will be tried to match by optimizing \eqn{r^k * (r-1)^(p-k) - n}. \code{r} = resolution, \code{p} = total number of parameters.}
#'     }
#'   }  
#' }
#' 
#' Inherited from \code{ParamSet}:
#' @inheritSection ParamSet Methods
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
#'   \item{param.classes}{[\code{character}] \cr
#'     The \code{R6} class name of each Parameter.}
#'   \item{range}{[\code{data.table}] \cr
#'     A \code{data.table} with the columns \code{id}, \code{lower}, \code{upper}.}
#'   \item{length}{[\code{integer(1)}] \cr
#'     The number of parameters.}
#'   \item{nlevels}{[\code{integer}] \cr
#'     For each discrete Parameter return the number of different values.}
#'   \item{member.tags}{[\code{list}] \cr
#'     The \code{tags} of each Parameter.}
#' }
#' 
#' Inherited from \code{ParamSet}:
#' @inheritSection ParamSet Active Bindings
#' 
#' @return [\code{\link{ParamSet}}].
#' @family ParamSet
#' @export
ParamSetFlat = R6Class(
  "ParamSetFlat",
  inherit = ParamSet,
  public = list(
   
    # member variables
    
    # constructor
    initialize = function(id = "parset", handle = NULL, params = list(), dictionary = NULL, tags = NULL, restriction = NULL, trafo = NULL) {
      # check function that checks the whole param set by simply iterating
      check = function(x, na.ok = FALSE, null.ok = FALSE) {
        assert_setEqual(names(x), self$ids)
        if (is.data.table(x)) x = as.list(x)
        res = checkList(x, names = "named")
        if (!is.null(self$restriction)) {
          x.n.dictionary = c(as.list(self$dictionary), x)
          if (!isTRUE(eval(self$restriction, envir = x.n.dictionary))) {
            return(sprintf("Value %s not allowed by restriction: %s", convertToShortString(x), deparse(restriction)))
          }
        }
        for (par.name in names(x)) {
          res = self$params[[par.name]]$check(x[[par.name]], na.ok = na.ok, null.ok = null.ok)
          if(!isTRUE(res)) return(res)
        }
        return(res)
      }

      # make params a named list according to the ids
      names(params) = extractSubList(params, "id")

      # A Flat ParamSet can only contain ParamSimple Objects?
      assert_list(params, types = "ParamSimple") # FIXME: Maybe too restricitve?
      
      # construct super class
      super$initialize(id, storage_type = "list", check = check, params = params, dictionary = dictionary, tags = tags, restriction = restriction, trafo = trafo)
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
        sample_validator = function(x) vectorizedForParamSetFlat(x, self$test)
        oversampleForbidden2(n = n, param = param, sample_generator = sample_generator, sample_validator = sample_validator)
      } else {
        sample_generator(n)
      }
    },

    denorm = function(x) {
      assert_list(x, names = 'strict')
      assert_setEqual(names(x), self$ids)
      xs = lapply(self$ids, function(id) self$params[[id]]$denorm(x = x[id]))
      names(xs) = NULL
      as.data.table(xs)
    },

    transform = function(x) {
      x = ensureDataTable(x)
      assert_setEqual(names(x), self$ids)
      if (is.null(self$trafo)) 
        return(x)
      # We require trafos to be vectorized! That's why we dont need the following
      #.mapply(function(x) {
      #  eval(self$trafo, envir = c(x, as.list(self$dictionary)))
      #}, x, list())
      xs = self$trafo(x = x, dict = self$dictionary, tags = self$member.tags)
      xs = ensureDataTable(xs)
      return(xs)
    },

    generateLHSDesign = function(n, lhs.function = lhs::maximinLHS) {
      assert_int(n, lower = 1L)
      assert_function(lhs.function, args = c("n", "k"))
      lhs.des = lhs.function(n, k = self$length)
      # converts the LHS output to values of the parameters
      sample.converter = function(lhs.des) {
        vec.cols = lapply(seq_len(ncol(lhs.des)), function(z) lhs.des[,z])
        names(vec.cols) = self$ids
        self$denorm(vec.cols)
      }
      if (!is.null(self$restriction)) {
        # we work on the matrix that is the LHS output to be able to use augmentLHS to sample additional values.
        sample_generator = function(n, old_x = NULL) {
          if (is.null(old_x)) return(lhs.des)
          lhs.des = lhs::augmentLHS(lhs = old_x, m = n)
          tail(lhs.des, n)
        }
        # validates the LHS output, according to the param restrictions
        sample_validator = function(lhs.des) {
          vectorizedForParamSetFlat(sample.converter(lhs.des), self$test)
        }
        lhs.des = oversampleForbidden2(n = n, param = param, oversample_rate = 1, sample_generator = sample_generator, sample_validator = sample_validator)
      }
      sample.converter(lhs.des)
    },

    # resolution int(1) - resolution used for each parameter
    # param.resolutions int() - resolution given per parameter (named vector)
    # n int(1) - approx. maximum number of samples in grid
    generateGridDesign = function(resolution = NULL, param.resolutions = NULL, n = NULL) {
      if (sum(!is.null(resolution), !is.null(param.resolutions), !is.null(n)) != 1) {
        stop("You can only specify one of the arguments!")
      }

      seqGen = function(r) seq(0, 1, length.out = r)

      if (!is.null(resolution)) {
        # build for resolution
        assert_int(resolution, lower = 1L)
        grid.vec = replicate(self$length, seqGen(resolution), simplify = FALSE)
        names(grid.vec) = self$ids
        res = as.list(self$denorm(grid.vec))
      } else {
        # build for n: calculate param.resolutions
        if (!is.null(n)) {
          assert_int(n, lower = 1L)
          param.resolutions = opt_grid_res(n, self$nlevels)
        }
        # build for param.resolutions
        assert_integerish(param.resolutions, lower = 1L, any.missing = FALSE, names = "strict")
        assert_setEqual(names(param.resolutions), self$ids)
        grid.vec = lapply(param.resolutions, seqGen)
        res = lapply(names(grid.vec), function(z) self$params[[z]]$denormVector(x = grid.vec[[z]]))
        names(res) = names(grid.vec)
      } 
      res = lapply(res, unique)
      res = do.call(CJ, as.list(res))
      if (!is.null(self$restriction)) {
        ind.valid = vectorizedForParamSetFlat(res, self$test)
        return(res[ind.valid, ])
      } else {
        return(res)
      }
    }
  ),

  active = list(
    ids = function() names(self$params),
    storage_types = function() vcapply(self$params, function(param) param$storage_type),
    values = function() lapply(self$params, function(param) param$values),
    lower = function() vnapply(self$params, function(param) param$lower %??% NA_real_),
    upper = function() vnapply(self$params, function(param) param$upper %??% NA_real_),
    param.classes = function() vcapply(self$params, function(param) class(param)[1]),
    range = function() data.table(id = self$ids, upper = self$upper, lower = self$lower),
    has_finite_bounds = function() all(vlapply(self$params, function(param) param$has_finite_bounds)),
    length = function() length(self$params),
    nlevels = function() viapply(self$params, function(param) param$nlevels %??% NA_integer_),
    member.tags = function() lapply(self$params, function(param) param$tags)
  )
)
