library("R6")
library("paradox")
library("checkmate")

Strategy = R6Class("Strategy",
  public = list(
    is_scalar = NULL,
    multiplicity = NULL,
    initialize = function(param_set, is_scalar = TRUE, multiplicity = FALSE) {
      self$is_scalar = assert_flag(is_scalar)
      self$multiplicity = assert_count(multiplicity, positive = TRUE)
      assert_true(!is_scalar || multiplicity == 1)
      private$param_set = param_set
    },
    run = function(x) stop("abstract"),
    run_vector = function(x) {
      if (self$is_scalar) {
        stop("Scalar strategy does not vectorize.")
      } else {
        stop("abstract")
      }
    }
  ),
  active = list(
    param_set = function(v) {
      if (!missing(v) && !identical(v, private$.param_set)) stop("param_set is read-only")
      private$.param_set
    }
  ),
  private = list(
    .param_set = NULL
  )
)


StrategyHybridVector = R6Class("StrategyHybridVector", inherits = Strategy,
  public = list(
    initialize = function(param_set, multi_init = NULL) {
      assert_data_table(multi_init, min.rows = 1, null.ok = TRUE)
      if (!is.null(multi_init)) {
        param_set$vectorize = TRUE
        param_set$multiplicity = nrow(multi_init)
        param_set$values = multi_init
        is_scalar = FALSE
        multiplicity = param_set$multiplicity
      } else {
        is_scalar = TRUE
        multiplicity = 1
      }
      super$initialize(param_set, is_scalar = is_scalar, multiplicity = multiplicity)
    }
  )
)

######## Making use of possible multiplicities
StrategyVector = R6Class("Strategy", inherits = Strategy,
  public = list(
    components = NULL,
    chunksizes = NULL,
    initialize = function(components) {
      assert_list(components, types = "Strategy")
      self$components = unname(components)
      self$chunksizes = sapply(self$components, `[[`, "multiplicity")
      super$initialize(param_set = NULL, is_scalar = FALSE, multiplicity = sum(self$chunksizes)
    },
    run = function(x) {
      result_listlist = lapply(self$components, function(comp) {
        if (comp$is_scalar) list(comp$run(x)) else comp$run(x)
      })
      unlist(result_listlist, recursive = FALSE)
    },
    run_vector = function(x) {
      assert_list(x, len = self$multiplicity)
      x = split(x, unname(rep(seq_along(x), self$chunksizes)))
      result_listlist = Map(self$components, x, f = function(comp, x) {
        if (comp$is_scalar) list(comp$run(x)) else comp$run(x)
      })
      unlist(result_listlist, recursive = FALSE)
    }
  )
)

######## as before
c.Strategy = function(...) {
  components = list(...)
  assert_list(components, types = "Strategy")
  components = unlist(lapply(unname(components), function(cmp) {
    if (inherits(cmp, "StrategyVector")) cmp$components else list(cmp)
  }), recursive = FALSE)
  StrategyVector$new(components)
}

StrategyAdd = R6Class("StrategyAdd", inherit = StrategyHybridVector,
  public = list(
    initialize = function(multi_init = NULL) {
      super$initizlize(ps(summand = p_dbl()), multi_init = multi_init)
    },
    run = function(x) {
      result = self$get_values()$summand + x
      if (!self$is_scalar) result = as.list(result)
      result
    },
    run_vector = function(x) {
      if (self$is_scalar) stop("Scalar strategy does not vectorize.")
      self$run(x)
    }
  )
)

######## quality of life
c.StrategyAdd = function(...) {
  components = list(...)
  if (!test_list(components, types = "StrategyAdd")) NextMethod()
  StrategyAdd$new(multi_init = data.table::rbindlist(lapply(components, function(x) x$param_set$get_values(as_dt = TRUE))))
}

######## as before
StrategyMultiply = R6Class("StrategyMultiply", inherit = Strategy,
  public = list(
    initialize = function() {
      super$initialize(ps(factor = p_dbl()))
    },
    run = function(x) {
      self$get_values()$factor * x
    }
  )
)

######## as before
StrategyMultiplyAdd = R6Class("StrategyMultiplyAdd", inherit = Strategy,
  public = list(
    initialize = function() {
      super$initialize(ps(factor = p_dbl(), summand = p_dbl()))
    },
    run = function(x) {
      self$get_values()$factor * x + self$get_values()$summand
    }
  )
)


sa = StrategyAdd$new()
sa$param_set$values$summand = 10

sm = StrategyMultiply$new()
sm$param_set$values$factor = 2

sam = StrategyMultiplyAdd$new()
sam$param_set$values$summand = -1
sam$param_set$values$factor = 4

sa$run(100)  # 110
sm$run(100)  # 200
sam$run(100) # 399

svec = StrategyVector$new(list(sa, sm, sam))
svec$run(100)  # list(110, 200, 399)
svec$run_vector(c(100, 200, 300))  # list(110, 400, 1199)

c(svec, sa, sm)$run_vector(c(100, 200, 300, 400, 500))  # list(110, 400, 1199, 410, 1000)



svec_long = StrategyVector$new(c(
  StrategyAdd$new(multi_init = data.table(summand = 1:2000)),
  list(sm, sam)
))

svec_long$run(100)
  # as.list(c(101:2100, 200, 399))
svec_long$run_vector(1:2002)
  # as.list(c((1:2000) * 2, 4002, 8007))


