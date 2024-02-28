library("R6")
library("paradox")
library("checkmate")


Strategy = R6Class("Strategy",
  public = list(
     initialize = function(param_set) {
       private$.param_set = param_set
     },
     run = function(x) stop("abstract")
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

StrategyVector = R6Class("StrategyVector", inherit = Strategy,
  public = list(
    components = NULL,
    initialize = function(components) {
      components = lapply(components, function(x) x$clone(deep = TRUE))
      self$components = components
      params = lapply(seq_along(components), function(ci) {
        comp = components[[ci]]
        p = comp$param_set
        p$set_id = paste0("element", ci)
        p
      })
      param_set = ParamSetCollection$new(params)
      super$initialize(param_set)
    },
    run = function(x) {
      lapply(self$components, function(comp) comp$run(x))
    },
    run_vector = function(x) {
      assert_list(x, len = length(self$components))
      Map(function(comp, xcomp) comp$run(xcomp), self$components, x)
    }
  )
)

c.Strategy = function(...) {
  components = list(...)
  assert_list(components, types = "Strategy")
  components = unlist(lapply(unname(components), function(cmp) {
    if (inherits(cmp, "StrategyVector")) cmp$components else list(cmp)
  }), recursive = FALSE)
  StrategyVector$new(components)
}


StrategyAdd = R6Class("StrategyAdd", inherit = Strategy,
  public = list(
    initialize = function() {
      super$initialize(ps(summand = p_dbl()))
    },
    run = function(x) {
      self$param_set$get_values()$summand + x
    }
  )
)

StrategyMultiply = R6Class("StrategyMultiply", inherit = Strategy,
  public = list(
    initialize = function() {
      super$initialize(ps(factor = p_dbl()))
    },
    run = function(x) {
      self$param_set$get_values()$factor * x
    }
  )
)

StrategyMultiplyAdd = R6Class("StrategyMultiplyAdd", inherit = Strategy,
  public = list(
    initialize = function() {
      super$initialize(ps(factor = p_dbl(), summand = p_dbl()))
    },
    run = function(x) {
      self$param_set$get_values()$factor * x + self$param_set$get_values()$summand
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
svec$run_vector(list(100, 200, 300))  # list(110, 400, 1199)

c(svec, sa, sm)$run_vector(list(100, 200, 300, 400, 500))  # list(110, 400, 1199, 410, 1000)



svec_long = StrategyVector$new(c(
  lapply(1:2000, function(x) {
    sa = StrategyAdd$new()
    sa$param_set$values$summand = x
    sa
  }),
  list(sm, sam)
))

svec_long$run(100)
  # as.list(c(101:2100, 200, 399))
svec_long$run_vector(as.list(1:2002))
  # as.list(c((1:2000) * 2, 4002, 8007))


