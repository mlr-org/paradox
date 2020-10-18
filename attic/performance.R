
devtools::load_all()
library("mlr3")
devtools::load_all("../mlr3pipelines")


microbenchmark::microbenchmark(as_learner(ppl("greplicate", po("subsample") %>>% lrn("classif.rpart"), 10) %>>% po("classifavg")), times = 10)
# Original:
##      min     lq     mean   median       uq      max neval
## 8.053063 8.8598 9.449606 9.150274 9.858639 11.94355    10

# Caching self$params access:
##      min       lq     mean   median     uq      max neval
## 1.401022 1.545326 1.738911 1.628105 1.9469 2.173466    10
# (Median improved 5.6x)

# Avoiding self$ids() call:
##      min       lq     mean   median       uq      max neval
## 1.013816 1.034226 1.119859 1.076886 1.243257 1.276072    10
# (Median further improved 1.5x)

profvis::profvis(as_learner(ppl("greplicate", po("subsample") %>>% lrn("classif.rpart"), 10) %>>% po("classifavg")))
# Original:
# 7690 ms
# Caching self$params access:
# 1490 ms
# Avoiding self$ids():
# 1020 ms
# Most of the runtime is spent in construction and `%>>%` now.

profvis::profvis(as_learner(ppl("greplicate", po("subsample") %>>% lrn("classif.rpart"), 30) %>>% po("classifavg")), interval = 0.005)

