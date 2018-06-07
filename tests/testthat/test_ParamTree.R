context("ParamTree")
test_that("test if ParamFac parse from flat", {
  ps = ParamTreeFac(id = "test",
      ParamCategorical$new(id = "model", values = c("SVM", "RF")),
      makeCondTreeNode(ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "model", fun = quote(model == "SVM"))),
      makeCondTreeNode(ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), depend = list(id = "model", fun = quote(model == "SVM"))),
      makeCondTreeNode(ParamReal$new(id = "gamma", lower = 0, upper = 100), depend = list(id = "kernel", fun = quote(kernel == "rbf"))),
      makeCondTreeNode(ParamInt$new(id = "n", lower = 1L, upper = 10L), depend = list(id = "kernel", fun = quote(kernel == "poly")))
      )
  ps$asample()
  ps$toStringVal()
  ps$getList()
  expect_true(TRUE)
})

test_that("test if two ParamTree works", {
  pt = ParamSetTreeX$new("pt1",
      ParamCategorical$new(id = "model", values = c("SVM", "RF")),
      makeCondTreeNode(ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "model", fun = quote(model == "SVM"))),
     makeCondTreeNode(ParamInt$new(id = "n_tree", lower = 1L, upper = 10L), depend = list(id = "model", fun = quote(model == "RF")))
      )
  pt$asample()
  pt$toStringVal()
  pt2 = ParamSetTreeX$new("pt2",
      ParamCategorical$new(id = "activation", values = c("sigmoid", "tanh")),
      ParamReal$new(id = "regu", lower = 0, upper = 100),
      ParamInt$new(id = "n", lower = 1L, upper = 100L)
      )
  pt2$asample()
  pt2$toStringVal()
  pt$setChild(pt2)
  pt$asample()
  pt$toStringVal()
  expect_true(TRUE)
})

test_that("ParamSetTree constructor works", {
   pst = ParamSetTreeX$new("test",
       ParamCategorical$new(id = "model", values = c("SVM", "RF")),
       addDep(ParamReal$new(id = "C", lower = 0, upper = 100), 
         did = "model", expr = quote(model == "SVM")), # did here means dependant id
       addDep(ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), 
         did = "model", expr = quote(model == "SVM")),
       addDep(ParamReal$new(id = "gamma", lower = 0, upper = 101), 
         did = "kernel", expr = quote(kernel == "rbf")),
       addDep(ParamInt$new(id = "n", lower = 1L, upper = 10L), 
         did = "kernel", expr = quote(kernel == "poly")),
       addDep(ParamInt$new(id = "n_tree", lower = 1L, upper = 10L), 
         did = "model", expr = quote(model == "RF"))
       )
   pst$sample(1L)
   pst$sampleList()
   #pst$getRecursiveList()
   pst$sampleList(recursive = TRUE)
   pst$toStringVal()
   pst$sample(10L)
   pst$rt.hinge$sample(3)
   expect_true(TRUE)
})

test_that("recursive para works", {
  ps = recursiveParaFac(nr = 2,
      ParamInt$new(id = "layer_dense.units", lower = 2L, upper = 1000L),
      ParamReal$new(id = "kernel_regularizer", lower = 0, upper = 3.0), 
      ParamReal$new(id = "bias_regularizer", lower = 0, upper = 3.0), 
      ParamCategorical$new(id = "reg_type", values = c("regularizer_l1", "regularizer_l2")),
      ParamCategorical$new(id = "activation_fun", values = c("sigmoid", "tanh", "linear")))
  ps$sample(3L)
  ps$sample(1L)
  ps$sampleList()
  expect_true(TRUE)
})

 test_that("test conditional params works for recursive ParamTree", {
   # this example does not make sense, just to prove it works
   ps = recursiveParaFac(nr = 2,
       ParamCategorical$new(id = "model", values = c("SVM", "RF")),
       makeCondTreeNode(ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "model", fun = quote(model == "SVM"))),
      makeCondTreeNode(ParamInt$new(id = "n_tree", lower = 1L, upper = 10L), depend = list(id = "model", fun = quote(model == "RF")))
       )
   ps$asample()
   ps$toStringVal()
   expect_true(TRUE)
 })


test_that("test conditional ParamTree with rlR", {
pst = ParamSetTreeX$new("rlR", context = list(a = 3),
    ParamCategorical$new(id = "agent.name", values = c("AgentDQN", "AgentFDQN", "AgentDDQN", "AgentPG", "AgentActorCritic")),
    ParamReal$new(id = "agent.gamma", lower = 0, upper = 1),
    ParamCategorical$new(id = "replay.memname", values = c("Uniform", "Latest")),
    ParamCategorical$new(id = "policy.name", values = c("PolicyEpsilonGreedy")),
    ParamInt$new(id = "replay.epochs", lower = 1L, upper = 50L),
    addDep(ParamInt$new(id = "replay.batchsize", lower = 0, upper = 100), 
      did = "agent.name", expr = quote(agent.name != "AgentPG")), 
    addDep(ParamReal$new(id = "policy.epsilon", lower = 0, upper = 1), 
      did = "policy.name", expr = quote(policy.name == "PolicyEpsilonGreedy")), # did here means dependant id
    addDep(ParamReal$new(id = "policy.minEpsilon", lower = 0, upper = 1), 
      did = "policy.epsilon", expr = quote(TRUE), sample.fun = quote(ParamReal$new(id = "policy.minEpsilon", lower = 0, upper = policy.epsilon)$sampleVector())), 
    addDep(ParamReal$new(id = "policy.decay", lower = 0, upper = 1),
      did = "policy.minEpsilon", expr = quote(TRUE)) # did here means dependant id
    )
  pst$sample()
  pst$sampleList()
  pst$toStringVal()
  expect_true(TRUE)
 })



test_that("user API for NN works", {
  expect_error({
  ps = ParamSetTreeRe$new("nn", nr = 1,
      ParamInt$new(id = "layer_dense.units", lower = 2L, upper = 1000L),
      ParamReal$new(id = "kernel_regularizer", lower = 0, upper = 3.0), 
      ParamReal$new(id = "bias_regularizer", lower = 0, upper = 3.0), 
      ParamCategorical$new(id = "reg_type", values = c("regularizer_l1", "regularizer_l2")),
      ParamCategorical$new(id = "activation_fun", values = c("sigmoid", "tanh", "linear")))

  })
  ps = ParamSetTreeRe$new("nn", nr = 2,
      ParamInt$new(id = "layer_dense.units", lower = 2L, upper = 1000L),
      ParamReal$new(id = "kernel_regularizer", lower = 0, upper = 3.0), 
      ParamReal$new(id = "bias_regularizer", lower = 0, upper = 3.0), 
      ParamCategorical$new(id = "reg_type", values = c("regularizer_l1", "regularizer_l2")),
      ParamCategorical$new(id = "activation_fun", values = c("sigmoid", "tanh", "linear")))
  ps$sample(3L)
  ps$sample(1L)
  ps$sampleList(recursive = TRUE)
  ps$sampleList(recursive = FALSE)
  expect_true(TRUE)
 })

test_that("ParamTree works with child", {
   pst = ParamSetTreeX$new("pre",     
     ParamCategorical$new(id = "preprocessing", values = c("PCA", "FeatureFiltering")),
      addDep(ParamInt$new(id = "pca.k", lower = 1, upper = 5), 
        did = "preprocessing", expr = quote(preprocessing == "PCA")),
       addDep(ParamInt$new(id = "filter.n", lower = 1, upper = 10), 
         did = "preprocessing", expr = quote(preprocessing == "FeatureFiltering")))
   pst1 = ParamSetTreeX$new("ml",
       ParamCategorical$new(id = "model", values = c("SVM", "RF")),
       addDep(ParamReal$new(id = "C", lower = 0, upper = 100), 
         did = "model", expr = quote(model == "SVM")), # did here means dependant id
       addDep(ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), 
         did = "model", expr = quote(model == "SVM")),
       addDep(ParamReal$new(id = "gamma", lower = 0, upper = 101), 
         did = "kernel", expr = quote(kernel == "rbf")),
       addDep(ParamInt$new(id = "poly.n", lower = 1L, upper = 10L), 
         did = "kernel", expr = quote(kernel == "poly")),
       addDep(ParamInt$new(id = "n_tree", lower = 1L, upper = 10L), 
         did = "model", expr = quote(model == "RF"))
       )
   pst$setChild(pst1)
   pst$sample(4L)
   pst$sampleList()
   pst$toStringVal()  # always keep the last sampled value

})

test_that("keras helper works", {
  ps = ParamSetTreeRe$new("nn", nr = 2,
      ParamInt$new(id = "layer_dense.units", lower = 2L, upper = 1000L),
      ParamReal$new(id = "kernel_regularizer", lower = 0, upper = 3.0), 
      ParamReal$new(id = "bias_regularizer", lower = 0, upper = 3.0), 
      ParamCategorical$new(id = "reg_type", values = c("regularizer_l1", "regularizer_l2")),
      ParamCategorical$new(id = "activation_fun", values = c("sigmoid", "tanh", "linear")))
  ps$sample(3L)
  ps$sample()
  list.par.val = ps$sampleList()
  tex = keras_helper(input.shape = 256, output.shape = 10L, 
  output.act = "softmax", loss = "mse", 
  lr = 0.0025, list.par.val = list.par.val)
  #library(keras)
  #eval(parse(text = tex))
  expect_true(TRUE)
})

# Fixme: make this works
# mtry [p/10, p/1.5]
# pst = ParamSetTree$new("tree", context = list(n = 1000, p = 10),
#   addDep(ParamReal$new(id = "mtry", lower = 0.1, upper = 1.0 / 1.5), 
#       did = "",
#       sample.fun = quote(ParamReal$new(id = "mtry", lower = 0, upper = context$p))),
#     ParamInt$new(id = "num.trees", lower = 100, upper = 5000),
#     ParamReal$new(id = "fw.perc", lower = 0.001, upper = 0.8),
#     ParamReal$new(id = "sample.fraction", lower = 0.1, upper = 1, default = 0.5),
#     ParamInt$new(id = "min.node.size", lower = 1L, upper = 50, default = 5L)
# )
