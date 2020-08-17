---
title: "ParamSet Tune Short Forms"
author: "Martin Binder"
date: "8/14/2020"
output: pdf_document
---



## The Problem

We currently have to write a lot to define a tuning paramset. E.g. our presentation for pipelines tuning tunes a stacked learner, and the slide looks like this:


```r
ps = ParamSet$new(list(
  ParamFct$new("branch.selection", levels = c("pca", "nothing")),
  ParamDbl$new("anova.filter.frac", lower = 0.1, upper = 1),
  ParamFct$new("lrn_branch.selection", levels = c("svm", "xgb", "rf")),
  ParamInt$new("rf.mtry", lower = 1L, upper = 20L),
  ParamInt$new("xgb.nrounds", lower = 1, upper = 500),
  ParamDbl$new("svm.cost", lower = -12, upper = 4),
  ParamDbl$new("svm.gamma", lower = -12, upper = -1)))
ps$add_dep("rf.mtry", "lrn_branch.selection", CondEqual$new("rf"))
ps$add_dep("xgb.nrounds", "lrn_branch.selection", CondEqual$new("xgb"))
ps$add_dep("svm.cost", "lrn_branch.selection", CondEqual$new("svm"))
ps$add_dep("svm.gamma", "lrn_branch.selection", CondEqual$new("svm"))
ps$trafo = function(x, param_set) {
  if (x$lrn_branch.selection == "svm") {
    x$svm.cost = 2^x$svm.cost; x$svm.gamma = 2^x$svm.gamma
  }
  return(x)
}
inst = TuningInstanceSingleCrit$new(tsk("sonar"), glrn, rsmp("cv", folds=3),
 msr("classif.ce"), ps, trm("evals", n_evals = 10))
```

A lot of the information here is redundant, because the `GraphLearner` being tuned already knows a lot about the parameters, their ranges, and their relationships. Furthermore, even writing down a new `ParamSet` takes much more typing than it should. In particular, trafos and dependencies are written at different places than the code which it affects. We identify three problems:

1. Generating `ParamSet`s is verbose
2. Trafos and dependencies are not defined "locally"
3. Specifying tuning `ParamSet`s is redundant.

### Teaser:

How nice would it be if we could write the `ParamSet` like the following:

```r
pars <- ps(
  branch.selection = p_fct(c("pca", "nothing")),
  anova.filter.frac = p_dbl(.1, 1),
  lrn_branch.selection = p_fct(c("svm", "xgb", "rf")),
  rf.mtry = p_int(1, 20, requires = lrn_branch.selection == "rf"),
  xgb.nrounds = p_int(1, 500, requires = lrn_branch.selection == "xgb"),
  svm.cost = p_dbl(-12, 4, requires = lrn_branch.selection == "svm",
    trafo = function(x) 2^x),
  svm.gamma = p_dbl(-12, -1, requires = lrn_branch.selection == "svm",
    trafo = function(x) 2^x)
)
```

or, making use of the information from the pipeline `ParamSet`:


```r
glrn$param_set$values = list(
  branch.selection = tune(),
  anova.filter.frac = tune(.1, 1),
  lrn_branch.selection = tune(),
  rf.mtry = tune(1, 20),
  xgb.nrounds = tune(1, 500),
  xgb.verbose = 0,
  svm.cost = tune(p_dbl(-12, 4, trafo = function(x) 2^x)),
  svm.type = "C-classification",
  svm.kernel = "radial"
)

glrn$param_set$tune_ps
#> <ParamSet>
#>                      id    class lower upper      levels        default value
#> 1:     branch.selection ParamFct    NA    NA pca,nothing <NoDefault[3]>      
#> 2:    anova.filter.frac ParamDbl   0.1     1             <NoDefault[3]>      
#> 3:             svm.cost ParamDbl -12.0     4             <NoDefault[3]>      
#> 4:          xgb.nrounds ParamInt   1.0   500             <NoDefault[3]>      
#> 5:              rf.mtry ParamInt   1.0    20             <NoDefault[3]>      
#> 6: lrn_branch.selection ParamFct    NA    NA  svm,xgb,rf <NoDefault[3]>      
#> Trafo is set.
```

## The Solution

I propose that these problems have solutions that are closely linked. A particular, there is one element in common to them: Parameter ranges, or what I am going to call "`Domain`". (This is probably close, in some way, to what set6 is doing?)

### `Domain`

A `Domain` is basically a `Param` without an ID, but with a trafo and with dependencies. It is an auxiliary object that has not much functionality and should only be used within shortform-functions, the user should not do any computation with them.

This is a nice object to have for the combined problems above, because

1. If we have a non-verbose constructor for `Domain`, then building a `ParamSet` from it could also be non-verbose. Granted, we can also get this by just having shortforms for `ParamDbl$new()` etc., but we won't get the following two benefits.
2. We can use `Domain` to specify trafos and dependencies locally. Instead of defining a `ParamSet` and *then* defining its dependencies and then its trafos, we can build a `ParamSet` from `Domain`s, where each domain contains the trafo and dependency that regards only itself.
3. We can use `Domain` to specify tuning ranges. This is why domains should be unnamed, so they can also be used for specifying tuning ranges. Here we have the benefit that we use a `Domain` object for both quickly defining `ParamSet`s and also for quickly defining tuning ranges.

We get the `Domain` constructors `p_int()`, `p_dbl()`, `p_fct()`, `p_lgl()`, `p_uty()`. We call them just like we call `ParamInt$new()`, *except* without an `id`, and *with* an optional `trafo` and `requirements` argument.


```r
single_digits <- p_int(0, 9)
logscale <- p_dbl(log(.01), log(10), trafo = exp)
fac <- p_fct(c("polynomial", "radial"))

# There is some implicit behaviour in that the `p_fct` Domain
# automatically generates a transformation for non-character elements.
# For example, the following:
funfac <- p_fct(c("identity", "log", "exp"),
  trafo = function(x) switch(x,
    identity = identity,
    log = log,
    exp = exp
))
# is much shorter like this:
funfac <- p_fct(list(identity = identity, log = log, exp = exp))

# we can specify requirements for a Domain. Here we say that, whatever
# parameter we define with it, will depend on some "kernel" parameter
# being equal to `"polynomial"`.
degree <- p_int(1, 4,
  requires = kernel == "polynomial" && kernel2 == "polynomial")
```

#### Internals

Because people are not supposed to use `Domain` outside of "sugary" usage, and in particular because they should not do any computation on these objects besides "sugar", we don't need to give these objects much inner life. They are just a `list()`, maybe with a printer (I will avoid calling things like this "`S3` objects" in this document for political reasons), with elements `constructor`, `constargs`, `trafo`, and `requirements`.

Constructing a `Param` from this is just `mlr3misc::invoke(constructor, id = <ID>, .args = constargs)`; the `trafo` and `requirements` will have to be handled in a way to be given to the resulting `ParamSet`.

### `ParamSet` Short Form Construction

We get a function `ps()` that collects `Domain` objects to a complete `ParamSet`. Its arguments must be named. This is very natural and similar to how we would write a list with named arguments etc.


```r
pars <- ps(
  a = p_int(1, 10),
  kernel = fac,
  kernel2 = fac,
  kernel3 = fac,
  c = funfac,
  degree = degree
)
pars
#> <ParamSet>
#>         id    class lower upper            levels        default        parents value
#> 1:       a ParamInt     1    10                   <NoDefault[3]>                     
#> 2:       c ParamFct    NA    NA  identity,log,exp <NoDefault[3]>                     
#> 3:  degree ParamInt     1     4                   <NoDefault[3]> kernel,kernel2      
#> 4:  kernel ParamFct    NA    NA polynomial,radial <NoDefault[3]>                     
#> 5: kernel2 ParamFct    NA    NA polynomial,radial <NoDefault[3]>                     
#> 6: kernel3 ParamFct    NA    NA polynomial,radial <NoDefault[3]>                     
#> Trafo is set.
```

This is mostly implemented, see how trafo is already working:

```r
set.seed(1)
generate_design_random(pars, 1)$transpose()
#> [[1]]
#> [[1]]$a
#> [1] 3
#> 
#> [[1]]$kernel
#> [1] "polynomial"
#> 
#> [[1]]$kernel2
#> [1] "radial"
#> 
#> [[1]]$kernel3
#> [1] "radial"
#> 
#> [[1]]$c
#> function (x) 
#> x
#> <bytecode: 0x563f67539520>
#> <environment: namespace:base>
```

Dependencies also work

```r
pars$deps
#>        id      on           cond
#> 1: degree  kernel <CondEqual[9]>
#> 2: degree kernel2 <CondEqual[9]>
```

If a `trafo` is needed that goes beyond modifying single parameters, it can be given to an `.extra_trafo` argument to `ps()`. It gets executed *after* the `Domain`-local trafos.


```r
pars <- ps(
  a = p_dbl(0, 1, trafo = exp),
  b = p_dbl(0, 1, trafo = exp),
  .extra_trafo = function(x, ps) {
    x$c <- x$a + x$b
    x
  }
)

# See how the addition happens after exp()ing:
pars$trafo(list(a = 0, b = 0))
#> $a
#> [1] 1
#> 
#> $b
#> [1] 1
#> 
#> $c
#> [1] 2
```

#### Internals

Just call the `Param` constructors from the `Domain` objects as described above. The `trafo` functions of each `Domain` are put together into one big `trafo` for the resulting `ParamSet`, and the dependencies are parsed and added.

### Tune `ParamSet` autogeneration

When defining tuning `ParamSet`s, we want to make use of the information stored in the `Learner`'s `ParamSet`. A nice way to define a tuning scenario is if we can define the fixed and variable parameters of an object at the same time. We solve this by using a `TuneToken` list-with-a-printer, constructed via `tune()`. It is given to the `$values` slot of a `ParamSet` and indicates that a parameter does not have a preset value, and instead should be tuned over.



```r
ll <- lrn("classif.rpart")
ll$param_set$values = list(
  minsplit = 10,
  cp = tune()
)
```

The `ParamSet` has a `$tune_ps` active binding that creates the `ParamSet` for tuning out of this. Tuner code, like e.g. `AutoTuner` etc., call this and create a tuning paramset automatically:

```r
ll$param_set$tune_ps
#> <ParamSet>
#>    id    class lower upper levels default value
#> 1: cp ParamDbl     0     1           0.01
```

The `Learner`-side of the `ParamSet` must use `get_values()`, which will throw an error if any `TuneToken` objects are present in the values, since it means the `Learner` is being called with a parameter that should actually be tuned.

The printer of the `TuneToken` can indicate that the respective value of a `ParamSet` is to be tuned:

```r
print(ll$param_set$values)
#> $minsplit
#> [1] 10
#> 
#> $cp
#> Tuning over:
#> <entire parameter range>
```

Nomenclature: We call `ll$param_set$params$cp` the *underlying parameter*, and `ll$param_set$tune_ps$cp` the *tuning parameter*. They could have different names or types if a `$trafo` is involved.

### `TuneToken`

However, maybe we do not want to tune over the full range of `cp`, or maybe we want to tune over integer values of a `ParamDbl`, or we want to tune over a `ParamUty` with a transformation. The `tune()` constructor therefore admits five behaviours:

1. **`tune()`**: Tune over the whole range of a (bounded) `Param`.
2. **`tune(lower, upper)`**: Tune over the (numeric or integer) `Param` with the given bounds.
3. **`tune(value_vector_or_list)`**: Tune over the values in the given vector or list. This is done by creating a `ParamFct` tuning-`Param` with a trafo that converts to the type required by the underlying parameter.
4. **`tune(Domain)`**: Tune over the domain, making use of the given dependencies and trafos if necessary. This is useful if the type over which we tune is different from the underlying parameter being tuned. See notes below.
5. **`tune(ParamSet)`**: Tune over the `ParamSet`, making use of its `trafo` etc. This is useful if we tune a single (usually `ParamUty`) underlying parameter with multiple tuning parameters.

**Notes**: Why is it nice to have `tune(Domain)` instead of `tune(lower, upper, trafo)` when we need a trafo? Because

1. This functionality is overlapping a lot with `Domain` already, we get two functionalities for the price of one
2. When we give a `trafo`, we can often expect that the tuning parameter and the underlying parameter have different types, e.g. tuning from `log(100)` to `log(1000)` with trafo `round(exp(x))` for a `ParamInt` underlying parameter where the tuning parameter is a `ParamDbl`.

The following performs tuning over the vector-valued `ParamUty` `regularization.factor`; the vector value is constructed from the `reg.sepal` and `reg.petal` tuning parameters.


```r
round_exp = function(x) round(exp(x))  # maybe we want this in paradox

lr <- lrn("classif.ranger")
lr$param_set$values = list(
  mtry = 2,
  num.trees = tune(p_dbl(log(10), log(1000), trafo = round_exp)),
  regularization.factor = tune(
    ps(
      reg.sepal = p_dbl(0, 1),
      reg.petal = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) {
        list(regularization.factor =
          c(x$reg.sepal, x$reg.sepal, x$reg.petal, x$reg.petal))
      }
    )
  )
)
lr$param_set$tune_ps
#> <ParamSet>
#>           id    class    lower    upper levels        default value
#> 1: num.trees ParamDbl 2.302585 6.907755        <NoDefault[3]>      
#> 2: reg.sepal ParamDbl 0.000000 1.000000        <NoDefault[3]>      
#> 3: reg.petal ParamDbl 0.000000 1.000000        <NoDefault[3]>      
#> Trafo is set.

generate_design_random(lr$param_set$tune_ps, 1)$transpose()
#> [[1]]
#> [[1]]$num.trees
#> [1] 775
#> 
#> [[1]]$regularization.factor
#> [1] 0.6607978 0.6607978 0.6291140 0.6291140
```

`mlr3pipelines` has the `affect_columns` parameter, which is a `ParamUty` that takes any object (though often a `Selector` object). Suppose we want to do `PCA` on all columns except one of the `iris` columns:


```r
ts = tsk("iris")
glrn = as_learner(po("pca") %>>% lrn("classif.rpart"))
glrn$param_set$values$pca.affect_columns = tune(
  p_fct(ts$feature_names, trafo = function(x) selector_invert(selector_name(x)))
)

generate_design_random(glrn$param_set$tune_ps, 1)$transpose()
#> [[1]]
#> [[1]]$pca.affect_columns
#> selector_invert(selector_name("Petal.Length"))
```

#### Internals

The `ParamSet$values` slot stores the `TuneToken` and uses that to create a tuning `ParamSet` whenever `$tune_ps` is queried. This is done by
1. Generating a `ParamSet` for each individual value that is set to a `TuneToken`, using the information retrieved from the `TuneToken` (range, factor levels, etc.) and information from the `Param` itself to create the tuning parameter. E.g. `tune()` just clones the `Param`, while `tune(p_fct(...))` needs only the `$id` of the `Param` and does some validity checking.
2. Putting the individual `ParamSets` gotten like this into a common `ParamSetCollection`. The `ParamSetCollection` is very useful here, because the individual tuning `ParamSets` of the parameters being tuned could be single-parameter or multi-parameter (see the `regularization.factor` example above). The `ParamSetCollection` also makes sure that the individual trafos of individual `ParamSets` are called correctly.
3. If we are dealing with a `GraphLearner`, then we are are already dealing with a `ParamSetCollection` on the outside (i.e. `glrn$param_set` is a `ParamSetCollection`). It must provide a `$tune_ps` active binding just as `ParamSet`. It just puts together the individual tuning paramsets into a `ParamSetCollection` as well; trafos etc. get handled transparently.

## Challenges

* Currently [dependencies on the `Learner`-side are broken](https://github.com/mlr-org/paradox/issues/216), but it should be investigated how they should be handled if we ever get them to work. They should probably be added to the result of `$tune_ps` automatically. A question that remains is how dependencies between `Params` that are in different `ParamSets` in a `ParamSetCollection` should be handled, e.g. when a `PipeOp`'s parameter depends on a `PipeOpBranch`.

* I am not sure whether `trafo = exp` should be allowed for `p_int`, and rounding should happen automatically. An alternative is to create the `round_exp` function as above.
