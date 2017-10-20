
phng
====

[![Build Status Linux](https://travis-ci.org/mlr-org/phng.svg?branch=master)](https://travis-ci.org/mlr-org/phng) [![Build Status Windows](https://ci.appveyor.com/api/projects/status/m26qhpq99cka8l1b?svg=true)](https://ci.appveyor.com/project/jakob-r/phng) [![Coverage Status](https://coveralls.io/repos/github/mlr-org/phng/badge.svg?branch=master)](https://coveralls.io/github/mlr-org/phng?branch=master)

Universal Parameter Space Description and Tools

-   [Issues and Bugs](https://github.com/mlr-org/phng/issues)
-   [Documentation](https://mlr-org.github.io/phng)

Installation
------------

``` r
devtools::install_github("mlr-org/phng", dependencies = TRUE)
```

Usage
-----

Create a simple ParamSet using all supported Parameter Types:

-   \_Int\_egers
-   *Real*-valued numbers
-   *Flag* for `TRUE`/`FALSE`
-   *Categorical* values, namely characters.
-   Further types are only possible by using transformations.

``` r
ps = ParamSetFlat$new(
  params = list(
    ParamInt$new(id = "z", lower = 1, upper = 3),
    ParamReal$new(id = "x", lower = -10, upper = 10),
    ParamFlag$new(id = "switch"),
    ParamCategorical$new(id = "methods", values = c("a","b","c"))
  )
)
```

Draw random samples / create random design:

``` r
ps$sample(3)
```

    ##    z         x switch methods
    ## 1: 1  7.660348  FALSE       b
    ## 2: 3  8.809346  FALSE       c
    ## 3: 2 -9.088870  FALSE       b

Generate LHS Design:

``` r
ps$generateLHSDesign(3)
```

    ##    z         x switch methods
    ## 1: 1 -9.019242  FALSE       b
    ## 2: 2  8.636449  FALSE       a
    ## 3: 3 -1.890614   TRUE       c

Generate Grid Design:

``` r
ps$generateGridDesign(resolution = 2)
```

    ##     z   x switch methods
    ##  1: 1 -10  FALSE       a
    ##  2: 1 -10  FALSE       c
    ##  3: 1 -10   TRUE       a
    ##  4: 1 -10   TRUE       c
    ##  5: 1  10  FALSE       a
    ##  6: 1  10  FALSE       c
    ##  7: 1  10   TRUE       a
    ##  8: 1  10   TRUE       c
    ##  9: 3 -10  FALSE       a
    ## 10: 3 -10  FALSE       c
    ## 11: 3 -10   TRUE       a
    ## 12: 3 -10   TRUE       c
    ## 13: 3  10  FALSE       a
    ## 14: 3  10  FALSE       c
    ## 15: 3  10   TRUE       a
    ## 16: 3  10   TRUE       c

Properties of the parameters within the ParamSet:

``` r
ps$values
```

    ## $z
    ## [1] 1 2 3
    ## 
    ## $x
    ## NULL
    ## 
    ## $switch
    ## [1]  TRUE FALSE
    ## 
    ## $methods
    ## [1] "a" "b" "c"

``` r
ps$param.classes
```

    ##                  z                  x             switch 
    ##         "ParamInt"        "ParamReal"        "ParamFlag" 
    ##            methods 
    ## "ParamCategorical"

``` r
ps$nlevels
```

    ##       z       x  switch methods 
    ##       3      NA       2       3

### Numeric ParamSet

Things you can do on an all numeric ParamSet:

``` r
ps = ParamSetFlat$new(
  params = c(
    list(ParamInt$new(id = "z", lower = -10, upper = 10)),
    repeatParam(2, ParamReal$new(id = "x", lower = 0, upper = 10))
  )
)

ps$lower
```

    ##            z x.repeated.1 x.repeated.2 
    ##          -10            0            0

``` r
ps$upper
```

    ##            z x.repeated.1 x.repeated.2 
    ##           10           10           10

``` r
ps$range
```

    ##              id upper lower
    ## 1:            z    10   -10
    ## 2: x.repeated.1    10     0
    ## 3: x.repeated.2    10     0

The usage of `repeatParam` generates tags show to which group the parameters belong to:

``` r
ps$member.tags
```

    ## $z
    ## NULL
    ## 
    ## $x.repeated.1
    ## [1] "x.repeated"
    ## 
    ## $x.repeated.2
    ## [1] "x.repeated"

This becomes useful if you want to do operations on parameters of one group like with transformations.

### Transformations

Transformations are functions with a fixed signature.

-   `x` A named list of parameter values. Each list item contains a vector of parameter values of a single parameter.
-   `dict` An environment that can be accessed using the `$` operator. It can contains values that don't belong to any parameter but are important for transformations.
-   `tags` A list of the tags for each parameter. Each parameter can have a various tags indicating additional characteristics.

### ParamTree

``` r
  ps = ParamTree$fac(
      ParamTree$dn(node = ParamCategorical$new(id = "model", values = c("SVM", "RF"))),
      ParamTree$dn(node = ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "model", val = "SVM")),
      ParamTree$dn(node = ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), depend = list(id = "model", val = "SVM")),
      ParamTree$dn(node = ParamReal$new(id = "gamma", lower = 0, upper = 100), depend = list(id = "kernel", val = "rbf"))
      )
```

    ## parsing model
    ## hit no depend : model
    ## number in wait list left 3
    ## parsing C
    ## hit depend:  C
    ## number in wait list left 2
    ## parsing kernel
    ## hit depend:  kernel
    ## number in wait list left 1
    ## parsing gamma
    ## hit depend:  gamma
    ## number in wait list left 0

``` r
  ps$getFirstMandChild$sample()
```

    ## sampling model
    ## 
    ## SVM
    ## 
    ## 
    ## sampling C
    ## 
    ## 41.4546335814521
    ## 
    ## 
    ## sampling kernel
    ## 
    ## rbf
    ## 
    ## 
    ## sampling gamma
    ## 
    ## 36.8845450924709

``` r
  ps$toStringVal()
```

    ## -Root:TBD
    ## ++-model:SVM
    ## ++++-C:41.4546335814521
    ## ++++-kernel:rbf
    ## ++++++-gamma:36.8845450924709
