
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

Create a simple ParamSet:

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
