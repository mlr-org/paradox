
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

    ##    z          x switch methods
    ## 1: 1  0.4842227  FALSE       a
    ## 2: 3 -9.7196031   TRUE       c
    ## 3: 2  7.9520227  FALSE       b

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
    repeatParam(2, ParamReal$new(id = "x", lower = 0, upper = 1))
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
    ##           10            1            1

``` r
ps$range
```

    ##              id upper lower
    ## 1:            z    10   -10
    ## 2: x.repeated.1     1     0
    ## 3: x.repeated.2     1     0

The usage of `repeatParam` generates tags that indicate to which group the parameters belong to:

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

-   `x` A `data.table` of parameter values. Each column contains a vector of parameter values of a single parameter.
-   `dict` An environment, that can be accessed using the `$` operator. It can contains values that don't belong to any parameter but are important for transformations.
-   `tags` A list of the tags for each parameter. Each parameter can have various tags indicating additional characteristics.

Transformations are useful to scale parameters:

``` r
ps = ParamSetFlat$new(
  params = list(
    ParamInt$new(id = "z", lower = -3, upper = 3),
    ParamReal$new(id = "x", lower = 0, upper = 1)
  ),
  trafo = function(x, dict, tags) {
    x$z = 2^x$z
    x$x = round(x$x * dict$p)
    return(x)
  }
)
(x = ps$sample(3))
```

    ##     z         x
    ## 1: -3 0.3688455
    ## 2: -1 0.1524447
    ## 3: -1 0.1388061

The transformation uses the dictionary and will fail if none is supplied:

``` r
ps$transform(x)
```

    ## Error in `[<-.data.table`(x, j = name, value = value): RHS of assignment to existing column 'x' is zero length but not NULL. If you intend to delete the column use NULL. Otherwise, the RHS must have length > 0; e.g., NA_integer_. If you are trying to change the column type to be an empty list column then, as with all column type changes, provide a full length RHS vector such as vector('list',nrow(DT)); i.e., 'plonk' in the new column.

The dictionary can always be changed:

``` r
ps$dictionary = list(p = 10)
ps$transform(x)
```

    ##        z x
    ## 1: 0.125 4
    ## 2: 0.500 2
    ## 3: 0.500 1

``` r
ps$dictionary = list(p = 1000)
ps$transform(x)
```

    ##        z   x
    ## 1: 0.125 369
    ## 2: 0.500 152
    ## 3: 0.500 139

### Advanced Transformations

The following creates a ParamSet with a transformation that scales the `x` values and returns them as a vector. The original parameters will be removed from the trafo result. Keep in mind that `z` stays untouched and remains after the transformation.

``` r
ps = ParamSetFlat$new(
  params = c(
    list(ParamInt$new(id = "z", lower = -10, upper = 10)),
    repeatParam(2, ParamReal$new(id = "x", lower = 0, upper = 1))
  ),
  trafo = function(x, dict, tags) {
    scale = function(x1, x2) c(x1, x2) / sqrt(x1^2+x2^2) 
    x$x = Map(scale, x$x.repeated.1, x$x.repeated.2)
    x$x.repeated.1 = NULL
    x$x.repeated.2 = NULL
    return(x)
  }
)
```

The output of all value generating functions won't change for a ParamSet that has a `trafo` function. Instead these outputs can be put into `ps$transform()` to obtain the desired parameter values.

``` r
x = ps$generateLHSDesign(3)
ps$transform(x)
```

    ##     z                     x
    ## 1: -2   0.9839367,0.1785177
    ## 2: 10   0.8173283,0.5761722
    ## 3: -5 0.04754544,0.99886908

For more advanced transformations on repeated parameters you can use `trafoOnRepeatedParam()`:

``` r
ps = ParamSetFlat$new(
  params = c(
    list(
      ParamFlag$new(id = "switch"),
      ParamInt$new(id = "z", lower = 1, upper = 4)),
    repeatParam(4, ParamReal$new(id = "x", lower = 0, upper = 1))
  ),
  trafo = trafoOnRepeatedParam(
    fun = function(x, dict, tags) {
      scale = function(z, ...) {
        x = c(...)[1:z]
        x / sum(x)
      }
      res = do.call(Map, c(list(f = scale, z = dict$z), as.list(x)))
      list(x = res)
    }, repeated.param.id = "x", additional.params = "z")
  )
(x = ps$sample(3))
```

    ##    switch z x.repeated.1 x.repeated.2 x.repeated.3 x.repeated.4
    ## 1:  FALSE 4 0.0006247733    0.3798165    0.1111354    0.4176468
    ## 2:  FALSE 3 0.4753165741    0.6127710    0.2436195    0.7881958
    ## 3:   TRUE 3 0.2201188852    0.3517979    0.6680556    0.1028646

``` r
ps$transform(x)
```

    ##    switch                                                   x
    ## 1:  FALSE 0.0006871504,0.4177372576,0.1222311373,0.4593444546
    ## 2:  FALSE                       0.3569228,0.4601395,0.1829377
    ## 3:   TRUE                       0.1775192,0.2837143,0.5387665
