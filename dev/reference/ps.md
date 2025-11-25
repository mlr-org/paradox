# Construct a ParamSet using Short Forms

The `ps()` short form constructor uses
[`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md) objects
(`p_dbl`, `p_fct`, ...) to construct
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)s in
a succinct and readable way.

For more specifics also see the documentation of
[`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md).

## Usage

``` r
ps(
  ...,
  .extra_trafo = NULL,
  .constraint = NULL,
  .allow_dangling_dependencies = FALSE
)
```

## Arguments

- ...:

  ([`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md))  
  Named arguments of
  [`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md)
  objects. The
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)
  will be constructed of the given
  [`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md)s, The
  names of the arguments will be used as `$id()` in the resulting
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md).

- .extra_trafo:

  (`function(x, param_set)`)  
  Transformation to set the resulting
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)'s
  `$trafo` value to. This is in addition to any `trafo` of
  [`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md)
  objects given in `...`, and will be run *after* transformations of
  individual parameters were performed.

- .constraint:

  (`function(x)`)  
  Constraint function. When given, this function must evaluate a named
  [`list()`](https://rdrr.io/r/base/list.html) of values and determine
  whether it satisfies constraints, returning a scalar `logical(1)`
  value.

- .allow_dangling_dependencies:

  (`logical`)  
  Whether dependencies depending on parameters that are not present
  should be allowed. A parameter `x` having `depends = y == 0` if `y` is
  not present in the `ps()` call would usually throw an error, but if
  dangling dependencies are allowed, the dependency is added regardless.
  This is usually a bad idea and mainly for internal use. Dependencies
  between
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)s
  when using
  [`to_tune()`](https://paradox.mlr-org.com/dev/reference/to_tune.md)
  can be realized using this.

## Value

A [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)
object.

## See also

Other ParamSet construction helpers:
[`Domain()`](https://paradox.mlr-org.com/dev/reference/Domain.md),
[`to_tune()`](https://paradox.mlr-org.com/dev/reference/to_tune.md)

## Examples

``` r
pars = ps(
  a = p_int(0, 10),
  b = p_int(upper = 20),
  c = p_dbl(),
  e = p_fct(letters[1:3]),
  f = p_uty(custom_check = checkmate::check_function)
)
print(pars)
#> <ParamSet(5)>
#>        id    class lower upper nlevels        default  value
#>    <char>   <char> <num> <num>   <num>         <list> <list>
#> 1:      a ParamInt     0    10      11 <NoDefault[0]> [NULL]
#> 2:      b ParamInt  -Inf    20     Inf <NoDefault[0]> [NULL]
#> 3:      c ParamDbl  -Inf   Inf     Inf <NoDefault[0]> [NULL]
#> 4:      e ParamFct    NA    NA       3 <NoDefault[0]> [NULL]
#> 5:      f ParamUty    NA    NA     Inf <NoDefault[0]> [NULL]

pars = ps(
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
#> 

pars$values = list(
  a = to_tune(ps(x = p_int(0, 1),
    .extra_trafo = function(x, param_set) list(a = x$x)
  )),
  # make 'y' depend on 'x', but they are defined in different ParamSets
  # Therefore we need to allow dangling dependencies here.
  b = to_tune(ps(y = p_int(0, 1, depends = x == 1),
    .extra_trafo = function(x, param_set) list(b = x$y),
    .allow_dangling_dependencies = TRUE
  ))
)

pars$search_space()
#> <ParamSet(2)>
#> Key: <id>
#>        id    class lower upper nlevels        default parents  value
#>    <char>   <char> <num> <num>   <num>         <list>  <list> <list>
#> 1:      x ParamInt     0     1       2 <NoDefault[0]>  [NULL] [NULL]
#> 2:      y ParamInt     0     1       2 <NoDefault[0]>       x [NULL]
#> Trafo is set.
```
