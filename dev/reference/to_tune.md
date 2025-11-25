# Indicate that a Parameter Value should be Tuned

`to_tune()` creates a `TuneToken` object which can be assigned to the
`$values` slot of a
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md) as
an alternative to a concrete value. This indicates that the value is not
given directly but should be tuned using
[bbotk](https://CRAN.R-project.org/package=bbotk) or
[mlr3tuning](https://CRAN.R-project.org/package=mlr3tuning). If the thus
parameterized object is invoked directly, without being wrapped by or
given to a tuner, it will give an error.

The tuning range
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md) that
is constructed from the `TuneToken` values in a
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)'s
`$values` slot can be accessed through the `ParamSet$search_space()`
method. This is done automatically by tuners if no tuning range is
given, but it is also possible to access the `$search_space()` method,
modify it further, and give the modified
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md) to a
tuning function (or do anything else with it, nobody is judging you).

A `TuneToken` represents the range over which the parameter whose
`$values` slot it occupies should be tuned over. It can be constructed
via the `to_tune()` function in one of several ways:

- **`to_tune()`**: Indicates a parameter should be tuned over its entire
  range. Only applies to finite parameters (i.e. discrete or bounded
  numeric parameters)

- **`to_tune(lower, upper, logscale)`**: Indicates a numeric parameter
  should be tuned in the inclusive interval spanning `lower` to `upper`,
  possibly on a log scale if `logscale` is se to `TRUE`. All parameters
  are optional, and the parameter's own lower / upper bounds are used
  without log scale, by default. Depending on the parameter, integer (if
  it is a
  [`p_int()`](https://paradox.mlr-org.com/dev/reference/Domain.md)) or
  real values (if it is a
  [`p_dbl()`](https://paradox.mlr-org.com/dev/reference/Domain.md)) are
  used.  
  `lower`, `upper`, and `logscale` can be given by position, except when
  only one of them is given, in which case it must be named to
  disambiguate from the following cases.  
  When `logscale` is `TRUE`, then a `trafo` is generated automatically
  that transforms to the given bounds. The bounds are log()'d pre-trafo
  (see examples). See the `logscale` argument of
  [`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md)
  functions for more info.  
  Note that "logscale" is *not* inherited from the
  [`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md) that
  the `TuneToken` belongs to! Defining a parameter with
  `p_dbl(... logscale = TRUE)` will *not* automatically give the
  `to_tune()` assigned to it log-scale.

- **`to_tune(levels)`**: Indicates a parameter should be tuned through
  the given discrete values. `levels` can be any named or unnamed atomic
  vector or list (although in the unnamed case it must be possible to
  construct a corresponding `character` vector with distinct values
  using `as.character`).

- **`to_tune(<Domain>)`**: The given
  [`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md) object
  (constructed e.g. with
  [`p_int()`](https://paradox.mlr-org.com/dev/reference/Domain.md) or
  [`p_fct()`](https://paradox.mlr-org.com/dev/reference/Domain.md))
  indicates the range which should be tuned over. The supplied `trafo`
  function is used for parameter transformation.

- **`to_tune(<ParamSet>)`**: The given
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md) is
  used to tune over a single dimension. This is useful for cases where a
  single evaluation-time parameter value (e.g.
  [`p_uty()`](https://paradox.mlr-org.com/dev/reference/Domain.md)) is
  constructed from multiple tuner-visible parameters (which may not be
  [`p_uty()`](https://paradox.mlr-org.com/dev/reference/Domain.md)). If
  not one-dimensional, the supplied
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)
  should always contain a `$extra_trafo` function, which must then
  always return a `list` with a single entry.

The `TuneToken` object's internals are subject to change and should not
be relied upon. `TuneToken` objects should only be constructed via
`to_tune()`, and should only be used by giving them to `$values` of a
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md).

## Usage

``` r
to_tune(..., internal = !is.null(aggr), aggr = NULL)
```

## Arguments

- ...:

  if given, restricts the range to be tuning over, as described above.

- internal:

  (`logical(1)`)  
  Whether to create an `InternalTuneToken`. This is only available for
  parameters tagged with `"internal_tuning"`.

- aggr:

  (`function`)  
  Function with one argument, which is a list of parameter values and
  returns a single aggregated value (e.g. the mean). This specifies how
  multiple parameter values are aggregated to form a single value in the
  context of internal tuning. If none specified, the default aggregation
  function of the parameter will be used.

## Value

A `TuneToken` object.

## See also

Other ParamSet construction helpers:
[`Domain()`](https://paradox.mlr-org.com/dev/reference/Domain.md),
[`ps()`](https://paradox.mlr-org.com/dev/reference/ps.md)

## Examples

``` r
params = ps(
  int = p_int(0, 10),
  int_unbounded = p_int(),
  dbl = p_dbl(0, 10),
  dbl_unbounded = p_dbl(),
  dbl_bounded_below = p_dbl(lower = 1),
  fct = p_fct(c("a", "b", "c")),
  uty1 = p_uty(),
  uty2 = p_uty(),
  uty3 = p_uty(),
  uty4 = p_uty(),
  uty5 = p_uty()
)

params$values = list(

  # tune over entire range of `int`, 0..10:
  int = to_tune(),

  # tune over 2..7:
  int_unbounded = to_tune(2, 7),

  # tune on a log scale in range 1..10;
  # recognize upper bound of 10 automatically, but restrict lower bound to 1:
  dbl = to_tune(lower = 1, logscale = TRUE),
  ## This is equivalent to the following:
  # dbl = to_tune(p_dbl(log(1), log(10), trafo = exp)),

  # nothing keeps us from tuning a dbl over integer values
  dbl_unbounded = to_tune(p_int(1, 10)),

  # tune over values "a" and "b" only
  fct = to_tune(c("a", "b")),

  # tune over integers 2..8.
  # ParamUty needs type information in form of p_xxx() in to_tune.
  uty1 = to_tune(p_int(2, 8)),

  # tune uty2 like a factor, trying 1, 10, and 100:
  uty2 = to_tune(c(1, 10, 100)),

  # tune uty3 like a factor. The factor levels are the names of the list
  # ("exp", "square"), but the trafo will generate the values from the list.
  # This way you can tune an objective that has function-valued inputs.
  uty3 = to_tune(list(exp = exp, square = function(x) x^2)),

  # tune through multiple parameters. When doing this, the ParamSet in tune()
  # must have the trafo that generates a list with one element and the right
  # name:
  uty4 = to_tune(ps(
    base = p_dbl(0, 1),
    exp = p_int(0, 3),
    .extra_trafo = function(x, param_set) {
      list(uty4 = x$base ^ x$exp)
    }
  )),

  # not all values need to be tuned!
  uty5 = 100
)

print(params$values)
#> $int
#> Tuning over:
#> <entire parameter range>
#> 
#> 
#> $int_unbounded
#> Tuning over:
#> range [2, 7]
#> 
#> 
#> $dbl
#> Tuning over:
#> range [1, ...] (log scale)
#> 
#> 
#> $dbl_unbounded
#> Tuning over:
#> p_int(lower = 1, upper = 10)
#> 
#> $fct
#> Tuning over:
#> p_fct(levels = c("a", "b"))
#> 
#> $uty1
#> Tuning over:
#> p_int(lower = 2, upper = 8)
#> 
#> $uty2
#> Tuning over:
#> p_fct(levels = c(`1` = 1, `10` = 10, `100` = 100))
#> 
#> $uty3
#> Tuning over:
#> p_fct(levels = list(exp = .Primitive("exp"), square = function (x) 
#> x^2))
#> 
#> $uty4
#> Tuning over:
#> <ParamSet(2)>
#>        id    class lower upper nlevels        default  value
#>    <char>   <char> <num> <num>   <num>         <list> <list>
#> 1:   base ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
#> 2:    exp ParamInt     0     3       4 <NoDefault[0]> [NULL]
#> Trafo is set.
#> 
#> $uty5
#> [1] 100
#> 

print(params$search_space())
#> <ParamSet(10)>
#>                id    class lower     upper nlevels        default  value
#>            <char>   <char> <num>     <num>   <num>         <list> <list>
#>  1:           int ParamInt     0 10.000000      11 <NoDefault[0]> [NULL]
#>  2: int_unbounded ParamInt     2  7.000000       6 <NoDefault[0]> [NULL]
#>  3:           dbl ParamDbl     0  2.302585     Inf <NoDefault[0]> [NULL]
#>  4: dbl_unbounded ParamInt     1 10.000000      10 <NoDefault[0]> [NULL]
#>  5:           fct ParamFct    NA        NA       2 <NoDefault[0]> [NULL]
#>  6:          uty1 ParamInt     2  8.000000       7 <NoDefault[0]> [NULL]
#>  7:          uty2 ParamFct    NA        NA       3 <NoDefault[0]> [NULL]
#>  8:          uty3 ParamFct    NA        NA       2 <NoDefault[0]> [NULL]
#>  9:          base ParamDbl     0  1.000000     Inf <NoDefault[0]> [NULL]
#> 10:           exp ParamInt     0  3.000000       4 <NoDefault[0]> [NULL]
#> Trafo is set.

# Change `$values` directly and generate new `$search_space()` to play around
params$values$uty3 = 8
params$values$uty2 = to_tune(c(2, 4, 8))

print(params$search_space())
#> <ParamSet(9)>
#>               id    class lower     upper nlevels        default  value
#>           <char>   <char> <num>     <num>   <num>         <list> <list>
#> 1:           int ParamInt     0 10.000000      11 <NoDefault[0]> [NULL]
#> 2: int_unbounded ParamInt     2  7.000000       6 <NoDefault[0]> [NULL]
#> 3:           dbl ParamDbl     0  2.302585     Inf <NoDefault[0]> [NULL]
#> 4: dbl_unbounded ParamInt     1 10.000000      10 <NoDefault[0]> [NULL]
#> 5:           fct ParamFct    NA        NA       2 <NoDefault[0]> [NULL]
#> 6:          uty1 ParamInt     2  8.000000       7 <NoDefault[0]> [NULL]
#> 7:          uty2 ParamFct    NA        NA       3 <NoDefault[0]> [NULL]
#> 8:          base ParamDbl     0  1.000000     Inf <NoDefault[0]> [NULL]
#> 9:           exp ParamInt     0  3.000000       4 <NoDefault[0]> [NULL]
#> Trafo is set.

# Notice how `logscale` applies `log()` to lower and upper bound pre-trafo:
params = ps(x = p_dbl())

params$values$x = to_tune(1, 100, logscale = TRUE)

print(params$search_space())
#> <ParamSet(1)>
#>        id    class lower   upper nlevels        default  value
#>    <char>   <char> <num>   <num>   <num>         <list> <list>
#> 1:      x ParamDbl     0 4.60517     Inf <NoDefault[0]> [NULL]
#> Trafo is set.

grid = generate_design_grid(params$search_space(), 3)

# The grid is equidistant within log-bounds pre-trafo:
print(grid)
#> <Design> with 3 rows:
#>           x
#>       <num>
#> 1: 0.000000
#> 2: 2.302585
#> 3: 4.605170

# But the values are on a log scale scale with desired bounds after trafo:
print(grid$transpose())
#> [[1]]
#> [[1]]$x
#> [1] 1
#> 
#> 
#> [[2]]
#> [[2]]$x
#> [1] 10
#> 
#> 
#> [[3]]
#> [[3]]$x
#> [1] 100
#> 
#> 
```
