# Generate a Grid Design

Generate a grid with a specified resolution in the parameter space. The
resolution for categorical parameters is ignored, these parameters
always produce a grid over all their valid levels. For number params the
endpoints of the params are always included in the grid.

## Usage

``` r
generate_design_grid(param_set, resolution = NULL, param_resolutions = NULL)
```

## Arguments

- param_set:

  ([`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)).

- resolution:

  (`integer(1)`)  
  Global resolution for all parameters.

- param_resolutions:

  (named [`integer()`](https://rdrr.io/r/base/integer.html))  
  Resolution per
  [`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md), named
  by parameter ID.

## Value

[`Design`](https://paradox.mlr-org.com/dev/reference/Design.md).

## See also

Other generate_design:
[`generate_design_lhs()`](https://paradox.mlr-org.com/dev/reference/generate_design_lhs.md),
[`generate_design_random()`](https://paradox.mlr-org.com/dev/reference/generate_design_random.md),
[`generate_design_sobol()`](https://paradox.mlr-org.com/dev/reference/generate_design_sobol.md)

## Examples

``` r
pset = ps(
  ratio = p_dbl(lower = 0, upper = 1),
  letters = p_fct(levels = letters[1:3])
)
generate_design_grid(pset, 10)
#> <Design> with 30 rows:
#>         ratio letters
#>         <num>  <char>
#>  1: 0.0000000       a
#>  2: 0.0000000       b
#>  3: 0.0000000       c
#>  4: 0.1111111       a
#>  5: 0.1111111       b
#>  6: 0.1111111       c
#>  7: 0.2222222       a
#>  8: 0.2222222       b
#>  9: 0.2222222       c
#> 10: 0.3333333       a
#> 11: 0.3333333       b
#> 12: 0.3333333       c
#> 13: 0.4444444       a
#> 14: 0.4444444       b
#> 15: 0.4444444       c
#> 16: 0.5555556       a
#> 17: 0.5555556       b
#> 18: 0.5555556       c
#> 19: 0.6666667       a
#> 20: 0.6666667       b
#> 21: 0.6666667       c
#> 22: 0.7777778       a
#> 23: 0.7777778       b
#> 24: 0.7777778       c
#> 25: 0.8888889       a
#> 26: 0.8888889       b
#> 27: 0.8888889       c
#> 28: 1.0000000       a
#> 29: 1.0000000       b
#> 30: 1.0000000       c
#>         ratio letters
#>         <num>  <char>
```
