# Generate a Random Design

Generates a design with randomly drawn points. Internally uses
[`SamplerUnif`](https://paradox.mlr-org.com/dev/reference/SamplerUnif.md),
hence, also works for
[ParamSet](https://paradox.mlr-org.com/dev/reference/ParamSet.md)s with
dependencies. If dependencies do not hold, values are set to `NA` in the
resulting data.table.

## Usage

``` r
generate_design_random(param_set, n)
```

## Arguments

- param_set:

  ([`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)).

- n:

  (`integer(1)`)  
  Number of points to draw randomly.

## Value

[`Design`](https://paradox.mlr-org.com/dev/reference/Design.md).

## See also

Other generate_design:
[`generate_design_grid()`](https://paradox.mlr-org.com/dev/reference/generate_design_grid.md),
[`generate_design_lhs()`](https://paradox.mlr-org.com/dev/reference/generate_design_lhs.md),
[`generate_design_sobol()`](https://paradox.mlr-org.com/dev/reference/generate_design_sobol.md)

## Examples

``` r
pset = ps(
  ratio = p_dbl(lower = 0, upper = 1),
  letters = p_fct(levels = letters[1:3])
)
generate_design_random(pset, 10)
#> <Design> with 10 rows:
#>         ratio letters
#>         <num>  <char>
#>  1: 0.7279909       c
#>  2: 0.2170845       b
#>  3: 0.4562302       c
#>  4: 0.3327998       c
#>  5: 0.5683527       c
#>  6: 0.2522057       a
#>  7: 0.4640136       a
#>  8: 0.9176605       c
#>  9: 0.9728442       c
#> 10: 0.8190824       a
```
