# Generate a Space-Filling LHS Design

Generate a space-filling design using Latin hypercube sampling.
Dependent parameters whose constraints are unsatisfied generate `NA`
entries in their respective columns.

## Usage

``` r
generate_design_lhs(param_set, n, lhs_fun = NULL)
```

## Arguments

- param_set:

  ([`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)).

- n:

  (`integer(1)`)  
  Number of points to sample.

- lhs_fun:

  (`function(n, k)`)  
  Function to use to generate a LHS sample, with n samples and k values
  per param. LHS functions are implemented in package lhs, default is to
  use
  [`lhs::maximinLHS()`](https://rdrr.io/pkg/lhs/man/maximinLHS.html).

## Value

[`Design`](https://paradox.mlr-org.com/dev/reference/Design.md).

## See also

Other generate_design:
[`generate_design_grid()`](https://paradox.mlr-org.com/dev/reference/generate_design_grid.md),
[`generate_design_random()`](https://paradox.mlr-org.com/dev/reference/generate_design_random.md),
[`generate_design_sobol()`](https://paradox.mlr-org.com/dev/reference/generate_design_sobol.md)

## Examples

``` r
pset = ps(
  ratio = p_dbl(lower = 0, upper = 1),
  letters = p_fct(levels = letters[1:3])
)

if (requireNamespace("lhs", quietly = TRUE)) {
  generate_design_lhs(pset, 10)
}
#> <Design> with 10 rows:
#>          ratio letters
#>          <num>  <char>
#>  1: 0.62529970       a
#>  2: 0.59188032       c
#>  3: 0.88673502       b
#>  4: 0.32485387       b
#>  5: 0.14028812       b
#>  6: 0.47696302       a
#>  7: 0.71194854       a
#>  8: 0.21946950       c
#>  9: 0.91645692       c
#> 10: 0.06632066       a
```
