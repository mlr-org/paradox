# Generate a Space-Filling Sobol Sequence Design

Generate a space-filling design using a Sobol sequence. Dependent
parameters whose constraints are unsatisfied generate `NA` entries in
their respective columns.

Uses
[spacefillr::generate_sobol_set](https://rdrr.io/pkg/spacefillr/man/generate_sobol_set.html).

Note that non determinism is achieved by sampling the seed argument via
`sample(.Machine$integer.max, size = 1L)`.

## Usage

``` r
generate_design_sobol(param_set, n)
```

## Arguments

- param_set:

  ([`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)).

- n:

  (`integer(1)`)  
  Number of points to sample.

## Value

[`Design`](https://paradox.mlr-org.com/dev/reference/Design.md).

## See also

Other generate_design:
[`generate_design_grid()`](https://paradox.mlr-org.com/dev/reference/generate_design_grid.md),
[`generate_design_lhs()`](https://paradox.mlr-org.com/dev/reference/generate_design_lhs.md),
[`generate_design_random()`](https://paradox.mlr-org.com/dev/reference/generate_design_random.md)

## Examples

``` r
pset = ps(
  ratio = p_dbl(lower = 0, upper = 1),
  letters = p_fct(levels = letters[1:3])
)

if (requireNamespace("spacefillr", quietly = TRUE)) {
  generate_design_sobol(pset, 10)
}
#> <Design> with 10 rows:
#>          ratio letters
#>          <num>  <char>
#>  1: 0.08103293       b
#>  2: 0.58103293       c
#>  3: 0.33103293       c
#>  4: 0.83103293       a
#>  5: 0.45603293       a
#>  6: 0.95603293       b
#>  7: 0.70603293       a
#>  8: 0.20603293       c
#>  9: 0.76853293       c
#> 10: 0.26853293       a
```
