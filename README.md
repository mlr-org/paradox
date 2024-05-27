---
output: github_document
---

# paradox

Package website: [release](https://paradox.mlr-org.com/) | [dev](https://paradox.mlr-org.com/dev/)

Universal Parameter Space Description and Tools.

<!-- badges: start -->
[![r-cmd-check](https://github.com/mlr-org/paradox/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/mlr-org/paradox/actions/workflows/r-cmd-check.yml)
[![CRAN Status](https://www.r-pkg.org/badges/version/paradox)](https://CRAN.R-project.org/package=paradox)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->



## Installation


```r
remotes::install_github("mlr-org/paradox")
```

## Usage

Create a simple ParamSet using all supported Parameter Types:

* integer numbers (`"int"`)
* real-valued numbers (`"dbl"`)
* truth values `TRUE` or `FALSE` (`"lgl"`)
* categorical values from a set of possible strings (`"fct"`)
* further types are only possible by using transformations.


```r
pset = ps(
  z = p_int(lower = 1, upper = 3),
  x = p_dbl(lower = -10, upper = 10),
  flag = p_lgl(),
  methods = p_fct(c("a","b","c"))
)
```

Draw random samples / create random design:


```r
generate_design_random(pset, 3)
#> <Design> with 3 rows:
#>    z         x  flag methods
#> 1: 1  7.660348 FALSE       b
#> 2: 3  8.809346 FALSE       c
#> 3: 2 -9.088870 FALSE       b
```

Generate LHS Design:


```r
requireNamespace("lhs")
#> Loading required namespace: lhs
generate_design_lhs(pset, 3)
#> <Design> with 3 rows:
#>    z         x  flag methods
#> 1: 1 -3.984673  TRUE       b
#> 2: 2  7.938035 FALSE       a
#> 3: 3  1.969783  TRUE       c
```

Generate Grid Design:


```r
generate_design_grid(pset, resolution = 2)
#> <Design> with 24 rows:
#>     z   x  flag methods
#>  1: 1 -10  TRUE       a
#>  2: 1 -10  TRUE       b
#>  3: 1 -10  TRUE       c
#>  4: 1 -10 FALSE       a
#>  5: 1 -10 FALSE       b
#>  6: 1 -10 FALSE       c
#>  7: 1  10  TRUE       a
#>  [ reached getOption("max.print") -- omitted 18 rows ]
```

Properties of the parameters within the `ParamSet`:


```r
pset$ids()
#> [1] "z"       "x"       "flag"    "methods"
pset$levels
#> $z
#> NULL
#> 
#> $x
#> NULL
#> 
#> $flag
#> [1]  TRUE FALSE
#> 
#> $methods
#> [1] "a" "b" "c"
pset$nlevels
#>       z       x    flag methods 
#>       3     Inf       2       3
pset$is_number
#>       z       x    flag methods 
#>    TRUE    TRUE   FALSE   FALSE
pset$lower
#>       z       x    flag methods 
#>       1     -10      NA      NA
pset$upper
#>       z       x    flag methods 
#>       3      10      NA      NA
```

### Parameter Checks

Check that a parameter satisfies all conditions of a `ParamSet`, using `$test()` (returns `FALSE` on mismatch), `$check()` (returns error description on mismatch), and `$assert()` (throws error on mismatch):


```r
pset$test(list(z = 1, x = 1))
#> [1] TRUE
pset$test(list(z = -1, x = 1))
#> [1] FALSE
pset$check(list(z = -1, x = 1))
#> [1] "z: Element 1 is not >= 0.5"
pset$assert(list(z = -1, x = 1))
#> Error in pset$assert(list(z = -1, x = 1)): Assertion on 'list(z = -1, x = 1)' failed: z: Element 1 is not >= 0.5.
```

### Transformations

Transformations are functions with a fixed signature.

* `x` A named list of parameter values
* `param_set` the `ParamSet` used to create the design

Transformations can be used to change the distributions of sampled parameters.
For example, to sample values between $2^-3$ and $2^3$ in a $log_2$-uniform distribution, one can sample uniformly between -3 and 3 and exponentiate the random value inside the transformation.
Alternatively, `logscale = TRUE` can be set; in this case, `lower` and `upper` represent the values *after* the transformation.


```r
pset = ps(
  z = p_int(lower = -3, upper = 3),
  x = p_dbl(lower = 2^-3, upper = 2^3, logscale = TRUE)
)
pset$extra_trafo = function(x, param_set) {
  x$z = 2^x$z
  return(x)
}
pset_smplr = SamplerUnif$new(pset)
x = pset_smplr$sample(2)
xst = x$transpose()
xst
#> [[1]]
#> [[1]]$z
#> [1] 0.125
#> 
#> [[1]]$x
#> [1] 0.6985067
#> 
#> 
#> [[2]]
#> [[2]]$z
#> [1] 0.5
#> 
#> [[2]]$x
#> [1] 0.5795772
```

Further documentation can be found in the [mlr3book](https://mlr3book.mlr-org.com/technical.html#paradox).

