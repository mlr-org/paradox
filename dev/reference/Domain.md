# Domain: Parameter Range without an Id

A `Domain` object is a representation of a single dimension of a
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md).
`Domain` objects are used to construct
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)s,
either through the
[`ps()`](https://paradox.mlr-org.com/dev/reference/ps.md) short form,
through the
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)
constructor itself, or through the
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)`$search_space()`
mechanism (see
[`to_tune()`](https://paradox.mlr-org.com/dev/reference/to_tune.md)).
For each of the basic parameter classes (`"ParamInt"`, `"ParamDbl"`,
`"ParamLgl"`, `"ParamFct"`, and `"ParamUty"`) there is a function
constructing a `Domain` object (`p_int()`, `p_dbl()`, `p_lgl()`,
`p_fct()`, `p_uty()`). They each have fitting construction arguments
that control their bounds and behavior.

`Domain` objects are representations of parameter ranges and are
intermediate objects to be used in short form constructions in
[`to_tune()`](https://paradox.mlr-org.com/dev/reference/to_tune.md) and
[`ps()`](https://paradox.mlr-org.com/dev/reference/ps.md). Because of
their nature, they should not be modified by the user, once constructed.
The `Domain` object's internals are subject to change and should not be
relied upon.

## Usage

``` r
p_dbl(
  lower = -Inf,
  upper = Inf,
  special_vals = list(),
  default = NO_DEF,
  tags = character(),
  tolerance = sqrt(.Machine$double.eps),
  depends = NULL,
  trafo = NULL,
  logscale = FALSE,
  init,
  aggr = NULL,
  in_tune_fn = NULL,
  disable_in_tune = NULL
)

p_fct(
  levels,
  special_vals = list(),
  default = NO_DEF,
  tags = character(),
  depends = NULL,
  trafo = NULL,
  init,
  aggr = NULL,
  in_tune_fn = NULL,
  disable_in_tune = NULL
)

p_int(
  lower = -Inf,
  upper = Inf,
  special_vals = list(),
  default = NO_DEF,
  tags = character(),
  tolerance = sqrt(.Machine$double.eps),
  depends = NULL,
  trafo = NULL,
  logscale = FALSE,
  init,
  aggr = NULL,
  in_tune_fn = NULL,
  disable_in_tune = NULL
)

p_lgl(
  special_vals = list(),
  default = NO_DEF,
  tags = character(),
  depends = NULL,
  trafo = NULL,
  init,
  aggr = NULL,
  in_tune_fn = NULL,
  disable_in_tune = NULL
)

p_uty(
  custom_check = NULL,
  special_vals = list(),
  default = NO_DEF,
  tags = character(),
  depends = NULL,
  trafo = NULL,
  repr = substitute(default),
  init,
  aggr = NULL,
  in_tune_fn = NULL,
  disable_in_tune = NULL
)
```

## Arguments

- lower:

  (`numeric(1)`)  
  Lower bound, can be `-Inf`.

- upper:

  (`numeric(1)`)  
  Upper bound can be `+Inf`.

- special_vals:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Arbitrary special values this parameter is allowed to take, to make it
  feasible. This allows extending the domain of the parameter. Note that
  these values are only used in feasibility checks, neither in
  generating designs nor sampling.

- default:

  (`any`)  
  Default value. Can be from the domain of the parameter or an element
  of `special_vals`. Has value
  [NO_DEF](https://paradox.mlr-org.com/dev/reference/NO_DEF.md) if no
  default exists. `NULL` can be a valid default. The value has no effect
  on `ParamSet$values` or the behavior of `ParamSet$check()`, `$test()`
  or `$assert()`. The `default` is intended to be used for documentation
  purposes. \`

- tags:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Arbitrary tags to group and subset parameters. Some tags serve a
  special purpose:  

  - `"required"` implies that the parameters has to be given when
    setting `values` in
    [ParamSet](https://paradox.mlr-org.com/dev/reference/ParamSet.md).

- tolerance:

  (`numeric(1)`)  
  Initializes the `$tolerance` field that determines the

- depends:

  (`call` \| `expression`)  
  An expression indicating a requirement for the parameter that will be
  constructed from this. Can be given as an expression (using
  [`quote()`](https://rdrr.io/r/base/substitute.html)), or the
  expression can be entered directly and will be parsed using NSE (see
  examples). The expression may be of the form `<Param> == <value>` or
  `<Param> %in% <values>`, which will result in dependencies according
  to `ParamSet$add_dep(on = "<Param>", cond = CondEqual(<value>))` or
  `ParamSet$add_dep(on = "<Param>", cond = CondAnyOf(<values>))`,
  respectively (see
  [`CondEqual`](https://paradox.mlr-org.com/dev/reference/Condition.md),
  [`CondAnyOf`](https://paradox.mlr-org.com/dev/reference/Condition.md)).
  The expression may also contain multiple conditions separated by `&&`.

- trafo:

  (`function`)  
  Single argument function performing the transformation of a parameter.
  When the `Domain` is used to construct a
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md),
  this transformation will be applied to the corresponding parameter as
  part of the `$trafo` function.  
  Note that the trafo is *not* inherited by
  [`TuneToken`](https://paradox.mlr-org.com/dev/reference/to_tune.md)s!
  Defining a parameter with e.g. `p_dbl(..., trafo = ...)` will *not*
  automatically give the
  [`to_tune()`](https://paradox.mlr-org.com/dev/reference/to_tune.md)
  assigned to it a transformation. `trafo` only makes sense for
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)s
  that get used as search spaces for optimization or tuning, it is not
  useful when defining domains or hyperparameter ranges of learning
  algorithms, because these do not use trafos.

- logscale:

  (`logical(1)`)  
  Put numeric domains on a log scale. Default `FALSE`. Log-scale
  `Domain`s represent parameter ranges where lower and upper bounds are
  logarithmized, and where a `trafo` is added that exponentiates sampled
  values to the original scale. This is *not* the same as setting
  `trafo = exp`, because `logscale = TRUE` will handle parameter bounds
  internally: a `p_dbl(1, 10, logscale = TRUE)` results in a parameter
  that has lower bound `0`, upper bound `log(10)`, and uses `exp`
  transformation on these. Therefore, the given bounds represent the
  bounds *after* the transformation. (see examples).  
  `p_int()` with `logscale = TRUE` results in a continuous parameter
  similar to `p_dbl()`, not an integer-valued parameter, with bounds
  `log(max(lower, 0.5))` ... `log(upper + 1)` and a trafo similar to
  "`as.integer(exp(x))`" (with additional bounds correction). The lower
  bound is lifted to `0.5` if `lower` 0 to handle the `lower == 0` case.
  The upper bound is increased to `log(upper + 1)` because the trafo
  would otherwise almost never generate a value of `upper`.  
  When `logscale` is `TRUE`, then upper bounds may be infinite, but
  lower bounds should be greater than 0 for `p_dbl()` or greater or
  equal 0 for `p_int()`.  
  Note that "logscale" is *not* inherited by
  [`TuneToken`](https://paradox.mlr-org.com/dev/reference/to_tune.md)s!
  Defining a parameter with `p_dbl(... logscale = TRUE)` will *not*
  automatically give the
  [`to_tune()`](https://paradox.mlr-org.com/dev/reference/to_tune.md)
  assigned to it log-scale. `logscale` only makes sense for
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)s
  that get used as search spaces for optimization or tuning, it is not
  useful when defining domains or hyperparameter ranges of learning
  algorithms, because these do not use trafos.  
  `logscale` happens on a natural (`e == 2.718282...`) basis. Be aware
  that using a different base
  ([`log10()`](https://rdrr.io/r/base/Log.html)/`10^`,
  [`log2()`](https://rdrr.io/r/base/Log.html)/`2^`) is completely
  equivalent and does not change the values being sampled after
  transformation.

- init:

  (`any`)  
  Initial value. When this is given, then the corresponding entry in
  `ParamSet$values` is initialized with this value upon construction.

- aggr:

  (`function`)  
  Default aggregation function for a parameter. Can only be given for
  parameters tagged with `"internal_tuning"`. Function with one
  argument, which is a list of parameter values and that returns the
  aggregated parameter value.

- in_tune_fn:

  (`function(domain, param_vals)`)  
  Function that converters a `Domain` object into a parameter value. Can
  only be given for parameters tagged with `"internal_tuning"`. This
  function should also assert that the parameters required to enable
  internal tuning for the given `domain` are set in `param_vals` (such
  as `early_stopping_rounds` for `XGBoost`).

- disable_in_tune:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  The parameter values that need to be set in the `ParamSet` to disable
  the internal tuning for the parameter. For `XGBoost` this would e.g.
  be `list(early_stopping_rounds = NULL)`.

- levels:

  (`character` \| `atomic` \| `list`)  
  Allowed categorical values of the parameter. If this is not a
  `character`, then a `trafo` is generated that converts the names (if
  not given: [`as.character()`](https://rdrr.io/r/base/character.html)
  of the values) of the `levels` argument to the values. This trafo is
  then performed *before* the function given as the `trafo` argument.

- custom_check:

  (`function()`)  
  Custom function to check the feasibility. Function which checks the
  input. Must return 'TRUE' if the input is valid and a `character(1)`
  with the error message otherwise. This function should *not* throw an
  error. Defaults to `NULL`, which means that no check is performed.

- repr:

  (`language`)  
  Symbol to use to represent the value given in `default`. The
  [`deparse()`](https://rdrr.io/r/base/deparse.html) of this object is
  used when printing the domain, in some cases.

## Value

A `Domain` object.

## Details

Although the `levels` values of a constructed `p_fct()` will always be
`character`-valued, the `p_fct` function admits a `levels` argument that
goes beyond this: Besides a `character` vector, any atomic vector or
list (optionally named) may be given. (If the value is a list that is
not named, the names are inferred using
[`as.character()`](https://rdrr.io/r/base/character.html) on the
values.) The resulting `Domain` will correspond to a range of values
given by the names of the `levels` argument with a `trafo` that maps the
`character` names to the arbitrary values of the `levels` argument.

## See also

Other ParamSet construction helpers:
[`ps()`](https://paradox.mlr-org.com/dev/reference/ps.md),
[`to_tune()`](https://paradox.mlr-org.com/dev/reference/to_tune.md)

## Examples

``` r
params = ps(
  unbounded_integer = p_int(),
  bounded_double = p_dbl(0, 10),
  half_bounded_integer = p_dbl(1),
  half_bounded_double = p_dbl(upper = 1),
  double_with_trafo = p_dbl(-1, 1, trafo = exp),
  extra_double = p_dbl(0, 1, special_vals = list("xxx"), tags = "tagged"),
  factor_param = p_fct(c("a", "b", "c")),
  factor_param_with_implicit_trafo = p_fct(list(a = 1, b = 2, c = list()))
)
print(params)
#> <ParamSet(8)>
#>                                  id    class lower upper nlevels        default
#>                              <char>   <char> <num> <num>   <num>         <list>
#> 1:                unbounded_integer ParamInt  -Inf   Inf     Inf <NoDefault[0]>
#> 2:                   bounded_double ParamDbl     0    10     Inf <NoDefault[0]>
#> 3:             half_bounded_integer ParamDbl     1   Inf     Inf <NoDefault[0]>
#> 4:              half_bounded_double ParamDbl  -Inf     1     Inf <NoDefault[0]>
#> 5:                double_with_trafo ParamDbl    -1     1     Inf <NoDefault[0]>
#> 6:                     extra_double ParamDbl     0     1     Inf <NoDefault[0]>
#> 7:                     factor_param ParamFct    NA    NA       3 <NoDefault[0]>
#> 8: factor_param_with_implicit_trafo ParamFct    NA    NA       3 <NoDefault[0]>
#>     value
#>    <list>
#> 1: [NULL]
#> 2: [NULL]
#> 3: [NULL]
#> 4: [NULL]
#> 5: [NULL]
#> 6: [NULL]
#> 7: [NULL]
#> 8: [NULL]
#> Trafo is set.

params$trafo(list(
  bounded_double = 1,
  double_with_trafo = 1,
  factor_param = "c",
  factor_param_with_implicit_trafo = "c"
))
#> $bounded_double
#> [1] 1
#> 
#> $double_with_trafo
#> [1] 2.718282
#> 
#> $factor_param
#> [1] "c"
#> 
#> $factor_param_with_implicit_trafo
#> list()
#> 

# logscale:
params = ps(x = p_dbl(1, 100, logscale = TRUE))

# The ParamSet has bounds log(1) .. log(100):
print(params)
#> <ParamSet(1)>
#>        id    class lower   upper nlevels        default  value
#>    <char>   <char> <num>   <num>   <num>         <list> <list>
#> 1:      x ParamDbl     0 4.60517     Inf <NoDefault[0]> [NULL]
#> Trafo is set.

# When generating a equidistant grid, it is equidistant within log values
grid = generate_design_grid(params, 3)
print(grid)
#> <Design> with 3 rows:
#>           x
#>       <num>
#> 1: 0.000000
#> 2: 2.302585
#> 3: 4.605170

# But the values are on a log scale with desired bounds after trafo
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

# Integer parameters with logscale are `p_dbl()`s pre-trafo
params = ps(x = p_int(0, 10, logscale = TRUE))
print(params)
#> <ParamSet(1)>
#>        id    class      lower    upper nlevels        default  value
#>    <char>   <char>      <num>    <num>   <num>         <list> <list>
#> 1:      x ParamDbl -0.6931472 2.397895     Inf <NoDefault[0]> [NULL]
#> Trafo is set.

grid = generate_design_grid(params, 4)
print(grid)
#> <Design> with 4 rows:
#>             x
#>         <num>
#> 1: -0.6931472
#> 2:  0.3372003
#> 3:  1.3675478
#> 4:  2.3978953

# ... but get transformed to integers.
print(grid$transpose())
#> [[1]]
#> [[1]]$x
#> [1] 0
#> 
#> 
#> [[2]]
#> [[2]]$x
#> [1] 1
#> 
#> 
#> [[3]]
#> [[3]]$x
#> [1] 3
#> 
#> 
#> [[4]]
#> [[4]]$x
#> [1] 10
#> 
#> 


# internal tuning

param_set = ps(
  iters = p_int(0, Inf, tags = "internal_tuning", aggr = function(x) round(mean(unlist(x))),
    in_tune_fn = function(domain, param_vals) {
      stopifnot(domain$lower <= 1)
      stopifnot(param_vals$early_stopping == TRUE)
      domain$upper
    },
    disable_in_tune = list(early_stopping = FALSE)),
  early_stopping = p_lgl()
)
param_set$set_values(
  iters = to_tune(upper = 100, internal = TRUE),
  early_stopping = TRUE
)
param_set$convert_internal_search_space(param_set$search_space())
#> $iters
#> [1] 100
#> 
param_set$aggr_internal_tuned_values(
  list(iters = list(1, 2, 3))
)
#> $iters
#> [1] 2
#> 

param_set$disable_internal_tuning("iters")
param_set$values$early_stopping
#> [1] FALSE
```
