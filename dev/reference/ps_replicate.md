# Create a ParamSet by Repeating a Given ParamSet

Repeat a
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md) a
given number of times and thus create a larger
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md). By
default, the resulting parameters are prefixed with the string
`"repX.", where `X`counts up from 1. It is also possible to tag parameters by their original name and by their prefix, making grouped retrieval e.g. using`\$get_values()\`
easier.

## Usage

``` r
ps_replicate(
  set,
  times = length(affixes),
  affixes = sprintf("rep%s", seq_len(times)),
  postfix = FALSE,
  tag_sets = FALSE,
  tag_params = FALSE
)
```

## Arguments

- set:

  ([`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md))  
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md) to
  use as template.

- times:

  (`integer(1)`)  
  Number of times to repeat `set`. Should not be given if `affixes` is
  provided.

- affixes:

  (`character`)  
  A `character` vector indicating the prefixes / postfixes to use for
  each repetition of `set`. Per default, these are prefixes; if
  `postfix` is `TRUE`, these values are postfixed instead. If this is
  given, `times` is inferred from `length(affixes)` and should not be
  given separately. If `times` is given, this defaults to `"repX"`, with
  `X` counting up from 1.

- postfix:

  (`logical(1)`)  
  Whether to use `affixes` as a postfix instead of a prefix. Default
  `FALSE` (use prefixes).

- tag_sets:

  (`logical(1)`)  
  Whether to add a tag of the form `"set_<affixes[[i]]>"` to each
  parameter in the result, indicating the repetition each parameter
  belongs to.

- tag_params:

  (`logical(1)`)  
  Whether to add a tag of the form `"param_<id>"` to each parameter in
  the result, indicating the original parameter ID inside `set`.

## Examples

``` r
pset = ps(
  i = p_int(),
  z = p_lgl()
)

ps_replicate(pset, 3)
#> <ParamSet(6)>
#>        id    class lower upper nlevels        default  value
#>    <char>   <char> <num> <num>   <num>         <list> <list>
#> 1: rep1.i ParamInt  -Inf   Inf     Inf <NoDefault[0]> [NULL]
#> 2: rep1.z ParamLgl    NA    NA       2 <NoDefault[0]> [NULL]
#> 3: rep2.i ParamInt  -Inf   Inf     Inf <NoDefault[0]> [NULL]
#> 4: rep2.z ParamLgl    NA    NA       2 <NoDefault[0]> [NULL]
#> 5: rep3.i ParamInt  -Inf   Inf     Inf <NoDefault[0]> [NULL]
#> 6: rep3.z ParamLgl    NA    NA       2 <NoDefault[0]> [NULL]

ps_replicate(pset, affixes = c("first", "last"))
#> <ParamSet(4)>
#>         id    class lower upper nlevels        default  value
#>     <char>   <char> <num> <num>   <num>         <list> <list>
#> 1: first.i ParamInt  -Inf   Inf     Inf <NoDefault[0]> [NULL]
#> 2: first.z ParamLgl    NA    NA       2 <NoDefault[0]> [NULL]
#> 3:  last.i ParamInt  -Inf   Inf     Inf <NoDefault[0]> [NULL]
#> 4:  last.z ParamLgl    NA    NA       2 <NoDefault[0]> [NULL]

ps_replicate(pset, affixes = c("first", "last"), postfix = TRUE)
#> <ParamSet(4)>
#>         id    class lower upper nlevels        default  value
#>     <char>   <char> <num> <num>   <num>         <list> <list>
#> 1: i.first ParamInt  -Inf   Inf     Inf <NoDefault[0]> [NULL]
#> 2: z.first ParamLgl    NA    NA       2 <NoDefault[0]> [NULL]
#> 3:  i.last ParamInt  -Inf   Inf     Inf <NoDefault[0]> [NULL]
#> 4:  z.last ParamLgl    NA    NA       2 <NoDefault[0]> [NULL]

pset$values = list(i = 1, z = FALSE)

psr = ps_replicate(pset, 2, tag_sets = TRUE, tag_params = TRUE)

# observe the effect of tag_sets, tag_params:
psr$tags
#> $rep1.i
#> [1] "set_rep1" "param_i" 
#> 
#> $rep1.z
#> [1] "set_rep1" "param_z" 
#> 
#> $rep2.i
#> [1] "set_rep2" "param_i" 
#> 
#> $rep2.z
#> [1] "set_rep2" "param_z" 
#> 

# note that values are repeated as well
psr$values
#> $rep1.i
#> [1] 1
#> 
#> $rep1.z
#> [1] FALSE
#> 
#> $rep2.i
#> [1] 1
#> 
#> $rep2.z
#> [1] FALSE
#> 

psr$set_values(rep1.i = 10, rep2.z = TRUE)
psr$values
#> $rep1.i
#> [1] 10
#> 
#> $rep1.z
#> [1] FALSE
#> 
#> $rep2.i
#> [1] 1
#> 
#> $rep2.z
#> [1] TRUE
#> 

# use `any_tags` to get subset of values.
# `any_tags = ` is preferable to `tags = `, since parameters
# could also have other tags. `tags = ` would require the
# selected params to have the given tags exclusively.

# get all values associated with the original parameter `i`
psr$get_values(any_tags = "param_i")
#> $rep1.i
#> [1] 10
#> 
#> $rep2.i
#> [1] 1
#> 

# get all values associated with the first repetition "rep1"
psr$get_values(any_tags = "set_rep1")
#> $rep1.i
#> [1] 10
#> 
#> $rep1.z
#> [1] FALSE
#> 
```
