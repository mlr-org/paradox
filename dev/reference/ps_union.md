# Create a ParamSet from a list of ParamSets

This emulates `ParamSetCollection$new(sets)`, except that the result is
a flat
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md), not
a
[`ParamSetCollection`](https://paradox.mlr-org.com/dev/reference/ParamSetCollection.md).
The resulting object is decoupled from the input
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)
objects: Unlike
[`ParamSetCollection`](https://paradox.mlr-org.com/dev/reference/ParamSetCollection.md),
changing `$values` of the resulting object will not change the input
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)
`$values` by reference.

This emulates `ParamSetCollection$new(sets)`, which in particular means
that the resulting
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md) has
all the [`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md)s
from the input `sets`, but some `$id`s are changed: If the
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md) is
given in `sets` with a name, then the
[`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md)s will
have their `<id>` changed to `<name in "sets">.<id>`. This is also
reflected in deps.

The [`c()`](https://rdrr.io/r/base/c.html) operator, applied to
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)s, is
a synony for `ps_union()`. The named arguments `tag_sets`, `tag_params`,
and `postfix_names` are also available in the
[`c()`](https://rdrr.io/r/base/c.html) operator, but need to be used
with a preceding dot instead: `.tag_sets`, `.tag_params`, and
`.postfix_names`.

## Usage

``` r
ps_union(sets, tag_sets = FALSE, tag_params = FALSE, postfix_names = FALSE)
```

## Arguments

- sets:

  (`list` of
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md))  
  This may be a named list, in which case non-empty names are prefixed
  to parameters in the corresponding
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md).

- tag_sets:

  (`logical(1)`)  
  Whether to add tags of the form `"set_<set_id>"` to each parameter
  originating from a given `ParamSet` given with name
  `<name in "sets">`.

- tag_params:

  (`logical(1)`)  
  Whether to add tags of the form `"param_<param_id>"` to each parameter
  with original ID `<param_id>`.

- postfix_names:

  (`logical(1)`)  
  Whether to use names in `sets` as postfixes, instead of prefixes.
  Default `FALSE`.

## Examples

``` r
ps1 = ps(x = p_dbl())
ps1$values = list(x = 1)

ps2 = ps(y = p_lgl())

pu = ps_union(list(ps1, ps2))
# same as:
pu = c(ps1, ps2)

pu
#> <ParamSet(2)>
#>        id    class lower upper nlevels        default  value
#>    <char>   <char> <num> <num>   <num>         <list> <list>
#> 1:      x ParamDbl  -Inf   Inf     Inf <NoDefault[0]>      1
#> 2:      y ParamLgl    NA    NA       2 <NoDefault[0]> [NULL]

pu$values
#> $x
#> [1] 1
#> 

pu$values$x = 2
pu$values
#> $x
#> [1] 2
#> 

# p1 is unchanged:
ps1$values
#> $x
#> [1] 1
#> 

# Prefixes automatically created for named elements.
# This allows repeating components.
pu2 = c(one = ps1, two = ps1, ps2)
pu2
#> <ParamSet(3)>
#>        id    class lower upper nlevels        default  value
#>    <char>   <char> <num> <num>   <num>         <list> <list>
#> 1:  one.x ParamDbl  -Inf   Inf     Inf <NoDefault[0]>      1
#> 2:  two.x ParamDbl  -Inf   Inf     Inf <NoDefault[0]>      1
#> 3:      y ParamLgl    NA    NA       2 <NoDefault[0]> [NULL]

pu2$values
#> $one.x
#> [1] 1
#> 
#> $two.x
#> [1] 1
#> 

pu3 = c(one = ps1, two = ps1, ps2, .postfix_names = TRUE)
pu3
#> <ParamSet(3)>
#>        id    class lower upper nlevels        default  value
#>    <char>   <char> <num> <num>   <num>         <list> <list>
#> 1:  x.one ParamDbl  -Inf   Inf     Inf <NoDefault[0]>      1
#> 2:  x.two ParamDbl  -Inf   Inf     Inf <NoDefault[0]>      1
#> 3:      y ParamLgl    NA    NA       2 <NoDefault[0]> [NULL]

```
