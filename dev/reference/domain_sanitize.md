# Map to Acceptable Value

Map values that are close enough to the given
[`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md) to
values that are truly acceptable.

This is used to map [`numeric()`](https://rdrr.io/r/base/numeric.html)
values that are close to but outside the acceptable interval to the
interval bounds. It is also used to convert integer-valued `numeric`
values to `integer` values for
[`p_int()`](https://paradox.mlr-org.com/dev/reference/Domain.md).

## Usage

``` r
domain_sanitize(param, values)
```

## Arguments

- param:

  (`Domain`).

- values:

  (`any`) – format depending on the `Domain`.

## Value

`any` – format depending on the `Domain`.
