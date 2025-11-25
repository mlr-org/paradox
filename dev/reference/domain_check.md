# Check Value Validity

checkmate-like check-function. Check whether a list of values is
feasible in the domain. A value is feasible if it is of the same
`storage_type`, inside of the bounds or element of `special_vals`.
`TuneToken`s are generally *not* accepted, so they should be filtered
out before the call, if present.

`domain_check` will return `TRUE` for accepted values, a `character(1)`
error message otherwise.

`domain_test` will return `TRUE` for accepted values, `FALSE` otherwise.

`domain_assert` will return the `param` argument silently for accepted
values, and throw an error message otherwise.

## Usage

``` r
domain_check(param, values, internal = FALSE)

domain_assert(
  param,
  values,
  internal = FALSE,
  .var.name = checkmate::vname(param),
  add = NULL
)

domain_test(param, values)
```

## Arguments

- param:

  (`Domain`).

- values:

  (`any`).

- internal:

  (`logical(1)`)  
  When set, function arguments are not checked for plausibility and
  `special_values` are not respected. This is an optimization for
  internal purposes and should not be used.

## Value

If successful `TRUE`, if not a string with the error message.
