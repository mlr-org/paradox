# Dependency Condition

Condition object, to specify the condition in a dependency.

## Usage

``` r
condition_test(cond, x)

condition_as_string(cond, lhs_chr = "x")

Condition(rhs, condition_format_string)
```

## Arguments

- cond:

  (`Condition`)  
  `Condition` to use

- x:

  (`any`)  
  Value to test

- lhs_chr:

  (`character(1)`)  
  Symbolic representation to use for `<lhs>` in the returned string.

- rhs:

  (`any`)  
  Right-hand-side of the condition.

- condition_format_string:

  (`character(1)`)  
  Format-string for representing the condition when pretty-printing in
  `condition_as_string()`. Should contain two `%s`, as it is used in an
  [`sprintf()`](https://rdrr.io/r/base/sprintf.html)-call with two
  further string values.

## Functions

- `condition_test()`: Used internally. Tests whether a value satisfies a
  given condition. Vectorizes when `x` is atomic.

- `condition_as_string()`: Used internally. Returns a string that
  represents the condition for pretty printing, in the form
  `"<lhs> <relation> <rhs>"`, e.g. `"x == 3"` or
  `"param %in% {1, 2, 10}"`.

## Currently implemented simple conditions

- `CondEqual(rhs)`  
  Value must be equal to `rhs`.

- `CondAnyOf(rhs)`  
  Value must be any value of `rhs`.
