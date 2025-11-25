# Assertions for Params and ParamSets

Assertions for Params and ParamSets

## Usage

``` r
assert_param_set(
  param_set,
  cl = NULL,
  no_untyped = FALSE,
  must_bounded = FALSE,
  no_deps = FALSE
)
```

## Arguments

- param_set:

  ([`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)).

- cl:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Allowed subclasses.

- no_untyped:

  (`logical(1)`)  
  Are untyped
  [`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md)s
  allowed?

- must_bounded:

  (`logical(1)`)  
  Only bounded
  [`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md)s
  allowed?

- no_deps:

  (`logical(1)`)  
  Are dependencies allowed?

## Value

The checked object, invisibly.
