# Design of Configurations

A lightweight wrapper around a
[ParamSet](https://paradox.mlr-org.com/dev/reference/ParamSet.md) and a
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html),
where the latter is a design of configurations produced from the
former - e.g., by calling a
[`generate_design_grid()`](https://paradox.mlr-org.com/dev/reference/generate_design_grid.md)
or by sampling.

## Public fields

- `param_set`:

  ([ParamSet](https://paradox.mlr-org.com/dev/reference/ParamSet.md)).

- `data`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Stored `data`.

## Methods

### Public methods

- [`Design$new()`](#method-Design-new)

- [`Design$format()`](#method-Design-format)

- [`Design$print()`](#method-Design-print)

- [`Design$transpose()`](#method-Design-transpose)

- [`Design$clone()`](#method-Design-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    Design$new(param_set, data, remove_dupl)

#### Arguments

- `param_set`:

  ([ParamSet](https://paradox.mlr-org.com/dev/reference/ParamSet.md)).

- `data`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Stored `data`.

- `remove_dupl`:

  (`logical(1)`)  
  Remove duplicates?

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    Design$format(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    Design$print(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method `transpose()`

Converts `data` into a list of lists of row-configurations, possibly
removes `NA` entries of inactive parameter values due to unsatisfied
dependencies, and possibly calls the `trafo` function of the
[ParamSet](https://paradox.mlr-org.com/dev/reference/ParamSet.md).

#### Usage

    Design$transpose(filter_na = TRUE, trafo = TRUE)

#### Arguments

- `filter_na`:

  (`logical(1)`)  
  Should `NA` entries of inactive parameter values due to unsatisfied
  dependencies be removed?

- `trafo`:

  (`logical(1)`)  
  Should the `trafo` function of the
  [ParamSet](https://paradox.mlr-org.com/dev/reference/ParamSet.md) be
  called?

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Design$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
