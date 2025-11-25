# Sampler Class

This is the abstract base class for sampling objects like
[Sampler1D](https://paradox.mlr-org.com/dev/reference/Sampler1D.md),
[SamplerHierarchical](https://paradox.mlr-org.com/dev/reference/SamplerHierarchical.md)
or
[SamplerJointIndep](https://paradox.mlr-org.com/dev/reference/SamplerJointIndep.md).

## See also

Other Sampler:
[`Sampler1D`](https://paradox.mlr-org.com/dev/reference/Sampler1D.md),
[`Sampler1DCateg`](https://paradox.mlr-org.com/dev/reference/Sampler1DCateg.md),
[`Sampler1DNormal`](https://paradox.mlr-org.com/dev/reference/Sampler1DNormal.md),
[`Sampler1DRfun`](https://paradox.mlr-org.com/dev/reference/Sampler1DRfun.md),
[`Sampler1DUnif`](https://paradox.mlr-org.com/dev/reference/Sampler1DUnif.md),
[`SamplerHierarchical`](https://paradox.mlr-org.com/dev/reference/SamplerHierarchical.md),
[`SamplerJointIndep`](https://paradox.mlr-org.com/dev/reference/SamplerJointIndep.md),
[`SamplerUnif`](https://paradox.mlr-org.com/dev/reference/SamplerUnif.md)

## Public fields

- `param_set`:

  ([`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md))  
  Domain / support of the distribution we want to sample from.

## Methods

### Public methods

- [`Sampler$new()`](#method-Sampler-new)

- [`Sampler$sample()`](#method-Sampler-sample)

- [`Sampler$format()`](#method-Sampler-format)

- [`Sampler$print()`](#method-Sampler-print)

- [`Sampler$clone()`](#method-Sampler-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

Note that this object is typically constructed via derived classes,
e.g.,
[Sampler1D](https://paradox.mlr-org.com/dev/reference/Sampler1D.md).

#### Usage

    Sampler$new(param_set)

#### Arguments

- `param_set`:

  ([`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md))  
  The
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md) to
  associated with this `Sampler`.

------------------------------------------------------------------------

### Method [`sample()`](https://rdrr.io/r/base/sample.html)

Sample `n` values from the distribution.

#### Usage

    Sampler$sample(n)

#### Arguments

- `n`:

  (`integer(1)`).

#### Returns

[`Design`](https://paradox.mlr-org.com/dev/reference/Design.md).

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    Sampler$format(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    Sampler$print(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Sampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
