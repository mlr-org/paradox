# SamplerJointIndep Class

Create joint, independent sampler out of multiple other samplers.

## See also

Other Sampler:
[`Sampler`](https://paradox.mlr-org.com/dev/reference/Sampler.md),
[`Sampler1D`](https://paradox.mlr-org.com/dev/reference/Sampler1D.md),
[`Sampler1DCateg`](https://paradox.mlr-org.com/dev/reference/Sampler1DCateg.md),
[`Sampler1DNormal`](https://paradox.mlr-org.com/dev/reference/Sampler1DNormal.md),
[`Sampler1DRfun`](https://paradox.mlr-org.com/dev/reference/Sampler1DRfun.md),
[`Sampler1DUnif`](https://paradox.mlr-org.com/dev/reference/Sampler1DUnif.md),
[`SamplerHierarchical`](https://paradox.mlr-org.com/dev/reference/SamplerHierarchical.md),
[`SamplerUnif`](https://paradox.mlr-org.com/dev/reference/SamplerUnif.md)

## Super class

[`paradox::Sampler`](https://paradox.mlr-org.com/dev/reference/Sampler.md)
-\> `SamplerJointIndep`

## Public fields

- `samplers`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List of
  [`Sampler`](https://paradox.mlr-org.com/dev/reference/Sampler.md)
  objects.

## Methods

### Public methods

- [`SamplerJointIndep$new()`](#method-SamplerJointIndep-new)

- [`SamplerJointIndep$clone()`](#method-SamplerJointIndep-clone)

Inherited methods

- [`paradox::Sampler$format()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-format)
- [`paradox::Sampler$print()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-print)
- [`paradox::Sampler$sample()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-sample)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    SamplerJointIndep$new(samplers)

#### Arguments

- `samplers`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List of
  [`Sampler`](https://paradox.mlr-org.com/dev/reference/Sampler.md)
  objects.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SamplerJointIndep$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
