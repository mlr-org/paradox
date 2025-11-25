# Sampler1D Class

1D sampler, abstract base class for Sampler like
[Sampler1DUnif](https://paradox.mlr-org.com/dev/reference/Sampler1DUnif.md),
[Sampler1DRfun](https://paradox.mlr-org.com/dev/reference/Sampler1DRfun.md),
[Sampler1DCateg](https://paradox.mlr-org.com/dev/reference/Sampler1DCateg.md)
and
[Sampler1DNormal](https://paradox.mlr-org.com/dev/reference/Sampler1DNormal.md).

## See also

Other Sampler:
[`Sampler`](https://paradox.mlr-org.com/dev/reference/Sampler.md),
[`Sampler1DCateg`](https://paradox.mlr-org.com/dev/reference/Sampler1DCateg.md),
[`Sampler1DNormal`](https://paradox.mlr-org.com/dev/reference/Sampler1DNormal.md),
[`Sampler1DRfun`](https://paradox.mlr-org.com/dev/reference/Sampler1DRfun.md),
[`Sampler1DUnif`](https://paradox.mlr-org.com/dev/reference/Sampler1DUnif.md),
[`SamplerHierarchical`](https://paradox.mlr-org.com/dev/reference/SamplerHierarchical.md),
[`SamplerJointIndep`](https://paradox.mlr-org.com/dev/reference/SamplerJointIndep.md),
[`SamplerUnif`](https://paradox.mlr-org.com/dev/reference/SamplerUnif.md)

## Super class

[`paradox::Sampler`](https://paradox.mlr-org.com/dev/reference/Sampler.md)
-\> `Sampler1D`

## Active bindings

- `param`:

  ([`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md))  
  Returns the one-dimensional
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)
  that is sampled from.

## Methods

### Public methods

- [`Sampler1D$new()`](#method-Sampler1D-new)

- [`Sampler1D$clone()`](#method-Sampler1D-clone)

Inherited methods

- [`paradox::Sampler$format()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-format)
- [`paradox::Sampler$print()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-print)
- [`paradox::Sampler$sample()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-sample)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

Note that this object is typically constructed via derived classes,
e.g.,
[`Sampler1DUnif`](https://paradox.mlr-org.com/dev/reference/Sampler1DUnif.md).

#### Usage

    Sampler1D$new(param)

#### Arguments

- `param`:

  ([`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md))  
  Domain / support of the distribution we want to sample from. Must be
  one-dimensional.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Sampler1D$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
