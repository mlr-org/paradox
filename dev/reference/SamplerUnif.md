# SamplerUnif Class

Uniform random sampling for an arbitrary (bounded)
[ParamSet](https://paradox.mlr-org.com/dev/reference/ParamSet.md).
Constructs 1 uniform sampler per parameter, then passes them to
[SamplerHierarchical](https://paradox.mlr-org.com/dev/reference/SamplerHierarchical.md).
Hence, also works for
[ParamSet](https://paradox.mlr-org.com/dev/reference/ParamSet.md)s sets
with dependencies.

## See also

Other Sampler:
[`Sampler`](https://paradox.mlr-org.com/dev/reference/Sampler.md),
[`Sampler1D`](https://paradox.mlr-org.com/dev/reference/Sampler1D.md),
[`Sampler1DCateg`](https://paradox.mlr-org.com/dev/reference/Sampler1DCateg.md),
[`Sampler1DNormal`](https://paradox.mlr-org.com/dev/reference/Sampler1DNormal.md),
[`Sampler1DRfun`](https://paradox.mlr-org.com/dev/reference/Sampler1DRfun.md),
[`Sampler1DUnif`](https://paradox.mlr-org.com/dev/reference/Sampler1DUnif.md),
[`SamplerHierarchical`](https://paradox.mlr-org.com/dev/reference/SamplerHierarchical.md),
[`SamplerJointIndep`](https://paradox.mlr-org.com/dev/reference/SamplerJointIndep.md)

## Super classes

[`paradox::Sampler`](https://paradox.mlr-org.com/dev/reference/Sampler.md)
-\>
[`paradox::SamplerHierarchical`](https://paradox.mlr-org.com/dev/reference/SamplerHierarchical.md)
-\> `SamplerUnif`

## Methods

### Public methods

- [`SamplerUnif$new()`](#method-SamplerUnif-new)

- [`SamplerUnif$clone()`](#method-SamplerUnif-clone)

Inherited methods

- [`paradox::Sampler$format()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-format)
- [`paradox::Sampler$print()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-print)
- [`paradox::Sampler$sample()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-sample)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    SamplerUnif$new(param_set)

#### Arguments

- `param_set`:

  ([`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md))  
  The
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md) to
  associated with this `SamplerUnif`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SamplerUnif$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
