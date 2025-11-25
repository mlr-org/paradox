# SamplerHierarchical Class

Hierarchical sampling for arbitrary param sets with dependencies, where
the user specifies 1D samplers per param. Dependencies are topologically
sorted, parameters are then sampled in topological order, and if
dependencies do not hold, values are set to `NA` in the resulting
`data.table`.

## See also

Other Sampler:
[`Sampler`](https://paradox.mlr-org.com/dev/reference/Sampler.md),
[`Sampler1D`](https://paradox.mlr-org.com/dev/reference/Sampler1D.md),
[`Sampler1DCateg`](https://paradox.mlr-org.com/dev/reference/Sampler1DCateg.md),
[`Sampler1DNormal`](https://paradox.mlr-org.com/dev/reference/Sampler1DNormal.md),
[`Sampler1DRfun`](https://paradox.mlr-org.com/dev/reference/Sampler1DRfun.md),
[`Sampler1DUnif`](https://paradox.mlr-org.com/dev/reference/Sampler1DUnif.md),
[`SamplerJointIndep`](https://paradox.mlr-org.com/dev/reference/SamplerJointIndep.md),
[`SamplerUnif`](https://paradox.mlr-org.com/dev/reference/SamplerUnif.md)

## Super class

[`paradox::Sampler`](https://paradox.mlr-org.com/dev/reference/Sampler.md)
-\> `SamplerHierarchical`

## Public fields

- `samplers`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List of
  [`Sampler1D`](https://paradox.mlr-org.com/dev/reference/Sampler1D.md)
  objects that gives a Sampler for each dimension in the `param_set`.

## Methods

### Public methods

- [`SamplerHierarchical$new()`](#method-SamplerHierarchical-new)

- [`SamplerHierarchical$clone()`](#method-SamplerHierarchical-clone)

Inherited methods

- [`paradox::Sampler$format()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-format)
- [`paradox::Sampler$print()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-print)
- [`paradox::Sampler$sample()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-sample)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    SamplerHierarchical$new(param_set, samplers)

#### Arguments

- `param_set`:

  ([`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md))  
  The
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md) to
  associated with this `SamplerHierarchical`.

- `samplers`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List of
  [`Sampler1D`](https://paradox.mlr-org.com/dev/reference/Sampler1D.md)
  objects that gives a Sampler for each dimension in the `param_set`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SamplerHierarchical$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
