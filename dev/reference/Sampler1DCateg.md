# Sampler1DCateg Class

Sampling from a discrete distribution, for a
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)
containing a single
[`p_fct()`](https://paradox.mlr-org.com/dev/reference/Domain.md) or
[`p_lgl()`](https://paradox.mlr-org.com/dev/reference/Domain.md).

## See also

Other Sampler:
[`Sampler`](https://paradox.mlr-org.com/dev/reference/Sampler.md),
[`Sampler1D`](https://paradox.mlr-org.com/dev/reference/Sampler1D.md),
[`Sampler1DNormal`](https://paradox.mlr-org.com/dev/reference/Sampler1DNormal.md),
[`Sampler1DRfun`](https://paradox.mlr-org.com/dev/reference/Sampler1DRfun.md),
[`Sampler1DUnif`](https://paradox.mlr-org.com/dev/reference/Sampler1DUnif.md),
[`SamplerHierarchical`](https://paradox.mlr-org.com/dev/reference/SamplerHierarchical.md),
[`SamplerJointIndep`](https://paradox.mlr-org.com/dev/reference/SamplerJointIndep.md),
[`SamplerUnif`](https://paradox.mlr-org.com/dev/reference/SamplerUnif.md)

## Super classes

[`paradox::Sampler`](https://paradox.mlr-org.com/dev/reference/Sampler.md)
-\>
[`paradox::Sampler1D`](https://paradox.mlr-org.com/dev/reference/Sampler1D.md)
-\> `Sampler1DCateg`

## Public fields

- `prob`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html) \| NULL)  
  Numeric vector of `param$nlevels` probabilities.

## Methods

### Public methods

- [`Sampler1DCateg$new()`](#method-Sampler1DCateg-new)

- [`Sampler1DCateg$clone()`](#method-Sampler1DCateg-clone)

Inherited methods

- [`paradox::Sampler$format()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-format)
- [`paradox::Sampler$print()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-print)
- [`paradox::Sampler$sample()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-sample)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    Sampler1DCateg$new(param, prob = NULL)

#### Arguments

- `param`:

  ([`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md))  
  Domain / support of the distribution we want to sample from. Must be
  one-dimensional.

- `prob`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html) \| NULL)  
  Numeric vector of `param$nlevels` probabilities, which is uniform by
  default.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Sampler1DCateg$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
