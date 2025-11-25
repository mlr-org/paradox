# Sampler1DNormal Class

Normal sampling (potentially truncated) for
[`p_dbl()`](https://paradox.mlr-org.com/dev/reference/Domain.md).

## See also

Other Sampler:
[`Sampler`](https://paradox.mlr-org.com/dev/reference/Sampler.md),
[`Sampler1D`](https://paradox.mlr-org.com/dev/reference/Sampler1D.md),
[`Sampler1DCateg`](https://paradox.mlr-org.com/dev/reference/Sampler1DCateg.md),
[`Sampler1DRfun`](https://paradox.mlr-org.com/dev/reference/Sampler1DRfun.md),
[`Sampler1DUnif`](https://paradox.mlr-org.com/dev/reference/Sampler1DUnif.md),
[`SamplerHierarchical`](https://paradox.mlr-org.com/dev/reference/SamplerHierarchical.md),
[`SamplerJointIndep`](https://paradox.mlr-org.com/dev/reference/SamplerJointIndep.md),
[`SamplerUnif`](https://paradox.mlr-org.com/dev/reference/SamplerUnif.md)

## Super classes

[`paradox::Sampler`](https://paradox.mlr-org.com/dev/reference/Sampler.md)
-\>
[`paradox::Sampler1D`](https://paradox.mlr-org.com/dev/reference/Sampler1D.md)
-\>
[`paradox::Sampler1DRfun`](https://paradox.mlr-org.com/dev/reference/Sampler1DRfun.md)
-\> `Sampler1DNormal`

## Active bindings

- `mean`:

  (`numeric(1)`)  
  Mean parameter of the normal distribution.

- `sd`:

  (`numeric(1)`)  
  SD parameter of the normal distribution.

## Methods

### Public methods

- [`Sampler1DNormal$new()`](#method-Sampler1DNormal-new)

- [`Sampler1DNormal$clone()`](#method-Sampler1DNormal-clone)

Inherited methods

- [`paradox::Sampler$format()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-format)
- [`paradox::Sampler$print()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-print)
- [`paradox::Sampler$sample()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-sample)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    Sampler1DNormal$new(param, mean = NULL, sd = NULL)

#### Arguments

- `param`:

  ([`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md))  
  Domain / support of the distribution we want to sample from. Must be
  one-dimensional.

- `mean`:

  (`numeric(1)`)  
  Mean parameter of the normal distribution. Default is
  `mean(c(param$lower, param$upper)`.

- `sd`:

  (`numeric(1)`)  
  SD parameter of the normal distribution. Default is
  `(param$upper - param$lower)/4`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Sampler1DNormal$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
