# Sampler1DRfun Class

Arbitrary sampling from 1D RNG functions from R.

## See also

Other Sampler:
[`Sampler`](https://paradox.mlr-org.com/dev/reference/Sampler.md),
[`Sampler1D`](https://paradox.mlr-org.com/dev/reference/Sampler1D.md),
[`Sampler1DCateg`](https://paradox.mlr-org.com/dev/reference/Sampler1DCateg.md),
[`Sampler1DNormal`](https://paradox.mlr-org.com/dev/reference/Sampler1DNormal.md),
[`Sampler1DUnif`](https://paradox.mlr-org.com/dev/reference/Sampler1DUnif.md),
[`SamplerHierarchical`](https://paradox.mlr-org.com/dev/reference/SamplerHierarchical.md),
[`SamplerJointIndep`](https://paradox.mlr-org.com/dev/reference/SamplerJointIndep.md),
[`SamplerUnif`](https://paradox.mlr-org.com/dev/reference/SamplerUnif.md)

## Super classes

[`paradox::Sampler`](https://paradox.mlr-org.com/dev/reference/Sampler.md)
-\>
[`paradox::Sampler1D`](https://paradox.mlr-org.com/dev/reference/Sampler1D.md)
-\> `Sampler1DRfun`

## Public fields

- `rfun`:

  (`function()`)  
  Random number generator function.

- `trunc`:

  (`logical(1)`)  
  `TRUE` enables naive rejection sampling, so we stay inside of \[lower,
  upper\].

## Methods

### Public methods

- [`Sampler1DRfun$new()`](#method-Sampler1DRfun-new)

- [`Sampler1DRfun$clone()`](#method-Sampler1DRfun-clone)

Inherited methods

- [`paradox::Sampler$format()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-format)
- [`paradox::Sampler$print()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-print)
- [`paradox::Sampler$sample()`](https://paradox.mlr-org.com/dev/reference/Sampler.html#method-sample)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    Sampler1DRfun$new(param, rfun, trunc = TRUE)

#### Arguments

- `param`:

  ([`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md))  
  Domain / support of the distribution we want to sample from. Must be
  one-dimensional.

- `rfun`:

  (`function()`)  
  Random number generator function, e.g. `rexp` to sample from
  exponential distribution.

- `trunc`:

  (`logical(1)`)  
  `TRUE` enables naive rejection sampling, so we stay inside of \[lower,
  upper\].

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Sampler1DRfun$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
