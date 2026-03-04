# Changelog

## paradox 1.0.1-9000

- `ParamSetCollection$flatten()` now detaches `$extra_trafo` completely
  from original ParamSetCollection.
- Option to postfix, instead of prefix, in `ParamSetCollection`,
  [`c()`](https://rdrr.io/r/base/c.html)/[`ps_union()`](https://paradox.mlr-org.com/dev/reference/ps_union.md),
  and
  [`ps_replicate()`](https://paradox.mlr-org.com/dev/reference/ps_replicate.md).
- Add `presence` argument with options `"all"`, `"required"`, and
  `"none"` (default) to `ParamSet$check()`, `$test()`, `$assert()`,
  `$check_dt()`, `$test_dt()`, and `$assert_dt()` to optionally check
  that all or all required parameters are present in the parameter set,
  except for parameters with unsatisfied dependencies.

## paradox 1.0.1

CRAN release: 2024-07-09

- Performance improvements.

## paradox 1.0.0

CRAN release: 2024-06-11

- Removed `Param` objects. `ParamSet` now uses a `data.table`
  internally; individual parameters are more like `Domain` objects now.
  `ParamSets` should be constructed using the
  [`ps()`](https://paradox.mlr-org.com/dev/reference/ps.md) shorthand
  and `Domain` objects. This entails the following major changes:
  - `ParamSet` now supports `extra_trafo` natively; it behaves like
    `.extra_trafo` of the
    [`ps()`](https://paradox.mlr-org.com/dev/reference/ps.md) call.
  - `ParamSet` has `$constraint`
  - `ParamSet` objects are now less mutable. The only properties that
    can be changed are `values`, `tags`, `deps`, `constraint` and
    `extra_trafo`.
  - `ParamSet$is_bounded` is a vector with an entry for each parameter.
    Use `$all_bounded` for the previous behavior.
  - `Condition` objects are now S3 objects and can be constructed with
    [`CondEqual()`](https://paradox.mlr-org.com/dev/reference/Condition.md)
    and
    [`CondAnyOf()`](https://paradox.mlr-org.com/dev/reference/Condition.md),
    instead of `CondXyz$new()`. (It is recommended to use the `Domain`
    interface for conditions, which has not changed)
  - `ParamSet` has new fields `$is_logscale`, `$has_trafo_param`
    (per-param), and `$has_trafo_param` (scalar for the whole set).
- Added a vignette which was previously a chapter in the `mlr3book`
- feat: added support for `InternalTuneToken`s

## paradox 0.11.1

CRAN release: 2023-03-17

- Minor bug fixes.

## paradox 0.11.0

CRAN release: 2022-11-21

- feat: The function
  [`generate_design_sobol()`](https://paradox.mlr-org.com/dev/reference/generate_design_sobol.md)
  generates a space-filling Sobol sequence design.
- refactor: `$set_values` returns the parameter set invisible.

## paradox 0.10.0

CRAN release: 2022-08-05

- Reset `.has_extra_trafo` to `FALSE` when trafo is set to `NULL`.
- `rd_info.ParamSet` collapses vector with `"\n"` due changes in roxygen
  7.2.0
- Add method `set_values()` to conveniently add parameter values.

## paradox 0.9.0

CRAN release: 2022-04-18

- Added
  [`default_values()`](https://paradox.mlr-org.com/dev/reference/default_values.md)
  function to extract default values from `ParamSet` objects.

## paradox 0.8.0

CRAN release: 2022-01-31

- Parameters now have a new (optional) field `description`.
- Improved printing of parameters in documentation
  ([\#355](https://github.com/mlr-org/paradox/issues/355)).
- A warning is now signaled if the package `ParamHelpers` is also
  loaded.
- Fixed some links.

## paradox 0.7.1

CRAN release: 2021-03-07

- `Sampler1D` also accept `ParamSet`s with one `Param` now
  ([\#335](https://github.com/mlr-org/paradox/issues/335)).
- Fixed sampling zero rows in `Sampler1DRfun`
  ([\#338](https://github.com/mlr-org/paradox/issues/338)).
- [`to_tune()`](https://paradox.mlr-org.com/dev/reference/to_tune.md),
  [`p_dbl()`](https://paradox.mlr-org.com/dev/reference/Domain.md), and
  [`p_int()`](https://paradox.mlr-org.com/dev/reference/Domain.md)
  accept `logscale` argument for tuning on a logarithmic scale.
- `to_tune` can be called with only `lower` or only `upper` now and will
  infer the other bound if possible.

## paradox 0.7.0

CRAN release: 2021-01-23

- `ParamSet$get_values()` checks whether all required parameter values
  are set. Required parameter are not checked anymore when new values
  are added to the parameter set.
- `ParamSet$check_dt()` accepts `data.frame`s.
- Rename `is_numeric` and `is_categorical` to `all_numeric` and
  `all_categorical`.
- Rename `requires` to `depends`.

## paradox 0.6.0

CRAN release: 2020-12-04

- [`ps()`](https://paradox.mlr-org.com/dev/reference/ps.md) shortcuts
  for `ParamSet` construction, with new `Domain` construct and
  constructors `p_dbl`, `p_int`, `p_lgl`, `p_fct`, and `p_uty`.
- `ParamSet$search_space()` method that constructs tunable `ParamSet`
  from `TuneToken` objects, which are constructed with
  [`to_tune()`](https://paradox.mlr-org.com/dev/reference/to_tune.md).

## paradox 0.5.0

CRAN release: 2020-10-21

- Compact in-memory representation of R6 objects to save space when
  saving objects via saveRDS(), serialize() etc.
- Improved performance for `ParamSetCollection`.

## paradox 0.4.0

CRAN release: 2020-07-21

- New public methods `is_numeric()` and `is_categorical()` for parameter
  sets.
- Fixed a test for upcoming release of
  [`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).
- Added a helper function to format parameter sets in Rd files.

## paradox 0.3.0

CRAN release: 2020-07-07

- New function
  [`transpose()`](https://rdrr.io/pkg/data.table/man/transpose.html)
  converts `data.table` of parameter values to a list of lists.
- New methods `ParamSet$check_dt()`, `$assert_dt()` and `test_dt()` can
  check a `data.table` for valid parameter values.
- Documentation updated.
- Unified style for object printers.

## paradox 0.2.0

CRAN release: 2020-04-15

- Fixed warnings about partial argument matching.
- Enforce integer bounds in ParamInt
  ([\#258](https://github.com/mlr-org/paradox/issues/258)).
- Reexport
  [`data.table::as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html).
- Deep cloning of `ParamSet$values`
  ([\#273](https://github.com/mlr-org/paradox/issues/273)).

## paradox 0.1.0

CRAN release: 2019-07-12

- Initial release.
