
# paradox 0.8.0
* Removed `Param` objects. `ParamSet` now uses a `data.table` internally; individual parameters are more like `Domain` objects now. `ParamSets` should be constructed using the `ps()` shorthand and `Domain` objects. This entails the following major changes:
    * `ParamSet` now supports `extra_trafo` natively; it behaves like `.extra_trafo` of the `ps()` call.
    * `ParamSet` has `$constraint`
    * `ParamSet` objects are now less mutable. The only properties that can be changed are `values`, `tags`, `deps`, `constraint` and `extra_trafo`.
    * `ParamSet$is_bounded` is a vector with an entry for each parameter. Use `$all_bounded` for the previous behavior.
    * `Condition` objects are now S3 objects and can be constructed with `CondEqual()` and `CondAnyOf()`, instead of `CondXyz$new()`. (It is recommended to use the `Domain` interface for conditions, which has not changed)


# paradox 0.7.1.9000

- Same as previous version.


# paradox 0.7.1

* `Sampler1D` also accept `ParamSet`s with one `Param` now (#335).
* Fixed sampling zero rows in `Sampler1DRfun` (#338).
* `to_tune()`, `p_dbl()`, and `p_int()` accept `logscale` argument for tuning on
  a logarithmic scale.
* `to_tune` can be called with only `lower` or only `upper` now and will infer
  the other bound if possible.

# paradox 0.7.0

* `ParamSet$get_values()` checks whether all required parameter values are set.
  Required parameter are not checked anymore when new values are added to the
  parameter set.
* `ParamSet$check_dt()` accepts `data.frame`s.
* Rename `is_numeric` and `is_categorical` to `all_numeric` and
  `all_categorical`.
* Rename `requires` to `depends`.

# paradox 0.6.0

* `ps()` shortcuts for `ParamSet` construction, with new `Domain` construct and
  constructors `p_dbl`, `p_int`, `p_lgl`, `p_fct`, and `p_uty`.
* `ParamSet$search_space()` method that constructs tunable `ParamSet` from
  `TuneToken` objects, which are constructed with `to_tune()`.

# paradox 0.5.0

* Compact in-memory representation of R6 objects to save space when
  saving objects via saveRDS(), serialize() etc.
* Improved performance for `ParamSetCollection`.

# paradox 0.4.0

* New public methods `is_numeric()` and `is_categorical()` for parameter sets.
* Fixed a test for upcoming release of `data.table()`.
* Added a helper function to format parameter sets in Rd files.

# paradox 0.3.0

* New function `transpose()` converts `data.table` of parameter values to a list
  of lists.
* New methods `ParamSet$check_dt()`, `$assert_dt()` and `test_dt()` can check a
  `data.table` for valid parameter values.
* Documentation updated.
* Unified style for object printers.

# paradox 0.2.0

* Fixed warnings about partial argument matching.
* Enforce integer bounds in ParamInt (#258).
* Reexport `data.table::as.data.table()`.
* Deep cloning of `ParamSet$values` (#273).

# paradox 0.1.0

* Initial release.
